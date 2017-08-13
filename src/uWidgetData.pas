
{
	This file is part of Mining Visualizer.

	Mining Visualizer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Mining Visualizer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cpp-ethereum.  If not, see <http://www.gnu.org/licenses/>.
}

unit uWidgetData;

{$mode objfpc}{$H+}

interface

// this is a generic interface for the desktop widgets.  it also acts as a repository
// for various mining data items - ones that are not persisted to disk.  

uses
	Classes, SysUtils, fpjson,
{$IFDEF Windows}
	uRainmeter;
	//uGeekTool;
{$ELSE}
{$IFDEF Darwin}
   uGeekTool;
{$ELSE}
	uNoWidget;
{$ENDIF}
{$ENDIF}

const
  
	GAUGE_ITEMS = 'BlockNumber, BlockTime, PeerCount, AccountBalance, SolutionCount, ' +
   				  'Target, BestHash, BestHashDate, LastSolution, NextSolution, MinersGPUs, MinersGPUsStyle, ' +
   				  'GPUTemps, GPUTempsStyle, HashRates, HashRateStyle, HashFaults, HashFaultsStyle';
   
   
type

{ CWidgetData }

CWidgetData = class
  	public
      constructor Create;
      destructor Destroy; override;
      procedure setValue(key: string; value: string; updateWidget: boolean = true);
      procedure setValue(key: string; value: integer; updateWidget: boolean = true);
      procedure setValue(key: string; value: Int64; updateWidget: boolean = true);
      procedure setValue(key: string; value: QWord; updateWidget: boolean = true);
      procedure setValue(key: string; value: double; digits: integer; updateWidget: boolean = true);
      
		function  getValue(key: string; _default: string = ''): string;
      procedure getValue(key: string; out value: integer; _default: integer = 0);
      procedure getValue(key: string; out value: Int64; _default: Int64 = 0);
      procedure getValue(key: string; out value: QWord; _default: QWord = 0);
      procedure getValue(key: string; out value: double; _default: double = 0);
      procedure getValue(key: string; out value: TDateTime; _default: TDateTime = 0);
      
      procedure heartBeat;
      procedure gaugeRefresh;
      procedure setCloseHits;
      procedure onSolution;
      function widgetOnline: boolean;
      
   public
      // difficulty threshold for Recent Close Hits (ones that are displayed on the desktop widget)
      closeHitThreshold: QWord;
      
   private
      // this is a list of simple name/value pairs. when we send a value out to the gauge, we also
      // store it in m_dataList in case we need to refer to it later, which comes in handy some times.
      m_dataList: TStringList;
      m_osWidget: CWidget;

end;

implementation

uses LCLType, uLog, uGlobals, dateutils, uLib;
      
constructor CWidgetData.Create;
var
   s: string;
   jUnused: TJSONObject;
   total: integer;
begin
   Log.Writeln(['Trace: CWidgetData.Create'], true);
	m_dataList := TStringList.Create;
   m_osWidget := CWidget.Create;
      
   s := g_miningData.getValue('LastSolution','');
   if s <> '' then
      m_dataList.Values['LastSolution'] := s;
   
   s := g_miningData.getValue('NextSolution','');
   if s <> '' then
      m_dataList.Values['NextSolution'] := s;
   
   s := g_miningData.getValue('BestHash','');
   if s = '' then
   	s := IntToStr(High(QWord));
   m_dataList.Values['BestHash'] := s;
   
   s := g_miningData.getValue('BestHashDate','');
   if s <> '' then
      m_dataList.Values['BestHashDate'] := s;
   
   g_miningData.aggregateSeries('HashFaults', 24, jUnused, total);
   m_dataList.Values['HashFaults'] := IntToStr(total);
   jUnused.Free;
   
   // set the widget balance color
   setValue('SolutionCount', g_miningData.getSeriesItemCount('Solutions') + 1);
   setCloseHits;
   
   // we don't know when the last block was mined by the network, so just assume Now.
   setValue('BlockTime', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
end;

destructor CWidgetData.Destroy;
begin
   Log.Writeln(['Trace: CWidgetData.Destroy'], true);
   m_dataList.Free;
   m_osWidget.Free;
	inherited Destroy;
end;

procedure CWidgetData.setValue(key: string; value: string; updateWidget: boolean);
begin
	m_dataList.Values[key] := value;
   m_osWidget.setValue(key, value, updateWidget);
   case key of 
      'BestHash', 'BestHashDate', 'LastSolution', 'NextSolution' :
         if value <> '' then
         	g_miningData.setValue(key, value);
	end;
end;

procedure CWidgetData.setValue(key: string; value: integer; updateWidget: boolean);
begin
	setValue(key, IntToStr(value), updateWidget);
end;

procedure CWidgetData.setValue(key: string; value: Int64; updateWidget: boolean);
begin
	setValue(key, IntToStr(value), updateWidget);
end;

procedure CWidgetData.setValue(key: string; value: QWord; updateWidget: boolean);
begin
	setValue(key, IntToStr(value), updateWidget);
end;

procedure CWidgetData.setValue(key: string; value: double; digits: integer; updateWidget: boolean);
begin
	setValue(key, FloatToStrF(value,TFloatFormat.ffFixed, 1, digits), updateWidget);
end;

function CWidgetData.getValue(key: string; _default: string): string;
begin
   if m_dataList.IndexOfName(key) = -1 then
      result := _default
   else
		result := m_dataList.Values[key];
end;

procedure CWidgetData.getValue(key: string; out value: integer; _default: integer);
begin
   try
	   if m_dataList.IndexOfName(key) = -1 then
	      value := _default
	   else
			value := StrToInt(m_dataList.Values[key]);
	except
   	on e: Exception do begin
      	value := _default;
         Log.Writeln(['Exception in CWidgetData.getValue - ' + e.Message]);
		end;
	end;
end;

procedure CWidgetData.getValue(key: string; out value: Int64; _default: Int64);
begin
   try
	   if m_dataList.IndexOfName(key) = -1 then
	      value := _default
	   else
			value := StrToInt64(m_dataList.Values[key]);
	except
   	on e: Exception do begin
      	value := _default;
         Log.Writeln(['Exception in CWidgetData.getValue - ' + e.Message]);
		end;
	end;
end;

procedure CWidgetData.getValue(key: string; out value: QWord; _default: QWord);
begin
   try
	   if m_dataList.IndexOfName(key) = -1 then
	      value := _default
	   else
			value := StrToQWord(m_dataList.Values[key]);
	except
   	on e: Exception do begin
      	value := _default;
         Log.Writeln(['Exception in CWidgetData.getValue - ' + e.Message]);
		end;
	end;
end;

procedure CWidgetData.getValue(key: string; out value: double; _default: double);
begin
   try
	   if m_dataList.IndexOfName(key) = -1 then
	      value := _default
	   else
			value := StrToFloat(m_dataList.Values[key]);
	except
   	on e: Exception do begin
      	value := _default;
         Log.Writeln(['Exception in CWidgetData.getValue - ' + e.Message]);
		end;
	end;
end;

procedure CWidgetData.getValue(key: string; out value: TDateTime; _default: TDateTime);
begin
   if m_dataList.IndexOfName(key) = -1 then
      value := _default
   else begin
      value := ToDateTime(m_dataList.Values[key]);
      if value = TDateTime(0) then begin
      	value := _default;
         Log.Writeln(['Exception in CWidgetData.getValue - error converting "', m_dataList.Values[key], '" to TDateTime']);
		end;
   end;
end;

procedure CWidgetData.heartBeat;
begin
   m_osWidget.heartBeat;
end;

procedure CWidgetData.gaugeRefresh;
var
   dataItems: TStringList;
   s: string;
begin
	// we're essentially taking all the values in m_dataList and pushing them out to the desktop widget
   Log.Writeln(['Trace: CCollector.gaugeRefresh'], true);
   dataItems := TStringList.Create;
   dataItems.CommaText := GAUGE_ITEMS;
   for s in dataItems do begin
   	setValue(s, getValue(s), false);
	end;
   m_osWidget.refreshAll;
   dataItems.Free;
end;

procedure CWidgetData.setCloseHits;
begin
   try
      g_miningData.beginRead;
		m_osWidget.setCloseHits(g_miningData.getSeries('CloseHits'));
	finally
     	g_miningData.endRead;
	end;
end;

procedure CWidgetData.onSolution;
begin
   // update the widget balance color
   setValue('SolutionCount', g_miningData.getSeriesItemCount('Solutions') + 1);
end;

function CWidgetData.widgetOnline: boolean;
begin
   result := m_osWidget.widgetOnline;
end;

end.

