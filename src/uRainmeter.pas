
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

unit uRainmeter;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpjson, Windows;

const
   MAX_CLOSE_HITS = 5;

type

CWidget = class
  	public
   	constructor Create;
      destructor Destroy; override;
      procedure setValue(key:  string; value: string; updateWidget: boolean);
      procedure setCloseHits(closeHits: TJSONArray);
      procedure heartBeat;
      procedure heartBeat(beatTime: TDateTime);
      procedure refreshAll;
      function widgetOnline: boolean;
      
   private
      procedure RainMeterCommand(cmd: string);
      procedure setMeasure(name, value, config: string);
      procedure setMeter(name, value, config: string);
      procedure setOption(section, key, value, config: string);
      procedure updateMeasure(name, config: string);
      procedure updateMeter(name, config: string);
      
   private
   	hWndRainmeter: HWND;
      
end;

implementation

uses uMisc, LCLType, uLog, uGlobals, uLib, dateutils;

const
  	configNetwork = 'MVis\Network';
   configMiners = 'MVis\Miners';
   configActivity = 'MVis\Activity';
   configCloseHits = 'MVis\CloseHits';
  
constructor CWidget.Create;
begin
	hWndRainmeter := 0;
end;
   
destructor CWidget.Destroy;
begin
   // set the gauges to "offline"
   heartBeat(Now - 1.0);
	inherited Destroy;
end;
     
procedure CWidget.setValue(key: string; value: string; updateWidget: boolean);
var
   gaugeStr: string;
   dt: TDateTime;
   s: string;
begin
   // with RainMeter, we ignore the updateWidget parameter.  it is primarily for GeekTool.
   gaugeStr := value;
   if gaugeStr = '' then
      gaugeStr := '0';
   
   case key of 
   	'BlockNumber' : begin
         setMeter('Meter_BlockNumberValue', gaugeStr, configNetwork);
         updateMeter('Meter_BlockNumberValue', configNetwork);
		end;

      'BlockTime' : begin
      	setMeasure('Measure_BlockTime', gaugeStr, configNetwork);
      	updateMeasure('Measure_BlockTime', configNetwork);
		end;

      'PeerCount' : begin
      	setMeter('Meter_PeerCountValue', gaugeStr, configNetwork);
      	updateMeter('Meter_PeerCountValue', configNetwork);
		end;

      'AccountBalance' : begin
      	setMeasure('Measure_Balance', gaugeStr, configNetwork);
      	updateMeasure('Measure_Balance', configNetwork);
		end;
      
      'SolutionCount' : begin
         setMeasure('Measure_SolutionCount', gaugeStr, configNetwork);
         updateMeasure('Measure_SolutionCount', configNetwork);
      	updateMeasure('Measure_SetBalanceColor', configNetwork);
		end;

      'Target' : begin
      	setMeter('Meter_TargetValue', formatHash64(gaugeStr), configActivity);
      	updateMeter('Meter_TargetValue', configActivity);
		end;

      'BestHash' : begin
      	setMeter('Meter_BestHashValue', formatHash64(gaugeStr), configActivity);
      	updateMeter('Meter_BestHashValue', configActivity);
		end;

      'BestHashDate' : begin
         dt := ToDateTime(gaugeStr);
         gaugeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
         setOption('Meter_BestHashValue', 'ToolTipText', gaugeStr, configActivity);
      	updateMeter('Meter_BestHashValue', configActivity);
		end;

   	'LastSolution' : begin
         setOption('Meter_LastSolutionValue', 'ToolTipText', value, configActivity);
         dt := ToDateTime(value);
         if dt = TDateTime(0) then
	      	setMeter('Meter_LastSolutionValue', '', configActivity)
         else begin
         	if MonthsBetween(Now, dt) <= 4 then
	   			setMeter('Meter_LastSolutionValue', FormatDateTime('mmm d', dt), configActivity)
	         else
	   			setMeter('Meter_LastSolutionValue', FormatDateTime('mmm yyyy', dt), configActivity);
         end;
      	updateMeter('Meter_LastSolutionValue', configActivity);
		end;
      
   	'NextSolution' : begin
         dt := ToDateTime(value);
         if MonthsBetween(Now, dt) <= 4 then begin
   			setMeter('Meter_NextSolutionValue', FormatDateTime('mmm d', dt), configActivity);
	         setOption('Meter_NextSolutionValue', 'ToolTipText', FormatDateTime('mmm d, yyyy', dt), configActivity);
			end else begin
   			setMeter('Meter_NextSolutionValue', FormatDateTime('mmm yyyy', dt), configActivity);
	         setOption('Meter_NextSolutionValue', 'ToolTipText', '', configActivity);
         end;
      	updateMeter('Meter_NextSolutionValue', configActivity);
		end;
      
      'MinersGPUs' : begin
      	setMeter('Meter_OnlineValue', gaugeStr, configMiners);
      	updateMeter('Meter_OnlineValue', configMiners);
		end;

      'MinersGPUsStyle' : begin
         if gaugeStr = 'Alert' then
            s := 'BodyStyle|StyleRigCountAlert'
         else
            s := 'BodyStyle';
         setOption('Meter_OnlineLabel', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_OnlineLabel', configMiners);
         setOption('Meter_OnlineValue', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_OnlineValue', configMiners);
		end;

      'GPUTemps' : begin
      	setMeter('Meter_GPUTempsValue', gaugeStr, configMiners);
      	updateMeter('Meter_GPUTempsValue', configMiners);
		end;

      'GPUTempsStyle' : begin
         if gaugeStr = 'Alert' then
            s := 'BodyStyle|StyleTemperatureAlert'
         else
            s := 'BodyStyle';
         setOption('Meter_GPUTempsLabel', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_GPUTempsLabel', configMiners);
         setOption('Meter_GPUTempsValue', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_GPUTempsValue', configMiners);
		end;

      'HashRates' : begin
      	setMeter('Meter_HashRatesValue', gaugeStr, configMiners);
      	updateMeter('Meter_HashRatesValue', configMiners);
		end;

      'HashRateStyle' : begin
         if gaugeStr = 'Alert' then
            s := 'BodyStyle|StyleHashRateAlert'
         else
            s := 'BodyStyle';
         setOption('Meter_HashRatesLabel', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_HashRatesLabel', configMiners);
         setOption('Meter_HashRatesValue', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_HashRatesValue', configMiners);
		end;

      'CloseHitThreshold' : begin
         setOption('Meter_Title', 'ToolTipText', 'Threshold = ' + formatHash64(gaugeStr), configCloseHits);
      	updateMeter('Meter_Title', configCloseHits);
		end;
      
      'HashFaults' : begin
      	setMeter('Meter_HashFaultsValue', gaugeStr, configMiners);
      	updateMeter('Meter_HashFaultsValue', configMiners);
		end;
      
      'HashFaultsStyle' : begin
         if gaugeStr = 'Alert' then
            s := 'BodyStyle|StyleHashFaultAlert'
         else
            s := 'BodyStyle';
         setOption('Meter_HashFaultsLabel', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_HashFaultsLabel', configMiners);
         setOption('Meter_HashFaultsValue', 'MeterStyle', s, configMiners);
      	updateMeter('Meter_HashFaultsValue', configMiners);
		end;
	end;

end;

procedure CWidget.setCloseHits(closeHits: TJSONArray);
var
   i, j: integer;
   jCloseHit: TJSONObject;
   sLabel, sCloseHit, sPercent: string;
   sTooltip: string;
   mask, s: string;
begin
   Log.Writeln(['Trace: CWidget.setCloseHits'], true);
   // get the label mask for the miner
   mask := g_settings.getString('CloseHits.miner_label');
   
   j := 1;
   for i := closeHits.Count - 1 downto closeHits.Count - MAX_CLOSE_HITS do begin
      if i >= 0 then begin
         jCloseHit := closeHits[i] as TJSONObject;
         sTooltip := FormatDateTime('yyyy-mm-dd hh:nn:ss', ToDateTime(jCloseHit['date'].AsString));
         s := FormatDateTime('mm-dd hh:nn', ToDateTime(jCloseHit['date'].AsString));
         sLabel := StringReplace(mask, '#MINER#', jCloseHit.Get('miner', ''), [rfIgnoreCase]);
         sLabel := StringReplace(sLabel, '#GPU#', IntToStr(jCloseHit['gpu'].AsInteger + 1), [rfIgnoreCase]);
         sLabel := StringReplace(sLabel, '#DATE#', s, [rfIgnoreCase]);
         sCloseHit := formatHash64(jCloseHit['close_hit'].AsString);
         sPercent := jCloseHit['percent'].AsString;
		end else begin
         sLabel := '';
         sCloseHit := '';
         sPercent := '0';
         sTooltip := '';
		end;
      
      setMeter('Meter_CloseHit' + IntToStr(j) + '_Label', sLabel, configCloseHits);
   	updateMeter('Meter_CloseHit' + IntToStr(j) + '_Label', configCloseHits);
      setMeter('Meter_CloseHit' + IntToStr(j) + '_Value', sCloseHit, configCloseHits);
      setOption('Meter_CloseHit' + IntToStr(j) + '_Value', 'ToolTipText', sTooltip, configCloseHits);
   	updateMeter('Meter_CloseHit' + IntToStr(j) + '_Value', configCloseHits);
      setOption('Measure_CloseHit' + IntToStr(j), 'Formula', sPercent, configCloseHits);
      updateMeasure('Measure_CloseHit' + IntToStr(j) , configCloseHits);
      
      j += 1;
	end;
end;

procedure CWidget.RainMeterCommand(cmd: string);
var
   cds: COPYDATASTRUCT;
   s: UnicodeString;
begin
   if hWndRainmeter = 0 then begin
	   hWndRainmeter := FindWindow(PChar('DummyRainWClass'), PChar(0));
	   if (hWndRainmeter = 0) then begin
	      Log.Writeln(['CWidget.RainMeterCommand: window handle not found'], true);
         exit;
		end;
	end;
	// Send command to the Rainmeter window
   s := UTF8Decode(cmd);
	cds.dwData := 1;
	cds.cbData := (Length(s) + 1) * sizeof(WCHAR);
	cds.lpData := PUnicodeString(s);
	SendMessage(hWndRainmeter, WM_COPYDATA, 0, LPARAM(@cds));
end;


procedure CWidget.setMeasure(name, value, config: string);
begin
	RainMeterCommand('[!SetOption "' + name + '" "String" "' + value + '" "' + config + '"]');
end;

procedure CWidget.updateMeasure(name, config: string);
begin
	RainMeterCommand('[!UpdateMeasure "' + name + '" "' + config + '"]');
end;

procedure CWidget.setMeter(name, value, config: string);
begin
	RainMeterCommand('[!SetOption "' + name + '" "Text" "' + value + '" "' + config + '"]');
end;

procedure CWidget.setOption(section, key, value, config: string);
begin
   RainMeterCommand('[!SetOption "' + section + '" "' + key + '" "' + value + '" "' + config +'"]');
end;

procedure CWidget.updateMeter(name, config: string);
begin
	RainMeterCommand('[!UpdateMeter "' + name + '" "' + config + '"]');
	RainMeterCommand('[!Redraw "' + config + '"]');
end;

procedure CWidget.heartBeat;
begin
	heartBeat(Now);
end;

procedure CWidget.heartBeat(beatTime: TDateTime);
begin
	RainMeterCommand('[!SetOption "Measure_HeartBeat" "LastBeat" "' + FormatDateTime('yyyy-mm-dd hh:nn:ss', beatTime) +
   						'" "MVis\Network"]')
end;

procedure CWidget.refreshAll;
begin
   // this was introduced primarily for GeekTool
end;

function CWidget.widgetOnline: boolean;
begin
   if hWndRainmeter = 0 then begin
	   hWndRainmeter := FindWindow(PChar('DummyRainWClass'), PChar(0));
	end;      
	result := hWndRainmeter <> 0;
end;


end.

