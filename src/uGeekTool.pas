
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

unit uGeekTool;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fgl, uHTMLDoc, fpjson;

const
   MAX_CLOSE_HITS = 5;

type

// this class, in conjunction with an instance of TItemMap below, forms an association between a string
// id value (like BlockNumber, or PeerCount), the associated CHTMLDoc instance, and the associated
// GeekTool widget.

CItemInfo = class
   public
      constructor Create(_doc: CHTMLDoc; _geeklet: string);
   public
	   doc: CHTMLDoc;
      geeklet: string;
end;	// class CItemInfo

TItemMap = specialize TFPGMapObject<string, CItemInfo>;

{ CWidget }

CWidget = class
  	public
      constructor Create;
      destructor Destroy; override;
      procedure setValue(key:  string; value: string; updateWidget: boolean);
      procedure setCloseHits(closeHits: TJSONArray);
      procedure refreshAll;
      procedure heartBeat;
      procedure heartBeat(beatTime: TDateTime);
      function widgetOnline: boolean;

   private
      procedure refreshGeeklet(name: string);

   private
      m_itemMap: TItemMap;
      m_docNetwork: CHTMLDoc;
      m_docMiners: CHTMLDoc;
      m_docActivity: CHTMLDoc;
      m_docCloseHits: CHTMLDoc;
      
end;	// class CWidget


implementation

uses Forms, uMisc, uLog, LCLType, Process, uGlobals, uLib, dateutils
{$IFDEF Darwin}
   , BaseUnix, Unix
{$ENDIF}
	;

constructor CItemInfo.Create(_doc: CHTMLDoc; _geeklet: string);
begin
   doc := _doc;
   geeklet := _geeklet;
end;

constructor CWidget.Create;
var
   srcFile, destFile, id: string;
{$IFDEF Darwin}
   sa: sigactionrec;
{$ENDIF}
   IDs: TStringList;
begin

{$IFDEF Darwin}
   // install a signal handler for SIGCHLD to reap zombie processes left behind by refreshGeeklet.
   // for more info see "MicroHOWTO : Reap Zombie processes using a SIGCHLD handler"
   //   (http://www.microhowto.info/howto/reap_zombie_processes_using_a_sigchld_handler.html)
   // if you don't do this you eventually get a "Failed to fork process" error.
   sa.Sa_Handler := sigactionhandler(SIG_IGN);		// automatically reap the zombie process
   FPsigEmptySet(sa.Sa_Mask);
   sa.Sa_Flags := 0;
   FPSigaction(SIGCHLD, @sa, nil);
{$ENDIF}

   try
      m_itemMap := TItemMap.Create;
      IDs := TStringList.Create;
      
		// network.html
		srcFile := ConcatPaths([getResourcesFolder, 'GeekTool', 'network.html']);
      destFile := ConcatPaths([getConfigFolder(), 'GeekTool', 'network.html']);
	   IDs.CommaText := 'BlockNumber, BlockTime, PeerCount, AccountBalance, SolutionCount, HeartBeat_Network';
      m_docNetwork := CHTMLDoc.Create(srcFile, destFile, IDs);
      for id in IDs do
      	m_itemMap.Add(id, CItemInfo.Create(m_docNetwork, 'MVis_Network'));
   
	   // miners.html
		srcFile := ConcatPaths([getResourcesFolder, 'GeekTool', 'miners.html']);
      destFile := ConcatPaths([getConfigFolder(), 'GeekTool', 'miners.html']);
	   IDs.CommaText := 'MinersGPUs, GPUTemps, HashRates, HeartBeat_Miners, HashFaults, ' +
      					  'MinersGPUsStyle, GPUTempsStyle, HashRateStyle, HashFaultsStyle';
      m_docMiners := CHTMLDoc.Create(srcFile, destFile, IDs);
      for id in IDs do
      	m_itemMap.Add(id, CItemInfo.Create(m_docMiners, 'MVis_Miners'));
	
	   // activity.html
		srcFile := ConcatPaths([getResourcesFolder, 'GeekTool', 'activity.html']);
      destFile := ConcatPaths([getConfigFolder(), 'GeekTool', 'activity.html']);
	   IDs.CommaText := 'Target, BestHash, BestHashDate, LastSolution, NextSolution, HeartBeat_Activity';
      m_docActivity := CHTMLDoc.Create(srcFile, destFile, IDs);
      for id in IDs do
      	m_itemMap.Add(id, CItemInfo.Create(m_docActivity, 'MVis_Activity'));
	
	   // closehits.html
		srcFile := ConcatPaths([getResourcesFolder, 'GeekTool', 'closehits.html']);
      destFile := ConcatPaths([getConfigFolder(), 'GeekTool', 'closehits.html']);
	   IDs.CommaText := 'CloseHit1_Label, CloseHit1_Value, CloseHit1_Percent, CloseHit1_Date, ' + 
      					  'CloseHit2_Label, CloseHit2_Value, CloseHit2_Percent, CloseHit2_Date, ' +
      					  'CloseHit3_Label, CloseHit3_Value, CloseHit3_Percent, CloseHit3_Date, ' +
      					  'CloseHit4_Label, CloseHit4_Value, CloseHit4_Percent, CloseHit4_Date, ' +
      					  'CloseHit5_Label, CloseHit5_Value, CloseHit5_Percent, CloseHit5_Date, ' +
                       'HeartBeat_CloseHits, CloseHitThreshold';
      m_docCloseHits := CHTMLDoc.Create(srcFile, destFile, IDs);
      for id in IDs do
      	m_itemMap.Add(id, CItemInfo.Create(m_docCloseHits, 'MVis_CloseHits'));

      IDs.Free;
      
	except
      on e: Exception do begin
			raise Exception2.create('Exception in GeekTool.Create - ' + e.Message);
		end;
	end;
end;

destructor CWidget.Destroy;
begin
   Log.Writeln(['Trace: CWidget.Destroy'], true);

   // set the gauges to "offline"
   heartBeat(Now - 1.0);

   m_docNetwork.Free;
   m_docMiners.Free;
   m_docActivity.Free;
   m_docCloseHits.Free;
	m_itemMap.Free;

	inherited Destroy;
end;

procedure CWidget.setValue(key: string; value: string; updateWidget: boolean);
var
   i: integer;
   dt: TDateTime;
   itemInfo: CItemInfo;
begin
   Log.Writeln(['Trace: CWidget.setValue - key = ', key, ', value = ', value], true);
   i := m_itemMap.IndexOf(key);
   if i < 0 then
		raise Exception2.Create('Exception in CWidget.setValue: key "' + key + '" not found in m_itemMap!')
   else begin
      // check for special cases
      if key = 'LastSolution' then begin
         dt := ToDateTime(value);
         if dt = TDateTime(0) then
            value := ''
         else begin
	         if MonthsBetween(Now, dt) <= 4 then
	            value := FormatDateTime('mmm d', dt)
				else
	            value := FormatDateTime('mmm yyyy', dt);
         end;
		end
	   else if key = 'NextSolution' then begin
         // take the full date/time string and reduce it to a simpler format.
         dt := ToDateTime(value);
         if MonthsBetween(Now, dt) <= 4 then
            value := FormatDateTime('mmm d', dt)
			else
            value := FormatDateTime('mmm yyyy', dt);
	   end
      else if key = 'CloseHitThreshold' then begin
         value := 'Threshold = ' + formatHash64(value);
		end
      else if (key = 'BestHash') or (key = 'Target') then begin
         if IsNumeric(value) then
         	value := formatHash64(value);
		end
      else if (key = 'MinersGPUsStyle') or (key = 'GPUTempsStyle') or 
      		  (key = 'HashRateStyle') or (key = 'HashFaultsStyle') then 
      begin
         if value = 'Nominal' then
            value := ''
         else if value = 'Alert' then
         	// the key is actually the value we want to set
            value := key;
		end;

      // now set the value and write to disk
      itemInfo := m_itemMap.Data[i];
      itemInfo.doc.setValue(key, value, updateWidget);
      if updateWidget then
      	refreshGeeklet(itemInfo.geeklet);
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


      setValue('CloseHit' + IntToStr(j) + '_Label', sLabel, false);
      setValue('CloseHit' + IntToStr(j) + '_Value', sCloseHit, false);
      setValue('CloseHit' + IntToStr(j) + '_Percent', sPercent, false);
      setValue('CloseHit' + IntToStr(j) + '_Date', sTooltip, false);

      j += 1;
	end;
end;

procedure CWidget.refreshAll;
begin
	refreshGeeklet('MVis_Network');
	refreshGeeklet('MVis_Miners');
	refreshGeeklet('MVis_Activity');
	refreshGeeklet('MVis_CloseHits');
end;

procedure CWidget.heartBeat;
begin
	heartBeat(Now);
end;

procedure CWidget.heartBeat(beatTime: TDateTime);
var
	s: string;
begin
   s := FormatDateTime('yyyy-mm-dd hh:nn:ss', beatTime);
	setValue('HeartBeat_Network', s, true);
	setValue('HeartBeat_Activity', s, true);
	setValue('HeartBeat_Miners', s, true);
	setValue('HeartBeat_CloseHits', s, true);
end;

function CWidget.widgetOnline: boolean;
var
   process: TProcess;
   res: TStringlist;
begin
   process := TProcess.Create(nil);
	process.Executable := 'osascript';
   process.Parameters.Add(ConcatPaths([getResourcesFolder, 'GeekTool', 'is_geektool_running.scpt']));
   process.Options := process.Options + [poWaitOnExit, poUsePipes];
   process.Execute;
	res := TStringlist.Create;
   res.LoadFromStream(process.Output);
   result := (res.Count > 0) and (res[0] = 'Yes');
	res.Free;
   process.Free;
end;

procedure CWidget.refreshGeeklet(name: string);
var
   process: TProcess;
begin

{$IFDEF Darwin}
	try
		// run an AppleScript command to refresh the widget.
	   Log.Writeln(['Trace: CWidget.refreshGeeklet - geeklet = ', name], true);
		process := TProcess.Create(nil);
	   process.Executable := 'osascript';
		with process.Parameters do begin
	      Add(ConcatPaths([getResourcesFolder, 'GeekTool', 'refresh_geeklet.scpt']));
	      Add(name);
		end;
	   process.Options := process.Options - [poWaitOnExit];
	   process.Execute;
	   process.Free;
	except
   	on e: Exception do begin
			MessageBox2(PChar('Exception in CWidget.refreshGeeklet - ' + e.Message), '', MB_ICONERROR);
		end;
	end;
{$ENDIF}

end;


end.

