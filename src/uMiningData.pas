
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

unit uMiningData;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpjson;

type
  
CMiningData = class
  	public
      constructor Create;
      destructor Destroy; override;

		function getValue(path: string; defaultVal: string): string;
		procedure setValue(key: string; value: string);
      procedure storeSolutions(newSolutions: TJSONArray; minerID: integer);
      procedure storeHashFaults(newHashFaults: TJSONArray; minerID: integer);
      procedure storeCloseHits(newCloseHits: TJSONArray; minerID: integer);
      
      function getSeries(series: string): TJSONArray;
      function getSeriesItemCount(series: string): integer;
		procedure pruneSeries(series: string; history: integer);
		function calcCloseHitThreshold(hashRate: double; seconds: integer): QWord;
		function scaleCloseHit(closeHit, target: QWord; scaleMix: integer) : integer;
		procedure updateCloseHitThreshold;
		procedure aggregateSeries(series: string; hoursBack: integer; var jResults: TJSONObject; var grandTotal: integer);
      
      procedure beginRead;
      procedure endRead;
      procedure beginWrite;
      procedure endWrite;
      
   private
		procedure writeToDisk;
      
   public
      // difficulty threshold that distinguishs close hits from work units.
      closeHitThreshold: QWord;
      
   private
		m_miningData: TJSONObject;
   	m_filePath: String;
      x_miningData: TMultiReadExclusiveWriteSynchronizer;
      
end;


implementation

uses
	uGlobals, uMisc, LCLType, FileUtil, uLib, math, dateutils;

const

   // days of historical mining data to keep
	CLOSE_HIT_HISTORY = -30;
   WORK_UNIT_HISTORY = -5;
   HASH_FAULT_HISTORY = -3;

   
constructor CMiningData.Create;
var
   contents : TStringList;
begin
	x_miningData := TMultiReadExclusiveWriteSynchronizer.Create;
	try
      try
			m_filePath := ConcatPaths([getConfigFolder(), 'mining_data.json']);
		   contents := TStringList.Create;
		   contents.LoadFromFile(m_filePath);
		   m_miningData := TJSONObject(GetJSON(contents.Text));
	      if m_miningData = nil then
	      	m_miningData := TJSONObject.Create;
		finally
	      contents.Free;
		end;
   except
   	on e: Exception do begin
      	Log.Writeln(['Exception in CMiningData.Create : ', e.Message, ', m_filePath = ', m_filePath]);
			// create an empty JSON object
	      m_miningData := TJSONObject.Create;
	      if FileExists(m_filePath) then begin
		      MessageBox2(PChar('An error ocurred reading JSON values from "' + m_filePath + '". ' +
		      	'This file will be cleared and reset to default values.  The old (corrupt) values ' +
		         'will be saved to "' + m_filePath + '.bak"'),  PChar('Error reading "' + m_filePath + '"'), MB_OK + MB_ICONEXCLAMATION);
	      end;
      	CopyFile(m_filePath, m_filePath + '.bak');
         writeToDisk;
		end;
	end;
end;

destructor CMiningData.Destroy;
begin
   m_miningData.Free;
   x_miningData.Free;
	inherited Destroy;
end;

function CMiningData.getSeries(series: string): TJSONArray;
begin
   // we're returning a reference here, not a copy.  if you need a copy, call getSeries('SomeSeries').clone.
   // the caller should wrap this call with a beginRead/endRead or beginWrite/endWrite, depending on what you're
   // doing with it.
   result := TJSONArray(m_miningData.Find(series));
   if result = nil then begin
      result := TJSONArray.Create;
      m_miningData.Add(series, result);
	end;
end;

function CMiningData.getSeriesItemCount(series: string): integer;
var
   jData: TJSONArray;
begin
   try
      beginRead;
		jData := getSeries(series);
	   result := jData.Count;
	finally
      endRead;
	end;
end;

function CMiningData.getValue(path: string; defaultVal: string): string;
var
	jData: TJSONData;
begin
   try
      beginRead;
		jData := m_miningData.FindPath(path);
	   if jData = nil then
	      result := defaultVal
	   else
	   	result := jData.AsString;
	finally
      endRead;
	end;
end;

procedure CMiningData.setValue(key: string; value: string);
begin
   if value = '' then exit;
   try
      beginWrite;
	   m_miningData.Delete(key);
	   m_miningData.Add(key, TJSONString.Create(value));
	   writeToDisk;
	finally
      endWrite;
	end;
end;

function compareJSON(Item1, Item2: pointer): integer;
// used in the sort routines
begin
   result := CompareStr(TJSONObject(Item1).Strings['date'], TJSONObject(Item2).Strings['date']);
end;

procedure CMiningData.storeSolutions(newSolutions: TJSONArray; minerID: integer);
var
   i: integer;
   solution, sStore: TJSONObject;
   solutions: TJSONArray;
   minerName: string;
begin
   // transfer the solution data into our own JSON format and persist to disk.
   try
      beginWrite;
	   solutions := getSeries('Solutions');
	   minerName := g_miners.byID[minerID].name;
		for i := 0 to newSolutions.count - 1 do begin
			solution := newSolutions.Objects[i];
			sStore := TJSONObject.Create;
	      sStore.Strings['miner'] := minerName;
	      sStore.Integers['miner_id'] := minerID;
	      sStore.Integers['gpu'] := solution['gpu_miner'].AsInteger;
	      sStore.Strings['block'] := solution['block'].AsString;
	      sStore.Strings['date'] := solution['date'].AsString;
	      solutions.Add(sStore);
		end;
	   solutions.Sort(@compareJSON);
	   writeToDisk;
	finally
      endWrite;
	end;
end;

procedure CMiningData.storeHashFaults(newHashFaults: TJSONArray; minerID: integer);
var
   jHashFault: TJSONObject;
   hashFaults: TJSONArray;
   i: integer;
   minerName: string;
begin
   (*
		expected format: 
      	"HashFaults" : [ 
   			{ "date" :  "2017-07-30 08:35:13", "gpu_miner" : 0 }
         ]
   *)
   try
      beginWrite;
	   hashFaults := getSeries('HashFaults');
	   minerName := g_miners.byID[minerID].name;
		for i := 0 to newHashFaults.count - 1 do begin
			jHashFault := TJSONObject.Create;
		   jHashFault.Strings['miner'] := minerName;
	   	jHashFault.Integers['miner_id'] := minerID;
		   jHashFault.Integers['gpu'] := newHashFaults.Objects[i].Integers['gpu_miner'];
		   jHashFault.Strings['date'] := newHashFaults.Objects[i].Strings['date'];
		   hashFaults.Add(jHashFault);
	    end;
	   pruneSeries('HashFaults', HASH_FAULT_HISTORY);
	   hashFaults.Sort(@compareJSON);
	   writeToDisk;
	finally
      endWrite;
	end;
end;

procedure CMiningData.storeCloseHits(newCloseHits: TJSONArray; minerID: integer);
var
   jItem, jCloseHit, jWorkUnit: TJSONObject;
   closeHits, workUnits: TJSONArray;
   i, iGPU, scaleMix: integer;
   minerName: string;
   closeHitRecorded: boolean;
   target: QWord;
begin
   Log.Writeln(['Trace: CMiningData.storeCloseHits'], true);
   try
      beginWrite;
      
      g_widgetData.getValue('Target', target);
   	scaleMix := g_settings.getInt('CloseHits.widget_scale');
      
	   closeHits := getSeries('CloseHits');
	   workUnits := getSeries('WorkUnits');
	   closeHitRecorded := false;
	   minerName := g_miners.byID[minerID].name;
		for i := 0 to newCloseHits.count - 1 do begin
			jItem := newCloseHits.Objects[i];
	      iGPU := jItem['gpu_miner'].AsInteger;
	      Log.Writeln(['CMiningData.onCloseHit : hash = ', jItem['close_hit'].AsQWord, ', date = ',
	      				 jItem['date'].AsString, ', widget = ', jItem['close_hit'].AsQWord < closeHitThreshold], true);
	      if jItem['close_hit'].AsQWord < closeHitThreshold then begin
	
	         // display it on the widget
				jCloseHit := TJSONObject.Create;
			   jCloseHit.Strings['miner'] := minerName;
	      	jCloseHit.Integers['miner_id'] := minerID;
			   jCloseHit.Integers['gpu'] := iGPU;
			   jCloseHit.QWords['close_hit'] := jItem['close_hit'].AsQWord;
			   jCloseHit.Integers['percent'] := scaleCloseHit(jItem['close_hit'].AsQWord, target, scaleMix);
			   jCloseHit.Strings['date'] := jItem['date'].AsString;
			   closeHits.Add(jCloseHit);
	         closeHitRecorded := true;
			end;
	
	      // everything else (including the high difficulty close hits) is counted as a work unit.
			jWorkUnit := TJSONObject.Create;
		   jWorkUnit.Strings['miner'] := minerName;
	   	jWorkUnit.Integers['miner_id'] := minerID;
		   jWorkUnit.Integers['gpu'] := iGPU;
		   jWorkUnit.Strings['date'] := jItem['date'].AsString;
		   workUnits.Add(jWorkUnit);
	    end;
	
	   if closeHitRecorded then begin
	      // use this as an occasion to prune mining data periodically
	      pruneSeries('CloseHits', CLOSE_HIT_HISTORY);
	      pruneSeries('WorkUnits', WORK_UNIT_HISTORY);
		end;
	   closeHits.Sort(@compareJSON);
	   workUnits.Sort(@compareJSON);
		writeToDisk;
	finally
      endWrite;
	end;
end;

procedure CMiningData.writeToDisk;
var
   contents : TStringList;
begin
   try
		contents := TStringList.Create;
	   contents.Text := m_miningData.FormatJSON;
	   contents.SaveToFile(m_filePath);
	   contents.Free;
	except
   	on e: Exception do
      	MessageBox2(PChar('Error in CMiningData.writeToDisk: ' + e.Message), 'Error', MB_OK + MB_ICONEXCLAMATION);
	end;
end;

procedure CMiningData.pruneSeries(series: string; history: integer);
var
   i: integer;
   jData: TJSONArray;
   cutoff: TDateTime;
begin
   Log.Writeln(['Trace: CMiningData.pruneSeries, series = ', series, ', history = ', history], true);
   cutoff := IncDay(Now, history);
   jData := getSeries(series);
   for i := jData.Count - 1 downto 0 do begin
      if ToDateTime(jData.Objects[i].Strings['date']) < cutoff then begin
         jData.Delete(i);
		end;
   end;
end;

function CMiningData.calcCloseHitThreshold(hashRate: double; seconds: integer): QWord;
// hashRate should be MH/s
var
   divisor: QWord;
begin
   divisor := Trunc(hashRate * 1000000 * seconds);
   if divisor = 0 then
   	result := 0
   else begin
   	result := High(QWord) div divisor;
   end;
end;

procedure CMiningData.updateCloseHitThreshold;
var
   i, scaleMix: integer;
   jCloseHit: TJSONObject;
   closeHits: TJSONArray;
   target: QWord;
begin
   // calculate a difficulty threshold for close hits that will be displayed on the desktop widget vs work units.
   closeHitThreshold := calcCloseHitThreshold(g_miners.farmHashRate, g_settings.getInt('CloseHits.closehit_frequency'));
   if closeHitThreshold > 0 then
      g_widgetData.setValue('CloseHitThreshold', closeHitThreshold);
   
   // recalculate the percentages for existing close hits
   try
      beginWrite;
      g_widgetData.getValue('Target', target);
   	scaleMix := g_settings.getInt('CloseHits.widget_scale');
		closeHits := getSeries('CloseHits');
	   for i := 0 to closeHits.Count - 1 do begin
	      jCloseHit := closeHits[i] as TJSONObject;
			jCloseHit.Integers['percent'] := scaleCloseHit(jCloseHit['close_hit'].AsQWord, target, scaleMix);
		end;
	   writeToDisk;
	finally
      endWrite;
	end;
   g_widgetData.setCloseHits;
end;

function CMiningData.scaleCloseHit(closeHit, target: QWord; scaleMix: integer) : integer;
const
  	c_transition: double = 70.0;
var
   x, R: QWord;
   B, divisor, B_Mod: double;
   linearTerm: integer;
   exponentialTerm: integer;
begin
   {
   	GENERAL
   	- user can specify a scaling to use when graphing close hits, ranging from linear to logarithmic.
      - we calculate two terms, linear and exponential, and mix them according to the users preference
        using the following method:
        - for mix values of 1 to c_transition, mix the two terms proportionally
        - for mix values > c_transition, use only the exponential term, with a proportional modification
      
      EXPONENTIAL TERM
   	- for the logarithmic scaling I'm actually using an exponential forumula I managed to hack together
        with my limited mathematical skills and stuff I found on the internet.
      - we're using the formula y = A / exp(B * x) to scale values.  we need to choose values
        for A and B that give us the shape of curve we want.  (a great online graphing site can be
        found at www.desmos.com/calculator.
      - also, see the StackOverflow question "Convert Linear scale to Logarithmic"  
      		http://stackoverflow.com/questions/19472747/convert-linear-scale-to-logarithmic
   	- define M = current close hit threshold (ie. m_closeHitsWidget)
      - define T = target
      - define R = M - T  (range of possible values we need to scale)
      - since we want y to be 100 when x == 0, it is easy to see that A needs to be 100
      - to further nail down the shape of the curve, we specify that at 0.8R, y should be equal to 2.  this
        is just a number I chose because it gave a graph I thought looked about right.
      - therefore, we can solve for B :
       
       		<=>  y = A / exp(Bx)
            <=>  exp(Bx) = A / y
            <=>  Bx = ln(A / y)				 (take the natural log of both sides)
      		<=>  B = ln(A / y) / x
            <=>  B = ln(100 / 2) / (0.8 * R)
            <=>  B = 3.912 / (0.8 * R)
   	
   }
   if (closeHit < target) or (closeHitThreshold < target) then
      exit(0);
   
	x := closeHit - target;
	R := closeHitThreshold - target;
   
   // linear term
   linearTerm := Max(0, Trunc(100 * (1.0 - (x / R))));
   
   // if mix is <= c_transition, we mix the linear and the exponential term 
   // together.  if mix > c_transition, we modify B of the exponential term to give the curve a 
   // sharper bend.
   
   // exponential term
   divisor := 0.8 * R;
   if divisor = 0 then
      exponentialTerm := 0
   else begin
      if scaleMix > c_transition then
         B_Mod := (scaleMix - c_transition + 8) / 8.0
      else
         B_Mod := 1;
	   B := 3.912 / divisor;
      B := B * B_Mod * x;
      // avoid divide by zero. I got the ~700 value from the source code for fpc_exp_real() in rtl/inc/genmath.inc
      if B > 700 then
         exponentialTerm := 0
      else
	   	exponentialTerm := Trunc(100.0 / exp(B));
	end;
   
   if scaleMix <= c_transition then
      // mix the linear and exponential terms together
      result := Trunc((scaleMix * exponentialTerm + (c_transition - scaleMix) * linearTerm) / c_transition)
   else
      // just use the exponential term
      result := exponentialTerm;
end;

procedure CMiningData.aggregateSeries(series: string; hoursBack: integer; var jResults: TJSONObject; var grandTotal: integer);
var
   jData, jCounts: TJSONArray;
	jItem: TJSONObject;
   wux, miner, gpu, i: integer;
   sMinerID: string;
   aDate, cutOff: TDateTime;
begin
   // count up the number of entries for the last number of hours, per rig per gpu.  store results in a json
   // object, containing (of course) name/value pairs.  The 'names' will be the string representation of the miner id,
   // and the 'values' will be an array of integers representing the number of items (typically work units or hash faults)
   // for each gpu of that miner.  the array may contain excess entries at the end, which will be set to zero.
   try
      beginRead;
		jData := getSeries(series);
	   wux := jData.Count - 1;
	   jResults := TJSONObject.Create;
	   grandTotal := 0;

	   // make sure at least the known miners are represented
	   for i := 0 to g_miners.count - 1 do
	      jResults.Add(IntToStr(g_miners.byRow[i].id), TJSONArray.Create([0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));

		cutOff := IncHour(Now, -hoursBack);
	   while wux >= 0 do begin
		   jItem := TJSONObject(jData[wux]);
		   aDate := ToDateTime(jItem.Strings['date']);
	      if aDate < cutOff then break;
	      sMinerID := jItem.Strings['miner_id'];
	      miner := jResults.IndexOfName(sMinerID);
	      if miner = -1 then
	         miner := jResults.Add(sMinerID, TJSONArray.Create([0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));
			jCounts := TJSONArray(jResults.Items[miner]);
	      gpu := jItem.Integers['gpu'];
	      // make sure we've got enough counters in the array
	      for i := jCounts.Count to gpu do
	         jCounts.Add(0);
	      jCounts.Integers[gpu] := jCounts.Integers[gpu] + 1;
	      inc(grandTotal);
	      dec(wux);
		end;
	finally
      endRead;
	end;
end;

procedure CMiningData.beginRead;
begin
   x_miningData.Beginread;
end;

procedure CMiningData.endRead;
begin
   x_miningData.endRead;
end;

procedure CMiningData.beginWrite;
begin
   x_miningData.beginWrite;
end;

procedure CMiningData.endWrite;
begin
   x_miningData.endWrite;
end;


end.

