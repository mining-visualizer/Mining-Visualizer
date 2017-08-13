
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

unit uTest;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, uWidgetData;

type


{ CTests }

CTests = class

	public
		constructor Create;
      procedure testTask(p: pointer);
      procedure testCloseHit(closeHit: Int64);
      procedure testSynchonousKeepAlive;
      procedure testOnSolution;
      
   private
      g_widgetData: CWidgetData;

end;

CGuardTest = class
	public
		constructor Create;
   	Destructor Destroy; override;
      function give: string;
   
end;


implementation

uses uLog, uTasks, uGlobals, math, fpjson;

constructor CGuardTest.Create;
begin
   Log.Writeln(['Guard Create']);
end;

destructor CGuardTest.Destroy;
begin
	inherited Destroy;
   Log.Writeln(['Guard Destroy']);
end;

function CGuardTest.give: string;
begin
   result := '333';
end;


constructor CTests.Create;
begin
   
end;

procedure CTests.testTask(p: pointer);
begin
end;

procedure CTests.testCloseHit(closeHit: Int64);
var
   jCloseHits: TJSONArray;
   jCloseHit: TJSONObject;
begin
   jCloseHit := TJSONObject.Create([
		'close_hit', closeHit, 
      'date', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
      'gpu_miner', 0,
      'work', 20
   ]);
   
   jCloseHits := TJSONArray.Create;
   jCloseHits.Add(jCloseHit);
	//g_collector.onCloseHit(jCloseHits, g_miners.byRow[0].id);
end;

procedure CTests.testSynchonousKeepAlive;
var
   c: CGuardTest;
   i: integer;
begin
   c := CGuardTest.Create;
   i := StrToInt(c.give);
   
//	if g_minerRPC.keepAlive('192.168.1.81', 5225) then
//   	Log.Writeln(['keepAlive success'])
//   else
//     	Log.Writeln(['keepAlive timed out']);
end;

procedure CTests.testOnSolution;
var
   solutions: TJSONArray;
   solution: TJSONObject;
begin
   solutions := TJSONArray.Create;
   solution := TJSONObject.Create;
   solution.Integers['block'] := 13832;
   solution.Strings['date'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
   solution.Integers['gpu_miner'] := 0;
   solution.Booleans['stale'] := false;
   solution.Integers['state'] := 1;
   solutions.Add(solution);   
   g_collector.onSolution(solutions, 1, true);
end;

end.

