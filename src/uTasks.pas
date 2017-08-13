
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

unit uTasks;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fgl, contnrs;

type

TTaskFn = Procedure (data: PtrUInt) of object;

TTaskItem = class
	task: TTaskFn;
   data: PtrUInt;
   when: integer;		// milliseconds until task execution
   timeLeft: integer;
   repeating: boolean;
   strID: string;
end;

TTaskSchedule = specialize TFPGObjectList<TTaskItem>;

{ CScheduler }

CScheduler = class

	public
		constructor Create;
      destructor Destroy; override;
		procedure scheduleTask(_task: TTaskFn; _data: PtrUInt; _when: integer; _repeating: boolean; _strID: string);
      procedure changeRate(_strID: string; _newRate: integer);
		procedure advanceClock;

	private
		m_tasks: TTaskSchedule;
      m_dispatchQ: TObjectQueue;
      m_lastAdvance: TDateTime;

end;


implementation

uses uGlobals, uLog, DateUtils, uMisc;

constructor CScheduler.Create;
begin
  	m_tasks := TTaskSchedule.Create(true);
   m_dispatchQ := TObjectQueue.Create;
   m_lastAdvance := Now;
end;

destructor CScheduler.Destroy;
begin
   Log.Writeln(['Trace: CScheduler.Destroy'], true);
   m_tasks.Free;
   m_dispatchQ.Free;
	inherited Destroy;
end;

procedure CScheduler.scheduleTask(_task: TTaskFn; _data: PtrUInt; _when: integer; _repeating: boolean; _strID: string);
var
	taskItem: TTaskItem;
begin
	taskItem := TTaskItem.Create;
   taskItem.task := _task;
   taskItem.data := _data;
   taskItem.when := _when;
   taskItem.timeLeft := _when;
   taskItem.repeating := _repeating;
   taskItem.strID := _strID;
   m_tasks.Add(taskItem);
end;

procedure CScheduler.changeRate(_strID: string; _newRate: integer);
var
   i: integer;
   found: boolean;
begin
   Log.Writeln(['Trace: CScheduler.changeRate, task = ', _strID, ', newRate = ', _newRate], true);
   found := false;
	for i := 0 to m_tasks.Count - 1 do
      if m_tasks[i].strID = _strID then begin
         m_tasks[i].when := _newRate;
         m_tasks[i].timeLeft := _newRate;
         exit;
		end;
	
   if not found then
   	raise Exception2.Create('Exception in CScheduler.changeRate : task "' + _strID + '" not found in task list.');      
end;

procedure CScheduler.advanceClock;
var
	i: integer;
   millis: integer;
begin
   // check if any task need to be run.
   millis := MillisecondsBetween(Now, m_lastAdvance);
   m_lastAdvance := Now;
	for i := m_tasks.count - 1 downto 0 do
      with m_tasks[i] do begin
	   	dec(timeLeft, millis);
	      if timeLeft <= 0 then begin
            // invoke the task handler.
            Log.Writeln(['Trace: CScheduler.advanceClock : task = ', strID], true);
            task(data);
				if repeating then
	         	timeLeft := when
				else
               // this list will take care of disposing the object.  see the constructor
	         	m_tasks.Delete(i);
			end;
      end;
end;


end.

