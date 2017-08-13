
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

unit uMiners;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, uMiner, fgl;

type

	TMinerList = specialize TFPGObjectList<CMiner>;

   // this class represents only the 'active' miners.
   
	CMiners = class
		constructor Create;
      destructor Destroy; override;
      procedure loadFromDisk;
      procedure Add(miner: CMiner);
      procedure Remove(minerID: integer);
		procedure resetBestHashRequired(minerID: integer; required: boolean);
      function minerIsActive(minerID: integer): boolean;
      function minerExists(minerID: integer): boolean;
      
   private
   	function getMinerByRow(i: integer): CMiner;
   	function getMinerByID(id: integer): CMiner;
      function getCount: integer;
      function calcFarmRate: double;
      function countMinersOnline: integer;
      function calcExpectedHashRate: double;
      function getTotalGPUs: integer;

   private
   	m_miners: TMinerList;

	public
      property byRow [i: integer] : CMiner read getMinerByRow;
      property byID [id: integer] : CMiner read getMinerByID;
      property count: integer read getCount;
      property expectedHashRate: double read calcExpectedHashRate;
      property farmHashRate: double read calcFarmRate;
      property onlineCount: integer read countMinersOnline;
      property totalGPUs: integer read getTotalGPUs;
      
	end;


implementation

uses fpjson, uGlobals, uMisc, uLog, LCLType;

constructor CMiners.Create;
begin
   m_miners := TMinerList.Create;
	loadFromDisk;
end;

destructor CMiners.Destroy;
begin
   Log.Writeln(['Trace: CMiners.Destroy'], true);
   m_miners.Free;
	inherited Destroy;
end;

procedure CMiners.loadFromDisk;
var
   jArray: TJSONArray;
   jObject: TJSONObject;
   i: integer;
   miner: CMiner;
begin
   Log.Writeln(['Trace: CMiners.loadFromDisk'], true);
   m_miners.Clear;
   // load up the miners definitions. only miners that are active.
	jArray := TJSONArray(g_settings.getValue('Miners', TJSONArray));
	for i := 0 to jArray.count - 1 do begin
      jObject := TJSONObject(jArray[i]);
      if jObject['inactive'].AsBoolean then continue;
		miner := CMiner.Create(jObject);
      miner.online := false;
      m_miners.Add(miner);
	end;
   jArray.Free;
end;

procedure CMiners.Add(miner: CMiner);
begin
   Log.Writeln(['Trace: CMiners.Add'], true);
   
   // we only deal with active miners here.
   if miner.inactive then exit;

   miner.online := false;
   m_miners.Add(miner);
end;

procedure CMiners.Remove(minerID: integer);
begin
   m_miners.Remove(getMinerByID(minerID));
end;

function CMiners.getMinerByRow(i: integer): CMiner;
begin
   if (i < 0) or (i > m_miners.count - 1) then
   	raise Exception2.Create('Exception in CMiners.getMinerByRow : invalid miner index = ' + IntToStr(i));
	result := m_miners[i];
end;

function CMiners.getMinerByID(id: integer): CMiner;
var
   i: integer;
begin
	for i := 0 to m_miners.count - 1 do begin
   	if m_miners[i].id = id then begin
         result := m_miners[i];
         exit;
		end;
	end;
	raise Exception2.Create('Exception in CMiners.getMinerByID : invalid parameter, id=' + IntToStr(id));
end;

function CMiners.minerExists(minerID: integer): boolean;
var
   i: integer;
begin
   result := false;
	for i := 0 to m_miners.count - 1 do begin
   	if m_miners[i].id = minerID then begin
         result := true;
         exit;
		end;
	end;
end;

function CMiners.getCount: integer;
begin
   result := m_miners.count;
end;

function CMiners.calcFarmRate: double;
var
   i: integer;
begin
   result := 0;
	for i := 0 to m_miners.count - 1 do begin
      if m_miners[i].online then
      	result := result + m_miners[i].hashRate;
	end;
end;

function CMiners.countMinersOnline: integer;
var
   i: integer;
begin
   result := 0;
	for i := 0 to m_miners.count - 1 do
      if m_miners[i].online then
      	inc(result);
end;

function CMiners.calcExpectedHashRate: double;
var
   i: integer;
begin
   result := 0;
	for i := 0 to m_miners.count - 1 do
      result += m_miners[i].expectedHashRate;
end;

function CMiners.getTotalGPUs: integer;
var
   i: integer;
begin
   result := 0;
	for i := 0 to m_miners.count - 1 do
      result += m_miners[i].gpuCountDeclared;
end;

procedure CMiners.resetBestHashRequired(minerID: integer; required: boolean);
var
   i: integer;
   jArray: TJSONArray;
   jObject: TJSONObject;
begin
   Log.Writeln(['Trace: CMiners.resetBestHashRequired, required = ', required], true);
	jArray := TJSONArray(g_settings.getValue('Miners', TJSONArray));
	for i := 0 to jArray.count - 1 do begin
      jObject := TJSONObject(jArray[i]);
      if jObject['id'].AsInteger = minerID then begin
	      jObject['reset_best_hash_required'] := TJSONBoolean.Create(required);
         g_settings.putValue('Miners', jArray);
         break;
		end;
	end;
   g_settings.putValue('Miners', jArray);
	jArray.Free;
end;

function CMiners.minerIsActive(minerID: integer): boolean;
var
   i: integer;
begin
   // inactive miners are not loaded into the m_miners array so just check if we can find it.
   result := false;
	for i := 0 to m_miners.count - 1 do begin
   	if m_miners[i].id = minerID then begin
         result := true;
         exit;
		end;
	end;
end;


end.




