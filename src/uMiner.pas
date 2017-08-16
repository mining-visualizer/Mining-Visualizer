
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

unit uMiner;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fgl, fpjson;

const

   // temperature status
   TS_HIGH = %0001;
   TS_LOW =  %0010;

type

// -------------  Class CMiner  --------------------

CGPU = class;

CMiner = class
   
   public
   	constructor Create;
      constructor Create(json: TJSONObject);
      destructor Destroy; override;
      procedure Init;
      function Clone: CMiner;
      procedure setGPUTemps(temps: TJSONArray);
      function getGPUTempStatus: integer;
      function getGPUTempStatusStr: string;
      procedure setHashRates(minerRate: integer; gpuRates: TJSONArray);
      procedure setFanSpeeds(fanSpeeds: TJSONArray);
      function AsJSONObject: TJSONObject;
      
   private type
		TGPUList = specialize TFPGObjectList<CGPU>;

	private
      procedure setGPUCount(_gpuCount: integer);
   	function getGPURates:TJSONArray;
   	function getGPUTemps:TJSONArray;
   	function getFanSpeeds:TJSONArray;
   
   private
      m_gpuCountReported: integer;

   public
      name: string;
      id: integer;
      ip: string;
      port: integer;
      online: boolean;
      lastWorkPackageTime: TDateTime;
      lastSolution: TDateTime;
      bestHash: QWord;
      expectedHashRate: double;
      hashRate: double;
      neverExceedTemp: integer;
      safetyShutdown: integer;
      tempRangeHi: integer;
      hashFaultsAlert: integer;
      resetBestHashRequired: boolean;
      inactive: boolean;
      // this miner should be deleted from our list if it disconnects.
      removeOnDisconnect: boolean;	
      GPUs: TGPUList;
      gpuCountDeclared: integer;
      
      property gpuCountReported: integer read m_gpuCountReported write setGPUCount;
      property gpuRates: TJSONArray read getGPURates;
      property gpuTemps: TJSONArray read getGPUTemps;
      property fanSpeeds: TJSONArray read getFanSpeeds;
      
      
end;
  
// -------------  Class CGPU  --------------------

CGPU = class
   temperature: integer;
	fanSpeed: integer;
   hashRate: double;
end;



implementation

uses uGlobals, uMisc, LCLType, uLog, Forms;

constructor CMiner.Create;
begin
   Init;
end;

constructor CMiner.Create(json: TJSONObject);
begin
   // we're expecting all user editable fields, and resetBestHashRequired, which is a persisted field.
   Init;
	try
		name := json['miner'].AsString;
   	id := json['id'].AsInteger;
	   ip := json['ip'].AsString;
	   port := json['port'].AsInteger;
	   gpuCountDeclared := json['gpu_count'].AsInteger;
	   expectedHashRate := json['expected_hash_rate'].AsFloat;
      neverExceedTemp := json.Get('never_exceed', 80);
      safetyShutdown := json.Get('safety_shutdown', 20);
      tempRangeHi := json.Get('temp_range_hi', 74);
      hashFaultsAlert := json.Get('hash_faults_alert', -1);
      resetBestHashRequired := json.Get('reset_best_hash_required', false);
	   inactive := json['inactive'].AsBoolean;
	except
   	on e: Exception do begin
			Application.MessageBox(PChar('Exception in CMiner.Create - ' + e.Message + LineEnding + 'See log file for complete diagnostics.'), '', MB_ICONERROR);
			Log.Writeln([PChar('Exception in CMiner.Create - ' + e.Message + LineEnding + 'JSON=' + json.AsJSON)]);
		end;
	end;
end;

procedure CMiner.Init;
begin
   lastWorkPackageTime := Now;
   resetBestHashRequired := false;
   name := '';
   ip := '';
   port := DEF_UDP_OUT;
   gpuCountDeclared := 0;
   gpuCountReported := 0;
   expectedHashRate := 0;
   inactive := false;
   neverExceedTemp := 80;
   safetyShutdown := 20;
   tempRangeHi := 74;
   hashFaultsAlert := -1;
   removeOnDisconnect := false;
end;

function CMiner.Clone: CMiner;
begin
   // this only clones user editable fields. note that resetBestHashRequired is not being set (this is a persisted field).
   result := CMiner.Create;
   result.id := self.id;
   result.name := self.name;
   result.ip := self.ip;
   result.port := self.port;
   result.gpuCountDeclared := self.gpuCountDeclared;
   result.hashRate := self.hashRate;
   result.expectedHashRate := self.expectedHashRate;
   result.inactive := self.inactive;
   result.neverExceedTemp := self.neverExceedTemp;
   result.safetyShutdown := self.safetyShutdown;
   result.tempRangeHi := self.tempRangeHi;
   result.hashFaultsAlert := self.hashFaultsAlert;
end;

function CMiner.AsJSONObject: TJSONObject;
begin
   result := TJSONObject.Create([
   				'id', id,
              	'miner', name,
              	'ip', ip,
              	'port', port,
              	'gpu_count', gpuCountDeclared,
              	'expected_hash_rate', expectedHashRate,
               'never_exceed', neverExceedTemp,
               'safety_shutdown', safetyShutdown,
               'temp_range_hi', tempRangeHi,
               'hash_faults_alert', hashFaultsAlert,
              	'inactive', inactive,
               'reset_best_hash_required', resetBestHashRequired
      			]);
end;

destructor CMiner.Destroy;
begin
   Log.Writeln(['Trace: CMiner.Destroy'], true);
	GPUs.Free;
	inherited Destroy;
end;

procedure CMiner.setGPUTemps(temps: TJSONArray);
var
   i: integer;
begin
   // temperature values are multiplied by 100
   try
     	for i := 0 to GPUs.count - 1 do
			GPUs[i].temperature := Round(temps[i].AsInteger / 100.0);
	except
   	on e: Exception do
			MessageBox2(PChar('Exception in CMiner.setGPUTemps - ' + e.Message), '', MB_ICONERROR);
	end;
end;

function CMiner.getGPUTempStatus: integer;
var
   i: integer;
begin
   result := 0;
   if not online then exit;
  	for i := 0 to GPUs.count - 1 do begin
   	if GPUs[i].temperature >= tempRangeHi then
      	result := result or TS_HIGH;
	end;
end;

function CMiner.getGPUTempStatusStr: string;
begin
	if getGPUTempStatus = 0 then
		result := 'OK'
	else
		result := 'Alert';
end;

function CMiner.getGPUTemps: TJSONArray;
var
   i: integer;
begin
   // caller is responsible to free
   result := TJSONArray.Create;
  	for i := 0 to GPUs.count - 1 do begin
      result.Add(GPUs[i].temperature);
	end;
end;


// Note: our concept of a mining farm is different than a miners concept of a mining farm.  we
// think of it as one or more mining rigs, but a mining rig thinks of it has one or more gpu's that
// are installed in its rig.  the 'farmRate' that is being reported here is just the total rate of
// that individual rig.
procedure CMiner.setHashRates(minerRate: integer; gpuRates: TJSONArray);
var
   i: integer;
begin
   // we receive all rates as kH/s
   try
	   hashRate := minerRate / 1000.0;
	  	for i := 0 to GPUs.count - 1 do begin
			GPUs[i].hashRate := gpuRates[i].AsInteger / 1000.0;
		end;
	except
   	on e: Exception do
			MessageBox2(PChar('Exception in CMiner.setHashRates - ' + e.Message), '', MB_ICONERROR);
	end;
end;

function CMiner.getGPURates: TJSONArray;
var
   i: integer;
begin
   // caller is responsible to free
   result := TJSONArray.Create;
  	for i := 0 to GPUs.count - 1 do begin
      result.Add(GPUs[i].hashRate);
	end;
end;

procedure CMiner.setFanSpeeds(fanSpeeds: TJSONArray);
var
   i: integer;
begin
   try
	  	for i := 0 to GPUs.count - 1 do begin
			GPUs[i].fanSpeed := fanSpeeds[i].AsInteger;
		end;
	except
   	on e: Exception do
			MessageBox2(PChar('Exception in CMiner.setFanSpeeds - ' + e.Message), '', MB_ICONERROR);
	end;
end;

function CMiner.getFanSpeeds: TJSONArray;
var
   i: integer;
begin
   // caller is responsible to free
   result := TJSONArray.Create;
  	for i := 0 to GPUs.count - 1 do begin
      result.Add(GPUs[i].fanSpeed);
	end;
end;

procedure CMiner.setGPUCount(_gpuCount: integer);
var
	i: integer;
begin
   m_gpuCountReported := _gpuCount;
	GPUs.Free;
   
   GPUs := TGPUList.Create;
   for i := 1 to _gpuCount do
   	GPUs.Add(CGPU.Create);
end;

end.

