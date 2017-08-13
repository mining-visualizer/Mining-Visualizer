
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

unit uMinerRPC;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpjson;

const
  
  	MINER_RPC_VERSION = 10;

type

	EnumRate = (
   	RateOff := -3,
      RateOneTime := -2,
      RateOnChange := -1,
      RateRegular := 1				// really, anything > 0  (milliseconds) 
   );

   EnumSolutionAcceptance = (
   	Accepted := 1,
      Rejected,
      Failed
   );
   
   TOnConnectFn = Procedure (connectData: TJSONObject) of object;
   TOnWorkPackageFn = Procedure (target: uint64; blockNumber: uint32) of object;
   TOnBestHashFn = Procedure (bestHash: QWord; bestHashDate: string; minerID: integer) of object;
   TOnGPUTempsFn = Procedure (temps: TJSONArray; minerID: integer) of object;
   TOnFanSpeedsFn = Procedure (speeds: TJSONArray; minerID: integer) of object;
   TOnHashRatesFn = Procedure (rates: TJSONObject; minerID: integer) of object;
   TOnSolutionFn = Procedure (solutions: TJSONArray; minerID: integer; realTime: boolean) of object;
   TOnPeerCountFn = Procedure (count: integer) of object;
   TOnAcctBalanceFn = Procedure (balance: double) of object;
   TOnHashSampleFn = Procedure (sample: QWord; isLastSample: boolean) of object;
   TOnCloseHitFn = Procedure (closeHits: TJSONArray; minerID: integer; realTime: boolean) of object;
   TOnHashFaultFn = Procedure (hashFaults: TJSONArray; minerID: integer; realTime: boolean) of object;
   TOnDisconnectFn = Procedure (minerID: integer; signaled: boolean) of object;

{ CMinerRPC }

CMinerRPC = class

public
   constructor Create;
	procedure connect(minerIP: string; minerPort: integer; minerID: integer; password: string);
	procedure requestWorkPackagesNotify(minerIP: string; minerPort: integer; rate: EnumRate);
   procedure requestBestHashNotify(minerIP: string; minerPort: integer);
   procedure resetBestHash(minerIP: string; minerPort: integer);
   procedure requestGPUTempsNotify(minerIP: string; minerPort: integer; rate: integer; delta: double);
   procedure requestFanSpeedNotify(minerIP: string; minerPort: integer; rate: integer; delta: integer);
   procedure setThermalProtection(minerIP: string; minerPort, neverExceed, safetyShutdown: integer);
   procedure requestHashRatesNotify(minerIP: string; minerPort: integer; rate: integer; delta: double);
   procedure requestPeerCountNotify(minerIP: string; minerPort, rate, delta: integer);
   procedure requestAcctBalanceNotify(minerIP: string; minerPort, rate: integer; delta: double);
   procedure requestHashSamplesNotify(minerIP: string; minerPort, gpu, rate: integer);
   procedure requestCloseHits(minerIP: string; minerPort: integer);
   procedure requestSolutions(minerIP: string; minerPort: integer);
   procedure requestHashFaults(minerIP: string; minerPort: integer);
   procedure setCloseHitThresholds(minerIP: string; minerPort: integer; closeHitThreshold, workUnitFrequency: QWord);
   function keepAlive(minerIP: string; minerPort, retries: integer): boolean;
   procedure disconnect(minerIP: string; minerPort: integer);
	function makePacket(const Args: array of const): TJSONObject;
	procedure stopListening;
   
private
	procedure onSocketReceive(msg: string; ip: string);
	procedure onSocketError(msg: string);
	function waitForPacket(id: integer; timeout: integer = 2000): TJSONObject;

private
   m_timeout: integer;
   m_nextID: integer;
   m_onConnect: TOnConnectFn;
   m_onWorkPackage: TOnWorkPackageFn;
   m_onBestHash: TOnBestHashFn;
   m_onGPUTemps: TOnGPUTempsFn;
   m_onFanSpeeds: TOnFanSpeedsFn;
   m_onHashRates: TOnHashRatesFn;
   m_onSolution: TOnSolutionFn;
   m_onPeerCount: TOnPeerCountFn;
   m_onAcctBalance: TOnAcctBalanceFn;
   m_onHashSample: TOnHashSampleFn;
   m_onCloseHit: TOnCloseHitFn;
   m_onHashFault: TOnHashFaultFn;
   m_onDisconnect: TOnDisconnectFn;

public
   property timeout: integer read m_timeout write m_timeout;
	property onConnect: TOnConnectFn write m_onConnect;
   property onWorkPackage: TOnWorkPackageFn write m_onWorkPackage;
   property onBestHash: TOnBestHashFn write m_onBestHash;
   property onGPUTemps: TOnGPUTempsFn write m_onGPUTemps;
   property onFanSpeeds: TOnGPUTempsFn write m_onFanSpeeds;
   property onHashRates: TOnHashRatesFn write m_onHashRates;
   property onSolution: TOnSolutionFn write m_onSolution;
   property onPeerCount: TOnPeerCountFn write m_onPeerCount;
   property onAcctBalance: TOnAcctBalanceFn write m_onAcctBalance;
   property onHashSample: TOnHashSampleFn write m_onHashSample;
   property onCloseHit: TOnCloseHitFn write m_onCloseHit;
   property onHashFault: TOnHashFaultFn write m_onHashFault;
   property onDisconnect: TOnDisconnectFn write m_onDisconnect;

end;


implementation

uses uGlobals, uLog, DateUtils, uSocket, Main;


{ CMinerRPC }

constructor CMinerRPC.Create;
begin
   Randomize;
   m_nextID := Random(9999);
   g_socket.addListener(ETReceive, TMethod(@onSocketReceive));
   g_socket.addListener(ETError, TMethod(@onSocketError));

   m_onConnect := nil;

end;

procedure CMinerRPC.stopListening;
begin
   Log.Writeln(['Trace: CMinerRPC.stopListening'], true);
   g_socket.removeListener(ETReceive, TMethod(@onSocketReceive));
   g_socket.removeListener(ETError, TMethod(@onSocketError));
end;

procedure CMinerRPC.connect(minerIP: string; minerPort: integer; minerID: integer; password: string);
var
	jObject: TJSONObject;
begin
   Log.Writeln(['Trace: CMinerRPC.connect'], true);
   jObject := makePacket(['command', 'connect',
                          'return_port', g_settings.getInt('UdpListen'),
                          'miner_id', minerID,
                          'password', password,
                          'rpc_version', MINER_RPC_VERSION]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
   // we pick up the response asynchronously
end;

procedure CMinerRPC.requestWorkPackagesNotify(minerIP: string; minerPort: integer; rate: EnumRate);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'work_package', 'rate', rate]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we don't bother with the response
   jObject.Free;
end;

procedure CMinerRPC.requestBestHashNotify(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'best_hash', 'rate', RateOnChange]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.resetBestHash(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'reset_best_hash']);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.requestGPUTempsNotify(minerIP: string; minerPort: integer; rate: integer; delta: double);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'gpu_temps', 'rate', rate, 'delta', delta]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.requestFanSpeedNotify(minerIP: string; minerPort: integer; rate: integer; delta: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'fan_speeds', 'rate', rate, 'delta', delta]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.setThermalProtection(minerIP: string; minerPort, neverExceed, safetyShutdown: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'thermal_protection', 'never_exceed', neverExceed, 'safety_shutdown', safetyShutdown]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.requestHashRatesNotify(
								minerIP: string; 
  								minerPort: integer; 
                        rate: integer; 
                        delta: double);
var
	jObject: TJSONObject;
begin
   jObject := makePacket([	
   					'command', 'hash_rates', 
                  'rate', rate, 
                  'delta', delta
                  ]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.requestPeerCountNotify(minerIP: string; minerPort, rate, delta: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'peer_count', 'rate', rate, 'delta', delta]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.requestAcctBalanceNotify(minerIP: string; minerPort, rate: integer; delta: double);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'account_balance', 'rate', rate, 'delta', delta]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.requestHashSamplesNotify(minerIP: string; minerPort, gpu, rate: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'hash_samples', 'gpu', gpu, 'rate', rate]);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   // we'll process the response as if it was a notification
   jObject.Free;
end;

procedure CMinerRPC.requestCloseHits(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'close_hits']);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.requestSolutions(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'solutions']);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.requestHashFaults(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
   jObject := makePacket(['command', 'hash_faults']);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

procedure CMinerRPC.setCloseHitThresholds(minerIP: string; minerPort: integer; closeHitThreshold, workUnitFrequency: QWord);
var
	jObject: TJSONObject;
begin
   // the JSON library can't handle QWords, so we convert to string
   jObject := makePacket([
   				'command', 'close_hit_threshold', 
               'close_hit_threshold', IntToStr(closeHitThreshold), 
               'work_unit_frequency', IntToStr(workUnitFrequency)]
   );
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

function CMinerRPC.keepAlive(minerIP: string; minerPort, retries: integer): boolean;
var
	jSend, jReceive: TJSONObject;
   i: integer;
begin
   Log.Writeln(['Trace: CMinerRPC.keepAlive'], true);
   jSend := makePacket(['command', 'keep_alive']);
   for i := 1 to retries do begin
		g_socket.Send(jSend.AsJSON, minerIP, minerPort);
		jReceive := waitForPacket(m_nextID);
      if jReceive <> nil then break;
	end;
   jSend.Free;
	result := jReceive <> nil;
   FreeAndNil(jReceive);
end;

// caller is responsible to free returned object.
function CMinerRPC.waitForPacket(id: integer; timeout: integer {= 2000}): TJSONObject;
var
   timeoutTime: TDateTime;
   json: string;
	jObject: TJSONObject;
begin
   Log.Writeln(['Trace: CMinerRPC.waitForPacket'], true);
   result := nil;
   timeoutTime := IncMilliSecond(Now, timeout);
   while Now < timeoutTime do begin
      // wait for a packet to arrive
		json := g_socket.Receive(MillisecondsBetween(Now, timeoutTime));
      if json = '' then begin
         Log.Writeln(['CMinerRPC.waitForPacket timed out.']);
      	break;  // Receive timed out
		end;
		try
         // inspect the packet to see if it the one we're waiting for
			jObject := TJSONObject(GetJSON(json));
	      if (jObject.IndexOfName('id') <> -1) and (jObject['id'].AsInteger = id) then begin
            result := jObject;
            break;
			end;
         jObject.Free;
		except
	   	on e: Exception do
	         Log.Writeln(['Exception in CMinerRPC.waitForPacket : ', e.Message, ', id = ', id, ', JSON = ', json]);
		end;
	end;
end;

procedure CMinerRPC.disconnect(minerIP: string; minerPort: integer);
var
	jObject: TJSONObject;
begin
	jObject := makePacket(['command', 'disconnect']);
   g_socket.Send(jObject.AsJSON, minerIP, minerPort);
   jObject.Free;
end;

function CMinerRPC.makePacket(const Args: array of const): TJSONObject;
var
   jObject: TJSONObject;
begin
   jObject := TJSONObject.Create(Args);
	inc(m_nextID);
	jObject.Add('id', m_nextID);
   result := jObject;
end;

procedure CMinerRPC.onSocketReceive(msg: string; ip: string);
var
	jObject: TJSONObject;
begin
   try
     	jObject := TJSONObject(GetJSON(msg));

      if jObject.IndexOfName('error') <> -1 then begin
         Log.Writeln(['CMinerRPC.onSocketReceive : Miner reported error "', jObject['error'].AsString, '".  Full Response = ', msg]);
         logView.LogMsg('Miner reported error "' + jObject['error'].AsString + '".  See the log file for full details.');
		end

      else begin

         // miner Connect
			if jObject['data_id'].AsString = 'connect' then begin
            if Assigned(m_onConnect) then
            	m_onConnect(jObject);
			end

         // best hash
         else if jObject['data_id'].AsString = 'best_hash' then begin
            if Assigned(m_onBestHash) then
               m_onBestHash(jObject['best_hash'].AsQWord, jObject['best_hash_date'].AsString, jObject['miner_id'].AsInteger);
			end

         // gpu temperatures
         else if jObject['data_id'].AsString = 'gpu_temps' then begin
            if Assigned(m_onGPUTemps) then
               m_onGPUTemps(jObject.Arrays['data'], jObject['miner_id'].AsInteger);
			end

         // fan speeds
         else if jObject['data_id'].AsString = 'fan_speeds' then begin
            if Assigned(m_onFanSpeeds) then
               m_onFanSpeeds(jObject.Arrays['data'], jObject['miner_id'].AsInteger);
			end

         // hash rates
         else if jObject['data_id'].AsString = 'hash_rates' then begin
            if Assigned(m_onHashRates) then
               m_onHashRates(jObject.Objects['data'], jObject['miner_id'].AsInteger);
			end

         // close hits
         else if jObject['data_id'].AsString = 'close_hits' then begin
            if Assigned(m_onCloseHit) then begin
            	m_onCloseHit(jObject.Arrays['data'], jObject['miner_id'].AsInteger, jObject.Get('type', '') = 'notify');
            end;
			end

         // solutions
         else if jObject['data_id'].AsString = 'solutions' then begin
            if Assigned(m_onSolution) then
               m_onSolution(jObject.Arrays['data'], jObject['miner_id'].AsInteger, jObject.Get('type', '') = 'notify');
			end

         // hash faults
         else if jObject['data_id'].AsString = 'hash_faults' then begin
            if Assigned(m_onHashFault) then
               m_onHashFault(jObject.Arrays['data'], jObject['miner_id'].AsInteger, jObject.Get('type', '') = 'notify');
			end

         // miner disconnect
         else if jObject['data_id'].AsString = 'disconnect' then begin
            if Assigned(m_onDisconnect) then
            	m_onDisconnect(jObject['miner_id'].AsInteger, true);
			end

         // work package
         else if jObject['data_id'].AsString = 'work_package' then begin
            if Assigned(m_onWorkPackage) then
            	m_onWorkPackage(jObject.FindPath('data.boundary').AsQWord, jObject.FindPath('data.blocknumber').AsInteger);
			end
         
         // peer count
         else if jObject['data_id'].AsString = 'peer_count' then begin
            if Assigned(m_onPeerCount) then
            	m_onPeerCount(jObject['data'].AsInteger);
			end

         // hash samples
         else if jObject['data_id'].AsString = 'hash_samples' then begin
            if Assigned(m_onHashSample) then
            	m_onHashSample(jObject['data'].AsQWord, jObject.Get('last_sample', false));
			end

         // account balance
         else if jObject['data_id'].AsString = 'account_balance' then begin
            if Assigned(m_onAcctBalance) then
            	m_onAcctBalance(jObject['data'].AsFloat);
			end;

		end;

	except
   	on e: Exception do
      	Log.Writeln(['Exception in CMinerRPC.onSocketReceive : ', e.Message, ', JSON = ', msg]);
	end;
   FreeAndNil(jObject);
end;

procedure CMinerRPC.onSocketError(msg: string);
begin
	Log.Writeln(['CMinerRPC.onSocketError : ', msg]);
end;


end.












