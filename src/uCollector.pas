
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

unit uCollector;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, uTasks, uWidgetData, fpjson;

const
   INTVL_DELAYED_START = 2000;
   INTVL_HASH_RATES = 3000;
   INTVL_ASSEMBLE_HASH_RATES = INTVL_HASH_RATES;
   INTVL_TEMPERATURES = 2000;
   INTVL_FANSPEEDS = 2000;
   INTVL_VERIFY_LIAISON = 30 * 1000;
   INTVL_KEEP_ALIVE = 45 * 1000;
   INTVL_POLL_OFFLINE = 10 * 1000;
   INTVL_PEER_COUNT = 20 * 1000;
   INTVL_BALANCE = 30 * 1000;
	
type


CCollector = class(TThread)
	public
		constructor Create;
   	Destructor Destroy; override;
      procedure gaugeRefresh;
      procedure awakeFromSleep;

     	procedure Execute; Override;
      procedure onMinerConnect(connectData: TJSONObject);
      procedure onWorkPackage(target: uint64; blockNumber: uint32);
      procedure onBestHash(bestHash: QWord; bestHashDate: string; minerID: integer);
      procedure resetBestHash(exceptMinerID: integer; realTime: boolean);
      procedure onGPUTemps(temps: TJSONArray; minerID: integer);
      procedure onFanSpeeds(speeds: TJSONArray; minerID: integer);
		procedure updateWidgetTemps;
      procedure onHashRates(rates: TJSONObject; minerID: integer);
      procedure setHashRateAlert;
      procedure setHashFaultDisplay;
      procedure onSolution(solutions: TJSONArray; minerID: integer; realTime: boolean);
      procedure onPeerCount(count: integer);
      procedure onAcctBalance(balance: double);
      procedure onCloseHit(closeHits: TJSONArray; minerID: integer; realTime: boolean);
      procedure onHashFault(hashFaults: TJSONArray; minerID: integer; realTime: boolean);
      procedure onMinerDisconnect(minerID: integer; signaled: boolean);
      procedure delayedStartTasks(mode: PtrUInt);
      procedure calcNextSolution;
		procedure pollOfflineMiners(data: PtrUInt);
      procedure pollOfflinerMinersIn(milliSeconds: integer);
		procedure verifyNodeLiaison(data: PtrUInt);
      procedure countMinersGPUsOnline;
      procedure keepAlive(data: PtrUInt);
      procedure keepAliveThread;
      procedure assembleHashRates(unused: PtrUInt);
      procedure widgetHeartBeat(data: PtrUInt);
		procedure updateAlerts(data: PtrUInt);
		function CheckAlerts: TJSONObject;
      procedure alertUser;
      procedure doAlertAction(ui: boolean = false);
      
      procedure disconnectMiners;

   private
      m_tasks: CScheduler;
      // id of miner who is supplying us with work packages, peerCount and account balance
      m_nodeLiaison: integer;
      // used to detect when hash rates change significantly (some things need recalculation)
      m_hashRateReference: double;
      m_lastWebAlert: string;
      // when did we start, or last awake from sleep.
      m_startedAt: TDateTime;
      m_alertsStartDelay: integer;

end;



// =================  Implementation  =================

implementation

uses uGlobals, uJson, uMinerRPC, uLog, DateUtils, Forms, LCLType, uMiners, 
   	uMiner, uLib, fphttpclient, Main, strutils, LazFileUtils, 
{$IfDef Windows}
      ShellApi,
{$EndIf}
      Process;

type

	DelayedStartTaskEnum = (Startup, SleepAwake);
   

constructor CCollector.Create;
begin
   m_tasks := CScheduler.Create;
   m_nodeLiaison := -1;
   m_hashRateReference := -1;
   m_lastWebAlert := '';
   m_startedAt := Now;
   m_alertsStartDelay := 5000;

   // launch the worker thread straight away
	Inherited Create(false)
end;

destructor CCollector.Destroy;
begin
   Log.Writeln(['Trace: CCollector.Destroy'], true);
	Terminate;	// this tells the worker thread to stop
   g_minerRPC.stopListening;
   disconnectMiners;
   While not Finished do begin
		Sleep(1);
	end;
   m_tasks.Free;
	inherited Destroy;
end;

// this is the main background worker thread.
procedure CCollector.Execute;
const
   sleepTime = 10;
begin

   Log.Writeln(['Trace: CCollector.Execute'], true);
	g_minerRPC.onConnect := @onMinerConnect;
   g_minerRPC.onWorkPackage := @onWorkPackage;
   g_minerRPC.onBestHash := @onBestHash;
   g_minerRPC.onGPUTemps := @onGPUTemps;
   g_minerRPC.onFanSpeeds := @onFanSpeeds;
   g_minerRPC.onHashRates := @onHashRates;
   g_minerRPC.onSolution := @onSolution;
   g_minerRPC.onPeerCount := @onPeerCount;
   g_minerRPC.onAcctBalance := @onAcctBalance;
   g_minerRPC.onCloseHit := @onCloseHit;
   g_minerRPC.onHashFault := @onHashFault;
   g_minerRPC.onDisconnect := @onMinerDisconnect;

   // send Connect messages to all miners. we catch the responses asynchronously in onMinerConnect.
	pollOfflineMiners(0);

   // various tasks that need to be run after a short delay.
   m_tasks.scheduleTask(@delayedStartTasks, Ord(Startup), INTVL_DELAYED_START, false, 'delayedStartTasks');

   // periodic check that our node liaison is still there
   m_tasks.scheduleTask(@verifyNodeLiaison, 0, INTVL_VERIFY_LIAISON, true, 'verifyNodeLiaison');

   // let the miners know we're still here, and to keep sending data
   m_tasks.scheduleTask(@keepAlive, 0, INTVL_KEEP_ALIVE, true, 'keepAlive');

	// periodic check to see if any offline miners have come online recently
   m_tasks.scheduleTask(@pollOfflineMiners, 0, INTVL_POLL_OFFLINE, true, 'pollOfflineMiners');
   
   // after things have settled down, check if there are any alerts
   m_tasks.scheduleTask(@updateAlerts, 0, m_alertsStartDelay + 1000, false, 'alertsStartDelay');

   widgetHeartBeat(0);
   m_tasks.scheduleTask(@widgetHeartBeat, 0, 10 * 1000, true, 'widgetHeartBeat');
   
   // other data is requested when we get the Connect response from the miner (onMinerConnect)
   
   while not Terminated do begin
      sleep(sleepTime);
      m_tasks.advanceClock;
      g_socket.dispatchMsg;
	end;

   Log.Writeln(['CCollector worker thread exiting'], true);
end;


procedure CCollector.delayedStartTasks(mode: PtrUInt);
begin
   Log.Writeln(['Trace: CCollector.delayedStartTasks'], true);
   if g_miners.count = 0 then exit;
   
   // one time call to pick a node liaison after the miners have had a chance to respond to the Connect msg.
	verifyNodeLiaison(0);

   countMinersGPUsOnline;
   // don't call if nobody is online
   if (m_nodeLiaison <> -1) and (DelayedStartTaskEnum(mode) = StartUp) then
   	calcNextSolution;
   if (g_miners.onlineCount = 0) and (DelayedStartTaskEnum(mode) = StartUp) then
      logView.LogMsg('Waiting for miners ...');
   gaugeRefresh;
   
end;


procedure CCollector.awakeFromSleep;
begin
   m_startedAt := Now;
   // things take longer to settle down after a 'wake from sleep'.
   m_alertsStartDelay := 20 * 1000;
   // after things have settled down, check if there are any alerts
   m_tasks.scheduleTask(@updateAlerts, 0, m_alertsStartDelay + 1000, false, 'alertsStartDelay');
   Log.Writeln(['Mining Visualizer has detected a wake from sleep event']);
end;


procedure CCollector.onMinerConnect(connectData: TJSONObject);
var
   minerID: integer;
   closeHitThreshold: QWord;
begin
   Log.Writeln(['Trace: CCollector.onMinerConnect'], true);
   try
      minerID := connectData['miner_id'].AsInteger;
      with g_miners.byID[minerID] do begin
         logView.LogMsg(name + ' has connected');
			online := true;
         gpuCountReported := connectData['gpu_count'].AsInteger;
         g_webFace.onMinerConnect;
   		countMinersGPUsOnline;
   		g_widgetData.setValue('Target', connectData['boundary'].AsQWord);

         if resetBestHashRequired then begin
         	// this miner was offline previously when a new solution came in, so it needs to reset its best hash.
				g_minerRPC.resetBestHash(ip, port);
            g_miners.resetBestHashRequired(minerID, false);
			end;

	      // best hash
			g_minerRPC.requestBestHashNotify(ip, port);

         // gpu temps and thermal protection
         g_minerRPC.requestGPUTempsNotify(ip, port, INTVL_TEMPERATURES, 1.0);
			g_minerRPC.setThermalProtection(ip, port, neverExceedTemp, safetyShutdown);
         g_minerRPC.requestFanSpeedNotify(ip, port, INTVL_FANSPEEDS, 20);
         
         // hash rates
         g_minerRPC.requestHashRatesNotify(ip, port, INTVL_HASH_RATES, 0.2);
        
         // get any new close hits
         if connectData['close_hits'].AsInteger > 0 then
	   		g_minerRPC.requestCloseHits(ip, port);
            
         // get any new solutions
         if connectData['solutions'].AsInteger > 0 then
	   		g_minerRPC.requestSolutions(ip, port);

         // hash faults
         if connectData['hash_faults'].AsInteger > 0 then
	   		g_minerRPC.requestHashFaults(ip, port);

   		closeHitThreshold := g_miningData.calcCloseHitThreshold(g_miners.expectedHashRate, g_settings.getInt('CloseHits.closehit_frequency'));
         g_minerRPC.setCloseHitThresholds(ip, port, closeHitThreshold, g_settings.getInt('CloseHits.workunit_frequency'));
         verifyNodeLiaison(0);
		end;
	except
   	on e: Exception do
			Log.Writeln(['Exception in CCollector.onMinerConnect : ', e.Message]);
	end;
end;


procedure CCollector.onMinerDisconnect(minerID: integer; signaled: boolean);
var
   msg: string;
begin
   Log.Writeln(['Trace: CCollector.onMinerDisconnect'], true);
   try
      with g_miners.byID[minerID] do begin
         logView.LogMsg(name + ' has disconnected');
         online := false;
         if signaled then
            msg := 'miner has disconnected.'
         else
            msg := 'miner has gone offline.';
			Log.Writeln(['CCollector.onMinerDisconnect: ', msg, ' ID = ', minerID, ', IP = ', ip, ', port = ', port]);
         if removeOnDisconnect then
         	g_miners.Remove(minerID);
		end;
      g_webFace.onMinerDisconnect;
   	countMinersGPUsOnline;
      if g_miners.onlineCount = 0 then begin
		   g_widgetData.setValue('GPUTemps', 0);
		end;
      assembleHashRates(0);
		if minerID = m_nodeLiaison then begin
         m_nodeLiaison := -1;
         verifyNodeLiaison(0);
		end;

	except
   	on e: Exception do
			Log.Writeln(['Exception in CCollector.onMinerDisconnect : ', e.Message]);
	end;
end;


procedure CCollector.disconnectMiners;
var
   i: integer;
begin
   Log.Writeln(['Trace: CCollector.shutDown'], true);
   for i := 0 to g_miners.count - 1 do
      if g_miners.byRow[i].online then
			g_minerRPC.disconnect(g_miners.byRow[i].ip, g_miners.byRow[i].port);
end;


procedure CCollector.calcNextSolution;
var
   target, q2: QWord;
   difficulty, seconds: QWord;
   hr: double;
   dt1, dt2: TDateTime;
begin
   Log.Writeln(['Trace: CCollector.calcNextSolution'], true);
   g_widgetData.getValue('NextSolution', dt1, 0);
   
   g_widgetData.getValue('LastSolution', dt1, 0);
   if dt1 = 0 then begin
      dt1 := Now;
	end;

   // get the uppper 64 bits of the target.
   g_widgetData.getValue('Target', target);
   if target = 0 then exit;
   q2 := High(QWord);
   // average # of hashes that need to be computed to get a solution
   difficulty := q2 Div target;
   hr := g_miners.farmHashRate;
   if hr = 0 then exit;
   // # of seconds it will take us based on our current total hash rate
   seconds := difficulty Div Trunc(hr * 1000000.0);
   // convert seconds to days and add it to the last solution
   dt2 := dt1 + seconds / 60.0 / 60.0 / 24.0;
   g_widgetData.setValue('NextSolution', FormatDateTime('yyyy-mm-dd hh:nn:ss', dt2));
end;


procedure CCollector.pollOfflineMiners(data: PtrUInt);
var
   i: integer;
begin
   Log.Writeln(['Trace: CCollector.pollOfflineMiners'], true);
   for i := 0 to g_miners.count - 1 do 
      with g_miners.byRow[i] do 
	      if not online then
				g_minerRPC.connect(ip, port, id, g_settings.getValue('UdpPassword', ''));
end;


procedure CCollector.pollOfflinerMinersIn(milliSeconds: integer);
begin
   m_tasks.scheduleTask(@pollOfflineMiners, 0, milliSeconds, false, 'pollOfflineMinersIn');
end;


procedure CCollector.verifyNodeLiaison(data: PtrUInt);
var
   i: integer;
   miner: CMiner;
begin
   {
		- this is called in DelayedStart, on a regular timer, and when a miner disconnects
		- if no miner has been picked, and there is at least one miner online, then pick the first one and 
        send the necessary data requests
		- if a miner has been picked:
			- if that miner is no longer online, pick another one
			- if we haven't received a work package in a while, send an synchronous keepAlive and 
           make sure we get a response.  if we don't, pick another node liaison.
	}
   
   Log.Writeln(['Trace: CCollector.verifyNodeLiaison'], true);
   if m_nodeLiaison = -1 then begin
      // nobody's currently assigned ... find someone who is online and put in
      // a request for work package notifications.
	   for i := 0 to g_miners.count - 1 do 
      	with g_miners.byRow[i] do 
		      if online then begin
	            m_nodeLiaison := id;
	            g_minerRPC.requestWorkPackagesNotify(ip, port, RateOnChange);
               g_minerRPC.requestPeerCountNotify(ip, port, INTVL_PEER_COUNT, 1);
               g_minerRPC.requestAcctBalanceNotify(ip, port, INTVL_BALANCE, 0.001);
	            break;
				end;
   end else begin
      // check if our supplier is still online
      miner := g_miners.byID[m_nodeLiaison];
      if not miner.online then begin
			m_nodeLiaison := -1;
         verifyNodeLiaison(0);
		end else
         // check that we're getting timely work package notifications
			if SecondsBetween(miner.lastWorkPackageTime, Now) > 60 then begin
	         // check if we're still connected to this miner
            if not g_minerRPC.keepAlive(miner.ip, miner.port, 1) then
               onMinerDisconnect(m_nodeLiaison, false);
			end;
	end;
end;


procedure CCollector.countMinersGPUsOnline;
var
   miners, GPUs, expectedGPUs, i: integer;
begin
   Log.Writeln(['Trace: CCollector.countMinersGPUsOnline'], true);
   // supply the widget with # of miners / GPUs online
   miners := 0;
   GPUs := 0;
   expectedGPUs := 0;
   for i := 0 to g_miners.count - 1 do
      if g_miners.byRow[i].online then begin
         inc(miners);
         GPUs := GPUs + g_miners.byRow[i].gpuCountReported;
         expectedGPUs := expectedGPUs + g_miners.byRow[i].gpuCountDeclared;
		end;
   
   g_widgetData.setValue('MinersGPUs', IntToStr(miners) + '/' + IntToStr(GPUs));
   if (miners <> g_miners.count) or (GPUs <> expectedGPUs) then
      g_widgetData.setValue('MinersGPUsStyle', 'Alert')
   else
      g_widgetData.setValue('MinersGPUsStyle', 'Nominal');
   updateAlerts(0);
end;


procedure CCollector.keepAlive(data: PtrUInt);
begin
   Log.Writeln(['Trace: CCollector.keepAlive'], true);
   
   // we'll be sending synchronous keep-alives, so this could take awhile if a miner
   // is unresponsive.  don't want to block the main worker thread too long.
   TThread.ExecuteInThread(@keepAliveThread);
end;

procedure CCollector.keepAliveThread;
var
   i: integer;
begin
   Log.Writeln(['Trace: CCollector.keepAliveThread'], true);
   // check all miners to be sure they are still online.
   for i := 0 to g_miners.count - 1 do
      with g_miners.byRow[i] do
	      if online then begin
				online := g_minerRPC.keepAlive(ip, port, 3);
            if not online then
               onMinerDisconnect(id, false);
			end;

   countMinersGPUsOnline;
end;

// take the hash rates that have been reported to us most recently and write them out to the widget data file
procedure CCollector.assembleHashRates(unused: PtrUInt);
var
   i: integer;
   s, sep: string;
begin
   Log.Writeln(['Trace: CCollector.assembleHashRates'], true);
   try
	   if EnumHashRates(g_settings.getInt('HashRates')) = IndividualGPUs then begin
		   sep := '';
	      s := '';
         // this option is only possible if there is one mining rig.
		   for i := 0 to g_miners.byRow[0].GPUs.count - 1 do begin
		      s := s + sep + FloatToStrF(g_miners.byRow[0].GPUs[i].hashRate, TFloatFormat.ffFixed, 1, 1);
		      sep := ', ';
			end;
	      g_widgetData.setValue('HashRates', s);
	   end 
	   else if EnumHashRates(g_settings.getInt('HashRates')) = HRGrandTotal then begin
	      g_widgetData.setValue('HashRates', FloatToStrF(g_miners.farmHashRate , ffFixed, 1, 1));
	   end 
	   else if EnumHashRates(g_settings.getInt('HashRates')) = HRIndividualRigs then begin
		   sep := '';
	      s := '';
		   for i := 0 to g_miners.count - 1 do 
         	if g_miners.byRow[i].online then begin
			      s := s + sep + FloatToStrF(g_miners.byRow[i].hashRate, TFloatFormat.ffFixed, 1, 1);
			      sep := ', ';
				end;
	      g_widgetData.setValue('HashRates', s);
	   end
	except
   	on e: Exception do
			Log.Writeln(['Exception in CCollector.assembleHashRates : ', e.Message]);
	end;
end;


procedure CCollector.widgetHeartBeat(data: PtrUInt);
begin
   g_widgetData.heartBeat;
end;


procedure CCollector.gaugeRefresh;
begin
   // push all values to the desktop widget
   Log.Writeln(['Trace: CCollector.gaugeRefresh'], true);
   g_widgetData.gaugeRefresh;
   g_widgetData.heartBeat;
   if g_miners.onlineCount > 0 then
   	g_miningData.updateCloseHitThreshold;
   setHashFaultDisplay;
end;


procedure CCollector.onWorkPackage(target: uint64; blockNumber: uint32);
begin
   Log.Writeln(['Trace: CCollector.onWorkPackage'], true);
   if m_nodeLiaison <> -1 then
   	g_miners.byID[m_nodeLiaison].lastWorkPackageTime := Now;
   Log.Writeln(['Work package received.  Target = ', target, ', blockNumber = ', blockNumber], true);
   g_widgetData.setValue('BlockNumber', blockNumber, false);
   g_widgetData.setValue('BlockTime', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
   g_widgetData.setValue('Target', target);
   g_webFace.onWorkPackage(target, blockNumber);
   
   // every so often we need to update the hash fault display to account for any faults that have expired.
   if blockNumber mod 12 = 0 then 
      setHashFaultDisplay;
end;


procedure CCollector.onBestHash(bestHash: QWord; bestHashDate: string; minerID: integer);
var
   bh: QWord;
begin
   // we get this in real time, but also in response to our requestBestHashNotify, so don't assume the best hash occurred now.
   Log.Writeln(['Trace: CCollector.onBestHash : hash = ', bestHash, ', miner = ', minerID, ', date = ', bestHashDate], true);
   g_miners.byID[minerID].bestHash := bestHash;
   g_widgetData.getValue('BestHash', bh, High(QWord));
   if bestHash < bh then begin
   	g_widgetData.setValue('BestHash', bestHash);
   	g_widgetData.setValue('BestHashDate', bestHashDate);
   end;
end;


procedure CCollector.onGPUTemps(temps: TJSONArray; minerID: integer);
begin
   Log.Writeln(['Trace: CCollector.onGPUTemps'], true);
   try
      // incoming temperature values are multiplied by 100
	   g_miners.byID[minerID].setGPUTemps(temps);
      updateWidgetTemps;
      g_webFace.onGPUTemps(minerID, g_miners.byID[minerID].getGPUTempStatusStr, g_miners.byID[minerID].gpuTemps);
	except
   	on e: Exception do
			Log.Writeln(['Exception in CCollector.onGPUTemps : ', e.Message]);
	end;
end;

procedure CCollector.onFanSpeeds(speeds: TJSONArray; minerID: integer);
begin
   Log.Writeln(['Trace: CCollector.onFanSpeeds = ', speeds.AsJSON], true);
   g_miners.byID[minerID].setFanSpeeds(speeds);
   g_webFace.onFanSpeeds(minerID, speeds);
end;

procedure CCollector.updateWidgetTemps;
var
   i, status: integer;
   s, sep: string;
begin
	// determine a status indicator to display
   Log.Writeln(['Trace: CCollector.updateWidgetTemps'], true);
   
   if g_miners.count = 0 then exit;
   status := 0;
	for i := 0 to g_miners.count - 1 do
		status := status or g_miners.byRow[i].getGPUTempStatus;
   
   if g_settings.getInt('TemperatureDisplay') = Ord(TDGPUs) then begin
      // display individual gpu temps. the only way this option can be active is if there is only one miner.
	   sep := '';
      s := '';
	   for i := 0 to g_miners.byRow[0].GPUs.Count - 1 do begin
	      s := s + sep + IntToStr(g_miners.byRow[0].GPUs[i].temperature);
	      sep := ', ';
		end;
      g_widgetData.setValue('GPUTemps', s);
	end
   else if g_settings.getInt('TemperatureDisplay') = Ord(TDStatus) then begin
		if status = 0 then begin
		   // this is the text that will be displayed
		   g_widgetData.setValue('GPUTemps', 'OK');
		end
		else begin
		   g_widgetData.setValue('GPUTemps', 'Alert');
		end;
	end;
   
	if status = 0 then
	   // this is a keyword that the widget can interpret to apply text styles.
	   g_widgetData.setValue('GPUTempsStyle', 'Nominal')
	else
	   g_widgetData.setValue('GPUTempsStyle', 'Alert');
   updateAlerts(0);
end;

procedure CCollector.onHashRates(rates: TJSONObject; minerID: integer);
var
   farmRate: double;
begin
   Log.Writeln(['Trace: CCollector.onHashRates'], true);
   try
   	// all rates are in kH/s
      g_miners.byID[minerID].setHashRates(rates['farm_rate'].AsInteger, TJSONArray(rates['miner_rates']));
      assembleHashRates(0);
      
      // check for significant change in farm rate, and recalculate some things if necessary.
      farmRate := g_miners.farmHashRate;
      if farmRate = 0 then exit;
      if m_hashRateReference < 0 then begin
         // first time
         m_hashRateReference := farmRate;
         g_miningData.updateCloseHitThreshold;
		end
		else begin
         if abs(m_hashRateReference - farmRate) / m_hashRateReference > 0.15 then begin
         	m_hashRateReference := farmRate;
            calcNextSolution;
            g_miningData.updateCloseHitThreshold;
			end;
		end;
      setHashRateAlert;
      g_webFace.onHashRates(minerID, g_miners.byID[minerID].hashRate, g_miners.byID[minerID].gpuRates);
	except
   	on e: Exception do
			Log.Writeln(['Exception in CCollector.onHashRates : ', e.Message]);
	end;
end;

procedure CCollector.setHashRateAlert;
begin
   // check if we need to display an alert on the widget for hash rate abnormalities.
   if abs(g_miners.farmHashRate - g_miners.expectedHashRate) > StrToFloat(g_settings.getString('HashRateDeviationAlert')) then
      g_widgetData.setValue('HashRateStyle', 'Alert')
   else
      g_widgetData.setValue('HashRateStyle', 'Nominal');
   updateAlerts(0);
end;

procedure CCollector.setHashFaultDisplay;
var
   jMiners: TJSONObject;
   jTotals: TJSONArray;
   grandTotal, i, j: integer;
   alert: boolean;
begin
   // check last 4 hours
   g_miningData.aggregateSeries('HashFaults', 4, jMiners, grandTotal);
   // check to see if we should display an alert
   alert := false;
   for i := 0 to g_miners.count - 1 do 
      with g_miners.byRow[i] do begin
      	if hashFaultsAlert = -1 then continue;
	      jTotals := jMiners.Arrays[IntToStr(id)];
	      for j := 0 to jTotals.Count - 1 do begin
	         if jTotals.Integers[j] > hashFaultsAlert then
	            alert := true;
			end;
		end;
   if alert then
	   // this is a keyword that the widget can interpret to apply text styles.
	   g_widgetData.setValue('HashFaultsStyle', 'Alert')
	else
	   g_widgetData.setValue('HashFaultsStyle', 'Nominal');
   jMiners.Free;
   
   // now do 24 hours, which is what we display for informational purposes.
   g_miningData.aggregateSeries('HashFaults', 24, jMiners, grandTotal);
	g_widgetData.setValue('HashFaults', grandTotal);
   jMiners.Free;
   updateAlerts(0);
end;


procedure CCollector.onCloseHit(closeHits: TJSONArray; minerID: integer; realTime: boolean);
var
   jCloseHit: TJSONObject;
begin
   g_miningData.storeCloseHits(closeHits, minerID);
   g_widgetData.setCloseHits;
   if realTime then begin
		jCloseHit := closeHits.Objects[0];
      if jCloseHit['close_hit'].AsQWord < g_miningData.closeHitThreshold then begin
         g_webFace.onCloseHit(g_miners.byID[minerID].name, minerID, jCloseHit.Integers['gpu_miner'], jCloseHit.QWords['close_hit']);
         // need to give the web page some time to send us back another long polling request.
         sleep(100);
		end;
		g_webFace.onWorkUnit(g_miners.byID[minerID].name, minerID, jCloseHit.Integers['gpu_miner']);
	end;
end;


procedure CCollector.onSolution(solutions: TJSONArray; minerID: integer; realTime: boolean);
var
   solution: TJSONObject;
   i: integer;
   lastSoln: TDateTime;
begin
   Log.Writeln(['Trace: CCollector.onSolution'], true);
   g_widgetData.getValue('LastSolution', lastSoln, 0);
   with g_miners.byID[minerID] do begin
		for i := 0 to solutions.count - 1 do begin
			solution := solutions.Objects[i];
	      if solution['state'].AsInteger = ord(Accepted) then begin
         	lastSolution := ToDateTime(solution['date'].AsString);
            if lastSolution > lastSoln then begin
			      g_widgetData.setValue('LastSolution', FormatDateTime('yyyy-mm-dd hh:nn:ss', lastSolution));
               lastSoln := lastSolution;
               calcNextSolution;
               resetBestHash(minerID, realTime);
				end;
			end;
		end;
      g_miningData.storeSolutions(solutions, minerID);
   	g_widgetData.onSolution;

	   if realTime then begin
			solution := solutions.Objects[0];
			g_webFace.onSolution(name, minerID, solution['gpu_miner'].AsInteger, solution['block'].AsInteger);
		end;
	end;
end;


procedure CCollector.onHashFault(hashFaults: TJSONArray; minerID: integer; realTime: boolean);
begin
   g_miningData.storeHashFaults(hashFaults, minerID);
   if realTime then begin
		g_webFace.onHashFault(g_miners.byID[minerID].name, minerID, hashFaults.Objects[0].Integers['gpu_miner']);
	end;
   setHashFaultDisplay;
end;


procedure CCollector.onPeerCount(count: integer);
begin
   Log.Writeln(['Trace: CCollector.onPeerCount'], true);
   g_widgetData.setValue('PeerCount', count);
end;


procedure CCollector.onAcctBalance(balance: double);
begin
   Log.Writeln(['Trace: CCollector.onAcctBalance'], true);
   g_widgetData.setValue('AccountBalance', balance, 3);
   
end;


procedure CCollector.resetBestHash(exceptMinerID: integer; realTime: boolean);
var
   i: integer;
begin
   Log.Writeln(['Trace: CCollector.resetBestHash'], true);
   for i := 0 to g_miners.count - 1 do begin
      with g_miners.byRow[i] do
      	if id <> exceptMinerID then
         	if online then
					g_minerRPC.resetBestHash(ip, port)
            else
               // next time this miner connects we'll tell it to reset its best hash.
               g_miners.resetBestHashRequired(id, true);
	end;

   if realTime then
   	g_widgetData.setValue('BestHash', High(QWord))
   else
		g_widgetData.setValue('BestHash', g_miners.byID[exceptMinerID].bestHash);
   
end;

procedure CCollector.updateAlerts(data: PtrUInt);
var
   s: string;
   jAlerts: TJSONObject;
begin
   jAlerts := CheckAlerts;
   s := jAlerts.AsJSON;
   // at startup, or after we awake from sleep, we want to avoid flashing alerts until all miners 
   // have reported in and things have stabilized somewhat.  there's a delayed start task scheduled 
   // at startup slightly after m_alertsStartDelay to make sure we send out the necessary
   // alerts at that time in case there's a lull in activity right then.
   if (s <> m_lastWebAlert) and (MillisecondsBetween(Now, m_startedAt) > m_alertsStartDelay) then begin
      if jAlerts['MasterAlert'].AsBoolean then
      	alertUser;
      g_webFace.updateAlerts(jAlerts); // jAlerts is freed in the g_webFace.updateAlerts call chain
      m_lastWebAlert := s;
	end else
   	jAlerts.Free;
end;

function CCollector.CheckAlerts: TJSONObject;
begin
   result := TJSONObject.Create;
   result.Add('MinersGPUs', g_widgetData.getValue('MinersGPUsStyle') = 'Alert');
   result.Add('HashRates', g_widgetData.getValue('HashRateStyle') = 'Alert');
   result.Add('GPUTemps', g_widgetData.getValue('GPUTempsStyle') = 'Alert');
   result.Add('HashFaults', g_widgetData.getValue('HashFaultsStyle') = 'Alert');
   result.Add('MasterAlert', result['MinersGPUs'].AsBoolean or result['HashRates'].AsBoolean or 
   								  result['GPUTemps'].AsBoolean or result['HashFaults'].AsBoolean);
end;

procedure CCollector.alertUser;
var
   waitHrs: integer;
   lastAlert: TDateTime;
begin
   if not g_settings.getBool('Alerts.enabled') then exit;
	waitHrs := g_settings.getInt('Alerts.wait');
   lastAlert := TDateTime(g_settings.getValue('Alerts.last_alert', 0.0));
   if HoursBetween(Now, lastAlert) > waitHrs then begin
      g_settings.putValue('Alerts', 'last_alert', double(Now));
      Log.Writeln(['Sending action alert'], true);
      doAlertAction;
	end else begin
      Log.Writeln(['Action alert, but we sent one recently'], true);
	end;
end;

procedure CCollector.doAlertAction(ui: boolean);
var
   http: TFPHTTPClient;
   action, s, msg: string;
   process: TProcess;
begin
   action := g_settings.getString('Alerts.action');
   if (not AnsiStartsText('http', action)) and (not FileExists(action)) then begin
      if ui then begin
			Application.MessageBox('The specified alert action could no be found', 'Mining Visualizer', MB_ICONERROR);
		end;
      exit;
	end;

   // URL
	if AnsiStartsText('http', action) then begin
		http := TFPHTTPClient.Create(nil);
      try
			s := http.Get(g_settings.getString('Alerts.action'));
         logView.LogMsg(s);
		except
	   	on e: Exception do begin
            if ui then begin
      			Application.MessageBox(PChar(e.Message), 'Mining Visualizer', MB_ICONERROR);
				end;
				Log.Writeln(['Exception in CCollector.doAlertAction : ', e.Message]);
			end;
		end;
      http.Free;
	end
   
   // Windows batch file
   else if CompareFileExt(action, 'bat', false) = 0 then begin
		{$IfDef Windows}
			ShellExecute(0, nil, PChar('cmd'), PChar('/c ' + action), nil, 1)
      {$Else}
      	msg := 'Exception in CCollector.doAlertAction : BAT file extension is invalid for this OS platform.';
         if ui then begin
   			Application.MessageBox(PChar(msg), 'Mining Visualizer', MB_ICONERROR);
			end;
			Log.Writeln([msg]);
		{$EndIf}
	end
   
   // bash script
   else if CompareFileExt(action, 'sh', false) = 0 then begin
		{$IfDef Windows}
      	msg := 'Exception in CCollector.doAlertAction : SH file extension is invalid for this OS platform.';
         if ui then begin
   			Application.MessageBox(PChar(msg), 'Mining Visualizer', MB_ICONERROR);
			end;
			Log.Writeln([msg]);
      {$Else}
			process := TProcess.Create(nil);
			process.Executable := '/bin/bash';
			process.Parameters.Add('-c');
			process.Parameters.Add(action);
			process.Execute;
			process.Free;
		{$EndIf}
	end
   
   // assume it is a binary executable
   else begin
   	process := TProcess.Create(nil);
		process.Executable := action;
      try
			process.Execute;
		except
	   	on e: Exception do begin
            if ui then begin
      			Application.MessageBox(PChar(e.Message), 'Mining Visualizer', MB_ICONERROR);
				end;
				Log.Writeln(['Exception in CCollector.doAlertAction : ', e.Message]);
			end;
		end;
      process.Free;
	end;
end;



end.

