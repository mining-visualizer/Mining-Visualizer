
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

unit uWebFace;

{$mode objfpc}{$H+}

interface

   // this module supports an interface between the application and the embedded web server.

uses
	Classes, SysUtils, uHttpServer, LCLType, syncobjs, fgl, fpjson, uHTMLDoc, ExtCtrls;

type
  
	// -------------  Class CDataRequest  --------------------

	CDataRequest = class
      Constructor Create;
      Destructor Destroy; override;
   public
		dataIDs: TStringList;
		response: CResponse;
      received: TDateTime;
	end;

	// -------------  Class CSessions  --------------------

	// this is used for session manageement / user authentication.
	CSessions = class
      Constructor Create;
      Destructor Destroy; override;
		function Check(cookies: TStringList): boolean;
      function NewSessionID: string;
      procedure RegisterSession(sessionID: string);
   private
      m_sessionList: TStringList;
   end;


	// -------------  Class CWebFace  --------------------

	CWebFace = class
   const
   	HASH_SAMPLES = 'HashSamples';
      MINER_DATA = 'MinerData';
      NETWORK_DATA = 'NetworkData';
      KEEP_ALIVE = 'KeepAlive';
      
      KEEPALIVE_TIMER_INTERVAL = 2000;		// ms
      SEND_KEEPALIVE_INTERVAL = 5;			// seconds
      WU_BUCKET_SIZE = 4;						// # of hours for one work unit histogram bar 

	type
	   THandlerFn = Procedure (uriRemaining: string; request: CRequest; response: CResponse) of object;
		TRequestHandlers = specialize TFPGMap<string, THandlerFn>;
		// map pageIDs to data requests
		TLongPollClients = specialize TFPGMapObject<string, CDataRequest>;
         
   public
      Constructor Create;
      Destructor Destroy; override;
      procedure StartServer;
      procedure StopServer;
      procedure RegisterHandlers;
      procedure LoadMimeTypes;
      function GetMimeType(filename: string): string;
     
   	procedure HttpRequest(request: CRequest; response: CResponse);
      procedure UserLogin(uriRemaining: string; request: CRequest; response: CResponse);
      function ProtectedURI(uri: string): boolean;
   	procedure HandleAjaxRequest(uriRemaining: string; request: CRequest; response: CResponse);
		procedure MiningResults(var jResults: TJSONObject);
		procedure MinerList(var jResults: TJSONArray; lite: boolean);
   	procedure HandleStreamRequest(uriRemaining: string; request: CRequest; response: CResponse);
   	procedure LongPoll(uriRemaining: string; request: CRequest; response: CResponse);
   	procedure ServeFile(request: CRequest; response: CResponse);
      procedure ServeFile(uri: string; response: CResponse);
      procedure onTimer(Sender: TObject);
      
      // register a given page as having requested a given data item
   	procedure registerPage(pageID, dataID: string);
      // save the response object to be used when there is some data to send
   	procedure holdResponse(pageID: string; response: CResponse);
      // cancel an earlier data item registration for this page.
      procedure cancelPage(pageID, dataID: string);
      // send data 'asynchronously' to any html page that has sent us the necessary long polling request.
      procedure Push(dataID: string; jData: TJSONObject);
      
		procedure onMinerConnect;
		procedure onMinerDisconnect;
		procedure onHashSample(sample: QWord; isLastSample: boolean);
      procedure onWorkUnit(miner: string; minerID, gpu: integer);
      procedure onCloseHit(miner: string; minerID, gpu: integer; closeHit: QWord);
      procedure onSolution(miner: string; minerID, gpu, block: integer);
		procedure onHashFault(miner: string; minerID, gpu: integer);
      procedure onHashRates(minerID: integer; farmRate: double; minerRates: TJSONArray);
      procedure onGPUTemps(minerID: integer; status: string; temps: TJSONArray);
      procedure onFanSpeeds(minerID: integer; speeds: TJSONArray);
      procedure onWorkPackage(target: uint64; blockNumber: uint32);
		procedure updateAlerts(jAlerts: TJSONObject);
      
      
   private
      m_httpServer: CHttpServer;
      // this is a map to track long poll clients.  the 'key' is the clients pageID.  the 'value' is a CDataRequest object.
      // the CDataRequest object simply contains a TStringList of data items the client has subscribed to, and a CResponse
      // object that will be used to send the data when it becomes available.
   	m_longPollClients: TLongPollClients;
		x_longPollClients: TCriticalSection;
      m_mimeTypes: TStringList;
      m_requestHandlers: TRequestHandlers;
      m_sessions: CSessions;
      m_requireLogin: boolean;
      m_docLogin: CHTMLDoc;
      m_keepAliveTimer: TTimer;
      m_terminating: boolean;
   
   public
      DocumentRoot: string;
      
	end;

      
// ======================== IMPLEMENTATION =============================

implementation

uses uMisc, uLog, strutils, uGlobals, dateutils, uLib;

// ------------------------------------------------------------------
// Class CDataRequest
// ------------------------------------------------------------------

constructor CDataRequest.Create;
begin
	dataIDs := TStringList.Create;
end;

destructor CDataRequest.Destroy;
begin
   dataIDs.Free;
   FreeAndNil(response);
	inherited Destroy;
end;


// ------------------------------------------------------------------
// Class CSessions
// ------------------------------------------------------------------

constructor CSessions.Create;
begin
	Randomize;
   m_sessionList := TStringList.Create;
end;

destructor CSessions.Destroy;
begin
   FreeAndNil(m_sessionList);
	inherited Destroy;
end;

function CSessions.Check(cookies: TStringList): boolean;
var
   s: string;
begin
   Log.Writeln(['Trace: CSessions.Check : cookies = ', cookies.CommaText], true);
   s := cookies.Values['session_id'];
   result := m_sessionList.IndexOf(s) <> -1;
end;

function CSessions.NewSessionID: string;
var
   i: integer;
begin
   result := '';
	for i := 1 to 16 do begin
      result := result + IntToHex(Random(15), 1);
	end;
   RegisterSession(result);
end;

procedure CSessions.RegisterSession(sessionID: string);
begin
	m_sessionList.Add(sessionID);
end;


// ------------------------------------------------------------------
// Class CWebFace
// ------------------------------------------------------------------

constructor CWebFace.Create;
var
   IDs: TStringList;
begin
   m_terminating := false;
	m_httpServer := CHttpServer.Create(g_settings.getInt('WebInterface.port'));
   m_httpServer.onRequest := @HttpRequest;
   DocumentRoot := ConcatPaths([getResourcesFolder(), 'WebApp']);
   
	m_longPollClients := TLongPollClients.Create(true);
   x_longPollClients := TCriticalSection.Create;
   loadMimeTypes;
   
   m_requestHandlers := TRequestHandlers.Create;
   registerHandlers;
   g_minerRPC.onHashSample := @onHashSample;
   m_sessions := CSessions.Create;
   m_requireLogin := g_settings.getBool('WebInterface.require_login');
   
   IDs := TStringList.Create;
   IDs.CommaText := 'AlertDisplay';
   m_docLogin := CHTMLDoc.Create(ConcatPaths([DocumentRoot, 'guest', 'login.html']), '', IDs);
   m_docLogin.setValue('AlertDisplay', 'display:none;');
   IDs.Free;
   
   m_keepAliveTimer := TTimer.Create(nil);
   m_keepAliveTimer.Enabled := true;
   m_keepAliveTimer.Interval := KEEPALIVE_TIMER_INTERVAL;
   m_keepAliveTimer.OnTimer := @onTimer;
end;

destructor CWebFace.Destroy;
begin
   Log.Writeln(['CWebFace.Destroy'], true);
   m_terminating := true;
   m_keepAliveTimer.Enabled := false;
   m_keepAliveTimer.Free;
   m_httpServer.StopServer;
   m_httpServer.Free;
   x_longPollClients.Free;
   m_longPollClients.Free;
   m_mimeTypes.Free;
   m_requestHandlers.Free;
   m_sessions.Free;
   m_docLogin.Free;
	inherited Destroy;
end;

procedure CWebFace.LoadMimeTypes;
begin
	m_mimeTypes := TStringList.Create;
   m_mimeTypes.Values['.txt'] := 	'text/plain';
   m_mimeTypes.Values['.css'] := 	'text/css';
   m_mimeTypes.Values['.csv'] := 	'text/csv';
   m_mimeTypes.Values['.html'] := 	'text/html';
   m_mimeTypes.Values['.htm'] := 	'text/html';
   m_mimeTypes.Values['.ico'] := 	'image/x-icon';
   m_mimeTypes.Values['.jar'] := 	'application/java-archive';
   m_mimeTypes.Values['.bmp'] := 	'image/bmp';
   m_mimeTypes.Values['.gif'] := 	'image/gif';
   m_mimeTypes.Values['.jpeg'] := 	'image/jpeg';
   m_mimeTypes.Values['.jpg'] := 	'image/jpeg';
   m_mimeTypes.Values['.png'] := 	'image/png';
   m_mimeTypes.Values['.tif'] := 	'image/tiff';
   m_mimeTypes.Values['.tiff'] := 	'image/tiff';
   m_mimeTypes.Values['.js'] := 		'application/javascript';
   m_mimeTypes.Values['.json'] := 	'application/json';
   m_mimeTypes.Values['.pdf'] := 	'application/pdf';
   m_mimeTypes.Values['.ttf'] := 	'font/ttf';
   m_mimeTypes.Values['.wav'] := 	'audio/x-wav';
   m_mimeTypes.Values['.xml'] := 	'application/xml';
   m_mimeTypes.Values['.zip'] := 	'application/zip';
   m_mimeTypes.Values['.7z'] := 		'application/x-7z-compressed';
   m_mimeTypes.Values['.rar'] := 	'application/x-rar-compressed';
   m_mimeTypes.Values['.tar'] := 	'application/x-tar';
end;

function CWebFace.GetMimeType(filename: string): string;
var
   ext: string;
begin
   ext := ExtractFileExt(filename);
	result := m_mimeTypes.Values[ext];
   Log.Writeln(['Trace: CWebFace.GetMimeType, filename=', filename, ', ext=', ext, ', result=', result], true);
end;

procedure CWebFace.StartServer;
begin
   m_httpServer.StartServer;
end;

procedure CWebFace.StopServer;
begin
   m_httpServer.StopServer;
end;

procedure CWebFace.RegisterHandlers;
begin
   m_requestHandlers.Add('/guest/login', @UserLogin);
	m_requestHandlers.Add('/ajax', @HandleAjaxRequest);
	m_requestHandlers.Add('/stream', @HandleStreamRequest);
	m_requestHandlers.Add('/poll', @LongPoll);
end;

function CWebFace.ProtectedURI(uri: string): boolean;
begin
   // if user login is enabled, all uri's are protected, and will be redirected
   // to the login page, except the following:
	result := not AnsiStartsStr('/plugins/', uri) and 
             not AnsiStartsStr('/guest/', uri) and
             not AnsiStartsStr('/favicon', uri);
end;

procedure CWebFace.HttpRequest(request: CRequest; response: CResponse);
var
	i: integer;
   s: string;
begin
   
   // note: this is running on the connection thread.
   
   if m_terminating then exit;
   Log.Writeln(['Trace: CWebFace.HttpRequest : uri = ', request.uri], true);
   // security check
   if Pos('..', request.uri) <> 0 then begin
   	response.resultCode := '400 Bad Request';
   	response.okToSend := true;
      exit;
	end;
   
   // if a valid session cookie is not present and login is required, send them to the login screen.
   if not m_sessions.Check(request.cookies) and m_requireLogin and ProtectedURI(request.uri) then begin
   	response.resultCode := '307 Temporary Redirect';
      response.headers.Add('Location: /guest/login');
      response.okToSend := true;
      exit;
	end;
   
   // check to see if a special handler has been registered for this uri.
	for i := 0 to m_requestHandlers.Count - 1 do begin
   	if AnsiStartsStr(m_requestHandlers.Keys[i], request.uri) then begin
         s := RightStr(request.uri, Length(request.uri) - Length(m_requestHandlers.Keys[i]));
         if IsPathDelimiter(s, 1) then
         	s := RightStr(s, Length(s) - 1);
      	m_requestHandlers.Data[i](s, request, response);
         exit;
		end;
	end;
   
   // no special handlers, so we assume it is a file request
   ServeFile(request, response);
   
end;

procedure CWebFace.UserLogin(uriRemaining: string; request: CRequest; response: CResponse);
var
   s: string;
   cookie: string;
   t: TDateTime;
begin
   Log.Writeln(['Trace: CWebFace.UserLogin : method = ', request.method], true);
   if request.method = 'GET' then begin
      m_docLogin.setValue('AlertDisplay', 'display:none;');
      m_docLogin.writeToStream(response.outputStream);
	   response.headers.Add('Content-Type: text/html');
	   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
   	response.resultCode := '200 Ok';
		response.okToSend := true;
      exit;
	end
   else if request.method = 'POST' then begin
      Log.Writeln(['Trace: CWebFace.UserLogin : parameters = ', request.parameters.CommaText], true);
	   s := request.parameters.Values['password'];
	   if (s <> '') and (s = g_settings.getValue('WebInterface.password', '')) then begin
	      cookie := 'Set-Cookie: session_id=' + m_sessions.newSessionID + '; HttpOnly; Path=/';
	      if request.parameters.Values['chkRemember'] = 'on' then begin
	         t := IncDay(Now, 60);
	         t := LocalTimeToUniversal(t);
	         cookie += '; Expires=' + FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"', t);
			end;
      	Log.Writeln(['Trace: CWebFace.UserLogin : Set-Cookie = ', cookie], true);
			response.headers.Add(cookie);
	   	response.resultCode := '303 See Other';
	      response.headers.Add('Location: /miners.html');
	      response.okToSend := true;
	   end
	   else begin
	      m_docLogin.setValue('AlertDisplay', 'display:block;');
	      m_docLogin.writeToStream(response.outputStream);
		   response.headers.Add('Content-Type: text/html');
		   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
   		response.resultCode := '200 Ok';
			response.okToSend := true;
	      exit;
		end;
	end
   else begin
	   response.resultCode := '400 Bad Request';
		response.okToSend := true;
      exit;
	end;

end;

procedure CWebFace.HandleAjaxRequest(uriRemaining: string; request: CRequest; response: CResponse);
var
	jObject, jResults: TJSONObject;
   jData: TJSONArray;
   target: QWord;
   
   procedure finalizeResponse(jSend: TJSONData);
   begin
	   response.okToSend := true;
	   jSend.DumpJSON(response.outputStream);
	   response.headers.Add('Content-Type: application/json');
	   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
	   response.resultCode := '200 OK';
	end;

begin		// HandleAjaxRequest
   
   if uriRemaining = 'test' then begin
	   jObject := TJSONObject.Create(['id', 7, 'text', FormatDateTime('hh:mm:ss', Now)]);
      finalizeResponse(jObject);
	end 
   
   else if uriRemaining = 'mining_results' then begin
      // this is called from barplot.html
      
      MiningResults(jResults);
      finalizeResponse(jResults);
      jResults.Free;
	end
   
   else if uriRemaining = 'miner_list' then begin
      // this is called from miners.html

      MinerList(jData, false);
      jObject := TJSONObject.Create;
      jObject.Add('miners', jData);
      jObject.Add('alerts', g_collector.CheckAlerts);
		finalizeResponse(jObject);
      jObject.Free;
	end
   
   else if uriRemaining = 'scatter_init' then begin
      // this is called from scatterplot.html
      MinerList(jData, true);
   	g_widgetData.getValue('Target', target);
      jObject := TJSONObject.Create;
      jObject.Add('miners', jData);
      jObject.Add('target', target / High(QWord));
		finalizeResponse(jObject);
      jObject.Free;
	end;

end;		// HandleAjaxRequest

procedure CWebFace.MiningResults(var jResults: TJSONObject);
var
	jObject, jElement: TJSONObject;
   jArray, jData: TJSONArray;
   i, expectedWorkUnits: integer;
   currentBucket, bucketID, bucketCount: integer;
   aDate: TDateTime;
begin
   try
      g_miningData.beginRead;
      
		// close hits
		jData := g_miningData.getSeries('CloseHits');
		jResults := TJSONObject.Create;
		jArray := TJSONArray.Create;
		for i := 0 to jData.Count - 1 do begin
		   jElement := TJSONObject(jData[i]);
		   aDate := ToDateTime(jElement.Strings['date']);
		   jObject := TJSONObject.Create;
		   jObject.Add('x', DateTimeToUnix(LocalTimeToUniversal(aDate)) * 1000);
		   jObject.Add('y', jElement.Integers['percent']);
		   jObject.Add('miner', jElement.strings['miner']);
		   jObject.Add('miner_id', jElement.Integers['miner_id']);
		   jObject.Add('gpu', jElement.Integers['gpu'] + 1);
		   jObject.Add('close_hit', jElement.QWords['close_hit']);
		   jArray.Add(jObject);
		end;
		jResults.Add('CloseHits', jArray);

		// solutions
		jData := g_miningData.getSeries('Solutions');
		jArray := TJSONArray.Create;
		for i := 0 to jData.Count - 1 do begin
		   jElement := TJSONObject(jData[i]);
		   aDate := ToDateTime(jElement.Strings['date']);
		   jObject := TJSONObject.Create;
		   jObject.Add('x', DateTimeToUnix(LocalTimeToUniversal(aDate)) * 1000);
		   jObject.Add('y', 100);
		   jObject.Add('miner', jElement.strings['miner']);
		   jObject.Add('miner_id', jElement.Integers['miner_id']);
		   jObject.Add('gpu', jElement.Integers['gpu'] + 1);
		   jObject.Add('block', jElement.Int64s['block']);
		   jArray.Add(jObject);
		end;
		jResults.Add('Solutions', jArray);

		// work units. group them into 4 hour buckets to be displayed on a histogram.
		jData := g_miningData.getSeries('WorkUnits');
		jArray := TJSONArray.Create;
		// # of work units we're expecting per time period
		currentBucket := -1;
		jObject := nil;
		bucketCount := 0;
		for i := 0 to jData.Count - 1 do begin
		   jElement := TJSONObject(jData[i]);
		   aDate := ToDateTime(jElement.Strings['date']);
		   bucketID := HourOfTheDay(aDate) div WU_BUCKET_SIZE * WU_BUCKET_SIZE + 2;
		   if bucketID <> currentBucket then begin
		      if Assigned(jObject) then begin
		         jObject.Add('y', bucketCount);
		         jArray.Add(jObject);
				end;
				jObject := TJSONObject.Create;
		      // calculate the center point of the 4 hour bucket.
		      aDate := RecodeDateTime(aDate, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, bucketID, 0, 0, 0);
		   	jObject.Add('x', DateTimeToUnix(LocalTimeToUniversal(aDate)) * 1000);
		      bucketCount := 0;
		      currentBucket := bucketID;
			end;
			inc(bucketCount);
		end;
      
	finally
      g_miningData.endRead;
	end;
   
	if Assigned(jObject) then begin
	   jObject.Add('y', bucketCount);
	   jArray.Add(jObject);
	end;
	expectedWorkUnits := (WU_BUCKET_SIZE * 60 * 60) div g_settings.getInt('CloseHits.workunit_frequency');
   expectedWorkUnits *= g_miners.totalGPUs;
	jResults.Add('WorkUnits', TJSONObject.Create(['ExpectedWorkUnits', expectedWorkUnits, 'data', jArray]));
end;

procedure CWebFace.MinerList(var jResults: TJSONArray; lite: boolean);
var
	jObject, jWorkUnits, jHashFaults: TJSONObject;
   i, minerID, x: integer;
begin
   g_miningData.aggregateSeries('WorkUnits', 24, jWorkUnits, x);
   g_miningData.aggregateSeries('HashFaults', 24, jHashFaults, x);
   
   // load miner info from the .json file
	jResults := TJSONArray(g_settings.getValue('Miners', TJSONArray));
   for i := 0 to jResults.count - 1 do begin
   	jObject := jResults.Objects[i];
      minerID := jObject['id'].AsInteger;
      
		// besides the fields in the .json file, we're going to add other data as well
      jObject.Add('miner_id', minerID);  // just for the convenience of the web page
      if not g_miners.minerIsActive(minerID) then continue;
   	with g_miners.byID[minerID] do begin
         jObject.Add('online', online);
         if not online then continue;
      	jObject.Add('gpus_reported', gpuCountReported);
         if lite then continue;
	      // hash rate data
	      jObject.Add('miner_rate', hashRate);
         jObject.Add('gpu_rates', gpuRates);
         // gpu temps
         jObject.Add('temp_status', getGPUTempStatusStr);
         jObject.Add('gpu_temps', gpuTemps);
         // fan speeds
         jObject.Add('fan_speeds', fanSpeeds);
         // work units
         jObject.Add('work_units', jWorkUnits.Arrays[IntToStr(minerID)].Clone);
         // hash faults
         jObject.Add('hash_faults', jHashFaults.Arrays[IntToStr(minerID)].Clone);
		end;
	end;
   jWorkUnits.Free;
   jHashFaults.Free;
end;


procedure CWebFace.HandleStreamRequest(uriRemaining: string; request: CRequest; response: CResponse);
var
	pageID, s: string;
	params, streams: TStringList;
	minerID, gpu, rate: integer;
  
	procedure errorResponse(errMsg: string);
	var
		jMsg: TJSONObject;
	begin
		jMsg := TJSONObject.Create(['message', errMsg]);
	   jMsg.DumpJSON(response.outputStream);
	   jMsg.Free;
	   response.headers.Add('Content-Type: application/json');
	   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
	   response.resultCode := '400 Bad Request';
	end;	// errorResponse

begin	// HandleStreamRequest
   try
	   response.resultCode := '204 No Content';
   	response.okToSend := true;
      
      params := request.parameters;
      s := params.Values['streams'];
      streams := Split(s, '|');
   	Log.Writeln(['Trace: CWebFace.HandleStreamRequest, streams=', s], true);
      
	   if request.method = 'GET' then begin
         pageID := request.headers.Values['X-MVis-PageID'];
         if pageID = '' then begin
            errorResponse('Exception in CWebFace.HandleStreamRequest : Missing X-MVis-PageID header');
            exit;
			end;

         for s in streams do begin
	         // register this data request under the given pageID.
	         RegisterPage(pageID, s);
            
            if s = HASH_SAMPLES then begin
            	// special handling for hash samples
		         minerID := StrToInt(params.Values['minerID']);
               if not g_miners.minerExists(minerID) then begin
		            errorResponse('Please specify a valid Miner ID');
		            exit;
					end;
		         gpu := StrToInt(params.Values['gpu']);
		         rate := StrToInt(params.Values['rate']);
		         // check input data for errors
		         if (gpu < 0) or (gpu > g_miners.byID[minerID].gpuCountReported - 1) then begin
		            errorResponse('Please specify a valid GPU #');
		            exit;
					end;
					// request hash samples from the minerID
		         with g_miners.byID[minerID] do
						g_minerRPC.requestHashSamplesNotify(ip, port, gpu, rate);
            end;
			end;
		end
		else if request.method = 'DELETE' then begin
	      // retrieve pageID
	      pageID := request.headers.Values['X-MVis-PageID'];
	      if pageID = '' then
	         errorResponse('Exception in CWebFace.HandleHashSamples : Missing X-MVis-PageID header')
         else begin
         	for s in streams do
	      		cancelPage(pageID, s);
			end;
		end;
      streams.Free;
      
	except
		on e: Exception do begin
   		errorResponse(e.Message);
		end;
	end;

end;



// ------------------------------------------------------------------
// long polling code
// ------------------------------------------------------------------

// register a given page as having requested a specific data stream.
procedure CWebFace.registerPage(pageID, dataID: string);
var
   dataRequest: CDataRequest;
   list: TStringList;
begin
   try
		x_longPollClients.Acquire;
		if m_longPollClients.IndexOf(pageID) = -1 then begin
	      dataRequest := CDataRequest.Create;
	      m_longPollClients.Add(pageID, dataRequest);
		end else
	   	dataRequest := m_longPollClients[pageID];

	   list := dataRequest.dataIDs;
	   if list.IndexOf(dataID) = -1 then
	      list.Add(dataID);
	finally
     	x_longPollClients.Release;
	end;
end;

// save the response object to be used when there is some data to send
procedure CWebFace.holdResponse(pageID: string; response: CResponse);
var
   dataRequest: CDataRequest;
begin
   try
		x_longPollClients.Acquire;
		if m_longPollClients.IndexOf(pageID) = -1 then begin
	      dataRequest := CDataRequest.Create;
	      m_longPollClients.Add(pageID, dataRequest);
		end else
	   	dataRequest := m_longPollClients[pageID];

	   if Assigned(dataRequest.response) then
	      dataRequest.response.Free;
	   dataRequest.response := response;
	   dataRequest.received := Now;
	finally
     	x_longPollClients.Release;
	end;
end;

// cancel the specified page's subscription to the data stream specified by dataID.
procedure CWebFace.cancelPage(pageID, dataID: string);
var
   msg: string;
   list: TStringList;
begin
   try
	   try
			x_longPollClients.Acquire;
	      list := m_longPollClients[pageID].dataIDs;
	      list.Delete(list.IndexOf(dataID));
		finally
	     	x_longPollClients.Release;
	   end;
	except
      msg := 'Exception in CWebFace.cancelPage : pageID=' + pageID + ', dataID="' + dataID;
      {$ifdef debug}
      MessageBox2(PChar(msg), 'Error', MB_ICONERROR);
      {$else}
      Log.Writeln([msg]);
      {$endif}
	end;
end;

procedure CWebFace.LongPoll(uriRemaining: string; request: CRequest; response: CResponse);
// the html page has sent a long polling request. we'll hang on to it until we have some data to send.
var
	pushResponse: CResponse;
   pageID: string;
	jMsg: TJSONObject;
begin
   // when we receive an agreed upon uri, we delay sending the response until we actually have
   // some data.  this is commonly referred to as long polling.  the expectation is that the page,
   // upon finally receiving the response, would immediately send another similar request.
   
   Log.Writeln(['Trace: CWebFace.LongPoll'], true);
   // retrieve pageID
   pageID := request.headers.Values['X-MVis-PageID'];
   if pageID = '' then begin
		jMsg := TJSONObject.Create(['message', 'Missing X-MVis-PageID header']);
      jMsg.DumpJSON(response.outputStream);
      jMsg.Free;
      response.headers.Add('Content-Type: application/json');
      response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
	   response.resultCode := '400 Bad Request';
   	response.okToSend := true;
      exit;
	end;

	try
      x_longPollClients.Acquire;
      // we're not going to send a response until we have some data. see the Push method below.
      response.okToSend := false;
      // create a response object, to be used later when some data becomes available.
      pushResponse := CResponse.Create(response.connection);
      pushResponse.protocol := request.protocol;
      holdResponse(pageID, pushResponse);
	finally
     	x_longPollClients.Release;
	end;
end;

procedure CWebFace.onMinerConnect;
begin
   Log.Writeln(['Trace: CWebFace.onMinerConnect'], true);
	Push(MINER_DATA, TJSONObject.Create(['data_id', MINER_DATA, 'sub_type', 'miner_connect']));
end;

procedure CWebFace.onMinerDisconnect;
begin
   Log.Writeln(['Trace: CWebFace.onMinerDisconnect'], true);
	Push(MINER_DATA, TJSONObject.Create(['data_id', MINER_DATA, 'sub_type', 'miner_disconnect']));
end;

// a hash sample has arrived from a miner -- push it out to the browser.
procedure CWebFace.onHashSample(sample: QWord; isLastSample: boolean);
var
   x: double;
begin
   Log.Writeln(['Trace: CWebFace.onHashSample'], true);
   x := sample / High(QWord);
	Push(HASH_SAMPLES, TJSONObject.Create(['data_id', HASH_SAMPLES, 'sample', x, 'is_last_sample', isLastSample]));
end;

procedure CWebFace.onWorkUnit(miner: string; minerID, gpu: integer);
begin
	Push(
   	MINER_DATA,
      TJSONObject.Create([
      	'data_id', MINER_DATA,
         'sub_type', 'work_unit',
         'miner_id', minerID,
         'gpu', gpu
   ]));
end;

procedure CWebFace.onHashFault(miner: string; minerID, gpu: integer);
begin
	Push(
   	MINER_DATA,
      TJSONObject.Create([
      	'data_id', MINER_DATA,
         'sub_type', 'hash_fault',
         'miner_id', minerID,
         'gpu', gpu
   ]));
end;

procedure CWebFace.onCloseHit(miner: string; minerID, gpu: integer; closeHit: QWord);
var
   jObject: TJSONObject;
   target: QWord;
begin
   g_widgetData.getValue('Target', target);
	jObject := TJSONObject.Create;
   jObject.Add('data_id', MINER_DATA);
   jObject.Add('sub_type', 'close_hit');
   jObject.Add('percent', g_miningData.scaleCloseHit(closeHit, target, g_settings.getInt('CloseHits.widget_scale')));
   jObject.Add('close_hit', closeHit);
   jObject.Add('miner', miner);
   jObject.Add('miner_id', minerID);
   jObject.Add('gpu', gpu + 1);
   Push(MINER_DATA, jObject);
end;

procedure CWebFace.onSolution(miner: string; minerID, gpu, block: integer);
var
   jObject: TJSONObject;
begin
	jObject := TJSONObject.Create;
   jObject.Add('data_id', MINER_DATA);
   jObject.Add('sub_type', 'solution');
   jObject.Add('miner', miner);
   jObject.Add('miner_id', minerID);
   jObject.Add('gpu', gpu + 1);
   jObject.Add('block', block);
   Push(MINER_DATA, jObject);
end;

procedure CWebFace.onHashRates(minerID: integer; farmRate: double; minerRates: TJSONArray);
begin
	// all rates are in kH/s
   // Push is going to free the json object we are sending it, but we are responsible to 
   // free minerRates, which is why we clone it below
   Push(MINER_DATA, TJSONObject.Create([
   			'data_id', MINER_DATA,
            'sub_type', 'hash_rates',
            'miner_id', minerID,
            'miner_rate', farmRate,
            'gpu_rates', minerRates.Clone 
   ]));
   minerRates.Free;
end;

procedure CWebFace.onGPUTemps(minerID: integer; status: string; temps: TJSONArray);
begin
   // Push is going to free the json object we are sending it, but we are responsible to
   // free temps, which is why we clone it below
   Push(MINER_DATA, TJSONObject.Create([
   			'data_id', MINER_DATA,
            'sub_type', 'gpu_temps',
            'miner_id', minerID,
            'temp_status', status,
            'gpu_temps', temps.Clone
   ]));
   temps.Free;
end;

procedure CWebFace.onFanSpeeds(minerID: integer; speeds: TJSONArray);
begin
   // Push is going to free the json object we are sending it, and our caller is responsible to
   // free speeds, which is why we clone it below
   Push(MINER_DATA, TJSONObject.Create([
   			'data_id', MINER_DATA,
            'sub_type', 'fan_speeds',
            'miner_id', minerID,
            'fan_speeds', speeds.Clone
   ]));
end;

procedure CWebFace.onWorkPackage(target: uint64; blockNumber: uint32);
begin
   Push(NETWORK_DATA, TJSONObject.Create([
   			'data_id', NETWORK_DATA,
            'sub_type', 'work_package',
            'target', int64(target),
            'block_number', blockNumber
   ]));
end;

procedure CWebFace.onTimer(Sender: TObject);
begin
   // send a keep-alive to any connection that we haven't sent any data to in the last 10 seconds.
	Push(KEEP_ALIVE, TJSONObject.Create(['data_id', KEEP_ALIVE]));
end;

procedure CWebFace.updateAlerts(jAlerts: TJSONObject);
begin
   Log.Writeln(['Trace: CWebFace.updateAlerts, jAlerts = ', jAlerts.AsJSON], true);
   Push(MINER_DATA, TJSONObject.Create([
   			'data_id', MINER_DATA,
            'sub_type', 'alerts',
            'alerts', jAlerts
   ]));
end;

procedure CWebFace.Push(dataID: string; jData: TJSONObject);
var
	i: integer;
	pushResponse: CResponse;
   send: boolean;
   dataRequest: CDataRequest;
begin
   // we are responsible for freeing jData
   try
      try
			x_longPollClients.Acquire;
			if not Assigned(m_httpServer) or m_httpServer.Stopped then exit;
	      Log.Writeln(['Trace: CWebFace.Push : ' + jData.AsJSON], true);

	      for i := 0 to m_longPollClients.Count - 1 do begin
	   		dataRequest := m_longPollClients.Data[i];
	         pushResponse := dataRequest.response;

	         // has this page provided us with a data polling request?
	         if not Assigned(pushResponse) then
	            continue;

	      	// make sure the connection is still open
	         if not m_httpServer.goodConnection(pushResponse.connection) then begin
	         	FreeAndNil(dataRequest.response);
	            continue;
	         end;

	         // send if this page has specifically requested this data item, or the client needs a keep-alive.  the
	         // client's long polling requests are programmed to timeout after 10 seconds, so if we have not sent
	         // any data in the last 5 seconds we send a keep-alive.
	         send := (dataRequest.dataIDs.IndexOf(dataID) <> -1) or
	         		  ((dataID = KEEP_ALIVE) and (SecondsBetween(Now, dataRequest.received) > SEND_KEEPALIVE_INTERVAL));
	         if not send then continue;

	         // we're good to send. fill the response object with the necessary data.
	         jData.DumpJSON(pushResponse.outputStream);
	   		pushResponse.headers.Add('Content-Type: application/json');
	   	   pushResponse.headers.Add('Content-Length: ' + IntTostr(pushResponse.outputStream.Size));
	   	   pushResponse.headers.Add('Cache-Control: no-cache, no-store, must-revalidate');
	   	   pushResponse.resultCode := '200 OK';
	   	   pushResponse.okToSend := true;
            try
               // connection might close on us at the last second.
					pushResponse.connection.Push(pushResponse);
				except
				end;
	         FreeAndNil(dataRequest.response);
	   	end;
		except
	   	on e: Exception do
	      	Log.Writeln(['Exception in CWebFace.Push : ', e.Message, ', dataID=', dataID, ', jData=', jData.AsJSON]);
		end;
      
	finally
     	x_longPollClients.Release;
      jData.Free;
	end;
end;

procedure CWebFace.ServeFile(request: CRequest; response: CResponse);
begin
   ServeFile(request.uri, response);
end;

procedure CWebFace.ServeFile(uri: string; response: CResponse);
var
	l: TStringlist;
	filename: string;
begin
   // set up a default
   if uri = '/' then
      uri := '/miners.html';
   
	filename := ConcatPaths([DocumentRoot, uri]);
   if FileExists(filename) then begin
   	response.outputStream.LoadFromFile(filename);
	   response.headers.Add('Content-Type: ' + getMimeType(filename));
	   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
	   response.resultCode := '200 OK';
   	response.okToSend := true;
      exit;
	end;

   // 404 Not Found
	response.resultCode := '404 Not Found';
   l := TStringList.Create;
	l.Add('<html><body>');
	l.Add('Not Found (404) : ' + uri);
	l.Add('</body></html>');
	l.SaveToStream(response.outputStream);
	l.free;
   response.headers.Add('Content-Type: text/html');
   response.headers.Add('Content-Length: ' + IntTostr(response.outputStream.Size));
	response.okToSend := true;
end;



end.

