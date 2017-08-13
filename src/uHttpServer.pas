
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

unit uHttpServer;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, blcksock, synsock, synautil, synacode, contnrs, LCLProc, LCLType, syncobjs;

type

   CResponse = class;
   CRequest = class;
   CConnection = class;
   
   TOnRequestFn = Procedure(request: CRequest; response: CResponse) of object;
   
   
	// -------------  Class CHttpServer  --------------------

   {
   	This class runs in a worker thread (not a TThread though).  it can be started and stopped
      at will.  it waits for an incoming request, and instantiates a CConnection class to handle 
      that request and all future requests from that same client, until the connection is closed.
      The connections also runs in a worker thread. Multiple connections are supported, although
      only a few are expected, considering the role this embedded server is designed to fulfill.
   }

   CHttpServer = class
   type
   	TThreadState = (TS_Started, TS_Stopping, TS_Stopped);
   private
      procedure ThreadProc;
		function getNextConnID: integer;
      procedure cleanUpConnections;
      
   private
      m_socket: TTCPBlockSocket;
      m_port: word;
      m_threadState: TThreadState;
      m_onRequestFn: TOnRequestFn;
      // each new incoming connection results in the creation of a CConnection object that is added to 
      // this list.  we check all the connections regularly to determine if they have been closed 
      // and the worker thread is finished, in which case we free the CConnection and delete its 
      // reference from this list.
      m_connections: TObjectList;
      x_connections: TCriticalSection;
      m_nextConnID: integer;

      
   public
      Constructor Create(port: word);
      Destructor Destroy; override;
      procedure StartServer;
      procedure StopServer;
      function Stopped: boolean;
		function GoodConnection(connection: CConnection): boolean;

	public
		useSSL: boolean;
		CACertBundle: string;
      ServerCert: string;
      ServerKey: string;
      
   public
      property socket: TTCPBlockSocket read m_socket;
      // callback address to call when a new request comes in. the handler should parse the request and prepare
      // a CResponse object to be sent back to the client.
      property onRequest: TOnRequestFn write m_onRequestFn;
      // connections use this to assign themselves a simple id to be able
      // to differentiate one connection from another.
      property nextConnID: integer read getNextConnID;
      
   end;

   
	// -------------  Class CConnection  --------------------

   { 
   	Each incoming connection results in the instantiation of a CConnection class, which runs
      in its own worker thread.  Connections support long polling: specific, agreed upon uri's are
      not responded to immediately, but are instead held until there is some actual data to respond
      with.
   }
   
   CConnection = class(TThread)
	   public
	      Constructor Create(socketHandle: TSocket; server: CHttpServer);
	      Destructor Destroy; override;
	      procedure Execute; override;
         function CheckSSL: boolean;
         procedure Close;
         // send an 'asynchronous' response to a previously received request (long polling).
         procedure Push(response: CResponse);
         
      private
      	m_server: CHttpServer;
		   m_request: CRequest;
		   m_response: CResponse;
         
	   public
         shouldClose: boolean;
         socket: TTCPBlockSocket;
         
         // this is used to keep track of connections: at present used only for debugging / tracing
         // purposes. it is generally a concatenation of the IP, Port, and an incrementing number.
         id: string;
         
   end;

   
	// -------------  Class CRequest  --------------------
   
   CRequest = class
	   public
		   Constructor Create(connection: CConnection);
	      Destructor Destroy; override;
         procedure Clear;
         procedure Parse(timeOut: integer);
         
      public 
      	lastError: integer;
         valid: boolean;
      	method: string;		// GET, POST, etc.
         uri: string;
         parameters: TStringList;
         cookies: TStringList;
         protocol: string;
         host: string;
         port: integer;
         headers: TStringList;
         
   	private
	      m_socket: TTCPBlockSocket;
         m_connection: CConnection;
   		m_inputStream: TMemoryStream;
         
	end;


	// -------------  Class CResponse  --------------------

   CResponse = class
	   public
		   Constructor Create(conn: CConnection);
	      Destructor Destroy; override;
         procedure Clear;
         procedure Finalize;
			procedure Send(shouldClose: boolean);

      public
         connection: CConnection;
         protocol: string;
         requestUri: string;
      	lastError: integer;
         resultCode: string;
         headers: TStringList;
         okToSend: boolean;
   		outputStream: TMemoryStream;

	end;

         
// ======================== IMPLEMENTATION =============================

implementation

uses dateutils, uLog, ssl_openssl, uLib, uGlobals, uMisc, strutils;

function LaunchServerThread(parameter: pointer):PtrInt;
begin
   CHttpServer(parameter).ThreadProc;
   result := 0;
end;


// ------------------------------------------------------------------
// Class CHttpServer
// ------------------------------------------------------------------

constructor CHttpServer.Create(port: word);
begin
   m_port := port;
   m_socket := nil;
   m_onRequestFn := nil;
   m_connections := TObjectList.Create(false);
   x_connections := TCriticalSection.Create;
   m_nextConnID := 1;
   m_threadState := TS_Stopped;
end;

destructor CHttpServer.Destroy;
begin
   Log.Writeln(['Trace: CHttpServer.Destroy'], true);
   StopServer;
   m_connections.Free;
   x_connections.Free;
   inherited Destroy;
end;

procedure CHttpServer.StartServer;
var
   i: integer;
   errMsg: string;
begin
   if m_threadState <> TS_Stopped then exit;
   useSSL := g_settings.getBool('WebInterface.use_ssl');
   if useSSL then begin
      for i := 0 to 0 do begin
	      CACertBundle := g_settings.getValue('WebInterface.ca_cert_bundle', '');
         errMsg := 'Unable to load certificate file : "' + CACertBundle + '".';
         if not FileExists(CACertBundle) then break;
	      ServerCert :=g_settings.getValue('WebInterface.server_cert', '');
         errMsg := 'Unable to load certificate file : "' + ServerCert + '".';
         if not FileExists(ServerCert) then break;
	      ServerKey :=g_settings.getValue('WebInterface.server_key', '');
         errMsg := 'Unable to load certificate file : "' + ServerKey + '".';
         if not FileExists(ServerKey) then break;
         errMsg := '';
		end;
      if errMsg <> '' then begin
			useSSL := false;
         MessageBox2(PChar('Error in CHttpServer.StartServer: ' + errMsg + '  SSL will be disabled'), 'SSL Error', MB_ICONERROR);
		end;
	end;

   m_threadState := TS_Started;
   // BeginThread can't launch a class method, only a procedure, so we have to 
   // do a bit of indirection.
   // ToDo: use TThread.ExecuteInThread. it allows you to invoke a class method in a separate thread.
   BeginThread(@LaunchServerThread, self);
end;

procedure CHttpServer.StopServer;
var
   i: integer;
   conn: CConnection;
begin
   Log.Writeln(['Trace: CHttpServer.StopServer'], true);
   if m_threadState = TS_Stopped then exit;
   
   // signal thread to stop
   m_threadState := TS_Stopping;
   While m_threadState <> TS_Stopped do
      Sleep(10);
	FreeAndNil(m_socket);
   
   try
		// close connections
      x_connections.Acquire;
	   Log.Writeln(['Trace: Closing ' + IntToStr(m_connections.Count) + ' connections'], true);
	   for i := 0 to m_connections.Count - 1 do begin
	      conn := CConnection(m_connections[i]);
	      if conn.Finished then
	         conn.Free
	      else begin
				conn.FreeOnTerminate := true;
				conn.Close;
			end;
		end;
	finally
      x_connections.Release;
	end;
end;

function CHttpServer.Stopped: boolean;
begin
   result := m_threadState = TS_Stopped;
end;

//  this is the actual worker thread for the server.
procedure CHttpServer.ThreadProc;
var
   ClientSock : TSocket;
   conn: CConnection;
begin
   Log.Writeln(['Trace: CHttpServer.ThreadProc : In'], true);
   m_socket := TTCPBlockSocket.create;
   with m_socket do begin
      CreateSocket;
      setLinger(true, 10000);
      EnableReuse(true);
      bind('0.0.0.0', IntToStr(m_port));
      Log.Writeln(['Trace: bind lasterror = ' + m_socket.LastErrorDesc + '(' + IntToStr(lastError) + ')'], true);
      listen;
      repeat
         // the server is being asked to stop. exit the thread.
      	if m_threadState = TS_Stopping then break;
         if canread(200) then begin
            ClientSock := accept;
            if lastError = 0 then begin
               // instantiate a CConnection to handle the life of this connection
               conn := CConnection.Create(ClientSock, self);
				   try
				      x_connections.Acquire;
						m_connections.Add(conn);
					finally
      				x_connections.Release;
					end;
               // use this as a convenient time to periodically free any closed connections
               cleanUpConnections;
				end;
			end;
      until false;
   end;
   m_threadState := TS_Stopped;
   Log.Writeln(['Trace: CHttpServer.ThreadProc [Out]'], true);
end;

function CHttpServer.getNextConnID: integer;
begin
	result := m_nextConnID;
   Inc(m_nextConnID);
end;

function CHttpServer.GoodConnection(connection: CConnection): boolean;
begin
   try
      x_connections.Acquire;
		result := (m_connections.IndexOf(connection) <> -1) and (not connection.Finished);
	finally
      x_connections.Release;
	end;
end;

procedure CHttpServer.cleanUpConnections;
// free & delete any connections that have closed.
var
   i: integer;
   conn: CConnection;
   msg: string;
begin
   try
      x_connections.Acquire;
		msg := '';
	   for i := m_connections.Count - 1 downto 0 do begin
	      conn := CConnection(m_connections[i]);
	      if conn.Finished then begin
	         msg := msg + 'Freeing connection, id = ' + conn.id;
	         conn.Free;
	         m_connections.Delete(i);
			end;
		end;
	   if msg <> '' then
	      Log.Writeln(['Trace: CHttpServer.cleanUpConnections - ', msg], true);
	finally
      x_connections.Release;
	end;
end;


// ------------------------------------------------------------------
//	Class CConnection
// ------------------------------------------------------------------

constructor CConnection.Create(socketHandle: TSocket; server: CHttpServer);
begin
   socket := TTCPBlockSocket.Create;
   socket.socket := socketHandle;
   Priority := tpNormal;
   m_server := server;
   with server.socket do
   	id := GetRemoteSinIP + ':' + IntToStr(GetRemoteSinPort) + ', #' + IntToStr(server.nextConnID);
   m_request := CRequest.Create(self);
   m_response := CResponse.Create(self);
	Log.Writeln(['Trace: CConnection.Create, id = ' + id], true);
   inherited create(false);
end;

destructor CConnection.Destroy;
begin
   Log.Writeln(['Trace: CConnection.Destroy,  id = ' + id], true);
   socket.Free;
   m_request.Free;
   m_response.Free;
   inherited Destroy;
end;

// this is the worker thread for the connection.
procedure CConnection.Execute;
var
   closeConnectionAt: TDateTime;
begin
   Log.Writeln(['Trace: CConnection.Execute [In], id = ' + id], true);
   try
      if not CheckSSL then exit;
	   repeat
	      // close the connection after specified amount of inactivity.
	      closeConnectionAt := IncSecond(Now, 120);
	      m_request.Clear;
	      m_response.Clear;
	      repeat
	         // wait for a request.
				m_request.Parse(200);
			until (m_request.LastError <> WSAETIMEDOUT) or m_server.Stopped or (Now > closeConnectionAt) or Terminated;
	      if (m_request.LastError <> 0) or (not m_request.valid) or Terminated then
	         break;
	      
         m_response.requestUri := m_request.uri;
	      if Assigned(m_server.m_onRequestFn) then begin
            try
					// call the Request Handler
		      	m_server.m_onRequestFn(m_request, m_response);
		         if m_response.okToSend then begin
			            // send the response.
		               m_response.protocol := m_request.protocol;
			            m_response.Send(shouldClose);
					      if socket.lasterror <> 0 then
					         break;
				      if shouldClose then
				         break;
			      end;
				except
			   	on e: Exception do
						Log.Writeln(['Exception in CConnection.Execute.  ', 
                  				 'HTTP Request handler generated the following exception : ', e.Message]);
				end;
			end;
	   until socket.LastError <> 0;
	finally
	   Log.Writeln(['Trace: CConnection.Execute [Out], id = ' + id], true);
	end;
end;

function CConnection.CheckSSL: boolean;
var
   b: byte;
   isHttp: boolean;
   msg: string;
begin
   result := false;
   b := socket.PeekByte(50);
   // the first byte of an SSL/TLS request is always 22.  For HTTP, it could be any ansi character.
   isHttp := (Ord(b) > 32) and (Ord(b) < 127);
   if isHttp and m_server.useSSL then begin
      // redirect client to the HTTPS version of this URI.
      m_request.Parse(50);
      if m_request.lastError <> 0 then begin
      	Log.Writeln(['Error in CConnection.Execute: Expecting SSL, got HTTP; could not parse request (' + self.socket.GetErrorDescEx + ')']);
         exit;
      end;
      Log.Writeln(['Expecting HTTPS, got HTTP. URI = ', m_request.uri]);
	   m_response.headers.Add('content-type: text/plain');
	   m_response.headers.Add('content-length: 0');
      m_response.headers.Add('Location: https://' + m_request.host + ':' + IntToStr(m_request.port) + m_request.uri);
	   m_response.resultCode := '307 Temporary Redirect';
      m_response.protocol := m_request.protocol;
      m_response.Send(true);
      exit;
	end;

   if (not m_server.useSSL) and (Ord(b) = 22) then begin
      Log.Writeln(['Expecting HTTP, got HTTPS']);
	   m_response.headers.Add('content-type: text/plain');
	   m_response.headers.Add('content-length: 0');
	   m_response.resultCode := '400 Bad Request';
      m_response.protocol := m_request.protocol;
      m_response.Send(true);
      exit;
	end;

	if Ord(b) = 22 then begin	// TLS connections always start with this byte
		socket.SSL.CertCAFile := m_server.CACertBundle;
		socket.SSL.CertificateFile := m_server.ServerCert;
		socket.SSL.PrivateKeyFile := m_server.ServerKey;

		if (not socket.SSLAcceptConnection) or (socket.SSL.LastError <> 0) then begin
         msg := 'Error while accepting SSL connection: ' + socket.SSL.LastErrorDesc;
			Log.Writeln([msg]);
		   m_response.headers.Add('content-type: text/plain');
         m_response.outputStream.WriteAnsiString(msg);
		   m_response.headers.Add('content-length: ' + IntTostr(m_response.outputStream.Size));
		   m_response.resultCode := '400 Bad Request';
	      m_response.protocol := m_request.protocol;
	      m_response.Send(true);
			exit;
		end;
	end;
	result := true;
end;

procedure CConnection.Close;
begin
   Log.Writeln(['Trace: CConnection.Close, id = ' + id], true);
	Terminate;
   While not Finished do
      Sleep(10);
end;

// send an asynchronous response to a previously received request (long polling).
procedure CConnection.Push(response: CResponse);
var
   n: integer;
begin
   Log.Writeln(['Trace: CConnection.Push'], true);
	response.Finalize;
	socket.SendString(response.protocol + ' ' + response.resultCode + CRLF);
	for n := 0 to response.headers.count - 1 do
	   socket.sendstring(response.headers[n] + CRLF);
	
	if socket.lasterror <> 0 then
	   exit;
	socket.SendBuffer(response.outputStream.Memory, response.outputStream.Size);
end;


// ------------------------------------------------------------------
//	Class CRequest
// ------------------------------------------------------------------

constructor CRequest.Create(connection: CConnection);
begin
   m_connection := connection;
	m_socket := connection.socket;
   m_inputStream := TMemoryStream.Create;
   headers := TStringList.Create;
   parameters := TStringList.Create;
   cookies := TStringList.Create;
end;

destructor CRequest.Destroy;
begin
   FreeAndNil(parameters);
   FreeAndNil(headers);
   FreeAndNil(m_inputStream);
   FreeAndNil(cookies);
	inherited Destroy;
end;

procedure CRequest.Clear;
begin
	headers.Clear;
   parameters.Clear;
   cookies.Clear;
end;

procedure CRequest.Parse(timeOut: integer);
var
   s, hdr, hdrValue, exitMsg: string;
   params, location: string;
   x, contentLength: integer;
   formData: boolean;
label
  	ProcOut;
begin
   exitMsg := '';
   
   //read request line
   valid := false;
	s := DecodeURL(m_socket.RecvString(timeOut));
   lastError := m_socket.LastError;
   if lastError = WSAETIMEDOUT then exit;
   if lastError = WSAECONNRESET then begin
      // connection reset by peer
      Log.Writeln(['Trace: CRequest.Parse : ' + m_socket.GetErrorDescEx  + ',  id = ' + m_connection.id], true);
      exit;
	end;
	if lastError <> 0 then begin
      exitMsg := '1 : ' + m_socket.GetErrorDescEx + ' (' + IntToStr(lastError) + '), s="' + s + '"';
      goto ProcOut;
	end;
   if s = '' then begin
      exitMsg := '2 : No data received.';
      goto ProcOut;
	end;
	Log.Writeln(['Trace: CRequest.Parse Request = ', s + ',  id = ' + m_connection.id], true);
   
   // read the method, ie. GET, POST, etc.
   method := fetch(s, ' ');
   if (s = '') or (method = '') then begin
      exitMsg := '3 : Unable to parse request method. s="' + s + '", method="' + method + '"';
      goto ProcOut;
	end;
   
   // read the uri
	uri := fetch(s, ' ');
   if uri = '' then begin
      exitMsg := '4 : Unable to parse request. URI missing.';
      goto ProcOut;
	end;

   // check for query parameters  (ie. GET)
   params := SeparateRight(uri, '?');
   if params <> uri then begin
      ParseParametersEx(params, '&', parameters);
      Log.Writeln(['Trace: CRequest.Parse, input parameters = ' + parameters.CommaText], true);
      uri := fetch(uri, '?');
	end;

   // read the protocol
   protocol := fetch(s, ' ');
   if protocol = '' then begin
      exitMsg := '5 : Unable to parse request. Protocol missing.';
      goto ProcOut;
	end;

   // check protocol
   contentLength := -1;
   m_connection.shouldClose := false;
   if pos('HTTP/', Uppercase(protocol)) <> 1 then begin
      exitMsg := '6 : Unexpected protocol - got "' + protocol + '"';
      goto ProcOut;
	end;
	if pos('HTTP/1.1', Uppercase(protocol)) <> 1 then
      m_connection.shouldClose := true;

	//read headers
   formData := false;
   repeat
      s := DecodeURL(m_socket.RecvString(timeOut));
		lastError := m_socket.LastError;
      if lasterror <> 0 then begin
      	exitMsg := '7 : ' + m_socket.GetErrorDescEx + ' (' + IntToStr(lastError) + ')';
      	goto ProcOut;
		end;
		if s = '' then break;	// blank line signals end of headers
		Log.Writeln(['Trace: CRequest.Parse, header = ', s], true);
      hdr := fetch(s, ':');
      hdrValue := s;
      headers.Values[hdr] := hdrValue;
      
      // look for special headers that we want to interpret
      if CompareText(hdr, 'Content-Type') = 0 then
         formData := AnsiContainsText(hdrValue, 'application/x-www-form-urlencoded');
      
		if CompareText(hdr, 'Content-Length') = 0 then
         contentLength := StrToIntDef(hdrValue, -1);
      
		if CompareText(hdr, 'Host') = 0 then begin
         host := SeparateLeft(hdrValue, ':');
         port := StrToIntDef(SeparateRight(hdrValue, ':'), 80);
		end;
      
		if CompareText(hdr, 'Connection') = 0 then begin
         if CompareText(hdrValue, 'close') = 0 then
            m_connection.shouldClose := true
         else
         if CompareText(hdrValue, 'keep-alive') = 0 then
            m_connection.shouldClose := false;
		end;
      
		if CompareText(hdr, 'Cookie') = 0 then begin
         FreeAndNil(cookies);
         cookies := Split(hdrValue, '; ');
		end;

	until false;
   
   // read document content, if any (ie. POST)
   m_inputStream.Clear;
   if contentLength > 0 then begin
      m_inputStream.SetSize(contentLength);
      // note: you can't use RecvString because even though it is text, it is not CRLF terminated.
      x := m_socket.RecvBufferEx(m_inputStream.Memory, contentLength, timeOut);
		lastError := m_socket.LastError;
      if lasterror <> 0 then begin
      	exitMsg := '8 : ' + m_socket.GetErrorDescEx + ' (' + IntToStr(lastError) + ')';
      	goto ProcOut;
		end;
      m_inputStream.SetSize(x);
      if formData then begin
	      SetLength(s, x);
	      m_inputStream.Read(s[1], x);
         s := DecodeURL(s);
         ParseParametersEx(s, '&', parameters);
         Log.Writeln(['Trace: CRequest.Parse, document content = ' + parameters.CommaText], true);
		end;
   end;

   valid := true;
   lastError := 0;
   
ProcOut:
	if exitMsg <> '' then begin
      location := fetch(exitMsg, ':');
      Log.Writeln(['Error in CRequest.Parse[' + location + '] - ' + exitMsg + ',  id = ' + m_connection.id]);
	end;

end;		// Parse


// ------------------------------------------------------------------
//	Class CResponse
// ------------------------------------------------------------------

constructor CResponse.Create(conn: CConnection);
begin
   connection := conn;
   headers := TStringList.Create;
   outputStream := TMemoryStream.Create;
end;

destructor CResponse.Destroy;
begin
   FreeAndNil(headers);
   FreeAndNil(outputStream);
	inherited Destroy;
end;

procedure CResponse.Clear;
begin
	outputStream.Clear;
   headers.Clear;
end;

procedure CResponse.Finalize;
begin
   headers.Add('Date: ' + Rfc822DateTime(now));
   headers.Add('Server: Mining Visualizer HTTP server');
   headers.Add('');
end;

procedure CResponse.Send(shouldClose: boolean);
var
   n: integer;
begin
   Log.Writeln(['Trace: CResponse.Send ', requestUri], true);
	Finalize;
   connection.socket.SendString(protocol + ' ' + resultCode + CRLF);
    if shouldClose then
       headers.Add('Connection: close');
    
    for n := 0 to headers.count - 1 do
       connection.socket.sendstring(headers[n] + CRLF);
    
    if connection.socket.lasterror <> 0 then
       exit;
    connection.socket.SendBuffer(outputStream.Memory, outputStream.Size);
end;



end.





