
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

unit uSocket;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, synsock, blcksock, Sockets, cTypes, fpjson, LCLProc, LCLType, Forms, syncobjs, contnrs;

type

   TEventType = (ETReceive, ETError);

   TOnReceiveFn = Procedure (msg: string; ip: string) of object;
   TOnErrorFn = Procedure (err: string) of object;

   TDispatchRec = class
      msg: string;
      ip: string;
      eType: TEventType
	end;

{ CSocket }

CSocket = class(TThread)

  	public
      constructor Create(portIn: word);
      function Init: boolean;
      Destructor Destroy; override;
		function Send(msg: string; ip: string; port: integer): boolean;
      function Receive(timeout: integer): string;
      procedure dispatchMsg;
      procedure addListener(listenerType: TEventType; fn: TMethod);
		procedure removeListener(listenerType: TEventType; fn: TMethod);
      function getLastError: cint;

   Protected
     	procedure Execute; Override;

   private
   	m_socket: Longint;
      m_portIn: word;
   	m_addrIn: TSockAddr;
   	m_addrOut: TSockAddr;
      m_recvListeners: TMethodList;
      m_errListeners: TMethodList;
      m_dispatchDataQ: TObjectQueue;
      m_lastError: cint;
      m_lastPacket: string;
      m_packetReceived: TEventObject;
      x_socket: TCriticalSection;

   public
      property listenPort: word read m_portIn;

end;

implementation

uses ULog, uMisc, uGlobals; //;

const

   WSAETIMEDOUT 		= 10060;			// A connection timed out
  	WSAEINTR 			= 10004;			// A blocking operation was interrupted by a call to WSACancelBlockingCall
   WSAECONNRESET 		= 10054;			// Connection reset by peer

type

 	TTimeVal = record
   	tv_sec : PtrInt;
      tv_usec : PtrInt;
   end;


constructor CSocket.Create(portIn: word);
begin
   m_portIn := portIn;
   FreeOnTerminate := False;
	Inherited create(true);
end;

function CSocket.Init: boolean;
var
   res: integer;
begin
   try
		Log.Writeln(['Trace: CSocket.Init'], true);
	   m_socket := fpSocket(AF_INET, SOCK_DGRAM, 0);
	   m_addrIn.sa_family := AF_INET;
	   m_addrIn.sin_port := htons(m_portIn);
	   m_addrIn.sin_addr.s_addr := INADDR_ANY;
	   res := fpbind(m_socket, @m_addrIn, sizeof(m_addrIn));
	   if res = -1 then
      	if SocketError = WSAEADDRINUSE then
	     		raise Exception2.Create('Error in CSocket.Init : Unable to open UDP port ' + IntToStr(m_portIn) + 
            								'. Port is already in use. Try using a different port number for "UDP Listen".')
         else
           	raise Exception2.Create('Error in CSocket.Init : Unable to open UDP port ' + IntToStr(m_portIn) + '. ' + TBlockSocket.GetErrorDesc(SocketError));

	   m_addrOut.sa_family := AF_INET;

	   m_recvListeners := TMethodList.Create;
	   m_errListeners := TMethodList.Create;

		m_packetReceived := TEventObject.Create(nil, true, false, 'packetReceived');
	   m_dispatchDataQ := TObjectQueue.Create;
	   x_socket := TCriticalSection.Create;
      Start;
      result := true;
	except
   	on e: Exception do begin
         result := false;
      	Application.MessageBox(PChar(e.Message), 'Mining Visualizer', MB_ICONERROR);
		end;
	end;
end;

destructor CSocket.Destroy;
var
   dispatchRec: TDispatchRec;
begin
   // if the worker thread is active, shut it down cleanly.
   Log.Writeln(['Trace: CSocket.Destroy:1'], true);
   if not Finished then begin
   	Terminate;
      While not Finished do begin
   		Sleep(1);
   	end;
	end;
   Log.Writeln(['Trace: CSocket.Destroy:2'], true);
   fpShutdown(m_socket, 2);
	CloseSocket(m_socket);
   m_recvListeners.Free;
   m_errListeners.Free;
   m_packetReceived.Free;
   while m_dispatchDataQ.Count > 0 do begin
		dispatchRec := TDispatchRec(m_dispatchDataQ.Pop);
      dispatchRec.Free;
	end;
	m_dispatchDataQ.Free;
   x_socket.Free;
	inherited Destroy;
end;

// this is an asynchronous send. we don't wait for a response.
function CSocket.Send(msg: string; ip: string; port: integer) : boolean;
var
   s: string;
begin
   Log.Writeln(['UDPTraffic: sent = ', msg, ', ip = ', ip, ', port = ', port], true);
   m_addrOut.sin_addr := StrToNetAddr(ip);
   m_addrOut.sin_port := htons(port);
	result := -1 <> fpSendTo(m_socket, Pointer(msg), Length(msg), 0, @m_addrOut, sizeof(m_addrOut));
   if not result then begin
      s := 'Socket.Send : fpSendTo failed with error ' + intToStr(SocketError);
      Log.Writeln([s]);
      m_lastError := SocketError;
	end;
end;

// this is a synchronous receive.
function CSocket.Receive(timeout: integer): string;
begin
   result := '';
   m_packetReceived.ResetEvent;
   // wait for the background thread to signal us that a packet has arrived.
   if m_packetReceived.WaitFor(timeout) = wrSignaled then begin
	   result := m_lastPacket;
   end;
end;

procedure CSocket.addListener(listenerType: TEventType; fn: TMethod);
begin
   case listenerType of
		ETReceive: 	m_recvListeners.Add(fn);
		ETError:		m_errListeners.Add(fn);
	end;
end;

procedure CSocket.removeListener(listenerType: TEventType; fn: TMethod);
begin
   case listenerType of
		ETReceive: 	m_recvListeners.Remove(fn);
		ETError:		m_errListeners.Remove(fn);
	end;
end;

function CSocket.getLastError: cint;
begin
   result := m_lastError;
end;

procedure CSocket.dispatchMsg;
var
   i: integer;
   done: boolean;
   dispatchRec: TDispatchRec;
begin
   
   Log.Writeln(['Trace: CSocket.dispatchMsg, Finished=', Finished], true);
   if Finished then exit;
   
   done := false;
   repeat
   	try
         x_socket.Acquire;
			dispatchRec := TDispatchRec(m_dispatchDataQ.Pop);
		finally
        	x_socket.Release;
		end;
		if dispatchRec = nil then
         done := true
      else begin
			case dispatchRec.eType of
				ETReceive:
					for i := 0 to m_recvListeners.count - 1 do
				      TOnReceiveFn(m_recvListeners[i])(dispatchRec.msg, dispatchRec.ip);
	
				ETError:
					for i := 0 to m_errListeners.count - 1 do
				      TOnErrorFn(m_errListeners[i])(dispatchRec.msg);
			end;
	      dispatchRec.Free;
		end;
	until done;
end;

procedure CSocket.Execute;
var
   buff: string;
   addr: TSockAddr;
   addrLen: TSockLen;
   tv: TTimeVal;
   res: integer;
   dispatchRec: TDispatchRec;
begin
	// this is the worker thread, an override of TThread.Execute.
   Log.Writeln(['Trace: CSocket.Execute : In'], true);

   // set the timeout
   {$IFDEF Windows}
   tv.tv_sec := 100;					// milliseconds
   tv.tv_usec := 0;					// unused
   {$ELSE}
   tv.tv_sec := 0;					// seconds
   tv.tv_usec := 100 * 1000;		// microseconds
   {$ENDIF}
   res := fpsetsockopt(m_socket, SOL_SOCKET, SO_RCVTIMEO,  @tv, sizeof(tv));
   if res = -1 then 
     	raise Exception.create('Socket.Execute : fpsetsockopt failed with error ' + IntToStr(SocketError()));

	While True Do Begin
   	SetLength(buff, 16384);
	   addrLen := sizeof(addr);
	   res := fpRecvFrom(m_socket, pointer(buff), length(buff), 0, @addr, @addrLen);
      
		If Terminated Then
         Break;
      
	   if res = -1 then begin
         res := SocketError();
         {$IFDEF Windows}
         if (res <> WSAETIMEDOUT) and (res <> WSAEINTR) and(res <> WSAECONNRESET) then 		// errors to ignore
         {$ELSE}{$IFDEF Linux}
         if (res <> 11) then 		// ignore timeout errors
         {$ELSE}
         // Mac OSX
         if (res <> 35) then 		// ignore timeout errors
         {$ENDIF}{$ENDIF}
         begin
				dispatchRec := TDispatchRec.Create;
         	dispatchRec.msg := 'Socket.Execute : fpRecvFrom failed with error ' + intToStr(res);
            dispatchRec.eType := ETError;
            try
            	x_socket.Acquire;
					m_dispatchDataQ.Push(dispatchRec);
				finally
            	x_socket.Release;
				end;
         	Log.Writeln([dispatchRec.msg]);
			end;
		end else begin
         SetLength(buff, res);
			Log.Writeln(['UDPTraffic: received = ', buff], true);
			dispatchRec := TDispatchRec.Create;
         dispatchRec.msg := buff;
         dispatchRec.ip := NetAddrToStr(addr.sin_addr);
         dispatchRec.eType := ETReceive;
         try
         	x_socket.Acquire;
				m_dispatchDataQ.Push(dispatchRec);
			finally
         	x_socket.Release;
			end;
         // if there is a synchronous read in progress, let it know something just came in.
         m_lastPacket := buff;
         m_packetReceived.SetEvent;
		end;

		Sleep(1);
	end;

   Log.Writeln(['Trace: CSocket.Execute : Out'], true);

end;


end.

