
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

unit uGlobals;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Main, uJson, uSocket, uMinerRPC, uCollector, uTest, 
   uMiners, uWidgetData, uWebFace, uLog, fpjson, uMiningData;

const

   // defaults
  	DEF_UDP_IN = 5226;
   DEF_NODE_IP = '127.0.0.1';
   DEF_NODE_PORT = 8545;
   DEF_UDP_OUT = 5225;

   DEF_MINER_LABEL = '#MINER# - #GPU#';
   
   
type
  
   // how to display hash rates on the desktop widget
   EnumHashRates = (
   	HRGrandTotal := 0,
      HRIndividualRigs,
      IndividualGPUs
   );
  
   // how to display GPU temps on the desktop widget
   EnumTempDisplay = (
   	TDStatus := 0,		// status indicator only (ie. Normal / Alert)
      TDGPUs				// actual GPU temperatures
   );
   
var
  	g_settings: CJson;
	g_socket: CSocket;
   g_minerRPC: CMinerRPC;
   g_miners: CMiners;
   g_collector: CCollector;
   g_tests: CTests;
   g_altConfig: string;
   g_widgetData: CWidgetData;
   g_miningData: CMiningData;
   g_webFace: CWebFace;
   g_mainForm: TLogView;
   Log: CLog;


function programInit: boolean;
procedure programShutdown;

implementation

uses uMisc, Forms, LCLType, fileutil, LazFileUtils;

function programInit: boolean;
var
   src, dest: string;
   uPort: integer;
   jObject: TJSONObject;
begin
   result := false;
   try
		// create config folder if necessary
	   dest := getConfigFolder;
      ForceDirectoriesUTF8(dest);
{$IFDEF Darwin}
   	dest := ConcatPaths([dest, 'GeekTool']);
      if not DirectoryExists(dest) then begin
			CreateDir(dest);
			src := ConcatPaths([getResourcesFolder, 'GeekTool']);
			CopyDirTree(src, dest, [cffOverwriteFile]);
		end;
{$ENDIF}
	
      Log := CLog.Create;
		Log.Init;
		Log.Writeln([]);
		Log.Writeln(['===========  Mining Visualizer startup  ===========']);
	
		g_settings := CJson.Create;
      g_settings.Load('config.json');
      
      g_settings.setDefaults([
      				'DevEnv', '0',
                  'UdpListen', '5226',
                  'HashRates', IntToStr(Ord(HRGrandTotal)),
                  'HashRateDeviationAlert', '10',
                  'TemperatureDisplay', IntToStr(Ord(TDStatus)),
                  'StartMinimized', '0',
                  'CloseHits.miner_label', '#MINER# - #GPU#',
                  'CloseHits.closehit_frequency', '10800',
                  'CloseHits.workunit_frequency','600',
                  'CloseHits.widget_scale', '50',
                  'WebInterface.port', '8018',
                  'WebInterface.require_login', '0',
                  'WebInterface.password', '',
                  'WebInterface.use_ssl', '0',
                  'WebInterface.ca_cert_bundle', '',
                  'WebInterface.server_cert', '',
                  'WebInterface.server_key', '',
                  'Alerts.enabled', '0',
                  'Alerts.action', '',
                  'Alerts.wait', '4'
      ]);
	
      uPort := g_settings.getInt('UdpListen');
		g_socket := CSocket.Create(uPort);
      if not g_socket.init then
      	exit;
	
	   g_miners := CMiners.Create;
	   g_minerRPC := CMinerRPC.Create;
      
      g_miningData := CMiningData.Create;
            
   	g_widgetData := CWidgetData.Create;
      g_webFace := CWebFace.Create;
      g_webFace.StartServer;
		g_collector := CCollector.Create;
	
	   g_tests := CTests.Create;
      g_mainForm := logView;
	   result := true;
      
	except
   	on e: Exception do begin
      	Application.MessageBox(PChar(e.Message), 'Mining Visualizer', MB_ICONERROR);
			result := false;
		end;
	end;
end;

procedure programShutdown;
begin
   g_collector.Free;
   g_webFace.Free;
   g_socket.Free;
   g_widgetData.Free;
   g_miners.Free;
   g_minerRPC.Free;
   g_tests.Free;
   Log.Close;
   Log.Free;
   g_miningData.Free;
   g_settings.Free;
end;


end.

