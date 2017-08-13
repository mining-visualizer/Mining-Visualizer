
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

program MiningVisualizer;


{$mode objfpc}{$H+}

uses 
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, Main, SysUtils, LCLType, uInstance, uGlobals; 

{$R *.res}

begin

	{$ifdef DEBUG} 
		{$if declared(UseHeapTrace)}
      GlobalSkipIfNoLeaks := true;
		if FileExists('heap.trc') then
			DeleteFile('heap.trc');
		SetHeapTraceOutput('heap.trc');
		{$ifend}
	{$endif DEBUG}

   RequireDerivedFormResource:=True;
	Application.Initialize;
   
   g_altConfig := '';
   // specify --config=<path> on the command line to provide an alternate config folder
   if Application.HasOption('config') then begin
      g_altConfig := Application.GetOptionValue('config');
	   if not DirectoryExists(g_altConfig) then begin
	      Application.MessageBox(PChar('The folder "' + g_altConfig + '" does not exist!'), 'Mining Visualizer', MB_ICONEXCLAMATION);
	      exit;
		end;
	end;

	if InstanceRunning then begin
      Application.MessageBox('An instance is already running!', 'Mining Visualizer', MB_ICONEXCLAMATION);
      exit;
	end;
   
	Application.CreateForm(TLogView, logView);
	Application.Run;
end.

