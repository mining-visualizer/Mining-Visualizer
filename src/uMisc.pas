
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

unit uMisc;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type
  
Exception2 = class(Exception)
	constructor Create(const msg : string);
end;


function getConfigFolder: string;
function getResourcesFolder: string;
function MessageBox2(Text: PChar; Caption: PChar; Flags: LongInt): integer;
function DevEnv: boolean;
function formatHash64(strNumber: string): string;

implementation

uses Forms, uLog, uGlobals, uLib, LazFileUtils;


constructor Exception2.Create(const msg: string);
begin
   inherited Create(msg);
   Log.Writeln([msg]);
end;

function getConfigFolder: string;
begin
   if g_altConfig <> '' then
      result := g_altConfig
   else
      // Linux :  $HOME/.config/MiningVisualizer  
      // MacOSX : $HOME/.config/MiningVisualizer
      // Windows : %LocalAppData%\MiningVisualizer
   	result := GetAppConfigDirUTF8(false);
end;

function getResourcesFolder: string;
begin
   // on OSX, this gives InstallFolder/Program.app/Contents/MacOS
	result := Application.Location;
	{$ifdef Darwin}
	result := ConcatPaths([result, '..', 'Resources']);
	{$endif}
end;

function MessageBox2(Text: PChar; Caption: PChar; Flags: LongInt): integer;
begin
	result := Application.MessageBox(Text, Caption, Flags);
   Log.Writeln([Text]);
end;

function DevEnv: boolean;
begin
   result := g_settings.getBool('DevEnv');
end;

function formatHash64(strNumber: string): string;
var
   n: QWord;
begin
   n := StrToQWord(strNumber);
	result := AddCommas(n);
end;

end.

