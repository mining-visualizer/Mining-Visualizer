
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

unit uLib;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

function IsNumeric(s: string): boolean;
function ReadTextFile(path: string): string;
function Split(s: string; delimiter: string): TStringList;
function IIf(condition: boolean; truePart: string; falsePart: string): string;
function IIf(condition: boolean; truePart: integer; falsePart: integer): integer;
function IIf(condition: boolean; truePart: double; falsePart: double): double;
function ToDateTime(dateStr: string) : TDateTime;
function AddCommas(n: QWord): string;

implementation

uses
	strutils;

function IsNumeric(s: string): boolean;
var
	v: double;
begin
	result := TryStrToFloat(s, v);
end;

function ReadTextFile(path: string): string;
// caller is responsible for exceptions
var
   fs: TFileStream;
begin
   result := '';
	fs := TFileStream.Create(path, fmOpenRead);
   SetLength(result, fs.Size);
   fs.Read(result[1], fs.Size);
   fs.Free;
end;

function Split(s: string; delimiter: string): TStringList;
var
   lineBreak: string;
begin
   // caller is responsible to Free the returned TStringList.
   result := TStringList.Create;
   lineBreak := result.LineBreak;
   result.LineBreak := delimiter;
   result.Text := s;
   result.LineBreak := lineBreak;
   
   // the above is a rather trick method, but it doesn't deal with all cases correctly:
   if (s = '') or AnsiEndsStr(delimiter, s) then
      result.Add('');
end;

function IIf(condition: boolean; truePart: string; falsePart: string): string;
begin
   if condition then
   	result := truePart
   else
     	result := falsePart;
end;

function IIf(condition: boolean; truePart: integer; falsePart: integer): integer;
begin
   if condition then
   	result := truePart
   else
     	result := falsePart;
end;

function IIf(condition: boolean; truePart: double; falsePart: double): double;
begin
   if condition then
   	result := truePart
   else
     	result := falsePart;
end;

function ToDateTime(dateStr: string): TDateTime;
var
   settings: TFormatSettings;
begin
   // expecting YYYY-MM-DD HH:MM:SS.  if dateStr cannot be converted we return TDateTime(0).
   settings := DefaultFormatSettings;
   settings.ShortDateFormat := 'yyyy-MM-dd';
   if not TryStrToDateTime(dateStr, result, settings) then
   	result := TDateTime(0);
end;

function AddCommas(n: QWord): string;
var 
   s: string;
   i, nextInsert: integer;
begin
	s := IntToStr(n);
   nextInsert := 2;
   for i := 1 to (Length(s) - 1) div 3 do begin
   	Insert(',', s, Length(s) - nextInsert);
      inc(nextInsert, 4);
	end;
   result := s;
end;


end.

