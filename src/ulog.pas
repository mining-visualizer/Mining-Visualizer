
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

unit uLog;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, syncobjs;

type

CLog = Class

	public
   	constructor Create;
		procedure Init;
		procedure Write(const Args: array of const; filtered: boolean = false; separator: string = '');
		procedure Writeln(const Args: array of const; filtered: boolean = false; separator: string = '');
      procedure Close;

   private
      procedure WriteArg(arg: TVarRec);
      function FiltersFilename: string;
      procedure CheckLoadFilters;
      procedure LoadFilters;
      function FilterMatch(const Args: array of const): boolean;

   private
		m_file: TextFile;
		m_needsTimestamp: boolean;
      m_critSection: TCriticalSection;
      m_useCritSect:  boolean;
      m_flush: boolean;
      m_filters: TStringList;
      	// the date/time stamp of LogFilters.txt
      m_filterTime: longint;
      	// the last time we checked LogFilters.txt for changes.
      m_lastFilterCheck: TDateTime;
      m_closed: boolean;

end;


implementation

uses uMisc, fpjson, StrUtils, FileUtil, dateutils;

constructor CLog.Create;
begin
   m_filterTime := 0;
   m_lastFilterCheck := TDateTime(0);
end;

procedure CLog.Init;
var
  temp: TStringList;
      filename: string;
begin
   
	filename := ConcatPaths([getConfigFolder, 'log.txt']);
   
   // trim file if necessary
   if FileExists(filename) and (FileUtil.FileSize(filename) > 5000000) then begin
      temp := TStringList.Create;
   	temp.LoadFromFile(filename);
      temp.Text := RightStr(temp.Text, 1000000);
      temp.SaveToFile(filename);
      temp.Free;      
	end;

	Assign(m_file, filename);
   if not FileExists(filename) then begin
   	Rewrite(m_file);
   end else begin
     	Append(m_file);
	end;
   m_needsTimestamp := true;
   m_critSection := TCriticalSection.Create;
   m_flush := true;
   m_useCritSect := true;
   m_closed := false;

   CheckLoadFilters;
   
end;


procedure CLog.Write(const Args: array of const; filtered: boolean; separator: string);
var
  i: Integer;
begin
   if m_closed then exit;
	CheckLoadFilters;
   if (filtered and not filterMatch(Args)) then exit;

   if m_useCritSect then
		m_critSection.Acquire;

   try
		if High(Args) = -1 then exit;		// empty array
		if m_needsTimestamp then begin
			System.Write(m_file, FormatDateTime('"d"d hh:nn:ss:zzz', Now), ' : ');
		end;
	   WriteArg(Args[0]);
		for i := 1 to High(Args) do begin
	   	System.Write(m_file, separator);
	   	WriteArg(Args[i]);
	  	end;

		if m_flush then
	   	System.flush(m_file);

	   m_needsTimestamp := false;
	finally
	   if m_useCritSect then
			m_critSection.Release;
	end;

end;


procedure CLog.Writeln(const Args: array of const; filtered: boolean; separator: string);
begin
   if m_closed then exit;
	CheckLoadFilters;
   if (filtered and not filterMatch(Args)) then exit;

	m_critSection.Acquire;
   try
	   m_flush := false;
      m_useCritSect := false;
	  	Write(Args, filtered, separator);
      m_useCritSect := true;
	   m_flush := true;

	   System.writeln(m_file);
	   System.Flush(m_file);

	   m_needsTimestamp := true;
	finally
      m_critSection.Release;
	end;

end;


procedure CLog.WriteArg(arg: TVarRec);
var
   pwc: PWideChar;
begin
	case arg.VType of
      vtInteger:  	System.write(m_file, arg.VInteger);
      vtBoolean:  	System.write(m_file, arg.VBoolean);
      vtChar:     	System.write(m_file, arg.VChar);
      vtExtended: 	System.write(m_file, arg.VExtended^:1:3);
      vtString:   	System.write(m_file, arg.VString^);
      vtPChar:    	System.write(m_file, arg.VPChar);
      vtObject:   	System.write(m_file, arg.VObject.ClassName);
      vtClass:    	System.write(m_file, arg.VClass.ClassName);
      vtAnsiString:  System.write(m_file, AnsiString(arg.VAnsiString));
		vtWideString: 
		begin
         pwc := arg.VWideString;
      	while pwc^ <> #0 do begin
         	System.write(m_file, pwc^);
            pwc := pwc + 1;
			end;
		end;
      vtCurrency:    System.write(m_file, arg.VCurrency^);
      vtVariant:     System.write(m_file, arg.VVariant^);
      vtInt64:       System.write(m_file, arg.VInt64^);
      vtQWord:			System.write(m_file, arg.VQWord^);
      vtPointer: 
      begin
         if Assigned(arg.VPointer) then
            System.write(m_file, 'Ptr=Assigned')
         else
           	System.write(m_file, 'Ptr=nil');
		end;
      else
      	System.write(m_file, '<UnsupportedType>');
	end;
end;

function CLog.FiltersFilename: string;
begin
	result := ConcatPaths([getConfigFolder, 'logfilters.txt']);
end;

procedure CLog.CheckLoadFilters;
var
   t: longint;
begin
   if SecondsBetween(Now, m_lastFilterCheck) > 5 then begin
      t := FileAge(FiltersFilename);
      if t <> m_filterTime then begin
         LoadFilters;
         m_filterTime := t;
		end;
		m_lastFilterCheck := Now;
	end;
end;

procedure CLog.LoadFilters;
var
  filters_path : string;
begin
	filters_path := FiltersFilename;
   if Assigned(m_filters) then 
      m_filters.Free;
   m_filters := TStringList.Create;
   try
      if FileExists(filters_path) then
      	m_filters.LoadFromFile(filters_path);
	except
   	on e: Exception do
      	System.write(m_file, 'Exception in Log.Init : ' + e.Message);
		// just leave m_filters empty
	end;
   
end;

function CLog.FilterMatch(const Args: array of const): boolean;
// check any string values in Args for a match against anything in m_filters. if
// we get a match, return true so the CLog statement passes through the filter
// and gets output to the file.
var
	i, j: integer;
   s: string;
begin
   result := true;
	for i := 0 to High(Args) do begin
      s := '';
      if Args[i].VType = vtString then
         s := Args[i].VString^;
      if Args[i].VType = vtAnsiString then
         s := AnsiString(Args[i].VAnsiString);
      if s <> '' then begin
         // check this string segment against the filters
			for j := 0 to m_filters.count - 1 do begin
		      if (m_filters[j] <> '') and AnsiContainsText(s, m_filters[j]) then
		         exit;
			end;
      end;
  	end;
	result := false;
end;


procedure CLog.Close;
begin
	m_critSection.Acquire;
   try
	   {$i-}
	   System.Close(m_file);
	   {$i+}
	   m_closed := true;
	finally
      m_critSection.Release;
	end;
   m_critSection.Free;
   m_filters.Free;
end;


end.

