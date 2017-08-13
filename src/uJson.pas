
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

unit uJson;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpjson;

type

   TJsonClass = class of TJSONData;
	
  	CJson = Class
	public
      procedure Load(filename: string);
      destructor Destroy; override;
      procedure setDefaults(const Args: array of string);
      
      // this returns a clone of the object, if found.  caller is responsible to free.
      function getValue(path: string; defaultClass: TJsonClass): TJSONData;
      function getValue(path: string; defaultVal: string): string;
      function getValue(path: string; defaultVal: integer): integer;
      function getValue(path: string; defaultVal: boolean): boolean;
      
      // these 3 functions will return pre-programmed defaults if 'path' does not exist (see setDefaults)
      function getString(path: string): string;
      function getInt(path: string): integer;
      function getBool(path: string): boolean;
      
		// these methods can only be used to operate on top-level elements
      procedure putValue(key: string; val: TJSONData);
      procedure putValue(key: string; val: string);
      procedure putValue(key: string; val: integer);
      procedure putValue(key: string; val: boolean);
      procedure deleteValue(key: string);
      
      // these can be used to set values on a TJSONObject at an arbitrary path. path must exist.
      procedure putValue(path, key: string; val: TJSONData);
      procedure putValue(path, key: string; val: string);
      procedure putValue(path, key: string; val: integer);
      procedure putValue(path, key: string; val: boolean);
   
      procedure beginUpdate;
      procedure endUpdate;
      
   private
      procedure writeToDisk;

   private
		m_json: TJSONObject;
   	m_filePath: String;
      m_updating: boolean;
      m_defaults: TStringList;
      x_mutex: TMultiReadExclusiveWriteSynchronizer;
      

end;


implementation

uses Forms, FileUtil, uMisc, LCLType, uLog, uGlobals;


procedure CJson.Load(filename: string);
var
   contents : TStringList;
begin
   try
   	x_mutex := TMultiReadExclusiveWriteSynchronizer.Create;
		m_updating := false;
		m_filePath := ConcatPaths([getConfigFolder(), filename]);
	   contents := TStringList.Create;
	   contents.LoadFromFile(m_filePath);
	   m_json := TJSONObject(GetJSON(contents.Text));
      if m_json = nil then
      	m_json := TJSONObject.Create;
      contents.Free;
      
   except
   	on e: Exception do begin
      	Log.Writeln(['Exception in CJson.Init : ', e.Message, ', m_filePath = ', m_filePath]);
			// create an empty JSON object
	      m_json := TJSONObject.Create;
	      if FileExists(m_filePath) then begin
		      MessageBox2(PChar('An error ocurred reading JSON values from "' + m_filePath + '". ' +
		      	'This file will be cleared and reset to default values.  The old (corrupt) values ' +
		         'will be saved to "' + m_filePath + '.bak"'),  PChar('Error reading "' + m_filePath + '"'), MB_OK + MB_ICONEXCLAMATION);
	      end;
	      CopyFile(m_filePath, m_filePath + '.bak');
		end;
	end;
end;

destructor CJson.Destroy;
begin
   Log.Writeln(['Trace: CJson.Destroy'], true);
   m_json.Clear;
   m_json.Free;
   m_defaults.Free;
   x_mutex.Free;
	inherited Destroy;
end;

procedure CJson.setDefaults(const Args: array of string);
var
   i: integer;
begin
   FreeAndNil(m_defaults);
   m_defaults := TStringList.Create;
	for i := 0 to High(Args) div 2 do begin
   	m_defaults.Values[Args[i * 2]] := Args[i * 2 + 1];
  	end;
end;

function CJson.getValue(path: string; defaultClass: TJsonClass): TJSONData;
var
	jData: TJSONData;
begin
   try
      x_mutex.Beginread;
		jData := m_json.FindPath(path);
	   if jData = nil then begin
		   if defaultClass = TJSONArray then
		   	result := TJSONArray.Create
		   else if defaultClass = TJSONObject then
		   	result := TJSONObject.Create
			else
	   		raise Exception2.Create('Exception in CJson.getValue : defaultClass is invalid');
		end else
	   	result := jData.Clone;
	finally
   	x_mutex.Endread;
	end;
end;

function CJson.getValue(path: string; defaultVal: string): string;
var
	jData: TJSONData;
begin
   try
      x_mutex.Beginread;
		jData := m_json.FindPath(path);
	   if jData = nil then
	      result := defaultVal
	   else
	   	result := jData.AsString;
	finally
   	x_mutex.Endread;
	end;
end;

function CJson.getValue(path: string; defaultVal: integer): integer;
var
	jData: TJSONData;
begin
   try
      x_mutex.Beginread;
		jData := m_json.FindPath(path);
	   if jData = nil then
	      result := defaultVal
	   else
	   	result := jData.AsInteger;
	finally
   	x_mutex.Endread;
	end;
end;

function CJson.getValue(path: string; defaultVal: boolean): boolean;
var
	jData: TJSONData;
begin
   try
      x_mutex.Beginread;
	jData := m_json.FindPath(path);
   if jData = nil then
      result := defaultVal
   else
   	result := jData.AsBoolean;
	finally
   	x_mutex.Endread;
	end;
end;

function CJson.getString(path: string): string;
begin
	if m_defaults.IndexOfName(path) = -1 then
   	raise Exception2.Create('Exception in CJson.getValue : path "' + path + '" not found in defaults list.');
   result := getValue(path, m_defaults.Values[path]);
end;

function CJson.getInt(path: string): integer;
begin
	if m_defaults.IndexOfName(path) = -1 then
   	raise Exception2.Create('Exception in CJson.getValue : path "' + path + '" not found in defaults list.');
   result := getValue(path, StrToInt(m_defaults.Values[path]));
end;
      
function CJson.getBool(path: string): boolean;
begin
	if m_defaults.IndexOfName(path) = -1 then
   	raise Exception2.Create('Exception in CJson.getValue : path "' + path + '" not found in defaults list.');
   result := getValue(path, StrToInt(m_defaults.Values[path]) <> 0);
end;

// the next set of methods can only be used to operate on top-level elements
procedure CJson.putValue(key: string; val: TJSONData);
begin
   try
      x_mutex.Beginwrite;
		m_json.Delete(key);
	   m_json.Add(key, val.Clone);
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(key: string; val: string);
begin
   try
      x_mutex.Beginwrite;
		m_json.Delete(key);
	   m_json.Add(key, TJSONString.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(key: string; val: integer);
begin
   try
      x_mutex.Beginwrite;
		m_json.Delete(key);
	   m_json.Add(key, TJSONIntegerNumber.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(key: string; val: boolean);
begin
   try
      x_mutex.Beginwrite;
		m_json.Delete(key);
	   m_json.Add(key, TJSONBoolean.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.deleteValue(key: string);
begin
   try
      x_mutex.Beginwrite;
		m_json.Delete(key);
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

// the next 3 puts can be used to set values on a TJSONObject at an arbitrary path. path must exist.
procedure CJson.putValue(path, key: string; val: TJSONData);
var
	jObject: TJSONObject;
begin
   try
      x_mutex.Beginwrite;
		jObject := TJSONObject(m_json.FindPath(path));
	   jObject.Delete(key);
	   jObject.Add(key, val.Clone);
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(path, key: string; val: string);
var
	jObject: TJSONObject;
begin
   try
      x_mutex.Beginwrite;
		jObject := TJSONObject(m_json.FindPath(path));
	   jObject.Delete(key);
	   jObject.Add(key, TJSONString.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(path, key: string; val: integer);
var
	jObject: TJSONObject;
begin
   try
      x_mutex.Beginwrite;
		jObject := TJSONObject(m_json.FindPath(path));
	   jObject.Delete(key);
	   jObject.Add(key, TJSONIntegerNumber.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.putValue(path, key: string; val: boolean);
var
	jObject: TJSONObject;
begin
   try
      x_mutex.Beginwrite;
		jObject := TJSONObject(m_json.FindPath(path));
	   jObject.Delete(key);
	   jObject.Add(key, TJSONBoolean.Create(val));
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;

procedure CJson.writeToDisk;
var
   contents : TStringList;
begin
   if m_updating then exit;
   contents := TStringList.Create;
   contents.Text := m_json.FormatJSON;
   contents.SaveToFile(m_filePath);
   contents.Free;
end;

procedure CJson.beginUpdate;
begin
	m_updating := true;
end;

procedure CJson.endUpdate;
begin
   try
      x_mutex.Beginwrite;
		m_updating := false;
	   writeToDisk;
	finally
   	x_mutex.Endwrite;
	end;
end;


end.

