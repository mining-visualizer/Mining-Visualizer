
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

unit uJsonRPC;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type


{ CRpc }

CJsonRpc = class

public
	constructor Create;
	function blockNumber: integer;

private
   m_url: string;
   m_port: integer;

end;

implementation

uses fphttpclient, fpjson, jsonparser, uLog, uJson, UGlobals, strutils;

constructor CJsonRpc.Create;
begin
	m_url := g_settings.getString('Node.ip');
   m_port := g_settings.getInt('Node.port');
end;

function CJsonRpc.blockNumber: integer;
var
	jObject: TJSONObject;
   http: TFPHTTPClient;
   s: string;
begin
	jObject := TJSONObject.Create;
   jObject.Add('jsonrpc', '2.0');
   jObject.Add('method', 'eth_blockNumber');
   jObject.Add('params', TJSONArray.Create);
   jObject.Add('id', 1);

	http := TFPHTTPClient.Create(nil);
   s := http.FormPost('http://' + m_url + ':' + IntToStr(m_port), jObject.AsJSON);
   http.Free;
   jObject.Free;
	Log.Writeln([s]);
	jObject := TJSONObject(GetJSON(s));
   s := jObject['result'].AsString;
   s := AnsiReplaceText(s, '0x', '');
   result := Hex2Dec(s);
   jObject.Free;
end;

end.




