
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

unit uNoWidget;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpjson;

type

{ CWidget }

CWidget = class
  	public
      procedure setValue(key:  string; value: string; updateWidget: boolean);
      procedure setCloseHits(closeHits: TJSONArray);
      procedure refreshAll;
      procedure heartBeat;
      function widgetOnline: boolean;
end;

implementation

procedure CWidget.setValue(key: string; value: string; updateWidget: boolean);
begin
end;

procedure CWidget.setCloseHits(closeHits: TJSONArray);
begin

end;

procedure CWidget.refreshAll;
begin

end;

procedure CWidget.heartBeat;
begin
end;

function CWidget.widgetOnline: boolean;
begin
   result := true;
end;

end.

