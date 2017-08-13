
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

unit uHTMLDoc;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fgl;

type
  
TItemMap = specialize TFPGMap<string, integer>;

{
	This class will ...
   	- read in an html file
      - parse it, looking for specially formatted placeholders: an identifier with leading and traling "__", ie. "__Identifier__"
      - set all placeholders to emtpy string (but remember their position)
      - allow you to set arbritrary text in a placeholders position, by calling setValue.
      - write everything out to a file or stream.  this would include the static text and the current value of all the placeholders.
      
   Usage:
   	- given the following html : 
	      	<html><body>
	         	<div>__Greeting__</div>
	            <div>__Message__</div>
	         </body></html>
      			
      - initialize as follows:
      	IDs := TStringList.Create;
         // not all IDs need be present in the source file
         IDs.CommaText := 'Greeting, Message';
         // destFile can be empty if you're just writing to a stream
      	doc := CHTMLDoc.Create(srcFile, destFile, IDs);
         doc.setValue('Greeting', 'Hello there');
         doc.writeToDisk;
         //  or
         doc.writeToStream(someStream);
}

CHTMLDoc = class
  	public
      constructor Create(srcFile, destFile: string; IDs: TStringList);
      destructor Destroy; override;
      procedure setValue(id: string; value: string; flushToDisk: boolean = false);
      procedure writeToStream(stream: TStream);
  
	private
      function decoratedID(id: string): string;
      procedure writeToDisk;
      
  	private
      m_destFilePath, m_destFileName: string;
  		m_chunks: TStringList;
      m_itemMap: TItemMap;

end;	// class CHTMLDoc

implementation

uses
	uLib, uMisc, uLog, uGlobals;

constructor CHTMLDoc.Create(srcFile, destFile: string; IDs: TStringList);
var
  	id: string;
   i, c: integer;
   moreChunks: TStringList;
begin
	//	  We start out with an HTML file that has special placeholders.  we end up with a TStringList, where
	//   each element of the stringlist is either some static text that was between placeholders, or an 
	//   empty string for where the placeholder was.
	//   we also prepare an index into this stringlist, of where all those (now) empty placeholders are.
   m_chunks := TStringList.Create;
   m_destFilePath := destFile;
   m_destFileName := ExtractFileName(m_destFilePath);
   
   try
      // start out with the entire file as one chunk.
		m_chunks.Add(ReadTextFile(srcFile));
      
      // split the html file up into chunks, where the data items are the dividers between chunks.
      for id in IDs do begin
         // search each chunk to see if it contains this id.
			for i := m_chunks.Count - 1 downto 0 do begin
            moreChunks := Split(m_chunks[i], decoratedID(id));
            if moreChunks.Count > 1 then begin
               // split the chunk that contains the id we're looking for into 3 or 
               // more chunks: <before>, <id>, <after> ...
	            for c := moreChunks.Count - 1 downto 1 do begin
						m_chunks.Insert(i + 1, moreChunks[c]);
               	m_chunks.Insert(i + 1, decoratedID(id));	// this is where the data item will go
					end;
            	m_chunks[i] := moreChunks[0];
				end;
           	moreChunks.Free
         end;
		end;
      
		// now make an indexed list of where each data item is
      m_itemMap := TItemMap.Create;
      for id in IDs do begin
         repeat
				i := m_chunks.IndexOf(decoratedID(id));
	         if i <> -1 then begin
		      	m_itemMap.Add(id, i);
		         m_chunks[i] := '';
				end;
			until i = -1;
		end;
      
	except
   	on e: Exception do begin
     		raise Exception2.Create('Exception in HTMLDoc.Create - ' + e.Message);
		end;
	end;
   
end;

destructor CHTMLDoc.Destroy;
begin
   m_chunks.Free;
   m_itemMap.Free;
	inherited Destroy;
end;

procedure CHTMLDoc.setValue(id: string; value: string; flushToDisk: boolean {=false} );
var
   i: integer;
   idFound: boolean = false;
begin
   Log.Writeln(['Trace: CHTMLDoc.setValue - id = ', id, ', value = ', value, ', destFile = ', m_destFileName], true);
   for i := 0 to m_itemMap.Count - 1 do begin
      if m_itemMap.Keys[i] = id then begin
      	m_chunks[m_itemMap.Data[i]] := value;
         idFound := true;
		end;
	end;
   if flushToDisk and idFound then
   	writeToDisk;
end;

function CHTMLDoc.decoratedID(id: string): string;
begin
	result := '__' + id + '__';
end;

procedure CHTMLDoc.writeToDisk;
var
   fs: TFileStream;
   chunk: string;
begin
	fs := TFileStream.Create(m_destFilePath, fmCreate);
   for chunk in m_chunks do begin
      fs.WriteBuffer(Pointer(chunk)^, Length(chunk));
	end;
	fs.Free;
end;

procedure CHTMLDoc.writeToStream(stream: TStream);
begin
   m_chunks.SaveToStream(stream);
end;

end.

