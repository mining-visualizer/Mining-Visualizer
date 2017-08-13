
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

unit frmMinerEdit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, uMiner;

type
   
TMinerEditDlg = class(TForm)
  
		btnOK: TButton;
		btnCancel: TButton;
		btnTestConnection: TButton;
		chkInactive: TCheckBox;
		imgPingResult: TImage;
		images: TImageList;
		Label10: TLabel;
		Label11: TLabel;
		Label12: TLabel;
		Label13: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		lblPinging: TLabel;
		Timer1: TTimer;
		txtHashRate: TEdit;
		txtName: TEdit;
		txtIP: TEdit;
		txtNeverExceed: TEdit;
		txtHashFaults: TEdit;
		txtTempLo: TEdit;
		txtShutdown: TEdit;
		txtPort: TEdit;
		txtGPUs: TEdit;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		txtTempHi: TEdit;
      constructor CreateWithMiner(TheOwner: TComponent; miner: CMiner);
		procedure btnOKClick(Sender: TObject);
		procedure btnTestConnectionClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);
  
	protected
		procedure onSocketReceive(msg: string; ip: string);
		procedure onSocketError(msg: string);
		
	private
		m_miner: CMiner;
		m_pingID: integer;
		m_timerTicks: integer;

end;

var
	frmMinerEditDlg: TMinerEditDlg;

implementation

uses uGlobals, uSocket, fpjson, uLog, LCLType, uLib;

{$R *.lfm}

constructor TMinerEditDlg.CreateWithMiner(TheOwner: TComponent; miner: CMiner);
begin
	inherited Create(TheOwner);
	txtName.Text := miner.name;
	txtIP.Text := miner.ip;
	txtPort.Text := IntToStr(miner.port);
	txtGPUs.Text := IntToStr(miner.gpuCountDeclared);
   txtHashRate.Text := FloatToStr(miner.expectedHashRate);
   chkInactive.Checked := miner.inactive;
   txtTempHi.Text := IntToStr(miner.tempRangeHi);
   txtNeverExceed.Text := IntToStr(miner.neverExceedTemp);
   txtShutdown.Text := IntToStr(miner.safetyShutdown);
   if miner.hashFaultsAlert <> -1 then
   	txtHashFaults.Text := IntToStr(miner.hashFaultsAlert);
	m_miner := miner;
end;

procedure TMinerEditDlg.FormCreate(Sender: TObject);
begin
	g_socket.addListener(ETReceive, TMethod(@onSocketReceive));
	g_socket.addListener(ETError, TMethod(@onSocketError));
	lblPinging.Caption := '';

   // fonts
	{$IfDef Darwin}
   	Font.Name := 'Lucida Grande';
   	Font.Size := 11;
	{$ENDIF}
end;

procedure TMinerEditDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  g_socket.removeListener(ETReceive, TMethod(@onSocketReceive));
  g_socket.removeListener(ETError, TMethod(@onSocketError));
end;

procedure TMinerEditDlg.btnOKClick(Sender: TObject);
begin
  
  	ModalResult := mrNone;
   
   if txtName.Text = '' then begin
   	Application.MessageBox('Please specify a miner name', '', MB_ICONEXCLAMATION);
      exit;
   end;
	if (txtPort.Text <> '') and (not isNumeric(txtPort.Text)) then  begin
   	Application.MessageBox('Please enter a numeric port number', '', MB_ICONEXCLAMATION);
   	exit;
	end;
   if StrToInt(txtPort.Text) = g_socket.listenPort then begin
   	Application.MessageBox('That UDP port number is the Listening port for Mining Visualizer.  Please choose a different port.', '', MB_ICONEXCLAMATION);
   	exit;
	end;

	if (txtGPUs.Text <> '') and (not isNumeric(txtGPUs.Text)) then  begin
   	Application.MessageBox('Please enter a numeric GPU count number', '', MB_ICONEXCLAMATION);
   	exit;
	end;
	if (txtHashRate.Text = '') or (not isNumeric(txtHashRate.Text)) then  begin
   	Application.MessageBox('Please enter the expected hash rate for this mining rig.', '', MB_ICONEXCLAMATION);
   	exit;
	end;
	if (txtNeverExceed.Text = '') or (not isNumeric(txtNeverExceed.Text)) then  begin
   	Application.MessageBox('Please enter a valid temperature for Never Exceed.', '', MB_ICONEXCLAMATION);
   	exit;
	end;
	if (txtShutdown.Text = '') or (not isNumeric(txtShutdown.Text)) then  begin
   	Application.MessageBox('Please enter the number of seconds for thermal safety shutdown.', '', MB_ICONEXCLAMATION);
   	exit;
	end;
	if (txtTempHi.Text = '') or (not isNumeric(txtTempHi.Text)) then  begin
   	Application.MessageBox('Please enter a temperature value for the high operating range.', '', MB_ICONEXCLAMATION);
   	exit;
	end;
	if (txtHashFaults.Text <> '') and (not isNumeric(txtHashFaults.Text)) then  begin
   	Application.MessageBox('Please enter a number for hash faults alert, or leave the field blank to disable this feature.', '', MB_ICONEXCLAMATION);
   	exit;
	end;

   // we have a reference to the CMiner instance that was passed to us in the form create method, so changes we
   // make here are reflected in the calling routine.
	m_miner.name := txtName.Text;
	m_miner.ip := txtIP.Text;
	m_miner.port := StrToInt(txtPort.Text);
	m_miner.gpuCountDeclared := StrToInt(txtGPUs.Text);
   m_miner.expectedHashRate := StrToFloat(txtHashRate.Text);
   m_miner.inactive := chkInactive.Checked;
   m_miner.neverExceedTemp := StrToInt(txtNeverExceed.Text);
   m_miner.safetyShutdown := StrToInt(txtShutdown.Text);
   m_miner.tempRangeHi := StrToInt(txtTempHi.Text);
   if txtHashFaults.Text <> '' then
   	m_miner.hashFaultsAlert := StrToInt(txtHashFaults.Text)
   else
   	m_miner.hashFaultsAlert := -1;
   
   ModalResult := mrOK;
end;

procedure TMinerEditDlg.btnTestConnectionClick(Sender: TObject);
var
	jObject: TJSONObject;
begin
   m_timerTicks := 0;
   Timer1.Enabled := true;
   jObject := g_minerRPC.makePacket(['command', 'ping', 'return_port', g_settings.getInt('UdpListen')]);
   g_socket.Send(jObject.AsJSON, txtIP.Text, StrToInt(txtPort.Text));
   m_pingID := jObject['id'].AsInteger;
  	lblPinging.Caption := 'Pinging';
   imgPingResult.Picture.Clear;
   jObject.Free;
end;

procedure TMinerEditDlg.Timer1Timer(Sender: TObject);
begin
   inc(m_timerTicks);
   if (m_timerTicks * Timer1.Interval) > 6000 then begin
      Timer1.Enabled := false;
      lblPinging.Caption := 'No response!';
      images.GetBitmap(1, imgPingResult.Picture.Bitmap);
   end else begin
     	lblPinging.Caption := 'Pinging ' + LeftStr('...', m_timerTicks mod 4);
	end;
end;


procedure TMinerEditDlg.onSocketReceive(msg: string; ip: string);
var
   jObject : TJSONObject;
begin
  	jObject := TJSONObject(GetJSON(msg));
   try
		if (jObject['data_id'].AsString = 'ping') and (jObject['id'].AsInteger = m_pingID) then begin
      	lblPinging.Caption := 'Success!';
      	images.GetBitmap(0, imgPingResult.Picture.Bitmap);
      	Timer1.Enabled := false;
      end;
   except
   	on e: Exception do
      	Log.Writeln(['Exception in TMinerEditDlg.onSocketReceive : ', e.Message, ', JSON = ', msg]);
	end;
   FreeAndNil(jObject);
end;

procedure TMinerEditDlg.onSocketError(msg: string);
begin
	// nothing here
end;


end.

