
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

unit Main;

{$mode objfpc}{$H+}

{$IfDef Unix}
// for Linux, BSD, Mac OS X, Solaris
{$ENDIF}

{$IfDef Win32}
// for Windows
{$ENDIF}

{$IfDef Windows}
// for Windows
{$ENDIF}

{$IfDef Linux}
// for Linux,
{$ENDIF}

{$IfDef Darwin}
// for OS X,
{$ENDIF}

interface

	// our default listening port is 5226, sending is 5225

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ActnList, USocket;

const
  
	MVis_Version = '1.0.0';
	
   DONATION_ENS = 'mining-visualizer.eth';
   DONATION_ETH = '0xA804e933301AA2C919D3a9834082Cddda877C205  (ETH)';
   DONATION_ETC = '0x29224Be72851D7Bad619f64c2E51E8Ca5Ba1094b  (ETC)';
   
type

	{ TLogView }

TLogView = class(TForm)
	actExit: TAction;
	actStartMinimized: TAction;
	actRestore: TAction;
	actRefreshGauge: TAction;
	actPreferences: TAction;
	actions: TActionList;
	btnClear: TButton;
	btnCopy: TButton;
	menuHelp: TMenuItem;
	mnuWebSite: TMenuItem;
	mnuContents: TMenuItem;
	txtDonation1: TEdit;
	txtDonation2: TEdit;
	txtDonation3: TEdit;
	Label1: TLabel;
	MenuItem1: TMenuItem;
	MenuItem2: TMenuItem;
	menuStartMinimizedTray: TMenuItem;
	menuSeparator1: TMenuItem;
	menuExit: TMenuItem;
	menuFile: TMenuItem;
	menuSeparator2: TMenuItem;
	menuStartMinimized: TMenuItem;
	Panel1: TPanel;
	Timer1: TTimer;
	trayExit: TMenuItem;
	menuRestore: TMenuItem;
	menuRefreshGauge: TMenuItem;
	menuTestKeepAlive: TMenuItem;
	popupTray: TPopupMenu;
	TrayIcon1: TTrayIcon;
	menu1: TMainMenu;
	menuTests: TMenuItem;
	menuTestScheduler: TMenuItem;
	mnuPreferences: TMenuItem;
	mnuTools: TMenuItem;
	txtLog: TMemo;
	procedure actExitExecute(Sender: TObject);
 procedure actPreferencesExecute(Sender: TObject);
	procedure actRefreshGaugeExecute(Sender: TObject);
	procedure actRestoreExecute(Sender: TObject);
	procedure actStartMinimizedExecute(Sender: TObject);
	procedure btnClearClick(Sender: TObject);
	procedure btnCopyClick(Sender: TObject);
	procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
	procedure FormCreate(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FormWindowStateChange(Sender: TObject);
	procedure menuFileClick(Sender: TObject);
	procedure menuTestKeepAliveClick(Sender: TObject);
	procedure menuTestSchedulerClick(Sender: TObject);
	procedure mnuContentsClick(Sender: TObject);
	procedure mnuWebSiteClick(Sender: TObject);
	procedure onSocketReceive(msg: string; ip: string);
	procedure onSocketError(msg: string);
	procedure Timer1Timer(Sender: TObject);
	procedure txtDonation1Change(Sender: TObject);
	procedure txtDonation2Change(Sender: TObject);
	procedure txtDonation3Change(Sender: TObject);

public
	procedure LogMsg(msg: string);

end;

var
	logView: TLogView;

implementation

{$R *.lfm}

uses uLog, frmPreferences, uGlobals, LCLType, DateUtils, fpjson, LCLIntf;

var
   firstShow: boolean;		// you can also use form.FormState for this
   
procedure TLogView.FormCreate(Sender: TObject);
begin
   
	if not programInit then begin
      Application.Terminate;
      exit;
	end;
   
   if g_miners.count = 0 then
      LogMsg('Use Tools/Preferences to set up your miners');
   
	//g_socket.addListener(ETReceive, TMethod(@onSocketReceive));
   g_socket.addListener(ETError, TMethod(@onSocketError));

   if g_settings.getBool('DevEnv') then
      menuTests.Visible := true;

   firstShow := true;
   menuStartMinimized.Checked := g_settings.getBool('StartMinimized');
   menuStartMinimizedTray.Checked := g_settings.getBool('StartMinimized');
   
   // version in title bar
   Caption := 'Mining Visualizer   v' + MVis_Version;

	{$IfDef Darwin}
   Panel1.Font.Name := 'Lucida Grande';
	Panel1.Font.Size := 11;
   // on the Mac, setting ReadOnly to true also prevents the user from
   // selecting and copying text, which we want them to be able to do.
   // we prevent changing in the onChange event handler.
   txtDonation1.ReadOnly := false;
   txtDonation2.ReadOnly := false;
   txtDonation3.ReadOnly := false;
	{$ENDIF}
   
   {$IfDef Linux}
   txtDonation1.color := GetRGBColorResolvingParent;
   txtDonation2.color := GetRGBColorResolvingParent;
   txtDonation3.color := GetRGBColorResolvingParent;
   {$EndIf}
   
   txtDonation1.Text := DONATION_ENS;
   txtDonation2.Text := DONATION_ETH;
   txtDonation3.Text := DONATION_ETC;
end;

procedure TLogView.FormShow(Sender: TObject);
begin
   if firstShow and (g_settings.getBool('StartMinimized') or Application.HasOption('tray')) then begin
		Hide;
		WindowState := wsMinimized;
		ShowInTaskBar := stNever;
	end;
   firstShow := false;
end;

procedure TLogView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   Log.Writeln(['Trace: TLogView.FormClose'], true);
   //g_socket.removeListener(ETReceive, TMethod(@onSocketReceive));
   g_socket.removeListener(ETError, TMethod(@onSocketError));
   programShutdown;
end;

procedure TLogView.btnClearClick(Sender: TObject);
begin
   txtLog.Text := '';
end;

procedure TLogView.btnCopyClick(Sender: TObject);
begin
   txtLog.SelectAll;
   txtLog.CopyToClipboard;
end;

// ******************************************************************************************
// ACTIONS
// ******************************************************************************************
procedure TLogView.actPreferencesExecute(Sender: TObject);
var
   form: TPreferencesDlg;
begin
	form := TPreferencesDlg.Create(Nil);
	form.ShowModal;
   if form.needsRestart then Close;
	FreeAndNil(form);
end;

procedure TLogView.actExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TLogView.actRefreshGaugeExecute(Sender: TObject);
begin
   if g_miners.onlineCount > 0 then
		g_collector.gaugeRefresh;
end;

procedure TLogView.LogMsg(msg: string);
begin
   if txtLog.Lines.count > 2000 then
   	txtLog.Clear;

   msg := FormatDateTime('"d"d hh:nn:ss', Now) + ' - ' + msg;

{$IfDef Darwin}
   msg := msg + #13;
   if (txtLog.Lines.count > 0) and (Trim(txtLog.Lines[txtLog.Lines.count - 1]) = '') then
      txtLog.lines[txtLog.Lines.count - 1] := msg
   else
{$ENDIF}
	txtLog.Append(msg);
end;

procedure TLogView.menuTestSchedulerClick(Sender: TObject);
begin

end;

procedure TLogView.menuTestKeepAliveClick(Sender: TObject);
begin

end;

procedure TLogView.mnuContentsClick(Sender: TObject);
begin
	OpenURL('https://github.com/mining-visualizer/Mining-Visualizer/wiki');
end;

procedure TLogView.mnuWebSiteClick(Sender: TObject);
begin
	OpenURL('https://github.com/mining-visualizer/Mining-Visualizer');
end;

procedure TLogView.onSocketReceive(msg: string; ip: string);
begin
	LogMsg('IN : ' + msg);
end;

procedure TLogView.onSocketError(msg: string);
begin
   LogMsg('ERR : ' + msg);
end;

procedure TLogView.Timer1Timer(Sender: TObject);
begin
   if g_widgetData.widgetOnline then begin
      Timer1.Enabled := false;
		g_collector.gaugeRefresh;
   end;
end;

procedure TLogView.txtDonation1Change(Sender: TObject);
begin
   txtDonation1.Text := DONATION_ENS;
end;

procedure TLogView.txtDonation2Change(Sender: TObject);
begin
   txtDonation2.Text := DONATION_ETH;
end;

procedure TLogView.txtDonation3Change(Sender: TObject);
begin
   txtDonation3.Text := DONATION_ETC;
end;

//  System Tray stuff

procedure TLogView.actRestoreExecute(Sender: TObject);
begin
   Show;
	WindowState := wsNormal;
   ShowInTaskBar := stAlways;
end;

procedure TLogView.actStartMinimizedExecute(Sender: TObject);
var
   startMinimized: boolean;
begin
   startMinimized := not g_settings.getBool('StartMinimized');
   g_settings.putValue('StartMinimized', startMinimized);
   menuStartMinimized.Checked := startMinimized;
   menuStartMinimizedTray.Checked := startMinimized;
end;

procedure TLogView.FormWindowStateChange(Sender: TObject);
begin
	if WindowState = TWindowState.wsMinimized then begin
		Hide;
		WindowState := wsNormal;
		ShowInTaskBar := stNever;
	end;
end;

procedure TLogView.menuFileClick(Sender: TObject);
begin

end;



end.

