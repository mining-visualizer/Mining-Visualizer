
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

unit frmPreferences;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Grids, ActnList,
	ExtCtrls, EditBtn, USocket, Types, uMiners;

const
   NameCol = 0;
	IPCol = 1;
   PortCol = 2;
   InactiveCol = 3;

   SecondsPerWeek = 604800;
   SecondsPerDay = 86400;
   SecondsPerHour = 3600;
   
type

	{ TPreferencesDlg }

 TPreferencesDlg = class(TForm)
		actAddRow: TAction;
		actDeleteRow: TAction;
		actCancel: TAction;
		actMoveUp: TAction;
		actMoveDown: TAction;
		actEdit: TAction;
		actOk: TAction;
		actions1: TActionList;
		btnClose: TButton;
		chkAlerts: TCheckBox;
		chkLogin: TCheckBox;
		chkUseSSL: TCheckBox;
		cmbFrequencyUnitWU: TComboBox;
		cmbFrequencyUnitCH: TComboBox;
		Label25: TLabel;
		Label26: TLabel;
		txtAlertAction: TEdit;
		Label22: TLabel;
		Label23: TLabel;
		Label24: TLabel;
		Shape9: TShape;
		tabAlerts: TTabSheet;
		txtAlertWait: TEdit;
		txtPassword: TEdit;
		txtCertBundle: TFileNameEdit;
		txtPort: TEdit;
		txtServerCert: TFileNameEdit;
		txtServerKey: TFileNameEdit;
		Label1: TLabel;
		Label12: TLabel;
		Label17: TLabel;
		Label2: TLabel;
		Label21: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label7: TLabel;
		Shape8: TShape;
		txtHashRateDeviation: TEdit;
		grpTempDisplay: TRadioGroup;
		Label13: TLabel;
		Label16: TLabel;
		Label18: TLabel;
		Label19: TLabel;
		Label20: TLabel;
		Label9: TLabel;
		Shape6: TShape;
		Shape7: TShape;
		trackScale: TTrackBar;
		Label14: TLabel;
		Label15: TLabel;
		Shape5: TShape;
		txtFrequency: TEdit;
		Label10: TLabel;
		Label11: TLabel;
		txtDifficulty: TLabel;
		Shape4: TShape;
		tabCloseHits: TTabSheet;
		txtUDPListen: TEdit;
		txtUdpPassword: TEdit;
		txtWorkUnitFrequency: TEdit;
		Label8: TLabel;
		grpHashRates: TRadioGroup;
		Shape3: TShape;
		tabDesktop: TTabSheet;
		tbnEdit: TToolButton;
		tbnMoveUp: TToolButton;
		tbnMoveDown: TToolButton;
		txtMinerMask: TEdit;
		images1: TImageList;
		Label3: TLabel;
		Label6: TLabel;
		lstNavigation: TListBox;
		gridMiners: TStringGrid;
		pgsSettings: TPageControl;
		Shape1: TShape;
		Shape2: TShape;
		tabMiners: TTabSheet;
		tabWebFace: TTabSheet;
		ToolBar1: TToolBar;
		tbnAdd: TToolButton;
		tbnDelete: TToolButton;
		procedure actAddRowExecute(Sender: TObject);
		procedure actDeleteRowExecute(Sender: TObject);
		procedure actEditExecute(Sender: TObject);
		procedure actMoveDownExecute(Sender: TObject);
		procedure actMoveUpExecute(Sender: TObject);
		procedure btnCloseClick(Sender: TObject);
		procedure chkAlertsChange(Sender: TObject);
		procedure chkLoginChange(Sender: TObject);
		procedure chkUseSSLChange(Sender: TObject);
		procedure cmbFrequencyUnitCHSelect(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
		procedure FormCreate(Sender: TObject);
		procedure gridMinersPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
		procedure lstNavigationSelectionChange(Sender: TObject; User: boolean);
		procedure pgsSettingsChange(Sender: TObject);
		procedure pgsSettingsChanging(Sender: TObject; var AllowChange: Boolean);
		procedure trackScaleChange(Sender: TObject);
		procedure trackScaleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure trackScaleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure trackScaleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
		procedure FileNameEditChange(Sender: TObject);
		procedure txtFrequencyChange(Sender: TObject);

 public
      needsRestart: boolean;
      
 private
		procedure saveSettings;
      function settingsOk : boolean;
      procedure calcDifficulty;
      procedure setCloseHitComboItems(combo: TComboBox);
      procedure loadCloseHitValues(path: string; textbox: TEdit; combo: TComboBox);
      function newMinerID: integer;
      function calcPageHash: string;

 private
 		m_mouseDown: boolean;
      m_minerCount: integer;
      m_sslNeedsRestart: boolean;
      // this is not a true hash, just a mash-up of all the values on the page, giving us
      // an easy way to detect if the user changed anything.
      m_pageHash: string;	
      
end;


implementation

uses ULog, fpjson, jsonparser, UGlobals, uLib, LCLType, frmMinerEdit, uMiner, uMisc, strutils, Main;

{$R *.lfm}

var
   pageLeaving: integer;
   formLoaded: boolean;		// you might also be able to use form.FormState for this
   

procedure TPreferencesDlg.FormCreate(Sender: TObject);
var
   i: integer;
   jArray: TJSONArray;
   jObject: TJSONObject;
   miner: CMiner;
begin
   formLoaded := false;
   needsRestart := false;
   m_sslNeedsRestart := false;
	Randomize;
   pgsSettings.ActivePageIndex := 0;

   // load up the left side navigation pane
   lstNavigation.AddItem('Miners', TObject(tabMiners));
   lstNavigation.AddItem('Desktop', TObject(tabDesktop));
   lstNavigation.AddItem('Web App', TObject(tabWebFace));
   lstNavigation.AddItem('Close Hits', TObject(tabCloseHits));
   lstNavigation.AddItem('Alerts', TObject(tabAlerts));
   
   lstNavigation.ItemIndex := 0;
   
   // ----------  load up the miner grid  ---------------------
   
   // set the number of rows in the grid
   gridMiners.RowCount := 1;  
   
   // load miner info from the .json file
	jArray := TJSONArray(g_settings.getValue('Miners', TJSONArray));
   m_minerCount := jArray.Count;
   
   // fill up the grid
	for i := 0 to jArray.Count - 1 do begin
      jObject := jArray.Objects[i];
      miner := CMiner.Create(jObject);
      try
	      gridMiners.InsertRowWithValues(
         		i + 1,
               [
               	jObject['miner'].AsString,
               	jObject['ip'].AsString,
               	jObject['port'].AsString
               ]
         );
         // store the miner object as part of the grid row, we'll use it later to write back 
         // to the config.json file, or when editing the miner.
         gridMiners.Objects[NameCol, i + 1] := miner;
         
		except
	   	on e: Exception do begin
				MessageBox2(PChar('Exception in TPreferencesDlg.FormCreate - ' + e.Message + LineEnding + 'See log file for complete diagnostics.'), '', MB_ICONERROR);
   			Log.Writeln([PChar('Exception in TPreferencesDlg.FormCreate - ' + e.Message + LineEnding + 'JSON=' + jObject.AsJSON)]);
			end;
		end;
	end;
   jArray.Free;

   if gridMiners.RowCount = 1 then
      gridMiners.RowCount := 2;	// add a blank row to make it look nice
   
   tbnEdit.Enabled := m_minerCount > 0;

   // UDP settings
   txtUDPListen.Text := g_settings.getString('UdpListen');
   txtUdpPassword.Text := g_settings.getValue('UdpPassword', '');
   
   // Desktop widget
   grpHashRates.ItemIndex := g_settings.getInt('HashRates');
   grpTempDisplay.ItemIndex := g_settings.getInt('TemperatureDisplay');
   txtHashRateDeviation.Text := g_settings.getString('HashRateDeviationAlert');
   
   // Close Hits 
   setCloseHitComboItems(cmbFrequencyUnitCH);
   setCloseHitComboItems(cmbFrequencyUnitWU);
   
   loadCloseHitValues('CloseHits.closehit_frequency', txtFrequency, cmbFrequencyUnitCH);
   loadCloseHitValues('CloseHits.workunit_frequency', txtWorkUnitFrequency, cmbFrequencyUnitWU);
   
   trackScale.Position := g_settings.getInt('CloseHits.widget_scale');
   txtMinerMask.Text := g_settings.getString('CloseHits.miner_label');
   
   // Web App
   txtPort.Text := IntToStr(g_settings.getInt('WebInterface.port'));
   chkLogin.Checked := g_settings.getBool('WebInterface.require_login');
   txtPassword.Enabled := chkLogin.Checked;
   if chkLogin.Checked then
   	txtPassword.Text := g_settings.getString('WebInterface.password');
   
   chkUseSSL.Checked := g_settings.getBool('WebInterface.use_ssl');
   txtCertBundle.Enabled := chkUseSSL.Checked;
   txtServerCert.Enabled := chkUseSSL.Checked;
   txtServerKey.Enabled := chkUseSSL.Checked;
   txtCertBundle.Text := g_settings.getString('WebInterface.ca_cert_bundle');
   txtServerCert.Text := g_settings.getString('WebInterface.server_cert');
   txtServerKey.Text := g_settings.getString('WebInterface.server_key');
   
   // Alerts
   chkAlerts.Checked := g_settings.getBool('Alerts.enabled');
   txtAlertAction.Text := g_settings.getString('Alerts.action');
   txtAlertWait.Text := g_settings.getString('Alerts.wait');
   txtAlertAction.Enabled := chkAlerts.Checked;
   txtAlertWait.Enabled := chkAlerts.Checked;
   
   // Miscellaneous
   pageLeaving := 0;
   calcDifficulty;
   m_mouseDown := false;
   formLoaded := true;

   // fonts
	{$IfDef Darwin}
   	Font.Name := 'Lucida Grande';
   	Font.Size := 11;
	{$ENDIF}

   m_pageHash := calcPageHash;   
end;

procedure TPreferencesDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
   CanClose := false;
	if settingsOk then begin
      CanClose := true;
      saveSettings;
      if g_miners.onlineCount > 0 then
         g_collector.countMinersGPUsOnline;
		g_collector.gaugeRefresh;
	end;
end;

procedure TPreferencesDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   i: integer;
begin
   // do a final check for conflicting settings
   if g_miners.count > 1 then begin
   	if grpHashRates.ItemIndex = Ord(IndividualGPUs) then
         g_settings.putValue('HashRates', Ord(HRGrandTotal));
      
   	if grpTempDisplay.ItemIndex = Ord(TDGPUs) then
         g_settings.putValue('TemperatureDisplay', Ord(TDStatus));
	end;

   // free the miner objects that we had associated with each row of the grid.
	for i := 1 to m_minerCount do 
      CMiner(gridMiners.Objects[NameCol, i]).Free;

   // do we need a restart?
   if (StrToInt(txtUDPListen.Text) <> g_socket.listenPort) or m_sslNeedsRestart then begin
      Application.MessageBox('Changes you made require a restart.  Please click Ok to close this program, ' +
      				  'and then restart it manually.', 'Restart required', MB_ICONINFORMATION);
      self.needsRestart := true;
	end;
end;

// this is the list box on the left hand side. the selected item has changed.
procedure TPreferencesDlg.lstNavigationSelectionChange(Sender: TObject; User: boolean);
begin
   pgsSettings.ActivePage := TTabSheet(lstNavigation.Items.Objects[lstNavigation.ItemIndex]);
   pageLeaving := lstNavigation.ItemIndex;
end;

// the tab sheet is going to change pages
procedure TPreferencesDlg.pgsSettingsChanging(Sender: TObject; var AllowChange: Boolean);
begin
   // ignore page changes while the form is being created
   if fsFirstShow in FormState then exit;
   AllowChange := false;
	if settingsOk then begin
      AllowChange := true;
      saveSettings;
	end;
end;

// the tab sheet HAS changed pages
procedure TPreferencesDlg.pgsSettingsChange(Sender: TObject);
var
   i: integer;
begin
   m_pageHash := calcPageHash;
   if pgsSettings.ActivePage = tabDesktop then begin
      // configure hash rate section
      i := Ord(IndividualGPUs);
      if g_miners.count <= 1 then
         grpHashRates.Controls[i].Enabled := true
      else begin
			grpHashRates.Controls[i].Enabled := false;
      	if grpHashRates.ItemIndex = i then
            grpHashRates.ItemIndex := 0;
		end;
      // configure temperature display section
      if g_miners.count <= 1 then
         grpTempDisplay.Controls[Ord(TDGPUs)].Enabled := true
      else begin
			grpTempDisplay.Controls[Ord(TDGPUs)].Enabled := false;
      	if grpTempDisplay.ItemIndex = Ord(TDGPUs) then
            grpTempDisplay.ItemIndex := Ord(TDStatus);
		end;
	end;
end;

// check the settings on the active page to make sure they're ok.
function TPreferencesDlg.settingsOk: boolean;
var
   i: integer;
   miner: CMiner;
begin
   result := false;
   case pgsSettings.ActivePage.Name of 
      
      'tabMiners': begin
			if (txtUDPListen.Text = '') or (not isNumeric(txtUDPListen.Text)) then begin
		      txtUDPListen.SetFocus;
		   	Application.MessageBox('Please enter a valid port UDP number', '', MB_ICONEXCLAMATION);
            lstNavigation.ItemIndex := pageLeaving;
		   	exit;
			end;
         // check to make sure the port # chosen is not being used by one of the miners.
			for i := 1 to m_minerCount do begin
            miner := gridMiners.Objects[NameCol, i] as CMiner;
	         if miner.port = StrToInt(txtUDPListen.Text) then begin
			      txtUDPListen.SetFocus;
			   	Application.MessageBox('That UDP port number is being used by one of the miners.  Please choose a different number.', '', MB_ICONEXCLAMATION);
	            lstNavigation.ItemIndex := pageLeaving;
		   		exit;
				end;
			end;
         
		end;

      'tabDesktop': begin
			if (txtHashRateDeviation.Text = '') or (not isNumeric(txtHashRateDeviation.Text)) then begin
		      txtHashRateDeviation.SetFocus;
		   	Application.MessageBox('Please enter a valid hash rate deviation alert', '', MB_ICONEXCLAMATION);
            lstNavigation.ItemIndex := pageLeaving;
		   	exit;
			end;
		end;

      'tabCloseHits': begin
			if not isNumeric(txtFrequency.Text) then  begin
		      txtFrequency.SetFocus;
		   	Application.MessageBox('Please enter a valid numeric frequency', '', MB_ICONEXCLAMATION);
            lstNavigation.ItemIndex := pageLeaving;
		   	exit;
			end;
			if not isNumeric(txtWorkUnitFrequency.Text) then begin
		      txtWorkUnitFrequency.SetFocus;
		   	Application.MessageBox('Please enter a valid numeric frequency', '', MB_ICONEXCLAMATION);
            lstNavigation.ItemIndex := pageLeaving;
		   	exit;
			end;
		end;
      
      'tabAlerts': begin
         if chkAlerts.Checked then begin
            if Trim(txtAlertAction.Text) = '' then begin
			   	Application.MessageBox('Please enter a valid alert action.  This can be a script, an executable program, or a URL', '', MB_ICONEXCLAMATION);
	            lstNavigation.ItemIndex := pageLeaving;
			   	exit;
				end;
            
				if not isNumeric(txtAlertWait.Text) then begin
			      txtAlertWait.SetFocus;
			   	Application.MessageBox('Please enter a valid number', '', MB_ICONEXCLAMATION);
	            lstNavigation.ItemIndex := pageLeaving;
			   	exit;
				end;

         	// if it's not a URL, check the file exists
            if not AnsiStartsText('http', txtAlertAction.Text) then begin
               if not FileExists(txtAlertAction.Text) then begin
			   		Application.MessageBox('Warning: the specified script / executable does not exists on the local file system.', '', MB_ICONEXCLAMATION);
					end;
				end;
			end;
 		end;

	end;
   
   result := true;
end;

// save settings on the active page
procedure TPreferencesDlg.saveSettings;
var
   jArray: TJSONArray;
   i, mult: integer;
   x: double;
   closeHitThreshold: QWord;
   miner: CMiner;
begin
   if calcPageHash = m_pageHash then exit;
	try
      case pgsSettings.ActivePage.Name of 
      	'tabMiners': begin
			   jArray := TJSONArray.Create;
				for i := 1 to m_minerCount do begin
               miner := gridMiners.Objects[NameCol, i] as CMiner;
		         jArray.Add(miner.AsJSONObject);
				end;
            try
					g_settings.beginUpdate;
				   g_settings.putValue('Miners', jArray);
					g_settings.putValue('UdpListen', StrToInt(txtUDPListen.Text));
	            g_settings.putValue('UdpPassword', txtUdpPassword.Text);
	            g_collector.updateWidgetTemps;
	            g_collector.setHashRateAlert;
				finally
					g_settings.endUpdate;
			   	jArray.Free;
				end;
			end;

         'tabWebFace': begin
            try
					g_settings.beginUpdate;
	            g_settings.putValue('WebInterface', 'port', StrToInt(txtPort.Text));
	            g_settings.putValue('WebInterface', 'require_login', chkLogin.Checked);
            	g_settings.putValue('WebInterface', 'password', txtPassword.Text);
	            g_settings.putValue('WebInterface', 'use_ssl', chkUseSSL.Checked);
	            g_settings.putValue('WebInterface', 'ca_cert_bundle', txtCertBundle.Text);
	            g_settings.putValue('WebInterface', 'server_cert', txtServerCert.Text);
	            g_settings.putValue('WebInterface', 'server_key', txtServerKey.Text);
				finally
					g_settings.endUpdate;
				end;
			end;
         
         'tabDesktop': begin
            try
					g_settings.beginUpdate;
					g_settings.putValue('HashRates', grpHashRates.ItemIndex);
	            g_collector.assembleHashRates(0);
	            g_settings.putValue('TemperatureDisplay', grpTempDisplay.ItemIndex);
               g_settings.putValue('HashRateDeviationAlert', txtHashRateDeviation.Text);
	            g_collector.updateWidgetTemps;
               g_collector.setHashRateAlert;
				finally
					g_settings.endUpdate;
				end;
			end;

         'tabCloseHits': begin
            try
               Screen.Cursor := crHourGlass;
					g_settings.beginUpdate;
	            x := StrToFloat(txtFrequency.Text);
	            mult := StrToInt(string(cmbFrequencyUnitCH.Items.Objects[cmbFrequencyUnitCH.ItemIndex]));
	            g_settings.putValue('CloseHits', 'closehit_frequency', Trunc(x * mult));
	            x := StrToFloat(txtWorkUnitFrequency.Text);
	            mult := StrToInt(string(cmbFrequencyUnitWU.Items.Objects[cmbFrequencyUnitWU.ItemIndex]));
	            g_settings.putValue('CloseHits', 'workunit_frequency', Trunc(x * mult));

	            // send the new settings to any connected miners
			   	closeHitThreshold := g_miningData.calcCloseHitThreshold(g_miners.expectedHashRate, g_settings.getInt('CloseHits.closehit_frequency'));
	            for i := 0 to g_miners.count - 1 do
	               if g_miners.byRow[i].online then
			         	g_minerRPC.setCloseHitThresholds(g_miners.byRow[i].ip, g_miners.byRow[i].port,
	                  											closeHitThreshold, g_settings.getInt('CloseHits.workunit_frequency'));

					g_settings.putValue('CloseHits', 'miner_label', txtMinerMask.Text);
	            if g_miners.onlineCount > 0 then
						g_collector.gaugeRefresh;
				finally
					g_settings.endUpdate;
               Screen.Cursor := crDefault;
				end;
			end;
         
         'tabAlerts': begin
            try
					g_settings.beginUpdate;
               g_settings.putValue('Alerts', 'enabled', chkAlerts.Checked);
               g_settings.putValue('Alerts', 'action', txtAlertAction.Text);
               g_settings.putValue('Alerts', 'wait', txtAlertWait.Text);
				finally
					g_settings.endUpdate;
				end;
			end;

		end;

	except
   	on e: Exception do 
			MessageBox2(PChar('Exception in TPreferencesDlg.saveSettings : ' + e.Message), '', MB_ICONERROR);
	end;
end;

// this is not a true hash, just a mash-up of all the values on the page, giving us
// an easy way to detect if the user changed anything.
function TPreferencesDlg.calcPageHash: string;
var
   miner: CMiner;
   i: integer;
begin
   result := '';
   case pgsSettings.ActivePage.Name of
   	'tabMiners': begin
			for i := 1 to m_minerCount do begin
            miner := gridMiners.Objects[NameCol, i] as CMiner;
            result += result + miner.AsJSONObject.AsJSON;
			end;
         result += '|' + txtUDPListen.Text + '|' + txtUdpPassword.Text + '|';
		end;

      'tabWebFace': begin
         result := '|' + txtPort.Text + '|' + IntToStr(integer(chkLogin.Checked)) + '|' + txtPassword.Text + '|' + IntToStr(integer(chkUseSSL.Checked)) + '|' + 
         					txtCertBundle.Text + '|' + txtServerCert.Text + '|' + txtServerKey.Text + '|';
		end;

      'tabDesktop': begin
         result := '|' + IntToStr(grpHashRates.ItemIndex) + '|' + IntToStr(grpTempDisplay.ItemIndex) + '|' + txtHashRateDeviation.Text + '|';
		end;

      'tabCloseHits': begin
         result := '|' + txtFrequency.Text + '|' + IntToStr(cmbFrequencyUnitCH.ItemIndex) + '|' + txtWorkUnitFrequency.Text + '|' + 
         					IntToStr(cmbFrequencyUnitWU.ItemIndex) + '|' + txtMinerMask.Text + '|' + IntToStr(trackScale.Position) + '|';
		end;

      'tabAlerts': begin
         result := '|' + IntToStr(integer(chkAlerts.Checked)) + '|' + txtAlertAction.Text + '|' + txtAlertWait.Text + '|';
		end;
	end;
   //Log.Writeln(['calcPageHash ', result]);
end;




// ==================  MINERS TAB  ====================

procedure TPreferencesDlg.actAddRowExecute(Sender: TObject);
var
   dlgMinerEdit: TMinerEditDlg;
   res: integer;
   miner: CMiner;
begin
   Log.Writeln(['Trace: TPreferencesDlg.actAddRowExecute'], true);
   miner := CMiner.Create;
   miner.id := newMinerID;
   dlgMinerEdit := TMinerEditDlg.CreateWithMiner(self, miner);
   res := dlgMinerEdit.ShowModal;
   if res <> mrOK then begin
   	miner.Free;      
	end
	else begin
      if m_minerCount = 0 then
         // if there are no miners, we added a blank row at form creation to make it look better.
         gridMiners.DeleteRow(1);
      
      with miner do begin
      	gridMiners.InsertRowWithValues(gridMiners.RowCount, [name, ip, IntToStr(port)]);
         gridMiners.Objects[NameCol, gridMiners.RowCount - 1] := miner;
         if not miner.inactive then begin
	         g_miners.Add(miner.Clone);
            g_collector.pollOfflineMiners(0);
			end;
      end;
      gridMiners.Invalidate;
      gridMiners.Update;
      Inc(m_minerCount);
      gridMiners.row := gridMiners.RowCount;
      gridMiners.col := NameCol;
      gridMiners.SetFocus;
	end;
   tbnEdit.Enabled := m_minerCount > 0;
   dlgMinerEdit.Free;
end;

// return a new, unused id value.
function TPreferencesDlg.newMinerID: integer;
var
   i: integer;
   maxID: integer;
   miner: CMiner;
begin
   maxID := 0;
	for i := 1 to m_minerCount do begin
      miner := gridMiners.Objects[NameCol, i] as CMiner;
   	if miner.id > maxID then
         maxID := miner.id;
	end;
   result := maxID + 1;
end;

procedure TPreferencesDlg.actDeleteRowExecute(Sender: TObject);
var
   gridMiner, activeMiner: CMiner;
   res: integer;
begin
   Log.Writeln(['Trace: TPreferencesDlg.actDeleteRowExecute'], true);
   if m_minerCount = 0 then exit;
   
   res := Application.MessageBox('Are you sure you want to delete this miner?', 'Delete Miner', MB_ICONQUESTION + MB_YESNOCANCEL);
   if res <> IDYES then
      exit;
   
   gridMiner := gridMiners.Objects[NameCol, gridMiners.Row] as CMiner;
   if not gridMiner.inactive then begin
	   // note: the miner we store in the grid is not the same instance as the the miner
      // in g_miners[], and their operational fields might be different, but their 
      // persisted fields should be the same.
      activeMiner := g_miners.byID[gridMiner.id];
      if activeMiner.online then begin
		   activeMiner.removeOnDisconnect := true;
			g_minerRPC.disconnect(activeMiner.ip, activeMiner.port);
      end 
      else
         g_miners.Remove(activeMiner.id);
	end;
   gridMiner.Free;
   gridMiners.DeleteRow(gridMiners.Row);
   Dec(m_minerCount);
   if gridMiners.RowCount = 1 then
      gridMiners.RowCount := 2;	// add a blank row
   tbnEdit.Enabled := m_minerCount > 0;
end;

procedure TPreferencesDlg.actEditExecute(Sender: TObject);
var
   dlgMinerEdit: TMinerEditDlg;
   res, gridRow: integer;
   gridMiner, activeMiner: CMiner;
   wasInactive: boolean;
begin
   Log.Writeln(['Trace: TPreferencesDlg.actEditExecute'], true);
   gridRow := gridMiners.Row;
   
   // grab the CMiner object out of the grid
   gridMiner := CMiner(gridMiners.Objects[NameCol, gridRow]);
   wasInactive := gridMiner.inactive;
   
   // display the dialog
   dlgMinerEdit := TMinerEditDlg.CreateWithMiner(self, gridMiner);
   res := dlgMinerEdit.ShowModal;
   
   if res = mrOK then begin
      // update the values in the grid with the changed values the user provided.
	   gridMiners.Cells[NameCol, gridRow] := gridMiner.name;
	   gridMiners.Cells[IPCol, gridRow] := gridMiner.ip;
	   gridMiners.Cells[PortCol, gridRow] := IntToStr(gridMiner.port);
      gridMiners.Invalidate;
      gridMiners.Update;
      
      if wasInactive and (not gridMiner.inactive) then begin
         // the user just activated this miner
         g_miners.Add(gridMiner.Clone);
         g_collector.pollOfflineMiners(0);
		end
      else if (not wasInactive) and gridMiner.inactive then begin
         // the user just deactivated this miner.
	      activeMiner := g_miners.byID[gridMiner.id];
         if activeMiner.online then begin
			   activeMiner.removeOnDisconnect := true;
				g_minerRPC.disconnect(activeMiner.ip, activeMiner.port);
			end
         else begin
         	g_miners.Remove(gridMiner.id);
			end;
		end
      else if not gridMiner.inactive then begin
         // the miner was active before and still is
	      activeMiner := g_miners.byID[gridMiner.id];
	      if activeMiner.online then
				g_minerRPC.disconnect(activeMiner.ip, activeMiner.port);
         with activeMiner do begin
         	name := gridMiner.name;
			   ip := gridMiner.ip;
			   port := gridMiner.port;
			   gpuCountDeclared := gridMiner.gpuCountDeclared;
			   expectedHashRate := gridMiner.expectedHashRate;
            neverExceedTemp := gridMiner.neverExceedTemp;
            safetyShutdown := gridMiner.safetyShutdown;
            tempRangeHi := gridMiner.tempRangeHi;
            hashFaultsAlert := gridMiner.hashFaultsAlert;
			end;
         // poll offline miners in 3 seconds.
         g_collector.pollOfflinerMinersIn(3000);
		end;
	end;
   dlgMinerEdit.Free;
end;

procedure TPreferencesDlg.actMoveDownExecute(Sender: TObject);
begin
   if gridMiners.Row < gridMiners.RowCount - 1 then begin
		gridMiners.MoveColRow(false, gridMiners.Row, gridMiners.Row + 1);
	end;
end;

procedure TPreferencesDlg.actMoveUpExecute(Sender: TObject);
begin
   if gridMiners.Row > 1 then begin
		gridMiners.MoveColRow(false, gridMiners.Row, gridMiners.Row - 1);
	end;
end;

procedure TPreferencesDlg.btnCloseClick(Sender: TObject);
begin
   Close;
end;


// ================  WEB APP TAB  ====================

procedure TPreferencesDlg.chkLoginChange(Sender: TObject);
begin
	if chkLogin.Checked then begin
	   txtPassword.Enabled := true;
   	txtPassword.Text := g_settings.getString('WebInterface.password');
	end else begin
	   txtPassword.Enabled := false;
   	txtPassword.Text := '';
	end;
end;

procedure TPreferencesDlg.chkUseSSLChange(Sender: TObject);
begin
   txtCertBundle.Enabled := chkUseSSL.Checked;
   txtServerCert.Enabled := chkUseSSL.Checked;
   txtServerKey.Enabled := chkUseSSL.Checked;
   m_sslNeedsRestart := true;
end;

procedure TPreferencesDlg.FileNameEditChange(Sender: TObject);
var
   ctrl: TFileNameEdit;
   path: string;
begin
   // this handles the 3 file name edits on the Web App page.
   if not formLoaded then exit;
   ctrl := TFileNameEdit(Sender);
   path := ExtractFileDir(ctrl.Text);
   txtCertBundle.InitialDir := path;
   txtServerCert.InitialDir := path;
   txtServerKey.InitialDir := path;
   m_sslNeedsRestart := true;
end;


// ================  CLOSE HITS TAB  ====================

procedure TPreferencesDlg.txtFrequencyChange(Sender: TObject);
begin
   if not formLoaded then exit;
   calcDifficulty;
end;


procedure TPreferencesDlg.cmbFrequencyUnitCHSelect(Sender: TObject);
begin
   if not formLoaded then exit;
   calcDifficulty;
end;

procedure TPreferencesDlg.calcDifficulty;
var
   x: double;
   mult, seconds: integer;
   difficulty, divisor: QWord;
begin
   if not isNumeric(txtFrequency.Text) then exit;
   x := StrToFloat(txtFrequency.Text);
   mult := StrToInt(string(cmbFrequencyUnitCH.Items.Objects[cmbFrequencyUnitCH.ItemIndex]));
   seconds := Trunc(x * mult);
   if seconds = 0 then exit;
   divisor := Trunc(g_miners.expectedHashRate * 1000000 * seconds);
   if divisor = 0 then exit;
   difficulty := Trunc(High(QWord) / divisor);
   txtDifficulty.Caption := 'Estimated Target Threshold: ' + AddCommas(difficulty);
end;

procedure TPreferencesDlg.setCloseHitComboItems(combo: TComboBox);
var
   s: string;
begin
   combo.Clear;
   s := '60';
   combo.AddItem('Minutes', TObject(s));
   s := '3600';
   combo.AddItem('Hours', TObject(s));
   s := '86400';
   combo.AddItem('Days', TObject(s));
   s := '604800';
   combo.AddItem('Weeks', TObject(s));
end;

procedure TPreferencesDlg.loadCloseHitValues(path: string; textbox: TEdit; combo: TComboBox);
var
   seconds, days: integer;
begin
   // we store seconds in the config file
   seconds := g_settings.getInt(path);
   days := seconds div SecondsPerDay;
   if days > 0 then begin
      if days mod 7 = 0 then begin
         // it is an even number of weeks
	      textbox.Text := IntToStr(days div 7);
	      combo.Text := 'Weeks';
		end
      else if (days * SecondsPerDay = Seconds) or (days < 7) then begin
         // it's an even number of days, or less than 1 week
	      textbox.Text := FloatToStr(seconds / SecondsPerDay);
	      combo.Text := 'Days';
		end
      else begin
         // display as weeks
	      textbox.Text := FloatToStr(seconds / SecondsPerWeek);
	      combo.Text := 'Weeks';
		end;
	end
   else if seconds div SecondsPerHour > 0 then begin
      textbox.Text := FloatToStr(seconds / SecondsPerHour);
      combo.Text := 'Hours';
	end
   else begin
      textbox.Text := FloatToStr(seconds / 60);
      combo.Text := 'Minutes';
	end;
end;

procedure TPreferencesDlg.trackScaleChange(Sender: TObject);
begin
   if not m_mouseDown then begin
   	g_settings.putValue('CloseHits', 'widget_scale', trackScale.Position);
      if g_miners.onlineCount > 0 then
			g_collector.gaugeRefresh;
	end;
end;

procedure TPreferencesDlg.trackScaleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   m_mouseDown := true;
end;

procedure TPreferencesDlg.trackScaleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   m_mouseDown := false;
   trackScaleChange(Sender);
end;

procedure TPreferencesDlg.trackScaleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	Handled := true;
end;


// ================  ALERTS TAB  ====================

procedure TPreferencesDlg.chkAlertsChange(Sender: TObject);
begin
   txtAlertAction.Enabled := chkAlerts.Checked;
   txtAlertWait.Enabled := chkAlerts.Checked;
end;


procedure TPreferencesDlg.gridMinersPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
   if (aRow > 0) and Assigned(gridMiners.Objects[NameCol, aRow]) and CMiner(gridMiners.Objects[NameCol, aRow]).inactive then
      gridMiners.Canvas.Font.Color := clSilver;
      
end;




end.

