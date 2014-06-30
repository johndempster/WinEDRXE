unit Setup;
{ ====================================================================
  WinEDR : Digital recording parameters setup dialog.
  (c) J. Dempster, University of Strathclyde 1996-99, All Rights Reserved
  1/9/99 ... CalibFactorToADCScale added to allow Channe calib. to change
  13/7/01 ... Sampling interval now updated even if return key not pressed
  20/7/01 ... Channel calibrations in table now retained when no. channels
              and sampling interval changed.
  24.06.03 No. of horizontal & vertical grid lines added
  3.12.03  MaxADCValue now preserved when Setup called
  3.03.04  Setup can now handle 12 channels
  14.04.04 VP500 channel settings set automatically when interface selected
  05.08.04 Telegraph channel # now only appears when needed
  08.03.05 Command Voltage Divide factor left unchanged with no amplifier selected
  10.05.05 UpdateAmplifierSettings modified to prevent changes to channel
           scale factor being made to existing data files
  30.07.06 Support for mode telegraph channel added
           Command voltage left unchanged in Manual amplifier gain entry
           Settings saved in RecChannel and Settings (in global.pas)
           No longer changes CDRFH file header`at all

  14.07.08  Amplifier number now stored as object in amplifier list rather than index
            Allowing amplifiers to be listed in a better order.

  ====================================================================}
interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Spin, ExtCtrls, shared, sysUtils, Grids, Dialogs, Global, FileIo,
  ValEdit, ValidatedEdit, Math ;

type
  TSetupDlg = class(TForm)
    GroupBox1: TGroupBox;
    lNumChannels: TLabel;
    Label1: TLabel;
    Channels: TGroupBox;
    ChannelTable: TStringGrid;
    Label2: TLabel;
    bOK: TButton;
    bCancel: TButton;
    GroupBox2: TGroupBox;
    edNumChannels: TValidatedEdit;
    edSamplingInterval: TValidatedEdit;
    cbADCVoltageRange: TComboBox;
    Label5: TLabel;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    edNumHorizontalGridLines: TValidatedEdit;
    edNumVerticalGridLines: TValidatedEdit;
    rbTmsecs: TRadioButton;
    rbTSecs: TRadioButton;
    Label4: TLabel;
    grp1902: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    cbAmplifier1: TComboBox;
    cbAmplifier2: TComboBox;
    GainTelegraphPanel2: TPanel;
    Label10: TLabel;
    edGainTelegraphChannel2: TValidatedEdit;
    ModeTelegraphPanel1: TPanel;
    Label12: TLabel;
    edModeTelegraphChannel1: TValidatedEdit;
    ModeTelegraphPanel2: TPanel;
    Label13: TLabel;
    edModeTelegraphChannel2: TValidatedEdit;
    GainTelegraphPanel1: TPanel;
    Label14: TLabel;
    edGainTelegraphChannel1: TValidatedEdit;
    NIPanel: TPanel;
    Label3: TLabel;
    Label11: TLabel;
    cbDeviceNumber: TComboBox;
    cbADCInputMode: TComboBox;
    cbLabInterface: TComboBox;
    procedure bOKClick(Sender: TObject);
    procedure rbTmsecsClick(Sender: TObject);
    procedure rbTSecsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLabInterfaceChange(Sender: TObject);
    procedure EdSamplingIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure edNumChannelsKeyPress(Sender: TObject; var Key: Char);
    procedure cbAmplifiersChange(Sender: TObject);
    procedure cbAmplifier1Change(Sender: TObject);
    procedure cbDeviceNumberChange(Sender: TObject);
    procedure cbADCInputModeChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateNumChannels ;
    procedure UpdateSamplingInterval ;
    procedure UpdateChannelCalibration ;
    procedure UpdateAmplifierSettings ;
    procedure ReopenLabInterface ;
  public
    { Public declarations }
  end;

const
     ChNum = 0 ;
     ChName = 1 ;
     ChCal = 2 ;
     ChUnits = 3 ;
var
  SetupDlg: TSetupDlg;
  UpdateOK : boolean ;
  OKorCancelButtonClicked : boolean ;

implementation

{$R *.DFM}
Uses maths, mdiform,ampModule, seslabio ;


procedure TSetupDlg.FormShow(Sender: TObject);
{ --------------------------------------------------------------------------
  Initialise setup's combo lists and tables with current recording parameters
  --------------------------------------------------------------------------}
var
   i : Integer ;
begin

     { Stop laboratory interface activity }
     if Main.SESLabIO.ADCActive then Main.SESLabIO.ADCStop ;
     if Main.SESLabIO.DACActive then Main.SESLabIO.DACStop ;

     Main.SESLabIO.GetLabInterfaceTypes( cbLabInterface.Items ) ;
     cbLabInterface.ItemIndex := cbLabInterface.Items.IndexOfObject(TObject(Settings.LaboratoryInterface)) ;
     Main.SESLabIO.OpenLabInterface( Settings.LaboratoryInterface,
                                     Settings.DeviceNumber,
                                     Settings.ADCInputMode ) ;

     // A/D channel input mode
     Main.SESLABIO.GetADCInputModes( cbADCInputMode.Items ) ;
     cbADCInputMode.ItemIndex := Min(Main.SESLABIO.ADCInputMode,cbADCInputMode.Items.Count-1) ;

     // Device list
     Main.SESLABIO.GetDeviceNumbers( cbDeviceNumber.Items ) ;
     cbDeviceNumber.ItemIndex := Min(Main.SESLABIO.DeviceNumber-1,cbADCInputMode.Items.Count-1) ;

     if cbADCInputMode.Items.Count > 1 then NIPanel.Visible := True
                                       else  NIPanel.Visible := False ;

     { Number of channels }
     edNumChannels.HiLimit := EDRChannelLimit + 1 ;
     edNumChannels.Value := Settings.NumChannels ;
     Main.SESLabIO.ADCNumChannels := Round(edNumChannels.Value) ;

     { Set up A/D Converter voltage range selection box }
     cbADCVoltageRange.clear ;
     for i := 0 to Main.SESLabIO.ADCNumVoltageRanges-1 do begin
         Main.SESLabIO.ADCVoltageRangeIndex := i ;
         cbADCVoltageRange.items.add(
           format(' +/- %.3g V ',[Main.SESLabIO.ADCVoltageRange] )) ;
         end ;
     cbADCVoltageRange.ItemIndex :=  Min( Settings.ADCVoltageRangeIndex,
                                          Main.SESLabIO.ADCNumVoltageRanges-1) ;

     Settings.MinSamplingInterval := Main.SESLabIO.ADCMinSamplingInterval ;
     Settings.MaxSamplingInterval := Main.SESLabIO.ADCMaxSamplingInterval ;

     { Sampling interval }
     EdSamplingInterval.Value := Settings.ADCSamplingInterval ;

     // Amplifier #1
     Amplifier.GetList( cbAmplifier1.Items ) ;
     cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(Amplifier.AmplifierType[1])) ;
     edGainTelegraphChannel1.Value := Amplifier.GainTelegraphChannel[1] ;
     edModeTelegraphChannel1.Value := Amplifier.ModeTelegraphChannel[1] ;
     GainTelegraphPanel1.Visible := Amplifier.NeedsGainTelegraphChannel[1] ;
     ModeTelegraphPanel1.Visible := Amplifier.NeedsModeTelegraphChannel[1] ;

     // Amplifier #2
     Amplifier.GetList( cbAmplifier2.Items ) ;
     cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(Amplifier.AmplifierType[2])) ;
     edGainTelegraphChannel2.Value := Amplifier.GainTelegraphChannel[2] ;
     edModeTelegraphChannel2.Value := Amplifier.ModeTelegraphChannel[2] ;
     GainTelegraphPanel2.Visible := Amplifier.NeedsGainTelegraphChannel[2] ;
     ModeTelegraphPanel2.Visible := Amplifier.NeedsModeTelegraphChannel[2] ;

     { Set channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (3*ChannelTable.DefaultColWidth) div 2 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := Settings.NumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     UpdateNumChannels ;

     { Set time units radio buttons }
     if Settings.TUnits = 's' then rbTSecs.checked := true
                              else rbTmsecs.checked := true ;

     UpDateOK := True ;
     OKorCancelButtonClicked := False ;
     end;


procedure TSetupDlg.rbTmsecsClick(Sender: TObject);
begin
     Settings.TUnits := 'ms' ;
     Settings.TScale := SecsToms ;
     Settings.TUnscale := MsToSecs ;
     end;


procedure TSetupDlg.rbTSecsClick(Sender: TObject);
begin
     Settings.TUnits := 's' ;
     Settings.TScale := 1. ;
     Settings.TUnscale := 1. ;
     end;


procedure TSetupDlg.bOKClick(Sender: TObject);
{ ---------------------------------
  Exit setup dialog and update file
  ---------------------------------}
begin
     UpdateOK := True ;
     OKorCancelButtonClicked := True ;
     Close ;
     end ;


procedure TSetupDlg.bCancelClick(Sender: TObject);
{ ---------------------------------------
  Exit setup dialog but don't update file
  ---------------------------------------}
begin
     UpdateOK := False ;
     OKorCancelButtonClicked := True ;
     Close ;
     end;


procedure TSetupDlg.FormClose(Sender: TObject; var Action: TCloseAction);
{ ---------------------------------------------
  Update values in Settings and RecChannel and Exit
  ---------------------------------------------}
var
   ch : LongInt ;
begin

     if not OKorCancelButtonClicked then begin
        if MessageDlg( 'Save changes! Are you Sure? ', mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then UpdateOK := True
                                     else UpdateOK := False ;
           end ;

     { Update file header block with new settings }
     if UpdateOK then begin

        // Update channel calibration factors
        UpdateChannelCalibration ;

        // Update channel numbers (& sampling rate)
        UpdateNumChannels ;

        // Update lab. interface if necessary
        Settings.LaboratoryInterface := Main.SESLabIO.LabInterfaceType ;

        // A/D channel input mode
        Main.SESLABIO.ADCInputMode := cbADCInputMode.ItemIndex ;
        Settings.ADCInputMode := Main.SESLABIO.ADCInputMode ;

        Settings.ADCSamplingInterval := Main.SESLabIO.ADCSamplingInterval ;
        Settings.NumChannels := Main.SESLabIO.ADCNumChannels ;
        Settings.ADCVoltageRangeIndex := cbADCVoltageRange.ItemIndex ;

        // No. of horizontal and vertical grid lines on displays
        // Update amplifier settings
        Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
        Amplifier.GainTelegraphChannel[1] := Round(edGainTelegraphChannel1.Value) ;
        Amplifier.ModeTelegraphChannel[1] := Round(edModeTelegraphChannel1.Value) ;
        Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
        Amplifier.GainTelegraphChannel[2] := Round(edGainTelegraphChannel2.Value) ;
        Amplifier.ModeTelegraphChannel[2] := Round(edModeTelegraphChannel2.Value) ;

        if Amplifier.AmplifierType[1] = amTriton then begin
            for ch := 0 to Settings.NumChannels-1 do begin
                   Amplifier.GetChannelSettings( ch,
                                                 RecChannel[ch].ADCName,
                                                 RecChannel[ch].ADCUnits,
                                                 RecChannel[ch].ADCCalibrationFactor,
                                                 RecChannel[ch].ADCAmplifierGain ) ;
                   end ;
            end ;

        // If using VP500 as interface, amplifier must also be VP500
        if Main.SESLabIO.LabInterfaceType = vp500 then begin
           cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(amvp500)) ;
           cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(amNone)) ;
           UpdateAmplifierSettings ;
           end ;

        if Main.SESLabIO.LabInterfaceType = Triton then begin
           cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(amTriton)) ;
           cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(amNone)) ;
           UpdateAmplifierSettings ;
           end ;

        Main.UpdateAllDisplays ;
        end ;

     // Close and destroy form
     Action := caFree ;
     end ;



procedure TSetupDlg.UpdateChannelCalibration ;
//
// Update channel calibration factors from data in table
//
var
   ch : Integer ;
begin
     // Channel calibration }
     for ch := 0 to Settings.NumChannels-1 do begin
         RecChannel[ch].ADCName := ChannelTable.cells[ChName,ch+1] ;
         RecChannel[ch].ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             RecChannel[ch].ADCCalibrationFactor);
         RecChannel[ch].ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
         RecChannel[ch].ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
         end ;

     end ;


procedure TSetupDlg.cbLabInterfaceChange(Sender: TObject);
// ---------------------------------
// Laboratory interface unit changed
// ---------------------------------
begin
    ReopenLabInterface ;
    end ;


procedure TSetupDlg.ReopenLabInterface ;
// ---------------------------
// Close and re-open interface
// ---------------------------
var
   i,ch : Integer ;
begin

     // Close interface
     Main.SESLABIO.CloseLabInterface ;

     Settings.LaboratoryInterface := Integer(cbLabInterface.Items.Objects[cbLabInterface.ItemIndex]) ;
     Settings.DeviceNumber := cbDeviceNumber.ItemIndex + 1 ;

     Main.SESLabIO.OpenLabInterface( Settings.LaboratoryInterface,
                                     Settings.DeviceNumber,
                                     Settings.ADCInputMode ) ;

     Settings.LaboratoryInterface := Main.SESLabIO.LabInterfaceType ;
     cbLabInterface.ItemIndex := cbLabInterface.Items.IndexOfObject(TObject(Settings.LaboratoryInterface)) ;

     cbDeviceNumber.ItemIndex := Settings.DeviceNumber - 1 ;

     // A/D channel input mode
     Main.SESLABIO.GetADCInputModes( cbADCInputMode.Items ) ;
     cbADCInputMode.ItemIndex := Min(Main.SESLABIO.ADCInputMode,cbADCInputMode.Items.Count-1) ;

     // Device list
     Main.SESLABIO.GetDeviceNumbers( cbDeviceNumber.Items ) ;
     cbDeviceNumber.ItemIndex := Min(Main.SESLABIO.DeviceNumber-1,cbADCInputMode.Items.Count-1) ;

     if cbADCInputMode.Items.Count > 1 then NIPanel.Visible := True
                                       else NIPanel.Visible := False ;

     // Update A/D voltage range
     cbADCVoltageRange.clear ;
     for i := 0 to Main.SESLabIO.ADCNumVoltageRanges-1 do begin
         Main.SESLabIO.ADCVoltageRangeIndex := i ;
         cbADCVoltageRange.items.add(
           format(' +/-%.3g V ',[Main.SESLabIO.ADCVoltageRange] )) ;
         end ;
     cbADCVoltageRange.ItemIndex := 0 ;

     // Initialise channel display settings to minimum magnification
     for ch := 0 to EDRChannelLimit do begin
         RecChannel[ch].yMin := Main.SESLabIO.ADCMinValue ;
         RecChannel[ch].yMax := Main.SESLabIO.ADCMaxValue ;
         end ;

        // If using VP500 as interface, amplifier must also be VP500
        if Main.SESLabIO.LabInterfaceType = vp500 then begin
           cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(amvp500)) ;
           cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(amNone)) ;
           UpdateAmplifierSettings ;
           end ;

        if Main.SESLabIO.LabInterfaceType = Triton then begin
           cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(amTriton)) ;
           cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(amNone)) ;
           UpdateAmplifierSettings ;
           end ;

     UpdateNumChannels ;

     end;


procedure TSetupDlg.EdSamplingIntervalKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------
// Sampling interval changed
// -------------------------
begin
     if key = chr(13) then begin
        UpdateChannelCalibration ;
        UpdateNumChannels ;
        end ;
     end;


procedure TSetupDlg.edNumChannelsKeyPress(Sender: TObject; var Key: Char);
begin
     if key = chr(13) then begin
        // Get existing channel calibration data from table
        UpdateChannelCalibration ;
        if (edNumChannels.Value < 1) or (edNumChannels.Value > (EDRChannelLimit+1)) then begin
           ShowMessage( format('Out of range (1-%d)!',[EDRChannelLimit+1])) ;
           edNumChannels.Value := Main.SESLabIO.ADCNumChannels ;
           end
        else UpdateNumChannels ;
        end ;
     end ;


procedure TSetupDlg.UpdateSamplingInterval ;
{ ----------------------------
  Update A/D sampling interval
  ---------------------------- }
begin
     Main.SESLabIO.ADCSamplingInterval := edSamplingInterval.Value ;
     edSamplingInterval.Value := Main.SESLabIO.ADCSamplingInterval ;
     end ;


procedure TSetupDlg.UpdateNumChannels ;
{ --------------------------
  Update no. of A/D channels
  -------------------------- }
var
   Row,ch : Integer ;
begin

     // VP500 has only 2 channels
     if Main.SESLabIO.LabInterfaceType = vp500 then begin
        Main.SESLabIO.ADCNumChannels := MinInt( [Round(edNumChannels.Value),2]) ;
        edNumChannels.Value := Main.SESLabIO.ADCNumChannels ;
        ChannelTable.RowCount := Main.SESLabIO.ADCNumChannels + 1 ;
        end ;

     Main.SESLabIO.ADCNumChannels := Round(edNumChannels.Value) ;
     edNumChannels.Value := Main.SESLabIO.ADCNumChannels ;
     UpdateSamplingInterval ;
     ChannelTable.RowCount := Main.SESLabIO.ADCNumChannels + 1 ;
     { Add details for new channels to table }
     for Row := 1 to ChannelTable.RowCount-1 do begin
            ch := Row-1 ;
            ChannelTable.cells[ChNum,Row] := format('%d',[ch]) ;
            ChannelTable.cells[ChName,Row] := RecChannel[ch].ADCName ;
            if RecChannel[ch].ADCCalibrationFactor = 0.0 then
               RecChannel[ch].ADCCalibrationFactor := 1E-3 ;
            ChannelTable.cells[ChCal,Row] := Format(
                                            '%5.4g',[RecChannel[ch].ADCCalibrationFactor] ) ;
            ChannelTable.cells[ChUnits,Row] := RecChannel[ch].ADCUnits ;
            RecChannel[ch].InUse := True ;
            end ;

     // Ensure channels have correct settings for amplifier
     UpdateAmplifierSettings ;

     end ;


procedure TSetupDlg.UpdateAmplifierSettings ;
// ----------------------------------------------
// Update channel settings when amplifier changed
// ----------------------------------------------
var
    ch : Integer ;

begin

     Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
     GainTelegraphPanel1.Visible := Amplifier.NeedsGainTelegraphChannel[1] ;
     ModeTelegraphPanel1.Visible := Amplifier.NeedsModeTelegraphChannel[1] ;

     Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
     GainTelegraphPanel2.Visible := Amplifier.NeedsGainTelegraphChannel[2] ;
     ModeTelegraphPanel2.Visible := Amplifier.NeedsModeTelegraphChannel[2] ;

     // Update table
     for ch := 0 to 1 do begin

         Amplifier.GetChannelSettings( ch,
                                   RecChannel[ch].ADCName,
                                   RecChannel[ch].ADCUnits,
                                   RecChannel[ch].ADCCalibrationFactor,
                                   RecChannel[ch].ADCAmplifierGain ) ;

         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := RecChannel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',[RecChannel[ch].ADCCalibrationFactor] ) ;
         ChannelTable.cells[ChUnits,ch+1] := RecChannel[ch].ADCUnits ;
         end ;

     // Set patch clamp command voltage divide factor
     //Amplifier.GetCommandVoltageDivideFactor( 1, Settings.VCommand[0].DivideFactor ) ;
     //Amplifier.GetCommandVoltageDivideFactor( 2, Settings.VCommand[1].DivideFactor ) ;

     end ;


procedure TSetupDlg.cbAmplifiersChange(Sender: TObject);
// ----------------------
// Amplifier type changed
// ----------------------
begin
     UpdateAmplifierSettings ;
     end ;


procedure TSetupDlg.cbAmplifier1Change(Sender: TObject);
// ----------------------
// Amplifier type changed
// ----------------------
begin
     UpdateAmplifierSettings ;
     end ;


procedure TSetupDlg.cbDeviceNumberChange(Sender: TObject);
// ---------------------
// Device number changed
// ---------------------
begin

    if Main.SESLabIO.DeviceExists(cbDeviceNumber.ItemIndex+1) then ReopenLabInterface
    else cbDeviceNumber.ItemIndex := Settings.DeviceNumber-1 ;

    end;


procedure TSetupDlg.cbADCInputModeChange(Sender: TObject);
// ---------------------------------
// Laboratory interface unit changed
// ---------------------------------
begin
    Settings.ADCInputMode := cbADCInputMode.ItemIndex ;
    ReopenLabInterface ;
    Settings.ADCInputMode := Main.SESLabIO.ADCInputMode ;
    end ;

end.
