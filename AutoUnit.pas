unit AutoUnit;
// -----------------------------
// WinEDR - Automation Interface
// -----------------------------
// 31.05.07
// 17.06.19 Updated to include ACTIVE X commands for Pico patch clamp (same as WinWCP)

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, WinEDR_TLB, StdVcl, ampmodule;

type
  TAUTO = class(TAutoObject, IAUTO)
  protected
    procedure CloseFile; safecall;
    procedure NewFile(FileName: OleVariant); safecall;
    procedure OpenFile(FileName: OleVariant); safecall;
    procedure StartRecording; safecall;
    procedure StopRecording; safecall;
    function Get_DACChannel: OleVariant; safecall;
    function Get_HoldingVoltage: OleVariant; safecall;
    function Get_RecordDuration: OleVariant; safecall;
    function Get_StimulusProtocol: OleVariant; safecall;
    function Get_TriggerMode: OleVariant; safecall;
    procedure Set_DACChannel(Value: OleVariant); safecall;
    procedure Set_HoldingVoltage(Value: OleVariant); safecall;
    procedure Set_RecordDuration(Value: OleVariant); safecall;
    procedure Set_StimulusProtocol(Value: OleVariant); safecall;
    procedure Set_TriggerMode(Value: OleVariant); safecall;
    procedure StartStimulus; safecall;
    procedure StopStimulus; safecall;
    function Get_NumTriggerSweeps: OleVariant; safecall;
    procedure Set_NumTriggerSweeps(Value: OleVariant); safecall;
    function Get_Cm: OleVariant; safecall;
    function Get_Ga: OleVariant; safecall;
    function Get_Gm: OleVariant; safecall;
    function Get_RSeal: OleVariant; safecall;
    function Get_SealTestPulseAmplitude: OleVariant; safecall;
    function Get_SealTestPulseDuration: OleVariant; safecall;
    procedure Set_Cm(Value: OleVariant); safecall;
    procedure Set_Ga(Value: OleVariant); safecall;
    procedure Set_Gm(Value: OleVariant); safecall;
    procedure Set_RSeal(Value: OleVariant); safecall;
    procedure Set_SealTestPulseAmplitude(Value: OleVariant); safecall;
    procedure Set_SealTestPulseDuration(Value: OleVariant); safecall;
    procedure StartSealTest; safecall;
    procedure StopSealTest; safecall;
    function Get_Status: OleVariant; safecall;
    procedure Set_Status(Value: OleVariant); safecall;
    function Get_PicoCFastComp: OleVariant; safecall;
    function Get_PicoConfig: Integer; safecall;
    function Get_PicoCSlowComp: OleVariant; safecall;
    function Get_PicoEnableCFast: Integer; safecall;
    function Get_PicoEnableCSlow: Integer; safecall;
    function Get_PicoEnableJP: Integer; safecall;
    function Get_PicoFilter: Integer; safecall;
    function Get_PicoGain: Integer; safecall;
    function Get_PicoInput: Integer; safecall;
    function Get_PicoJPComp: OleVariant; safecall;
    function Get_SealTestGaFromPeak: Integer; safecall;
    function Get_SealTestNumAverages: OleVariant; safecall;
    procedure AutoCompCFast; safecall;
    procedure AutoCompCSlow; safecall;
    procedure AutoCompJP; safecall;
    procedure PicoClearCompC; safecall;
    procedure PicoClearCompJP; safecall;
    procedure Set_PicoCFastComp(Value: OleVariant); safecall;
    procedure Set_PicoConfig(Value: Integer); safecall;
    procedure Set_PicoCSlowComp(Value: OleVariant); safecall;
    procedure Set_PicoEnableCFast(Value: Integer); safecall;
    procedure Set_PicoEnableCSlow(Value: Integer); safecall;
    procedure Set_PicoEnableJP(Value: Integer); safecall;
    procedure Set_PicoFilter(Value: Integer); safecall;
    procedure Set_PicoGain(Value: Integer); safecall;
    procedure Set_PicoInput(Value: Integer); safecall;
    procedure Set_PicoJPComp(Value: OleVariant); safecall;
    procedure Set_SealTestGaFromPeak(Value: Integer); safecall;
    procedure Set_SealTestNumAverages(Value: OleVariant); safecall;
    procedure PicoAutoCompCFast; safecall;
    procedure PicoAutoCompCSlow; safecall;
    procedure PicoAutoCompJP; safecall;

  end;

implementation

uses ComServ, mdiform, global, fileio, sysutils, rec, strutils, math, sealtest, tritonpanelunit ;

procedure TAUTO.CloseFile;
// ---------------
// Close data file
// ---------------
begin
    Main.CloseFormsAndDataFile( AllForms ) ;
    end;


procedure TAUTO.NewFile(FileName: OleVariant);
// ---------------------
// Create new data file
// ---------------------
begin

    CdrFH.FileName := ChangeFileExt( FileName, DataFileExtension ) ;

    if Main.CreateNewDataFile( CdrFH ) then begin
       { Close again to permit re-opening by LoadDataFiles }
       Main.CloseFormsAndDataFile( AllForms ) ;
       WriteToLogFile( 'New file ' + CdrFH.FileName ) ;
       { Re-open data file }
       Main.LoadDataFiles( CdrFH.FileName ) ;

       Main.DataDirectory := ExtractFilePath( CdrFH.FileName ) ;

       end ;

    end;


procedure TAUTO.OpenFile(FileName: OleVariant);
// -----------------------
// Open existing data file
// -----------------------
begin
     { Close existing data file }
     Main.CloseFormsAndDataFile( AllForms ) ;

     if FileExists( FileName ) then begin

        CdrFH.FileName := FileName ;
        Main.LoadDataFiles( CdrFH.FileName ) ;

        { Save data directory }
        Main.DataDirectory := ExtractFilePath( CdrFH.FileName ) ;

        end ;

     Main.SetMenus ;

     end;

procedure TAUTO.StartRecording;
// -----------------------
// Start recording to disk
// -----------------------
begin
     // Make recording form active
     Main.mnRecord.Click ;
     // Start recording
     if Main.FormExists('RecordFrm') then RecordFrm.StartRecording ;

     end;

procedure TAUTO.StopRecording;
// -----------------------
// Stop recording to disk
// -----------------------
begin
     if Main.FormExists('RecordFrm') then RecordFrm.StopRecording ;
     end;

function TAUTO.Get_DACChannel: OleVariant;
// ----------------------------------------------------------
// Return DAC channel selected for updating by HoldingVoltage
// ----------------------------------------------------------
begin
     Result := Settings.DACSelected ;
     end;


function TAUTO.Get_HoldingVoltage: OleVariant;
// ---------------------------------------------------
// Return Holding Voltage on selected DAC output channel
// ---------------------------------------------------
begin
     Result := Main.SESLabIO.DACHoldingVoltage[Settings.DACSelected]*
               Amplifier.CommandScaleFactor[Settings.DACSelected]*1E3 ;
     end;

function TAUTO.Get_RecordDuration: OleVariant;
// -----------------------
// Get recording duration
// -----------------------
begin
     Result := Settings.RecordDuration ;
     end;


function TAUTO.Get_StimulusProtocol: OleVariant;
// -----------------------------------------
// Return current selected stimulus protocol
// -----------------------------------------
begin
     Result := AnsiReplaceText(ExtractFileName( Settings.VProgramFileName),'.sti', '' ) ;
     end;


function TAUTO.Get_TriggerMode: OleVariant;
// -----------------------------------------------
// Return trigger mode (0=free run, 1=ext trigger)
// -----------------------------------------------
begin
    if Settings.TriggerMode = 'F' then Result := 0
                                  else Result := 1 ;
    end;


procedure TAUTO.Set_DACChannel(Value: OleVariant);
// ------------------------------------------------
// Set DAC selected for updating via AUTO interface
// ------------------------------------------------
begin
     Settings.DACSelected := Max(Min(Round(Value),Main.SESLabIO.DACNumChannels-1),0) ;
     end;


procedure TAUTO.Set_HoldingVoltage(Value: OleVariant);
// ---------------------------------------------------
// Set holding voltage on current selected DAC channel
// ---------------------------------------------------
begin
     Main.SESLabIO.DACHoldingVoltage[Settings.DACSelected] :=
               Value / (Amplifier.CommandScaleFactor[Settings.DACSelected]*1E3) ;
     if Main.FormExists('RecordFrm') then RecordFrm.UpdateOutputs ;
     if Main.FormExists('SealTestFrm') then SealTestFrm.HoldingVoltage[1] := Value ;
     end;


procedure TAUTO.Set_RecordDuration(Value: OleVariant);
// --------------------
// Set record duration
// --------------------
begin
    If Value <= 0.0 then Exit ;
    Settings.RecordDuration := Value ;
    if Main.FormExists('RecordFrm') then RecordFrm.RecordDuration := Value ;
    end;


procedure TAUTO.Set_StimulusProtocol(Value: OleVariant);
// ---------------------
// Set stimulus protocol
// ---------------------
begin
     Settings.VProgramFileName := Main.VProtDirectory + Value + '.xml' ;
     if Main.FormExists('RecordFrm') then RecordFrm.StimulusProtocol := Value ;
     end;


procedure TAUTO.Set_TriggerMode(Value: OleVariant);
// --------------------------------
// Set recording sweep trigger mode
// --------------------------------
begin
     if Round(Value) = 0 then Settings.TriggerMode := 'F'
                         else Settings.TriggerMode := 'E' ;
     if Main.FormExists('RecordFrm') then RecordFrm.TriggerMode := Round(Value) ;
     end;

     
procedure TAUTO.StartStimulus;
// --------------
// Start stimulus
// --------------
begin
     // Make recording form active
     Main.mnRecord.Click ;
     // Start recording
     if Main.FormExists('RecordFrm') then RecordFrm.StartStimulus ;

     end;

procedure TAUTO.StopStimulus;
// --------------
// Stop stimulus
// --------------
begin
     // Start recording
     if Main.FormExists('RecordFrm') then RecordFrm.StopStimulus ;
     end;


function TAUTO.Get_NumTriggerSweeps: OleVariant;
// --------------------------------------------------
// Return no. of externally triggered sweeps required
// --------------------------------------------------
begin
     Result := Settings.NumTriggerSweeps ;
     end;


procedure TAUTO.Set_NumTriggerSweeps(Value: OleVariant);
// --------------------------------------------------
// Set no. of externally triggered sweeps required
// --------------------------------------------------
begin
     Settings.NumTriggerSweeps := Max(Round(Value),1) ;
     if Main.FormExists('RecordFrm') then
        RecordFrm.NumTriggerSweeps := Settings.NumTriggerSweeps ;
     end;


function TAUTO.Get_Cm: OleVariant;
// ---------------------------
// Read cell membrane capacity
// ---------------------------
begin
     Result := Main.Cm ;
     end;


function TAUTO.Get_Ga: OleVariant;
// ---------------------------
// Read pipette access conductance
// ---------------------------
begin
     Result := Main.Ga ;
     end;

function TAUTO.Get_Gm: OleVariant;
// ---------------------------
// Read cell membrane conductance
// ---------------------------
begin
     Result := Main.Gm ;
     end;

function TAUTO.Get_RSeal: OleVariant;
// ---------------------------
// Read seal resistance
// ---------------------------
begin
     Result := Main.RSeal ;
     end;

function TAUTO.Get_SealTestPulseAmplitude: OleVariant;
// ------------------------
// Get test pulse amplitude
// ------------------------
begin
     Result := Settings.SealTest.PulseHeight1 ;
    end ;

function TAUTO.Get_SealTestPulseDuration: OleVariant;
begin
     Result := Settings.SealTest.PulseWidth ;
     end;


procedure TAUTO.Set_Cm(Value: OleVariant);
begin

end;

procedure TAUTO.Set_Ga(Value: OleVariant);
begin

end;

procedure TAUTO.Set_Gm(Value: OleVariant);
begin

end;

procedure TAUTO.Set_RSeal(Value: OleVariant);
begin

end;

procedure TAUTO.Set_SealTestPulseAmplitude(Value: OleVariant);
// -----------------------------
// Set seal test pulse amplitude
// -----------------------------
begin

     Settings.SealTest.PulseHeight1 := Value ;
     if Main.FormExists('SealTestFrm') then begin
        SealTestFrm.TestPulseNumber := 1 ;
        SealTestFrm.TestPulseAmplitude[1] := Settings.SealTest.PulseHeight1 ;
        end ;

     end;


procedure TAUTO.Set_SealTestPulseDuration(Value: OleVariant);
// -------------------------------
// Set duration of seal test pulse
// -------------------------------
begin
     Settings.SealTest.PulseWidth := Value ;
     if Main.FormExists('SealTestFrm') then begin
        SealTestFrm.TestPulseNumber := 1 ;
        SealTestFrm.TestPulseWidth := Settings.SealTest.PulseWidth ;
        end ;
     end ;


procedure TAUTO.StartSealTest;
// ---------------
// Start seal test
// ---------------
begin

     Main.mnSealTest.Click ;

     end;

procedure TAUTO.StopSealTest;
// --------------
// Stop seal test
// --------------
begin
     if main.FormExists('SealTestFrm') then SealTestFrm.Close ;
     end;


function TAUTO.Get_Status: OleVariant;
// -----------------------------------------
// Return current status of program activity
// -----------------------------------------
// 0 = IDLE, 1=SEAL TEST, 2=ANALOGUE DISPLAY, 3=RECORDING
begin

     Result := 0 ;
     if main.FormExists('SealTestFrm') then begin
        if SealTestFrm.Running then Result := 1 ;
        end
     else if main.FormExists('RecordFrm') then begin
        if RecordFrm.Running then Result := 2 ;
        if RecordFrm.Recording then Result := 3 ;
        end ;

     end;

procedure TAUTO.Set_Status(Value: OleVariant);
begin

end;

function TAUTO.Get_PicoConfig: Integer;
// ---------------------------------
// Read Triton amplifier user config
// ---------------------------------
begin
     if Main.ShowTritonPanel then Result := TritonPanelFrm.UserConfig
                             else  Result := 0 ;
     end;

function TAUTO.Get_PicoENableCFast: Integer;
// --------------------------------------------------------------------
// Read Triton amplifier Cfast comp enabled(1), disabled(0)
// --------------------------------------------------------------------
begin
      Result := 0 ;
     if Main.ShowTritonPanel then if TritonPanelFrm.EnableCFast then Result := 1 ;
end;

function TAUTO.Get_PicoEnableCSlow: Integer;
// --------------------------------------------------------------------
// Read Triton amplifier Cslow comp enabled(1), disabled(0)
// --------------------------------------------------------------------
begin
      Result := 0 ;
     if Main.ShowTritonPanel then if TritonPanelFrm.EnableCSlow then Result := 1 ;
end;

function TAUTO.Get_PicoEnableJP: Integer;
// --------------------------------------------------------------------
// Read Triton amplifier junction potential comp enabled(1), disabled(0)
// --------------------------------------------------------------------
begin
      Result := 0 ;
     if Main.ShowTritonPanel then if TritonPanelFrm.EnableJP then Result := 1 ;
end;


function TAUTO.Get_PicoFilter: Integer;
// --------------------------------
// Read Triton amplifier filter index
// --------------------------------
begin
     if Main.ShowTritonPanel then Result := TritonPanelFrm.Filter
                             else  Result := 0 ;
     end;

function TAUTO.Get_PicoGain: Integer;
// --------------------------------
// Read Triton amplifier gain index
// --------------------------------
begin
     if Main.ShowTritonPanel then Result := TritonPanelFrm.Gain
                             else  Result := 0 ;
     end;

function TAUTO.Get_PicoInput: Integer;
// --------------------------------
// Read Triton amplifier input index
// --------------------------------
begin
     if Main.ShowTritonPanel then Result := TritonPanelFrm.Source
                             else  Result := 0 ;
     end;


procedure TAUTO.Set_PicoConfig(Value: Integer);
// ----------------------------------
// Read Triton amplifier config index
// ----------------------------------
begin
     if Main.ShowTritonPanel then TritonPanelFrm.UserConfig := Value ;
     end;


procedure TAUTO.Set_PicoENableCFast(Value: Integer);
// ----------------------------------------
// Set Triton amplifier Cfast comp enabled
// ----------------------------------------
begin
     if Main.ShowTritonPanel then
        begin
        if Value <> 0 then TritonPanelFrm.EnableCFast := True
                      else TritonPanelFrm.EnableCFast := False ;
        end;
end;

procedure TAUTO.Set_PicoEnableCSlow(Value: Integer);
// ----------------------------------------
// Set Triton amplifier Cslow comp enabled
// ----------------------------------------
begin
     if Main.ShowTritonPanel then
        begin
        if Value <> 0 then TritonPanelFrm.EnableCSlow := True
                      else TritonPanelFrm.EnableCSlow := False ;
        end;
end;

procedure TAUTO.Set_PicoEnableJP(Value: Integer);
// ----------------------------------------------------
// Set Triton amplifier junction potential comp enabled
// ----------------------------------------------------
begin
     if Main.ShowTritonPanel then
        begin
        if Value <> 0 then TritonPanelFrm.EnableJP := True
                      else TritonPanelFrm.EnableJP := False ;
        end;
end;

procedure TAUTO.Set_PicoFilter(Value: Integer);
// -----------------------------------
// Set Triton amplifier filter setting
// -----------------------------------
begin
     if Main.ShowTritonPanel then TritonPanelFrm.Filter := Value ;
end;

procedure TAUTO.Set_PicoGain(Value: Integer);
// -----------------------------------
// Set Triton amplifier filter setting
// -----------------------------------
begin
     if Main.ShowTritonPanel then TritonPanelFrm.Gain := Value ;
end;

procedure TAUTO.Set_PicoInput(Value: Integer);
// -----------------------------------
// Set Triton amplifier source setting
// -----------------------------------
begin
     if Main.ShowTritonPanel then TritonPanelFrm.Source := Value ;
end;


procedure TAUTO.AutoCompCFast;
// ----------------
// Compensate CFast
// ----------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompCFast ;
end;

procedure TAUTO.AutoCompCSlow;
// ----------------
// Compensate CSlow
// ----------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompCSlow ;
end;

procedure TAUTO.AutoCompJP;
// -----------------------------
// Compensate junction potential
// -----------------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompJunctionPot ;
end;


function TAUTO.Get_PicoCfastComp: OleVariant;
// -------------------------------
// Return CFast compensation value
// -------------------------------
begin
    if Main.ShowTritonPanel then Result := TritonPanelFrm.CFast ;
end;

function TAUTO.Get_PicoCSlowComp: OleVariant;
// -------------------------------
// Return CSlow compensation value
// -------------------------------
begin
    if Main.ShowTritonPanel then Result := TritonPanelFrm.CSlow ;
end;

function TAUTO.Get_PicoJPComp: OleVariant;
// --------------------------------------------
// Return junction potential compensation value
// --------------------------------------------
begin
    if Main.ShowTritonPanel then Result := TritonPanelFrm.JP ;
end;

procedure TAUTO.PicoAutoCompCFast;
// ----------------------
// Auto compensate CFast
// ----------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompCFast ;
end;

procedure TAUTO.PicoAutoCompCSlow;
// ----------------------
// Auto compensate CSlow
// ----------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompCSlow ;
end;

procedure TAUTO.PicoAutoCompJP;
// ----------------------------------
// Auto compensate junction potential
// ----------------------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.AutoCompJunctionPot ;
end;

procedure TAUTO.Set_PicoCfastComp(Value: OleVariant);
begin

end;

procedure TAUTO.Set_PicoCSlowComp(Value: OleVariant);
begin

end;

procedure TAUTO.Set_PicoJPComp(Value: OleVariant);
begin

end;

{procedure TAUTO.Method1;
begin

end;

procedure TAUTO.Method2;
begin

end;}

procedure TAUTO.PicoClearCompC;
// --------------------
// Clear C compensation
// --------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.ClearCompC ;
end;

procedure TAUTO.PicoClearCompJP;
// --------------------------------
// Clear junction pot. compensation
// --------------------------------
begin
    if Main.ShowTritonPanel then TritonPanelFrm.ClearCompJP ;
end;

function TAUTO.Get_SealTestNumAverages: OleVariant;
// ---------------------------------------------
// Get seal test cell parameters no. of averages
// ---------------------------------------------
begin
     Result := Settings.SealTest.NumAverages ;
end;

procedure TAUTO.Set_SealTestNumAverages(Value: OleVariant);
// ---------------------------------------------
// Set seal test cell parameters no. of averages
// ---------------------------------------------
begin
     Settings.SealTest.NumAverages := Max(Round(Value),1) ;
     end;


function TAUTO.Get_SealTestGaFromPeak: Integer;
// ---------------------------------------------
// Get seal test G access calculation mode
// ---------------------------------------------
begin
     if Settings.SealTest.GaFromPeak then Result := 1
                                     else Result := 0 ;
end;

procedure TAUTO.Set_SealTestGaFromPeak(Value: Integer);
// ---------------------------------------------
// Set seal test G access calculation mode
// ---------------------------------------------
begin
    if Value <> 0 then Settings.SealTest.GaFromPeak := True
                  else  Settings.SealTest.GaFromPeak := False ;
end;



initialization
  TAutoObjectFactory.Create(ComServer, TAUTO, Class_AUTO,
    ciMultiInstance, tmApartment);
end.
