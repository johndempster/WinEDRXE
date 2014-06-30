unit AutoUnit;
// -----------------------------
// WinEDR - Automation Interface
// -----------------------------
// 31.05.07
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

  end;

implementation

uses ComServ, mdiform, global, fileio, sysutils, rec, strutils, math, sealtest  ;

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

       Settings.DataDirectory := ExtractFilePath( CdrFH.FileName ) ;

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
        Settings.DataDirectory := ExtractFilePath( CdrFH.FileName ) ;

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
     Settings.VProgramFileName := Settings.VProtDirectory + Value + '.xml' ;
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

initialization
  TAutoObjectFactory.Create(ComServer, TAUTO, Class_AUTO,
    ciMultiInstance, tmApartment);
end.
