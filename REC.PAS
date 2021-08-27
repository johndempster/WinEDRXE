unit Rec;
{ ==============================================================================
  WinEDR - Analogue Recording module (c) J. Dempster 1998, All Rights Reserved.
    9/6/99 ...  Now uses WinRT device driver for Digidata 1200
  26/8/99 ... Real-time fluorescence ratio computation added
  1/9/99 ... File storage now preallocated by Record button pressed
             ChangeDisplayGrid added
  4/9/99 ... Problems with -10080 error when A/D converter voltage range invalid
             now fixed
  26/6/01 ... V2.1.3 Live display duration controlled by slider bar
              Signal readout now at left edge of display
              Stop watch removed
  11/12/01 ... Stimulator now turned off when record form closed or de-activated
  9/1/02 ... V2.2.6 Stimulator now uses SESLABIO.DACMaxValue (fixes PCI-6035E problems)
  20/3/02 ... Stimulator function disabled with Instrutech and Digidata 132X interfaces
  6.6.02 .... Holding voltage increment now works
  12.9.02 ... Calibration factor variable for calculated channels now saved
              (Fixes problem where capacity scaling factors were stored incorrectly)
              Now displays signal level readout while recording
  3.12.02 ... Stimulus timing now done within SESLABIO component
  16.12.02 ... Display now works correctly at slow sampling rates (<50ms)
  19.6.03 .... Recording now prevented if amplifier gain changed when file
               contains sample data
  24.6.03 .... No. horizontal/vertical grid lines changeable
  26.6.03 .... Display now uses min/max signal compression
               Flashing Recording indicator added
               Markers can now be added to chart
  28.6.03 .... Write buffer sizing function changed (to work with ITC-16)
  24.3.04 .... SESLabIO.ADCMaxValue now used consistently throughout
               Min/MaxADCValue updated when recording started
  06.08.04 ... Ext Trigger can now be Active High or Low (interface dependent)
  09.08.04 ... Amplifier channels automatically switched when changed
  13.09.04 ... Update Gain button added allowing user to force amp. gain update.
  14.09.04 ... Capacitance calculation now works correctly (no memory overflow)
  07.03.05 ... Stimulus pulse output now works again (repeated DAC output)
  16.05.05 ... UpdateOutputPorts now before start of A/D conversion (in ADCStart)
               to get A/D sampling to work with Digidata 1200
  23.09.05 ... Display Duration control buttons &, ms,s,mins check boxes added
               X2 Zoom In and Out added
  09.06.06 ... Bug which caused loss of samples during record when display resized fixed
  11.06.06 ... Capacity calculation working again (Plymouth 2006)
  25.06.06 ... ADCVoltageRangeIndex initialised in formshow
               FP Error when no Ch.1 defined fixed
  26.09.06 ... Form now exits if no laboratory interface defined
  21.05.07 ... Multi-protocol stimuli now supported
  06.06.07 ... Stimulus now started within timer proc after A/D is re-started
  29.08.07 ... Data file clusters no longer pre-allocated before recording
  25.07.08 ... RecChannel[ch].ADCMaxValue now updated correctly and recorded
               in EDR file header when file saved
  24.02.09 ... Capacity calculation updated.
               Vm and Im now smoothed.
               Greal & Gimag can now be inverted
               Greal & Gimag scaled now takes into account current gain setting
  27.05.09 ... Greal & Gimag scaling can now be either fixed or multiplied by
               gain telegraph.
  14.09.09 ... Stored GReal and GImag data now inverted when inversion options selected
  08.12.10 ... lbReadout color property changed to clBtnFace, to keep it the same as form background
  07.07.11 ... No. of WriteBufs increased to MaxADCSsamples/16 to
               prevent access violations when sampling interval >= 10ms with NI boards
  09.07.12 ... now uses new XML based stimulus protocols
  08.08.12 ... Event Frequency now computed in this module.
  17.08.12 ... Resistance pulse plot now works in both current- and voltage- clamp mode
  28.08.12 ... Zero levels popup settings box added (on right-click over channel)
               Fixed zero levels option no longer sets levelsd to zero (only holds constant)
  10.01.13 ... VCLAMP/ICLAMP button now correctly responds to changes in clamp mode for amplifier #2
  12.02.13 ... Stimulus support added for Digidata 1440A
  18.11.13 ... Stimulus support for Digidata 132X added
               Sampling interval box now updated with valid limits when changed
               Default display grid setting now correctly applied when formf first opened
  20.11.13 ... A/D conversion now stopped on entry to .Newfile to allow changes to sampling rate
               for NI boards.
               Changes to data file name when parameters changed now .xx rather that ++++
  30.06.14 ... StimulusAvailable now set from SESLabIO.DACAvailableWhileADCActive
               Recording progress displayed in window caption
  15.09.14 ... EPC9 panel now opened when form opened (if Heka EPC9/10)
  17.10.14 ... Settings.ADCVoltageRangeIndex no longer used to store selected A/D voltage range
  11.12.14 ... UpdateAOHold added. AO Hold scaling and units can now handle separate voltage
               and current stimulation channels (e.g. Axoclamp 2)
  12.02.15 ... Now escape sample counting loop if a complete half buffer is acquired
  13.12.16 ... Computed channel readout now works correctly. Fluorescence ratio calculations
               moved to ComputerRatio(). Calculations now respond to changes in input channel
               zero levels.
  09.01.17 ... Fluorescence ratio updated. Now changes label of ratio channel to channels
               being ratioed and concentration channel given name of ion and units changed
               defined in the computaion options.
               Zero level form now displays channel name as well as number
               Ion concentration now computed correctly in uM,mM and M units.
  24.07.17 .. Amplifier.GetChannelSettings() now returns ADCInput, so
              secondary channel analogue input for be changed in CC mode
              for Axopatch 200 and AMS-2400
  24.04.18 .. Option to start stimulus protocol when recording started added
  ==============================================================================}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Global, Shared, FileIO,
  Spin, ChartDisplay, ValEdit, ScopeDisplay, SESLabIO, mmsystem, ced1902u,
  Grids, ComCtrls, ValidatedEdit, CursorLabel, HTMLLabel, Math, strutils ;

const
     MinScansPerWriteBuf = 16 ;
     NumWriteBufsMax = MaxADCSamples div 16 ;

type

     // Circular write buffer control record
     TWriteBufs = record
        NumScans : Integer ;          // Total number of scans all buffers
        NumSamples : Integer ;        // Total number of samples
        NumBufs : Integer ;           // No. of sub-buffers in circular buffer
        NumScansPerBuf : Integer ;    // No. of scans in sub-buffer
        NumSamplesPerBuf : Integer ;  // No. of samples on sub-buffer
        NumBytesPerBuf : Integer ;    // No. of bytes in sub-buffer
        ActiveBuf : Integer ;         // Index no. of sub-buffer currently filling
        WriteBuf : Integer ;          // Index no. of next sub-buffer to be written
        BufStart : Array[0..NumWriteBufsMax-1] of Integer ; // Sub-buffer starting indexes (in ADC^)
        BufEnd : Array[0..NumWriteBufsMax-1] of Integer ;   // Sub-buffer end indexes (in ADC^)
        OldestScan : Integer ;               //
        LatestScan : Integer ;               // Index of latest available scan (ADC^)
        EmptyPointer : Integer ;             // Index of empty region (ADC^)
        Recording : Boolean ;
        NumSamplesInFileRequired : Integer ;
        NumRecordsToAcquire : Integer ;
        TAcquired : Single ;
        end ;

       TCapacity = record
          GrealScale : single ;
          GrealOffset : Integer ;
//          GrealZero : Integer ;
          GimagScale : single ;
          GimagOffset : Integer ;
//          GimagZero : Integer ;
          ImScale : single ;
          ImOffset : Integer ;
//          ImZero : Integer ;
          VmScale : single ;
          VmOffset : Integer ;
//          VmZero : Integer ;
          GsScale : single ;
          GsOffset : Integer ;
          GmScale : single ;
          GmOffset : Integer ;
          CmScale : single ;
          CmOffset : Integer ;
          ImSmoothed : Single ;
          VmSmoothed : Single ;
          end ;

          TFluorescence = record
              NumerOffset : Integer ;
              NumerZero : Integer ;
              NumerScale : single ;
              DenomOffset : Integer ;
              DenomZero : Integer ;
              DenomScale : single ;
              RatioOffset : Integer ;
              RatioScale : single ;
              ConcOffset : Integer ;
              ConcScale : single ;
              end ;

          TEventAnalysis = record
              Channel : Integer ;
              ChannelOffset : Integer ;
              Baseline : Single ;
              Smooth : Single ;
              Threshold : Integer ;
              Polarity : Integer ;
              DeadScanCounter : Integer ;
              ScanCounter : Integer ;
              EventAtScan : Integer ;
              NumEvents : Integer ;
              ScansPerCountingInterval : Integer ;

              end ;

    TRAnalysis = record
       VmChan : Integer ;
       VmUnits : String ;
       ImChan : Integer ;
       ImUnits : String ;
       Threshold : Integer ;
       Polarity : Integer ;
       PreviousValue : Integer ;
       VAvg : Array[0..1] of Single ;
       IAvg : Array[0..1] of Single ;
       NAvg : Array[0..1] of Integer ;
       Level : Integer ;
       Smooth : Single ;
       NAvgMin : Integer ;
       ScaleToVolts : Single ;
       ScaleToAmps : Single ;
       end ;

    TCursors = record
             Base : Array[0..EDRChannelLimit] of Integer ;
             end ;

  TRecordFrm = class(TForm)
    ControlGrp: TGroupBox;
    bRecord: TButton;
    bStop: TButton;
    TriggerGrp: TGroupBox;
    Timer: TTimer;
    edIdent: TEdit;
    Label4: TLabel;
    StatusGrp: TGroupBox;
    Bevel1: TBevel;
    lbFileSize: TLabel;
    scDisplay: TScopeDisplay;
    StimulatorGrp: TGroupBox;
    bStartStimulus: TButton;
    shSeparator: TShape;
    ReadoutGrp: TGroupBox;
    ckFixedZeroLevels: TCheckBox;
    lbRecording: TLabel;
    edMarker: TEdit;
    bMark: TButton;
    EdNumMarkers: TEdit;
    lbReadOut: THTMLLabel;
    bStopStimulus: TButton;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    lbTDisplay: TLabel;
    rbTDisplayUnitsMSecs: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    rbTDisplayUnitsMins: TRadioButton;
    cbStimProgram: TComboBox;
    RecordPanel: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    edRecordDuration: TValidatedEdit;
    edNumChannels: TValidatedEdit;
    edSamplingInterval: TValidatedEdit;
    AOGrp: TGroupBox;
    AOPage: TPageControl;
    AO0Tab: TTabSheet;
    AO1Tab: TTabSheet;
    AO2Tab: TTabSheet;
    AO3Tab: TTabSheet;
    Label29: TLabel;
    edAO0Hold: TValidatedEdit;
    Label8: TLabel;
    edAO1Hold: TValidatedEdit;
    Label11: TLabel;
    edAO2Hold: TValidatedEdit;
    Label27: TLabel;
    edAO3Hold: TValidatedEdit;
    AmpGainGrp: TGroupBox;
    bUpdateGain: TButton;
    AmpilifiersPage: TPageControl;
    Amp0Tab: TTabSheet;
    Label7: TLabel;
    edAmplifierGain0: TValidatedEdit;
    rbVCLAMP0: TRadioButton;
    rbICLAMP0: TRadioButton;
    Amp1Tab: TTabSheet;
    Label9: TLabel;
    edAmplifierGain1: TValidatedEdit;
    Amp2Tab: TTabSheet;
    Label10: TLabel;
    edAmplifierGain2: TValidatedEdit;
    rbVCLAMP2: TRadioButton;
    RBICLAMP2: TRadioButton;
    Amp3Tab: TTabSheet;
    Label12: TLabel;
    edAmplifierGain3: TValidatedEdit;
    rbVCLAMP3: TRadioButton;
    rbICLAMP3: TRadioButton;
    rbICLamp1: TRadioButton;
    rbVClamp1: TRadioButton;
    edNumTriggerSweeps: TValidatedEdit;
    Label2: TLabel;
    SpecialPage: TPageControl;
    TrigTab: TTabSheet;
    rbFreeRun: TRadioButton;
    rbExtTrigger: TRadioButton;
    ExtTriggerGrp: TGroupBox;
    rbExtTriggerHigh: TRadioButton;
    rbExttriggerLow: TRadioButton;
    SpecialTab: TTabSheet;
    ckCapacity: TCheckBox;
    ckFluorescence: TCheckBox;
    ckEventFrequency: TCheckBox;
    ckResistance: TCheckBox;
    bSpecial: TButton;
    ckStartStimOnRecord: TCheckBox;
    procedure TimerTimer(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbFreeRunClick(Sender: TObject);
    procedure rbExtTriggerClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure edIdentKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure edRecordDurationKeyPress(Sender: TObject; var Key: Char);
    procedure bStartStimulusClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bSpecialClick(Sender: TObject);
    procedure ckFluorescenceClick(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure ckCapacityClick(Sender: TObject);
    procedure ckFixedZeroLevelsClick(Sender: TObject);
    procedure edIdentChange(Sender: TObject);
    procedure bMarkClick(Sender: TObject);
    procedure edMarkerKeyPress(Sender: TObject; var Key: Char);
    procedure rbExtTriggerHighClick(Sender: TObject);
    procedure rbExttriggerLowClick(Sender: TObject);
    procedure bUpdateGainClick(Sender: TObject);
    procedure bStopStimulusClick(Sender: TObject);
    procedure edAmplifierGainKeyPress(Sender: TObject; var Key: Char);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure rbTDisplayUnitsMSecsClick(Sender: TObject);
    procedure rbTDisplayUnitsSecsClick(Sender: TObject);
    procedure rbTDisplayUnitsMinsClick(Sender: TObject);
    procedure cbStimProgramChange(Sender: TObject);
    procedure edNumTriggerSweepsKeyPress(Sender: TObject; var Key: Char);
    procedure ckEventFrequencyClick(Sender: TObject);
    procedure ckResistanceClick(Sender: TObject);
    procedure edNumChannelsKeyPress(Sender: TObject; var Key: Char);
    procedure edSamplingIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure edAO0HoldKeyPress(Sender: TObject; var Key: Char);
    procedure edAO1HoldKeyPress(Sender: TObject; var Key: Char);
    procedure edAO2HoldKeyPress(Sender: TObject; var Key: Char);
    procedure edAO3HoldKeyPress(Sender: TObject; var Key: Char);
    procedure edAmplifierGain0KeyPress(Sender: TObject; var Key: Char);
    procedure edAmplifierGain1KeyPress(Sender: TObject; var Key: Char);
    procedure edAmplifierGain2KeyPress(Sender: TObject; var Key: Char);
    procedure edAmplifierGain3KeyPress(Sender: TObject; var Key: Char);
    procedure rbVCLAMP2Click(Sender: TObject);
    procedure rbVCLAMP0Click(Sender: TObject);
    procedure rbICLAMP0Click(Sender: TObject);
    procedure rbVClamp1Click(Sender: TObject);
    procedure rbICLamp1Click(Sender: TObject);
    procedure RBICLAMP2Click(Sender: TObject);
    procedure rbVCLAMP3Click(Sender: TObject);
    procedure rbICLAMP3Click(Sender: TObject);
    procedure scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ckStartStimOnRecordClick(Sender: TObject);
  private
    { Private declarations }
    NewAmplifierGain : Boolean ;

    //FileName : String ;           // Data file in use for recording

    WriteBufs : TWriteBufs ;      // Circular A/D data buffer control record

    ADC : PSmallIntArray ;         { A/D data buffer }
    DAC : PSmallIntArray ;         { D/A data buffer }
    DigBuf : PSmallIntArray ;      { Digital port output array }
    DisplayBuf : PSmallIntArray ; { Scope display data buffer}
    DisplayBufSize : Integer ;     // Size of DisplayBuf (bytes)

    StimulatorAvailable : Boolean ;
    ActiveStimFileName : String ;
    StimulusStartTime : Integer ;
    StimulusNumber : Integer ;
    StimulusStatus : String ;      // Name of stimulus in use
//  SumProcTime : Integer ;
  nProcTime : Integer ;

    Initialised : Boolean ; { FormShow initialisations complete }
    EmptyFlag : Integer ;
    StimulatorStartRequired : Boolean ; // Stimulator start request flag

    SweepStatus : string ;  // Current recording sweep information
    RecordingStatus : string ;
    ReadUserEnteredGain : Boolean ;
    Cursors : TCursors ;         { Display cursors }

    Cap : TCapacity ;            { Internal real-time capacity parameters }
    Fluor : TFluorescence ;      { Internal real-time fluorescence parameters }
    EventAnalysis : TEventAnalysis ; // Event analysis parameters
    RAnalysis : TRAnalysis ; // Cell resistance analysis

    TLastReadout : Integer ;
    nTicks : Cardinal ;
    ii : Integer ;
    function CheckNewDataFileNeeded : Boolean ;
    procedure ShowAmplifierGains ;
    procedure StartADC ;
    procedure StartStimulator ;
    procedure StopStimulator ;
    procedure CalcComputedChannels ;
    procedure CalculateRatio ;
    procedure StartSineWave ;
    procedure DisplaySignalLevels( var Buf : Array of SmallInt ;
                                   StartAt : Integer ) ;
    procedure DisplayLive( Initialise : Boolean ) ;


    function GetRecordDuration : Single ;
    procedure SetRecordDuration( Value : Single ) ;
    function GetStimulusProtocol : String ;
    procedure SetStimulusProtocol( Value : String ) ;
    function GetTriggerMode : Integer ;
    procedure SetTriggerMode( Value : Integer ) ;
    function GetNumTriggerSweeps : Integer ;
    procedure SetNumTriggerSweeps( Value : Integer ) ;
    function GetRunning : Boolean ;
    function GetRecording : Boolean ;
    procedure CalculateResistance ;
    procedure CalculateCapacity ;
    procedure UpdateAmplifierGain ;
    //procedure GetAmplifierGainsFromTextBox ;
    procedure UpdateChannelSettings ;
    procedure DisplayClampMode ;
    procedure DisplayAmplifierGainAndMode(
              AmpNumber : Integer ;
              AmpTab : TTabSheet ;
              edGain : TValidatedEdit ;
              rbVCLAMP : TRadioButton ;
              rbICLAMP : TRadioButton ;
              AOHold :  TValidatedEdit ) ;
    procedure DisplayAOHold(
              AONum : Integer ;
              AOTab : TTabSheet ;
              edAOHold : TValidatedEdit ) ;
    procedure UpdateAOHold(
              Amp : Integer ;
              rbVClamp : TRadioButton ;
              rbIClamp : TRadioButton  ) ;


  public
    { Public declarations }
    TimerBusy : boolean ;     // Timer scheduled operation in progress

    procedure StopADCandDAC ;
    procedure NewFile ;
    procedure ChangeDisplayGrid ;
    procedure ZoomIn( ChanNum : Integer ) ;
    procedure ZoomOut( ChanNum : Integer ) ;
    procedure ZoomOutAll ;
    procedure UpdateStimProgramList ;
    procedure RestartADC ;
    procedure StartRecording ;
    procedure StopRecording ;
    procedure StartStimulus ;
    procedure StopStimulus ;
    procedure UpdateOutputs ;
    procedure UpdateStimulusProtocolList ;

    property RecordDuration : Single
             read GetRecordDuration write SetRecordDuration ;
    property StimulusProtocol : String
             read GetStimulusProtocol write SetStimulusProtocol ;
    property TriggerMode : Integer
             read GetTriggerMode write SetTriggerMode ;
    property NumTriggerSweeps : Integer
             read GetNumTriggerSweeps write SetNumTriggerSweeps ;
    property Running : Boolean read GetRunning ;
    property Recording : Boolean read GetRecording ;

  end;

var
  RecordFrm: TRecordFrm;

implementation

{$R *.DFM}

uses MDiform, maths, SetComp, AmpModule , StimModule, RecPlotUnit,
  Sealtest, Zero, ViewSig;
const
     MinDACInterval = 0.0002 ;
     MaxPointsPerTimerTick = 8 ;
     MaxDisplayPoints = 1000 ;
     BufferWriteInterval = 0.5 ;
     MinDTForDisplayLevels = 0.02 ;
     MaxDisplayScans = 2000 ;
     CurrentClampUnits = 'pA' ;
     CurrentClampScale = 1E12 ;
     VoltageClampUnits = 'mV' ;
     VoltageClampScale = 1E3 ;


procedure TRecordFrm.FormShow(Sender: TObject);
{ ------------------------------------
  Initialisation when form is created
  ----------------------------------- }
var
    i : Integer ;
begin

     ii := 0 ;
     Initialised := False ;
//     Settings.NumChannels := 4 ;
     Resize ;

     // Exit if no interface selected
     if (Main.SESLabIO.LabInterfaceType = NoInterface12) or
        (Main.SESLabIO.LabInterfaceType = NoInterface16) then begin
        ShowMessage( 'No laboratory interface selected!' ) ;
        Close ;
        Exit ;
        end ;

     // Display Triton control panel if it is not open
     case Main.SESLabIO.LabInterfaceType of
          Triton : begin
             if not Main.FormExists( 'TritonPanelFrm' ) then begin
                Main.mnTriton.Enabled := True ;
                Main.mnTriton.Click ;
                end ;
             end ;
          HekaEPC9,HekaEPC10,HekaEPC10Plus,HekaEPC10USB : begin
             if not Main.FormExists( 'EPC9PanelFrm' ) then begin
                Main.mnEPC9Panel.Enabled := True ;
                Main.mnEPC9Panel.Click ;
                end ;
             end ;
          end;

     { Get locations of A/D, D/A and digital I/O buffers from lab. interface }
     Main.SESLabIO.GetADCBuffer( ADC ) ;
     Main.SESLabIO.GetDACBuffer( DAC ) ;
     Main.SESLabIO.GetDIGBuffer( DigBuf ) ;
     EmptyFlag := Main.SESLabIO.ADCEmptyFlag ;

     { Check that the selected hardware/software is available }
     if not Main.SESLabIO.LabInterfaceAvailable then begin
        ShowMessage( 'Laboratory interface not available!' ) ;
        Close ;
        Exit ;
        end ;

     // Determine whether stimulator function is supported
     StimulatorAvailable := Main.SESLabIO.DACAvailableWhileADCActive ;

     bStartStimulus.Enabled := StimulatorAvailable ;
     bStopStimulus.Enabled := False ;
     StimulatorStartRequired := False ;

     ckStartStimOnRecord.Checked := Settings.StartStimulusOnRecord ;

     // Update list of stimulus programs
     UpdateStimProgramList ;

     // Set display duration
     edTDisplay.Value := Settings.DisplayDuration ;

     edIdent.text := CdrFH.IdentLine ;

     { Set trigger mode }
     if Settings.TriggerMode = 'E' then rbExtTrigger.checked := True
                                   else rbFreeRun.checked := True ;
     // Set external trigger polarity
     Main.SESLabIO.ADCExternalTriggerActiveHigh := Settings.ExternalTriggerActiveHigh ;
     rbExtTriggerHigh.Checked := Main.SESLabIO.ADCExternalTriggerActiveHigh ;
     // Set voltage range
     //Main.SESLabIO.ADCVoltageRangeIndex := Settings.ADCVoltageRangeIndex ;

     { If a CED 1902 is in use, update its settings }
     for i := 1 to 2 do begin
         if Amplifier.AmplifierType[i] = amCED1902 then Amplifier.SetCED1902 ;
         end ;

     // Show amplifier gains and modes
     ShowAmplifierGains ;

     edRecordDuration.Value := Settings.RecordDuration ;
     edNumTriggerSweeps.Value := Settings.NumTriggerSweeps ;

     TimerBusy := False ;

     // Initialise time scale
     if Settings.TScale = 1000.0 then rbTDisplayUnitsMsecs.Checked := True
     else if Settings.TScale = 1.0 then rbTDisplayUnitsSecs.Checked := True
     else rbTDisplayUnitsMins.Checked := True ;
     edTDisplay.Scale := Settings.TSCale ;
     edTDisplay.Units := Settings.TUnits ;

     { No recording }
     WriteBufs.Recording := False ;
     lbRecording.Visible := False ;
     bMark.Enabled := False ;

     { Make sure voltage clamp command voltage is at current setting }
     DisplayAOHold( 0, AO0Tab, edAO0Hold ) ;
     DisplayAOHold( 1, AO1Tab, edAO1Hold ) ;
     DisplayAOHold( 2, AO2Tab, edAO2Hold ) ;
     DisplayAOHold( 3, AO3Tab, edAO3Hold ) ;
     AOPage.ActivePage := AO0Tab ;

     UpdateOutputs ;

     NewFile ;

     StimulusStatus := '' ;
     RecordingStatus := '' ;
     SweepStatus := '' ;
     ReadUserEnteredGain := False ;
     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     Timer.Enabled := True ;
     Initialised := True ;
     end ;

procedure TRecordFrm.DisplayAOHold(
          AONum : Integer ;
          AOTab : TTabSheet ;
          edAOHold : TValidatedEdit ) ;
// -----------------------
// Set AO Hold entry field
// -----------------------
begin
     if AONum < Main.SESLabIO.DACNumChannels then begin
        AOTab.TabVisible := True ;
        edAOHold.Value := Main.SESLabIO.DACHoldingVoltage[AONum] ;
        end
     else AOTab.TabVisible := False ;
     end ;


procedure TRecordFrm.ShowAmplifierGains ;
// --------------------------------------
// Show gain values for amplifiers in use
// --------------------------------------
begin
     if Amplifier.AmplifierType[0] = amNone then Amp0Tab.Visible := False
                                      else Amp0Tab.Visible := True ;
     if Amplifier.AmplifierType[1] = amNone then Amp1Tab.Visible := False
                                      else Amp1Tab.Visible := True ;
     if Amplifier.AmplifierType[2] = amNone then Amp2Tab.Visible := False
                                      else Amp2Tab.Visible := True ;
     if Amplifier.AmplifierType[3] = amNone then Amp3Tab.Visible := False
                                      else Amp3Tab.Visible := True ;

     end ;


procedure TRecordFrm.NewFile ;
{ ---------------------------------------------------------------
  Re-initialise display and A/D system when a new file is created
  --------------------------------------------------------------- }
var
   ch : Integer ;
begin

     // Stop A/D conversion to allow changes
     Main.SESLabIO.ADCStop ;

     { Get the A/D sample offsets for each input channel }
     Main.SESLabIO.ADCNumChannels := Settings.NumChannels ;
     Settings.NumChannels := Main.SESLabIO.ADCNumChannels ;
     edNumChannels.Value := Settings.NumChannels ;

     Main.SESLabIO.ADCSamplingInterval := Settings.ADCSamplingInterval ;
     Settings.ADCSamplingInterval := Main.SESLabIO.ADCSamplingInterval ;
     edSamplingInterval.Value := Settings.ADCSamplingInterval ;
     edRecordDuration.Value := Settings.RecordDuration ;

     UpdateAmplifierGain ;
     //edAmplifierGain.Value := RecChannel[0].ADCCalibrationFactor ;
     //edAmplifierGain.Units := 'V/' + RecChannel[0].ADCUnits ;

     { Initialise scope display area }
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.NumPoints := 0 ;
     scDisplay.NumChannels := Settings.NumChannels ;

     { Create a set of zero level cursors }
     scDisplay.ClearHorizontalCursors ;
     for ch := 0 to Settings.NumChannels-1 do begin
        Cursors.Base[ch] := scDisplay.AddHorizontalCursor( ch, clGray, True,'z' ) ;
        scDisplay.HorizontalCursors[ch] := Main.SESLabIO.ADCChannelZero[ch] ;
        end ;

     { Set channel information }
     for ch := 0 to Settings.NumChannels-1 do begin
        scDisplay.ChanOffsets[ch] := Main.SESLabIO.ADCChannelOffset[ch] ;
        scDisplay.ChanUnits[ch] := Main.SESLabIO.ADCChannelUnits[ch] ;
        scDisplay.ChanName[ch] := Main.SESLabIO.ADCChannelName[ch] ;
        scDisplay.ChanScale[ch] := Abs(Main.SESLabIO.ADCChannelUnitsPerBit[ch]) ;
        scDisplay.HorizontalCursors[ch] := Main.SESLabIO.ADCChannelZero[ch] ;
        scDisplay.yMin[ch] := Main.SESLabIO.ADCChannelYMin[ch] ;
        scDisplay.yMax[ch] := Main.SESLabIO.ADCChannelYMax[ch] ;
        scDisplay.ChanVisible[ch] := Main.SESLabIO.ADCChannelVisible[ch] ;
        end ;
     scDisplay.Invalidate ;

//     for i := 0 to High(TSmallIntArray) do DisplayBuf[i] := 0 ;

     { Enable/disable real-time fluorescence ratio computation }
     if Settings.NumChannels < Settings.Fluorescence.MinChannels then begin
        Settings.Fluorescence.InUse := False ;
        ckFluorescence.Enabled := False ;
        end
     else ckFluorescence.Enabled := True ;
     ckFluorescence.Checked :=  Settings.Fluorescence.InUse ;

     { Enable/disable real-time capacity computation }
     if Settings.NumChannels < Settings.Capacity.MinChannels then begin
        Settings.Capacity.InUse := False ;
        ckCapacity.Enabled := False ;
        end
     else ckCapacity.Enabled := True ;
     ckCapacity.Checked :=  Settings.Capacity.InUse ;

     // Enable/disable event frequency plot
     ckEventFrequency.Checked := Settings.RTEventAnalysis.InUse ;

     // Enable/disable cell resistance plot
     ckResistance.Checked := Settings.RTResistance.InUse ;

     // Number of chart markers still available
     edNumMarkers.text := format('%d',[MaxMarkers-MarkerList.Count]);

     end ;


procedure TRecordFrm.UpdateChannelSettings ;
// ----------------------------------------------------------------------
// Update channel scaling factors in case amplifier gain/mode has changed
// ----------------------------------------------------------------------
var
    ch,ADCInput : Integer ;
    Changed : Boolean ;
    OldValue : Single ;
    OldUnits : String ;
    OldName : String ;
   Name,Units : string ;
   VPU,Gain : Single ;
begin

     Changed := False ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin

         OldValue := CDRFH.ADCVoltageRange ;
         OldUnits := Main.SESLabIO.ADCChannelUnits[ch] ;
         OldName := Main.SESLabIO.ADCChannelName[ch] ;

         // Update channel scaling factors
         ADCInput := Main.SESLabIO.ADCChannelInputNumber[ch] ;
         Amplifier.GetChannelSettings( ch,Name,Units,VPU,Gain,ADCInput ) ;
         Main.SESLabIO.ADCChannelName[ch] := Name ;
         Main.SESLabIO.ADCChannelUnits[ch] := Units ;
         Main.SESLabIO.ADCChannelVoltsPerUnit[ch] := VPU ;
         Main.SESLabIO.ADCChannelGain[ch] := Gain ;
         Main.SESLabIO.ADCChannelInputNumber[ch] := ADCInput ;

//         RH.ADCVoltageRange[ch] := Main.SESLabIO.ADCVoltageRange /
//                                   Main.SESLabIO.ADCChannelGain[ch] ;

         if (OldValue <> CDRFH.ADCVoltageRange) or
            (OldUnits <> Units ) or
            (OldName <> Name) then Changed := True ;

         end ;

     UpdateAmplifierGain ;

     // Update current/voltage clamp state
     DisplayClampMode ;

     // Update display readout scaling factors
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
         scDisplay.ChanScale[ch] := Main.SESLabIO.ADCChannelUnitsPerBit[ch] ;
         scDisplay.ChanUnits[ch] := Main.SESLabIO.ADCChannelUnits[ch] ;
         scDisplay.ChanName[ch] := Main.SESLabIO.ADCChannelName[ch] ;
         end ;

     // Force display display update if factors changed
     if Changed then scDisplay.Invalidate ;


     end ;


procedure TRecordFrm.DisplayClampMode ;
// ----------------------------------
// Display current/voltage clamp mode
// ----------------------------------
begin

   edAO0Hold.Units := 'mV' ;
   edAO0Hold.Scale := 1000.0 ;
   edAO1Hold.Units := 'mV' ;
   edAO1Hold.Scale := 1000.0 ;
   edAO2Hold.Units := 'mV' ;
   edAO2Hold.Scale := 1000.0 ;
   edAO3Hold.Units := 'mV' ;
   edAO3Hold.Scale := 1000.0 ;

   UpdateAOHold( 0, rbVClamp0, rbIClamp0) ;
   UpdateAOHold( 1, rbVClamp1, rbIClamp1) ;
   UpdateAOHold( 2, rbVClamp2, rbIClamp2) ;
   UpdateAOHold( 3, rbVClamp3, rbIClamp3) ;

     end ;


procedure TRecordFrm.UpdateAOHold(
          Amp : Integer ;
          rbVClamp : TRadioButton ;
          rbIClamp : TRadioButton
          ) ;
// -------------------------
// Update AO hold entry box
// -------------------------
var
    Units : string ;
    Scale : single ;
    AOChan : Integer ;
begin

    if Amplifier.AmplifierType[Amp] = amNone then exit ;

    // Units & scale
    if Amplifier.ClampMode[Amp] = amCurrentClamp then begin
      Units := CurrentClampUnits ;
      AOChan := Amplifier.CurrentCommandChannel[Amp] ;
      Scale := CurrentClampScale*Amplifier.CurrentCommandScaleFactor[AOChan] ;
      rbVClamp.checked := False ;
      rbIClamp.checked := True ;
      end
    else begin
      Units := VoltageClampUnits ;
      Scale := VoltageClampScale ;
      AOChan := Amplifier.VoltageCommandChannel[Amp] ;
      Scale := VoltageClampScale*Amplifier.VoltageCommandScaleFactor[AOChan] ;
      rbVClamp.checked := True ;
      rbIClamp.checked := False ;
      end ;

    // Output channel
    case AOChan of
        0 : begin
          edAO0Hold.Units := Units ;
          edAO0Hold.Scale := Scale ;
          end ;
        1 : begin
          edAO1Hold.Units := Units ;
          edAO1Hold.Scale := Scale ;
          end ;
        2 : begin
          edAO2Hold.Units := Units ;
          edAO2Hold.Scale := Scale ;
          end ;
        3 : begin
          edAO3Hold.Units := Units ;
          edAO3Hold.Scale := Scale ;
          end ;

        end;
    end;


procedure TRecordFrm.UpdateAmplifierGain ;
// ------------------------------
// Update amplifier gain readouts
// ------------------------------
begin

    DisplayAmplifierGainAndMode( 0, Amp0Tab, edAmplifierGain0, rbVClamp0, rbIClamp0, edAO0Hold ) ;
    DisplayAmplifierGainAndMode( 1, Amp1Tab, edAmplifierGain1, rbVClamp1, rbIClamp1, edAO1Hold ) ;
    DisplayAmplifierGainAndMode( 2, Amp2Tab, edAmplifierGain2, rbVClamp2, rbIClamp2, edAO2Hold ) ;
    DisplayAmplifierGainAndMode( 3, Amp3Tab, edAmplifierGain3, rbVClamp3, rbIClamp3, edAO3Hold ) ;

     NewAmplifierGain := False ;

     end ;


{procedure TRecordFrm.GetAmplifierGainsFromTextBox ;
// -----------------------------------------------
// Update amplifier gains with user-entered values
// -----------------------------------------------
begin
    if Amp0Tab.Visible then
       Amplifier.PrimaryChannelScaleFactor[0] := edAmplifierGain0.Value ;
    if Amp1Tab.Visible then
       Amplifier.PrimaryChannelScaleFactor[1] := edAmplifierGain1.Value ;
    if Amp2Tab.Visible then
       Amplifier.PrimaryChannelScaleFactor[2] := edAmplifierGain2.Value ;
    if Amp3Tab.Visible then
       Amplifier.PrimaryChannelScaleFactor[3] := edAmplifierGain3.Value ;
    NewAmplifierGain := True ;
    end ;  }


procedure TRecordFrm.TimerTimer(Sender: TObject);
{ ---------------------
  Timed Event scheduler
  ---------------------}
var
   i,iEnd,NumNewScans,NewScan,np,npHalfBuf : Integer ;

   FilePointer : Integer ;
   //ProcessingTime : Single ;
   StimulusTime : Single ;
begin

     // Abort execution if last call still running

//     outputDebugString( PChar(format('%d',[TimeGetTime-nTicks]))) ;
     nTicks := TimeGetTime ;
     if TimerBusy then Exit ;
     TimerBusy := True ;

     //if CDRFH.NumSamplesInFile > 0 then bUpdateGain.Enabled := False
     //                              else bUpdateGain.Enabled := True ;

     if not Main.SESLabIO.ADCActive then
        begin
        { Start continuous A/D conversion into cyclic buffer }
        StartADC ;
        nTicks := TimeGetTime ;
        DisplayLive( True ) ;
        Settings.NewCalculation := True ;

        // If recording and a stimulus is protocol selected, request its start
        //if bStop.Enabled then bStartStimulus.Click ;

        End
     else
        begin

        { * A/D running .. process samples * }

        { Copy A/D samples from A/D converter's internal buffers }
        Main.SESLabIO.ADCBufferRefresh ;

        // Find latest A/D samples
        np := 0 ;
        npHalfBuf := (WriteBufs.NumSamples div 2) - 1 ;
        While ((ADC^[WriteBufs.EmptyPointer] <> EmptyFlag) or
              (ADC^[WriteBufs.EmptyPointer+1] <> -EmptyFlag)) and
              (np < npHalfBuf) do begin
              WriteBufs.EmptyPointer := WriteBufs.EmptyPointer + Main.SESLabIO.ADCNumChannels*2 ;
              if WriteBufs.EmptyPointer >= WriteBufs.NumSamples then
                 WriteBufs.EmptyPointer := WriteBufs.EmptyPointer - WriteBufs.NumSamples ;
              Inc(np) ;
              end ;

        WriteBufs.LatestScan := WriteBufs.EmptyPointer - Main.SESLabIO.ADCNumChannels*4 ;
        if WriteBufs.LatestScan < 0 then WriteBufs.LatestScan := WriteBufs.LatestScan + WriteBufs.NumSamples ;

        NumNewScans := 0 ;
        NewScan :=  WriteBufs.OldestScan ;
        while NewScan <> WriteBufs.LatestScan do begin
             NewScan := NewScan + Main.SESLabIO.ADCNumChannels ;
             if NewScan >= WriteBufs.NumSamples then NewScan := NewScan - WriteBufs.NumSamples ;
             Inc(NumNewScans) ;
             end ;

//     outputdebugString(PChar(format('%d %d %d',[WriteBufs.NumSamples,WriteBufs.NumSamplesPerBuf,NumNewScans]))) ;
        // Calculate computed channels
        if NumNewScans >= 2 then
           begin
           CalcComputedChannels ;
           // Update display
           DisplayLive( False ) ;
           end ;

        // If latest write block full .. write to file
        WriteBufs.ActiveBuf := WriteBufs.OldestScan div WriteBufs.NumSamplesPerBuf ;

        if (WriteBufs.ActiveBuf <> WriteBufs.WriteBuf) then
           begin

           // Write block to file (if recording)
           if WriteBufs.Recording then
              begin
              FilePointer := (CDRFH.NumSamplesInFile*2) + CDRFH.NumBytesInHeader ;
              FileSeek( CDRFH.FileHandle, FilePointer, 0 ) ;
              if FileWrite( CDRFH.FileHandle,
                            ADC^[WriteBufs.BufStart[WriteBufs.WriteBuf]],
                            WriteBufs.NumBytesPerBuf )
                 <> WriteBufs.NumBytesPerBuf then
                 Main.StatusBar.SimpleText := 'Error writing to ' + CDRFH.FileName ;
              CDRFH.NumSamplesInFile := CDRFH.NumSamplesInFile + WriteBufs.NumSamplesPerBuf ;
              WriteBufs.TAcquired := WriteBufs.TAcquired
                                     + CDRFH.dt*(WriteBufs.NumSamplesPerBuf div CDRFH.NumChannels) ;
              lbRecording.Visible := not lbRecording.Visible ;

              RecordingStatus := format( 'Recording: %.1f/%.1f s (%d Kb) ',
                                           [WriteBufs.TAcquired,
                                            Settings.RecordDuration,
                                            CDRFH.NumSamplesInFile div 512
                                            ]) ;
              Main.StatusBar.SimpleText := SweepStatus + RecordingStatus + StimulusStatus ;
              Caption := 'Record to Disk: ' + SweepStatus + RecordingStatus + StimulusStatus ;
              end ;

           // Fill buffer with empty flags
           i := WriteBufs.BufStart[WriteBufs.WriteBuf] ;
           iEnd := WriteBufs.BufEnd[WriteBufs.WriteBuf] ;
           while i <= iEnd do begin
                 ADC^[i] := EmptyFlag ;
                 Inc(i) ;
                 ADC^[i] := -EmptyFlag ;
                 Inc(i) ;
                 end ;
           WriteBufs.WriteBuf := WriteBufs.WriteBuf + 1 ;
           if WriteBufs.WriteBuf >= WriteBufs.NumBufs then WriteBufs.WriteBuf := 0 ;

           //ProcessingTime := (100.0*SumProcTime/nProcTime)/Timer.Interval ;
           end ;

        if WriteBufs.Recording and
           (CDRFH.NumSamplesInFile >= WriteBufs.NumSamplesInFileRequired) then
           begin
           { Restart recording if more records to be done }
           Dec(WriteBufs.NumRecordsToAcquire) ;
           if WriteBufs.NumRecordsToAcquire > 0 then bRecord.Click
                                                else bStop.Click  ;
           end ;

        end ;

//           iTicks := TimeGetTime - iTicks ;
//           SumProcTime :=  SumProcTime + iTicks ;
//           Inc(nProcTime) ;

     // Start stimulus (if one requested)
     // (Note. Wait till some A/D samples have been collected)
     if StimulatorStartRequired and (WriteBufs.LatestScan > 0) then
        begin
        StopStimulator ;
        StartStimulator ;
        StimulatorStartRequired := False ;
        end ;

     // Check for end of stimulus (non-repeating stimuli)
     if bStopStimulus.Enabled and (not Stimulator.Prot.RepeatedProtocol) then
        begin
        StimulusTime := (TimeGetTime - StimulusStartTime)*1E-3 ;
        if StimulusTime >= Stimulator.TotalProtocolDuration then
           begin
           // Stop existing stimulus
           StopStimulator ;
           // Start next stimulus (if required)
           if Stimulator.Prot.NextProtocolFileName <> '' then
              begin
              ActiveStimFileName := Stimulator.Prot.NextProtocolFileName ;
              StimulatorStartRequired := True ;
              end ;
           end ;
        end ;

     // Update voltage and digital outputs
     if Settings.UpdateOutputs then
        begin
        if not WriteBufs.Recording then UpdateOutputs ;
        end ;

     TimerBusy := False ;

     end ;


procedure TRecordFrm.DisplayLive(
          Initialise : Boolean         // TRUE = Clear/initialise display
          ) ;
// -----------------------------------------------
// Display digitised signals in acquisition buffer
// -----------------------------------------------
var

    NumScans : Integer ;           // No. of multi-channel scans in display window
    OldDisplayBufSize : Integer ;  //
    Done : Boolean ;               // Loop done flag
    i,j,ch : Integer ;             // Counters/index variables
    Value : Single ;
    NewPoints : Integer ;
    LastDisplayedScan : Integer ;
    s : string ;
begin

     // Erase display and initialise live display counters
     // --------------------------------------------------

     if Initialise or (scDisplay.Numpoints >= scDisplay.Maxpoints) then begin

       // No. of multi-channel scans to be displayed
       NumScans := Max( Round(edTDisplay.Value/Main.SESLabIO.ADCSamplingInterval),1 ) ;

       // Max. number of points in display
       scDisplay.MaxPoints := NumScans ;

       scDisplay.TScale := Main.SESLabIO.ADCSamplingInterval*Settings.TScale ;
       scDisplay.TUnits := Settings.TUnits + ' ' ;

       scDisplay.xMin := 0 ;
       scDisplay.xMax := scDisplay.MaxPoints-1 ;

       // Enable/disable display calibration grid
       scDisplay.DisplayGrid := Settings.DisplayGrid ;

       OldDisplayBufSize := DisplayBufSize ;
       DisplayBufSize := scDisplay.MaxPoints*scDisplay.NumChannels*2 ;
       if (DisplayBuf = Nil) or (DisplayBufSize <> OldDisplayBufSize) then begin
          if DisplayBuf <> Nil then FreeMem(DisplayBuf) ;
          DisplayBuf := GetMemory( DisplayBufSize ) ;
          end;
       scDisplay.SetDataBuf( DisplayBuf ) ;

       scDisplay.XOffset := CDRFH.NumSamplesInFile div Main.SESLabIO.ADCNumChannels ;
       scDisplay.NumPoints := 0 ;

       scDisplay.Invalidate ;

       // Clear markers on display
       scDisplay.ClearMarkers ;

       end ;

     // Read and display samples from A/D sample buffer
     // -----------------------------------------------

     if WriteBufs.OldestScan = WriteBufs.LatestScan then Done := True
                                                    else Done := False ;
     NewPoints := scDisplay.NumPoints ;
     LastDisplayedScan := 0 ;
     While not Done do begin
        // Increment pointer to next available scan
        j := NewPoints*scDisplay.NumChannels ;
        LastDisplayedScan := WriteBufs.OldestScan ;
        for i := 0 to scDisplay.NumChannels-1 do begin
            DisplayBuf^[j] := ADC^[WriteBufs.OldestScan] ;
            Inc(j) ;
            Inc(WriteBufs.OldestScan) ;
            end;
        Inc(NewPoints) ;
        if WriteBufs.OldestScan >= WriteBufs.NumSamples then
           WriteBufs.OldestScan := WriteBufs.OldestScan - WriteBufs.NumSamples ;
        if (WriteBufs.OldestScan = WriteBufs.LatestScan) then Done := True ;
        if NewPoints >= scDisplay.MaxPoints then Done := True ;

        end ;

     // Display latest points added to display buffer
     scDisplay.DisplayNewPoints( NewPoints );

     // Update cursor readout
     if (WriteBufs.LatestScan > 0) and
        (WriteBufs.LatestScan < WriteBufs.NumSamples) and
        (TimeGetTime > (TLastReadout+500)) then begin
        s := '' ;
        for ch := 0 to scDisplay.NumChannels-1 do begin
            Value := Main.SESLabIO.ADCChannelUnitsPerBit[ch] *
                     ( ADC^[LastDisplayedScan + scDisplay.ChanOffsets[ch]] - scDisplay.ChanZero[ch] ) ;
            s := s + format( ' %s= %.4g %s<br>',
                             [scDisplay.ChanName[ch],Value ,scDisplay.ChanUnits[ch]] ) ;
            end ;
        lbReadOut.caption := s ;
        lbReadOut.Invalidate ;
        TLastReadout := TimeGetTime ;
        end ;

     end ;


procedure TRecordFrm.StartADC ;
{ ---------------------
  Start A/D conversions
  ---------------------}
const
    IdealWriteBufDuration = 2.0 ;
    MinScansPerWriteBuf = 16 ;
var

   ExtTrigger : Boolean ;
   i : Integer ;
   MaxScans : Integer ;     // Max. number of scans in A/D buffer
   Done : Boolean ;         // Loop termination flag
begin

     // Get amplifier current/voltage gain settings
     UpdateChannelSettings ;

     { Start A/D converter sampling }
     Main.SESLabIO.ADCStop ;
     Main.SESLabIO.ADCNumChannels := Settings.NumChannels ;
     Main.SESLabIO.ADCSamplingInterval := Settings.ADCSamplingInterval ;
     //Main.SESLabIO.ADCVoltageRangeIndex := Settings.ADCVoltageRangeIndex ;

     // Ensure timer interval is long enough to capture some samples
     Timer.Interval := Max(Round(Main.SESLabIO.ADCNumChannels*Main.SESLabIO.ADCSamplingInterval*4000.0),55) ;

     // Set number of scans in write block
     MaxScans := Main.SESLabIO.ADCBufferLimit div Main.SESLabIO.ADCNumChannels ;

     WriteBufs.NumBufs := 1 ;
     Done := False ;
     while not Done do begin
          Inc(WriteBufs.NumBufs) ;
          WriteBufs.NumScansPerBuf := MaxScans div WriteBufs.NumBufs ;
          WriteBufs.NumScansPerBuf := 4*(WriteBufs.NumScansPerBuf div 4) ;
          if ((WriteBufs.NumScansPerBuf*Main.SESLabIO.ADCSamplingInterval) <= IdealWriteBufDuration) or
             (WriteBufs.NumScansPerBuf <= MinScansPerWriteBuf) then Done := True ;
          end ;
     WriteBufs.NumSamplesPerBuf := WriteBufs.NumScansPerBuf*Main.SESLabIO.ADCNumChannels ;
     WriteBufs.NumBytesPerBuf := WriteBufs.NumSamplesPerBuf*2 ;

     // Set up write Buf pointers
     for i := 0 to WriteBufs.NumBufs-1 do begin
         WriteBufs.BufStart[i] := i*WriteBufs.NumSamplesPerBuf ;
         WriteBufs.BufEnd[i] := WriteBufs.BufStart[i] + WriteBufs.NumSamplesPerBuf - 1 ;
         end ;

     // Total number of scans in circular buffer
     WriteBufs.NumScans := WriteBufs.NumBufs*WriteBufs.NumScansPerBuf ;
     // Total number of A/D samples
     WriteBufs.NumSamples := WriteBufs.NumScans*Main.SESLabIO.ADCNumChannels ;
     // Initialse sub-buffer index
     WriteBufs.WriteBuf := 0 ;
     WriteBufs.EmptyPointer := Main.SESLabIO.ADCNumChannels*4 ;
     WriteBufs.LatestScan := 0 ;
     WriteBufs.OldestScan := 0 ;

     scDisplay.CursorsEnabled := not Settings.FixedZeroLevels ;

     { If in external trigger mode when recording ...
       Request A/D converter to wait for a trigger pulse }
     if rbExtTrigger.checked and bStop.Enabled  then begin
        ExtTrigger := True ;
        Main.SESLabIO.ADCExternalTriggerActiveHigh := rbExtTriggerHigh.checked ;
        end
     else ExtTrigger := False ;

     { Enable on-line fluorescence ratio computation }
     if Main.SESLabIO.ADCNumChannels < Settings.Fluorescence.MinChannels then
        Settings.Fluorescence.InUse := False ;

     if Settings.Fluorescence.InUse then begin
        Main.SESLabIO.ADCChannelZero[Settings.Fluorescence.RatioChan] := 0 ;
        Main.SESLabIO.ADCChannelYMin[Settings.Fluorescence.RatioChan] :=
           Max( Main.SESLabIO.ADCChannelYMin[Settings.Fluorescence.RatioChan],
                (-Main.SESLabIO.ADCmaxValue -1) div 10 ) ;
        end ;

     if Main.SESLabIO.ADCNumChannels < Settings.Capacity.MinChannels then
        Settings.Capacity.InUse := False ;

     scDisplay.Invalidate ;

     { Start A/D converter sampling }
     Main.SESLabIO.ADCStop ;
     Main.SESLabIO.ADCNumChannels := Settings.NumChannels ;
     Main.SESLabIO.ADCNumSamples := WriteBufs.NumScans ;
     Main.SESLabIO.ADCSamplingInterval := Settings.ADCSamplingInterval ;
     //Main.SESLabIO.ADCVoltageRangeIndex := Settings.ADCVoltageRangeIndex ;

     // Set A/D sweep triggering
     if not ExtTrigger then Main.SESLabIO.ADCTriggerMode := tmFreeRun
                       else Main.SESLabIO.ADCTriggerMode := tmExtTrigger ;


     Main.SESLabIO.ADCCircularBuffer := True ;
     Main.SESLabIO.ADCStart ;

//     SumProcTime := 0 ;
     nProcTime := 0 ;
     TLastReadout := TimeGetTime ;

//     if Settings.Capacity.InUse then StartSineWave ;

     end ;


procedure TRecordFrm.StartStimulator ;
{ -----------------------
  Start stimulator output
  -----------------------}
begin

     if not StimulatorAvailable then Exit ;

     // Disable changes to stimulus & holding voltages
     bStopStimulus.Enabled := True ;
     bStartStimulus.Enabled := False ;
     cbStimProgram.Enabled := False ;

     // Stop D/A output
     Main.SESLabIO.DACStop ;

     // Load stimulus program from file
     Stimulator.LoadProtocol( ActiveStimFileName ) ;

     { D/A update interval }
     if Stimulator.Prot.NumDOChannels > 0 then Main.SESLabIO.DigitalStimulusEnabled := True
                                          else Main.SESLabIO.DigitalStimulusEnabled := False ;


//     if Stimulator.Prog.ExtNumPoints > 0 then DACdt := Stimulator.Prog.ExtDACdt
//                                         else DACdt := Stimulator.ProtocolDuration/MaxDACPoints ;
     Stimulator.SetDACUpdateIntervals( Stimulator.Prot ) ;
     //DACdt := Max( DACdt, Main.SESLabIO.DACMinUpdateInterval ) ;
//     DACdt := Main.SESLabIO.DACUpdateInterval := DACdt ;
//     DACdt := Main.SESLabIO.DACUpdateInterval ;

     Main.SESLabIO.DACNumChannels := Min( Stimulator.Prot.NumAOChannels, Main.SESLabIO.DACMaxChannels ) ;
     Main.SESLabIO.DACNumSamples := Stimulator.CreateWaveform( DAC^,DigBuf^,Main.SESLabIO.DACNumChannels ) ;
     Main.SESLabIO.DACRepeatWaveform := Stimulator.Prot.RepeatedProtocol ;
     if Main.SESLabIO.DigitalStimulusEnabled then Main.SESLabIO.DACDigStart(0)
                                             else Main.SESLabIO.DACStart ;

     // Save start time of stimulus
     StimulusStartTime := TimeGetTime ;

     // Add marker and log entry if recording
     if bStop.Enabled then begin
        Inc(StimulusNumber) ;
        edMarker.Text := format('Stim%d',[StimulusNumber]) ;
        WriteToLogFile( format('Stim:%d %s started',[StimulusNumber,ActiveStimFileName])) ;
        bMark.Click ;
        end ;

     StimulusStatus := 'Stim: '
                       + ChangeFileExt(ExtractFileName(ActiveStimFileName),'') ;

     Main.StatusBar.SimpleText := SweepStatus + RecordingStatus + StimulusStatus ;

     //Settings.NewCalculation := True ;

     end ;


procedure TRecordFrm.StopStimulator ;
{ ------------------------
  Stop stimulator pulses
  ------------------------ }
begin

   if not StimulatorAvailable then Exit ;

   // Stop any D/A output
   Main.SESLabIO.DACStop ;

   // Update outputs
   UpdateOutputs ;

   bStopStimulus.Enabled := False ;
   bStartStimulus.Enabled := True ;
   cbStimProgram.Enabled := True ;
   StimulusStatus := '' ;

   Main.StatusBar.SimpleText := SweepStatus + RecordingStatus + StimulusStatus ;

   end ;


procedure TRecordFrm.StopRecording ;
{ ----------------------
  Stop recording to disk
  ----------------------}
begin
     bRecord.Enabled := True ;
     RecordPanel.Enabled := bRecord.Enabled ;
     bStop.Enabled := False ;
     edTDisplay.Enabled := True ;
     TDisplayPanel.Enabled := True ;
     ckFixedZeroLevels.Enabled := True ;
     lbRecording.Visible := False ;
     bMark.Enabled := False ;
     bUpdateGain.Enabled := True ;
     AmpGainGrp.Enabled := bUpdateGain.Enabled ;

     { Save data to file header }
     if CDRFH.FileHandle >= 0 then SaveCDRHeader( CdrFH ) ;

     { Update signal display window (if it is open) }
     Main.UpdateViewSig ;

     WriteBufs.Recording := False ;

     { Shut down lab. interface and re-start
       (This turns off any stimulus pattern that is in progress) }
     Main.SESLabIO.ADCStop ;

     // Stop any running stimulus program
     StopStimulator ;

     { Indicate when recording stopped on log file }
     WriteToLogFile( SweepStatus + format(
                    ' Stopped at %.4g s',[(CdrFH.NumSamplesInFile*CdrFH.dt)/
                                           CdrFH.NumChannels])  ) ;

     RecordingStatus := '' ;
     SweepStatus := '' ;
     Main.StatusBar.SimpleText := SweepStatus + RecordingStatus + StimulusStatus ;

     Main.SetMenus ;
     end ;


procedure TRecordFrm.StartStimulus ;
// -----------------------
// Start stimulus protocol
// -----------------------
begin
     bStartStimulus.Click ;
     end ;


procedure TRecordFrm.StopStimulus ;
// -----------------------
// Stop stimulus protocol
// -----------------------
begin
     bStopStimulus.Click ;
     end ;


procedure TRecordFrm.CalcComputedChannels ;
{ -------------------------------------------------------
  Calculate computed channels from analogue input channels
  ------------------------------------------------------- }
const
     mVToVolt = 1E-3 ;
     pFToFarad = 1E-12 ;
     nSToSiemen = 1E-9 ;
     pAToAmp = 1E-12 ;
     TwoPi = 2*pi ;
var
   iScan : Integer ;
   yDiff : Integer ;
begin

     if Settings.Fluorescence.InUse then CalculateRatio ;

     if Settings.Capacity.InUse then CalculateCapacity ;

     // Event frequency display
     if Settings.RTEventAnalysis.InUse then begin

        // Initialise event detector
        if Settings.NewCalculation then begin

           // Clear scans acquired counter
           EventAnalysis.ScanCounter := 0 ;

           // Channel offset
           EventAnalysis.ChannelOffset := Main.SESLabIO.ADCChannelOffset[Settings.RTEventAnalysis.Channel] ;

           // Clear dead time counter
           EventAnalysis.DeadScanCounter := 0 ;

           // Clear event counter
           EventAnalysis.NumEvents := 0 ;
           EventAnalysis.ScansPerCountingInterval := Round(Settings.RTEventAnalysis.CountingInterval
                                                      /Main.SESLabIO.ADCSamplingInterval);
           // Set detection threshold and polarity
           EventAnalysis.Threshold := Round( Settings.RTEventAnalysis.DetectionThreshold /
                                             Main.SESLabIO.ADCChannelUnitsPerBit[Settings.RTEventAnalysis.Channel] ) ;
           if EventAnalysis.Threshold < 0 then begin
              EventAnalysis.Threshold := Abs(EventAnalysis.Threshold) ;
              EventAnalysis.Polarity := -1 ;
              end
           else EventAnalysis.Polarity := 1 ;

           // Set running mean baseline and smoothing factor
           EventAnalysis.Baseline := ADC^[WriteBufs.OldestScan+EventAnalysis.ChannelOffset] ;
           EventAnalysis.Smooth := Main.SESLabIO.ADCSamplingInterval /
                                   Max(Settings.RTEventAnalysis.RunningMeanTime,
                                       Main.SESLabIO.ADCSamplingInterval);

           end ;

        // Open plotting window (if not already open)
        if not Main.FormExists( 'RecPlotFrm' ) then begin
           RecPlotFrm := TRecPlotFrm.Create(Self) ;
           RecPlotFrm.Width :=  Main.ClientWidth - RecordFrm.Left - RecordFrm.Width - 20 ;
           RecPlotFrm.Left := RecordFrm.Left + RecordFrm.Width + 2 ;
           RecPlotFrm.Top := 10 ;
           end ;


        iScan := WriteBufs.OldestScan ;
        while iScan <> WriteBufs.LatestScan do begin

              yDiff := ADC^[iScan+EventAnalysis.ChannelOffset] - Round(EventAnalysis.Baseline) ;

              //outputdebugString(PChar(format('%d %d ',[yDiff,Round(EventAnalysis.Baseline)]))) ;

              // Detect event
              if ((EventAnalysis.Polarity*yDiff) > EventAnalysis.Threshold) and
                 (EventAnalysis.DeadScanCounter <= 0) then begin
                 EventAnalysis.EventAtScan := EventAnalysis.ScanCounter ;

                 EventAnalysis.NumEvents := EventAnalysis.NumEvents + 1 ;
                 EventAnalysis.DeadScanCounter := Round( Settings.RTEventAnalysis.DeadTime/
                                                         Main.SESLabIO.ADCSamplingInterval ) ;
                 end ;

              // Decrement dead time counter
              EventAnalysis.DeadScanCounter := Max(EventAnalysis.DeadScanCounter-1,0) ;

              // Update running mean baseline
              EventAnalysis.Baseline := (EventAnalysis.Smooth*yDiff)
                                        - (1.0-EventAnalysis.Smooth)*EventAnalysis.Baseline ;

              Inc(EventAnalysis.ScanCounter) ;
              if EventAnalysis.ScanCounter >= EventAnalysis.ScansPerCountingInterval then begin
                 RecPlotFrm.PlotFrequency( EventAnalysis.NumEvents / Settings.RTEventAnalysis.CountingInterval ) ;
                 EventAnalysis.NumEvents := 0 ;
                 EventAnalysis.ScanCounter := 0 ;
                 end ;

              { Next block of channels }
              iScan := iScan + Main.SESLabIO.ADCNumChannels ;
              if iScan >= WriteBufs.NumSamples then
                 iScan := iScan - WriteBufs.NumSamples ;

              end ;


        end ;

     if Settings.RTResistance.InUse then CalculateResistance ;

     Settings.NewCalculation := False ;
     end ;


procedure TRecordFrm.CalculateRatio ;
// ------------------------------------
// Calculate ratio betweem two channels
// ------------------------------------
var
   iScan,ch : integer ;
   Ratio,Numerator,Denominator,Threshold : single ;
begin

   if Settings.NewCalculation then
      begin

      { Ratio O/P channel settings }
      if Settings.Fluorescence.RatioChan >= 0 then
         begin
         Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.RatioChan] :=
                             Settings.Fluorescence.RatioDisplayMax/Main.SESLabIO.ADCMaxValue ;
         Fluor.RatioScale := 1.0 / Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.RatioChan] ;
         Fluor.RatioOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Fluorescence.RatioChan] ;
         Main.SESLabIO.ADCChannelName[Settings.Fluorescence.RatioChan] :=
            Main.SESLabIO.ADCChannelName[Settings.Fluorescence.NumerChan] + '/' +
            Main.SESLabIO.ADCChannelName[Settings.Fluorescence.DenomChan] ;
         Main.SESLabIO.ADCChannelUnits[Settings.Fluorescence.RatioChan] := ' ';
         end
      else Fluor.RatioOffset := -1 ;

      { Concentration O/P channel settings }
      if Settings.Fluorescence.ConcChan >= 0 then
         begin
         Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.ConcChan] :=
                             Settings.Fluorescence.ConcDisplayMax/Main.SESLabIO.ADCMaxValue ;
         Fluor.ConcScale := Settings.Fluorescence.Keff /
                            Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.ConcChan] ;
         Fluor.ConcOffset := Main.SESLabIO.ADCChannelOffset[Settings.Fluorescence.ConcChan] ;
         Main.SESLabIO.ADCChannelName[Settings.Fluorescence.ConcChan] := Settings.Fluorescence.IonName ;
         Main.SESLabIO.ADCChannelUnits[Settings.Fluorescence.ConcChan] := Settings.Fluorescence.IonUnits ;
         end
      else Fluor.ConcOffset := -1 ;

      { Set channel information }
      for ch := 0 to Main.SESLabIO.ADCNumChannels -1 do
          begin
          scDisplay.ChanOffsets[ch] := Main.SESLabIO.ADCChannelOffset[ch] ;
          scDisplay.ChanUnits[ch] := Main.SESLabIO.ADCChannelUnits[Ch] ;
          scDisplay.ChanName[ch] := Main.SESLabIO.ADCChannelName[Ch] ;
          scDisplay.ChanScale[ch] := Abs(Main.SESLabIO.ADCChannelUnitsPerBit[Ch]) ;
          scDisplay.HorizontalCursors[ch] := Main.SESLabIO.ADCChannelZero[ch] ;
          scDisplay.yMin[ch] := Main.SESLabIO.ADCChannelYMin[ch] ;
          scDisplay.yMax[ch] := Main.SESLabIO.ADCChannelYMax[ch] ;
          scDisplay.ChanVisible[ch] := Main.SESLabIO.ADCChannelVisible[ch] ;
          end ;
      scDisplay.Invalidate ;

      if CDRFH.FileHandle >= 0 then SaveCDRHeader ( CDRFH ) ;
      Settings.NewCalculation := False ;
      end ;

   { Acceptance threshold for ratio calculation }
   Threshold := Settings.Fluorescence.FThreshold /
                Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.NumerChan] ;

   // Channel offsets and zero levels
   Fluor.NumerZero := Main.SESLabIO.ADCChannelZero[Settings.Fluorescence.NumerChan] ;
   Fluor.NumerOffset := Main.SESLabIO.ADCChannelOffset[Settings.Fluorescence.NumerChan] ;
   Fluor.NumerScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.NumerChan] ;
   Fluor.DenomZero := Main.SESLabIO.ADCChannelZero[Settings.Fluorescence.DenomChan] ;
   Fluor.DenomOffset := Main.SESLabIO.ADCChannelOffset[Settings.Fluorescence.DenomChan] ;
   Fluor.DenomScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Fluorescence.DenomChan] ;
   // Ensure computed channel zero are kept at zero
   if Settings.Fluorescence.RatioChan >= 0 then
      Main.SESLabIO.ADCChannelZero[Settings.Fluorescence.RatioChan] := 0 ;
   if Settings.Fluorescence.ConcChan >= 0 then
      Main.SESLabIO.ADCChannelZero[Settings.Fluorescence.ConcChan] := 0 ;

   iScan := WriteBufs.OldestScan ;
   while iScan <> WriteBufs.LatestScan do
         begin
         { Calculate ratio of fluorescence channels }
         Numerator := (ADC^[iScan+Fluor.NumerOffset] - Fluor.NumerZero)*Fluor.NumerScale ;
         Denominator := (ADC^[iScan+Fluor.DenomOffset] - Fluor.DenomZero)*Fluor.DenomScale ;
         if (Numerator >= Threshold) and (Denominator >= Threshold) then Ratio := Numerator/Denominator
                                                                    else Ratio := 0.0 ;

         if Fluor.RatioOffset >= 0 then
            begin
            ADC^[iScan+Fluor.RatioOffset] := Round(Ratio*Fluor.RatioScale) ;

            { Calculate ion concentration using binding eqn. }
            if Fluor.ConcOffset >= 0 then
               begin
               if Ratio <> Settings.Fluorescence.RMax then
                  ADC^[iScan+Fluor.ConcOffset] := Round( Fluor.ConcScale *
                                              (Ratio - Settings.Fluorescence.RMin) /
                                              (Settings.Fluorescence.RMax - Ratio) )
               else ADC^[iScan+Fluor.ConcOffset] := 0 ;
               end ;
            { Next block of channels }
            iScan := iScan + Main.SESLabIO.ADCNumChannels ;
            if iScan >= WriteBufs.NumSamples then iScan := iScan - WriteBufs.NumSamples ;
            end ;
        end ;

   end;



procedure TRecordFrm.CalculateCapacity ;
// ----------------------------
// On-line capacity calculation
// ----------------------------
const
     mVToVolt = 1E-3 ;
     pFToFarad = 1E-12 ;
     nSToSiemen = 1E-9 ;
     pAToAmp = 1E-12 ;
     TwoPi = 2*pi ;
var
   A,X,GImag,GReal,Gm,Gs,Vm,Im,Cm : single ;
   iScan,iZero : Integer ;
   OK : Boolean ;
   ch : Integer ;
   yDiff : Integer ;
   Gdc,cm2 : single ;
   Omega,Denom : Single ;
begin

     if Settings.NewCalculation then begin
        // Set channel names and units }

        { Cell membrane capacity }
        if Settings.Capacity.CmChan >= 0 then begin
            Main.SESLabIO.ADCChannelName[Settings.Capacity.CmChan] := 'Cm' ;
            Main.SESLabIO.ADCChannelUnits[Settings.Capacity.CmChan] := 'pF' ;
            Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.CmChan] :=
                               Settings.Capacity.CmDisplayMax/Main.SESLabIO.ADCMaxValue ;
            Main.SESLabIO.ADCChannelYMin[Settings.Capacity.CmChan] := Main.SESLabIO.ADCMinValue div 10 ;
            Cap.CmScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.CmChan]*pFToFarad ;
            Cap.CmOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.CmChan] ;
            end
        else Cap.CmOffset := -1 ;

        { Cell membrane conductance }
        if Settings.Capacity.GmChan >= 0 then begin
           Main.SESLabIO.ADCChannelName[Settings.Capacity.GmChan] := 'Gm' ;
           Main.SESLabIO.ADCChannelUnits[Settings.Capacity.GmChan] := 'nS' ;
           Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GmChan] :=
                                        Settings.Capacity.GmDisplayMax/Main.SESLabIO.ADCMaxValue ;
           Main.SESLabIO.ADCChannelYMin[Settings.Capacity.GmChan] := (-Main.SESLabIO.ADCMaxValue-1) div 10 ;
           Cap.GmScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GmChan]*nSToSiemen ;
           Cap.GmOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.GmChan] ;
           end
        else Cap.GmOffset := -1 ;

        { Cell access conductance }
        if Settings.Capacity.GsChan >= 0 then begin
           Main.SESLabIO.ADCChannelName[Settings.Capacity.GsChan] := 'Gs' ;
           Main.SESLabIO.ADCChannelUnits[Settings.Capacity.GsChan] := 'nS' ;
           Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GsChan] :=
                                     Settings.Capacity.GsDisplayMax/Main.SESLabIO.ADCMaxValue ;
           Main.SESLabIO.ADCChannelYMin[Settings.Capacity.GsChan] := (-Main.SESLabIO.ADCMaxValue-1) div 10 ;
           Cap.GsScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GsChan]*nSToSiemen ;
           Cap.GsOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.GsChan] ;
           end
        else Cap.GsOffset := -1 ;

        { Cell membrane current }
        Main.SESLabIO.ADCChannelName[Settings.Capacity.ImChan] := 'Im' ;
        Main.SESLabIO.ADCChannelUnits[Settings.Capacity.ImChan] := 'pA' ;
        Cap.ImOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.ImChan] ;
        Cap.ImScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.ImChan]*pAToAmp ;

        { Cell membrane potential }
        Main.SESLabIO.ADCChannelName[Settings.Capacity.VmChan] := 'Vm' ;
        Main.SESLabIO.ADCChannelUnits[Settings.Capacity.VmChan] := 'mV' ;
        Cap.VmOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.VmChan] ;
        Cap.VmScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.VmChan]*mVToVolt ;

        { Real component of cell membrane impedance }
        Main.SESLabIO.ADCChannelName[Settings.Capacity.GrealChan] := 'Greal' ;
        Main.SESLabIO.ADCChannelUnits[Settings.Capacity.GrealChan] := 'nS' ;
        Cap.GrealOffset :=  Main.SESLabIO.ADCChannelOffset[Settings.Capacity.GrealChan] ;
        // Calculate scaling factor
        if Settings.Capacity.GChannelsUseGainTelegraph then begin
           // Apply current gain factor from telegraph (if in use)
           Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GrealChan] :=
               Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GrealChan] /
               Main.SESLabIO.ADCChannelGain[Settings.Capacity.ImChan] ;
           end ;
        Cap.GrealScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GrealChan]*nSToSiemen ;
        //if Settings.Capacity.InvertGReal then Cap.GrealScale := -Cap.GrealScale ;

        { Imaginary component of cell membrane impedance }
        Main.SESLabIO.ADCChannelName[Settings.Capacity.GimagChan] := 'Gimag' ;
        Main.SESLabIO.ADCChannelUnits[Settings.Capacity.GimagChan] := 'nS' ;
        if Settings.Capacity.GChannelsUseGainTelegraph then begin
           // Apply current gain factor from telegraph (if in use)
           Main.SESLabIO.ADCChannelGain[Settings.Capacity.GimagChan] :=
               Main.SESLabIO.ADCChannelGain[Settings.Capacity.ImChan] ;
           end ;
        Cap.GimagScale := Main.SESLabIO.ADCChannelUnitsPerBit[Settings.Capacity.GimagChan]*nSToSiemen ;

        { Set channel information }
        for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
               scDisplay.ChanOffsets[ch] := Main.SESLabIO.ADCChannelOffset[ch] ;
               scDisplay.ChanUnits[ch] := Main.SESLabIO.ADCChannelUnits[Ch] ;
               scDisplay.ChanName[ch] := Main.SESLabIO.ADCChannelName[Ch] ;
               scDisplay.ChanScale[ch] := Abs(Main.SESLabIO.ADCChannelUnitsPerBit[Ch]) ;
               scDisplay.HorizontalCursors[ch] := Main.SESLabIO.ADCChannelZero[ch] ;
               scDisplay.yMin[ch] := Main.SESLabIO.ADCChannelYMin[ch] ;
               scDisplay.yMax[ch] := Main.SESLabIO.ADCChannelYMax[ch] ;
               scDisplay.ChanVisible[ch] := Main.SESLabIO.ADCChannelVisible[ch] ;
               end ;
        scDisplay.Invalidate ;
        if CDRFH.FileHandle >= 0 then SaveCDRHeader ( CDRFH ) ;
        Settings.NewCalculation := False ;

        end ;

//     outputdebugString(PChar(format('%d %d ',[WriteBufs.OldestScan,WriteBufs.LatestScan]))) ;
     iScan := WriteBufs.OldestScan ;
     while iScan <> WriteBufs.LatestScan do begin

            {- Convert the input signals from binary to SI units - }

            iZero := Main.SESLabIO.ADCChannelZero[Settings.Capacity.ImChan] ;
            Im := (ADC^[iScan+Cap.ImOffset] - iZero)*Cap.ImScale ;
            if Settings.NewCalculation then Cap.ImSmoothed := Im ;
            Cap.ImSmoothed := 0.02*Im + (1.0 - 0.02)*Cap.ImSmoothed ;
            Cap.ImSmoothed := Im ;

            // Real conductance
            iZero := Main.SESLabIO.ADCChannelZero[Settings.Capacity.GRealChan] ;
            if Settings.Capacity.InvertGReal then begin
               ADC^[iScan+Cap.GRealOffset] := -(ADC^[iScan+Cap.GRealOffset] - iZero) + iZero ;
               end ;
            GReal := (ADC^[iScan+Cap.GrealOffset] - iZero)*Cap.GrealScale ;

            // Imaginary conductance
            iZero := Main.SESLabIO.ADCChannelZero[Settings.Capacity.GimagChan] ;
            if Settings.Capacity.InvertGImag then begin
               ADC^[iScan+Cap.GimagOffset] := -(ADC^[iScan+Cap.GimagOffset] - iZero) + iZero ;
               end ;
            GImag := (ADC^[iScan+Cap.GimagOffset] - iZero)*Cap.GimagScale ;

            iZero := Main.SESLabIO.ADCChannelZero[Settings.Capacity.VmChan] ;
            Vm := (ADC^[iScan+Cap.VmOffset] - iZero)*Cap.VmScale ;
            Cap.VmSmoothed := 0.02*Vm + (1.0 - 0.02)*Cap.VmSmoothed ;
            if Settings.NewCalculation then Cap.VmSmoothed := Vm ;
            //Cap.VmSmoothed := -0.05 ;

            Gm := 0.0 ;
            Gs := 0.0 ;
            Cm := 0.0 ;

            OK := True ;
	          if Cap.VmSmoothed <> Settings.Capacity.Vrev then begin
               Gdc := ABS(Cap.ImSmoothed/(Cap.VmSmoothed - Settings.Capacity.Vrev)) ;
               if GDC = 0.0 then OK := False ;
               end
            else OK := False ;

            if Settings.Capacity.CapacityCompensationInUse and OK then begin
               if Gdc = Settings.Capacity.RSeriesComp then Break ;
               Omega := TwoPi*Settings.Capacity.Frequency ;
               A := Omega*Settings.Capacity.CellCapacityComp*Settings.Capacity.RSeriesComp ;
               Denom := 1. + A*A ;
               GReal := GReal +
                        ((Omega*Omega*Settings.Capacity.CellCapacityComp*
                        Settings.Capacity.CellCapacityComp*
                         Settings.Capacity.RSeriesComp ) / Denom ) ;

               GImag := GImag +
                        ((Omega*Settings.Capacity.CellCapacityComp) / Denom ) ;

               end ;

            if GReal <> Gdc then Gs := GReal + ((GImag*GImag)/(GReal-Gdc))
                        else OK := False ;

            if Gs <> Gdc then Gm := (Gdc*Gs)/(Gs - Gdc)
                         else OK := False ;

            if (GImag <> 0.0) and OK then
               Cm := (Gs*Gs*(GReal - Gdc))/((Gs - Gdc)*GImag*TwoPi*Settings.Capacity.Frequency) ;

            if Cap.GsOffset >= 0 then
               ADC^[iScan+Cap.GsOffset] := Round(Min(Max(Gs/Cap.GsScale,
                                           0.0),Main.SESLabIO.ADCMaxValue-1)) ;
            if Cap.GmOffset >= 0 then
               ADC^[iScan+Cap.GmOffset] := Round(Min(Max(Gm/Cap.GmScale,
                                           0.0),Main.SESLabIO.ADCMaxValue-1)) ;
            if Cap.CmOffset >= 0 then
               ADC^[iScan+Cap.CmOffset] := Round(Min(Max(Cm/Cap.CmScale,
                                           -Main.SESLabIO.ADCMaxValue),Main.SESLabIO.ADCMaxValue-1)) ;
            { Next block of channels }
            iScan := iScan + Main.SESLabIO.ADCNumChannels ;
            if iScan >= WriteBufs.NumSamples then
               iScan := iScan - WriteBufs.NumSamples ;

            Settings.NewCalculation := False ;

//            Main.StatusBar.SimpleText := format('Vm(smoothed)=%.4g mV, G(dc)=%.4g nS',
//            [Cap.VmSmoothed*1E3,Gdc*1E9])  ;

            end ;
     end ;


procedure TRecordFrm.StartSineWave ;
{ -----------------------
  Start sine wave output
  -----------------------}
const
    NumDACChannels = 2 ;
var
   DACdt : single ;
   MaxDACPoints : Integer ;
   i,nDACValues : Integer ;
   x,xStep : single ;
   sVPeak : Single ;
   sVHold : Array[0..1] of Single ;
   DACScale : Single ;

begin

     if not StimulatorAvailable then Exit ;

     // Stop D/A output
     Main.SESLabIO.DACStop ;

     { D/A update interval }
     Main.SESLabIO.DigitalStimulusEnabled := False ;

     MaxDACPoints := Main.SESLabIO.DACBufferLimit div (NumDACChannels) - 10 ;

     DacDT := 1.0/(100.0*Settings.Capacity.Frequency) ;
     DACdt := Max( DACdt, Main.SESLabIO.DACMinUpdateInterval ) ;
     Main.SESLabIO.DACUpdateInterval := DACdt ;
     DACdt := Main.SESLabIO.DACUpdateInterval ;

     nDacValues := Min( Round( (10.0/(DacDT*Settings.Capacity.Frequency))), MaxDACPoints) ;
     sVPeak := (Settings.Capacity.SineWaveRMS/0.707)*Main.SESLabIO.DACHoldingVoltage[0] ;
     sVHold[0] := Main.SESLabIO.DACHoldingVoltage[0] ;
     sVHold[1] := Main.SESLabIO.DACHoldingVoltage[1] ;

     { D/A channel voltage -> bits scaling factors }
     x := 0.0 ;
     xStep := 2.0*Pi*Settings.Capacity.Frequency*DacDT ;
     for i := 0 to nDacValues-1 do begin
         // DAC channel 0
         DACScale := Main.SESLabIO.DACMaxValue/ Main.SESLabIO.DACVoltageRange[0] ;
         DAC^[i*NumDACChannels] := Round((sVPeak*Sin(x) + sVHold[0])*DACScale) ;
         // Other channel held at holding potential
         DACScale := Main.SESLabIO.DACMaxValue/ Main.SESLabIO.DACVoltageRange[1] ;
         DAC^[i*NumDACChannels+1] := Round(sVHold[1]*DACScale) ;
         x := x + xStep ;
         end ;

     Main.SESLabIO.DACNumChannels := NumDACChannels ;
     Main.SESLabIO.DACNumSamples := nDacValues ;

     Main.SESLabIO.DACRepeatWaveform := True ;
     Main.SESLabIO.DACStart ;

     end ;


procedure TRecordFrm.CalculateResistance ;
// --------------------------------
// Real-time resistance calculation
// --------------------------------
const
    EndMargin = 1E-3 ; //ms
    ResistanceStimFileName = 'RTest.XML' ;
var
    i,iScan : Integer ;
    VTest, ITest, R, G : Single ;
    iVm,iIm,iStep : Integer ;
begin

    // Initialise event detector
    if Settings.NewCalculation then begin

       // Set voltage channel and scaling
       RAnalysis.VmChan := Settings.RTResistance.VmChannel ;
       RAnalysis.VmUnits := Main.SESLabIO.ADCChannelUnits[RAnalysis.VmChan] ;
       if RAnalysis.VmUnits = 'mV' then RAnalysis.ScaleToVolts := 1E-3
                                   else RAnalysis.ScaleToVolts := 1.0 ;

       // Set current channel and scaling
       RAnalysis.ImChan := Settings.RTResistance.ImChannel ;
       RAnalysis.ImUnits := Main.SESLabIO.ADCChannelUnits[RAnalysis.ImChan] ;
       if AnsiContainsText(RAnalysis.ImUnits,'pa') then RAnalysis.ScaleToAmps := 1E-12
       else if AnsiContainsText(RAnalysis.ImUnits,'na') then RAnalysis.ScaleToAmps := 1E-9
       else if AnsiContainsText(RAnalysis.ImUnits,'ua') then RAnalysis.ScaleToAmps := 1E-6
       else if AnsiContainsText(RAnalysis.ImUnits,'ma') then RAnalysis.ScaleToAmps := 1E-3
       else RAnalysis.ScaleToAmps := 1.0 ;

       RAnalysis.NAvgMin := Max(Round(0.2*(Settings.RTResistance.Duration/Main.SESLabIO.ADCSamplingInterval)),1) ;
       RAnalysis.Smooth := 1.0/RAnalysis.NAvgMin ;
       RAnalysis.Level := -2 ;
       RAnalysis.PreviousValue := 0 ;
       for i := 0 to High(RAnalysis.VAvg) do begin
           RAnalysis.VAvg[i] := 0.0 ;
           RAnalysis.IAvg[i] := 0.0 ;
           RAnalysis.NAvg[i] := 0 ;
           end ;
       if Amplifier.ClampMode[0] = amCurrentClamp then begin
          RAnalysis.Threshold := Round( (0.25*Settings.RTResistance.Amplitude/(RAnalysis.ScaleToAmps*1E12))/
                                     Main.SESLabIO.ADCChannelUnitsPerBit[RAnalysis.ImChan]);
          end
       else begin
          RAnalysis.Threshold := Round( (0.1*Settings.RTResistance.Amplitude/(RAnalysis.ScaleToVolts*1E3))/
                                     Main.SESLabIO.ADCChannelUnitsPerBit[RAnalysis.VmChan]);
          end ;
       RAnalysis.Polarity := Sign(RAnalysis.Threshold) ;
       RAnalysis.Threshold := Abs(RAnalysis.Threshold);

       // Create resistance stimulus pulse
       Stimulator.ClearProtocol( Stimulator.Prot ) ;
       Stimulator.Prot.NumAOChannels := 1 ;
       If (Amplifier.ClampMode[0] = amCurrentClamp) and
          (Amplifier.AmplifierType[0] <> amNone) then begin
          // Current clamp mode
          Stimulator.Prot.AOStimType[0] := 0 ;
          Stimulator.Prot.AOChannelUnits[0] := CurrentClampUnits ;
          Stimulator.Prot.AOScale[0] := 1E12 ;
          end
       else begin
          // voltage clamp mode
          Stimulator.Prot.AOStimType[0] := 2 ;
          Stimulator.Prot.AOChannelUnits[0] := VoltageClampUnits ;
          Stimulator.Prot.AOScale[0] := 1E3 ;
          end ;

       Stimulator.Prot.Stimulus[0].WaveShape := wvStep0 ;
       Stimulator.Prot.Stimulus[0].Parameters[spDelay].Exists := True ;
       Stimulator.Prot.Stimulus[0].Parameters[spDelay].Value := Settings.RTResistance.Interval*0.1 ;
       Stimulator.Prot.Stimulus[0].Parameters[spDuration].Exists := True ;
       Stimulator.Prot.Stimulus[0].Parameters[spDuration].Value := Settings.RTResistance.Duration ;
       Stimulator.Prot.Stimulus[0].Parameters[spStartAmplitude].Exists := True ;
       Stimulator.Prot.Stimulus[0].Parameters[spStartAmplitude].Value := Settings.RTResistance.Amplitude ;
       Stimulator.Prot.Stimulus[0].Parameters[spEndAmplitude].Exists := True ;
       Stimulator.Prot.Stimulus[0].Parameters[spEndAmplitude].Value := Settings.RTResistance.Amplitude ;
       Stimulator.Prot.StimulusPeriod := Settings.RTResistance.Interval ;
       Stimulator.Prot.NumRecords := 1 ;
       Stimulator.Prot.NumRepeatsPerIncrement := 1 ;
       Stimulator.Prot.RepeatedProtocol := True ;
       Stimulator.Prot.NextProtocolFileName := '' ;

       // Save file to stim folder
       ActiveStimFileName := Main.VProtDirectory + ResistanceStimFileName ;
       if FileExists(ActiveStimFileName) then DeleteFile(PChar(ActiveStimFileName)) ;
       Stimulator.SaveProtocolToXMLFile(Stimulator.Prot, ActiveStimFileName);

       // Request stimulus pulse to be started
       StimulatorStartRequired := True ;

       end ;

    // Open plotting window (if not already open)
    if not Main.FormExists( 'RecPlotFrm' ) then begin
       RecPlotFrm := TRecPlotFrm.Create(Self) ;
       RecPlotFrm.Width :=  Main.ClientWidth - RecordFrm.Left - RecordFrm.Width - 20 ;
       RecPlotFrm.Left := RecordFrm.Left + RecordFrm.Width + 2 ;
       RecPlotFrm.Top := 10 ;
       end ;

    iScan := WriteBufs.OldestScan ;
    while iScan <> WriteBufs.LatestScan do begin

          // Get current and voltage
          iVm := ADC^[iScan+Main.SESLabIO.ADCChannelOffset[RAnalysis.VmChan]] ;
          iIm := ADC^[iScan+Main.SESLabIO.ADCChannelOffset[RAnalysis.ImChan]] ;
          if RAnalysis.Level = -2 then begin
             if Amplifier.ClampMode[0] = amCurrentClamp then RAnalysis.PreviousValue := iIm
                                                        else RAnalysis.PreviousValue := iVm ;
             RAnalysis.Level := -1 ;
             end ;

          // Locate pulse using voltage step in voltage-clamp mode
          // and current step in current-clamp mode.
          if Amplifier.ClampMode[0] = amCurrentClamp then begin
             iStep := iIm - RAnalysis.PreviousValue ;
             RAnalysis.PreviousValue := iIm ;
             end
          else begin
             iStep := iVm - RAnalysis.PreviousValue ;
             RAnalysis.PreviousValue := iVm ;
             end ;

          if iStep*RAnalysis.Polarity > RAnalysis.Threshold then begin
             // Transition to test level
             RAnalysis.Level := 1 ;
             RAnalysis.NAvg[RAnalysis.Level] := 0 ;
             end
          else if (iStep*RAnalysis.Polarity) < (-RAnalysis.Threshold) then begin
             // Transition to holding level
             RAnalysis.Level := 0 ;
             if (RAnalysis.NAvg[0] >= RAnalysis.NAvgMin) and
                (RAnalysis.NAvg[1] >= RAnalysis.NAvgMin) then begin
                VTest := (RAnalysis.VAvg[1] - RAnalysis.VAvg[0])*
                      Main.SESLabIO.ADCChannelUnitsPerBit[RAnalysis.VmChan]*RAnalysis.ScaleToVolts ;
                ITest := (RAnalysis.IAvg[1] - RAnalysis.IAvg[0])*
                      Main.SESLabIO.ADCChannelUnitsPerBit[RAnalysis.ImChan]*RAnalysis.ScaleToAmps ;
                // Calculate conductance & resistance
                if Abs(VTest) > 1E-4 then G := ITest / VTest
                                     else G := 0.0 ;
                if G <> 0.0 then R := 1/G
                            else R := 0.0 ;

                // Update plot
                case Settings.RTResistance.Plot of
                    0 : RecPlotFrm.PlotCurrent( ITest ) ;
                    1 : RecPlotFrm.PlotVoltage( VTest ) ;
                    2 : RecPlotFrm.PlotResistance( R ) ;
                    3 : RecPlotFrm.PlotConductance( G ) ;
                    end ;
                end ;
             RAnalysis.NAvg[RAnalysis.Level] := 0 ;
             end ;

          if RAnalysis.Level >= 0 then begin
             RAnalysis.IAvg[RAnalysis.Level] := RAnalysis.Smooth*iIm +
                                             (1.0 - RAnalysis.Smooth)*RAnalysis.IAvg[RAnalysis.Level] ;
             RAnalysis.VAvg[RAnalysis.Level] := RAnalysis.Smooth*iVm +
                                             (1.0 - RAnalysis.Smooth)*RAnalysis.VAvg[RAnalysis.Level] ;
             Inc(RAnalysis.NAvg[RAnalysis.Level]);
             end ;

              //outputdebugString(PChar(format('%d %d ',[yDiff,Round(EventAnalysis.Baseline)]))) ;

          { Next block of channels }
          iScan := iScan + Main.SESLabIO.ADCNumChannels ;
          if iScan >= WriteBufs.NumSamples then
             iScan := iScan - WriteBufs.NumSamples ;

          end ;

     Settings.NewCalculation := False ;

     end ;



procedure TRecordFrm.DisplaySignalLevels(
          var Buf : Array of SmallInt ;    { A/D  data buffer }
          StartAt : Integer                { Start at sample }
          ) ;
{ -------------------------------------------------------
  Display signal levels for a block of input channels
  ------------------------------------------------------- }
begin

   //  for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
      //   Value := (Buf[StartAt+Main.SESLabIO.ADCChannelOffset[ch]] - Main.SESLabIO.ADCChannelZero[ch])
      //            * Main.SESLabIO.ADCChannelUnitsPerBit[ch] ;
   {      lbReadOut.Add( format('%s=%7.4g %s ',
                        [RecChannel[ch].ADCName,
                         Value,
                         RecChannel[ch].ADCUnits] )) ;}
      //   end ;
     {lbReadOut.Invalidate ;}
     end ;



procedure TRecordFrm.StopADCandDAC ;
{ ---------------------------------------------
  Shut down A/D, D/A and digital O/P subsystems
  ---------------------------------------------}
begin

     { Disable A/D }
     Main.SESLabIO.ADCStop ;

     { Disable D/A waveform generation }
     Main.SESLabIO.DACStop ;

     UpdateOutputs ;

     bStartStimulus.Enabled := True ;
     bStopStimulus.Enabled := False ;

     Timer.Enabled := False ;

     end ;


procedure TRecordFrm.bRecordClick(Sender: TObject);
{ -----------------------
  Start recording to file
  ----------------------- }
var
  ch : Integer ;
begin

     //
     if CDRFH.FileHandle < 0 then begin
        ShowMessage('RECORD: No data file! Select File/New to create a data file') ;
        Exit ;
        end ;

     // Open and new data file (with + appended) if channel scaling settings changed
     CheckNewDataFileNeeded ;

     // Copy data into file header
     CDRFH.NumChannels := Main.SESLabIO.ADCNumChannels ;
     CDRFH.dt := Main.SESLabIO.ADCSamplingInterval ;
     CDRFH.ADCVoltageRange := Main.SESLabIO.ADCVoltageRange ;
     CdrFH.NumBytesInHeader := NumBytesInHeader ;

     bRecord.Enabled := False ;
     bStop.Enabled := True ;
     //TDisplayPanel.Enabled := False ;
     bMark.Enabled := True ;
     bUpdateGain.Enabled := False ;
     AmpGainGrp.Enabled := False ;
     RecordPanel.Enabled := bRecord.Enabled ;

     ckFixedZeroLevels.Enabled := False ;

     { Update number of recording sweeps to be done }
     // Set no. of records to acquire if this first sweep of ext. triggered sweeps
     if (WriteBufs.NumRecordsToAcquire = 0) and rbExtTrigger.Checked then begin
        Settings.NumTriggerSweeps := Round(edNumTriggerSweeps.Value) ;
        WriteBufs.NumRecordsToAcquire := Settings.NumTriggerSweeps ;
        end ;

     { Number of bytes to be acquired from A/D converter }
     Settings.RecordDuration := edRecordDuration.Value ;
     WriteBufs.NumSamplesInFileRequired := CDRFH.NumSamplesInFile +
                                           Round((Settings.RecordDuration*Main.SESLabIO.ADCNumChannels)
                                                 /Settings.ADCSamplingInterval ) ;

     { Sweep count if in Free Run or Ext. Trigger mode }
     if rbExtTrigger.Checked then begin
        SweepStatus := format( 'Sweep %d/%d : ',
                                  [Settings.NumTriggerSweeps-WriteBufs.NumRecordsToAcquire+1,
                                   Settings.NumTriggerSweeps]
                                    ) ;
        end
     else SweepStatus := '' ;

     WriteBufs.TAcquired := 0.0 ;
     Main.StatusBar.SimpleText := SweepStatus + RecordingStatus + StimulusStatus ;

     WriteToLogFile( SweepStatus + format(
                    ' Started at %.4g s',[(CdrFH.NumSamplesInFile*CdrFH.dt)/
                                           CdrFH.NumChannels])  ) ;

     FileSeek(CdrFH.FileHandle,0,2) ;

     UpdateChannelSettings ;

     // Copy recording channel settings to file channel settings
     CDRFH.ADCVoltageRange := Main.SESLabIO.ADCVoltageRange ;
     CDRFH.ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
     CDRFH.NumChannels := Main.SESLabIO.ADCNumChannels ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
        Channel[ch].ADCName := Main.SESLabIO.ADCChannelName[ch] ;
        Channel[ch].ADCUnits := Main.SESLabIO.ADCChannelUnits[ch] ;
        Channel[ch].yMin := Main.SESLabIO.ADCChannelYMin[ch] ;
        Channel[ch].yMax := Main.SESLabIO.ADCChannelYMax[ch] ;
        Channel[ch].yScale := Main.SESLabIO.ADCChannelUnitsPerBit[ch] ;
        Channel[ch].ADCSCale := Main.SESLabIO.ADCChannelUnitsPerBit[ch] ;
        Channel[ch].ADCCalibrationFactor := Main.SESLabIO.ADCChannelVoltsPerUnit[ch] ;
        Channel[ch].ADCAmplifierGain := Main.SESLabIO.ADCChannelGain[ch] ;
        Channel[ch].ADCZero := Main.SESLabIO.ADCChannelZero[ch] ;
        Channel[ch].ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
        end ;

     DisplayLive( True ) ;

     WriteBufs.Recording := True ;

     // Stop A/D recording (to force a re-start)
     RestartADC ;

     RecordingStatus := format( 'Recording: 0/%.1f s (0 Kb) ',
                                [Settings.RecordDuration]) ;

     // Start stimulus if start on record is requested
     if ckStartStimOnRecord.Checked then bStartStimulus.Click ;

     end;


function TRecordFrm.CheckNewDataFileNeeded : Boolean ;
// ------------------------------------------------------------
// Check record size and created a new file if size has changed
// ------------------------------------------------------------
var
    NewFileNeeded : Boolean ;
    ch,n : Integer ;
    NewFileName,Stem : String ;
begin

     NewFileNeeded := False ;
     Result := True ;

     // Not required if file is empty
     if CDRFH.NumSamplesInFile <= 0 then Exit ;

     if CDRFH.NumChannels <> Main.SESLabIO.ADCNumChannels then NewFileNeeded := True ;

     if Abs((CDRFH.dt/Main.SESLabIO.ADCSamplingInterval)-1.0) > 1E-4 then NewFileNeeded := True ;
     if CDRFH.ADCVoltageRange <> Main.SESLabIO.ADCVoltageRange then NewFileNeeded := True ;

     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
         if Channel[ch].ADCName <> Main.SESLabIO.ADCChannelName[ch] then NewFileNeeded := True ;
         if Channel[ch].ADCUnits <> Main.SESLabIO.ADCChannelUnits[ch] then NewFileNeeded := True ;
         if Abs((Channel[ch].ADCCalibrationFactor/Main.SESLabIO.ADCChannelVoltsPerUnit[ch])-1.0) > 1E-4 then NewFileNeeded := True ;
         if Abs((Channel[ch].ADCAmplifierGain/Main.SESLabIO.ADCChannelGain[ch])-1.0) > 1E-4 then NewFileNeeded := True ;
        end ;

     if NewFileNeeded then begin
        // Extract stem of file name
        Stem := ANSIReplaceText( CDRFH.FileName, '.edr', '.' ) ;
        n := Pos( '.', Stem ) ;
        if n > 0 then Stem := LeftStr(Stem,n) ;

        n := 1 ;
        repeat
          NewFileName := Stem + format('%d.edr',[n]) ;
          Inc(n) ;
          until not FileExists(NewFileName) ;

        // New file with '+' added to end of file name
        CDRFH.FileName :=NewFileName ;
        if not FileExists(NewFileName) then begin
           Main.CreateNewDataFile( CDRFH ) ;
           end
        else begin
           ShowMessage('Additional data file ' + NewFileName + ' already exists. New data file required!') ;
           Result := False ;
           end ;

        end ;
     end ;


procedure TRecordFrm.FormResize(Sender: TObject);
{ ------------------------------------------
  Adjust components when display is re-sized
  ------------------------------------------ }
begin

     ReadOutGrp.Height := Max( ClientHeight - ReadoutGrp.Top - 5, 2) ;
     lbReadout.Height := Max( ReadOutGrp.ClientHeight - lbReadout.Top - 5, 2) ;
     ckFixedZeroLevels.Top := ReadOutGrp.ClientHeight - ckFixedZeroLevels.Height - 2 ;

     StatusGrp.Top := ClientHeight - StatusGrp.Height - 5 ;
     StatusGrp.Width := Max( ClientWidth - StatusGrp.Left - 5, 2) ;
     TDisplayPanel.Left := StatusGrp.Width - TDisplayPanel.Width - 2 ;

     scDisplay.Height := Max(StatusGrp.Top - scDisplay.Top  - 10,2) ;
     scDisplay.Width := Max( ClientWidth - scDisplay.Left - 5,2) ;

     // Adjust width of ident boxes to match display area
     edIdent.Width := scDisplay.Left + scDisplay.Width - edIdent.Left ;

     if not Initialised then Timer.Enabled := False
                        else Timer.Enabled := True ;

     end;


procedure TRecordFrm.bStopClick(Sender: TObject);
{ ---------------
  Stop recording
  --------------- }
begin
     { Set number of records to be acquired to zero }
     WriteBufs.NumRecordsToAcquire := 0 ;

     // Restore origin holding voltage
     UpdateOutputs ;

     { Stop recording to disk and shutdown A/D & D/A conversion }
     StopRecording ;

     // Stop A/D recording (to force a re-start)
     Main.SESLabIO.ADCStop ;

     end;


procedure TRecordFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ -----------------------------
  Save settings and close form
  ----------------------------- }
begin

     { Save data to file header }
     if CDRFH.FileHandle >= 0 then SaveCDRHeader( CdrFH ) ;

     // Close frequency plotting window if it is open
     if Main.FormExists( 'RecPlotFrm' ) then begin
        RecPlotFrm.Close  ;
        end ;

     { Shut down Lab. interface hardware ... essential to avoid program crash }
     StopADCandDAC ;

     Main.SetMenus ;

     Action := caFree ;

     Main.mnRecord.Enabled := True ;
     Timer.Enabled := False ;
     Initialised := False ;

     end;


procedure TRecordFrm.FormCreate(Sender: TObject);
begin
    DisplayBuf := Nil ;
    DisplayBufSize := 0 ;
    end;

procedure TRecordFrm.rbFreeRunClick(Sender: TObject);
{ ----------------------------
  Set trigger mode to free run
  ----------------------------}
begin
     Settings.TriggerMode := 'F' ;
     end;


procedure TRecordFrm.rbExtTriggerClick(Sender: TObject);
{ ------------------------------------
  Set trigger mode to External trigger
  ------------------------------------}
begin
     Settings.TriggerMode := 'E' ;
     end;


procedure TRecordFrm.FormDeactivate(Sender: TObject);
{ ---------------------------------------
  Actions to be taken if form loses focus
  ---------------------------------------}
begin
    
     { If we're recording give up (in disgust!!) }
    { bStop.Click ;}
     //Timer.Enabled := False ;
     Main.SetMenus ;
     end;


procedure TRecordFrm.FormDestroy(Sender: TObject);
begin
    if DisplayBuf = Nil then FreeMem(DisplayBuf) ;
    end;

procedure TRecordFrm.edIdentKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------
  Update identification text line
  -------------------------------}
begin
     { Only save to log file if recording is not in progress }
     if (bRecord.Enabled) and (key = chr(13)) then WriteToLogFile( CdrFH.IdentLine ) ;
     end;


procedure TRecordFrm.FormActivate(Sender: TObject);
{ --------------------------------------------
  Activities required when form becomes active
  --------------------------------------------}
begin

     // Stop seal test activity
     if Main.FormExists('SealTestFrm') then SealTestFrm.StopADCandDAC ;

     if Initialised then begin
        // Update amplifier gain and mode controls
        UpdateAmplifierGain ;
        end ;

     if not bStopStimulus.Enabled and
        Main.SESLabIO.DACAvailableWhileADCActive then Settings.UpdateOutputs := True ;

     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     Timer.Enabled := True ;

     end;


procedure TRecordFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{ --------------
  Function keys
  -------------}
begin
     case Key of
          { F1 starts recording }
          VK_F1 : if bRecord.enabled then bRecord.Click ;
          { F2 stops recording }
          VK_F2 : if bStop.enabled then bStop.click ;
          end ;
     end;


procedure TRecordFrm.edTDisplayKeyPress(Sender: TObject;
  var Key: Char);
{ -------------------------
  Change display duration
  ------------------------- }
begin
     if key = #13 then begin
        Settings.DisplayDuration := edTDisplay.Value ;
        // Stop A/D recording (to force a re-start)
        if not WriteBufs.Recording then RestartADC ;
        end ;
     end;


procedure TRecordFrm.edRecordDurationKeyPress(Sender: TObject;
  var Key: Char);
{ -------------------------
  Change recording duration
  ------------------------- }
begin
     if key = chr(13) then Settings.RecordDuration := edRecordDuration.Value ;
     end;


procedure TRecordFrm.bStartStimulusClick(Sender: TObject);
// -----------------------------------
// Request start of a stimulus program
// -----------------------------------
begin
     if cbStimProgram.ItemIndex > 0 then begin
        if not ckResistance.Checked then ActiveStimFileName := Settings.VProgramFileName ;
        StimulatorStartRequired := True ;
        end ;
     end;


procedure TRecordFrm.bSpecialClick(Sender: TObject);
{ -------------------------------------------------------
  Set channels/parameters for real-time computed channels
  ------------------------------------------------------- }
begin
     if Main.FormExists('SetCompFrm') then begin
        if SetCompFrm.WindowState = wsMinimized then SetCompFrm.WindowState := wsNormal ;
        SetCompFrm.BringToFront ;
        //SetCompFrm.SetFocus ;
        end
     else begin
        SetCompFrm := TSetCompFrm.Create(Self) ;
        SetCompFrm.Left := 15 ;
        SetCompFrm.Top := 15 ;
        end ;

     SetCompFrm.ResistanceStimulusUnits := edAO0Hold.Units ;
     if ckFluorescence.Checked then SetCompFrm.SelectPage(0)
     else if ckCapacity.Checked then SetCompFrm.SelectPage(1)
     else if ckEventFrequency.Checked then SetCompFrm.SelectPage(2)
     else if ckResistance.Checked then SetCompFrm.SelectPage(3) ;

     end ;


procedure TRecordFrm.ckFluorescenceClick(Sender: TObject);
{ -------------------------------------------------------
  Enable/disable real-time fluorescence ratio computation
  ------------------------------------------------------- }
begin

     if ckFluorescence.Checked then begin
        ckEventFrequency.Checked := False ;
        ckResistance.Checked := False ;
        ckCapacity.Checked := False ;
        end ;

     Settings.Fluorescence.InUse := ckFluorescence.Checked ;
     Main.SESLabIO.ADCStop ;
     end;


procedure TRecordFrm.scDisplayCursorChange(Sender: TObject);
var
   ch : Integer ;
begin
    if Main.SESLabIO.ADCActive then begin
       { Update zero level }
       //ch := scDisplay.ActiveHorizontalCursor ;
//       if ch >= 0 then Main.SESLabIO.ADCChannelZero[ch] := scDisplay.HorizontalCursors[ch] ;
       for ch := 0 to scDisplay.NumChannels-1 do begin
          { Get signal baseline cursor }
          if Settings.FixedZeroLevels then begin
             if scDisplay.HorizontalCursors[ch] <> Main.SESLabIO.ADCChannelZero[ch] then
                scDisplay.HorizontalCursors[ch] := Main.SESLabIO.ADCChannelZero[ch] ;
             end
          else begin
             Main.SESLabIO.ADCChannelZero[ch] := Round(scDisplay.HorizontalCursors[ch]) ;
             end ;
          end ;

       { Update vertical display magnification }
       for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
           Main.SESLabIO.ADCChannelYMin[ch] := scDisplay.YMin[ch] ;
           Main.SESLabIO.ADCChannelYMax[ch] := scDisplay.YMax[ch] ;
           Main.SESLabIO.ADCChannelVisible[ch] := scDisplay.ChanVisible[ch] ;
           end ;
       end ;

    end;


procedure TRecordFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SESLabIO.ADCMinValue ;

     scDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDisplay.Invalidate ;
     end ;


procedure TRecordFrm.ZoomOut(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin

     scDisplay.YZoom( ChanNum, 50.0 ) ;

     end ;


procedure TRecordFrm.ZoomIn( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin

     scDisplay.YZoom( ChanNum, -50.0 ) ;

     end ;


procedure  TRecordFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDisplay.ZoomOut ;
     end ;


procedure TRecordFrm.ckCapacityClick(Sender: TObject);
{ -------------------------------------------------------
  Enable/disable real-time capacity computation
  ------------------------------------------------------- }
begin
     if ckCapacity.Checked then begin
        ckEventFrequency.Checked := False ;
        ckResistance.Checked := False ;
        ckFluorescence.Checked := False ;
        end ;

     Settings.Capacity.InUse := ckCapacity.Checked ;
     //if ckCapacity.Checked then ckFixedZeroLevels.Checked := True ;
     Main.SESLabIO.ADCStop ;
     end;


procedure TRecordFrm.ckFixedZeroLevelsClick(Sender: TObject);
// ---------------------------------------------------
// Force a re-start when fixed zero level mode changed
// ---------------------------------------------------
begin
     if not WriteBufs.Recording then Main.SESLabIO.ADCStop ;
     Settings.FixedZeroLevels := ckFixedZeroLevels.Checked ;
     end;


procedure TRecordFrm.UpdateOutputs ;
// ------------------------------
// Update D/A and digital outputs
// ------------------------------
var
   ch : Integer ;
begin

    { Return voltage command to holding voltage and Sync. O/P to OFF }
    for ch := 0 to Main.SESLabIO.DACMaxChannels-1 do
        Main.SESLabIO.DACHoldingVoltage[ch] := Main.SESLabIO.DACHoldingVoltage[ch] ;
    Main.SESLabIO.DIGHoldingLevel := Main.SESLabIO.DIGHoldingLevel ;

    UpdateAmplifierGain ;

    Settings.UpdateOutputs := False ;
    //Settings.NewCalculation := True ;

    end ;


procedure TRecordFrm.edIdentChange(Sender: TObject);
 {-------------------------------------------
  Update identification string in file header
  -------------------------------------------}
begin
     { Update ident line if it is changed }
     CdrFH.IdentLine := edIdent.text ;
     if bRecord.Enabled then SaveCDRHeader(CdrFH) ;
     end;


procedure TRecordFrm.bMarkClick(Sender: TObject);
// ------------------------------
//  Add a text marker to the chart
// ------------------------------
var
     MarkerTime : Single ;
     TimeScale : Single ;
begin

     if MarkerList.Count < MaxMarkers then begin

          TimeScale := scDisplay.TScale/Settings.TScale ;

          MarkerTime := (scDisplay.NumPoints + scDisplay.XOffset)*TimeScale ;
          // Plot marker on chart
          MarkerList.AddObject( EdMarker.text, TObject(MarkerTime) ) ;
          scDisplay.AddMarker( Round(MarkerTime/TimeScale) - scDisplay.XOffset,
                               EdMarker.text );

          end ;
     edNumMarkers.text := format('%d',[MaxMarkers-MarkerList.Count]);
     end;


procedure TRecordFrm.edMarkerKeyPress(Sender: TObject; var Key: Char);
// -----------------------------------------
// Add marker to chart when <CR> key pressed
// -----------------------------------------
begin
     if (Key = #13) and WriteBufs.Recording  then bMark.Click ;
     end;

procedure TRecordFrm.rbExtTriggerHighClick(Sender: TObject);
// ----------------------------------------
// Ext. Trigger Active High option selected
// ----------------------------------------
begin

     Settings.ExternalTriggerActiveHigh := rbExtTriggerHigh.Checked ;
     Main.SESLabIO.ADCExternalTriggerActiveHigh := Settings.ExternalTriggerActiveHigh ;
     rbExtTriggerHigh.Checked := Main.SESLabIO.ADCExternalTriggerActiveHigh ;

     end;


procedure TRecordFrm.rbExttriggerLowClick(Sender: TObject);
// ----------------------------------------
// Ext. Trigger Active Low option selected
// ----------------------------------------
begin
     Settings.ExternalTriggerActiveHigh := rbExtTriggerHigh.Checked ;
     Main.SESLabIO.ADCExternalTriggerActiveHigh := Settings.ExternalTriggerActiveHigh ;
     rbExtTriggerHigh.Checked := Main.SESLabIO.ADCExternalTriggerActiveHigh ;
     end;


procedure TRecordFrm.bUpdateGainClick(Sender: TObject);
//  ----------------------------------
// Force re-start to update Ch.0 gain
// ----------------------------------
begin
     if bRecord.Enabled {and (CDRFH.NumSamplesInFile = 0)} then begin
        ReadUserEnteredGain := True ;
        Settings.NewCalculation := True ;
        RestartADC ;
        end ;
     end;


procedure TRecordFrm.bStopStimulusClick(Sender: TObject);
{ ------------------------
  Toggle stimulator Off
  ------------------------ }
begin
     StopStimulator ;
     end;


procedure TRecordFrm.edAmplifierGainKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------------------
// Patch clamp amplifier gain changed by user
// -------------------------------------------
begin
     if Key = #13 then begin
        if bRecord.Enabled and (CDRFH.NumSamplesInFile = 0) then begin
           ReadUserEnteredGain := True ;
           RestartADC ;
           end ;
        end ;
     end;


procedure TRecordFrm.bTDisplayHalfClick(Sender: TObject);
// -------------------------------
// Halve display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*0.5 ;
     Settings.DisplayDuration := edTDisplay.Value ;
     if not WriteBufs.Recording then RestartADC ;
     end;


procedure TRecordFrm.bTDisplayDoubleClick(Sender: TObject);
// -------------------------------
// Double display window duration
// -------------------------------
begin
     edTDisplay.Value := edTDisplay.Value*2.0 ;
     Settings.DisplayDuration := edTDisplay.Value ;
     if not WriteBufs.Recording then RestartADC ;
     end;


procedure TRecordFrm.rbTDisplayUnitsMSecsClick(Sender: TObject);
// --------------------------------------
// Set time readout units to milliseconds
// --------------------------------------
begin
    Settings.TUnits := 'ms' ;
    Settings.TSCale := 1000.0 ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    Main.SESLabIO.ADCStop ;
    end;


procedure TRecordFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// --------------------------------------
// Set time readout units to milliseconds
// --------------------------------------
begin
    Settings.TUnits := 's' ;
    Settings.TSCale := 1.0 ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    Main.SESLabIO.ADCStop ;
    end;

procedure TRecordFrm.rbTDisplayUnitsMinsClick(Sender: TObject);
// --------------------------------------
// Set time readout units to minutes
// --------------------------------------
begin
    Settings.TUnits := 'min' ;
    Settings.TSCale := 1./60.0 ;
    edTDisplay.Scale := Settings.TSCale ;
    edTDisplay.Units := Settings.TUnits ;
    Main.SESLabIO.ADCStop ;
    end;


procedure TRecordFrm.UpdateStimProgramList ;
// --------------------------------
// Update list of stimulus programs
// --------------------------------
var
    NamePart : String ;
begin

     // Load list of stimulus programs
     Stimulator.CreateProtocolList( cbStimProgram ) ;
     if Settings.VProgramFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceText(ExtractFileName( Settings.VProgramFileName),'.xml', '' ) ;
        cbStimProgram.ItemIndex := cbStimProgram.Items.IndexOf(NamePart) ;
        end
     else begin
        cbStimProgram.ItemIndex := 0 ;
        end ;

     if cbStimProgram.ItemIndex <= 0 then begin
        bStartStimulus.Enabled := False ;
        bStopStimulus.Enabled := False ;
        end
     else begin
        bStartStimulus.Enabled := StimulatorAvailable ;
        bStopStimulus.Enabled := False ;
        end ;

     end ;


procedure TRecordFrm.ReStartADC ;
//
// Stop ADC and DAC to force re-start
begin

     Main.SESLabIO.ADCStop ;
     if Main.SESLabIO.DACActive then begin
        Main.SESLabIO.DACStop ;
        StimulatorStartRequired := True ;
        end ;

    end ;

procedure TRecordFrm.cbStimProgramChange(Sender: TObject);
// ------------------------
// Stimulus program changed
// ------------------------
begin
     if cbStimProgram.ItemIndex > 0 then begin
        bStartStimulus.Enabled := True ;
        Settings.VProgramFileName := Main.VProtDirectory + cbStimProgram.Text + '.xml' ;
        end
     else bStartStimulus.Enabled := False ;

     end;

procedure TRecordFrm.StartRecording ;
// -------------------------------
// Start recording (external call)
// -------------------------------
begin
     bRecord.Click ;
     end ;

function TRecordFrm.GetRecordDuration : Single ;
begin
    Result := edRecordDuration.Value ;
    end ;

procedure TRecordFrm.SetRecordDuration( Value : Single ) ;
begin
    Settings.RecordDuration := Value ;
    edRecordDuration.Value := Value ;
    end ;

function TRecordFrm.GetStimulusProtocol : String ;
// -------------------------------------------
// Return currently selected stimulus protocol
// -------------------------------------------
begin
    Result := cbStimProgram.Text ;
    end ;


procedure TRecordFrm.SetStimulusProtocol( Value : String ) ;
// ----------------------
// Set stimulus protocol
// ----------------------
var
    idx : Integer ;
begin

    idx := cbStimProgram.Items.IndexOf(Value) ;
    if (idx > 0) and (idx < cbStimProgram.Items.Count) then begin
       cbStimProgram.ItemIndex := idx ;
       Settings.VProgramFileName := Main.VProtDirectory + cbStimProgram.Text + '.xml' ;
       end
    else begin
       cbStimProgram.ItemIndex := 0 ;
       Settings.VProgramFileName := '' ;
       end ;

    end ;

function TRecordFrm.GetTriggerMode : Integer ;
// --------------------------------------------
// Return trigger mode (0=free run, 1=external)
// --------------------------------------------
begin
    if Settings.TriggerMode = 'F' then Result := 0
                                  else Result := 1 ;
    end ;


procedure TRecordFrm.SetTriggerMode( Value : Integer ) ;
// --------------------------------------------
// Set trigger mode (0=free run, 1=external)
// --------------------------------------------
begin
    if Value = 0 then begin
       Settings.TriggerMode := 'F' ;
       rbFreeRun.Checked := True ;
       end
    else begin
       Settings.TriggerMode := 'E' ;
       rbExtTrigger.Checked := True ;
       end ;
    end ;


function TRecordFrm.GetNumTriggerSweeps : Integer ;
// --------------------------------------------------
// Return no. of externally triggered sweeps required
// --------------------------------------------------
begin
    Result := Settings.NumTriggerSweeps ;
    end ;


procedure TRecordFrm.SetNumTriggerSweeps( Value : Integer ) ;
// --------------------------------------------
// Set no. of externally triggered sweeps required
// --------------------------------------------
begin
    Settings.NumTriggerSweeps :=  Round(Value) ;
    edNumTriggerSweeps.Value := Settings.NumTriggerSweeps ;
    end ;


function TRecordFrm.GetRunning : Boolean ;
// -----------------------------------------
// Return TRUE if digitising analogue signal
// -----------------------------------------
begin
    Result := Timer.Enabled ;
    end ;


function TRecordFrm.GetRecording : Boolean ;
// -----------------------------------------
// Return TRUE if recording analogue signal
// -----------------------------------------
begin
    Result := bStop.Enabled ;
    end ;


procedure TRecordFrm.DisplayAmplifierGainAndMode(
          AmpNumber : Integer ;
          AmpTab : TTabSheet ;
          edGain : TValidatedEdit ;
          rbVCLAMP : TRadioButton ;
          rbICLAMP : TRadioButton ;
          AOHold :  TValidatedEdit ) ;
// ------------------------------------------
// Set amplifier gain and clamp mode controls
// ------------------------------------------
begin

    if Amplifier.AmplifierType[AmpNumber] = amNone then begin
       AmpTab.TabVisible := False ;
       Exit ;
       end
    else AmpTab.TabVisible := True ;

    if Amplifier.ClampMode[AmpNumber] = amCurrentClamp then begin
       rbICLAMP.Checked := True ;
       AOHold.Scale := CurrentClampScale*Amplifier.CommandScaleFactor[AmpNumber] ;
       AOHold.Units := CurrentClampUnits ;
       AOHold.Value := Main.SESLabIO.DACHoldingVoltage[AmpNumber] ;
       end
    else begin
       rbVCLAMP.Checked := True ;
       AOHold.Scale := VoltageClampScale*Amplifier.CommandScaleFactor[AmpNumber] ;
       AOHold.Units := VoltageClampUnits ;
       AOHold.Value := Main.SESLabIO.DACHoldingVoltage[AmpNumber] ;
       end ;

    edGain.Units := 'V/' + Amplifier.PrimaryChannelUnits[AmpNumber,Amplifier.ClampMode[AmpNumber]] ;
    edGain.Value := Amplifier.PrimaryChannelScaleFactor[AmpNumber] ;

    end ;


procedure TRecordFrm.edNumTriggerSweepsKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------------------
// No. of external trigger sweeps changed
// --------------------------------------
begin
     if Key = #13 then Settings.NumTriggerSweeps := Round(edNumTriggerSweeps.Value)  ;
     end;

procedure TRecordFrm.ckEventFrequencyClick(Sender: TObject);
{ -------------------------------------------------------
  Enable/disable real-time fluorescence ratio computation
  ------------------------------------------------------- }
begin
     if ckEventFrequency.Checked then begin
        ckResistance.Checked := False ;
        ckFluorescence.Checked := False ;
        ckCapacity.Checked := False ;
        end ;

     Settings.RTEventAnalysis.InUse := ckEventFrequency.Checked ;
     Settings.NewCalculation := True ;
     end;

procedure TRecordFrm.ckResistanceClick(Sender: TObject);
// -------------------------------------------------------
//  Enable/disable real-time resistance computation
//  ------------------------------------------------------- }
begin
     if ckResistance.Checked then begin
        ckEventFrequency.Checked := False ;
        ckFluorescence.Checked := False ;
        ckCapacity.Checked := False ;
        end ;
     Settings.RTResistance.InUse := ckResistance.Checked ;
     Settings.NewCalculation := True ;
     if not ckResistance.Checked then StopStimulator ;
     end;

procedure TRecordFrm.ckStartStimOnRecordClick(Sender: TObject);
// -----------------------------------
// Start Stim on Record option changed
// -----------------------------------
begin
      Settings.StartStimulusOnRecord := ckStartStimOnRecord.Checked
      end;

procedure TRecordFrm.edNumChannelsKeyPress(Sender: TObject; var Key: Char);
// ---------------------------
// No. of A/D channels changed
// ---------------------------
begin
    if Key = #13 then begin
       Settings.NumChannels := Round(edNumChannels.Value);
       NewFile ;
       end ;

    end;

procedure TRecordFrm.edSamplingIntervalKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------
// Sampling interval changed
// ---------------------------
begin
    if Key = #13 then begin
       Settings.ADCSamplingInterval := edSamplingInterval.Value ;
       NewFile ;
       end ;

    end;

procedure TRecordFrm.UpdateStimulusProtocolList ;
// ----------------------------------
// Update list of stimulus protocols
// ----------------------------------
begin

     { Fill combo box with list of available command voltage programs }
     Stimulator.CreateProtocolList( cbStimProgram ) ;

     if (Settings.VProgramFileName = '') or
        ANSIContainsText(Settings.VProgramFileName,'\ .') then begin
        cbStimProgram.ItemIndex := 0 ;
        end
     else begin
        cbStimProgram.ItemIndex := cbStimProgram.Items.IndexOf(
                                    ExtractFileNameOnly(Settings.VProgramFileName))
        end ;

     // Update list of protocol lists
 {    CreateProtocolListList( cbProtocolList ) ;
     if (Settings.ProtocolListFileName = '') or
        ANSIContainsText(Settings.ProtocolListFileName,'\ .') then begin
        cbProtocolList.ItemIndex := 0 ;
        meProtocolList.Clear ;
        end
     else begin
        cbProtocolList.ItemIndex := cbProtocolList.Items.IndexOf(
                                    ExtractFileNameOnly(Settings.ProtocolListFileName)) ;
        LoadProtocolList ;
        end ;}

     end ;



procedure TRecordFrm.edAO0HoldKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------
  Analog output holding holding voltage
  ------------------------------------- }
begin
     if (Key = #13) and bRecord.Enabled then begin
        Main.SESLabIO.DACHoldingVoltage[0] := edAO0Hold.Value ;
        if bStopStimulus.Enabled then begin
           // Re-start stimulator
           StopStimulator ;
           Stimulator.LoadProtocol( ActiveStimFileName ) ;
           StartStimulator ;
           end
        else UpdateOutputs ;
        end ;
     end;


procedure TRecordFrm.edAO1HoldKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------
  Analog output holding holding voltage
  ------------------------------------- }
begin
     if (Key = #13) and bRecord.Enabled then begin
        Main.SESLabIO.DACHoldingVoltage[1] := edAO1Hold.Value ;
        if bStopStimulus.Enabled then begin
           // Re-start stimulator
           StopStimulator ;
           Stimulator.LoadProtocol( ActiveStimFileName ) ;
           StartStimulator ;
           end
        else UpdateOutputs ;
        end ;
     end;


procedure TRecordFrm.edAO2HoldKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------
  Analog output holding holding voltage
  ------------------------------------- }
begin
     if (Key = #13) and bRecord.Enabled then begin
        Main.SESLabIO.DACHoldingVoltage[2] := edAO2Hold.Value ;
        if bStopStimulus.Enabled then begin
           // Re-start stimulator
           StopStimulator ;
           Stimulator.LoadProtocol( ActiveStimFileName ) ;
           StartStimulator ;
           end
        else UpdateOutputs ;
        end ;
     end;


procedure TRecordFrm.edAO3HoldKeyPress(Sender: TObject; var Key: Char);
{ -------------------------------------
  Analog output holding holding voltage
  ------------------------------------- }
begin
     if (Key = #13) and bRecord.Enabled then begin
        Main.SESLabIO.DACHoldingVoltage[3] := edAO3Hold.Value ;
        if bStopStimulus.Enabled then begin
           // Re-start stimulator
           StopStimulator ;
           Stimulator.LoadProtocol( ActiveStimFileName ) ;
           StartStimulator ;
           end
        else UpdateOutputs ;
        end ;
     end;


procedure TRecordFrm.edAmplifierGain0KeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Amplifier #0 gain changed by user
// ---------------------------------
begin
     if Key = #13 then begin
        Amplifier.PrimaryChannelScaleFactor[0] := edAmplifierGain0.Value ;
        bUpdateGain.Click ; // Request channel scaling update
        end ;
     end;

procedure TRecordFrm.edAmplifierGain1KeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Amplifier #1 gain changed by user
// ---------------------------------
begin
     if Key = #13 then begin
        Amplifier.PrimaryChannelScaleFactor[1] := edAmplifierGain1.Value ;
        bUpdateGain.Click ; // Request channel scaling update
        end ;
     end;

procedure TRecordFrm.edAmplifierGain2KeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Amplifier #2 gain changed by user
// ---------------------------------
begin
     if Key = #13 then begin
        Amplifier.PrimaryChannelScaleFactor[2] := edAmplifierGain2.Value ;
        bUpdateGain.Click ; // Request channel scaling update
        end ;
     end;

procedure TRecordFrm.edAmplifierGain3KeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Amplifier #3 gain changed by user
// ---------------------------------
begin
     if Key = #13 then begin
        Amplifier.PrimaryChannelScaleFactor[3] := edAmplifierGain3.Value ;
        bUpdateGain.Click ; // Request channel scaling update
        end ;
     end;

procedure TRecordFrm.rbVCLAMP2Click(Sender: TObject);
// --------------------------------------
// Amplifier 2: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[2] := amVoltageClamp ;
    bUpdateGain.Click ;
    end;


procedure TRecordFrm.rbVCLAMP0Click(Sender: TObject);
// --------------------------------------
// Amplifier 0: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[0] := amVoltageClamp ;
    bUpdateGain.Click ;
    end;

procedure TRecordFrm.rbICLAMP0Click(Sender: TObject);
// --------------------------------------
// Amplifier 0: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[0] := amCurrentClamp ;
    bUpdateGain.Click ;
    end;

procedure TRecordFrm.rbVClamp1Click(Sender: TObject);
// --------------------------------------
// Amplifier 1: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[1] := amVoltageClamp ;
    bUpdateGain.Click ;
    end;


procedure TRecordFrm.rbICLamp1Click(Sender: TObject);
// --------------------------------------
// Amplifier 1: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[1] := amCurrentClamp ;
    bUpdateGain.Click ;
    end;


procedure TRecordFrm.RBICLAMP2Click(Sender: TObject);
// --------------------------------------
// Amplifier 2: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[2] := amCurrentClamp ;
    bUpdateGain.Click ;
    end;


procedure TRecordFrm.rbVCLAMP3Click(Sender: TObject);
// --------------------------------------
// Amplifier 3: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[3] := amVoltageClamp ;
    bUpdateGain.Click ;
    end;


procedure TRecordFrm.rbICLAMP3Click(Sender: TObject);
// --------------------------------------
// Amplifier 3: Select voltage clamp mode
// --------------------------------------
begin
    Amplifier.ClampMode[3] := amCurrentClamp ;

    bUpdateGain.Click ;
    end;


procedure TRecordFrm.scDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Set channel zero level
// ----------------------
begin
     if (Button = mbRight) and (scDisplay.ActiveHorizontalCursor >=0) then begin
        // If right-mouse button down, display zero baseline level selection dialog box
        ZeroFrm.ChSel := scDisplay.ActiveHorizontalCursor ;
        ZeroFrm.NewZeroAt := Round(scDisplay.ScreenCoordToX( ZeroFrm.ChSel, X )) ;
        ZeroFrm.ZeroLevel := Main.SESLabIO.ADCChannelZero[ZeroFrm.ChSel] ;
        ZeroFrm.ChanName := Main.SESLabIO.ADCChannelName[ZeroFrm.ChSel] ;
        ZeroFrm.Left := RecordFrm.Left + Main.Left + 10 + scDisplay.Left + X;
        ZeroFrm.Top := RecordFrm.Top + Main.Top + 10 + scDisplay.Top + Y ;
        ZeroFrm.ShowModal ;
        Main.SESLabIO.ADCChannelZero[ZeroFrm.ChSel] := ZeroFrm.ZeroLevel ;
        scDisplay.HorizontalCursors[ZeroFrm.ChSel] := Main.SESLabIO.ADCChannelZero[ZeroFrm.ChSel] ;
        end
     end;

Initialization

end.
