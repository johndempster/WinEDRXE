unit ECG;
// -----------------------------------------------
// ECG Analysis Module
// (c) J. Dempster, University of Strathclyde 2004
// -----------------------------------------------
// 20.1.04
// 16.3.04 Both leading and trailing edges of R wave must now be detected
//         before an R wave is accepted

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ScopeDisplay, ComCtrls, RangeEdit,
  ADCDataFile, ExtCtrls, XYPlotDisplay;

const
    ECGFileExtension = '.ECG' ;
    EC2FileExtension = '.EC2' ;
    MaxRWaves = 300000 ;            // ECG R wave list size
    MaxSpectrumRecords = 60000 ;    // Power spectrum record list size

    NumECGFileChannels = 3 ;        // No. of signal channels in .ECG file
    RawCh = 0 ;                     // Raw ECG channel
    HPFCh = 1 ;                     // High pass filtered channel
    SubCh = 2 ;                     // Average subtracted channel

    NumFFTPoints = 512 ;            // No. of points in FFT
    SpecInterval = 0.02 ;
    TMinutesScale = 1.0/60.0 ;      // Seconds-Minutes scale factor
    MilliToMicroVolts = 1E3 ;       // mV -. uV scale factor
    TwoPi = 2.0*Pi ;

    // Plotting variable codes
    vRRInterval = 0 ;
    vHeartRate = 1  ;
    vPeakAFFrequency = 2  ;
    vAFCycleLength = 3 ;
    vAFPower = 4 ;
    vRWaveAmplitude = 5 ;

    MaxAvgs = 100 ;                 // ECG averages buffer limit
    MaxCorrelationPoints = 20 ;     // Max. sample shift for autocorrelations
    MaxSamplesPerAvg = 2200 ;       // Max. no. of samples / average
    AvgRWaveSpacing = 0.02 ;        // R wave amplitude spacing


type

  THPFilter = packed record
              Index : Integer ;
              Filtered : Boolean ;
              end ;

  TRWave = packed record
            Num : Integer ;
            Times : Array[0..MaxRWaves-1] of Integer ;
            end ;

  TECGAvg = packed record
        NumAvgs : Integer ;
        Buf : Array[0..MaxAvgs-1,0..MaxSamplesPerAvg-1] of Single ;
        NumECGs : Array[0..MaxAvgs-1] of Integer ;
        RWaveAmp : Array[0..MaxAvgs-1] of Single ;
        NumSamplesPerAvg : Integer ;
        end ;

  TSpectrum = packed record
        NumRecordsPerSpectrum : Integer ;
        YAxisRange : Single ;
        RecordVarianceLimit : Single ;
        NumSpectra : Integer ;
        DecimationFactor : Integer ;
        NumScansPerRecord : Integer ;
        RecordSpacing : Integer ;
        UseRecord : Array[0..MaxSpectrumRecords-1] of Boolean ;
        end ;


  TECGFrm = class(TForm)
    PageControl: TPageControl;
    ECGPage: TTabSheet;
    ECGControlGrp: TGroupBox;
    GroupBox8: TGroupBox;
    Label6: TLabel;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    cbChannel: TComboBox;
    ECGFile: TADCDataFile;
    GroupBox2: TGroupBox;
    edDeadTime: TValidatedEdit;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    rbPositiveRWave: TRadioButton;
    rbNegativeRWave: TRadioButton;
    Label2: TLabel;
    edThreshold: TValidatedEdit;
    GroupBox4: TGroupBox;
    rbShowRaw: TRadioButton;
    rbShowHPFiltered: TRadioButton;
    rbShowSubtracted: TRadioButton;
    bDetect: TButton;
    ECGGrp: TGroupBox;
    scECGDisplay: TScopeDisplay;
    sbECGDisplay: TScrollBar;
    GroupBox6: TGroupBox;
    AvgGrp: TGroupBox;
    scAvgDisplay: TScopeDisplay;
    bComputeAverageECG: TButton;
    lbAvgI0Cursor: TLabel;
    lbAvgI1Cursor: TLabel;
    shAvgI0I1Line: TShape;
    bSubtractECGAverage: TButton;
    SpectrumPage: TTabSheet;
    scSpecDisplay: TScopeDisplay;
    GroupBox9: TGroupBox;
    edSpecDisplay: TRangeEdit;
    Label4: TLabel;
    sbSpecDisplay: TScrollBar;
    SpectrumGrp: TGroupBox;
    SpectrumControlsGrp: TGroupBox;
    plSpectrum: TXYPlotDisplay;
    Label5: TLabel;
    edSpecNum: TRangeEdit;
    sbSpecNum: TScrollBar;
    ckRejected: TCheckBox;
    meSpecResults: TMemo;
    Label7: TLabel;
    edNumRecordsPerSpectrum: TValidatedEdit;
    meWindowResults: TMemo;
    Timer: TTimer;
    PlotPage: TTabSheet;
    PlotControlsGrp: TGroupBox;
    PlotGrp: TGroupBox;
    plPlot: TXYPlotDisplay;
    GroupBox14: TGroupBox;
    cbPlotVar: TComboBox;
    bPlotGraph: TButton;
    GroupBox15: TGroupBox;
    edAFFrequencyBand: TRangeEdit;
    GroupBox16: TGroupBox;
    bRejectRecords: TButton;
    edVarianceLimit: TValidatedEdit;
    Label8: TLabel;
    GroupBox5: TGroupBox;
    bDigitalFilter: TButton;
    cbHPFilter: TComboBox;
    sbAvgDisplay: TScrollBar;
    edAvgDisplay: TRangeEdit;
    lbAvgDisplay: TLabel;
    edSpectrumYMax: TValidatedEdit;
    Label10: TLabel;
    Label11: TLabel;
    GroupBox17: TGroupBox;
    edRecordsPerSpectrumRequired: TValidatedEdit;
    Label12: TLabel;
    edPeakAmplitudeRequired: TValidatedEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    lbDisplayPoints: TLabel;
    edECGDisplayPoints: TValidatedEdit;
    sbRWave: TScrollBar;
    lbRWave: TLabel;
    edRWave: TRangeEdit;
    lbSpectrum: TLabel;
    ckUseLPFilter: TCheckBox;
    lbSpecDisplay: TLabel;
    lbECGDisplay: TLabel;
    WindowGrp: TGroupBox;
    lbPlot: TLabel;
    bSePlotAxes: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbECGDisplayChange(Sender: TObject);
    procedure bDigitalFilterClick(Sender: TObject);
    procedure rbShowRawClick(Sender: TObject);
    procedure bDetectClick(Sender: TObject);
    procedure sbRWaveChange(Sender: TObject);
    procedure edRWaveKeyPress(Sender: TObject; var Key: Char);
    procedure bComputeAverageECGClick(Sender: TObject);
    procedure scAvgDisplayCursorChange(Sender: TObject);
    procedure bSubtractECGAverageClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure edECGDisplayPointsKeyPress(Sender: TObject; var Key: Char);
    procedure scECGDisplayCursorChange(Sender: TObject);
    procedure sbSpecDisplayChange(Sender: TObject);
    procedure edSpecDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure ckRejectedClick(Sender: TObject);
    procedure sbSpecNumChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure scSpecDisplayCursorChange(Sender: TObject);
    procedure bPlotGraphClick(Sender: TObject);
    procedure bRejectRecordsClick(Sender: TObject);
    procedure sbAvgDisplayChange(Sender: TObject);
    procedure edAvgDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure edNumRecordsPerSpectrumKeyPress(Sender: TObject;
      var Key: Char);
    procedure edSpectrumYMaxKeyPress(Sender: TObject; var Key: Char);
    procedure plSpectrumCursorChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bSePlotAxesClick(Sender: TObject);
    procedure scECGDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scAvgDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scSpecDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure plSpectrumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure plPlotCursorChange(Sender: TObject);
  private
    { Private declarations }
    ECGFileName : String ;     // ECG analysis file name

    ADC : Array[0..32767] of SmallInt ;
    SpecBuf : Array[0..32767] of SmallInt ;
    Spec : TSpectrum ;

    ECGCursor : Integer ;

    HPFilter : THPFilter ;  // High pass filter record
    // ECG average variables
    AvgDisplayBuf : Array[0..32767] of SmallInt ;        // Average currently on display
    ECGAvg : TECGAvg ;
    AvgCursor : Integer ;
    AvgI0Cursor : Integer ;                              // Subtraction region cursor 0
    AvgI1Cursor : Integer ;                              // Subtraction region cursor 1
    SpecCursor : Integer ;
    SpecBaselineCursor : Integer ;

    // R wave detection time list
    RWave : TRWave ;                   // R wave detection list

    DisplayChanged : Boolean ;
    PlotAvailable : Boolean ;

    procedure NewFile ;
    procedure InitialiseECGDisplay ;
    procedure InitialiseAvgDisplay ;
    procedure InitialiseSpecDisplay ;
    procedure DisplayRecord ;
    procedure DisplaySpecRecord ;
    procedure DisplayRWave ;
    procedure UpdateDisplayMagnifications ;
    procedure ButterworthHPFilter( iCutOff : Integer ) ;
    procedure ChebyshevLPFilter ;
    procedure RWaveDetector ;
    procedure ComputeAverageECG ;
    procedure SubtractAverageECG ;
    procedure DisplayAverageECG ;
    procedure LoadEC2File ;
    procedure SaveEC2File ;
    procedure SpectrumRecordRejection ;
    procedure DisplayPowerSpectrum ;
    function ComputePowerSpectrum(
             SpecNum : Integer ;
             var Frequency : Array of Single ;
             var Power : Array of Single ;
             var RecStart : Integer ;
             var RecEnd : Integer ;
             var TRecord : Single
             ) : Integer ;

    procedure PlotRRInterval( iVar : Integer ) ;
    procedure PlotRWaveAmplitude ;
    function RWaveAmplitude(
             var Buf : Array of SmallInt ;
             NumScansInBuf : Integer ;
             ChanNum : Integer ;
             RWaveStart : Integer ;
             AnalysisWindow : Integer ) : Single ;

    procedure PlotAFVariable( iVar : Integer ) ;

  public
    { Public declarations }
    procedure CopyDataToClipboard ;
    procedure CopyImageToClipboard ;
    function IsClipboardDataAvailable : Boolean ;
    procedure PrintDisplay ;
  end;

var
  ECGFrm: TECGFrm;

implementation

{$R *.dfm}

uses global, fileio , Mdiform, maths , Setaxes, Printrec, Printgra;

type
    TADCBuf = Array[0..9999999] of SmallInt ;
    PADCBuf = ^TADCBuf ;


procedure TECGFrm.FormShow(Sender: TObject);
//  ------------------------------------------
// Initialise controls when form is displayed
// ------------------------------------------
var
     ch : Integer ;
begin

     Main.mnECGFrm.Enabled := True ;

     { Set block of CDR file to be scanned }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiValue := CdrFH.RecordDuration ;
     edRange.HiLimit := CdrFH.RecordDuration ;

     { Fill channel selection list }
     cbChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
          cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
     cbChannel.ItemIndex := 0 ;

     // Create menu of ploting parameters
     cbPlotVar.Clear ;
     cbPlotVar.Items.AddObject('R-R Interval',TObject(vRRInterval)) ;
     cbPlotVar.Items.AddObject('Heart Rate',TObject(vHeartRate)) ;
     cbPlotVar.Items.AddObject('R Wave Amplitude',TObject(vRWaveAmplitude)) ;
     cbPlotVar.Items.AddObject('Peak AF Frequency',TObject(vPeakAFFrequency)) ;
     cbPlotVar.Items.AddObject('AF Cycle Length',TObject(vAFCycleLength)) ;
     cbPlotVar.Items.AddObject('AF Power',TObject(vAFPower)) ;
     cbPlotVar.ItemIndex := 0 ;

     // Set control sizes
     Resize ;

     // Initialise for a new data file
     NewFile ;

     rbShowRaw.Checked := True ;
     DisplayChanged := False ;
     if Rwave.Num > 0 then DisplayRWave
                      else DisplayRecord ;
     DisplayAverageECG ;

     // Mark ECG display as selected for copying
     scECGDisplay.Tag := 1 ;
     scAvgDisplay.Tag := 0 ;
     PlotAvailable := False ;

     end ;


procedure TECGFrm.NewFile ;
{ -----------------------------------------
  Initialise display when data file changed
  -----------------------------------------}
const
    NumScansPerBuf = 256 ;
var
    SelCh : Integer ;
    i,jIn,jOut,ch : Integer ;
    Done : Boolean ;
    StartScan : Integer ;
    EndScan : Integer ;
    InScan : Integer ;
    OutScan : Integer ;
    NumScansToCopy : Integer ;
    NumScansToRead : Integer ;
    NumScansRead : Integer ;
    NumScansToWrite : Integer ;
    NumSamples : Integer ;
    NumScansInFile  : Integer ;
    NyquistFreq : Single ;
    InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;

begin

    // Create name of ECG analysis file (EC1 extension)
    ECGFileName := '' ;
    for i := 1 to Length(CdrFH.FileName) do begin
        if CdrFH.FileName[i] = '.' then
           ECGFileName := ECGFileName + format('[Ch%d]',[cbChannel.ItemIndex]) ;
        ECGFileName := ECGFileName + CdrFH.FileName[i] ;
        end ;
    ECGFileName := ChangeFileExt( ECGFileName, ECGFileExtension ) ;

    // Close any existing file
    ECGFile.CloseDataFile ;

    if FileExists(ECGFileName) then begin
       // Open existing ECG analysis file
       ECGFile.OpenDataFile( ECGFileName, ftEDR ) ;
       end
    else begin
       // Create new ECG analysis file
       ECGFile.CreateDataFile( ECGFileName, ftEDR ) ;

       // Copy file parameters from selected analysis channel
       ECGFile.NumChannelsPerScan := NumECGFileChannels ;
       ECGFile.NumScansPerRecord := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
       ECGFile.MaxADCValue := Channel[0].ADCMaxValue ;
       ECGFile.MinADCValue := -Channel[0].ADCMaxValue -1 ;
       ECGFile.ScanInterval := CdrFH.dt ;
       ECGFile.IdentLine := CdrFH.IdentLine ;
       ECGFile.RecordNum := 1 ;

       SelCh :=  cbChannel.ItemIndex ;
       for ch := 0 to NumECGFileChannels-1 do begin
           ECGFile.ChannelOffset[ch] := ch ;
           ECGFile.ChannelADCVoltageRange[ch] := CdrFH.ADCVoltageRange ;
           ECGFile.ChannelName[ch] := Channel[SelCh].ADCName ;
           ECGFile.ChannelUnits[ch] := Channel[SelCh].ADCUnits ;
           ECGFile.ChannelScale[ch] := Channel[SelCh].ADCSCale ;
           ECGFile.ChannelCalibrationFactor[ch] := Channel[SelCh].ADCCalibrationFactor ;
           ECGFile.ChannelGain[ch] := Channel[SelCh].ADCAmplifierGain ;
           end ;

       // Copy data from selected channel to "raw" channel of ECG file

       InScan := 0 ;
       NumScansInFile := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
       NumScansToCopy := NumScansInFile ;
       OutScan := 0 ;
       for i := 0 to NumScansPerBuf*NumECGFileChannels-1 do OutBuf[i] := 0 ;
       Done := False ;
       While not Done do begin

            // Read from buffer
            NumScansToRead := MinInt( [NumScansToCopy,NumScansPerBuf] ) ;
            NumScansRead := ReadCDRBuffer( CDRFH, InScan, InBuf, NumScansToRead ) ;
            if NumScansRead <= 0 then Done := True ;

            // Copy required channel into ECG file channels
            jIn := Channel[SelCh].ChannelOffset ;
            jOut := 0 ;
            for i := 0 to NumScansRead-1 do begin
                for ch := 0 to NumECGFileChannels-1 do OutBuf[jOut+ch] := InBuf[jIn] ;
                jIn := jIn + CdrFH.NumChannels ;
                jOut := jOut + NumECGFileChannels ;
                end ;

            // Write to ECG analysis file
            ECGFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
            OutScan := OutScan + NumScansRead ;

            // Report progress
            Main.StatusBar.SimpleText := format(
            ' ECG: Reading samples into ECG file %d/%d ',
            [InScan,NumScansInFile]) ;

            InScan := InScan + NumScansRead ;
            NumScansToCopy := NumScansToCopy - NumScansRead ;
            if NumScansToCopy <= 0 then Done := True ;

            end ;

       ECGFile.CloseDataFile ;
       ECGFile.OpenDataFile( ECGFileName, ftEDR ) ;

       end ;

    // Load QRS events, spectrum valid records and averages
    LoadEC2File  ;

    // Initial displays
    InitialiseECGDisplay ;
    InitialiseAvgDisplay ;
    InitialiseSpecDisplay ;

    NyquistFreq := 1.0 / (CDRFH.dt*2.0) ;
    cbHPFilter.Clear ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.001*NyquistFreq]),TObject(1)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.002*NyquistFreq]),TObject(2)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.003*NyquistFreq]),TObject(3)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.004*NyquistFreq]),TObject(4)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.005*NyquistFreq]),TObject(5)) ;
    cbHPFilter.ItemIndex := 0 ;

    // Create cursor for power spectrum
    plSpectrum.ClearVerticalCursors ;
    plSpectrum.AddVerticalCursor(clBlue,'',0) ;

    end ;


procedure TECGFrm.InitialiseECGDisplay ;
{ --------------------------------------
  Initialise ECG display window settings
  --------------------------------------}
var
   i,ch : Integer ;
   SelCh : Integer ;
begin

     { Continuous record display channel }
     scECGDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scECGDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scECGDisplay.MaxPoints := Round(edECGDisplayPoints.Value) ;
     scECGDisplay.NumPoints := scECGDisplay.MaxPoints ;
     scECGDisplay.NumChannels := NumECGFileChannels ;
     scECGDisplay.xMin := 0 ;
     scECGDisplay.xMax := scECGDisplay.NumPoints - 1  ;
     scECGDisplay.DisplayGrid := Settings.DisplayGrid ;

     scECGDisplay.SetDataBuf( @ADC ) ;

     { Set display scaling information }
     SelCh := cbChannel.ItemIndex ;
     for ch := 0 to scECGDisplay.NumChannels-1 do begin
         scECGDisplay.ChanUnits[ch] := Channel[SelCh].ADCUnits ;
         scECGDisplay.ChanName[ch] := Channel[SelCh].ADCName ;
         scECGDisplay.yMin[ch] := Channel[SelCh].yMin ;
         scECGDisplay.yMax[ch] := Channel[SelCh].yMax ;
         scECGDisplay.ChanScale[ch] := Channel[SelCh].ADCScale ;
         scECGDisplay.ChanUnits[ch] := Channel[SelCh].ADCUnits ;
         scECGDisplay.ChanZero[ch] := Channel[SelCh].ADCZero ;
         scECGDisplay.ChanOffsets[ch] := ch ;
         scECGDisplay.ChanColor[ch] := clBlue ;
         scECGDisplay.ChanVisible[ch] := True ;
         end ;
     scECGDisplay.TScale := CdrFH.dt*TMinutesScale ;
     scECGDisplay.TUnits := 'mins' ;

     { Create display cursors }
     scECGDisplay.ClearHorizontalCursors ;
     for ch := 0 to scECGDisplay.NumChannels-1 do
         scECGDisplay.AddHorizontalCursor(ch,clgray,True,'z' ) ;

     scECGDisplay.ClearVerticalCursors ;
     ECGCursor := scECGDisplay.AddVerticalCursor( -1, clBlue, '' ) ;

     // Set upper limit of display slider bar range
     sbECGDisplay.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)
                       - scECGDisplay.MaxPoints ;
     sbECGDisplay.LargeChange := scECGDisplay.MaxPoints div 4 ;

     end ;


procedure TECGFrm.InitialiseAvgDisplay ;
{ -----------------------------------------------
  Initialise averqage ECG display window settings
  -----------------------------------------------}
var
   i,ch : Integer ;
begin

     { Continuous record display channel }
     scAvgDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scAvgDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scAvgDisplay.MaxPoints := Round(edECGDisplayPoints.Value) ;
     scAvgDisplay.NumPoints := scAvgDisplay.MaxPoints ;
     scAvgDisplay.NumChannels := 1 ;
     scAvgDisplay.xMin := 0 ;
     scAvgDisplay.xMax := scAvgDisplay.NumPoints - 1  ;
     scAvgDisplay.DisplayGrid := Settings.DisplayGrid ;

     scAvgDisplay.SetDataBuf( @AvgDisplayBuf ) ;

     { Set display scaling information }
     for ch := 0 to scAvgDisplay.NumChannels-1 do begin
         scAvgDisplay.ChanUnits[ch] := scECGDisplay.ChanUnits[RawCh] ;
         scAvgDisplay.ChanName[ch] := scECGDisplay.ChanName[RawCh] ;
         scAvgDisplay.yMin[ch] := scECGDisplay.yMin[RawCh] ;
         scAvgDisplay.yMax[ch] := scECGDisplay.yMax[RawCh] ;
         scAvgDisplay.ChanScale[ch] := scECGDisplay.ChanScale[RawCh] ;
         scAvgDisplay.ChanUnits[ch] := scECGDisplay.ChanUnits[RawCh] ;
         scAvgDisplay.ChanZero[ch] := scECGDisplay.ChanZero[RawCh] ;
         scAvgDisplay.ChanOffsets[ch] := scECGDisplay.ChanOffsets[RawCh] ;
         scAvgDisplay.ChanColor[ch] := scECGDisplay.ChanColor[RawCh] ;
         scAvgDisplay.ChanVisible[ch] := True
         end ;
     scAvgDisplay.TScale := CdrFH.dt ;
     scAvgDisplay.TUnits := 's' ;

     { Create display cursors }
     scAvgDisplay.ClearHorizontalCursors ;
     scAvgDisplay.AddHorizontalCursor( 0, clgray, True, 'z' ) ;

     scAvgDisplay.ClearVerticalCursors ;
     AvgCursor := scAvgDisplay.AddVerticalCursor( -1, clBlue, '' ) ;
     AvgI0Cursor := scAvgDisplay.AddVerticalCursor( -1, clgray, 'a' ) ;
     AvgI1Cursor := scAvgDisplay.AddVerticalCursor( -1, clgray, 'a' ) ;

     // Set upper limit of display slider bar range
{     sbECGDisplay.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)
                       - scAvgDisplay.MaxPoints ;
     sbECGDisplay.LargeChange := scAvgDisplay.MaxPoints div 4 ;}

     end ;


procedure TECGFrm.InitialiseSpecDisplay ;
{ -----------------------------------------------
  Initialise power spectrum source record display
  -----------------------------------------------}
var
   i,ch : Integer ;
begin

     scSpecDisplay.SetDataBuf( @SpecBuf ) ;

     Spec.DecimationFactor := Round(SpecInterval/ECGFile.ScanInterval) ;
     Spec.NumScansPerRecord := NumFFTPoints*Spec.DecimationFactor ;
     Spec.RecordSpacing := (Spec.NumScansPerRecord*3) div 4 ;
     sbSpecDisplay.Max := ECGFile.NumScansPerRecord div Spec.RecordSpacing ;
     edSpecDisplay.HiLimit := sbSpecDisplay.Max ;
     edSpecDisplay.HiValue := edSpecDisplay.HiLimit ;
     edSpecDisplay.LoValue := edSpecDisplay.HiValue ;

     { Continuous record display channel }
     scSpecDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scSpecDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scSpecDisplay.MaxPoints := NumFFTPoints ;
     scSpecDisplay.NumPoints := scSpecDisplay.MaxPoints ;
     scSpecDisplay.NumChannels := 1 ;
     scSpecDisplay.xMin := 0 ;
     scSpecDisplay.xMax := scSpecDisplay.NumPoints - 1  ;
     scSpecDisplay.DisplayGrid := Settings.DisplayGrid ;

     scSpecDisplay.SetDataBuf( @SpecBuf ) ;

     { Set display scaling information }
     for ch := 0 to scSpecDisplay.NumChannels-1 do begin
         scSpecDisplay.ChanUnits[ch] := scECGDisplay.ChanUnits[RawCh] ;
         scSpecDisplay.ChanName[ch] := scECGDisplay.ChanName[RawCh] ;
         scSpecDisplay.yMin[ch] := scECGDisplay.yMin[RawCh] ;
         scSpecDisplay.yMax[ch] := scECGDisplay.yMax[RawCh] ;
         scSpecDisplay.ChanScale[ch] := scECGDisplay.ChanScale[RawCh] ;
         scSpecDisplay.ChanUnits[ch] := scECGDisplay.ChanUnits[RawCh] ;
         scSpecDisplay.ChanZero[ch] := scECGDisplay.ChanZero[RawCh] ;
         scSpecDisplay.ChanOffsets[ch] := scECGDisplay.ChanOffsets[RawCh] ;
         scSpecDisplay.ChanColor[ch] := scECGDisplay.ChanColor[RawCh] ;
         scSpecDisplay.ChanVisible[ch] := True
         end ;
     scSpecDisplay.TScale := ECGFile.ScanInterval*Spec.DecimationFactor*TMinutesScale ;
     scSpecDisplay.TUnits := ' min' ;

     { Create display cursors }
     scSpecDisplay.ClearHorizontalCursors ;
     SpecBaselineCursor := scSpecDisplay.AddHorizontalCursor(0,clgray,True,'z' ) ;

     scSpecDisplay.ClearVerticalCursors ;
     SpecCursor := scSpecDisplay.AddVerticalCursor( -1, clBlue, '' ) ;

     sbSpecNum.Max := sbSpecDisplay.Max div Round(edNumRecordsPerSpectrum.Value) ;
     edSpecNum.HiLimit := sbSpecNum.Max ;
     edSpecNum.HiValue := sbSpecNum.Max ;

     end ;


procedure TECGFrm.DisplayRecord ;
{ ---------------------------------------------
  Display currently selected block of data file
  ---------------------------------------------}
var
    ch : Integer ;
    iStep : Integer ;
    iEvent : Integer ;
    iQRS : Integer ;
    QRSZero : Integer ;
    QRSTick : Integer ;
    Done : Boolean ;
begin

   scECGDisplay.xOffset := sbECGDisplay.Position ;
   scECGDisplay.SetDataBuf( @ADC ) ;

   // Display selected channel
   for ch := 0 to NumECGFileChannels-1 do scECGDisplay.ChanVisible[ch] := False ;
   if rbShowRaw.Checked then scECGDisplay.ChanVisible[RawCh] := True
   else if rbShowHPFiltered.Checked then scECGDisplay.ChanVisible[HPFCh] := True
   else scECGDisplay.ChanVisible[SubCh] := True ;

   // Read A/D sample data to be displayed
   ECGFile.RecordNum := 1 ;
   ECGFile.LoadADCBuffer( sbECGDisplay.Position,
                          Round(scECGDisplay.MaxPoints),
                          ADC ) ;

   // Fix horizontal cursors at zero
   for ch := 0 to NumECGFileChannels-1 do
       scECGDisplay.HorizontalCursors[ch] := 0 ;

   // Indicate QRS detection points

   if RWave.Num > 0 then begin

        edRWave.HiLimit := RWave.Num ;
        edRWave.HiValue := RWave.Num ;
        sbRWave.Max := MaxInt( [RWave.Num,1] ) ;
        lbRWave.Visible := True ;
        edRWave.Visible := True ;
        sbRWave.Visible := True ;

      // Find event # preceding displayed region

      iStep := RWave.Num div 2 ;
      iEvent := iStep ;
      while iStep > 0 do begin
            if sbECGDisplay.Position > RWave.Times[iEvent] then
               iEvent := iEvent + iStep
            else iEvent := iEvent - iStep ;
            iStep := iStep div 2 ;
            end ;
      iEvent := MaxInt([iEvent-1,0]) ;

      iEvent := 0 ;
      QRSZero := Round(scECGDisplay.yMin[cbChannel.ItemIndex]) ;
      QRSTick := QRSZero + Round( (scECGDisplay.yMax[cbChannel.ItemIndex]
                                - scECGDisplay.yMin[cbChannel.ItemIndex]) / 20.0) ;
      scECGDisplay.CreateLine( 0, clRed, psSolid, 1 ) ;
      scECGDisplay.AddPointToLine( 0, QRSZero ) ;
      While not Done do begin
          iQRS := RWave.Times[iEvent] - scECGDisplay.XOffset ;
          if iQRS <= scECGDisplay.MaxPoints then begin
             if iQRS >= 0 then begin
                scECGDisplay.AddPointToLine( iQRS, QRSZero ) ;
                scECGDisplay.AddPointToLine( iQRS, QRSTick ) ;
                scECGDisplay.AddPointToLine( iQRS, QRSZero ) ;
                end ;
             end
          else Done := True ;
          Inc(iEvent) ;
          if iEvent >= RWave.Num then Done := True ;
          end ;
      scECGDisplay.AddPointToLine( scECGDisplay.MaxPoints-1, QRSZero ) ;

      end
   else begin
      lbRWave.Visible := False ;
      edRWave.Visible := False ;
      sbRWave.Visible := False ;
      end ;

   scECGDisplay.Invalidate ;

   end;


procedure TECGFrm.DisplayRWave ;
{ ---------------------------------------------
  Display currently selected ECG waveform
  ---------------------------------------------}
var
     StartScan : Integer ;
     PreScans : Integer ;
begin

     StartScan := RWave.Times[sbRWave.Position-1] ;
     PreScans := Round( edECGDisplayPoints.Value / 4.0 ) ;
     StartScan := MaxInt([StartScan - PreScans,0]) ;
     sbECGDisplay.Position := StartScan ;
     edRWave.LoValue := sbRWave.Position ;
     scECGDisplay.VerticalCursors[0] := PreScans ;
     DisplayRecord ;

     end ;


procedure TECGFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     Main.mnECGFrm.Enabled := True ;

     { Update EDR file header }
     SaveCDRHeader( CDRfH ) ;

     // Close ECG data file
     ECGFile.CloseDataFile ;

     { Disable copy and print menus }
     //Main.ZoomMenus( False ) ;
     //Main.CopyAndPrintMenus( False, False ) ;

     Action := caFree ;

     end;



procedure TECGFrm.sbECGDisplayChange(Sender: TObject);
// -----------------------------------
// ECG display cursor position changed
// -----------------------------------
begin
     DisplayRecord ;
     end;


procedure TECGFrm.ButterworthHPFilter(
          iCutOff : Integer            // Cut-off frequency index
          ) ;
// ------------------------------------------------------------------
// Create forward-backward butterworth high pass filtered output file
// ------------------------------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 5 ;

   b001 : Array[0..4] of extended
   = ( 0.99590372213842,  -3.98361488855370,   5.97542233283055,
       -3.98361488855370, 0.99590372213842 ) ;
   a001 : Array[0..4] of extended
   = (1.00000000000000,  -3.99179062441447,   5.97540555338674,
      -3.97543915264442, 0.99182422376917 ) ;

   b002 : Array[0..4] of extended
   = ( 0.99182421200053,  -3.96729684800213,   5.95094527200320,
       -3.96729684800213, 0.99182421200053 ) ;
   a002 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.98358125865852,   5.95087842926670,
       -3.95101243657283, 0.98371526751048 ) ;

   b003 : Array[0..4] of extended
   = ( 0.98776138927683,  -3.95104555710730,   5.92656833566096,
       -3.95104555710730, 0.98776138927683 ) ;
   a003 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.97537191256092,   5.92641855596542,
       -3.92671919775679, 0.97567256214609 ) ;

   b004 : Array[0..4] of extended
   = ( 0.98371517412976,  -3.93486069651902,   5.90229104477854,
       -3.93486069651902, 0.98371517412976 ) ;
   a004 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.96716259594885,   5.90202586149088,
       -3.90255878482324, 0.96769554381314 ) ;

   b005 : Array[0..4] of extended
   = ( 0.97968548719040,  -3.91874194876162,   5.87811292314243,
       -3.91874194876162, 0.97968548719040 ) ;
   a005 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.95895331864708,   5.87770027353615,
       -3.87853054905174, 0.95978365381150 ) ;

var
    i,iEnd,j,ch,iCoeff : Integer ;
    Done : Boolean ;
    FirstSample : Boolean ;
    StartScan : Integer ;
    EndScan : Integer ;
    InScan : Integer ;
    OutScan : Integer ;
    NumScansRead : Integer ;
    NumScansToWrite : Integer ;
    NumSamples : Integer ;
    InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    FPSize : Integer ;
    x : extended ;
    y : extended ;
    z : Array[0..NumCoeffs-1] of extended ;
    a : Array[0..NumCoeffs-1] of extended ;
    b : Array[0..NumCoeffs-1] of extended ;
    yS : Single ;
     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;

begin

    // Select coefficients for selected cut-off frequency
    Case iCutOff of
       1 : Begin
          // 0.001
          for i := 0 to High(a) do a[i] := a001[i] ;
          for i := 0 to High(b) do b[i] := b001[i] ;
          end ;
       2 : Begin
          // 0.002
          for i := 0 to High(a) do a[i] := a002[i] ;
          for i := 0 to High(b) do b[i] := b002[i] ;
          end ;
       3 : Begin
          // 0.003
          for i := 0 to High(a) do a[i] := a003[i] ;
          for i := 0 to High(b) do b[i] := b003[i] ;
          end ;
       4 : Begin
          // 0.004
          for i := 0 to High(a) do a[i] := a004[i] ;
          for i := 0 to High(b) do b[i] := b004[i] ;
          end ;
       5 : Begin
          // 0.005
          for i := 0 to High(a) do a[i] := a005[i] ;
          for i := 0 to High(b) do b[i] := b005[i] ;
          end ;
       end ;


    FPSize := SizeOf(x) ;

     EndScan := ECGFile.NumScansPerRecord - 1 ;

     // Create temporary files
     GetTempPath( High(TempPath), TempPath )  ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName1 := String(TempName) ;
     TempHandle1 := FileCreate( TempFileName1 ) ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName2 := String(TempName) ;
     TempHandle2 := FileCreate( TempFileName2 ) ;

     // Copy samples to floating point temp file #1
     // -------------------------------------------
     InScan := 0 ;
     FileSeek( TempHandle1, 0,0) ;
     Done := False ;
     While not Done do begin

         // Read A/D data from source file
         NumScansRead := ECGFile.LoadADCBuffer( InScan, NumScansPerBuf, InBuf ) ;
         if NumScansRead <= 0 then Break ;

         j := RawCh ;
         for i := 0 to NumScansRead-1 do begin
             x := InBuf[j] ;
             FileWrite( TempHandle1, x, FPSize ) ;
             j := j + NumECGFileChannels ;
             end ;
         InScan := InScan + NumScansPerBuf ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' ECG: Reading raw ECG signal %d/%d',
         [InScan,ECGFile.NumScansPerRecord]) ;

         if InScan >= ECGFile.NumScansPerRecord then Done := True ;

         end ;
     NumSamples := FileSeek( TempHandle1, 0, 2 ) div FPSize ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     FileSeek( TempHandle1, 0, 0 ) ;
     FileRead( TempHandle1, x, FPSize ) ;
     y := 0.0 ;
     {z[5] := b[6]*x - a[6]*y ;
     z[4] := b[5]*x + z[5] - a[5]*y ;
     z[3] := b[4]*x + z[4] - a[4]*y ;
     z[2] := b[3]*x + z[3] - a[3]*y ;
     z[1] := b[2]*x + z[2] - a[2]*y ;
     z[0] := b[1]*x + z[1] - a[1]*y ;}
     z[NumCoeffs-1] := 0.0 ;
     for j := NumCoeffs-1 downto 1 do
         z[j-1] := b[j]*x + z[j] - a[j]*y ;

     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for i := 0 to NumSamples-1 do begin

          FileRead( TempHandle1, x, FPSize ) ;
          y := b[0]*x + z[0] ;
         { z[0] := b[1]*x + z[1] - a[1]*y ;
          z[1] := b[2]*x + z[2] - a[2]*y ;
          z[2] := b[3]*x + z[3] - a[3]*y ;
          z[3] := b[4]*x + z[4] - a[4]*y ;
          z[4] := b[5]*x + z[5] - a[5]*y ;
          z[5] := b[6]*x - a[6]*y ;  }
          z[NumCoeffs-1] := 0.0 ;
          for j := 1 to NumCoeffs-1 do
              z[j-1] := b[j]*x + z[j] - a[j]*y ;

          FileWrite( TempHandle2, y, FPSize ) ;

          // Report progress
          if (i mod NumScansPerBuf) = 0 then
             Main.StatusBar.SimpleText := format(
             ' ECG-HP Filter: Applying forward filter %d/%d',
             [i,NumSamples]) ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

     FileSeek( TempHandle2, (NumSamples-1)*FPSize, 0 ) ;
     FileRead( TempHandle2, x, FPSize ) ;
     y := 0.0 ;
     {z[5] := b[6]*x - a[6]*y ;
     z[4] := b[5]*x + z[5] - a[5]*y ;
     z[3] := b[4]*x + z[4] - a[4]*y ;
     z[2] := b[3]*x + z[3] - a[3]*y ;
     z[1] := b[2]*x + z[2] - a[2]*y ;
     z[0] := b[1]*x + z[1] - a[1]*y ;}
     z[NumCoeffs-1] := 0.0 ;
     for j := NumCoeffs-1 downto 1 do
         z[j-1] := b[j]*x + z[j] - a[j]*y ;

     for i := NumSamples-1 downto 0 do begin

         FileSeek( TempHandle2, i*FPSize, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y := b[0]*x + z[0] ;
{         z[0] := b[1]*x + z[1] - a[1]*y ;
         z[1] := b[2]*x + z[2] - a[2]*y ;
         z[2] := b[3]*x + z[3] - a[3]*y ;
         z[3] := b[4]*x + z[4] - a[4]*y ;
         z[4] := b[5]*x + z[5] - a[5]*y ;
         z[5] := b[6]*x - a[6]*y ;}
         z[NumCoeffs-1] := 0.0 ;
         for j := 1 to NumCoeffs-1 do
              z[j-1] := b[j]*x + z[j] - a[j]*y ;
              
         FileSeek( TempHandle1, i*FPSize, 0 ) ;
         FileWrite( TempHandle1, y, FPSize ) ;

         // Report progress
         if (i mod NumScansPerBuf) = 0 then
            Main.StatusBar.SimpleText := format(
            ' ECG-HP Filter: Applying reverse filter %d/%d',
            [i,NumSamples]) ;

          end ;

     // Copy results to HP filtered channel of ECG file
     // -----------------------------------------------

     OutScan := 0 ;
     Done := False ;
     FileSeek( TempHandle1, 0, 0 ) ;
     While not Done do begin

          NumScansToWrite := MinInt( [NumSamples-OutScan,NumScansPerBuf] ) ;

          // Read data buffer from output file
          ECGFile.LoadADCBuffer( OutScan, NumScansToWrite, OutBuf ) ;

          j := 0 ;
          for i := 0 to NumScansToWrite-1 do begin
              FileRead( TempHandle1, y, FPSize ) ;
              yS := y ;
              OutBuf[j+HPFCh] := Round(y) ;
              OutBuf[j+SubCh] := Round(y) ;
              j := j + NumECGFileChannels ;
              end ;

          // Write to output file
          ECGFile.SaveADCBuffer( OutScan, NumScansToWrite, OutBuf ) ;
          OutScan := OutScan + NumScansToWrite ;
          if OutScan >= NumSamples then Done := True ;

          // Report progress
          Main.StatusBar.SimpleText := format(
          ' ECG-HP Filter: Updating data file %d/%d',
          [OutScan,NumSamples]) ;

          end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Close & re-open file to update header
     ECGFile.CloseDataFile ;
     ECGFile.OpenDataFile( ECGFileName, ftEDR ) ;

     // Final Report
     Main.StatusBar.SimpleText :=
     ' ECG: High pass filtering completed ' ;

     end ;


procedure TECGFrm.ChebyshevLPFilter ;
// ---------------------------------------------------------------
// Create Chebyshev low pass filtered output file
// ---------------------------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 9 ;

  b02 : Array[0..NumCoeffs-1] of Double
        = ( 0.00205005353827E-11, 0.016400428306187E-11, 0.057401499071627E-11,
            0.114802998143257E-11, 0.143503747679067E-11, 0.114802998143257E-11,
            0.057401499071627E-11, 0.016400428306187E-11, 0.002050053538277E-11 ) ;
  a02 : Array[0..NumCoeffs-1] of Double
        = ( 1.00000000000000,  -7.92014419561650,  27.45193480926308,
            -54.38799856956691,  67.36605321073601, -53.41787529616806,
            26.48129832483051,  -7.50379024193808, 0.93052195846549 ) ;

  b025 : Array[0..NumCoeffs-1] of Double
        = ( 0.01210281647111E-11,   0.09682253176891E-11,   0.33887886119117E-11,
            0.67775772238234E-11,   0.84719715297793E-11,   0.67775772238234E-11,
            0.33887886119117E-11,   0.09682253176891E-11,   0.01210281647111E-11 ) ;
  a025 : Array[0..NumCoeffs-1] of Double
        = ( 1.00000000000000,  -7.89774845576782,  27.30148803996289,
            -53.95504953343868, 66.67421107745334, -52.75491014258318,
            26.10035640743903,  -7.38226807633012, 0.91392068329736 ) ;


    b05 : Array[0..NumCoeffs-1] of Double
          = ( 0.00297161925749E-8, 0.02377295405990E-8, 0.08320533920965E-8,
              0.16641067841930E-8, 0.20801334802412E-8, 0.16641067841930E-8,
              0.08320533920965E-8, 0.02377295405990E-8, 0.00297161925749E-8 ) ;
    a05 : Array[0..NumCoeffs-1] of Double
          = ( 1.00000000000000,  -7.77142501656024,  26.47269072495892,
              -51.62596175753188, 63.04081514131654, -49.35745632050427,
              24.19666924666336,  -6.79059018138137, 0.83525817109705 ) ;

    b1 : Array[0..NumCoeffs-1] of Double
        = ( 0.00703924326028E-6, 0.05631394608227E-6, 0.19709881128793E-6,
            0.39419762257586E-6, 0.49274702821983E-6, 0.39419762257586E-6,
            0.19709881128793E-6, 0.05631394608227E-6,0.00703924326028E-6 ) ;

    a1 : Array[0..NumCoeffs-1] of Double
        = ( 1.00000000000000, -7.44912258934158, 24.46749067762108,
            -46.27560200466141, 55.11160187999928, -42.31640010161038,
            20.45543300484147, -5.69110270561444, 0.69770374759022 ) ;

    b15 : Array[0..NumCoeffs-1] of Double
         = ( 0.00168337981086E-4, 0.01346703848690E-4, 0.04713463470415E-4,
             0.09426926940831E-4, 0.11783658676038E-4, 0.09426926940831E-4,
             0.04713463470415E-4, 0.01346703848690E-4, 0.00168337981086E-4 ) ;

    a15 : Array[0..NumCoeffs-1] of Double
         = ( 1.00000000000000,  -7.03848157462604,  22.07914782462711,
             -40.28279089382605, 46.72359700253385, -35.26467440752428,
             16.90850144425789,  -4.70813439075344, 0.58288064334514 ) ;

    b2 : Array[0..NumCoeffs-1] of Double
        = ( 0.00158235533091E-3, 0.01265884264730E-3, 0.04430594926555E-3,
            0.08861189853109E-3, 0.11076487316387E-3, 0.08861189853109E-3,
            0.04430594926555E-3, 0.01265884264730E-3, 0.00158235533091E-3 ) ;

    a2 : Array[0..NumCoeffs-1] of Double
        = ( 1.00000000000000,  -6.54522688134438,  19.41294521474746,
            -33.98729349381804, 38.34929605644247, -28.52528050508299,
            13.65107068678909,  -3.84213174594256, 0.48704975384833 ) ;

var
    i,iEnd,j,ch,iCoeff : Integer ;
    Done : Boolean ;
    FirstSample : Boolean ;
    StartScan : Integer ;
    EndScan : Integer ;
    InScan : Integer ;
    OutScan : Integer ;
    NumScansRead : Integer ;
    NumScansToWrite : Integer ;
    NumSamples : Integer ;
    InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    FPSize : Integer ;
    x : Double ;
    y : Double ;
    z : Array[0..NumCoeffs-1] of Double ;
    a : Array[0..NumCoeffs-1] of Double ;
    b : Array[0..NumCoeffs-1] of Double ;

     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;

begin


    for i := 0 to High(a) do a[i] := a02[i] ;
    for i := 0 to High(b) do b[i] := b02[i] ;

    FPSize := SizeOf(x) ;

     EndScan := ECGFile.NumScansPerRecord - 1 ;

     // Create temporary files
     GetTempPath( High(TempPath), TempPath )  ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName1 := String(TempName) ;
     TempHandle1 := FileCreate( TempFileName1 ) ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName2 := String(TempName) ;
     TempHandle2 := FileCreate( TempFileName2 ) ;

     // Copy samples to floating point temp file #1
     // -------------------------------------------
     InScan := 0 ;
     FileSeek( TempHandle1, 0,0) ;
     Done := False ;
     While not Done do begin

         // Read A/D data from source file
         NumScansRead := ECGFile.LoadADCBuffer( InScan, NumScansPerBuf, InBuf ) ;
         if NumScansRead <= 0 then Break ;

         j := SubCh ;
         for i := 0 to NumScansRead-1 do begin
             x := InBuf[j] ;
             FileWrite( TempHandle1, x, FPSize ) ;
             j := j + NumECGFileChannels ;
             end ;
         InScan := InScan + NumScansPerBuf ;

         Main.StatusBar.SimpleText := format(
         ' ECG: Applying LP filter to average-subtracted channel %d/%d',
         [InScan,EndScan]) ;

         end ;
     NumSamples := FileSeek( TempHandle1, 0, 2 ) div FPSize ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     FileSeek( TempHandle1, 0, 0 ) ;
     FileRead( TempHandle1, x, FPSize ) ;
     y := x ;
     z[7] := b[8]*x - a[8]*y ;
     z[6] := b[7]*x + z[7] - a[7]*y ;
     z[5] := b[6]*x + z[6] - a[6]*y ;
     z[4] := b[5]*x + z[5] - a[5]*y ;
     z[3] := b[4]*x + z[4] - a[4]*y ;
     z[2] := b[3]*x + z[3] - a[3]*y ;
     z[1] := b[2]*x + z[2] - a[2]*y ;
     z[0] := b[1]*x + z[1] - a[1]*y ;

     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for i := 0 to NumSamples-1 do begin

          FileRead( TempHandle1, x, FPSize ) ;
          y := b[0]*x + z[0] ;
          z[0] := b[1]*x + z[1] - a[1]*y ;
          z[1] := b[2]*x + z[2] - a[2]*y ;
          z[2] := b[3]*x + z[3] - a[3]*y ;
          z[3] := b[4]*x + z[4] - a[4]*y ;
          z[4] := b[5]*x + z[5] - a[5]*y ;
          z[5] := b[6]*x + z[6] - a[6]*y ;
          z[6] := b[7]*x + z[7] - a[7]*y ;
          z[7] := b[8]*x - a[8]*y ;
          FileWrite( TempHandle2, y, FPSize ) ;

          // Report progress
          if (i mod NumScansPerBuf) = 0 then
             Main.StatusBar.SimpleText := format(
             ' ECG: Applying LP filter to average-subtracted channel %d/%d',
             [i,NumSamples]) ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

{     FileSeek( TempHandle2, (NumSamples-1)*FPSize, 0 ) ;
     FileRead( TempHandle2, x, FPSize ) ;
     y := x ;
     z[7] := b[8]*x - a[8]*y ;
     z[6] := b[7]*x + z[7] - a[7]*y ;
     z[5] := b[6]*x + z[6] - a[6]*y ;
     z[4] := b[5]*x + z[5] - a[5]*y ;
     z[3] := b[4]*x + z[4] - a[4]*y ;
     z[2] := b[3]*x + z[3] - a[3]*y ;
     z[1] := b[2]*x + z[2] - a[2]*y ;
     z[0] := b[1]*x + z[1] - a[1]*y ;

     for i := NumSamples-1 downto 0 do begin

         FileSeek( TempHandle2, i*FPSize, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y := b[0]*x + z[0] ;
         z[0] := b[1]*x + z[1] - a[1]*y ;
         z[1] := b[2]*x + z[2] - a[2]*y ;
         z[2] := b[3]*x + z[3] - a[3]*y ;
         z[3] := b[4]*x + z[4] - a[4]*y ;
         z[4] := b[5]*x + z[5] - a[5]*y ;
         z[5] := b[6]*x + z[6] - a[6]*y ;
         z[6] := b[7]*x + z[7] - a[7]*y ;
         z[7] := b[8]*x - a[8]*y ;
         FileSeek( TempHandle1, i*FPSize, 0 ) ;
         FileWrite( TempHandle1, y, FPSize ) ;

         // Report progress
         if (i mod NumScansPerBuf) = 0 then
            Main.StatusBar.SimpleText := format(
            ' ECG: Applying reverse LP filter %d/%d',
            [i,NumSamples]) ;

          end ;}

     // Copy results to subtraction channel of ECG file
     // -----------------------------------------------

     OutScan := 0 ;
     Done := False ;
     FileSeek( TempHandle2, 0, 0 ) ;
     While not Done do begin

          NumScansToWrite := MinInt( [NumSamples-OutScan,NumScansPerBuf] ) ;

          // Read data buffer from output file
          ECGFile.LoadADCBuffer( OutScan, NumScansToWrite, OutBuf ) ;

          j := SubCh ;
          for i := 0 to NumScansToWrite-1 do begin
              FileRead( TempHandle2, y, FPSize ) ;
              OutBuf[j] := Round(y) ;
              j := j + NumECGFileChannels ;
              end ;

          // Write to output file
          ECGFile.SaveADCBuffer( OutScan, NumScansToWrite, OutBuf ) ;
          OutScan := OutScan + NumScansToWrite ;
          if OutScan >= NumSamples then Done := True ;

          // Report progress
          Main.StatusBar.SimpleText := format(
          ' ECG: Applying LP filter to scans %d/%d',
          [OutScan,NumSamples]) ;

          end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Close & re-open file to update header
     ECGFile.CloseDataFile ;
     ECGFile.OpenDataFile( ECGFileName, ftEDR ) ;

     // Final Report
     Main.StatusBar.SimpleText :=
     ' ECG: Low pass filtering completed ' ;

     end ;



procedure TECGFrm.RWaveDetector ;
// -----------------
// Detect ECG R wave
// -----------------
const
   jLow = -2 ; // Rate of change differentiator window limits
   jHigh = 2 ;

   MaxPoints = 4096 ;
   MaxRWaveDuration = 0.05 ;
var
    i,j,k,iSample,StartPos,y : Integer ;
    StartScan : Integer ;
    EndScan : Integer ;
    PreStartScan : Integer ;
    NumScansRead : Integer ;
    NumScansPerBuf : Integer ;
    iScan : Integer ;
    iDif : Integer ;
    ChannelOffset : Integer ;
    DeadScans : Integer ;
    DifThreshold, Polarity : Integer ;
    Done, NewBufferNeeded, UpdateStatusBar : Boolean ;
    FirstCall : Boolean ;
    ADCBuf : Array[jLow*(EDRChannelLimit+1)..(MaxPoints+jHigh)*(EDRChannelLimit+1)-1] of SmallInt ;
    DifBuf : Array[0..(MaxPoints)*(EDRChannelLimit+1)-1] of SmallInt ;

    A : Array[jLow..jHigh] of Single ; // Differentiator coefficients array
    ASum, Diff : Single ;              // Differentiator summation

    QRSTick : Integer ;
    QRSZero : Integer ;
    RWaveDetected : Boolean ;
    RWaveOnsetDetected : Boolean ;
    RWaveDetectedAt : Integer ;
    MaxScansPerRWave : Integer ;

begin

     bDetect.Enabled := False ;
     //bAbort.Enabled := True ;

     // Number of samples to skip after an event has been detected
     DeadScans := Round( edDeadTime.Value / CdrFH.dt ) ;

     { Let user clear event list }
     if RWave.Num > 0 then begin
        if MessageDlg('Clear existing events in list',mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then begin
           // Delete any existing event list files
           DeleteFile( PChar(ChangeFileExt(ECGFile.FileName,EventFileExtension))) ;
           RWave.Num := 0 ;
           end ;
        end ;

     { Range of samples to be scanned for events }
     if rbAllRecords.Checked then begin
        StartScan := 0 ;
        EndScan := ECGFile.NumScansPerRecord-1 ;
        end
     else begin
        StartScan := Round( edRange.LoValue/CdrFH.dt ) ;
        EndScan :=   Round( edRange.HiValue/CdrFH.dt ) - 1 ;
        end ;

    // R wave detection threshold and polarity
    if  rbPositiveRWave.Checked then Polarity := 1
                                else Polarity := -1 ;


    // Loop initialisations
    Done := False ;
    NewBufferNeeded := True ;
    UpdateStatusBar := False ;
    FirstCall := True ;
    iScan := StartScan ;
    NumScansPerBuf := Round( edECGDisplayPoints.Value) ;
    iDif := 0 ;
    RWaveDetected := False ;
    RWaveOnsetDetected := False ;
    MaxScansPerRWave := Round(MaxRWaveDuration/CdrFH.dt) ;
    QRSZero := Round(scECGDisplay.yMin[HPFCh]) ;
    QRSTick := QRSZero + Round( (scECGDisplay.yMax[HPFCh]
                                - scECGDisplay.yMin[HPFCh]) / 20.0) ;
    // Display it

    scECGDisplay.SetDataBuf( @ADCBuf[0] ) ;
    scECGDisplay.Invalidate ;
    ChannelOffset := HPFCh ;

    // Set differentiator coefficients
    for j := jLow to jHigh do A[j] := j ;
    ASum := 10.0 ;

    // Detect events loop
    // ------------------

    Done := False ;
    while not Done do begin

        if NewBufferNeeded then begin
           // Load and display new buffer of A/D samples
           // ------------------------------------------

           // Read A/D sample data from file (with pre-start samples)

           PreStartScan := MaxInt( [iScan + jLow,0] ) ;
           if PreStartScan >= 0 then begin
              NumScansRead := ECGFile.LoadADCBuffer( PreStartScan,
                                                     NumScansPerBuf-jLow+jHigh,
                                                     ADCBuf[jLow] ) ;
              NumScansRead := NumScansRead - jHigh + jLow ;
              end
           else begin
              // Pre-start samples not available
              NumScansRead := ECGFile.LoadADCBuffer( iScan,
                                                     NumScansPerBuf+jHigh,
                                                     ADCBuf[0] ) ;
              for i := -1 to jLow do begin
                  j := (i*NumECGFileChannels) + HPFCh ;
                  ADCBuf[j] := ADCBuf[HPFCh] ;
                  end ;
              NumScansRead := NumScansRead - jHigh ;
              end ;

           // Copy to display buffer
           for i := 0 to NumScansRead*NumECGFileChannels-1 do ADC[i] := ADCBuf[i] ;
           sbECGDisplay.Position := iScan ;
           scECGDisplay.xOffset := sbECGDisplay.Position ;

           // Compute rate of change
           for i := 0 to NumScansRead-1 do begin
               Diff := 0.0 ;
               for j := jLow to jHigh do begin
                   k := (i+j)*NumECGFileChannels + HPFCh ;
                   Diff := Diff + A[j]*ADCBuf[k] ;
                   end ;
               DifBuf[i] := Round(Diff) ;
               end ;

           if FirstCall then begin
              DifThreshold := 0 ;
              for i := 0 to NumScansRead-1 do begin
                  if (Polarity*DifBuf[i]) > DifThreshold then
                     DifThreshold := Polarity*DifBuf[i] ;
                  end ;
              DifThreshold := Round( DifThreshold*edThreshold.Value ) ;
              FirstCall := False ;
              end ;

           { Initialise detected event line }
           scECGDisplay.CreateLine( 0, clRed, psSolid, 1 ) ;
           scECGDisplay.AddPointToLine( 0, QRSZero ) ;

           NewBufferNeeded := False ;
           iDif := 0 ;

           UpdateStatusBar := True ;
           Application.ProcessMessages ;
           end ;

        // Does detection criterion exceed threshold?
        if not RWaveOnsetDetected then begin
           // Possible leading edge of R wave detected
           if (Polarity*DifBuf[iDif]) > DifThreshold then begin
              RWaveOnsetDetected := True ;
              RWaveDetectedAt := iScan ;
              end ;
           end
        else begin
           // Only accept R wave if trailing edge detected
           if (Polarity*DifBuf[iDif]) < (-DifThreshold) then begin
              RWaveDetected := True ;
              end ;
           if (iScan - RWaveDetectedAt) > MaxScansPerRWave then begin
              RWaveOnsetDetected := False ;
              end ;
           end ;

        if RWaveDetected then begin
           // Save location in event in event list
           if RWave.Num <= High(RWave.Times) then begin
              RWave.Times[RWave.Num] := MaxInt([RWaveDetectedAt,0]) ;
              Inc(RWave.Num) ;
              end ;

           { Draw detected event marker }
           scECGDisplay.AddPointToLine( iDif, QRSZero ) ;
           scECGDisplay.AddPointToLine( iDif, QRSTick ) ;
           scECGDisplay.AddPointToLine( iDif, QRSZero ) ;
           iScan := iScan + DeadScans ;
           iDif := iDif + DeadScans ;
           UpdateStatusBar := True ;
           RWaveDetected := False ;
           RWaveOnsetDetected := False ;
           end ;

        // Increment counters
        Inc(iScan) ;
        Inc(iDif) ;
        if (iDif >= NumScansRead) then NewBufferNeeded := True ;

        if (iScan > EndScan) {or (not bAbort.Enabled)} then Done := True ;

        // Update status bar
        if UpdateStatusBar then begin
           Main.StatusBar.SimpleText := format(
                                        ' QRS Detection: %.2f/%.2f s (%d waves detected)',
                                          [iScan*CdrFH.dt,
                                           EndScan*CdrFH.dt,
                                           RWave.Num] ) ;
           UpdateStatusBar := False ;
           end ;

        end ;

    // Save list of events in file
    Main.StatusBar.SimpleText := format(' Detect Events : %d events detected.',
                                          [RWave.Num] ) ;
    WriteToLogFile(Main.StatusBar.SimpleText) ;

    // Remove final detection flag line
    scECGDisplay.CreateLine( 0, clRed, psSolid, 1 ) ;

    edRWave.HiLimit := RWave.Num ;
    edRWave.HiValue := RWave.Num ;
    edRWave.LoValue := 1 ;
    sbRWave.Max := RWave.Num ;
    sbRWave.Position := 1 ;

    // Re-enable detect button
    //bAbort.Enabled := False ;
    bDetect.Enabled := True ;

    // Save QRS event data to file
    SaveEC2File ;
    
    end ;


procedure TECGFrm.LoadEC2File ;
{ ------------------------------
  Load data from .EC2 data file
  ------------------------------ }
var
     FileName : string ;
     FileHandle : Integer ;
     OK : Boolean ;
     i : Integer ;
begin

     // Add event list file extension to existing data file name
     FileName := ChangeFileExt( ECGFile.FileName, EC2FileExtension ) ;

     OK := True ;
     if FileExists( FileName ) then begin

        // Open an existing EC2 results file
        FileHandle := FileOpen(FileName, fmOpenReadWrite ) ;

        // Read number of events from start of file
        FileSeek(FileHandle,0,0) ;

        // Read high pass filter data
        if FileRead(FileHandle,HPFilter,SizeOf(HPFilter))
           <> SizeOf(HPFilter) then OK := False ;

        // Read detected R wave times
        if FileRead(FileHandle,RWave,SizeOf(RWave))
           <> SizeOf(RWave) then OK := False ;

        // Read number of ECG averages available
        if FileRead(FileHandle,ECGAvg,SizeOf(ECGAvg))
           <> SizeOf(ECGAvg) then OK := False ;

        // Read power spectrum data
        if FileRead(FileHandle,Spec,SizeOf(Spec))
           <> SizeOf(Spec) then OK := False ;

        if not OK then
           MessageDlg( 'ECG: Error loading data from ' + FileName,mtInformation,[mbOk],0);

        // Close file
        if FileHandle >= 0 then FileClose( FileHandle ) ;

        end
     else begin
        // Initialise data arrays when new file
        ECGAvg.NumAvgs := 0 ;
        for i := 0 to High(ECGAvg.NumECGs) do ECGAvg.NumECGs[i] := 0 ;
        RWave.Num := 0 ;
        for i := 0 to High(Spec.UseRecord) do Spec.UseRecord[i] := True ;

        end ;

     cbHPFilter.ItemIndex := HPFilter.Index ;

     if RWave.Num > 0 then begin
        edRWave.HiLimit := RWave.Num ;
        edRWave.HiValue := RWave.Num ;
        sbRWave.Max := MaxInt( [RWave.Num,1] ) ;
        lbRWave.Visible := True ;
        edRWave.Visible := True ;
        sbRWave.Visible := True ;
        end
     else begin
        lbRWave.Visible := False ;
        edRWave.Visible := False ;
        sbRWave.Visible := False ;
        end ;

     sbAvgDisplay.Max := Maxint([ ECGAvg.NumAvgs,1] ) ;
     if ECGAvg.NumSamplesPerAvg > 0 then
        edECGDisplayPoints.Value := ECGAvg.NumSamplesPerAvg - 2*MaxCorrelationPoints ;

     if Spec.NumRecordsPerSpectrum <= 0 then Spec.NumRecordsPerSpectrum := 8 ;
     edNumRecordsPerSpectrum.Value := Spec.NumRecordsPerSpectrum ;
     if Spec.YAxisRange <= 0.0 then Spec.YAxisRange := 100.0 ;
     edSpectrumYMax.Value := Spec.YAxisRange ;
     edVarianceLimit.Value := Spec.RecordVarianceLimit ;

     end ;


procedure TECGFrm.SaveEC2File ;
// ---------------------------
//  Save data to EC2 data file
//  --------------------------
var
     FileName : string ;
     FileHandle : Integer ;
     OK : Boolean ;
begin

     // Add event list file extension to existing data file name
     FileName := ChangeFileExt( ECGFile.FileName, EC2FileExtension ) ;

     // Open file
     if FileExists(FileName) then DeleteFile( PChar(FileName)) ;

     FileHandle := FileCreate( FileName ) ;

     if FileHandle >= 0 then begin
        OK := True ;
        FileSeek(FileHandle,0,0) ;

        // Read high pass filter data
        if FileWrite(FileHandle,HPFilter,SizeOf(HPFilter))
           <> SizeOf(HPFilter) then OK := False ;

        // Write detected R wave times
        if FileWrite(FileHandle,RWave,SizeOf(RWave))
           <> SizeOf(RWave) then OK := False ;

        // Write number of ECG averages available
        if FileWrite(FileHandle,ECGAvg,SizeOf(ECGAvg))
           <> SizeOf(ECGAvg) then OK := False ;

        // Write spectrum time window record accept/reject flags
        if FileWrite(FileHandle,Spec,SizeOf(Spec))
           <> SizeOf(Spec) then OK := False ;

        if not OK then
           MessageDlg( 'ECG: Error writing data to ' + FileName,mtInformation,[mbOk],0);

        // Close file
        FileClose( FileHandle ) ;

        end
     else begin
        MessageDlg('ECG : Unable to create file ' + FileName,mtInformation,[mbOk], 0);
        end ;

     end ;


procedure TECGFrm.ComputeAverageECG ;
// -------------------------------------
// Compute average of ECG beat waveforms
// -------------------------------------
const
     MaxCorrelationShift = 20 ;
     MaxPoints = 4096 ;
var
    i,j : Integer ;
    iStart : Integer ;
    NumScansAvg : Integer ;
    AvgStart : Integer ;
    NumScansECG : Integer ;
    ECGStart : Integer ;
    BestECGStart : Integer ;
    FirstEvent : Boolean ;
    iEvent : Integer ;
    iEventStart : Integer ;
    iEventEnd : Integer ;
    iScanStart : Integer ;
    NumScansRead : Integer ;
    iECG : Integer ;
    iAvg : Integer ;
    ADCBuf : Array[0..MaxPoints*(EDRChannelLimit+1)-1] of SmallInt ;
    ECG : Array[0..MaxPoints-1] of Single ;
    R : Single ;
    RMax : Single ;
    RWaveAmp : Single ;

begin

     NumScansAvg := Round(edECGDisplayPoints.Value) ;
     AvgStart := NumScansAvg div 4 ;
     NumScansECG := NumScansAvg + MaxCorrelationShift*2 ;
     ECGStart :=  AvgStart + MaxCorrelationShift ;

     iEventStart := 1 ;
     iEventEnd := RWave.Num ;

     // Initialise averaging buffers
     FirstEvent := True ;
     ECGAvg.NumAvgs := 0 ;
     for iAvg := 0 to MaxAvgs-1 do begin
         for i := 0 to NumScansAvg-1 do ECGAvg.Buf[iAvg,i] := 0.0 ;
         ECGAvg.NumECGs[iAvg] := 0 ;
         ECGAvg.RWaveAmp[iAvg] := 0.0 ;
         end ;

     for iEvent := iEventStart to iEventEnd do begin

         iScanStart :=  RWave.Times[iEvent-1] - ECGStart ;
         if iScanStart >= 0 then begin
             // Load data
             NumScansRead := ECGFile.LoadADCBuffer( iScanStart,
                                                    NumScansECG,
                                                    ADCBuf ) ;

             // Calculate R Wave amplitude
             RWaveAmp := RWaveAmplitude( ADCBuf, NumScansRead, HPFCh, ECGStart, 30 ) ;

             // Find best matching average
             iAvg := 0 ;
             While (Abs(RWaveAmp - ECGAvg.RWaveAmp[iAvg]
                   /MaxInt([1,ECGAvg.NumECGs[iAvg]])) > AvgRWaveSpacing)
                   and (ECGAvg.NumECGs[iAvg] > 0) and (iAvg < (MaxAvgs-1)) do Inc(iAvg) ;
             ECGAvg.NumAvgs := MaxInt([iAvg + 1,ECGAvg.NumAvgs]) ;
             ECGAvg.RWaveAmp[iAvg] := ECGAvg.RWaveAmp[iAvg] + RWaveAmp ;
             ECGAvg.NumECGs[iAvg] := ECGAvg.NumECGs[iAvg] + 1 ;

             // Copy selected channel to ECG buffer
             j := HPFCh ;
             for i := 0 to NumScansECG-1 do begin
                 ECG[i] := ADCBuf[j] ;
                 j := j + NumECGFileChannels ;
                 end ;

             // Align ECG to average using auto-correlation
             if iAvg = 7 then begin
                Main.StatusBar.SimpleText := 'x' ;
                end ;
             if ECGAvg.NumECGs[iAvg] > 1 then begin
                RMax := 0.0 ;
                for iECG := ECGStart-MaxCorrelationShift to ECGStart+MaxCorrelationShift do begin
                    R := 0.0 ;
                    for i := -40 to 40 do begin
                        R := R + ECGAvg.Buf[iAvg,AvgStart+i]*ECG[iECG + i] ;
                        end ;
                    if R >= RMax then begin
                       RMax := R ;
                       BestECGStart := iECG ;
                       end ;
                    end ;
                 end
             else begin
                 BestECGStart := ECGStart ;
                 end ;

             // Add ECG to averaging buffer
             j := ((BestECGStart-AvgStart)*NumECGFileChannels) + HPFCh ;
             for i := 0 to NumScansAvg-1 do begin
                 ECGAvg.Buf[iAvg,i] := ECGAvg.Buf[iAvg,i] + ADCBuf[j] ;
                 j := j + NumECGFileChannels ;
                 end ;

             end ;

         Main.StatusBar.SimpleText := format(
         'Computing average ECG: Events %d/%d (%d averaged)',
                                          [iEvent,
                                           iEventEnd,
                                           ECGAvg.NumAvgs] ) ;

         end ;

     // Calculate averages
     for iAvg := 0 to ECGAvg.NumAvgs-1 do begin
         ECGAvg.RWaveAmp[iAvg] := ECGAvg.RWaveAmp[iAvg] / ECGAvg.NumECGs[iAvg] ;
         for i := 0 to NumScansAvg-1 do
             ECGAvg.Buf[iAvg,i] := ECGAvg.Buf[iAvg,i] / ECGAvg.NumECGs[iAvg] ;
         end ;

     sbAvgDisplay.Max :=  ECGAvg.NumAvgs ;
     edAvgDisplay.HiLimit := ECGAvg.NumAvgs ;
     edAvgDisplay.HiValue := ECGAvg.NumAvgs ;
     edAvgDisplay.LoValue := 1.0 ;

     scAvgDisplay.HorizontalCursors[0] := 0 ;
     scAvgDisplay.Invalidate ;

     // Update EC2 file
     SaveEC2File ;

     end ;


procedure TECGFrm.DisplayAverageECG ;
// -------------------------------------------------
// Display average ECG beat waveform for ECG signal
// -------------------------------------------------
var
    i : Integer ;
    iAvg : Integer ;
    NumScansAvg : Integer ;
begin

     NumScansAvg := Round(edECGDisplayPoints.Value) ;

     if ECGAvg.NumAvgs > 0 then begin

        // Display average

        sbAvgDisplay.Enabled := True ;
        edAvgDisplay.Visible := True ;
        lbAvgDisplay.Visible := True ;
        edAvgDisplay.LoValue := sbAvgDisplay.Position ;
        edAvgDisplay.HiValue := sbAvgDisplay.Max ;

        iAvg := sbAvgDisplay.Position ;
        for i := 0 to NumScansAvg-1 do
            AvgDisplayBuf[i] := Round( ECGAvg.Buf[iAvg,i] )  ;

        NumScansAvg := Round(edECGDisplayPoints.Value) ;
        scAvgDisplay.VerticalCursors[AvgCursor] := Round(edECGDisplaypoints.Value) div 4 ;
        scAvgDisplay.HorizontalCursors[0] := 0 ;
        end
     else begin

        // Clear average display (no averages)

        sbAvgDisplay.Enabled := False ;
        edAvgDisplay.Visible := False ;
        lbAvgDisplay.Visible := False ;
        edAvgDisplay.LoValue := sbAvgDisplay.Position ;
        edAvgDisplay.HiValue := sbAvgDisplay.Max ;

        for i := 0 to NumScansAvg-1 do AvgDisplayBuf[i] := 0  ;

        end ;


     scAvgDisplay.Invalidate ;


     end ;


procedure TECGFrm.SubtractAverageECG ;
// -------------------------------------------------
// Subtract average ECG beat waveform for ECG signal
// -------------------------------------------------
const
     NumScansPerBuf = 256 ;
     MaxCorrelationShift = 20 ;
var
    i,i0,i1,j,ch : Integer ;
    InScan : Integer ;
    iStart : Integer ;
    NumScansAvg : Integer ;
    AvgStart : Integer ;
    NumScansECG : Integer ;
    ECGStart : Integer ;
    BestECGStart : Integer ;
    FirstEvent : Boolean ;
    iEvent : Integer ;
    iEventStart : Integer ;
    iEventEnd : Integer ;
    iScanStart : Integer ;
    iScan : Integer ;
    NumScansRead : Integer ;
    iECG : Integer ;
    ADCBuf : Array[0..MaxPoints*(EDRChannelLimit+1)-1] of SmallInt ;
    R : Single ;
    RMax : Single ;
    RWaveAmp : Single ;
    ResSD : Single ;
    Diff : Single ;
    MinSD : Single ;
    iAvg : Integer ;
    BestAvg : Integer ;
    MinDiff : Single ;
    Done : Boolean ;
begin

     // Copy HP filtered channel to subtracted channel
     // ----------------------------------------------
     InScan := 0 ;
     Done := False ;
     While not Done do begin
         // Read A/D data from source file
         NumScansRead := ECGFile.LoadADCBuffer( InScan, NumScansPerBuf, ADCBuf ) ;
         // Copy samples from HPF to Sub channel
         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             ADCBuf[j+SubCh] := ADCBuf[j+HPFCh] ;
             j := j + NumECGFileChannels ;
             end ;
         // Write buffer back to file
         ECGFile.SaveADCBuffer( InScan, NumScansRead, ADCBuf ) ;
         InScan := InScan + NumScansPerBuf ;
         if InScan >= ECGFile.NumScansPerRecord then Done := True ;

         Main.StatusBar.SimpleText := format(
         'ECG: Initialising subtracted channel: %d/%d',
         [InScan,ECGFile.NumScansPerRecord] ) ;

         end ;


     NumScansAvg := Round(edECGDisplayPoints.Value) ;
     AvgStart := NumScansAvg div 4 ;
     NumScansECG := NumScansAvg + MaxCorrelationShift*2 ;
     ECGStart :=  AvgStart + MaxCorrelationShift ;

     iEventStart := 1 ;
     iEventEnd := RWave.Num ;

     for iEvent := iEventStart to iEventEnd do begin

         iScanStart :=  RWave.Times[iEvent-1] - ECGStart ;
         if iScanStart >= 0 then begin
            // Load data
            NumScansRead := ECGFile.LoadADCBuffer(iScanStart,NumScansECG,ADCBuf) ;

            // Calculate R Wave amplitude
            RWaveAmp := RWaveAmplitude( ADCBuf, NumScansRead, HPFCh, ECGStart, 30 ) ;

            // Select average with closest amplitude
            MinDiff := 1E30 ;
            for i := 0 to ECGAvg.NumAvgs-1 do if ECGAvg.NumECGs[i] > 10 then begin
                Diff := Abs(ECGAvg.RWaveAmp[i] - RWaveAmp) ;
                if Diff < MinDiff then begin
                   MinDiff := Diff ;
                   iAvg := i
                   end ;
                end ;

            // Align ECG to averages using auto-correlation
            // and select average which produces smallest residuals

            MinSD := 1E30 ;
            for iAvg := 0 to ECGAvg.NumAvgs-1 do if (ECGAvg.NumECGs[iAvg] > 10) then begin
                RMax := 0.0 ;
                for iECG := ECGStart-MaxCorrelationShift
                    to ECGStart+MaxCorrelationShift do begin

                    // Calculate correlation coeff & residual SD
                    R := 0.0 ;
                    ResSD := 0.0 ;
                    for i := -40 to 40 do begin
                        j := (iECG + i)*NumECGFileChannels + HPFCh ;
                        R := R + ECGAvg.Buf[iAvg,AvgStart+i]*ADCBuf[j] ;
                        Diff := ECGAvg.Buf[iAvg,AvgStart+i]-ADCBuf[j] ;
                        ResSD := ResSD + Diff*Diff ;
                        end ;

                    // Determine best correlation coeff and residual SD
                    if R >= RMax then begin
                       RMax := R ;
                       if (ResSD <= MinSD)
                          and (Abs(iECG-ECGStart) < (MaxCorrelationShift-3)) then begin
                          MinSD := ResSD ;
                          BestAvg := iAvg ;
                          BestECGStart := iECG ;
                          end ;
                       end ;
                     end ;

                end ;

            // Subtract average ECG
            i0 := scAvgDisplay.VerticalCursors[AvgI0Cursor] - AvgStart ;
            i1 := scAvgDisplay.VerticalCursors[AvgI1Cursor] - AvgStart ;
            if i0 > i1 then begin
               i := i0 ;
               i0 := i1 ;
               i1 := i ;
               end ;
            j := ((BestECGStart+i0)*NumECGFileChannels) ;
            for i := AvgStart+i0 to AvgStart+i1 do begin
                ADCBuf[j+SubCh] := Round(ADCBuf[j+HPFCh] - ECGAvg.Buf[BestAvg,i]);
                j := j + NumECGFileChannels;
                end ;

            // Save data back to file
            ECGFile.SaveADCBuffer(iScanStart,NumScansRead,ADCBuf) ;

            end ;

         Main.StatusBar.SimpleText := format(
         'Subtracting average ECG: Events %d/%d',
         [iEvent,iEventEnd] ) ;

         end ;

     // Close & re-open file to update header
     ECGFile.CloseDataFile ;
     ECGFile.OpenDataFile( ECGFileName, ftEDR ) ;

     end ;



procedure TECGFrm.bDigitalFilterClick(Sender: TObject);
// ----------------------------------
// Apply high pass butterworth filter
// ----------------------------------
begin
     ButterworthHPFilter(Integer(cbHPFilter.Items.Objects[cbHPFilter.ItemIndex])) ;
     end;


procedure TECGFrm.rbShowRawClick(Sender: TObject);
begin
     DisplayRecord ;
     end;


procedure TECGFrm.bDetectClick(Sender: TObject);
// --------------------------------
// Detect position of ECG QRS waves
// --------------------------------
begin
     // Detect QRS waves within ECG recording
     RWaveDetector ;

     // Display first QRS event
     if RWave.Num > 0 then DisplayRWave
                         else DisplayRecord ;

     end;


procedure TECGFrm.sbRWaveChange(Sender: TObject);
// ----------------------------
// QRS Event slider bar changed
// ----------------------------
begin
     DisplayRWave ;
     end;

procedure TECGFrm.edRWaveKeyPress(Sender: TObject; var Key: Char);
// --------------------------
// Displayed R Wave # changed
// --------------------------
begin
     if Key = #13 then begin
        sbRWave.Position := Round(edRWave.LoValue) ;
        DisplayRWave ;
        end ;
     end;


procedure TECGFrm.bComputeAverageECGClick(Sender: TObject);
// -------------------------------
// Compute new set of ECG averages
// -------------------------------
begin
     ComputeAverageECG ;
     DisplayAverageECG ;
     end;


procedure TECGFrm.scAvgDisplayCursorChange(Sender: TObject);
var
     ch : Integer ;
     ChannelOnDisplay : Integer ;
begin
             // Set Fitting/area cursor labels
        lbAvgI0Cursor.Left := scAvgDisplay.Left +
                              scAvgDisplay.XScreenCoord[
                              scAvgDisplay.VerticalCursors[AvgI0Cursor]] ;
        lbAvgI1Cursor.Left := scAvgDisplay.Left +
                              scAvgDisplay.XScreenCoord[
                              scAvgDisplay.VerticalCursors[AvgI1Cursor]] ;

        // Place horizontal line between fitting region cursors
        shAvgI0I1Line.Top := lbAvgI0Cursor.Top + (lbAvgI0Cursor.Height div 2) ;
        shAvgI0I1Line.Left := MinInt([lbAvgI0Cursor.Left,lbAvgI1Cursor.Left])
                              + lbAvgI0Cursor.Width ;

        shAvgI0I1Line.Width := MaxInt([lbAvgI0Cursor.Left,lbAvgI1Cursor.Left])
                               - shAvgI0I1Line.Left - lbAvgI1Cursor.Width ;

        // Update for changes in display magnification
        for ch := 0 to scAvgDisplay.NumChannels-1 do
            if scAvgDisplay.ChanVisible[ch] then ChannelOnDisplay := ch ;
        Channel[cbChannel.ItemIndex].yMin := scAvgDisplay.yMin[ChannelOnDisplay] ;
        Channel[cbChannel.ItemIndex].yMax := scAvgDisplay.yMax[ChannelOnDisplay] ;
        UpdateDisplayMagnifications ;

        end;


procedure TECGFrm.UpdateDisplayMagnifications ;
// -------------------------------------------
// Synchronise display vertical magnifications
// -------------------------------------------
var
     ch : Integer ;
     Update : Boolean ;
begin
     // ECG display
     Update := False ;
     for ch := 0 to scECGDisplay.NumChannels-1 do begin
            if scECGDisplay.YMax[ch] <> Channel[cbChannel.ItemIndex].YMax then begin
               scECGDisplay.YMax[ch] := Channel[cbChannel.ItemIndex].YMax ;
               Update := True ;
               end ;
            if scECGDisplay.YMin[ch] <> Channel[cbChannel.ItemIndex].YMin then begin
               scECGDisplay.YMin[ch] := Channel[cbChannel.ItemIndex].YMin ;
               Update := True ;
               end ;
            end ;
     if Update then scECGDisplay.Invalidate ;

     // Average ECG display
     Update := False ;
     for ch := 0 to scAvgDisplay.NumChannels-1 do begin
            if scAvgDisplay.YMax[ch] <> Channel[cbChannel.ItemIndex].YMax then begin
               scAvgDisplay.YMax[ch] := Channel[cbChannel.ItemIndex].YMax ;
               Update := True ;
               end ;
            if scAvgDisplay.YMin[ch] <> Channel[cbChannel.ItemIndex].YMin then begin
               scAvgDisplay.YMin[ch] := Channel[cbChannel.ItemIndex].YMin ;
               Update := True ;
               end ;
            end ;
     if Update then scAvgDisplay.Invalidate ;

     // Spectrum time window display
     Update := False ;
     for ch := 0 to scSpecDisplay.NumChannels-1 do begin
            if scSpecDisplay.YMax[ch] <> Channel[cbChannel.ItemIndex].YMax then begin
               scSpecDisplay.YMax[ch] := Channel[cbChannel.ItemIndex].YMax ;
               Update := True ;
               end ;
            if scSpecDisplay.YMin[ch] <> Channel[cbChannel.ItemIndex].YMin then begin
               scSpecDisplay.YMin[ch] := Channel[cbChannel.ItemIndex].YMin ;
               Update := True ;
               end ;
            end ;
     if Update then scSpecDisplay.Invalidate ;

     end ;


procedure TECGFrm.bSubtractECGAverageClick(Sender: TObject);
begin
     SubtractAverageECG ;
     if ckUseLPFilter.Checked then ChebyshevLPFilter ;
     end;

procedure TECGFrm.cbChannelChange(Sender: TObject);
// -------------------------------------
// Channel selected for analysis changed
// -------------------------------------
begin
     NewFile ;
     DisplayRecord ;
     DisplayAverageECG ;
     end;


procedure TECGFrm.edECGDisplayPointsKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        NewFile ;
        DisplayRecord ;
        end ;
     end;


procedure TECGFrm.scECGDisplayCursorChange(Sender: TObject);
// -------------------
// ECG display changed
// -------------------
var
     ch : Integer ;
     ChannelOnDisplay : Integer ;
     iCursorPos : Integer ;
begin

     // Determine which ECG channel is on display
     for ch := 0 to scECGDisplay.NumChannels-1 do
         if scECGDisplay.ChanVisible[ch] then ChannelOnDisplay := ch ;

     { Read out cursor }
     iCursorPos := scECGDisplay.VerticalCursors[0] ;
     lbECGDisplay.Top := sbRWave.Top + sbRWave.Height + 1 ;
     lbECGDisplay.Left := MinInt( [scECGDisplay.XScreenCoord[iCursorPos]
                           + scECGDisplay.Left - (lbECGDisplay.Width div 2),
                           ClientWidth - lbECGDisplay.Width ] ) ;

     lbECGDisplay.Caption  := format( 't= %5.4g s, %.4g mV',
                               [iCursorPos*scECGDisplay.TScale*60.0,
                                ADC[iCursorPos*NumECGFileChannels+ChannelOnDisplay]
                                *scECGDisplay.ChanScale[ChannelOnDisplay]] ) ;


        ch := cbChannel.ItemIndex ;
        Channel[ch].yMin := scECGDisplay.yMin[ChannelOnDisplay] ;
        Channel[ch].yMax := scECGDisplay.yMax[ChannelOnDisplay] ;
        UpdateDisplayMagnifications ;

        end;


procedure TECGFrm.DisplaySpecRecord ;
{ -------------------------------------------------
  Display currently selected spectrum source record
  ------------------------------------------------- }
const
    MilliToMicroVolts = 1E3 ;
var
    i,j,jStep : Integer ;
    y : Single ;
    Mean : Single ;
    Variance : Single ;
begin


   // Read A/D sample data to be displayed
   ECGFile.RecordNum := 1 ;
   ECGFile.LoadADCBuffer( (sbSpecDisplay.Position-1)*Spec.RecordSpacing,
                          Spec.NumScansPerRecord,
                          SpecBuf ) ;
   scSpecDisplay.xOffset := Round((sbSpecDisplay.Position-1)*Spec.RecordSpacing/Spec.DecimationFactor) ;

   j := SubCh ;
   jStep := NumECGFileChannels*Spec.DecimationFactor ;
   for i := 0 to scSpecDisplay.MaxPoints-1 do begin
       SpecBuf[i] := SpecBuf[j] ;
       j := j + jStep ;
       end ;

   scSpecDisplay.Invalidate ;

   edSpecDisplay.LoValue := sbSpecDisplay.Position ;

   // Set record rejected tick box
   ckRejected.Checked := not Spec.UseRecord[sbSpecDisplay.Position-1] ;

   // Calculate signal variance within window record
   Mean := 0.0 ;
   for i := 0 to scSpecDisplay.MaxPoints-1 do Mean := Mean + SpecBuf[i] ;
   Mean := Mean / scSpecDisplay.MaxPoints ;

   Variance := 0.0 ;
   for i := 0 to scSpecDisplay.MaxPoints-1 do begin
       y := (SpecBuf[i] - Mean) ;
       Variance := Variance + y*y ;
       end ;
   Variance := Variance / (scSpecDisplay.MaxPoints-1) ;
   Variance := Variance*ECGFile.ChannelScale[SubCh]*ECGFile.ChannelScale[SubCh]
               *MilliToMicroVolts*MilliToMicroVolts ;

   meWindowResults.Clear ;
   meWindowResults.Lines.Add( format('Var= %.4g uV^2',[Variance])) ;

   end;


procedure TECGFrm.SpectrumRecordRejection ;
{ -------------------------------------------------
  Reject spectrum time window records
  ------------------------------------------------- }
var
    i,j,jStep : Integer ;
    iRec : Integer ;
    y : Single ;
    Mean : Single ;
    Variance : Single ;
    NumRejected : Integer ;
    InBuf : Array[0..32767] of SmallInt ;
begin

   Spec.RecordVarianceLimit := edVarianceLimit.Value ;
   NumRejected := 0 ;
   for iRec := 1 to sbSpecDisplay.Max do begin

       // Read A/D sample data to be displayed
       ECGFile.RecordNum := 1 ;
       ECGFile.LoadADCBuffer( (iRec-1)*Spec.RecordSpacing,
                              Spec.NumScansPerRecord,
                              InBuf ) ;

       // Calculate mean DC level of record
       j := SubCh ;
       jStep := NumECGFileChannels*Spec.DecimationFactor ;
       Mean := 0.0 ;
       for i := 0 to scSpecDisplay.MaxPoints-1 do begin
           Mean := Mean + InBuf[j] ;
           j := j + jStep ;
           end ;
       Mean := Mean /  scSpecDisplay.MaxPoints ;

       // Calculate variance of record
       j := SubCh ;
       jStep := NumECGFileChannels*Spec.DecimationFactor ;
       Variance := 0.0 ;
       for i := 0 to scSpecDisplay.MaxPoints-1 do begin
           y := (InBuf[j] - Mean) ;
           Variance := Variance + y*y ;
           j := j + jStep ;
           end ;
       Variance := Variance / (scSpecDisplay.MaxPoints-1) ;
       Variance := Variance*ECGFile.ChannelScale[SubCh]*ECGFile.ChannelScale[SubCh]
                   *MilliToMicroVolts*MilliToMicroVolts ;

       if Variance <= edVarianceLimit.Value then Spec.UseRecord[iRec-1] := True
       else begin
          Spec.UseRecord[iRec-1] := False ;
          Inc(NumRejected) ;
          end ;

       Main.StatusBar.SimpleText := format(
       ' ECG: Rejecting power spectrum records %d/%d (%d rejected)',
       [iRec,sbSpecDisplay.Max,NumRejected]) ;


       end ;

   // Update EC2 data file with changes to Spec.UseRecord
   SaveEC2File ;

   end;


procedure TECGFrm.DisplayPowerSpectrum ;
// -----------------------------
// Display average power spectum
// -----------------------------
var
    Frequency : Array[0..NumFFTPoints div 2] of Single ;
    Power : Array[0..NumFFTPoints div 2] of Single ;
    NumAvgd : Integer ;
    i : Integer ;
    npSpectrum : Integer ;
    RecStart : Integer ;
    RecEnd : Integer ;
    TRecord : Single ;
    TRecordSpacing : Single ;
begin

     edSpecNum.LoValue := sbSpecNum.Position ;

     // Compute averaged power spectrum
     NumAvgd := ComputePowerSpectrum( sbSpecNum.Position,
                                      Frequency,
                                      Power,
                                      RecStart,
                                      RecEnd,
                                      TRecord
                                      ) ;

     npSpectrum := NumFFTPoints div 2 ;

     plSpectrum.XAxisAutoRange := False ;
     plSpectrum.XAxisMin := 0.0 ;
     plSpectrum.XAxisMax := {Frequency[npSpectrum-1]} 15.0 ;
     plSpectrum.XAxisTick := 1.0 ;

     plSpectrum.YAxisAutoRange := False ;
     plSpectrum.YAxisMin := 0.0 ;
     if edSpectrumYMax.Value <= 0.0 then edSpectrumYMax.Value := 100 ; 
     plSpectrum.YAxisMax := edSpectrumYMax.Value ;
     plSpectrum.YAxisTick := edSpectrumYMax.Value / 5.0 ;
     plSpectrum.YAxisLabel := 'uV^2/Hz' ;

     plSpectrum.CreateLine( 0, clBlue, msNone, psSolid ) ;

     if plSpectrum.VerticalCursors[0] <= 0.0 then
        plSpectrum.VerticalCursors[0] := (plSpectrum.XAxisMax + plSpectrum.XAxisMin)*0.5 ;

     meSpecResults.Clear ;
     meSpecResults.Lines.Add( format(' Recs %d-%d (%d)',
     [RecStart,RecEnd,NumAvgd] )) ;

     TRecordSpacing := Spec.RecordSpacing*ECGFile.ScanInterval ;
     meSpecResults.Lines.Add( format(' t= %.3g - %.3g min.',
     [(RecStart-1)*TRecordSpacing*TMinutesScale,(RecEnd)*TRecordSpacing*TMinutesScale] )) ;

     if NumAvgd > 0 then begin
        for i := 0 to npSpectrum-1 do
            plSpectrum.AddPoint( 0, Frequency[i], Power[i] ) ;
        end ;

     sbSpecDisplay.Position := RecStart ;

     end ;


function TECGFrm.ComputePowerSpectrum(
         SpecNum : Integer ;
         var Frequency : Array of Single ;
         var Power : Array of Single ;
         var RecStart : Integer ;
         var RecEnd : Integer ;
         var TRecord : Single
         ) : Integer ;
// ----------------------------------------------------------------
// Compute average power spectrum from series of FFT'd time windows
// ----------------------------------------------------------------
const
     MilliToMicroVolts = 1E3 ;
var
     NumRecordsPerSpectrum : Integer ;
     Rec : Integer ;
     npFFT : Integer ;
     i, j, jStep, n : Integer ;
     NumAveraged : Integer ;
     dFreq : Single ;
     yReal : Single ;
     yImag : Single ;
     yScale : Single ;
     Sum : Single ;
     DCLevel : Single ;
     InBuf : Array[0..32767] of SmallInt ;
     FFT : Array[0..NumFFTPoints] of Single ;
begin

     // Determine window records to be averaged in spectrum
     NumRecordsPerSpectrum := Round(edNumRecordsPerSpectrum.Value) ;
     RecStart := NumRecordsPerSpectrum*(SpecNum-1) + 1 ;
     RecEnd := MinInt( [RecStart + NumRecordsPerSpectrum - 1,
                        Round(sbSpecDisplay.Max)]) ;
     TRecord := NumFFTPoints*Spec.DecimationFactor*ECGFile.ScanInterval ;

     // Calculate spectral frequency points and initialise power array
     NumAveraged := 0 ;
     npFFT := NumFFTPoints div 2 ;
     dFreq := 1.0 / (NumFFTPoints*Spec.DecimationFactor*ECGFile.ScanInterval) ;
     for i := 0 to npFFT-1 do begin
         Power[i] := 0.0 ;
         Frequency[i] := (i+1)*dFreq ;
         end ;

     for Rec := RecStart to RecEnd do if Spec.UseRecord[Rec-1] then begin

         // Read A/D sample data
         ECGFile.RecordNum := 1 ;
         ECGFile.LoadADCBuffer( (Rec-1)*Spec.RecordSpacing,
                                Spec.NumScansPerRecord,
                                InBuf ) ;

         // Copy into FFT work array
         j := SubCh ;
         jStep := NumECGFileChannels*Spec.DecimationFactor ;
         for i := 1 to NumFFTPoints do begin
             FFT[i] := InBuf[j]*scSpecDisplay.ChanScale[0] ;
             j := j + jStep ;
             end ;

         // Calculate and subtract mean DC signal level
         Sum := 0.0 ;
         for i := 1 to NumFFTPoints do begin
             Sum := Sum + FFT[i] ;
             end ;
         DCLevel := Sum/NumFFTPoints ;
         for i := 1 to NumFFTPoints do FFT[i] := FFT[i] - DCLevel ;

         // Apply Hanning window
         for i := 1 to NumFFTPoints do
             FFT[i] := FFT[i]*0.5*(1.0 - cos((TwoPi*i)/(NumFFTPoints+1))) ;

         {Transform to frequency domain }
         RealFFT( FFT, npFFT, 1 ) ;

         { Compute power spectrum }
         j := 3 ;
         n := 0 ;
         yScale := ECGFile.ChannelScale[SubCh]*MilliToMicroVolts ;
         for i := 2 to npFFT do begin
             YReal := FFT[j]*yScale ;
             YImag := FFT[j+1]*yScale ; ;
             Power[n] := Power[n] + ((YReal*YReal) + (YImag*YImag)) ;
             Inc(n) ;
             j := j + 2 ;
             end ;
         Power[n] := Power[n] + FFT[2]*FFT[2] ;

         Inc(NumAveraged) ;
         end ;

     // Compute average
     if NumAveraged > 0 then begin
        for i := 0 to npFFT-1 do Power[i] :=  Power[i] / NumAveraged ;
        end ;

     Result := NumAveraged ;

     end ;


procedure TECGFrm.sbSpecDisplayChange(Sender: TObject);
// ----------------------------------------------
// Spectrum time window display selection changed
// ----------------------------------------------
begin
     DisplaySpecRecord ;
     end;

procedure TECGFrm.edSpecDisplayKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        sbSpecDisplay.Position := Round(edSpecDisplay.LoValue) ;
        DisplaySpecRecord ;
        end ;
     end;


procedure TECGFrm.ckRejectedClick(Sender: TObject);
// --------------------------------------
// Spectrum record rejected state changed
// --------------------------------------
begin
     Spec.UseRecord[sbSpecDisplay.Position-1] := not ckRejected.Checked ;
     SaveEC2File ;
     end;


procedure TECGFrm.sbSpecNumChange(Sender: TObject);
// ------------------------
// Power spectrum # changed
// ------------------------
begin
     DisplayPowerSpectrum ;
     end;


procedure TECGFrm.PageControlChange(Sender: TObject);
begin
     if PageControl.ActivePage = SpectrumPage then begin
        DisplaySpecRecord ;
        DisplayPowerSpectrum ;
        end ;
     end;


procedure TECGFrm.TimerTimer(Sender: TObject);
var
     ch : Integer ;
begin

     end;

procedure TECGFrm.scSpecDisplayCursorChange(Sender: TObject);
// ------------------------------------
// Spectrum time window display changed
// ------------------------------------
var
     ch : Integer ;
     ChannelOnDisplay : Integer ;
     iCursorPos,j : Integer ;
begin

     { Read out cursor }
     iCursorPos := scSpecDisplay.VerticalCursors[0] ;
     lbSpecDisplay.Top := scSpecDisplay.Top + scSpecDisplay.Height + 1 ;
     lbSpecDisplay.Left := MinInt( [scSpecDisplay.XScreenCoord[iCursorPos]
                           + scSpecDisplay.Left - (lbSpecDisplay.Width div 2),
                           ClientWidth - lbSpecDisplay.Width ] ) ;

     lbSpecDisplay.Caption  := format( 't= %5.4g s, %.4g mV',
                               [iCursorPos*scSpecDisplay.TScale*60.0,
                                SpecBuf[iCursorPos]*scSpecDisplay.ChanScale[0]] ) ;

     for ch := 0 to scSpecDisplay.NumChannels-1 do
         if scSpecDisplay.ChanVisible[ch] then ChannelOnDisplay := ch ;
     Channel[cbChannel.ItemIndex].yMin := scSpecDisplay.yMin[ChannelOnDisplay] ;
     Channel[cbChannel.ItemIndex].yMax := scSpecDisplay.yMax[ChannelOnDisplay] ;
     UpdateDisplayMagnifications ;

     end;


procedure TECGFrm.bPlotGraphClick(Sender: TObject);
// -------------------------------
// Plot graph of selected variable
// -------------------------------
var
    iVar : Integer ;
begin

    iVar := Integer(cbPlotVar.Items.Objects[cbPlotVar.ItemIndex]) ;

    case iVar of
         vHeartRate : PlotRRInterval(iVar) ;
         vRRInterval : PlotRRInterval(iVar) ;
         vAFPower : PlotAFVariable(iVar) ;
         vPeakAFFrequency : PlotAFVariable(iVar) ;
         vAFCycleLength : PlotAFVariable(iVar) ;
         vRWaveAmplitude : PlotRWaveAmplitude ;
         end ;

    PlotAvailable := True ;
    end;


procedure TECGFrm.PlotRRInterval(
          iVar : Integer // Plot variable (vRRInterval/vHeartRate)
          ) ;
// ------------------------------
// Plot RR interval or heart rate
// ------------------------------
var
     i : Integer ;
     x,y : Single ;
begin

     plPlot.MaxPointsPerLine := RWave.Num ;
     plPlot.CreateLine( 0, clBlue, msNone, psSolid ) ;

     if iVar = vHeartRate then begin
        plPlot.YAxisLabel := 'Heart Rate (BPM)' ;
        end
     else begin
        plPlot.YAxisLabel := 'R-R Interval (s)' ;
        end ;
     plPlot.XAxisLabel := 'Mins.' ;

     for i := 1 to RWave.Num-1 do begin
         y := (RWave.Times[i] - RWave.Times[i-1])*ECGFile.ScanInterval ;
         x := (RWave.Times[i]*ECGFile.ScanInterval)/60.0 ;
         if (iVar = vHeartRate) and (y > 0.0) then y := 60.0 / y ;
         if y > 0.0 then plPlot.AddPoint( 0, x, y )
         end ;

    plPlot.ClearVerticalCursors ;
    plPlot.AddVerticalCursor(clBlue,'',0) ;
    plPlot.VerticalCursors[0] := x*0.5 ;

    end ;


procedure TECGFrm.PlotRWaveAmplitude ;
// ---------------------
// Plot R wave amplitude
// ---------------------
const
    NumScansToRead = 50 ;
var
     i,j : Integer ;
     iEvent : Integer ;
     iScanStart : Integer ;
     NumScansRead : Integer ;
     x,y : Single ;
     InBuf : Array[0..NumScansToRead*NumECGFileChannels-1] of SmallInt ;
begin

     plPlot.MaxPointsPerLine := RWave.Num ;
     plPlot.CreateLine( 0, clBlue, msNone, psSolid ) ;
     plPlot.YAxisLabel := 'R Wave Amplitude (mV)' ;
     plPlot.XAxisLabel := 'Mins.' ;

     for iEvent := 0 to RWave.Num-1 do begin

         iScanStart :=  RWave.Times[iEvent] - (NumScansToRead div 2) ;
         if iScanStart >= 0 then begin
             // Load data
             NumScansRead := ECGFile.LoadADCBuffer( iScanStart,
                                                    NumScansToRead,
                                                    InBuf ) ;
             // Add to plot
             y := RWaveAmplitude( InBuf, NumScansRead, HPFCh, NumScansToRead div 2, NumScansToRead ) ;
             x := (RWave.Times[iEvent]*ECGFile.ScanInterval)/60.0 ;
             plPlot.AddPoint( 0, x, y )

             end ;

         if (iEvent mod 100) = 0 then
            Main.StatusBar.SimpleText := format(
            ' ECG: Plotting R Wave amplitudes %d/%d ',
            [iEvent+1,RWave.Num]) ;

         end ;

     plPlot.ClearVerticalCursors ;
     plPlot.AddVerticalCursor(clBlue,'',0) ;
     plPlot.VerticalCursors[0] := x*0.5 ;

     end ;


function TECGFrm.RWaveAmplitude(
         var Buf : Array of SmallInt ;
         NumScansInBuf : Integer ;
         ChanNum : Integer ;
         RWaveStart : Integer ;
         AnalysisWindow : Integer ) : Single ;
// --------------------------
// Calculate R wave amplitude
// --------------------------
var
     i,j : Integer ;
     ADCValue : Integer ;
     MinValue : Integer ;
     MaxValue : Integer ;
     HalfWindow : Integer ;
     iChanOffset : Integer ;
begin

     iChanOffset := ECGFile.ChannelOffset[ChanNum] ;
     MinValue := 32767 ;
     MaxValue := -32767 ;
     HalfWindow := AnalysisWindow div 2 ;
     for i := MaxInt([0,RWaveStart-HalfWindow]) to
              MinInt([NumScansInBuf,RWaveStart+HalfWindow]) do begin
         j := i*NumECGFileChannels + iChanOffset ;
         ADCValue := Buf[j] ;
         if ADCValue <= MinValue then MinValue := ADCValue ;
         if ADCValue >= MaxValue then MaxValue := ADCValue ;
         end ;
     Result := (MaxValue - MinValue)*ECGFile.ChannelScale[ChanNum] ;
     end ;


procedure TECGFrm.PlotAFVariable(
          iVar : Integer // Plot variable
          ) ;
// --------------------------------------------------------
// Plot power/frequency/cycle length of atrial fibrillation
// --------------------------------------------------------
var
     iSpec : Integer ;
     i : Integer ;
     x,y : Single ;
     AFPower : Single ;
     MaxPower : Single ;
     MaxPowerFrequency : Single ;
     Frequency : Array[0..NumFFTPoints div 2] of Single ;
     Power : Array[0..NumFFTPoints div 2] of Single ;
     npSpectrum : Integer ;
     RecStart : Integer ;
     RecEnd : Integer ;
     TRecord : Single ;
     NumAvgd : Integer ;
     AFBandMin : Single ;
     AFBandMax : Single ;
     df : Single ;
begin

     plPlot.MaxPointsPerLine := sbSpecNum.Max ;
     plPlot.CreateLine( 0, clBlue, msOpenSquare, psClear ) ;
     plPlot.MarkerSize := 5 ;

     // Set axes labels
     case iVar of
        vPeakAFFrequency : plPlot.YAxisLabel := 'Peak AF Frequency (Hz)' ;
        vAFCycleLength : plPlot.YAxisLabel := 'AF Cycle Length (s)' ;
        vAFPower : plPlot.YAxisLabel := 'AF Power (mV^2)' ;
        end ;
     plPlot.XAxisLabel := 'Mins.' ;

     AFBandMin := edAFFrequencyBand.LoValue ;
     AFBandMax := edAFFrequencyBand.HiValue ;

     for iSpec := 1 to sbSpecNum.Max do begin

         // Compute averaged power spectrum
         NumAvgd := ComputePowerSpectrum( iSpec,
                                          Frequency,
                                          Power,
                                          RecStart,
                                          RecEnd,
                                          TRecord
                                          ) ;

         if NumAvgd >= Round(edRecordsPerSpectrumRequired.Value) then begin

            AFPower := 0.0 ;
            MaxPower := 0.0 ;
            npSpectrum := NumFFTPoints div 2 ;
            df := Frequency[1] - Frequency[0] ;
            for i := 0 to npSpectrum-1 do begin
                if (Frequency[i] >= AFBandMin) and (Frequency[i] <= AFBandMax) then begin
                   AFPower := AFPower + Power[i]*df ;
                   if Power[i] > MaxPower then begin
                      MaxPower := Power[i] ;
                      MaxPowerFrequency := Frequency[i] ;
                      end ;
                   end ;
                end ;

            Case iVar of
                 vPeakAFFrequency : y := MaxPowerFrequency ;
                 vAFCycleLength : y := 1.0 / MaxPowerFrequency ;
                 vAFPower : y := AFPower ;
                 end ;

            x := ({RecStart-1} + RecEnd)*Spec.RecordSpacing*ECGFile.ScanInterval/60.0 ;

            if (MaxPower >= edPeakAmplitudeRequired.Value) then plPlot.AddPoint( 0, x, y ) ;

            end ;
         end ;

     plPlot.ClearVerticalCursors ;
     plPlot.AddVerticalCursor(clBlue,'',0) ;
     plPlot.VerticalCursors[0] := x*0.5 ;

     end ;


procedure TECGFrm.bRejectRecordsClick(Sender: TObject);
// -------------------------------------------
// Automatically reject power spectrum records
// -------------------------------------------
begin
     SpectrumRecordRejection ;
     end;


procedure TECGFrm.sbAvgDisplayChange(Sender: TObject);
begin
     DisplayAverageECG ;
     end;

procedure TECGFrm.edAvgDisplayKeyPress(Sender: TObject; var Key: Char);
// ---------------------------
// Displayed Average # changed
// ---------------------------
begin
     if Key = #13 then begin
        sbAvgDisplay.Position := Round(edAvgDisplay.LoValue) ;
        DisplayAverageECG ;
        end ;
     end;

procedure TECGFrm.edNumRecordsPerSpectrumKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------------
// Number of records averaged per spectrum changed
// -----------------------------------------------
begin
     if Key = #13 then begin
        InitialiseSpecDisplay ;
        DisplayPowerSpectrum ;
        SaveEC2File ;
        end ;
     end;

procedure TECGFrm.edSpectrumYMaxKeyPress(Sender: TObject; var Key: Char);
// ----------------------------------
// Spectrum display Y maximum changed
// ----------------------------------
begin
     if Key = #13 then begin
        InitialiseSpecDisplay ;
        DisplayPowerSpectrum ;
        SaveEC2File ;
        end ;
     end;

procedure TECGFrm.plSpectrumCursorChange(Sender: TObject);
// ---------------------------
// Power spectrum plot changed
// ---------------------------
var
     x : Single ;
     y : Single ;
begin

    plSpectrum.GetPoint(0,plSpectrum.FindNearestIndex(0,0),x,y);
    lbSpectrum.Caption := format('%.3g Hz, %.3g uV^2/Hz',[x,y]) ;
    lbSpectrum.Left := plSpectrum.Left
                       + plSpectrum.XToCanvasCoord(x)
                       - (lbSpectrum.Width div 2) ;
    lbSpectrum.Top := plSpectrum.Top + plSpectrum.Height + 1 ;

    end;

    
procedure TECGFrm.FormResize(Sender: TObject);
// --------------------------------------
// Resize controls when form size changed
// --------------------------------------
begin

     // Set size of tabbed page
     PageControl.Height := ClientHeight - PageControl.Top - 5 ;
     PageControl.Width := ClientWidth - PageControl.Left - 5 ;

//     ECG Analysis Page
//     -----------------

     ECGControlGrp.Height := ECGPage.ClientHeight - ECGControlGrp.Top - 5 ;

     // ECG and average ECG group boxes
     ECGGrp.Height := ECGPage.ClientHeight div 2 ;
     ECGGrp.Width :=  ECGPage.ClientWidth - ECGGrp.Left - 5 ;
     AvgGrp.Top := ECGGrp.Top + ECGGrp.ClientHeight + 2 ;
     AvgGrp.Height := ECGPage.ClientHeight - AvgGrp.Top ;
     AvgGrp.Width := ECGGrp.Width ;

     // ECG Display
     lbECGDisplay.Top := ECGGrp.ClientHeight - lbECGDisplay.Height - 2 ;
     sbRWave.Top := lbECGDisplay.Top - sbRWave.Height - 1 ;
     edRWave.Top := sbRWave.Top ;
     lbRWave.Top := sbRWave.Top ;

     sbECGDisplay.Top := sbRWave.Top - sbRWave.Height - 1 ;
     scECGDisplay.Height := sbECGDisplay.Top - scECGDisplay.Top - 1 ;

     sbECGDisplay.Width := ECGGrp.ClientWidth - sbECGDisplay.Left - 5 ;
     scECGDisplay.Width := sbECGDisplay.Width ;
     sbRWave.Width := sbECGDisplay.Left + sbECGDisplay.Width - sbRWave.Left ;

     // Average ECG Display
     edECGDisplayPoints.Top := AvgGrp.ClientHeight - edECGDisplayPoints.Height - 5 ;
     lbDisplayPoints.Top := edECGDisplayPoints.Top ;

     sbAvgDisplay.Top := edECGDisplayPoints.Top - edECGDisplayPoints.Height - 1 ;
     scAvgDisplay.Height := sbAvgDisplay.Top - scAvgDisplay.Top - 1 ;
     edAvgDisplay.Top := sbAvgDisplay.Top ;
     lbAvgDisplay.Top := sbAvgDisplay.Top ;

     scAvgDisplay.Width := AvgGrp.ClientWidth - scAvgDisplay.Left - 5 ;
     sbAvgDisplay.Width := scAvgDisplay.Width - edAvgDisplay.Left
                           - edAvgDisplay.Width - 2 ;
     sbAvgDisplay.Left := scAvgDisplay.Left + scAvgDisplay.Width
                          - sbAvgDisplay.Width ;

     edECGDisplayPoints.Left := scAvgDisplay.Left + scAvgDisplay.Width
                                - edECGDisplayPoints.Width ;
     lbDisplayPoints.Left := edECGDisplayPoints.Left - lbDisplayPoints.Width - 1;

     lbAvgI0Cursor.Top := sbAvgDisplay.Top + sbAvgDisplay.Height + 1 ;
     lbAvgI1Cursor.Top := lbAvgI0Cursor.Top ;
     shAvgI0I1Line.Top := lbAvgI0Cursor.Top +  (lbAvgI0Cursor.Height div 2) ;

//     Power Spectrum Page
//     -------------------

     SpectrumGrp.Top := WindowGrp.Top + WindowGrp.Height + 1 ;
     SpectrumGrp.Height := SpectrumPage.ClientHeight - SpectrumGrp.Top ;
     WindowGrp.Width := SpectrumPage.ClientWidth - WindowGrp.Left - 5 ;
     SpectrumGrp.Width := WindowGrp.Width ;

     lbSpecDisplay.Top := WindowGrp.ClientHeight - lbSpecDisplay.Height - 5 ;
     scSpecDisplay.Height := lbSpecDisplay.Top - scSpecDisplay.Top - 1 ;
     scSpecDisplay.Width := WindowGrp.ClientWidth - scSpecDisplay.Left - 5 ;

     lbSpectrum.Top := SpectrumGrp.ClientHeight - lbSpectrum.Height - 5 ;
     plSpectrum.Height := lbSpectrum.Top - plSpectrum.Top - 1 ;
     plSpectrum.Width := SpectrumGrp.ClientWidth - plSpectrum.Left - 5 ;

     SpectrumControlsGrp.Height := SpectrumGrp.ClientHeight
                                   - SpectrumControlsGrp.Top - 5 ;
     meSpecResults.Height := SpectrumControlsGrp.ClientHeight
                             - meSpecResults.Top - 5 ;

//     Plot Page
//     ---------

     PlotGrp.Height := PlotPage.ClientHeight - PlotGrp.Top - 5 ;
     PlotControlsGrp.Height := PlotGrp.Height ;
     PlotGrp.Width := PlotPage.ClientWidth - PlotGrp.Left - 5 ;

     lbPlot.Top := PlotGrp.ClientHeight - lbPlot.Height - 5 ;
     plPlot.Height := lbPlot.Top - plPlot.Top - 1 ;
     plPlot.Width := PlotGrp.ClientWidth - plPlot.Left - 5 ;

     end;

procedure TECGFrm.bSePlotAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;



procedure TECGFrm.CopyDataToClipboard ;
// ----------------------------------
// Copy data on display to clipboard
// ----------------------------------
begin

     // Copy data from appropriate display component depending upon
     // which page is on display

     if PageControl.ActivePage = ECGPage then begin
        // ECG page displays
        if scECGDisplay.Tag = 1 then scECGDisplay.CopyDataToClipboard
                                else scAvgDisplay.CopyDataToClipboard ;
        end
     else if PageControl.ActivePage = SpectrumPage then begin
         // Spectrum page displays
         if scSpecDisplay.Tag = 1 then scSpecDisplay.CopyDataToClipboard
                                  else plSpectrum.CopyDataToClipboard ;
         end
     else begin
         // Plot page
         plPlot.CopyDataToClipboard ;
         end ;

     end ;


procedure TECGFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy display image to clipboard as metafile
  -------------------------------------------}
begin

     if PageControl.ActivePage = ECGPage then begin
        // ECG page displays
        if scECGDisplay.Tag = 1 then PrintRecFrm.Display := scECGDisplay
                                else PrintRecFrm.Display := scAvgDisplay ;
        PrintRecFrm.Destination := deClipboard ;
        PrintRecFrm.ShowModal ;
        if scECGDisplay.Tag = 1 then begin
           if PrintRecFrm.ModalResult = mrOK then scECGDisplay.CopyImageToClipboard ;
           end
        else begin
           if PrintRecFrm.ModalResult = mrOK then scAvgDisplay.CopyImageToClipboard ;
           end ;

        end
     else if PageControl.ActivePage = SpectrumPage then begin
        // Spectrum page
        if scSpecDisplay.Tag = 1 then begin
           // Time window
           PrintRecFrm.Destination := deClipboard ;
           PrintRecFrm.Display := scSpecDisplay ;
           PrintRecFrm.ShowModal ;
           if PrintRecFrm.ModalResult = mrOK then scSpecDisplay.CopyImageToClipboard ;
           end
        else begin
           // Spectrum
           PrintGraphFrm.Plot := plSpectrum ;
           PrintGraphFrm.ToPrinter := False ;
           PrintGraphFrm.ShowModal ;
           if PrintGraphFrm.ModalResult = mrOK then plSpectrum.CopyImageToClipboard ;
           end ;
        end
     else begin
        // Plot display
        PrintGraphFrm.Plot := plPlot ;
        PrintGraphFrm.ToPrinter := False ;
        PrintGraphFrm.ShowModal ;
        if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
        end ;

     end ;


procedure TECGFrm.PrintDisplay ;
{ --------------------
  Print selected image
  --------------------}
begin

     if PageControl.ActivePage = ECGPage then begin
        // ECG page displays
        if scECGDisplay.Tag = 1 then PrintRecFrm.Display := scECGDisplay
                                else PrintRecFrm.Display := scAvgDisplay ;
        PrintRecFrm.Destination := dePrinter ;
        PrintRecFrm.ShowModal ;
        if scECGDisplay.Tag = 1 then begin
           if PrintRecFrm.ModalResult = mrOK then scECGDisplay.Print ;
           end
        else begin
           if PrintRecFrm.ModalResult = mrOK then scAvgDisplay.Print ;
           end ;

        end
     else if PageControl.ActivePage = SpectrumPage then begin
        // Spectrum page
        if scSpecDisplay.Tag = 1 then begin
           // Time window
           PrintRecFrm.Destination := dePrinter ;
           PrintRecFrm.Display := scSpecDisplay ;
           PrintRecFrm.ShowModal ;
           if PrintRecFrm.ModalResult = mrOK then scSpecDisplay.Print ;
           end
        else begin
           // Spectrum
           PrintGraphFrm.Plot := plSpectrum ;
           PrintGraphFrm.ToPrinter := True ;
           PrintGraphFrm.ShowModal ;
           if PrintGraphFrm.ModalResult = mrOK then plPlot.Print ;
           end ;
        end
     else begin
        // Plot display
        PrintGraphFrm.Plot := plPlot ;
        PrintGraphFrm.ToPrinter := True ;
        PrintGraphFrm.ShowModal ;
        if PrintGraphFrm.ModalResult = mrOK then plPlot.Print ;
        end ;

     end ;


function TECGFrm.IsClipboardDataAvailable : Boolean ;
// ----------------------------------------------------------
// Is data (data or image) available for copying to clipboard?
// ----------------------------------------------------------
begin
     Result := False ;
     if PageControl.ActivePage = ECGPage then begin
        Result := True ;
        end
     else if PageControl.ActivePage = SpectrumPage then begin
        if sbSpecDisplay.Max > 0 then Result := True ;
        end
     else if PageControl.ActivePage = PlotPage then begin
        Result := PlotAvailable ;
        end ;

     end ;



procedure TECGFrm.scECGDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------------
// Mouse button depressed over ECG display
// ---------------------------------------
begin
     // Set groups to indicate ECG display is selected for copying
     scECGDisplay.Tag := 1 ;
     scAvgDisplay.Tag := 0 ;
     end;


procedure TECGFrm.scAvgDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// -------------------------------------------
// Mouse button depressed over Average display
// -------------------------------------------
begin
     // Set groups to indicate average ECG display is selected for copying
     scECGDisplay.Tag := 0 ;
     scAvgDisplay.Tag := 1 ;
     end;


procedure TECGFrm.scSpecDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ------------------------------------
// Spectrum data time window mouse down
// ------------------------------------
begin
     // Select time window for copying/printing
     scSpecDisplay.Tag := 1 ;
     plSpectrum.Tag := 0 ;
     end;

procedure TECGFrm.plSpectrumMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ------------------------------------
// Power Spectrum plot mouse down
// ------------------------------------
begin
     // Select power spectrum for copying/printing
     scSpecDisplay.Tag := 0 ;
     plSpectrum.Tag := 1 ;
     end;


procedure TECGFrm.plPlotCursorChange(Sender: TObject);
// ---------------------------
// Power spectrum plot changed
// ---------------------------
var
     x : Single ;
     y : Single ;
begin

    plPlot.GetPoint(0,plPlot.FindNearestIndex(0,0),x,y);
    lbPlot.Caption := format('%.4g min, %.4g',[x,y]) ;
    lbPlot.Left := plPlot.Left
                       + plPlot.XToCanvasCoord(x)
                       - (lbPlot.Width div 2) ;
    lbPlot.Top := plPlot.Top + plPlot.Height + 1 ;

    end;

end.
