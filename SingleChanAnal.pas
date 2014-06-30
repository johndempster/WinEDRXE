unit SingleChanAnal;
{ ================================================================
  WinEDR (c) J. Dempster, University of Strathclyde, 1998-2001
  Single-channel analysis module
  ================================================================
  1/9/98
  20/3/99 SigDisp components used signal and plot display
  1/9/99 ... ChangeDisplayGrid added
  10/9/99 ... Stability plot event range now fixed
  24/2/00 ... Copy data and image now works with Detect and Edit transitions pages
  25/2/00 ... Individual exponential components now displayed
  18/6/01 ... Histogram/plot types now included in hard copies
  28/6/01 ... Amplitude histograms now incorporated into this module
  9/7/01 .... Name changed to SingleChanAnal
  13/7/01 ... Only Closed and Open states now recognised (no muliple openings)
  14/8/01 ... Dwell times summary table added
  12/10/01 ... All points in State amplitude histogram fixed (List Index Out or range error)
  23/10/01 ... Summary mean open time in burst now computed correctly
               no longer produces negative values
  9/2/02   ... (V2.2.6) Amplitude histogram sub-range now works
                Now updates channel zero levels in EDR file header
  17.12.02 ... Initial guesses now incorporated into SetFitPar.pas
  20.12.02 ... Mean state amplitude histogram now works with All & Open states
  13.02.03 ... Range definition cursors (C0-C1) now work over whole range of data file
               Amplitude and stability plot range can now be set using C0-C1 cursors
  24.6.03 .... No. horizontal/vertical grid lines changeable
  5.02.04 .... Event file I/O methods moved from fileio.pas to into this form
               (All single channel analysis code now in this form
  16.01.05 ... Stability plot averaging block size now defined (rather than number of blocks)
  09.06.06 ... Single-channel current stability plot added
  13.11.06 ... Detection/Edit Display widths now separate and in time units
  21.12.06 ... Manual cursor measurement facility added
  11.01.10 ... No. bins, bin range and width can be set on amplitude histogram
  22.06.10 ... unitary current label in amplitude histogram now at top of cursor line
  26.07.10 ... Time/event sub-range selections now retained in file and between pages
               Unit current now set using unit-c cursor (rather than button) and
               changes in cursors detection window now reflected in amplitude
               histogram and vice versa. Stability plot block size and no. channels
               now stored in EDR file
  20.04.11 ... No. bins, bin width and bin range now all updated when any one changed,
               fixing bug where histograms were plotted using incorrect bin width when
               width was set below lower limit (lower limit also now set to amplitude resolution
               Max. no. of bins in histogram increased to 4000.
               Initial guesses for gaussian fits improved
               Stability plots now only plot complete averaging blocks.
              }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, TabNotBk, printers, ClipBrd,
  global, shared, maths, fileio, Grids, setfitpa, strutils,
  RangeEdit, ValEdit, ScopeDisplay, XYPlotDisplay, ValidatedEdit, Log, Math,
  ComCtrls, HTMLLabel, Menus ;

type

  TEventFile = record
             Name : string ;
             Handle : Integer ;
             Pointer : Integer ;
             NumEvents : Integer ;
             Open : Boolean ;
             end ;
  TEvent = packed record
           ChannelState : Integer ;
           ZeroLevel : Integer ;
           StartAt : Integer ;
           EndAt : Integer ;
           ExactStart : double ;
           ExactEnd : double ;
           Duration : double ;
           Average : single ;
           Variance : single ;
           Next : Integer ;
           Previous : Integer ;
           Ignore : ByteBool  ;
           Available : ByteBool ;
           Pad : Array[1..2] of Byte ;
           // Added to make record length 60 bytes to be
           // compatible with V2.1.2 and earlier. .EDE
           // Problem arose when going from Delphi V3 to V5 compiler
           // 23/10/01
           end ;

    TStabPlotType = ( stMeanCurrent,
                      stOpenProb,
                      stClosedTimes,
                      stOpenTimes,
                      stChannelCurrents,
                      stAvgVsOpenTimes,
                      stSummary,
                      stCursorAverage,
                      stCursorDuration,
                      stCursorSD ) ;

  TSingleChanAnalFrm = class(TForm)
    Page: TPageControl;
    AmpHistTab: TTabSheet;
    DetectTransitionsTab: TTabSheet;
    EditTransitionsTab: TTabSheet;
    lbVertCursor1: TLabel;
    DwellTHistTab: TTabSheet;
    OpenDialog: TOpenDialog;
    StabPlotTab: TTabSheet;
    StabGrp: TGroupBox;
    bDoStabPlot: TButton;
    bStabPlotSetAxes: TButton;
    GroupBox5: TGroupBox;
    rbStabPlotAllEvents: TRadioButton;
    rbStabPlotRange: TRadioButton;
    edStabPlotRange: TRangeEdit;
    plStabPlot: TXYPlotDisplay;
    DwellTHistGrp: TGroupBox;
    bNewDwellTHist: TButton;
    bDwellTSetAxes: TButton;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    rbLinear: TRadioButton;
    rbLogLin: TRadioButton;
    nbLinLogSettings: TNotebook;
    Label7: TLabel;
    edHistRange: TRangeEdit;
    Label3: TLabel;
    edNumLogBinsPerDecade: TValidatedEdit;
    edNumBins: TValidatedEdit;
    GroupBox9: TGroupBox;
    lbTCritical: TLabel;
    rbAllEvents: TRadioButton;
    rbEventRange: TRadioButton;
    edDwellEventRange: TRangeEdit;
    edTCritical: TValidatedEdit;
    plDwellTHist: TXYPlotDisplay;
    DwellTResultsGrp: TGroupBox;
    bDwellTFitCurve: TButton;
    cbDwellTEqn: TComboBox;
    erDwellTResults: TRichEdit;
    lbDwellTArea: TLabel;
    lbDwellTC0: TLabel;
    lbDwellTC1: TLabel;
    shLine: TShape;
    EditGrp: TGroupBox;
    sbEvent: TScrollBar;
    edEvent: TRangeEdit;
    GroupBox4: TGroupBox;
    ckIgnoreState: TCheckBox;
    bBlockIgnore: TButton;
    bDeleteIgnored: TButton;
    scEditDisplay: TScopeDisplay;
    DetectGrp: TGroupBox;
    bDetect: TButton;
    bAbortDetection: TButton;
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edDetRange: TRangeEdit;
    CriteriaGrp: TGroupBox;
    Label2: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    cbDetChannel: TComboBox;
    edDetUnitCurrent: TValidatedEdit;
    edThreshold: TValidatedEdit;
    GroupBox1: TGroupBox;
    bAddBlock: TButton;
    cbTrendEquation: TComboBox;
    bClearSamples: TButton;
    bRemoveTrend: TButton;
    edNumBlocks: TValidatedEdit;
    bRestoreOriginal: TButton;
    edStatus: TEdit;
    scDetDisplay: TScopeDisplay;
    sbDetDisplay: TScrollBar;
    AmpHistGrp: TGroupBox;
    Label1: TLabel;
    bNewAmpHist: TButton;
    GroupBox6: TGroupBox;
    Label5: TLabel;
    rbAmpWholeFile: TRadioButton;
    rbAmpRange: TRadioButton;
    cbAmpHistChannel: TComboBox;
    edAHRange: TRangeEdit;
    AmpHistTypePage: TNotebook;
    Label9: TLabel;
    cbChannelState: TComboBox;
    Label12: TLabel;
    Label13: TLabel;
    edPatlakSDLimit: TValidatedEdit;
    edNumPatlakAvg: TValidatedEdit;
    bAmpHistSetAxes: TButton;
    cbAmpHistType: TComboBox;
    bAbortAmpHist: TButton;
    plAmpHist: TXYPlotDisplay;
    AmpHistResultsGrp: TGroupBox;
    bAmpFitCurve: TButton;
    cbAmpHistEqn: TComboBox;
    erAmpResults: TRichEdit;
    lbAmpHistC0: TLabel;
    lbAmpHistC1: TLabel;
    shAmpHistLine: TShape;
    lbAmpHistArea: TLabel;
    shDwellTLine: TShape;
    GroupBox3: TGroupBox;
    rbEventList: TRadioButton;
    rbClosedTimes: TRadioButton;
    rbOpenTimes: TRadioButton;
    bExportEventList: TButton;
    SaveDialog: TSaveDialog;
    bAbortStabPlot: TButton;
    cbDwellTHistType: TComboBox;
    Label10: TLabel;
    bFTest: TButton;
    cbStabPlotType: TComboBox;
    Label17: TLabel;
    GroupBox10: TGroupBox;
    bSaveDwellTHIst: TButton;
    sgSummary: TStringGrid;
    bUseCursors: TButton;
    CursorsGrp: TGroupBox;
    bGetC0: TButton;
    bGetC1: TButton;
    bUseCursorsForAmpHistRange: TButton;
    bUseCursorsForStabPlotRange: TButton;
    Label14: TLabel;
    edAHNUmBins: TValidatedEdit;
    GroupBox7: TGroupBox;
    bSetZero: TButton;
    bSaveToLog: TButton;
    bSaveToLogSummary: TButton;
    bSaveToLogDwellTimes: TButton;
    StabPlotPage: TPageControl;
    Default: TTabSheet;
    AvgVsOpenTimes: TTabSheet;
    StabPlotBlockSizePanel: TPanel;
    lbStabPlotBlockSize: TLabel;
    edStabPlotBlockSize: TValidatedEdit;
    StabPlotNumChannelsPanel: TPanel;
    Label16: TLabel;
    edStabPlotNumChannels: TValidatedEdit;
    StabPlotTCriticalPanel: TPanel;
    Label15: TLabel;
    edStabPlotTCritical: TValidatedEdit;
    Label18: TLabel;
    DetDisplayWidthPanel: TPanel;
    Label21: TLabel;
    edDetDisplayWidth: TValidatedEdit;
    EditDisplayWidthPanel: TPanel;
    Label22: TLabel;
    edEditDisplayWidth: TValidatedEdit;
    GroupBox11: TGroupBox;
    Label11: TLabel;
    edMarginPoints: TValidatedEdit;
    GroupBox12: TGroupBox;
    bSaveCursorReading: TButton;
    bSaveToMeasurementsFile: TButton;
    bLoadFromMeasurementsFile: TButton;
    edCursorSpacing: TValidatedEdit;
    Label19: TLabel;
    bClearCursorReadingList: TButton;
    lbResults: THTMLLabel;
    lbCursors: THTMLLabel;
    edNumCursorMeasurements: TValidatedEdit;
    AHBinRangePanel: TPanel;
    Label20: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edAHBinWidth: TValidatedEdit;
    edAHBinsLower: TValidatedEdit;
    edAHBinsUpper: TValidatedEdit;
    bHalveDetDisplay: TButton;
    bTDisplayDouble: TButton;
    bHalveEditDisplayDuration: TButton;
    BDoubleEditDisiplayDuration: TButton;
    edAHUnitCurrent: TValidatedEdit;
    Label25: TLabel;
    PopupMenu1: TPopupMenu;
    ckEnableCursorMeasurement: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure sbDetDisplayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edDetUnitCurrentKeyPress(Sender: TObject; var Key: Char);
    procedure bDetectClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbEventChange(Sender: TObject);
    procedure bAbortDetectionClick(Sender: TObject);
    procedure bNewDwellTHistClick(Sender: TObject);
    procedure bDwellTSetAxesClick(Sender: TObject);
    procedure bDwellTFitCurveClick(Sender: TObject);
    procedure cbDwellTHistTypeChange(Sender: TObject);
    procedure scDetDisplayCursorChange(Sender: TObject);
    procedure PageChange(Sender: TObject);
    procedure plDwellTHistCursorChange(Sender: TObject);
    procedure rbLinearClick(Sender: TObject);
    procedure rbLogLinClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bAddBlockClick(Sender: TObject);
    procedure bClearSamplesClick(Sender: TObject);
    procedure cbTrendEquationChange(Sender: TObject);
    procedure bRemoveTrendClick(Sender: TObject);
    procedure bRestoreOriginalClick(Sender: TObject);
    procedure cbDetChannelChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure bDoStabPlotClick(Sender: TObject);
    procedure bStabPlotSetAxesClick(Sender: TObject);
    procedure cbStabPlotTypeChange(Sender: TObject);
    procedure ckIgnoreStateClick(Sender: TObject);
    procedure bBlockIgnoreClick(Sender: TObject);
    procedure bDeleteIgnoredClick(Sender: TObject);
    procedure scEditDisplayCursorChange(Sender: TObject);
    procedure edEventKeyPress(Sender: TObject; var Key: Char);
    procedure bNewAmpHistClick(Sender: TObject);
    procedure plAmpHistCursorChange(Sender: TObject);
    procedure bSetZeroClick(Sender: TObject);
    procedure bSetUnitCurrentClick(Sender: TObject);
    procedure bAmpFitCurveClick(Sender: TObject);
    procedure bAmpHistSetAxesClick(Sender: TObject);
    procedure bAbortAmpHistClick(Sender: TObject);
    procedure cbAmpHistTypeChange(Sender: TObject);
    procedure bExportEventListClick(Sender: TObject);
    procedure bAbortStabPlotClick(Sender: TObject);
    procedure bFTestClick(Sender: TObject);
    procedure bSaveDwellTHIstClick(Sender: TObject);
    procedure cbAmpHistChannelChange(Sender: TObject);
    procedure bUseCursorsClick(Sender: TObject);
    procedure bGetC0Click(Sender: TObject);
    procedure bGetC1Click(Sender: TObject);
    procedure bUseCursorsForAmpHistRangeClick(Sender: TObject);
    procedure bUseCursorsForStabPlotRangeClick(Sender: TObject);
    procedure bSaveToLogClick(Sender: TObject);
    procedure bSaveToLogSummaryClick(Sender: TObject);
    procedure bSaveToLogDwellTimesClick(Sender: TObject);
    procedure edDetDisplayWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edEditDisplayWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edMarginPointsKeyPress(Sender: TObject; var Key: Char);
    procedure bSaveCursorReadingClick(Sender: TObject);
    procedure bSaveToMeasurementsFileClick(Sender: TObject);
    procedure bClearCursorReadingListClick(Sender: TObject);
    procedure bLoadFromMeasurementsFileClick(Sender: TObject);
    procedure edCursorSpacingKeyPress(Sender: TObject; var Key: Char);
    procedure edAHBinWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edAHBinsLowerKeyPress(Sender: TObject; var Key: Char);
    procedure edAHBinsUpperKeyPress(Sender: TObject; var Key: Char);
    procedure edAHNUmBinsKeyPress(Sender: TObject; var Key: Char);
    procedure bHalveDetDisplayClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure bHalveEditDisplayDurationClick(Sender: TObject);
    procedure BDoubleEditDisiplayDurationClick(Sender: TObject);
    procedure edAHUnitCurrentKeyPress(Sender: TObject; var Key: Char);
    procedure edAHRangeKeyPress(Sender: TObject; var Key: Char);
    procedure ckEnableCursorMeasurementClick(Sender: TObject);
    procedure edThresholdKeyPress(Sender: TObject; var Key: Char);

  private

    // Region selection cursors
    SelectionCursor0 : Integer ;
    SelectionCursor1 : Integer ;

    { Private declarations }
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure InitialiseDisplays ;

    // Amplitude histogram methods
    procedure InitialiseAmpHist ;
    procedure AllPointsHistogram ;
    procedure AllPointsInStateHistogram ;
    procedure StateAverageHistogram ;
    procedure PatlakAverageHistogram ;
    procedure ExternalAmplitudeHistogram ;
    procedure CursorMeasurementsAverageHistogram ;
    procedure AdjustAmpHistZeroLevel( NewADCZero : Integer );
    function AverageEventAmplitude(
             Event : TEvent ;
             var AverageAmplitude : Single ;
             var StDev : Single
             ) : Boolean ;

    procedure ChannelChanged ;
    procedure DisplayRecord ;
    procedure DisplayEvent ;
    procedure UpdateCursors( scDisplay : TScopeDisplay ) ;

    procedure InitialiseDetect ;
    procedure FitTrendLine ;

    // Dwell time histogram methods
    procedure InitialiseDwellTHist ;
    procedure DwellTimeHistogram( RequiredState : Integer ) ;
    procedure BurstDurationHistogram ;
    procedure OpenTimesWithinBurstHistogram ;
    procedure SingleOpenTimesHistogram ;
    procedure OpeningsPerBurstHistogram ;
    procedure ExternalDwellTimesList ;
    procedure ExternalDwellTimeHistogram ;
    procedure UpdateHistogram( Time : Double ; Value : single ) ;

    procedure SetCopyAndPrintMenus ;


    // Stability plot methods
    procedure InitialiseStabPlot ;
    procedure DwellTimeStabPlot(
              StartAt : Integer ;
              EndAt : Integer ;
              BlockSize : Integer ;
              RequiredState : Integer
              ) ;
    procedure ChannelCurrentStabPlot(
              StartAt : Integer ;
              EndAt : Integer ;
              BlockSize : Integer
              ) ;
    procedure MeanCurrentStabPlot(
              StartAt : Integer ;
              EndAt : Integer ;
              RegionSize : Integer ;
              DivideFactor : single
              ) ;
   procedure StateAverageVsOpenTime(
             StartAt : Integer ;
             EndAt : Integer
             ) ;

   procedure SummaryTable(
             StartAt : Integer ;
             EndAt : Integer
           ) ;

   procedure CursorMeasurementsStabPlot(
             StabPlotType : TStabPlotType ; // Type of plot (in)
             StartAt : Integer ;       { Measurement to start at [In] }
             EndAt : Integer ;         { Measurement to end at [In] }
             RegionSize : Integer      { No. of seasurements per block [In] }
             ) ;

    procedure NewStabPlotType ;

    procedure SaveCursorMeasurementsToFile(
              FileName : String
              ) ;

    procedure LoadCursorMeasurementsFromFile(
              FileName : String
              ) ;

  public

    EventFile : TEventFile ;         // Event list file record

    { Public declarations }
    procedure CopyDataToClipBoard ;
    procedure PrintDisplay ;
    procedure CopyImageToClipboard ;
    procedure ZoomOutAll ;
    procedure ZoomIn( Chan : Integer ) ;
    procedure ZoomOut( Chan : Integer ) ;
    procedure ChangeDisplayGrid ;

    procedure OpenEventFile( var EventFile : TEventFile ) ;
    procedure CloseEventFile( var EventFile : TEventFile ) ;
    procedure WriteEventToFile(
              var EventFile : TEventFile ;
              EventNum : Integer ;
              var Event : TEvent
              ) ;
    procedure ReadEventFromFile(
              var EventFile : TEventFile ;
              EventNum : Integer ;
              var Event : TEvent
              ) ;

  end;

var
  SingleChanAnalFrm: TSingleChanAnalFrm;



implementation

uses mdiform, setaxes, printgra, SetIgnor, Printrec, ftest ;

const
     MaxDisplayPoints = 100000 ;
     MaxCursorMeasurements = 10000 ;

     csClosedState = 0 ;
     csOpenState = 1 ;
     FittedLine = 1 ;

     MaxHistogramSize = 1024 ;
     MaxHistogramBins = MaxHistogramSize - 1 ;

     AllPointsPage = 0 ;
     EventHistPage = 1 ;
     PatlakAveragePage = 2 ;

type
    TAmpHistType = (htAllPoints,htAllPointsInState,htStateAverage,
                      htPatlakAverage,htAmplitudesFile,htCursorAvg) ;

    TDwellTHistType = (htOpenTimes,
                      htClosedTimes,
                      htBurstDurations,
                      htBurstOpenTimes,
                      htSingleOpenTimes,
                      htOpeningsPerBurst,
                      htExternalHist,
                      htExternalTimes ) ;

    TDetector = record
              Initialised : Boolean ;
              UnitLevel : Integer ;
              Threshold : Integer ;
              TransitionTime : double ;
              OldTransitionTime : double ;
              TransitionSample : Integer ;
              OldTransitionSample : Integer ;
              ChannelState : Integer ;
              OldChannelState : Integer ;
              SampleNum : Integer ;
              StartAtSample : Integer ;
              EndAtSample : Integer ;
              ExactStart : double ;
              ExactEnd : double ;
              Y : Integer ;
              YOld : Integer ;
              Sum : double ;
              SumSquares : double ;
              NumSamples : Integer ;
              EventNum : Integer ;
              end ;
    TTrendLine = record
                 Available : boolean ;
                 NumPoints : Integer ;
                 x : Array[0..100] of single ;
                 y : Array[0..100] of single ;
                 Func : TMathFunc ;
                 end ;
    TCursors = record
             C0 : Integer ;
             OldC0 : Integer ;
             C1 : Integer ;
             Z0 : Integer ;
             OldZ0 : Integer ;
             Z1 : Integer ;
             Read : Integer ;
             IUnit : Integer ;
             Base : Integer ;
             Threshold : Integer ;
             end ;

    TCursorMeasurement = record
        AverageCurrent : Single ;
        SDCurrent : Single ;
        TStart : Single ;
        Duration : Single ;
        end ;

var
   DetBuf : PSmallIntArrayDyn ;           { A/D sample data array }
   EditBuf : PSmallIntArrayDyn ;           { A/D sample data array }

   ChanNum : Integer ;       // Input channel being analysed

   AmpHist : THistogram ;    // Amplitude histogram
   AmpFunc :TMathFunc ;      // Amplitude histogram fitting equation
   AmpResults : TStringList ; // Amplitude histogram fitting results
   AmpCurs : TCursors ;       // Histogram cursors
   AmpHistADCZero : Integer ;

   // Detection page
   DetCurs : TCursors ;

   EditCurs : TCursors ;

   DwellTHist : THistogram ; // Dwell-time histogram data record
   DwellTFunc :TMathFunc ;  // Dwell-time histogram fitting equation
   DwellTResults : TStringList ; // Dwell-time histogram fitting results
   DwellTCurs : TCursors ;       // Histogram cursors

   BuffersAllocated : Boolean ;
   Det : TDetector ;

   StateNames : Array[0..10] of string ;
   TrendLine : TTrendLine ;
   StabCurs : TCursors ;       // Histogram cursors

   CursorMeasurements : Array[0..MaxCursorMeasurements-1] of TCursorMeasurement ;
   NumCursorMeasurements : Integer ;


{$R *.DFM}


procedure TSingleChanAnalFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                //New(ADC) ;
                GetMem(DetBuf,2*MaxDisplayPoints*CDRFH.NumChannels) ;
                GetMem(EditBuf,2*MaxDisplayPoints*CDRFH.NumChannels) ;
                // Amplitude histogram objects
                AmpHist := THistogram.Create ;
                AmpFunc := TMathFunc.Create ;
                AmpFunc.Setup( None, ' ', ' ' ) ;
                AmpResults := TStringList.Create ;
                // Dwell time histogram objects
                DwellTHist := THistogram.Create ;
                DwellTFunc := TMathFunc.Create ;
                DwellTFunc.Setup( None, ' ', ' ' ) ;
                DwellTResults := TStringList.Create ;

                TrendLine.Func := TMathFunc.Create ;
                TrendLine.Func.Setup( None, ' ', ' ' ) ;

                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Freemem(DetBuf) ;
                FreeMem(EditBuf) ;
                AmpHist.Free ;
                AmpFunc.Free ;
                AmpResults.Free ;
                DwellTHist.Free ;
                DwellTFunc.Free ;
                DwellTResults.Free ;

                TrendLine.Func.Free ;

                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TSingleChanAnalFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
begin

     ClientWidth := Page.Left + Page.Width + 10 ;
     ClientHeight := Page.Top + Page.Height + 10 ;

     { Allocate working buffers from heap }
     HeapBuffers( Allocate ) ;

     { Open event file }
     EventFile.Open := False ;
     EventFile.Handle := -1 ;
     OpenEventFile( EventFile ) ;

     // Load cursor measurements list
     NumCursorMeasurements := 0 ;
     LoadCursorMeasurementsFromFile( ChangeFileExt( CDRFH.FileName, '.crm' )) ;

     // Initialise amplitude histogram methods
     ChanNum := 0 ;
     InitialiseAmpHist ;

     // Initialise dwell time histogram methods
     InitialiseDwellTHist ;

     // Initialise transition detection page
     InitialiseDetect ;

     // Initialise stability plot detection page
     InitialiseStabPlot ;

     { Signal display window duration }
     edDetDisplayWidth.Units := 'ms' ;
     edDetDisplayWidth.Scale := CDRFH.dt*1000.0 ;
     edDetDisplayWidth.Value := Settings.DwellTimes.RecordSize ;

     edEditDisplayWidth.Units := 'ms' ;
     edEditDisplayWidth.Scale := CDRFH.dt*1000.0 ;
     edEditDisplayWidth.Value := Settings.DwellTimes.RecordSize ;

     SelectionCursor0 := 0 ;
     SelectionCursor1 := 100 ;

     InitialiseDisplays ;
     DisplayRecord ;

     Page.ActivePage := AmpHistTab ;

     AHBinRangePanel.Visible := False ;

     lbCursors.Caption := '' ;
     lbResults.Caption := '' ;

     resize ;

     end;


procedure TSingleChanAnalFrm.InitialiseDisplays ;
{ ------------------------------------------------
  Initialise display to selected detection channel
  ------------------------------------------------}
var
   ch : Integer ;
begin

     // Update internal channel selected for analysis
     ChanNum := Settings.DwellTimes.ChanNum ;

     { Continuous record display channel }
     scDetDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDetDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDetDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDetDisplay.MaxPoints := Round(edDetDisplayWidth.Value) ;
     scDetDisplay.NumPoints := scDetDisplay.MaxPoints ;
     scDetDisplay.NumChannels := CdrFH.NumChannels ;
     scDetDisplay.xMin := 0 ;
     scDetDisplay.xMax := scDetDisplay.MaxPoints  ;
     scDetDisplay.DisableChannelVisibilityButton := True ;
     scDetDisplay.SetDataBuf( DetBuf ) ;     

     { Set channel information (only detected channel on display) }
     for ch := 0 to scDetDisplay.NumChannels-1 do begin
         scDetDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDetDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scDetDisplay.yMin[ch] := Channel[ch].yMin ;
         scDetDisplay.yMax[ch] := Channel[ch].yMax ;
         scDetDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scDetDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDetDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scDetDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scDetDisplay.ChanColor[ch] := clBlue ;
         if ch = ChanNum then scDetDisplay.ChanVisible[ch] := True
                         else scDetDisplay.ChanVisible[ch] := False ;
         end ;
     scDetDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDetDisplay.TUnits := Settings.TUnits ;

     { Create display cursors }
     scDetDisplay.ClearHorizontalCursors ;
     DetCurs.Base :=  scDetDisplay.AddHorizontalCursor( ChanNum, clgray, True,'zero' ) ;
     DetCurs.Threshold := scDetDisplay.AddHorizontalCursor( ChanNum, clgray, False,'threshold' ) ;
     DetCurs.IUnit := scDetDisplay.AddHorizontalCursor( ChanNum, clgray, False,'unit-c' ) ;

     scDetDisplay.ClearVerticalCursors ;
     DetCurs.C0 := scDetDisplay.AddVerticalCursor( AllChannels, clBlue, 'c0' ) ;
     DetCurs.C1 := scDetDisplay.AddVerticalCursor( AllChannels, clBlue, 'c1' ) ;
     scDetDisplay.LinkVerticalCursors(DetCurs.C0,DetCurs.C1);

     scDetDisplay.VerticalCursors[DetCurs.C0] := 0 ;
     scDetDisplay.VerticalCursors[DetCurs.C0] := 100 ;

     { Detected event display }
     scEditDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scEditDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scEditDisplay.DisplayGrid := Settings.DisplayGrid ;

     scEditDisplay.MaxPoints := Round(edEditDisplayWidth.Value) ;
     scEditDisplay.NumPoints := scEditDisplay.MaxPoints ;
     scEditDisplay.NumChannels := CdrFH.NumChannels ;
     scEditDisplay.xMin := 0 ;
     scEditDisplay.xMax := scEditDisplay.MaxPoints  ;
     scEditDisplay.DisableChannelVisibilityButton := True ;
     scEditDisplay.SetDataBuf( EditBuf ) ;

     { Create display cursors }
     scEditDisplay.ClearHorizontalCursors ;
     EditCurs.Base :=  scEditDisplay.AddHorizontalCursor( ChanNum, clgray, True,'zero' ) ;
     EditCurs.Threshold := scEditDisplay.AddHorizontalCursor( ChanNum, clgray, False,'threshold' ) ;
     EditCurs.IUnit := scEditDisplay.AddHorizontalCursor( ChanNum, clgray, False,'unit-c' ) ;

     scEditDisplay.ClearVerticalCursors ;
     if ckEnableCursorMeasurement.Checked then begin
        EditCurs.C0 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'a' ) ;
        EditCurs.C1 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'a' ) ;
        scEditDisplay.LinkVerticalCursors( EditCurs.C0, EditCurs.C1 );
        EditCurs.Z0 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'z' ) ;
        EditCurs.Z1 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'z' ) ;
        scEditDisplay.LinkVerticalCursors( EditCurs.Z0, EditCurs.Z1 );
        end ;

     { Set Edit Display channel information (only one channel on display) }
     for ch := 0 to scDetDisplay.NumChannels-1 do begin
         scEditDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scEditDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scEditDisplay.yMin[ch] := Channel[ch].yMin ;
         scEditDisplay.yMax[ch] := Channel[ch].yMax ;
         scEditDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scEditDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scEditDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scEditDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scEditDisplay.ChanColor[ch] := clBlue ;
         if ch = ChanNum then scEditDisplay.ChanVisible[ch] := True
                         else scEditDisplay.ChanVisible[ch] := False ;
         end ;
     scEditDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scEditDisplay.TUnits := Settings.TUnits ;

     edDetDisplayWidth.Units := 'ms' ;
     edDetDisplayWidth.Scale := CDRFH.dt*1000.0 ;
     edDetDisplayWidth.HiLimit := MaxDisplayPoints ;

     edMarginPoints.Scale := CDRFH.dt*1000.0 ;

     end ;


procedure TSingleChanAnalFrm.DisplayRecord ;
{ ---------------------------------------------
  Display currently selected block of data file
  ---------------------------------------------}
var
   i : Integer ;
   x,xStep : single ;
begin

   sbDetDisplay.Max := Max( (CdrFH.NumSamplesInFile div CdrFH.NumChannels)
                            - scDetDisplay.MaxPoints,1) ;
   sbDetDisplay.LargeChange := scDetDisplay.MaxPoints div 4 ;

   scDetDisplay.xOffset := sbDetDisplay.Position ;

   if ReadCDRBuffer(CdrFH,sbDetDisplay.Position,DetBuf^,scDetDisplay.MaxPoints)
      = scDetDisplay.MaxPoints then begin

      scDetDisplay.HorizontalCursors[DetCurs.Base] := Channel[ChanNum].ADCZero ;
      Det.UnitLevel := Round( Settings.DwellTimes.UnitCurrent / Channel[ChanNum].ADCScale ) ;
      scDetDisplay.HorizontalCursors[DetCurs.IUnit] := Channel[ChanNum].ADCZero
                                                      + Round(edDetUnitCurrent.Value/
                                                              Channel[ChanNum].ADCScale) ;
      scDetDisplay.HorizontalCursors[DetCurs.Threshold] := Channel[ChanNum].ADCZero
                                                 + Round((edDetUnitCurrent.Value
                                                         *edThreshold.Value)/
                                                         Channel[ChanNum].ADCScale) ;

      end ;

   // Display selection cursors
   scDetDisplay.VerticalCursors[DetCurs.C0] := SelectionCursor0 - scDetDisplay.xOffset ;
   scDetDisplay.VerticalCursors[DetCurs.C1] := SelectionCursor1 - scDetDisplay.xOffset ;

   { Display trend line, if one is available }
   scDetDisplay.CreateLine( ChanNum, clRed, psSolid, 1 ) ;
   if TrendLine.Available then begin
      x := 0.0 ;
      xStep := scDetDisplay.NumPoints / 100.0 ;
      for i := 0 to 99 do begin
          scDetDisplay.AddPointToLine( x, TrendLine.Func.Value(x+scDetDisplay.xOffset) ) ;
          x := x + xStep ;
          end ;
      end ;

   end;


procedure TSingleChanAnalFrm.UpdateCursors(
          scDisplay : TScopeDisplay
          ) ;
{ --------------------------------------------
  Move horizontal cursors on signal display
  --------------------------------------------}
begin
     scDisplay.HorizontalCursors[DetCurs.Base] := Channel[ChanNum].ADCZero ;
     scDisplay.HorizontalCursors[DetCurs.IUnit] := Channel[ChanNum].ADCZero
                                                   + Round(Settings.DwellTimes.UnitCurrent/
                                                           Channel[ChanNum].ADCScale) ;
     scDisplay.HorizontalCursors[DetCurs.Threshold] := Channel[ChanNum].ADCZero
                                                 + Round((Settings.DwellTimes.UnitCurrent
                                                         *edThreshold.Value)/
                                                         Channel[ChanNum].ADCScale) ;
     end ;


procedure TSingleChanAnalFrm.sbDetDisplayChange(Sender: TObject);
{ --------------------------------------
  New display window scroll bar position
  --------------------------------------}
begin
     if bDetect.Enabled then DisplayRecord ;
     end;


procedure TSingleChanAnalFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     //Settings.DwellTimes.UnitCurrent := edDetUnitCurrent.Value  ;
     //Settings.DwellTimes.Threshold := edThreshold.Value ;

     SaveCDRHeader( cdrFH ) ;

     { Close event file }
     CloseEventFile( EventFile ) ;

     HeapBuffers( Deallocate ) ;

     Main.CopyAndPrintMenus( False, False ) ;

     Screen.Cursor := crDefault ;

     Action := caFree ;
     end;


procedure TSingleChanAnalFrm.edDetUnitCurrentKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then begin
        Settings.DwellTimes.UnitCurrent := edDetUnitCurrent.Value ;
        UpdateCursors( scDetDisplay ) ;
        end ;
     end;

procedure TSingleChanAnalFrm.InitialiseDetect ;
//
// Initialise detection page controls
//
var
   i,ch : Integer ;
begin

     { Fill detection channel selection list }
     cbDetChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
          cbDetChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
          end ;
     cbDetChannel.ItemIndex := Settings.DwellTimes.ChanNum ;
     ChanNum := Settings.DwellTimes.ChanNum ;

     { Unitary current level }
     edDetUnitCurrent.Units := Channel[ChanNum].ADCUnits ;
     edDetUnitCurrent.Value := Settings.DwellTimes.UnitCurrent ;
     edAHUnitCurrent.Value := Settings.DwellTimes.UnitCurrent ;
     Det.UnitLevel := Round( Settings.DwellTimes.UnitCurrent / Channel[ChanNum].ADCScale ) ;

     { Transition detection threshold (% of unit current) }
     edThreshold.Value := Settings.DwellTimes.Threshold ;
     Det.Threshold := Round( Det.UnitLevel*edThreshold.Value ) ;

    { Set block of CDR file to be scanned }
     edDetRange.Scale := CdrFH.dt ;
     edDetRange.LoLimit := 0.0 ;
     edDetRange.HiLimit := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
     edDetRange.HiValue := Min(Settings.DwellTimes.SampleRangeHi,edDetRange.HiLimit) ;
     edDetRange.LoValue := Min(Settings.DwellTimes.SampleRangeLo,edDetRange.HiLimit) ;
     if edDetRange.LoValue >= edDetRange.HiValue then edDetRange.LoValue := 0.0 ;

     StateNames[0] := 'Closed' ;
     StateNames[1] := 'Open' ;
     for i := 2 to High(StateNames) do
         StateNames[i] := format('Open(%d)', [i] ) ;

     { Scroll bar control }
     sbDetDisplay.Max := (CdrFH.NumSamplesInFile div
                         (CdrFH.NumChannels*scDetDisplay.MaxPoints)) - 1 ;
     sbDetDisplay.Position := 0 ;

     { Create list of baseline trend curves that can be fitted }
     cbTrendEquation.Clear ;
     cbTrendEquation.Items.AddObject( 'Linear', TObject(Linear)) ;
     cbTrendEquation.Items.AddObject( 'Quadratic', TObject(Quadratic)) ;
     cbTrendEquation.Items.AddObject( 'Cubic', TObject(Cubic)) ;
     { Set initial  equation to Linear }
     cbTrendEquation.ItemIndex := 0 ;
     TrendLine.Func.Setup( TEqnType(
                           cbTrendEquation.Items.Objects[cbTrendEquation.ItemIndex]),
                           Settings.TUnits,
                           Channel[ChanNum].ADCUnits)  ;

     TrendLine.NumPoints := 0 ;
     TrendLine.Available := False ;
     bRemoveTrend.Enabled := False ;
     bRestoreOriginal.Enabled := CdrFH.BackedUp and
                                 FileExists(ChangeFileExt(CdrFH.FileName,'.bak')) ;

     end ;


procedure TSingleChanAnalFrm.bDetectClick(Sender: TObject);
{ --------------------------------
  Detect channel state transitions
  --------------------------------}
var
   NewBufferNeeded : Boolean ;
   Done : Boolean ;
   FirstSample : Boolean ;
   FirstEvent : Boolean ;
   j : Integer ;
   DeltaT : double ;
   ADCPointer : Integer ;
   Event : TEvent ;
begin

     bAbortDetection.Enabled := True ;
     sbDetDisplay.Enabled := False ;
     bDetect.Enabled := False ;

     { Let user clear event list }
     if EventFile.NumEvents > 0 then begin
        if MessageDlg('Clear existing events in list',mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then begin
           CloseEventFile( EventFile ) ;
           DeleteFile( PChar(EventFile.Name) ) ;
           OpenEventFile( EventFile ) ;
           end ;
        end ;

     Det.UnitLevel := Round(Settings.DwellTimes.UnitCurrent/Channel[ChanNum].ADCScale) ;
     Det.Threshold := Round(Det.UnitLevel*edThreshold.Value) ;

     { Range of samples to be scanned for single-channel transitions }

     Settings.DwellTimes.SampleRangeLo := Round(edDetRange.LoValue) ;
     Settings.DwellTimes.SampleRangeHi := Round(edDetRange.HiValue) ;
     if rbAllRecords.Checked then begin
        Det.StartAtSample := 0 ;
        Det.EndAtSample := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        Det.StartAtSample := Settings.DwellTimes.SampleRangeLo ;
        Det.EndAtSample :=   Settings.DwellTimes.SampleRangeHi ;
        end ;

     { Move data file pointer to start of data to be plotted }
     Det.SampleNum := Det.StartAtSample ;

     { Move event pointer to end of event file list }
     Det.EventNum := EventFile.NumEvents + 1 ;
     Det.Sum := 0.0 ;
     Det.SumSquares := 0.0 ;
     Det.NumSamples := 0 ;
     { Initial settings of while loop control flags }
     NewBufferNeeded := True ;
     ADCPointer := 0 ;
     FirstSample := True ;
     FirstEvent := True ;
     Done := False ;
     while not Done do begin
         Application.ProcessMessages ;
         { Load another buffer of A/D samples from file when needed }
         if NewBufferNeeded then begin
            scDetDisplay.xOffset := Det.SampleNum ;
            if ReadCDRBuffer(CdrFH,Det.SampleNum,DetBuf^,scDetDisplay.MaxPoints)
               = scDetDisplay.MaxPoints then begin
               UpdateCursors( scDetDisplay ) ;
               scDetDisplay.SetDataBuf( DetBuf ) ;
               end ;

            ADCPointer := 0 ;
            { Initialise idealised trace line }
            scDetDisplay.CreateLine( ChanNum, clRed, psSolid, 2 ) ;
           { Application.ProcessMessages ;}

            if not bAbortDetection.Enabled then Done := True ;

            { Indicate progress of detection }
            Main.StatusBar.SimpleText := format(
            ' Single-channel Analysis : Detecting Events %.1f/%.1f (%d events detected)',
            [Det.SampleNum*CDRFH.dt,Det.EndAtSample*CDRFH.dt,EventFile.NumEvents]) ;

            NewBufferNeeded := False ;
            end ;

         { Subtract baseline }
         j := (ADCPointer*CdrFH.NumChannels) + Channel[ChanNum].ChannelOffset ;
         Det.YOld := Det.Y ;
         Det.Y := DetBuf^[j] - Channel[ChanNum].ADCZero ;

         { Invert if negative single-channel currents }

        { if Det.UnitLevel < 0 then Det.Y := Abs(Det.Y) ;}

         { Determine number of open channels }
       {  Det.ChannelState := 0 ;
         if Det.UnitLevel >= 0 then begin
            While Det.Y >= (Det.Threshold + Det.UnitLevel*Det.ChannelState) do
                  Inc(Det.ChannelState) ;
            end
         else begin
            While Det.Y <= (Det.Threshold + Det.UnitLevel*Det.ChannelState) do
                  Inc(Det.ChannelState) ;
             end ; REMOVED 13/7/01 mutiple open states ignored}

         if Det.UnitLevel >= 0 then begin
            if Det.Y >= Det.Threshold then Det.ChannelState := 1
                                      else Det.ChannelState := 0 ;
            end
         else begin
            if Det.Y < Det.Threshold then Det.ChannelState := 1
                                     else Det.ChannelState := 0 ;
             end ;

         if FirstSample then begin
            Det.YOld := Det.Y ;
            Det.OldChannelState := Det.ChannelState ;
            FirstSample := False ;
            Det.OldTransitionTime := 0.0 ;
            Det.OldTransitionSample := 0 ;
            end ;

         { If a channel state transition has occurred - save old state data }

         if Det.OldChannelState <> Det.ChannelState then begin
            { Determine exact time of transition between samples }
            if Det.YOld <> Det.Y then begin
               DeltaT := Abs((Det.Y - Det.Threshold)/(Det.YOld - Det.Y)) ;
               end
            else begin
               DeltaT := 0.0 ;
               end ;
           { DeltaT := 0.0 ;}
            Det.TransitionTime := (Det.SampleNum - DeltaT) ;
            Det.TransitionSample := Det.SampleNum ;

            Event.ChannelState := Det.OldChannelState ;
            { Duration of channel state }
            Event.Duration := (Det.TransitionTime - Det.OldTransitionTime)*CdrFH.dt;
            { Sample where channel entered state }
            Event.StartAt := Det.OldTransitionSample ;
            Event.ExactStart := Det.OldTransitionTime ;
            Event.ExactEnd := Det.TransitionTime ;
            { Sample where channel left state }
            Event.EndAt := Det.TransitionSample - 1 ;

            Event.Average := (Det.Sum/Det.NumSamples)*Channel[ChanNum].ADCScale ;
            if Det.NumSamples > 1 then
               Event.Variance := Channel[ChanNum].ADCScale*Channel[ChanNum].ADCScale*
                                 (Det.Sumsquares
                                  - (Det.Sum*Det.Sum)/Det.NumSamples) /
                                 (Det.NumSamples-1)
            else Event.Variance := 0.0 ;
            Event.Ignore := False ;
            Det.Sum := 0.0 ;
            Det.SumSquares := 0.0 ;
            Det.NumSamples := 0 ;

            if not FirstEvent then begin
               WriteEventToFile( EventFile, Det.EventNum, Event ) ;
               Inc(EventFile.NumEvents) ;

               edStatus.text := format(' Event %d : %s : %.3g ms ',
                                [Det.EventNum,
                                 StateNames[Event.ChannelState],
                                 Event.Duration*SecsToMs] ) ;

               Inc(Det.EventNum) ;

               end
            else begin
               FirstEvent := False ;
               end ;

            Det.OldTransitionTime := Det.TransitionTime ;
            Det.OldChannelState := Det.ChannelState ;
            Det.OldTransitionSample := Det.TransitionSample ;
            end ;

         { Accumulate sum and sum of squares }
         Det.Sum := Det.Sum + Det.Y ;
         Det.SumSquares := Det.SumSquares + Det.Y*Det.Y ;

         Inc(Det.NumSamples) ;

         { Draw idealised channel time course }
         scDetDisplay.AddPointToLine( ADCPointer,
                                      Det.ChannelState*Det.UnitLevel + Channel[ChanNum].ADCZero ) ;

         { Increment A/D sample buffer pointer }
         Inc(ADCPointer) ;
         if ADCPointer >= scDetDisplay.MaxPoints then NewBufferNeeded := True ;

         { Increment main sample counter - and check for end of region }
         Inc(Det.SampleNum) ;
         if (Det.SampleNum > Det.EndAtSample)
            or (not bAbortDetection.Enabled) then Done := True ;

         end ;

     { Restore controls to idle state }
     bAbortDetection.Enabled := False ;
     sbDetDisplay.Enabled := True ;
     bDetect.Enabled := True ;
     scDetDisplay.CreateLine( ChanNum, clRed, psSolid, 2 ) ;

     { Clear status/progress bar }
     edStatus.Text := '' ;
     Main.StatusBar.SimpleText := format(
     ' Single-channel Analysis : %d events detected',
     [EventFile.NumEvents]) ;

     Settings.DwellTimes.EventRangeLo := 1 ;
     Settings.DwellTimes.EventRangeHi := EventFile.NumEvents ;

     end;



procedure TSingleChanAnalFrm.FormResize(Sender: TObject);
// ------------------------------------------------------
// Update control sizes/position when window size changed
// ------------------------------------------------------
var
   AtBottom : Integer ;
begin

     { Set size of notebook control }
     Page.Height := Max(ClientHeight - Page.Top - 5,2) ;
     Page.Width := Max(ClientWidth  - Page.Left - 5,2) ;

     { **** SINGLE-CHANNEL DETECTION PAGE **** }

     { Set height of detection control buttons/edit boxes group }

     AtBottom := DetectTransitionsTab.Height - 5 ;
     DetectGrp.Height := Max(AtBottom - DetectGrp.Top,2)  ;

     DetDisplayWidthPanel.Top := DetectTransitionsTab.ClientHeight -
                                 DetDisplayWidthPanel.Height - 5 ;
     DetDisplayWidthPanel.Left := DetectTransitionsTab.ClientWidth - 5
                                 - DetDisplayWidthPanel.Width ;

     // Display scroll bar size/position
     sbDetDisplay.Top := Max( DetDisplayWidthPanel.Top - sbDetDisplay.Height - 1,2) ;
     sbDetDisplay.Width := Max( DetectTransitionsTab.ClientWidth - 5 - sbDetDisplay.Left,2) ;

     // Set size of display
     scDetDisplay.Height := Max(sbDetDisplay.Top - scDetDisplay.Top,2) ;
     scDetDisplay.Width := Max(DetectTransitionsTab.ClientWidth - scDetDisplay.Left - 5,2) ;

     // Display record size box
     DetDisplayWidthPanel.Top := sbDetDisplay.Top + sbDetDisplay.Height + 2 ;

     { Events detected status box }
     edStatus.Top := DetectGrp.Height -  edStatus.Height - 5 ;

     // Set position of detection display points box
     DetDisplayWidthPanel.Left := sbDetDisplay.Width + sbDetDisplay.Left
                                - DetDisplayWidthPanel.Width ;

     { **** DETECTED EVENT EDITING PAGE **** }

     { Set height of event editing control buttons/edit boxes group }

     EditDisplayWidthPanel.Top := EditTransitionsTab.ClientHeight -
                                  EditDisplayWidthPanel.Height - 5 ;
     EditDisplayWidthPanel.Left := EditTransitionsTab.ClientWidth - 5
                                 - EditDisplayWidthPanel.Width ;

     // Size of edit event display area
     scEditDisplay.Height := Max(EditDisplayWidthPanel.Top -scEditDisplay.Top,2) ;
     scEditDisplay.Width := Max(EditTransitionsTab.ClientWidth - scEditDisplay.Left - 5,2) ;

     EditGrp.Height := EditTransitionsTab.Height - EditGrp.Top - 5 ;


     { **** DWELL-TIME HISTOGRAM PAGE **** }

     { Set width of histogram display area }
     plDwellTHist.Width :=  Max( DwellTHistTab.Width - plDwellTHist.Left - 5,2) ;
     { Height of controls group }
     DwellTHistGrp.Height := DwellTHistTab.Height - DwellTHistGrp.Top - 10 ;
     { Size of results group }
     DwellTResultsGrp.Top := DwellTHistGrp.Top + DwellTHistGrp.Height
                             - DwellTResultsGrp.Height ;
     DwellTResultsGrp.Width := plDwellTHist.Width ;
     erDwellTResults.Width := DwellTResultsGrp.Width - erDwellTResults.Left - 10 ;

     { Display cursor labels }
     lbDwellTC0.Top := DwellTResultsGrp.Top - lbDwellTC0.Height - 2 ;
     lbDwellTC1.Top := lbDwellTC0.Top ;
     { Height of histogram display area }
     plDwellTHist.Height := Max( lbDwellTC1.Top - plDwellTHist.Top - 2,2) ;
     DwellTResultsGrp.Width := plDwellTHist.Width ;
     erDwellTResults.Width := DwellTResultsGrp.Width - erDwellTResults.Left - 10 ;


     { *** Stability plot page *** }

     { Height of control buttons group }              
     StabGrp.Height := StabPlotTab.Height - StabGrp.Top - 10 ;

     { Size of plotting area }

     plStabPlot.Width := Max(StabPlotTab.ClientWidth - plStabPlot.Left - 5,2) ;
     plStabPlot.Height := Max(StabPlotTab.ClientHeight - plStabPlot.Top - 5,2) ;

     // Make dwell times summary table the same size as stabilty plot
     sgSummary.Width := plStabPlot.Width ;
     sgSummary.Height :=  plStabPlot.Height ;

     // *** Amplitude histogram page ***

     AmpHistGrp.Height := AmpHistTab.Height - AmpHistGrp.Top - 10 ;
     AmpHistResultsGrp.Top := AmpHistTab.Height - AmpHistResultsGrp.Height - 10 ;
     AmpHistResultsGrp.Width := AmpHistTab.ClientWidth - AmpHistResultsGrp.Left - 5 ;
     erAmpResults.Width := AmpHistResultsGrp.Width - erAmpResults.Left - 10 ;

     plAmpHist.Height := Max( AmpHistResultsGrp.Top
                              - lbAmpHistC0.Height - 20,2) ;
     plAmpHist.Width := Max(AmpHistResultsGrp.Width,2) ;

     lbAmpHistC0.Top := plAmpHist.Top + plAmpHist.Height + 2;
     shAmpHistLine.Top := lbAmpHistC0.Top + lbAmpHistC0.Height div 2 ;
     lbAmpHistC0.Visible := False ;
     lbAmpHistC1.Top := lbAmpHistC0.Top ;
     lbAmpHistC1.Visible := False ;
     lbAmpHistArea.Visible := False ;
     shAmpHistLine.Visible := False ;

     end;


procedure TSingleChanAnalFrm.DisplayEvent ;
{ --------------------------------------------
  Display the currently selected channel event
  --------------------------------------------}
var
   UnitLevel : Integer ;
   EventNum : Integer ;
   PreEventPoints : Integer ;
   iStart : Integer ;
   Event : TEvent ;
   PreviousEvent : TEvent ;
   NextEvent : TEvent ;
   x, y : single ;
   NumPointsAvailable : Integer ;  // No. of time points loaded from file
   AverageAmplitude : Single ;
   StDevAmplitude : Single ;
   s : String ;
begin

     if EventFile.NumEvents > 0 then begin

        { Read event selected by scroll bar }
        sbEvent.Min := 1 ;
        sbEvent.Max := EventFile.NumEvents ;
        sbEvent.Position := IntLimitTo(sbEvent.Position,1,EventFile.NumEvents) ;
        edEvent.LoValue := sbEvent.Position ;
        edEvent.HiValue := EventFile.NumEvents ;
        EventNum := sbEvent.Position ;

        { Read previous, current and next event from file }
        ReadEventFromFile( EventFile, EventNum, Event ) ;
        ReadEventFromFile( EventFile, EventNum-1, PreviousEvent ) ;
        ReadEventFromFile( EventFile, EventNum+1, NextEvent ) ;

        PreEventPoints := scEditDisplay.MaxPoints div 4 ;

        iStart := Max( Event.StartAt - PreEventPoints, 0 ) ;

        // Data from file
        NumPointsAvailable := ReadCDRBuffer(
                              CdrFH,
                              iStart,
                              EditBuf^,
                              scEditDisplay.MaxPoints) ;

        // Adjust display
        if NumPointsAvailable > 1 then begin
           UpdateCursors( scEditDisplay ) ;
           scEditDisplay.NumPoints := NumPointsAvailable ;

           scEditDisplay.xOffset := iStart ;
           end ;

         scEditDisplay.HorizontalCursors[EditCurs.Base] := Channel[ChanNum].ADCZero ;
         scEditDisplay.HorizontalCursors[EditCurs.IUnit] := Channel[ChanNum].ADCZero
                                                      + Round(Settings.DwellTimes.UnitCurrent/
                                                              Channel[ChanNum].ADCScale) ;
         scEditDisplay.HorizontalCursors[EditCurs.Threshold] := Channel[ChanNum].ADCZero
                                                 + Round((Settings.DwellTimes.UnitCurrent
                                                         *edThreshold.Value)/
                                                         Channel[ChanNum].ADCScale) ;

        // Amplitude zero cursors
        scEditDisplay.VerticalCursors[EditCurs.Z0] := Max(Event.StartAt -
                                                      Round(edCursorSpacing.Value) -
                                                      Round(edMarginPoints.Value)
                                                      - scEditDisplay.xOffset,0) ;
        EditCurs.OldZ0 := -1 ;

        // Amplitude average cursors
        scEditDisplay.VerticalCursors[EditCurs.C0] := Event.StartAt +
                                                      Round(edMarginPoints.Value)
                                                      - scEditDisplay.xOffset ;
        EditCurs.OldC0 := -1 ;

        { Plot idealised channel time course }
        UnitLevel := Round( Settings.DwellTimes.UnitCurrent / Channel[ChanNum].ADCScale ) ;
        scEditDisplay.CreateLine( ChanNum, clGreen, psSolid, 2 ) ;
        if PreviousEvent.Available then begin
           x := (Event.StartAt - iStart) - (scEditDisplay.MaxPoints*0.05) ;
           y := (PreviousEvent.ChannelState*UnitLevel) + Channel[ChanNum].ADCZero ;
           scEditDisplay.AddPointToLine( x, y ) ;
           x := (Event.ExactStart) - iStart ;
           scEditDisplay.AddPointToLine( x, y ) ;
           end ;
        x := (Event.ExactStart) - iStart ;
        y := (Event.ChannelState*UnitLevel) + Channel[ChanNum].ADCZero ;
        scEditDisplay.AddPointToLine( x, y ) ;
        x := Event.ExactEnd - iStart ;
        scEditDisplay.AddPointToLine( x, y ) ;
        if NextEvent.Available then begin
           y := (NextEvent.ChannelState*UnitLevel) + Channel[ChanNum].ADCZero ;
           scEditDisplay.AddPointToLine( x, y ) ;
           x := (Event.ExactEnd - iStart)
                + (scEditDisplay.MaxPoints*0.05) ;
           scEditDisplay.AddPointToLine( x, y ) ;
           end ;

        { State to be ignored in analysis check box }
        ckIgnoreState.Checked := Event.Ignore ;

        { Display event results }
        s := format( ' State= %s<br>',[StateNames[Event.ChannelState]]) ;
        s := s + format(' Duration= %.3g ms<br>',[Event.Duration*1000.0]) ;

        if AverageEventAmplitude( Event, AverageAmplitude, StDevAmplitude ) = True then begin
           s := s + format(' Avg.= %.3g %s<br>',
                    [AverageAmplitude,Channel[ChanNum].ADCUnits]) ;
           s := s + format(' S.D.= %.3g %s',
                    [StDevAmplitude,Channel[ChanNum].ADCUnits]) ;
           end
        else begin
           s := s + ' Avg.= n/a<br> St. Dev.= n/a' ;
           end ;
        lbResults.Caption := s ;

        end
    else begin
         edEvent.LoValue := 0 ;
         edEvent.HiValue := 0 ;
         end ;
    end;


procedure TSingleChanAnalFrm.sbEventChange(Sender: TObject);
begin
     { Force display of new event }
     DisplayEvent ;
     end;


procedure TSingleChanAnalFrm.bAbortDetectionClick(Sender: TObject);
{ --------------------------
  Abort transition detection
  --------------------------}
begin
     { Transition detection loop monitors
       enabled/disabled state of bAbort button }
     bAbortDetection.Enabled := False ;
     end;


{ ************************************************************************
  DWELL-TIME HISTOGRAM PROCEDURES
  ************************************************************************}


procedure TSingleChanAnalFrm.InitialiseDwellTHist ;
{ ----------------------------------------------------------
  Amplitude histogram initialisations when form is displayed
  ----------------------------------------------------------}
begin

     // Create list of available dwell time histogram
     cbDwellTHistType.Clear ;
     cbDwellTHistType.Items.AddObject( 'Closed times', TObject(htClosedTimes) ) ;
     cbDwellTHistType.Items.AddObject( 'Open times', TObject(htOpenTimes) ) ;
     cbDwellTHistType.Items.AddObject( 'Burst duration', TObject(htBurstDurations) ) ;
     cbDwellTHistType.Items.AddObject( 'Open times in burst', TObject(htBurstOpenTimes) ) ;
     cbDwellTHistType.Items.AddObject( 'Single open times', TObject(htSingleOpenTimes) ) ;
     cbDwellTHistType.Items.AddObject( 'No. openings per burst', TObject(htOpeningsPerBurst) ) ;
     cbDwellTHistType.Items.AddObject( 'External times file', TObject(htExternalTimes) ) ;
     cbDwellTHistType.Items.AddObject( 'External histogram file', TObject(htExternalHist) ) ;
     cbDwellTHistType.ItemIndex := 0 ;

     // Upper limit of dwell time histogram
     edHistRange.Scale := Settings.TScale ;
     edHistRange.Units := Settings.TUnits ;
     edHistRange.LoLimit := 0.0 ;
     edHistRange.LoValue := 0.0 ;
     edHistRange.HiValue := CdrFH.dt*1000.0 ;

     edTCritical.Scale := Settings.TScale ;
     edTCritical.Units := Settings.TUnits ;
     edTCritical.Value := Settings.DwellTimes.TCritical ;

     // Create list of curves that can be fitted to dwell time histogram
     cbDwellTEqn.Clear ;
     cbDwellTEqn.Items.AddObject( 'None', TObject(None)) ;
     cbDwellTEqn.Items.AddObject( 'Exponential', TObject(PDFExp)) ;
     cbDwellTEqn.Items.AddObject( '2 Exponentials', TObject(PDFExp2)) ;
     cbDwellTEqn.Items.AddObject( '3 Exponentials', TObject(PDFExp3)) ;
     cbDwellTEqn.Items.AddObject( '4 Exponentials', TObject(PDFExp4)) ;
     cbDwellTEqn.Items.AddObject( '5 Exponentials', TObject(PDFExp5)) ;
     { Set initial  equation to None }
     cbDwellTEqn.ItemIndex := 0 ;

     { Initialise histogram cursors }
     plDwellTHist.ClearVerticalCursors ;
     DwellTCurs.C0 := plDwellTHist.AddVerticalCursor( clGray, 'f',0 ) ;
     DwellTCurs.C1 := plDwellTHist.AddVerticalCursor( clGray, 'f',0 ) ;

     DwellTCurs.Read := plDwellTHist.AddVerticalCursor( clGreen, '?r',0 ) ;

     lbDwellTArea.visible := false ;
     shDwellTLine.Visible := false ;
     lbDwellTC0.visible := false ;
     lbDwellTC1.visible := false ;

     DwellTHist.Available := False ;
     bDwellTSetAxes.Enabled := False ;
     bDwellTFitCurve.Enabled := False ;

     end;


procedure TSingleChanAnalFrm.bNewDwellTHistClick(Sender: TObject);
{ ------------------------------------------
  Create and plot a new dwell time histogram
  ------------------------------------------}
var
   HistType : TDwellTHistType ;
   i,iBin : Integer ;
   x,Scale : single ;
begin

     bNewDwellTHist.Enabled := False ;
     Screen.Cursor := crHourGlass ;

     { Get type of histogram }
     HistType := TDwellTHistType(
                     cbDwellTHistType.Items.Objects[cbDwellTHistType.ItemIndex]) ;

     if rbAllEvents.checked then begin
        DwellTHist.StartAt := Round( edDwellEventRange.LoLimit ) ;
        DwellTHist.EndAt := Round( edDwellEventRange.HiLimit ) ;
        end
     else begin
        Settings.DwellTimes.EventRangeLo := Round(edDwellEventRange.LoValue) ;
        Settings.DwellTimes.EventRangeHi := Round(edDwellEventRange.HiValue) ;
        DwellTHist.StartAt := Settings.DwellTimes.EventRangeLo ;
        DwellTHist.EndAt := Settings.DwellTimes.EventRangeHi ;
        end ;

     DwellTHist.NumBins := Round( edNumBins.Value ) ;
     DwellTHist.NumLogBinsPerDecade := Round(edNumLogBinsPerDecade.Value) ;

     { Histogram bin range }
     if HistType = htOpeningsPerBurst then begin
        DwellTHist.RangeLo := 0.5 ;
        DwellTHist.RangeHi := DwellTHist.NumBins + 0.5 ;
        end
     else begin
        DwellTHist.RangeLo := edHistRange.LoValue ;
        DwellTHist.RangeHi := edHistRange.HiValue ;
        end ;

     if DwellTHist.RangeHi <= 0.0 then begin
        DwellTHist.RangeLo := 0.0  ;
        DwellTHist.RangeHi := CdrFH.dt*1000.0 ;
        ShowMessage( 'Histogram bin range must be > 0 ' ) ;
        end ;

    { Initialise histogram record }

    if rbLinear.Checked then begin
       { Linear histogram - fixed bin width }
       x := DwellTHist.RangeLo ;
       DwellTHist.MaxBin := DwellTHist.NumBins - 1 ;
       DwellTHist.BinWidth := (DwellTHist.RangeHi - DwellTHist.RangeLo) /
                              Max(DwellTHist.NumBins,1) ;
       for iBin := 0 to DwellTHist.MaxBin do begin
           DwellTHist.Bins[iBin].Lo := x ;
           DwellTHist.Bins[iBin].Hi := x + DwellTHist.BinWidth ;
           DwellTHist.Bins[iBin].Mid := x + (DwellTHist.BinWidth/2.0) ;
           DwellTHist.Bins[iBin].y := 0.0 ;
           x := x + DwellTHist.BinWidth
           end ;
       end
    else begin

       { logarithmically increasing bin width }
       DwellTHist.MaxBin := DwellTHist.NumBins - 1 ;
       DwellTHist.TMin := log10(CdrFH.dt)  ;
       DwellTHist.BinWidth := 1.0/DwellTHist.NumLogBinsPerDecade ;
       for iBin := 0 to DwellTHist.MaxBin do begin
            DwellTHist.Bins[iBin].Hi :=  ((iBin+1)*DwellTHist.BinWidth)
                                        + DwellTHist.TMin ;
            DwellTHist.Bins[iBin].Lo := DwellTHist.Bins[iBin].Hi - DwellTHist.BinWidth ;
            DwellTHist.Bins[iBin].Mid := DwellTHist.Bins[iBin].Hi
                                        - (DwellTHist.BinWidth/2.0) ;
            DwellTHist.Bins[iBin].y := 0.0 ;
            DwellTHist.Bins[iBin].Lo := Antilog10(DwellTHist.Bins[iBin].Lo) ;
            DwellTHist.Bins[iBin].Mid := Antilog10(DwellTHist.Bins[iBin].Mid) ;
            DwellTHist.Bins[iBin].Hi := Antilog10(DwellTHist.Bins[iBin].Hi) ;
            end ;
       end ;

    if DwellTHist.RangeLo <> DwellTHist.RangeHi then
       DwellTHist.BinScale := DwellTHist.NumBins
                             /(DwellTHist.RangeHi - DwellTHist.RangeLo)
    else DwellTHist.BinScale := 1.0 ;

     { Choose appropriate histogram computation procedure }
     case HistType of
          htClosedTimes : DwellTimeHistogram( csClosedState ) ;
          htOpenTimes : DwellTimeHistogram( csOpenState ) ;
          htBurstDurations : BurstDurationHistogram ;
          htSingleOpenTimes : SingleOpenTimesHistogram ;
          htBurstOpenTimes : OpenTimesWithinBurstHistogram ;
          htOpeningsPerBurst : OpeningsPerBurstHistogram ;
          htExternalTimes : ExternalDwellTimesList ;
          htExternalHist : ExternalDwellTimeHistogram ;
          end ;

    { Total number of events in histogram }
    DwellTHist.TotalCount := 0.0 ;
    for iBin := 0 to DwellTHist.MaxBin do
        DwellTHist.TotalCount := DwellTHist.TotalCount + DwellTHist.Bins[iBin].y ;

     DwellTHist.NewPlot := True ;
     { Remove any fitted equation }
     DwellTFunc.Setup( None, ' ', ' ' ) ;

     Screen.Cursor := crDefault ;
     { Re-enable buttons }
     bNewDwellTHist.Enabled := True ;

     if HistType = htOpeningsPerBurst then Scale := 1.0
                                      else Scale := Settings.TScale ;

     // Clear all existing lines on plot }
     plDwellTHist.ClearPlot ;

        { Plot new histogram }
     plDwellTHist.xAxisAutoRange := False ;
     plDwellTHist.xAxisMin := DwellTHist.Bins[0].Lo*Scale ;
     plDwellTHist.xAxisMax := DwellTHist.Bins[DwellTHist.MaxBin].Hi*Scale ;
     plDwellTHist.XAxisTick := plDwellTHist.TickSpacing( plDwellTHist.xAxisMax - plDwellTHist.xAxisMin) ;

     plDwellTHist.yAxisAutoRange := True ;
     if HistType = htOpeningsPerBurst then
        plDwellTHist.xAxisLabel := cbDwellTHistType.text
     else plDwellTHist.xAxisLabel := cbDwellTHistType.text + ' (' + Settings.TUnits + ')';
     plDwellTHist.yAxisLabel := 'No. Events' ;

     if rbLinear.checked then begin
        plDwellTHist.xAxisLaw := axLinear ;
        plDwellTHist.yAxisLaw := axLinear ;
        end
     else if rbLogLin.checked then begin
        plDwellTHist.xAxisLaw := axLog ;
        plDwellTHist.yAxisLaw := axSquareRoot ;
        end
     else begin
        plDwellTHist.xAxisLaw := axLog ;
        plDwellTHist.yAxisLaw := axLog ;
        end ;

     plDwellTHist.CreateHistogram( 0 ) ;
     for i := 0 to DwellTHist.MaxBin do plDwellTHist.AddBin( 0,
                                       DwellTHist.Bins[i].Lo*Scale,
                                       DwellTHist.Bins[i].Mid*Scale,
                                       DwellTHist.Bins[i].Hi*Scale,
                                       DwellTHist.Bins[i].y ) ;

     { Erase any fitted line which might exist }
     cbDwellTEqn.ItemIndex := 0 ;
     plDwellTHist.CreateLine( 1, clRed, msNone, psSolid ) ;

     // Clear results fields
     erDwellTResults.Clear ;
     DwellTResults.Clear ;

     { Initial cursor positions }
     plDwellTHist.VerticalCursors[DwellTCurs.C0] := DwellTHist.Bins[0].Mid*Scale ;
     i := DwellTHist.MaxBin ;
     while (DwellTHist.Bins[i].y = 0.0) and (i > 0) do Dec(i) ;
     plDwellTHist.VerticalCursors[DwellTCurs.C1] := DwellTHist.Bins[i].Mid*Scale ;
     plDwellTHist.VerticalCursors[DwellTCurs.Read] := DwellTHist.Bins[DwellTHist.MaxBin div 2].Mid
                                                 *Scale ;

     { Enable printer and copy menus }
     Main.CopyAndPrintMenus( True, True ) ;

     bDwellTSetAxes.Enabled := True ;
     bDwellTFitCurve.Enabled := True ;


     end;



procedure TSingleChanAnalFrm.DwellTimeHistogram(
          RequiredState : Integer           { State to be used in histogram }
          ) ;
{ -------------------------------------------
  Compute channel state dwell times histogram
  -------------------------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
begin

    { Read records of data from file and add to histogram }
    EventNum := DwellTHist.StartAt ;
    while EventNum <> DwellTHist.EndAt do begin
         { Get event data from file }
        ReadEventFromFile( EventFile, EventNum, Event ) ;
         { Add duration to histogram, if it is the correct type of state }
         if (Event.ChannelState = RequiredState) and (not Event.Ignore) then
            UpdateHistogram(Event.Duration, 1.0) ;
         { Next event }
         Inc(EventNum) ;
         end ;
    end ;


procedure TSingleChanAnalFrm.BurstDurationHistogram ;
{ -------------------------------------------
  Compute channel burst duration histogram
  -------------------------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
   BurstDuration : Double ;
   Done : Boolean ;
begin

    { Read records of data from file and add to histogram }
    Settings.DwellTimes.TCritical := edTCritical.Value ;
    BurstDuration := 0.0 ;
    EventNum := DwellTHist.StartAt ;
    Done := False ;

    while Not Done do begin

         { Read event data from file }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         { Process event if it not marked as ignored }
         if (not Event.Ignore) then begin
            if (Event.ChannelState = csClosedState) and
               (Event.Duration >= Settings.DwellTimes.TCritical) then begin
               UpdateHistogram( BurstDuration, 1.0 ) ;
               BurstDuration := 0.0 ;
               end
            else begin
               BurstDuration := BurstDuration + Event.Duration ;
               end ;
            end ;

         { Get next event }
          Inc(EventNum) ;
         if EventNum = DwellTHist.EndAt then Done := True ;
         end ;
    end ;


procedure TSingleChanAnalFrm.OpeningsPerBurstHistogram ;
{ -------------------------------------------
  Compute number of openings per burst histogram
  -------------------------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
   NumOpeningsPerBurst : Integer ;
   Done : Boolean ;
begin

    { Read records of data from file and add to histogram }
    Settings.DwellTimes.TCritical := edTCritical.Value ;
    NumOpeningsPerBurst := 0 ;
    EventNum := DwellTHist.StartAt ;
    Done := False ;

    while not Done do begin

         { Read event data from file }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         { Add to histogram if not marked as ignored }
         if not Event.Ignore then begin
            if (Event.ChannelState = csClosedState) and
               (Event.Duration >= Settings.DwellTimes.TCritical) then begin
               if NumOpeningsPerBurst > 0.0 then
                  DwellTHist.Bins[NumOpeningsPerBurst-1].y :=
                      DwellTHist.Bins[NumOpeningsPerBurst-1].y + 1.0 ;
                  NumOpeningsPerBurst := 0 ;
               end
            else if Event.ChannelState <> csClosedState then begin
               Inc(NumOpeningsPerBurst) ;
               end ;
            end ;

         { Get next event }
          Inc(EventNum) ;
         if EventNum = DwellTHist.EndAt then Done := True ;
         end ;
    end ;



procedure TSingleChanAnalFrm.OpenTimesWithinBurstHistogram ;
{ -------------------------------------------
  Compute open times within a burst
  -------------------------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
   NumOpeningsPerBurst : Integer ;
   LastOpenTime : Double ;
   Done : Boolean ;
begin

    { Read records of data from file and add to histogram }
    Settings.DwellTimes.TCritical := edTCritical.Value ;
    NumOpeningsPerBurst := 0 ;
    LastOpenTime := 0.0 ;
    EventNum := DwellTHist.StartAt ;
    Done := False ;

    while not Done do begin

         { Read event data from file }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         { Add to histogram if not marked as ignored }
         if not Event.Ignore then begin
            if (Event.ChannelState = csClosedState) and
               (Event.Duration >= Settings.DwellTimes.TCritical) then begin
               { End of burst. Remove last open time from histogram if the
                 burst only contained a single opening }
               if NumOpeningsPerBurst = 1 then UpdateHistogram(LastOpenTime,-1.0) ;
               NumOpeningsPerBurst := 0 ;
               end
            else if Event.ChannelState <> csClosedState then begin
               { Add open time to histogram }
               UpdateHistogram( Event.Duration, 1.0 ) ;
               LastOpenTime := Event.Duration ;
               Inc(NumOpeningsPerBurst) ;
               end ;
            end ;

          Inc(EventNum) ;
         if EventNum = DwellTHist.EndAt then Done := True ;
         end ;

    end ;


procedure TSingleChanAnalFrm.SingleOpenTimesHistogram ;
{ --------------------------
  Compute single open times
  --------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
   NumOpeningsPerBurst : Integer ;
   LastOpenTime : Double ;
   Done : Boolean ;
begin

    { Read records of data from file and add to histogram }
    Settings.DwellTimes.TCritical := edTCritical.Value ;
    NumOpeningsPerBurst := 0 ;
    EventNum := DwellTHist.StartAt ;
    Done := False ;

    while not Done do begin

         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         { Add to histogram if not marked as ignored }
         if not Event.Ignore then begin
            if (Event.ChannelState = csClosedState) and
               (Event.Duration >= Settings.DwellTimes.TCritical) then begin
               { End of burst. Remove last open time from histogram if the
                 burst only contained a single opening }
               if NumOpeningsPerBurst = 1 then UpdateHistogram(LastOpenTime,1.0) ;
               NumOpeningsPerBurst := 0 ;
               end
            else if Event.ChannelState <> csClosedState then begin
               LastOpenTime := Event.Duration ;
               Inc(NumOpeningsPerBurst) ;
               end ;
            end ;

          Inc(EventNum) ;
         if EventNum = DwellTHist.EndAt then Done := True ;
         end ;
    end ;


procedure TSingleChanAnalFrm.ExternalDwellTimesList ;
{ -----------------------------------------------------
  Compute histogram from list of times in external file
  ----------------------------------------------------- }
var
   Time : double ;
   InFile : TextFile ;
begin

    { Read records of data from file and add to histogram }
     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     OpenDialog.Title := 'Open dwell times file' ;

     if OpenDialog.execute then begin
        AssignFile( InFile, OpenDialog.FileName ) ;
        Reset( InFile ) ;
        while not EOF( InFile ) do begin
            { Get event data from file }
            ReadLn( InFile, Time ) ;
            { Add duration to histogram, if it is the correct type of state }
            UpdateHistogram( Time, 1.0) ;
            end ;
        CloseFile( InFile ) ;
        end ;
    end ;


procedure TSingleChanAnalFrm.ExternalDwellTimeHistogram ;
{ -----------------------------------------------------
  Compute histogram from list of times in external file
  ----------------------------------------------------- }
var
   iBin : Integer ;
   Bin0,Bin1 : single ;
   InFile : TextFile ;
begin

    { Read records of data from file and add to histogram }
     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     OpenDialog.Title := 'Open dwell time histogram file' ;

     if OpenDialog.execute then begin
        AssignFile( InFile, OpenDialog.FileName ) ;
        Reset( InFile ) ;
        iBin := 0 ;
        while not EOF( InFile ) do begin
            { Get event data from file }
            ReadLn( InFile, DwellTHist.Bins[iBin].Lo,
                            DwellTHist.Bins[iBin].Mid,
                            DwellTHist.Bins[iBin].Hi,
                            DwellTHist.Bins[iBin].y ) ;
            Inc(iBin) ;
            end ;
        DwellTHist.NumBins := iBin ;
        DwellTHist.MaxBin := DwellTHist.NumBins - 1 ;

        // No. of bins in histogram
        edNumBins.Value :=  DwellTHist.NumBins ;



        // Determine whether this is a logarithmic histogram
        Bin0 := DwellTHist.Bins[0].Hi - DwellTHist.Bins[0].Lo ;
        Bin1 := DwellTHist.Bins[DwellTHist.MaxBin].Hi - DwellTHist.Bins[DwellTHist.MaxBin].Lo ;
        if Abs(Bin1/Bin0) > 2.0 then begin
           rbLogLin.Checked := True ;
           nbLinLogSettings.PageIndex := 1 ;
           iBin := 0 ;
           Bin0 := DwellTHist.Bins[0].Mid ;
           while (DwellTHist.Bins[iBin].Mid <= (10.0*Bin0)) and
                 ( iBin < DwellTHist.MaxBin ) do Inc(iBin) ;
           DwellTHist.NumLogBinsPerDecade := iBin - 1 ;
           edNumLogBinsPerDecade.Value := DwellTHist.NumLogBinsPerDecade ;
           end
        else begin
           rbLinear.Checked := True ;
           nbLinLogSettings.PageIndex := 0 ;
           DwellTHist.RangeLo := DwellTHist.Bins[0].Lo ;
           DwellTHist.RangeHi := DwellTHist.Bins[DwellTHist.MaxBin].Hi ;
           edHistRange.LoValue := DwellTHist.RangeLo ;
           edHistRange.HiValue := DwellTHist.RangeHi ;
           end ;
        CloseFile( InFile ) ;

        end ;
    end ;



procedure TSingleChanAnalFrm.UpdateHistogram(
          Time : Double ;
          Value : Single
          ) ;
{ --------------------------------------
  Add event to appropriate histogram bin
  --------------------------------------}
var
   iBin : Integer ;
begin
     if rbLinear.Checked then begin
        { Linear fixed width binning }
        iBin := Round( (Time - DwellTHist.RangeLo)*DwellTHist.BinScale ) ;
        end
     else begin
        { Logarithmic binning }
        if Time > 0.0 then
           iBin := Round(DwellTHist.NumLogBinsPerDecade*(log10(Time) - DwellTHist.TMin))
        else iBin := 0 ;
        end ;

     iBin := IntLimitTo( iBin, 0, DwellTHist.MaxBin ) ;

     { Increment bin count }
     DwellTHist.Bins[iBin].y := DwellTHist.Bins[iBin].y + Value ;
     end ;


procedure TSingleChanAnalFrm.bDwellTSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plDwellTHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TSingleChanAnalFrm.bDwellTFitCurveClick(Sender: TObject);
{ ----------------------------------------------------
  Fit exponential prob. density functions to histogram
  ----------------------------------------------------
    }
const
     NumFitPoints = 500 ;
var
   i,iStart,iEnd,nFit,iBins,Comp,LineNum,NumComp : Integer ;
   x,y,BinWidth,TScale : single ;
   ParTemp,DisplayPars,DisplayParsSD : Array[0..LastParameter] of single ;
   ParUnits : Array[0..LastParameter] of String ;
   FitData : PXYData ;
   OK : Boolean ;
begin
     OK := True ;
     New( FitData ) ;

     Try

        { Clear all existing lines on plot }
        plDwellTHist.ClearAllLines ;

        { Select type of equation to be fitted }
        DwellTFunc.Setup( TEqnType(cbDwellTEqn.Items.Objects[cbDwellTEqn.ItemIndex]),
                          Settings.TUnits,
                          '%')  ;
        if DwellTFunc.Equation = None then OK := False ;

        { Copy data into fitting array }
        nFit := 0 ;
        if OK then begin
           { Lower and upper x data limit set by display cursors }
           iStart := plDwellTHist.FindNearestIndex( 0, DwellTCurs.C0 ) ;
           iEnd :=   plDwellTHist.FindNearestIndex( 0, DwellTCurs.C1 ) ;
           for iBins := Min(iStart,iEnd) to Max(iStart,iEnd) do begin
               FitData^.x[nFit] := DwellTHist.Bins[iBins].Mid ;
               FitData^.BinWidth[nFit] := (DwellTHist.Bins[iBins].Hi
                                           - DwellTHist.Bins[iBins].Lo) ;
               FitData^.y[nFit] := DwellTHist.Bins[iBins].y
                                   / (DwellTHist.TotalCount*0.01) ;
               Inc(nFit) ;
               end ;

           { Abort curve fit, if not enough data points }
           if nFit < DwellTFunc.NumParameters then begin
              ShowMessage( format('%d points is insufficient for fit',[nFit])) ;
              DwellTFunc.Setup( None, ' ',' ' ) ;
              OK := False ;
              end ;
           end ;

        if OK then begin
           { Let user create/modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := DwellTFunc ;
           SetFitParsFrm.XYData := FitData ;
           SetFitParsFrm.NumPoints := nFit ;
           SetFitParsFrm.Left := SingleChanAnalFrm.Left + Main.Left + 50 ;
           SetFitParsFrm.Top := SingleChanAnalFrm.Top + Main.Top + 50 ;
           SetFitParsFrm.ShowModal ;
           if SetFitParsFrm.ModalResult <> mrOK then OK := False ;
           end ;

        { Fit curve using non-linear regression }
        if OK then begin
           { Prevent FitCurve from changing parameter settings }
           DwellTFunc := SetFitParsFrm.MathFunc ;
           DwellTFunc.ParametersSet := True ;
           { NOTE Use histogram bins widths to calculate p.d.f. in fit }
           DwellTFunc.UseBinWidths := True ;
           DwellTFunc.FitCurve( FitData^, nFit ) ;
           OK := DwellTFunc.GoodFit ;
           if not OK then ShowMessage( 'Curve fit failed to converge!' ) ;
           end ;

        { Plot equation on graph }

        if TDwellTHistType(cbDwellTHistType.Items.Objects[cbDwellTHistType.ItemIndex])
           = htOpeningsPerBurst then TScale := 1.0
                                else TScale := Settings.TScale ;

        plDwellTHist.CreateLine( FittedLine, clRed, msNone, psSolid ) ;
        if OK and (DwellTFunc.Equation <> None) then begin
           plDwellTHist.ShowLines := True ;
           for i := 0 to DwellTHist.NumBins-1 do begin
               BinWidth := (DwellTHist.Bins[i].Hi - DwellTHist.Bins[i].Lo)
                            * DwellTHist.TotalCount*0.01 ;
               x := DwellTHist.Bins[i].Mid*TScale ;
               y := DwellTFunc.Value(DwellTHist.Bins[i].Mid)*BinWidth ;
               plDwellTHist.AddPoint( 1, x, y ) ;
               end ;

           { Save parameters and initialise exponential component lines }
           NumComp := DwellTFunc.NumParameters div 2 ;
           LineNum := FittedLine ;
           for Comp := 0 to NumComp-1 do begin
               Inc(LineNum) ;
               plDwellTHist.CreateLine( LineNum, clRed, msNone, psSolid ) ;
               ParTemp[Comp*2] := DwellTFunc.Parameters[Comp*2] ;
               ParTemp[Comp*2+1] := DwellTFunc.Parameters[Comp*2+1] ;
               DwellTFunc.Parameters[Comp*2] := 0.0 ;
               end ;

           { Plot each individual exponential component }
           if NumComp > 1 then begin
              LineNum := FittedLine ;
              for Comp := 0 to NumComp-1 do begin
                  DwellTFunc.Parameters[Comp*2] := ParTemp[Comp*2] ;
                  Inc(LineNum) ;
                  for i := 0 to DwellTHist.NumBins-1 do begin
                      BinWidth := (DwellTHist.Bins[i].Hi - DwellTHist.Bins[i].Lo)
                                  * DwellTHist.TotalCount*0.01 ;
                      x := DwellTHist.Bins[i].Mid*TScale ;
                      y := DwellTFunc.Value(DwellTHist.Bins[i].Mid)*BinWidth ;
                      plDwellTHist.AddPoint( LineNum, x, y ) ;
                      end ;
                  DwellTFunc.Parameters[Comp*2] := 0.0 ;
                  end ;
              end ;

           { Restore parameters }
           for Comp := 0 to NumComp-1 do begin
               DwellTFunc.Parameters[Comp*2] := ParTemp[Comp*2] ;
               DwellTFunc.Parameters[Comp*2+1] := ParTemp[Comp*2+1] ;
               end ;
           end ;

        { Display results }
        DwellTResults.Clear ;
        if OK then begin

           DwellTResults.Add( DwellTFunc.Name ) ;
           { Best fit parameters and standard error }
           for i := 0 to DwellTFunc.NumParameters-1 do begin

               { Convert time parameters to appropriate time units }
               if (DwellTFunc.ParUnits[i] = 'ms') or
                  (DwellTFunc.ParUnits[i] = 's') then begin
                  DisplayPars[i] := DwellTFunc.Parameters[i]*TScale ;
                  DisplayParsSD[i] := DwellTFunc.ParameterSDs[i]*TScale ;
                  end
               else begin
                  DisplayPars[i] := DwellTFunc.Parameters[i] ;
                  DisplayParsSD[i] := DwellTFunc.ParameterSDs[i] ;
                  end ;
               ParUnits[i] := DwellTFunc.ParUnits[i] ;

               if not DwellTFunc.FixedParameters[i] then
                  DwellTResults.Add( format(' %s = %.4g ^~ %.4g (sd) %s',
                                       [DwellTFunc.ParNames[i],
                                        DisplayPars[i],
                                        DisplayParsSD[i],
                                        DwellTFunc.ParUnits[i]] ))
               else
                  DwellTResults.Add( format(' %s = %.4g (fixed) %s',
                                       [DwellTFunc.ParNames[i],
                                        DisplayPars[i],
                                        DwellTFunc.ParUnits[i]] )) ;
               end ;

           { Residual standard deviation }
           DwellTResults.Add( format(' Residual S.D. = %.4g %s',
                                [DwellTFunc.ResidualSD,'%'] )) ;

           //{ Hamilton's R }
           //DwellTResults.Add( format(' Hamilton''s R = %.4g',
           //                     [DwellTFunc.R] )) ;

           { Statistical degrees of freedom }
           DwellTResults.Add( format(' Degrees of freedom = %d ',
                                [DwellTFunc.DegreesOfFreedom])) ;

           { No. of iterations }
           DwellTResults.Add( format(' No. of iterations = %d ',
                                [DwellTFunc.Iterations])) ;

           DwellTFunc.CopyResultsToRichEdit( DwellTResults, erDwellTResults ) ;

          FTestFrm.AddResult( DwellTFunc.NumParameters div 2,
                               DwellTFunc.ResidualSD,
                               DwellTFunc.NumParameters,
                               DwellTFunc.DegreesOfFreedom,
                               DisplayPars,
                               ParUnits ) ;
           end ;

     finally
            Dispose(FitData) ;
            end ;

     { Make sure plot is updated with changes }
     plDwellTHist.Invalidate ;

     end ;


procedure TSingleChanAnalFrm.PrintDisplay ;
{ ------------------------------------
  Print record or histogram on display
  ------------------------------------ }
var
   i : Integer ;
begin

    // Print amplitude histogram
    if Page.ActivePage = AmpHistTab then begin
       PrintGraphFrm.Plot := plAmpHist ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          { Add title information to plot }
          plAmpHist.ClearPrinterTitle ;
          plAmpHist.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plAmpHist.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          plAmpHist.AddPrinterTitleLine( 'Histogram : ' + cbAmpHistType.text ) ;
          for i := 0 to AmpResults.Count-1 do
              plAmpHist.AddPrinterTitleLine( AmpResults[i] ) ;
          { Plot graph to printer }
          plAmpHist.Print ;
          end ;
       end ;


    if Page.ActivePage = DwellTHistTab then begin
       { Print histogram }
       PrintGraphFrm.Plot := plDwellTHist ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          { Add title information to plot }
          plDwellTHist.ClearPrinterTitle ;
          plDwellTHist.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plDwellTHist.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          plDwellTHist.AddPrinterTitleLine( 'Histogram : ' + cbDwellTHistType.text ) ;
          for i := 0 to DwellTResults.Count-1 do
              plDwellTHist.AddPrinterTitleLine( DwellTResults[i] ) ;
          { Plot graph to printer }
          plDwellTHist.Print ;
          end ;
       end

    else if Page.ActivePage = StabPlotTab then begin
       if sgSummary.Visible then begin
          // Print summary table
          PrintStringGrid( sgSummary ) ;
          end
       else begin
          { Print stability plot }
          PrintGraphFrm.Plot := plStabPlot ;
          PrintGraphFrm.ToPrinter := True ;
          PrintGraphFrm.ShowModal ;
          if PrintGraphFrm.ModalResult = mrOK then begin
             { Add title information to plot }
             plStabPlot.ClearPrinterTitle ;
             plStabPlot.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
             plStabPlot.AddPrinterTitleLine( CdrFH.IdentLine ) ;
             plDwellTHist.AddPrinterTitleLine( 'Plot : ' + cbStabPlotType.text ) ;
             { Plot graph to printer }
             plStabPlot.Print ;
             end ;
          end ;
       end

    else if Page.ActivePage = EditTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := dePrinter ;
       PrintRecFrm.Display := scEditDisplay ;

       PrintRecFrm.ShowModal ;

       if PrintRecFrm.ModalResult = mrOK then begin
          PrintRecFrm.Display.ClearPrinterTitle ;
          PrintRecFrm.Display.AddPrinterTitleLine(
                                                'File : ' + cdrFH.FileName ) ;
          PrintRecFrm.Display.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          PrintRecFrm.Display.Print ;
          end ;
       end ;

    end ;


procedure TSingleChanAnalFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy histogram image to clipboard as Windows metafile
  ----------------------------------------------------- }
begin

    // Copy amplitude histogram to clipboard
    if Page.ActivePage = AmpHistTab then begin
       PrintGraphFrm.Plot := plAmpHist ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plAmpHist.CopyImageToClipboard ;
       end ;

    // Copy dwell-time histogram
    if Page.ActivePage = DwellTHistTab then begin
       PrintGraphFrm.Plot := plDwellTHist ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plDwellTHist.CopyImageToClipboard ;
       end ;

    // Copy stability plot
    if Page.ActivePage = StabPlotTab then begin
       PrintGraphFrm.Plot := plStabPlot ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plStabPlot.CopyImageToClipboard ;
       end ;

    if Page.ActivePage = EditTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := deClipboard ;
       PrintRecFrm.Display := scEditDisplay ;
       PrintRecFrm.ShowModal ;
       if PrintRecFrm.ModalResult = mrOK then begin
          PrintRecFrm.Display.CopyImageToClipboard ;
          end ;
       end ;

    if Page.ActivePage = DetectTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := deClipboard ;
       PrintRecFrm.Display := scDetDisplay ;
       PrintRecFrm.ShowModal ;
       if PrintRecFrm.ModalResult = mrOK then begin
          PrintRecFrm.Display.CopyImageToClipboard ;
          end ;
       end ;


     end ;


procedure TSingleChanAnalFrm.CopyDataToClipboard ;
{ -----------------------------------------------------
  Copy data to clipboard as table of Tab text
  ----------------------------------------------------- }
begin

    if Page.ActivePage = AmpHistTab then plAmpHist.CopyDataToClipboard ;
    if Page.ActivePage = DwellTHistTab then plDwellTHist.CopyDataToClipboard ;
    if Page.ActivePage = StabPlotTab then begin
       if sgSummary.Visible then CopyStringGrid( sgSummary, False )
                            else plStabPlot.CopyDataToClipboard ;
       end ;
    if Page.ActivePage = EditTransitionsTab then scEditDisplay.CopyDataToClipboard ;
    if Page.ActivePage = DetectTransitionsTab then scDetDisplay.CopyDataToClipboard ;
    end ;


procedure TSingleChanAnalFrm.cbDwellTHistTypeChange(Sender: TObject);
// -------------------------------------
// Type of dwell time histogram changed
// -------------------------------------
begin
    { Make Tcritical field visible when needed }
     if (TDwellTHistType(
        cbDwellTHistType.Items.Objects[cbDwellTHistType.ItemIndex])
        in [htOpenTimes,htClosedTimes]) then begin
        lbTCritical.Visible := False ;
        edTCritical.Visible := False ;
        end
     else begin
        lbTCritical.Visible := True ;
        edTCritical.Visible := True ;
        end ;

     end;



procedure TSingleChanAnalFrm.scDetDisplayCursorChange(Sender: TObject);
{ -------------------------------------------
  Update fields when display cursors change
  -------------------------------------------}
var
     OldValue : Integer ;
begin

     edDetUnitCurrent.Value := (scDetDisplay.HorizontalCursors[DetCurs.IUnit]
                               - scDetDisplay.HorizontalCursors[DetCurs.Base])
                               * Channel[ChanNum].ADCScale ;
     Settings.DwellTimes.UnitCurrent := edDetUnitCurrent.Value ;

     if Settings.DwellTimes.UnitCurrent <> 0.0 then
        edThreshold.Value :=   (scDetDisplay.HorizontalCursors[DetCurs.Threshold]
                                - scDetDisplay.HorizontalCursors[DetCurs.Base])
                                * Channel[ChanNum].ADCScale / Settings.DwellTimes.UnitCurrent
     else edThreshold.Value := 0.0 ;
     Settings.DwellTimes.Threshold := edThreshold.Value ;

     OldValue := Channel[ChanNum].ADCZero ;
     Channel[ChanNum].ADCZero := scDetDisplay.HorizontalCursors[DetCurs.Base] ;
     // Update header and view module
     if OldValue <> Channel[ChanNum].ADCZero then begin
        SaveCDRHeader( CDRFH ) ;
        Main.UpdateViewSig ;
        end ;

     { Set position of trend removal selection block labels }

     SelectionCursor0 := scDetDisplay.xOffset + scDetDisplay.VerticalCursors[DetCurs.C0] ;
     SelectionCursor1 := scDetDisplay.xOffset + scDetDisplay.VerticalCursors[DetCurs.C1];

     { Update vertical display magnification so that changes are retained }
     Channel[ChanNum].yMin := scDetDisplay.YMin[ChanNum] ;
     Channel[ChanNum].yMax := scDetDisplay.YMax[ChanNum] ;

     end ;


procedure TSingleChanAnalFrm.PageChange(Sender: TObject);
{ ----------------------------------------------
  Set up controls on pages when tab page changes
  ---------------------------------------------- }
var
    IUnit : Single ;
begin

     // Amplitude histogram page
     if Page.ActivePage = AmpHistTab then begin
        IUnit := Settings.DwellTimes.UnitCurrent ;
        Settings.DwellTimes.Threshold := edThreshold.Value ;
        if plAmpHist.Available then begin
           AdjustAmpHistZeroLevel( Channel[ChanNum].ADCZero ) ;
           plAmpHist.VerticalCursors[AmpCurs.IUnit] := IUnit  ;
           Settings.DwellTimes.UnitCurrent := IUnit ;
           edAHUnitCurrent.Value :=  IUnit ;
           plAmpHist.Invalidate ;
           end ;
        end ;

     // Transition detection page
     if Page.ActivePage = DetectTransitionsTab then begin

        edDetRange.LoValue := Settings.DwellTimes.SampleRangeLo ;
        edDetRange.HiValue := Settings.DwellTimes.SampleRangeHi ;
        edDetUnitCurrent.Value := Settings.DwellTimes.UnitCurrent ;
        edThreshold.Value := Settings.DwellTimes.Threshold ;

        if EventFile.NumEvents > 0 then EditTransitionsTab.Enabled := True
                                   else EditTransitionsTab.Enabled := False ;

        Resize ;

        DisplayRecord ;
        end ;

      // Event editing page
      if Page.ActivePage = EditTransitionsTab then begin

         if EventFile.NumEvents > 0 then begin
            if sbEvent.Max <> EventFile.NumEvents then begin
               sbEvent.Max := EventFile.NumEvents ;
               sbEvent.Min := 1 ;
               sbEvent.Position := 1 ;
               end ;
            sbEvent.Enabled := True ;
            EditTransitionsTab.Enabled := True ;
            scEditDisplay.Visible := True ;
            EditDisplayWidthPanel.Visible := True ;
            end
         else begin
            sbEvent.Min := 1 ;
            sbEvent.Position := 1 ;
            sbEvent.Max := 1 ;
            sbEvent.Enabled := False ;
            EditTransitionsTab.Enabled := False ;
            scEditDisplay.Visible := False ;
            EditDisplayWidthPanel.Visible := False ;
            end ;

         Resize ;
         DisplayEvent ;

         end ;

      // Dwell time histogram page
      if Page.ActivePage = DwellTHistTab then begin
         Resize ;

         edDwellEventRange.LoLimit := 1 ;
         edDwellEventRange.HiLimit := EventFile.NumEvents ;
         edDwellEventRange.LoValue := Settings.DwellTimes.EventRangeLo ;
         edDwellEventRange.HiValue := Settings.DwellTimes.EventRangeHi ;
         if EventFile.NumEvents > 0 then bNewDwellTHist.Enabled := True 
                                    else bNewDwellTHist.Enabled := False ;

         if rbLinear.checked then nbLinLogSettings.PageIndex := 0
                             else nbLinLogSettings.PageIndex := 1 ;

         { Add/remove T.critical edit box depending on type of histogram selected }
         if (TDwellTHistType(
            cbDwellTHistType.Items.Objects[cbDwellTHistType.ItemIndex])
            in [htOpenTimes,htClosedTimes]) then begin
            lbTCritical.Visible := False ;
            edTCritical.Visible := False ;
            end
         else begin
            lbTCritical.Visible := True ;
            edTCritical.Visible := True ;
            end ;

         end ;

      // Stability plot page
      if Page.ActivePage = StabPlotTab then begin
         Resize ;
         NewStabPlotType ;
         end ;

     { Enable/disable print and copy menu items as necessary }
     SetCopyAndPrintMenus ;
     end;


procedure TSingleChanAnalFrm.plDwellTHistCursorChange(Sender: TObject);
{ -------------------------------------------
  Update labels when histogram cursors change
  -------------------------------------------}
var
   Lo,Mid,Hi,y : single ;
   iStart,iEnd,i : Integer ;
   XMean,XYSum,YSum,TScale : single ;
   TUnits : string ;
begin

     if TDwellTHistType(cbDwellTHistType.Items.Objects[cbDwellTHistType.ItemIndex])
        = htOpeningsPerBurst then begin
        TScale := 1.0 ;
        TUnits := '' ;
        end
     else begin
        TScale := Settings.TScale ;
        TUnits := Settings.TUnits ;
        end ;

     { Set Fitting/area cursor labels }
     plDwellTHist.GetBin( 0, plDwellTHist.FindNearestIndex(0,DwellTCurs.C0),
                          Lo, Mid, Hi, y ) ;
     lbDwellTC0.Visible := True ;
     lbDwellTC0.Left := plDwellTHist.Left + plDwellTHist.XToCanvasCoord( Mid ) ;
     plDwellTHist.GetBin( 0, plDwellTHist.FindNearestIndex(0,DwellTCurs.C1),
                          Lo, Mid, Hi, y ) ;
     lbDwellTC1.Visible := True ;
     lbDwellTC1.Left := plDwellTHist.Left + plDwellTHist.XToCanvasCoord( Mid ) ;

     { Calculate area between cursors }
     iStart := plDwellTHist.FindNearestIndex( 0, DwellTCurs.C0 ) ;
     iEnd :=   plDwellTHist.FindNearestIndex( 0, DwellTCurs.C1 ) ;
     XYSum := 0.0 ;
     YSum := 0.0 ;
     for i := Min(iStart,iEnd) to Max(iStart,iEnd) do begin
         XYSum := XYSum + (DwellTHist.Bins[i].y*DwellTHist.Bins[i].Mid) ;
         YSum := YSum + DwellTHist.Bins[i].y ;
         end ;
     if XYSum <> 0.0 then XMean := XYSum / YSum
                     else XMean := 0.0 ;

     lbDwellTArea.visible := true ;
     shline.visible := true ;
     lbDwellTArea.caption := format(' Mean= %.3g %s /Events= %d ',
                                [XMean*TScale,TUnits,
                                Round(YSum)] ) ;

     { Display mean signal level and histogram % between cursors }
     Mid := (lbDwellTC0.Left + lbDwellTC1.Left) div 2 ;
     lbDwellTArea.Left := ((lbDwellTC0.Left + lbDwellTC1.Left) div 2)
                          - (lbDwellTArea.Width div 2) ;
     lbDwellTArea.Top := lbDwellTC0.Top ;
     lbDwellTArea.Visible := True ;

     { Place horizontal line between fit/analysis cursors }
     shDwellTLine.Top := lbDwellTArea.Top + (lbDwellTArea.Height div 2) ;
     shDwellTLine.Left := Min(lbDwellTC0.Left,lbDwellTC1.Left)
                    + lbDwellTC0.Width ;
     shDwellTLine.Width := Max(lbDwellTC0.Left,lbDwellTC1.Left)
                     - shDwellTLine.Left - lbDwellTC0.Width ;
     shDwellTLine.Visible := True ;

     end ;


procedure TSingleChanAnalFrm.rbLinearClick(Sender: TObject);
begin
     nbLinLogSettings.PageIndex := 0
     end;

procedure TSingleChanAnalFrm.rbLogLinClick(Sender: TObject);
begin
     nbLinLogSettings.PageIndex := 1
     end;


procedure TSingleChanAnalFrm.SetCopyAndPrintMenus ;
{ ------------------------------------------------
  Enable Copy and Print menus if data is available
  ------------------------------------------------}
begin
     if Page.ActivePage = DwellTHistTab then begin
        if plDwellTHist.Available then Main.CopyAndPrintMenus( True, True )
                            else Main.CopyAndPrintMenus( False, False ) ;
        end
     else if Page.ActivePage = EditTransitionsTab then begin
        if EditTransitionsTab.Enabled then begin
           Main.CopyAndPrintMenus( True, True ) ;
           end
        else begin
           Main.CopyAndPrintMenus( False, False ) ;
           end ;
        end
     else begin
        Main.CopyAndPrintMenus( True, True ) ;
        end ;
     end ;


procedure TSingleChanAnalFrm.FormActivate(Sender: TObject);
begin

     SetCopyAndPrintMenus ;
     end;


{ *** BASELINE TREND REMOVAL PROCEDURES ********************************* }


procedure TSingleChanAnalFrm.bAddBlockClick(Sender: TObject);
{ --------------------------------------------------
  Add another baseline block to trend line data list
  --------------------------------------------------}
const
   BufSize = 512 ; // Max. no. blocks per buffer
var
   i,j,iStart,iEnd : Integer ;
   Sum : Double ;   // Summation value
   nSum : Integer ; // No. points added to Sum
   Done : Boolean ; // Loop termination flag
   NumBlocksPerBuffer : Integer ; // No. of sample blocks in buffer
   ADC : Array[0..BufSize*MaxChannels-1] of SmallInt ;
begin

     { Get start/end of block of data points containing baseline level }
     iStart := Min(SelectionCursor0,SelectionCursor1 ) ;
     iEnd := Max( SelectionCursor0,SelectionCursor1 ) ;

     Sum := 0.0 ;
     nSum := 0 ;
     Done := False ;
     while not Done do begin

          // Read A/D samples from file
          NumBlocksPerBuffer := Min(iEnd - iStart + 1,BufSize) ;
          ReadCDRBuffer(CdrFH,iStart,ADC,NumBlocksPerBuffer) ;

          // Add to sum
          j := Channel[cbDetChannel.ItemIndex].ChannelOffset ;
          for i := 0 to NumBlocksPerBuffer-1 do begin
              Sum := Sum + ADC[j] ;
              j := j + CDRFH.NumChannels ;
              Inc(nSum) ;
              end ;
          iStart := iStart + NumBlocksPerBuffer ;
          if iStart >= iEnd then Done := True ;
          end ;

     { Add to current set of baseline blocks }
     if TrendLine.NumPoints < High(TrendLine.y) then begin
        TrendLine.y[TrendLine.NumPoints] := Sum / nSum ;
        TrendLine.x[TrendLine.NumPoints] := (SelectionCursor0 + SelectionCursor1)*0.5 ;
        Inc(TrendLine.NumPoints) ;
        edNumBlocks.Value := TrendLine.NumPoints ;
        Main.StatusBar.SimpleText := format(
        ' Single-channel Analysis : Trend Removal block added (%d)',[TrendLine.NumPoints]);
        end
     else bAddBlock.Enabled := False ;

     { Do a fit if enough points are available }
     if TrendLine.NumPoints >= TrendLine.Func.NumParameters then FitTrendLine ;
     DisplayRecord ;
     end ;


procedure TSingleChanAnalFrm.FitTrendLine ;
{ -------------------------------------------
  Fit a line to slow trends in baseline level
  -------------------------------------------}
var
   i : Integer ;
   FitData : ^TXYData ;
begin

     New( FitData ) ;

     Try
        { Select type of equation to be fitted (linear,quadratic,cubic) }
        TrendLine.Func.Setup( TEqnType(
                              cbTrendEquation.Items.Objects[cbTrendEquation.ItemIndex]),
                              Settings.TUnits,
                              Channel[ChanNum].ADCUnits)  ;
        { Copy data into fitting buffer }
        for i := 0 to TrendLine.NumPoints-1 do begin
           FitData^.x[i] := TrendLine.x[i] ;
           FitData^.y[i] := TrendLine.y[i] ;
           end ;

        for i := 0 to TrendLine.Func.NumParameters-1 do
            TrendLine.Func.Parameters[i] := TrendLine.Func.InitialGuess(FitData^,
                                            TrendLine.NumPoints,i) ;

        TrendLine.Func.FitCurve( FitData^, TrendLine.NumPoints ) ;
        TrendLine.Available := TrendLine.Func.GoodFit ;
        if TrendLine.Available then begin
           bRemoveTrend.Enabled := True ;
           scDetDisplay.Invalidate ;
           end
        else bRemoveTrend.Enabled := False ;
     finally
        Dispose(FitData) ;
        end ;

     end;

procedure TSingleChanAnalFrm.bClearSamplesClick(Sender: TObject);
{ --------------------------
  Reset trend line data list
  --------------------------}
begin
     TrendLine.NumPoints := 0 ;
     edNumBlocks.Value := TrendLine.NumPoints ;
     TrendLine.Available := False ;
     bRemoveTrend.Enabled := False ;
     bAddBlock.Enabled := True ;
     DisplayRecord ;
     end;


procedure TSingleChanAnalFrm.cbTrendEquationChange(Sender: TObject);
{ ----------------------------------
  Change trend line fitting function
  ---------------------------------- }
begin
     { Set new equation in math function object }
     TrendLine.Func.Setup( TEqnType(
                           cbTrendEquation.Items.Objects[cbTrendEquation.ItemIndex]),
                           Settings.TUnits,
                           Channel[ChanNum].ADCUnits)  ;

     { Do a fit if enough points are available }
     if TrendLine.NumPoints >= TrendLine.Func.NumParameters then begin
        FitTrendLine ;
        end
     else begin
        { Otherwise disable trend line on display }
        TrendLine.Available := False ;
        bRemoveTrend.Enabled := False ;
        end;

     { Re-display record }
     DisplayRecord ;
     end;

procedure TSingleChanAnalFrm.bRemoveTrendClick(Sender: TObject);
{ ---------------------------------------------------
  Subtract baseline trend from selected data region
  -------------------------------------------------- }
const
     NumBlocksPerBuffer = 256 ;
var
   NumSamplesPerBuffer, BufferStart, iPointer : Integer ;
   Done,NewBufferNeeded : Boolean ;
   ADC : Array[0..NumBlocksPerBuffer*MaxChannels-1] of SmallInt ;
begin
     { Make a back up copy of original data file if one doesn't already exist }
     Main.StatusBar.SimpleText :=
     ' Single-channel Analysis : Making back-up of original data file' ;
     MakeBackupFile ;

     bRestoreOriginal.Enabled := CdrFH.BackedUp and
                                 FileExists(ChangeFileExt(CdrFH.FileName,'.bak')) ;

     { Range of samples to have trend subtracted from them }
     if rbAllRecords.Checked then begin
        Det.StartAtSample := 0 ;
        Det.EndAtSample := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        Det.StartAtSample := Settings.DwellTimes.SampleRangeLo ;
        Det.EndAtSample := Settings.DwellTimes.SampleRangeHi ;
        end ;

     { Move data file pointer to start of data to be plotted }
     Det.SampleNum := Det.StartAtSample ;
     Main.StatusBar.SimpleText := 'Single-channel Analysis : Wait ... Removing trend' ;

     NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

     { Initial settings of while loop control flags }
     NewBufferNeeded := True ;
     iPointer := Channel[ChanNum].ChannelOffset ;
     BufferStart := Det.SampleNum ;
     Done := False ;
     while not Done do begin

         { Load another buffer of A/D samples from file when needed }
         if NewBufferNeeded then begin
            BufferStart := Det.SampleNum ;
            { Read into buffer }
            if ReadCDRBuffer(CdrFH,BufferStart,ADC,NumBlocksPerBuffer)
               <> NumBlocksPerBuffer then edStatus.Text := 'Read Error' ;

            { Set buffer pointer to first sample }
            iPointer := Channel[ChanNum].ChannelOffset ;

            NewBufferNeeded := False ;
            Application.ProcessMessages ;
            end ;

         { Subtract trend from A/D samples }
         ADC[iPointer] := ADC[iPointer]
                           + Round( TrendLine.Func.Parameters[0]
                                    - TrendLine.Func.Value( Det.SampleNum*1.0 ) ) ;

         { Next sample }
         iPointer := iPointer + CdrFH.NumChannels ; ;
         Inc(Det.SampleNum) ;

         { End when last sample done }
         if Det.SampleNum > Det.EndAtSample then Done := True ;

         { Write buffer back to file }
         if (iPointer >= NumSamplesPerBuffer) or Done then begin
            { Read into buffer }
            if WriteCDRBuffer(CdrFH,BufferStart,ADC,NumBlocksPerBuffer)
               <> NumBlocksPerBuffer then edStatus.Text := 'Write Error' ;
            NewBufferNeeded := True ;
            end ;
         end ;

     // Final report
     Main.StatusBar.SimpleText :=
     ' Single-channel Analysis : Trend removed from digitised signal.' ;

     { Set current baseline level to new value after trend removal }
     Channel[ChanNum].ADCZero := Round(TrendLine.Func.Parameters[0]) ;

     { Clear trend line }
     TrendLine.NumPoints := 0 ;
     edNumBlocks.Value := TrendLine.NumPoints ;
     TrendLine.Available := False ;
     bRemoveTrend.Enabled := False ;
     bAddBlock.Enabled := True ;
     DisplayRecord ;

     end;

procedure TSingleChanAnalFrm.bRestoreOriginalClick(Sender: TObject);
{ --------------------------------------
  Restore orginal data from back up file
  -------------------------------------- }
begin
     Screen.Cursor := crHourGlass ;
     RestoreFromBackupFile ;
     Screen.Cursor := crDefault ;
     end;

procedure TSingleChanAnalFrm.cbDetChannelChange(Sender: TObject);
// --------------------------------------------
// Channel selected for event detection changed
// --------------------------------------------
begin
     Settings.DwellTimes.ChanNum := cbDetChannel.ItemIndex ;
     ChannelChanged ;
     end;


procedure TSingleChanAnalFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menu items }
     Main.CopyAndPrintMenus( False, False ) ;
     end;


procedure  TSingleChanAnalFrm.ZoomIn( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDetDisplay.YZoom( Chan, -50.0 ) ;
     scEditDisplay.YZoom( Chan, -50.0 ) ;
     end ;


procedure  TSingleChanAnalFrm.ZoomOut( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin

     scDetDisplay.YZoom( Chan, 50.0 ) ;
     scEditDisplay.YZoom( Chan, 50.0 ) ;

     end ;


procedure  TSingleChanAnalFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDetDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDetDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDetDisplay.ZoomOut ;
     scEditDisplay.MaxADCValue := scDetDisplay.MaxADCValue ;
     scEditDisplay.MinADCValue := scDetDisplay.MinADCValue ;
     scEditDisplay.ZoomOut ;
     end ;


// **** Stability plot methods *************************************

procedure TSingleChanAnalFrm.InitialiseStabPlot ;
{ -------------------------------
  Initialise stability plot page
  ------------------------------- }
begin

     cbStabPlotType.Clear ;
     cbStabPlotType.Items.AddObject( 'Mean current', TObject(stMeanCurrent) ) ;
     cbStabPlotType.ItemIndex := 0 ;
     NewStabPlotType ;

     plStabPlot.ClearVerticalCursors ;
     StabCurs.Read := plStabPlot.AddVerticalCursor( clGreen, '?r',0 ) ;

     // Make summary table invisible
     sgSummary.Visible := False ;

     bStabPlotSetAxes.Enabled := False ;

     end ;


procedure TSingleChanAnalFrm.bDoStabPlotClick(Sender: TObject);
{ ---------------------------
  Create a new stability plot
  --------------------------- }
var
   StartAt,EndAt : Integer ;
   StabPlotType : TStabPlotType ;
   BlockSize : Integer ;
begin

     bDoStabPlot.Enabled := False ;
     bAbortStabPlot.Enabled := True ;
     Screen.Cursor := crHourGlass ;

     { Get type of stability plot }
     StabPlotType := TStabPlotType(
                     cbStabPlotType.Items.Objects[cbStabPlotType.ItemIndex]) ;

     // Get data range to plotted
     if rbStabPlotAllEvents.Checked then begin
        StartAt := Round(edStabPlotRange.LoLimit) ;
        EndAt :=   Round(edStabPlotRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edStabPlotRange.LoValue) ;
        EndAt :=   Round(edStabPlotRange.HiValue) ;
        end ;

     { No. of averaging regions to be plotted }
     BlockSize := Round(edStabPlotBlockSize.Value) ;

     { Choose appropriate histogram computation procedure }
     sgSummary.Visible := False ;
     case StabPlotType of
          stClosedTimes : DwellTimeStabPlot(StartAt,EndAt,BlockSize,csClosedState ) ;
          stOpenTimes : DwellTimeStabPlot(StartAt,EndAt,BlockSize,csOpenState ) ;
          stChannelCurrents : ChannelCurrentStabPlot(StartAt,EndAt,BlockSize ) ;
          stMeanCurrent : MeanCurrentStabPlot( StartAt, EndAt, BlockSize, 0.0 ) ;
          stOpenProb :    MeanCurrentStabPlot( StartAt, EndAt, BlockSize,
                                               Settings.DwellTimes.UnitCurrent*
                                               edStabPlotNumChannels.Value ) ;
          stAvgVsOpenTimes : StateAverageVsOpenTime( StartAt, EndAt ) ;
          stSummary : SummaryTable( StartAt, EndAt ) ;
          stCursorAverage,
          stCursorDuration,
          stCursorSD : CursorMeasurementsStabPlot( StabPlotType,
                                                   StartAt,
                                                   EndAt,
                                                   BlockSize ) ;
          end ;

     Screen.Cursor := crDefault ;
     bDoStabPlot.Enabled := True ;
     bStabPlotSetAxes.Enabled := True ;
     bAbortStabPlot.Enabled := False ;
     end;


procedure  TSingleChanAnalFrm.DwellTimeStabPlot(
           StartAt : Integer ;       { Event to start at [In] }
           EndAt : Integer ;         { Event to end at [In] }
           BlockSize : Integer ;    { No. of events/block [In] }
           RequiredState : Integer   { State to analysed  [In] }
           ) ;
{ -------------------------------------------------------------------
  Plot average of selected dwell time over series of NumRegions blocks
  ------------------------------------------------------------------- }
var
   EventNum,nAvg : Integer ;
   SumX, SumY : single ;
   x,y : Single ;
   Event : TEvent ;
   Done : Boolean ;
begin

     Settings.DwellTimes.EventRangeLo := Round(edStabPlotRange.LoValue) ;
     Settings.DwellTimes.EventRangeHi := Round(edStabPlotRange.HiValue) ;
     Settings.DwellTimes.EventBlockSize := Round(edStabPlotBlockSize.Value) ;

     { Plot graph of currently selected variables }
     plStabPlot.xAxisAutoRange := True ;
     plStabPlot.yAxisAutoRange := True ;
     plStabPlot.xAxisLabel := 'Time (s) ' ;
     plStabPlot.yAxisLabel := cbStabPlotType.Text + ' (' + Settings.TUnits + ')' ;

     plStabPlot.MaxPointsPerLine := Max( ((EndAt - StartAt + 1) div BlockSize) + 1,1000) ;

     { Clear data points line }
     plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

    { Read records of data from file and add to histogram }
    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    EventNum := StartAt ;

    Done := False ;
    while not Done do begin

         { Get event data from file  }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         { Quit if event not available }
         Done := not Event.Available ;

         { Add duration to average, if it is the correct type of state }
         if not Event.Ignore then begin
            if Event.ChannelState = RequiredState then begin
               SumY := SumY + Event.Duration*Settings.TScale ;
               SumX := SumX + Event.StartAt*cdrFH.dt ;
               Inc(nAvg) ;
               end ;
            end ;

        { Add average of block to plot when block is done }
        if nAvg >= BlockSize then begin
           if nAvg > 0 then plStabPlot.AddPoint( 0, SumX/nAvg, SumY/nAvg ) ;
           SumX := 0.0 ;
           SumY := 0.0 ;
           nAvg := 0 ;
           end ;

        { Next event }
         Inc(EventNum) ;

        { Exit loop when at end of sequence }
        if (EventNum > EndAt) or (not bAbortStabPlot.Enabled) then Done := True ;

        { Report progress }
        Main.StatusBar.SimpleText := format(
        ' Single-channel Analysis : Dwell time stability plot Event %d/%d',
        [EventNum,EndAt] ) ;
        end ;

    // Set initial cursor position
    plStabPlot.GetPoint( 0 ,
                         plStabPlot.GetNumPointsInLine(0) div 2,
                         x,y) ;
    plStabPlot.VerticalCursors[StabCurs.Read] := x ;

    { Final report }
    Main.StatusBar.SimpleText := format(
    ' Single-channel Analysis : Dwell time stability plot created (%d events)',
    [EventNum-1] ) ;

    end ;


procedure  TSingleChanAnalFrm.ChannelCurrentStabPlot(
           StartAt : Integer ;       { Event to start at [In] }
           EndAt : Integer ;         { Event to end at [In] }
           BlockSize : Integer      { No. of events/block [In] }
           ) ;
{ -------------------------------------------------------------------
  Plot average of single-channel currents over series of NumRegions blocks
  ------------------------------------------------------------------- }
var
   EventNum,nAvg : Integer ;
   x,y, SumX, SumY : single ;
   AverageAmplitude, StDev : single ;
   Event : TEvent ;
begin

     Settings.DwellTimes.EventRangeLo := Round(edStabPlotRange.LoValue) ;
     Settings.DwellTimes.EventRangeHi := Round(edStabPlotRange.HiValue) ;
     Settings.DwellTimes.EventBlockSize := Round(edStabPlotBlockSize.Value) ;

    { Plot graph of currently selected variables }
    plStabPlot.xAxisAutoRange := True ;
    plStabPlot.yAxisAutoRange := True ;
    plStabPlot.xAxisLabel := 'Time (s) ' ;
    plStabPlot.yAxisLabel := cbStabPlotType.Text
                             + ' (' + Channel[ChanNum].ADCUnits + ')';
    plStabPlot.MaxPointsPerLine := Max( ((EndAt - StartAt +1) div BlockSize) + 1,1000) ;

    { Clear data points line }
    plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

    // Read records of data from file and add to histogram
    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    for EventNum := StartAt to EndAt do begin

         { Read event }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         if (not Event.Available) or (not bAbortStabPlot.Enabled) then Break ;

         { Extract samples associated with event and add to histogram }
         if (not Event.Ignore) and (Event.ChannelState = 1) then begin
            // Calculate average and add to runningaverage
            if AverageEventAmplitude( Event, AverageAmplitude, StDev ) then begin
               SumY := SumY + AverageAmplitude ;
               SumX := SumX + Event.StartAt*CDRFH.dt ;
               Inc(nAvg) ;
               end ;
            end ;

        { Add average of block to plt when block is done }
        if (nAvg >= BlockSize) or (EventNum >= EndAt) then begin
           if nAvg > 0 then plStabPlot.AddPoint( 0, SumX/nAvg, SumY/nAvg ) ;
           SumX := 0.0 ;
           SumY := 0.0 ;
           nAvg := 0 ;
           Application.ProcessMessages ;
           end ;

        { Report progress }
        Main.StatusBar.SimpleText := format(
        ' Single-channel Analysis : Channel current stability plot Event %d/%d',
        [EventNum,EndAt] ) ;

        end ;

    // Set initial cursor position
    plStabPlot.GetPoint( 0 ,
                         plStabPlot.GetNumPointsInLine(0) div 2,
                         x,y) ;
    plStabPlot.VerticalCursors[StabCurs.Read] := x ;

    { Final report }
    Main.StatusBar.SimpleText := format(
    ' Single-channel Analysis : Channel current stability plot created (%d events)',
    [EventNum-1] ) ;

    end ;


procedure  TSingleChanAnalFrm.MeanCurrentStabPlot(
           StartAt : Integer ;       { Sample to start at [In] }
           EndAt : Integer ;         { Sample to end at [In] }
           RegionSize : Integer ;    { No. of Samples per block [In] }
           DivideFactor : single     { Unitary current for calculation of open prob. }
           ) ;
{ ----------------------------------------------------------------------
  Plot average of mean current (or open probability if DivideFactor <>0) )
  ---------------------------------------------------------------------- }
const
  MaxBlocksPerBuffer = 256 ;
var
   nAvg : Integer ;
   BlockPointer : Integer ;        { Sample block pointer }
   BufPointer : Integer ;          { Sample pointer within buffer }
   NumSamplesPerBuffer : Integer ; { No. of individual samples in buffer }
   NumBlocksToRead : Integer ;
   SumX, SumY : double ;
   x,y : Single ;
   Avg,AvgTime : single ;
   Done : Boolean ;
   ADC : Array[0..MaxBlocksPerBuffer*MaxChannels-1] of SmallInt ;
begin

     Settings.DwellTimes.SampleRangeLo := Round(edStabPlotRange.LoValue) ;
     Settings.DwellTimes.SampleRangeHi := Round(edStabPlotRange.HiValue) ;
     Settings.DwellTimes.SampleBlockSize := Round(edStabPlotBlockSize.Value) ;
     RegionSize := Max(RegionSize,1) ;
     Settings.DwellTimes.NumChannelsPerPatch := Round(edStabPlotNumChannels.Value);

     { Plot graph of currently selected variables }
     plStabPlot.xAxisAutoRange := True ;
     plStabPlot.yAxisAutoRange := True ;
     plStabPlot.xAxisLabel := 'Time (s) ' ;

     { If probability plot, no units for Y axis }
     if DivideFactor <> 0.0 then
        plStabPlot.yAxisLabel := cbStabPlotType.Text
     else
        plStabPlot.yAxisLabel := cbStabPlotType.Text
                                 + ' (' + Channel[ChanNum].ADCUnits + ')';

     plStabPlot.MaxPointsPerLine := Max( ((EndAt - StartAt +1) div RegionSize) + 1,1000) ;

     { Clear data points line }
     plStabPlot.ClearPlot ;
     plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

    { Read A/D samples from file and add to average }

    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    EndAt := Min(EndAt,(CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1) ;

    { Sample/buffer pointers }
    BlockPointer := StartAt ;
    NumSamplesPerBuffer := MaxBlocksPerBuffer*CdrFH.NumChannels ;
    BufPointer := NumSamplesPerBuffer ;

    Done := False ;
    while not Done do begin

         { Get buffer of samples from file }
         if BufPointer >= NumSamplesPerBuffer then begin
            NumBlocksToRead := Min(MaxBlocksPerBuffer,EndAt-BlockPointer + 1) ;
            ReadCDRBuffer(CdrFH,BlockPointer,ADC,NumBlocksToRead) ;
            BufPointer := Channel[ChanNum].ChannelOffset ;
            end ;

         { Add sample to average, if it is the correct type of state }
         SumY := SumY + ADC[BufPointer] ;
         SumX := SumX + BlockPointer ;
         Inc(nAvg) ;

        { Add average of block to plot when block is done }
        if (nAvg >= RegionSize) then begin
           AvgTime := (SumX/nAvg)*CdrFH.dt ;
           Avg := ((SumY/nAvg)-Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
           { If divide factor non-zero divide by it to convert current to
             channel open probability }
           if DivideFactor <> 0.0 then Avg := Avg/DivideFactor ;

           plStabPlot.AddPoint( 0, AvgTime, Avg ) ;
           SumX := 0.0 ;
           SumY := 0.0 ;
           nAvg := 0 ;
           { Report progress }
           Main.StatusBar.SimpleText := format(
           ' Single-channel Analysis : Mean current stability Plot %.1f/%.1fa',
           [BlockPointer*CDRFH.dt,EndAt*CDRFH.dt] ) ;
           end ;

        { Next sample block }
        Inc(BlockPointer) ;
        BufPointer := BufPointer + CdrFH.NumChannels ;

        if (BlockPointer > EndAt) or
           (not bAbortStabPlot.Enabled) then Done := True ;

        end ;

    // Set initial cursor position
    plStabPlot.GetPoint( 0 ,
                         plStabPlot.GetNumPointsInLine(0) div 2,
                         x,y) ;
    plStabPlot.VerticalCursors[StabCurs.Read] := x ;

    { Report progress }
    Main.StatusBar.SimpleText := format(
    ' Single-channel Analysis : Mean current stability plot created (%.1fs)',
    [(BlockPointer-StartAt)*CDRFH.dt] ) ;

    end ;


procedure  TSingleChanAnalFrm.CursorMeasurementsStabPlot(
           StabPlotType : TStabPlotType ; // Type of plot (in)
           StartAt : Integer ;       { Measurement to start at [In] }
           EndAt : Integer ;         { Measurement to end at [In] }
           RegionSize : Integer      { No. of seasurements per block [In] }
           ) ;
// ---------------------------------------
//  Plot cursor measurements
// ---------------------------------------
var
   nAvg : Integer ;
   iMeasurement,Last,Current : Integer ;
   SumX, SumY : double ;
   x,y : Single ;
   Done : Boolean ;
   Temp : TCursorMeasurement ;
begin

     { Plot graph of currently selected variables }
     plStabPlot.xAxisAutoRange := True ;
     plStabPlot.yAxisAutoRange := True ;
     plStabPlot.xAxisLabel := 'Time (s) ' ;

     // Plot type
     case StabPlotType of
        stCursorDuration : plStabPlot.yAxisLabel := 'Duration (ms)' ;
        stCursorAverage : plStabPlot.yAxisLabel := 'Average ' +
                          '(' + Channel[ChanNum].ADCUnits + ')';
        stCursorSD : plStabPlot.yAxisLabel := 'S.D. ' +
                          '(' + Channel[ChanNum].ADCUnits + ')';
        end ;

     plStabPlot.MaxPointsPerLine := Max( ((EndAt - StartAt +1) div RegionSize) + 1,1000) ;

     { Clear data points line }
     plStabPlot.ClearPlot ;
     plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     // Sort measurements into ascending X order
     for Last := NumCursorMeasurements-1 DownTo 1 do begin
         for Current := 0 to Last-1 do begin
             if CursorMeasurements[Current].TStart >
                CursorMeasurements[Current+1].TStart then begin
                Temp := CursorMeasurements[Current] ;
                CursorMeasurements[Current] := CursorMeasurements[Current+1] ;
                CursorMeasurements[Current+1] := Temp ;
                end ;
             end ;
         end ;


    // Average over blocks of size RegionSize and plot

    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    iMeasurement := StartAt-1 ;
    Done := False ;
    while not Done do begin

         case StabPlotType of
              stCursorAverage : begin
                  SumY := SumY + CursorMeasurements[iMeasurement].AverageCurrent ;
                  end ;
              stCursorDuration : begin
                  SumY := SumY + CursorMeasurements[iMeasurement].Duration ;
                  end ;
              stCursorSD : begin
                  SumY := SumY + CursorMeasurements[iMeasurement].SDCurrent ;
                  end ;
              end ;
         SumX := SumX + CursorMeasurements[iMeasurement].TStart ;
         Inc(nAvg) ;

        { Add average of block to plot when block is done }
        if (nAvg >= RegionSize) then begin
           plStabPlot.AddPoint( 0, SumX/nAvg, SumY/nAvg ) ;
           SumX := 0.0 ;
           SumY := 0.0 ;
           nAvg := 0 ;
           if not bAbortStabPlot.Enabled then Done := True ;
           { Report progress }
           Main.StatusBar.SimpleText := format(
           ' Single-channel Analysis : Cursor Measurements stability Plot %d/%d',
           [iMeasurement,EndAt] ) ;
           end ;

        Inc(iMeasurement) ;
        if iMeasurement >= EndAt then Done := True ;

        end ;

    // Set initial cursor position
    plStabPlot.GetPoint( 0 ,
                         plStabPlot.GetNumPointsInLine(0) div 2,
                         x,y) ;
    plStabPlot.VerticalCursors[StabCurs.Read] := x ;

    { Report progress }
    Main.StatusBar.SimpleText := format(
    ' Single-channel Analysis : Cursor Measurements stability plot created (%d)',
    [EndAt - StartAt + 1] ) ;

    end ;


procedure TSingleChanAnalFrm.StateAverageVsOpenTime(
           StartAt : Integer ;       { Event No. to start at [In] }
           EndAt : Integer           { Event No. to end at [In] }
           ) ;

{ ------------------------------------------
  Compute average current amplitude by state
  ------------------------------------------}
var
   EventNum : Integer ;
   Event : TEvent ;
   AverageAmplitude, StDev : Single ;
   xCurs,yCurs : Single ;
   NumPoints : Integer ;
begin

     Settings.DwellTimes.EventRangeLo := Round(edStabPlotRange.LoValue) ;
     Settings.DwellTimes.EventRangeHi := Round(edStabPlotRange.HiValue) ;

      { Plot graph of currently selected variables }
      plStabPlot.xAxisAutoRange := True ;
      plStabPlot.yAxisAutoRange := True ;
      plStabPlot.xAxisLabel := 'Open Time (ms) ' ;
      plStabPlot.yAxisLabel := format('Avg. Current (%s)',[Channel[ChanNum].ADCUnits]) ;
      plStabPlot.MaxPointsPerLine :=  Max( ((EndAt - StartAt) div 2) + 1,1000) ;

      { Clear data points line }
      plStabPlot.ClearPlot ;
      plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psClear ) ;

      { Read records of data from file and add to histogram }
      NumPoints := 0 ;
      for EventNum := StartAt to EndAt do begin

         { Read event }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         if (not Event.Available) or (not bAbortStabPlot.Enabled) then Break ;

         { Extract samples associated with event and add to histogram }
         if (not Event.Ignore) and (Event.ChannelState = 1) then begin
            // Calculate average and add to plot
            if AverageEventAmplitude( Event, AverageAmplitude, StDev ) then begin
               plStabPlot.AddPoint( 0, Event.Duration*1000.0, AverageAmplitude ) ;
               Inc(NumPoints) ;
               end ;
            end ;

         { Report progress }
         Main.StatusBar.SimpleText := format(
         ' Single-channel Analysis : Average open time plot Event %d/%d',
         [EventNum,EndAt] ) ;

         { Allow other events to be serviced }
         if (NumPoints mod 100) = 0 then Application.ProcessMessages ;

         end ;

      // Set initial cursor position
      plStabPlot.GetPoint( 0 ,
                           plStabPlot.GetNumPointsInLine(0) div 2,
                           xCurs,yCurs) ;
      plStabPlot.VerticalCursors[StabCurs.Read] := xCurs ;

      { Final Report }
      Main.StatusBar.SimpleText := format(
      ' Single-channel Analysis : Average open time plot created (%d events)',
      [EventNum-1] ) ;

    end ;


procedure TSingleChanAnalFrm.SummaryTable(
           StartAt : Integer ;       { Event No. to start at [In] }
           EndAt : Integer           { Event No. to end at [In] }
           ) ;

{ --------------------------------------------------------------
  Compute and display table of mean open/close times/probability
  --------------------------------------------------------------}
var
   Row,Col,MaxWidth,EventNum : Integer ;
   Done : Boolean ;
   Event : TEvent ;
   NumEventsIgnored : Integer ;
   TimeIgnored : double ;
   NumOpenEvents : Integer ;
   TimeOpen,MeanOpenTime : double ;
   NumClosedEvents : Integer ;
   TimeClosed,MeanClosedTime : double ;
   TimeClosedInBurst,TimeClosedBetweenBursts,MeanBurstDuration : double ;
   MeanClosedTimeBetweenBursts,NumOpeningsPerBurst,MeanClosedTimeInBurst : Double ;
   POpen,POpenInBurst : Double ;
   NumBursts : Integer ;

begin

      Settings.DwellTimes.TCritical := edStabPlotTCritical.Value ;
      Settings.DwellTimes.NumChannelsPerPatch := Round(edStabPlotNumChannels.Value) ;

      { Read records of data from file and add to histogram }
      Done := False ;
      EventNum := StartAt ;
      NumEventsIgnored := 0 ;
      TimeIgnored := 0.0 ;
      NumOpenEvents := 0 ;
      TimeOpen := 0.0 ;
      NumClosedEvents := 0 ;
      TimeClosed := 0.0 ;
      TimeClosedInBurst := 0.0 ;
      TimeClosedBetweenBursts := 0.0 ;
      NumBursts := 0 ;

      while not Done do begin

         { Read event }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         // Add event duration to averages

         if Event.Ignore then begin
            // Ignored events
            Inc(NumEventsIgnored) ;
            TimeIgnored := TimeIgnored + Event.Duration ;
            end
         else if Event.ChannelState = 1 then begin
            // Open events
            Inc(NumOpenEvents) ;
            TimeOpen := TimeOpen + Event.Duration ;
            end
         else if Event.ChannelState = 0 then begin
            // Closed events
            Inc(NumClosedEvents) ;
            TimeClosed := TimeClosed + Event.Duration ;

            if Event.Duration <= Settings.DwellTimes.TCritical then begin
               TimeClosedInBurst :=  TimeClosedInBurst + Event.Duration ;
               end
            else begin
               TimeClosedBetweenBursts := TimeClosedBetweenBursts + Event.Duration ;
               Inc(NumBursts) ;
               end ;
            end ;

         { Report progress }
         Main.StatusBar.SimpleText := format(
         ' Single-channel Analysis : Event Summary %d/%d',
         [EventNum,EndAt] ) ;

         { Increment to next record }
         Inc(EventNum)  ;
         if (EventNum > EndAt) or
            (not bAbortStabPlot.Enabled) then Done := True ;

         { Allow other events to be serviced }
         if (EventNum mod 100) = 0 then Application.ProcessMessages ;
         { If the Stability Plot button has been re-enabled, abort the run }
         if Not bAbortStabPlot.Enabled then Done := True ;

         end ;

      // Calculate means
      if NumOpenEvents > 0 then MeanOpenTime := TimeOpen / NumOpenEvents
                           else MeanOpenTime := 0.0 ;
      if NumClosedEvents > 0 then MeanClosedTime := TimeClosed / NumClosedEvents
                             else MeanClosedTime := 0.0 ;

      if (TimeOpen + TimeClosed) > 0.0 then begin
         POpen := TimeOpen / ((TimeOpen + TimeClosed)*edStabPlotNumChannels.Value) ;
         end
      else POpen := 0.0 ;


      if NumBursts > 0 then begin
         MeanBurstDuration := (TimeOpen + TimeClosedInBurst) / NumBursts ;
         if NumClosedEvents > NumBursts then
            MeanClosedTimeInBurst := TimeClosedInBurst / (NumClosedEvents-NumBursts)
         else MeanClosedTimeInBurst := 0.0 ;
         MeanClosedTimeBetweenBursts := TimeClosedBetweenBursts / NumBursts ;
         NumOpeningsPerBurst :=  NumOpenEvents / NumBursts ;
         POpenInBurst := TimeOpen / ((MeanBurstDuration*NumBursts)*edStabPlotNumChannels.Value) ;
         end
      else begin
         MeanBurstDuration := 0.0 ;
         MeanClosedTimeBetweenBursts := 0.0 ;
         MeanClosedTimeInBurst := 0.0 ;
         NumOpeningsPerBurst := 0 ;
         POpenInBurst := 0.0 ;
         end ;
      Row := 0 ;
      // Event range analysed
      sgSummary.Cells[0,Row] := ' Dwell time summary results ' ;
      Inc(Row) ;

      // File name
      sgSummary.Cells[0,Row] := ' File ' ;
      sgSummary.Cells[1,Row] := CDRFH.FileName ;
      Inc(Row) ;

      // ID
      sgSummary.Cells[0,Row] := ' Comment ' ;
      sgSummary.Cells[1,Row] := CDRFH.IdentLine ;
      Inc(Row) ;


      // Event range analysed
      sgSummary.Cells[0,Row] := ' Event range ' ;
      sgSummary.Cells[1,Row] := format(' %d-%d ',[StartAt,EndAt]) ;
      Inc(Row) ;

      // Open events
      sgSummary.Cells[0,Row] := format(' Mean open time (%s) ',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[MeanOpenTime*Settings.TScale]) ;
      Inc(Row) ;
      sgSummary.Cells[0,Row] := ' No. open events ' ;
      sgSummary.Cells[1,Row] := format(' %d ',[NumOpenEvents]) ;
      Inc(Row) ;

      // Closed events
      sgSummary.Cells[0,Row] := format(' Mean closed time (%s) ',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[MeanClosedTime*Settings.TScale]) ;
      Inc(Row) ;
      sgSummary.Cells[0,Row] := ' No. closed events ' ;
      sgSummary.Cells[1,Row] := format(' %d ',[NumClosedEvents]) ;
      Inc(Row) ;

      // Open channel probability
      sgSummary.Cells[0,Row] := format(' P.open (n= %d channels)',
                                        [Round(edStabPlotNumChannels.Value)]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[POpen]) ;
      Inc(Row) ;

      // Burst duration
      sgSummary.Cells[0,Row] := format(' Mean burst duration (%s) ',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[MeanBurstDuration*Settings.TScale]) ;
      Inc(Row) ;
      // No. of bursts
      sgSummary.Cells[0,Row] := ' No. of bursts ' ;
      sgSummary.Cells[1,Row] := format(' %d ',[NumBursts]) ;
      Inc(Row) ;
      // No. of openings per burst
      sgSummary.Cells[0,Row] := ' Mean no. of openings per burst ' ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[NumOpeningsPerBurst]) ;
      Inc(Row) ;
      // Mean closed time within burst
      sgSummary.Cells[0,Row] := format(' Mean closed time within burst (%s) ',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[MeanClosedTimeInBurst*Settings.TScale]) ;
      Inc(Row) ;
      // Mean closed time between bursts
      sgSummary.Cells[0,Row] := format(' Mean closed time between bursts (%s) ',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[MeanClosedTimeBetweenBursts*Settings.TScale]) ;
      Inc(Row) ;
      // Open channel probability (within burst)
      sgSummary.Cells[0,Row] := format(' P.open (within burst n= %d channels) ',
                                       [Round(edStabPlotNumChannels.Value)]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[POpenInBurst]) ;
      Inc(Row) ;
      // Critical closed time
      sgSummary.Cells[0,Row] := format(' T.critical closed time (%s)',[Settings.TUnits]) ;
      sgSummary.Cells[1,Row] := format(' %.4g ',[Settings.DwellTimes.TCritical*Settings.TScale]) ;
      Inc(Row) ;
      // No. of events ignored
      sgSummary.Cells[0,Row] := ' No. events ignored ' ;
      sgSummary.Cells[1,Row] := format(' %d ',[NumEventsIgnored]) ;

      // Adjust column widths to accomodate text
      for Col := 0 to 1 do begin
          MaxWidth := 0 ;
          for Row := 0 to sgSummary.RowCount-1 do begin
              if MaxWidth < sgSummary.Canvas.TextWidth(sgSummary.Cells[Col,Row]+'xxx') then
                 MaxWidth := sgSummary.Canvas.TextWidth(sgSummary.Cells[Col,Row]+'xxx')
              end ;
          sgSummary.ColWidths[Col] := MaxWidth ;
          end ;

      // Place table over plot
      sgSummary.Top := plStabPlot.Top ;
      sgSummary.Left := plStabPlot.Left ;
      sgSummary.Width := plStabPlot.Width ;
      sgSummary.Height := plStabPlot.Height ;
      sgSummary.Visible := True ;

      { Final Report}
      Main.StatusBar.SimpleText := format(
      ' Single-channel Analysis : Event Summary created (%d events)',
      [EventNum-1] ) ;

      end ;



procedure TSingleChanAnalFrm.bStabPlotSetAxesClick(Sender: TObject);
{ ----------------------------------------
  Set stability plot axes range/law/labels
  ---------------------------------------- }
begin
     SetAxesFrm.Plot := plStabPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;


procedure TSingleChanAnalFrm.NewStabPlotType ;
{ -------------------------------------------------------
  Update controls when a new stability plot type selected
  ------------------------------------------------------- }
var
   StabPlotType : TStabPlotType ;
   OldIndex : Integer ;
begin

     { Update stability plot type list depending upon whether dwell times are available }
     OldIndex := cbStabPlotType.ItemIndex ;
     cbStabPlotType.Clear ;
     cbStabPlotType.Items.AddObject( 'Mean current', TObject(stMeanCurrent) ) ;
     cbStabPlotType.Items.AddObject( 'Open Probability', TObject(stOpenProb) ) ;
     if EventFile.NumEvents > 0 then begin
        cbStabPlotType.Items.AddObject( 'Closed times', TObject(stClosedTimes) ) ;
        cbStabPlotType.Items.AddObject( 'Open times', TObject(stOpenTimes) ) ;
        cbStabPlotType.Items.AddObject( 'Single-channel currents', TObject(stChannelCurrents) ) ;
        cbStabPlotType.Items.AddObject( 'Current vs Open times', TObject(stAvgVsOpenTimes) ) ;
        cbStabPlotType.Items.AddObject( 'Summary table', TObject(stSummary) ) ;
        end ;

     // Cursor measurement list options
     cbStabPlotType.Items.AddObject( 'Cursor Meas. (Avg.)', TObject(stCursorAverage) ) ;
     cbStabPlotType.Items.AddObject( 'Cursor Meas. (Dur.)', TObject(stCursorDuration) ) ;
     cbStabPlotType.Items.AddObject( 'Cursor Meas. (S.D.)', TObject(stCursorSD) ) ;
     cbStabPlotType.ItemIndex := OldIndex ;

     StabPlotType := TStabPlotType(
                     cbStabPlotType.Items.Objects[cbStabPlotType.ItemIndex]) ;

     // No. averaging regions edit box

     case StabPlotType of

          stClosedTimes, stOpenTimes, stChannelCurrents : Begin
             StabPlotBlockSizePanel.Visible := True ;
             StabPlotTCriticalPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := False ;
             edStabPlotRange.Scale := 1.0 ;
             edStabPlotRange.Units := '' ;
             edStabPlotRange.HiLimit := EventFile.NumEvents ;
             edStabPlotRange.LoLimit := 1 ;
             edStabPlotRange.LoValue := Settings.DwellTimes.EventRangeLo ;
             edStabPlotRange.HiValue := Settings.DwellTimes.EventRangeHi ;
             edStabPlotBlockSize.Scale := edStabPlotRange.Scale ;
             edStabPlotBlockSize.Units := edStabPlotRange.Units ;
             edStabPlotBlockSize.LoLimit := Max( 1, EventFile.NumEvents div plStabPlot.MaxPointsPerLine) ;
             edStabPlotBlockSize.HiLimit := EventFile.NumEvents ;
             edStabPlotBlockSize.Value := Settings.DwellTimes.EventBlockSize ;
             bUseCursorsForStabPlotRange.Enabled := False ;
             end ;

          stAvgVsOpenTimes : begin
             StabPlotBlockSizePanel.Visible := False ;
             StabPlotTCriticalPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := False ;
             edStabPlotRange.Scale := 1.0 ;
             edStabPlotRange.Units := '' ;
             edStabPlotRange.HiLimit := EventFile.NumEvents ;
             edStabPlotRange.LoLimit := 1 ;
             edStabPlotRange.LoValue := Settings.DwellTimes.EventRangeLo ;
             edStabPlotRange.HiValue := Settings.DwellTimes.EventRangeHi ;
             edStabPlotBlockSize.HiLimit := EventFile.NumEvents ;
             bUseCursorsForStabPlotRange.Enabled := False ;
             end ;

          stOpenProb : begin
             StabPlotBlockSizePanel.Visible := True ;
             StabPlotTCriticalPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := True ;
             edStabPlotRange.Scale := CdrFH.dt ;
             edStabPlotRange.Units := 's' ;
             edStabPlotRange.HiLimit := ((CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1) ;
             edStabPlotRange.LoLimit := 0.0 ;
             edStabPlotRange.LoValue := Settings.DwellTimes.SampleRangeLo ;
             edStabPlotRange.HiValue := Settings.DwellTimes.SampleRangeHi ;

             edStabPlotBlockSize.Scale := edStabPlotRange.Scale ;
             edStabPlotBlockSize.Units := edStabPlotRange.Units ;
             edStabPlotBlockSize.LoLimit := Max( CdrFH.dt*10,
                                                 edStabPlotRange.HiLimit/plStabPlot.MaxPointsPerLine) ;
             edStabPlotBlockSize.HiLimit := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;
             edStabPlotBlockSize.Value := Settings.DwellTimes.SampleBlockSize ;
             edStabPlotNumChannels.Value := Settings.DwellTimes.NumChannelsPerPatch ;
             bUseCursorsForStabPlotRange.Enabled := True ;
             end ;

          stSummary : begin
             StabPlotBlockSizePanel.Visible := False ;
             StabPlotTCriticalPanel.Visible := True ;
             StabPlotNumChannelsPanel.Visible := True ;
             edStabPlotTCritical.Scale := Settings.TScale ;
             edStabPlotTCritical.Units := Settings.TUnits ;
             edStabPlotTCritical.value := Settings.DwellTimes.TCritical ;
             edStabPlotRange.Scale := 1.0 ;
             edStabPlotRange.Units := '' ;
             edStabPlotRange.HiLimit := EventFile.NumEvents ;
             edStabPlotRange.LoLimit := 1 ;
             edStabPlotRange.LoValue := Settings.DwellTimes.EventRangeLo ;
             edStabPlotRange.HiValue := Settings.DwellTimes.EventRangeHi ;
             edStabPlotBlockSize.Scale := edStabPlotRange.Scale ;
             edStabPlotBlockSize.Units := edStabPlotRange.Units ;
             edStabPlotBlockSize.LoLimit := Max( 1, EventFile.NumEvents div plStabPlot.MaxPointsPerLine) ;
             edStabPlotBlockSize.HiLimit := EventFile.NumEvents ;
             edStabPlotBlockSize.Value := Settings.DwellTimes.EventBlockSize ;
             edStabPlotNumChannels.Value := Settings.DwellTimes.NumChannelsPerPatch ;
             bUseCursorsForStabPlotRange.Enabled := False ;
             end ;

          stMeanCurrent : begin
             StabPlotBlockSizePanel.Visible := True ;
             StabPlotTCriticalPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := False ;
             edStabPlotRange.Scale := CdrFH.dt ;
             edStabPlotRange.Units := 's' ;
             edStabPlotRange.HiLimit := ((CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1) ;
             edStabPlotRange.LoLimit := 0.0 ;
             edStabPlotRange.LoValue := Settings.DwellTimes.SampleRangeLo ;
             edStabPlotRange.HiValue := Settings.DwellTimes.SampleRangeHi ;
             edStabPlotBlockSize.Scale := edStabPlotRange.Scale ;
             edStabPlotBlockSize.Units := edStabPlotRange.Units ;
             edStabPlotBlockSize.LoLimit := Max( CdrFH.dt*10,
                                                 edStabPlotRange.HiLimit/plStabPlot.MaxPointsPerLine) ;
             edStabPlotBlockSize.HiLimit := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;
             edStabPlotBlockSize.Value := Settings.DwellTimes.SampleBlockSize ;
             bUseCursorsForStabPlotRange.Enabled := True ;

             end ;

          stCursorAverage,stCursorDuration,stCursorSD : Begin
             StabPlotBlockSizePanel.Visible := True ;
             StabPlotTCriticalPanel.Visible := False ;
             StabPlotNumChannelsPanel.Visible := False ;
             edStabPlotRange.Scale := 1.0 ;
             edStabPlotRange.Units := '' ;
             edStabPlotRange.HiLimit := NumCursorMeasurements ;
             edStabPlotRange.LoLimit := 1 ;
             edStabPlotRange.LoValue := 1 ;
             edStabPlotRange.HiValue := NumCursorMeasurements ;
             edStabPlotBlockSize.Scale := edStabPlotRange.Scale ;
             edStabPlotBlockSize.Units := edStabPlotRange.Units ;
             edStabPlotBlockSize.LoLimit := Max( 1, NumCursorMeasurements div plStabPlot.MaxPointsPerLine) ;
             bUseCursorsForStabPlotRange.Enabled := False ;
             end ;

        end ;

     end ;


procedure TSingleChanAnalFrm.cbStabPlotTypeChange(Sender: TObject);
begin
     NewStabPlotType ;
     end;


procedure TSingleChanAnalFrm.ckIgnoreStateClick(Sender: TObject);
{ -------------------------------------------------------
  Update event record when Ignore state check box changed
  ------------------------------------------------------- }
var
   Event : TEvent ;
begin
     ReadEventFromFile( EventFile,  sbEvent.Position, Event ) ;
     Event.Ignore := ckIgnoreState.Checked ;
     WriteEventToFile( EventFile,  sbEvent.Position, Event ) ;
     DisplayEvent ;
     end;


procedure TSingleChanAnalFrm.bBlockIgnoreClick(Sender: TObject);
{ --------------------------------
  Set a block of events to ignored
  -------------------------------- }
begin
     SetIgnoreFrm.SelectedChannel := cbDetChannel.ItemIndex ;
     SetIgnoreFrm.ShowModal ;
     DisplayEvent ;
     end;


procedure TSingleChanAnalFrm.bDeleteIgnoredClick(Sender: TObject);
{ ----------------------------------------------------------
  Remove event marked as "Ignored" completely from the list
  by combining them with adjacent events
  ---------------------------------------------------------- }
var
   FromEventNum : Integer ;      // Index into old event list
   ToEventNum : Integer ;        // Index into new event list
   EndAt,PreviousState : Integer ;
   ExactEnd,Duration : double ;
   NumDeleted : Integer ;        // No. of events deleted counter
   Event : TEvent ;              // Event data record
   Done : Boolean ;              // Process completed flag
begin

     Screen.Cursor := crHourglass ;

     FromEventNum := 1 ;
     ToEventNum := 0 ;
     NumDeleted := 0 ;
     Main.StatusBar.SimpleText := format(
     'Single-channel Analysis : Delete events (%d deleted)',
     [NumDeleted] ) ;
     Done := False ;
     while not Done do begin

         { Read event data from file }
         ReadEventFromFile( EventFile, FromEventNum, Event ) ;

         if Event.Available then begin
            if not Event.Ignore then begin
               { Write event to file }
               Inc(ToEventNum) ;
               WriteEventToFile( EventFile, ToEventNum, Event ) ;
               Inc(FromEventNum) ;
               end
            else if ToEventNum >= 1 then begin
               { Remove ignored event by combining with the adjacent events }
               Duration := Event.Duration ;
               EndAt := Event.EndAt ;
               ExactEnd := Event.ExactEnd ;
               { Get event just before ignored one }
               ReadEventFromFile( EventFile, ToEventNum, Event ) ;
               PreviousState := Event.ChannelState ;
               Inc(FromEventNum) ;
               { Combine the next event, if it is in the same state }
               ReadEventFromFile( EventFile, FromEventNum, Event ) ;
               if Event.Available and (Event.ChannelState = PreviousState)then begin
                  Duration := Duration + Event.Duration ;
                  EndAt := Event.EndAt ;
                  ExactEnd := Event.ExactEnd ;
                  Inc(FromEventNum) ;
                  end ;
               { Get previous event again, and update duration }
               ReadEventFromFile( EventFile, ToEventNum, Event ) ;
               Event.Duration := Duration + Event.Duration ;
               Event.EndAt := EndAt ;
               Event.ExactEnd := ExactEnd ;
               { Write combined event to file }
               WriteEventToFile( EventFile, ToEventNum, Event ) ;
               Inc(NumDeleted) ;
               Main.StatusBar.SimpleText := format(
               'Single-channel Analysis : Delete events. Event %d deleted (%d deleted)',
               [FromEventNum,NumDeleted] ) ;
               end
            else begin
               Inc(NumDeleted) ;
               Inc(FromEventNum) ;
               end ;
            end
         else Done := True ;

         if FromEventNum >= EventFile.NumEvents then Done := True ;
         end ;

     { Update number of events }
     EventFile.NumEvents := ToEventNum + 1 ;

     Main.StatusBar.SimpleText := format(
     'Single-channel Analysis : Delete events (%d deleted)',
     [NumDeleted] ) ;


     Screen.Cursor := crDefault ;

     DisplayEvent ;

     end;

procedure TSingleChanAnalFrm.scEditDisplayCursorChange(Sender: TObject);
// ----------------------------
// Edit display cursors changed
// ----------------------------
var
    i0,i1,i,n : Integer ;
    Y,YZero,YSD,YResidual : Single ;
    s : string ;
begin

     { Update vertical display magnification so that changes are retained }
     Channel[ChanNum].yMin := scEditDisplay.YMin[ChanNum] ;
     Channel[ChanNum].yMax := scEditDisplay.YMax[ChanNum] ;

     if not ckEnableCursorMeasurement.Checked then Exit ;

     // Move Z1 cursor with Z0
     if scEditDisplay.VerticalCursors[EditCurs.Z0] <> EditCurs.OldZ0 then begin
        scEditDisplay.VerticalCursors[EditCurs.Z1] :=
                                   scEditDisplay.VerticalCursors[EditCurs.Z0] +
                                   Round(edCursorSpacing.Value) - 1 ;
        EditCurs.OldZ0  := scEditDisplay.VerticalCursors[EditCurs.Z0] ;
        end ;

     // Move C1 cursor with C0
     if scEditDisplay.VerticalCursors[EditCurs.C0] <> EditCurs.OldC0 then begin
        scEditDisplay.VerticalCursors[EditCurs.C1] :=
                                   scEditDisplay.VerticalCursors[EditCurs.C0] +
                                   Round(edCursorSpacing.Value) - 1 ;
        EditCurs.OldC0  := scEditDisplay.VerticalCursors[EditCurs.C0] ;
        end ;

     // Calculate zero level
     i0 := Min(Max(scEditDisplay.VerticalCursors[EditCurs.Z0],0),scEditDisplay.MaxPoints-1) ;
     i1 := Min(Max(scEditDisplay.VerticalCursors[EditCurs.Z1],0),scEditDisplay.MaxPoints-1) ;
     YZero := 0.0 ;
     n := 0 ;
     for i := Min(i0,i1) to Max(i0,i1) do begin
         YZero := YZero + EditBuf[i*CDRFH.NumChannels + scEditDisplay.ChanOffsets[ChanNum]] ;
         Inc(n) ;
         end ;
     if n > 0 then YZero := YZero / n ;

     // Calculate cursor level
     i0 := Min(Max(scEditDisplay.VerticalCursors[EditCurs.C0],0),scEditDisplay.MaxPoints-1) ;
     i1 := Min(Max(scEditDisplay.VerticalCursors[EditCurs.C1],0),scEditDisplay.MaxPoints-1) ;
     Y := 0.0 ;
     n := 0 ;
     for i := Min(i0,i1) to Max(i0,i1) do begin
         Y := Y + EditBuf[i*CDRFH.NumChannels + scEditDisplay.ChanOffsets[ChanNum]] ;
         Inc(n) ;
         end ;
     if n > 0 then Y := Y / n ;

     YSD := 0.0 ;
     n := 0 ;
     for i := Min(i0,i1) to Max(i0,i1) do begin
         YResidual := Y - EditBuf[i*CDRFH.NumChannels + scEditDisplay.ChanOffsets[ChanNum]] ;
         YSD := YSD + YResidual*YResidual ;
         Inc(n) ;
         end ;
     if n > 1 then YSD := YSD / (n-1)
              else YSD := 0.0 ;
     if YSD > 0.0 then YSD := Sqrt(YSD) ;

     // Calculate measurements
     CursorMeasurements[NumCursorMeasurements].AverageCurrent :=
         (Y-YZero)*scEditDisplay.ChanScale[ChanNum] ;
     CursorMeasurements[NumCursorMeasurements].SDCurrent :=
         YSD*scEditDisplay.ChanScale[ChanNum] ;
     CursorMeasurements[NumCursorMeasurements].Duration :=
         Abs(i0-i1)*CDRFH.dt*1000.0 ;
     CursorMeasurements[NumCursorMeasurements].TStart := (
         scEditDisplay.XOffset + i0)*CDRFH.dt ;

     // Display cursor measurements
     s := format( ' T= %.4g s<br>',
          [CursorMeasurements[NumCursorMeasurements].TStart]) ;
     s := s + format( ' Avg.(a-a) = %.4g %s<br>',
              [CursorMeasurements[NumCursorMeasurements].AverageCurrent,
               scEditDisplay.ChanUnits[ChanNum]]) ;
     s := s + format( ' S.D.(a-a) = %.4g %s<br>',
              [CursorMeasurements[NumCursorMeasurements].SDCurrent,
               scEditDisplay.ChanUnits[ChanNum]]) ;
     s := s + format(' Dur.(a-a) = %.4g ms',
              [CursorMeasurements[NumCursorMeasurements].Duration]) ;
     lbCursors.Caption := s ;

     end;


procedure TSingleChanAnalFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDetDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDetDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDetDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDetDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDetDisplay.TUnits := Settings.TUnits ;
     scDetDisplay.Invalidate ;

     scEditDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scEditDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scEditDisplay.DisplayGrid := Settings.DisplayGrid ;

     scEditDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scEditDisplay.TUnits := Settings.TUnits ;
     scEditDisplay.Invalidate ;
     
     end ;

procedure TSingleChanAnalFrm.edEventKeyPress(Sender: TObject; var Key: Char);
{ -----------------------------
  Go to user-entered event no.
  ----------------------------- }
begin
     if key = chr(13) then begin
        sbEvent.Position := Round(edEvent.LoValue) ;
        DisplayEvent ;
        end ;
     end;


// ****************************
// Amplitude histogram methods
// ****************************

procedure TSingleChanAnalFrm.InitialiseAmpHist ;
{ ----------------------------------------------------------
  Amplitude histogram initialisations when form is displayed
  ----------------------------------------------------------}
var
   ch : Integer ;
   AmplitudeRange : Single ;
begin

     // Create list of amplitude histogram options
     cbAmpHistType.Clear ;
     cbAmpHistType.Items.AddObject('All Points', TObject(htAllPoints) ) ;
     cbAmpHistType.Items.AddObject('All Points in State',TObject(htAllPointsInState)) ;
     cbAmpHistType.Items.AddObject('Mean State Amplitude',TObject(htStateAverage) ) ;
     cbAmpHistType.Items.AddObject('Patlak Average', TObject(htPatlakAverage) ) ;
     cbAmpHistType.Items.AddObject('Cursor Measurements (avg)', TObject(htCursorAvg) ) ;
     cbAmpHistType.Items.AddObject('External', TObject(htAmplitudesFile) ) ;
     cbAmpHistType.ItemIndex := 0 ;
     AmpHistTypePage.PageIndex := AllPointsPage ;

     // Time range to be included in histogram
     edAHRange.Scale := CdrFH.dt ;
     edAHRange.LoLimit := 0.0 ;
     edAHRange.HiLimit := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
     edAHRange.HiValue := Min(Settings.DwellTimes.SampleRangeHi,edAHRange.HiLimit) ;
     edAHRange.LoValue := Min(Settings.DwellTimes.SampleRangeLo,edAHRange.HiLimit) ;
     if edAHRange.LoValue >= edAHRange.HiValue then edAHRange.LoValue := 0.0 ;

     edAHRange.Units := 's' ;

     { Fill data source channel selection list }
     cbAmpHistChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
         cbAmpHistChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
         end ;
     cbAmpHistChannel.ItemIndex := ChanNum ;

     { Create channel state options list }
     cbChannelState.Clear ;
     cbChannelState.Items.AddObject('All',TObject(-1)) ;    // State = -1
     cbChannelState.Items.AddObject('Closed',TObject(0)) ; // State = 0
     cbChannelState.Items.AddObject('Open',TObject(1)) ;   // State = 1
     cbChannelState.ItemIndex := 0 ;

     { Number of bins in histogram }

     edAHNumBins.HiLimit := High(AmpHist.Bins) + 1 ;
     AmplitudeRange := (2*Channel[0].ADCMaxValue + 1)*Channel[ChanNum].ADCScale ;
     edAHBinWidth.LoLimit := Max( Channel[ChanNum].ADCScale,
                                       AmplitudeRange/High(AmpHist.Bins)) ;
     edAHBinWidth.LoLimit := Channel[ChanNum].ADCScale ;
     edAHBinWidth.HiLimit := AmplitudeRange / 4.0 ;
    // edAmpHistBinWidth.Value := Channel[ChanNum].ADCScale*4 ;
     edAHBinWidth.Units := Channel[ChanNum].ADCUnits ;

     edAHBinsLower.Units := Channel[ChanNum].ADCUnits ;
     edAHBinsUpper.Units := Channel[ChanNum].ADCUnits ;
     edAHUnitCurrent.Units := Channel[ChanNum].ADCUnits ;
     edAHUnitCurrent.Value := Settings.DwellTimes.UnitCurrent ;

     { Initialise display cursors }
     plAmpHist.ClearVerticalCursors ;
     AmpCurs.IUnit := plAmpHist.AddVerticalCursor( clred, 'unit-c',1 ) ;
     AmpCurs.C0 := plAmpHist.AddVerticalCursor( clGray, '',0 ) ;
     AmpCurs.C1 := plAmpHist.AddVerticalCursor( clGray, '',0 ) ;
     AmpCurs.Read := plAmpHist.AddVerticalCursor( clGreen, '?r',0 ) ;

     { Create list of curves that can be fitted to amplitude histogram }
     cbAmpHistEqn.Clear ;
     cbAmpHistEqn.Items.AddObject( 'None', TObject(None)) ;
     cbAmpHistEqn.Items.AddObject( 'Gaussian', TObject(Gaussian)) ;
     cbAmpHistEqn.Items.AddObject( '2 Gaussians', TObject(Gaussian2)) ;
     cbAmpHistEqn.Items.AddObject( '3 Gaussians', TObject(Gaussian3)) ;
     { Set initial  equation to None }
     cbAmpHistEqn.ItemIndex := 0 ;

     bAbortAmpHist.Enabled := False ;
     bAmpHistSetAxes.Enabled := False ;
     bSetZero.Enabled := False ;
     AmpHist.Available := False ;
     bAmpFitCurve.Enabled := False ;

     // Clear all existing lines on plot }
     plAmpHist.MaxPointsPerLine := High(AmpHist.Bins) + 1 ;
     plAmpHist.ClearPLot ;
     plAmpHist.Invalidate ;

     end ;


procedure TSingleChanAnalFrm.AllPointsHistogram ;
{ ----------------------------
  Compute all-points histogram
  ----------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,NumBlocks : LongInt ;
   y,BinScale : single ;
   iBin,j,i : Integer ;
   Done : Boolean ;
   ADC : Array[0..NumBlocksPerBuffer*MaxChannels-1] of SmallInt ;
begin

    { Read records of data from file and add to histogram }
    BinScale := 1.0 / edAHBinWidth.Value ;

    BlockPointer := AmpHist.StartAt ;
    Done := False ;
    while not Done do begin

       { Read a record from file and add its data to histogram }
       NumBlocks := ReadCDRBuffer(CdrFH,BlockPointer,ADC,NumBlocksPerBuffer) ;

       if NumBlocks = NumBlocksPerBuffer then begin
          j := Channel[ChanNum].ChannelOffset ;
          for i := 0 to NumBlocksPerBuffer-1 do begin
              { Get amplitude value }
              y := (ADC[j] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
              { Find index of histogram bin }
              iBin := Round( (y - AmpHist.RangeLo)*BinScale ) ;
              iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
              { Increment bin count }
              AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
              j := j + CdrFH.NumChannels ;
              end ;
          end
       else Done := True ;

       { Report progress }
       Main.StatusBar.SimpleText := format(
       'Single-channel Analysis : Compiling histogram %.1f/%.1f',
       [BlockPointer*CDRFH.dt,AmpHist.EndAt*CDRFH.dt]) ;

       { Increment to next record }
       BlockPointer := BlockPointer + NumBlocksPerBuffer ;
       if BlockPointer > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    { Report progress }
    Main.StatusBar.SimpleText := '' ;

    end ;


procedure TSingleChanAnalFrm.AllPointsInStateHistogram ;
{ ----------------------------
  Compute all-points histogram
  ----------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,SamplePointer,LastSampleInBuffer,EventNum : Integer ;
   y,BinScale : single ;
   iBin,MarginPoints,SelectedState : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
   ADC : Array[0..NumBlocksPerBuffer*MaxChannels-1] of SmallInt ;
   begin

    { Get channel state to be used }
//    SelectedState := Integer(cbChannelState.Items.Objects[cbChannelState.ItemIndex]) ;
    SelectedState := cbChannelState.ItemIndex - 1 ;
    MarginPoints := Round(edMarginPoints.Value) ;
    LastSampleInBuffer := NumBlocksPerBuffer*CdrFH.NumChannels - 1 ;

    BinScale := 1.0/edAHBinWidth.Value ;

    { Read records of data from file and add to histogram }
    Done := False ;
    EventNum := AmpHist.StartAt ;
    while not Done do begin

       { Read event }
       ReadEventFromFile( EventFile, EventNum, Event ) ;
       Done := not Event.Available ;

       { Extract samples associated with event and add to histogram }
       if (not Event.Ignore)
          and ((Event.ChannelState = SelectedState) or (SelectedState < 0))
          and ((Event.EndAt - Event.StartAt +1) >= (MarginPoints*2)) then begin

          { Remove margin points }
          Event.StartAt := Event.StartAt + MarginPoints ;
          Event.EndAt := Event.EndAt - MarginPoints ;

          BlockPointer := Event.StartAt ;
          NewBufferNeeded := True ;
          SamplePointer := Channel[ChanNum].ChannelOffset ;
          while BlockPointer <= Event.EndAt do begin

              { Get A/D samples from file }
              if NewBufferNeeded then begin
                 ReadCDRBuffer(CdrFH,BlockPointer,ADC,NumBlocksPerBuffer) ;
                 NewBufferNeeded := False ;
                 SamplePointer := Channel[ChanNum].ChannelOffset ;
                 end ;

              { Get amplitude value }
              y := (ADC[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;

              { Find index of histogram bin }
              iBin := Round( (y - AmpHist.RangeLo)*BinScale ) ;
              iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;

              { Increment bin count }
              AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
              SamplePointer := SamplePointer + CdrFH.NumChannels ;

              // Next sample
              if SamplePointer > LastSampleInBuffer then NewBufferNeeded := True ;
              Inc(BlockPointer) ;

              end ;
          end ;

       { Report progress }
       Main.StatusBar.SimpleText := format(
       'Single-channel Analysis : Compiling histogram %d/%d',
       [EventNum,AmpHist.EndAt]) ;

       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    // Clear status bar
    Main.StatusBar.SimpleText := '' ;

    end ;


procedure TSingleChanAnalFrm.StateAverageHistogram ;
{ ------------------------------------------
  Compute average current amplitude by state
  ------------------------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   EventNum : Integer ;
   BinScale : single ;
   iBin,SelectedState : Integer ;
   Done : Boolean ;
   AverageAmplitude, StDev : Single ;
   Event : TEvent ;
begin

    { Get channel state to be used }
    SelectedState := cbChannelState.ItemIndex - 1 ;

    // Histogram bin selection factor
    BinScale := 1.0/edAHBinWidth.Value ;

    { Read records of data from file and add to histogram }
    Done := False ;
    EventNum := AmpHist.StartAt ;
    while not Done do begin

       { Read event }
       ReadEventFromFile( EventFile, EventNum, Event ) ;
       Done := not Event.Available ;

       { Extract samples associated with event and add to histogram }
       if (not Event.Ignore)
          and ((Event.ChannelState = SelectedState) or (SelectedState < 0)) then begin

          // Calculate average and add to histogram
          if AverageEventAmplitude( Event, AverageAmplitude, StDev ) then begin
             { Find index of histogram bin }
             iBin := Round( (AverageAmplitude - AmpHist.RangeLo)*BinScale ) ;
             iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
             { Increment bin count }
             AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
             end ;
          end ;

       { Report progress }
       Main.StatusBar.SimpleText := format(
       'Single-channel Analysis : Compiling histogram %.d/%d',
       [EventNum,AmpHist.EndAt]) ;

       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    // Clear status bar
    Main.StatusBar.SimpleText := '' ;

    end ;


function TSingleChanAnalFrm.AverageEventAmplitude(
         Event : TEvent ;
         var AverageAmplitude : Single ;
         var StDev : Single
         ) : Boolean ;
// ---------------------------------
// Calculate average event amplitude
// ---------------------------------
var
    ADC : ^TSmallIntArray ;
    NumScans : Integer ;
    i,j : Integer ;
    Sum,Avg : Double ;
begin

     { Remove margin points }
     Event.StartAt := Event.StartAt + Round(edMarginPoints.Value) ;
     Event.EndAt := Event.EndAt - Round(edMarginPoints.Value) ;

     if Event.EndAt >= Event.StartAt then begin

        NumScans := Event.EndAt - Event.StartAt + 1 ;
        GetMem( ADC, NumScans*CDRFH.NumChannels*2 ) ;

        // Read data from file
        ReadCDRBuffer(CdrFH,Event.StartAt,ADC^,NumScans) ;

        // Calculate average
        Sum := 0.0 ;
        j := 0 ;
        for i := 0 to NumScans-1 do begin
            Sum := Sum + ADC^[j] ;
            j := j + CDRFH.NumChannels ;
            end ;
        Avg := Sum/NumScans;
        AverageAmplitude := (Avg - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale;

        // Calculate standard deviation
        Sum := 0.0 ;
        j := 0 ;
        for i := 0 to NumScans-1 do begin
            Sum := Sum + (ADC^[j] - Avg)*(ADC^[j] - Avg) ;
            j := j + CDRFH.NumChannels ;
            end ;
        if (NumScans > 1) and (Sum > 0.0) then begin
           StDev := SQRT(Sum/(NumScans-1))*Channel[ChanNum].ADCScale ;
           end
        else StDev := 0.0 ;

        Result := True ;
        FreeMem(ADC) ;

        end
     else begin
        Result := False ;
        AverageAmplitude := 0.0 ;
        StDev := 0.0 ;
        end ;
     end ;


procedure TSingleChanAnalFrm.PatlakAverageHistogram ;
{ --------------------------------------
  Compute Patlak variance-mean histogram
  --------------------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   NumSamplesPerBuffer : LongInt ;
   SamplePointer,BlockPointer,iRing,NumAvg : Integer ;
   Avg,Variance,Sum,SumSquares,VarianceThreshold : single ;
   y,BinScale : single ;
   iBin : Integer ;
   FirstBuffer,Done : Boolean ;
   Ring : Array[0..255] of Single ;
   ADC : Array[0..NumBlocksPerBuffer*MaxChannels-1] of SmallInt ;
begin

    { Determine number of bytes to be read from file for each buffer }
    NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

    { Number of samples to average }
    NumAvg := IntLimitTo(Round(edNumPatlakAvg.Value),1,High(Ring)+1) ;
    { Variance acceptance threshold for inclusion in histogram }
    VarianceThreshold := edPatlakSDLimit.Value*edPatlakSDLimit.Value ;

    BinScale := 1.0/edAHBinWidth.Value ;

    { Read records of data from file and add to histogram }
    Done := False ;
    SamplePointer := NumSamplesPerBuffer ;
    BlockPointer := AmpHist.StartAt ;
    FirstBuffer := True ;
    Sum := 0.0 ;
    SumSquares := 0.0 ;
    iRing := 0 ;
    while not Done do begin
        { Get new buffer of samples, if needed }
        if SamplePointer >= NumSamplesPerBuffer then begin
           if ReadCDRBuffer(CdrFH,BlockPointer,ADC,NumBlocksPerBuffer)
              <> NumBlocksPerBuffer then Done := True ;
           SamplePointer := Channel[ChanNum].ChannelOffset ;

           { Report progress }
           Main.StatusBar.SimpleText := format(
           'Single-channel Analysis : Compiling histogram %.1f/%.1f',
           [BlockPointer*CDRFH.dt,AmpHist.EndAt*CDRFH.dt]) ;

           { Allow other events to be serviced }
           Application.ProcessMessages ;
           end ;

        if FirstBuffer then begin
           Sum := 0.0 ;
           SumSquares := 0.0 ;
           for iRing := 0 to NumAvg-1 do begin
               y := (ADC[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
               Ring[iRing] := y ;
               Sum := Sum + y ;
               SumSquares := SumSquares + y*y ;
               SamplePointer := SamplePointer + CdrFH.NumChannels ;
               end ;
           FirstBuffer := False ;
           iRing := NumAvg-1 ;
           end
        else begin
           Inc(iRing) ;
           if iRing = NumAvg then iRing := 0 ;
           Sum := Sum - Ring[iRing] ;
           SumSquares := SumSquares - Ring[iRing]*Ring[iRing] ;
           y := (ADC[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
           Sum := Sum + y ;
           SumSquares := SumSquares + y*y ;
           Ring[iRing] := y ;
           SamplePointer := SamplePointer + CdrFH.NumChannels ;
           end ;

        { Calculate mean and variance of segment }
        Avg := Sum / NumAvg ;
        Variance := (SumSquares - NumAvg*Avg*Avg) / (NumAvg-1) ;

        { Add average to histogram if variance low enough }
        if (Variance <= VarianceThreshold) and (not Done) then begin
           iBin := Round( (Avg - AmpHist.RangeLo)*BinScale ) ;
           iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
           { Increment bin count }
           AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
           end ;

       { Increment to next block of samples }
       Inc(BlockPointer) ;
       if BlockPointer > AmpHist.EndAt then Done := True ;

       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    // Clear status bar
    Main.StatusBar.SimpleText := '' ;

    end ;


procedure TSingleChanAnalFrm.bNewAmpHistClick(Sender: TObject);
{ -------------------------------
  Create and plot a new histogram
  -------------------------------}
var
   HistType : TAmpHistType ;
   x,BinWidth,Sum : single ;
   i,iBin : Integer ;
   iStart : Integer ;
   iEnd : Integer ;
begin

     bNewAmpHist.Enabled := False ;
     bAbortAmpHist.Enabled := True ;

     Screen.Cursor := crHourGlass ;
     { Ensure all text box parameters are up to date }

     Settings.DwellTimes.SampleRangeLo := Round(edAHRange.LoValue) ;
     Settings.DwellTimes.SampleRangeHi := Round(edAHRange.HiValue) ;

     if rbAmpWholeFile.Checked then begin
        AmpHist.StartAt := Round( edAHRange.LoLimit ) ;
        AmpHist.EndAt := Round( edAHRange.HiLimit ) ;
        end
     else begin
        AmpHist.StartAt := Settings.DwellTimes.SampleRangeLo ;
        AmpHist.EndAt :=   Settings.DwellTimes.SampleRangeHi ;
        end ;

          // Clear histogram and set up bin ranges
     if not AHBinRangePanel.Visible then begin
        AmpHist.NumBins := Round(edAHNumBins.Value) ;

        AmpHist.RangeLo := (-Channel[0].ADCMaxValue -1
                         - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
        edAHBinsLower.Value := AmpHist.RangeLo ;

        AmpHist.RangeHi := (Channel[0].ADCMaxValue
                          - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
        edAHBinsUpper.Value := AmpHist.RangeHi ;

        edAHBinWidth.Value := (AmpHist.RangeHi - AmpHist.RangeLo) / AmpHist.NumBins ;
        AHBinRangePanel.Visible := True ;
        edAHBinWidth.Units := Channel[ChanNum].ADCUnits ;
        edAHBinsLower.Units := edAHBinWidth.Units ;
        edAHBinsUpper.Units := edAHBinWidth.Units ;
        end
     else begin
        // Ensure user-entered values are updated
        edAHNumBins.Value := edAHNumBins.Value ;
        edAHBinWidth.Value :=  (edAHBinsUpper.Value - edAHBinsLower.Value) / edAHNumBins.Value ;
        end ;

     if edAHBinsUpper.Value <= edAHBinsLower.Value then
        edAHBinsUpper.Value := edAHBinsLower.Value + 1.0 ;

     AmpHist.RangeLo :=  edAHBinsLower.Value ;
     edAHBinsLower.Value := AmpHist.RangeLo ;  // Ensure units is updated
     AmpHist.RangeHi :=  edAHBinsUpper.Value ;
     edAHBinsUpper.Value := AmpHist.RangeHi ;     // Ensure units is updated
     edAHBinWidth.Value := (edAHBinsUpper.Value - edAHBinsLower.Value)/Round(edAHNumBins.Value) ;
     AmpHist.BinWidth := edAHBinWidth.Value ;
     AmpHist.NumBins := Round(edAHNumBins.Value) ;

     { Get type of histogram }
     HistType := TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex]) ;

    { Initialise histogram record }
    x := AmpHist.RangeLo ;
    AmpHist.MaxBin := AmpHist.NumBins - 1 ;
    BinWidth := edAHBinWidth.Value ;
    for iBin := 0 to AmpHist.MaxBin do begin
        AmpHist.Bins[iBin].Lo := x ;
        AmpHist.Bins[iBin].Hi := x + BinWidth ;
        AmpHist.Bins[iBin].Mid := x + (BinWidth/2.0) ;
        AmpHist.Bins[iBin].y := 0.0 ;
        x := x + BinWidth
        end ;

     { Choose appropriate histogram computation procedure }
     case HistType of
          htAllPoints : AllPointsHistogram ;
          htAllPointsInState : AllPointsInStateHistogram ;
          htStateAverage : StateAverageHistogram ;
          htPatlakAverage : PatlakAverageHistogram ;
          htAmplitudesFile : ExternalAmplitudeHistogram ;
          htCursorAvg : CursorMeasurementsAverageHistogram ;
          end ;

    // Remove multiple zero bins from edges of histogram

    iStart := 0 ;
    while (AmpHist.Bins[iStart].y = 0.0) and (iStart < AmpHist.MaxBin) do Inc(iStart) ;
    iEnd := AmpHist.MaxBin ;
    while (AmpHist.Bins[iEnd].y = 0.0) and (iEnd > 0) do Dec(iEnd) ;
    if iEnd >= iStart then begin
       iStart := Max(iStart-1, 0) ;
       iEnd := Min(iEnd+1, AmpHist.MaxBin) ;
       AmpHist.MaxBin := 0 ;
       for iBin := iStart to iEnd do begin
           AmpHist.Bins[AmpHist.MaxBin] := AmpHist.Bins[iBin] ;
           Inc(AmpHist.MaxBin) ;
           end ;
       Dec(AmpHist.MaxBin) ;
       end ;

    { Convert to percentage }
    Sum := 0.0 ;
    for iBin := 0 to AmpHist.MaxBin do Sum := Sum + AmpHist.Bins[iBin].y ;
    if Sum > 0.0 then begin
       for iBin := 0 to AmpHist.MaxBin do
           AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y*(100.0/Sum) ;
       end ;

     AmpHist.NewPlot := True ;
     { Remove any fitted equation }
     AmpFunc.Setup(None,' ',' ') ;

     { Standard settings for mean/variance plot }

     Screen.Cursor := crDefault ;

     // Clear all existing lines on plot }
     plAmpHist.ClearAllLines ;

     { Plot new histogram }
     plAmpHist.xAxisAutoRange := False ;
     plAmpHist.xAxisMin := AmpHist.Bins[0].Lo ;
     plAmpHist.xAxisMax := AmpHist.Bins[AmpHist.MaxBin].Hi ;
     plAmpHist.XAxisTick := plAmpHist.TickSpacing( plAmpHist.xAxisMax - plAmpHist.xAxisMin) ;
     plAmpHist.yAxisAutoRange := True ;
     plAmpHist.xAxisLabel := cbAmpHistType.text + ' (' + Channel[ChanNum].ADCUnits + ')' ;
     plAmpHist.yAxisLabel := '%' ;
     plAmpHist.CreateHistogram( 0 ) ;

     for i := 0 to AmpHist.MaxBin do begin
         plAmpHist.AddBin( 0,
                                       AmpHist.Bins[i].Lo,
                                       AmpHist.Bins[i].Mid,
                                       AmpHist.Bins[i].Hi,
                                       AmpHist.Bins[i].y ) ;
         end ;
     { Erase any fitted line which might exist }
     cbAmpHistEqn.ItemIndex := 0 ;
     plAmpHist.CreateLine( 1, clRed, msNone, psSolid ) ;

     // Clear results fields
     AmpResults.Clear ;
     erAmpResults.Lines.Clear ;

     { Initial cursor positions }
     i := 0 ;
     while (AmpHist.Bins[i].y = 0.0) and (i < AmpHist.MaxBin) do Inc(i) ;
     plAmpHist.VerticalCursors[AmpCurs.C0] := AmpHist.Bins[i].Mid ;
     lbAmpHistC0.Visible := True ;

     i := AmpHist.MaxBin ;
     while (AmpHist.Bins[i].y = 0.0) and (i > 0) do Dec(i) ;
     plAmpHist.VerticalCursors[AmpCurs.C1] := AmpHist.Bins[i].Mid ;
     lbAmpHistC1.Visible := True ;
     plAmpHist.VerticalCursors[AmpCurs.Read] := AmpHist.Bins[AmpHist.MaxBin div 2].Mid ;

     // Keep signal zero level
     AmpHistADCZero := Channel[ChanNum].ADCZero ;

     plAmpHist.VerticalCursors[AmpCurs.IUnit] := Settings.DwellTimes.UnitCurrent ;

     { Enable copy and print menu items }
     Main.CopyAndPrintMenus( True, True ) ;

     bNewAmpHist.Enabled := True ;
     bAbortAmpHist.Enabled := False ;
     bAmpHistSetAxes.Enabled := True ;
     bSetZero.Enabled := True ;
     bAmpFitCurve.Enabled := True ;
     AmpHist.Available := False ;


     end;


procedure TSingleChanAnalFrm.plAmpHistCursorChange(Sender: TObject);
{ ---------------------------------------
  Update labels when plot cursors change
  ---------------------------------------}
var
   Lo,Mid,Hi,y : single ;
   iStart,iEnd,i : Integer ;
   XMean,XYSum,YSum : single ;
begin

     { Set Fitting/area cursor labels }
     plAmpHist.GetBin( 0, plAmpHist.FindNearestIndex(0,AmpCurs.C0), Lo, Mid, Hi, y ) ;
     lbAmpHistC0.Visible := True ;
     lbAmpHistC0.Left := plAmpHist.Left + plAmpHist.XToCanvasCoord( Mid ) ;
     plAmpHist.GetBin( 0, plAmpHist.FindNearestIndex(0,AmpCurs.C1), Lo, Mid, Hi, y ) ;
     lbAmpHistC1.Visible := True ;
     lbAmpHistC1.Left := plAmpHist.Left + plAmpHist.XToCanvasCoord( Mid ) ;

     { Calculate histogram area between cursors }
     iStart := plAmpHist.FindNearestIndex( 0, AmpCurs.C0 ) ;
     iEnd :=   plAmpHist.FindNearestIndex( 0, AmpCurs.C1 ) ;
     YSum := 0.0 ;
     XYSum := 0.0 ;
     for i := Min(iStart,iEnd) to Max(iStart,iEnd) do begin
         YSum := YSum + AmpHist.Bins[i].y ;
         XYSum := XYSum + AmpHist.Bins[i].y*AmpHist.Bins[i].Mid ;
         end ;
     if YSum <> 0.0 then XMean := XYSum / YSum
                    else XMean := 0.0 ;

     lbAmpHistArea.visible := true ;
     shAmpHistLine.visible := true ;
     lbAmpHistArea.caption := format(' Mean= %.3g %s /Area= %.3g %% ',
                             [XMean,
                             Channel[ChanNum].ADCUnits,
                              YSum] ) ;

     { Display mean signal level and histogram % between cursors }
     Mid := (lbAmpHistC0.Left + lbAmpHistC1.Left) div 2 ;
     lbAmpHistArea.Left := Min(((lbAmpHistC0.Left + lbAmpHistC1.Left) div 2)
                                    - (lbAmpHistArea.Width div 2),
                                    AmpHistTab.Width - lbAmpHistArea.Width);
     lbAmpHistArea.Top := lbAmpHistC0.Top ;
     lbAmpHistArea.Visible := True ;

     { Place horizontal line between fit/analysis cursors }
     shAmpHistLine.Top := lbAmpHistArea.Top + (lbAmpHistArea.Height div 2) ;
     shAmpHistLine.Left := Min(lbAmpHistC0.Left,lbAmpHistC1.Left)
                           + lbAmpHistC0.Width ;
     shAmpHistLine.Width := Max(lbAmpHistC0.Left,lbAmpHistC1.Left)
                            - shAmpHistLine.Left - lbAmpHistC0.Width ;
     shAmpHistLine.Visible := True ;

     // Ensure that unit-c cursor remains fixed
     //if plAmpHist.VerticalCursors[AmpCurs.IUnit] <> Settings.DwellTimes.UnitCurrent then
     Settings.DwellTimes.UnitCurrent := plAmpHist.VerticalCursors[AmpCurs.IUnit] ;
     edAHUnitCurrent.Value := Settings.DwellTimes.UnitCurrent ;

     end;


procedure TSingleChanAnalFrm.bSetZeroClick(Sender: TObject);
{ ----------------------
  Set zero current level
  ----------------------}
begin

     AdjustAmpHistZeroLevel( Round(plAmpHist.VerticalCursors[AmpCurs.Read]
                             /Channel[ChanNum].ADCScale) + AmpHistADCZero ) ;

     end ;


procedure TSingleChanAnalFrm.AdjustAmpHistZeroLevel(
          NewADCZero : Integer
          );
// --------------------------------------------------
// Replot amplitude histogram with updated zero level
// --------------------------------------------------
var
   OldZeroCurrent,ZeroCurrent,Shift : single ;
   i : Integer ;
begin

     // Calculate new zero current level (in A/D converter units)
     OldZeroCurrent := AmpHistADCZero*Channel[ChanNum].ADCScale ;

     // Update channel being analysed
     Channel[ChanNum].ADCZero := NewADCZero ;
     // Make sure it is saved in file header
     SaveCDRHeader( cdrFH ) ;
     // And View module zero levels are updated
     Main.UpdateViewSig ;

     // Calculate offset needed to correct AmpHist bins for new zero
     ZeroCurrent := NewADCZero*Channel[ChanNum].ADCScale ;
     Shift := OldZeroCurrent - ZeroCurrent ;

     // Update bins
     for i := 0 to AmpHist.MaxBin do begin
         AmpHist.Bins[i].Lo := AmpHist.Bins[i].Lo + Shift ;
         AmpHist.Bins[i].Mid := AmpHist.Bins[i].Mid + Shift ;
         AmpHist.Bins[i].Hi := AmpHist.Bins[i].Hi + Shift ;
         end ;

     plAmpHist.xAxisMin := plAmpHist.xAxisMin + Shift ;
     plAmpHist.xAxisMax := plAmpHist.xAxisMax + Shift ;

     // Re-plot updated histogram
     plAmpHist.CreateHistogram( 0 ) ;
     for i := 0 to AmpHist.MaxBin do plAmpHist.AddBin( 0,
                                       AmpHist.Bins[i].Lo,
                                       AmpHist.Bins[i].Mid,
                                       AmpHist.Bins[i].Hi,
                                       AmpHist.Bins[i].y ) ;

     AmpHistADCZero := NewADCZero ;

     end;


procedure TSingleChanAnalFrm.bSetUnitCurrentClick(Sender: TObject);
{ -----------------------------
  Set unitary current amplitude
  -----------------------------}
begin
     Settings.DwellTimes.UnitCurrent := plAmpHist.VerticalCursors[AmpCurs.Read] ;
     plAmpHist.VerticalCursors[AmpCurs.IUnit] := plAmpHist.VerticalCursors[AmpCurs.Read] ;
     plAmpHist.Invalidate ;
     end;


procedure TSingleChanAnalFrm.bAmpFitCurveClick(Sender: TObject);
{ ---------------------------------------------------
  Fit a gaussian prob. density functions to histogram
  ---------------------------------------------------
    25/6/98 Clipboard buffer limit reduced to 31000
    14/1/99 New MathFunc object used }
const
     NumFitPoints = 500 ;
var
   i,iStart,iEnd,nFit,iBins, Comp,LineNum,NumComp : Integer ;
   ParName : string ;
   Scale,BinWidth,x,dx : single ;
   ParTemp : Array[0..LastParameter] of single ;
   FitData : PXYData ;
   OK : Boolean ;
begin
     OK := True ;
     New( FitData ) ;
     Try

        { Clear all existing lines on plot }
        plAmpHist.ClearAllLines ;

        { Select type of equation to be fitted }
        AmpFunc.Setup( TEqnType(cbAmpHistEqn.Items.Objects[cbAmpHistEqn.ItemIndex]),
                       Channel[ChanNum].ADCUnits,
                       '%')  ;
        if AmpFunc.Equation = None then OK := False ;

        { Copy data into fitting array }
        if OK then begin

           nFit := 0 ;
           { Lower and upper x data limit set by display cursors }
           iStart := plAmpHist.FindNearestIndex( 0, AmpCurs.C0 ) ;
           iEnd :=   plAmpHist.FindNearestIndex( 0, AmpCurs.C1 ) ;
           for iBins := Min(iStart,iEnd) to Max(iStart,iEnd) do begin
               FitData^.x[nFit] := AmpHist.Bins[iBins].Mid ;
               FitData^.y[nFit] := AmpHist.Bins[iBins].y ;
               Inc(nFit) ;
               end ;

           { Abort curve fit, if not enough data points }
           if nFit < AmpFunc.NumParameters then begin
              ShowMessage( format('%d points is insufficient for fit',[nFit]) ) ;
              AmpFunc.Setup( None, ' ',' ' ) ;
              OK := False ;
              end ;
           end ;

        if OK then begin
           { Let user create/modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := AmpFunc ;
           SetFitParsFrm.XYData := FitData ;
           SetFitParsFrm.NumPoints := nFit ;
           SetFitParsFrm.Left := SingleChanAnalFrm.Left + Main.Left + 50 ;
           SetFitParsFrm.top := SingleChanAnalFrm.Top + Main.Top + 50 ;
           SetFitParsFrm.ShowModal ;
           if SetFitParsFrm.ModalResult <> mrOK then OK := False ;
           end ;

        { Fit curve using non-linear regression }
        if OK then begin
           AmpFunc := SetFitParsFrm.MathFunc ;
           { Prevent FitCurve from changing parameter settings }
           AmpFunc.ParametersSet := True ;
           AmpFunc.UseBinWidths := False ;
           AmpFunc.FitCurve( FitData^, nFit ) ;
           OK := AmpFunc.GoodFit ;
           end ;

        { Plot equation on graph }
        if OK and (AmpFunc.Equation <> None) then begin

           x := plAmpHist.xAxisMin ;
           dx := (plAmpHist.xAxisMax - plAmpHist.xAxisMin) / NumFitPoints ;
           plAmpHist.ShowLines := True ;
           plAmpHist.CreateLine( FittedLine, clRed, msNone, psSolid ) ;
           for i := 0 to NumFitPoints-1 do begin
               plAmpHist.AddPoint( FittedLine, x, AmpFunc.Value(x) ) ;
               x := x + dx ;
               end ;

           { Save parameters and initialise gaussian component lines }
           NumComp := AmpFunc.NumParameters div 3 ;
           LineNum := FittedLine ;
           for Comp := 0 to NumComp-1 do begin
               Inc(LineNum) ;
               plAmpHist.CreateLine( LineNum, clRed, msNone, psSolid ) ;
               ParTemp[Comp*3] := AmpFunc.Parameters[Comp*3] ;
               ParTemp[Comp*3+1] := AmpFunc.Parameters[Comp*3+1] ;
               ParTemp[Comp*3+2] := AmpFunc.Parameters[Comp*3+2] ;
               AmpFunc.Parameters[Comp*3+2] := 0.0 ;
               end ;

           { Plot each individual gaussian component }
           if NumComp > 1 then begin
              LineNum := FittedLine ;
              for Comp := 0 to NumComp-1 do begin
                  AmpFunc.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
                  Inc(LineNum) ;
                  x := plAmpHist.xAxisMin ;
                  dx := (plAmpHist.xAxisMax - plAmpHist.xAxisMin) / NumFitPoints ;
                  for i := 0 to AmpHist.NumBins-1 do begin
                      plAmpHist.AddPoint( LineNum, x, AmpFunc.Value(x) ) ;
                      x := x + dx ;
                      end ;
                  AmpFunc.Parameters[Comp*3+2] := 0.0 ;
                  end ;
              end ;

           { Restore parameters }
           for Comp := 0 to NumComp-1 do begin
               AmpFunc.Parameters[Comp*3] := ParTemp[Comp*3] ;
               AmpFunc.Parameters[Comp*3+1] := ParTemp[Comp*3+1] ;
               AmpFunc.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
               end ;
           end ;


        { Display results }
        AmpResults.Clear ;
        if OK then begin

          case AmpFunc.Equation of
              Gaussian : AmpResults.Add(
                ' y(x) = (a/^!(2^sp^ss^+2))*exp(-(x-^sm)^+2/(2*^ss^+2) )') ;
              Gaussian2 : AmpResults.Add(
                ' y(x) = ^sS^-i^-=^-1^-.^-.^-2 (a^-i/^!(2^sp^ss^-i^+2))*exp(-(x-^sm^-i)^+2/(2*^ss^-i^+2) )') ;
              Gaussian3 : AmpResults.Add(
                ' y(x) = ^sS^-i^-=^-1^-.^-.^-3 (a^-i/^!(2^sp^ss^-i^+2))*exp(-(x-^sm^-i)^+2/(2*^ss^-i^+2) )') ;
              end ;

           { Best fit parameters and standard error }
           for i := 0 to AmpFunc.NumParameters-1 do begin

               { Convert gaussian peak parameter to gaussian area }
               if ((i+1) mod 3) = 0 then begin
                  ParName := format('A^-%d',[(i div 3)+1]) ;
                  BinWidth := (AmpHist.RangeHi - AmpHist.RangeLo) /
                               AmpHist.NumBins ;
                  Scale := (sqrt(6.2831)*AmpFunc.Parameters[i-1])/BinWidth ;
                  end
               else begin
                  ParName := AmpFunc.ParNames[i] ;
                  Scale := 1.0 ;
                  end ;

               if not AmpFunc.FixedParameters[i] then
                  AmpResults.Add( format(' %s = %.4g ^~ %.4g (sd) %s',
                                       [ParName,
                                        Scale*AmpFunc.Parameters[i],
                                        Scale*AmpFunc.ParameterSDs[i],
                                        AmpFunc.ParUnits[i]] ) )
               else
                  { Fixed parameter }
                  AmpResults.Add( format(' %s = %.4g (fixed) %s',
                                       [ParName,
                                        AmpFunc.Parameters[i],
                                        AmpFunc.ParUnits[i]] ) ) ;
               end ;

           { Residual standard deviation }
           AmpResults.Add( format(' Residual S.D. = %.4g %s',
                                [AmpFunc.ResidualSD,'%'] ) ) ;

           { Statistical degrees of freedom }
           AmpResults.Add( format(' Degrees of freedom = %d ',
                                [AmpFunc.DegreesOfFreedom]) );

           { No. of iterations }
           AmpResults.Add( format(' No. of iterations = %d ',
                                [AmpFunc.Iterations]) ) ;

           AmpFunc.CopyResultsToRichEdit( AmpResults, erAmpResults ) ;
           end ;

     finally
            Dispose(FitData) ;
            end ;

     { Make sure plot is updated with changes }
     plAmpHist.Invalidate ;

     end ;


procedure TSingleChanAnalFrm.ExternalAmplitudeHistogram ;
{ ----------------------------------------------------------
  Compute histogram from list of amplitudes in external file
  ---------------------------------------------------------- }
var
   Value : double ;
   InFile : TextFile ;
   iBin : Integer ;
   x : Single ;
   BinWidth,BinScale : Single ;
begin

    { Read records of data from file and add to histogram }
     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     OpenDialog.Title := 'Open amplitudes file';

     if not OpenDialog.execute then Exit ;

     // Open file
     AssignFile( InFile, OpenDialog.FileName ) ;

     // Determine min./max. limits
     Reset( InFile ) ;
     AmpHist.RangeLo := 1E30 ;
     AmpHist.RangeHi := -1E30 ;
     while not EOF( InFile ) do begin
         ReadLn( InFile, Value ) ;
         if AmpHist.RangeLo > Value then AmpHist.RangeLo := Value ;
         if AmpHist.RangeHi < Value then AmpHist.RangeHi := Value ;
         end ;

     AmpHist.RangeLo := AmpHist.RangeLo - 0.1*Max(Abs(AmpHist.RangeLo),Abs(AmpHist.RangeHi));
     AmpHist.RangeHi := AmpHist.RangeHi + 0.1*Max(Abs(AmpHist.RangeLo),Abs(AmpHist.RangeHi));

     AmpHist.NumBins := Round((AmpHist.RangeHi - AmpHist.RangeLo) / edAHBinWidth.Value) ;
     if AmpHist.NumBins > (High(AmpHist.Bins)+1) then begin
        AmpHist.NumBins := High(AmpHist.Bins)+1 ;
        edAHBinWidth.Value := (AmpHist.RangeHi - AmpHist.RangeLo) / AmpHist.NumBins ;
        end ;
     AmpHist.MaxBin := AmpHist.NumBins - 1 ;

     { Initialise histogram record }
     x := AmpHist.RangeLo ;
     BinWidth := edAHBinWidth.Value ;
     for iBin := 0 to AmpHist.MaxBin do begin
        AmpHist.Bins[iBin].Lo := x ;
        AmpHist.Bins[iBin].Hi := x + BinWidth ;
        AmpHist.Bins[iBin].Mid := x + (BinWidth/2.0) ;
        AmpHist.Bins[iBin].y := 0.0 ;
        x := x + BinWidth
        end ;

    BinScale := 1.0/edAHBinWidth.Value ;

     // Fill histogram
     Reset( InFile ) ;
     while not EOF( InFile ) do begin
         ReadLn( InFile, Value ) ;
         iBin := Round( (Value - AmpHist.RangeLo)*BinScale ) ;
         //iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
         AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
         end ;

     CloseFile( InFile ) ;

     end ;


procedure TSingleChanAnalFrm.CursorMeasurementsAverageHistogram ;
{ ----------------------------------------------------------
  Compute histogram from list of amplitudes in external file
  ---------------------------------------------------------- }
var
   Value : double ;
   i,iBin : Integer ;
   x : Single ;
   BinWidth : Single ;
begin

     if AmpHist.StartAt >= AmpHist.EndAt then Exit ;

     // Determine min./max. limits
     AmpHist.RangeLo := 1E30 ;
     AmpHist.RangeHi := -1E30 ;
     for i := AmpHist.StartAt to AmpHist.EndAt do begin
         Value := CursorMeasurements[i-1].AverageCurrent ;
         if AmpHist.RangeLo > Value then AmpHist.RangeLo := Value ;
         if AmpHist.RangeHi < Value then AmpHist.RangeHi := Value ;
         end ;

     AmpHist.NumBins := Round((AmpHist.RangeHi - AmpHist.RangeLo) / edAHBinWidth.Value) ;
     if AmpHist.NumBins > (High(AmpHist.Bins)+1) then begin
        AmpHist.NumBins := High(AmpHist.Bins)+1 ;
        edAHBinWidth.Value := (AmpHist.RangeHi - AmpHist.RangeLo) / AmpHist.NumBins ;
        end ;
     AmpHist.MaxBin := AmpHist.NumBins - 1 ;

     { Initialise histogram record }
     x := AmpHist.RangeLo ;
     BinWidth := edAHBinWidth.Value ;

     for iBin := 0 to AmpHist.MaxBin do begin
        AmpHist.Bins[iBin].Lo := x ;
        AmpHist.Bins[iBin].Hi := x + BinWidth ;
        AmpHist.Bins[iBin].Mid := x + (BinWidth/2.0) ;
        AmpHist.Bins[iBin].y := 0.0 ;
        x := x + BinWidth
        end ;

     // Fill histogram
     for i := AmpHist.StartAt to AmpHist.EndAt do begin
         Value := CursorMeasurements[i-1].AverageCurrent ;
         iBin := Round( (Value - AmpHist.RangeLo)/BinWidth{*AmpHist.BinScale} ) ;
         iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
         AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
         end ;

     end ;



procedure TSingleChanAnalFrm.bAmpHistSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plAmpHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TSingleChanAnalFrm.bAbortAmpHistClick(Sender: TObject);
// ----------------------------------------------
// Amplitude histogram operation has been aborted
// ----------------------------------------------
begin
     { Re-enable New Histogram button ... This signals that the
       currently active histogram generation run is to be aborted }
     bNewAmpHist.Enabled := True ;
     bAbortAmpHist.Enabled := False ;
     end;


procedure TSingleChanAnalFrm.cbAmpHistTypeChange(Sender: TObject);
//
// Update associated controls when amplitude histogram type changed
//
begin

     if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htAllPoints then begin
        edAHRange.LoLimit := 0.0 ;
        edAHRange.Scale := CdrFH.dt ;
        edAHRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAHRange.Units := 's' ;
        AmpHistTypePage.PageIndex := AllPointsPage ;
        bUseCursorsForAmpHistRange.Enabled := True ;
        end
     else if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htAmplitudesFile then begin
        edAHRange.LoLimit := 0.0 ;
        edAHRange.Scale := CdrFH.dt ;
        edAHRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAHRange.Units := 's' ;
        AmpHistTypePage.PageIndex := AllPointsPage ;
        bUseCursorsForAmpHistRange.Enabled := True ;
        end
     else if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htPatlakAverage then begin
        edAHRange.LoLimit := 0.0 ;
        edAHRange.Scale := CdrFH.dt ;
        edAHRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAHRange.Units := 's' ;
        AmpHistTypePage.PageIndex := PatlakAveragePage ;
        edPatlakSDLimit.Units := Channel[ChanNum].ADCUnits ;
        bUseCursorsForAmpHistRange.Enabled := True ;
        end
     else if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htCursorAvg then begin
        edAHRange.LoLimit := 1 ;
        edAHRange.Scale := 1.0 ;
        edAHRange.HiLimit := NumCursorMeasurements ;
        edAHRange.Units := '' ;
        AmpHistTypePage.PageIndex := AllPointsPage ;
        bUseCursorsForAmpHistRange.Enabled := False ;
        end
     else begin
        edAHRange.LoLimit := 1 ;
        edAHRange.Scale := 1.0 ;
        edAHRange.HiLimit := EventFile.NumEvents ;
        edAHRange.Units := '' ;
        AmpHistTypePage.PageIndex := EventHistPage ;
        bUseCursorsForAmpHistRange.Enabled := False ;
        end ;

     edAHRange.LoValue := edAHRange.LoLimit ;
     edAHRange.hiValue := edAHRange.HiLimit ;

     AHBinRangePanel.Visible := False ;

     end;


procedure TSingleChanAnalFrm.bExportEventListClick(Sender: TObject);
{ --------------------------------------------
  Export channel transition event data to file
  -------------------------------------------- }
var
   OutFile : TextFile ;
   EventNum : Integer ;
   Event : TEvent ;
   FileNameEnding : string ;
begin

     // Get the name of a data file to hold ezported data
     SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := '.txt' ;
     SaveDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     SaveDialog.Title := 'Save data file';

     // Default file name
     SaveDialog.FileName := ChangeFileExt( CDRFH.FileName, '.txt' ) ;
     if rbClosedTimes.Checked then FileNameEnding := ' (closed times).txt'
     else if rbOpenTimes.Checked then  FileNameEnding := ' (open times).txt'
     else FileNameEnding := ' (event list).txt' ;
     SaveDialog.FileName := ANSIReplaceStr( SaveDialog.FileName,
                                            '.txt',
                                            FileNameEnding ) ;

     if SaveDialog.execute then begin
        // Open text file
        AssignFile( OutFile, SaveDialog.FileName ) ;
        ReWrite( OutFile ) ;
        // Write data to file
        for EventNum := 1 to EventFile.NumEvents do begin
            // Read event
            ReadEventFromFile( EventFile, EventNum, Event ) ;

            if not Event.Ignore then begin
               if rbClosedTimes.Checked and (Event.ChannelState = 0) then
                  WriteLn(OutFile, format( '%.6g', [Event.Duration] ))
               else if rbOpenTimes.Checked and (Event.ChannelState > 0) then
                  WriteLn(OutFile, format( '%.6g', [Event.Duration] ))
               else if rbEventList.Checked then
                  WriteLn(OutFile, format( '%.6g%s%d',
                           [Event.Duration,chr(9),Event.ChannelState] )) ;
               end ;
            end ;
        CloseFile( OutFile ) ;
        end ;
    end ;



procedure TSingleChanAnalFrm.bAbortStabPlotClick(Sender: TObject);
begin
     // Disabling Abort button to indicate
     // that current stability plot method should be aborted
     bAbortStabPlot.Enabled := False ;
     end;

procedure TSingleChanAnalFrm.bFTestClick(Sender: TObject);
//
// Display F-Test dialog box
//
begin
     FTestFrm.ClearPrinterTitleLines ;
     FTestFrm.AddPrinterTitleLine( 'File ... ' + CdrFH.FileName ) ;
     FTestFrm.AddPrinterTitleLine( CdrFH.IdentLine ) ;
     FTestFrm.AddPrinterTitleLine( 'Histogram : ' + cbDwellTHistType.Text ) ;
     FTestFrm.ShowModal ;
     end;


procedure TSingleChanAnalFrm.bSaveDwellTHIstClick(Sender: TObject);
{ --------------------------------------------
  Export dwell time histogram data
  -------------------------------------------- }
var
   OutFile : TextFile ;
   iBin : Integer ;

begin

     // Get the name of a data file to hold ezported data
     SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := '.txt' ;
     SaveDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     SaveDialog.Title := 'Save histogram data';

     SaveDialog.FileName := ChangeFileExt( CDRFH.FileName, '.txt' ) ;
     SaveDialog.FileName := ANSIReplaceStr( SaveDialog.FileName,
                                            '.txt',
                                            ' (dwell time histogram).txt' ) ;

     if SaveDialog.execute then begin
        // Open text file
        AssignFile( OutFile, SaveDialog.FileName ) ;
        ReWrite( OutFile ) ;
        // Write data to file
        for iBin := 0 to DwellTHist.MaxBin do begin
            WriteLn( OutFile, DwellTHist.Bins[iBin].Lo,
                              DwellTHist.Bins[iBin].Mid,
                              DwellTHist.Bins[iBin].Hi,
                              DwellTHist.Bins[iBin].y ) ;
            end ;
        CloseFile( OutFile ) ;
        end ;
    end ;


procedure TSingleChanAnalFrm.cbAmpHistChannelChange(Sender: TObject);
// ------------------------------------------------
// Channel selected for amplitude histogram changed
// ------------------------------------------------
begin
     Settings.DwellTimes.ChanNum := cbAmpHistChannel.ItemIndex ;
     ChannelChanged ;
     end;


procedure TSingleChanAnalFrm.ChannelChanged ;
// -------------------------------------------
// Re-initialise displays when channel changed
// -------------------------------------------
begin

     ChanNum := Settings.DwellTimes.ChanNum ;

     AHBinRangePanel.Visible := False ;

     // Initialise amplitude histogram
     InitialiseAmpHist ;

     edDetUnitCurrent.Units := Channel[ChanNum].ADCUnits ;

     { Initialise continuous and detected event displays with new channel }
     InitialiseDisplays ;

     if bDetect.Enabled then DisplayRecord ;

     if cbDetChannel.ItemIndex <> Settings.DwellTimes.ChanNum  then
        cbDetChannel.ItemIndex := Settings.DwellTimes.ChanNum ;
     if cbAmpHistChannel.ItemIndex <> Settings.DwellTimes.ChanNum  then
        cbAmpHistChannel.ItemIndex := Settings.DwellTimes.ChanNum ;

     end ;


procedure TSingleChanAnalFrm.bUseCursorsClick(Sender: TObject);
// ------------------------------------------
// Get detection range from selection cursors
// ------------------------------------------
begin
     edDetRange.LoValue := Min(SelectionCursor0,SelectionCursor1 ) ;
     Settings.DwellTimes.SampleRangeLo := Round(edDetRange.LoValue) ;
     edDetRange.HiValue := Max(SelectionCursor0,SelectionCursor1 ) ;
     Settings.DwellTimes.SampleRangeHi := Round(edDetRange.HiValue) ;
     end;

procedure TSingleChanAnalFrm.bGetC0Click(Sender: TObject);
// -------------------------------------
// Place selection cursor C-0 on display
// -------------------------------------
begin
     SelectionCursor0 := scDetDisplay.XOffset
                         + (scDetDisplay.NumPoints div 4) ;
     DisplayRecord ;
     end;

procedure TSingleChanAnalFrm.bGetC1Click(Sender: TObject);
// -------------------------------------
// Place selection cursor C-1 on display
// -------------------------------------
begin
     SelectionCursor1 := scDetDisplay.XOffset
                         + 3*(scDetDisplay.NumPoints div 4) ;
     DisplayRecord ;
     end;

procedure TSingleChanAnalFrm.bUseCursorsForAmpHistRangeClick(Sender: TObject);
// -----------------------------------------------------
// Get amplitude histogram range from selection cursors
// ----------------------------------------------------
begin
     edAHRange.LoValue := Min( SelectionCursor0,SelectionCursor1 ) ;
     edAHRange.HiValue := Max( SelectionCursor0,SelectionCursor1 ) ;
     end;

procedure TSingleChanAnalFrm.bUseCursorsForStabPlotRangeClick(
  Sender: TObject);
begin
// -----------------------------------------------------
// Get stability plot range from selection cursors
// ----------------------------------------------------
begin
     edStabPlotRange.LoValue := Min( SelectionCursor0,SelectionCursor1 ) ;
     edStabPlotRange.HiValue := Max( SelectionCursor0,SelectionCursor1 ) ;
     end;

end;

{ *** SINGLE-CHANNEL EVENT DATA FILE I/O ROUTINES ************************
  Used by dweltime.pas, amphist.pas
  ************************************************************************ }

procedure TSingleChanAnalFrm.OpenEventFile(
          var EventFile : TEventFile { Event file record }
          ) ;
{ ------------------------------------------
  Create/Open single-channel event data file
  ------------------------------------------}
begin

     // Warn me if event record size ever changes
     if SizeOf(TEvent) <> 60 then
        ShowMessage('Internal Error : Event record wrong size');

     { Event data file has a .EDE file ending }
     EventFile.Name := ChangeFileExt( CdrFH.FileName, EventFileExtension ) ;
     if FileExists( EventFile.Name ) then begin
        EventFile.Handle := FileOpen(EventFile.Name, fmOpenReadWrite ) ;
        { Read number of events from start of file  }
        EventFile.Pointer := FileSeek(EventFile.Handle,0,0) ;
        if FileRead(EventFile.Handle,EventFile.NumEvents,SizeOf(EventFile.NumEvents))
           <> SizeOf(EventFile.NumEvents) then
           ShowMessage('Error reading Event file');
        end
     else begin
        EventFile.Handle := FileCreate(EventFile.Name) ;
        EventFile.NumEvents := 0 ;
        // Reset sample range
        Settings.DwellTimes.SampleRangeLo := 0 ;
        Settings.DwellTimes.SampleRangeHi := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        Settings.DwellTimes.SampleBlockSize :=  Max(Settings.DwellTimes.SampleRangeHi div 20,1) ;
        end ;

     if EventFile.Handle < 0 then begin
        ShowMessage('Error opening Event file');
        EventFile.Open := False ;
        end
     Else EventFile.Open := True ;

     end ;


procedure TSingleChanAnalFrm.CloseEventFile(
          var EventFile : TEventFile       { Event file description record }
          ) ;
{ ------------------------------------
  Close single-channel event data file
  ------------------------------------}
begin

     if EventFile.Handle < 0 then Exit ;

     { Save number of events at beginning of file }
     EventFile.Pointer := FileSeek(EventFile.Handle,0,0) ;
     if FileWrite(EventFile.Handle,EventFile.NumEvents,SizeOf(EventFile.NumEvents))
        <> SizeOf(EventFile.NumEvents) then
        ShowMessage('Error writing Event file');
     { Close file }
     FileClose( EventFile.Handle ) ;
     EventFile.Handle := -1 ;
     EventFile.Open := False ;

     end ;


procedure TSingleChanAnalFrm.WriteEventToFile(
          var EventFile : TEventFile ;      { Event file description record }
          EventNum : Integer ;              { Event number to be written }
          var Event : TEvent                { Event data record }
          ) ;
{ ----------------------------------------------------
  Write single-channel event record to event data file
  ----------------------------------------------------}
begin
     { Move to position in event file }
     EventFile.Pointer := FileSeek(EventFile.Handle,
                                   (EventNum-1)*SizeOf(TEvent) + SizeOf(EventFile.NumEvents),
                                   0) ;
     { Write event data }
     if FileWrite(EventFile.Handle,Event,SizeOf(TEvent)) <> SizeOf(TEvent) then
        ShowMessage('Error writing Event file');

     end ;


procedure TSingleChanAnalFrm.ReadEventFromFile(
          var EventFile : TEventFile ;      { Event file description record }
          EventNum : Integer ;              { Event number to be read }
          var Event : TEvent                { Event data record }
          ) ;
{ -----------------------------------------------------
  Read single-channel event record from event data file
  -----------------------------------------------------}
begin
     if (EventNum >= 1) and (EventNum <= EventFile.NumEvents) then begin
        { Move to position in event file }
        EventFile.Pointer := FileSeek(EventFile.Handle,
                             (EventNum-1)*SizeOf(TEvent) + SizeOf(EventFile.NumEvents),
                             0) ;
        { Read event data }
        if FileRead(EventFile.Handle,Event,SizeOf(TEvent)) <> SizeOf(TEvent) then
           ShowMessage('Error reading Event file') ;

        // Fix incorrect zero level which occurred
        if (Event.ZeroLevel > Channel[0].ADCMaxValue) or
           (Event.ZeroLevel < (-Channel[0].ADCMaxValue-1)) then
           Event.ZeroLevel := Channel[0].ADCZero ;

        Event.Available := True ;
        end
     else Event.Available := False ;
     end ;




procedure TSingleChanAnalFrm.bSaveToLogClick(Sender: TObject);
// ----------------------------------------
// Write amplitude curve fit results to log
// ----------------------------------------
var
    i : Integer ;
begin
     for i := 0 to erAmpResults.Lines.Count-1 do
         WriteToLogFile( erAmpResults.Lines[i] ) ;
     end;

procedure TSingleChanAnalFrm.bSaveToLogSummaryClick(Sender: TObject);
// -------------------------------
// Write summary table to log file
// -------------------------------
var
    Row : Integer ;
    s : String ;
begin
     for Row := 0 to sgSummary.RowCount-1 do begin
          s := sgSummary.Cells[0,Row] ;
          s := s + ' = ' + sgSummary.Cells[1,Row] ;
          WriteToLogFile(s) ;
          end ;
     end;

procedure TSingleChanAnalFrm.bSaveToLogDwellTimesClick(Sender: TObject);
// ----------------------------------------
// Write amplitude curve fit results to log
// ----------------------------------------
var
    i : Integer ;
begin
     for i := 0 to erDwellTResults.Lines.Count-1 do
         WriteToLogFile( erDwellTResults.Lines[i] ) ;
     end;

procedure TSingleChanAnalFrm.edDetDisplayWidthKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------------------------------
// Update detect events display when no. points in display changed
// ---------------------------------------------------------------
begin
     if key = #13 then begin
         Settings.DwellTimes.RecordSize := Round(edDetDisplayWidth.Value) ;
         edDetDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
         InitialiseDisplays ;
         DisplayRecord ;
         end ;
     end;


procedure TSingleChanAnalFrm.edEditDisplayWidthKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------------------------------
// Update edit events display when no. points in display changed
// ---------------------------------------------------------------
begin
     if key = #13 then begin
         Settings.DwellTimes.RecordSize := Round(edEditDisplayWidth.Value) ;
         edEditDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
         InitialiseDisplays ;
         DisplayEvent ;
         end ;
     end;


procedure TSingleChanAnalFrm.edMarginPointsKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then DisplayEvent ;
     end;


procedure TSingleChanAnalFrm.bSaveCursorReadingClick(Sender: TObject);
// ---------------------------------------
// Save current cursor measurement in list
// ---------------------------------------
begin

     if NumCursorMeasurements < High(CursorMeasurements) then
        Inc(NumCursorMeasurements) ;
     edNumCursorMeasurements.Value := NumCursorMeasurements ;

     // Save cursor measurements list
     SaveCursorMeasurementsToFile( ChangeFileExt( CDRFH.FileName, '.crm' )) ;

     end ;


procedure TSingleChanAnalFrm.SaveCursorMeasurementsToFile(
          FileName : String
          ) ;
// ------------------------------------
// Save cursor measurement list to file
// ------------------------------------
var
    i : Integer ;
    FileVar : TextFile ;
begin

     // Create new text file
     AssignFile( FileVar, FileName ) ;
     Rewrite( FileVar ) ;

     // Title
     WriteLn( FileVar,
              'Time (s)' + #9 +
              'Duration (ms)' + #9 +
              'Average (' + Channel[ChanNum].ADCUnits + ')'  + #9 +
              'S.D. (' + Channel[ChanNum].ADCUnits + ')'
              ) ;

     // Write measurements
     for i := 0 to NumCursorMeasurements-1 do begin
         // Write to file
         WriteLn( FileVar,
                  format( '%.5g%s%.5g%s%.5g%s%.5g',
                  [CursorMeasurements[i].TStart,#9,
                   CursorMeasurements[i].Duration,#9,
                   CursorMeasurements[i].AverageCurrent,#9,
                   CursorMeasurements[i].SDCurrent] )) ;
         end ;

     // Close file
     CloseFile( FileVar) ;

     end ;


procedure TSingleChanAnalFrm.LoadCursorMeasurementsFromFile(
          FileName : String
          ) ;
// ------------------------------------
// Load cursor measurement list from file
// ------------------------------------
var
    FileVar : TextFile ;
    Title : String ;
begin

     if not FileExists(FileName) then Exit ;

     // Open text file
     AssignFile( FileVar, FileName ) ;
     Reset( FileVar ) ;

     // Read Title line
     ReadLn( FileVar, Title ) ;

     // Write measurements
     While not EOF(FileVar) do begin
         // Write to file
         ReadLn( FileVar,
                 CursorMeasurements[NumCursorMeasurements].TStart,
                 CursorMeasurements[NumCursorMeasurements].Duration,
                 CursorMeasurements[NumCursorMeasurements].AverageCurrent,
                 CursorMeasurements[NumCursorMeasurements].SDCurrent ) ;
         Inc(NumCursorMeasurements) ;
         if NumCursorMeasurements > High(CursorMeasurements) then Break ;
         end ;

     edNumCursorMeasurements.Value := NumCursorMeasurements ;    

     // Close file
     CloseFile(FileVar) ;

     end ;


procedure TSingleChanAnalFrm.bSaveToMeasurementsFileClick(Sender: TObject);
// -----------------------------------
// Save a-a cursor measurement to file
// -----------------------------------
begin

     // Get the name of a data file to hold ezported data
     SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := '.txt' ;
     SaveDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     SaveDialog.Title := 'Cursor measurements file';

     SaveDialog.FileName := ChangeFileExt( CDRFH.FileName, '.txt' ) ;
     SaveDialog.FileName := ANSIReplaceStr( SaveDialog.FileName,
                                            '.txt',
                                            ' (cursor measurements).txt' ) ;

     // Save to file
     if SaveDialog.execute then SaveCursorMeasurementsToFile( SaveDialog.FileName ) ;

     end;


procedure TSingleChanAnalFrm.bClearCursorReadingListClick(Sender: TObject);
// ------------------------------
// Clear cursor measurements list
// ------------------------------
begin
     if NumCursorMeasurements > 0 then begin
        if MessageDlg('Clear cursor measurements list',mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then begin
           NumCursorMeasurements := 0 ;
           edNumCursorMeasurements.Value := NumCursorMeasurements ;
           end ;
        end ;

     end;

procedure TSingleChanAnalFrm.bLoadFromMeasurementsFileClick(
  Sender: TObject);
// -----------------------------------------
// Load set of cursor measurements from file
// -----------------------------------------
begin

     // Get the name of a data file to hold ezported data
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     OpenDialog.Title := 'Cursor measurements file';
     OpenDialog.FileName := '' ;

     // Load from file
     if OpenDialog.execute then begin
        if NumCursorMeasurements > 0 then begin
           if MessageDlg('Clear cursor measurements list',mtConfirmation,
              [mbYes,mbNo], 0 ) = mrYes then NumCursorMeasurements := 0 ;
           end ;

        LoadCursorMeasurementsFromFile( OpenDialog.FileName ) ;

        end ;

     end;

procedure TSingleChanAnalFrm.edCursorSpacingKeyPress(Sender: TObject;
  var Key: Char);
begin
     // Force display of new event }
     if Key = #13 then DisplayEvent ;
     end;

procedure TSingleChanAnalFrm.edAHBinWidthKeyPress(Sender: TObject;
  var Key: Char);
// -----------------
// Bin width changed
// -----------------
begin
     if Key = #13 then begin
        if edAHBinWidth.Value <= 0.0 then begin
           edAHBinWidth.Value := (edAHBinsUpper.Value - edAHBinsLower.Value)/edAHNumBins.Value ;
           end;
        edAHNumBins.Value := Round((edAHBinsUpper.Value - edAHBinsLower.Value)/edAHBinWidth.Value) ;
        edAHBinsUpper.Value := edAHBinsLower.Value + edAHNumBins.Value*edAHBinWidth.Value ;
        end;
    end;

procedure TSingleChanAnalFrm.edAHBinsLowerKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Histogram lower limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edAHBinWidth.Value := (edAHBinsUpper.Value - edAHBinsLower.Value)/edAHNumBins.Value ;
        end;
      end;

procedure TSingleChanAnalFrm.edAHBinsUpperKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Histogram upper limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edAHBinWidth.Value := (edAHBinsUpper.Value - edAHBinsLower.Value)/edAHNumBins.Value ;
        end;
      end;

procedure TSingleChanAnalFrm.edAHNUmBinsKeyPress(Sender: TObject;
  var Key: Char);
// -------------------
// No. of bins changed
// -------------------
begin
    if Key = #13 then begin
       edAHNumBins.Value := Round(edAHNumBins.Value) ;
       edAHBinWidth.Value := (edAHBinsUpper.Value - edAHBinsLower.Value)/edAHNumBins.Value ;
       edAHBinsUpper.Value := edAHBinsLower.Value + edAHNumBins.Value*edAHBinWidth.Value ;
       end;
    end;

procedure TSingleChanAnalFrm.bHalveDetDisplayClick(Sender: TObject);
// -----------------------------------------------
// Halve duration of transition detection display
// ------------------------------------------------
begin
      edDetDisplayWidth.Value := edDetDisplayWidth.Value*0.5 ;
      Settings.DwellTimes.RecordSize := Round(edDetDisplayWidth.Value) ;
      edDetDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
      InitialiseDisplays ;
      DisplayRecord ;
      end ;

procedure TSingleChanAnalFrm.bTDisplayDoubleClick(Sender: TObject);
// -----------------------------------------------
// Double duration of transition detection display
// ------------------------------------------------
begin
      edDetDisplayWidth.Value := edDetDisplayWidth.Value*2.0 ;
      Settings.DwellTimes.RecordSize := Round(edDetDisplayWidth.Value) ;
      edDetDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
      InitialiseDisplays ;
      DisplayRecord ;
      end ;

procedure TSingleChanAnalFrm.bHalveEditDisplayDurationClick(Sender: TObject);
// ----------------------------
// Halve edit display duration
// ----------------------------
begin
     edEditDisplayWidth.Value := edEditDisplayWidth.Value*0.5 ;
     Settings.DwellTimes.RecordSize := Round(edEditDisplayWidth.Value) ;
     edEditDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
     InitialiseDisplays ;
     DisplayEvent ;
     end;

procedure TSingleChanAnalFrm.BDoubleEditDisiplayDurationClick(
  Sender: TObject);
// ----------------------------
// Double edit display duration
// ----------------------------
begin
     edEditDisplayWidth.Value := edEditDisplayWidth.Value*2.0 ;
     Settings.DwellTimes.RecordSize := Round(edEditDisplayWidth.Value) ;
     edEditDisplayWidth.Value := Settings.DwellTimes.RecordSize ;
     InitialiseDisplays ;
     DisplayEvent ;
     end;

procedure TSingleChanAnalFrm.edAHUnitCurrentKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------
// Unitary current changed
// -----------------------
begin
     if Key = #13 then begin
        Settings.DwellTimes.UnitCurrent := edAHUnitCurrent.Value ;
        plAmpHist.VerticalCursors[AmpCurs.IUnit] := Settings.DwellTimes.UnitCurrent ;
        end ;

     end;

procedure TSingleChanAnalFrm.edAHRangeKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
          Settings.DwellTimes.SampleRangeLo := Round(edAHRange.LoValue) ;
          Settings.DwellTimes.SampleRangeHi := Round(edAHRange.HiValue) ;
          end ;
     end;

procedure TSingleChanAnalFrm.ckEnableCursorMeasurementClick(
  Sender: TObject);
begin
     scEditDisplay.ClearVerticalCursors ;
     if ckEnableCursorMeasurement.Checked then begin
        EditCurs.C0 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'a' ) ;
        EditCurs.C1 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'a' ) ;
        scEditDisplay.LinkVerticalCursors( EditCurs.C0, EditCurs.C1 );
        EditCurs.Z0 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'z' ) ;
        EditCurs.Z1 := scEditDisplay.AddVerticalCursor( -1, clGreen, 'z' ) ;
        scEditDisplay.LinkVerticalCursors( EditCurs.Z0, EditCurs.Z1 );
        end
     else lbCursors.Caption := '' ;
     DisplayEvent ;

     end;

procedure TSingleChanAnalFrm.edThresholdKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        Settings.DwellTimes.Threshold := edThreshold.Value ;
        UpdateCursors( scDetDisplay ) ;
        end ;
      end;

end.

