unit EventDetector;
// --------------------------------------------------
// Spontaneous Event Detector module
// (c) John Dempster, University of Strathclyde, 2002
// --------------------------------------------------
// 25/2/02
// 9/4/02 .... RH.ADCVoltageRange now correctly takes account of CDRFH.ADCAmplifierGain
// 14/4/02 ... MatchTemplate detection criterion now aligned with peak signal
// 29.5.02 ... Selection of channel in multi-channel files now works correctly
//             "t" label added to indicate detection threshold
// 30.7.02 ... Changes to detected event list now saved to file when form closed
// 31.7.02 ... Occasional misplacement of inserted events fixed
// 26.8.02 ... Rate over specified interval added
// 14.12.02 ... Area add decay time constant measurements added
//              Event filter added
// 12.6.03 .... Bug: Incorrect display of detection criteria trace when more than
//              one channel fixed
// 24.6.03 .... No. horizontal/vertical grid lines changeable
// 8.4.03 ..... NumEvents now restricted to limits of Events array
// 7.8.03 ..... Invalid NumEvents in .EVE now trapped
// 25.8.03 .... Event averaging page added
// 26.8.03 .... Bugs in Template matching fixed
// 8.9.03 ..... Noise at end of average fixed
// 19.11.03 ... MaxADCValue now obtained from data file value
// 23.02.04 ... Incomplete records are beginning/end of recording
//              now excluded from detected event average
//              FP Error with very small data files fixed
// 24.03.04 ... Time constants now computed from peak - 10% of peak
//              Average is re-computed when waveform measurement polarity changed
// 15.01.05 ... Pre-trigger zero level added as plottable measurement variable
// 02.02.05 ... Analysis window now indicated by cursors
//              Duration variable added
// 08.01.06 ... Detection channel, dead time and analysis window size now saved
//              Window duration now in ms rather than sample points.
// 11.01.06 ... Range of events exported can now be set by user
// 09.06.06 ... Threshold detector running mean now preset to 1s
// 07.06.06 ... Bug which limited export event range to 1-1 fixed
// 13.08.06 ... Count-matched (selected n larged events) averaging option added
// 18.11.06 ... Decay time % can now be set by user
//              Export to WCP files now works correctly
//              Analysis cursors removed. Analysis now on whole edit display
// 28.11.06 ... Event list file now updated when events added or deleted
// 21.12.06 ... WCPRecordHeader no longer used in WCP file export
// 31.01.07 ... Waveform analysis peak and area now restricted to region defined by a-a cursors
// 08.03.07 ... T.(x%) decay can now be relative to peak, mid-point of rise, or a0 cursor
//              Zero level can be at start of record or event
//              Decaying exponential fits added
//              Scaling factors of detected event exported to WCP files now correct
// 20.08.07 ... No. of points averaged to compute baseline can now be changed
//              and can be offset from baseline point.
//              Tau Decay now computed correctly
// 17.10.07 ... Event detection criterion signal now kept within min/max limits of
//              channel. Roll-over is now avoided
// 01.05.08 ... Buffer sizes now adjusted to size of display window to avoid
//              buffer overflow errors with large display windows
// 05.06.08 ... Control panels widened to avoid waveform measurements wrapping round
//              to multiple lines
// 20.08.08 ... Buffer overflow error when exporting large events fixed
//              Exported event record limited to 65536 sample points per channel
// 16.12.08 ... Exported event record limit icreased to 1048576 total sample points
//              Event list size increased to 1000000
//              Buffers allocated using GetMem rather than in stack
// 14.04.09 ... Event export bug where first record was duplicated in all exported records
//              fixed. All buffers now allocated with GetMem rather than New
// 29.04.09 ... Out of memory bug (introduced 14/4/9) which caused crash when edit display duration changed fixed
// 08.12.09 ... Event export range now adjusted when events add/deleted
// 22.12.09 ... Detected event analysis parameters now stored in EDR file header
// 11.01.10 ... Histogram bin width, range and no. bins can be set by user
//              Exponential fit to histogram added
// 27.01.10 ... Access violation when event detector opened with data files containing
//              more than one channel fixed.
// 28.01.10 ... Access violations when Subt. Baseline Trend selected fixed.
// 15.02.10 ... Detected events now aligned at mid-point of rising phase
//              Size of rising edge detection window can be set by user
// 19.04.10 ... Channel selection list now drop down list to avoid
//              accidental entry of invalid channel leading to access violation error
// 17.06.10 ... Events now written in V9.0 WCP file format
//              Record group # = event #
// 26.06.14 ... SaveEventList() removed from Insert/Delete events
//              to avoid Error writing event file when buttons pressed too fast on slow systems
//              Access violation when manual event created by user off end recording trapped
// 16.07.14 ... SaveEventList() now executed from .timer() every time Insert/Delete events pressed
//              to avoid events insertions/deletions being lost when NewFile() function called
// 30.07.19 ... Baseline tracking in Threshold detection mode can now be disabled.
// 27.08.19 ... Detection criterion display Y range no longer drifts off scale when signal zero level adjusted by user
// 26.09.19 ... Baseline tracking enabled/disabled option now works correctly
//              Detection mode,thresholds and other settings now preserved in EDR data file header
// 12.03.21 ... Detected events aligned by mid-point of rising, irrespective of detection method used
//              First window of signal now fully displayed when detection window opened
//              Template match tau rise and decay times now preserved in EDR file
// 04.05.21 ... Shift in detection point when TimeThreshold <> 0 fixed
// 24.05.21 ... Incorrect placement of mid-point at start of rising edge window when rising egde very slow fixed.
// 25.05.21 ... Displayed signal data can now be exported to CSV file
// 26.05.21 ... 'Fixed' event baseline level added which uses user-set baseline cursors for signal chaannel
//               Baseline type now set using drop-down list rather than radio buttons
// 02.06.21 ... Events can now be aligned on either max. rate of rise or mid-point of rise
//              Mid-points now detected more reliably.
// 08.02.22 ... Rising edge detection buffer duration set to dead time to prevent detection point
//              being shifted to previous event at high event frequencies.
//              Event alignment point can now be left at detection threshold (as in pre 3.9 versions)
//              Rate of rise and template displays no longer display junk data at end of file
//              Control positions now set using anchors
// 09.03.22 ... Set Ampl. = 4*SD button now works (was setting wrong detection windows cursor

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, RangeEdit, ScopeDisplay, ComCtrls, global, maths, fileio,
  XYPlotDisplay, ExtCtrls, ValidatedEdit, CursorLabel, HTMLLabel,
  ADCDataFile, math, CurveFitter, StrUtils, seslabio, System.UITypes ;

const
    EventFileExtension = '.EVF' ;
    WCPFileExtension = '.WCP' ;
    MaxExportSamples = 1048576 ;
    MaxEvents = 1000000 ;
    mdThreshold = 0 ;
    mdRateOfRise = 1 ;
    mdPatternMatch = 3 ;
    vEventNum = 0 ;
    vTime = 1 ;
    vInterval = 2 ;
    vFrequencyAvg = 3 ;
    vFrequencyInst = 4 ;
    vPeak = 5 ;
    vArea = 6 ;
    vTRise = 7 ;
    vTDecay = 8 ;
    vTauDecay = 9 ;
    vDuration = 10 ;
    vBaseline = 11 ;
    MaxVar = 12 ;
    BaselineAtStart = 0 ;
    BaselineAtEvent = 1 ;
    FixedBaseline = 2 ;
    AlignMaxRateOfRise = 0 ;
    AlignMidPointOfRise = 1 ;

type

  TEventAnalysis = record     // Event waveform analysis record
    YBaseline : Integer ;
    Rate : Single ;
    Peak : Single ;
    Area : Single ;
    TRise : Single ;
    TDecay : Single ;
    TauDecay : Single ;
    Duration : Single ;
    end ;

  TFilter = record            // Filter criterion record
    Use : Boolean ;
    Variable : Integer ;
    LoLimit : Single ;
    HiLimit : Single ;
    end ;

  TCursors = record
    C0 : Integer ;
    C1 : Integer ;
    Read : Integer ;
    end ;

  TEventDetFrm = class(TForm)
    Page: TPageControl;
    DetectEventsPage: TTabSheet;
    sbDisplay: TScrollBar;
    EditEventsPage: TTabSheet;
    EditEventsGrp: TGroupBox;
    sbEditDisplay: TScrollBar;
    edEvent: TRangeEdit;
    sbEvent: TScrollBar;
    editGrp: TGroupBox;
    bInsertEvent: TButton;
    bDeleteEvent: TButton;
    XYPlotPage: TTabSheet;
    XYPlotGrp: TGroupBox;
    GroupBox5: TGroupBox;
    Label9: TLabel;
    Label1: TLabel;
    cbPlotYVar: TComboBox;
    cbPlotXVar: TComboBox;
    bNewPlot: TButton;
    GroupBox6: TGroupBox;
    rbPlotAllEvents: TRadioButton;
    rbPlotRange: TRadioButton;
    plPlot: TXYPlotDisplay;
    ExportGrp: TGroupBox;
    bExportToWCPFile: TButton;
    bSetPlotAxes: TButton;
    bExportNonEvents: TButton;
    HistPage: TTabSheet;
    plHist: TXYPlotDisplay;
    AnalysisGrp: TGroupBox;
    meResults: TMemo;
    gpEventPolarity: TGroupBox;
    rbPositive: TRadioButton;
    rbNegative: TRadioButton;
    EventFilterGrp: TGroupBox;
    GroupBox4: TGroupBox;
    cbVariable: TComboBox;
    GroupBox15: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    edLoLimit: TValidatedEdit;
    edHiLimit: TValidatedEdit;
    EventListGrp: TGroupBox;
    bLoadEventList: TButton;
    bSaveEventList: TButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    GroupBox18: TGroupBox;
    meFilterSet: TMemo;
    GroupBox14: TGroupBox;
    rbAND: TRadioButton;
    rbOR: TRadioButton;
    bDelete: TButton;
    bApply: TButton;
    bAddFilter: TButton;
    Label14: TLabel;
    HistResultsGrp: TGroupBox;
    bHistFitCurve: TButton;
    cbHistEqn: TComboBox;
    AveragePage: TTabSheet;
    AverageGrp: TGroupBox;
    GroupBox19: TGroupBox;
    rbAverageAllEvents: TRadioButton;
    rbAverageRange: TRadioButton;
    edAverageRange: TRangeEdit;
    bDoAverage: TButton;
    scAverageDisplay: TScopeDisplay;
    AverageResultsGrp: TGroupBox;
    bAverageFitCurve: TButton;
    cbAverageEqn: TComboBox;
    GroupBox20: TGroupBox;
    meAverageResults: TMemo;
    WCPFile: TADCDataFile;
    EditDisplayWidthPanel: TPanel;
    edEditDisplayWidth: TValidatedEdit;
    lbEditDisplayPoints: TLabel;
    bPlotAbort: TButton;
    edExportRange: TRangeEdit;
    Label20: TLabel;
    bAbortExport: TButton;
    GroupBox16: TGroupBox;
    ckCountMatchedAvg: TCheckBox;
    edNumCountMatchedAvg: TValidatedEdit;
    Label21: TLabel;
    CVFit: TCurveFitter;
    lbAvgFitResults: THTMLLabel;
    lbHistResults: THTMLLabel;
    GroupBox23: TGroupBox;
    edTDecayPercentage: TValidatedEdit;
    Label4: TLabel;
    edPreTrigger: TValidatedEdit;
    bAbortAverage: TButton;
    gpZeroLevel: TGroupBox;
    ckSubtractBaseline: TCheckBox;
    Label3: TLabel;
    Label5: TLabel;
    edZeroNumAvg: TValidatedEdit;
    edZeroGap: TValidatedEdit;
    cbDecayFrom: TComboBox;
    Label7: TLabel;
    HistGrp: TGroupBox;
    GroupBox12: TGroupBox;
    Label17: TLabel;
    cbHistVar: TComboBox;
    ckPercentage: TCheckBox;
    ckCumulative: TCheckBox;
    edNumBins: TValidatedEdit;
    bNewHistogram: TButton;
    BinRangePanel: TPanel;
    Label13: TLabel;
    Label19: TLabel;
    Label22: TLabel;
    edBinWidth: TValidatedEdit;
    edBinsLower: TValidatedEdit;
    edBinsUpper: TValidatedEdit;
    bSetHistAxes: TButton;
    GroupBox13: TGroupBox;
    rbHistAllEvents: TRadioButton;
    rbHistRange: TRadioButton;
    edHistRange: TRangeEdit;
    meHistResults: TMemo;
    DetDisplayPanel: TPanel;
    edDetDisplayWidth: TValidatedEdit;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    Label18: TLabel;
    bDoubleEditDisplayWidth: TButton;
    bEditDisplayWidthHalve: TButton;
    cktCursorAtDetectionPoint: TCheckBox;
    FrequencyAvgPan: TPanel;
    edAverageInterval: TValidatedEdit;
    Label2: TLabel;
    edPlotTimeRange: TRangeEdit;
    EventRangePan: TPanel;
    edPlotEventRange: TRangeEdit;
    scDisplay: TScopeDisplay;
    scDetDisplay: TScopeDisplay;
    scEditDisplay: TScopeDisplay;
    scMarkDisplay: TScopeDisplay;
    Timer: TTimer;
    cbReviewChannel: TComboBox;
    Label11: TLabel;
    bExportAnalysis: TButton;
    cbBaseline: TComboBox;
    DetectGrp: TGroupBox;
    bDetect: TButton;
    bAbort: TButton;
    GroupBox8: TGroupBox;
    Label6: TLabel;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    cbChannel: TComboBox;
    CriteriaGrp: TGroupBox;
    GroupBox1: TGroupBox;
    rbRateOfRise: TRadioButton;
    rbPatternMatch: TRadioButton;
    rbThreshold: TRadioButton;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    lbTimeThreshold: TLabel;
    edThreshold: TValidatedEdit;
    edTimeThreshold: TValidatedEdit;
    bSetThresholdTo4SD: TButton;
    ModePage: TNotebook;
    GroupBox7: TGroupBox;
    Label25: TLabel;
    edBaselineAveragingInterval: TValidatedEdit;
    ckEnableBaselineTracking: TCheckBox;
    GroupBox11: TGroupBox;
    GroupBox9: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    edTauRise: TValidatedEdit;
    edTauDecay: TValidatedEdit;
    GroupBox10: TGroupBox;
    Label23: TLabel;
    edDeadTime: TValidatedEdit;
    gpEventAlignment: TGroupBox;
    cbEventAlignment: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure sbDisplayChange(Sender: TObject);
    procedure bDetectClick(Sender: TObject);
    procedure scDetDisplayCursorChange(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure sbEditDisplayChange(Sender: TObject);
    procedure PageChange(Sender: TObject);
    procedure sbEventChange(Sender: TObject);
    procedure edEventKeyPress(Sender: TObject; var Key: Char);
    procedure bInsertEventClick(Sender: TObject);
    procedure bDeleteEventClick(Sender: TObject);
    procedure scEditDisplayCursorChange(Sender: TObject);
    procedure bNewPlotClick(Sender: TObject);
    procedure bExportToWCPFileClick(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure bSetThresholdTo4SDClick(Sender: TObject);
    procedure bSetPlotAxesClick(Sender: TObject);
    procedure rbThresholdClick(Sender: TObject);
    procedure edTauRiseKeyPress(Sender: TObject; var Key: Char);
    procedure edThresholdKeyPress(Sender: TObject; var Key: Char);
    procedure bExportNonEventsClick(Sender: TObject);
    procedure edEditDisplayWidthKeyPress(Sender: TObject; var Key: Char);
    procedure cbChannelChange(Sender: TObject);
    procedure bNewHistogramClick(Sender: TObject);
    procedure bAddFilterClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure cbHistVarChange(Sender: TObject);
    procedure bLoadEventListClick(Sender: TObject);
    procedure bSaveEventListClick(Sender: TObject);
    procedure cbVariableChange(Sender: TObject);
    procedure edLoLimitKeyPress(Sender: TObject; var Key: Char);
    procedure edHiLimitKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edAnalysisWindowKeyPress(Sender: TObject; var Key: Char);
    procedure edDeadTimeKeyPress(Sender: TObject; var Key: Char);
    procedure edBaselineAveragingIntervalKeyPress(Sender: TObject;
      var Key: Char);
    procedure rbANDClick(Sender: TObject);
    procedure rbPositiveClick(Sender: TObject);
    procedure bHistFitCurveClick(Sender: TObject);
    procedure bSetHistAxesClick(Sender: TObject);
    procedure bDoAverageClick(Sender: TObject);
    procedure scAverageDisplayCursorChange(Sender: TObject);
    procedure rbAveragePositiveClick(Sender: TObject);
    procedure rbAverageNegativeClick(Sender: TObject);
    procedure ckSubtractBaselineClick(Sender: TObject);
    procedure bPlotAbortClick(Sender: TObject);
    procedure bAbortExportClick(Sender: TObject);
    procedure edDetDisplayWidthKeyPress(Sender: TObject; var Key: Char);
    procedure bAverageFitCurveClick(Sender: TObject);
    procedure edPreTriggerKeyPress(Sender: TObject; var Key: Char);
    procedure edTDecayPercentageKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bAbortAverageClick(Sender: TObject);
    procedure scEditDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scAverageDisplayMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure rbTDecayFromPeakClick(Sender: TObject);
    procedure rbTDecayFromMidRiseClick(Sender: TObject);
    procedure rbTDecayFromC0Click(Sender: TObject);
    procedure edZeroNumAvgKeyPress(Sender: TObject; var Key: Char);
    procedure edZeroGapKeyPress(Sender: TObject; var Key: Char);
    procedure cbDecayFromChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure bEditDisplayWidthHalveClick(Sender: TObject);
    procedure bDoubleEditDisplayWidthClick(Sender: TObject);
    procedure edNumBinsKeyPress(Sender: TObject; var Key: Char);
    procedure edBinWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edBinsLowerKeyPress(Sender: TObject; var Key: Char);
    procedure edBinsUpperKeyPress(Sender: TObject; var Key: Char);
    procedure cbPlotXVarChange(Sender: TObject);
    procedure cbPlotYVarChange(Sender: TObject);
    procedure edAverageIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure TimerTimer(Sender: TObject);
    procedure cbReviewChannelChange(Sender: TObject);
    procedure ckEnableBaselineTrackingClick(Sender: TObject);
    procedure edTimeThresholdKeyPress(Sender: TObject; var Key: Char);
    procedure bExportAnalysisClick(Sender: TObject);
    procedure cbBaselineChange(Sender: TObject);
    procedure cbEventAlignmentChange(Sender: TObject);
  private
    { Private declarations }


    ADC : PSmallIntArray ;      // A/D sample buffer
    DetBuf : PSmallIntArray ;   // Detection criterion buffer
    EditBuf : PSmallIntArray ;   // Edit display buffer
    AvgBuf : PSmallIntArray ;   // Averaged event buffer
    NumEventsAveraged : Integer ;
    BaselineCursor : Integer ;
    ThresholdCursor : Integer ;
    DetZeroCursor : Integer ;
    DisplayCursor : Integer ;
    EditCursor : Integer ;
    EditC0Cursor : Integer ;
    EditC1Cursor : Integer ;
    OldEditC0CursorPos : Single ;
    OldEditC1CursorPos : Single ;
    AverageCursor : Integer ;
    AverageT0Cursor : Integer ;
    AverageC0Cursor : Integer ;
    AverageC1Cursor : Integer ;
    AvgStartEvent : Integer ;
    AvgEndEvent : Integer ;
    AvgEventScan : Integer ;
    AvgNumScans : Integer ;
    AvgPreScans : Integer ;
    OldAvgC0CursorPos : Integer ;
    OldAvgC1CursorPos : Integer ;

    BuffersAllocated : Boolean ;
    PlotAvailable : Boolean  ;             // X-Y plot available
    HistAvailable : Boolean  ;             // Histogram available
    xUnits : string ;
    yUnits : string ;

    Hist : THistogram ;         // Histogram data structure
    HistResults : TStringList ; // Histogram fitting results
    HistCurs : TCursors ;       // Histogram cursors

    Events : Array[0..MaxEvents-1] of Integer ;        // Event list
    YBaseline : Array[0..MaxEvents-1] of Integer ; // Event baselines
    YBaseline2 : Array[0..MaxEvents-1] of Single ; // Event baselines
    BaselineSplinesAvailable : Boolean ;

    NumEvents : Integer ;                        // No. of events in list
    EventAnalysisFile : Integer ;                // Event analysis file handle
    EventAnalysisFileUpdateRequired : Boolean ;  // Update required flag
    VarNames : Array[0..MaxVar] of string ;

    Filters : Array[0..MaxVar] of TFilter ;

    // Running mean variables used by "threshold" detection method
    RunningMean : Single ;
    AbortFlag : Boolean ;
    ComputationInProgress : Boolean ;
    SaveEventListRequested : Boolean ;     // Request event list to be saved (by .Timer()) proc)

    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure SetupDetector ;
    function RateofRise(
             StartAtSample : Integer ;        // Sample in file to start at
             NumPoints : Integer ;            // Number of A/D sample points
             YBuf : PSmallIntArray ;          // Signal data buffer
             YDet : PSmallIntArray            // Detection criterion buffer
             ) : Integer ;

    function Threshold(
              StartAtSample : Integer ;         // Sample in file to start at
              NumPoints : Integer ;             // Number of A/D sample points
              var InitialiseRunningMean : Boolean ;
              YBuf : PSmallIntArray ;          // Signal data buffer
              YDet : PSmallIntArray            // Detection criterion buffer
              ) : Integer ;

    function  MatchTemplate(
              StartAtSample : Integer ;        // Sample in file to start at
              NumPoints : Integer ;            // Number of A/D sample points
              YBuf : PSmallIntArray ;          // Signal data buffer
              YDet : PSmallIntArray            // Detection criterion buffer
              ) : Integer ;

    function  FindPeakTemplateMatch(
              iDetectedAtSample : Integer          // (IN) Point threshold crossed
              ) : Integer ;                        // (RET) Point of peak templatre match

   function  FindMidPointOfRise(
             iDetectedAtSample : Integer          // (IN) Point threshold crossed
             ) : Integer ;                        // (RET) Point of peak rate of rise

    procedure DisplayEvent;
    procedure LoadEventList ;
    procedure SaveEventList ;

    procedure DisplayEditRecord ;

    procedure AnalyseEvent(
              iEvent : Integer ;          // Event # to be analysed [IN]
              var Event : TEventAnalysis  // Analysis results record [OUT]
              ) ;
    procedure AnalyseAverage ;

    function PreEventBaselineLevel(
             var Buf : Array of SmallInt ; // Data buffer
             EventScan : Integer ;     // Scan # of start of event in Buf
             NumScans : Integer      // No. of scans in Buf
             ) : Integer ;

    procedure SubtractBaseline(
              iEvent : Integer  ;
              var Buf : Array of SmallInt ;
              BufStartScan : Integer ;
              NumScans : Integer
              ) ;
    procedure CalculateBaselineSplines ;

    procedure UpdateEventAnalysisFile ;
    function CalculateVariables(
          var iEvent : Integer ;                // Event number to be analysed [In]
          XVarType : Integer ;              // Variable selected for X axis [IN]
          YVarType : Integer ;              // Variable selected for Y axis [IN]
          var X : Single ;                  // X axis variable value
          var Y : Single                    // Y axis variable value
          ) : Boolean ;                     // TRUE = valid variable returned

function CalculateVariablesInInterval(
          StartOfInterval : Integer ;       // Sample at which interval ends [IN]
          EndOfInterval : Integer ;         // Sample at which interval ends [IN]
          XVarType : Integer ;              // Variable selected for X axis [IN]
          YVarType : Integer ;              // Variable selected for Y axis [IN]
          var iEvent : Integer ;            // Event number [OUT]
          var X : Single ;                  // X axis variable value
          var Y : Single                    // Y axis variable value
          ) : Boolean ;                     // TRUE = valid variable returned


     procedure CreateAmplitudeSortedEventList(
               StartEvent : Integer ;
               EndEvent : Integer ;
               var SortedEvents : Array of Integer ;
               var NumSortedEvents : Integer
               ) ;

    function GetVariableUnits( VarNum : Integer ) : String ;


    procedure UpdateFilterSet( Filters : Array of TFilter ) ;
    procedure UpdateEditEventPage ;

  public
    { Public declarations }
    procedure NewFile ;
    procedure DisplayRecord ;
    function IsClipboardDataAvailable : Boolean ;
    procedure CopyDataToClipboard ;
    procedure SaveDataToFile ;
    procedure CopyImageToClipboard ;
    procedure PrintDisplay ;
    procedure ZoomOutAll ;
    procedure ZoomIn( Chan : Integer ) ;
    procedure ZoomOut( Chan : Integer ) ;
    procedure ChangeDisplayGrid ;

  end;

var
  EventDetFrm: TEventDetFrm;

implementation

uses Mdiform, shared, Printrec, Printgra, Setaxes , Setfitpa,
  SetFitPars1Unit;

{$R *.DFM}
const
    cTDecayFromPeak = 0 ;
    cTDecayFromMidRise = 1 ;
    cTDecayFromA0 = 2 ;


procedure TEventDetFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
var
    TempName,TempPath : Array[0..255] of Char ;
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                Hist := THistogram.Create ;
                HistResults := TStringList.Create ;

                // Get path to temporary file directory }
                GetTempPath( High(TempPath), TempPath ) ;
                // Create a temporary file name
                GetTempFileName( TempPath, PChar('EDR'), 0, TempName ) ;
                EventAnalysisFile := FileCreate( String(TempName) ) ;
                if EventAnalysisFile < 0 then
                   ShowMessage('Cannot open detected events file!') ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Hist.Free ;
                HistResults.Free ;
                if EventAnalysisFile >= 0 then FileClose(EventAnalysisFile) ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TEventDetFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   ch,i : Integer ;
begin

     ClientWidth := Page.Left + Page.Width + 10 ;
     ClientHeight := Page.Top + Page.Height + 10 ;

     // Ensure form opens on Detect Events page
     Page.ActivePage := DetectEventsPage ;

     HeapBuffers( Allocate ) ;

     { Set block of CDR file to be scanned }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiValue := CdrFH.RecordDuration ;
     edRange.HiLimit := CdrFH.RecordDuration ;

     { Fill channel selection list }
     cbChannel.Clear ;
     cbReviewChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
         begin
         cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
         cbReviewChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
         end;
     cbChannel.ItemIndex := Min(Max(Settings.EventDetector.Channel,0),CdrFH.NumChannels-1) ;
     cbReviewChannel.ItemIndex := Min(Max(Settings.EventDetector.Channel,0),CdrFH.NumChannels-1) ;

     VarNames[vEventNum] := 'Event No.' ;
     VarNames[vTime] := 'Time' ;
     VarNames[vInterval] := 'Interval ' ;
     VarNames[vFrequencyAvg] := 'Frequency (avg.)' ;
     VarNames[vFrequencyInst] := 'Frequency (inst.)' ;
     VarNames[vPeak] := 'Peak' ;
     VarNames[vArea] := 'Area' ;
     VarNames[vTRise] := 'T.rise' ;
     VarNames[vTDecay] := 'T(90%)' ;
     VarNames[vTauDecay] := 'Tau(decay)' ;
     VarNames[vDuration] := 'Duration' ;
     VarNames[vBaseline] := 'Zero Baseline' ;

     // X/Y Plot variables
     cbPlotYVar.Clear ;
     cbPlotYVar.Items.AddObject( VarNames[vEventNum], TObject(vEventNum) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vTime], TObject(vTime) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vInterval], TObject(vInterval) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vFrequencyAvg], TObject(vFrequencyAvg) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vFrequencyInst], TObject(vFrequencyInst) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vPeak], TObject(vPeak) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vArea], TObject(vArea) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vTRise], TObject(vTRise) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vTDecay], TObject(vTDecay) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vTauDecay], TObject(vTauDecay) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vDuration], TObject(vDuration) ) ;
     cbPlotYVar.Items.AddObject( VarNames[vBaseline], TObject(vBaseline) ) ;
     cbPlotYVar.ItemIndex := 2 ;

     // X/Y Plot variables
     cbPlotXVar.Clear ;
     cbPlotXVar.Items.AddObject( VarNames[vEventNum], TObject(vEventNum) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vTime], TObject(vTime) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vInterval], TObject(vInterval) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vPeak], TObject(vPeak) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vArea], TObject(vArea) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vTRise], TObject(vTRise) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vTDecay], TObject(vTDecay) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vTauDecay], TObject(vTauDecay) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vDuration], TObject(vDuration) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vBaseline], TObject(vBaseline) ) ;
     cbPlotXVar.ItemIndex := 0 ;


     // Event filter criterion variable list
     cbVariable.Clear ;
     cbVariable.Items.AddObject( VarNames[vTime], TObject(vTime) ) ;
     cbVariable.Items.AddObject( VarNames[vInterval], TObject(vInterval) ) ;
     cbVariable.Items.AddObject( VarNames[vPeak], TObject(vPeak) ) ;
     cbVariable.Items.AddObject( VarNames[vArea], TObject(vArea) ) ;
     cbVariable.Items.AddObject( VarNames[vTRise], TObject(vTRise) ) ;
     cbVariable.Items.AddObject( VarNames[vTDecay], TObject(vTDecay) ) ;
     cbVariable.Items.AddObject( VarNames[vTauDecay], TObject(vTauDecay) ) ;
     cbVariable.Items.AddObject( VarNames[vDuration], TObject(vDuration) ) ;
     cbVariable.Items.AddObject( VarNames[vBaseline], TObject(vBaseline) ) ;
     cbVariable.ItemIndex := 0 ;

     // Histogram variables
     cbHistVar.Clear ;
     cbHistVar.Items.AddObject( VarNames[vInterval], TObject(vInterval) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vFrequencyAvg], TObject(vFrequencyAvg) ) ;
     cbPlotXVar.Items.AddObject( VarNames[vFrequencyInst], TObject(vFrequencyInst) ) ;
     cbHistVar.Items.AddObject( VarNames[vPeak], TObject(vPeak) ) ;
     cbHistVar.Items.AddObject( VarNames[vArea], TObject(vArea) ) ;
     cbHistVar.Items.AddObject( VarNames[vTRise], TObject(vTRise) ) ;
     cbHistVar.Items.AddObject( VarNames[vTDecay], TObject(vTDecay) ) ;
     cbHistVar.Items.AddObject( VarNames[vTauDecay], TObject(vTauDecay) ) ;
     cbHistVar.Items.AddObject( VarNames[vDuration], TObject(vDuration) ) ;
     cbHistVar.Items.AddObject( VarNames[vBaseline], TObject(vBaseline) ) ;
     cbHistVar.ItemIndex := 0 ;

     { Initialise histogram display cursors }
     plHist.ClearVerticalCursors ;
     HistCurs.C0 := plHist.AddVerticalCursor( clGray,'',0 ) ;
     HistCurs.C1 := plHist.AddVerticalCursor( clGray,'',0 ) ;
     plHist.LinkVerticalCursors( HistCurs.C0, HistCurs.C1 );

     // Create list of curves that can be fitted to histogram
     cbHistEqn.Clear ;
     cbHistEqn.Items.AddObject( 'None', TObject(None)) ;
     cbHistEqn.Items.AddObject( 'Gaussian', TObject(Gaussian)) ;
     cbHistEqn.Items.AddObject( '2 Gaussians', TObject(Gaussian2)) ;
     cbHistEqn.Items.AddObject( '3 Gaussians', TObject(Gaussian3)) ;
     cbHistEqn.Items.AddObject( 'Exponential', TObject(DecayingExp)) ;
     cbHistEqn.Items.AddObject( '2 Exponentials', TObject(DecayingExp2)) ;

     { Set initial  equation to None }
     cbHistEqn.ItemIndex := 0 ;
     lbHistResults.Caption := '' ;
     HistCurs.Read := plHist.AddVerticalCursor( clGreen, '?r',0) ;

     // Create list of curves that can be fitted to average
     cbAverageEqn.Clear ;
     cbAverageEqn.Items.AddObject( 'None', TObject(None)) ;
     cbAverageEqn.Items.AddObject( 'Linear', TObject(Linear)) ;
     cbAverageEqn.Items.AddObject( 'Exponential', TObject(Exponential)) ;
     cbAverageEqn.Items.AddObject( '2 Exponentials', TObject(Exponential2)) ;
     cbAverageEqn.Items.AddObject( '3 Exponentials', TObject(Exponential3)) ;
     cbAverageEqn.Items.AddObject( 'Decay Exp.', TObject(DecayingExp)) ;
     cbAverageEqn.Items.AddObject( '2 Decay Exps.', TObject(DecayingExp2)) ;
     cbAverageEqn.Items.AddObject( '3 Decay Exps.', TObject(DecayingExp3)) ;
     { Set initial  equation to None }
     cbAverageEqn.ItemIndex := 0 ;
     lbAvgFitResults.Caption := '' ;

     // Disable all selection filters
     for i := 0 to High(Filters) do Filters[i].Use := False ;
     UpdateFilterSet( Filters ) ;

     // Default no. of points in display
     edDetDisplayWidth.Scale := CDRFH.dt ;
     edDetDisplayWidth.Value := Round(5000.0) ;
     edEditDisplayWidth.Scale := edDetDisplayWidth.Scale ;
     edEditDisplayWidth.Value := edDetDisplayWidth.Value ;

     cbBaseline.Clear ;
     cbBaseline.Items.Add('At Start') ;
     cbBaseline.Items.Add('At Event') ;
     cbBaseline.Items.Add('Fixed') ;
     cbBaseline.ItemIndex := 0 ;

     { Initialise continuous and detected event displays }
     NewFile ;

     { Force a re-size to get controls in right/size place }
     ReSize ;

     { Display records }
     DisplayRecord ;

     end;


procedure TEventDetFrm.NewFile ;
{ ------------------------------------------------
  Initialise display when data file changed
  ------------------------------------------------}
var
   i,ch,Temp : Integer ;
   NPAvgBuf : Integer ;
begin

     // Analysis window duration
     if Settings.EventDetector.AnalysisWindow <= 0 then Settings.EventDetector.AnalysisWindow := 512 ;
     edEditDisplayWidth.Value := Settings.EventDetector.AnalysisWindow ;

     edPreTrigger.Value := Settings.EventDetector.PreTriggerFraction ;

     case Settings.EventDetector.DetectionMode of
          mdPatternMatch : rbPatternMatch.Checked := True ;
          mdRateOfRise : rbRateOfRise.Checked := True ;
          else rbThreshold.Checked := True ;
          end ;

     // Dead time
     edDeadTime.Value := Settings.EventDetector.DeadTime ;

     // Time threshold
     EdTimeThreshold.Value := Settings.EventDetector.tThreshold ;

     // Detection channel
     cbChannel.ItemIndex := Max(Min(Settings.EventDetector.Channel,CDRFH.NumChannels-1),0) ;
     cbReviewChannel.ItemIndex := Max(Min(Settings.EventDetector.Channel,CDRFH.NumChannels-1),0) ;

     // Threshold detection mode baseline traccking
     ckEnableBaselineTracking.Checked := Settings.EventDetector.EnableBaselineTracking ;
     edBaselineAveragingInterval.Value := Settings.EventDetector.BaselineAveragingInterval ;

     // Analysis polarity
     rbPositive.Checked := Settings.EventDetector.PositivePeaks ;
     rbNegative.Checked := not Settings.EventDetector.PositivePeaks ;

     // Event alignment
     cbEventAlignment.Clear ;
     cbEventAlignment.Items.Add('Max. rate of rise');
     cbEventAlignment.Items.Add('Mid-point of rise');
     cbEventAlignment.Items.Add('Detection threshold');
     cbEventAlignment.ItemIndex := Settings.EventDetector.Alignment ;

     cbBaseline.ItemIndex := Settings.EventDetector.Baseline ;
     ckSubtractBaseline.Checked := Settings.EventDetector.SubtractBaseline ;
     edZeroNumAvg.Value := Max(Settings.EventDetector.NumBaselinePoints,2) ;
     edZeroGap.Value := Max(Settings.EventDetector.NumBaselineGap,0) ;
     edTDecayPercentage.Value := Min(Max(Settings.EventDetector.TDecayPercent,0.0),100.0) ;
     cbDecayFrom.ItemIndex := Min(Max(Settings.EventDetector.TDecayFrom,0),
                              cbDecayFrom.Items.Count-1) ;

     Temp := Round(edDetDisplayWidth.Value) ;
     edDetDisplayWidth.Scale := CDRFH.dt ;
     edDetDisplayWidth.Value := Temp ;

     { Continuous record display channel }
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scDisplay.MaxPoints := Round(edDetDisplayWidth.Value) ;
     scDisplay.NumPoints := scDisplay.MaxPoints ;
     scDisplay.NumChannels := CdrFH.NumChannels ;
     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.NumPoints - 1  ;
     scDisplay.DisableChannelVisibilityButton := True ;

     if ADC <> Nil then FreeMem(ADC) ;
     GetMem( ADC, scDisplay.MaxPoints*scDisplay.NumChannels*4 ) ;
     scDisplay.SetDataBuf( ADC ) ;

     { Set display scaling information }
     for ch := 0 to scDisplay.NumChannels-1 do begin
         scDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scDisplay.yMin[ch] := Channel[ch].yMin ;
         scDisplay.yMax[ch] := Channel[ch].yMax ;
         scDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scDisplay.ChanColor[ch] := clBlue ;
         if ch = cbChannel.ItemIndex then scDisplay.ChanVisible[ch] := True
                                     else scDisplay.ChanVisible[ch] := False ;
         end ;
     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits ;

     { Create display cursors }
     scDisplay.ClearHorizontalCursors ;
     BaselineCursor := scDisplay.AddHorizontalCursor( cbChannel.ItemIndex,
                                                      clgray,
                                                      True,
                                                      'z' ) ;
     scDisplay.HorizontalCursors[0] := Channel[cbChannel.ItemIndex].ADCZero ;

     scDisplay.ClearVerticalCursors ;

     // Set upper limit of display slider bar range
     DisplayCursor := scDisplay.AddVerticalCursor( -1, clGreen, '?y?t' ) ;
     sbDisplay.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels) -1 ;
     sbDisplay.LargeChange := scDisplay.MaxPoints div 4 ;

     { Event detection function display channel }
     scDetDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDetDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scDetDisplay.MaxPoints := scDisplay.MaxPoints ;
     scDetDisplay.NumPoints := scDetDisplay.MaxPoints ;
     scDetDisplay.NumChannels := 1 ;
     scDetDisplay.xMin := 0 ;
     scDetDisplay.xMax := scDetDisplay.NumPoints - 1  ;
     scDetDisplay.DisableChannelVisibilityButton := True ;

     // Allocate detection buffer
     if DetBuf <> Nil then FreeMem( DetBuf ) ;
     GetMem( DetBuf, scDetDisplay.MaxPoints*2 ) ;
     scDetDisplay.SetDataBuf( DetBuf ) ;

     { Set display scaling information }
     scDetDisplay.ChanUnits[0] := Channel[cbChannel.ItemIndex].ADCUnits ;
     scDetDisplay.ChanName[0] := 'Det. Criterion' ;
     scDetDisplay.yMin[0] := -Round((scDisplay.yMax[cbChannel.ItemIndex] - scDisplay.yMin[cbChannel.ItemIndex])*0.5) ;
     scDetDisplay.yMax[0] := Round((scDisplay.yMax[cbChannel.ItemIndex] - scDisplay.yMin[cbChannel.ItemIndex])*0.5) ;

     scDetDisplay.ChanScale[0] := Channel[cbChannel.ItemIndex].ADCScale ;
     scDetDisplay.ChanUnits[0] := Channel[cbChannel.ItemIndex].ADCUnits ;
     scDetDisplay.ChanZero[0] := 0 ;
     scDetDisplay.ChanOffsets[0] := 0 ;
     scDetDisplay.ChanColor[0] := clBlue ;
     scDetDisplay.ChanVisible[0] := True ;
     scDetDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDetDisplay.TUnits := Settings.TUnits ;

     { Create detection threshold cursor }
     scDetDisplay.ClearHorizontalCursors ;
     DetZeroCursor := scDetDisplay.AddHorizontalCursor( 0,clgray,True,'z' ) ;
     ThresholdCursor := scDetDisplay.AddHorizontalCursor( 0,clgray,True,'threshold' ) ;

     // Set threshold
     edTHreshold.Value := Settings.EventDetector.yThreshold ;
     scDetDisplay.HorizontalCursors[ThresholdCursor] := Round(edThreshold.Value) ;
     scDetDisplay.HorizontalCursors[DetZeroCursor] := 0 ;

     scDetDisplay.ClearVerticalCursors ;
     DisplayCursor := scDetDisplay.AddVerticalCursor( -1, clGReen, '?y?t' ) ;

     // Set up display on Edit Events panel
     // -----------------------------------

     Temp := Round(edEditDisplayWidth.Value) ;
     edEditDisplayWidth.Scale := 1000.*CDRFH.dt ;
     edEditDisplayWidth.Value := Temp ;

     scEditDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scEditDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scEditDisplay.MaxPoints := Round(edEditDisplayWidth.Value) ;
     scEditDisplay.NumPoints := scEditDisplay.MaxPoints ;
     scEditDisplay.NumChannels := CdrFH.NumChannels ;
     scEditDisplay.xMin := 0 ;
     scEditDisplay.xMax := scEditDisplay.NumPoints - 1  ;
     scEditDisplay.DisableChannelVisibilityButton := True ;

     if EditBuf <> Nil then FreeMem( EditBuf ) ;
     GetMem( EditBuf, scEditDisplay.MaxPoints*scEditDisplay.NumChannels*2 ) ;
     scEditDisplay.SetDataBuf( EditBuf ) ;

     { Set display scaling information }
     for ch := 0 to scEditDisplay.NumChannels-1 do begin
         scEditDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scEditDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scEditDisplay.yMin[ch] := Channel[ch].yMin ;
         scEditDisplay.yMax[ch] := Channel[ch].yMax ;
         scEditDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scEditDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scEditDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scEditDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scEditDisplay.ChanColor[ch] := clBlue ;
         if ch = cbChannel.ItemIndex then scEditDisplay.ChanVisible[ch] := True
                                     else scEditDisplay.ChanVisible[ch] := False ;
         end ;
     scEditDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scEditDisplay.TUnits := Settings.TUnits ;

     { Create display cursors }
     scEditDisplay.ClearVerticalCursors ;
     // Readout cursor
     EditC0Cursor := scEditDisplay.AddVerticalCursor( -1, clGray, 'a0' ) ;
     EditC1Cursor := scEditDisplay.AddVerticalCursor( -1, clGray, 'a1' ) ;
     scEditDisplay.LinkVerticalCursors( EditC0Cursor, EditC1Cursor );
     scEditDisplay.VerticalCursors[EditC0Cursor] := 2 ;
     scEditDisplay.VerticalCursors[EditC1Cursor] := scEditDisplay.NumPoints-2 ;
     EditCursor := scEditDisplay.AddVerticalCursor( -1, clGreen, '?y?t' ) ;

     scEditDisplay.ClearHorizontalCursors ;
     // Baseline cursor
     scEditDisplay.AddHorizontalCursor( cbChannel.ItemIndex, clgray, True, 'z' ) ;

     { Set display scaling information }
     scMarkDisplay.MaxADCValue := 1 ; //Channel[0].ADCMaxValue ;
     scMarkDisplay.MinADCValue := 0 ;//-Channel[0].ADCMaxValue - 1 ;
     scMarkDisplay.MaxPoints := Round(edEditDisplayWidth.Value) ;
     scMarkDisplay.NumPoints := scMarkDisplay.MaxPoints ;
     scMarkDisplay.xMin := 0 ;
     scMarkDisplay.xMax := scMarkDisplay.NumPoints - 1  ;
     scMarkDisplay.NumChannels := 1 ;
     scMarkDisplay.ChanUnits[0] := '' ;
     scMarkDisplay.ChanName[0] := 'Det.' ;
     scMarkDisplay.yMin[0] := 0 ;
     scMarkDisplay.yMax[0] := 1 ;
     scMarkDisplay.ChanScale[0] := 1.0 ;
     scMarkDisplay.ChanZero[0] := 0 ;
     scMarkDisplay.ChanOffsets[0] := 0 ;
     scMarkDisplay.ChanColor[0] := clBlue ;
     scMarkDisplay.ChanVisible[0] := True ;
     scMarkDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scMarkDisplay.TUnits := Settings.TUnits ;
     scMarkDisplay.DisableChannelVisibilityButton := True ;

     // Set upper limit of slider bar
     sbEditDisplay.Max := sbDisplay.Max ;
     sbEditDisplay.LargeChange := scEditDisplay.MaxPoints div 4 ;

     edTauRise.LoLimit := CDRFH.dt ;
     edTauDecay.LoLimit := CDRFH.dt ;

     // Open an existing event list list or create a new one
     LoadEventList ;

     // Set up event detector controls
     SetupDetector ;

     // Set X/Y plot available flag to no plot
     { Create X/Y plot cursor }
     plPlot.ClearVerticalCursors ;
     plPlot.AddVerticalCursor( clGreen, '?r',0 ) ;
     PlotAvailable := False ;

     // Set histogram available flag to none
     { Create X/Y plot cursor }
     HistAvailable := False ;

     { Averaged record display channel }
     scAverageDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scAverageDisplay.MinADCValue := -Channel[0].ADCMaxValue - 1 ;
     scAverageDisplay.MaxPoints := Round(edEditDisplayWidth.Value) ;
     scAverageDisplay.NumPoints := scAverageDisplay.MaxPoints ;
     scAverageDisplay.NumChannels := CdrFH.NumChannels ;
     scAverageDisplay.xMin := 0 ;
     scAverageDisplay.xMax := scAverageDisplay.NumPoints - 1  ;
     scAverageDisplay.DisableChannelVisibilityButton := True ;

     // Create averages buffer
     if AvgBuf <> Nil then FreeMem(AvgBuf) ;
     NPAvgBuf := scAverageDisplay.MaxPoints*scAverageDisplay.NumChannels ;
     GetMem( AvgBuf, NPAvgBuf*2 ) ;
     // Clear average buffer
     for i := 0 to NPAvgBuf-1 do AvgBuf^[i] := 0 ;
     NumEventsAveraged := 0 ;

     scAverageDisplay.SetDataBuf( AvgBuf ) ;

     { Set display scaling information }
     for ch := 0 to scAverageDisplay.NumChannels-1 do begin
         scAverageDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scAverageDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scAverageDisplay.yMin[ch] := Channel[ch].yMin ;
         scAverageDisplay.yMax[ch] := Channel[ch].yMax ;
         scAverageDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scAverageDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scAverageDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scAverageDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scAverageDisplay.ChanColor[ch] := clBlue ;
         if ch = cbChannel.ItemIndex then scAverageDisplay.ChanVisible[ch] := True
                                     else scAverageDisplay.ChanVisible[ch] := False ;
         end ;
     scAverageDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scAverageDisplay.TUnits := Settings.TUnits ;

     { Create display cursors }
     // Readout cursor
     scAverageDisplay.ClearVerticalCursors ;

     AverageT0Cursor := scAverageDisplay.AddVerticalCursor( -1, clGray, 't0' ) ;
     AverageC0Cursor := scAverageDisplay.AddVerticalCursor( -1, clGray, 'a0' ) ;
     AverageC1Cursor := scAverageDisplay.AddVerticalCursor( -1, clGray, 'a1' ) ;
     scAverageDisplay.LinkVerticalCursors( AverageC0Cursor, AverageC1Cursor );
     AverageCursor := scAverageDisplay.AddVerticalCursor( -1, clGreen, '?y?t' ) ;
     // Baseline cursor
     scAverageDisplay.ClearHorizontalCursors ;
     scAverageDisplay.AddHorizontalCursor( cbChannel.ItemIndex, clgray, True, 'z' ) ;

     scAverageDisplay.VerticalCursors[AverageCursor] := scAverageDisplay.NumPoints div 2 ;
     scAverageDisplay.VerticalCursors[AverageT0Cursor] := Round(scAverageDisplay.NumPoints*edPreTrigger.Value) ;
     scAverageDisplay.VerticalCursors[AverageC0Cursor] := scAverageDisplay.VerticalCursors[AverageT0Cursor]
                                                          + 1 ;
     scAverageDisplay.VerticalCursors[AverageC1Cursor] := scAverageDisplay.NumPoints - 3 ;

     // Event counting interval for average frequency plot
     edAverageInterval.Value := Settings.EventDetector.AvgFrequencyInterval ;

     // Set display calibration grid
     ChangeDisplayGrid ;

     // Indicate that event waveform analysis file is out of date
     EventAnalysisFileUpdateRequired := True ;
     BaselineSplinesAvailable := False ;
     AbortFlag := False ;
     ComputationInProgress := False ;
     BinRangePanel.Visible := False ;

     end ;


procedure TEventDetFrm.DisplayRecord ;
{ ---------------------------------------------
  Display currently selected block of data file
  ---------------------------------------------}
var
   InitialiseRunningMean : Boolean ;
begin

   if ADC = Nil then Exit ;
   if DetBuf = Nil then Exit ;

   scDisplay.xOffset := sbDisplay.Position ;

   if rbThreshold.Checked then
      begin
      InitialiseRunningMean := True ;
      scDisplay.NumPoints := Threshold( sbDisplay.Position,
                                        scDisplay.MaxPoints,
                                        InitialiseRunningMean,
                                        ADC,
                                        DetBuf )
      end
   else if rbRateOfRise.Checked then
      begin
      scDisplay.NumPoints := RateOfRise( sbDisplay.Position,
                                         scDisplay.MaxPoints,
                                         ADC,
                                         DetBuf ) ;
      end
   else
      begin
      scDisplay.NumPoints := MatchTemplate( sbDisplay.Position,
                                            scDisplay.MaxPoints,
                                            ADC,
                                            DetBuf ) ;

      end ;

   scDisplay.SetDataBuf( ADC ) ;
   scDisplay.Invalidate ;

   scDetDisplay.SetDataBuf( DetBuf ) ;
   scDetDisplay.NumPoints := scDisplay.NumPoints ;
   scDetDisplay.Invalidate ;

   end;


function  TEventDetFrm.RateofRise(
          StartAtSample : Integer ;        // Sample to start at
          NumPoints : Integer ;            // Number of A/D sample points
          YBuf : PSmallIntArray ;          // Signal data buffer
          YDet :  PSmallIntArray          // Rate of change data buffer (out)
          ) : Integer ;
{ ---------------------------------
  Calculate rate of rise of signal
  ---------------------------------}
const
   jLow = -2 ;
   jHigh = 2 ;
var
   i,j,k,iOffset,PreStartAtSample : Integer ;
   A : Array[jLow..jHigh] of Single ;
   Diff : Single ;
   ChannelOffset : Integer ;
   NPBuf : Integer ;
   Buf : PSmallIntArray ;
begin

   Result := NumPoints ;
   if NumPoints <= 0 then Exit ;

   // Read A/D sample data from file (with pre-start samples)
   PreStartAtSample := Max( StartAtSample + jLow,0 ) ;
   NPBuf := (NumPoints-jLow+jHigh)*CDRFH.NumChannels ;
   GetMem( Buf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt));
   ReadCDRBuffer(CdrFH,PreStartAtSample,Buf^,NPBuf) ;

   for j := jLow to jHigh do A[j] := j ;

   iOffset := (StartAtSample - PreStartAtSample) ;
   ChannelOffset := Channel[cbChannel.ItemIndex].ChannelOffset ;
   for i := iOffset to iOffset + (NumPoints-1) do
       begin
       Diff := 0.0 ;
       for j := jLow to jHigh do
           begin
           k := Min(Max(i+j,0),iOffset+NumPoints-1+jHigh)*CdrFH.NumChannels +
                ChannelOffset ;
           Diff := Diff + A[j]*Buf[k] ;
           end ;
       YDet[i-iOffset] := Round(Min(Max(Diff,-Channel[0].ADCMaxValue-1),Channel[0].ADCMaxValue)) ;
       end ;

   // Load signal buffer Starting at StartAtSample
   if YBuf <> Nil then Result := ReadCDRBuffer(CdrFH,StartAtSample,YBuf^,NumPoints) ;
//   Result := Min(CDRFH.NumSamplesInFile - StartAtSample,NumPoints) ;

   FreeMem(Buf) ;

   end ;


function  TEventDetFrm.Threshold(
          StartAtSample : Integer ;        // Sample to start at
          NumPoints : Integer ;            // Number of A/D sample points
          var InitialiseRunningMean : Boolean ;//
          YBuf : PSmallIntArray ;          // Signal data buffer (out)
          YDet : PSmallIntArray            // Detection criterion buffer (out)
          ) : Integer ;
{ ------------------------------
  Calculate detection threshold
  ------------------------------}
var
   i,j : Integer ;
   NumSamplesInRunningMean : single ;
begin

   Result := NumPoints ;
   if NumPoints <= 0 then Exit ;

   NumSamplesInRunningMean := edBaselineAveragingInterval.Value / CdrFH.dt ;
   Settings.EventDetector.BaselineAveragingInterval := edBaselineAveragingInterval.Value ;

   // Read A/D sample data from file
   Result := ReadCDRBuffer(CdrFH,StartAtSample,YBuf^,NumPoints) ;

   // Initialise running mean (if required)
   if InitialiseRunningMean and ckEnableBaselineTracking.checked then
      begin
      j := Channel[cbChannel.ItemIndex].ChannelOffset ;
      i := ROund(RunningMean) ;
      RunningMean := YBuf[j] ;
      InitialiseRunningMean := False ;
      end ;

   j := Channel[cbChannel.ItemIndex].ChannelOffset ;
   if not ckEnableBaselineTracking.checked then RunningMean := 0 ;

   for i := 0 to NumPoints-1 do
       begin

       // Difference between signal and running mean zero level
       YDet[i] := Round(Min(Max( (YBuf[j] - RunningMean),
                    -Channel[0].ADCMaxValue-1),Channel[0].ADCMaxValue)) ;

       // Update running mean baseline average
       if ckEnableBaselineTracking.checked then
          RunningMean := ((NumSamplesInRunningMean*RunningMean)+ YBuf[j])
                         /(NumSamplesInRunningMean + 1.0 ) ;

       j := j + CdrFH.NumChannels ;

       end ;


   end ;


procedure TEventDetFrm.TimerTimer(Sender: TObject);
// ----------------
// Timed procedures
// ----------------
begin
    if SaveEventListRequested then SaveEventList ;
    end;


function  TEventDetFrm.MatchTemplate(
          StartAtSample : Integer ;        // Sample to start at
          NumPoints : Integer ;            // Number of A/D sample points
          YBuf : PSmallIntArray ;          // Signal data buffer (out)
          YDet : PSmallIntArray            // Detection criterion buffer (out)
          ) : Integer ;
{ ------------------------------------------------
  Calculate template matching detection criterion
  ------------------------------------------------}
var
   i,j,k,PreStartAtSample : Integer ;
   FirstTemplateSample,LastTemplateSample,NumTemplateSamples : Integer ;
   t : Double ;
   NumRead : Integer ;
   Y,SumY,SumYSq,SumTemplate,SumTemplateSq,SumYxTemplate : Double ;
   Amplitude,Scale,Offset,SSE,StandardError : Double ;
   InitialiseTemplate : Boolean ;
   Template : Array[-1024..2048] of Single ;
   NumScansPerTau : Integer ;
   ChannelOffset : Integer ;
   iIn,iOut : Integer ;
   Denom : Double ;
   NPBuf,iEndSample : Integer ;
   Buf : PSmallIntArray ;
begin

   Result := NumPoints ;
   if NumPoints <= 0 then Exit ;

   // Rise and decay time constants of EPSC template
   Settings.EventDetector.TauRise := edTauRise.Value ;
   Settings.EventDetector.TauDecay := edTauDecay.Value ;

   // Set last 75% of template to be a decay time constant
   NumScansPerTau := Max( Round(Settings.EventDetector.TauDecay/CdrFH.dt),1 ) ;
   LastTemplateSample := Min( NumScansPerTau,High(Template)) ;
   // Set first 25% of template as pre-event baseline
   FirstTemplateSample := Max(-NumScansPerTau,Low(Template)) ;
   // If insufficient sample data for pre-trigger set it to zero
   if (StartAtSample + FirstTemplateSample) < 0 then FirstTemplateSample := 0 ;
   // Total number of samples in template
   NumTemplateSamples := LastTemplateSample - FirstTemplateSample + 1 ;
   Scale := 1.0/ edThreshold.Scale ;

   // Create template
   SumTemplate := 0.0 ;
   SumTemplateSq := 0.0 ;
   for i := FirstTemplateSample to LastTemplateSample do
       begin
       if i >= 0 then
          begin
          t := i*CdrFH.dt ;
          Template[i] := (1.0 - exp(-t/Settings.EventDetector.TauRise))*exp(-t/Settings.EventDetector.TauDecay) ;
          end
       else Template[i] := 0.0 ;
       SumTemplate := SumTemplate + Template[i] ;
       SumTemplateSq := SumTemplateSq + (Template[i]*Template[i]) ;
       end ;

   // Read A/D sample buffer to be scanned
   NPBuf := NumPoints*2 ;

   PreStartAtSample := Max( StartAtSample+FirstTemplateSample,0 ) ;
   iEndSample := Min( PreStartAtSample + NumPoints*2, CDRFH.NumSamplesInFile -1);
   NPBuf := iEndSample - PreStartAtSample + 1 ;
   GetMem( Buf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt));
   for iIn := 0 to (NPBuf*CDRFH.NumChannels)-1 do Buf[iIn] := 0 ;
   NumRead := ReadCDRBuffer(CdrFH,PreStartAtSample,Buf^,NPBuf) ;

   iOut := 0 ;
   for iIn := -FirstTemplateSample to -FirstTemplateSample + NumPoints-1 do
      begin

      // Compute sums
      SumY := 0.0 ;
      SumYSq := 0.0 ;
      SumYxTemplate := 0.0 ;
      ChannelOffset := Channel[cbChannel.ItemIndex].ChannelOffset ;
      for k := FirstTemplateSample to LastTemplateSample do
          begin
          j := (iIn+k)*CdrFH.NumChannels + ChannelOffset ;
          Y := Buf[j] ;
          SumY := SumY + Y ;
          SumYSq := SumYSq + Y*Y ;
          SumYxTemplate := SumYxTemplate + (Y*Template[k]) ;
          end ;

       // Calculate best fit template amplitude
       Denom := (SumTemplateSq - ((SumTemplate*SumTemplate)/NumTemplateSamples)) ;
       if Denom <> 0.0 then
          Amplitude := (SumYxTemplate - ((SumTemplate*SumY)/NumTemplateSamples)) / Denom
       else Amplitude := 0.0 ;

       // Calculate best fit template offset
       Offset := (SumY - Amplitude*SumTemplate)/NumTemplateSamples ;

       // Calculate sum of squared errors

       SSE := SumYSq
              + (Amplitude*Amplitude*SumTemplateSq)
              + (NumTemplateSamples*Offset*Offset)
              - 2.0*(Amplitude*SumYxTemplate
                     + Offset*SumY
                     - (Amplitude*Offset*SumTemplate)) ;

       StandardError := Sqrt(Abs(SSE)/(NumTemplateSamples-1)) ;

       // Detection criterion
       if StandardError > 0.0 then
          begin
          YDet[iOut] := Round( Min(Max( (Scale*Amplitude)/StandardError,
                                 -Channel[0].ADCMaxValue-1),Channel[0].ADCMaxValue)) ;
          end
       else YDet[iOut] := 0 ;
       Inc(iOut) ;

      end ;

   // Load signal buffer Starting at StartAtSample
   if YBuf <> Nil then Result := ReadCDRBuffer(CdrFH,StartAtSample,YBuf^,NumPoints) ;

   FreeMem(Buf) ;

   end ;


function  TEventDetFrm.FindPeakTemplateMatch(
          iDetectedAtSample : Integer                    // (IN) Point threshold crossed
          ) : Integer ;                        // (RET) Point of peak templatre match
// -------------------------------------------------
// Find point at which templates best matches signal
// -------------------------------------------------
var
    i,NPBuf,NPHalf,iPeakAt,iEventAt : Integer ;
    yMax,yMin,y,Polarity : Integer ;
    Buf : PSmallIntArray ;
    DBuf : PSmallIntArray ;
    DescentCount,MaxDescentCount : Integer ;
    iStartSample,iEndSample : Integer ;
begin

   // Get a signal buffer post detection point
   NPBuf := 500 ;
   NPHalf := NPBuf div 2 ;
   iStartSample := Max(iDetectedAtSample - NPHalf,0) ;
   iEventAt := iDetectedAtSample - iStartSample ;
   iEndSample := Min(iStartSample + NPBuf - 1, CDRFH.NumSamplesInFile - 1);
   NPBuf := iEndSample - iStartSample + 1 ;

   // Allocate buffers
   GetMem( Buf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt) ) ;
   GetMem( DBuf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt) ) ;

   // Calculate template match
   MatchTemplate( iStartSample,NPBuf,Nil,DBuf ) ;

   // Determine positive/negative-going polarity of signal
   YMin := High(Integer) ;
   YMax := Low(Integer) ;
   for i := 0 to NPBuf-1 do
       begin
       y := DBuf^[i] ;
       if y > YMax then YMax := y ;
       if y < YMin then YMin := y ;
       end;
   if Abs(YMax) >= Abs(YMin) then Polarity := 1
                             else Polarity := -1 ;

   // Find peak
   yMax := Low(Integer) ;
   i := Max(iEventAt - 1,0);
   DescentCount := 0 ;
   MaxDescentCount := 10 ;
   iPeakAt := i ;
   repeat
       Inc(i) ;
       y := Polarity*DBuf^[i] ;
       if y >= YMax then
          begin
          YMax := y ;
          iPeakAt := i ;
          DescentCount := 0 ;
          end
       else Inc(DescentCount) ;
       until (DescentCount > MaxDescentCount) or (i >= NPBuf) ;

    Result := iStartSample + iPeakAt ;

    FreeMem(DBuf) ;
    FreeMem(Buf) ;

    end ;


function  TEventDetFrm.FindMidPointOfRise(
          iDetectedAtSample : Integer          // (IN) Point threshold crossed
          ) : Integer ;                        // (RET) Point of peak rate of rise
// -----------------------------------------------------------
// Find point on signal rising edge with greatest rate of rise
// -----------------------------------------------------------
var
    iStartSample,iEndSample : Integer ;
    i,j,NPBuf,NPHalf,iStart,iEnd : Integer ;
    yMax,yMin,y,yMid,Polarity,yMaxAt,yMinAt : Integer ;
    Buf : PSmallIntArray ;
    DBuf : PSmallIntArray ;
    iPeakRateOfRise,iYMidAt : Integer ;
    Done : Boolean ;
begin

   // Get a signal buffer post detection point
   // Set no. samples in rising edge detection buffer equal to dead time
   NPBuf := Min(Max(Round(edDeadTime.Value/CDRFH.dt),200),4) ;
   NPHalf := NPBuf div 2 ;
   iStartSample := Max(iDetectedAtSample - NPHalf,0) ;
   iEndSample := Min(iStartSample + NPBuf - 1, CDRFH.NumSamplesInFile - 1);
   NPBuf := iEndSample - iStartSample + 1 ;

   // Allocate buffers
   GetMem( Buf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt) ) ;
   GetMem( DBuf, NPBuf*CDRFH.NumChannels*SizeOf(SmallInt) ) ;

   // Calculate rate of rise
   RateOfRise( iStartSample,NPBuf,Buf,DBuf ) ;
   // Extract detection channel
   for i := 0 to NPBuf-1 do
       begin
       j := i*CDRFH.NumChannels + cbChannel.ItemIndex ;
       Buf^[i] := Buf^[j] ;
       end;

   // Determine positive/negative-going polarity of signal from threshold value
   if edThreshold.Value >= 0.0 then Polarity := 1
                               else Polarity := -1 ;

   if cbEventAlignment.ItemIndex = AlignMaxRateOfRise then
      begin
      // Align by max. rate of rise
      // --------------------------
      // Find peak rate of rise
      YMax := Low(Integer) ;
      iPeakRateOfRise := 0 ;
      for i := 0 to NPBuf-1 do
          begin
          y := DBuf^[i]*Polarity ;
          if y >= YMax then
             begin
             YMax := y ;
             iPeakRateOfRise := i ;
             end;
          end;

      Result := iStartSample + iPeakRateOfRise ;

      end
   else if cbEventAlignment.ItemIndex = AlignMidPointOfRise then
      begin
      // Align by mid-point of rise
      // --------------------------
      // Find Min./Max. of signal rising edge
      iStart := 0 ;
      iEnd := NPBuf-1 ;
      yMin := High(Integer) ;
      yMax := Low(Integer) ;
      for i := iStart to iEnd do
          begin
          y := Buf^[i];
          if y >= yMax then
             begin
             YMax := y ;
             YMaxAt := i ;
             end;
          if y <= yMin then
             begin
             YMin := y ;
             YMinAt := i ;
             end;
          end;

      // Find mid-point of rising edge
      yMid := (YMax + YMin) div 2 ;
      iYMidAt := NPHalf ;
      iStart := Min(YMaxAt,YMinAt);
      iEnd := Max(YMaxAt,YMinAt);
      for i := iStart to iEnd-1 do
          begin
          if ((Buf^[i] - YMid)*(Buf^[i+1] - YMid)) <= 0 then iYMidAt := i ;
          end;

      Result := iStartSample + iYMidAt

      end
   else
     begin
     // Leave aligned at detection threshold
     Result := iDetectedAtSample ;
     end;

   FreeMem(Buf) ;
   FreeMem(DBuf) ;

    end ;


procedure TEventDetFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     AbortFlag := True ;
     Application.ProcessMessages ;

     if AvgBuf <> Nil then FreeMem( AvgBuf ) ;
     if DetBuf <> Nil then FreeMem( DetBuf ) ;
     if EditBuf <> Nil then FreeMem( EditBuf ) ;
     if ADC <> Nil then FreeMem( ADC ) ;

     HeapBuffers( Deallocate ) ;
     Main.mnDetectEvent2.Enabled := True ;

     // Save detected event list
     SaveEventList ;

     { Update EDR file header }
     SaveCDRHeader( CDRfH ) ;

     { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;

     Action := caFree ;

     end;


procedure TEventDetFrm.FormResize(Sender: TObject);
// ----------------------------------------
// Adjust size/position of controls on form
// ----------------------------------------
begin
     // Adjust size of tabs page to form size

     scDisplay.Width := Max( DetectEventsPage.Width - scDisplay.Left - 2,2) ;
     scDetDisplay.Width := scDisplay.Width ;
//     sbDisplay.Width := scDisplay.Width ;

     scDisplay.Height := Max( (2*(sbDisplay.Top - scDisplay.Top)) div 3,2) ;
     scDetDisplay.Top := scDisplay.Top + scDisplay.Height + 2 ;
     scDetDisplay.Height := Max( sbDisplay.Top - scDetDisplay.Top - 1,2) ;

     scDetDisplay.Left := scDisplay.Left ;

     // Display points label and box

     scMarkDisplay.Height := Max( (sbEditDisplay.Top - scEditDisplay.Top) div 5,2) ;
     scMarkDisplay.Top := sbEditDisplay.Top - scMarkDisplay.Height ;
     scEditDisplay.Height := Max( scMarkDisplay.Top - scEditDisplay.Top - 2, 2) ;

     // Set horizontal positions/sizes of controls
     scEditDisplay.Width := Max(DetectEventsPage.Width - scEditDisplay.Left - 2,2) ;
     scMarkDisplay.Width := scEditDisplay.Width ;

     // Set sizes of controls on X/Y Plot page

     plPlot.Width := Max( XYPlotPage.ClientWidth - plPlot.Left - 5,2) ;
     plPlot.Height := Max( XYPlotPage.ClientHeight - plPlot.Top - 5,2) ;

     // Set sizes of controls on histogram page
     meHistResults.Height := Max( HistGrp.Height - meHistResults.Top - 5,2) ;
     lbHistResults.Width := Max( HistResultsGrp.Width - lbHistResults.Left - 5,2) ;

     plHist.Width := Max( HistPage.Width - plHist.Left - 2,2) ;

     // Histogram display area height
     plHist.Height := Max( HistResultsGrp.Top - plHist.Top - 5,2) ;

     // Set sizes of controls on average page

     lbAvgFitResults.Width := Max(AverageResultsGrp.Width - lbAvgFitResults.Left - 5,2) ;

     scAverageDisplay.Height := Max(AverageResultsGrp.Top - scAverageDisplay.Top - 2,2) ;
     scAverageDisplay.Width := Max(AveragePage.ClientWidth - scAverageDisplay.Left - 5,2) ;

     end;


procedure TEventDetFrm.sbDisplayChange(Sender: TObject);
// -------------------------------
// Event detection display changed
// -------------------------------
begin
    DisplayRecord ;
    end;


procedure TEventDetFrm.bDetectClick(Sender: TObject);
// -------------
// Detect events
// -------------
var
    iEnd,iSample,StartAtSample,EndAtSample,iEventSample : Integer ;
    DeadSamples : Integer ;
    i,OverThresholdCount,TimeThreshold,ThresholdLevel, Polarity : Integer ;
    Done,NewBufferNeeded, UpdateStatusBar, InitialiseRunningMean : Boolean ;

begin

     { Let user clear event list }
     if NumEvents > 0 then
        begin
        if MessageDlg('Clear existing events in list',mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then
           begin
           // Delete any existing event list files
           DeleteFile( PChar(ChangeFileExt(CdrFH.FileName,EventFileExtension))) ;
           NumEvents := 0 ;
           end
        else Exit ;
        end ;

     bDetect.Enabled := False ;
     bAbort.Enabled := True ;
     ComputationInProgress := True ;
     AbortFlag := False ;

     // Save detection channel
     Settings.EventDetector.Channel := cbChannel.ItemIndex ;

     // Number of samples to skip after an event has been detected
     DeadSamples := Round( edDeadTime.Value / CdrFH.dt ) ;
     Settings.EventDetector.DeadTime := edDeadTime.Value ;

     { Range of samples to be scanned for events }
     if rbAllRecords.Checked then
        begin
        StartAtSample := 0 ;
        EndAtSample := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else
        begin
        StartAtSample := Round( edRange.LoValue/CdrFH.dt ) ;
        EndAtSample :=   Round( edRange.HiValue/CdrFH.dt ) ;
        end ;

    // Loop initialisations
    Done := False ;
    NewBufferNeeded := True ;
    UpdateStatusBar := False ;
    iSample := StartAtSample ;

    // Initialise time threshold
    ThresholdLevel := Round(edThreshold.Value) ;
    if  ThresholdLevel > 0 then Polarity := 1
                           else Polarity := -1 ;
    OverThresholdCount := 0 ;

    Settings.EventDetector.tThreshold := EdTimeThreshold.Value ;
    if rbThreshold.Checked then
       begin
       TimeThreshold := Round( edTimeThreshold.Value/CdrFH.dt ) ;
       InitialiseRunningMean := True ;
       end
    else TimeThreshold := 0 ;

    // Detect events loop
    // ------------------
    i := 0 ;
    iEnd := 0 ;
    while not Done do begin

        if NewBufferNeeded then begin
           // Load and display new buffer of A/D samples
           // ------------------------------------------
           sbDisplay.Position := iSample ;

           // Display record and load ADC and DetBuf buffers
           DisplayRecord ;

           // Start/end of ADC buffer indices
           iEnd := scDisplay.MaxPoints - 1 ;

           // Display it
           scDisplay.xOffset := sbDisplay.Position ;
           scDisplay.Invalidate ;

           scDetDisplay.xOffset := 0 ;
           scDetDisplay.Invalidate ;

           NewBufferNeeded := False ;
           i := 0 ;

           UpdateStatusBar := True ;
           Application.ProcessMessages ;

           end ;

//        scDetDisplay.AddPointToLine( iLine, i, 0 ) ;
        // Does detection criterion exceed threshold?
        if (Polarity*DetBuf^[i]) > (Polarity*ThresholdLevel) then
           begin

           // Increment threshold exceeded counter
           Inc(OverThresholdCount) ;

           // If threshold has been exceeded for a sufficiently long time accept detection
           if OverThresholdCount >= TimeThreshold then
              begin
//              y := Round(edThreshold.Value)*2 ;

              // Save location in event in event list
              if NumEvents <= High(Events) then
                 begin
                 // Find point of peak match between template and signal
                 if rbPatternMatch.Checked then iSample := FindPeakTemplateMatch(iSample)
                 else if rbThreshold.Checked then
                      begin
                      // Subtract TimeThreshold samples to find detection point
                      iSample := Max(iSample - TimeThreshold,0) ;
                      i := i - TimeThreshold ;
                      InitialiseRunningMean := True ;
                      end;
                 // Find mid-point of signal rising edge
                iEventSample := FindMidPointOfRise(iSample) ;
      //          iEventSample := iSample ;
                 Events[NumEvents] := Max(iEventSample,0) ;
                 Inc(NumEvents) ;
                 end ;

              iSample := iSample + DeadSamples ;
              i := i + DeadSamples ;
              OverThresholdCount := 0 ;
              UpdateStatusBar := True ;
              end ;
           end
        else begin
           // Cancel over-threshold count if signal falls  below threshold
           OverThresholdCount := 0 ;
           end ;

        // Increment counters
        Inc(iSample) ;
        Inc(i) ;
        if (i > iEnd) then NewBufferNeeded := True ;

        if (iSample > EndAtSample) or (not bAbort.Enabled) then Done := True ;

        // Update status bar
        if UpdateStatusBar then begin
           Main.StatusBar.SimpleText := format(
                                        ' Detect Events : %.2f/%.2f s (%d events detected)',
                                          [iSample*CdrFH.dt,
                                           EndAtSample*CdrFH.dt,
                                           NumEvents] ) ;
           UpdateStatusBar := False ;
           end ;

        if AbortFlag then Break ;

        end ;

    // Save list of events in file
    SaveEventList ;
    Main.StatusBar.SimpleText := format(' Detect Events : %d events detected.',
                                          [NumEvents] ) ;
    WriteToLogFile(Main.StatusBar.SimpleText) ;

    // Remove final detection flag line
    scDetDisplay.CreateLine( 0, clRed, psSolid, 1 ) ;

    // Set upper limit of plot and histogram event ranges to 1-1
    // to force an update of ranges (see PageChange)
    edHistRange.HiValue := 1 ;
    edPlotEventRange.HiValue := 1 ;

    // Re-enable detect button
    bAbort.Enabled := False ;
    bDetect.Enabled := True ;
    EventAnalysisFileUpdateRequired := True ;
    BaselineSplinesAvailable := False ;
    ComputationInProgress := False ;

    edAverageRange.HiLimit := NumEvents ;
    edAverageRange.LoValue := 1 ;
    edAverageRange.HiValue := NumEvents ;

    sbEvent.Position := 1 ;

    scDetDisplay.ClearLines ;

    end ;


procedure TEventDetFrm.DisplayEditRecord ;
{ -------------------------------------------------------------
  Display currently selected block of data file on edit display
  ------------------------------------------------------------- }
var
   i,iStart,iEnd,iEvent,Step,iLine : Integer ;
begin

   if EditBuf = Nil then Exit ;

   scEditDisplay.xOffset := sbEditDisplay.Position ;

   scEditDisplay.NumPoints := ReadCDRBuffer( CdrFH,
                                             sbEditDisplay.Position,
                                             EditBuf^,
                                             scEditDisplay.MaxPoints) ;

   if ckSubtractBaseline.Checked then
      begin
      SubtractBaseline( sbEvent.Position-1,
                        EditBuf^,
                        sbEditDisplay.Position,
                        scEditDisplay.MaxPoints ) ;
      end ;

   scEditDisplay.SetDataBuf( EditBuf ) ;

   // Find
   iStart :=  scEditDisplay.xOffset ;
   iEnd := iStart + scEditDisplay.MaxPoints - 1 ;

   // Find nearest event occuring within or before display

   iEvent := NumEvents div 2 ;
   Step :=  NumEvents div 2 ;
   while Step > 0 do begin
       if Events[iEvent] < iEnd then begin
          iEvent := Min(iEvent + Step,NumEvents-1) ;
          end
       else begin
          iEvent := Max(iEvent - Step,0) ;
          end ;
       Step := Step div 2 ;
       end ;
   // Work back to first event within display
   While (Events[iEvent] > iStart) and (iEvent >=0) do Dec(iEvent) ;
   iEvent := Min( iEvent+1,NumEvents-1 ) ;

   { Initialise detected event line }
   scMarkDisplay.xOffset := 0 ;
   scMarkDisplay.ClearLines ;
   iLine := scMarkDisplay.CreateLine( 0, clRed, psSolid, 1 ) ;
   scMarkDisplay.AddPointToLine( iLine, 0, scMarkDisplay.MinADCValue ) ;
   for i := 0 to scMarkDisplay.MaxPoints-1 do begin
       if ((iStart+i) >= Events[iEvent]) and (iEvent < NumEvents) then begin
          Inc(iEvent) ;
          scMarkDisplay.AddPointToLine( iLine, i, scMarkDisplay.MinADCValue ) ;
          scMarkDisplay.AddPointToLine( iLine, i, scMarkDisplay.MaxADCValue ) ;
          scMarkDisplay.AddPointToLine( iLine, i, scMarkDisplay.MinADCValue ) ;
          end ;
       end ;

   scMarkDisplay.AddPointToLine( iLine, scMarkDisplay.MaxPoints-1, scMarkDisplay.MinADCValue ) ;

   scEditDisplay.Invalidate ;
   scMarkDisplay.Invalidate ;

   end;


procedure TEventDetFrm.LoadEventList ;
{ ---------------------------------------
  Load list of detected events from file
  --------------------------------------- }
var
     FileName : string ;
     FileHandle : Integer ;
begin

     // Initialise no. of events
     NumEvents := 0 ;

     // Add event list file extension to existing data file name
     FileName := ChangeFileExt( CdrFH.FileName, EventFileExtension ) ;

     if not FileExists( FileName ) then Exit ;

     // Open an existing event list file
     FileHandle := FileOpen(FileName, fmOpenReadWrite ) ;
     if FileHandle < 0 then begin
        ShowMessage('Unable to open ' + FileName ) ;
        Exit ;
        end ;

     // Read number of events from start of file
     FileSeek(FileHandle,0,0) ;
     FileRead(FileHandle,NumEvents,SizeOf(NumEvents)) ;
     NumEvents :=  Max( Min( NumEvents,High(Events)+1 ),0) ;

     // Read List of events
     if NumEvents > 0 then begin
        if FileRead(FileHandle,Events,NumEvents*SizeOf(Integer))
           <> NumEvents*SizeOf(Integer) then
           ShowMessage('Error reading Event file') ;
        end ;

     // Close file
     if FileHandle >= 0 then FileClose( FileHandle ) ;

     edAverageRange.HiLimit := NumEvents ;
     edAverageRange.LoValue := 1 ;
     edAverageRange.HiValue := NumEvents ;

     end ;


procedure TEventDetFrm.SaveEventList ;
{ ------------------------------------
  Save list of detected events to file
  ------------------------------------ }
var
     FileName : string ;
     FileHandle : Integer ;
     OK : Boolean ;
begin

     // Add event list file extension to existing data file name
     FileName := ChangeFileExt( CdrFH.FileName, EventFileExtension ) ;

     OK := True ;

     // Open file
     if FileExists(FileName) then FileHandle := FileOpen(FileName, fmOpenReadWrite)
                             else FileHandle := FileCreate(FileName) ;
     if FileHandle < 0 then OK := False ;

     if OK then begin

        // Write number of events from start of file
        FileSeek(FileHandle,0,0) ;
        if FileWrite(FileHandle,NumEvents,SizeOf(NumEvents))
           <> SizeOf(NumEvents) then OK := False ;

        // Write List of events
        if OK and (NumEvents > 0) then
           if FileWrite(FileHandle,Events,NumEvents*SizeOf(Integer))
              <> NumEvents*SizeOf(Integer) then OK := False ;

        // Close file
        if FileHandle >= 0 then FileClose( FileHandle ) ;

        // Set export events range
        edExportRange.LoLimit := edEvent.LoLimit ;
        edExportRange.LoValue := edEvent.LoLimit ;
        edExportRange.HiLimit := edEvent.HiLimit ;
        edExportRange.HiValue := edEvent.HiLimit ;

        end ;

     if not OK then ShowMessage('Error writing Event file') ;

     SaveEventListRequested := False ;

     end ;


procedure TEventDetFrm.scDetDisplayCursorChange(Sender: TObject);
// -------------------------------------------------
// Cursor on detection criterion display has changed
// -------------------------------------------------
begin
     edThreshold.Value := scDetDisplay.HorizontalCursors[ThresholdCursor] ;
     Settings.EventDetector.yThreshold := edThreshold.Value ;

     // Keep zero cursot at zero
     if scDetDisplay.HorizontalCursors[DetZeroCursor] <> 0 then
        scDetDisplay.HorizontalCursors[DetZeroCursor] := 0 ;

     // Align detection display cursor
     if scDetDisplay.VerticalCursors[DisplayCursor]
        <> scDisplay.VerticalCursors[DisplayCursor] then
        scDisplay.VerticalCursors[DisplayCursor] :=
          scDetDisplay.VerticalCursors[DisplayCursor] ;

     end;


procedure TEventDetFrm.bAbortClick(Sender: TObject);
// -----------------------------------------
// Request that detection process be aborted
// -----------------------------------------
begin
     bAbort.Enabled := False ;
     bDetect.Enabled := True ;
     AbortFlag := True ;
     end;


procedure TEventDetFrm.sbEditDisplayChange(Sender: TObject);
begin
    DisplayEditRecord ;
    end;


procedure TEventDetFrm.PageChange(Sender: TObject);
// ------------------------
// Display page has changed
// ------------------------
var
    i,iKeep : Integer ;
begin

     if Page.ActivePage = DetectEventsPage then begin
        // Update Detect events page
        DisplayRecord ;
        end
     else if Page.ActivePage = EditEventsPage then begin

        // Update controls on Edit Events page
        UpdateEditEventPage ;

        end
     else if Page.ActivePage = XYPlotPage then begin
        // Update controls on X/Y plot page
        edPlotEventRange.HiLimit := NumEvents ;
        if edPlotEventRange.HiValue = 1 then edPlotEventRange.HiValue := edPlotEventRange.HiLimit ;
        edPlotEventRange.LoValue := Min( edPlotEventRange.LoValue, NumEvents ) ;
        edPlotEventRange.HiValue := Min( edPlotEventRange.HiValue, NumEvents ) ;
        edPlotTimeRange.HiLimit := CDRFH.NumSamplesInFile*CDRFH.dt ;
        edPlotTimeRange.LoValue := 0.0 ;
        edPlotTimeRange.HiValue := edPlotTimeRange.HiLimit ;
        bSetPlotAxes.Enabled := PlotAvailable ;

        iKeep := cbPlotXVar.ItemIndex ;
        i := cbPlotYVar.Items.IndexOfObject(TObject(vTDecay)) ;
        cbPlotXVar.Items.Strings[i] := format(
                                       'T.%d%%',
                                       [Round(edTDecayPercentage.Value)]) ;
        cbPlotXVar.ItemIndex := iKeep ;

        iKeep := cbPlotYVar.ItemIndex ;
        i := cbPlotYVar.Items.IndexOfObject(TObject(vTDecay)) ;
        cbPlotYVar.Items.Strings[i] := format(
                                       'T.%d%%',
                                       [Round(edTDecayPercentage.Value)]) ;
        cbPlotYVar.ItemIndex := iKeep ;

        if NumEvents > 0 then bNewPlot.Enabled := True
                         else bNewPlot.Enabled := False ;

        end
     else if Page.ActivePage = HistPage then begin
        // Update controls on histogram page
        edHistRange.HiLimit := NumEvents ;
        if edHistRange.HiValue = 1 then edHistRange.HiValue := edHistRange.HiLimit ;
        edHistRange.LoValue := Min( edHistRange.LoValue, NumEvents ) ;
        edHistRange.HiValue := Min( edHistRange.HiValue, NumEvents ) ;
        bSetHistAxes.Enabled := HistAvailable ;
        i := cbPlotXVar.Items.IndexOfObject(TObject(vTDecay)) ;
        cbHistVar.Items.Strings[i] := format(
                                       'T.%d%%',
                                       [Round(edTDecayPercentage.Value)]) ;

        if NumEvents > 0 then begin
           bNewHistogram.Enabled := True ;
           end
        else begin
           bNewHistogram.Enabled := False ;
           end

        end
     else if Page.ActivePage = AveragePage then begin
        // Update controls on average page
        if NumEvents > 0 then bDoAverage.Enabled := True
                         else bDoAverage.Enabled := False ;
        if NumEventsAveraged > 0 then AnalyseAverage ;
        end ;
     end;


procedure TEventDetFrm.UpdateEditEventPage ;
// -----------------------------------------
// Update edit events page with new settings
// -----------------------------------------
begin

    sbEvent.Max := Max(NumEvents,1) ;
    if NumEvents > 0 then begin
       sbEvent.Enabled := True ;
       bExportToWCPFile.Enabled := True ;
       end
    else begin
       sbEvent.Enabled := False ;
       bExportToWCPFile.Enabled := False ;
       end ;
    edEvent.LoLimit := 1 ;
    edEvent.HiLimit := NumEvents ;
    edEvent.LoValue := sbEvent.Position ;
    edEvent.HiValue := edEvent.HiLimit ;

    // Set export events range
    edExportRange.LoLimit := edEvent.LoLimit ;
    edExportRange.LoValue := edEvent.LoLimit ;
    edExportRange.HiLimit := edEvent.HiLimit ;
    edExportRange.HiValue := edEvent.HiLimit ;

    bExportToWCPFile.Enabled := True ;
    bAbortExport.Enabled := False ;
    bExportNonEvents.Enabled := True ;

    // Display selected event
    if NumEvents > 0 then DisplayEvent
    else begin
       sbEditDisplay.Position := 0 ;
       DisplayEditRecord ;
       end ;
    end ;


procedure TEventDetFrm.sbEventChange(Sender: TObject);
// ---------------------------
// Event selection bar changed
// ---------------------------
begin
     DisplayEvent ;
     end ;


procedure TEventDetFrm.DisplayEvent;
// ---------------------------------
// Display currently  selected event
// ---------------------------------
const
     nAvg = 20 ;
var
     Event : TEventAnalysis ; // Event waveform analysis record
begin

     // Move edit display to selected event
     sbEditDisplay.Position := Max( Events[sbEvent.Position-1]
                                    - Round(scEditDisplay.MaxPoints*edPreTrigger.Value), 0) ;

     // Display event
     DisplayEditRecord ;

     // Calculate event waveform measurements
     AnalyseEvent( sbEvent.Position-1, Event ) ;

     scEditDisplay.HorizontalCursors[BaseLineCursor] := Event.YBaseline ;
     scEditDisplay.ChanZero[cbChannel.ItemIndex] := Event.YBaseline ;

     meResults.Clear ;
     meResults.Lines.Add( format( 'Detected at %.7g s',
                                [Events[sbEvent.Position-1]*CdrFH.dt] )) ;

     meResults.Lines.Add( format( 'Peak (a-a)= %.5g %s',
                                  [Event.Peak,
                                   Channel[cbChannel.ItemIndex].ADCUnits]));

     meResults.Lines.Add( format( 'Area (a-a)= %.5g %s.ms',
                                  [Event.Area,
                                   Channel[cbChannel.ItemIndex].ADCUnits]));

     meResults.Lines.Add( format( 'T(rise)= %.5g ms',[Event.TRise]));

     meResults.Lines.Add( format( 'T(%.0f%%)= %.5g ms',
                          [edTDecayPercentage.Value,Event.TDecay]));

     meResults.Lines.Add( format( 'Tau(decay)= %.5g ms',[Event.TauDecay]));

     meResults.Lines.Add( format( 'Duration= %.5g ms',[Event.Duration]));

     meResults.Lines.Add( format( 'Baseline= %.5g %s',
                          [Channel[cbChannel.ItemIndex].ADCScale*(Event.YBaseline - Channel[cbChannel.ItemIndex].ADCZero),
                           Channel[cbChannel.ItemIndex].ADCUnits]));


     scEditDisplay.HorizontalCursors[0] := Event.YBaseline ;

     if cktCursorAtDetectionPoint.Checked then begin
        scEditDisplay.VerticalCursors[EditCursor] := Events[sbEvent.Position-1]
                                                     - scEditDisplay.xOffset ;
        end
     else begin
        // Place cursor over event
        if (scEditDisplay.VerticalCursors[EditCursor] < 0) or
           (scEditDisplay.VerticalCursors[EditCursor] > scEditDisplay.NumPoints) then
           scEditDisplay.VerticalCursors[EditCursor] := scEditDisplay.NumPoints div 2 ;
           end ;

     // Update event number display
     edEvent.LoValue := sbEvent.Position ;

     // Store analysis area cursor positions
     OldEditC0CursorPos := scEditDisplay.VerticalCursors[EditC0Cursor] ;
     OldEditC1CursorPos := scEditDisplay.VerticalCursors[EditC1Cursor] ;

     end;


procedure TEventDetFrm.AnalyseEvent(
          iEvent : Integer ;          // Event # to be analysed [IN]
          var Event : TEventAnalysis   // Analysis results record [OUT]
          ) ;
// -----------------------------------
// Calculate event waveform parameters
// -----------------------------------
var
     PreScans : Integer ;   // No. of scans before event detection point
     PostScans : Integer ;  // No. of scans after event detection point
     StartScan : Integer ;  // Start of analysis block
     EndScan : Integer ;    // End of analysis block
     NumScans : Integer ;   // No. of scans in analysis block
     EventScan : Integer ;  // Scan (within Buf) where event occurs
     AnalysisStart : Integer ; // First time point range to be analysed
     AnalysisEnd : Integer ;   // Last time point in range to be analysed
     i,j : Integer ;        // Counters

     Polarity : Integer ;   // Event polarity (1=positive-going,-1=negative-)
     Sum : Single ;         // Summation variable
     NumAvg : Integer ;     // No. samples averaged for baseline
     Y : Single ;           // A/D sample value
     YMax : Single ;        // Maximum value within block
     PeakAt : Integer ;     // Scan at which peak occurs
     MidPointOfRiseAt : Integer ; // Scan at mid-point of event rise
     YDecay : Single ;      // x% of peak
     Y90 : Single ;         // 90% of peak
     Y50 : Single ;         // 50% of peak
     Y10 : Single ;         // 10% of peak
     Num10to90PercentPeak : Integer ;    // No. of scans within event rising phase
     Num10toPeak : Integer ;             // No. rising phase scans from 10% to peak
     T : Single ;          //
     SumT : Single ;        // Summation variables for time constant fit
     SumT2 : Single ;         //
     SumY : Single ;         //
     SumYT : Single ;         //
     nPoints : Integer ;
     Slope : Single ;

     Buf : ^TSmallIntArray ;
     Done : Boolean ;
begin

   // Clear event results (in case of premature exit)
   Event.YBaseline := 0 ;
   Event.Rate := 0.0 ;
   Event.Peak := 0.0 ;
   Event.Area := 0.0 ;
   Event.TRise := 0.0 ;
   Event.TDecay := 0.0 ;
   Event.TauDecay := 0.0 ;
   Event.Duration := 0.0 ;

   // Get first and last scans of analysis window
   NumScans := Round(edEditDisplayWidth.Value) ;
   PreScans := Round(NumScans*edPreTrigger.Value) ;
   Settings.EventDetector.PreTriggerFraction := edPreTrigger.Value ;
   PostScans := NumScans - PreScans ;

   // First multi-channel scan in analysis block
   StartScan := Max( Events[iEvent] - PreScans, 0) ;
   if iEvent > 0 then StartScan := Max( Events[iEvent-1]+1,StartScan) ;
   EndScan := Min( Events[iEvent] + PostScans,
                   CDRFH.NumSamplesInFile div CDRFH.NumChannels ) ;

   NumScans := EndScan - StartScan + 1 ;
   EventScan := Events[iEvent] - StartScan ;

   // Exit if invalid area
   if NumScans < 1 then Exit ;

   // Set positive/negative polarity of event to be measured
   if rbPositive.Checked then Polarity := 1
                         else Polarity := -1 ;

   // Allocate buffer
   GetMem( Buf, NumScans*CDRFH.NumChannels*2 ) ;

   // Read A/D sample data from file
   ReadCDRBuffer(CdrFH,StartScan,Buf^,NumScans) ;

   // Subtract baseline trend
   if ckSubtractBaseline.Checked then begin
      SubtractBaseline( iEvent, Buf^, StartScan, NumScans ) ;
      end ;

   // Find baseline level
   Event.YBaseline := PreEventBaselineLevel( Buf^, EventScan, NumScans ) ;

   // Get range of points to be analysed from cursors
   AnalysisStart := Min( Round(scEditDisplay.VerticalCursors[EditC0Cursor]),
                         Round(scEditDisplay.VerticalCursors[EditC1Cursor] )) ;
   AnalysisEnd := Max( Round(scEditDisplay.VerticalCursors[EditC0Cursor]),
                       Round(scEditDisplay.VerticalCursors[EditC1Cursor]) ) ;

   // Prevent range exceeding data buffer
   AnalysisStart := Min( Max( AnalysisStart,0), NumScans-2) ;
   AnalysisEnd := Min( Max(AnalysisEnd,AnalysisStart+1),NumScans-1 ) ;

   // Find peak amplitude and area under curve
   YMax := -1E30 ;
   SumY := 0.0 ;
   PeakAt := AnalysisStart ;
   for i := AnalysisStart to AnalysisEnd do begin
       j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
       Y := Polarity*(Buf[j] - Event.YBaseline) ;
       if Y >= YMax then begin
          YMax := Y ;
          PeakAt := i ;
          end ;
       SumY := SumY + Y ;
       end ;
   Event.Peak := Polarity*YMax*Channel[cbChannel.ItemIndex].ADCScale ;
   Event.Area := Polarity*SumY*Channel[cbChannel.ItemIndex].ADCScale*CDRFH.dt*SecsToMs ;

   // Find rise time
   i := PeakAt ;
   MidPointOfRiseAt := PeakAt ;
   Y10 := YMax / 10.0 ;
   Y50 := YMax / 2.0 ;
   Y90 := YMax - Y10 ;
   Num10to90PercentPeak := 0 ;
   Num10toPeak := 0 ;
   Done := False ;
   while not Done do begin
       j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
       Y := Polarity*(Buf[j] - Event.YBaseline) ;
       if Y <= Y90 then Inc(Num10to90PercentPeak) ;
       if Y >= Y50 then MidPointOfRiseAt := i ;
       Inc(Num10toPeak) ;
       Dec(i) ;
       if (Y < Y10) or (i<=0) then Done := True ;
       end ;
   Event.TRise := Num10to90PercentPeak*CDRFH.dt*SecsToMs ;

   // Find time from peak to X% of peak decay
   i := PeakAt ;
   YDecay := YMax*(1.0 - (0.01*edTDecayPercentage.Value)) ;
   Done := False ;
   while not Done do begin
       j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
       Y := Polarity*(Buf[j] - Event.YBaseline) ;
       Inc(i) ;
       if (Y < YDecay) or (i >= NumScans) then Done := True ;
       end ;

   if cbDecayFrom.ItemIndex = cTDecayFromPeak then begin
      // Decay time from peak
      Event.TDecay := (i-PeakAt)*CDRFH.dt*SecsToMs ;
      end
   else if cbDecayFrom.ItemIndex = cTDecayFromMidRise then begin
      // Decay time from mid-point of rising phase
      Event.TDecay := (i-MidPointOfRiseAt)*CDRFH.dt*SecsToMs ;
      end
   else begin
      // Decay time from first analysis cursor
      Event.TDecay := (i-AnalysisStart)*CDRFH.dt*SecsToMs ;
      end ;

   // Event duration (10% of rising phase to 90% decay after peak)
   i := PeakAt ;
   YDecay := YMax*0.1 ;
   Done := False ;
   while not Done do begin
       j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
       Y := Polarity*(Buf[j] - Event.YBaseline) ;
       Inc(i) ;
       if (Y < YDecay) or (i >= NumScans) then Done := True ;
       end ;
   Event.Duration :=  ((i-PeakAt) + Num10toPeak)*CDRFH.dt*SecsToMs ;

   // Calculate decay time constant
   i := PeakAt ;
   T := 0.0 ;
   SumT := 0.0 ;
   SumT2 := 0.0 ;
   SumY := 0.0 ;
   SumYT := 0.0 ;
   nPoints := 0 ;
   Done := False ;
   while not Done do begin
      j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
      Y := Polarity*(Buf[j] - Event.YBaseline) ;
      if Y > Y10 then begin
         Y := Ln(Y) ;
         SumT := SumT + T ;
         SumT2 := SumT2 + T*T ;
         SumY := SumY + Y ;
         SumYT := SumYT + Y*T ;
         Inc(nPoints) ;
         end
      else Done := True ;
      Inc(i) ;
      T := T + CDRFH.dt ;
      if i >= AnalysisEnd then Done := True ;
      end ;
   if nPoints > 1 then begin
      Slope := ((nPoints*SumYT) - (SumT*SumY)) /
               ((nPoints*SumT2) - (SumT*SumT)) ;
      if Slope < 0.0 then Event.TauDecay := (-1.0/Slope)*SecsToMs
                     else Event.TauDecay := 0.0 ;
      end
   else Event.TauDecay := 0.0 ;

   FreeMem(Buf) ;

   end ;

function TEventDetFrm.PreEventBaselineLevel(
         var Buf : Array of SmallInt ; // Data buffer
         EventScan : Integer ;         // Scan # of start of event in Buf
         NumScans : Integer            // No. of scans in Buf
         ) : Integer ;
// ----------------------------------
// Calculate pre-event baseline level
// ----------------------------------
var
    i,j : Integer ;
    Polarity : Integer ;
    Sum : Single ;
    Y : Single ;
    NumAvg : Integer ;
    iStart,iEnd : Integer ;
    NumAvgRequired : Integer ;
    iZeroGap : Integer ;
begin

   Result := 0 ;
   if NumScans < 1 then Exit ;

   NumAvgRequired := Round( edZeroNumAvg.Value) ;
   iZeroGap := Round( edZeroGap.Value) ;

   if cbBaseline.ItemIndex = BaselineAtEvent then
      begin
      // Baseline at start of event
      // --------------------------
      // Set positive/negative polarity of event to be measured
      if rbPositive.Checked then Polarity := 1
                            else Polarity := -1 ;

      // Find start of rising phase of event
      i := EventScan ;
      Repeat
          j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
          Y := Polarity*(Buf[j] - Buf[j-CDRFH.NumChannels]);
          iEnd := i ;
          Dec(i) ;
          Until (i = 0) or (Y <= 0.0) ;

      iEnd := Max(iEnd - iZeroGap,0) ;
      iStart := Max(iEnd - NumAvgRequired + 1,0) ;

      end
   else
      begin
      // Baseline at start of event window or fixed
      // ------------------------------------------
      iStart := iZeroGap ;
      iEnd := NumAvgRequired-1 ;
      end ;

   // Calculate baseline average
   if cbBaseline.ItemIndex <> FixedBaseline then
      begin
      Sum := 0.0 ;
      NumAvg := 0 ;
      for i := iStart to iEnd do
          begin
          j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
          Sum := Sum + Buf[j] ;
          Inc(NumAvg) ;
          end ;

      // Determine pre-event baseline level
      if NumAvg > 0 then Result := Round( Sum / NumAvg )
                    else Result := Buf[Channel[cbChannel.ItemIndex].ChannelOffset] ;
      end
   else begin
     // Fixed baseline, derived from current user-set baseline cursor.
     Result := Channel[cbChannel.ItemIndex].ADCZero ;
     end ;

   end ;


procedure TEventDetFrm.SubtractBaseline(
          iEvent : Integer  ;       // Event #
          var Buf : Array of SmallInt ; // Sample buffer
          BufStartScan : Integer ;
          NumScans : Integer
          ) ;
// --------------------------------------
// Subtract pre-event baseline from event
// --------------------------------------
const
     NumAvgRequired = 10 ;

var
     iScan,klo,khi,j,i : Integer ;
     A,B,H,Y : Single ;
begin



    // Calculate cublic splines for baseline interpolation
    if not BaselineSplinesAvailable then begin
       CalculateBaselineSplines ;
       end ;

    for i := 0 to NumScans-1 do begin

        iScan :=  BufStartScan + i ;

        if iScan < Events[iEvent] then begin
           KLo := Max(iEvent-1,0) ;
           KHi := iEvent ;
           end
        else begin
           KLo := iEvent ;
           KHi := Min(iEvent+1,NumEvents-1) ;
           end ;

        H := Events[KHi] - Events[KLo] ;

        if H <> 0.0 then begin
           A := (Events[KHi] - iScan)/H ;
           B := (iScan - Events[KLo])/H ;
           Y := A*YBaseline[KLo] + B*YBaseline[KHi] +
                ((Power(A,3)-A)*YBaseline2[KLo] +
                (Power(B,3)-B)*YBaseline2[KHi])*(H*H)/6.0 ;
           end
        else begin
           Y := YBaseline[KHi] ;
           end ;

        j := i*CDRFH.NumChannels  + cbChannel.ItemIndex ;
        Buf[j] := Buf[j] - Round(Y) + YBaseline[0] ;

        end ;

    end ;

procedure TEventDetFrm.CalculateBaselineSplines ;
// -------------------------------------------------
// Calculate cublic splines for baseline subtraction
// -------------------------------------------------
const
     NumAvgRequired = 10 ;

var
     PreScans : Integer ;   // No. of scans before event detection point
     PostScans : Integer ;  // No. of scans after event detection point
     StartScan : Integer ;  // Start of analysis block
     EndScan : Integer ;    // End of analysis block
     NScans : Integer ;   // No. of scans in analysis block
     i : Integer ;
     Buf : ^TSmallIntArray ;
     U : PSingleArray ;
     Sig : Single ;
     P,qn,un : Single ;
begin

    GetMem( U, Max(NumEvents,1)*4 ) ;

    Screen.Cursor := crHourGlass ;

    // Get first and last scans of analysis window
    NScans := Round(edEditDisplayWidth.Value) ;
    PreScans := Round(NScans*edPreTrigger.Value) ;
    PostScans := NScans - PreScans ;

   // Allocate buffer
   GetMem( Buf, NScans*CDRFH.NumChannels*2 ) ;

    // Calculate pre-event baselines

    for i := 0 to NumEvents-1 do begin

        // Find limits of event
        StartScan := Max( Events[i] - PreScans,0) ;
        if i > 0 then StartScan := Max( Events[i-1]+1,StartScan) ;
        EndScan := Min( Events[i] + PostScans -1,
                        CDRFH.NumSamplesInFile div CDRFH.NumChannels ) ;
        if i < (NumEvents-1) then EndScan := Min( Events[i+1]-1,EndScan) ;
        NScans := Min(EndScan - StartScan + 1, NScans) ;

        // Read A/D sample data from file
        ReadCDRBuffer(CdrFH,StartScan,Buf^,NScans) ;

        // Pre-event baseline
        YBaseline[i] := PreEventBaselineLevel( Buf^,
                                               Events[i] - StartScan,
                                               NScans ) ;

        end ;

    // Calculate cubic spline

    YBaseline2[0] := 0.0 ;
    U^[0] := 0.0 ;
    for i := 1 to NumEvents-2 do begin
        sig := (Events[i] - Events[i-1])/(Events[i+1] - Events[i-1]) ;
        P := sig*YBaseline2[i-1] + 2.0 ;
        YBaseline2[i] := (sig - 1.0) / P ;
        U^[i] := ( 6.0*( (YBaseline[i+1] - YBaseline[i])/(Events[i+1] - Events[i])
                      - (YBaseline[i] - YBaseline[i-1])/(Events[i] - Events[i-1]))
                      / (Events[i+1] - Events[i-1]) - sig*U^[i-1])/P ;
        end ;

    qn := 0.0 ;
    un := 0.0 ;
    YBaseline2[NumEvents-1] := (un - qn*U^[NumEvents-2]) /
                               (qn*YBaseline2[NumEvents-2] + 1.0 ) ;
    for i := NumEvents-2 downto 0 do begin
        YBaseline2[i] := YBaseline2[i]*YBaseline2[i+1] + U^[i] ;
        end ;

    BaselineSplinesAvailable := True ;
    Screen.Cursor := crDefault ;
    FreeMem(Buf) ;
    FreeMem(U) ;

    end ;


procedure TEventDetFrm.edEventKeyPress(Sender: TObject; var Key: Char);
// -------------------------------
// Key pressed in edited event box
// -------------------------------
begin

     if key = #13 then begin
        sbEvent.Position := Round(edEvent.LoValue) ;
        edEvent.HiValue := NumEvents ;
        end ;

     end;


procedure TEventDetFrm.bInsertEventClick(Sender: TObject);
// ------------------------------------------
// Insert an event at current cursor position
// ------------------------------------------
var
   i,iEvent,NewEventAt : Integer ;
   AddEvent : Boolean ;
begin

   // Sample position of new event
   NewEventAt := Round(scEditDisplay.VerticalCursors[EditCursor]
                 + scEditDisplay.xOffset) ;

   // Find next higher event in list
   iEvent := 0 ;
   While (Events[iEvent] <= NewEventAt) and
         (iEvent < NumEvents) do Inc(iEvent) ;

   // Make space for new event in list (only if it doesn't exist already)
   if ((NumEvents = 0) or (Events[iEvent] <> NewEventAt)) and
      (NumEvents <= High(Events)) then begin
      AddEvent := True ;
      Inc(NumEvents) ;
      for i := NumEvents-1 downto iEvent+1 do Events[i] := Events[i-1] ;
      end
   else AddEvent := False ;

   if AddEvent then begin

      Events[iEvent] := NewEventAt ;

      sbEvent.Max := NumEvents ;
      edEvent.HiLimit := NumEvents ;
      edEvent.HiValue := edEvent.HiLimit ;
      edEvent.LoValue := iEvent+1 ;
      sbEvent.Position := iEvent+1 ;

      // Move edit display to selected event
      sbEditDisplay.Position := Max( Events[sbEvent.Position-1]
                                     - scEditDisplay.MaxPoints div 5, 0) ;

      // Enable/disable controls
      if NumEvents > 0 then begin
         sbEvent.Enabled := True ;
         bExportToWCPFile.Enabled := True ;
         end
      else begin
         sbEvent.Enabled := False ;
         bExportToWCPFile.Enabled := False ;
         end ;

      DisplayEvent ;
      end ;

   // Request update of event list file
   SaveEventListRequested := True ;

   EventAnalysisFileUpdateRequired := True ;
   BaselineSplinesAvailable := False ;
   
   end;


procedure TEventDetFrm.bDeleteEventClick(Sender: TObject);
// -----------------------------------------
// Delete currently selected event from list
// -----------------------------------------
var
   iEvent,i : Integer ;
begin

   // Delete current event from list
   iEvent := sbEvent.Position - 1 ;
   for i := iEvent to NumEvents-1 do Events[i] := Events[i+1] ;
   Dec(NumEvents) ;

   sbEvent.Max := Max(NumEvents,1) ;
   edEvent.HiLimit := NumEvents ;
   edEvent.HiValue := edEvent.HiLimit ;
   edEvent.LoValue := iEvent+1 ;
   sbEvent.Position := iEvent+1 ;

   // Move edit display to selected event
   sbEditDisplay.Position := Max( Events[sbEvent.Position-1]
                                  - scEditDisplay.MaxPoints div 5, 0) ;

   // Enable/disable controls
   if NumEvents > 0 then begin
      sbEvent.Enabled := True ;
      bExportToWCPFile.Enabled := True ;
      end
   else begin
      sbEvent.Enabled := False ;
      bExportToWCPFile.Enabled := False ;
      end ;

   // Request update of event list file
   SaveEventListRequested := True ;

   DisplayEvent ;
   EventAnalysisFileUpdateRequired := True ;
   BaselineSplinesAvailable := False ;
   end ;


procedure TEventDetFrm.scEditDisplayCursorChange(Sender: TObject);
// ----------------------------------
// Edit display window cursor changed
// ----------------------------------
var
     ch : Integer ;
     CursorAtSample : Integer ;
begin

      // Sample position of new event
      CursorAtSample := Round(scEditDisplay.VerticalCursors[EditCursor]
                        + scEditDisplay.xOffset) ;

      // Enable/disable Insert Event and Delete Event buttons
      // depending upon whether cursor is over an event
      if CursorAtSample = Events[sbEvent.Position-1] then begin
         bDeleteEvent.Enabled := True ;
         bInsertEvent.Enabled := False ;
         end
      else begin
         bDeleteEvent.Enabled := True ;
         bInsertEvent.Enabled := True ;
         end ;

     for ch := 0 to scEditDisplay.NumChannels-1 do
         if scEditDisplay.ChanVisible[ch] then begin
         { Get signal baseline cursor }
         Channel[ch].yMin := scEditDisplay.yMin[ch] ;
         Channel[ch].yMax := scEditDisplay.yMax[ch] ;
         end ;

     end;


procedure TEventDetFrm.bNewPlotClick(Sender: TObject);
// ------------------------------------------
// Plot selected event variables on X/Y graph
// ------------------------------------------
var
   iEvent, StartAtEvent, EndAtEvent : Integer ;
   StartAtSample, EndAtSample : Integer ;
   StartofInterval, EndOfInterval : Integer ;
   XVarType, YVarType : Integer ;
   IntervalWidth : Integer ;
   xMin,xMax, x,y : Single ;
   OK : Boolean ;
   Done : Boolean ;
begin

   bPlotAbort.Enabled := True ;
   bNewPlot.Enabled := False ;
   ComputationInProgress := True ;
   AbortFlag := False ;

   Main.StatusBar.SimpleText :=
   'Event Detector : WAIT ... Plotting X/Y Graph' ;


   if rbPlotAllEvents.Checked then begin
      { Use all Events }
      StartAtEvent := 0 ;
      EndAtEvent := NumEvents - 1 ;
      StartAtSample := 0 ;
      EndAtSample := CDRFH.NumSamplesInFile-1 ;
      end
   else begin
      { Use selected range of records }
       StartAtEvent := Round(edPlotEventRange.LoValue)-1 ;
       EndAtEvent := Round(edPlotEventRange.HiValue)-1 ;
       StartAtSample := Round(edPlotTimeRange.LoValue/CDRFH.dt) ;
       EndAtSample := Round(edPlotTimeRange.HiValue/CDRFH.dt) ;
       end ;

   // X axis variable
   XVarType := Integer(cbPlotXVar.Items.Objects[cbPlotXVar.ItemIndex]) ;
   xUnits := GetVariableUnits( XVarType ) ;

   // Y axis variable
   YVarType := Integer(cbPlotYVar.Items.Objects[cbPlotYVar.ItemIndex]) ;
   yUnits := GetVariableUnits( YVarType ) ;

   { Plot graph of currently selected variables }
   plPlot.xAxisAutoRange := True ;
   plPlot.yAxisAutoRange := True ;
   plPlot.xAxisLabel := cbPlotXVar.text ;
   if xUnits <> '' then plPlot.xAxisLabel := plPlot.xAxisLabel + '(' + xUnits + ')' ;
   plPlot.yAxisLabel := cbPlotYVar.text ;
   if yUnits <> '' then plPlot.yAxisLabel := plPlot.yAxisLabel + '(' + yUnits + ')' ;

   { Clear data points line }
   plPlot.ClearAllLines ;
   plPlot.MaxPointsPerLine := NumEvents*2 ;
   plPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     { Clear any fitted line }
   //  plVarPlot.CreateLine( VarFitLine , clRed, msNone, psSolid ) ;

   xMin := 1E30 ;
   xMax := -xMin ;
   iEvent := StartAtEvent ;
   Settings.EventDetector.AvgFrequencyInterval := edAverageInterval.Value ;
   IntervalWidth := Max(Round(edAverageInterval.Value/CdrFH.dt),1) ;
   StartOfInterval := StartAtSample ;
   EndOfInterval := StartOfInterval + IntervalWidth ;
   AbortFlag := False ;
   Done := False ;
   While (Not Done) and (not AbortFlag) do begin

         if (XVarType = vFrequencyAvg) or (YVarType = vFrequencyAvg) then begin
            // Calculate average event frequency
            OK :=CalculateVariablesInInterval( StartOfInterval,
                                               EndOfInterval,
                                               xVarType,
                                               yVarType,
                                               iEvent,
                                               X,
                                               Y ) ;
            StartOfInterval := StartOfInterval + IntervalWidth ;
            EndOfInterval := EndOfInterval + IntervalWidth ;
            if EndOfInterval > EndAtSample then Done := True ;
            end
         else begin
            // Calculate variables and instantaneous frequency
            OK := CalculateVariables( iEvent,
                                      xVarType,
                                      yVarType,
                                      X,
                                      Y ) ;
            if iEvent > EndAtEvent then Done := True ;
            end ;

         // Add point to X/Y graph
         if OK then begin
            plPlot.AddPoint( 0, X, Y ) ;
            if xMin > X then xMin := X ;
            if xMax < X then xMax := X ;
            end ;

         end ;

     //plPlot.SortByX( 0 ) ;

   plPlot.XAxisMin := xMin ;
   plPlot.XAxisMax := xMax ;
   if plPlot.XAxisMin = plPlot.XAxisMax then plPlot.XAxisMax := plPlot.XAxisMin + 1.0 ;
//   plPlot.XAxisTick := (xMax - xMin)*0.1 ;

   { Create and set Histogram cursors to initial positions }
   plPlot.ClearVerticalCursors ;
   plPlot.AddVerticalCursor( clGReen, '?r',0 ) ;
   plPlot.VerticalCursors[0] := (xMin + xMax)*0.5 ;

   PlotAvailable := True ;
   bSetPlotAxes.Enabled := True ;
   bPlotAbort.Enabled := False ;
   bNewPlot.Enabled := True ;

   Main.StatusBar.SimpleText := '' ;
   ComputationInProgress := False ;

   end;


procedure TEventDetFrm.UpdateEventAnalysisFile ;
// -------------------------------------------
// Update event waveform analysis results file
// -------------------------------------------
var
     iEvent : Integer ;      // Event No.
     Event : TEventAnalysis ; // Event waveform measurements
begin

     Screen.Cursor := crHourGlass ;

     // Position file pointer at start of file
     FileSeek( EventAnalysisFile, 0, 0 ) ;

     // Write waveform measurement record
     AbortFlag := False ;
     for iEvent := 0 to NumEvents-1 do begin
         // Analyse event waveform
         AnalyseEvent( iEvent, Event ) ;
         // Write to file
         FileWrite( EventAnalysisFile, Event, SizeOf(Event)) ;
         // Report progress
         Main.StatusBar.SimpleText := format(
         ' Event Detector : Calculating waveform measurements record %d/%d',
         [iEvent+1,NumEvents]) ;

         if (iEvent mod 20) = 0 then Application.ProcessMessages ;
         if AbortFlag then Break ;

         end ;

     if not AbortFlag then EventAnalysisFileUpdateRequired := False ;
     Screen.Cursor := crDefault ;
     Main.StatusBar.SimpleText := ' ' ;

     end ;


function TEventDetFrm.CalculateVariables(
          var iEvent : Integer ;                // Event number to be analysed [In]
          XVarType : Integer ;              // Variable selected for X axis [IN]
          YVarType : Integer ;              // Variable selected for Y axis [IN]
          var X : Single ;                  // X axis variable value
          var Y : Single                    // Y axis variable value
          ) : Boolean ;                     // TRUE = valid variable returned
// -----------------------------------------
// Calculate selected X and Y plot variables
// -----------------------------------------
var
     Time, Interval, EventNum : Single ;
     Event : TEventAnalysis ;  // Event waveform measurements record
begin

     // Update event waveform analysis file if required
     if EventAnalysisFileUpdateRequired then UpdateEventAnalysisFile ;

     // Read event results from file
     FileSeek( EventAnalysisFile, iEvent*SizeOf(Event), 0) ;
     FileRead( EventAnalysisFile, Event, SizeOf(Event)) ;

     // Calculate inter-event intervals/instantaneous frequency

     EventNum := iEvent + 1 ;

     // Time of detection
     Time := Events[iEvent]*CdrFH.dt  ;

     // Time interval since previous event
     if iEvent > 0 then Interval := (Events[iEvent] - Events[iEvent-1])*CdrFH.dt
                   else Interval := -1.0 ;

     Inc(iEvent) ;

     // X axis variable
     Result := True ;
     Case XVarType of
          vBaseline : X := Channel[cbChannel.ItemIndex].ADCScale*
                          (Event.YBaseline - Channel[cbChannel.ItemIndex].ADCZero) ;
          vEventNum :  X := EventNum ;
          vTime :  X := Time ;
          vPeak : X := Event.Peak ;
          vArea : X := Event.Area ;
          vTRise : X := Event.TRise ;
          vTDecay : X := Event.TDecay ;
          vTauDecay : X := Event.TauDecay ;
          vDuration : X := Event.Duration ;
          vInterval : begin
                      X := Interval ;
                      if X < 0.0 then Result := False ;
                      end ;
          vFrequencyInst : begin
                      if Interval > 0.0 then X := 1.0/Interval
                                        else Result := False ;
                      end ;
          end ;

     // Y axis variable
     Case YVarType of
          vBaseline : Y := Channel[cbChannel.ItemIndex].ADCScale*
                          (Event.YBaseline - Channel[cbChannel.ItemIndex].ADCZero) ;
          vEventNum :  Y := EventNum ;
          vTime :  Y := Time ;
          vPeak : Y := Event.Peak ;
          vArea : Y := Event.Area ;
          vTRise : Y := Event.TRise ;
          vTDecay : Y := Event.TDecay ;
          vTauDecay : Y := Event.TauDecay ;
          vDuration : Y := Event.Duration ;
          vInterval : begin
                Y := Interval ;
                if Y < 0.0 then Result := False ;
                end ;
          vFrequencyInst : begin
                if Interval > 0.0 then Y := 1.0/Interval
                                  else Result := False ;
                end ;
          end ;

     end ;


function TEventDetFrm.CalculateVariablesInInterval(
          StartOfInterval : Integer ;       // Sample at which interval ends [IN]
          EndOfInterval : Integer ;         // Sample at which interval ends [IN]
          XVarType : Integer ;              // Variable selected for X axis [IN]
          YVarType : Integer ;              // Variable selected for Y axis [IN]
          var iEvent : Integer ;            // Event number [OUT]
          var X : Single ;                  // X axis variable value
          var Y : Single                    // Y axis variable value
          ) : Boolean ;                     // TRUE = valid variable returned
// -----------------------------------------
// Calculate selected X and Y plot variables
// -----------------------------------------
var
     Time, Interval, TInterval, EventNum : Single ;
     Event : TEventAnalysis ;  // Event waveform measurements record
     NumEventsInInterval : Integer  ;
begin

     Result := True ;

     // Update event waveform analysis file if required
     if EventAnalysisFileUpdateRequired then UpdateEventAnalysisFile ;

     // Clear measuresments to avoid spurious values being set back if
     // no events in interval

     Event.YBaseline := 0 ;
     EventNum := iEvent + 1 ;
     Event.Peak := 0 ;
     Event.Area := 0 ;
     Event.TRise := 0 ;
     Event.TDecay := 0 ;
     Event.TauDecay  := 0 ;
     Event.Duration := 0 ;

     Time := (StartOfInterval + EndOfInterval)*0.5*CDRFH.dt ;
     TInterval := (EndOfInterval - StartOfInterval)*CDRFH.dt ;
     NumEventsInInterval := 0 ;

     // Move event index to first event in interval
     while (Events[iEvent] < StartofInterval) and
           (iEvent < NumEvents) do begin
           Inc(iEvent) ;
           end ;

     Interval := -1.0 ;
     while (Events[iEvent] < EndofInterval) and (iEvent < NumEvents) do begin

         // Read event results from file
         FileSeek( EventAnalysisFile, iEvent*SizeOf(Event), 0) ;
         FileRead( EventAnalysisFile, Event, SizeOf(Event)) ;

         EventNum := iEvent + 1 ;

         // Time interval since previous event
         if iEvent > 0 then Interval := (Events[iEvent] - Events[iEvent-1])*CdrFH.dt
                       else Interval := -1.0 ;

         Inc(iEvent) ;

         Inc(NumEventsInInterval) ; // Increment event count

         end ;

     // X axis variable
     Case XVarType of
          vBaseline : X := Channel[cbChannel.ItemIndex].ADCScale*
                          (Event.YBaseline - Channel[cbChannel.ItemIndex].ADCZero) ;
          vEventNum :  X := EventNum ;
          vTime :  X := Time ;
          vPeak : X := Event.Peak ;
          vArea : X := Event.Area ;
          vTRise : X := Event.TRise ;
          vTDecay : X := Event.TDecay ;
          vTauDecay : X := Event.TauDecay ;
          vDuration : X := Event.Duration ;
          vInterval : X := Interval ;
          vFrequencyAvg : X := NumEventsInInterval/TInterval ;
          end ;

     // Y axis variable
     Case YVarType of
             vBaseline : Y := Channel[cbChannel.ItemIndex].ADCScale*
                           (Event.YBaseline - Channel[cbChannel.ItemIndex].ADCZero) ;
             vEventNum :  Y := EventNum ;
             vTime :  Y := Time ;
             vPeak : Y := Event.Peak ;
             vArea : Y := Event.Area ;
             vTRise : Y := Event.TRise ;
             vTDecay : Y := Event.TDecay ;
             vTauDecay : Y := Event.TauDecay ;
             vDuration : Y := Event.Duration ;
             vInterval : Y := Interval ;
             vFrequencyAvg : Y := NumEventsInInterval/TInterval ;
             end ;

     end ;


procedure TEventDetFrm.bExportToWCPFileClick(Sender: TObject);
// -----------------------------------------
// Export detected events to .WCP data file
// -----------------------------------------
var
     i,ch, iStart, NumPretrigger : Integer ;
     NumRecords : Integer ;
     Buf : PSmallIntArrayDyn ;
begin

     // Present user with standard Save File dialog box
     Main.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     Main.SaveDialog.DefaultExt := WCPFileExtension ;

     Main.SaveDialog.FileName := AnsiReplaceText(
                                 LowerCase(ExtractFileName(CdrFH.FileName)),
                                 '.edr',
                                 format('-events %d-%d.wcp',
                                         [Round(edExportRange.LoValue),
                                          Round(edExportRange.HiValue)] ))
                                           ;
     Main.SaveDialog.Filter := format( ' WCP Files (*%s)|*%s',
                                  [WCPFileExtension,WCPFileExtension]) ;
     Main.SaveDialog.Title := 'Export to WCP File' ;

     { Create new data file }
     if not Main.SaveDialog.execute then Exit ;
     if not WCPFile.CreateDataFile( Main.SaveDialog.FileName, ftWCP ) then Exit ;

     bExportToWCPFile.Enabled := False ;
     bAbortExport.Enabled := True ;
     bExportNonEvents.Enabled := False ;

     WCPFile.NumScansPerRecord := Min(Round(edEditDisplayWidth.Value),
                                      MaxExportSamples div CdrFH.NumChannels ) ;
     WCPFile.NumScansPerRecord := Max(256*(WCPFile.NumScansPerRecord div 256),256) ;
     WCPFile.NumChannelsPerScan := CdrFH.NumChannels ;
     WCPFile.WCPNumZeroAvg := 20 ;
     WCPFile.ScanInterval := CDRfH.dt ;
     WCPFile.MaxADCValue := Channel[0].ADCMaxValue ;

     if WCPFile.NumScansPerRecord < (Round(edEditDisplayWidth.Value)-256) then begin
        ShowMessage('EXPORT EVENTS: Warning! Exported record duration truncated.') ;
        end ;

     // Allocate export buffer
     GetMem( Buf, WCPFile.NumScansPerRecord*WCPFile.NumChannelsPerScan*2 ) ;

     for ch := 0 to WCPFile.NumChannelsPerScan-1 do begin
         WCPFile.ChannelName[ch] := Channel[ch].ADCName ;
         WCPFile.ChannelUnits[ch] := Channel[ch].ADCUnits ;
         WCPFile.ChannelOffset[ch] := Channel[ch].ChannelOffset ;
         WCPFile.ChannelCalibrationFactor[ch] := Channel[ch].ADCCalibrationFactor ;
         WCPFile.ChannelGain[ch] := Channel[ch].ADCAmplifierGain ;
         WCPFile.ChannelScale[ch] := Channel[ch].ADCScale ;
         WCPFile.ChannelZeroAt[ch] := 1 ;
         WCPFile.ChannelADCVoltageRange[ch]:= CdrFH.ADCVoltageRange ;
         end ;

     NumPretrigger := Round(WCPFile.NumScansPerRecord*edPreTrigger.Value) ;

     NumRecords := 0  ;
     AbortFlag := False ;
     for i := Round(edExportRange.LoValue-1)
              to Round(edExportRange.HiValue-1) do begin

         // Include pre-trigger samples
         iStart := Events[i] - NumPretrigger ;

         // Extract and save to WCP data file
         if ReadCDRBuffer(CdrFH,iStart,Buf^,WCPFile.NumScansPerRecord)
            = WCPFile.NumScansPerRecord then begin

            // Subtract baseline (if required)
            if ckSubtractBaseline.Checked then begin
               SubtractBaseline( i,
                                 Buf^,
                                 iStart,
                                 WCPFile.NumScansPerRecord ) ;
               end ;

            Inc( NumRecords ) ;

            WCPFile.RecordNum := NumRecords ;

            WCPFile.WCPNumZeroAvg := 20 ;
            WCPFile.WCPRecordAccepted := True ;
            WCPFile.WCPRecordType := 'TEST' ;
            WCPFile.WCPRecordNumber := i+1 ;   // Event #
            WCPFile.WCPRecordTime := Events[i]*CdrFH.dt ;

            WCPFile.SaveADCBuffer( 0, WCPFile.NumScansPerRecord, Buf^ ) ;

            Main.StatusBar.SimpleText := format(
                                         ' Detect Events : Event %d/%d written to %s',
                                           [i+1,NumEvents,WCPFile.FileName] ) ;

            Application.ProcessMessages ;

            end ;

         if AbortFlag then Break ;

         end ;

     // Close WCP data file
     WCPFile.CloseDataFile ;

     // Release buffer
     FreeMem(Buf) ;

     Main.StatusBar.SimpleText := format(
                                  ' Detect Events : %d events written to %s',
                                    [NumRecords,WCPFile.FileName] ) ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     bExportToWCPFile.Enabled := True ;
     bAbortExport.Enabled := False ;
     bExportNonEvents.Enabled := True ;

     end;


procedure TEventDetFrm.CopyDataToClipboard ;
// ----------------------------------
// Copy data on display to clipboard
// ----------------------------------
begin

     // Copy data from appropriate display component depending upon
     // which page is on display

     if Page.ActivePage = DetectEventsPage then scDisplay.CopyDataToClipboard
     else if Page.ActivePage = EditEventsPage then scEditDisplay.CopyDataToClipboard
     else if Page.ActivePage = XYPlotPage then plPlot.CopyDataToClipboard
     else if Page.ActivePage = HistPage then plHist.CopyDataToClipboard
     else if Page.ActivePage = AveragePage then scAverageDisplay.CopyDataToClipboard ;
     end ;


procedure TEventDetFrm.SaveDataToFile ;
// -------------------------------------
// Save data on display to CSV text file
// -------------------------------------
var
    TStart,TEnd : single ;
begin

     // Present user with standard Save File dialog box
     Main.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     Main.SaveDialog.DefaultExt := 'csv' ;
     Main.SaveDialog.Filter := ' CSV Files (*.csv)|*.csv' ;
     Main.SaveDialog.Title := 'Export to CSV File' ;

     { Create new data file }

     if Page.ActivePage = DetectEventsPage then
        begin
        // Save signal in event detection display panel
        TStart := sbDisplay.Position*scDisplay.TScale ;
        TEnd := TStart + scDisplay.NumPoints*scDisplay.TScale ;
        Main.SaveDialog.FileName := AnsiReplaceText(
                                    LowerCase(ExtractFileName(CdrFH.FileName)),
                                    '.edr',
                                    format('.%.3g-%.3gs.csv',
                                    [TStart,TEnd] ));
        end
     else if Page.ActivePage = EditEventsPage then
         Main.SaveDialog.FileName := AnsiReplaceText(
                                    LowerCase(ExtractFileName(CdrFH.FileName)),
                                    '.edr',
                                    format('-event.%d.csv',[sbEvent.Position] ))
     else if Page.ActivePage = XYPlotPage then
         Main.SaveDialog.FileName := AnsiReplaceText(
                                    LowerCase(ExtractFileName(CdrFH.FileName)),
                                    '.edr',
                                    format('.event.%s.%s.csv',
                                    [plPlot.XAxisLabel,plPlot.YAxisLabel]))
     else if Page.ActivePage = HistPage then
         Main.SaveDialog.FileName := AnsiReplaceText(
                                    LowerCase(ExtractFileName(CdrFH.FileName)),
                                    '.edr',
                                    format('.event.%s.%s.csv',
                                    [plPlot.XAxisLabel,plPlot.YAxisLabel]))
     else if Page.ActivePage = AveragePage then
         Main.SaveDialog.FileName := AnsiReplaceText(
                                    LowerCase(ExtractFileName(CdrFH.FileName)),
                                    '.edr',
                                    '-avg.csv' ) ;

     if not Main.SaveDialog.execute then Exit ;

     if Page.ActivePage = DetectEventsPage then scDisplay.SaveDataToFile(Main.SaveDialog.FileName)
     else if Page.ActivePage = EditEventsPage then scEditDisplay.SaveDataToFile(Main.SaveDialog.FileName)
     else if Page.ActivePage = AveragePage then scAverageDisplay.SaveDataToFile(Main.SaveDialog.FileName)
     else if Page.ActivePage = XYPlotPage then plPlot.SaveDataToFile(Main.SaveDialog.FileName)
     else if Page.ActivePage = HistPage then plHist.SaveDataToFile(Main.SaveDialog.FileName) ;

     end ;



procedure TEventDetFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy display image to clipboard as metafile
  -------------------------------------------}
begin

     if Page.ActivePage = DetectEventsPage then begin
        // Detect events display
        PrintRecFrm.Destination := deClipboard ;
        PrintRecFrm.Display := scDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then scDisplay.CopyImageToClipboard ;
        end
     else if Page.ActivePage = EditEventsPage then begin
        // Edit events display
        PrintRecFrm.Destination := deClipboard ;
        PrintRecFrm.Display := scEditDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then scEditDisplay.CopyImageToClipboard ;
        end
     else if Page.ActivePage = XYPlotPage then begin
        // X-Y plot display
       PrintGraphFrm.Plot := plPlot ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
       end
     else if Page.ActivePage = HistPage then begin
        // X-Y plot display
       PrintGraphFrm.Plot := plHist ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plHist.CopyImageToClipboard ;
       end
     else if Page.ActivePage = AveragePage then begin
        // Average events display
        PrintRecFrm.Destination := deClipboard ;
        PrintRecFrm.Display := scAverageDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then scAverageDisplay.CopyImageToClipboard ;
        end

     end ;


function TEventDetFrm.IsClipboardDataAvailable : Boolean ;
// ----------------------------------------------------------
// Is data (data or image) available for copying to clipboard?
// ----------------------------------------------------------
begin
     Result := False ;
     if Page.ActivePage = DetectEventsPage then begin
        Result := True ;
        end
     else if Page.ActivePage = EditEventsPage then begin
        if NumEvents > 0 then Result := True ;
        end
     else if Page.ActivePage = XYPlotPage then begin
        Result := PlotAvailable ;
        end
     else if Page.ActivePage = HistPage then begin
        Result := HistAvailable ;
        end
     else if Page.ActivePage = AveragePage then begin
        if NumEvents > 0 then Result := True ;
        end ;

     end ;


procedure TEventDetFrm.PrintDisplay ;
{ ------------------------------------
  Print record graph or histogram on display
  ------------------------------------ }
begin

     if Page.ActivePage = DetectEventsPage then begin
        // Print detect events display
        PrintRecFrm.Destination := dePrinter ;
        PrintRecFrm.Display := scDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then begin
           scDisplay.ClearPrinterTitle ;
           scDisplay.AddPrinterTitleLine('File : ' + cdrFH.FileName ) ;
           scDisplay.AddPrinterTitleLine( CdrFH.IdentLine ) ;
           scDisplay.Print ;
          end ;
        end
     else if Page.ActivePage = EditEventsPage then begin
        // Print edit events display
        PrintRecFrm.Destination := dePrinter ;
        PrintRecFrm.Display := scEditDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then begin
           PrintRecFrm.Display.ClearPrinterTitle ;
           PrintRecFrm.Display.AddPrinterTitleLine('File : ' + cdrFH.FileName ) ;
           PrintRecFrm.Display.AddPrinterTitleLine( CdrFH.IdentLine ) ;
           PrintRecFrm.Display.Print ;
          end ;
        end
     else if Page.ActivePage = XYPlotPage then begin
        // Print X-Y plot
       PrintGraphFrm.Plot := plPlot ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          plPlot.ClearPrinterTitle ;
          plPlot.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plPlot.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          plPlot.Print ;
          end ;
       end
     else if Page.ActivePage = HistPage then begin
        // Print Histogram
       PrintGraphFrm.Plot := plHist ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          plHist.ClearPrinterTitle ;
          plHist.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plHist.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          plHist.Print ;
          end ;
       end
     else if Page.ActivePage = AveragePage then begin
        // Print average events display
        PrintRecFrm.Destination := dePrinter ;
        PrintRecFrm.Display := scAverageDisplay ;
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


procedure TEventDetFrm.scDisplayCursorChange(Sender: TObject);
// -------------------------------
// Display cursor position changed
// -------------------------------
var
     ch : Integer ;
     iCursorPos : Integer ;
begin

     // Align detection display cursor
     iCursorPos := Round(scDisplay.VerticalCursors[DisplayCursor]) ;
     if Round(scDetDisplay.VerticalCursors[DisplayCursor]) <> iCursorPos then
        scDetDisplay.VerticalCursors[DisplayCursor] := iCursorPos ;

     for ch := 0 to scDisplay.NumChannels-1 do if scDisplay.ChanVisible[ch] then
         begin
         { Get signal baseline cursor }
         Channel[ch].ADCZero := Round(scDisplay.HorizontalCursors[BaseLineCursor]) ;
         Channel[ch].yMin := scDisplay.yMin[ch] ;
         Channel[ch].yMax := scDisplay.yMax[ch] ;
         end ;

     end;


procedure TEventDetFrm.bSetThresholdTo4SDClick(Sender: TObject);
// -------------------------------------------------------
// Set detection threshold to 4 x S.D. of background noise
// -------------------------------------------------------
var
    i,j : Integer ;
    Sum,Mean,SD : Single ;
begin

    // Calculate mean value
    Sum := 0.0 ;
    j := Channel[cbChannel.ItemIndex].ChannelOffset ;
    for i := 0 to scDetDisplay.MaxPoints-1 do
        begin
        Sum := Sum + DetBuf^[j] ;
        j := j + CdrFH.NumChannels ;
        end ;
    Mean := Sum / scDetDisplay.MaxPoints ;

    // Calculate sum of squares
    Sum := 0.0 ;
    j := Channel[cbChannel.ItemIndex].ChannelOffset ;
    for i := 0 to scDetDisplay.MaxPoints-1 do
        begin
        Sum := Sum + (DetBuf^[j] - Mean)*(DetBuf^[j] - Mean) ;
        j := j + CdrFH.NumChannels ;
        end ;

    // Standard deviation
    SD := Sqrt( Sum/(scDetDisplay.MaxPoints-1) ) ;

    // Set threshold cursor
    edThreshold.Value := SD*4.0 ;
    Settings.EventDetector.yThreshold := edThreshold.Value ;
    scDetDisplay.HorizontalCursors[ThresholdCursor] := Round(edThreshold.Value) ;

    end;


procedure TEventDetFrm.bSetPlotAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;


procedure TEventDetFrm.SetupDetector ;
// -------------------------------
// Set up event detection controls
// -------------------------------
begin

     if rbThreshold.Checked then begin
        // Base-line tracking threshold detection
        edThreshold.Units := Channel[cbChannel.ItemIndex].ADCUnits ;
        edThreshold.Scale := Channel[cbChannel.ItemIndex].ADCScale ;
        edTimeThreshold.Visible := True ;
        lbTimeThreshold.Visible := True ;
        ModePage.ActivePage := 'Threshold' ;
        Settings.EventDetector.DetectionMode := mdThreshold ;
        end
     else if rbRateOfRise.Checked then begin
        // Rate of rise detection
        edThreshold.Units := Channel[cbChannel.ItemIndex].ADCUnits
                             + '/' + Settings.TUnits ;
        edThreshold.Scale := 0.1*Channel[cbChannel.ItemIndex].ADCScale /
                             (CdrFH.dt*Settings.TScale) ;
        edTimeThreshold.Visible := False ;
        lbTimeThreshold.Visible := False ;
        ModePage.ActivePage := 'RateOfRise' ;
        Settings.EventDetector.DetectionMode := mdRateOfRise ;

        end
     else if rbPatternMatch.Checked then begin
        // Pattern match detection
        edThreshold.Units := ' ' ;
        edThreshold.Scale := 0.001 ;
        edTauRise.Value := Settings.EventDetector.TauRise ;
        edTauDecay.Value := Settings.EventDetector.TauDecay ;
        edTimeThreshold.Visible := False ;
        lbTimeThreshold.Visible := False ;
        ModePage.ActivePage := 'Template' ;
        Settings.EventDetector.DetectionMode := mdPatternMatch ;
        end ;

     DisplayRecord ;

     end ;


procedure TEventDetFrm.rbThresholdClick(Sender: TObject);
// ----------------------------
// Event detection mode changed
// ----------------------------
begin
     // Set up event detection controls
     SetupDetector ;
     end;


procedure TEventDetFrm.edTauRiseKeyPress(Sender: TObject; var Key: Char);
// -------------------------------------------------------
// Set time constant of rising phase of detection template
// -------------------------------------------------------
begin
     if key = #13 then DisplayRecord ;
     end;


procedure TEventDetFrm.edThresholdKeyPress(Sender: TObject; var Key: Char);

begin
     if key = #13 then begin
        scDetDisplay.HorizontalCursors[ThresholdCursor] := Round(edThreshold.Value) ;
        Settings.EventDetector.yThreshold := edThreshold.Value ;
        end ;
     end;


procedure TEventDetFrm.edTimeThresholdKeyPress(Sender: TObject; var Key: Char);
// ----------------------
// Time threshold changed
// ----------------------
begin
      if Key = #13 then Settings.EventDetector.tThreshold := EdTimeThreshold.Value ;
end;


procedure TEventDetFrm.bExportAnalysisClick(Sender: TObject);
// ----------------------------------------------------
// Export event waveform measuerments to .csv data file
// ----------------------------------------------------
var
     iStart, iEnd : Integer ;
     iEvent: Integer;
     Event : TEventAnalysis ;
     TInterval : Single ;
     s : string ;
     YTable : TStringList ;

begin

     // Present user with standard Save File dialog box
     Main.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     Main.SaveDialog.DefaultExt := WCPFileExtension ;

     Main.SaveDialog.FileName := AnsiReplaceText(
                                 LowerCase(ExtractFileName(CdrFH.FileName)),
                                 '.edr',
                                 format('-events %d-%d.wfm.csv',
                                         [Round(edExportRange.LoValue),
                                          Round(edExportRange.HiValue)] ))
                                           ;
     Main.SaveDialog.Filter := ' CSV Files (*.csv)|*.csv' ;
     Main.SaveDialog.Title := 'Export to CSV File' ;

     { Create new data file }
     if not Main.SaveDialog.execute then Exit ;

     // Create string list
     YTable := TStringList.Create ;

     // Column labels
     s := '"Event",'; ;
     s := s + '"Time",';
     s := s + '"Interval",';
     s := s + '"Frequency",';
     s := s + '"Peak",';
     s := s + '"Area",';
     s := s + '"TRise",';
     s := s + '"TDecay",';
     s := s + '"TauDecay",';
     s := s + '"Duration"';
     YTable.Add(s) ;

     // Update event measurements files
     UpdateEventAnalysisFile ;

     iStart := Round(edExportRange.LoValue)-1 ;
     iEnd := Round(edExportRange.HiValue)-1 ;

     for iEvent := iStart to iEnd do
         begin

         // Read event measurements from file
         FileSeek( EventAnalysisFile, iEvent*SizeOf(Event), 0 ) ;
         FileRead( EventAnalysisFile, Event, SizeOf(Event)) ;

         // Write to CSV table
         s := format('"%d",',[iEvent]) ;
         s := s + format('"%.6g",',[Events[iEvent]*CDRFH.dt]) ;

         // Inter-event interval and frequency
         TInterval := (Events[iEvent]-Events[Max(iEvent-1,0)])*CDRFH.dt ;
         if TInterval > 0.0 then
            begin
            s := s + format('"%.6g",',[TInterval]) ;
            s := s + format('"%.6g",',[1.0/TInterval]) ;
            end
         else
            begin
            s := s + '"",' ;
            s := s + '"",' ;
            end ;

         s := s + format('"%.6g",',[Event.Peak]) ;
         s := s + format('"%.6g",',[Event.Area]) ;
         s := s + format('"%.6g",',[Event.TRise]) ;
         s := s + format('"%.6g",',[Event.TDecay]) ;
         s := s + format('"%.6g",',[Event.TauDecay]) ;
         s := s + format('"%.6g"',[Event.Duration]) ;
         YTable.Add(s) ;


         if (iEvent mod 20) = 0 then Application.ProcessMessages ;
         if AbortFlag then Break ;

         end ;

     // Report completion
     Main.StatusBar.SimpleText := format(
     ' Event Detector : Waveform measurements events (%d-%d) saved to %s',
     [iStart+1,iEnd+1,Main.SaveDialog.FileName]) ;

     // Write to CSV file
     YTable.SaveToFile( Main.SaveDialog.FileName ) ;
     YTable.Free

     end;


procedure TEventDetFrm.bExportNonEventsClick(Sender: TObject);
// -------------------------------------------------------
// Export A/D sample data between events to .EDR data file
// -------------------------------------------------------
const
     NumScansPerBuffer = 256 ;
var
     i,j,ch, NumPretrigger : Integer ;
     RdScan,WrScan,RdEnd,WrEnd : Integer ;
     NumScansInFile,NumScansPerEvent : Integer ;
     iEvent,EventStart : Integer ;
     ExpFH : TCDRFileHeader ;
     InBuf,OutBuf : Array[0..NumScansPerBuffer*(ChannelLimit+1)-1] of SmallInt ;
     YLast,YOffset : Array[0..EDRChannelLimit] of SmallInt ;
     NewYOffsetNeeded : Boolean ;
begin

     bExportToWCPFile.Enabled := False ;
     bAbortExport.Enabled := True ;
     bExportNonEvents.Enabled := False ;

     // Present user with standard Save File dialog box
     Main.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     Main.SaveDialog.DefaultExt := DataFileExtension ;
     Main.SaveDialog.FileName := AnsiReplaceText(
                                 LowerCase(ExtractFileName(CdrFH.FileName)),
                                 '.edr',
                                 format('-gaps %d-%d.edr',
                                         [Round(edExportRange.LoValue),
                                          Round(edExportRange.HiValue)] ))
                                           ;
     Main.SaveDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     Main.SaveDialog.Title := 'Export to EDR File' ;

     { Create new data file }
     if Main.SaveDialog.execute then begin

        // Copy EDR file header data
        ExpFH := CDRFH ;

        // Create an empty EDR file to hold extracted data
        ExpFH.FileHandle := FileCreate( Main.SaveDialog.FileName ) ;
        ExpFH.FileName := Main.SaveDialog.FileName ;
        if ExpFH.FileHandle < 0 then ShowMessage( 'Error creating '
                                     + Main.SaveDialog.FileName ) ;

        // Initialise extraction loop

        NumScansInFile := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;
        NumScansPerEvent := scEditDisplay.MaxPoints ;
        NumPretrigger := Round(NumScansPerEvent*edPreTrigger.Value) ;
        RdScan := 0 ;
        RdEnd := 0 ;
        WrScan := 0 ;
        WrEnd := NumScansPerBuffer ;
        j := 0 ;
        i := 0 ;
        iEvent := Round(edExportRange.LoValue-1) ;
        if iEvent < Round(edExportRange.HiValue) then
           EventStart := Events[iEvent] - NumPreTrigger
        else EventStart := NumScansInFile ;

        for ch := 0 to CdrFH.NumChannels-1 do YOffset[ch] := 0 ;
        NewYOffsetNeeded := False ;

        // Extract A/D samples between events

        while RdScan < NumScansInFile do begin

            // Read A/D samples from file
            if (RdScan >= RdEnd) then begin
               ReadCDRBuffer(CdrFH,RdScan,InBuf,NumScansPerBuffer) ;
               RdEnd := RdScan + NumScansPerBuffer ;
               i := 0 ;
               // Report progress
               Main.StatusBar.SimpleText := format(
               ' Exporting inter-event gaps %.0f/%.0fs to %s',
               [rdScan*CdrFH.dt,NumScansInFile*CdrFH.dt,ExpFH.FileName] ) ;

               Application.ProcessMessages ;

               end ;

            if RdScan >= EventStart then begin
               // Skip A/D samples containing detected event
               RdScan := RdScan + NumScansPerEvent ;
               Inc(iEvent) ;
               if iEvent <= NumEvents then EventStart := Events[iEvent] - NumPreTrigger
                                      else EventStart := NumScansInFile ;
               NewYOffsetNeeded := True ;
               end
            else begin
               // Copy A/D samples not containing event to O/P buffer
               if NewYOffsetNeeded then begin
                  for ch := 0 to CdrFH.NumChannels-1 do begin
                      YOffset[ch] := {YLast[ch] - InBuf[i+ch]} 0 ;
                      end ;
                  NewYOffsetNeeded := False ;
                  end ;
               for ch := 0 to CdrFH.NumChannels-1 do begin
                  OutBuf[j] := InBuf[i] + YOffset[ch] ;
                  YLast[ch] := OutBuf[j] ;
                  Inc(i) ;
                  Inc(j) ;
                  end ;
               Inc(RdScan) ;
               Inc(WrScan) ;
               end ;

            // Write A/D samples to export file
            if WrScan >= WrEnd then begin
               WriteCDRBuffer(ExpFH,WrScan-NumScansPerBuffer,OutBuf,NumScansPerBuffer) ;
               j := 0 ;
               WrEnd := WrScan + NumScansPerBuffer ;
               end ;


            end ;

        // Update file header
        ExpFH.NumSamplesInFile := (WrEnd-NumScansPerBuffer)*CdrFH.NumChannels ;
        SaveCDRHeader( ExpFH ) ;

        // Close EDR export file
        if ExpFH.FileHandle >= 0 then FileClose( ExpFH.FileHandle ) ;

        // Final report
        Main.StatusBar.SimpleText := format(
        ' Export Gaps : %.3g%% of file exported to %s',
        [(100.0*ExpFH.NumSamplesInFile)/CdrFH.NumSamplesInFile,
         ExpFH.FileName] ) ;
        WriteToLogFile( Main.StatusBar.SimpleText ) ;

        end ;

     bExportToWCPFile.Enabled := True ;
     bAbortExport.Enabled := False ;
     bExportNonEvents.Enabled := True ;

     end;


procedure TEventDetFrm.edEditDisplayWidthKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------
// Update no. sample scans in edit display
// ---------------------------------------
begin
     if key = #13 then begin
        Settings.EventDetector.AnalysisWindow := edEditDisplayWidth.Value ;
        NewFile ;
        DisplayEvent ;
        end ;
     end;


procedure TEventDetFrm.cbBaselineChange(Sender: TObject);
// -------------------------
// Baseline position changed
// -------------------------
begin

     Settings.EventDetector.Baseline := cbBaseline.ItemIndex ;
     SaveCDRHeader( CDRFH ) ;

     DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;


procedure TEventDetFrm.cbChannelChange(Sender: TObject);
// ----------------------------
// Change channel to be scanned
// ----------------------------
begin
     Settings.EventDetector.Channel := cbChannel.ItemIndex ;
     NewFile ;
     end;


procedure TEventDetFrm.bNewHistogramClick(Sender: TObject);
{ -------------------------------------
  Request a new Histogram to be plotted
  -------------------------------------}
var
   i,n,VarType,iEvent,StartAtEvent,EndAtEvent,EndOfRateInterval,XNum : Integer ;
   X,XMin,XMax,XMean,XSum,Y, HScale,HLowerLimit,Temp : Single ;
   XUnits : string ;
   OK : Boolean ;
   XBuf : PSingleArray ;
begin

     GetMem( XBuf, Max(NumEvents,1)*4 ) ;

     bNewHistogram.Enabled := false ;
     ComputationInProgress := True ;
     AbortFlag := False ;


     Main.StatusBar.SimpleText := 'Event Detector : WAIT ... Plotting Histogram ' ;

     if rbHistAllEvents.Checked then begin
        { Use all Events }
        StartAtEvent := 0 ;
        EndAtEvent := NumEvents - 1 ;
        end
     else begin
        { Use selected range of records }
        StartAtEvent := Round(edHistRange.LoValue)-1 ;
        EndAtEvent := Round(edHistRange.HiValue)-1 ;
        end ;

     // X axis variable
     VarType := Integer(cbHistVar.Items.Objects[cbHistVar.ItemIndex]) ;
     xUnits := GetVariableUnits( VarType ) ;

     // Read measurement into XBuf and determine min. - max. limits

     iEvent := StartAtEvent ;
     XNum :=  0 ;
     Hist.RangeLo := 1E30 ;
     Hist.RangeHi := -1E30 ;
     while iEvent < EndAtEvent do begin

          OK := CalculateVariables( iEvent,
                                    VarType,
                                    VarType,
                                    X,
                                    Y ) ;
          if OK then begin
             if X < Hist.RangeLo then Hist.RangeLo := X ;
             if X > Hist.RangeHi then Hist.RangeHi := X ;
             XBuf^[XNum] := X ;
             Inc(XNum) ;
             end ;

          if (xNum >= MaxEvents) then Break ;
          if AbortFlag then begin
             bNewHistogram.Enabled := True ;
             FreeMem( XBuf ) ;
             Exit ;
             end ;
          end ;

     // Clear histogram and set up bin ranges
     if not BinRangePanel.Visible then begin
        Hist.NumBins := Round(edNumBins.Value) ;
        edBinsUpper.Value := Hist.RangeHi ;
        edBinsLower.Value := Hist.RangeLo ;
        edBinWidth.Value := (Hist.RangeHi - Hist.RangeLo) / Hist.NumBins ;
        BinRangePanel.Visible := True ;
        edBinWidth.Units := xUnits ;
        edBinsLower.Units := edBinWidth.Units ;
        edBinsUpper.Units := edBinWidth.Units ;
        end ;

     if edBinsUpper.Value <= edBinsLower.Value then
        edBinsUpper.Value := edBinsLower.Value + 1.0 ;
     Hist.RangeLo :=  edBinsLower.Value ;
     edBinsLower.Value := Hist.RangeLo ;  // Ensure units is updated
     Hist.RangeHi :=  edBinsUpper.Value ;
     edBinsUpper.Value := Hist.RangeHi ;     // Ensure units is updated
     edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/Round(edNumBins.Value) ;
     Hist.BinWidth := edBinWidth.Value ;
     Hist.NumBins := Round(edNumBins.Value) ;

     Hist.MaxBin := Hist.NumBins - 1 ;
     x := Hist.RangeLo ;
     for i := 0 to Hist.NumBins-1 do begin
         Hist.Bins[i].Lo := x ;
         Hist.Bins[i].Mid := x + Hist.BinWidth / 2. ;
         Hist.Bins[i].Hi := x + Hist.BinWidth ;
         Hist.Bins[i].y := 0. ;
         x := x + Hist.BinWidth ;
         end ;

     // Fill Histogram from XBuf

     HScale := (Hist.NumBins) / ( Hist.Bins[Hist.NumBins-1].Mid
                                         - Hist.Bins[0].Mid ) ;
     HLowerLimit := Hist.Bins[0].Lo ;
     XMin := 1E30 ;
     XMax := -XMin ;
     XSum := 0.0 ;
     for iEvent := 0 to XNum-1 do begin
         X := XBuf[iEvent] ;
         i := Round( (x - HLowerLimit) * HScale ) ;
         i := Max( Min( i,Hist.NumBins-1 ), 0 ) ;
         Hist.Bins[i].y := Hist.Bins[i].y + 1. ;
         if Hist.Bins[i].y > Hist.yHi then Hist.yHi := Hist.Bins[i].y ;
         if XMin >= X then XMin := X ;
         if XMax <= X then XMax := X ;
         XSum := XSum + X ;
         end ;

     { Plot new Histogram }
     plHist.xAxisAutoRange := True ;
     plHist.xAxisMin := Hist.Bins[0].Lo ;
     plHist.xAxisMax := Hist.Bins[Hist.NumBins-1].Hi ;
//     plHist.XAxisTick := (plHist.xAxisMax - plHist.xAxisMin) / 5.0 ;

     plHist.HistogramCumulative := ckCumulative.Checked ;
     plHist.HistogramPercentage := ckPercentage.Checked ;

     { Create X and Y axes labels }
     plHist.xAxisLabel := cbHistVar.Text + ' (' + xUnits + ')' ;

     plHist.yAxisAutoRange := True ;
     if ckPercentage.Checked then plHist.yAxisLabel := '%'
                             else plHist.yAxisLabel := ' ' ;

     { Create Histogram plot }
     plHist.CreateHistogram( 0 ) ;
     for i := 0 to Hist.NumBins-1 do plHist.AddBin( 0,
                                     Hist.Bins[i].Lo,
                                     Hist.Bins[i].Mid,
                                     Hist.Bins[i].Hi,
                                     Hist.Bins[i].y ) ;

     { Create and set Histogram cursors to initial positions }

     { Initial cursor positions }
     i := 0 ;
     while (Hist.Bins[i].y = 0.0) and (i < Hist.MaxBin) do Inc(i) ;
     plHist.VerticalCursors[HistCurs.C0] := Hist.Bins[i].Mid ;

     i := Hist.MaxBin ;
     while (Hist.Bins[i].y = 0.0) and (i > 0) do Dec(i) ;
     plHist.VerticalCursors[HistCurs.C1] := Hist.Bins[i].Mid ;

     plHist.VerticalCursors[HistCurs.Read] := Hist.Bins[Hist.MaxBin div 2].Mid ;

     // Calculate mean
     if XNum > 0 then XMean := XSum / XNum
                 else XMean := 0.0 ;

     meHistResults.Clear ;
     meHistResults.Lines.Add(format('Mean = %.5g %s',[XMean,XUnits])) ;
     meHistResults.Lines.Add(format('Min. = %.5g %s',[XMin,XUnits])) ;
     meHistResults.Lines.Add(format('Max. = %.5g %s',[XMax,XUnits])) ;
     meHistResults.Lines.Add(format('n= %d',[XNum])) ;

     HistAvailable := True ;
     bSetHistAxes.Enabled := HistAvailable ;
     bNewHistogram.Enabled := True ;
     Main.StatusBar.SimpleText := '' ;
     ComputationInProgress := False ;

     FreeMem( XBuf ) ;

     end;


procedure TEventDetFrm.bAddFilterClick(Sender: TObject);
// ----------------------------------
// Add a new filter criterion to set
// ----------------------------------
var
     i : Integer ;
begin
     // Find next empty filter slot
     i := 0 ;
     while Filters[i].Use and (i<High(Filters)) do Inc(i) ;
     // Add filter criteria
     if not Filters[i].Use then begin
        Filters[i].Use := True ;
        Filters[i].Variable := Integer(cbVariable.Items.Objects[cbVariable.ItemIndex]) ;
        Filters[i].LoLimit := edLoLimit.Value ;
        Filters[i].HiLimit := edHiLimit.Value ;
        end
     else bAddFilter.Enabled := False ;

     // Update filter criteria list
     UpdateFilterSet( Filters ) ;
     end;


procedure TEventDetFrm.bDeleteClick(Sender: TObject);
// ---------------------------------
// Delete all filter criteria in set
// ---------------------------------
var
     i : Integer ;
begin
     for i := 0 to High(Filters) do Filters[i].Use := False ;
     UpdateFilterSet( Filters ) ;
     bAddFilter.Enabled := True ;
     end;


procedure TEventDetFrm.UpdateFilterSet(
          Filters : Array of TFilter ) ;
// ------------------------------
// Update list of filter criteria
// ------------------------------
var
    Action : string ;
    i : Integer ;
begin

    meFilterSet.Clear ;

    // Filter combination mode

    // Update filter list
    for i := 0 to High(Filters) do if Filters[i].Use then begin

        // Add criterion combination mode to end of line
        if rbAND.Checked then Action := ' AND'
                         else Action := ' OR' ;

        // Clear combination mode from last line
        if i <  High(Filters) then begin
           if not Filters[i+1].Use then Action := '' ;
           end
        else Action := '' ;

        meFilterSet.Lines.Add(format('%.3g <= %s <= %.3g %s',
        [Filters[i].LoLimit,
         VarNames[Filters[i].Variable],
         Filters[i].HiLimit,
         Action] )) ;

        end ;
    end ;


procedure TEventDetFrm.bApplyClick(Sender: TObject);
// ----------------------------------
// Select events using filter critera
// ----------------------------------
var
     iEvent : Integer ;                     // Event no.
     Event : TEventAnalysis ;               // Event waveform anaysis results
     i : Integer ;                          // Counter
     Results : Array[0..MaxVar] of Single ; // Event analysis results
     Val : Single ;                         //
     Match : Boolean ;           // Criterion match flag
     RemoveEvent : Boolean ;     // Combined criterion
     NumDeleted : Integer ;                 // No. events deleted
begin

     iEvent := 0 ;
     NumDeleted := 0 ;
     while iEvent < NumEvents do begin

         // Analyse event waveform
         AnalyseEvent( iEvent, Event ) ;
         Results[vPeak] := Event.Peak ;
         Results[vArea] := Event.Area ;
         Results[vTRise] := Event.TRise ;
         Results[vTDecay] := Event.TDecay ;
         Results[vTauDecay] := Event.TauDecay ;
         Results[vDuration] := Event.Duration ;

         if iEvent > 0 then
            Results[vInterval] := (Events[iEvent] - Events[iEvent-1])*CDRFH.dt
         else Results[vInterval] := 0.0 ;

         Results[vEventNum] := iEvent ;
         Results[vTime] := Events[iEvent]*CDRFH.dt ;

        // Determine if event meets the rejection criteria
        if rbAND.Checked then RemoveEvent := True
                         else RemoveEvent := False ;
        for i := 0 to High(Filters) do if Filters[i].Use then begin

            // Criterion match
            Val := Results[Filters[i].Variable] ;
            if (Val >= Filters[i].LoLimit) and
               (Val <= Filters[i].HiLimit) then
               Match := True
            else Match := False ;

            // Combine with results of previous criteria
            if rbAND.Checked then RemoveEvent := RemoveEvent AND Match
                             else RemoveEvent := RemoveEvent OR Match ;

            end ;

        // Remove event if it matches criteria
        if RemoveEvent then begin
           for i := iEvent to NumEvents-1 do Events[i] := Events[i+1] ;
           Dec(NumEvents) ;
           Inc(NumDeleted) ;
           end
        else Inc(iEvent) ;

        Main.StatusBar.SimpleText :=  format(
        ' Filtering Events %d/%d (%d events deleted)',
        [iEvent+1,NumEvents,NumDeleted] ) ;

        EventAnalysisFileUpdateRequired := True ;
        BaselineSplinesAvailable := False ;

        end ;

     Main.StatusBar.SimpleText :=  format(
     ' Event Filter : Completed (%d events deleted)',
     [NumDeleted] ) ;
     WriteToLogFile(Main.StatusBar.SimpleText) ;

     sbEvent.Position := Min( sbEvent.Position, NumEvents ) ;
     sbEvent.Max := Max( NumEvents,1 ) ;
     Application.ProcessMessages ;

     // Update event list file
     SaveEventList ;

     // Update controls on edit events page
     UpdateEditEventPage ;

     end ;


procedure TEventDetFrm.cbHistVarChange(Sender: TObject);
// ------------------------------------
// Histogram variable selection changed
// ------------------------------------
begin
     BinRangePanel.Visible := False ;
     end;


procedure TEventDetFrm.bLoadEventListClick(Sender: TObject);
// ------------------------------------------
// Load list of event detection times from file
// ------------------------------------------
var
   Time : Single ;
   InFile : TextFile ;
begin

     // Get the name of a data file to hold ezported data
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*.txt)|*.txt' ;
     OpenDialog.Title := 'Load event times';

     if OpenDialog.execute then begin
        // Open text file
        AssignFile( InFile, OpenDialog.FileName ) ;
        Reset( InFile ) ;
        // Write data to file
        NumEvents := 0 ;
        While not EOF(InFile) do begin
            ReadLn(InFile, Time ) ;
            Events[NumEvents] := Round(Time/CdrFH.dt) ;
            if NumEvents < High(Events) then Inc(NumEvents) ;
            end ;
        CloseFile( InFile ) ;
        end ;

    Main.StatusBar.SimpleText := format( 'Detect Events : Event list (%d events) loaded from %s',
                                 [NumEvents,OpenDialog.FileName]) ;
    WriteToLogFile(Main.StatusBar.SimpleText) ;

    // Update controls on edit events page
    UpdateEditEventPage ;

    // Update event list file
    SaveEventList ;

    EventAnalysisFileUpdateRequired := True ;
    BaselineSplinesAvailable := False ;

    end ;


procedure TEventDetFrm.bSaveEventListClick(Sender: TObject);
// ------------------------------------------
// Save list of event detection times to file
// ------------------------------------------
var
   i : Integer ;
   Time : Single ;
   OutFile : TextFile ;

begin

     // Get the name of a data file to hold exported data
     SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := '.txt' ;
     SaveDialog.Filter := ' Text Files (*.txt)|*.txt' ;
     SaveDialog.Title := 'Save event times';

     SaveDialog.FileName := AnsiReplaceStr(
                            LowerCase(ExtractFileName(CdrFH.FileName)),
                                 '.edr',
                                 ' (event list).txt' ) ;

     if SaveDialog.execute then begin
        // Open text file
        AssignFile( OutFile, SaveDialog.FileName ) ;
        ReWrite( OutFile ) ;
        // Write data to file
        for i := 0 to NumEvents-1 do begin
            Time := Events[i]*CdrFH.dt ;
            WriteLn(OutFile, format( '%.6g', [Time] ))
            end ;
        CloseFile( OutFile ) ;

        Main.StatusBar.SimpleText := format( 'Detect Events : Event list (%d events) saved to %s',
                                 [NumEvents,OpenDialog.FileName]) ;
        WriteToLogFile(Main.StatusBar.SimpleText) ;

        end ;


    end ;


procedure TEventDetFrm.cbVariableChange(Sender: TObject);
// -----------------------------
// Event filter variable changed
// -----------------------------
begin
     edLoLimit.Units := GetVariableUnits(Integer(cbVariable.Items.Objects[cbVariable.ItemIndex])) ;
     edHiLimit.Units := edLoLimit.Units ;
     end;


function TEventDetFrm.GetVariableUnits(
         VarNum : Integer
         ) : String ;
// --------------------------------
// Return units of variable VarNum
// --------------------------------
begin
     case VarNum of
        vEventNum : Result := '' ;
        vTime,vInterval : Result := 's' ;
        vTRise,vTDecay,vTauDecay,vDuration : Result := 'ms' ;
        vFrequencyAvg,vFrequencyInst : Result := 'Hz' ;
        vPeak,vBaseline : Result := Channel[cbChannel.ItemIndex].ADCUnits ;
        vArea : Result := Channel[cbChannel.ItemIndex].ADCUnits + '.ms' ;
        end ;
     end;


procedure TEventDetFrm.edLoLimitKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        edLoLimit.Value := edLoLimit.Value ;
        edHiLimit.Value := edHiLimit.Value ;
        end ;
     end;


procedure TEventDetFrm.edHiLimitKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        edLoLimit.Value := edLoLimit.Value ;
        edHiLimit.Value := edHiLimit.Value ;
        end ;
     end;


procedure TEventDetFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// -------------------------------
// Special control keys processing
// -------------------------------
begin
     Case Key of
          VK_F2 : bDeleteEvent.Click ;
          VK_F1 : bInsertEvent.Click ;
          end ;
     end;


procedure TEventDetFrm.edAnalysisWindowKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------------------------
// Re-display event when analysis window duration changed
// ------------------------------------------------------
begin
     if Key = #13 then DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;


procedure TEventDetFrm.edDeadTimeKeyPress(Sender: TObject; var Key: Char);
// ----------------------------------------------
// Update display when dead time has been changed
// ----------------------------------------------
begin
     if Key = #13 then begin
        Settings.EventDetector.DeadTime := edDeadTime.Value ;
        DisplayRecord ;
        end ;

     end;

     
procedure TEventDetFrm.edBaselineAveragingIntervalKeyPress(Sender: TObject;
  var Key: Char);
// ----------------------------------------------------------------
// Update display when baseline averaging interval has been changed
// ----------------------------------------------------------------
begin
     if Key = #13 then begin
        Settings.EventDetector.BaselineAveragingInterval := edBaselineAveragingInterval.Value ;
        DisplayRecord ;
        end ;
     end;


procedure TEventDetFrm.rbANDClick(Sender: TObject);
begin
     UpdateFilterSet( Filters ) ;
     end;

procedure TEventDetFrm.rbPositiveClick(Sender: TObject);
// ---------------------------
// Analysis polarity changed
// ---------------------------
begin

     Settings.EventDetector.PositivePeaks := rbPositive.Checked ;
     SaveCDRHeader( CDRFH ) ;
     DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;


procedure TEventDetFrm.bHistFitCurveClick(Sender: TObject);
{ ---------------------------------------------------
  Fit a gaussian prob. density functions to histogram
  --------------------------------------------------- }
const
     NumFitPoints = 500 ;
     FittedLine = 1 ;
var
   i,iStart,iEnd,iBins, Comp,LineNum,NumComp : Integer ;
   x,dx : single ;
   ParTemp : Array[0..LastParameter] of single ;
begin

    lbHistResults.Caption := '' ;
    CVFit.Equation := TCFEqnType(cbHistEqn.Items.Objects[cbHistEqn.ItemIndex]) ;
    CVFit.xUnits := GetVariableUnits( Integer(cbHistVar.Items.Objects[cbHistVar.ItemIndex]) ) ;
    CVFit.YUnits := '' ;

    // Add points to curve fitter
    CVFit.ClearPoints ;

    { Clear all existing lines on plot }
    plHist.ClearAllLines ;

    if CVFit.Equation = None then begin
        plHist.Invalidate ;
        Exit ;
        end;

    { Lower and upper x data limit set by display cursors }
    iStart := Min( plHist.FindNearestIndex( 0, HistCurs.C0 ),
                   plHist.FindNearestIndex( 0, HistCurs.C1 )) ;
    iEnd := Max( plHist.FindNearestIndex( 0, HistCurs.C0 ),
                 plHist.FindNearestIndex( 0, HistCurs.C1 )) ;
    for iBins := iStart to iEnd do begin
        CVFit.AddPoint( Hist.Bins[iBins].Mid, Hist.Bins[iBins].y ) ;
        end ;

    if CVFit.NumPoints < CVFit.NumParameters then begin
       ShowMessage('Not enough points to fit curve to histogram' ) ;
       Exit ;
       end ;

    // Let user set fitting parameters
    CVFit.ParametersSet := False ;
    SetFitPars1frm.CVFit := CVFit ;
    SetFitPars1frm.ShowModal ;
    if SetFitPars1frm.ModalResult <> mrOK then Exit ;

    // Find best fit curve
    CVFit.FitCurve ;

    x := plHist.xAxisMin ;
    dx := (plHist.xAxisMax - plHist.xAxisMin) / NumFitPoints ;
    plHist.ShowLines := True ;
    plHist.CreateLine( FittedLine, clRed, msNone, psSolid ) ;
    for i := 0 to NumFitPoints-1 do begin
        plHist.AddPoint( FittedLine, x, CVFit.EquationValue(x) ) ;
         x := x + dx ;
         end ;

    { Save parameters and initialise gaussian component lines }
    if (CVFit.Equation = Gaussian2) or
       (CVFit.Equation = Gaussian3) then begin

       NumComp := CVFit.NumParameters div 3 ;
       LineNum := FittedLine ;
       for Comp := 0 to NumComp-1 do begin
          Inc(LineNum) ;
          plHist.CreateLine( LineNum, clRed, msNone, psSolid ) ;
          ParTemp[Comp*3] := CVFit.Parameters[Comp*3] ;
          ParTemp[Comp*3+1] := CVFit.Parameters[Comp*3+1] ;
          ParTemp[Comp*3+2] := CVFit.Parameters[Comp*3+2] ;
          CVFit.Parameters[Comp*3+2] := 0.0 ;
          end ;

       { Plot each individual gaussian component }
       if NumComp > 1 then begin
          LineNum := FittedLine ;
          for Comp := 0 to NumComp-1 do begin
             CVFit.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
             Inc(LineNum) ;
             x := plHist.xAxisMin ;
             dx := (plHist.xAxisMax - plHist.xAxisMin) / NumFitPoints ;
             for i := 0 to Hist.NumBins-1 do begin
                plHist.AddPoint( LineNum, x, CVFit.EquationValue(x) ) ;
                x := x + dx ;
                end ;
             CVFit.Parameters[Comp*3+2] := 0.0 ;
             end ;
          end ;

       { Restore parameters }
       for Comp := 0 to NumComp-1 do begin
          CVFit.Parameters[Comp*3] := ParTemp[Comp*3] ;
          CVFit.Parameters[Comp*3+1] := ParTemp[Comp*3+1] ;
          CVFit.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
          end ;

       end ;

     { Make sure plot is updated with changes }
     plHist.Invalidate ;

    lbHistResults.Caption := CVFit.FitResults ;

     end ;



procedure TEventDetFrm.bSetHistAxesClick(Sender: TObject);
{ -----------------------------------
  Set histogram axes range/law/labels
  -----------------------------------}
begin
     SetAxesFrm.Plot := plHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TEventDetFrm.bDoAverageClick(Sender: TObject);
// ----------------------------------
// Compute average of detected events
// ----------------------------------
const
     NumAvgRequired = 10 ;
var
     StartAtEvent : Integer ;  // First event in averaging range
     EndAtEvent : Integer ;    // Last event
     iEvent : Integer ;        // Event index
     i,j : Integer ;
     PreScans : Integer ;   // No. of scans before event detection point
     StartScan : Integer ;  // Start of analysis block
     NumScans : Integer ;   // No. of scans in analysis block
     EventScan : Integer ;  // Scan (within Buf) where event occurs

     NumRead : Integer ;      // No. of scans read into buffer

     SortedEvents : PIntArray ;
     iEv : Integer ;
     NumSortedEvents : Integer ;
     AvgSum : PSingleArrayDyn ;

begin


     bDoAverage.Enabled := False ;
     bAbortAverage.Enabled := True ;
     ComputationInProgress := True ;
     AbortFlag := False ;

     Main.StatusBar.SimpleText := 'Event Detector : WAIT ... Plotting Histogram ' ;

     if rbAverageAllEvents.Checked then begin
        { Use all Events }
        StartAtEvent := 0 ;
        EndAtEvent := NumEvents - 1 ;
        end
     else begin
        { Use selected range of records }
        StartAtEvent := Round(edAverageRange.LoValue)-1 ;
        EndAtEvent := Round(edAverageRange.HiValue)-1 ;
        end ;

     // Get first and last scans of analysis window
     NumScans := Round(edEditDisplayWidth.Value) ;
     PreScans := Round(NumScans*edPreTrigger.Value) ;
     Settings.EventDetector.PreTriggerFraction := edPreTrigger.Value ;

     // Allocate averaging buffer
     GetMem( AvgSum, NumScans*4 ) ;
     GetMem( SortedEvents, Max(NumEvents,1)*4 ) ;

     for i := 0 to NumScans-1 do AvgSum^[i] := 0.0 ;

     // Sort events by amplitude if count match averaging in use
     if ckCountMatchedAvg.Checked then begin
        CreateAmplitudeSortedEventList( StartAtEvent,
                                        EndAtEvent,
                                        SortedEvents^,
                                        NumSortedEvents ) ;
        StartAtEvent := 0 ;
        EndAtEvent := Max( Round(edNumCountMatchedAvg.Value)-1,0) ;
        end ;

     NumEventsAveraged := 0 ;
     EventScan := 0 ;
     for iEv := StartAtEvent to EndAtEvent do begin

        // Select event number from sorted list if count matched averaging in use
        if ckCountMatchedAvg.Checked then iEvent := SortedEvents^[iEv]
                                     else iEvent := iEv ;

        // First multi-channel scan in analysis block
        StartScan := Events[iEvent] - PreScans ;
        // Skip if insufficient pre-event scans
        if StartScan < 0 then Continue ;

        // Set scan at which event detection point is located
        EventScan := Events[iEvent] - StartScan ;

        // Read A/D sample data for event from file
        NumRead := ReadCDRBuffer(CdrFH,StartScan,AvgBuf^,NumScans) ;

        // Terminate averaging loop if insufficient post-detection scans
        if NumRead < NumScans then Break ;

        // Add to averaging buffer
        j := Channel[cbChannel.ItemIndex].ChannelOffset ;
        for i := 0 to NumScans-1 do begin
            AvgSum[i] := AvgSum[i] + AvgBuf^[j] ;
            j :=  j + CDRFH.NumChannels ;
            end ;

        Inc( NumEventsAveraged) ;

        Main.StatusBar.SimpleText := format(
                                     'Event Detector : Averaging %d/%d ',
                                     [iEvent+1,EndAtEvent+1]) ;

        Application.ProcessMessages ;

        if AbortFlag then Break ;

        end ;

     // Calculate average
     j := Channel[cbChannel.ItemIndex].ChannelOffset ;
     for i := 0 to NumScans-1 do begin
         AvgBuf^[j] := Round( AvgSum[i]/Max(NumEventsAveraged,1) ) ;
         j :=  j + CDRFH.NumChannels ;
         end ;

     // Calculate waveform measurements for average
     AvgStartEvent := StartAtEvent ;
     AvgEndEvent := EndAtEvent ;
     AvgEventScan := EventScan ;
     AvgNumScans := NumScans ;
     AvgPreScans := Prescans ;
     AnalyseAverage ;

     bDoAverage.Enabled := True ;
     bAbortAverage.Enabled := False ;
     ComputationInProgress := False ;

     scAverageDisplay.Invalidate ;

     // Release buffer
     FreeMem( AvgSum ) ;
     FreeMem( SortedEvents ) ;

     end ;


procedure TEventDetFrm.AnalyseAverage ;
// -------------------------------------------
// Calculate waveform measurements for average
// -------------------------------------------
const
     NumAvgRequired = 10 ;
var
     i,j : Integer ;
     PostScans : Integer ;  // No. of scans after event detection point
     StartScan : Integer ;  // Start of analysis block
     EndScan : Integer ;    // End of analysis block

     Event : TEventAnalysis ;
     Polarity : Integer ;   // Event polarity (1=positive-going,-1=negative-)
     Sum : Single ;         // Summation variable
     NumAvg : Integer ;     // No. samples averaged for baseline
     NumNegDiffs : Integer ; // No. of negative differences (baseline detector)
     Y : Single ;           // A/D sample value
     YMax : Single ;        // Maximum value within block
     PeakAt : Integer ;     // Scan at which peak occurs
     MidPointOfRiseAt : Integer ; // Scan at mid-point of event rise
     YDecay : Single ;         // X% of peak
     Y90 : Single ;         // 90% of peak
     Y50 : Single ;         // 50% of peak
     Y10 : Single ;         // 10% of peak
     NumRise : Integer ;    // No. of scans within event rising phase
     Num10toPeak : Integer ;
     T : Single ;          //
     SumT : Single ;        // Summation variables for time constant fit
     SumT2 : Single ;         //
     SumY : Single ;         //
     SumYT : Single ;         //
     nPoints : Integer ;
     Slope : Single ;
     NumRead : Integer ;      // No. of scans read into buffer
     Done : Boolean ;
     AnalysisStart : Integer ;
     AnalysisEnd : Integer ;

begin

     if NumEventsAveraged <= 0 then Exit ;

     // Calculate waveform measurements

     // Set positive/negative polarity of event to be measured
     if rbPositive.Checked then Polarity := 1
                           else Polarity := -1 ;

     // Find baseline level
     Event.YBaseline := PreEventBaselineLevel( AvgBuf^,
                                               AvgPreScans,
                                               scAverageDisplay.NumPoints ) ;

     scAverageDisplay.HorizontalCursors[0] := Event.YBaseline ;

     // Set limits of range to be analysed
     AnalysisStart := Min( Round(scAverageDisplay.VerticalCursors[AverageC0Cursor]),
                           Round(scAverageDisplay.VerticalCursors[AverageC1Cursor]) ) ;
     AnalysisStart := Max( AnalysisStart,0) ;
     AnalysisEnd := Max( Round(scAverageDisplay.VerticalCursors[AverageC0Cursor]),
                         Round(scAverageDisplay.VerticalCursors[AverageC1Cursor]) ) ;
     AnalysisEnd := Min( Max(AnalysisEnd,AnalysisStart+1),AvgNumScans-1 ) ;

     // Find peak amplitude and area under curve
     YMax := -1E30 ;
     SumY := 0.0 ;
     PeakAt := AnalysisStart ;
     for i := AnalysisStart to AnalysisEnd do begin
          j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
          Y := Polarity*(AvgBuf^[j] - Event.YBaseline) ;
          if Y >= YMax then begin
             YMax := Y ;
             PeakAt := i ;
             end ;
         SumY := SumY + Y ;
         end ;

     Event.Peak := Polarity*YMax*Channel[cbChannel.ItemIndex].ADCScale ;
     Event.Area := Polarity*SumY*Channel[cbChannel.ItemIndex].ADCScale*CDRFH.dt*SecsToMs ;

     // Find rise time
     i := PeakAt ;
     MidPointOfRiseAt := PeakAt ;
     Y10 := YMax / 10.0 ;
     Y50 := YMax / 2.0 ;
     Y90 := YMax - Y10 ;
     NumRise := 0 ;
     Num10toPeak := 0 ;
     Done := False ;
     while not Done do begin
          j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
          Y := Polarity*(AvgBuf^[j] - Event.YBaseline) ;
          if Y <= Y90 then Inc(NumRise) ;
          if Y >= Y50 then MidPointOfRiseAt := i ;
          Inc(Num10toPeak) ;
          Dec(i) ;
          if (Y < Y10) or (i<=0) then Done := True ;
          end ;
     Event.TRise := NumRise*CDRFH.dt*SecsToMs ;

     // Find time to X% decay
     i := PeakAt ;
     YDecay := YMax*(1.0 - (0.01*edTDecayPercentage.Value)) ;
     Done := False ;
     while not Done do begin
          j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
          Y := Polarity*(AvgBuf^[j] - Event.YBaseline) ;
          Inc(i) ;
          if (Y < YDecay) or (i>=AvgNumScans) then Done := True ;
          end ;

     if cbDecayFrom.ItemIndex = cTDecayFromPeak then begin
        // Decay time from peak
        Event.TDecay := (i-PeakAt)*CDRFH.dt*SecsToMs ;
        end
     else if cbDecayFrom.ItemIndex = cTDecayFromMidRise then begin
        // Decay time from mid-point of rising phase
        Event.TDecay := (i-MidPointOfRiseAt)*CDRFH.dt*SecsToMs ;
        end
     else begin
        // Decay time from first analysis cursor
        Event.TDecay := (i-AnalysisStart)*CDRFH.dt*SecsToMs ;
        end ;

     // Event duration (10% of rising phase to 90% decay after peak)
     i := PeakAt ;
     YDecay := YMax*0.1 ;
     Done := False ;
     while not Done do begin
         j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
         Y := Polarity*(AvgBuf[j] - Event.YBaseline) ;
         Inc(i) ;
         if (Y < YDecay) or (i>=AvgNumScans) then Done := True ;
         end ;
     Event.Duration :=  ((i-PeakAt) + Num10toPeak)*CDRFH.dt*SecsToMs ;

   // Calculate decay time constant
   i := PeakAt ;
   T := 0.0 ;
   SumT := 0.0 ;
   SumT2 := 0.0 ;
   SumY := 0.0 ;
   SumYT := 0.0 ;
   nPoints := 0 ;
   Done := False ;
   while not Done do begin
      j := (i*CDRFH.NumChannels) + Channel[cbChannel.ItemIndex].ChannelOffset ;
      Y := Polarity*(AvgBuf^[j] - Event.YBaseline) ;
      if Y > Y90 then begin
         Y := Ln(Y) ;
         SumT := SumT + T ;
         SumT2 := SumT2 + T*T ;
         SumY := SumY + Y ;
         SumYT := SumYT + Y*T ;
         Inc(nPoints) ;
         end
      else Done := True ;
      Inc(i) ;
      T := T + CDRFH.dt ;
      if i >= AnalysisEnd then Done := True ;
      end ;
   if nPoints > 1 then begin
      Slope := ((nPoints*SumYT) - (SumT*SumY)) /
               ((nPoints*SumT2) - (SumT*SumT)) ;
      if Slope < 0.0 then Event.TauDecay := (-1.0/Slope)*SecsToMs
                     else Event.TauDecay := 0.0 ;
      end
   else Event.TauDecay := 0.0 ;


     meAverageResults.Clear ;
     meAverageResults.Lines.Add( format( 'Avg. of %d-%d (%d)',
                                [AvgStartEvent+1,
                                 AvgEndEvent+1,
                                 AvgEndEvent-AvgStartEvent+1] )) ;

     meAverageResults.Lines.Add( format( 'Peak (a-a)= %.5g %s',
                                  [Event.Peak,
                                   Channel[cbChannel.ItemIndex].ADCUnits]));

     meAverageResults.Lines.Add( format( 'Area (a-a)= %.5g %s.ms',
                                  [Event.Area,
                                   Channel[cbChannel.ItemIndex].ADCUnits]));

     meAverageResults.Lines.Add( format( 'T(rise)= %.5g ms',[Event.TRise]));

     meAverageResults.Lines.Add( format( 'T(%.0f%%)= %.5g ms',
                                 [edTDecayPercentage.Value,Event.TDecay]));

     meAverageResults.Lines.Add( format( 'Tau(decay)= %.5g ms',[Event.TauDecay]));

     meAverageResults.Lines.Add( format( 'Duration= %.5g ms',[Event.Duration]));

     end;


procedure TEventDetFrm.CreateAmplitudeSortedEventList(
          StartEvent : Integer ;
          EndEvent : Integer ;
          var SortedEvents : Array of Integer ;
          var NumSortedEvents : Integer
          ) ;
// --------------------------------------------
// Calculate list of events sorted by amplitude
// --------------------------------------------
var
    Amplitude : PSingleArray ; // Event amplitude
    X,Y : Single ;
    iEvent,iEv : Integer ;
    Last,Current : Integer ;
    Temp : Single ;
    iTemp : Integer ;
begin

   GetMem( Amplitude, Max(NumEvents,1)*4 ) ;

   // Calculate peak amplitude
   AbortFlag := False ;
   NumSortedEvents := 0 ;

   for iEv := StartEvent to EndEvent do begin

       iEvent := iEv ;
       CalculateVariables( iEvent,
                           vEventNum,
                           vPeak,
                           X,
                           Y ) ;

       SortedEvents[NumSortedEvents] := iEv ;
       Amplitude^[NumSortedEvents] := Y ;

       Inc(NumSortedEvents) ;

       if AbortFlag then begin
          FreeMem( Amplitude ) ;
          Exit ;
          end ;

       end ;

   // Sort by amplitude
   for Last := NumSortedEvents-1 DownTo 1 do begin
       for Current := 0 to Last-1 do begin
           if Amplitude[Current] < Amplitude[Current+1] then begin
              Temp := Amplitude[Current] ;
              Amplitude^[Current] := Amplitude^[Current+1] ;
              Amplitude^[Current+1] := Temp ;
              iTemp := SortedEvents[Current] ;
              SortedEvents[Current] := SortedEvents[Current+1] ;
              SortedEvents[Current+1] := iTemp ;
              end ;
           end ;
       end ;

   FreeMem( Amplitude ) ;

   end ;


procedure TEventDetFrm.scAverageDisplayCursorChange(Sender: TObject);
// ----------------------------------
// Average display window cursor changed
// ----------------------------------
var
     ch : Integer ;
begin

     for ch := 0 to scAverageDisplay.NumChannels-1 do
         if scAverageDisplay.ChanVisible[ch] then begin
         { Get signal baseline cursor }
         Channel[ch].yMin := scAverageDisplay.yMin[ch] ;
         Channel[ch].yMax := scAverageDisplay.yMax[ch] ;
         end ;
     end;


procedure TEventDetFrm.rbAveragePositiveClick(Sender: TObject);
// -----------------------------------
// Positive waveform analysis selected
// -----------------------------------
begin
     // Force new average (if one already exists)
     if NumEventsAveraged > 0 then bDoAverage.Click ;
     end;


procedure TEventDetFrm.rbAverageNegativeClick(Sender: TObject);
// -----------------------------------
// Negative waveform analysis selected
// -----------------------------------
begin
     // Force new average (if one already exists)
     if NumEventsAveraged > 0 then bDoAverage.Click ;
     end;


procedure  TEventDetFrm.ZoomIn( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDisplay.YZoom( Chan, -50.0 );
     scEditDisplay.YZoom( Chan, -50.0 );
     scAverageDisplay.YZoom( Chan, -50.0 );
     end ;


procedure  TEventDetFrm.ZoomOut( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDisplay.YZoom( Chan, 50.0 );
     scEditDisplay.YZoom( Chan, 50.0 );
     scAverageDisplay.YZoom( Chan, 50.0 );
     end ;



procedure  TEventDetFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.ZoomOut ;
     scEditDisplay.MaxADCValue := scDisplay.MaxADCValue ;
     scEditDisplay.MinADCValue := scDisplay.MinADCValue ;
     scEditDisplay.ZoomOut ;
     scAverageDisplay.MaxADCValue := scDisplay.MaxADCValue ;
     scAverageDisplay.MinADCValue := scDisplay.MinADCValue ;
     scAverageDisplay.ZoomOut ;
     end ;


procedure TEventDetFrm.ckEnableBaselineTrackingClick(Sender: TObject);
// -------------------------------------------------------------------
// Enabled/disable baseline tracking in threshold event detection mode
// -------------------------------------------------------------------
begin
     Settings.EventDetector.EnableBaselineTracking := ckEnableBaselineTracking.Checked ;
     DisplayRecord ;
end;


procedure TEventDetFrm.ckSubtractBaselineClick(Sender: TObject);
// --------------------------------------
// Baseline subtraction check box changed
// --------------------------------------
begin

     Settings.EventDetector.SubtractBaseline := ckSubtractBaseline.Checked ;
     SaveCDRHeader( CDRFH ) ;

     EventAnalysisFileUpdateRequired := True ;
     DisplayEvent ;
     end ;

procedure TEventDetFrm.bPlotAbortClick(Sender: TObject);
begin
     AbortFlag := True ;
     end;

procedure TEventDetFrm.bAbortExportClick(Sender: TObject);
// --------------------------
// Terminate export of events
// --------------------------
begin
     AbortFlag := True ;
     end;


procedure TEventDetFrm.edDetDisplayWidthKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------
// Update no. sample scans in display window
// ---------------------------------------
begin
     if key = #13 then begin
        NewFile ;
        DisplayRecord ;
        end ;
     end;


procedure TEventDetFrm.bAverageFitCurveClick(Sender: TObject);
{ ------------------------------------------
  Fit exponential functions to event average
  ------------------------------------------}
var
     iStart,iEnd,TZero,YZero,iLine : Integer ;
     i, j : Integer ;
     X, Y : Single ;
begin


    CVFit.Equation := TCFEqnType(cbAverageEqn.Items.Objects[cbAverageEqn.ItemIndex]) ;
    CVFit.XUnits := Settings.TUnits ;
    CVFit.YUnits := Channel[cbChannel.ItemIndex].ADCUnits ;
    lbAvgFitResults.Caption := '' ;
    scAverageDisplay.CreateLine(cbChannel.ItemIndex,clred,psSolid, 1);

    if CVFit.Equation = None then begin
        scAverageDisplay.Invalidate ;
        Exit ;
        end;

    // Add points to curve fitter
    TZero := Round(scAverageDisplay.VerticalCursors[AverageT0Cursor]) ;
    iStart := Min( Round(scAverageDisplay.VerticalCursors[AverageC0Cursor]),
                   Round(scAverageDisplay.VerticalCursors[AverageC1Cursor]) ) ;
    iEnd := Max( Round(scAverageDisplay.VerticalCursors[AverageC0Cursor]),
                 Round(scAverageDisplay.VerticalCursors[AverageC1Cursor]) ) ;
    YZero := Round(scAverageDisplay.HorizontalCursors[cbChannel.ItemIndex]) ;
    CVFit.ClearPoints ;
    for i := iStart to iEnd do begin
        j := (i*scAverageDisplay.NumChannels)
             + scAverageDisplay.ChanOffsets[cbChannel.ItemIndex] ;
        X := (i-TZero)*scAverageDisplay.TScale ;
        Y := (AvgBuf^[j] - YZero)*Channel[cbChannel.ItemIndex].ADCScale ;
        CVFit.AddPoint( X, Y ) ;
        end ;

    // Let user set fitting parameters
    CVFit.ParametersSet := False ;
    SetFitPars1frm.CVFit := CVFit ;
    SetFitPars1frm.ShowModal ;
    if SetFitPars1frm.ModalResult <> mrOK then Exit ;

    // Fit curve
    CVFit.FitCurve ;

    // Copy results to display
    lbAvgFitResults.Caption := CVFit.FitResults ;

    // Draw best fit line
    scAverageDisplay.ClearLines ;
    iLine := scAverageDisplay.CreateLine(cbChannel.ItemIndex,clred,psSolid, 1);
    for i := TZero to iEnd do begin
        X := (i-TZero)*scAverageDisplay.TScale ;
        scAverageDisplay.AddPointToLine(iLine, i, CVFit.EquationValue(X)/
                                           scAverageDisplay.ChanScale[cbChannel.ItemIndex]
                                           + YZero ) ;
        end ;

     // Ensure display is updated
     scAverageDisplay.Invalidate ;

     end;



procedure TEventDetFrm.edPreTriggerKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------
// Pre-trigger % changed
// ---------------------
begin
     if Key = #13 then begin
        Settings.EventDetector.PreTriggerFraction := edPreTrigger.Value ;
        DisplayEvent ;
        end ;
     end ;

     
procedure TEventDetFrm.edTDecayPercentageKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Decay time percentage changed
// -----------------------------
begin
     if Key = #13 then begin
        Settings.EventDetector.TDecayPercent := edTDecayPercentage.Value ;
        SaveCDRHeader( CDRFH ) ;
        DisplayEvent ;
        end ;
     end;

procedure TEventDetFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     if ComputationInProgress then begin
        AbortFlag := True ;
        ComputationInProgress := False ;
        CanClose := false ;
        end
     else CanClose := True ;
     end;

procedure TEventDetFrm.bAbortAverageClick(Sender: TObject);
// ---------------
// Abort averaging
// ---------------
begin
     AbortFlag := True ;
     end;

procedure TEventDetFrm.scEditDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (scEditDisplay.VerticalCursors[EditC0Cursor] <> OldEditC0CursorPos) or
        (scEditDisplay.VerticalCursors[EditC1Cursor] <> OldEditC1CursorPos) then begin
        DisplayEvent ;
        EventAnalysisFileUpdateRequired := True ;
        end ;
     end;


procedure TEventDetFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin

     scDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDetDisplay.DisplayGrid := Settings.DisplayGrid ;
     scEditDisplay.DisplayGrid := Settings.DisplayGrid ;
     scAverageDisplay.DisplayGrid := Settings.DisplayGrid ;


     end ;


procedure TEventDetFrm.scAverageDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------
// Mouse button released
// ---------------------
begin
     if (scAverageDisplay.VerticalCursors[AverageC0Cursor] <> OldAvgC0CursorPos) or
        (scAverageDisplay.VerticalCursors[AverageC1Cursor] <> OldAvgC1CursorPos) then begin
        AnalyseAverage ;
        end ;
end;


procedure TEventDetFrm.rbTDecayFromPeakClick(Sender: TObject);
// -----------------------------------------
// Decay time relative to peak selected
// -----------------------------------------
begin
     DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;

procedure TEventDetFrm.rbTDecayFromMidRiseClick(Sender: TObject);
// -----------------------------------------
// Decay time relative to mid-point of rise selected
// -----------------------------------------
begin
     DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;

procedure TEventDetFrm.rbTDecayFromC0Click(Sender: TObject);
// -----------------------------------------
// Decay time relative to a0 cursor selected
// -----------------------------------------
begin
     DisplayEvent ;
     EventAnalysisFileUpdateRequired := True ;
     end ;

procedure TEventDetFrm.edZeroNumAvgKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        Settings.EventDetector.NumBaselinePoints := Round(edZeroNumAvg.Value) ;
        SaveCDRHeader( CDRFH ) ;
        DisplayEvent ;
        end ;
     end ;

procedure TEventDetFrm.edZeroGapKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        Settings.EventDetector.NumBaselineGap := Round(edZeroGap.Value) ;
        SaveCDRHeader( CDRFH ) ;
        DisplayEvent ;
        end ;
     end ;


procedure TEventDetFrm.cbDecayFromChange(Sender: TObject);
// -----------------------
// Decay time mode changed
// -----------------------
begin
     Settings.EventDetector.TDecayFrom := cbDecayFrom.ItemIndex ;
     SaveCDRHeader( CDRFH ) ;
     DisplayEvent ;
     end;

procedure TEventDetFrm.cbEventAlignmentChange(Sender: TObject);
// ----------------------------
// Event alignment menu changed
// ----------------------------
begin
     Settings.EventDetector.Alignment := cbEventAlignment.ItemIndex ;
end;


procedure TEventDetFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
begin
     AvgBuf := Nil ;
     DetBuf := Nil ;
     EditBuf := Nil ;
     ADC := Nil ;
     SaveEventListRequested := False ;
     end;

procedure TEventDetFrm.bTDisplayDoubleClick(Sender: TObject);
// -------------------------------------------
// Double size of detect events display window
// -------------------------------------------
begin
        edDetDisplayWidth.Value := edDetDisplayWidth.Value*2.0 ;
        NewFile ;
        DisplayRecord ;
        end;

procedure TEventDetFrm.bTDisplayHalfClick(Sender: TObject);
// -------------------------------------------
// Halve size of detect events display window
// -------------------------------------------
begin
        edDetDisplayWidth.Value := edDetDisplayWidth.Value*0.5 ;
        NewFile ;
        DisplayRecord ;
        end;

procedure TEventDetFrm.bEditDisplayWidthHalveClick(Sender: TObject);
// ---------------------------------
// Halve width of edit event display
// ---------------------------------
begin
    edEditDisplayWidth.Value := edEditDisplayWidth.Value*0.5 ;
    Settings.EventDetector.AnalysisWindow := edEditDisplayWidth.Value ;
    NewFile ;
    DisplayEvent ;
    end ;

procedure TEventDetFrm.bDoubleEditDisplayWidthClick(Sender: TObject);
// ---------------------------------
// Double width of edit event display
// ---------------------------------
begin
    edEditDisplayWidth.Value := edEditDisplayWidth.Value*2.0 ;
    Settings.EventDetector.AnalysisWindow := edEditDisplayWidth.Value ;
    NewFile ;
    DisplayEvent ;
    end ;

procedure TEventDetFrm.edNumBinsKeyPress(Sender: TObject; var Key: Char);
// -------------------
// No. of bins changed
// -------------------
begin
    if Key = #13 then begin
       edNumBins.Value := Round(edNumBins.Value) ;
       edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
       end;
    end;

procedure TEventDetFrm.edBinWidthKeyPress(Sender: TObject; var Key: Char);
// -----------------
// Bin width changed
// -----------------
begin
     if Key = #13 then begin
        if edBinWidth.Value <= 0.0 then begin
           edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
           end;
        edNumBins.Value := Round((edBinsUpper.Value - edBinsLower.Value)/edBinWidth.Value) ;
        end;
    end;

procedure TEventDetFrm.edBinsLowerKeyPress(Sender: TObject; var Key: Char);
// -----------------------------
// Histogram lower limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
        end;
      end;

procedure TEventDetFrm.edBinsUpperKeyPress(Sender: TObject; var Key: Char);
// -----------------------------
// Histogram upper limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
        end;
      end;


procedure TEventDetFrm.cbPlotXVarChange(Sender: TObject);
// -----------------------
// X axis variable changed
// -----------------------
begin
     if (cbPlotXVar.Items.Objects[cbPlotXVar.ItemIndex] = TObject(VFrequencyAvg)) or
        (cbPlotYVar.Items.Objects[cbPlotYVar.ItemIndex] = TObject(VFrequencyAvg)) then begin
        FrequencyAvgPan.Visible := True ;
        EventRangePan.Visible := False ;
        end
     else begin
        FrequencyAvgPan.Visible := False ;
        EventRangePan.Visible := True ;
        end ;

     end;

procedure TEventDetFrm.cbPlotYVarChange(Sender: TObject);
// -----------------------
// Y axis variable changed
// -----------------------
begin
     if (cbPlotXVar.Items.Objects[cbPlotXVar.ItemIndex] = TObject(VFrequencyAvg)) or
        (cbPlotYVar.Items.Objects[cbPlotYVar.ItemIndex] = TObject(VFrequencyAvg)) then begin
        FrequencyAvgPan.Visible := True ;
        EventRangePan.Visible := False ;
        end
     else begin
        FrequencyAvgPan.Visible := False ;
        EventRangePan.Visible := True ;
        end ;

     end;


procedure TEventDetFrm.cbReviewChannelChange(Sender: TObject);
// -----------------------------
// Change channel to be reviewed
// -----------------------------
begin
     Settings.EventDetector.Channel := cbReviewChannel.ItemIndex ;
     NewFile ;
     DisplayEvent ;
     end;


procedure TEventDetFrm.edAverageIntervalKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = #13 then
        Settings.EventDetector.AvgFrequencyInterval := edAverageInterval.Value ;
     end;

end.


