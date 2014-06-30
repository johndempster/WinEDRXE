unit Dweltime;
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
              }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, TabNotBk, printers, ClipBrd,
  global, shared, maths, fileio, Grids, setfitpa,
  ComCtrls, RangeEdit, ValEdit, ScopeDisplay, XYPlotDisplay ;

type


  TDwellTimesFrm = class(TForm)
    Page: TPageControl;
    AmpHistTab: TTabSheet;
    DetectTransitionsTab: TTabSheet;
    EditTransitionsTab: TTabSheet;
    DisplayGrp: TGroupBox;
    edTDisplay: TValidatedEdit;
    spTDisplay: TSpinButton;
    lbVertCursor1: TLabel;
    DwellTHistTab: TTabSheet;
    OpenDialog: TOpenDialog;
    StabPlotTab: TTabSheet;
    StabGrp: TGroupBox;
    bDoStabPlot: TButton;
    bStabPlotSetAxes: TButton;
    GroupBox5: TGroupBox;
    lbStabPlotNumRegions: TLabel;
    rbStabPlotAllEvents: TRadioButton;
    rbStabPlotRange: TRadioButton;
    edStabPlotRange: TRangeEdit;
    edStabPlotNumRegions: TValidatedEdit;
    pbStabPlotProgress: TProgressBar;
    plStabPlot: TXYPlotDisplay;
    lbStabRead: TLabel;
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
    edDwellTRange: TRangeEdit;
    edTCritical: TValidatedEdit;
    plDwellTHist: TXYPlotDisplay;
    DwellTResultsGrp: TGroupBox;
    bDwellTFitCurve: TButton;
    cbDwellTEqn: TComboBox;
    erDwellTResults: TRichEdit;
    lbDwellTArea: TLabel;
    lbDwellTRead: TLabel;
    lbDwellTC0: TLabel;
    lbDwellTC1: TLabel;
    shLine: TShape;
    EditGrp: TGroupBox;
    sbEvent: TScrollBar;
    edEvent: TRangeEdit;
    meResults: TMemo;
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
    edRange: TRangeEdit;
    CriteriaGrp: TGroupBox;
    Label2: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    cbDetChannel: TComboBox;
    edUnitCurrent: TValidatedEdit;
    edThreshold: TValidatedEdit;
    GroupBox1: TGroupBox;
    bAddBlock: TButton;
    cbTrendEquation: TComboBox;
    bClearSamples: TButton;
    bRemoveTrend: TButton;
    edNumBlocks: TValidatedEdit;
    bRestoreOriginal: TButton;
    edStatus: TEdit;
    pbDetection: TProgressBar;
    scDetDisplay: TScopeDisplay;
    sbDetDisplay: TScrollBar;
    shTranDetLine: TShape;
    lbTranDetC0: TLabel;
    lbTranDetBase: TLabel;
    lbTranDetThreshold: TLabel;
    lbTranDetIUnit: TLabel;
    AmpHistGrp: TGroupBox;
    Label1: TLabel;
    bNewAmpHist: TButton;
    GroupBox6: TGroupBox;
    Label5: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    cbAmpHistChannel: TComboBox;
    edAmpHistRange: TRangeEdit;
    AmpHistTypePage: TNotebook;
    Label9: TLabel;
    Label11: TLabel;
    cbChannelState: TComboBox;
    edMarginPoints: TValidatedEdit;
    Label12: TLabel;
    Label13: TLabel;
    edPatlakSDLimit: TValidatedEdit;
    edNumPatlakAvg: TValidatedEdit;
    bAmpHistSetAxes: TButton;
    cbAmpHistType: TComboBox;
    GroupBox7: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    edAmpHistNumBins: TValidatedEdit;
    edRangeLo: TValidatedEdit;
    edRangeHi: TValidatedEdit;
    bSetZero: TButton;
    bSetUnitCurrent: TButton;
    pbAmpHist: TProgressBar;
    bAbortAmpHist: TButton;
    plAmpHist: TXYPlotDisplay;
    AmpHistResultsGrp: TGroupBox;
    bAmpFitCurve: TButton;
    cbAmpHistEqn: TComboBox;
    erAmpResults: TRichEdit;
    lbAmpHistRead: TLabel;
    lbAmpHistC0: TLabel;
    lbAmpHistC1: TLabel;
    lbUnitArrow: TImage;
    shAmpHistLine: TShape;
    lbAmpHistArea: TLabel;
    lbTranDetC1: TLabel;
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
    procedure FormShow(Sender: TObject);
    procedure spTDisplayUpClick(Sender: TObject);
    procedure spTDisplayDownClick(Sender: TObject);
    procedure sbDetDisplayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edUnitCurrentKeyPress(Sender: TObject; var Key: Char);
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
    procedure plStabPlotCursorChange(Sender: TObject);
    procedure edEventKeyPress(Sender: TObject; var Key: Char);
    procedure bNewAmpHistClick(Sender: TObject);
    procedure plAmpHistCursorChange(Sender: TObject);
    procedure bSetZeroClick(Sender: TObject);
    procedure bSetUnitCurrentClick(Sender: TObject);
    procedure bAmpFitCurveClick(Sender: TObject);
    procedure bAmpHistSetAxesClick(Sender: TObject);
    procedure AmpHistTabEnter(Sender: TObject);
    procedure bAbortAmpHistClick(Sender: TObject);
    procedure cbAmpHistTypeChange(Sender: TObject);
    procedure bExportEventListClick(Sender: TObject);
    procedure bAbortStabPlotClick(Sender: TObject);
    procedure bFTestClick(Sender: TObject);

  private
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
    procedure AdjustAmpHistZeroLevel( NewADCZero : Integer );


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
    procedure ExternalDwellTimeHistogram ;
    procedure UpdateHistogram( Time : Double ; Value : single ) ;

    procedure SetCopyAndPrintMenus ;


    // Stability plot methods
    procedure InitialiseStabPlot ;
    procedure DwellTimeStabPlot(
              StartAt : Integer ;
              EndAt : Integer ;
              NumRegions : Integer ;
              RequiredState : Integer
              ) ;
    procedure MeanCurrentStabPlot(
              StartAt : Integer ;
              EndAt : Integer ;
              NumRegions : Integer ;
              DivideFactor : single
              ) ;
   procedure StateAverageVsOpenTime(
             StartAt : Integer ;
             EndAt : Integer
             ) ;

    procedure NewStabPlotType ;

  public
    { Public declarations }
    procedure CopyDataToClipBoard ;
    procedure PrintDisplay ;
    procedure CopyImageToClipboard ;
    procedure ZoomOut ;
    procedure ZoomIn( Chan : Integer ) ;
    procedure ChangeDisplayGrid ;
  end;

var
  DwellTimesFrm: TDwellTimesFrm;



implementation

uses mdiform, setaxes, printgra, SetIgnor, Printrec, ftest ;

const
     MinRecordSize = 32 ;
     MaxRecordSize = 8192 ;

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
                      htPatlakAverage,htAmplitudesFile) ;

    TDwellTHistType = (htOpenTimes,
                      htClosedTimes,
                      htBurstDurations,
                      htBurstOpenTimes,
                      htSingleOpenTimes,
                      htOpeningsPerBurst,
                      htExternalFile ) ;

    TStabPlotType = ( stMeanCurrent,
                      stOpenProb,
                      stClosedTimes,
                      stOpenTimes,
                      stAvgVsOpenTimes ) ;

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
             C1 : Integer ;
             Read : Integer ;
             IUnit : Integer ;
             Base : Integer ;
             Threshold : Integer ;
             end ;

var
   ADC : ^TSmallIntArray ;           { A/D sample data array }

   ChanNum : Integer ;       // Input channel being analysed

   AmpHist : THistogram ;    // Amplitude histogram
   AmpFunc :TMathFunc ;      // Amplitude histogram fitting equation
   AmpResults : TStringList ; // Amplitude histogram fitting results
   AmpCurs : TCursors ;       // Histogram cursors
   AmpHistADCZero : Integer ;

   // Detection page
   DetCurs : TCursors ;

   DwellTHist : THistogram ; // Dwell-time histogram data record
   DwellTFunc :TMathFunc ;  // Dwell-time histogram fitting equation
   DwellTResults : TStringList ; // Dwell-time histogram fitting results
   DwellTCurs : TCursors ;       // Histogram cursors

   BuffersAllocated : Boolean ;
   Det : TDetector ;

   StateNames : Array[0..10] of string ;
   TrendLine : TTrendLine ;
   StabCurs : TCursors ;       // Histogram cursors


{$R *.DFM}


procedure TDwellTimesFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
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
                Dispose(ADC) ;
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


procedure TDwellTimesFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
begin

     { Allocate working buffers from heap }
     HeapBuffers( Allocate ) ;

     { Open event file }
     if not EventFile.Open then OpenEventFile( EventFile ) ;

     // Initialise amplitude histogram methods
     InitialiseAmpHist ;

     // Initialise dwell time histogram methods
     InitialiseDwellTHist ;

     // Initialise transition detection page
     InitialiseDetect ;

     // Initialise stability plot detection page
     InitialiseStabPlot ;

     { Make sure no more forms can be created }
     Main.mnDwellTimes.Enabled := False ;

     { Signal display window duration }
     edTDisplay.Scale := Settings.TScale ;
     edTDisplay.Units := Settings.TUnits ;
     edTDisplay.Value := Settings.DwellTimes.RecordSize*CdrFH.dt ;


     Resize ;

     InitialiseDisplays ;
     DisplayRecord ;

     Page.ActivePage := AmpHistTab ;

     end;


procedure TDwellTimesFrm.InitialiseDisplays ;
{ ------------------------------------------------
  Initialise display to selected detection channel
  ------------------------------------------------}
var
   ch : Integer ;
begin
     { Continuous record display channel }
     scDetDisplay.MaxADCValue := MaxADCValue ;
     scDetDisplay.MinADCValue := MinADCValue ;
     scDetDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDetDisplay.MaxPoints := Settings.DwellTimes.RecordSize ;
     scDetDisplay.NumPoints := scDetDisplay.MaxPoints ;
     scDetDisplay.NumChannels := CdrFH.NumChannels ;
     scDetDisplay.xMin := 0 ;
     scDetDisplay.xMax := Settings.DwellTimes.RecordSize  ;

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
         if ch = Settings.DwellTimes.ChanNum then scDetDisplay.ChanVisible[ch] := True
                                             else scDetDisplay.ChanVisible[ch] := False ;
         end ;
     scDetDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDetDisplay.TFormat := ' %.6g ' + Settings.TUnits + ' ' ;

     { Detected event display }
     scEditDisplay.MaxADCValue := MaxADCValue ;
     scEditDisplay.MinADCValue := MinADCValue ;
     scEditDisplay.DisplayGrid := Settings.DisplayGrid ;
     scEditDisplay.MaxPoints := Settings.DwellTimes.RecordSize ;
     scEditDisplay.NumPoints := scEditDisplay.MaxPoints ;
     scEditDisplay.NumChannels := CdrFH.NumChannels ;
     scEditDisplay.xMin := 0 ;
     scEditDisplay.xMax := Settings.DwellTimes.RecordSize  ;


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
         if ch = Settings.DwellTimes.ChanNum then scEditDisplay.ChanVisible[ch] := True
                                             else scEditDisplay.ChanVisible[ch] := False ;
         end ;
     scEditDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scEditDisplay.TFormat := ' %.6g ' + Settings.TUnits + ' ' ;

     end ;


procedure TDwellTimesFrm.DisplayRecord ;
{ ---------------------------------------------
  Display currently selected block of data file
  ---------------------------------------------}
var
   i : Integer ;
   x,xStep : single ;
begin

   sbDetDisplay.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)
                       - Settings.DwellTimes.RecordSize ;

   scDetDisplay.xOffset := sbDetDisplay.Position ;

   if ReadCDRBuffer(CdrFH,sbDetDisplay.Position,ADC^,Settings.DwellTimes.RecordSize)
      = Settings.DwellTimes.RecordSize then begin

      scDetDisplay.HorizontalCursors[DetCurs.Base] := Channel[ChanNum].ADCZero ;
      Det.UnitLevel := Round( Settings.UnitCurrent / Channel[ChanNum].ADCScale ) ;
      scDetDisplay.HorizontalCursors[DetCurs.IUnit] := Channel[ChanNum].ADCZero
                                                      + Round(edUnitCurrent.Value/
                                                              Channel[ChanNum].ADCScale) ;
      scDetDisplay.HorizontalCursors[DetCurs.Threshold] := Channel[ChanNum].ADCZero
                                                 + Round((edUnitCurrent.Value
                                                         *edThreshold.Value)/
                                                         Channel[ChanNum].ADCScale) ;
      scDetDisplay.SetDataBuf( ADC^ ) ;
      end ;

   { Display trend line, if one is available }
   scDetDisplay.CreateLine( ChanNum, clRed, psSolid ) ;
   if TrendLine.Available then begin
      x := 0.0 ;
      xStep := scDetDisplay.NumPoints / 100.0 ;
      for i := 0 to 99 do begin
          scDetDisplay.AddPointToLine( x, TrendLine.Func.Value(x+scDetDisplay.xOffset) ) ;
          x := x + xStep ;
          end ;
      end ;

   end;


procedure TDwellTimesFrm.UpdateCursors(
          scDisplay : TScopeDisplay
          ) ;
{ --------------------------------------------
  Move horizontal cursors on signal display
  --------------------------------------------}
begin
     scDisplay.HorizontalCursors[DetCurs.Base] := Channel[ChanNum].ADCZero ;
     scDisplay.HorizontalCursors[DetCurs.IUnit] := Channel[ChanNum].ADCZero
                                                   + Round(edUnitCurrent.Value/
                                                           Channel[ChanNum].ADCScale) ;
     scDisplay.HorizontalCursors[DetCurs.Threshold] := Channel[ChanNum].ADCZero
                                                 + Round((edUnitCurrent.Value
                                                         *edThreshold.Value)/
                                                         Channel[ChanNum].ADCScale) ;
     end ;


procedure TDwellTimesFrm.spTDisplayUpClick(Sender: TObject);
{ --------------------------------
  Increase size of display windows
  --------------------------------}
begin
     Settings.DwellTimes.RecordSize := MinInt(
                                       [Settings.DwellTimes.RecordSize*2,
                                        MaxRecordSize]) ;
     edTDisplay.Value := Settings.DwellTimes.RecordSize*CdrFH.dt ;
     InitialiseDisplays ;
     { Update other parameters that may depend on this }
     { Request a re-plot of signal }
     if (Page.ActivePage = DetectTransitionsTab) then DisplayRecord
                                                 else DisplayEvent ;

     end;


procedure TDwellTimesFrm.spTDisplayDownClick(Sender: TObject);
{ ------------------------------
  Reduce size of display windows
  ------------------------------}
begin
     { Decrease number of points displayed }
     Settings.DwellTimes.RecordSize := MaxInt(
                                       [Settings.DwellTimes.RecordSize div 2,
                                        MinRecordSize]) ;
     edTDisplay.Value := Settings.DwellTimes.RecordSize*CdrFH.dt ;
     InitialiseDisplays ;
     { Re-plot the signal }
     if (Page.ActivePage = DetectTransitionsTab) then DisplayRecord
                                                 else DisplayEvent ;

     end;


procedure TDwellTimesFrm.sbDetDisplayChange(Sender: TObject);
{ --------------------------------------
  New display window scroll bar position
  --------------------------------------}
begin
     if bDetect.Enabled then DisplayRecord ;
     end;


procedure TDwellTimesFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     Settings.UnitCurrent := edUnitCurrent.Value  ;
     Settings.DwellTimes.Threshold := edThreshold.Value ;

     SaveCDRHeader( cdrFH ) ;

     { Close event file }
     if EventFile.Open then CloseEventFile( EventFile ) ;

     HeapBuffers( Deallocate ) ;

     Main.mnDwellTimes.Enabled := True ;
     Main.CopyAndPrintMenus( False, False ) ;

     Screen.Cursor := crDefault ;

     Action := caFree ;
     end;


procedure TDwellTimesFrm.edUnitCurrentKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then begin
        UpdateCursors( scDetDisplay ) ;
        end ;
     end;

procedure TDwellTimesFrm.InitialiseDetect ;
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
     edUnitCurrent.Units := Channel[ChanNum].ADCUnits ;
     edUnitCurrent.Value := Settings.UnitCurrent ;
     Det.UnitLevel := Round( Settings.UnitCurrent / Channel[ChanNum].ADCScale ) ;

     { Transition detection threshold (% of unit current) }
     edThreshold.Value := Settings.DwellTimes.Threshold ;
     Det.Threshold := Round( Det.UnitLevel*edThreshold.Value ) ;

    { Set block of CDR file to be scanned }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiLimit := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)*cdrFH.dt ;
     edRange.HiValue := edRange.HiLimit ;

     { Create display cursors }
     DetCurs.Base :=  scDetDisplay.AddHorizontalCursor( 0, clgray, True ) ;
     DetCurs.Threshold := scDetDisplay.AddHorizontalCursor( 0, clgray, False ) ;
     DetCurs.IUnit := scDetDisplay.AddHorizontalCursor( 0, clgray, False ) ;
     DetCurs.C0 := scDetDisplay.AddVerticalCursor( AllChannels, clgray ) ;
     DetCurs.C1 := scDetDisplay.AddVerticalCursor( AllChannels, clgray ) ;

     scDetDisplay.VerticalCursors[DetCurs.C0] := 0 ;
     scDetDisplay.VerticalCursors[DetCurs.C0] := 100 ;
     DetCurs.Base :=  scEditDisplay.AddHorizontalCursor( 0, clgray, True ) ;
     DetCurs.Threshold := scEditDisplay.AddHorizontalCursor( 0, clgray, False ) ;
     DetCurs.IUnit := scEditDisplay.AddHorizontalCursor( 0, clgray, False ) ;

     StateNames[0] := 'Closed' ;
     StateNames[1] := 'Open' ;
     for i := 2 to High(StateNames) do
         StateNames[i] := format('Open(%d)', [i] ) ;

     { Scroll bar control }
     sbDetDisplay.Max := (CdrFH.NumSamplesInFile div
                         (CdrFH.NumChannels*Settings.DwellTimes.RecordSize)) - 1 ;
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


procedure TDwellTimesFrm.bDetectClick(Sender: TObject);
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

     Det.UnitLevel := Round(edUnitCurrent.Value/Channel[ChanNum].ADCScale) ;
     Det.Threshold := Round(Det.UnitLevel*edThreshold.Value) ;

     { Range of samples to be scanned for single-channel transitions }
     if rbAllRecords.Checked then begin
        Det.StartAtSample := 0 ;
        Det.EndAtSample := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        Det.StartAtSample := Round( edRange.LoValue / CdrFH.dt ) ;
        Det.EndAtSample :=   Round( edRange.HiValue / CdrFH.dt ) ;
        end ;

     { Initialise progress bar }
     pbDetection.Min := 1 ;
     pbDetection.Max := High(Det.EndAtSample) ;
     pbDetection.Position := Det.StartAtSample ;
     pbDetection.Min := Det.StartAtSample ;
     pbDetection.Max := Det.EndAtSample ;
     { Move data file pointer to start of data to be plotted }
     Det.SampleNum := Det.StartAtSample ;

     { Move event pointer to end of event file list }
     Det.EventNum := EventFile.NumEvents + 1 ;
     Det.Sum := 0.0 ;
     Det.SumSquares := 0.0 ;
     Det.NumSamples := 0 ;
     { Initial settings of while loop control flags }
     NewBufferNeeded := True ;
     FirstSample := True ;
     FirstEvent := True ;
     Done := False ;
     while not Done do begin
         Application.ProcessMessages ;
         { Load another buffer of A/D samples from file when needed }
         if NewBufferNeeded then begin
            scDetDisplay.xOffset := Det.SampleNum ;
            if ReadCDRBuffer(CdrFH,Det.SampleNum,ADC^,Settings.DwellTimes.RecordSize)
               = Settings.DwellTimes.RecordSize then begin
               UpdateCursors( scDetDisplay ) ;
               scDetDisplay.SetDataBuf( ADC^ ) ;
               end ;

            ADCPointer := 0 ;
            { Initialise idealised trace line }
            scDetDisplay.CreateLine( ChanNum, clRed, psSolid ) ;
           { Application.ProcessMessages ;}

            if not bAbortDetection.Enabled then Done := True ;

            { Indicate progress of detection }
            pbDetection.Position := Det.SampleNum ;

            NewBufferNeeded := False ;
            end ;

         { Subtract baseline }
         j := (ADCPointer*CdrFH.NumChannels) + Channel[ChanNum].ChannelOffset ;
         Det.YOld := Det.Y ;
         Det.Y := ADC^[j] - Channel[ChanNum].ADCZero ;

         { Invert if negative single-channel currents }

        { if Det.UnitLevel < 0 then Det.Y := Abs(Det.Y) ;}

         { Determine number of open channels }
         Det.ChannelState := 0 ;
         if Det.UnitLevel >= 0 then begin
            While Det.Y >= (Det.Threshold + Det.UnitLevel*Det.ChannelState) do
                  Inc(Det.ChannelState) ;
            end
         else begin
            While Det.Y <= (Det.Threshold + Det.UnitLevel*Det.ChannelState) do
                  Inc(Det.ChannelState) ;
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
               Event.Next := Det.EventNum + 1 ;
               Event.Previous := Det.EventNum - 1 ;

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
               Event.Next := Det.EventNum + 1 ;
               Event.Previous := Det.EventNum - 1 ;
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
         if ADCPointer >= Settings.DwellTimes.RecordSize then NewBufferNeeded := True ;

         { Increment main sample counter - and check for end of region }
         Inc(Det.SampleNum) ;
         if (Det.SampleNum > Det.EndAtSample)
            or (not bAbortDetection.Enabled) then Done := True ;

         end ;

     { Restore controls to idle state }
     bAbortDetection.Enabled := False ;
     sbDetDisplay.Enabled := True ;
     bDetect.Enabled := True ;
     scDetDisplay.CreateLine( ChanNum, clRed, psSolid ) ;

     { Clear status/progress bar }
     edStatus.Text := '' ;
     pbDetection.Position := 1 ;

     end;



procedure TDwellTimesFrm.FormResize(Sender: TObject);
var
   AtBottom : Integer ;
begin
     { Set size of notebook control }
     Page.Height := ClientHeight - Page.Top - 5 ;
     Page.Width := ClientWidth  - Page.Left - 5 ;

     { **** SINGLE-CHANNEL DETECTION PAGE **** }

     { Set height of detection control buttons/edit boxes group }

     AtBottom := DetectTransitionsTab.Height - 5 ;
     DetectGrp.Height := AtBottom - DetectGrp.Top  ;


     { Put display duration box at bottom of window }
     DisplayGrp.Top := AtBottom - DisplayGrp.Height + DetectTransitionsTab.Top ;

    { Put detection display scroll bar above labels }
     sbDetDisplay.Top := DisplayGrp.Top
                         - DisplayGrp.Height  - sbDetDisplay.Height
                         - Page.Top ;

     { Events detected status box }
     pbDetection.Top := DetectGrp.Height -  pbDetection.Height - 5 ;
     bAbortDetection.Top := pbDetection.Top ;
     edStatus.Top := pbDetection.Top - edStatus.Height - 2 ;

     { Set size of detection display area }
     scDetDisplay.Height := sbDetDisplay.Top - scDetDisplay.Top ;
     scDetDisplay.Width := Page.Width - scDetDisplay.Left - 20 ;

     { Make width of detection scroll bar match display area }
     sbDetDisplay.Width := scDetDisplay.Width ;
     sbDetDisplay.Left := scDetDisplay.Left ;

     lbTranDetC0.Top := sbDetDisplay.Top + sbDetDisplay.Height + 2 ;
     lbTranDetC1.Top := lbTranDetC0.Top ;

     { Place display duration controls at right of display area }
     DisplayGrp.Left := scDetDisplay.Left + scDetDisplay.Width
                        - DisplayGrp.Width ;

     { **** DETECTED EVENT EDITING PAGE **** }

     { Set height of event editing control buttons/edit boxes group }

     scEditDisplay.Height := EditTransitionsTab.Height - scEditDisplay.Top
                             - DisplayGrp.Height - 10 ;
     scEditDisplay.Width := EditTransitionsTab.Width - scEditDisplay.Left - 5 ;
     EditGrp.Height := EditTransitionsTab.Height - EditGrp.Top - 5 ;

     { **** DWELL-TIME HISTOGRAM PAGE **** }

     { Set width of histogram display area }
     plDwellTHist.Width :=  DwellTHistTab.Width - plDwellTHist.Left - 5 ;
     { Height of controls group }
     DwellTHistGrp.Height := DwellTHistTab.Height - DwellTHistGrp.Top - 10 ;
     { Size of results group }
     DwellTResultsGrp.Top := DwellTHistGrp.Top + DwellTHistGrp.Height
                             - DwellTResultsGrp.Height ;
     DwellTResultsGrp.Width := plDwellTHist.Width ;
     erDwellTResults.Width := DwellTResultsGrp.Width - erDwellTResults.Left - 10 ;

     { Display cursor labels }
     lbDwellTRead.Top := DwellTResultsGrp.Top - lbDwellTRead.Height - 2 ;
     lbDwellTC0.Top := lbDwellTRead.Top - lbDwellTC0.Height - 2 ;
     lbDwellTC1.Top := lbDwellTC0.Top ;
     { Height of histogram display area }
     plDwellTHist.Height := lbDwellTC1.Top - plDwellTHist.Top - 2 ;
     DwellTResultsGrp.Width := plDwellTHist.Width ;
     erDwellTResults.Width := DwellTResultsGrp.Width - erDwellTResults.Left - 10 ;


     { *** Stability plot page *** }

     { Height of control buttons group }
     StabGrp.Height := StabPlotTab.Height - StabGrp.Top - 10 ;
     pbStabPlotProgress.Top := StabGrp.Height - pbStabPlotProgress.Height - 5 ;
     bAbortStabPlot.Top := pbStabPlotProgress.Top ;

     { Size of plotting area }

     plStabPlot.Width := StabPlotTab.Width - plStabPlot.Left - 5 ;
     lbStabRead.Top := StabPlotTab.Height - lbStabRead.Height - 5 ;
     plStabPlot.Height := lbStabRead.Top - plStabPlot.Top - 2 ;

     // *** Amplitude histogram page ***

     AmpHistGrp.Height := AmpHistTab.Height - AmpHistGrp.Top - 10 ;
     pbAmpHist.Top := AmpHistGrp.Height - pbAmpHist.Height - 10 ;
     bAbortAmpHist.Top := pbAmpHist.Top ;
     AmpHistResultsGrp.Top := AmpHistTab.Height - AmpHistResultsGrp.Height - 10 ;
     AmpHistResultsGrp.Width := AmpHistTab.Width - AmpHistResultsGrp.Left - 5 ;
     erAmpResults.Width := AmpHistResultsGrp.Width - erAmpResults.Left - 10 ;

     plAmpHist.Height := AmpHistResultsGrp.Top
                         - lbAmpHistC0.Height - lbAmpHistRead.Height - 20 ;
     plAmpHist.Width := AmpHistResultsGrp.Width ;

     lbAmpHistC0.Top := plAmpHist.Top + plAmpHist.Height + 2;
     shAmpHistLine.Top := lbAmpHistC0.Top + lbAmpHistC0.Height div 2 ;
     lbAmpHistC0.Visible := False ;
     lbAmpHistC1.Top := lbAmpHistC0.Top ;
     lbAmpHistC1.Visible := False ;
     lbAmpHistRead.Top := lbAmpHistC1.Top + lbAmpHistC1.Height + 2 ;
     lbAmpHistRead.Visible := False ;
     lbAmpHistArea.Visible := False ;
     shAmpHistLine.Visible := False ;

     end;


procedure TDwellTimesFrm.DisplayEvent ;
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
        ReadEventFromFile( EventFile, Event.Previous, PreviousEvent ) ;
        ReadEventFromFile( EventFile, Event.Next, NextEvent ) ;

        PreEventPoints := Settings.DwellTimes.RecordSize div 4 ;

        iStart := MaxInt([ Event.StartAt - PreEventPoints, 0] ) ;
        { Plot data }
        if ReadCDRBuffer(CdrFH,iStart,ADC^,Settings.DwellTimes.RecordSize)
           = Settings.DwellTimes.RecordSize then begin
           UpdateCursors( scEditDisplay ) ;
           scEditDisplay.SetDataBuf( ADC^ ) ;
           scEditDisplay.xOffset := iStart ;
           end ;

        { Plot idealised channel time course }
        UnitLevel := Round( edUnitCurrent.Value / Channel[ChanNum].ADCScale ) ;
        scEditDisplay.CreateLine( ChanNum, clRed, psSolid ) ;
        if PreviousEvent.Available then begin
           x := (Event.StartAt - iStart) - (Settings.DwellTimes.RecordSize*0.05) ;
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
                + (Settings.DwellTimes.RecordSize*0.05) ;
           scEditDisplay.AddPointToLine( x, y ) ;
           end ;

        { State to be ignored in analysis check box }
        ckIgnoreState.Checked := Event.Ignore ;

        { Display event results }
        meResults.Clear ;
        meResults.Lines.Add(format(' State= %s ',[StateNames[Event.ChannelState]])) ;
        meResults.Lines.Add(format(' Duration= %.3g %s',
                            [Event.Duration*Settings.TScale,Settings.TUnits])) ;
        meResults.Lines.Add(format(' Avg.= %.3g %s',
                            [Event.Average,Channel[ChanNum].ADCUnits])) ;
        meResults.Lines.Add(format(' St. Dev.= %.3g %s',
                            [SQRT(Event.Variance),Channel[ChanNum].ADCUnits])) ;

        end
    else begin
         edEvent.LoValue := 0 ;
         edEvent.HiValue := 0 ;
         end ;
    end;


procedure TDwellTimesFrm.sbEventChange(Sender: TObject);
begin
     { Force display of new event }
     DisplayEvent ;
     end;


procedure TDwellTimesFrm.bAbortDetectionClick(Sender: TObject);
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


procedure TDwellTimesFrm.InitialiseDwellTHist ;
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
     cbDwellTHistType.Items.AddObject( 'External File', TObject(htExternalFile) ) ;
     cbDwellTHistType.ItemIndex := 0 ;

     // Upper limit of dwell time histogram
     edHistRange.Scale := Settings.TScale ;
     edHistRange.Units := Settings.TUnits ;
     edHistRange.LoLimit := 0.0 ;
     edHistRange.LoValue := 0.0 ;
     edHistRange.HiValue := CdrFH.dt*1000.0 ;

     edTCritical.Scale := Settings.TScale ;
     edTCritical.Units := Settings.TUnits ;
     edTCritical.Value := 0.0 ;

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
     DwellTCurs.C0 := plDwellTHist.AddVerticalCursor( clGray ) ;
     DwellTCurs.C0 := plDwellTHist.AddVerticalCursor( clGray ) ;
     DwellTCurs.Read := plDwellTHist.AddVerticalCursor( clRed ) ;

     lbDwellTArea.visible := false ;
     shDwellTLine.Visible := false ;
     lbDwellTC0.visible := false ;
     lbDwellTC1.visible := false ;
     lbDwellTRead.visible := false ;

     DwellTHist.Available := False ;
     bDwellTSetAxes.Enabled := False ;
     bDwellTFitCurve.Enabled := False ;

     end;


procedure TDwellTimesFrm.bNewDwellTHistClick(Sender: TObject);
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
        DwellTHist.StartAt := Round( edDwellTRange.LoLimit ) ;
        DwellTHist.EndAt := Round( edDwellTRange.HiLimit ) ;
        end
     else begin
        DwellTHist.StartAt := Round( edDwellTRange.LoValue ) ;
        DwellTHist.EndAt := Round( edDwellTRange.HiValue ) ;
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
        MessageDlg( 'Histogram bin range must be > 0 ',mtWarning, [mbOK], 0 ) ;
        end ;

    { Initialise histogram record }

    if rbLinear.Checked then begin
       { Linear histogram - fixed bin width }
       x := DwellTHist.RangeLo ;
       DwellTHist.MaxBin := DwellTHist.NumBins - 1 ;
       DwellTHist.BinWidth := (DwellTHist.RangeHi - DwellTHist.RangeLo) /
                              MaxInt([DwellTHist.NumBins,1]) ;
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
          htExternalFile : ExternalDwellTimeHistogram ;
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

        { Plot new histogram }
     plDwellTHist.xAxisAutoRange := False ;
     plDwellTHist.xAxisMin := DwellTHist.Bins[0].Lo*Scale ;
     plDwellTHist.xAxisMax := DwellTHist.Bins[DwellTHist.MaxBin].Hi*Scale ;
     plDwellTHist.XAxisTick := (plDwellTHist.xAxisMax - plDwellTHist.xAxisMin) / 5.0 ;

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



procedure TDwellTimesFrm.DwellTimeHistogram(
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


procedure TDwellTimesFrm.BurstDurationHistogram ;
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


procedure TDwellTimesFrm.OpeningsPerBurstHistogram ;
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



procedure TDwellTimesFrm.OpenTimesWithinBurstHistogram ;
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


procedure TDwellTimesFrm.SingleOpenTimesHistogram ;
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
    Done := True ;

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


procedure TDwellTimesFrm.ExternalDwellTimeHistogram ;
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


procedure TDwellTimesFrm.UpdateHistogram(
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


procedure TDwellTimesFrm.bDwellTSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plDwellTHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TDwellTimesFrm.bDwellTFitCurveClick(Sender: TObject);
{ ----------------------------------------------------
  Fit exponential prob. density functions to histogram
  ----------------------------------------------------
    }
const
     NumFitPoints = 500 ;
var
   i,iStart,iEnd,nFit,iBins,Comp,LineNum,NumComp : Integer ;
   x,y,z,BinWidth,Chi2 : single ;
   ParTemp,DisplayPars,DisplayParsSD : Array[0..LastParameter] of single ;
   ParUnits : Array[0..LastParameter] of String ;
   FitData : ^TXYData ;
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
        if OK then begin

           nFit := 0 ;
           { Lower and upper x data limit set by display cursors }
           iStart := plDwellTHist.FindNearestIndex( 0, DwellTCurs.C0 ) ;
           iEnd :=   plDwellTHist.FindNearestIndex( 0, DwellTCurs.C1 ) ;
           for iBins := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
               FitData^.x[nFit] := DwellTHist.Bins[iBins].Mid ;
               FitData^.BinWidth[nFit] := (DwellTHist.Bins[iBins].Hi
                                           - DwellTHist.Bins[iBins].Lo) ;
               FitData^.y[nFit] := DwellTHist.Bins[iBins].y
                                   / (DwellTHist.TotalCount*0.01) ;
               Inc(nFit) ;
               end ;

           { Abort curve fit, if not enough data points }
           if nFit < DwellTFunc.NumParameters then begin
              MessageDlg( format('%d points is insufficient for fit',[nFit]),
                           mtWarning, [mbOK], 0 ) ;
              DwellTFunc.Setup( None, ' ',' ' ) ;
              OK := False ;
              end ;
           end ;

        { Create an initial set of guesses for parameters }
        if OK then begin
           for i := 0 to DwellTFunc.NumParameters-1 do
               if not DwellTFunc.FixedParameters[i] then begin
               DwellTFunc.Parameters[i] := DwellTFunc.InitialGuess(FitData^,nFit,i) ;
               end ;

           { Let user modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := DwellTFunc ;
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
           if not OK then MessageDlg( 'Curve fit failed to converge!',mtWarning, [mbOK], 0 ) ;
           end ;

        { Plot equation on graph }

        plDwellTHist.CreateLine( FittedLine, clRed, msNone, psSolid ) ;
        if OK and (DwellTFunc.Equation <> None) then begin
           plDwellTHist.ShowLines := True ;
           for i := 0 to DwellTHist.NumBins-1 do begin
               BinWidth := (DwellTHist.Bins[i].Hi - DwellTHist.Bins[i].Lo)
                            * DwellTHist.TotalCount*0.01 ;
               x := DwellTHist.Bins[i].Mid*Settings.TScale ;
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
                      x := DwellTHist.Bins[i].Mid*Settings.TScale ;
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
                  DisplayPars[i] := DwellTFunc.Parameters[i]*Settings.TScale ;
                  DisplayParsSD[i] := DwellTFunc.ParameterSDs[i]*Settings.TScale ;
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


procedure TDwellTimesFrm.PrintDisplay ;
{ ------------------------------------
  Print record or histogram on display
  ------------------------------------ }
var
   i : Integer ;
begin

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
          for i := 0 to erDwellTResults.Lines.Count-1 do
              plStabPlot.AddPrinterTitleLine( erDwellTResults.Lines[i] ) ;
          { Plot graph to printer }
          plStabPlot.Print ;
          end ;
       end

    else if Page.ActivePage = EditTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := dePrinter ;
       PrintRecFrm.DisplayObj := scEditDisplay ;

       PrintRecFrm.ShowModal ;

       if PrintRecFrm.ModalResult = mrOK then begin
          TScopeDisplay(PrintRecFrm.DisplayObj).ClearPrinterTitle ;
          TScopeDisplay(PrintRecFrm.DisplayObj).AddPrinterTitleLine(
                                                'File : ' + cdrFH.FileName ) ;
          TScopeDisplay(PrintRecFrm.DisplayObj).AddPrinterTitleLine( CdrFH.IdentLine ) ;
          TScopeDisplay(PrintRecFrm.DisplayObj).Print ;
          end ;
       end ;

    end ;


procedure TDwellTimesFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy histogram image to clipboard as Windows metafile
  ----------------------------------------------------- }
begin

    if Page.ActivePage = DwellTHistTab then begin
       PrintGraphFrm.Plot := plDwellTHist ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plDwellTHist.CopyImageToClipboard ;
       end ;

    if Page.ActivePage = StabPlotTab then begin
       PrintGraphFrm.Plot := plStabPlot ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plStabPlot.CopyImageToClipboard ;
       end ;

    if Page.ActivePage = EditTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := deClipboard ;
       PrintRecFrm.DisplayObj := scEditDisplay ;
       PrintRecFrm.ShowModal ;
       if PrintRecFrm.ModalResult = mrOK then begin
          TScopeDisplay(PrintRecFrm.DisplayObj).CopyImageToClipboard ;
          end ;
       end ;

    if Page.ActivePage = DetectTransitionsTab then begin
       { Print transition }
       PrintRecFrm.Destination := deClipboard ;
       PrintRecFrm.DisplayObj := scDetDisplay ;
       PrintRecFrm.ShowModal ;
       if PrintRecFrm.ModalResult = mrOK then begin
          TScopeDisplay(PrintRecFrm.DisplayObj).CopyImageToClipboard ;
          end ;
       end ;


     end ;


procedure TDwellTimesFrm.CopyDataToClipboard ;
{ -----------------------------------------------------
  Copy data to clipboard as table of Tab text
  ----------------------------------------------------- }
begin

    if Page.ActivePage = DwellTHistTab then plDwellTHist.CopyDataToClipboard ;
    if Page.ActivePage = StabPlotTab then plStabPlot.CopyDataToClipboard ;
    if Page.ActivePage = EditTransitionsTab then scEditDisplay.CopyDataToClipboard ;
    if Page.ActivePage = DetectTransitionsTab then scDetDisplay.CopyDataToClipboard ;
    end ;


procedure TDwellTimesFrm.cbDwellTHistTypeChange(Sender: TObject);
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



procedure TDwellTimesFrm.scDetDisplayCursorChange(Sender: TObject);
{ -------------------------------------------
  Update fields when display cursors change
  -------------------------------------------}
begin
     { Baseline cursor label 'z' }
     lbTranDetBase.Left := scDetDisplay.Left + scDetDisplay.Width + 1 ;
     lbTranDetBase.Top := scDetDisplay.Top - (lbTranDetBase.Height div 2)
                       + scDetDisplay.YToScreenCoord(ChanNum,
                         scDetDisplay.HorizontalCursors[DetCurs.Base]) ;

     { Unit current label 'u' }
     lbTranDetIUnit.Left := lbTranDetBase.Left ;
     lbTranDetIUnit.Top := scDetDisplay.Top - (lbTranDetIUnit.Height div 2)
                       + scDetDisplay.YToScreenCoord(ChanNum,
                         scDetDisplay.HorizontalCursors[DetCurs.IUnit]) ;

     { Threshold current label 't' }
     lbTranDetThreshold.Left := lbTranDetBase.Left ;
     lbTranDetThreshold.Top := scDetDisplay.Top - (lbTranDetThreshold.Height div 2)
                       + scDetDisplay.YToScreenCoord(ChanNum,
                         scDetDisplay.HorizontalCursors[DetCurs.Threshold]) ;

     edUnitCurrent.Value := (scDetDisplay.HorizontalCursors[DetCurs.IUnit]
                            - scDetDisplay.HorizontalCursors[DetCurs.Base])
                            * Channel[ChanNum].ADCScale ;

     if edUnitCurrent.Value <> 0.0 then
        edThreshold.Value :=   (scDetDisplay.HorizontalCursors[DetCurs.Threshold]
                                - scDetDisplay.HorizontalCursors[DetCurs.Base])
                                * Channel[ChanNum].ADCScale / edUnitCurrent.Value
     else edThreshold.Value := 0.0 ;
     Channel[ChanNum].ADCZero := scDetDisplay.HorizontalCursors[DetCurs.Base] ;

     { Set position of trend removal selection block labels }
     lbTranDetC0.Visible := True ;
     lbTranDetC0.Left := scDetDisplay.Left
                           + scDetDisplay.XToScreenCoord( 0,
                             scDetDisplay.VerticalCursors[DetCurs.C0] ) ;
     lbTranDetC1.Visible := True ;
     lbTranDetC1.Left := scDetDisplay.Left
                           + scDetDisplay.XToScreenCoord( 0,
                             scDetDisplay.VerticalCursors[DetCurs.C1] ) ;


     { Place horizontal line between baseline block cursors }
     shTranDetLine.visible := true ;
     shTranDetLine.Top := lbTranDetC0.Top + (lbTranDetC0.Height div 2) ;
     shTranDetLine.Left := MinInt([lbTranDetC0.Left,lbTranDetC1.Left])
                         + lbTranDetC0.Width ;
     shTranDetLine.Width := MaxInt([lbTranDetC0.Left,lbTranDetC1.Left])
                          - shTranDetLine.Left - lbTranDetC1.Width ;

     { Update vertical display magnification so that changes are retained }
     Channel[ChanNum].yMin := scDetDisplay.YMin[ChanNum] ;
     Channel[ChanNum].yMax := scDetDisplay.YMax[ChanNum] ;

     end ;


procedure TDwellTimesFrm.PageChange(Sender: TObject);
{ ----------------------------------------------
  Set up controls on pages when tab page changes
  ---------------------------------------------- }
begin

     Main.ZoomMenus( False ) ;

     // Amplitude histogram page
     if Page.ActivePage = AmpHistTab then begin
        DisplayGrp.Visible := False ;
        Settings.UnitCurrent := edUnitCurrent.Value ;
        Settings.DwellTimes.Threshold := edThreshold.Value ;
        if AmpHist.Available then begin
           AdjustAmpHistZeroLevel( Channel[ChanNum].ADCZero ) ;
           plAmpHist.Invalidate ;
           end ;
        end ;

     // Transition detection page
     if Page.ActivePage = DetectTransitionsTab then begin
        Resize ;
        edUnitCurrent.Value := Settings.UnitCurrent ;
        edThreshold.Value := Settings.DwellTimes.Threshold ;

        if EventFile.NumEvents > 0 then EditTransitionsTab.Enabled := True
                                   else EditTransitionsTab.Enabled := False ;
        DisplayRecord ;
        DisplayGrp.Visible := True ;
        Main.ZoomMenus( True ) ;
        end ;

      // Event editing page
      if Page.ActivePage = EditTransitionsTab then begin
         Resize ;
         if EventFile.NumEvents > 0 then begin
            if sbEvent.Max <> EventFile.NumEvents then begin
               sbEvent.Max := EventFile.NumEvents ;
               sbEvent.Min := 1 ;
               sbEvent.Position := 1 ;
               end ;
            sbEvent.Enabled := True ;
            EditTransitionsTab.Enabled := True ;
            Main.ZoomMenus( True ) ;
            end
         else begin
            sbEvent.Min := 1 ;
            sbEvent.Position := 1 ;
            sbEvent.Max := 1 ;
            sbEvent.Enabled := False ;
            EditTransitionsTab.Enabled := False ;
            Main.ZoomMenus( False ) ;
            end ;

         DisplayEvent ;
         DisplayGrp.Visible := True ;
         end ;

      // Dwell time histogram page
      if Page.ActivePage = DwellTHistTab then begin
         DisplayGrp.Visible := False ;
         Resize ;

         if EventFile.NumEvents > 0 then begin
            bNewDwellTHist.Enabled := True ;
            if edDwellTRange.HiLimit <> EventFile.NumEvents then begin
               edDwellTRange.LoLimit := 1 ;
               edDwellTRange.HiLimit := EventFile.NumEvents ;
               edDwellTRange.LoValue := 1 ;
               edDwellTRange.HiValue := EventFile.NumEvents ;
               edDwellTRange.Enabled := True ;
               end
            end
         else begin
            edDwellTRange.LoLimit := 0 ;
            edDwellTRange.HiLimit := 0 ;
            edDwellTRange.LoValue := 0 ;
            edDwellTRange.HiValue := 0 ;
            edDwellTRange.Enabled := False ;
            bNewDwellTHist.Enabled := False ;
            end ;

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
         DisplayGrp.Visible := False ;
         Resize ;
         NewStabPlotType ;
         end ;

     { Enable/disable print and copy menu items as necessary }
     SetCopyAndPrintMenus ;
     end;


procedure TDwellTimesFrm.plDwellTHistCursorChange(Sender: TObject);
{ -------------------------------------------
  Update labels when histogram cursors change
  -------------------------------------------}
var
   Lo,Mid,Hi,y : single ;
   iStart,iEnd,i : Integer ;
   XMean,XYSum,YSum : single ;
begin
     { Set readout cursor label }
     plDwellTHist.GetBin( 0, plDwellTHist.FindNearestIndex(0,DwellTCurs.Read),
                          Lo, Mid, Hi, y ) ;
     lbDwellTRead.caption := format(' %.4g %s / %.4g',[Mid,Settings.TUnits,y] ) ;
     lbDwellTRead.Visible := True ;
     lbDwellTRead.Left := plDwellTHist.Left
                          + plDwellTHist.XToCanvasCoord( Mid )
                          - (lbDwellTRead.Width div 2);

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
     for i := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
         XYSum := XYSum + (DwellTHist.Bins[i].y*DwellTHist.Bins[i].Mid) ;
         YSum := YSum + DwellTHist.Bins[i].y ;
         end ;
     if XYSum <> 0.0 then XMean := XYSum / YSum
                     else XMean := 0.0 ;

     lbDwellTArea.visible := true ;
     shline.visible := true ;
     lbDwellTArea.caption := format(' Mean= %.3g ms /Events= %d ',
                                [XMean*Settings.TScale,
                                Round(YSum)] ) ;

     { Display mean signal level and histogram % between cursors }
     Mid := (lbDwellTC0.Left + lbDwellTC1.Left) div 2 ;
     lbDwellTArea.Left := ((lbDwellTC0.Left + lbDwellTC1.Left) div 2)
                          - (lbDwellTArea.Width div 2) ;
     lbDwellTArea.Top := lbDwellTC0.Top ;
     lbDwellTArea.Visible := True ;

     { Place horizontal line between fit/analysis cursors }
     shDwellTLine.Top := lbDwellTArea.Top + (lbDwellTArea.Height div 2) ;
     shDwellTLine.Left := MinInt([lbDwellTC0.Left,lbDwellTC1.Left])
                    + lbDwellTC0.Width ;
     shDwellTLine.Width := MaxInt([lbDwellTC0.Left,lbDwellTC1.Left])
                     - shDwellTLine.Left - lbDwellTC0.Width ;
     shDwellTLine.Visible := True ;

     end ;


procedure TDwellTimesFrm.rbLinearClick(Sender: TObject);
begin
     nbLinLogSettings.PageIndex := 0
     end;

procedure TDwellTimesFrm.rbLogLinClick(Sender: TObject);
begin
     nbLinLogSettings.PageIndex := 1
     end;


procedure TDwellTimesFrm.SetCopyAndPrintMenus ;
{ ------------------------------------------------
  Enable Copy and Print menus if data is available
  ------------------------------------------------}
begin
     if Page.ActivePage = DwellTHistTab then begin
        if plDwellTHist.Available then Main.CopyAndPrintMenus( True, True )
                            else Main.CopyAndPrintMenus( False, False ) ;
        Main.ZoomMenus( False ) ;
        end
     else if Page.ActivePage = EditTransitionsTab then begin
        if EditTransitionsTab.Enabled then begin
           Main.CopyAndPrintMenus( True, True ) ;
           Main.ZoomMenus( True ) ;
           end
        else begin
           Main.CopyAndPrintMenus( False, False ) ;
           Main.ZoomMenus( False ) ;
           end ;
        end
     else begin
        Main.ZoomMenus( True ) ;
        Main.CopyAndPrintMenus( True, True ) ;
        end ;
     end ;


procedure TDwellTimesFrm.FormActivate(Sender: TObject);
begin

     SetCopyAndPrintMenus ;
     end;


{ *** BASELINE TREND REMOVAL PROCEDURES ********************************* }


procedure TDwellTimesFrm.bAddBlockClick(Sender: TObject);
{ --------------------------------------------------
  Add another baseline block to trend line data list
  --------------------------------------------------}
var
   i,iStart,iEnd : Integer ;
   Sum : single ;
begin
     { Get start/end of block of data points containing baseline level }
     iStart := MinInt([ scDetDisplay.VerticalCursors[DetCurs.C0],
                        scDetDisplay.VerticalCursors[DetCurs.C1] ]) ;
     iEnd :=   MaxInt([ scDetDisplay.VerticalCursors[DetCurs.C0],
                        scDetDisplay.VerticalCursors[DetCurs.C1] ]) ;

     { Calculate average level }
     Sum := 0.0 ;
     for i := iStart to iEnd do begin
         Sum := Sum + ADC^[i] ;
         end ;

     { Add to current set of baseline blocks }
     if TrendLine.NumPoints < High(TrendLine.y) then begin
        TrendLine.y[TrendLine.NumPoints] := Sum / (iEnd - iStart + 1) ;
        TrendLine.x[TrendLine.NumPoints] := (iEnd + iStart)*0.5
                                            + scDetDisplay.xOffset ;
        Inc(TrendLine.NumPoints) ;
        edNumBlocks.Value := TrendLine.NumPoints ;
        end
     else bAddBlock.Enabled := False ;

     { Do a fit if enough points are available }
     if TrendLine.NumPoints >= TrendLine.Func.NumParameters then FitTrendLine ;
     DisplayRecord ;
     end ;


procedure TDwellTimesFrm.FitTrendLine ;
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

procedure TDwellTimesFrm.bClearSamplesClick(Sender: TObject);
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


procedure TDwellTimesFrm.cbTrendEquationChange(Sender: TObject);
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

procedure TDwellTimesFrm.bRemoveTrendClick(Sender: TObject);
{ ---------------------------------------------------
  Subtract baseline trend from selected data region
  -------------------------------------------------- }
const
     NumBlocksPerBuffer = 256 ;
var
   NumSamplesPerBuffer, BufferStart, iPointer : Integer ;
   Done,NewBufferNeeded : Boolean ;
begin
     { Make a back up copy of original data file if one doesn't already exist }
     edStatus.text := 'Making backup' ;
     MakeBackupFile ;
     bRestoreOriginal.Enabled := CdrFH.BackedUp and
                                 FileExists(ChangeFileExt(CdrFH.FileName,'.bak')) ;

     { Range of samples to have trend subtracted from them }
     if rbAllRecords.Checked then begin
        Det.StartAtSample := 0 ;
        Det.EndAtSample := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        Det.StartAtSample := Round( edRange.LoValue / CdrFH.dt ) ;
        Det.EndAtSample :=   Round( edRange.HiValue / CdrFH.dt ) ;
        end ;

     { Initialise progress bar }
     pbDetection.Min := 1 ;
     pbDetection.Max := High(Det.EndAtSample) ;
     pbDetection.Position := Det.StartAtSample ;
     pbDetection.Min := Det.StartAtSample ;
     pbDetection.Max := Det.EndAtSample ;

     edStatus.Text := 'Removing Trend' ;
     
     { Move data file pointer to start of data to be plotted }
     Det.SampleNum := Det.StartAtSample ;


     NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

     { Initial settings of while loop control flags }
     NewBufferNeeded := True ;
     Done := False ;
     while not Done do begin

         { Load another buffer of A/D samples from file when needed }
         if NewBufferNeeded then begin
            BufferStart := Det.SampleNum ;
            { Read into buffer }
            if ReadCDRBuffer(CdrFH,BufferStart,ADC^,NumBlocksPerBuffer)
               <> NumBlocksPerBuffer then edStatus.Text := 'Read Error' ;

            { Set buffer pointer to first sample }
            iPointer := Channel[ChanNum].ChannelOffset ;
            pbDetection.Position := Det.SampleNum ;
            NewBufferNeeded := False ;
            Application.ProcessMessages ;
            end ;

         { Subtract trend from A/D samples }
         ADC^[iPointer] := ADC^[iPointer]
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
            if WriteCDRBuffer(CdrFH,BufferStart,ADC^,NumBlocksPerBuffer)
               <> NumBlocksPerBuffer then edStatus.Text := 'Write Error' ;
            NewBufferNeeded := True ;
            end ;
         end ;

     { Clear status/progress bar }
     edStatus.Text := '' ;
     pbDetection.Position := 1 ;

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

procedure TDwellTimesFrm.bRestoreOriginalClick(Sender: TObject);
{ --------------------------------------
  Restore orginal data from back up file
  -------------------------------------- }
begin
     Screen.Cursor := crHourGlass ;
     RestoreFromBackupFile ;
     Screen.Cursor := crDefault ;
     end;

procedure TDwellTimesFrm.cbDetChannelChange(Sender: TObject);
begin
     Settings.DwellTimes.ChanNum := cbDetChannel.ItemIndex ;
     ChanNum := Settings.DwellTimes.ChanNum ;
     { Initialise continuous and detected event displays with new channel }
     InitialiseDisplays ;
     if bDetect.Enabled then DisplayRecord ;
     end;


procedure TDwellTimesFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menu items }
     Main.CopyAndPrintMenus( False, False ) ;
     end;


procedure  TDwellTimesFrm.ZoomIn( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     if Page.ActivePage = DetectTransitionsTab then
        scDetDisplay.ZoomIn(ChanNum)
     else if Page.ActivePage = EditTransitionsTab then
        scEditDisplay.ZoomIn(ChanNum) ;
     end ;


procedure  TDwellTimesFrm.ZoomOut ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDetDisplay.MaxADCValue := MaxADCValue ;
     scDetDisplay.MinADCValue := MinADCValue ;
     scDetDisplay.ZoomOut ;
     scEditDisplay.MaxADCValue := MaxADCValue ;
     scEditDisplay.MinADCValue := MinADCValue ;
     scEditDisplay.ZoomOut ;
     end ;


// **** Stability plot methods *************************************

procedure TDwellTimesFrm.InitialiseStabPlot ;
{ -------------------------------
  Initialise stability plot page
  ------------------------------- }
begin

     { Stability plot type list }
     cbStabPlotType.Clear ;
     cbStabPlotType.Items.AddObject( 'Mean current', TObject(stMeanCurrent) ) ;
     cbStabPlotType.Items.AddObject( 'Open Probability', TObject(stOpenProb) ) ;
     cbStabPlotType.Items.AddObject( 'Closed times', TObject(stClosedTimes) ) ;
     cbStabPlotType.Items.AddObject( 'Open times', TObject(stOpenTimes) ) ;
    cbStabPlotType.Items.AddObject( 'Current vs Open times',
                                    TObject(stAvgVsOpenTimes) ) ;
     cbStabPlotType.ItemIndex := 0 ;

     plStabPlot.ClearVerticalCursors ;
     StabCurs.Read := plStabPlot.AddVerticalCursor( clRed ) ;
     lbStabRead.visible := false ;

     bStabPlotSetAxes.Enabled := False ;

     end ;


procedure TDwellTimesFrm.bDoStabPlotClick(Sender: TObject);
{ ---------------------------
  Create a new stability plot
  --------------------------- }
var
   StartAt,EndAt,NumRegions : Integer ;
   StabPlotType : TStabPlotType ;
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
     NumRegions := Round(edStabPlotNumRegions.Value) ;

     { Choose appropriate histogram computation procedure }
     case StabPlotType of
          stClosedTimes : DwellTimeStabPlot(StartAt,EndAt,NumRegions,csClosedState ) ;
          stOpenTimes :   DwellTimeStabPlot(StartAt,EndAt,NumRegions,csOpenState ) ;
          stMeanCurrent : MeanCurrentStabPlot( StartAt, EndAt, NumRegions, 0.0 ) ;
          stOpenProb :    MeanCurrentStabPlot( StartAt, EndAt, NumRegions,
                                               edUnitCurrent.Value ) ;
          stAvgVsOpenTimes : StateAverageVsOpenTime( StartAt, EndAt ) ;
          end ;

     Screen.Cursor := crDefault ;
     bDoStabPlot.Enabled := True ;
     bStabPlotSetAxes.Enabled := True ;
     bAbortStabPlot.Enabled := False ;
     end;


procedure  TDwellTimesFrm.DwellTimeStabPlot(
           StartAt : Integer ;       { Event to start at [In] }
           EndAt : Integer ;         { Event to end at [In] }
           NumRegions : Integer ;    { No. of averaging regions [In] }
           RequiredState : Integer   { State to analysed  [In] }
           ) ;
{ -------------------------------------------------------------------
  Plot average of selected dwell time over series of NumRegions blocks
  ------------------------------------------------------------------- }
var
   EventNum,BlockSize,nAvg : Integer ;
   SumX, SumY : single ;
   Event : TEvent ;
   Done : Boolean ;
begin

     { Plot graph of currently selected variables }
     plStabPlot.xAxisAutoRange := True ;
     plStabPlot.yAxisAutoRange := True ;
     plStabPlot.xAxisLabel := 'Time (s) ' ;
     plStabPlot.yAxisLabel := cbStabPlotType.Text + ' (' + Settings.TUnits + ')' ;
     { Clear data points line }
     plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

    { Read records of data from file and add to histogram }
    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    BlockSize := MaxInt( [(EndAt - StartAt + 1) div (NumRegions*2),1] ) ;
    EventNum := StartAt ;

    pbStabPlotProgress.Min := 1 ;
    pbStabPlotProgress.Max := High(EndAt) ;
    pbStabPlotProgress.Position := StartAt ;
    pbStabPlotProgress.Max := EndAt ;
    pbStabPlotProgress.Min := StartAt ;

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
        if EventNum > EndAt then Done := True ;

        { Update progress bar }
        pbStabPlotProgress.Position := EventNum ;
        end ;

    pbStabPlotProgress.Position := pbStabPlotProgress.Min
    end ;


procedure  TDwellTimesFrm.MeanCurrentStabPlot(
           StartAt : Integer ;       { Sample to start at [In] }
           EndAt : Integer ;         { Sample to end at [In] }
           NumRegions : Integer ;    { No. of Samples per block [In] }
           DivideFactor : single     { Unitary current for calculation of open prob. }
           ) ;
{ ----------------------------------------------------------------------
  Plot average of mean current (or open probability if DivideFactor <>0) )
  ---------------------------------------------------------------------- }
var
   RegionSize,nAvg : Integer ;
   BlockPointer : Integer ;        { Sample block pointer }
   BufPointer : Integer ;          { Sample pointer within buffer }
   NumBlocksPerBuffer : Integer ;  // No. of sample blocks in work buffer
   NumSamplesPerBuffer : Integer ; { No. of individual samples in buffer }
   NumBlocksToDo : Integer ;       { Number of sample blocks still to be processed }

   SumX, SumY : double ;
   Avg,AvgTime : single ;
   Done : Boolean ;
begin

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

     { Clear data points line }
     plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;


    { Read A/D samples from file and add to average }

    SumX := 0.0 ;
    SumY := 0.0 ;
    nAvg := 0 ;
    EndAt := MinInt( [EndAt,(CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1]) ;
    RegionSize := MaxInt( [(EndAt - StartAt + 1) div (NumRegions),1] ) ;

    { Sample/buffer pointers }
    BlockPointer := StartAt ;
    NumBlocksToDo := EndAt - StartAt + 1 ;
    NumBlocksPerBuffer := MinInt([NumBlocksToDo, 256]) ;
    NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;
    BufPointer := NumSamplesPerBuffer ;

    { Progress indicator }
    pbStabPlotProgress.Min := 1 ;
    pbStabPlotProgress.Max := High(EndAt) ;
    pbStabPlotProgress.Position := StartAt ;
    pbStabPlotProgress.Min := StartAt ;
    pbStabPlotProgress.Max := EndAt ;

    Done := False ;
    while not Done do begin

         { Get buffer of samples from file }
         if BufPointer >= NumSamplesPerBuffer then begin
            ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;
            NumBlocksToDo := NumBlocksToDo - NumBlocksPerBuffer ;
            BufPointer := Channel[ChanNum].ChannelOffset ;
            end ;

         { Add sample to average, if it is the correct type of state }
         SumY := SumY + ADC^[BufPointer] ;
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
           if NumBlocksToDo <= 0 then Done := True ;
           { Update progress bar }
           pbStabPlotProgress.Position := BlockPointer ;
           end ;

        { Next sample block }
        Inc(BlockPointer) ;
        BufPointer := BufPointer + CdrFH.NumChannels ;
        end ;

    pbStabPlotProgress.Position := pbStabPlotProgress.Min
    end ;


procedure TDwellTimesFrm.StateAverageVsOpenTime(
           StartAt : Integer ;       { Event No. to start at [In] }
           EndAt : Integer           { Event No. to end at [In] }
           ) ;

{ ------------------------------------------
  Compute average current amplitude by state
  ------------------------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   i,BlockPointer,SamplePointer,LastSampleInBuffer,EventNum,NumPoints : Integer ;
   Sum,y : double ;
   nSum : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
   xy : ^TSingleXYArray ;
begin

    try

      New(xy) ;

      { Initialise progress bar }
      pbStabPlotProgress.Min := 1 ;
      pbStabPlotProgress.Max := High(EndAt) ;
      pbStabPlotProgress.Position := StartAt ;
      pbStabPlotProgress.Max := EndAt ;
      pbStabPlotProgress.Min := StartAt ;

      { Plot graph of currently selected variables }
      plStabPlot.xAxisAutoRange := True ;
      plStabPlot.yAxisAutoRange := True ;
      plStabPlot.xAxisLabel := format('Open Time (%s) ',[Settings.TUnits] ) ;
      plStabPlot.yAxisLabel := format('Avg. Current (%s)',[Channel[ChanNum].ADCUnits]) ;

      { Clear data points line }
      plStabPlot.CreateLine( 0 , clBlue, msOpenSquare, psClear ) ;

      LastSampleInBuffer := NumBlocksPerBuffer*CdrFH.NumChannels - 1 ;

      { Read records of data from file and add to histogram }
      Done := False ;
      EventNum := StartAt ;
      Sum := 0.0 ;
      nSum := 0 ;
      NumPoints := 0 ;
      while not Done do begin

         { Read event }
         ReadEventFromFile( EventFile, EventNum, Event ) ;
         Done := not Event.Available ;

         { Extract samples associated with event and add to histogram }
         if (not Event.Ignore) and (Event.ChannelState = 1) then begin

            BlockPointer := Event.StartAt ;
            NewBufferNeeded := True ;
            while BlockPointer <= Event.EndAt do begin
               { Get A/D samples from file }
               if NewBufferNeeded then begin
                  ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;
                  NewBufferNeeded := False ;
                  SamplePointer := Channel[ChanNum].ChannelOffset ;
                  end ;

               { Get amplitude value }
               y := (ADC^[SamplePointer]
                    - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
               { Add to sum for average }
               Sum := Sum + y ;
               Inc(nSum) ;
               { Add average and dwell time to list  }
               if Blockpointer = Event.EndAt then begin
                  if nSum > 0 then y := Sum / nSum ;
                  Sum := 0.0 ;
                  nSum := 0 ;
                  xy[NumPoints].x := Event.Duration*Settings.TScale ;
                  xy[NumPoints].y := y ;
                  Inc(NumPoints) ;
                  end ;

               SamplePointer := SamplePointer + CdrFH.NumChannels ;
               if SamplePointer > LastSampleInBuffer then NewBufferNeeded := True ;
               Inc(BlockPointer) ;
               end ;
            end ;

         { Update progress bar }
         pbStabPlotProgress.Position := EventNum ;
         { Increment to next record }
         Inc(EventNum)  ;
         if (EventNum > EndAt) or (NumPoints >= MaxTBuf) then Done := True ;

         { Allow other events to be serviced }
         if (NumPoints mod 100) = 0 then Application.ProcessMessages ;
         { If the Stability Plot button has been re-enabled, abort the run }
         if Not bAbortStabPlot.Enabled then Done := True ;

         end ;

      // Display results on plot
      for i := 0 to NumPoints-1 do plStabPlot.AddPoint( 0, xy[i].x, xy[i].y ) ;

      { Initialise progress bar }
      pbStabPlotProgress.Position := pbStabPlotProgress.Min ;

    finally
      Dispose(xy) ;
      end ;

    end ;


procedure TDwellTimesFrm.bStabPlotSetAxesClick(Sender: TObject);
{ ----------------------------------------
  Set stability plot axes range/law/labels
  ---------------------------------------- }
begin
     SetAxesFrm.Plot := plStabPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;


procedure TDwellTimesFrm.NewStabPlotType ;
{ -------------------------------------------------------
  Update controls when a new stability plot type selected
  ------------------------------------------------------- }
var
   StabPlotType : TStabPlotType ;
   OldIndex : Integer ;
begin

     { Update stability plot type list depending upon whether dwell timesare available }
     OldIndex := cbStabPlotType.ItemIndex ;
     cbStabPlotType.Clear ;
     cbStabPlotType.Items.AddObject( 'Mean current', TObject(stMeanCurrent) ) ;
     cbStabPlotType.Items.AddObject( 'Open Probability', TObject(stOpenProb) ) ;
     if EventFile.NumEvents > 0 then begin
        cbStabPlotType.Items.AddObject( 'Closed times', TObject(stClosedTimes) ) ;
        cbStabPlotType.Items.AddObject( 'Open times', TObject(stOpenTimes) ) ;
        cbStabPlotType.Items.AddObject( 'Current vs Open times', TObject(stAvgVsOpenTimes) ) ;
        end ;

     cbStabPlotType.ItemIndex := OldIndex ;

     StabPlotType := TStabPlotType(
                     cbStabPlotType.Items.Objects[cbStabPlotType.ItemIndex]) ;

     if (StabPlotType = stOpenTimes) or
        (StabPlotType = stClosedTimes) or
        (StabPlotType = stAvgVsOpenTimes) then begin
        edStabPlotRange.Scale := 1.0 ;
        edStabPlotRange.Units := '' ;
        edStabPlotRange.HiLimit := EventFile.NumEvents ;
        edStabPlotRange.LoLimit := 1 ;
        end
     Else begin
        edStabPlotRange.Scale := CdrFH.dt ;
        edStabPlotRange.Units := 's' ;
        edStabPlotRange.HiLimit := ((CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1) ;
        edStabPlotRange.LoLimit := 0.0 ;
        end ;

     edStabPlotRange.LoValue := edStabPlotRange.LoLimit ;
     edStabPlotRange.HiValue := edStabPlotRange.HiLimit ;

     if (StabPlotType = stAvgVsOpenTimes) then begin
        edStabPlotNumRegions.Visible := False ;
        lbStabPlotNumRegions.Visible := False ;
        end
     else begin
        edStabPlotNumRegions.Visible := True ;
        lbStabPlotNumRegions.Visible := True ;
        end

     end ;


procedure TDwellTimesFrm.cbStabPlotTypeChange(Sender: TObject);
begin
     NewStabPlotType ;
     end;


procedure TDwellTimesFrm.ckIgnoreStateClick(Sender: TObject);
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


procedure TDwellTimesFrm.bBlockIgnoreClick(Sender: TObject);
{ --------------------------------
  Set a block of events to ignored
  -------------------------------- }
begin
     SetIgnoreFrm.SelectedChannel := cbDetChannel.ItemIndex ;
     SetIgnoreFrm.ShowModal ;
     DisplayEvent ;
     end;


procedure TDwellTimesFrm.bDeleteIgnoredClick(Sender: TObject);
{ ----------------------------------------------------------
  Remove event marked as "Ignored" completely from the list
  by combining them with adjacent events
  ---------------------------------------------------------- }
var
   FromEventNum,ToEventNum,EndAt,PreviousState : Integer ;
   ExactEnd,Duration : double ;
   Event : TEvent ;
   Done : Boolean ;
begin

     Screen.Cursor := crHourglass ;

     FromEventNum := 1 ;
     ToEventNum := 0 ;
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
               end ;
            end
         else Done := True ;

         if FromEventNum >= EventFile.NumEvents then Done := True ;
         end ;

     { Update number of events }
     EventFile.NumEvents := ToEventNum + 1 ;

     Screen.Cursor := crDefault ;
     end;

procedure TDwellTimesFrm.scEditDisplayCursorChange(Sender: TObject);
begin
     { Update vertical display magnification so that changes are retained }
     Channel[ChanNum].yMin := scEditDisplay.YMin[ChanNum] ;
     Channel[ChanNum].yMax := scEditDisplay.YMax[ChanNum] ;
     end;

procedure TDwellTimesFrm.plStabPlotCursorChange(Sender: TObject);
var
   x,y : single ;
begin
     { Set readout cursor label }
     plStabPlot.GetPoint( 0,
                          plStabPlot.FindNearestIndex(0,StabCurs.Read),
                          x,y ) ;
     lbStabRead.caption := format('x= %.4g y= %.4g ',[x,y]) ;

     lbStabRead.Visible := True ;
     lbStabRead.Left := plStabPlot.Left
                            + plStabPlot.XToCanvasCoord( x )
                            - (lbStabRead.Width div 2);

     end;


procedure TDwellTimesFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDetDisplay.MaxADCValue := MaxADCValue ;
     scDetDisplay.MinADCValue := MinADCValue ;
     scDetDisplay.DisplayGrid := Settings.DisplayGrid ;
     scDetDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDetDisplay.TFormat := ' %.6g ' + Settings.TUnits + ' ' ;
     scDetDisplay.Invalidate ;

     scEditDisplay.MaxADCValue := MaxADCValue ;
     scEditDisplay.MinADCValue := MinADCValue ;
     scEditDisplay.DisplayGrid := Settings.DisplayGrid ;
     scEditDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scEditDisplay.TFormat := ' %.6g ' + Settings.TUnits + ' ' ;
     scEditDisplay.Invalidate ;
     
     end ;

procedure TDwellTimesFrm.edEventKeyPress(Sender: TObject; var Key: Char);
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

procedure TDwellTimesFrm.InitialiseAmpHist ;
{ ----------------------------------------------------------
  Amplitude histogram initialisations when form is displayed
  ----------------------------------------------------------}
var
   ch,State : Integer ;
begin

     // Create list of amplitude histogram options
     cbAmpHistType.Clear ;
     cbAmpHistType.Items.AddObject('All Points', TObject(htAllPoints) ) ;
     cbAmpHistType.Items.AddObject('All Points in State',TObject(htAllPointsInState)) ;
     cbAmpHistType.Items.AddObject('Mean State Amplitude',TObject(htStateAverage) ) ;
     cbAmpHistType.Items.AddObject('Patlak Average', TObject(htPatlakAverage) ) ;
     cbAmpHistType.Items.AddObject('External', TObject(htAmplitudesFile) ) ;
     cbAmpHistType.ItemIndex := 0 ;
     AmpHistTypePage.PageIndex := AllPointsPage ;

     // Time range to be included in histogram
     edAmpHistRange.Scale := CdrFH.dt ;
     edAmpHistRange.LoLimit := 0.0 ;
     edRange.LoValue := edAmpHistRange.LoLimit ;
     edAmpHistRange.HiLimit := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
     edAmpHistRange.HiValue := edAmpHistRange.HiLimit ;
     edAmpHistRange.Units := 's' ;

     { Fill data source channel selection list }
     cbAmpHistChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
         cbAmpHistChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
         end ;
     cbAmpHistChannel.ItemIndex := 0 ;
     ChanNum := cbAmpHistChannel.ItemIndex ;

     { Create channel state options list }
     cbChannelState.Clear ;
     State := -1 ;
     cbChannelState.Items.AddObject( 'All', TObject(State) ) ;
     State := 0 ;
     cbChannelState.Items.AddObject( 'Closed', TObject(State) ) ;
     for State := 1 to 6 do cbChannelState.Items.AddObject( format('Open (X%d)'
                                                            ,[State]),TObject(State)) ;
     cbChannelState.ItemIndex := 0 ;


     { Number of bins in histogram }
     edAmpHistNumBins.LoLimit := 2 ;
     edAmpHistNumBins.HiLimit := High(AmpHist.Bins)+1 ;
     edAmpHistNumBins.Value := 512 ;

     { Amplitude range of histogram bins }
     edRangeLo.Value := (MinADCValue-Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
     edRangeLo.Units := Channel[ChanNum].ADCUnits ;
     edRangeHi.Value := (MaxADCValue-Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
     edRangeHi.Units := Channel[ChanNum].ADCUnits ;

     { Initialise display cursors }
     plAmpHist.ClearVerticalCursors ;
     AmpCurs.C0 := plAmpHist.AddVerticalCursor( clGray ) ;
     AmpCurs.C1 := plAmpHist.AddVerticalCursor( clGray ) ;
     AmpCurs.Read := plAmpHist.AddVerticalCursor( clRed ) ;
     lbUnitArrow.Visible := False ;

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
     bSetUnitCurrent.Enabled := False ;
     AmpHist.Available := False ;
     bAmpFitCurve.Enabled := False ;

     end ;


procedure TDwellTimesFrm.AllPointsHistogram ;
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
begin

    { Initialise progress bar }
    pbAmpHist.Min := 1 ;
    pbAmpHist.Max := High(AmpHist.EndAt) ;
    pbAmpHist.Position := AmpHist.StartAt ;
    pbAmpHist.Min := AmpHist.StartAt ;
    pbAmpHist.Max := AmpHist.EndAt ;

    { Read records of data from file and add to histogram }

    if AmpHist.RangeLo <> AmpHist.RangeHi then
       BinScale := AmpHist.NumBins/(AmpHist.RangeHi - AmpHist.RangeLo)
    else BinScale := 1.0 ;

    BlockPointer := AmpHist.StartAt ;
    Done := False ;
    while not Done do begin

       { Read a record from file and add its data to histogram }
       NumBlocks := ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;

       if NumBlocks = NumBlocksPerBuffer then begin
          j := Channel[ChanNum].ChannelOffset ;
          for i := 0 to NumBlocksPerBuffer-1 do begin
              { Get amplitude value }
              y := (ADC^[j] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
              { Find index of histogram bin }
              iBin := Round( (y - AmpHist.RangeLo)*BinScale ) ;
              iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
              { Increment bin count }
              AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
              j := j + CdrFH.NumChannels ;
              end ;
          end
       else Done := True ;

       { Update progress bar }
       pbAmpHist.Position := BlockPointer ;
       { Increment to next record }
       BlockPointer := BlockPointer + NumBlocksPerBuffer ;
       if BlockPointer > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbAmpHist.Position := pbAmpHist.Min ;

    end ;


procedure TDwellTimesFrm.AllPointsInStateHistogram ;
{ ----------------------------
  Compute all-points histogram
  ----------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,SamplePointer,LastSampleInBuffer,EventNum : Integer ;
   y,BinScale : single ;
   iBin,MarginPoints,SelectedState,NumRead : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
begin

    { Initialise progress bar }
    pbAmpHist.Min := 1 ;
    pbAmpHist.Max := High(AmpHist.EndAt) ;
    pbAmpHist.Position := AmpHist.StartAt ;
    pbAmpHist.Min := AmpHist.StartAt ;
    pbAmpHist.Max := AmpHist.EndAt ;

    { Get channel state to be used }
    SelectedState := Integer(cbChannelState.Items.Objects[cbChannelState.ItemIndex]) ;
    MarginPoints := Round(edMarginPoints.Value) ;
    LastSampleInBuffer := NumBlocksPerBuffer*CdrFH.NumChannels - 1 ;

    // Histogram bin selection factor
    if AmpHist.RangeLo <> AmpHist.RangeHi then
       BinScale := AmpHist.NumBins/(AmpHist.RangeHi - AmpHist.RangeLo)
    else BinScale := 1.0 ;

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
          while BlockPointer <= Event.EndAt do begin

              { Get A/D samples from file }
              if NewBufferNeeded then begin
                 NumRead := ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;
                 NewBufferNeeded := False ;
                 SamplePointer := Channel[ChanNum].ChannelOffset ;
                 end ;

              { Get amplitude value }
              y := (ADC^[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;

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

       { Update progress bar }
       pbAmpHist.Position := EventNum ;
       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbAmpHist.Position := pbAmpHist.Min ;

    end ;


procedure TDwellTimesFrm.StateAverageHistogram ;
{ ------------------------------------------
  Compute average current amplitude by state
  ------------------------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,SamplePointer,LastSampleInBuffer,EventNum : Integer ;
   BinScale : single ;
   Sum,y : double ;
   iBin,MarginPoints,SelectedState,nSum : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
begin

    { Initialise progress bar }
    pbAmpHist.Min := 1 ;
    pbAmpHist.Max := High(AmpHist.EndAt) ;
    pbAmpHist.Position := AmpHist.StartAt ;
    pbAmpHist.Min := AmpHist.StartAt ;
    pbAmpHist.Max := AmpHist.EndAt ;

    { Get channel state to be used }
    SelectedState := Integer(cbChannelState.Items.Objects[cbChannelState.ItemIndex]) ;
    MarginPoints := Round(edMarginPoints.Value) ;
    LastSampleInBuffer := NumBlocksPerBuffer*CdrFH.NumChannels - 1 ;

    // Histogram bin selection factor
    if AmpHist.RangeLo <> AmpHist.RangeHi then
       BinScale := AmpHist.NumBins/(AmpHist.RangeHi - AmpHist.RangeLo)
    else BinScale := 1.0 ;

    { Read records of data from file and add to histogram }
    Done := False ;
    EventNum := AmpHist.StartAt ;
    Sum := 0.0 ;
    nSum := 0 ;
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
          while BlockPointer <= Event.EndAt do begin
              { Get A/D samples from file }
              if NewBufferNeeded then begin
                 ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;
                 NewBufferNeeded := False ;
                 SamplePointer := Channel[ChanNum].ChannelOffset ;
                 end ;

              { Get amplitude value }
              y := (ADC^[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
              { Add to sum for average }
              Sum := Sum + y ;
              Inc(nSum) ;
              { Add average to histogram and end of state }
              if Blockpointer = Event.EndAt then begin
                 if nSum > 0 then y := Sum / nSum ;
                 Sum := 0.0 ;
                 nSum := 0 ;
                 { Find index of histogram bin }
                 iBin := Round( (y - AmpHist.RangeLo)*BinScale ) ;
                 iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
                 { Increment bin count }
                 AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
                 end ;

              SamplePointer := SamplePointer + CdrFH.NumChannels ;
              if SamplePointer > LastSampleInBuffer then NewBufferNeeded := True ;
              Inc(BlockPointer) ;
              end ;
          end ;

       { Update progress bar }
       pbAmpHist.Position := EventNum ;
       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > AmpHist.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bNewAmpHist.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbAmpHist.Position := pbAmpHist.Min ;

    end ;


procedure TDwellTimesFrm.PatlakAverageHistogram ;
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
   iBin,j,i : Integer ;
   FirstBuffer,Done : Boolean ;
   Ring : Array[0..255] of Single ;
begin

    { Initialise progress bar }
    pbAmpHist.Min := 1 ;
    pbAmpHist.Max := High(AmpHist.EndAt) ;
    pbAmpHist.Position := AmpHist.StartAt ;
    pbAmpHist.Min := AmpHist.StartAt ;
    pbAmpHist.Max := AmpHist.EndAt ;

    { Determine number of bytes to be read from file for each buffer }
    NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

    { Number of samples to average }
    NumAvg := IntLimitTo(Round(edNumPatlakAvg.Value),1,High(Ring)+1) ;
    { Variance acceptance threshold for inclusion in histogram }
    VarianceThreshold := edPatlakSDLimit.Value*edPatlakSDLimit.Value ;

    if AmpHist.RangeLo <> AmpHist.RangeHi then
       BinScale := AmpHist.NumBins/(AmpHist.RangeHi - AmpHist.RangeLo)
    else BinScale := 1.0 ;

    { Read records of data from file and add to histogram }
    Done := False ;
    SamplePointer := NumSamplesPerBuffer ;
    BlockPointer := AmpHist.StartAt ;
    FirstBuffer := True ;
    while not Done do begin
        { Get new buffer of samples, if needed }
        if SamplePointer >= NumSamplesPerBuffer then begin
           if ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer)
              <> NumBlocksPerBuffer then Done := True ;
           SamplePointer := Channel[ChanNum].ChannelOffset ;
           { Update progress bar }
           pbAmpHist.Position := BlockPointer ;
           { Allow other events to be serviced }
           Application.ProcessMessages ;
           end ;

        if FirstBuffer then begin
           Sum := 0.0 ;
           SumSquares := 0.0 ;
           for iRing := 0 to NumAvg-1 do begin
               y := (ADC^[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
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
           y := (ADC^[SamplePointer] - Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
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

    { Initialise progress bar }
    pbAmpHist.Position := pbAmpHist.Min ;

    end ;


procedure TDwellTimesFrm.bNewAmpHistClick(Sender: TObject);
{ -------------------------------
  Create and plot a new histogram
  -------------------------------}
var
   HistType : TAmpHistType ;
   x,temp,BinWidth,Sum : single ;
   i,iBin : Integer ;
begin

     bNewAmpHist.Enabled := False ;
     bAbortAmpHist.Enabled := True ;

     Screen.Cursor := crHourGlass ;
     { Ensure all text box parameters are up to date }

     if rbAllRecords.Checked then begin
        AmpHist.StartAt := Round( edAmpHistRange.LoLimit ) ;
        AmpHist.EndAt := Round( edAmpHistRange.HiLimit ) ;
        end
     else begin
        AmpHist.StartAt := Round( edAmpHistRange.LoValue ) ;
        AmpHist.EndAt :=   Round( edAmpHistRange.HiValue ) ;
        end ;

     { Histogram bin range }
     AmpHist.NumBins := Round(edAmpHistNumBins.Value) ;
     AmpHist.RangeLo := edRangeLo.Value ;
     AmpHist.RangeHi := edRangeHi.Value ;

     if AmpHist.RangeLo > AmpHist.RangeHi then begin
        Temp := AmpHist.RangeLo ;
        AmpHist.RangeLo := AmpHist.RangeHi ;
        AmpHist.RangeHi := Temp ;
        end ;

     if AmpHist.RangeLo = AmpHist.RangeHi then begin
        AmpHist.RangeLo := (MinADCValue-Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
        AmpHist.RangeHi := (MaxADCValue-Channel[ChanNum].ADCZero)*Channel[ChanNum].ADCScale ;
        MessageDlg( 'Upper and Lower limits must be different',mtWarning, [mbOK], 0 ) ;
        end ;

     { Get type of histogram }
     HistType := TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex]) ;

    { Initialise histogram record }
    x := AmpHist.RangeLo ;
    AmpHist.MaxBin := AmpHist.NumBins - 1 ;
    BinWidth := (AmpHist.RangeHi - AmpHist.RangeLo) /
                MaxInt([AmpHist.NumBins,1]) ;
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

     { Plot new histogram }
     plAmpHist.xAxisAutoRange := False ;
     plAmpHist.xAxisMin := AmpHist.Bins[0].Lo ;
     plAmpHist.xAxisMax := AmpHist.Bins[AmpHist.MaxBin].Hi ;
     plAmpHist.XAxisTick := (plAmpHist.xAxisMax - plAmpHist.xAxisMin) / 5.0 ;
     plAmpHist.yAxisAutoRange := True ;
     plAmpHist.xAxisLabel := cbAmpHistType.text + ' (' + Channel[ChanNum].ADCUnits + ')' ;
     plAmpHist.yAxisLabel := '%' ;
     plAmpHist.CreateHistogram( 0 ) ;

     for i := 0 to AmpHist.MaxBin do plAmpHist.AddBin( 0,
                                       AmpHist.Bins[i].Lo,
                                       AmpHist.Bins[i].Mid,
                                       AmpHist.Bins[i].Hi,
                                       AmpHist.Bins[i].y ) ;

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
     lbAmpHistRead.Visible := True ;

     // Keep signal zero level
     AmpHistADCZero := Channel[ChanNum].ADCZero ;

     { Enable copy and print menu items }
     Main.CopyAndPrintMenus( True, True ) ;

     bNewAmpHist.Enabled := True ;
     bAbortAmpHist.Enabled := False ;
     bAmpHistSetAxes.Enabled := True ;
     bSetZero.Enabled := True ;
     bSetUnitCurrent.Enabled := True ;
     bAmpFitCurve.Enabled := True ;
     AmpHist.Available := False ;


     end;


procedure TDwellTimesFrm.plAmpHistCursorChange(Sender: TObject);
{ ---------------------------------------
  Update labels when plot cursors change
  ---------------------------------------}
var
   Lo,Mid,Hi,y : single ;
   iStart,iEnd,i : Integer ;
   XMean,XYSum,YSum : single ;
begin

     { Set readout cursor label }
     plAmpHist.GetBin( 0, plAmpHist.FindNearestIndex(0,AmpCurs.Read), Lo, Mid, Hi, y ) ;
     lbAmpHistRead.caption := format(' %.4g %s / %4.g %% ',
                             [Mid,Channel[ChanNum].ADCUnits,y] ) ;
     lbAmpHistRead.Visible := True ;
     lbAmpHistRead.Left := MinInt( [ plAmpHist.Left
                                    + plAmpHist.XToCanvasCoord( Mid )
                                    - (lbAmpHistRead.Width div 2),
                                    ClientWidth - lbAmpHistRead.Width]) ;

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
     for i := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
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
     lbAmpHistArea.Left := MinInt([((lbAmpHistC0.Left + lbAmpHistC1.Left) div 2)
                                    - (lbAmpHistArea.Width div 2),
                                    AmpHistTab.Width - lbAmpHistArea.Width]);
     lbAmpHistArea.Top := lbAmpHistC0.Top ;
     lbAmpHistArea.Visible := True ;

     { Place horizontal line between fit/analysis cursors }
     shAmpHistLine.Top := lbAmpHistArea.Top + (lbAmpHistArea.Height div 2) ;
     shAmpHistLine.Left := MinInt([lbAmpHistC0.Left,lbAmpHistC1.Left])
                           + lbAmpHistC0.Width ;
     shAmpHistLine.Width := MaxInt([lbAmpHistC0.Left,lbAmpHistC1.Left])
                            - shAmpHistLine.Left - lbAmpHistC0.Width ;
     shAmpHistLine.Visible := True ;

     lbUnitArrow.Left := plAmpHist.XToCanvasCoord( Settings.UnitCurrent )
                         + plAmpHist.Left - 5 ;
     lbUnitArrow.Top := plAmpHist.YToCanvasCoord( plAmpHist.YAxisMin )
                        + plAmpHist.Top + 10 ;
     lbUnitArrow.Visible := True ;


     end;


procedure TDwellTimesFrm.bSetZeroClick(Sender: TObject);
{ ----------------------
  Set zero current level
  ----------------------}
begin

     AdjustAmpHistZeroLevel( Round(plAmpHist.VerticalCursors[AmpCurs.Read]
                             /Channel[ChanNum].ADCScale) + AmpHistADCZero ) ;

     end ;


procedure TDwellTimesFrm.AdjustAmpHistZeroLevel(
          NewADCZero : Integer
          );
var
   OldZeroCurrent,ZeroCurrent,Shift : single ;
   i : Integer ;
begin

     // Calculate new zero current level (in A/D converter units)
     OldZeroCurrent := AmpHistADCZero*Channel[ChanNum].ADCScale ;

     // Update channel being analysed
     Channel[ChanNum].ADCZero := NewADCZero ;

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


procedure TDwellTimesFrm.bSetUnitCurrentClick(Sender: TObject);
{ -----------------------------
  Set unitary current amplitude
  -----------------------------}
begin
     Settings.UnitCurrent := plAmpHist.VerticalCursors[AmpCurs.Read] ;
     plAmpHist.Invalidate ;
     end;


procedure TDwellTimesFrm.bAmpFitCurveClick(Sender: TObject);
{ ---------------------------------------------------
  Fit a gaussian prob. density functions to histogram
  ---------------------------------------------------
    25/6/98 Clipboard buffer limit reduced to 31000
    14/1/99 New MathFunc object used }
const
     NumFitPoints = 500 ;
var
   i,iStart,iEnd,Temp,nFit,Row,iBins, Comp,LineNum,NumComp : Integer ;
   ParName : string ;
   Scale,BinWidth,x,dx : single ;
   ParTemp : Array[0..LastParameter] of single ;
   FitData : ^TXYData ;
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
           for iBins := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
               FitData^.x[nFit] := AmpHist.Bins[iBins].Mid ;
               FitData^.y[nFit] := AmpHist.Bins[iBins].y ;
               Inc(nFit) ;
               end ;

           { Abort curve fit, if not enough data points }
           if nFit < AmpFunc.NumParameters then begin
              MessageDlg( format('%d points is insufficient for fit',[nFit]),
                           mtWarning, [mbOK], 0 ) ;
              AmpFunc.Setup( None, ' ',' ' ) ;
              OK := False ;
              end ;
           end ;

        { Create an initial set of guesses for parameters }
        if OK then begin
           for i := 0 to AmpFunc.NumParameters-1 do
               if not AmpFunc.FixedParameters[i] then begin
               AmpFunc.Parameters[i] := AmpFunc.InitialGuess(FitData^,nFit,i) ;
               end ;

           { Let user modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := AmpFunc ;
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


procedure TDwellTimesFrm.ExternalAmplitudeHistogram ;
{ ----------------------------------------------------------
  Compute histogram from list of amplitudes in external file
  ---------------------------------------------------------- }
var
   Value : double ;
   InFile : TextFile ;
   iBin : Integer ;

begin

    { Read records of data from file and add to histogram }
     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := '.txt' ;
     OpenDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     OpenDialog.Title := 'Open amplitudes file';

     if OpenDialog.execute then begin
        AssignFile( InFile, OpenDialog.FileName ) ;
        Reset( InFile ) ;
        while not EOF( InFile ) do begin
            ReadLn( InFile, Value ) ;
            iBin := Round( (Value - AmpHist.RangeLo)*AmpHist.BinScale ) ;
            iBin := IntLimitTo( iBin, 0, AmpHist.MaxBin ) ;
            AmpHist.Bins[iBin].y := AmpHist.Bins[iBin].y + 1.0 ;
            end ;
        CloseFile( InFile ) ;
        end ;
    end ;


procedure TDwellTimesFrm.bAmpHistSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plAmpHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TDwellTimesFrm.AmpHistTabEnter(Sender: TObject);
begin
     DisplayGrp.Visible := False ;
     end;

procedure TDwellTimesFrm.bAbortAmpHistClick(Sender: TObject);
begin
     { Re-enable New Histogram button ... This signals that the
       currently active histogram generation run is to be aborted }
     bNewAmpHist.Enabled := True ;
     end;


procedure TDwellTimesFrm.cbAmpHistTypeChange(Sender: TObject);
//
// Update associated controls when amplitude histogram type changed
//
begin

     if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htAllPoints then begin
        edAmpHistRange.LoLimit := 0.0 ;
        edAmpHistRange.Scale := CdrFH.dt ;
        edAmpHistRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAmpHistRange.Units := 's' ;
        AmpHistTypePage.PageIndex := AllPointsPage ;
        end
     else if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htAmplitudesFile then begin
        edAmpHistRange.LoLimit := 0.0 ;
        edAmpHistRange.Scale := CdrFH.dt ;
        edAmpHistRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAmpHistRange.Units := 's' ;
        AmpHistTypePage.PageIndex := AllPointsPage ;
        end

     else if TAmpHistType(cbAmpHistType.Items.Objects[cbAmpHistType.ItemIndex])
         = htPatlakAverage then begin
        edAmpHistRange.LoLimit := 0.0 ;
        edAmpHistRange.Scale := CdrFH.dt ;
        edAmpHistRange.HiLimit := CdrFH.RecordDuration / CdrFH.dt ;
        edAmpHistRange.Units := 's' ;
        AmpHistTypePage.PageIndex := PatlakAveragePage ;
        edPatlakSDLimit.Units := Channel[ChanNum].ADCUnits ;
        end
     else begin
        edAmpHistRange.LoLimit := 1 ;
        edAmpHistRange.Scale := 1.0 ;
        edAmpHistRange.HiLimit := EventFile.NumEvents ;
        edAmpHistRange.Units := '' ;
        AmpHistTypePage.PageIndex := EventHistPage ;
        end ;

     edAmpHistRange.LoValue := edAmpHistRange.LoLimit ;
     edAmpHistRange.hiValue := edAmpHistRange.HiLimit ;

     end;


procedure TDwellTimesFrm.bExportEventListClick(Sender: TObject);
{ --------------------------------------------
  Export channel transition event data to file
  -------------------------------------------- }
var
   OutFile : TextFile ;
   EventNum : Integer ;
   Event : TEvent ;

begin

     // Get the name of a data file to hold ezported data
     SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := '.txt' ;
     SaveDialog.Filter := ' Text Files (*%.txt)|*.txt' ;
     SaveDialog.Title := 'Save data file';

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



procedure TDwellTimesFrm.bAbortStabPlotClick(Sender: TObject);
begin
     // Disabling Abort button to indicate
     // that current stability plot method should be aborted
     bAbortStabPlot.Enabled := False ;
     end;

procedure TDwellTimesFrm.bFTestClick(Sender: TObject);
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

end.

