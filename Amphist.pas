unit Amphist;
{ ======================================================================
  WinCDR (c) J. Dempster, University of Strathclyde, All Rights Reserved
  Signal amplitude histogram module
  Computes various types of amplitude histograms of digitised signal
  ======================================================================
  19/3/99 XYPlotDisplay component now used to plot histogram
  25/6/01 Equation and parameters now displayed with super/subscripts
  }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Grids, printers, ClipBrd,
  global, shared,  maths, setfitpa, ValEdit, RangeEdit,
  XYPlotDisplay, ComCtrls, ValidatedEdit ;

type


  TAmpHistFrm = class(TForm)
    FitGrp: TGroupBox;
    bFitCurve: TButton;
    cbEquation: TComboBox;
    lbReadCursor: TLabel;
    lbFitCursor0: TLabel;
    lbFitCursor1: TLabel;
    shLine: TShape;
    lbArea: TLabel;
    lbUnitArrow: TImage;
    erResults: TRichEdit;
    HistGrp: TGroupBox;
    Label1: TLabel;
    bDoAmpHistogram: TButton;
    GroupBox9: TGroupBox;
    Label2: TLabel;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    cbHistChannel: TComboBox;
    edRange: TRangeEdit;
    HistTypePage: TNotebook;
    Label6: TLabel;
    Label7: TLabel;
    cbChannelState: TComboBox;
    edMarginPoints: TValidatedEdit;
    Label8: TLabel;
    Label9: TLabel;
    edPatlakVarThreshold: TValidatedEdit;
    edNumPatlakAvg: TValidatedEdit;
    bSpecSetAxes: TButton;
    cbHistogramType: TComboBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edNumBins: TValidatedEdit;
    edRangeLo: TValidatedEdit;
    edRangeHi: TValidatedEdit;
    bSetZero: TButton;
    bSetUnitCurrent: TButton;
    pbProgress: TProgressBar;
    bAbort: TButton;
    plPlot: TXYPlotDisplay;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bDoAmpHistogramClick(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure cbHistChannelChange(Sender: TObject);
    procedure bSpecSetAxesClick(Sender: TObject);
    procedure bFitCurveClick(Sender: TObject);
    procedure cbEquationChange(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bSetZeroClick(Sender: TObject);
    procedure bSetUnitCurrentClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure plPlotCursorChange(Sender: TObject);
    procedure cbHistogramTypeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure UpdateHistogramOptionsList ;
    procedure AllPointsHistogram ;
    procedure AllPointsInStateHistogram ;
    procedure StateAverageHistogram ;
    procedure PatlakAverageHistogram ;
  public
    { Public declarations }
    procedure PrintDisplay ;
    procedure CopyImageToClipboard ;
    procedure CopyHistogramDataToClipBoard ;
  end;

var
  AmpHistFrm: TAmpHistFrm;

implementation

{$R *.DFM}

uses mdiform,setaxes,printgra, fileio ;

const
     MaxHistogramSize = 1024 ;
     MaxHistogramBins = MaxHistogramSize - 1 ;
     AllPointsPage = 0 ;
     EventHistPage = 1 ;
     PatlakAveragePage = 2 ;
     FittedLine = 1 ;
type
    THistogramType = (htAllPoints,htAllPointsInState,htStateAverage,
                      htPatlakAverage,htExternal) ;
var
   Histogram : THistogram ;
   ADC : ^TIntArray ; { A/D sample data array }
   HistChannel : TChannel ;
   MathFunc : TMathFunc ;
   BuffersAllocated : Boolean ;
   iCursor : integer ;
   iFitCursor0 : integer ;
   iFitCursor1 : integer ;
   FitResults : TStringList ;


procedure TAmpHistFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
                Histogram := THistogram.Create ;
                MathFunc := TMathFunc.Create ;
                MathFunc.Setup( None, ' ', ' ' ) ;
                FitResults := TStringList.Create ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(ADC) ;
                Histogram.Free ;
                MathFunc.Free ;
                FitResults.Free ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TAmpHistFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   ch,State : Integer ;
begin

     { Allocate working buffers from heap }
     HeapBuffers( Allocate ) ;

     { Make sure no more forms can be created }
     Main.mnAmplitudeHistograms.Enabled := False ;

     { Open single-channel event data file }
     if not EventFile.Open then OpenEventFile( EventFile ) ;

     { Create list of amplitude histogram options }
     UpdateHistogramOptionsList ;

     { Fill data source channel selection list }
     cbHistChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
         cbHistChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
         end ;
     cbHistChannel.ItemIndex := 0 ;
     HistChannel := Channel[cbHistChannel.ItemIndex] ;

     { Create channel state options list }
     cbChannelState.Clear ;
     State := -1 ;
     cbChannelState.Items.AddObject( 'All', TObject(State) ) ;
     State := 0 ;
     cbChannelState.Items.AddObject( 'Closed', TObject(State) ) ;
     for State := 1 to 6 do cbChannelState.Items.AddObject( format('Open (X%d)'
                                                            ,[State]),TObject(State)) ;


     { Time range to be included in histogram }
     edRange.LoValue := 0.0 ;
     edRange.LoLimit := 0.0 ;
     edRange.HiValue := CdrFH.RecordDuration ;
     edRange.HiLimit := CdrFH.RecordDuration ;

     { Number of bins in histogram }
     edNumBins.LoLimit := 2 ;
     edNumBins.HiLimit := High(Histogram.Bins)+1 ;
     edNumBins.Value := 512 ;

     { Amplitude range of histogram bins }
     edRangeLo.Value := (MinADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
     edRangeLo.Units := HistChannel.ADCUnits ;
     edRangeHi.Value := (MaxADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
     edRangeHi.Units := HistChannel.ADCUnits ;

     { Initialise display cursors }
     plPlot.ClearVerticalCursors ;
     iFitCursor0 := plPlot.AddVerticalCursor( clGray ) ;
     iFitCursor1 := plPlot.AddVerticalCursor( clGray ) ;
     iCursor := plPlot.AddVerticalCursor( clRed ) ;
     lbUnitArrow.Visible := False ;

     { Create list of curves that can be fitted to histogram }
     cbEquation.Clear ;
     cbEquation.Items.AddObject( 'None', TObject(None)) ;
     cbEquation.Items.AddObject( 'Gaussian', TObject(Gaussian)) ;
     cbEquation.Items.AddObject( '2 Gaussians', TObject(Gaussian2)) ;
     cbEquation.Items.AddObject( '3 Gaussians', TObject(Gaussian3)) ;
     { Set initial  equation to None }
     cbEquation.ItemIndex := 0 ;

     bAbort.Enabled := False ;
     Histogram.Available := False ;

     ClientHeight := HistGrp.Top + HistGrp.Height + 10 ;
     Resize ;

     end ;


procedure TAmpHistFrm.UpdateHistogramOptionsList ;
{ --------------------------------
  Create list of histogram options
  -------------------------------- }
begin
     cbHistogramType.Clear ;
     cbHistogramType.Items.AddObject( 'All Points', TObject(htAllPoints) ) ;
     if EventFile.Open and (EventFile.NumEvents > 0) then begin
        cbHistogramType.Items.AddObject( 'All Points in State',
                                         TObject(htAllPointsInState) ) ;
        cbHistogramType.Items.AddObject( 'Mean State Amplitude',
                                         TObject(htStateAverage) ) ;
        end ;
     cbHistogramType.Items.AddObject( 'Patlak Avg.', TObject(htPatlakAverage) ) ;
     cbHistogramType.Items.AddObject( 'External', TObject(htExternal) ) ;

     cbHistogramType.ItemIndex := 0 ;
     edRange.LoLimit := 0.0 ;
     edRange.HiLimit := CdrFH.RecordDuration ;
     edRange.Units := 's' ;
     HistTypePage.PageIndex := AllPointsPage ;

     end ;



procedure TAmpHistFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ ----------
  Close form
  ----------}
begin
     { Allocate working buffers from heap }
     HeapBuffers( Deallocate ) ;

     { Re-enable analysis menu option }
     Main.mnAmplitudeHistograms.Enabled := True ;

     { Disable copy and print menus }
     Main.CopyAndPrintMenus( False, False ) ;

     { Request closure and destruction of form }
     Action := caFree ;
     end;


procedure TAmpHistFrm.bDoAmpHistogramClick(Sender: TObject);
{ -----------------------------------------
  Create and plot a new amplitude histogram
  ----------------------------------------- }
var
   HistType : THistogramType ;
   x,temp,BinWidth,Sum : single ;
   i,iBin : Integer ;
begin

     bDoAmpHistogram.Enabled := False ;
     bAbort.Enabled := True ;

     Screen.Cursor := crHourGlass ;
     { Ensure all text box parameters are up to date }

     if rbAllRecords.Checked then begin
        Histogram.StartAt := 0 ;
        Histogram.EndAt := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        Histogram.StartAt := Round( edRange.LoValue/ CDRfH.dt ) ;
        Histogram.EndAt :=   Round( edRange.HiValue/ CDRfH.dt ) ;
        end ;

     { Histogram bin range }
     Histogram.NumBins := Round(edNumBins.Value) ;
     Histogram.RangeLo := edRangeLo.Value ;
     Histogram.RangeHi := edRangeHi.Value ;

     if Histogram.RangeLo > Histogram.RangeHi then begin
        Temp := Histogram.RangeLo ;
        Histogram.RangeLo := Histogram.RangeHi ;
        Histogram.RangeHi := Temp ;
        end ;

     if Histogram.RangeLo = Histogram.RangeHi then begin
        Histogram.RangeLo := (MinADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
        Histogram.RangeHi := (MaxADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
        MessageDlg( 'Upper and Lower limits must be different',mtWarning, [mbOK], 0 ) ;
        end ;

     { Get type of histogram }
     HistType := THistogramType(
                     cbHistogramType.Items.Objects[cbHistogramType.ItemIndex]) ;

    { Initialise histogram record }
    x := Histogram.RangeLo ;
    Histogram.MaxBin := Histogram.NumBins - 1 ;
    BinWidth := (Histogram.RangeHi - Histogram.RangeLo) /
                MaxInt([Histogram.NumBins,1]) ;
    for iBin := 0 to Histogram.MaxBin do begin
        Histogram.Bins[iBin].Lo := x ;
        Histogram.Bins[iBin].Hi := x + BinWidth ;
        Histogram.Bins[iBin].Mid := x + (BinWidth/2.0) ;
        Histogram.Bins[iBin].y := 0.0 ;
        x := x + BinWidth
        end ;

     { Choose appropriate histogram computation procedure }
     case HistType of
          htAllPoints : AllPointsHistogram ;
          htAllPointsInState : AllPointsInStateHistogram ;
          htStateAverage : StateAverageHistogram ;
          htPatlakAverage : PatlakAverageHistogram ;
          end ;

    { Convert to percentage }
    Sum := 0.0 ;
    for iBin := 0 to Histogram.MaxBin do Sum := Sum + Histogram.Bins[iBin].y ;
    if Sum > 0.0 then begin
       for iBin := 0 to Histogram.MaxBin do
           Histogram.Bins[iBin].y := Histogram.Bins[iBin].y*(100.0/Sum) ;
       end ;

     Histogram.NewPlot := True ;
     { Remove any fitted equation }
     MathFunc.Setup(None,' ',' ') ;

     { Standard settings for mean/variance plot }

     Screen.Cursor := crDefault ;
     { Re-enable buttons }
     bDoAmpHistogram.Enabled := True ;
     bAbort.Enabled := False ;

     { Plot new histogram }
     plPlot.xAxisAutoRange := False ;
     plPlot.xAxisMin := Histogram.Bins[0].Lo ;
     plPlot.xAxisMax := Histogram.Bins[Histogram.MaxBin].Hi ;
     plPlot.XAxisTick := (plPlot.xAxisMax - plPlot.xAxisMin) / 5.0 ;
     plPlot.yAxisAutoRange := True ;
     plPlot.xAxisLabel := HistChannel.ADCUnits ;
     plPlot.yAxisLabel := '%' ;
     plPlot.CreateHistogram( 0 ) ;

     for i := 0 to Histogram.MaxBin do plPlot.AddBin( 0,
                                       Histogram.Bins[i].Lo,
                                       Histogram.Bins[i].Mid,
                                       Histogram.Bins[i].Hi,
                                       Histogram.Bins[i].y ) ;

     { Erase any fitted line which might exist }
     cbEquation.ItemIndex := 0 ;
     plPlot.CreateLine( 1, clRed, msNone, psSolid ) ;

     // Clear results fields
     FitResults.Clear ;
     erResults.Lines.Clear ;

     { Initial cursor positions }
     i := 0 ;
     while (Histogram.Bins[i].y = 0.0) and (i < Histogram.MaxBin) do Inc(i) ;
     plPlot.VerticalCursors[iFitCursor0] := Histogram.Bins[i].Mid ;
     i := Histogram.MaxBin ;
     while (Histogram.Bins[i].y = 0.0) and (i > 0) do Dec(i) ;
     plPlot.VerticalCursors[iFitCursor1] := Histogram.Bins[i].Mid ;
     plPlot.VerticalCursors[iCursor] := Histogram.Bins[Histogram.MaxBin div 2].Mid ;

     Histogram.Available := True ;
     { Enable copy and print menu items }
     Main.CopyAndPrintMenus( True, True ) ;

    end;


procedure TAmpHistFrm.AllPointsHistogram ;
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
    pbProgress.Min := 1 ;
    pbProgress.Max := High(Histogram.EndAt) ;
    pbProgress.Position := Histogram.StartAt ;
    pbProgress.Min := Histogram.StartAt ;
    pbProgress.Max := Histogram.EndAt ;

    { Read records of data from file and add to histogram }

    if Histogram.RangeLo <> Histogram.RangeHi then
       BinScale := Histogram.NumBins/(Histogram.RangeHi - Histogram.RangeLo)
    else BinScale := 1.0 ;

    BlockPointer := Histogram.StartAt ;
    Done := False ;
    while not Done do begin

       { Read a record from file and add its data to histogram }
       NumBlocks := ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer) ;

       if NumBlocks = NumBlocksPerBuffer then begin
          j := HistChannel.ChannelOffset ;
          for i := 0 to NumBlocksPerBuffer-1 do begin
              { Get amplitude value }
              y := (ADC^[j] - HistChannel.ADCZero)*HistChannel.ADCScale ;
              { Find index of histogram bin }
              iBin := Round( (y - Histogram.RangeLo)*BinScale ) ;
              iBin := IntLimitTo( iBin, 0, Histogram.MaxBin ) ;
              { Increment bin count }
              Histogram.Bins[iBin].y := Histogram.Bins[iBin].y + 1.0 ;
              j := j + CdrFH.NumChannels ;
              end ;
          end
       else Done := True ;

       { Update progress bar }
       pbProgress.Position := BlockPointer ;
       { Increment to next record }
       BlockPointer := BlockPointer + NumBlocksPerBuffer ;
       if BlockPointer > Histogram.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bDoAmpHistogram.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbProgress.Position := pbProgress.Min ;

    end ;


procedure TAmpHistFrm.AllPointsInStateHistogram ;
{ ----------------------------
  Compute all-points histogram
  ----------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,SamplePointer,NumSamples,EventNum,NumBytesPerBuffer : Integer ;
   y,BinScale : single ;
   iBin,MarginPoints,SelectedState,NumRead : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
begin

    { Initialise progress bar }
    pbProgress.Min := 1 ;
    pbProgress.Max := High(Histogram.EndAt) ;
    pbProgress.Position := Histogram.StartAt ;
    pbProgress.Min := Histogram.StartAt ;
    pbProgress.Max := Histogram.EndAt ;

    { Get channel state to be used }
    SelectedState := Integer(cbChannelState.Items.Objects[cbChannelState.ItemIndex]) ;
    MarginPoints := Round(edMarginPoints.Value) ;

    { Read records of data from file and add to histogram }
    Done := False ;
    if Histogram.RangeLo <> Histogram.RangeHi then
       BinScale := Histogram.NumBins/(Histogram.RangeHi - Histogram.RangeLo)
    else BinScale := 1.0 ;
    EventNum := Histogram.StartAt ;
    while not Done do begin

       { Read event }
       ReadEventFromFile( EventFile, EventNum, Event ) ;
       Done := not Event.Available ;

       { Extract samples associated with event and add to histogram }
       if (not Event.Ignore)
          and ((Event.ChannelState = SelectedState) or (SelectedState < 0))
          and ((Event.EndAt - Event.StartAt +1) >= (MarginPoints*2)) then begin

          NumBytesPerBuffer := 256*CdrFH.NumChannels*2 ;
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
                 SamplePointer := HistChannel.ChannelOffset ;
                 end ;

              { Get amplitude value }
              y := (ADC^[SamplePointer] - HistChannel.ADCZero)*HistChannel.ADCScale ;
              { Find index of histogram bin }
              iBin := Round( (y - Histogram.RangeLo)*BinScale ) ;
              iBin := IntLimitTo( iBin, 0, Histogram.MaxBin ) ;
              { Increment bin count }
              Histogram.Bins[iBin].y := Histogram.Bins[iBin].y + 1.0 ;
              SamplePointer := SamplePointer + CdrFH.NumChannels ;
              if SamplePointer >= NumSamples then NewBufferNeeded := True ;
              Inc(BlockPointer) ;
              end ;
          end ;

       { Update progress bar }
       pbProgress.Position := EventNum ;
       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > Histogram.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bDoAmpHistogram.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbProgress.Position := pbProgress.Min ;

    end ;


procedure TAmpHistFrm.StateAverageHistogram ;
{ ------------------------------------------
  Compute average current amplitude by state
  ------------------------------------------}
const
     NumBlocksPerBuffer = 256 ;
var
   BlockPointer,SamplePointer,NumSamples,EventNum,NumBytesPerBuffer : Integer ;
   BinScale : single ;
   Sum,y : double ;
   iBin,MarginPoints,SelectedState,nSum : Integer ;
   Done,NewBufferNeeded : Boolean ;
   Event : TEvent ;
begin

    { Initialise progress bar }
    pbProgress.Min := 1 ;
    pbProgress.Max := High(Histogram.EndAt) ;
    pbProgress.Position := Histogram.StartAt ;
    pbProgress.Min := Histogram.StartAt ;
    pbProgress.Max := Histogram.EndAt ;

    { Get channel state to be used }
    SelectedState := Integer(cbChannelState.Items.Objects[cbChannelState.ItemIndex]) ;
    MarginPoints := Round(edMarginPoints.Value) ;

    { Read records of data from file and add to histogram }
    Done := False ;
    if Histogram.RangeLo <> Histogram.RangeHi then
       BinScale := Histogram.NumBins/(Histogram.RangeHi - Histogram.RangeLo)
    else BinScale := 1.0 ;
    EventNum := Histogram.StartAt ;
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

          NumBytesPerBuffer := 256*CdrFH.NumChannels*2 ;
          { Remove margin points }
          Event.StartAt := Event.StartAt + MarginPoints ;
          Event.EndAt := Event.EndAt - MarginPoints ;

          BlockPointer := Event.StartAt ;
          NewBufferNeeded := True ;
          while BlockPointer <= Event.EndAt do begin
              { Get A/D samples from file }
              if NewBufferNeeded then begin
                 NumSamples := ReadCDRBuffer(CdrFH,BlockPointer,ADC^,
                               NumBlocksPerBuffer)*CdrFH.NumChannels ;
                 NewBufferNeeded := False ;
                 SamplePointer := HistChannel.ChannelOffset ;
                 end ;

              { Get amplitude value }
              y := (ADC^[SamplePointer] - HistChannel.ADCZero)*HistChannel.ADCScale ;
              { Add to sum for average }
              Sum := Sum + y ;
              Inc(nSum) ;
              { Add average to histogram and end of state }
              if Blockpointer = Event.EndAt then begin
                 if nSum > 0 then y := Sum / nSum ;
                 Sum := 0.0 ;
                 nSum := 0 ;
                 { Find index of histogram bin }
                 iBin := Round( (y - Histogram.RangeLo)*BinScale ) ;
                 iBin := IntLimitTo( iBin, 0, Histogram.MaxBin ) ;
                 { Increment bin count }
                 Histogram.Bins[iBin].y := Histogram.Bins[iBin].y + 1.0 ;
                 end ;

              SamplePointer := SamplePointer + CdrFH.NumChannels ;
              if SamplePointer >= NumSamples then NewBufferNeeded := True ;
              Inc(BlockPointer) ;
              end ;
          end ;

       { Update progress bar }
       pbProgress.Position := EventNum ;
       { Increment to next record }
       Inc(EventNum) ;
       if EventNum > Histogram.EndAt then Done := True ;

       { Allow other events to be serviced }
       Application.ProcessMessages ;
       { If the New Histogram button has been re-enabled, abort the run }
       if bDoAmpHistogram.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbProgress.Position := pbProgress.Min ;

    end ;


procedure TAmpHistFrm.PatlakAverageHistogram ;
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
    pbProgress.Min := 1 ;
    pbProgress.Max := High(Histogram.EndAt) ;
    pbProgress.Position := Histogram.StartAt ;
    pbProgress.Min := Histogram.StartAt ;
    pbProgress.Max := Histogram.EndAt ;

    { Determine number of bytes to be read from file for each buffer }
    NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

    { Number of samples to average }
    NumAvg := IntLimitTo(Round(edNumPatlakAvg.Value),1,High(Ring)+1) ;
    { Variance acceptance threshold for inclusion in histogram }
    VarianceThreshold := edPatlakVarThreshold.Value ;

    if Histogram.RangeLo <> Histogram.RangeHi then
       BinScale := Histogram.NumBins/(Histogram.RangeHi - Histogram.RangeLo)
    else BinScale := 1.0 ;

    { Read records of data from file and add to histogram }
    Done := False ;
    SamplePointer := NumSamplesPerBuffer ;
    BlockPointer := Histogram.StartAt ;
    FirstBuffer := True ;
    while not Done do begin
        { Get new buffer of samples, if needed }
        if SamplePointer >= NumSamplesPerBuffer then begin
           if ReadCDRBuffer(CdrFH,BlockPointer,ADC^,NumBlocksPerBuffer)
              <> NumBlocksPerBuffer then Done := True ;
           SamplePointer := HistChannel.ChannelOffset ;
           { Update progress bar }
           pbProgress.Position := BlockPointer ;
           { Allow other events to be serviced }
           Application.ProcessMessages ;
           end ;

        if FirstBuffer then begin
           for iRing := 0 to NumAvg-1 do begin
               y := (ADC^[SamplePointer] - HistChannel.ADCZero)*HistChannel.ADCScale ;
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
           y := (ADC^[SamplePointer] - HistChannel.ADCZero)*HistChannel.ADCScale ;
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
           iBin := Round( (Avg - Histogram.RangeLo)*BinScale ) ;
           iBin := IntLimitTo( iBin, 0, Histogram.MaxBin ) ;
           { Increment bin count }
           Histogram.Bins[iBin].y := Histogram.Bins[iBin].y + 1.0 ;
           end ;

       { Increment to next block of samples }
       Inc(BlockPointer) ;
       if BlockPointer > Histogram.EndAt then Done := True ;

       { If the New Histogram button has been re-enabled, abort the run }
       if bDoAmpHistogram.Enabled then Done := True ;

       end ;

    { Initialise progress bar }
    pbProgress.Position := pbProgress.Min ;

    end ;


procedure TAmpHistFrm.CopyHistogramDataToClipBoard ;
{ ---------------------------------------------
  Copy the amplitude histogram to the clipboard
  ---------------------------------------------
  }
begin
     plPlot.CopyDataToClipboard ;
     end ;


procedure TAmpHistFrm.PrintDisplay ;
{ ----------------
  Print histogram
  --------------- }
var
   i : Integer ;  
begin
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := True ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then begin
        { Add title information to plot }
        plPlot.ClearPrinterTitle ;
        plPlot.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
        plPlot.AddPrinterTitleLine( CdrFH.IdentLine ) ;
        plPlot.AddPrinterTitleLine( 'Histogram : ' + cbHistogramType.text ) ;
        for i := 0 to FitResults.Count-1 do
            plPlot.AddPrinterTitleLine( FitResults[i] ) ;
        { Plot graph to printer }
        plPlot.Print ;
        end ;
     end ;


procedure TAmpHistFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy histogram image to clipboard as Windows metafile
  ----------------------------------------------------- }
begin
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := False ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
     end ;


procedure TAmpHistFrm.bAbortClick(Sender: TObject);
begin
     { Re-enable New Histogram button ... This signals that the
       currently active histogram generation run is to be aborted }
     bDoAmpHistogram.Enabled := True ;
     end;


procedure TAmpHistFrm.cbHistChannelChange(Sender: TObject);
begin
     HistChannel := Channel[cbHistChannel.ItemIndex] ;

     { Amplitude range of histogram bins }
     edRangeLo.Value := (MinADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
     edRangeLo.Units := HistChannel.ADCUnits ;
     edRangeHi.Value := (MaxADCValue-HistChannel.ADCZero)*HistChannel.ADCScale ;
     edRangeHi.Units := HistChannel.ADCUnits  ;

     end;


procedure TAmpHistFrm.bSpecSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plPlot ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TAmpHistFrm.bFitCurveClick(Sender: TObject);
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
        plPlot.ClearAllLines ;

        { Select type of equation to be fitted }
        MathFunc.Setup( TEqnType(cbEquation.Items.Objects[cbEquation.ItemIndex]),
                        HistChannel.ADCUnits,
                        '%')  ;
        if MathFunc.Equation = None then OK := False ;

        { Copy data into fitting array }
        if OK then begin

           nFit := 0 ;
           { Lower and upper x data limit set by display cursors }
           iStart := plPlot.FindNearestIndex( 0, iFitCursor0 ) ;
           iEnd :=   plPlot.FindNearestIndex( 0, iFitCursor1 ) ;
           for iBins := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
               FitData^.x[nFit] := Histogram.Bins[iBins].Mid ;
               FitData^.y[nFit] := Histogram.Bins[iBins].y ;
               Inc(nFit) ;
               end ;

           { Abort curve fit, if not enough data points }
           if nFit < MathFunc.NumParameters then begin
              MessageDlg( format('%d points is insufficient for fit',[nFit]),
                           mtWarning, [mbOK], 0 ) ;
              MathFunc.Setup( None, ' ',' ' ) ;
              OK := False ;
              end ;
           end ;

        { Create an initial set of guesses for parameters }
        if OK then begin
           for i := 0 to MathFunc.NumParameters-1 do
               if not MathFunc.FixedParameters[i] then begin
               MathFunc.Parameters[i] := MathFunc.InitialGuess(FitData^,nFit,i) ;
               end ;

           { Let user modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := MathFunc ;
           SetFitParsFrm.ShowModal ;
           if SetFitParsFrm.ModalResult <> mrOK then OK := False ;
           end ;

        { Fit curve using non-linear regression }
        if OK then begin
           MathFunc := SetFitParsFrm.MathFunc ;
           { Prevent FitCurve from changing parameter settings }
           MathFunc.ParametersSet := True ;
           MathFunc.UseBinWidths := False ;
           MathFunc.FitCurve( FitData^, nFit ) ;
           OK := MathFunc.GoodFit ;
           end ;

        { Plot equation on graph }
        if OK and (MathFunc.Equation <> None) then begin

           x := plPlot.xAxisMin ;
           dx := (plPlot.xAxisMax - plPlot.xAxisMin) / NumFitPoints ;
           plPlot.ShowLines := True ;
           plPlot.CreateLine( FittedLine, clRed, msNone, psSolid ) ;
           for i := 0 to NumFitPoints-1 do begin
               plPlot.AddPoint( FittedLine, x, MathFunc.Value(x) ) ;
               x := x + dx ;
               end ;

           { Save parameters and initialise gaussian component lines }
           NumComp := MathFunc.NumParameters div 3 ;
           LineNum := FittedLine ;
           for Comp := 0 to NumComp-1 do begin
               Inc(LineNum) ;
               plPlot.CreateLine( LineNum, clRed, msNone, psSolid ) ;
               ParTemp[Comp*3] := MathFunc.Parameters[Comp*3] ;
               ParTemp[Comp*3+1] := MathFunc.Parameters[Comp*3+1] ;
               ParTemp[Comp*3+2] := MathFunc.Parameters[Comp*3+2] ;
               MathFunc.Parameters[Comp*3+2] := 0.0 ;
               end ;

           { Plot each individual gaussian component }
           if NumComp > 1 then begin
              LineNum := FittedLine ;
              for Comp := 0 to NumComp-1 do begin
                  MathFunc.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
                  Inc(LineNum) ;
                  x := plPlot.xAxisMin ;
                  dx := (plPlot.xAxisMax - plPlot.xAxisMin) / NumFitPoints ;
                  for i := 0 to Histogram.NumBins-1 do begin
                      plPlot.AddPoint( LineNum, x, MathFunc.Value(x) ) ;
                      x := x + dx ;
                      end ;
                  MathFunc.Parameters[Comp*3+2] := 0.0 ;
                  end ;
              end ;

           { Restore parameters }
           for Comp := 0 to NumComp-1 do begin
               MathFunc.Parameters[Comp*3] := ParTemp[Comp*3] ;
               MathFunc.Parameters[Comp*3+1] := ParTemp[Comp*3+1] ;
               MathFunc.Parameters[Comp*3+2] := ParTemp[Comp*3+2] ;
               end ;
           end ;


        { Display results }
        FitResults.Clear ;
        if OK then begin

          case MathFunc.Equation of
              Gaussian : FitResults.Add(
                ' y(x) = (a/^!(2^sp^ss^+2))*exp(-(x-^sm)^+2/(2*^ss^+2) )') ;
              Gaussian2 : FitResults.Add(
                ' y(x) = ^sS^-i^-=^-1^-.^-.^-2 (a^-i/^!(2^sp^ss^-i^+2))*exp(-(x-^sm^-i)^+2/(2*^ss^-i^+2) )') ;
              Gaussian3 : FitResults.Add(
                ' y(x) = ^sS^-i^-=^-1^-.^-.^-3 (a^-i/^!(2^sp^ss^-i^+2))*exp(-(x-^sm^-i)^+2/(2*^ss^-i^+2) )') ;
              end ;

           { Best fit parameters and standard error }
           for i := 0 to MathFunc.NumParameters-1 do begin

               { Convert gaussian peak parameter to gaussian area }
               if ((i+1) mod 3) = 0 then begin
                  ParName := format('A^-%d',[(i div 3)+1]) ;
                  BinWidth := (Histogram.RangeHi - Histogram.RangeLo) /
                               Histogram.NumBins ;
                  Scale := (sqrt(6.2831)*MathFunc.Parameters[i-1])/BinWidth ;
                  end
               else begin
                  ParName := MathFunc.ParNames[i] ;
                  Scale := 1.0 ;
                  end ;

               if not MathFunc.FixedParameters[i] then
                  FitResults.Add( format(' %s = %.4g ^~ %.4g (sd) %s',
                                       [ParName,
                                        Scale*MathFunc.Parameters[i],
                                        Scale*MathFunc.ParameterSDs[i],
                                        MathFunc.ParUnits[i]] ) )
               else
                  { Fixed parameter }
                  FitResults.Add( format(' %s = %.4g (fixed) %s',
                                       [ParName,
                                        MathFunc.Parameters[i],
                                        MathFunc.ParUnits[i]] ) ) ;
               end ;

           { Residual standard deviation }
           FitResults.Add( format(' Residual S.D. = %.4g %s',
                                [MathFunc.ResidualSD,'%'] ) ) ;

           { Statistical degrees of freedom }
           FitResults.Add( format(' Degrees of freedom = %d ',
                                [MathFunc.DegreesOfFreedom]) );

           { No. of iterations }
           FitResults.Add( format(' No. of iterations = %d ',
                                [MathFunc.Iterations]) ) ;

           MathFunc.CopyResultsToRichEdit( FitResults, erResults ) ;
           end ;

     finally
            Dispose(FitData) ;
            end ;

     { Make sure plot is updated with changes }
     plPlot.Invalidate ;

     end ;



procedure TAmpHistFrm.cbEquationChange(Sender: TObject);
begin
     if cbEquation.ItemIndex = 0 then begin
        erResults.Lines.Clear ;
        plPlot.CreateLine( 1, clRed, msNone, psSolid ) ;
        end ;
     end;

procedure TAmpHistFrm.bCloseClick(Sender: TObject);
begin
    Close ;
     end;


procedure TAmpHistFrm.bSetZeroClick(Sender: TObject);
{ ----------------------
  Set zero current level
  ----------------------}
var
   OldZeroCurrent,ZeroCurrent,Shift : single ;
   i : Integer ;
begin
     { Calculate new zero current level (in A/D converter units) }
     OldZeroCurrent := HistChannel.ADCZero*HistChannel.ADCScale ;

     HistChannel.ADCZero := Round( (plPlot.VerticalCursors[iCursor] + OldZeroCurrent)
                                   /HistChannel.ADCScale ) ;
     { Update channel being analysed }
     Channel[cbHistChannel.ItemIndex] := HistChannel ;

     { Calculate offset needed to correct histogram bins for new zero }
     ZeroCurrent := HistChannel.ADCZero*HistChannel.ADCScale ;
     Shift := OldZeroCurrent - ZeroCurrent ;

     { Update bins }
     for i := 0 to Histogram.MaxBin do begin
         Histogram.Bins[i].Lo := Histogram.Bins[i].Lo + Shift ;
         Histogram.Bins[i].Mid := Histogram.Bins[i].Mid + Shift ;
         Histogram.Bins[i].Hi := Histogram.Bins[i].Hi + Shift ;
         end ;

     plPlot.xAxisMin := plPlot.xAxisMin + Shift ;
     PlPlot.xAxisMax := plPlot.xAxisMax + Shift ;
     { re-plot updated histogram }
     plPlot.CreateHistogram( 0 ) ;
     for i := 0 to Histogram.MaxBin do plPlot.AddBin( 0,
                                       Histogram.Bins[i].Lo,
                                       Histogram.Bins[i].Mid,
                                       Histogram.Bins[i].Hi,
                                       Histogram.Bins[i].y ) ;

     end;


procedure TAmpHistFrm.bSetUnitCurrentClick(Sender: TObject);
{ -----------------------------
  Set unitary current amplitude
  -----------------------------}
begin
     Settings.UnitCurrent := plPlot.VerticalCursors[iCursor] ;
     plPlot.Invalidate ;
     end;


procedure TAmpHistFrm.FormResize(Sender: TObject);
{ ----------------------------------------------------
  Adjust size/location of controls when window resized
  ----------------------------------------------------}
begin
     HistGrp.Height := ClientHeight - HistGrp.Top - 10 ;
     pbProgress.Top := HistGrp.Height - pbProgress.Height - 10 ;
     FitGrp.Top := HistGrp.Top + HistGrp.Height - FitGrp.Height ;

     plPlot.Height := FitGrp.Top
                         - lbFitCursor0.Height - lbFitCursor0.Height - 20 ;
     plPlot.Width := ClientWidth - plPlot.Left - 10 ;

     lbFitCursor0.Top := plPlot.Top + plPlot.Height + 2;
     lbFitCursor0.Visible := False ;
     lbFitCursor1.Top := lbFitCursor0.Top ;
     lbFitCursor1.Visible := False ;
     lbReadCursor.Top := lbFitCursor1.Top + lbFitCursor1.Height + 2 ;
     lbReadCursor.Visible := False ;
     lbArea.Visible := False ;
     shLine.Visible := False ;

     FitGrp.Width := plPlot.Width ;
     erResults.Width := FitGrp.Width - erResults.Left - 10 ;

     end;

procedure TAmpHistFrm.plPlotCursorChange(Sender: TObject);
{ ---------------------------------------
  Update labels when plot cursors change
  ---------------------------------------}
var
   Lo,Mid,Hi,y : single ;
   iStart,iEnd,i : Integer ;
   XMean,XYSum,YSum : single ;
begin

     { Set readout cursor label }
     plPlot.GetBin( 0, plPlot.FindNearestIndex(0,iCursor), Lo, Mid, Hi, y ) ;
     lbReadCursor.caption := format(' %.4g %s / %4.g %% ',
                             [Mid,HistChannel.ADCUnits,y] ) ;
     lbReadCursor.Visible := True ;
     lbReadCursor.Left := MinInt( [ plPlot.Left
                                    + plPlot.XToCanvasCoord( Mid )
                                    - (lbReadCursor.Width div 2),
                                    ClientWidth - lbReadCursor.Width]) ;

     { Set Fitting/area cursor labels }
     plPlot.GetBin( 0, plPlot.FindNearestIndex(0,iFitCursor0), Lo, Mid, Hi, y ) ;
     lbFitCursor0.Visible := True ;
     lbFitCursor0.Left := plPlot.Left + plPlot.XToCanvasCoord( Mid ) ;
     plPlot.GetBin( 0, plPlot.FindNearestIndex(0,iFitCursor1), Lo, Mid, Hi, y ) ;
     lbFitCursor1.Visible := True ;
     lbFitCursor1.Left := plPlot.Left + plPlot.XToCanvasCoord( Mid ) ;

     { Calculate histogram area between cursors }
     iStart := plPlot.FindNearestIndex( 0, iFitCursor0 ) ;
     iEnd :=   plPlot.FindNearestIndex( 0, iFitCursor1 ) ;
     YSum := 0.0 ;
     XYSum := 0.0 ;
     for i := MinInt([iStart,iEnd]) to MaxInt([iStart,iEnd]) do begin
         YSum := YSum + Histogram.Bins[i].y ;
         XYSum := XYSum + Histogram.Bins[i].y*Histogram.Bins[i].Mid ;
         end ;
     if YSum <> 0.0 then XMean := XYSum / YSum
                    else XMean := 0.0 ;

     lbArea.visible := true ;
     shline.visible := true ;
     lbArea.caption := format(' Mean= %.3g %s /Area= %.3g %% ',
                             [XMean,
                              HistChannel.ADCUnits,
                              YSum] ) ;

     { Display mean signal level and histogram % between cursors }
     Mid := (lbFitCursor0.Left + lbFitCursor1.Left) div 2 ;
     lbArea.Left := MinInt([((lbFitCursor0.Left + lbFitCursor1.Left) div 2)
                             - (lbArea.Width div 2),ClientWidth - lbArea.Width]);
     lbArea.Top := lbFitCursor0.Top ;
     lbArea.Visible := True ;

     { Place horizontal line between fit/analysis cursors }
     shLine.Top := lbArea.Top + (lbArea.Height div 2) ;
     shLine.Left := MinInt([lbFitCursor0.Left,lbFitCursor1.Left])
                    + lbFitCursor0.Width ;
     shLine.Width := MaxInt([lbFitCursor0.Left,lbFitCursor1.Left])
                     - shLine.Left - lbFitCursor0.Width ;
     shLine.Visible := True ;

     lbUnitArrow.Left := plPlot.XToCanvasCoord( Settings.UnitCurrent )
                         + plPlot.Left - 5 ;
     lbUnitArrow.Top := plPlot.YToCanvasCoord( PlPlot.YAxisMin )
                        + plPlot.Top + 10 ;
     lbUnitArrow.Visible := True ;


     end;


procedure TAmpHistFrm.cbHistogramTypeChange(Sender: TObject);
begin
     if  THistogramType(cbHistogramType.Items.Objects[cbHistogramType.ItemIndex])
         = htAllPoints then begin
        edRange.LoLimit := 0.0 ;
        edRange.HiLimit := CdrFH.RecordDuration ;
        edRange.Units := 's' ;
        HistTypePage.PageIndex := AllPointsPage ;
        end
     else if  THistogramType(cbHistogramType.Items.Objects[cbHistogramType.ItemIndex])
         = htPatlakAverage then begin
        edRange.LoLimit := 0.0 ;
        edRange.HiLimit := CdrFH.RecordDuration ;
        edRange.Units := 's' ;
        HistTypePage.PageIndex := PatlakAveragePage ;
        edPatlakVarThreshold.Units := HistChannel.ADCUnits + '^2' ;
        end
     else begin
        edRange.LoLimit := 1 ;
        edRange.HiLimit := EventFile.NumEvents ;
        edRange.Units := '' ;
        HistTypePage.PageIndex := EventHistPage ;
        end ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.hiValue := edRange.HiLimit ;
     end;

procedure TAmpHistFrm.FormActivate(Sender: TObject);
begin
     { Enable copy and print menu items }
     if Histogram.Available then Main.CopyAndPrintMenus( True, True ) ;
     end;

procedure TAmpHistFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menu items }
     Main.CopyAndPrintMenus( False, False ) ;
     end;

end.
