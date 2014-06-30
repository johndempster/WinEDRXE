unit NoiseAnal;
{ ======================================================================
  WinEDR (c) J. Dempster, University of Strathclyde, All Rights Reserved                         
  Variance / Spectral Analysis module
  Computes mean level, variance, skew for blocks of data.
  Computes power spectrum and fits Lorentzians
  Computes MEPC frequency using Segal et al method
  ======================================================================
    25/6/98 Clipboard buffer limit reduced to 31000
    29/6/98 MEPC Frequency button added (and removed from variable list)
    16/7/98 DCChannel and ACChannel information  now correctly updated when
            channel selection list box changed
    13/8/98 50Hz subtraction option added
    18/5/99
    1/9/99 ... ChangeDisplayGrid added
    7/2/00 ... Background subtraction now works correctly for variance/mean plot
    22/2/00 ... Lorentzian components now plotted on power spectrum
    24/2/00 ... time units now switch between ms/secs
    14/3/00 ... Power spectrum of a range of records now works correctly
    3/4/00 ... Power spectrum set axes now for X/Y not histogram
    9/7/01 ... Name changed to NoiseAnal
    26/2/02 ... Copy of variance and spectrum data in clipboard now works correctly
    9/3/02 ... Amplitude histogram page added
               Progress now reported on status bar
    28/4/03 ... Number of records is now limited to available buffer capacity           
    24.6.03 .... No. horizontal/vertical grid lines changeable
    23.1.04 .... 50 Hz peak removal now enhanced to allow removal of peaks
                 at any frequency and its harmonics
    18.11.05 ... Cursors updated
    27.10.09 ... Memory violation errors when copying large plots to clipboard fixed
    11.01.10 ... No. bins, bin range and width can be set on amplitude histogram
    13.08.12 ... Plots .MaxPointsPerLine now set to include no.of points in fitted line
    21.08.12 ... No. Freqs averaged box now invisible when no averaging
    09.11.12 ... Record Overlap option now works.
                 Record time added to Records panel
                 Absolute time now show on display
                 Display no longer goes into loop setting zero levels AC=DC channel
    }


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, TabNotBk, ClipBrd,
  global, shared, maths, Grids, setfitpa, printers, fileio,
  setblock, ComCtrls, RangeEdit, ValEdit, ScopeDisplay, XYPlotDisplay,
  ValidatedEdit, math  ;

const
     MaxRecordSize = 8192 ;
     MaxRecords = High(TSingleArray) + 1 ;
type

    TRecType = ( Background,Test) ;
    TRecordStatus = record
                  Valid : boolean ;
                  RecType : TRecType ;
                  end ;
    TRecordStatusArray = Array[0..High(TSingleArray)] of TRecordStatus ;
    TRecordStatusFileOp = ( rsfLoad, rsfSave ) ;

   TSpectrum = record
              RecordNum : Integer ;
              RecordSize : Integer ;
              StartAt : Integer ;
              EndAt : Integer ;
              NumAveraged : Integer ;
              NumPoints : Integer ;
              Available : Boolean ;
              Frequency : Array[0..(MaxRecordSize div 2)+1] of single ;
              Power : Array[0..(MaxRecordSize div 2)+1] of single ;
              Variance : single ;
              AvgDCMean : single ;
              MedianFrequency : single ;
              NewPlot : Boolean ;
              end ;

  TNoiseAnalFrm = class(TForm)
    Page: TPageControl;
    DataTab: TTabSheet;
    VarianceTab: TTabSheet;
    SpectrumTab: TTabSheet;
    ControlGrp: TGroupBox;
    Label10: TLabel;
    sbRecord: TScrollBar;
    ckRejected: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label12: TLabel;
    cbACChannel: TComboBox;
    cbDCChannel: TComboBox;
    edRecordSize: TValidatedEdit;
    edRecordOverlap: TValidatedEdit;
    cbRecType: TComboBox;
    bSetRecordState: TButton;
    scDisplay: TScopeDisplay;
    DataGrp: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    edMean: TEdit;
    edVariance: TEdit;
    VarGrp: TGroupBox;
    GroupBox5: TGroupBox;
    Label9: TLabel;
    Label8: TLabel;
    cbVarYAxis: TComboBox;
    cbVarXAxis: TComboBox;
    bDoVariance: TButton;
    GroupBox6: TGroupBox;
    rbVarAllRecords: TRadioButton;
    rbVarRange: TRadioButton;
    edVarRange: TRangeEdit;
    bVarSetAxes: TButton;
    GroupBox12: TGroupBox;
    ckVarSubtractBackground: TCheckBox;
    VarFitGrp: TGroupBox;
    bVarFit: TButton;
    cbVarEquation: TComboBox;
    specGrp: TGroupBox;
    GroupBox8: TGroupBox;
    rbNoWindow: TRadioButton;
    rbCosineWindow: TRadioButton;
    bDoSpectrum: TButton;
    GroupBox9: TGroupBox;
    rbSpecAllRecords: TRadioButton;
    rbSpecRange: TRadioButton;
    edSpecRange: TRangeEdit;
    bSpecSetAxes: TButton;
    GroupBox10: TGroupBox;
    rbNoFreqAveraging: TRadioButton;
    rbLogFreqAveraging: TRadioButton;
    rbLinFreqAveraging: TRadioButton;
    GroupBox11: TGroupBox;
    ckSubtractTrends: TCheckBox;
    ckSpecSubtractBackground: TCheckBox;
    ckRemoveHarmonics: TCheckBox;
    SpecFitGrp: TGroupBox;
    bFitLorentzian: TButton;
    cbSpecEquation: TComboBox;
    lbSpecFit0: TLabel;
    lbSpecFit1: TLabel;
    plSpecPlot: TXYPlotDisplay;
    erSpecResults: TRichEdit;
    erVarResults: TRichEdit;
    plVarPlot: TXYPlotDisplay;
    edRecord: TRangeEdit;
    shLine: TShape;
    lbArea: TLabel;
    bMEPCFrequency: TButton;
    AmpHistTab: TTabSheet;
    AmpHistGrp: TGroupBox;
    bNewAmpHist: TButton;
    GroupBox1: TGroupBox;
    rbAmpHistAllRecords: TRadioButton;
    rbAmpRange: TRadioButton;
    edAmpHistRange: TRangeEdit;
    bAmpHistSetAxes: TButton;
    plAmpHist: TXYPlotDisplay;
    ChanGrp: TGroupBox;
    rbACChannel: TRadioButton;
    rbDCChannel: TRadioButton;
    GroupBox4: TGroupBox;
    bSetZero: TButton;
    edBaseFrequency: TValidatedEdit;
    Label4: TLabel;
    bSaveToLog: TButton;
    bSaveToLogSpectrum: TButton;
    BinRangePanel: TPanel;
    Label13: TLabel;
    Label19: TLabel;
    Label22: TLabel;
    edBinWidth: TValidatedEdit;
    edBinsLower: TValidatedEdit;
    edBinsUpper: TValidatedEdit;
    edNumBins: TValidatedEdit;
    Label17: TLabel;
    panNumFreqAveraged: TPanel;
    Label11: TLabel;
    edNumFreqAveraged: TValidatedEdit;
    GroupBox3: TGroupBox;
    ckFixedZeroLevels: TCheckBox;
    Label5: TLabel;
    EdTime: TEdit;
    procedure FormShow(Sender: TObject);
    procedure edRecordSizeKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbRecordChange(Sender: TObject);
    procedure bDoVarianceClick(Sender: TObject);
    procedure bDoSpectrumClick(Sender: TObject);
    procedure bSpecSetAxesClick(Sender: TObject);
    procedure bVarSetAxesClick(Sender: TObject);
    procedure cbRecTypeChange(Sender: TObject);
    procedure ckRejectedClick(Sender: TObject);
    procedure edRecordKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure bFitLorentzianClick(Sender: TObject);
    procedure bVarFitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbRecordOverlapDownClick(Sender: TObject);
    procedure sbRecordOverlapUpClick(Sender: TObject);
    procedure bMEPCFrequencyClick(Sender: TObject);
    procedure cbDCChannelChange(Sender: TObject);
    procedure bSetRecordStateClick(Sender: TObject);
    procedure cbACChannelChange(Sender: TObject);
    procedure plSpecPlotCursorChange(Sender: TObject);
    procedure PageChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure bNewAmpHistClick(Sender: TObject);
    procedure bAmpHistSetAxesClick(Sender: TObject);
    procedure bSetZeroClick(Sender: TObject);
    procedure bSaveToLogClick(Sender: TObject);
    procedure bSaveToLogSpectrumClick(Sender: TObject);
    procedure edBinsUpperKeyPress(Sender: TObject; var Key: Char);
    procedure edBinsLowerKeyPress(Sender: TObject; var Key: Char);
    procedure edBinWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edNumBinsKeyPress(Sender: TObject; var Key: Char);
    procedure rbNoFreqAveragingClick(Sender: TObject);
    procedure rbLogFreqAveragingClick(Sender: TObject);
    procedure rbLinFreqAveragingClick(Sender: TObject);
    procedure ckFixedZeroLevelsClick(Sender: TObject);
    procedure scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edRecordOverlapKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure SetRecordSize( NewSize : Integer ; SizeChange : Boolean ) ;
    procedure InitialiseDisplay(
              scDisplay : TScopeDisplay   { Drawing area }
              ) ;
    procedure GetRecord ;
    procedure DisplayMeanAndVariance ;
    procedure ReadRecord( Rec : Integer ;
                           var Buf : Array of SmallInt ;
                           var xMin,xMax : single ) ;
    procedure CopyRecordToClipBoard ;
    function GetVariable( ItemIndex, Rec : Integer ) : single ;
    function GetVariableUnits( ItemIndex : Integer ) : string ;

    procedure CopyVarianceDataToClipboard ;

    procedure ComputePowerSpectrum( RecType : TRecType ;
                                    StartAt,EndAt : Integer ;
                                    var Spectrum : TSpectrum ) ;
    procedure AverageFrequencies( var Spectrum : TSpectrum ) ;
    procedure SubtractLinearTrend( var Y : Array of single ;
                                   iStart,iEnd : Integer ) ;
    procedure RemoveHarmonics( var Spectrum : TSpectrum ) ;

    procedure CosineWindow( iStart,iEnd : Integer ;
                            var Y : Array of single ;
                            var VarianceCorrection : single ) ;
    procedure SpectralVariance( var Spectrum : TSpectrum ) ;

    procedure CopySpectrumDataToClipBoard ;

    procedure RecordStatusFile(
              Operation : TRecordStatusFileOp
              ) ;


  public
    { Public declarations }
    procedure PrintDisplay ;
    procedure CopyDataToClipboard ;
    procedure CopyImageToClipboard ;
    procedure SetCopyAndPrintMenus ;
    procedure ZoomOutAll ;
    procedure ZoomIn( Chan : Integer ) ;
    procedure ZoomOut( Chan : Integer ) ;
    procedure ChangeDisplayGrid ;
  end;

var
  NoiseAnalFrm: TNoiseAnalFrm;

implementation

uses mdiform,setaxes, MEPCFreq, Printgra, Printrec , Zero, ViewSig;

const
     RecordLimit = 16383 ;
     MinRecordSize = 32 ;
     NumFitPoints = 500 ;

     vRecordNum = 0 ;
     vTime = 1 ;
     vMeanDC = 2 ;
     vStDev = 3 ;
     vVariance = 4 ;
     vSkew = 5 ;
     vMedianFrequency = 6 ;

     DataPage = 0 ;
     VariancePage = 1 ;
     SpectrumPage = 2 ;
     VarDataLine = 0 ;
     VarFitLine = 1 ;
     SpecDataLine = 0 ;
     SpecBackLine = 1 ;
     SpecFitLine = 2 ;
     SpecFitLorentzian1 = 3 ;
     SpecFitLorentzian2 = 4 ;

     MaxBins = 4096 ;
type
    TData = record
          RecordNum : Integer ;
          RecordSize : Integer ;
          RecordOffset : Integer ;
          RecordOverlap : Integer ;
          MaxRecord : Integer ;
          end ;

    TVariance = record
              RecordNum : Integer ;
              RecordSize : Integer ;
              StartAt : Integer ;
              EndAt : Integer ;
              nPoints : Integer ;
              Available : Boolean ;
              NewPlot : Boolean ;
              end ;
TCursors = record
         DCZero : Integer ;
         ACZero : Integer ;
         Fit0 : Integer ;
         Fit1 : Integer ;
         Read : Integer ;
         end ;

var
   DCChan : Integer ; // DC Channel number
   ACChan : Integer ; // AC Channel number
   Data : TData ; { Data description record }
   { Power spectral data arrays }
   PowerSpectrum : ^TSpectrum ;
   BackgroundSpectrum : ^TSpectrum ;
   TempSpectrum : ^TSpectrum ;
   Variance : TVariance ; { Variance analysis control record }
   ADC : ^TSmallIntArray ; { A/D sample data array }
   DCMean : ^TSingleArray ; { DC (mean) signal level array }
   ACVariance : ^TSingleArray ; { AC channel signal variance array }
   ACSkew : ^TSingleArray ; { AC channel signal skew array }
   MedFreq : ^TSingleArray ; { AC channel median power frequency }
   RecordStatus : ^TRecordStatusArray ;
   VarFunc : TMathFunc ;
   VarCursors : TCursors ;

   SpecFunc : TMathFunc ;
   SpecCursors : TCursors ;
   SpecResults : TStringList ;
   VarResults : TStringList ;

   BuffersAllocated : Boolean ;

   {$R *.DFM}


procedure TNoiseAnalFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
                New(DCMean) ;
                New(ACVariance) ;
                New(ACSkew) ;
                New(MedFreq) ;
                New(PowerSpectrum) ;
                New(BackgroundSpectrum) ;
                New(TempSpectrum) ;
                New(RecordStatus) ;
                VarFunc := TMathFunc.Create ;
                VarFunc.Setup( None, ' ', ' ' ) ;
                SpecFunc := TMathFunc.Create ;
                SpecFunc.Setup( None, ' ', ' ' ) ;
                SpecResults := TStringList.Create ;
                VarResults := TStringList.Create ;                

                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(ADC) ;
                Dispose(DCMean) ;
                Dispose(ACVariance) ;
                Dispose(ACSkew) ;
                Dispose(MedFreq) ;
                Dispose(PowerSpectrum) ;
                Dispose(BackgroundSpectrum) ;
                Dispose(TempSpectrum) ;
                Dispose(RecordStatus) ;
                VarFunc.Free ;
                SpecFunc.Free ;
                VarResults.Free ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TNoiseAnalFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   ch : Integer ;
begin

     { Allocate working buffers from heap }
     HeapBuffers( Allocate ) ;

     { Open file which hold Valid/Type of variance records }
     RecordStatusFile( rsfLoad ) ;

     { Fill AC and DC channel selection lists }
     cbACChannel.Clear ;
     cbDCChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
          cbACChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
          cbDCChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
          end ;
     cbACChannel.ItemIndex := 0 ;
     cbDCChannel.ItemIndex := Min(1,CdrFH.NumChannels-1) ;
     ACChan := cbACChannel.ItemIndex ;
     DCChan := cbDCChannel.ItemIndex ;

     { Set size of variance/FFT record }
     Data.RecordOverlap := Settings.Variance.RecordOverlap ;
     Data.RecordSize := Settings.Variance.RecordSize ;
     SetRecordSize( Data.RecordSize, False ) ;

     { Initial axis variable selections for variance plot }
     cbVarXAxis.ItemIndex := vRecord ;
     cbVarYAxis.ItemIndex := vVariance ;

     sbRecord.Position := 0 ;
     Data.RecordNum := sbRecord.Position ;

     { Initialise amplitude histogram cursors }
     plAmpHist.ClearVerticalCursors ;
     plAmpHist.AddVerticalCursor( clGreen, '?r' ,0) ;

     { Initialise variance plot display cursors }
     plVarPlot.ClearVerticalCursors ;
     VarCursors.Fit0 := plVarPlot.AddVerticalCursor( clGray, '',0 ) ;
     VarCursors.Fit1 := plVarPlot.AddVerticalCursor( clGray, '',0 ) ;
     plVarPlot.LinkVerticalCursors(VarCursors.Fit0,VarCursors.Fit1) ;

     VarCursors.Read := plVarPlot.AddVerticalCursor( clGreen, '?r',0 ) ;

     { Initialise power spectrum plot display cursors }
     plSpecPlot.ClearVerticalCursors ;
     SpecCursors.Fit0 := plSpecPlot.AddVerticalCursor( clGray, 'f',0 ) ;
     SpecCursors.Fit1 := plSpecPlot.AddVerticalCursor( clGray, 'f',0 ) ;

     SpecCursors.Read := plSpecPlot.AddVerticalCursor( clGreen, '?r',0 ) ;
     lbSpecFit0.Visible := False ;
     lbSpecFit1.Visible := False ;
     shLine.Visible := False ;
     lbArea.Visible := False ;

     { Create list of curves that can be fitted to x/y plots }
     cbVarEquation.Clear ;
     cbVarEquation.Items.AddObject( 'None', TObject(None)) ;
     cbVarEquation.Items.AddObject( 'Linear', TObject(Linear)) ;
     cbVarEquation.Items.AddObject( 'Parabola', TObject(Parabola)) ;
     cbVarEquation.Items.AddObject( 'Exponential', TObject(Exponential)) ;
     cbVarEquation.ItemIndex := 0 ;

     { Create list of curves that can be fitted to spectrum }
     cbSpecEquation.Clear ;
     cbSpecEquation.Items.AddObject( 'None', TObject(None)) ;
     cbSpecEquation.Items.AddObject( 'Lorentzian', TObject(Lorentzian)) ;
     cbSpecEquation.Items.AddObject( 'Lorentzian(X2)', TObject(Lorentzian2)) ;
     cbSpecEquation.Items.AddObject( 'Lor. + 1/f ', TObject(LorAndOneOverF)) ;
     cbSpecEquation.Items.AddObject( 'MEPC Noise ', TObject(MEPCNoise)) ;
     { Set initial power spectrum equation to None }
     cbSpecEquation.ItemIndex := 0 ;

     bMEPCFrequency.Enabled := False ;

     if rbNoFreqAveraging.Checked then panNumFreqAveraged.Visible := False
                                  else panNumFreqAveraged.Visible := True ;

     Resize ;

     // Start on Variance Records page
     Page.ActivePage := DataTab ;

     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;

     GetRecord ;

     BinRangePanel.Visible := False ;

     end;


procedure TNoiseAnalFrm.CopyDataToClipboard ;
{ -----------------------------------------------------------
  Copy the data in currently displayed graph to the clipboard
  -----------------------------------------------------------}
begin
     if (Page.ActivePage = VarianceTab) and Variance.Available then begin
        { Variance/mean plot }
        CopyVarianceDataToClipboard ;
        end
     else if (Page.ActivePage = AmpHistTab) and plAmpHist.Available then begin
        { amplitude histogram plot }
        plAmpHist.CopyDataToClipboard ;
        end
     else if (Page.ActivePage = SpectrumTab)
        and PowerSpectrum^.Available then begin
        CopySpectrumDataToClipboard ;
        end
     else CopyRecordToClipboard ;
     end ;



{ *******************************************************
  Data record display procedures
  *******************************************************}


procedure TNoiseAnalFrm.InitialiseDisplay(
          scDisplay : TScopeDisplay   { Drawing area }
          ) ;
{ ------------------------------------------------
  Initialise AC & DC display to selected channels
  ------------------------------------------------}
var
   ch : integer ;
begin
     { Continuous record display channel }
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.MaxPoints := Data.RecordSize ;
     scDisplay.NumPoints := scDisplay.MaxPoints ;
     scDisplay.NumChannels := CdrFH.NumChannels ;
     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.NumPoints  ;
     scDisplay.DisableChannelVisibilityButton := True ;

     { Set channel information }
     scDisplay.ClearHorizontalCursors ;
     for ch := 0 to CdrFH.NumChannels-1 do begin
         scDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDisplay.ChanName[ch] := Channel[ch].ADCName ;
         scDisplay.ChanScale[ch] := Channel[ch].ADCScale ;
         scDisplay.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDisplay.ChanZero[ch] := Channel[ch].ADCZero ;
         scDisplay.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scDisplay.yMin[ch] := Channel[ch].yMin ;
         scDisplay.yMax[ch] := Channel[ch].yMax ;
         scDisplay.ChanVisible[ch] := False ;
         scDisplay.ChanColor[ch] := clBlue ;
         scDisplay.AddHorizontalCursor(ch,clRed,True,'z') ;
         end ;

     scDisplay.ClearVerticalCursors ;
     scDisplay.AddVerticalCursor(AllChannels, clGreen, '?y?t') ;

     { Make selected AC and DC channels visible }
     scDisplay.ChanVisible[cbACChannel.ItemIndex] := True ;
     scDisplay.ChanName[cbACChannel.ItemIndex] := scDisplay.ChanName[ACChan] + '(AC)';
     scDisplay.ChanVisible[DCChan] := True ;
     scDisplay.ChanName[DCChan] := scDisplay.ChanName[DCChan] + '(DC)';

     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits + ' ' ;

     end ;


procedure TNoiseAnalFrm.GetRecord ;
{ -----------------------------------------
  Get A/D samples for record from data file
  -----------------------------------------}
var
  Time : Double ;  
begin

    edRecord.LoValue := Data.RecordNum + 1 ;
    edRecord.HiValue := Data.MaxRecord + 1 ;

    ReadRecord( Data.RecordNum, ADC^, Channel[ACChan].xMin, Channel[ACChan].xMax ) ;
    { Set Min/Max limits of DC axis }

    DisplayMeanAndVariance ;

    { Record accepted/rejected status }
    ckRejected.Checked := not RecordStatus^[Data.RecordNum].Valid ;
    { Type of record }
    if RecordStatus^[Data.RecordNum].RecType = Background then cbRecType.ItemIndex := 1
                                                          else cbRecType.ItemIndex := 0 ;

    Time := Data.RecordNum*Data.RecordOffset*CDRFH.dt ;
    scDisplay.xOffset := Round(Time/CDRFH.dt) ;
    edTime.Text := format('%.4g s',[Time]);
    { Re-plot channels }

    scDisplay.SetDataBuf( ADC ) ;
    scDisplay.Invalidate ;

    end ;


procedure TNoiseAnalFrm.DisplayMeanAndVariance ;
{ ----------------------------------------------
  Calculate mean and variance of variance record
  ---------------------------------------------- }
var
   Sum,MeanAC,MeanDC,VarianceAC : single ;
   i,j : Integer ;
begin

    { Calculate mean signal level of DC Channel }
    Sum := 0.0 ;
    j := Channel[DCChan].ChannelOffset ;
    for i := 0 to Data.RecordSize-1 do begin
        Sum := Sum + (ADC^[j] - Channel[DCChan].ADCZero) ;
        j := j + CdrFH.NumChannels ;
        end ;

    MeanDC := (Sum/Data.RecordSize) * Channel[DCChan].ADCScale ;
    edMean.text := format( ' %.4g %s',[MeanDC,Channel[DCChan].ADCUnits] ) ;

    { Calculate signal variance of AC Channel }
    { First, calculate mean level }
    Sum := 0.0 ;
    j := Channel[ACChan].ChannelOffset ;
    for i := 0 to Data.RecordSize-1 do begin
        Sum := Sum + (ADC^[j]) ;
        j := j + CdrFH.NumChannels ;
        end ;
    MeanAC := Sum / Data.RecordSize ;

    // Update AC channel zero level if it is difference from DC channel
    if DCChan <> ACChan then Channel[ACChan].ADCZero := Round(MeanAC) ;

    { Next, calculate variance }
    Sum := 0.0 ;
    j := Channel[ACChan].ChannelOffset ;
    for i := 0 to Data.RecordSize-1 do begin
        Sum := Sum + (ADC^[j] - MeanAC)*(ADC^[j] - MeanAC) ;
        j := j + CdrFH.NumChannels ;
        end ;
    VarianceAC := (Sum/Data.RecordSize)*Channel[ACChan].ADCScale*
                  Channel[ACChan].ADCScale ;
    edVariance.text := format( ' %.4g %s^2', [VarianceAC,Channel[DCChan].ADCUnits] ) ;
    end ;


procedure TNoiseAnalFrm.ReadRecord(
          Rec : Integer ;
          var Buf : Array of SmallInt ;
          var xMin,xMax : single
          ) ;
{ --------------------------------------
  Read record from A/D sample data file
  -------------------------------------}
begin
    { Read data record }
    ReadCDRBuffer(CdrFH,Rec*Data.RecordOffset,Buf,Data.RecordSize) ;
    xMin := Rec*Data.RecordOffset ;
    xMax := xMin + Data.RecordSize ;
    end ;


procedure TNoiseAnalFrm.SetRecordSize(
          NewSize : Integer ;
          SizeChange : Boolean
          ) ;
{ -------------------------------
  Set size of variance/FFT record
  -------------------------------}
var
   n,Rec,NumRecords : Integer ;
   QuarterRecord,OldMaxRecord : Integer ;
   Src,SrcStep,Dest,DestStep : single ;
   OldRecordStatus : ^TRecordStatusArray ;
begin

     { Ensure record size is a power of 2 and within valid limits }
     n := MinRecordSize div 2 ;
     repeat
       n := n*2 ;
       // Determine number of records in data file
       NumRecords := CdrFH.NumSamplesInFile div (n*CdrFH.NumChannels) ;
       until ((n >= NewSize) or (n = MaxRecordSize) or (NumRecords < 4))
       and (NumRecords < MaxRecords) ;

     { Set record size }
     edRecordSize.Value := n ;
     PowerSpectrum^.RecordSize := n ;
     BackgroundSpectrum^.RecordSize := n ;
     Data.RecordSize := n ;

     OldMaxRecord := Data.MaxRecord ;

     { Keep record overlapping with limits }
     if (Data.RecordOverlap < 0) or  (Data.RecordOverlap > 3)then Data.RecordOverlap := 0 ;

     { Calculate record offset to provide required degree of overlap }
     QuarterRecord := Data.RecordSize div 4 ;
     Data.RecordOffset := Data.RecordSize - (Data.RecordOverlap*QuarterRecord) ;
     { Increase number of records to account for record overlapping }
     NumRecords := (NumRecords*Data.RecordSize) div Data.RecordOffset ;

     edRecordOverlap.Value := Data.RecordOverlap*25.0 ;

     Data.MaxRecord := NumRecords - 1 ;

     { Warn user if working limits exceeded }
     if Data.MaxRecord > High(TRecordStatusArray) then begin
        Main.StatusBar.SimpleText :=
        format( ' Record storage capacity exceeded (%d)',
                            [High(TRecordStatusArray)]) ;
        end ;

     sbRecord.Max := Data.MaxRecord ;
     sbRecord.Position := 0 ;

     { Set range of records for variance and spectra }
     edVarRange.LoLimit := 1 ;
     edVarRange.LoValue := 1 ;
     edVarRange.HiLimit := Data.MaxRecord+1 ;
     edVarRange.HiValue := Data.MaxRecord+1 ;

     edSpecRange.LoLimit := 1 ;
     edSpecRange.LoValue := 1 ;
     edSpecRange.HiLimit := Data.MaxRecord+1 ;
     edSpecRange.HiValue := Data.MaxRecord+1 ;

{    Change record type / rejected array if record size/overlap changed }

     if SizeChange then begin

        New(OldRecordStatus) ;

        try

           { Save old record status data }
           for Rec := 0 to OldMaxRecord do
               OldRecordStatus^[Rec] := RecordStatus^[Rec] ;

           Src := 0.0 ;
           Dest := 0.0 ;
           if Data.MaxRecord > OldMaxRecord then begin
              SrcStep := OldMaxRecord / Data.MaxRecord ;
              DestStep := 1.0 ;
              end
           else begin
              SrcStep := 1.0 ;
              DestStep := Data.MaxRecord / OldMaxRecord ;
              end ;

           for Rec := 0 to Max(Data.MaxRecord,OldMaxRecord) do begin
               RecordStatus^[Round(Dest)] := OldRecordStatus^[Round(Src)] ;
               Src := Src + SrcStep ;
               Dest := Dest + DestStep ;
               end ;

        finally
           Dispose(OldRecordStatus) ;
           end ;
        end ;

     { Reinitialise display }
     InitialiseDisplay( scDisplay ) ;

     end ;


procedure TNoiseAnalFrm.edRecordSizeKeyPress(Sender: TObject; var Key: Char);
{ ---------------------------------
  Set size of variance/FFT record
  ---------------------------------}
begin
     if key = #13 then begin
        Data.RecordSize := Round( edRecordSize.Value ) ;
        SetRecordSize( Data.RecordSize,True ) ;
        InitialiseDisplay( scDisplay ) ;
        GetRecord ;
        end ;
     end;


procedure TNoiseAnalFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin

     { Save record status data to file }
     RecordStatusFile( rsfSave ) ;

     Settings.Variance.RecordSize := Data.RecordSize ;
     Settings.Variance.RecordOverlap := Data.RecordOverlap ;
     SaveCDRHeader( cdrFH ) ;

     HeapBuffers( Deallocate ) ;

     { Disable menus }
     Main.CopyAndPrintMenus( False, False ) ;

     Screen.Cursor := crDefault ;

     Action := caFree ;
     end;


procedure TNoiseAnalFrm.sbRecordChange(Sender: TObject);
{ -----------------------------------------------
  Data record slider bar changed - Update display
  -----------------------------------------------}
begin
     Data.RecordNum := sbRecord.Position ;
     GetRecord ;
     end;


procedure TNoiseAnalFrm.cbRecTypeChange(Sender: TObject);
{ ---------------------
  Change type of record
  ---------------------}
begin
     if cbRecType.ItemIndex = 0 then RecordStatus^[Data.RecordNum].RecType := Test
                                else RecordStatus^[Data.RecordNum].RecType := Background ;
     end ;


procedure TNoiseAnalFrm.ckRejectedClick(Sender: TObject);
{ -----------------------------------------
  Change accepted/rejected status of record
  -----------------------------------------}
begin
     RecordStatus^[Data.RecordNum].Valid := not ckRejected.Checked ;
     end ;


procedure TNoiseAnalFrm.edRecordKeyPress(Sender: TObject; var Key: Char);
{ -----------------------------------------
  Let user enter the record number directly
  -----------------------------------------}
begin
     if key = chr(13) then begin
        sbRecord.Position := Round(edRecord.LoValue)-1 ;
        GetRecord ;
        end ;
     end;


procedure TNoiseAnalFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{ --------------------------------
  Process special accelerator keys
  --------------------------------}
begin
     case key of
          VK_SUBTRACT : begin { - key }
                if sbRecord.Position >= 0 then begin
                   sbRecord.Position := sbRecord.Position - 1 ;
                   GetRecord ;
                   end ;
                end ;
          VK_ADD : begin { + key }
                if sbRecord.Position < Data.MaxRecord then begin
                   sbRecord.Position := sbRecord.Position + 1 ;
                   GetRecord ;
                   end ;
                end ;
          $54,$4c,$45,$4d,$46 : begin
              { if (Shift = [ssCtrl]) then Action := NewRecordType ;}
               end ;
          $52 : begin
               if (Shift = [ssCtrl]) then begin
                   ckRejected.Checked := not ckRejected.Checked ;
                   end ;
               end ;
          else ;
          end ;
     end;


{ ***********************************************************************
  Variance/mean plot procedures
  ***********************************************************************}

procedure TNoiseAnalFrm.bDoVarianceClick(Sender: TObject);
{ ------------------------------------------
  Calculate record DC means and AC variances
  ------------------------------------------}
var
   Rec,i,j,nAvg : Integer ;
   Sum,Sum2,Sum3,MeanAC,xMin,xMax,x,y,Average : single ;
   SumMean,SumVar,AvgBackgroundMean,AvgBackgroundVar : single ;
begin

     if rbVarAllRecords.Checked then begin
        { Use all records }
        Variance.StartAt := 0 ;
        Variance.EndAt := Data.MaxRecord ;
        end
     else begin
        { Use selected range of records }
        Variance.StartAt := Round(edVarRange.LoValue)-1 ;
        Variance.EndAt :=   Round(edVarRange.HiValue)-1 ;
        end ;

     Variance.Available := False ;
     { Initialise progress bar }

     for Rec := Variance.StartAt to Variance.EndAt do
         if RecordStatus^[Rec].Valid then begin

         { Read record from file }
         ReadRecord(Rec, ADC^, xMin, xMax ) ;

         { Calculate mean signal level of DC Channel }
         Sum := 0.0 ;
         j := Channel[DCChan].ChannelOffset ;
         for i := 0 to Data.RecordSize-1 do begin
             Sum := Sum + (ADC^[j] - Channel[DCChan].ADCZero) ;
             j := j + CdrFH.NumChannels ;
             end ;
         DCMean^[Rec] := (Sum/Data.RecordSize) * Channel[DCChan].ADCScale ;

         { Calculate signal variance of AC Channel }
         { First, calculate mean level }
         Sum := 0.0 ;
         j := Channel[ACChan].ChannelOffset ;
         for i := 0 to Data.RecordSize-1 do begin
             Sum := Sum + (ADC^[j]) ;
             j := j + CdrFH.NumChannels ;
             end ;
         MeanAC := Sum / Data.RecordSize ;

         // Update AC channel zero level (if AC channel is different from DC)
         if DCChan <> ACChan then Channel[ACChan].ADCZero := Round(MeanAC) ;

         { Next, calculate variance and skew }
         Sum2 := 0.0 ;
         Sum3 := 0.0 ;
         j := Channel[ACChan].ChannelOffset ;
         for i := 0 to Data.RecordSize-1 do begin
             y := (ADC^[j] - MeanAC) ;
             Sum2 := Sum2 + y*y ;
             Sum3 := Sum3 + y*y*y ;
             j := j + CdrFH.NumChannels ;
             end ;
         ACVariance^[Rec] :=
         (Sum2/Data.RecordSize)*Channel[ACChan].ADCScale*Channel[ACChan].ADCScale ;
         ACSkew^[Rec] := (Sum3/Data.RecordSize)*Channel[ACChan].ADCScale
                          *Channel[ACChan].ADCScale*Channel[ACChan].ADCScale ;

         { Compute the median power frequency of the fluctuations }
         ComputePowerSpectrum( Test, Rec, Rec, TempSpectrum^ ) ;
         MedFreq^[Rec] := TempSpectrum^.MedianFrequency ;

         Main.StatusBar.SimpleText := format(
         ' Noise Analysis (Variance Analysis) : %d/%d',
         [Rec+1,Data.MaxRecord+1]) ;

         application.ProcessMessages ;

         Variance.Available := True ;
         end ;

     { Compute and subtract background AC variance & DC mean signals }

     if ckVarSubtractBackground.checked then begin
        nAvg := 0 ;
        SumMean := 0.0 ;
        SumVar := 0.0 ;
        for Rec := 0 to Data.MaxRecord do
            if (RecordStatus^[Rec].Valid)
               and (RecordStatus^[Rec].RecType = Background) then begin
               SumMean := SumMean + DCMean^[Rec] ;
               SumVar := Sumvar + ACVariance^[Rec] ;
               Inc(nAvg) ;
               end ;
        { Subtract mean background values }
        if nAvg > 0 then begin
           AvgBackgroundMean := SumMean / nAvg ;
           AvgBackgroundVar := SumVar / nAvg ;
           for Rec := Variance.StartAt to Variance.EndAt do
               if (RecordStatus^[Rec].Valid)
                  and (RecordStatus^[Rec].RecType = Test) then begin
                  DCMean^[Rec] := DCMean^[Rec] - AvgBackgroundMean ;
                  ACVariance^[Rec]  := ACVariance^[Rec] - AvgBackgroundVar ;
                  end ;
           end ;
        end ;

     { Calculate average of Y variable }
     nAvg := 0 ;
     Sum := 0.0 ;
     for Rec := Variance.StartAt to Variance.EndAt do
         if RecordStatus^[Rec].Valid and (RecordStatus^[Rec].RecType = Test) then begin
            Sum := Sum + GetVariable( cbVarYAxis.ItemIndex, Rec ) ;
            Inc(nAvg) ;
            end ;
     Average := Sum / Max(nAvg,1) ;

     { Plot graph of currently selected variables }
     plVarPlot.MaxPointsPerLine := Max(Round(edRecord.HiValue),NumFitPoints) ;
     plVarPlot.xAxisAutoRange := True ;
     plVarPlot.yAxisAutoRange := True ;
     plVarPlot.xAxisLabel := cbVarXAxis.text + ' '
                             + GetVariableUnits(cbVarXAxis.ItemIndex) ;
     plVarPlot.yAxisLabel := cbVarYAxis.text + ' '
                             + GetVariableUnits(cbVarYAxis.ItemIndex) ;
     { Clear data points line }
     plVarPlot.CreateLine( VarDataLine , clBlue, msOpenSquare, psSolid ) ;
     { Clear any fitted line }
     plVarPlot.CreateLine( VarFitLine , clRed, msNone, psSolid ) ;

     xMin := MaxSingle ;
     xMax := -MaxSingle ;
     for Rec := Variance.StartAt to Variance.EndAt do
         if RecordStatus^[Rec].Valid and (RecordStatus^[Rec].RecType = Test) then begin
         x := GetVariable( cbVarXAxis.ItemIndex, Rec ) ;
         y := GetVariable( cbVarYAxis.ItemIndex, Rec ) ;
         if x < xMin then xMin := x ;
         if x > xMax then xMax := x ;
         plVarPlot.AddPoint( VarDataLine, x, y ) ;
         end ;
     plVarPlot.SortByX( VarDataLine ) ;
     { Initial cursor positions }
     plVarPlot.VerticalCursors[VarCursors.Fit0] := xMin ;
     plVarPlot.VerticalCursors[VarCursors.Fit1] := xMax ;

     { Display average and number of records in results table }
     VarResults.Clear ;

     VarResults.Add( format(' Avg. %s = %.4g %s',
                            [cbVarYAxis.text,Average,
                             GetVariableUnits(cbVarYAxis.ItemIndex)]) ) ;
     VarResults.Add( format(' No. records = %d',[nAvg]) );
     VarFunc.CopyResultsToRichEdit( VarResults, erVarResults ) ;

     bMEPCFrequency.Enabled := True ;

     { Enable copy and print menus }
     Main.CopyAndPrintMenus( True, True ) ;
     bVarSetAxes.Enabled := plVarPlot.Available ;


     end;


procedure TNoiseAnalFrm.CopyVarianceDataToClipBoard ;
{ ----------------------------------------------------------------
  Copy the data points in the variance/mean graph to the clipboard
  ----------------------------------------------------------------
    25/6/98 Clipboard buffer limit reduced to 31000}
const
     BufSize = 31000 ;
     NumBytesPerNumber = 12 ;
var
   Rec,i,iSkip,NumBytesNeeded,NumColumns : Integer ;
   xOffset : single ;
   Line : String ;
   CopyBuf0 : PChar ;
   xBuf,yBuf : PSingleArrayDyn ;
   MaxPoints,NumPoints : Integer ;
begin

     MaxPoints := Variance.EndAt - Variance.StartAt + 1 ;

     GetMem( xBuf, MaxPoints*4 ) ;
     GetMem( yBuf, MaxPoints*4 ) ;

     { Allocate ASCII text buffer to hold graph }
     CopyBuf0 := StrAlloc( BufSize ) ;
     StrPCopy( CopyBuf0, '' ) ;

     try

        // Open clipboard
        Clipboard.Open ;        { Create x/y array for graph }

        { Get data }
        NumPoints := 0 ;
        for Rec := Variance.StartAt to Variance.EndAt do
            if RecordStatus^[Rec].Valid and (RecordStatus^[Rec].RecType = Test) then begin
               xBuf[NumPoints] := GetVariable( cbVarXAxis.ItemIndex, Rec ) ;
               yBuf[NumPoints] := GetVariable( cbVarYAxis.ItemIndex, Rec ) ;
               Inc(NumPoints) ;
               end ;

        { Sort data into ascending order of x }
        Sort( xBuf^, yBuf^, NumPoints ) ;

        { Determine starting point for an exponential fit }
        if VarFunc.Equation = Exponential then
           xOffset := Min( plVarPlot.VerticalCursors[VarCursors.Fit0],
                           plVarPlot.VerticalCursors[VarCursors.Fit1] )
        else xOffset := 0.0 ;

        { Determine sample skip factor to ensure that the compete record
          fits into the buffer }
        NumColumns := 2 ;
        if VarFunc.Equation <> None then NumColumns := NumColumns + 1 ;
        NumBytesNeeded := NumPoints*NumBytesPerNumber*NumColumns ;
        iSkip := Max( (NumBytesNeeded+BufSize-1) div BufSize,1) ;

        i := 0 ;
        screen.cursor := crHourglass ;
        repeat
             Line := format( '%8.5g'#9'%8.5g', [xBuf[i],yBuf[i]] ) ;
             if VarFunc.Equation <> None then begin
                 Line := Line + format( #9'%8.5g',[VarFunc.Value(xBuf[i] - xOffset )]) ;
                 end ;
             Line := Line + #13#10{chr(13) + chr(10)} ;
             CopyBuf0 := StrCat( CopyBuf0, PChar(Line) ) ;
             i := i + iSkip ;
             until i >= NumPoints ;

        { Copy text accumulated in copy buffer to clipboard }
        ClipBoard.SetTextBuf( CopyBuf0 ) ;

     finally
         screen.cursor := crDefault ;
         { Dispose of buffers }
         StrDispose( CopyBuf0 ) ;
         FreeMem( XBuf ) ;
         FreeMem( YBuf ) ;
         Clipboard.Close ;
         end ;
     end ;


procedure TNoiseAnalFrm.CopyRecordToClipBoard ;
{ -------------------------------------------------------------
  Copy the data points in the displayed record to the clipboard
  -------------------------------------------------------------}
const
     BufSize = 31000 ;
     NumBytesPerNumber = 12 ;
var
   i,j,iSkip,NumBytesNeeded,NumColumns : Integer ;
   x : single ;
   Line : String ;
   CopyBuf0,Line0 : PChar ;

begin
     { Allocate ASCII text buffer to hold graph }
     CopyBuf0 := StrAlloc( BufSize ) ;
     StrPCopy( CopyBuf0, '' ) ;
     Line0 := StrAlloc(  256 ) ;

     try
        screen.cursor := crHourglass ;

        // Open clipboard
        Clipboard.Open ;

        { Determine number of samples to skip to keep data within
          clipboard buffer limit  }
        if cbACChannel.ItemIndex <> cbACChannel.ItemIndex then NumColumns := 3
                                                          else NumColumns := 2 ;
        NumBytesNeeded := Data.RecordSize*NumBytesPerNumber*NumColumns ;
        iSkip := Max( (NumBytesNeeded+BufSize-1) div BufSize,1) ;

        { ** Copy AC and DC channel data to clipboard **
          (NOTE. Only copy one channel if AC=DC channel}
        x := 0.0 ;
        i := 0 ;
        while i < Data.RecordSize-1 do begin
            {Create line of text }
            j := i*CdrFH.NumChannels + Channel[DCChan].ChannelOffset ;
            Line := format( '%8.5g', [x] )  ;
            Line := Line + chr(9) + format( '%8.5g', [ADC^[j]*Channel[DCChan].ADCScale] ) ;
            if DCChan <> ACChan then begin
               Line := Line + chr(9) + format( '%8.5g', [ADC^[j]*Channel[ACChan].ADCScale] ) ;
               end ;
            Line := Line + chr(13) + chr(10) ;
            { Add line to copy buffer }
            StrPCopy( Line0, Line ) ;
            CopyBuf0 := StrCat( CopyBuf0, Line0 ) ;
            { Increment to next sample }
            x := x + CdrFH.dt*iSkip ;
            i := i + iSkip ;
            end ;

        { Copy text accumulated in copy buffer to clipboard }
         strlen( COpyBuf0) ;
         ClipBoard.SetTextBuf( CopyBuf0 ) ;

     finally
         screen.cursor := crDefault ;
         { Dispose of buffers }
         StrDispose( Line0 ) ;
         StrDispose( CopyBuf0 ) ;
         Clipboard.Close ;
         end ;
     end ;



function TNoiseAnalFrm.GetVariable( ItemIndex, Rec : Integer ) : single ;
{ ---------------------------------------------------------------------------
  Get the variable selected by the combo box "ItemIndex" for the record "Rec"
  ---------------------------------------------------------------------------}
begin
     case ItemIndex of
          vRecordNum : Result := Rec ;
          vTime : Result := (((Rec-1)*Data.RecordOffset) + Data.RecordSize)
                            *CdrFH.dt*Settings.TScale ;
          { Signal mean DC level }
          vMeanDC : Result := DCMean^[Rec] ;
          { Standard Deviation of signal }
          vStDev : begin
                   if ACVariance^[Rec] > 0.0 then Result := Sqrt(ACVariance^[Rec])
                                             else Result := 0.0 ;
                   end ;
          { Variance of signal }
          vVariance : Result := ACVariance^[Rec] ;
          { Skew of signal }
          vSkew : Result := ACSkew^[Rec] ;
          { Median frequency of fluctuations }
          vMedianFrequency : Result := MedFreq^[Rec] ;
          else Result := 0.0 ;
          end ;
     end ;


function TNoiseAnalFrm.GetVariableUnits( ItemIndex : Integer ) : string ;
{ --------------------------------------------------------------------
  Get the units for the variable selected by the combo box "ItemIndex"
  --------------------------------------------------------------------}
begin
     case ItemIndex of
          vRecordNum : Result := '' ;
          vTime : Result := Settings.TUnits ;
          vMeanDC : Result := Channel[DCChan].ADCUnits ;
          vStDev : Result := Channel[ACChan].ADCUnits ;
          vVariance : Result := Channel[ACChan].ADCUnits + '^2' ;
          vSkew : Result := Channel[ACChan].ADCUnits + '^3' ;
          vMedianFrequency : Result := 'Hz' ;
          else Result := '' ;
          end ;
     end ;


procedure TNoiseAnalFrm.bVarSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plVarPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;


procedure TNoiseAnalFrm.bVarFitClick(Sender: TObject);
{ -------------------------------------------
  Fit a function to variance/mean VarPlot
  -------------------------------------------
    25/6/98 Clipboard buffer limit reduced to 31000
    13/1/99 }
var
   i,nFit,iSkip,Rec : Integer ;
   x,xLoLimit,xHiLimit,xOffset,dx : single ;
   FitData : PXYData ;
   t,Prob : single ;       { T value,Probability (for linear fit only) }
   OK : Boolean ;
begin
     OK := True ;
     New( FitData ) ;
     nFit := 0 ;
     xOffset := 0.0 ;
     Try
        { Select type of equation to be fitted }
        VarFunc.Setup( TEqnType(cbVarEquation.Items.Objects[cbVarEquation.ItemIndex]),
                        GetVariableUnits(cbVarXAxis.ItemIndex),
                        GetVariableUnits(cbVarYAxis.ItemIndex) ) ;
        if VarFunc.Equation = None then OK := False ;

        { Copy data into fitting array }
        if OK then begin
           { Note. records skipped if necessary to fit into buffer }
           iSkip := ((Variance.EndAt - Variance.StartAt ) div High(FitData^.x)) + 1 ;
           nFit := 0 ;
           Rec := Variance.StartAt ;
           { Lower and upper x data limit set by display cursors }
           xLoLimit := MinFlt([plVarPlot.VerticalCursors[VarCursors.Fit0],
                               plVarPlot.VerticalCursors[VarCursors.Fit1]]) ;
           xHiLimit := MaxFlt([plVarPlot.VerticalCursors[VarCursors.Fit0],
                               plVarPlot.VerticalCursors[VarCursors.Fit1]]) ;
           { If an exponential function is being fitted, measure X relative
             to xLoLimit set by display cursor }
           if VarFunc.Equation = Exponential then xOffset := xLoLimit
                                             else xOffset := 0.0 ;
           repeat
              if RecordStatus^[Rec].Valid
                 and (RecordStatus^[Rec].RecType = Test) then begin
                 x := GetVariable( cbVarXAxis.ItemIndex, Rec ) ;
                 if (xLoLimit <= x) and (x <= xHiLimit) then begin
                    FitData^.x[nFit] := x - xOffset ;
                    FitData^.y[nFit] := GetVariable( cbVarYAxis.ItemIndex, Rec ) ;
                    Inc(nFit) ;
                    end ;
                 end ;
              Rec := Rec + iSkip ;
              until Rec >= Variance.EndAt ;

           { Abandon fit if not enough data points }
           if nFit < VarFunc.NumParameters then begin
              MessageDlg( format('%d points is insufficient for fit',[nFit]),
                          mtWarning, [mbOK], 0 ) ;

              OK := False ;
              end ;
           end ;

        if OK then begin
           { Let user modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := VarFunc ;
           SetFitParsFrm.XYData := FitData ;
           SetFitParsFrm.NumPoints := nFit ;
           SetFitParsFrm.Left := NoiseAnalFrm.Left + Main.Left + 50 ;
           SetFitParsFrm.top := NoiseAnalFrm.Top + Main.Top + 50 ;
           SetFitParsFrm.ShowModal ;
           if SetFitParsFrm.ModalResult <> mrOK then OK := False ;
           end ;

        { Fit curve using non-linear regression }
        if OK then begin
           VarFunc := SetFitParsFrm.MathFunc ;
           VarFunc.ParametersSet := True ;
           VarFunc.UseBinWidths := False ;
           VarFunc.FitCurve( FitData^, nFit ) ;
           OK :=  VarFunc.GoodFit ;
           end ;

        { Plot equation on graph }
        if OK and (VarFunc.Equation <> None) then begin
           { Set starting point of fitted line }
           if VarFunc.Equation = Exponential then x := xOffset
                                             else x := plVarPlot.xAxisMin ;
           dx := (plVarPlot.xAxisMax - x) / NumFitPoints ;
           plVarPlot.ShowLines := True ;
           plVarPlot.LineStyles[VarDataLine] := psClear ;
           plVarPlot.CreateLine( VarFitLine, clRed, msNone, psSolid ) ;
           for i := 0 to NumFitPoints-1 do begin
               plVarPlot.AddPoint( VarFitLine, x, VarFunc.Value(x-xOffset) ) ;
               x := x + dx ;
               end ;
           end
        else begin
          plVarPlot.CreateLine( VarFitLine, clRed, msNone, psSolid ) ;
          plVarPlot.LineStyles[VarDataLine] := psSolid ;
          end ;

        { Display results if a good fit }
        VarResults.Clear ;
        if OK then begin

           VarResults.Add( VarFunc.Name ) ;

           { Best fit parameters and standard error }
           for i := 0 to VarFunc.NumParameters-1 do begin
               if not VarFunc.FixedParameters[i] then
                  VarResults.Add( format(' %s = %.4g ^~ %.4g (sd) %s',
                                       [VarFunc.ParNames[i],
                                        VarFunc.Parameters[i],
                                        VarFunc.ParameterSDs[i],
                                        VarFunc.ParUnits[i]] ) )
               else
                  { fixed parameter }
                   VarResults.Add( format(' %s = %.4g (fixed) %s',
                                           [VarFunc.ParNames[i],
                                            VarFunc.Parameters[i],
                                            VarFunc.ParUnits[i]] ) ) ;
               end ;

           { Residual standard deviation }
            VarResults.Add( format(' Residual S.D. = %.4g %s',
                                 [VarFunc.ResidualSD,
                                  GetVariableUnits(cbVarYAxis.ItemIndex)] ) ) ;
           { Statistical degrees of freedom }
            VarResults.Add( format(' Degrees of freedom = %d ',
                                 [VarFunc.DegreesOfFreedom]) ) ;
           { No. of iterations }
            VarResults.Add( format(' No. of iterations = %d ',
                                 [VarFunc.Iterations]) ) ;

           { Special processing for straight line fits only
             (Only do it if there is at least one degree of freedom
              and the slope of the line has not been fixed ) }
           if (VarFunc.Equation = Linear)
              and (not VarFunc.FixedParameters[0])
              and (VarFunc.DegreesOfFreedom > 0) then begin
              { Calculate t-value for slope different from 0 }
              t := VarFunc.Parameters[1] / VarFunc.ParameterSDs[1] ;
              { Calculate probability of encountering this t value by chance }
              Prob := 2.*TProb( Abs(t), VarFunc.DegreesOfFreedom ) ;
              VarResults.Add(
              format('Slope T-Test : t(M<>0) = %.4g, p = %.4g',[t,Prob]) ) ;
              if Prob < 0.05 then begin
                 { Report significance of t value }
                  VarResults.Add( 'A significant linear trend exists (p<=0.05)' ) ;
                 end ;
              end ;
           end ;

        VarFunc.CopyResultsToRichEdit( VarResults, erVarResults ) ;

     finally
            Dispose(FitData) ;
            end ;
     end ;


procedure TNoiseAnalFrm.bMEPCFrequencyClick(Sender: TObject);
{ ----------------------------------------------------
  Calculate MEPC frequency from Avg. Skew and Variance
  (See Segal et al, Biophys J. (1985) Vol. 47 pp183-202,)
  ----------------------------------------------------}
var
   MEPCFrequency,TauRise,TauDecay : double ;
   SumSkew,SumVar,AvgVariance,AvgSkew,I2,I3,Alpha,Beta,BetaMinusAlpha : double ;
   Rec,nAvg : Integer ;
begin
     { Get MEPC rise/decay time constants from user }
     MEPCFreqFrm.ShowModal ;
     if MEPCFreqFrm.ModalResult = mrOK then begin

        { Calculate average variance and skew of records in selected range }

        SumVar := 0.0 ;
        SumSkew := 0.0 ;
        nAvg := 0 ;
        for Rec := Variance.StartAt to Variance.EndAt do begin
            if RecordStatus^[Rec].Valid
               and (RecordStatus^[Rec].RecType = Test) then begin
               SumVar := SumVar + ACVariance^[Rec] ;
               SumSkew := SumSkew + ACSkew^[Rec] ;
               Inc(nAvg)
               end ;
            end ;
        if nAvg > 0 then begin
           AvgVariance := SumVar / nAvg ;
           AvgSkew := SumSkew / nAvg ;
           end
        else  begin
           AvgVariance := 0.0 ;
           AvgSkew := 0.0 ;
           end ;

        TauRise := Settings.Variance.TauRise ;
        TauDecay := Settings.Variance.TauDecay ;

        I2 := ((TauRise - TauDecay)*(TauRise - TauDecay)) / (2.0*(TauRise + TauDecay)) ;
        Alpha := 1.0 / TauRise ;
        Beta := 1.0 / TauDecay ;
        BetaMinusAlpha := Beta - Alpha ;
        I3 := (2.0*BetaMinusAlpha*BetaMinusAlpha*BetaMinusAlpha) /
              (3.0*Alpha*Beta*(2.0*Alpha + Beta)*(2.0*Beta + Alpha)) ;

        MEPCFrequency := ( (AvgVariance*AvgVariance*AvgVariance)
                           / ( AvgSkew*AvgSkew) ) * ( (I3*I3) / (I2*I2*I2) ) ;

        erSpecResults.Lines.Clear ;
        erSpecResults.Lines.Add( format(' Avg. Variance = %.4g %s^2',
                             [AvgVariance,Channel[ACChan].ADCUnits] ) ) ;

        erSpecResults.Lines.Add( format(' Avg. Skew = %.4g %s^3',
                             [AvgSkew,Channel[ACChan].ADCUnits] ) ) ;

        erSpecResults.Lines.Add( format(' MEPC Frequency = %f /s',
                                        [MEPCFrequency] ) ) ;

        erSpecResults.Lines.Add( format(' No. records = %d',[nAvg]) ) ;

        end ;

     end;



{ ***********************************************************************
  Power spectrum routines
  ***********************************************************************}

procedure TNoiseAnalFrm.bDoSpectrumClick(Sender: TObject);
{ -----------------------
  Calculate power spectra
  -----------------------}
var
   i : Integer ;
   StartAt,EndAt : Integer ;
   x,y : single ;
begin

     if rbSpecAllRecords.Checked then begin
        { Use all records }
        StartAt := 0 ;
        EndAt := Data.MaxRecord ;
        end
     else begin
        { Use selected range of records }
        StartAt := Round(edSpecRange.LoValue)-1 ;
        EndAt :=   Round(edSpecRange.HiValue)-1 ;
        end ;

    ComputePowerSpectrum( Test, StartAt, EndAt, PowerSpectrum^ ) ;

    { Subtract background spectrum, if required }
    if ckSpecSubtractBackground.checked then begin
       ComputePowerSpectrum( Background, 0, Data.MaxRecord, BackgroundSpectrum^ ) ;
       for i := 0 to PowerSpectrum^.NumPoints-1 do
           PowerSpectrum^.Power[i] := PowerSpectrum^.Power[i] -
                                      BackgroundSpectrum^.Power[i] ;
       PowerSpectrum^.AvgDCMean := PowerSpectrum^.AvgDCMean
                                   - BackgroundSpectrum^.AvgDCMean ;
       PowerSpectrum^.Variance := PowerSpectrum^.Variance
                                   - BackgroundSpectrum^.Variance ;
       end ;


    if rbLogFreqAveraging.checked then begin
       { Make axis logarithmic, if log. averaging in use }
       plSpecPlot.xAxisLaw := axLog ;
       plSpecPlot.yAxisLaw := axLog ;
       end
    else begin
       { Make axes linear for all other averaging modes }
       plSpecPlot.xAxisLaw := axLinear ;
       plSpecPlot.yAxisLaw := axLinear ;
       end ;

    { No equation }
    cbSpecEquation.ItemIndex := 0 ;

    { Plot graph of currently selected variables }
    plSpecPlot.xAxisAutoRange := True ;
    plSpecPlot.yAxisAutoRange := True ;
    plSpecPlot.xAxisLabel := 'Hz' ;
    plSpecPlot.yAxisLabel := Channel[ACChan].ADCUnits + '^2' ;
    plSpecPlot.MaxPointsPerLine := Max(PowerSpectrum^.NumPoints,NumFitPoints) ;

    { Plot main spectrum }
    plSpecPlot.CreateLine( SpecDataLine, clBlue, msOpenSquare, psSolid ) ;
    for i := 0 to PowerSpectrum^.NumPoints-1 do begin
        x := PowerSpectrum^.Frequency[i] ;
        y := PowerSpectrum^.Power[i] ;
        plSpecPlot.AddPoint( SpecDataLine, x, y ) ;
        end ;

    { Plot background spectrum }
    plSpecPlot.CreateLine( SpecBackLine, clBlue, msOpenTriangle, psSolid ) ;
    if ckSpecSubtractBackground.checked then begin
       for i := 0 to PowerSpectrum^.NumPoints-1 do begin
           x := BackgroundSpectrum^.Frequency[i] ;
           y := BackgroundSpectrum^.Power[i] ;
           plSpecPlot.AddPoint( SpecBackLine, x, y ) ;
           end ;
       end ;

     { Clear any fitted line }
     plSpecPlot.CreateLine( SpecFitLine , clRed, msNone, psSolid ) ;

     { Initial cursor positions }
     plSpecPlot.VerticalCursors[SpecCursors.Fit0] := PowerSpectrum^.Frequency[0] ;
     plSpecPlot.VerticalCursors[SpecCursors.Fit1] := PowerSpectrum^.Frequency[
                                              PowerSpectrum^.NumPoints-1] ;
     plSpecPlot.VerticalCursors[SpecCursors.Read] := PowerSpectrum^.Frequency[
                                              PowerSpectrum^.NumPoints div 2] ;

     { Enable copy and print menus }
     Main.CopyAndPrintMenus( True, True ) ;
     bSpecSetAxes.Enabled := plSpecPlot.Available ;

     end;


{ -------------------------------
  Compute averaged power spectrum
  -------------------------------}
procedure TNoiseAnalFrm.ComputePowerSpectrum(
          RecType : TRecType ;             { Type record to be used (LEAK/TEST) }
          StartAt,EndAt : Integer ;        { Start/end of range of records }
          var Spectrum : TSpectrum ) ;     { Spectrum record to hold result }
var
   Rec,i,j,n,npFFT : Integer ;
   Sum,MeanAC,xMin,xMax,Denom,YReal,YImag,dFreq : single ;
   FFT : ^TSingleArray ;
   VarianceCorrection : single ;
begin

     New(FFT) ;

     try

        Spectrum.StartAt := StartAt ;
        Spectrum.EndAt := EndAt ;
        Spectrum.Available := False ;
        Spectrum.NumAveraged := 0 ;
        Spectrum.RecordSize := Data.RecordSize ;
        Spectrum.AvgDCMean := 0.0 ;

        npFFT := Data.RecordSize div 2 ;
        dFreq := 1.0 / (Spectrum.RecordSize*CdrFH.dt) ;
        for i := 0 to npFFT-1 do begin
            Spectrum.Power[i] := 0.0 ;
            Spectrum.Frequency[i] := (i+1)*dFreq ;
            end ;

        for Rec := StartAt to EndAt do begin
            if RecordStatus^[Rec].Valid
               and (RecordStatus^[Rec].RecType = RecType) then begin

               { Read record from file }
               ReadRecord(Rec, ADC^, xMin, xMax ) ;

               { Calculate mean signal level of DC Channel }
               Sum := 0.0 ;
               j := Channel[DCChan].ChannelOffset ;
               for i := 0 to Spectrum.RecordSize-1 do begin
                   Sum := Sum + (ADC^[j] - Channel[DCChan].ADCZero) ;
                   j := j + CdrFH.NumChannels ;
                   end ;
               Spectrum.AvgDCMean := Spectrum.AvgDCMean +
                                  (Sum/Spectrum.RecordSize) * Channel[DCChan].ADCScale ;

               { Calculate mean signal level of AC Channel }
               Sum := 0.0 ;
               j := Channel[ACChan].ChannelOffset ;
               for i := 0 to Spectrum.RecordSize-1 do begin
                   Sum := Sum + (ADC^[j]) ;
                   j := j + CdrFH.NumChannels ;
                   end ;
               MeanAC := Sum / Spectrum.RecordSize ;

               { Copy signal to be transformed into FFT buffer,
                 after subtracting DC level }
               j := Channel[ACChan].ChannelOffset ;
               for i := 1 to Spectrum.RecordSize do begin
                   FFT^[i] := (ADC^[j] - Round(MeanAC))*Channel[ACChan].ADCScale ;
                   j := j + CdrFH.NumChannels ;
                   end ;

               { Subtract linear trend from signal, if required }
               if ckSubtractTrends.checked then
                  SubtractLinearTrend( FFT^,1, Data.RecordSize ) ;

               { Apply 10% cosine windows, if required }
               if rbCosineWindow.Checked then
                  CosineWindow( 1, Data.RecordSize, FFT^, VarianceCorrection )
               else VarianceCorrection := 1.0 ;

               {Transform to frequency domain }
               RealFFT( FFT^, npFFT, 1 ) ;

               { Compute power spectrum }
               j := 3 ;
               n := 0 ;
               for i := 2 to npFFT do begin
                   YReal := FFT^[j] ;
                   YImag := FFT^[j+1] ;
                   Spectrum.Power[n] := Spectrum.Power[n] + ((YReal*YReal) + (YImag*YImag)) ;
                   Inc(n) ;
                   j := j + 2 ;
                   end ;
               Spectrum.Power[n] := FFT^[2]*FFT^[2] ;
               Spectrum.NumPoints := n + 1 ;

               { Allow other events to be processed }
               application.ProcessMessages ;

               Spectrum.Available := True ;
               Inc(Spectrum.NumAveraged) ;
               end ;

            // Report progress
            if RecType = Test then begin
               Main.StatusBar.SimpleText := format(
               ' Noise Analysis (Spectral Analysis) : %d/%d (Test)',
               [Rec+1,Endat+1]) ;
               end
            else begin
               Main.StatusBar.SimpleText := format(
               ' Noise Analysis (Spectral Analysis) : %d/%d (Background)',
               [Rec+1,Endat+1]) ;
               end ;

            end ;

        { Average spectral time periods }
        if Spectrum.NumAveraged > 0 then begin
           Denom := (Spectrum.NumAveraged*Spectrum.RecordSize*VarianceCorrection)
                    /(2.0*CdrFH.DT) ;
           for i := 0 to Spectrum.NumPoints-1 do begin
               Spectrum.Power[i] := Spectrum.Power[i] / Denom ;
               end ;

           { Subtract frequency 3 points around 50Hz }
           if ckRemoveHarmonics.checked then RemoveHarmonics( Spectrum ) ;

           { Average adjacent frequencies within spectrum }
           AverageFrequencies( Spectrum ) ;

           { Compute total variance of spectrum and median power  frequency }
           SpectralVariance( Spectrum ) ;

           { Average DC channel signal level
             (used for unitary current calculation)}
           Spectrum.AvgDCMean := Spectrum.AvgDCMean / Spectrum.NumAveraged ;

           { Display spectrum results }
           SpecResults.Clear ;
           SpecResults.Add( format(' Median power frequency = %.4g Hz',
                                    [PowerSpectrum^.MedianFrequency]) ) ;

           SpecResults.Add( format(' Total variance = %.4g %s^2',
                                    [PowerSpectrum^.Variance,
                                     Channel[ACChan].ADCUnits]) ) ;
           SpecFunc.CopyResultsToRichEdit( SpecResults, erSpecResults ) ;

           if RecType = Test then begin
              Main.StatusBar.SimpleText := format(
              ' Noise Analysis (Spectral Analysis) : %d-%d (%d Test records averaged)',
               [StartAt+1,EndAt+1,Spectrum.NumAveraged]) ;
               end
            else begin
              Main.StatusBar.SimpleText := format(
              ' Noise Analysis (Spectral Analysis) : %d-%d (%d Background records averaged)',
               [StartAt+1,EndAt+1,Spectrum.NumAveraged]) ;
               end ;

           end ;

     finally
            Dispose(FFT) ;
            end ;

     end ;


{ -------------------------------------------
  Subtract any linear trend from data array Y
  ------------------------------------------}
procedure TNoiseAnalFrm.SubtractLinearTrend(
          var Y : Array of single ;   { Array containing data }
          iStart,iEnd : Integer ) ;   { Start/end of points within Y
                                        to have linear trend subtracted from them }
var
   SumX,SumY,SumXX,SumXY,AvgX,AvgY,Slope,X,YIntercept : single ;
   i : Integer ;
begin

     {Calculate average of X and Y data points}
     SumX := 0.0 ;
     SumY := 0.0 ;
     for i := iStart to iEnd do begin
         SumX := SumX + i ;
         SumY := SumY + Y[i] ;
         end ;
     AvgX := SumX / (iEnd - iStart + 1) ;
     AvgY := SumY / (iEnd - iStart + 1) ;

     { Calculate best fit straight line }
     SumXY  := 0.0 ;
     SumXX := 0.0 ;
     for i := iStart to iEnd do begin
         X := i - AvgX ;
         SumXY := SumXY + (Y[i] - AvgY)*X ;
	 SumXX := SumXX + X*X ;
         end ;
     Slope := SumXY / SumXX ;
     YIntercept := AvgY - Slope*AvgX ;

      for i := iStart to iEnd do begin
          Y[i] := Y[i] - (Slope*i) - YIntercept ;
          end ;
      end ;


procedure TNoiseAnalFrm.RemoveHarmonics(
          var Spectrum : TSpectrum        { Spectrum to be processed }
          ) ;
{ ----------------------------------------------------------
  Remove points around specified frequency and its harmonics
  ----------------------------------------------------------}
var
   iLo : Integer ;     { Index of nearest frequency below 50 Hz }
   iHi : Integer ;     { Index of nearest frequency above 50 Hz }
   iFrom : Integer ;   { Source index }
   iTo : Integer ;     { Destination index }
   iEnd : Integer ;    { Last index in TSpectrum arrays }
   Harm : Integer ;    { Harmonic index }
   LastHarm : Integer ;{ Highest harmonic in spectral range}
   Frequency : Single ; { Frequency to be removed }
begin
     { Index of last point in spectrum }

     if edBaseFrequency.Value <= 0.0 then Exit ;

     LastHarm := Round(Spectrum.Frequency[Spectrum.NumPoints-1]/edBaseFrequency.Value) ;

     for Harm := 1 to LastHarm do begin

         Frequency := Harm*edBaseFrequency.Value ;
         iEnd := Spectrum.NumPoints-1 ;

         { Find nearest frequency point below 50Hz }
         iLo := 0 ;
         while (Spectrum.Frequency[iLo] <= Frequency) and (iLo < iEnd) do inc(iLo) ;
         { Find nearest frequency point above 50Hz }
         iHi := iEnd ;
         while (Spectrum.Frequency[iHi] >= Frequency) and (iHi > 0) do dec(iHi) ;

         { Remove iLo and iHi points from spectrum }
         if (iHi > 0) and (iLo < iEnd) then begin
            iTo := 0 ;
            for iFrom := 0 to iEnd do if (iFrom = iLo) or (iFrom = iHi) then
            else begin
                Spectrum.Frequency[iTo] := Spectrum.Frequency[iFrom] ;
                Spectrum.Power[iTo] := Spectrum.Power[iFrom] ;
                Inc(iTo) ;
                end ;
            Spectrum.NumPoints := iTo ;
            end ;

         end ;

     end ;


{ -------------------------------------------------
  Apply 10% cosine taper to ends of data in array Y
  -------------------------------------------------}
procedure TNoiseAnalFrm.CosineWindow(
          iStart,iEnd : Integer ;   { Start/end of data points to window }
          var Y : Array of single ; { Array containing data to be windowed }
          var VarianceCorrection : single { Returns as variance correction factor }
          ) ;
var
   i,nPoints,i10,i90 : Integer ;
   Pi10,YScale : single ;
begin
    nPoints := iEnd - iStart + 1 ;
    i10 := nPoints div 10 ;
    i90 := npoints - i10 ;
    Pi10 := Pi / i10 ;
    VarianceCorrection := 0.0 ;
    for i := iStart to iEnd do begin
        { Scaling factors }
        if i <= i10 then YScale := 0.5*(1.0 - cos((i - iStart)*Pi10))
        else if i >= i90 then YScale := 0.5*(1.0 - cos((iEnd - i)*Pi10))
        else YScale := 1.0 ;

        Y[i] := Y[i] * YScale ;

        VarianceCorrection := VarianceCorrection + YScale*Yscale
        end ;
    VarianceCorrection := VarianceCorrection / nPoints ;

    end ;


{ ------------------------------------------------
  Average adjacent frequency bands within spectrum
  (Using linear or logarithmic rule)
  ------------------------------------------------}
procedure TNoiseAnalFrm.AverageFrequencies(
          var Spectrum : TSpectrum
          ) ;
var
   iFrom,iTo,NumAveraged,NumFrequenciesRequired,BlockSize,BlockCount : Integer ;
   Pwr,Freq : single ;
begin

     if rbLinFreqAveraging.Checked then begin
        { ** Linear averaging **
          Average adjacent frequencies in blocks of fixed size,
          set by user (edNumFreqAveraged) }
        iFrom := 0 ;
        iTo := 0 ;
        NumAveraged := 0 ;
        NumFrequenciesRequired := Round( edNumFreqAveraged.Value ) ;
        Pwr := 0.0 ;
        Freq := 0.0 ;
        repeat
            { Summate power and frequency for average }
            Pwr := Pwr + Spectrum.Power[iFrom] ;
            Freq := Freq + Spectrum.Frequency[iFrom] ;
            Inc(iFrom) ;
            Inc(NumAveraged) ;
            { Calculate average when required }
            if NumAveraged = NumFrequenciesRequired then begin
               Spectrum.Power[iTo] := Pwr / NumAveraged ;
               Spectrum.Frequency[iTo] := Freq / NumAveraged ;
               Inc(iTo) ;
               NumAveraged := 0 ;
               Pwr := 0.0 ;
               Freq := 0.0 ;
               end ;
            until iFrom >= Spectrum.NumPoints ;
        Spectrum.NumPoints := iTo ;
        end
     else if rbLogFreqAveraging.Checked then begin
        { ** Logarithmic averaging **
          Double the size of the averaging block with increasing
          frequency, at intervals set by the user (edNumFreqAveraged) }
        iFrom := 0 ;
        iTo := 0 ;
        NumAveraged := 0 ;
        { Start with  no averaging }
        NumFrequenciesRequired := 1 ;
        { Set size of block using this average }
        BlockSize := Round( edNumFreqAveraged.Value ) ;
        BlockCount := 0 ;
        Pwr := 0.0 ;
        Freq := 0.0 ;
        repeat
              { Summate power and frequency for average }
              Pwr := Pwr + Spectrum.Power[iFrom] ;
              Freq := Freq + Spectrum.Frequency[iFrom] ;
              Inc(iFrom) ;
              Inc(NumAveraged) ;
              { Calculate average when required }
              if NumAveraged = NumFrequenciesRequired then begin
                 Spectrum.Power[iTo] := Pwr / NumAveraged ;
                 Spectrum.Frequency[iTo] := Freq / NumAveraged ;
                 Inc(iTo) ;
                 NumAveraged := 0 ;
                 Pwr := 0.0 ;
                 Freq := 0.0 ;
                 Inc(BlockCount) ;
                 { Double size of averaging block, when required }
                 if BlockCount = BlockSize then begin
                    NumFrequenciesRequired := NumFrequenciesRequired*2 ;
                    BlockCount := 0 ;
                    end ;
                 end ;
              until iFrom >= Spectrum.NumPoints ;
        Spectrum.NumPoints := iTo ;
        end ;

     end ;


{ ---------------------------------------------------------------
  Calculate total variance of spectrum and median power frequency
  ---------------------------------------------------------------}
procedure TNoiseAnalFrm.SpectralVariance(
          var Spectrum : TSpectrum
          ) ;
var
   Sum,BinWidth,dF,HalfPower,SumHi,SumLo : single ;
   i,iLo,iHi : Integer ;
begin
     { Calculate variance as integral of power spectrum }
     Spectrum.Variance := 0.0 ;
     for i := 0 to Spectrum.NumPoints-1 do begin
         if i < (Spectrum.NumPoints-2) then
            BinWidth := Spectrum.Frequency[i+1] - Spectrum.Frequency[i] ;
         Spectrum.Variance := Spectrum.Variance + Spectrum.Power[i]*BinWidth ;
         end ;

     { Calculate median power frequency }
     i := 0 ;
     Sum := 0.0 ;
     HalfPower := Spectrum.Variance/2.0 ;
     repeat
         if i < Spectrum.NumPoints-2 then
            BinWidth := Spectrum.Frequency[i+1] - Spectrum.Frequency[i] ;
         Sum := Sum + Spectrum.Power[i]*BinWidth ;
         Inc(i) ;
         until (Sum >= HalfPower) or (i >= Spectrum.NumPoints) ;

     iLo := Max(Min(i-2,Spectrum.NumPoints-1),0) ;
     iHi := Min(i-1,Spectrum.NumPoints-1) ;
     SumLo := Sum - (BinWidth*Spectrum.Power[iHi]) ;
     SumHi := Sum ;

     if iLo <> iHi then begin
        dF := ((Spectrum.Frequency[iHi] - Spectrum.Frequency[iLo])
               * (HalfPower - SumLo)) / (SumHi - SumLo ) ;
        end
     else dF := 0.0 ;

     Spectrum.MedianFrequency := Spectrum.Frequency[iLo] + dF ;

     end ;


procedure TNoiseAnalFrm.CopySpectrumDataToClipBoard ;
{ ----------------------------------------------------------------
  Copy the data points in the power spectrum graph to the clipboard
  ----------------------------------------------------------------
    25/6/98 Clipboard buffer limit reduced to 31000}
const
     BufSize = 31000 ;
     NumBytesPerNumber = 12 ;
var
   i,iSkip,NumBytesNeeded,NumColumns : Integer ;
   Line : String ;
   CopyBuf0 : PChar ;

begin

    { Allocate ASCII text buffer to hold graph }
    CopyBuf0 := StrAlloc( BufSize ) ;
    StrPCopy( CopyBuf0, '' ) ;

     try
        Clipboard.Open ;


        { Determine sample skip factor to ensure that the compete record
          fits into the buffer }
        NumColumns := 2 ;
        if SpecFunc.Equation <> None then NumColumns := NumColumns + 1 ;
        NumBytesNeeded := PowerSpectrum^.NumPoints*NumBytesPerNumber*NumColumns ;
        iSkip := Max( (NumBytesNeeded+BufSize-1) div BufSize,1) ;

        i := 0 ;
        screen.cursor := crHourglass ;
        repeat
              Line := format( '%8.5g'#9'%8.5g',
              [PowerSpectrum^.Frequency[i],PowerSpectrum^.Power[i]] ) ;
              if SpecFunc.Equation <> None then begin
                 Line := Line + format( #9'%8.5g',
                 [SpecFunc.Value(PowerSpectrum^.Frequency[i])]) ;
                 end ;
             Line := Line + #13#10 ;
             CopyBuf0 := StrCat( CopyBuf0, PChar(Line) ) ;
             i := i + iSkip ;
             until i >= PowerSpectrum^.NumPoints ;

        { Copy text accumulated in copy buffer to clipboard }
         ClipBoard.SetTextBuf( CopyBuf0 ) ;

     finally
         screen.cursor := crDefault ;
         { Dispose of buffers }
         StrDispose( CopyBuf0 ) ;
         Clipboard.Close ;
         end ;
     end ;


procedure TNoiseAnalFrm.bSpecSetAxesClick(Sender: TObject);
{ ------------------------------
  Set plot axes range/law/labels
  ------------------------------}
begin
     SetAxesFrm.Plot := plSpecPlot ;
     SetAxesFrm.Histogram := False ;
     SetAxesFrm.ShowModal ;
     end;


procedure TNoiseAnalFrm.bFitLorentzianClick(Sender: TObject);
{ -------------------------------------------
  Fit a Lorentzian function to power spectrum
  -------------------------------------------}
var
   i,nFit : Integer ;
   FitData : PXYData ;
   Tau,IUnit,FreqLo,FreqHi,x,y,Temp : single ;
   OK : Boolean ;
begin
     OK := True ;
     New( FitData ) ;
     Try
        { Select type of equation to be fitted }
        SpecFunc.Setup( TEqnType(cbSpecEquation.Items.Objects[cbSpecEquation.ItemIndex]),
                        'Hz',
                        Channel[ACChan].ADCUnits)  ;
        if SpecFunc.Equation = None then OK := False ;

        if OK then begin

           { Get range of points to be fitted }
           FreqLo := MinFlt([plSpecPlot.VerticalCursors[SpecCursors.Fit0],
                             plSpecPlot.VerticalCursors[SpecCursors.Fit1] ]) ;
           FreqHi := MaxFlt([plSpecPlot.VerticalCursors[SpecCursors.Fit0],
                             plSpecPlot.VerticalCursors[SpecCursors.Fit1] ]) ;
           nFit := 0 ;
           for i := 0 to PowerSpectrum^.NumPoints-1 do
               if (FreqLo <= PowerSpectrum^.Frequency[i]) and
                  (PowerSpectrum^.Frequency[i] <= FreqHi ) then begin
                  FitData^.x[nFit] := PowerSpectrum^.Frequency[i] ;
                  FitData^.y[nFit] := PowerSpectrum^.Power[i] ;
                  Inc(nFit) ;
                  end ;

           { Abandon fit if not enough data points }
           if nFit < SpecFunc.NumParameters then begin
              MessageDlg( format('%d points is insufficient for fit',[nFit]),
                          mtWarning, [mbOK], 0 ) ;
              OK := False ;
              end ;
           end ;

        if OK then begin
           { Let user modify initial parameter settings and/or
             fix parameters at constant values }
           SetFitParsFrm.MathFunc := SpecFunc ;
           SetFitParsFrm.XYData := FitData ;
           SetFitParsFrm.NumPoints := nFit ;
           SetFitParsFrm.Left := NoiseAnalFrm.Left + Main.Left + 50 ;
           SetFitParsFrm.top := NoiseAnalFrm.Top + Main.Top + 50 ;
           SetFitParsFrm.ShowModal ;
           if SetFitParsFrm.ModalResult <> mrOK then OK := False ;
           end ;

        { Fit curve using non-linear regression }
        if OK then begin
           { Prevent FitCurve from changing parameter settings }
           SpecFunc := SetFitParsFrm.MathFunc ;
           SpecFunc.ParametersSet := True ;
           SpecFunc.UseBinWidths := False ;
           SpecFunc.FitCurve( FitData^, nFit ) ;
           OK := SpecFunc.GoodFit ;
           end ;

        { Plot equation on graph }
        if OK and (SpecFunc.Equation <> None) then begin
           plSpecPlot.LineStyles[SpecDataLine] := psClear ;
           plSpecPlot.ShowLines := True ;
           plSpecPlot.CreateLine( SpecFitLine, clRed, msNone, psSolid ) ;
           for i := 0 to PowerSpectrum.NumPoints-1 do begin
               x := PowerSpectrum.Frequency[i] ;
               y := SpecFunc.Value(PowerSpectrum.Frequency[i]) ;
               plSpecPlot.AddPoint( SpecFitLine, x, y ) ;
               end ;
           { Plot components of double Lorentzian fit }
           plSpecPlot.CreateLine( SpecFitLorentzian1, clRed, msNone, psSolid ) ;
           plSpecPlot.CreateLine( SpecFitLorentzian2, clRed, msNone, psSolid ) ;
           if SpecFunc.Equation = Lorentzian2 then begin
              { Plot first lorentzian }
              plSpecPlot.CreateLine( SpecFitLorentzian1, clRed, msNone, psSolid ) ;
              Temp :=  SpecFunc.Parameters[2] ;
              SpecFunc.Parameters[2] := 0.0 ;
              for i := 0 to PowerSpectrum.NumPoints-1 do begin
                  x := PowerSpectrum.Frequency[i] ;
                  y := SpecFunc.Value(PowerSpectrum.Frequency[i]) ;
                  plSpecPlot.AddPoint( SpecFitLorentzian1, x, y ) ;
                  end ;
              { Plot second lorentzian }
              SpecFunc.Parameters[2] := Temp ;
              Temp :=  SpecFunc.Parameters[0] ;
              SpecFunc.Parameters[0] := 0.0 ;
              for i := 0 to PowerSpectrum.NumPoints-1 do begin
                  x := PowerSpectrum.Frequency[i] ;
                  y := SpecFunc.Value(PowerSpectrum.Frequency[i]) ;
                  plSpecPlot.AddPoint( SpecFitLorentzian2, x, y ) ;
                  end ;
              SpecFunc.Parameters[0] := Temp ;
              end ;

           end
        else begin
             plSpecPlot.CreateLine( SpecFitLine, clRed, msNone, psSolid ) ;
             plSpecPlot.LineStyles[VarDataLine] := psSolid ;
             end ;

        { Display results }
        SpecResults.Clear ;
        if OK then begin

           SpecResults.Add( SpecFunc.Name ) ;

           { Best fit parameters and standard error }
           for i := 0 to SpecFunc.NumParameters-1 do begin
               if not SpecFunc.FixedParameters[i] then
                  SpecResults.Add( format(' %s = %.4g ^~ %.4g (sd) %s',
                                           [SpecFunc.ParNames[i],
                                           SpecFunc.Parameters[i],
                                           SpecFunc.ParameterSDs[i],
                                           SpecFunc.ParUnits[i]] ) )
               else
                  SpecResults.Add( format(' %s = %.4g (fixed) %s',
                                           [SpecFunc.ParNames[i],
                                           SpecFunc.Parameters[i],
                                           SpecFunc.ParUnits[i]] ) ) ;
               end ;

           { Additional results for particular types of fitted curve }

           case SpecFunc.Equation of
                { Single Lorentzian }
                Lorentzian : begin
                    Tau := SecsToMs / (2.0*Pi*SpecFunc.Parameters[1]) ;
                    SpecResults.Add( format(' ^st = %.4g ms',[Tau]) ) ;
                    IUnit := (Pi*SpecFunc.Parameters[0]
                              *SpecFunc.Parameters[1]) /
                              (2.0*PowerSpectrum^.AvgDCMean) ;
                    SpecResults.Add( format(' I^-u = %.4g %s',
                                             [IUnit,Channel[ACChan].ADCUnits]) ) ;
                    end ;

                { Sum of 2 Lorentzians }
                Lorentzian2 : begin
                    Tau := SecsToMs / (2.0*Pi*SpecFunc.Parameters[1]) ;
                    SpecResults.Add( format(' ^st^-1 = %.4g ms',[Tau]) ) ;
                    Tau := SecsToMs / (2.0*Pi*SpecFunc.Parameters[3]) ;
                    SpecResults.Add( format(' ^st^-2 = %.4g ms',[Tau]) ) ;
                    IUnit := ( (Pi*SpecFunc.Parameters[0]
                               *SpecFunc.Parameters[1])
                               + (Pi*SpecFunc.Parameters[2]
                               *SpecFunc.Parameters[3]) ) /
                               (2.0*PowerSpectrum^.AvgDCMean) ;
                   SpecResults.Add( format(' I^-u = %.4g %s',
                                            [IUnit,Channel[ACChan].ADCUnits]) ) ;
                    end ;

                { MEPC noise spectrum }
                MEPCNoise : begin
                   Settings.Variance.TauRise := 1.0 /
                                                (2.0*Pi*SpecFunc.Parameters[2]) ;
                   SpecResults.Add( format(' ^st^-R = %.4g ms',
                                    [Settings.Variance.TauRise*SecsToMs]) ) ;
                   Settings.Variance.TauDecay := 1.0 /
                                                 (2.0*Pi*SpecFunc.Parameters[1]) ;
                   SpecResults.Add( format(' ^st^-D = %.4g ms',
                                    [Settings.Variance.TauDecay*SecsToMs]) ) ;
                   end ;

                end ;

           { Residual standard deviation }
           SpecResults.Add( format(' Residual S.D. = %.4g %s^2',
                                    [SpecFunc.ResidualSD,
                                    Channel[ACChan].ADCUnits] ) ) ;
           { Statistical degrees of freedom }
           SpecResults.Add( format(' Degrees of freedom = %d ',
                                    [SpecFunc.DegreesOfFreedom]) ) ;
           { No. of iterations }
           SpecResults.Add( format(' No. of iterations = %d ',
                                         [SpecFunc.Iterations]) ) ;

           SpecFunc.CopyResultsToRichEdit( SpecResults, erSpecResults ) ;
           end ;
     finally
            Dispose(FitData) ;
            end ;
     end ;


procedure TNoiseAnalFrm.FormResize(Sender: TObject);
{ -------------------------------------------
  Resize components from form size is changed
  -------------------------------------------}
var
   Bottom : Integer ;
begin

     page.Height := Max( ClientHeight - page.Top - 5, 2) ;
     page.Width := Max( ClientWidth - page.Left - 5, 2) ;

     scDisplay.Width := Max( DataTab.ClientWidth - scDisplay.Left - 5, 2) ;
     plVarPlot.Width := Max( VarianceTab.ClientWidth - plVarPlot.Left - 5, 2) ;
     plSpecPlot.Width := Max( SpectrumTab.ClientWidth - plSpecPlot.Left - 5, 2) ;

     Bottom := DataTab.ClientHeight - 5 ;
     ControlGrp.Height := Max( Bottom - ControlGrp.Top, 2) ;
     DataGrp.Top := Max( Bottom - DataGrp.Height, 2)  ;
     DataGrp.Left := scDisplay.Left ;
     DataGrp.Width := scDisplay.Width ;

     scDisplay.Height := Max( DataGrp.Top - scDisplay.Top - 10, 2 )  ;

     // Amplitude histogram  page
     AmpHistGrp.Height := ControlGrp.Height ;
     plAmpHist.Height := Max( AmpHistTab.ClientHeight - plAmpHist.Top - 5, 2) ;
     plAmpHist.Width := Max( AmpHistTab.ClientWidth - plAmpHist.Left - 5, 2) ;

     { Extend variance page control group to bottom of page }
     VarGrp.Height := Max( VarianceTab.ClientHeight - VarGrp.Top - 5, 2) ;

     { Place variance curve fit group at bottom of page }
     VarFitGrp.Top := VarGrp.Top + VarGrp.Height - VarFitGrp.Height ;
     VarFitGrp.Width := plVarPlot.Width ;
     erVarResults.Width := VarFitGrp.Width - erVarResults.Left - 20 ;

     plVarPlot.Height := Max( VarFitGrp.Top - plVarPlot.Top - 5, 2 ) ;

     { Extend spectrum page control group to bottom of page }
     SpecGrp.Height := Max( SpectrumTab.ClientHeight - SpecGrp.Top - 5, 2) ;

     { Place spectrum curve fit group at bottom of page }
     SpecFitGrp.Top := SpecGrp.Top + SpecGrp.Height - SpecFitGrp.Height ;
     SpecFitGrp.Width := plSpecPlot.Width ;
     plSpecPlot.Height := Max( SpecFitGrp.Top - plSpecPlot.Top
                               - lbSpecFit0.Height
                               - lbArea.Height, 2)  ;

     erSpecResults.Width := SpecFitGrp.Width - erSpecResults.Left - 20 ;
     end;


procedure TNoiseAnalFrm.sbRecordOverlapDownClick(Sender: TObject);
{ -----------------------
  Decrease record overlap
  -----------------------}
var
   QuarterRecord : Integer ;
begin
     Data.RecordOverlap := Max(Data.RecordOverlap-1,0) ;
     QuarterRecord := Data.RecordSize div 4 ;
     Data.RecordOffset := Data.RecordSize - (Data.RecordOverlap*QuarterRecord) ;
     SetRecordSize( Data.RecordSize,True ) ;

     GetRecord ;

     end;

procedure TNoiseAnalFrm.sbRecordOverlapUpClick(Sender: TObject);
{ -----------------------
  Increase record overlap
  -----------------------}
var
   QuarterRecord : Integer ;
begin
     Data.RecordOverlap := Min(Data.RecordOverlap+1,3) ;
     QuarterRecord := Data.RecordSize div 4 ;
     Data.RecordOffset := Data.RecordSize - (Data.RecordOverlap*QuarterRecord) ;
     SetRecordSize( Data.RecordSize, True ) ;

     GetRecord ;

     end;


procedure TNoiseAnalFrm.cbDCChannelChange(Sender: TObject);
{ --------------------------------------------
  Update channel selected as DC signal channel
  --------------------------------------------}
begin
     DCChan := cbDCChannel.ItemIndex ;
     InitialiseDisplay( scDisplay ) ;
     scDisplay.SetDataBuf( ADC ) ;
     end;


procedure TNoiseAnalFrm.bSetRecordStateClick(Sender: TObject);
{ -----------------------------------
  Set the state of a block of records
  -----------------------------------}
var
   Rec : Integer ;
begin
     SetBlockFrm.StartAt := 0 ;
     SetBlockFrm.EndAt := Data.MaxRecord ;
     SetBlockFrm.ShowModal ;
     if SetBlockFrm.ModalResult = mrOK then begin
        for Rec := SetBlockFrm.StartAt to SetBlockFrm.EndAt do begin
            if SetBlockFrm.cbRecType.ItemIndex = 0 then
               RecordStatus^[Rec].RecType := Test
            else
               RecordStatus^[Rec].RecType := Background ;
            RecordStatus^[Rec].Valid := not SetBlockFrm.ckRejected.checked ;
            end ;
        end;
     end ;


procedure TNoiseAnalFrm.cbACChannelChange(Sender: TObject);
{ --------------------------------------------
  Update channel selected as AC signal channel
  --------------------------------------------}
begin
     ACChan := cbACChannel.ItemIndex ;
     InitialiseDisplay( scDisplay ) ;
     scDisplay.SetDataBuf( ADC ) ;
     end;


procedure TNoiseAnalFrm.PrintDisplay ;
{ -----------------------------------------------
  Print currently displayed plot or signal record
  ----------------------------------------------- }
var
   i : Integer ;
begin

    { Print variance plot }
    if Page.ActivePage = VarianceTab then begin
       PrintGraphFrm.Plot := plVarPlot ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          { Add title information to plot }
          plVarPlot.ClearPrinterTitle ;
          plVarPlot.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plVarPlot.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          for i := 0 to erVarResults.Lines.Count-1 do
              plVarPlot.AddPrinterTitleLine( VarResults[i] ) ;
          { Plot graph to printer }
          plVarPlot.Print ;
          end ;
       end ;

    { Print amplitude histogram plot }
    if Page.ActivePage = AmpHistTab then begin
       PrintGraphFrm.Plot := plAmpHist ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          { Add title information to plot }
          plAmpHist.ClearPrinterTitle ;
          plAmpHist.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plAmpHist.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          { Plot graph to printer }
          plAmpHist.Print ;
          end ;
       end ;

    { Print Spectrum plot }
    if Page.ActivePage = SpectrumTab then begin
       PrintGraphFrm.Plot := plSpecPlot ;
       PrintGraphFrm.ToPrinter := True ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then begin
          { Add title information to plot }
          plSpecPlot.ClearPrinterTitle ;
          plSpecPlot.AddPrinterTitleLine( 'File ... ' + cdrFH.FileName ) ;
          plSpecPlot.AddPrinterTitleLine( CdrFH.IdentLine ) ;
          for i := 0 to erSpecResults.Lines.Count-1 do
              plSpecPlot.AddPrinterTitleLine( SpecResults[i] ) ;
          { Plot graph to printer }
          plSpecPlot.Print ;
          end ;
       end ;

     { Print AC/DC signal record }
     if Page.ActivePage = DataTab then begin
        PrintRecFrm.Destination := dePrinter ;
        PrintRecFrm.Display := scDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then begin
           scDisplay.ClearPrinterTitle ;
           scDisplay.AddPrinterTitleLine( 'File : ' + cdrFH.FileName ) ;
           scDisplay.AddPrinterTitleLine( CdrFH.IdentLine ) ;
           scDisplay.Print ;
           end ;
        end ;

     end ;


procedure TNoiseAnalFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy active plot to clipboard as Windows metafile
  ----------------------------------------------------- }
begin
    { Copy variance/mean plot }
    if Page.ActivePage = VarianceTab then begin
       PrintGraphFrm.Plot := plVarPlot ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plVarPlot.CopyImageToClipboard ;
       end ;

    { Copy amplitude histogram plot }
    if Page.ActivePage = AmpHistTab then begin
       PrintGraphFrm.Plot := plAmpHist ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plAmpHist.CopyImageToClipboard ;
       end ;

    { Copy power spectrum }
    if Page.ActivePage = SpectrumTab then begin
       PrintGraphFrm.Plot := plSpecPlot ;
       PrintGraphFrm.ToPrinter := False ;
       PrintGraphFrm.ShowModal ;
       if PrintGraphFrm.ModalResult = mrOK then plSpecPlot.CopyImageToClipboard ;
       end ;

     { Copy signal record to clipboard }
     if Page.ActivePage = DataTab then begin
        PrintRecFrm.Destination := deClipboard ;
        PrintRecFrm.Display := scDisplay ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then scDisplay.CopyImageToClipboard ;
        end ;


     end ;

procedure TNoiseAnalFrm.plSpecPlotCursorChange(Sender: TObject);
{ -----------------------------------------------
  Update labels when spectrum plot cursors change
  -----------------------------------------------}
var
   Freq0,Freq1,FreqLo,FreqHi,Variance,y0,y1 : single ;
   i : Integer ;
begin

     { Set readout cursor label }
     { Set Fitting/area cursor labels }
     lbSpecFit0.Visible := True ;
     lbSpecFit0.Top := plSpecPlot.Top + plSpecPlot.Height + 2 ;
     lbSpecFit1.Top := lbSpecFit0.Top ;

     lbSpecFit0.caption := format( '%.3g Hz|',
                                   [plSpecPlot.VerticalCursors[SpecCursors.Fit0]]) ;
     lbSpecFit0.Left := plSpecPlot.Left - lbSpecFit0.Width +
                        plSpecPlot.XToCanvasCoord(
                        plSpecPlot.VerticalCursors[SpecCursors.Fit0] ) ;
     lbSpecFit1.Visible := True ;
     lbSpecFit1.caption := format( '|%.3g Hz',
                                   [plSpecPlot.VerticalCursors[SpecCursors.Fit1]]) ;
     lbSpecFit1.Left := plSpecPlot.Left +
                        plSpecPlot.XToCanvasCoord(
                        plSpecPlot.VerticalCursors[SpecCursors.Fit1] ) ;


     { Calculate variance as integral of power spectrum }
     Variance := 0.0 ;
     FreqLo := MinFlt( [plSpecPlot.VerticalCursors[SpecCursors.Fit0],
                        plSpecPlot.VerticalCursors[SpecCursors.Fit1]] ) ;
     FreqHi := MaxFlt( [plSpecPlot.VerticalCursors[SpecCursors.Fit0],
                        plSpecPlot.VerticalCursors[SpecCursors.Fit1]] ) ;
     for i := 0 to plSpecPlot.GetNumPointsInLine(SpecDataLine)-2 do begin
         plSpecPlot.GetPoint( SpecDataLine,i,Freq0,y0) ;
         plSpecPlot.GetPoint( SpecDataLine,i+1,Freq1,y1) ;
         if (FreqLo <= Freq0) and (Freq0 <= FreqHi) then
            Variance := Variance + y0*(Freq1 - Freq0) ;
         end ;

     { Place horizontal line between fit/analysis cursors }
     shLine.Top := lbSpecFit0.Top + (lbSpecFit0.Height div 2) ;

     shLine.Left :=  Min(lbSpecFit0.Left + lbSpecFit0.Width,lbSpecFit1.Left) + 2 ;
     shLine.Width := Max(lbSpecFit0.Left + lbSpecFit0.Width,lbSpecFit1.Left)
                     - lbSpecFit0.Width - lbSpecFit0.Left - 4 ;
     shLine.Visible := True ;

     lbArea.Top := lbSpecFit0.Top + lbSpecFit0.Height - (lbArea.Height div 3) ;
     lbArea.caption := format(' %.4g %s ',[Variance,Channel[ACChan].ADCUnits+'^2'] ) ;
     lbArea.Left := shLine.Left + (shLine.Width - lbArea.Width) div 2 ;
     lbArea.visible := true ;

     end;


procedure TNoiseAnalFrm.PageChange(Sender: TObject);
{ ----------------------------
  Updates when page is changed
  ----------------------------}
begin
     SetCopyAndPrintMenus ;
     if Page.ActivePage = DataTab then begin
        // Variance record data editing page
        Data.RecordNum := sbRecord.Position ;
        GetRecord ;
        end
     else if Page.ActivePage = AmpHistTab then begin
        // Amplitude histogram page
        bAmpHistSetAxes.Enabled := plAmpHist.Available ;

        edAmpHistRange.HiLimit := sbRecord.Max + 1;
        if not plAmpHist.Available then begin
           edAmpHistRange.LoValue := 1  ;
           edAmpHistRange.HiValue := edAmpHistRange.HiLimit ;
           end ;

        end
     else if Page.ActivePage = VarianceTab then begin
        // Variance plot page
        bVarSetAxes.Enabled := plVarPlot.Available ;
        edVarRange.HiLimit := sbRecord.Max + 1;
        if not plVarPlot.Available then begin
           edVarRange.LoValue := 1  ;
           edVarRange.HiValue := edVarRange.HiLimit ;
           end ;
        end
     else if Page.ActivePage = SpectrumTab then begin
        // Spectrum plot page
        bSpecSetAxes.Enabled := plSpecPlot.Available ;
        edSpecRange.HiLimit := sbRecord.Max + 1;
        if not plSpecPlot.Available then begin
           edSpecRange.LoValue := 1  ;
           edSpecRange.HiValue := edSpecRange.HiLimit ;
           end ;
        end ;

     end;


procedure TNoiseAnalFrm.SetCopyAndPrintMenus ;
{ --------------------------------
  Update copy and print menu items
  -------------------------------- }
begin
     if Page.ActivePage = VarianceTab then begin
        if plVarPlot.Available then Main.CopyAndPrintMenus( True, True )
                               else Main.CopyAndPrintMenus( False, False ) ;
        end
     else if Page.ActivePage = AmpHistTab then begin
        if plAmpHist.Available then Main.CopyAndPrintMenus( True, True )
                               else Main.CopyAndPrintMenus( False, False ) ;
        end
     else if Page.ActivePage = SpectrumTab then begin
        if plSpecPlot.Available then Main.CopyAndPrintMenus( True, True )
                                else Main.CopyAndPrintMenus( False, False ) ;
        end
     else begin
        Main.CopyAndPrintMenus( True, True ) ;
        end ;
     end ;

procedure TNoiseAnalFrm.FormActivate(Sender: TObject);
begin
     SetCopyAndPrintMenus ;
     ckFixedZeroLevels.Checked := Settings.FixedZeroLevels ;
     end;


procedure  TNoiseAnalFrm.ZoomIn( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDisplay.YZoom( cbACChannel.ItemIndex, -50.0 );
     end ;


procedure  TNoiseAnalFrm.ZoomOut( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDisplay.YZoom( cbACChannel.ItemIndex, 50.0 );
     end ;



procedure  TNoiseAnalFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.ZoomOut ;
     end ;



procedure TNoiseAnalFrm.FormDeactivate(Sender: TObject);
begin
     { Disable menus }
     Main.CopyAndPrintMenus( False, False ) ;
     end;


procedure TNoiseAnalFrm.RecordStatusFile(
          Operation : TRecordStatusFileOp { rsfLoad or rsfSave }
          ) ;
{ ---------------------------------------
  Load/save variance record array to file
  --------------------------------------- }
var
   FileName : string ;
   FileHandle : Integer ;
   Rec : Integer ;
begin

     { Create name of record status file }
     FileName := ChangeFileExt( CdrFH.FileName, '.rec' ) ;

     { Open/create file }
     if FileExists(FileName) then begin
        { Open existing file }
        FileHandle := FileOpen(FileName,fmOpenReadWrite) ;
        end
     else begin
        { Create new file }
        FileHandle := FileCreate( FileName ) ;
        { Fill with default values }
        for Rec := 0 to High(TRecordStatusArray) do begin
            RecordStatus^[Rec].Valid := True ;
            RecordStatus^[Rec].RecType := Test ;
            end ;
        Operation := rsfSave ;
        end ;

     { Load/save data from/to file }
     if FileHandle >= 0 then begin
        { Move file pointer to start of record }
        FileSeek( FileHandle, 0, 0 ) ;

        if Operation = rsfLoad then begin
           { Load status array }
           if FileRead(FileHandle,RecordStatus^,SizeOf(TRecordStatusArray))
              <> SizeOf(TRecordStatusArray) then
              MessageDlg( 'Error reading ' + FileName,mtWarning, [mbOK], 0 ) ;
           end
        else begin
           { Save status array }
           if FileWrite(FileHandle,RecordStatus^,SizeOf(TRecordStatusArray))
              <> SizeOf(TRecordStatusArray) then
              MessageDlg( 'Error writing ' + FileName,mtWarning, [mbOK], 0 ) ;
           end ;
        { Close file }
        FileClose( FileHandle ) ;
        end
     else MessageDlg( 'Error opening ' + FileName,mtWarning, [mbOK], 0 ) ;
     end ;


procedure TNoiseAnalFrm.scDisplayCursorChange(Sender: TObject);
var
   ch : Integer ;
begin

     { Update vertical display magnification so that changes are retained }
     for ch := 0 to CdrFH.NumChannels-1 do if scDisplay.ChanVisible[ch] then begin

          { Get signal baseline cursor }
          if Settings.FixedZeroLevels then begin
             if scDisplay.HorizontalCursors[ch] <> Channel[ch].ADCZero then
                scDisplay.HorizontalCursors[ch] := Channel[ch].ADCZero ;
             end
          else begin
             Channel[ch].ADCZero := scDisplay.HorizontalCursors[ch] ;
             end ;

         Channel[ch].yMin := scDisplay.YMin[ch] ;
         Channel[ch].yMax := scDisplay.YMax[ch] ;
         end ;

     { Get signal baseline cursor }
     //Channel[DCChan].ADCZero := scDisplay.HorizontalCursors[DataCursors.DCZero] ;

     DisplayMeanAndVariance ;

     end;


procedure TNoiseAnalFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits + ' ' ;
     scDisplay.Invalidate ;
     end ;


procedure TNoiseAnalFrm.bNewAmpHistClick(Sender: TObject);
{ -------------------------------
  Create and plot a new histogram
  -------------------------------}
var
   x,y,temp,Sum,yScale,yZero,xMin,xMax : single ;
   i,j,iBin,NumBins,Rec,StartAtRec,EndAtRec,SelectedChan,NumDone : Integer ;
   BinRangeLo,BinRangeHi,BinWidth : Single ;
   Bins : Array[0..MaxBins-1] of Single ;
begin

     bNewAmpHist.Enabled := False ;

     Screen.Cursor := crHourGlass ;
     { Ensure all text box parameters are up to date }

     // Get range of record to be include in histogram
     if rbAmpHistAllRecords.Checked then begin
        StartAtRec := Round( edAmpHistRange.LoLimit )-1 ;
        EndAtRec := Round( edAmpHistRange.HiLimit )-1 ;
        end
     else begin
        StartAtRec := Round( edAmpHistRange.LoValue )-1 ;
        EndAtRec :=   Round( edAmpHistRange.HiValue )-1 ;
        end ;

     // Get channels to be analysed
     if rbACChannel.Checked then SelectedChan := ACChan
                            else SelectedChan := DCChan ;

     { Initialise histogram bin range }

     { Histogram bin range }
     if not BinRangePanel.Visible then begin

        NumBins := Round(edNumBins.Value) ;
        BinRangeLo := (-Channel[SelectedChan].ADCMaxValue -1
                      - Channel[SelectedChan].ADCZero)*Channel[SelectedChan].ADCScale ;
        BinRangeHi := (Channel[SelectedChan].ADCMaxValue
                    - Channel[SelectedChan].ADCZero)*Channel[SelectedChan].ADCScale ;
        edBinsUpper.Value := BinRangeHi ;
        edBinsLower.Value := BinRangeLo ;
        edBinWidth.Value := (BinRangeHi - BinRangeLo) / NumBins ;
        BinRangePanel.Visible := True ;
        edBinWidth.Units := Channel[SelectedChan].ADCUnits ;
        edBinsLower.Units := edBinWidth.Units ;
        edBinsUpper.Units := edBinWidth.Units ;
        end ;

     if edBinsUpper.Value <= edBinsLower.Value then
        edBinsUpper.Value := edBinsLower.Value + 1.0 ;
     BinRangeLo :=  edBinsLower.Value ;
     edBinsLower.Value := BinRangeLo ;  // Ensure units is updated
     BinRangeHi :=  edBinsUpper.Value ;
     edBinsUpper.Value := BinRangeHi ;     // Ensure units is updated
     edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/Round(edNumBins.Value) ;
     BinWidth := edBinWidth.Value ;
     NumBins := Round(edNumBins.Value) ;


     // Compile histogram
     for iBin := 0 to NumBins-1 do Bins[iBin] := 0.0 ;

     // Compile amplitude histogram
     NumDone := 0 ;
     for Rec := StartAtRec to EndAtRec do begin

         if RecordStatus^[Rec].Valid then begin   // Use only unrejected records

            { Read record from file }
            ReadRecord(Rec, ADC^, xMin, xMax ) ;

            // Add points in record to histogram
            j := Channel[SelectedChan].ChannelOffset ;
            yScale := Channel[SelectedChan].ADCScale ;
            yZero := Channel[SelectedChan].ADCZero ;
            for i := 0 to Data.RecordSize-1 do begin
                y := (ADC^[j] - yZero)*yScale ;
                iBin := Round((y - BinRangeLo)/BinWidth) ;
                iBin := Max(Min(iBin,NumBins-1),0);
                Bins[iBin] := Bins[iBin] + 1.0 ;
                j := j + CdrFH.NumChannels ;
                end ;
            Inc(NumDone) ;
            end ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' Noise Analysis (Amplitude Histogram) : Rec %d/%d (%d rejected)',
         [Rec+1,EndAtRec+1,Rec-StartAtRec+2-NumDone] ) ;

         application.ProcessMessages ;
         end ;

     // Final report
     Main.StatusBar.SimpleText := format(
     ' Noise Analysis (Amplitude Histogram) : Records %d-%d (%d included, %d rejected)',
     [StartAtRec+1,EndAtRec+1,NumDone,EndAtRec-StartAtRec+1-NumDone] ) ;

     // Convert to % histogram
     Sum := 0.0 ;
     for iBin := 0 to NumBins-1 do Sum := Sum + Bins[iBin] ;
     if Sum > 0.0 then
        for iBin := 0 to NumBins-1 do Bins[iBin] := 100.0*(Bins[iBin]/Sum) ;

     { Plot new histogram }
     plAmpHist.xAxisAutoRange := False ;
     plAmpHist.xAxisMin := BinRangeLo ;
     plAmpHist.xAxisMax := BinRangeLo + BinWidth*NumBins ;
     plAmpHist.XAxisTick := (plAmpHist.xAxisMax - plAmpHist.xAxisMin) / 5.0 ;
     plAmpHist.yAxisAutoRange := True ;
     plAmpHist.xAxisLabel := Channel[SelectedChan].ADCName
                             +'(' + Channel[SelectedChan].ADCUnits + ')' ;
     plAmpHist.yAxisLabel := '%' ;
     plAmpHist.CreateHistogram( 0 ) ;

     for i := 0 to NumBins-1 do plAmpHist.AddBin( 0,
                                                  i*BinWidth + BinRangeLo,
                                                  i*BinWidth + BinRangeLo+BinWidth*0.5,
                                                  i*BinWidth + BinRangeLo+BinWidth,
                                                  Bins[i] ) ;

     { Enable copy and print menu items }
     Main.CopyAndPrintMenus( True, True ) ;

     bNewAmpHist.Enabled := True ;
     bAmpHistSetAxes.Enabled := plAmpHist.Available ;
     Screen.Cursor := crDefault ;

     end;


procedure TNoiseAnalFrm.bAmpHistSetAxesClick(Sender: TObject);
{ ---------------------------------------------
  Set amplitude histogram axes range/law/labels
  ---------------------------------------------}
begin
     SetAxesFrm.Plot := plAmpHist ;
     SetAxesFrm.Histogram := True ;
     SetAxesFrm.ShowModal ;
     end;


procedure TNoiseAnalFrm.bSetZeroClick(Sender: TObject);
// -------------------------
// Change channel zero level
// -------------------------
var
   SelChan : Integer ;
   Lo,Mid,Hi,y : single ;
begin

     if rbACChannel.Checked then SelChan := ACChan
                            else SelChan := DCChan ;
     { Set readout cursor label }
     plAmpHist.GetBin( 0, plAmpHist.FindNearestIndex(0,0), Lo, Mid, Hi, y ) ;

     // Adjust channel zero level
     Channel[SelChan].ADCZero := Round(Mid/Channel[SelChan].ADCScale)
                                 + Channel[SelChan].ADCZero ;

     // Update histogram
     bNewAmpHist.Click ;

     end;

procedure TNoiseAnalFrm.bSaveToLogClick(Sender: TObject);
// ----------------------------------------
// Write variance curve fit results to log
// ----------------------------------------
var
    i : Integer ;
begin
     for i := 0 to erVarResults.Lines.Count-1 do
         WriteToLogFile( erVarResults.Lines[i] ) ;
     end;

procedure TNoiseAnalFrm.bSaveToLogSpectrumClick(Sender: TObject);
// ----------------------------------------
// Write spectrum curve fit results to log
// ----------------------------------------
var
    i : Integer ;
begin
     for i := 0 to erSpecResults.Lines.Count-1 do
         WriteToLogFile( erSpecResults.Lines[i] ) ;
     end;

procedure TNoiseAnalFrm.edBinsUpperKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Histogram upper limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
        end;
      end;

procedure TNoiseAnalFrm.edBinsLowerKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Histogram lower limit changed
// -----------------------------
begin
     if Key = #13 then begin
        edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
        end;
      end;

procedure TNoiseAnalFrm.edBinWidthKeyPress(Sender: TObject; var Key: Char);
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

procedure TNoiseAnalFrm.edNumBinsKeyPress(Sender: TObject;
  var Key: Char);
// -------------------
// No. of bins changed
// -------------------
begin
    if Key = #13 then begin
       edNumBins.Value := Round(edNumBins.Value) ;
       edBinWidth.Value := (edBinsUpper.Value - edBinsLower.Value)/edNumBins.Value ;
       end;
    end;

procedure TNoiseAnalFrm.rbNoFreqAveragingClick(Sender: TObject);
begin
    panNumFreqAveraged.Visible := False ;
    end;

procedure TNoiseAnalFrm.rbLogFreqAveragingClick(Sender: TObject);
begin
    panNumFreqAveraged.Visible := True ;
    end;

procedure TNoiseAnalFrm.rbLinFreqAveragingClick(Sender: TObject);
begin
    panNumFreqAveraged.Visible := True ;
    end;

procedure TNoiseAnalFrm.ckFixedZeroLevelsClick(Sender: TObject);
// --------------------------------
// Enable/Disable fixed zero levels
// --------------------------------
begin
     Settings.FixedZeroLevels := ckFixedZeroLevels.Checked ;
     end;

procedure TNoiseAnalFrm.scDisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Set channel zero level
// ----------------------
begin
     if (Button = mbRight) and (scDisplay.ActiveHorizontalCursor >=0) then begin
        // If right-mouse button down, display zero baseline level selection dialog box
        ZeroFrm.ChSel := scDisplay.ActiveHorizontalCursor ;
        ZeroFrm.ZeroLevel := Channel[ZeroFrm.ChSel].ADCZero ;
        ZeroFrm.ChanName := Channel[ZeroFrm.ChSel].ADCName ;
        ZeroFrm.NewZeroAt := Round(scDisplay.ScreenCoordToX( ZeroFrm.ChSel, X )) ;
        ZeroFrm.Left := NoiseAnalFrm.Left + Main.Left + 10 + scDisplay.Left + X;
        ZeroFrm.Top := NoiseAnalFrm.Top + Main.Top + 10 + scDisplay.Top + Y ;
        ZeroFrm.ShowModal ;
        Channel[ZeroFrm.ChSel].ADCZero := ZeroFrm.ZeroLevel ;
        Channel[ZeroFrm.ChSel].ADCZero := Max(-Channel[ZeroFrm.ChSel].ADCMaxValue-1,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZero := Min(Channel[ZeroFrm.ChSel].ADCMaxValue,ZeroFrm.ZeroLevel) ;
        Channel[ZeroFrm.ChSel].ADCZeroAt := -1 ;
        SaveCDRHeader( CDRfH ) ;
        scDisplay.HorizontalCursors[ZeroFrm.ChSel] := Channel[ZeroFrm.ChSel].ADCZero ;
        end
     end;

procedure TNoiseAnalFrm.edRecordOverlapKeyPress(Sender: TObject;
  var Key: Char);
// ----------------------
// Record overlap changed
// ----------------------
var
   QuarterRecord : Integer ;
   OldFraction : Double ;
begin
     if Key = #13 then begin
        OldFraction := sbRecord.Position/sbRecord.Max ;
        Data.RecordOverlap := Min(Max(Round(edRecordOverlap.Value/25.0),0),3);
        QuarterRecord := Data.RecordSize div 4 ;
        Data.RecordOffset := Data.RecordSize - (Data.RecordOverlap*QuarterRecord) ;
        SetRecordSize( Data.RecordSize,True ) ;
        Data.RecordNum := Min(Round(OldFraction*sbRecord.Max),sbRecord.Max);
        sbRecord.Position := Data.RecordNum ;
        GetRecord ;
        end ;
     end;

end.
