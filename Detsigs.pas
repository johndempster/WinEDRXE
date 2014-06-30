unit Detsigs;
{ ========================================================
  WinEDR (c) John Dempster, University of Strathclyde 1998
  SIGNAL DETECTION MODULE
  V0.99 0/3/98
  V1.0b 2/6/98 ... Changes to Record Size now correctly handled
                   Data Entry -> Parameter update now done by UpdateParameters ;
  11/2/99 ... TScopeDisplay used to display records
              Flicker removed using WMEraseBKGND
  23/6/99 ...
  24/2/00 ... Time settings can now be ms or s
  6/3/01 .... Changes record size now updated correctly
  9/4/02 .... RH.ADCVoltageRange now correctly takes account of
              CDRFH.ADCAmplifierGain
  ========================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, global, maths, shared, fileio, {plotlib,}
  ValEdit, ScopeDisplay, RangeEdit, ComCtrls, ValidatedEdit ;

type
  TDetSignalsFrm = class(TForm)
    AnalysisGrp: TGroupBox;
    bDetect: TButton;
    bAbort: TButton;
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    CriteriaGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbChannel: TComboBox;
    Label4: TLabel;
    edStatus: TEdit;
    sbDispCDR: TScrollBar;
    Label8: TLabel;
    edYThreshold: TValidatedEdit;
    edTThreshold: TValidatedEdit;
    edDeadTime: TValidatedEdit;
    edBaselineAverage: TValidatedEdit;
    Label5: TLabel;
    edRecordSize: TValidatedEdit;
    Label7: TLabel;
    edPreTriggerPercentage: TValidatedEdit;
    sbDispWCP: TScrollBar;
    OutputGrp: TGroupBox;
    edOutputFileName: TEdit;
    lbContinuousRecord: TLabel;
    lbDetectedEvents: TLabel;
    edRange: TRangeEdit;
    SaveDialog: TSaveDialog;
    prProgress: TProgressBar;
    lbBaseline: TLabel;
    lbThreshold: TLabel;
    scDispWCP: TScopeDisplay;
    scDispCDR: TScopeDisplay;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbDispCDRChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bDetectClick(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure edRecordSizeKeyPress(Sender: TObject; var Key: Char);
    procedure edYThresholdKeyPress(Sender: TObject; var Key: Char);
    procedure sbDispWCPChange(Sender: TObject);
    procedure edOutputFileNameKeyPress(Sender: TObject; var Key: Char);
    procedure ScDispCDRCursorChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);

  private
    { Private declarations }
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
    procedure DisplayRecord ;
    procedure DisplayDetectedEvent ;
    procedure PlotChannel(
              scDisplay : TScopeDisplay ;
              var Buf : TSmallIntArray ) ;
    procedure SaveDetectedEvent(
              AtSample : Integer
              ) ;
    procedure InitialiseDisplays ;
    procedure OpenWCPFile ;
  public
    { Public declarations }
    procedure CopyDataToClipboard ;
    procedure PrintDisplay ;
    procedure CopyImageToClipboard ;
    procedure ZoomOut ;
    procedure ZoomIn( Chan : Integer ) ;
    procedure ChangeDisplayGrid ;
  end;

var
  DetSignalsFrm: TDetSignalsFrm;

implementation

{$R *.DFM}

uses mdiform,Printrec ;

type
    TDetector = record
              yThreshold : Integer ;
              tCounter : Integer ;
              tThreshold : Integer ;
              yBaseline : single ;
              yOldBaseline : single ;
              RunningMean : single ;
              PreTriggerSamples : Integer ;
              DetectedAt : LongInt ;
              AtSample : LongInt ;
              RecordSize : LongInt ;
              StartAt : LongInt ;
              EndAt : LongInt ;
              SkipSamples : LongInt ;
              JustStarted : Boolean ;
              SignalDetected : Boolean ;
              end ;

var
   ADC : ^TSmallIntArray ;
   DetBuf : ^TSmallIntArray ;
   WCPfH : TWCPFileHeader ;
   rH : ^TWCPRecHeader ;
   BuffersAllocated : Boolean ;
   DetChannel : TChannel ;
   MinHeight : Integer ;
   Detector : TDetector ;
   BaselineCursor : Integer ;
   ThresholdCursor : Integer ;
   DetectionCursor : Integer ;


procedure TDetSignalsFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
                New(DetBuf) ;
                New(rH) ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(ADC) ;
                Dispose(DetBuf) ;
                Dispose( rH ) ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TDetSignalsFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   ch : Integer ;
begin

     Main.mnDetectSignals.Enabled := False ;
     MinHeight := AnalysisGrp.Top + AnalysisGrp.Height + 5 ;

     HeapBuffers( Allocate ) ;


     { Set block of CDR file to be scanned }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiValue := CdrFH.RecordDuration ;
     edRange.HiLimit := CdrFH.RecordDuration ;

     { Fill channel selection list }
     cbChannel.Clear ;
     for ch := 0 to CdrFH.NumChannels-1 do
          cbChannel.items.add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;
     cbChannel.ItemIndex := IntLimitTo(Settings.EventDetector.Channel,
                                       0,CdrFH.NumChannels-1 ) ;                               ;
     DetChannel := Channel[Settings.EventDetector.Channel] ;

     { Detection threshold amplitude }
     edYThreshold.Units := Channel[Settings.EventDetector.Channel].ADCUnits ;
     edYThreshold.LoLimit := Main.SESLabIO.ADCMinValue*
                             Channel[Settings.EventDetector.Channel].ADCScale ;
     edYThreshold.HiLimit := Main.SESLabIO.ADCMaxValue*
                             Channel[Settings.EventDetector.Channel].ADCScale ;
     if (edYThreshold.LoLimit > Settings.EventDetector.yThreshold) or
        (Settings.EventDetector.yThreshold > edYThreshold.HiLimit) or
        (Settings.EventDetector.yThreshold = 0.0) then
        Settings.EventDetector.yThreshold := edYThreshold.HiLimit / 2.0 ;
     edYThreshold.Value := Settings.EventDetector.yThreshold ;
     
     { Detection threshold duration }
     edTThreshold.Scale := Settings.TScale ;
     edTThreshold.Units := Settings.TUnits ;
     edTThreshold.LoLimit := 0.0 ;
     edTThreshold.HiLimit := CdrFH.dt*Settings.EventDetector.RecordSize ;
     edTThreshold.Value := Settings.EventDetector.tThreshold ;

    { Detection dead time (before another event can be detected }
     edDeadTime.Scale := Settings.TScale ;
     edDeadTime.Units := Settings.TUnits ;
     edDeadTime.LoLimit := 0.0 ;
     edDeadTime.HiLimit := CdrFH.dt*Settings.EventDetector.RecordSize ;
     edDeadTime.Value := Settings.EventDetector.DeadTime ;

     { Baseline averaging time }
     edBaselineAverage.Scale := Settings.TScale ;
     edBaselineAverage.Units := Settings.TUnits ;
     edBaselineAverage.LoLimit := 0.0 ;
     edBaselineAverage.HiLimit := CdrFH.dt*1024 ;
     edBaselineAverage.Value := Settings.EventDetector.BaselineAverage ;

     { Size of record in which detected events are stored }
     edRecordSize.LoLimit := 256 ;
     edRecordSize.HiLimit := 4096 ;
     edRecordSize.Value := Settings.EventDetector.RecordSize ;

     { Baseline averaging time }
     edPreTriggerPercentage.Value := Settings.EventDetector.PreTriggerPercentage ;


     { Force a re-size to get controls in right/size place }
     Resize ;

    { Open a .WCP data file to hold detected events }
     WcpFH.FileHandle := -1 ;
     OpenWCPFile ;

     { Initialise continuous and detected event displays }
     InitialiseDisplays ;

     Detector.SignalDetected := False ;


     { Display records }
     DisplayRecord ;
     DisplayDetectedEvent ;


     end;


procedure TDetSignalsFrm.PlotChannel(
          scDisplay : TScopeDisplay ;
          var Buf : TSmallIntArray ) ;
{ ------------------------------------------------
  Plot a block of the signal channel being scanned
  ------------------------------------------------}
begin

     Settings.EventDetector.yThreshold := edyThreshold.Value ;
     Detector.yThreshold := Round ( Settings.EventDetector.yThreshold /
                                    DetChannel.ADCScale ) ;

     { If baseline is invalid, set it to first data points }
     if (Detector.yBaseline > MaxADCValue) or
        (Detector.yBaseline < Main.SESLabIO.ADCMinValue)then
        Detector.yBaseline := Buf[DetChannel.ChannelOffset] ;

     scDisplay.HorizontalCursors[BaselineCursor] := Round(Detector.yBaseline) ;
     scDisplay.HorizontalCursors[ThresholdCursor] := Round(Detector.yBaseline
                                                     + Detector.yThreshold) ;

     scDisplay.SetDataBuf( Buf ) ;

     end ;


procedure TDetSignalsFrm.InitialiseDisplays ;
{ ------------------------------------------------
  Initialise display to selected detection channel
  ------------------------------------------------}
var
   ch : Integer ;
begin
     { Continuous record display channel }
     scDispCDR.MaxADCValue := MaxADCValue ;
     scDispCDR.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDispCDR.MaxPoints := Settings.EventDetector.RecordSize ;
     scDispCDR.NumPoints := scDispCDR.MaxPoints ;
     scDispCDR.NumChannels := CdrFH.NumChannels ; ;
     scDispCDR.xMin := 0 ;
     scDispCDR.xMax := Settings.EventDetector.RecordSize  ;
     scDispCDR.DisplayGrid := Settings.DisplayGrid ;

     { Set DetChannel information }
     for ch := 0 to scDispCDR.NumChannels-1 do begin
         scDispCDR.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDispCDR.ChanName[ch] := Channel[ch].ADCName ;
         scDispCDR.yMin[ch] := Channel[ch].yMin ;
         scDispCDR.yMax[ch] := Channel[ch].yMax ;
         scDispCDR.ChanScale[ch] := Channel[ch].ADCScale ;
         scDispCDR.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDispCDR.ChanZero[ch] := Channel[ch].ADCZero ;
         scDispCDR.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scDispCDR.ChanColor[ch] := clBlue ;
         if ch = cbChannel.ItemIndex then scDispCDR.ChanVisible[ch] := True
                                     else scDispCDR.ChanVisible[ch] := False ;
         end ;
     scDispCDR.TScale := CdrFH.dt*Settings.TScale ;
     scDispCDR.TFormat := ' %.6g ' + Settings.TUnits ;

     { Create display cursors }
     scDispCDR.ClearHorizontalCursors ;
     BaselineCursor := scDispCDR.AddHorizontalCursor(
                       cbChannel.ItemIndex,
                       clgray,
                       True ) ;
     ThresholdCursor := scDispCDR.AddHorizontalCursor(
                        cbChannel.ItemIndex,
                        clgray,
                        False ) ;

     { Detected event display }
     scDispWCP.MaxADCValue := MaxADCValue ;
     scDispWCP.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDispWCP.MaxPoints := Settings.EventDetector.RecordSize ;
     scDispWCP.NumPoints := scDispWCP.MaxPoints ;
     scDispWCP.NumChannels := CdrFH.NumChannels  ;
     scDispWCP.xMin := 0 ;
     scDispWCP.xMax := Settings.EventDetector.RecordSize  ;
     for ch := 0 to scDispWCP.NumChannels-1 do begin
         scDispWCP.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDispWCP.ChanName[ch] := Channel[ch].ADCName ;
         scDispWCP.yMin[ch] := Channel[ch].yMin ;
         scDispWCP.yMax[ch] := Channel[ch].yMax ;
         scDispWCP.ChanScale[ch] := Channel[ch].ADCScale ;
         scDispWCP.ChanUnits[ch] := Channel[ch].ADCUnits ;
         scDispWCP.ChanZero[ch] := Channel[ch].ADCZero ;
         scDispWCP.ChanOffsets[ch] := Channel[ch].ChannelOffset ;
         scDispWCP.ChanColor[ch] := clBlue ;
         if ch = cbChannel.ItemIndex then scDispWCP.ChanVisible[ch] := True
                                     else scDispWCP.ChanVisible[ch] := False ;
         end ;
     scDispWCP.TScale := CdrFH.dt*Settings.TScale ;
     scDispWCP.TFormat := ' %.6g ' + Settings.TUnits ;
     scDispWCP.DisplayGrid := Settings.DisplayGrid ;

     scDispWCP.ClearHorizontalCursors ;
     BaselineCursor := scDispWCP.AddHorizontalCursor(
                       cbChannel.ItemIndex,
                       clgray,
                       True ) ;
     ThresholdCursor := scDispWCP.AddHorizontalCursor(
                        cbChannel.ItemIndex,
                        clgray,
                        False ) ;
     scDispWCP.ClearVerticalCursors ;
     DetectionCursor := scDispWCP.AddVerticalCursor( AllChannels, clgray ) ;

     DetChannel := Channel[cbChannel.ItemIndex] ;

     Settings.EventDetector.yThreshold := edyThreshold.Value ;
     Detector.yThreshold := Round ( Settings.EventDetector.yThreshold /
                                    DetChannel.ADCScale ) ;


     end ;


procedure TDetSignalsFrm.DisplayRecord ;
{ ---------------------------------------------
  Display currently selected block of data file
  ---------------------------------------------}
begin

   sbDispCDR.Max := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)
                      - Settings.DwellTimes.RecordSize ;

   WCPfH.NumSamples := Settings.EventDetector.RecordSize ;

   DetChannel.xMin := sbDispCDR.Position*CdrFH.dt ;
   DetChannel.xMax := DetChannel.xMin + (Settings.EventDetector.RecordSize*CdrFH.dt) ;
   scDispCDR.xOffset := sbDispCDR.Position ;

   if ReadCDRBuffer(CdrFH,sbDispCDR.Position,ADC^,Settings.EventDetector.RecordSize)
      = Settings.EventDetector.RecordSize then begin
      if bDetect.Enabled then Detector.yBaseline := ADC^[DetChannel.ChannelOffset] ;
      PlotChannel( scDispCDR, ADC^ ) ;
      end ;

   end;


procedure TDetSignalsFrm.sbDispCDRChange(Sender: TObject);
begin
     if bDetect.Enabled then DisplayRecord ;
     end ;


procedure TDetSignalsFrm.DisplayDetectedEvent ;
{ ---------------------------------------------
  Display detected event record from .WCP file
  ---------------------------------------------}
var
   i : Integer ;  
begin
     if  WCPfH.NumRecords > 0 then begin
         sbDispWCP.Enabled := True ;
         sbDispWCP.Min := 1 ;
         sbDispWCP.Max := WCPfH.NumRecords ;

         WCPfH.RecordNum := sbDispWCP.Position ;
         GetWCPRecord( WCPfH, rH^, WCPfH.RecordNum, DetBuf^ ) ;
         Detector.yBaseline := DetBuf^[ MaxInt([Detector.PreTriggerSamples-1,0])
                                        *CdrFH.NumChannels + DetChannel.ChannelOffset ] ;
         scDispWCP.xOffset := 0 ;
         scDispWCP.VerticalCursors[DetectionCursor] := Detector.PreTriggerSamples ;

         PlotChannel( scDispWCP, DetBuf^ ) ;

         edStatus.text := format('Event %d/%d at %.2f s',
                             [WCPfH.RecordNum,
                              WCPfH.NumRecords,
                              rH^.Time]) ;

         end
     else begin
         for i := 0 to (CdrFH.NumChannels*Settings.EventDetector.RecordSize)-1 do
            DetBuf^[i] := 0 ;
         PlotChannel( scDispWCP, DetBuf^ ) ;
         edStatus.text := '' ;
         sbDispWCP.Enabled := False ;
         end ;

     end;


procedure TDetSignalsFrm.bDetectClick(Sender: TObject);
{ -------------------------------------------
  Scan and extract signals from CDR data file
  -------------------------------------------}
var
   i,j,y,iVal : Integer ;
   Done,NewBufferNeeded,NewBaselineNeeded : Boolean ;
begin

     { Re-open .WCP data file }
     CDRfH.WCPFileName := ExtractFilePath(CDRfH.FileName)
                          + ExtractFileName(edOutputFileName.text) ;
     CdrFH.WCPFileName := ChangeFileExt( CdrFH.WCPFileName, '.wcp' ) ;
     OpenWCPFile ;

     if WCPfH.NumRecords > 0 then begin
        if MessageDlg( 'Clear existing detected records? ',
           mtWarning,[mbYes,mbNo],0) = mrYes then begin
           FileClose( WCPfH.FileHandle ) ;
           DeleteFile( PChar(CDRfH.WCPFileName) ) ;
           WCPfH.FileHandle := FileCreate( CDRfH.WCPFileName ) ;
           WCPfH.NumRecords := 0 ;
           end ;
        end ;

     { Change buttons to detection running mode }
     bDetect.Enabled := false ;
     bAbort.Enabled := True ;
     Screen.Cursor := crHourGlass ;
     { Initialise progress bar }
     prProgress.Min := 0 ;
     prProgress.Max := High(sbDispCDR.Max) ;
     prProgress.Position := prProgress.Min ;
     prProgress.Min := 0 ;
     prProgress.Max := sbDispCDR.Max ;

     sbDispCDR.Enabled := false ;

     { Get latest data from data entry boxes }
     { Amplitude threshold }
     Settings.EventDetector.yThreshold := edYThreshold.Value ;
     Detector.yThreshold := Round ( Settings.EventDetector.yThreshold /
                                    DetChannel.ADCScale ) ;

     { Time threshold }
     Settings.EventDetector.tThreshold := edtThreshold.Value ;
     Detector.tThreshold := Round(Settings.EventDetector.tThreshold/CdrFH.dt) ;

     { Dead time after event detection }
     Settings.EventDetector.DeadTime := edDeadTime.Value ;
     Detector.SkipSamples := Round( Settings.EventDetector.DeadTime / CdrFH.dt ) ;

     { Baseline tracking running mean }
     Settings.EventDetector.BaselineAverage := edBaselineAverage.Value ;
     Detector.RunningMean := Round( Settings.EventDetector.BaselineAverage/
                                    CdrFH.dt ) ;

     { Record size }
     iVal := Round( edRecordSize.Value ) ;
     iVal := (iVal div 256) * 256 ;
     iVal := MinInt( [MaxInt([iVal,256]),2048]) ;
     edRecordSize.Value := iVal ;
     Settings.EventDetector.RecordSize := iVal ;
     Detector.RecordSize := Settings.EventDetector.RecordSize ;

     { Pre-trigger samples }
     Settings.EventDetector.PreTriggerPercentage := edPreTriggerPercentage.Value ;
     Detector.PreTriggerSamples := Round( (Settings.EventDetector.PreTriggerPercentage
                                   * Settings.EventDetector.RecordSize) / 100.0 ) ;

     { Get range of blocks to be analysed }
     Detector.StartAt := Round(edRange.LoValue/CdrFH.dt) ;
     Detector.EndAt :=   Round(edRange.HiValue/CdrFH.dt) ;

     Detector.tCounter := 0 ;
     Detector.AtSample := Detector.StartAt ;
     Detector.DetectedAt := 0 ;
     Detector.JustStarted := True ;
     DetChannel.xMin := Detector.AtSample ;
     DetChannel.xMax := DetChannel.xMin + Detector.RecordSize ;
     NewBufferNeeded := True ;
     NewBaselineNeeded := True ;
     Detector.JustStarted := True ;
     Detector.tCounter := 0 ;
     Done := False ;
     i := 0 ;
     while not Done do begin


         { Load another buffer of A/D samples from file when needed }
         if NewBufferNeeded then begin
            if ReadCDRBuffer(CdrFH,Detector.AtSample,ADC^,Detector.RecordSize)
               <> Detector.RecordSize then Done := True ;

            DetChannel.xMin := Detector.AtSample*cdrFH.dt ;
            DetChannel.xMax := (Detector.AtSample + Detector.RecordSize)*cdrFH.dt ;
            scDispCDR.xOffset := Detector.AtSample ;

            PlotChannel( scDispCDR, ADC^ ) ;

            { Indicate progress }
            prProgress.Position := Detector.AtSample ;

            NewBufferNeeded := False ;
            i := 0 ;
            
            { Let other events be processed }
            Application.ProcessMessages ;

            end ;

         if Detector.JustStarted then begin
            Detector.yBaseline := ADC^[DetChannel.ChannelOffset] ;
            scDispCDR.HorizontalCursors[BaselineCursor] := Round(Detector.yBaseline) ;
            Detector.yOldBaseline := Detector.yBaseline ;
            Detector.SignalDetected := False ;
            Detector.JustStarted := False ;
            end ;

         j := (i*CdrFH.NumChannels) + DetChannel.ChannelOffset ;

         if  NewBaselineNeeded then begin
             Detector.yBaseline := ADC^[j] ;
             NewBaselineNeeded := False ;
             end ;

         y := ADC^[j] - Round(Detector.yBaseline) ;

         { Update running mean baseline }
         Detector.yBaseline := ((Detector.yBaseline*Detector.RunningMean)+ ADC^[j])
                               /(Detector.RunningMean + 1.0 ) ;

         { Plot baseline and trigger level cursors }
         if ((i mod 10) = 0) then begin
             { Remove old cursors }
             scDispCDR.HorizontalCursors[BaselineCursor] := Round(Detector.yOldBaseline) ;
             scDispCDR.HorizontalCursors[ThresholdCursor] := Round(Detector.yOldBaseline
                                                             + Detector.yThreshold) ;
            { Add new cursors }
            scDispCDR.HorizontalCursors[BaselineCursor] := Round(Detector.yBaseline) ;
            scDispCDR.HorizontalCursors[ThresholdCursor] := Round(Detector.yBaseline
                                                            + Detector.yThreshold) ;
            Detector.yOldBaseline := Detector.yBaseline ;
            end ;

         { If signal exceeds detection threshold ... decrement
           super-threshold sample counter }
         if Detector.yThreshold > 0 then begin
            if y >= Detector.yThreshold then Inc(Detector.tCounter)
                                        else Detector.tCounter := 0 ;
            end
         else begin
            if y <= Detector.yThreshold then Inc(Detector.tCounter)
                                        else Detector.tCounter := 0 ;
            end ;

         if Detector.tCounter = 1 then Detector.DetectedAt := Detector.AtSample ;

         { If an event has been detected ... copy it to a WCP file }
         if Detector.tCounter > Detector.tThreshold then begin

            SaveDetectedEvent( Detector.DetectedAt - Detector.PreTriggerSamples ) ;
            Detector.AtSample := Detector.DetectedAt + Detector.SkipSamples ;
           { Detector.yBaseline := y + Detector.yBaseline ;}
            Detector.yOldBaseline := Detector.yBaseline ;
            Detector.tCounter := 0 ;
            NewBufferNeeded := True ;
            NewBaselineNeeded := True ;
            end
         else begin
            { Increment to next sample }
            Inc(Detector.AtSample) ;
            if (Detector.AtSample >= Detector.EndAt)
               or bDetect.Enabled then Done := True ;
            { Increment buffer pointer }
            Inc(i) ;
            if i >= Detector.RecordSize then NewBufferNeeded := True ;
            end ;
         end ;

     SaveWCPHeader( WCPfH ) ;
     { Close and re-open WCP file }
     FileClose( WCPfH.FileHandle ) ;
     WCPfH.FileHandle := FileOpen( WCPfH.FileName, fmOpenReadWrite ) ;

     WriteToLogFile( format('%d signals detected in %s',
                     [WCPfH.NumRecords,CdrFH.FileName] )) ;
     WriteToLogFile( 'written to ' + WCPfH.FileName ) ;

     { Enable detected event display }
     if WCPFH.NumRecords > 0 then begin
        sbDispWCP.Min := 1 ;
        sbDispWCP.Max := WCPFH.NumRecords ;
        sbDispWCP.Position := 1 ;
        sbDispWCP.Enabled := True ;
        DisplayDetectedEvent ;
        end ;

     { Restore buttons }
     bDetect.Enabled := True ;
     bAbort.Enabled := False ;
     Screen.Cursor := crDefault ;
     { Initialise progress bar }
     prProgress.Position := prProgress.Min ;
     sbDispCDR.Enabled := True ;

     end ;


procedure TDetSignalsFrm.SaveDetectedEvent(
          AtSample : Integer
          ) ;
var
   ch : Integer ;
begin

     if ReadCDRBuffer(CdrFH,AtSample,DetBuf^,Detector.RecordSize)
      = Detector.RecordSize then PlotChannel( scDispWCP, DetBuf^ ) ;

     { Save record to WCP data file }
     Inc(WCPFH.NumRecords) ;
     edStatus.text := format(' Event #%d detected at %.2f s',
                             [WCPfH.NumRecords,
                              Detector.DetectedAt*CdrFH.dt]) ;

     rH^.Status := 'ACCEPTED' ;
     rH^.RecType := 'TEST' ;
     rH^.Number := WCPFH.NumRecords ;
     rH^.Time := Detector.DetectedAt*CdrFH.dt ;
     rH^.dt := CdrFH.dt ;
     rH^.Ident := ' ' ;
     if Channel[0].ADCAmplifierGain = 0.0 then Channel[0].ADCAmplifierGain := 1.0 ;
     for ch := 0 to CdrFH.NumChannels do
         rH^.ADCVoltageRange[ch] := CdrFH.ADCVoltageRange / Channel[0].ADCAmplifierGain ;
     rH^.Equation.Available := False ;
     rH^.Analysis.Available := False ;
     PutWCPRecord( WCPfH, rH^, WCPfH.NumRecords, DetBuf^ ) ;

     end ;


procedure TDetSignalsFrm.bAbortClick(Sender: TObject);
begin
     bDetect.Enabled := True ;
     bAbort.Enabled := False ;
     end;

procedure TDetSignalsFrm.cbChannelChange(Sender: TObject);
begin
     Settings.EventDetector.Channel := cbChannel.ItemIndex ;
     DetChannel := Channel[Settings.EventDetector.Channel] ;
     DetChannel := Channel[Settings.EventDetector.Channel] ;

     { Detection threshold amplitude }
     edYThreshold.Units := Channel[Settings.EventDetector.Channel].ADCUnits ;
     edYThreshold.LoLimit := Main.SESLabIO.ADCMinValue*
                             Channel[Settings.EventDetector.Channel].ADCScale ;
     edYThreshold.HiLimit := MaxADCValue*
                             Channel[Settings.EventDetector.Channel].ADCScale ;
     edYThreshold.Value := Settings.EventDetector.yThreshold ;
     { Initialise continuous and detected event displays with new channel }
     InitialiseDisplays ;

     if bDetect.Enabled then DisplayRecord ;
     end;


procedure TDetSignalsFrm.edRecordSizeKeyPress(Sender: TObject;
  var Key: Char);
var
   iVal : Integer ;
begin
     if key = chr(13) then begin
        iVal := Round( edRecordSize.Value ) ;
        iVal := (iVal div 256) * 256 ;
        iVal := MinInt( [MaxInt([iVal,256]),2048]) ;
        if (iVal <> WCPfH.NumSamples) and (WCPfH.NumRecords > 0) then begin
            if MessageDlg( 'Erase existing records in file?',mtWarning,
               [mbYes,mbNo], 0 ) = mrYes then begin
               // Erase WCP file
               FileClose( WCPfH.FileHandle ) ;
               WCPfH.FileHandle := -1 ;
               DeleteFile( PChar(CDRfH.WCPFileName) ) ;
               OpenWCPFile ;
               end
            else iVal := WCPfH.NumSamples ;
            end ;
        edRecordSize.Value := iVal ;
        { Initialise continuous and detected event displays with new channel }
        Settings.EventDetector.RecordSize := Round(edRecordSize.Value) ;
        InitialiseDisplays ;
        DisplayRecord ;
        DisplayDetectedEvent ;
        end ;
     end;


procedure TDetSignalsFrm.edYThresholdKeyPress(Sender: TObject;
  var Key: Char);
begin
     if (key = chr(13)) and bDetect.Enabled then DisplayRecord ;
     end;


procedure TDetSignalsFrm.sbDispWCPChange(Sender: TObject);
begin
     if bDetect.Enabled then DisplayDetectedEvent ;
     end;


procedure TDetSignalsFrm.FormResize(Sender: TObject);
{ -------------------------------------------------------------
  Adjust the size and position of controls from form is resized
  -------------------------------------------------------------}
var
   RightEdge,BottomEdge : Integer ;
begin

     if ClientHeight < MinHeight then ClientHeight := MinHeight ;

     AnalysisGrp.Height := ClientHeight - AnalysisGrp.Top - 10 ;
     RightEdge := ClientWidth - 10 ;
     BottomEdge := AnalysisGrp.Top + AnalysisGrp.Height ;

     scDispCDR.Height := (BottomEdge - AnalysisGrp.Top
                          - (lbContinuousRecord.Height + 1)
                          - (lbDetectedEvents.Height + 1)
                          - (20)
                          - sbDispCDR.Height -sbDispWCP.Height - 4 ) div 2 ;

     lbContinuousRecord.Top := AnalysisGrp.Top ;
     scDispCDR.Top := lbContinuousRecord.Top + lbContinuousRecord.Height ;
     sbDispCDR.Top := scDispCDR.Top + scDispCDR.Height ;

     lbDetectedEvents.Top := sbDispCDR.Top + sbDispCDR.Height ;
     scDispWCP.Height := scDispCDR.Height ;
     scDispWCP.Top := lbDetectedEvents.Top + lbDetectedEvents.Height ;
     sbDispWCP.Top := scDispWCP.Top + scDispWCP.Height ;

     edStatus.Top := sbDispWCP.Top ;
     edStatus.Left := sbDispWCP.Left + sbDispWCP.Width ;
     edStatus.Width := RightEdge - edStatus.Left ;

     scDispCDR.Width := RightEdge - scDispCDR.Left ;
     scDispWCP.Width := scDispCDR.Width ;
     sbDispCDR.WIdth := scDispCDR.Width ;

     end;


procedure TDetSignalsFrm.CopyDataToClipboard ;
{ ----------------------------------------------------
  Copy sample values of displayed signals to clipboard
  ---------------------------------------------------- }
begin
     if sbDispCDR.Focused then scDispCDR.CopyDataToClipboard
                          else scDispWCP.CopyDataToClipboard ;
     end ;


procedure TDetSignalsFrm.PrintDisplay ;
{ -------------------
  Print display image
  -------------------}
begin
     PrintRecFrm.Destination := dePrinter ;
     if sbDispCDR.Focused then PrintRecFrm.DisplayObj := scDispCDR
                          else PrintRecFrm.DisplayObj := scDispWCP ;

     PrintRecFrm.ShowModal ;

     if PrintRecFrm.ModalResult = mrOK then begin
        TScopeDisplay(PrintRecFrm.DisplayObj).ClearPrinterTitle ;
        TScopeDisplay(PrintRecFrm.DisplayObj).AddPrinterTitleLine(
                                              'File : ' + cdrFH.FileName ) ;
        TScopeDisplay(PrintRecFrm.DisplayObj).AddPrinterTitleLine( CdrFH.IdentLine ) ;
        TScopeDisplay(PrintRecFrm.DisplayObj).Print ;
        end ;
     end ;


procedure TDetSignalsFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy display image to clipboard as metafile
  -------------------------------------------}
begin

     if sbDispCDR.Focused then PrintRecFrm.DisplayObj := scDispCDR
                          else PrintRecFrm.DisplayObj := scDispWCP ;
     PrintRecFrm.Destination := deClipboard ;
     PrintRecFrm.ShowModal ;
     if PrintRecFrm.ModalResult = mrOK then begin
        TScopeDisplay(PrintRecFrm.DisplayObj).CopyImageToClipboard ;
        end ;
     end ;


procedure TDetSignalsFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ -------------------------
  Close and dispose of form
  -------------------------}
begin
     HeapBuffers( Deallocate ) ;
     Main.mnDetectSignals.Enabled := True ;

     { Close WCP data file }
     FileClose( WCPfH.FileHandle ) ;
     { Update WCD file header }
     SaveCDRHeader( CDRfH ) ;

     { Disable copy and print menus }
     Main.ZoomMenus( False ) ;
     Main.CopyAndPrintMenus( False, False ) ;

     Action := caFree ;
     end;


procedure TDetSignalsFrm.edOutputFileNameKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then begin
        CDRfH.WCPFileName := ExtractFilePath(CDRfH.WCPFileName)
                             + ExtractFileName(edOutputFileName.text) ;
        CdrFH.WCPFileName := ReplaceFileEnding( CdrFH.WCPFileName, '.wcp' ) ;
        OpenWCPFile ;
        end ;
     end;


procedure TDetSignalsFrm.OpenWCPFile ;
{ -----------------------------------------
  Open a .WCP file to hold detected signals
  ----------------------------------------- }
var
   ch : Integer ;
begin
     { Close currently open data file }
     if WcpFH.FileHandle >= 0 then FileClose( WcpFH.FileHandle ) ;

     { Open/create a .WCP data file to hold detected events }
     if FileExists( CDRfH.WCPFileName ) then begin
        WcpFH.FileName := CdrFH.WCPFileName ;
        WCPfH.FileHandle := FileOpen( CdrfH.WCPFileName, fmOpenReadWrite ) ;
        if WCPfH.FileHandle >= 0 then GetWCPHeader( WCPfH )
        else MessageDlg( 'Error opening ' + WCPfH.FileName,mtWarning, [mbOK], 0 ) ;
        end
     else begin
        { Create an empty WCP file to hold detected signals }
        if CdrFH.WCPFileName = '' then
           CdrFH.WCPFileName:= ReplaceFileEnding( CdrFH.FileName, '.wcp' ) ;
        WcpFH.FileName := CdrFH.WCPFileName ;
        WCPfH.FileHandle := FileCreate( WcpFH.FileName ) ;
        if WCPfH.FileHandle < 0 then MessageDlg( 'Error creating '
                                     + WCPfH.FileName,mtWarning, [mbOK], 0 ) ;
        WCPfH.NumRecords := 0 ;
        end ;

     { Initialise .WCP file header settings }
     if WCPfH.NumRecords = 0 then begin
        WCPfH.NumSamples := Round(edRecordSize.Value) ;
        WCPfH.NumChannels := CdrFH.NumChannels ;
        WCPfH.NumBytesInHeader := 512 ;
        WCPfH.NumDataBytesPerRecord := WCPfH.NumSamples*WCPfH.NumChannels*2 ;
        WCPfH.NumAnalysisBytesPerRecord := 512 ;
        WCPfH.NumBytesPerRecord := WCPfH.NumAnalysisBytesPerRecord +
                                   WCPfH.NumDataBytesPerRecord ;
        for ch := 0 to WCPfH.NumChannels-1 do Channel[ch].ADCZeroAt := 1 ;
        WCPfH.NumZeroAvg := 20 ;
        WCPfH.dt := CDRfH.dt ;
        SaveWCPHeader( wcpFH ) ;
        end ;

     edOutputFileName.text := ExtractFileName( CdrFH.WCPFileName ) ;
     end ;

     
procedure TDetSignalsFrm.ScDispCDRCursorChange(Sender: TObject);
{ -----------------------------------------------------
  Update readout and cursor labels when cursors changed
  ----------------------------------------------------- }
begin
     lbBaseline.Left := scDispCDR.Left + scDispCDR.Width + 1 ;
     lbBaseline.Top := scDispCDR.Top - (lbBaseline.Height div 2)
                       + scDispCDR.YToScreenCoord(cbChannel.ItemIndex,
                         scDispCDR.HorizontalCursors[BaselineCursor]) ;

     lbThreshold.Left := lbBaseline.Left ;
     lbThreshold.Top := scDispCDR.Top - (lbThreshold.Height div 2)
                       + scDispCDR.YToScreenCoord(cbChannel.ItemIndex,
                         scDispCDR.HorizontalCursors[ThresholdCursor]) ;


     edYThreshold.Value := (scDispCDR.HorizontalCursors[ThresholdCursor]
                           - scDispCDR.HorizontalCursors[BaselineCursor])
                             * DetChannel.ADCScale ;

     Detector.yBaseline := scDispCDR.HorizontalCursors[BaselineCursor] ;

     Channel[cbChannel.ItemIndex].ADCZero := DetChannel.ADCZero ;

     { Update vertical display magnification so that changes are retained }
     Channel[cbChannel.ItemIndex].yMin := scDispCDR.YMin[cbChannel.ItemIndex] ;
     Channel[cbChannel.ItemIndex].yMax := scDispCDR.YMax[cbChannel.ItemIndex] ;

     end;


procedure TDetSignalsFrm.FormActivate(Sender: TObject);
begin
     { Enable copy and print menus }
     Main.ZoomMenus( True ) ;
     Main.CopyAndPrintMenus( True, True ) ;
     end;

procedure TDetSignalsFrm.FormDeactivate(Sender: TObject);
begin
     { Disable copy and print menus }
     Main.ZoomMenus( False ) ;
     Main.CopyAndPrintMenus( False, False ) ;
     end;

procedure  TDetSignalsFrm.ZoomIn( Chan : Integer ) ;
{ -----------------------------------------------------
  Let user set display magnification for channel 'Chan'
  ----------------------------------------------------- }
begin
     scDispCDR.ZoomIn( cbChannel.ItemIndex ) ;
     end ;


procedure  TDetSignalsFrm.ZoomOut ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDispCDR.MaxADCValue := MaxADCValue ;
     scDispCDR.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDispCDR.ZoomOut ;
     scDispWCP.MaxADCValue := MaxADCValue ;
     scDispWCP.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDispWCP.ZoomOut ;
     end ;


procedure TDetSignalsFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin

     scDispCDR.DisplayGrid := Settings.DisplayGrid ;
     scDispCDR.TScale := CdrFH.dt*Settings.TScale ;
     scDispCDR.TFormat := ' %.6g ' + Settings.TUnits ;
     scDispCDR.Invalidate ;

     scDispWCP.DisplayGrid := Settings.DisplayGrid ;
     scDispWCP.TScale := CdrFH.dt*Settings.TScale ;
     scDispWCP.TFormat := ' %.6g ' + Settings.TUnits ;
     scDispWCP.Invalidate ;

     end ;


end.
