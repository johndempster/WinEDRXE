unit Simchan;
{ ============================================================
  WinCDR - SimChan.pas - Single-channel current simulation
  (c) J. Dempster, University of Strathclyde 1998-99
  ============================================================
  30/1/99 ... Now uses TScopeDisplay and TValidatedEdit custom controls
  19/4/99 ... Models open channel block, short closed state can only
              be entered/left via open state
  24/8/99 ... Revised
  11/7/01 ... LP filter removed
  1.12.02 ... Progress bar removed
  13.02.03 ... No. of channels in scdisplay now updated correctly
               when number of channels is reduced
  24.6.03 .... No. horizontal/vertical grid lines changeable
  12.09.03 ... Channel name and units now set correcting
  12.01.10 ... Simulation data file now correctly updated with ADCMaxValue
  13.08.12 ... Sampling interval can now be set in window
  }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls,
  global, {plotlib,} shared, maths, fileio, ValEdit, ScopeDisplay, math,
  ComCtrls, ValidatedEdit ;

type
  TSimChanFrm = class(TForm)
    GroupBox1: TGroupBox;
    bStart: TButton;
    bAbort: TButton;
    DurationGrp: TGroupBox;
    PropertiesGrp: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ConditionsGrp: TGroupBox;
    Label9: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    scDisplay: TScopeDisplay;
    edRecordingTime: TValidatedEdit;
    edUnitCurrent: TValidatedEdit;
    edNumChannels: TValidatedEdit;
    edTOpen: TValidatedEdit;
    edTClosedShort: TValidatedEdit;
    edTClosedLong: TValidatedEdit;
    edOpeningsPerBurst: TValidatedEdit;
    edNoiseRMS: TValidatedEdit;
    edBaselineDrift: TValidatedEdit;
    edSamplingInterval: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bStartClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
  private
    { Private declarations }
    procedure HeapBuffers( Operation : THeapBufferOp ) ;
  public
    { Public declarations }
    procedure ChangeDisplayGrid ;
    procedure ZoomOut ;
  end;

var
  SimChanFrm: TSimChanFrm;

implementation

{$R *.DFM}

uses mdiform ;

const
     NumSamplesPerBuffer = 512 ;
     MaxChannels = 5 ;
type
    TChannelState = (Open,ClosedShort,ClosedLong) ;
    TChannelRecord = record
                  Time : Single ;
                  State : TChannelState ;
                  end ;
    TSim = record
         Initialised : Boolean ;        { Parameters set/not set flag }
         RecordingTime : double ;       { Duration of simulated recording }
         UnitCurrent : single ;         { Single-channel current amplitude }
         NumChannels : Integer ;        { Number of channels in patch }
         TOpen : double ;               { Mean open time }
         TClosedShort : double ;         { Mean intra-burst closed time }
         TClosedLong : double ;         { Mean inter-burst closed time }
         NumOpeningsPerBurst : double ; { Mean number of openings per burst }
         ShortClosedStateProb : single ;{ Prob. of of short vs long closed state }
         NoiseRMS : Single ;            { Standard deviation of gaussian noise }
         BaselineDrift : double ;       { Rate of drift of signal baseline }
         ZeroLevel : double ;           { Baseline value }
         t : double ;                   { Simulation Time }
         EndTime : double ;
         Channel : Array[0..MaxChannels-1] of TChannelRecord ;
         end ;
var
   ADC : ^TSmallIntArray ; { Digitised signal buffer }
   Sim : TSim ;
   BuffersAllocated : boolean ;{ Indicates if memory buffers have been allocated }


procedure TSimChanFrm.HeapBuffers( Operation : THeapBufferOp ) ;
{ -----------------------------------------------
  Allocate/deallocation dynamic buffers from heap
  -----------------------------------------------}
begin
     case Operation of
          Allocate : begin
             if not BuffersAllocated then begin
                New(ADC) ;
                BuffersAllocated := True ;
                end ;
             end ;
          Deallocate : begin
             if BuffersAllocated then begin
                Dispose(ADC) ;
                BuffersAllocated := False ;
                end ;
             end ;
          end ;
     end ;


procedure TSimChanFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   i : Integer ;
begin

     Resize ;

     { Create buffers }
     HeapBuffers( Allocate ) ;

     { Set up buttons }
     bStart.Enabled := True ;
     bAbort.Enabled := False ;

     edSamplingInterval.Value := Settings.ADCSamplingInterval ;

     { Display simulated record }
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.MaxPoints := NumSamplesPerBuffer ;
     scDisplay.NumPoints := NumSamplesPerBuffer ;
     scDisplay.NumChannels := 1 ;
     { Set channel information }
     scDisplay.ChanUnits[0] := 'pA' ;
     scDisplay.ChanName[0] := 'Im' ;
     scDisplay.ChanScale[0] := Channel[0].ADCScale ;
     scDisplay.yMin[0] := scDisplay.MinADCValue ;
     scDisplay.yMax[0] := scDisplay.MaxADCValue ;
     scDisplay.ChanVisible[0] := True ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := NumSamplesPerBuffer - 1 ;
     scDisplay.HorizontalCursors[0] := Channel[0].ADCZero ;
     scDisplay.xOffset := Round( sim.t /Settings.ADCSamplingInterval ) ;
     scDisplay.TScale := Settings.ADCSamplingInterval*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits + ' ' ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     { Clear all channels }
     for i := 0 to NumSamplesPerBuffer-1 do ADC^[i] := 0 ;

     scDisplay.SetDataBuf( ADC ) ;

     { Set units for amplitude edit boxes }
     edUnitCurrent.Units := scDisplay.ChanUnits[0] ;
     edNoiseRMS.Units := scDisplay.ChanUnits[0] ;
     edBaselineDrift.Units := scDisplay.ChanUnits[0] + '/s';

     scDisplay.AddHorizontalCursor(0,clGray,True,'z') ;

     { Initialise simulation parameters }
     Sim.Initialised := False ;

     ReSize ;

     end ;


procedure TSimChanFrm.bStartClick(Sender: TObject);
{ -------------------------
  Create simulated currents
  -------------------------}
var
   TBuffer : single ;
   NumOpenChannels,i,j,iCh : Integer ;
   NumBytesToWrite : Integer ;
   Done,FirstTime : Boolean ;
begin

     { Set buttons to be active during simulation run }
     bStart.Enabled := False ;
     bAbort.Enabled := True ;

     { Get parameters from edit boxes }
     Sim.NumChannels := Round(edNumChannels.Value) ;
     Sim.TOpen := edTOpen.Value ;
     Sim.TClosedShort := edTClosedShort.Value ;
     Sim.TClosedLong := edTClosedLong.Value ;
     Sim.NumOpeningsPerBurst := edOpeningsPerBurst.Value ;
     Sim.ShortClosedStateProb := 1.0 - (1./Sim.NumOpeningsPerBurst) ;
     Sim.NoiseRMS := edNoiseRMS.Value ;
     Sim.UnitCurrent := edUnitCurrent.Value ;
     Sim.RecordingTime := edRecordingTime.Value ;
     Sim.BaselineDrift := edBaselineDrift.Value*Settings.ADCSamplingInterval ;
     Settings.ADCSamplingInterval := edSamplingInterval.Value ;

     { Set scaling factor if this is an empty file }
     if CdrFH.NumSamplesInFile = 0 then begin
        CdrFH.dt := Settings.ADCSamplingInterval ;
        cdrFH.NumChannels := 1 ;
        CdrFH.NumChannels := scDisplay.NumChannels ;
        CDRFH.ADCVoltageRange := Main.SESLabIO.ADCVoltageRange ;
        Channel[0].ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
        Channel[0].ADCName := scDisplay.ChanName[0] ;
        Channel[0].ADCUnits := scDisplay.ChanUnits[0] ;
        Channel[0].ADCCalibrationFactor := 1. ;
        Channel[0].ADCAmplifierGain := 1.0 ;
        Channel[0].ADCScale := Abs(Sim.UnitCurrent*Sim.NumChannels*1.5) / Channel[0].ADCMaxValue ;

        scDisplay.ChanScale[0] := Channel[0].ADCScale ;
        scDisplay.TScale := Settings.ADCSamplingInterval*Settings.TScale ;
        Channel[0].ADCCalibrationFactor := CdrFH.ADCVoltageRange /
                                               ( Channel[0].ADCScale * (Channel[0].ADCMaxValue+1) ) ;

        if Sim.UnitCurrent > 0.0 then Channel[0].ADCZero := -1024
                                 else Channel[0].ADCZero := 1024 ;
        end ;

     Channel[0].ChannelOffset := 0 ;

     { Position data file pointer at end of data in file }
     FileSeek(CdrFH.FileHandle,0,2) ;

     { *** Ion channel simulation loop *** }

     scDisplay.HorizontalCursors[0] := Channel[0].ADCZero ;

     Done := False ;
     FirstTime := True ;
     TBuffer := NumSamplesPerBuffer*CdrFH.dt ;
     Sim.ZeroLevel := Channel[0].ADCZero*Channel[0].ADCScale ;
     Sim.t := 0.0 ;
     while (not Done) do begin

         { Background noise }
         j := Channel[0].ChannelOffset ;
         for i := 0 to NumSamplesPerBuffer-1 do begin
             ADC^[j] := Round( (RandG(0.0,Sim.NoiseRMS) + Sim.ZeroLevel)/
                               Channel[0].ADCScale ) ;
             j := j + CdrFH.NumChannels ;
             Sim.ZeroLevel := Sim.ZeroLevel + Sim.BaselineDrift ;
             end ;

         { Ion channel noise }
         if FirstTime then begin
            { Set all channels to closed }
            for iCh := 0 to Sim.NumChannels-1 do begin
                Sim.Channel[iCh].State := ClosedLong ;
                Sim.Channel[iCh].Time := -Sim.TClosedLong*ln(Random) ;
                end ;
            FirstTime := False ;
            end ;

         { Calculate ionic current for each sample point in buffer }
         j := Channel[0].ChannelOffset ;
         for i := 0 to NumSamplesPerBuffer do begin

             NumOPenChannels := 0 ;
             for iCh := 0 to Sim.NumChannels-1 do begin
                 { If at end dwell time in current channel state,
                      flip to other state and obtain a new dwell time }
                 if Sim.Channel[iCh].Time <= 0.0 then begin

                    if Sim.Channel[iCh].State = Open then begin
                       { CLOSED(SHORT) <-- OPEN --> CLOSED(LONG) }
                       if Random <= Sim.ShortClosedStateProb then begin
                          { Change to short closed state }
                          Sim.Channel[iCh].Time := -Sim.TClosedShort*ln(Random) ;
                          Sim.Channel[iCh].State := ClosedShort ;
                          end
                       else begin
                          { Change to long closed state }
                          Sim.Channel[iCh].Time := -Sim.TClosedLong*ln(Random) ;
                          Sim.Channel[iCh].State := ClosedLong ;
                          end ;
                       end
                    else begin
                       { CLOSED(LONG) --> OPEN }
                       Sim.Channel[iCh].Time := -Sim.TOpen*ln(Random) ;
                       Sim.Channel[iCh].State := Open ;
                       end ;
                    end ;
                { else begin}
                 { Decrement time in this state }
                 Sim.Channel[iCh].Time := Sim.Channel[iCh].Time - CdrFH.dt ;
                   { end ;}
                 { If channel is open add it to open count }
                 if Sim.Channel[iCh].State = Open then Inc(NumOpenChannels) ;
                 end ;

             { Add ionic current to data buffer }
             ADC^[j] := ADC^[j] + Round( (NumOpenChannels*Sim.UnitCurrent) /
                                             Channel[0].ADCScale ) ;
             j := j + CdrFH.NumChannels ;
             end ;


         { Save data to file }
         NumBytesToWrite := NumSamplesPerBuffer*CdrFH.NumChannels*2 ;
         if FileWrite(CdrFH.FileHandle,ADC^,NumBytesToWrite) <> NumBytesToWrite then
            MessageDlg( 'File write error ',mtWarning, [mbOK], 0 ) ;

         CdrFH.NumSamplesInFile := CdrFH.NumSamplesInFile
                                   + NumSamplesPerBuffer*CdrFH.NumChannels ;

         Sim.t := Sim.t + TBuffer ;
         if Sim.t > Sim.RecordingTime then Done := True ;
         if bStart.Enabled = True then Done := True ;

         scDisplay.xOffset := Round( sim.t /CdrFH.dt ) ;
         scDisplay.Invalidate ;
         
         Main.StatusBar.SimpleText := format(
         'Single-channel current simulation : %.0f/%0.0fs',
         [Sim.t,Sim.RecordingTime] ) ;
         Application.ProcessMessages ;

         end ;

     Main.StatusBar.SimpleText := format(
     'Single-channel current simulation : %.0fs written to %s',
     [Sim.RecordingTime,CdrFH.FileName] ) ;

     { Close form if simulation has not been aborted }
     {if not bStart.Enabled then close ;}
     bStart.Enabled := True ;
     bAbort.Enabled := False ;

     CdrFH.BackedUp := False ;
     end;



procedure TSimChanFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     { Prevent form being closed if a simulation is running
       (Close button disabled) }
     if bStart.Enabled then CanClose := True
                       else CanClose := False ;
     end;


procedure TSimChanFrm.bAbortClick(Sender: TObject);
{ ------------------------------------
  ABORT button - Aborts simulation run
  ------------------------------------}
begin

     bStart.Enabled := True ;
     { NOTE. Simulation loop terminates when (bStart.Enabled = True) }

     { Reset other buttons to simulation idle condition }
     bAbort.Enabled := False ;
     end;


procedure TSimChanFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ ------------------------
  Tidy up when form closed
  ------------------------}
begin
     { Save data file header data }
     SaveCDRHeader( CdrFH ) ;

     { Get rid of buffers }
     HeapBuffers( Deallocate ) ;

     { Request the form's resources to be freed }
     Action := caFree ;

     { Display results }
      if CdrFH.NumSamplesInFile > 0 then Main.UpdateMDIWIndows ;
     end;


procedure TSimChanFrm.FormResize(Sender: TObject);
begin
     PropertiesGrp.Height := ClientHeight - PropertiesGrp.Top - 10 ;
     ConditionsGrp.Top := PropertiesGrp.Top + PropertiesGrp.Height
                          - ConditionsGrp.Height ;


     scDisplay.Height := ConditionsGrp.Top - scDisplay.Top - 5 ;
     scDisplay.Width := ClientWidth - scDisplay.Left - 10 ;
     ConditionsGrp.Width := scDisplay.Width ;
     end;


procedure TSimChanFrm.scDisplayCursorChange(Sender: TObject);
begin
     Channel[0].yMin := scDisplay.YMin[0] ;
     Channel[0].yMax := scDisplay.YMax[0] ;
     end;


procedure TSimChanFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxvalue ;
     scDisplay.MinADCValue := -Main.SESLabIO.ADCMaxvalue -1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits ;
     scDisplay.Invalidate ;
     end ;


procedure  TSimChanFrm.ZoomOut ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxvalue ;
     scDisplay.MinADCValue := -Main.SESLabIO.ADCMaxvalue -1 ;
     scDisplay.ZoomOut ;
     end ;

initialization
     BuffersAllocated := False ;

end.
