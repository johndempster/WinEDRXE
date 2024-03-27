unit Simnoise;
{ ============================================================
  WinEDR - SimNoise.pas -Ion channel noise simulation
  (c) J. Dempster, University of Strathclyde 1998
  ============================================================
  29/6/98 Close button disabled during simulation run to avoid
          GPF due to closing form while Start button method running
  30/1/99 ... Now uses TScopeDisplay and TValidatedEdit custom controls
  25/8/99 ... Revised
  12/3/02 ... Simulation of variance-mean curves now produces more realistic values
              mean currents now reach values predicted by open probability setting
  24.6.03 .... No. horizontal/vertical grid lines changeable
  12.01.10 ... Simulation data file now correctly updated with ADCMaxValue
  13.08.12 ... Sampling interval can now be set in window
  15.03.24 ... Form position saved to INI file
  }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,
  EDRFileUnit, maths, ValEdit, ScopeDisplay, math, ComCtrls,
  ValidatedEdit ;

type
  TSimNoiseFrm = class(TForm)
    GroupBox1: TGroupBox;
    bStart: TButton;
    bAbort: TButton;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    Label11: TLabel;
    IonCurrentGrp: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RecCondGrp: TGroupBox;
    Label9: TLabel;
    scDisplay: TScopeDisplay;
    edBackGroundTime: TValidatedEdit;
    edTransientTime: TValidatedEdit;
    edSteadyStateTime: TValidatedEdit;
    edUnitCurrent: TValidatedEdit;
    edNumChannels: TValidatedEdit;
    edPOpen: TValidatedEdit;
    edTauOPen: TValidatedEdit;
    edNoiseRMS: TValidatedEdit;
    edSamplingInterval: TValidatedEdit;
    procedure bStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bAbortClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
  SimNoiseFrm: TSimNoiseFrm;

implementation

{$R *.DFM}

uses mdiform ;

const
     NumSamplesPerBuffer = 512 ;
     ChSim = 0 ;
     MaxChannels = 300 ;
type
    TChannelState = (Open,Closed) ;
    TChannelRecord = record
                  Time : Single ;
                  State : TChannelState ;
                  end ;
    TSim = record
         Channel : Array[0..MaxChannels-1] of TChannelRecord ;
         end ;
var
   ADC : ^TSmallIntArray ; { Digitised signal buffer }
   Sim : TSim ;
   BuffersAllocated : boolean ;{ Indicates if memory buffers have been allocated }


procedure TSimNoiseFrm.HeapBuffers( Operation : THeapBufferOp ) ;
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


procedure TSimNoiseFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   ch,i : Integer ;
begin

     resize ;

     { Create buffers }
     HeapBuffers( Allocate ) ;

     bStart.Enabled := True ;
     bAbort.Enabled := False ;

     EDRFile.Channel[ChSim].ADCUnits := 'pA' ;
     edUnitCurrent.Units := EDRFile.Channel[ChSim].ADCUnits ;
     edNoiseRMS.Units := EDRFile.Channel[ChSim].ADCUnits ;
     edSamplingInterval.Value := EDRFile.Settings.ADCSamplingInterval ;
     EDRFile.Channel[ChSim].ADCName := 'Im' ;
     EDRFile.Channel[ChSim].ADCZero := 0 ;
     EDRFile.Channel[ChSim].InUse := True ;

     { Set up Display }
     scDisplay.MaxADCValue := Main.SesLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SesLabIO.ADCMinValue ;
     scDisplay.DisplayGrid := EDRFile.Settings.DisplayGrid ;

     scDisplay.MaxPoints := NumSamplesPerBuffer ;
     scDisplay.NumPoints := NumSamplesPerBuffer ;
     scDisplay.NumChannels := EDRFIle.Cdrfh.NumChannels ;
     { Set channel information }
     for ch := 0 to EDRFIle.Cdrfh.NumChannels-1 do begin

         EDRFile.Channel[Ch].yMin := Main.SesLabIO.ADCMinValue ;
         EDRFile.Channel[Ch].yMax := Main.SesLabIO.ADCMaxValue ;

         scDisplay.ChanUnits[ch] := EDRFile.Channel[Ch].ADCUnits ;
         scDisplay.ChanName[ch] := EDRFile.Channel[Ch].ADCName ;
         scDisplay.ChanScale[ch] := EDRFile.Channel[Ch].ADCScale ;
         scDisplay.yMin[ch] := EDRFile.Channel[Ch].yMin ;
         scDisplay.yMax[ch] := EDRFile.Channel[Ch].yMax ;
         if ch = ChSim then scDisplay.ChanVisible[ch] := True
                       else scDisplay.ChanVisible[ch] := False ;
         end ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := NumSamplesPerBuffer-1  ;
     scDisplay.HorizontalCursors[0] := EDRFile.Channel[ChSim].ADCZero ;

     scDisplay.TScale := EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale ;
     scDisplay.TUnits := EDRFile.Settings.TUnits ;

     { Clear all channels }
     for i := 0 to EDRFIle.Cdrfh.NumChannels*NumSamplesPerBuffer-1 do ADC^[i] := 0 ;

     scDisplay.SetDataBuf( ADC ) ;

     scDisplay.AddHorizontalCursor(0,clGray,True,'z') ;

     end ;


procedure TSimNoiseFrm.bStartClick(Sender: TObject);
{ -------------------------
  Create simulated currents
  -------------------------}
var
   TBuffer : single ;
   Alpha, Beta, p : single ;
   NumOpenChannels,iZeroLevel,i,j,iCh : Integer ;
   NumBytesToWrite : LongInt ;
   t,EndAtTime,TauOpen,TauClosed,NoiseRMS : single ;
   Done : Boolean ;
begin

     { Set buttons to be active during simulation run }
     bStart.Enabled := False ;
     bAbort.Enabled := True ;

     { Get parameters from edit boxes }

     { Set scaling factor if this is an empty file }
     if EDRFIle.Cdrfh.NumSamplesInFile = 0 then begin
        EDRFIle.Cdrfh.NumChannels := 1 ;
        EDRFile.Settings.ADCSamplingInterval := edSamplingInterval.Value ;
        EDRFIle.Cdrfh.dt := EDRFile.Settings.ADCSamplingInterval ;
        scDisplay.NumChannels := EDRFIle.Cdrfh.NumChannels ;
        EDRFile.Channel[ChSim].ADCMaxValue := Main.SesLabIO.ADCMaxValue ;
        EDRFile.Channel[ChSim].ADCCalibrationFactor := 1. ;
        EDRFile.Channel[ChSim].ADCAmplifierGain := 1.0 ;
        EDRFile.Channel[ChSim].ADCScale := Abs(edUnitCurrent.Value*edNumChannels.Value*2.0)
                                   / Main.SesLabIO.ADCMaxValue ;
        scDisplay.ChanScale[ChSim] := EDRFile.Channel[ChSim].ADCScale ;
        EDRFIle.Cdrfh.ADCVoltageRange :=  EDRFile.Channel[ChSim].ADCCalibrationFactor
                                  * ( EDRFile.Channel[ChSim].ADCScale * (Main.SesLabIO.ADCMaxValue+1) ) ;

        if edUnitCurrent.Value > 0.0 then
           EDRFile.Channel[ChSim].ADCZero := Main.SesLabIO.ADCMinValue div 2
        else EDRFile.Channel[ChSim].ADCZero := Main.SesLabIO.ADCMaxValue div 2 ;
        scDisplay.HorizontalCursors[0] := EDRFile.Channel[ChSim].ADCZero ;
        end ;

     EDRFile.Channel[ChSim].ChannelOffset := 0 ;

     { Position data file pointer at end of data in file }
     EDRFIle.Cdrfh.FilePointer := (EDRFIle.Cdrfh.NumSamplesInFile*2*EDRFIle.Cdrfh.NumChannels)
                          + EDRFIle.Cdrfh.NumBytesInHeader ;
     EDRFIle.Cdrfh.FilePointer := FileSeek(EDRFIle.Cdrfh.FileHandle,0,2) ;

     { *** Ion channel simulation loop *** }

     Done := False ;
     TBuffer := NumSamplesPerBuffer*EDRFIle.Cdrfh.dt ;
     EndAtTime := edBackgroundTime.Value
                  + edTransientTime.Value
                  + EdSteadyStateTime.Value ;
     iZeroLevel := Round(EDRFile.Channel[ChSim].ADCZero) ;
     t := 0.0 ;
     while (not Done) do begin

         { Background noise }
         NoiseRMS := edNoiseRMS.Value ;
         j := EDRFile.Channel[ChSim].ChannelOffset ;
         for i := 0 to NumSamplesPerBuffer do begin
             ADC^[j] := Round( RandG(0.0,NoiseRMS)
                        /EDRFile.Channel[ChSim].ADCScale ) + iZeroLevel ;
             j := j + EDRFIle.Cdrfh.NumChannels ;
             end ;

         { Ion channel noise }
         if t >= edBackgroundTime.Value then begin

            { Open channel probability for this time point }
            p := (1.0 - exp( -(t - edBackgroundTime.Value)/
                 (edTransientTime.Value*0.25)))* edPOpen.Value ;

            { Calculate channel mean open and closed dwell times }
            TauOpen := edTauOpen.Value ;
            Alpha := 1.0 / TauOpen ;
            if p > 0.0 then begin
               Beta :=(Alpha*p)/(1.0 - p) ;
               TauClosed := 1.0 / Beta ;
               end
            else TauClosed := 1E10 ;

            // Set initial channel open/close states
            for iCh := 0 to Round(edNumChannels.Value)-1 do begin
                if random >= p then begin
                   Sim.Channel[iCh].State := Closed ;
                   Sim.Channel[iCh].Time := -TauClosed*ln(Random) ;
                   end
                else begin
                   Sim.Channel[iCh].State := Open ;
                   Sim.Channel[iCh].Time := -TauOpen*ln(Random) ;
                   end ;
                end ;

            { Calculate ionic current for each sample point in buffer }
            j := EDRFile.Channel[ChSim].ChannelOffset ;
            for i := 0 to NumSamplesPerBuffer-1 do begin
                NumOPenChannels := 0 ;
                for iCh := 0 to Round(edNumChannels.Value)-1 do begin
                    { If at end dwell time in current channel state,
                      flip to other state and obtain a new dwell time }
                    if Sim.Channel[iCh].Time <= 0.0 then begin
                       if Sim.Channel[iCh].State = Open then begin
                          { Change to closed state }
                          Sim.Channel[iCh].Time := -TauClosed*ln(Random) ;
                          Sim.Channel[iCh].State := Closed ;
                          end
                       else begin
                          { Change to open state }
                          Sim.Channel[iCh].Time := -TauOpen*ln(Random) ;
                          Sim.Channel[iCh].State := Open ;
                          end ;
                       end
                    else begin
                         { Decrement time in this state }
                         Sim.Channel[iCh].Time := Sim.Channel[iCh].Time - EDRFIle.Cdrfh.dt ;
                         end ;
                    { If channel is open add it to open count }
                    if Sim.Channel[iCh].State = Open then Inc(NumOpenChannels) ;
                    end ;
                ADC^[j] := ADC^[j] + Round( (NumOpenChannels*edUnitCurrent.Value) /
                                             EDRFile.Channel[ChSim].ADCScale ) ;

                j := j + EDRFIle.Cdrfh.NumChannels ;
                end ;
            end ;


         NumBytesToWrite := NumSamplesPerBuffer*EDRFIle.Cdrfh.NumChannels*2 ;
         if FileWrite(EDRFIle.Cdrfh.FileHandle,ADC^,NumBytesToWrite) <> NumBytesToWrite then
            MessageDlg( ' File write file failed',mtWarning, [mbOK], 0 ) ;

         EDRFIle.Cdrfh.NumSamplesInFile := EDRFIle.Cdrfh.NumSamplesInFile
                                   + NumSamplesPerBuffer*EDRFIle.Cdrfh.NumChannels ;

         scDisplay.xOffset := Round( t /EDRFIle.Cdrfh.dt ) ;
         scDisplay.Invalidate ;

         t := t + TBuffer ;
         if t > EndAtTime then Done := True ;
         if bStart.Enabled = True then Done := True ;

         Main.StatusBar.SimpleText := format(
         ' Noise Simulation : %.1f / %.1f s',
         [t,EndAtTime]) ;

         Application.ProcessMessages ;

         end ;

     // Final report
     Main.StatusBar.SimpleText := format(
     ' Ion Channel Noise Simulation : i=%.3g %s, n=%.0f, p=%.3g, Tau(open)=%.3g ms in file %s ',
     [edUnitCurrent.Value,EDRFile.Channel[ChSim].ADCUnits,edNumChannels.Value,
     edPOpen.Value,edTauOpen.Value*1000.0,EDRFIle.Cdrfh.FileName]);

     { Close form if simulation has not been aborted }
     bStart.Enabled := True ;
     bAbort.Enabled := False ;

     end;


procedure TSimNoiseFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ ------------------------
  Tidy up when form closed
  ------------------------}
begin

     HeapBuffers( Deallocate ) ;

     EDRFile.SaveHeader( EDRFile.CdrFH ) ;

     Main.UpdateMDIWindows ;
     Action := caFree ;

     // Save form position
     EDRFile.SaveFormPosition( Self ) ;

     end;


procedure TSimNoiseFrm.bAbortClick(Sender: TObject);
{ ------------------------------------
  ABORT button - Aborts simulation run
  ------------------------------------}
begin
     bStart.Enabled := True ;
     bAbort.Enabled := False ;
     end;


procedure TSimNoiseFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     { Prevent form being closed if a simulation is running (Start button disabled) }
     if bStart.Enabled then CanClose := True
                       else CanClose := False ;
     end;

procedure TSimNoiseFrm.FormResize(Sender: TObject);
{ ------------------------------------------------------
  Adjust size/position of controls when form is re-sized
  ------------------------------------------------------ }
begin
      IonCurrentGrp.Top := ClientHeight - IonCurrentGrp.Height - 5 ;
      RecCondGrp.Top := ClientHeight - RecCondGrp.Height - 5 ;
      RecCondGrp.Width := ClientWidth - RecCondGrp.Left - 5 ;
      scDisplay.Height := RecCondGrp.Top - scDisplay.Top - 10 ;
      scDisplay.Width := ClientWidth - scDisplay.Left - 5 ;
      end;



procedure TSimNoiseFrm.scDisplayCursorChange(Sender: TObject);
begin
     EDRFile.Channel[ChSim].yMin := scDisplay.YMin[ChSim] ;
     EDRFile.Channel[ChSim].yMax := scDisplay.YMax[ChSim] ;
     end;


procedure TSimNoiseFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
    scDisplay.MaxADCValue := Main.SesLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SesLabIO.ADCMinValue ;
     scDisplay.DisplayGrid := EDRFile.Settings.DisplayGrid ;

     scDisplay.TScale := EDRFIle.Cdrfh.dt*EDRFile.Settings.TScale ;
     scDisplay.TUnits := EDRFile.Settings.TUnits ;
     scDisplay.Invalidate ;
     end ;


procedure  TSimNoiseFrm.ZoomOut ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Main.SesLabIO.ADCMaxValue ; ;
     scDisplay.MinADCValue := Main.SesLabIO.ADCMinValue ; ;
     scDisplay.ZoomOut ;
     end ;


initialization
     BuffersAllocated := False ;

end.
