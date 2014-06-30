unit Simmepc;
{ ===============================================================================
  WinCDR ... Miniature postsynaptic current simulation module
  (c) J. Dempster 1996-98 All Rights Reserved
  ===============================================================================
  24/6/98 ... MEPC inter-event interval distribution now corrected
  26/4/98 ... MEPC rising phase now exponential (rather than erf)
  29/6/98 ... Close button disabled during simulation run to avoid
              GPFs caused by closing form while bStart.Click running
  15/8/98 ... Negative MEPCs can now be created
  30/1/99 ... Now uses TScopeDisplay and TValidatedEdit custom controls
  11/2/99 ... Flicker removed using WMEraseBKGND
  13/4/99 ... RandG function now used to generate gaussian random variable
  24/8/99 ... Revised
  4/9/99 ... Display grid added
  11/7/01 ... LP filter removed
  1.12.02 ... Progress bar removed
  13.02.03 ... No. of channels in scdisplay now updated correctly
               when number of channels is reduced
  24.6.03 .... No. horizontal/vertical grid lines changeable
  04.02.07 ... Zero level now set to zero.
  30.04.13 ... Random number generator now initialised when form opened
               Fixed intervals between PSCs now supported as well as random
               Optional delay added before events start
               Settings saved in INI file
  15.05.13 ... Binomial release probability and depression/facilitation added to fixed frequency trains
  07.06.13 ... Fixed frequency variation now functions from start of trains
  }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Shared, FileIo, Global, maths, {plotlib,}
  ScopeDisplay, ValEdit, math, ComCtrls, ValidatedEdit ;

type

    TSim = record
         Amplitude : Array[0..200] of Single ;
         LPFilterFirstCall : Boolean ;
         tStart : Array[0..200] of double ;
         tLast : double ;
         tEndAt : Double ;
         t : double ;
         NumMEPCS : Integer ;
         end ;

  TSimMEPCFrm = class(TForm)
    GroupBox1: TGroupBox;
    Label7: TLabel;
    bStart: TButton;
    bAbort: TButton;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label11: TLabel;
    Label3: TLabel;
    SineWaveGrp: TGroupBox;
    Label10: TLabel;
    Label5: TLabel;
    scDisplay: TScopeDisplay;
    edSimDuration: TValidatedEdit;
    edAmplitude: TValidatedEdit;
    edStDev: TValidatedEdit;
    edTauRise: TValidatedEdit;
    edTauDecay: TValidatedEdit;
    edSineAmplitude: TValidatedEdit;
    edSineFrequency: TValidatedEdit;
    GroupBox2: TGroupBox;
    rbRandom: TRadioButton;
    Label6: TLabel;
    edFrequency: TValidatedEdit;
    rbFixed: TRadioButton;
    lbDelay: TLabel;
    edInitialDelay: TValidatedEdit;
    PanFrequencySD: TPanel;
    Label4: TLabel;
    edFrequencySD: TValidatedEdit;
    cbUnits: TComboBox;
    Label8: TLabel;
    Label12: TLabel;
    edDepression: TValidatedEdit;
    Label13: TLabel;
    edTauDepression: TValidatedEdit;
    edReleaseProbability: TValidatedEdit;
    Label14: TLabel;
    Label15: TLabel;
    edReleasablePool: TValidatedEdit;
    Label9: TLabel;
    edNoiseRMS: TValidatedEdit;
    procedure bStartClick(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure scDisplayCursorChange(Sender: TObject);
    procedure rbRandomClick(Sender: TObject);
    procedure rbFixedClick(Sender: TObject);
    procedure cbUnitsChange(Sender: TObject);
  private
    { Private declarations }
    Sim : TSim ;
    ADC : TSmallIntArray ; { Digitised signal buffer }

    procedure MEPCSimulation ;
    procedure GaussianFilter( Var Buf,OldBuf : Array of SmallInt ;
                              NumSamples,NumChannels : Integer ;
                              CutOffFrequency : single ;
                              var FirstCall : Boolean ) ;
    function binomial( pIn, n : Single ) : Single ;
    function gammln( xx : Single ) : Single ;
  public
    { Public declarations }
    procedure ChangeDisplayGrid ;
    procedure ZoomOut ;
  end;

var
  SimMEPCFrm: TSimMEPCFrm;

implementation
uses
    MDIForm ;
{$R *.DFM}
const
     NumSamplesPerBuffer = 512 ;
type
    TPointArray = Array[0..2000] of TPoint ;


procedure TSimMEPCFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
var
   i : Integer ;
begin
     resize ;

     { Disable "MEPC Simulation" item in "Simulation" menu }
     Main.mnMEPCSim.enabled := false ;

     bStart.Enabled := True ;
     bAbort.Enabled := False ;

     cbUnits.Clear ;
     cbUnits.Items.Add('pA') ;
     cbUnits.Items.Add('nA') ;

    // Get settings
    edSimDuration.Value :=  Settings.SimEPC.Duration ;
    edAmplitude.Value := Settings.SimEPC.Amplitude ;
    edStDev.Value := Settings.SimEPC.AmplitudeSD ;
    edTauRise.Value := Settings.SimEPC.TauRise ;
    edTauDecay.Value := Settings.SimEPC.TauDecay ;
    edNoiseRMS.Value := Settings.SimEPC.NoiseSD ;
    edFrequency.Value := Settings.SimEPC.Frequency ;
    edFrequencySD.Value := Settings.SimEPC.FrequencySD ;
    edInitialDelay.Value := Settings.SimEPC.Delay ;
    rbRandom.Checked := Settings.SimEPC.RandomEvents ;
    rbFixed.Checked := not rbRandom.Checked ;
    panFrequencySD.Visible := rbFixed.Checked ;
    edSineAmplitude.Value := Settings.SimEPC.SineAmplitude ;
    edSineFrequency.Value := Settings.SimEPC.SineFrequency ;
    cbUnits.ItemIndex := Min(Max(Settings.SimEPC.UnitsIndex,0),cbUnits.Items.Count-1) ;
    edReleaseProbability.Value := Settings.SimEPC.ReleaseProbability ;
    edReleasablePool.Value := Settings.SimEPC.ReleasablePool ;
    edDepression.Value := Settings.SimEPC.Depression ;
    edTauDepression.Value := Settings.SimEPC.TauDepression ;

     Channel[0].ADCUnits := cbUnits.Text ;
     Channel[0].ADCName := 'Im' ;

     { Set units for amplitude parameter boxes }
     edAmplitude.Units := Channel[0].ADCUnits ;
     edStDev.Units := Channel[0].ADCUnits ;
     edNoiseRMS.Units := Channel[0].ADCUnits ;
     edSineAmplitude.Units := Channel[0].ADCUnits ;

     { Set up scope display }
     scDisplay.MaxADCValue := Main.SESLabIO.ADCMaxValue ;
     scDisplay.MinADCValue := Main.SESLabIO.ADCMinValue ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.MaxPoints := NumSamplesPerBuffer ;
     scDisplay.NumPoints := NumSamplesPerBuffer ;
     scDisplay.NumChannels := 1 ;
     { Set channel information }
     scDisplay.ChanUnits[0] := cbUnits.Text ;
     scDisplay.ChanName[0] := 'Im' ;
     scDisplay.ChanScale[0] := Main.SESLabIO.ADCChannelUnitsPerBit[0] ;
     scDisplay.yMin[0] := scDisplay.MinADCValue ;
     scDisplay.yMax[0] := scDisplay.MaxADCValue ;
     scDisplay.ChanVisible[0] := True ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := NumSamplesPerBuffer-1  ;
     scDisplay.AddHorizontalCursor(0,clGray,True,'z') ;
     scDisplay.HorizontalCursors[0] := 0 ;
     scDisplay.xOffset := Round( Channel[0].xMin /Settings.ADCSamplingInterval ) ;
     scDisplay.TScale := Settings.ADCSamplingInterval*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits ;

     { Clear all channels }
     for i := 0 to NumSamplesPerBuffer-1 do ADC[i] := 0 ;

     scDisplay.SetDataBuf( @ADC ) ;

     // Initialise random number generator
     randomize ;

     end ;


procedure TSimMEPCFrm.bStartClick(Sender: TObject);
{ --------------------
  Start simulation run
  --------------------}
begin
     { Update all edit boxes and transfer their parameters to
       simulation control record (SIM) }
     { Update log with parameters for this simulation run }
     WriteToLogFile( 'Miniature endplate current simulation') ;
     WriteToLogFile( 'Mean Amplitude = ' + edAmplitude.text ) ;
     WriteToLogFile( 'St. dev. = ' + edStDev.text ) ;
     WriteToLogFile( 'Tau(rise) = ' + edTauRise.text ) ;
     WriteToLogFile( 'Noise RMS = ' + edNoiseRMS.text ) ;
     WriteToLogFile( 'Tau(decay) = ' + edTauDecay.text ) ;
     WriteToLogFile( 'Frequency = ' + edFrequency.text ) ;
     WriteToLogFile( 'FrequencySD = ' + edFrequencySD.text ) ;
     WriteToLogFile( 'Sine wave amplitude = ' + edSineAmplitude.text ) ;
     WriteToLogFile( 'Sine wave frequency = ' + edSineFrequency.text ) ;
     //WriteToLogFile( 'Low pass filter cut-off = ' + edLPFilter.text ) ;
     WriteToLogFile( 'Duration of simulation = ' + edSimDuration.text ) ;

     bStart.Enabled := False ;
     bAbort.Enabled := True ;

     MEPCSimulation ;
     end;


procedure TSimMEPCFrm.MEPCSimulation ;
{ ==================================
  Create simulated endplate currents
  ==================================}
var
   i,j,ch,ChOffset : Integer ;
   NumBytesToWrite : LongInt ;
   x,xLimit,y,yMEPC,omega,MaxAmplitude,Freq,Depression,SteadyStateDepression : Single  ;
   t : double ;
   Done : Boolean ;
begin

     Sim.t := 0.0 ;

     if CdrFH.NumSamplesInFile = 0 then begin
        { Set scaling factor }
        cdrFH.NumChannels := scDisplay.NumChannels ;
        CDRFH.NumBytesInHeader := NumBytesInHeader ;
        Channel[0].ADCCalibrationFactor := 1. ;
        Channel[0].ADCAmplifierGain := 1.0 ;
        Channel[0].InUse := True ;

        MaxAmplitude := 5.0*Abs(edAmplitude.Value)*
                        Max( edTauDecay.Value*edFrequency.Value, 1.0 ) ;
        if rbFixed.Checked then begin
           MaxAmplitude := MaxAmplitude*Max(edReleasablePool.Value,1.0)*Max(1.-edDepression.Value,1.0) ;
           end ;
        Channel[0].ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
        Channel[0].ADCScale := MaxAmplitude / Channel[0].ADCMaxValue ;
        scDisplay.ChanScale[0] := Channel[0].ADCScale ;
        CdrFH.ADCVoltageRange :=  Channel[0].ADCCalibrationFactor
                                * ( Channel[0].ADCScale * (scDisplay.MaxADCValue+1) ) ;
        Channel[0].ChannelOffset := 0 ;

        // Sampling interval
        cdrFH.dt := edTauRise.Value / 2.0 ;
        if CdrFH.dt <= 0.0 then cdrFH.dt := 0.05 ;
        scDisplay.TScale := CdrFH.dt*Settings.TScale ;

        end ;

     { Fill MEPC start times list }
     t := edInitialDelay.Value ; ;
     for i := 0 to High(Sim.tStart) do begin
          Sim.tStart[i] := t ;
          Sim.tLast := t ;
         if rbRandom.Checked then begin
            // Random frequency
            t := t - ln(Random)/edFrequency.Value ;
            Sim.Amplitude[i] := RandG( edAmplitude.Value, edStDev.Value ) ;
            end
         else begin
            //Fixed frequency
            Freq := Max(RandG(edFrequency.Value,edFrequencySD.Value),edFrequency.LoLimit) ;
            SteadyStateDepression := (1.0 - edDepression.Value) ;
            Depression := SteadyStateDepression
                          - ((SteadyStateDepression-1.0)*exp(-(t - edInitialDelay.Value)/edTauDepression.Value)) ;
            Sim.Amplitude[i] := binomial( edReleaseProbability.Value,
                                          Round(edReleasablePool.Value*Depression))*
                                          RandG(edAmplitude.Value,edStDev.Value) ;
            t := t + 1.0/Freq ;
            end ;
         end ;

     Sim.TEndAt := edSimDuration.Value ;
     Sim.NumMEPCs := 0 ;
     Sim.LPFilterFirstCall := True ;

     { Position data file pointer at end of data }
     CdrFH.FilePointer := FileSeek(CdrFH.FileHandle,
     (CdrFH.NumSamplesInFile*2) + CdrFH.NumBytesInHeader,0) ;

     ChOffset := Channel[0].ChannelOffset ;
     xLimit := edTauDecay.Value*6.0 ;

     omega := 2.0*Pi*edSineFrequency.Value ;
     Channel[0].ADCZero := 0 ;

     Done := False ;
     { Create simulated MEPCs }
     while not Done do begin

         Channel[0].xMin := Sim.t ;

         { Clear all channels }
         for ch := 0 to cdrFH.NumChannels-1 do
             for i := 0 to cdrFH.NumSamples-1 do ADC[i] := 0 ;

         { Create a buffer-ful of simulated samples }
         for i := 0 to NumSamplesPerBuffer-1 do begin

             { Create background noise with gaussian distribution
               and sine wave interference }
             y := RandG(0.0,edNoiseRMS.Value)
                  + edSineAmplitude.Value*sin(Sim.t*omega) ;

             for j := 0 to High(Sim.tStart) do
                 if Sim.t > Sim.tStart[j] then begin
                 x := Sim.t - Sim.tStart[j] ;
                 yMEPC := Sim.Amplitude[j]*exp(-x/Max(edTauDecay.Value,CDRFH.dt))  ;
                 if edTauRise.Value > 0. then
                    yMEPC := yMEPC*(1.0-exp(-x/Max(edTauRise.Value,CDRFH.dt))) ;
                    y := y + yMEPC ;
                    { If MEPC decayed to zero, put a new one into this slot }

                    if x >= xLimit then begin
                       if rbRandom.Checked then begin
                          Sim.tStart[j] := Sim.tLast - ln(Random)/edFrequency.Value ;
                          Sim.tLast := Sim.tStart[j] ;
                          end
                       else begin
                          Freq := Max(RandG(edFrequency.Value,edFrequencySD.Value),edFrequency.LoLimit) ;
                          SteadyStateDepression := (1.0 - edDepression.Value) ;
                          Depression := SteadyStateDepression
                                        - ((SteadyStateDepression-1.0)*exp(-(Sim.tLast - edInitialDelay.Value)/edTauDepression.Value)) ;
                          Sim.Amplitude[j] := binomial( edReleaseProbability.Value,
                                              Round(edReleasablePool.Value*Depression))*
                                              RandG(edAmplitude.Value,edStDev.Value) ;

                          Sim.tStart[j] := Sim.tLast + 1.0/Freq ;
                          Sim.tLast := Sim.tStart[j] ;

                          end ;

                       Inc(Sim.NumMEPCs) ;
                       end ;
                    end ;

                 Sim.t := Sim.t + cdrFH.dt ;
                 j := cdrFH.NumChannels*i + ChOffset ;
                 ADC[j] := Round(y/Channel[0].ADCScale) + Channel[0].ADCZero ;
                 end ;

         Channel[0].xMax := Sim.t ;

         { Save data to file }
         NumBytesToWrite := NumSamplesPerBuffer*CdrFH.NumChannels*2 ;
         cdrFH.NumSamplesInFile := cdrFH.NumSamplesInFile
                                   + (NumSamplesPerBuffer*CdrFH.NumChannels) ;
        if FileWrite(CdrFH.FileHandle,ADC,NumBytesToWrite) <> NumBytesToWrite then
           WriteToLogFile(' Error writing to ' + CdrFH.FileName ) ;

        if (Sim.t >= Sim.tEndAt) or bStart.Enabled then begin
           Done := True ;
           bStart.Enabled := True ;
           bAbort.Enabled := False ;
           end ;

        scDisplay.xOffset := Round( Channel[0].xMin /CdrFH.dt ) ;
        scDisplay.Invalidate ;

        // Update status bar
        Main.StatusBar.SimpleText := format(
        'MPSC Simulation : %.1f/%.1f s (%d mPSCs created)',
        [Sim.t,Sim.tEndAt,Sim.NumMEPCs]) ;
        Application.ProcessMessages ;

        end ;

     // Update status bar
     Main.StatusBar.SimpleText := format(
     'MPSC Simulation : %d mPSCs created over %.1f s',
     [Sim.NumMEPCs,Sim.tEndAt]) ;

     end ;


procedure TSimMEPCFrm.bAbortClick(Sender: TObject);
{ --------------------
  Abort simulation run
  --------------------}
begin
     bStart.Enabled := True ;
     bAbort.Enabled := False ;
     end;


procedure TSimMEPCFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

    // Save settings
    Settings.SimEPC.Duration := edSimDuration.Value ;
    Settings.SimEPC.Amplitude := edAmplitude.Value ;
    Settings.SimEPC.AmplitudeSD := edStDev.Value ;
    Settings.SimEPC.TauRise := edTauRise.Value ;
    Settings.SimEPC.TauDecay := edTauDecay.Value ;
    Settings.SimEPC.NoiseSD := edNoiseRMS.Value ;
    Settings.SimEPC.Frequency := edFrequency.Value ;
    Settings.SimEPC.FrequencySD := edFrequencySD.Value ;
    Settings.SimEPC.Delay := edInitialDelay.Value ;
    Settings.SimEPC.RandomEvents := rbRandom.Checked ;
    Settings.SimEPC.SineAmplitude:= edSineAmplitude.Value ;
    Settings.SimEPC.SineFrequency:= edSineFrequency.Value ;
    Settings.SimEPC.ReleaseProbability := edReleaseProbability.Value ;
    Settings.SimEPC.ReleasablePool := edReleasablePool.Value ;
    Settings.SimEPC.Depression := edDepression.Value ;
    Settings.SimEPC.TauDepression := edTauDepression.Value ;
    Settings.SimEPC.UnitsIndex := cbUnits.ItemIndex ;

     if CdrFH.NumSamplesInFile > 0 then begin
        Main.mnViewSig.Enabled := True ;
        Main.mnViewSig.checked := True ;
        Main.Analysis.Enabled := True ;
        end ;

     SaveCDRHeader( CdrFH ) ;
     WriteToLogFile( format( '%d MEPCs created',[Sim.NumMEPCs])) ;

     { Enable "Synaptic Current" item in "Simulation" menu }
     Main.mnMEPCSim.enabled := true ;
     { Display results }

     if CdrFH.NumSamplesInFile > 0 then Main.UpdateMDIWIndows ;
     Action := caFree ;
     end;


procedure TSimMEPCFrm.GaussianFilter(
          Var Buf,OldBuf : Array of SmallInt ;
          NumSamples,NumChannels : Integer ;
          CutOffFrequency : single ;
          var FirstCall : Boolean ) ;
{ --------------------------------------------------
  Gaussian digital filter. (based on Sigworth, 1983)
  --------------------------------------------------}
const
     MaxCoeff = 54 ;
type
    TWorkArray = Array[-2*MaxCoeff..MaxTBuf] of Integer ;
var
   a : Array[-MaxCoeff..MaxCoeff] of single ;
   b : Array[-2*MaxCoeff..0] of single ;
   Temp,sum,sigma,aScale : single ;
   i,iIn,iOut,j,j1,nca,ncb,Ch,ChOffset,EndOfData : Integer ;
   Work : ^TWorkArray ;
begin

      New(Work) ;

      try

      { Generate filter coefficients }
      sigma := 0.132505/CutOffFrequency ;
      if  sigma >= 0.62  then begin

	     aScale := -1./(2.*sigma*sigma) ;
	     nca := 0 ;
	     a[0] := 1.0 ;
	     sum := 1.0 ;
	     temp := 1.0 ;
	     while (temp >= 10.0*MinSingle) and (nca < MaxCoeff) do begin
	           Inc(nca) ;
	           temp := exp( nca*nca*aScale ) ;
	           a[nca] := temp ;
                   a[-nca] := Temp ;
	           sum := sum + 2.0*temp ;
                   end ;

             { Normalise coefficients so that they summate to 1. }
	     for i := -nca to nca do a[i] := a[i]/sum ;
             end
         else begin
            { Special case for very light filtering
              (See Colquhoun & Sigworth, 1983) }
            a[1] := (sigma*sigma)/2. ;
            a[-1] := a[1] ;
	    a[0] := 1.0 - 2.0*a[2] ;
	    nca := 1 ;
            end ;

         ncb := 1 ;
         for i := nca downto -nca do begin
             Dec(ncb) ;
             b[ncb] := a[i] ;
             end ;

         { Copy data into work array }
         for i := 0 to (NumSamples*NumChannels)-1 do Work^[i] := Buf[i] ;

         { If this is the first call to the filter, fill OldBuf
           buffer with the first samples in Buf }
         if FirstCall then begin
            for ch := 0 to NumChannels-1 do begin
                ChOffset := Channel[ch].ChannelOffset ;
                for i := 0 to NumSamples-1 do begin
                    j := i*NumChannels + ChOffset ;
                    OldBuf[j] := Buf[ch] ;
                    end ;
                end ;
            FirstCall := False ;
            end ;

         { Apply filter to each channel }
         for Ch := 0 to NumChannels-1 do begin

             { Get samples at end of old buffer still to needed by filter }
             j := NumSamples - NumChannels + Channel[ch].ChannelOffset ;
             for i := -1 downto Low(Work^) do begin
                 Work^[i] := OldBuf[j] ;
                 j := j - NumChannels ;
                 end ;

             { Copy a channel from the new data to work buffer }
             j := Channel[ch].ChannelOffset ;
             for i := 0 to NumSamples-1 do begin
                 Work^[i] := Buf[j] ;
                 j := j + NumChannels ;
                 end ;

             { Apply gaussian filter to each point
               and store result back in buffer }
             iOut := Channel[ch].ChannelOffset ;
             EndOfData := NumSamples -1 ;
             for iIn := 0 to EndOfData do begin
	         sum := 0.0 ;
	         for j := ncb to 0 do begin
                     j1 := (j+iIn) ;
	             sum := sum + (Work^[j1])*b[j] ;
                     end ;
                 Buf[iOut] := Round(sum) ;
                 iOut := iOut + NumChannels ;
                 end ;
             end ;
      finally
             { Keep old data buffer because next call will need it }
             for i := 0 to (NumSamples*NumChannels)-1 do OldBuf[i] := Buf[i] ;
             Dispose(Work) ;
             end ;
      end ;



procedure TSimMEPCFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     { Prevent form being closed if a simulation is running (Close button disabled) }
     if bStart.Enabled then CanClose := True
                       else CanClose := False ;
     end;


procedure TSimMEPCFrm.FormResize(Sender: TObject);
{ ------------------------------------------------------
  Adjust size/position of controls when form is re-sized
  ------------------------------------------------------ }
begin

      scDisplay.Height := max(ClientHeight - scDisplay.Top - 5,2) ;
      scDisplay.Width := Max(ClientWidth - scDisplay.Left - 5,2) ;

      end;


procedure TSimMEPCFrm.scDisplayCursorChange(Sender: TObject);
begin
     Channel[0].yMin := scDisplay.YMin[0] ;
     Channel[0].yMax := scDisplay.YMax[0] ;
     end;


procedure TSimMEPCFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.DisplayGrid := Settings.DisplayGrid ;

     scDisplay.TScale := CdrFH.dt*Settings.TScale ;
     scDisplay.TUnits := Settings.TUnits ;
     scDisplay.Invalidate ;
     end ;


procedure  TSimMEPCFrm.ZoomOut ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     scDisplay.MaxADCValue := Channel[0].ADCMaxValue ;
     scDisplay.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     scDisplay.ZoomOut ;
     end ;


procedure TSimMEPCFrm.rbRandomClick(Sender: TObject);
begin
     panFrequencySD.Visible := rbFixed.Checked ;
     end;

procedure TSimMEPCFrm.rbFixedClick(Sender: TObject);
begin
     panFrequencySD.Visible := rbFixed.Checked ;
     end;

procedure TSimMEPCFrm.cbUnitsChange(Sender: TObject);
// -------------
// Units changed
// -------------
begin
     Channel[0].ADCUnits := cbUnits.Text ;
     edAmplitude.Units := Channel[0].ADCUnits ;
     edStDev.Units := Channel[0].ADCUnits ;
     edNoiseRMS.Units := Channel[0].ADCUnits ;
     edSineAmplitude.Units := Channel[0].ADCUnits ;
     scDisplay.ChanUnits[0] := cbUnits.Text ;
     end;

function TSimMEPCFrm.binomial( pIn, n : Single ) : Single ;
{ --------------------------------------------
  Binomial random number generator
  Returns a number from the distribution B(pIn,n)
  where pIn = probability, n = number of items
  (Base on Numerical Recipes code)
  --------------------------------------------}
var
   p,mean,r,em,g,t,oldg,pc,pclog,y,plog,sq,zz : Single ;
   i : Integer ;
   quit : Boolean ;
begin

	if pIn > 0.5 then p := 1. - pIn
                     else p := pIn ;

	mean := n*p ;
	if n <= 25.  then begin
	    r := 0. ;
	    for i := 1 to Round(n) do if random < p then r := r + 1. ;
            end
	else if mean < 1. then begin
	    g := exp(-mean) ;
	    t := 1. ;
	    r := 0. ;
	    while( (r<n) and (t<g) ) do begin
		t := t*random ;
		r := r + 1. ;
                end ;
            end
	else begin
	   oldg := gammln(n+1. ) ;
	   pc := 1. - p ;
	   plog := ln(p) ;
	   pclog := ln(pc) ;
	   sq := sqrt(2.*mean*pc) ;

	   quit := False ;
	   while( not quit ) do begin
         { Make sure TAN(infinity) is not calculated }
         repeat zz := random until (Abs(zz-0.5)>0.001) ;
	       y := sin(zz*Pi) / cos(zz*Pi) ;
	       em := sq*y + mean ;
	       if (em >= 0. ) and (em < n+1. ) then begin
		        em := Int(em) ;
		        t := 1.2*sq*(1.+y*y)*exp(oldg-gammln(em+1. ) -
     		          gammln(n-em+1. ) + em*plog + (n-em)*pclog) ;
		        if( random <= t ) then quit := True ;
            end ;
         end ;
	    r := em ;
	    end ;

	if ( p <> pIn ) then r := n - r ;
	binomial := r ;
        end ;

function TSimMEPCFrm.gammln( xx : Single ) : Single ;
var
   stp,x,tmp,ser : Double ;
   cof : Array[1..7] of Double ;
   i : Integer ;
begin
	cof[1] := 76.18009173 ;
	cof[2] := -86.50532033 ;
	cof[3] := 24.01409822 ;
	cof[4] := -1.231739516 ;
	cof[5] := 0.120858003E-2 ;
	cof[6] := -0.536382E-5 ;
	stp := 2.50662827465 ;

	x := (xx - 1. ) ;
	tmp := x + 5.5 ;
	tmp := ( x + 0.5)*ln(tmp) - tmp;
	ser := 1. ;
	for i := 1 to 6 do begin
	    x := x + 1. ;
	    ser := ser + cof[i]/x ;
	    end ;
	tmp := tmp + ln(stp*ser) ;
	gammln := tmp ;
        end ;




end.
