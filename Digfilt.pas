unit DigFilt;
{ ==========================================================
  WinEDR - Digital high/low pass filter module
  (c) J. Dempster, University of Strathclyde, 1999-2005
  10/6/99 Filter modified to support high as well as low pass
  16/5/01 Bug where channels got mixed up in 1st 256 samples fixed
  10/7/01 Channels can now be filtered individually
  13/2/03 Progress now reported on status bar
  13/9/04 Un-filtered bit at end prevented (possibly, needs further attention)
  19/1/05 End of file now low pass filtered correctly
          HP filter now implemented with forward-backward Butterworth
  03/6/09 Mixing of multiple channels in first block of low pass filter fixed
  27/10/9 Notch filter now works correctly at centre frequencies which are
          a small fraction of sampling frequency.
  14/06/10 Low pass filter can now also do sample rate reduction
           Filtered data now created as separate file Backup no longer created).
           Channels increased to 16
  14/08/12 Filtered signals can now be written to new channels
  15/08/12 Extra low pass filter cut-off added   

  ========================================================== }


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, ComCtrls, global, fileio, ValidatedEdit, math,
  ExtCtrls, strutils ;

type
  TDigFilterDlg = class(TForm)
    GroupBox1: TGroupBox;
    bOK: TButton;
    bCancel: TButton;
    rbLowPass: TRadioButton;
    rbHighPass: TRadioButton;
    ChannelsGrp: TGroupBox;
    ckInUse0: TCheckBox;
    ckInUse1: TCheckBox;
    ckInUse2: TCheckBox;
    ckInUse3: TCheckBox;
    ckInUse4: TCheckBox;
    ckInUse5: TCheckBox;
    ckInUse6: TCheckBox;
    ckInUse7: TCheckBox;
    Filter: TPageControl;
    LPFilter: TTabSheet;
    Label1: TLabel;
    edLPCutOffFreq: TValidatedEdit;
    HPFilter: TTabSheet;
    Label2: TLabel;
    cbHPFilter: TComboBox;
    rbNotchFilter: TRadioButton;
    Label3: TLabel;
    edSamplingRateReductionFactor: TValidatedEdit;
    ckInuse8: TCheckBox;
    ckInuse9: TCheckBox;
    ckInuse10: TCheckBox;
    ckInuse11: TCheckBox;
    ckInuse12: TCheckBox;
    ckInuse13: TCheckBox;
    ckInuse14: TCheckBox;
    ckInuse15: TCheckBox;
    NFFilter: TTabSheet;
    Label4: TLabel;
    edNFCutOffFreq: TValidatedEdit;
    GroupBox2: TGroupBox;
    rbNewChannel: TRadioButton;
    rbSameChannel: TRadioButton;
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bCancelClick(Sender: TObject);
    procedure ckInUse0Click(Sender: TObject);
    procedure rbLowPassClick(Sender: TObject);
    procedure rbHighPassClick(Sender: TObject);
    procedure rbNotchFilterClick(Sender: TObject);
  private
    { Private declarations }
    UseChannel : Array[0..EDRChannelLimit] of Boolean ;
    procedure SetChannelCheckBox(
              ckInUse : TCheckBox ;
              ChanNum : Integer
              ) ;
    procedure LowPassFilter ;
    procedure ButterworthHPFilter ;
    procedure NotchFilter ;
  public
    { Public declarations }
  end;

var
  DigFilterDlg: TDigFilterDlg;

implementation

uses Mdiform;

{$R *.DFM}

var
   Abort : Boolean ;


procedure TDigFilterDlg.FormShow(Sender: TObject);
{ -----------------------------------
  Initialise control settings on form
  -----------------------------------}
var
    NyquistFreq : Single ;
begin

     { Set channel in use check boxes }
     SetChannelCheckBox( ckInUse0, 0 ) ;
     SetChannelCheckBox( ckInUse1, 1 ) ;
     SetChannelCheckBox( ckInUse2, 2 ) ;
     SetChannelCheckBox( ckInUse3, 3 ) ;
     SetChannelCheckBox( ckInUse4, 4 ) ;
     SetChannelCheckBox( ckInUse5, 5 ) ;
     SetChannelCheckBox( ckInUse6, 6 ) ;
     SetChannelCheckBox( ckInUse7, 7 ) ;
     SetChannelCheckBox( ckInUse8, 8 ) ;
     SetChannelCheckBox( ckInUse9, 9 ) ;
     SetChannelCheckBox( ckInUse10, 10 ) ;
     SetChannelCheckBox( ckInUse11, 11 ) ;
     SetChannelCheckBox( ckInUse12, 12 ) ;
     SetChannelCheckBox( ckInUse13, 13) ;
     SetChannelCheckBox( ckInUse14, 14 ) ;
     SetChannelCheckBox( ckInUse15, 15 ) ;

    { Set limits and initial value of LP cut-off frequency }
    edLPCutOffFreq.Scale := 1.0 / CdrFH.dt ;
    edLPCutOffFreq.LoLimit := 0.132505/200.0 ;
    edLPCutOffFreq.HiLimit := 0.132505/0.5 ;
    edLPCutOffFreq.Value := 0.132505/2.5 ;

    // Set cut-off frequencies for HP filter
    NyquistFreq := 1.0 / (CDRFH.dt*2.0) ;
    cbHPFilter.Clear ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.0005*NyquistFreq]),TObject(1)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.001*NyquistFreq]),TObject(2)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.002*NyquistFreq]),TObject(3)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.003*NyquistFreq]),TObject(4)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.004*NyquistFreq]),TObject(5)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.005*NyquistFreq]),TObject(6)) ;
    cbHPFilter.ItemIndex := 0 ;

    { Set limits and initial value of NF cut-off frequency }
    edNFCutOffFreq.Scale := 1.0 / CdrFH.dt ;
    edNFCutOffFreq.LoLimit := 0.132505/200.0 ;
    edNFCutOffFreq.HiLimit := 0.132505/0.5 ;
    edNFCutOffFreq.Value := 0.132505/2.5 ;

    rbLowPass.Checked := True ;
    Filter.ActivePage := LPFilter ;

    Abort := False ;

    end;


procedure TDigFilterDlg.bOKClick(Sender: TObject);
// ------------
// Apply filter
// ------------
begin

    { Disable button }
    bOK.Enabled := False ;

    // Backup no longer needed since new file created for output of filter
    { Make a back up copy of original data file if one doesn't already exist }
    //Main.StatusBar.SimpleText :=
    // ' Digital Filter : Making back-up of original data file' ;
    //MakeBackupFile ;

    // Apply selected filter
    if rbLowPass.Checked then LowPassFilter
    else if rbHighPass.Checked then ButterworthHPFilter
    else NotchFilter ;

    { Re-enable button }
    bOK.Enabled := True ;

    end ;


procedure TDigFilterDlg.LowPassFilter ;
{ ----------------------------------------------------------------
  Gaussian low/high pass digital filter. (based on Sigworth, 1983)
  ---------------------------------------------------------------- }
const
     MaxCoeff = 128 ;
     MinBlocksPerBuffer = 256 ;
var
   a : Array[-MaxCoeff..MaxCoeff] of single ;
   InBuf : PSmallIntArrayDyn ;
   OutBuf : PSmallIntArrayDyn ;
   Work :  PSmallIntArrayDyn ;
   Temp,sum,sigma,aScale : single ;
   i,j,k,Ch,Coeff,Src,Dest,jFrom,jTo : Integer ;
   BufSize : Integer ;             { No. of samples in file I/O buffer }
   NumBuffersToDo : Integer ; { No. of input buffers to be processed }
   NumBuffersRead : Integer ;  // No. of buffer still to be read
   NumBuffersWritten : Integer ;  // No. of buffer still to be written
   NumSamples : Integer ;          { Maximum sample in filter work buffer }
   NumCoeffs : Integer ;           { Maximum filter coefficient index }
   FirstBuffer : Boolean ;
   InBlockStart,OutBlockStart : Integer ;
   iStart : Integer ;
   FilterOperation : String ;
   NumBlocksPerBuffer : Integer ;
   SampleReductionFactor : Integer ;
   OutFH : TCDRFileHeader ;
   ChannelMap : Array[0..MaxChannels-1] of Integer ;
begin

    // Type of filtering
    FilterOperation := format('[LP=%.4gHz RD=%d]',
                       [edLPCutOffFreq.Value*edLPCutOffFreq.Scale,
                        Round(edSamplingRateReductionFactor.Value)]) ;

    { Generate filter coefficients }
    sigma := 0.132505/(edLPCutOffFreq.Value) ;
    if sigma >= 0.62  then begin

       aScale := -1./(2.*sigma*sigma) ;
       NumCoeffs := 0 ;
       a[0] := 1.0 ;
       sum := 1.0 ;
       temp := 1.0 ;
       while (temp >= 10.0*MinSingle) and (NumCoeffs < MaxCoeff) do begin
          Inc(NumCoeffs) ;
          temp := exp( NumCoeffs*NumCoeffs*aScale ) ;
          a[NumCoeffs] := temp ;
          a[-NumCoeffs] := Temp ;
          sum := sum + 2.0*temp ;
          end ;

      { Normalise coefficients so that they summate to 1. }
      for i := -NumCoeffs to NumCoeffs do a[i] := a[i]/sum ;
      end
    else begin
      { Special case for very light filtering (See Colquhoun & Sigworth, 1983) }
      a[1] := (sigma*sigma)/2. ;
      a[-1] := a[1] ;
      a[0] := 1.0 - 2.0*a[1] ;
      NumCoeffs := 1 ;
      end ;

    // No. of blocks in I/P buffer (taking into account sample reduction factor
    SampleReductionFactor := Round(edSamplingRateReductionFactor.Value) ;
    NumBlocksPerBuffer := MinBlocksPerBuffer*SampleReductionFactor ;
    BufSize := NumBlocksPerBuffer*CdrFH.NumChannels ;

    // Allocate buffers
    GetMem( InBuf, BufSize*2 ) ;
    GetMem( OutBuf, BufSize*4 ) ;
    GetMem( Work, BufSize*4 ) ;

    // Create output file
    OutFH := CDRFH ;
    OutFH.dt := OutFH.dt*edSamplingRateReductionFactor.Value ;
    OutFH.FileName := ANSIReplaceText( OutFH.FileName,
                                       '.edr',
                                       FilterOperation + '.edr') ;
    OutFH.FileHandle := FileCreate( OutFH.FileName ) ;
    OutFH.NumSamplesInFile := 0 ;

     // Set output channel mapping for filtered channels
     if rbNewChannel.Checked then begin
        // Output to extra channels
        for ch := 0 to CDRFH.NumChannels-1 do if UseChannel[ch] then begin
            Inc(OutFH.NumChannels) ;
            ChannelMap[ch] := OutFH.NumChannels-1 ;
            Channel[ChannelMap[ch]] := Channel[ch] ;
            Channel[ChannelMap[ch]].ChannelOffset := ChannelMap[ch] ;
            Channel[ChannelMap[ch]].ADCName := Channel[ch].ADCName + '(f)';
            end ;
        end
     else begin
        // Overwrite existing channels
        for ch := 0 to CDRFH.NumChannels-1 do ChannelMap[ch] := ch ;
        end ;

    NumBuffersToDo := CdrFH.NumSamplesInFile div BufSize ;
    NumBuffersRead := 0 ;
    NumBuffersWritten := 0 ;
    NumSamples := NumCoeffs*CdrFH.NumChannels ;
    Src := BufSize ;
    Dest := 0 ;

    { Point to start of A/D data }
    InBlockStart := 0 ;
    // Fill output buffer with original unfiltered data
    // (in case some channels are not selected for filtering)
    OutBlockStart := 0 ;
    ReadCDRBuffer(CdrFH,OutBlockStart,OutBuf^,NumBlocksPerBuffer) ;
    // Expand to fill O/P buffer
    for i := NumBlocksPerBuffer-1 downto 0 do begin
        jFrom := i*CDRFH.NumChannels ;
        jTo := i*OutFH.NumChannels ;
        for ch := 0 to CDRFH.NumChannels-1 do begin
            OutBuf^[JTo+ch] := OutBuf^[JFrom+ch] ;
            end ;
        end ;

    FirstBuffer := True ;
    while (NumBuffersWritten < NumBuffersToDo) and (not Abort) do begin

       { Get data from input data file }
       if Src >= BufSize then begin
          if NumBuffersRead < NumBuffersToDo then begin
             ReadCDRBuffer(CdrFH,InBlockStart,InBuf^,NumBlocksPerBuffer) ;
             InBlockStart := InBlockStart + NumBlocksPerBuffer ;
             end
          else begin
             { No more buffers, fill buffer with last sample block }
             ch := BufSize - CdrFH.NumChannels ;
             for i := 0 to BufSize-1 do begin
                 InBuf[i] := InBuf[ch] ;
                 Inc(ch) ;
                 if ch >= BufSize then ch := BufSize - CdrFH.NumChannels ;
                 end ;
             end ;
          Inc(NumBuffersRead) ;
          Src := 0 ;
          // Report progress
          Main.StatusBar.SimpleText := format(
          ' Digital Filter : Filtering signal %s %.0f%% done.',
          [FilterOperation,(NumBuffersWritten*100.0)/NumBuffersToDo]) ;
          end ;

       { If first buffer, fill working array }
       if FirstBuffer then begin
          for Coeff := -NumCoeffs to NumCoeffs do begin
              for ch := 0 to CdrFH.NumChannels-1 do begin
                  i := Coeff*CdrFH.NumChannels + ch ;
                  if i >= 0 then Work[i+BufSize] := InBuf[i]
                            else Work[i+BufSize] := InBuf[ch] ;
                  end ;
              end ;
          iStart := -NumSamples ;
          FirstBuffer := False ;
          Src := NumCoeffs*CdrFH.NumChannels ;
          end ;

       { Apply gaussian filter to each channel }
       for ch := 0 to CdrFH.NumChannels-1 do begin
           if UseChannel[ch] then begin
              Sum := 0.0 ;
              j := iStart + ch ;
              for Coeff := -NumCoeffs to NumCoeffs do begin
                  Sum := Sum + Work[j+BufSize]*a[Coeff] ;
                  j := j + CdrFH.NumChannels ;
                  if j > NumSamples then j := -NumSamples + ch ;
                  end ;
              OutBuf[Dest+ChannelMap[ch]] := Round(Sum) ;
              end ;
//           Inc(Dest) ;
           end ;
       Dest := Dest + OutFH.NumChannels ;

       { Get next block of samples from input buffer }
       for ch := 0 to CdrFH.NumChannels-1 do begin
           Work[iStart+ch+BufSize] := InBuf[Src] ;
           Inc(Src) ;
           end ;

       { Increment start pointer }
       iStart := iStart + CdrFH.NumChannels ;
       if iStart > NumSamples then iStart := -NumSamples ;

       if Dest >= (NumBlocksPerBuffer*OutFH.NumChannels) then begin

          // Apply sample reduction factor
          for i := 0 to MinBlocksPerBuffer-1 do begin
                 j := i*OutFH.NumChannels ;
                 k := i*OutFH.NumChannels*SampleReductionFactor ;
                 for ch := 0 to OutFH.NumChannels-1 do OutBuf[j+ch] := OutBuf[k+ch] ;
             end ;

          // Write to file
          WriteCDRBuffer(OutFH,OutBlockStart,OutBuf^,MinBlocksPerBuffer) ;
          OutBlockStart := OutBlockStart + MinBlocksPerBuffer ;
          OutFH.NumSamplesInFile := OutFH.NumSamplesInFile + MinBlocksPerBuffer*OutFH.NumChannels ;

          // Fill O/P buffer with old values
          ReadCDRBuffer(CdrFH,OutBlockStart*SampleReductionFactor,OutBuf^,NumBlocksPerBuffer) ;
          for i := NumBlocksPerBuffer-1 downto 0 do begin
              jFrom := i*CDRFH.NumChannels ;
              jTo := i*OutFH.NumChannels ;
              for ch := 0 to CDRFH.NumChannels-1 do begin
                  OutBuf^[JTo+ch] := OutBuf^[JFrom+ch] ;
                  end ;
              end ;

          Dest := 0 ;
          Inc(NumBuffersWritten) ;
          end ;

       end ;

    // Update output file and open for display
    SaveCDRHeader( OutFH ) ;
    FileClose( OutFH.FileHandle ) ;
    FileClose( CDRFH.FileHandle ) ;
    Main.LoadDataFiles( OutFH.FileName ) ;

    Main.StatusBar.SimpleText := 'Digital Filter: File created ' +
                                 OutFH.FileName ;
    WriteToLogFile( Main.StatusBar.SimpleText ) ;

    FreeMem(InBuf) ;
    FreeMem(OutBuf) ;
    FreeMem(Work) ;

    end ;


procedure TDigFilterDlg.ButterworthHPFilter ;
// ----------------------------------------------
// Forward-backward butterworth high pass filter
// ----------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 5 ;

  b001 : Array[0..4] of extended
  = ( 0.997949760065881,  -3.991799040263525,   5.987698560395288,
      -3.991799040263525,   0.997949760065881) ;
   a001 : Array[0..4] of extended
  = ( 1.000000000000000,  -3.995895311592879,   5.987694356914537,
       -3.987702768931133,  0.995903723615550) ;

   b002 : Array[0..4] of extended
   = ( 0.99590372213842,  -3.98361488855370,   5.97542233283055,
       -3.98361488855370, 0.99590372213842 ) ;
   a002 : Array[0..4] of extended
   = (1.00000000000000,  -3.99179062441447,   5.97540555338674,
      -3.97543915264442, 0.99182422376917 ) ;

   b003 : Array[0..4] of extended
   = ( 0.99182421200053,  -3.96729684800213,   5.95094527200320,
       -3.96729684800213, 0.99182421200053 ) ;
   a003 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.98358125865852,   5.95087842926670,
       -3.95101243657283, 0.98371526751048 ) ;

   b004 : Array[0..4] of extended
   = ( 0.98776138927683,  -3.95104555710730,   5.92656833566096,
       -3.95104555710730, 0.98776138927683 ) ;
   a004 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.97537191256092,   5.92641855596542,
       -3.92671919775679, 0.97567256214609 ) ;

   b005 : Array[0..4] of extended
   = ( 0.98371517412976,  -3.93486069651902,   5.90229104477854,
       -3.93486069651902, 0.98371517412976 ) ;
   a005 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.96716259594885,   5.90202586149088,
       -3.90255878482324, 0.96769554381314 ) ;

   b006 : Array[0..4] of extended
   = ( 0.97968548719040,  -3.91874194876162,   5.87811292314243,
       -3.91874194876162, 0.97968548719040 ) ;
   a006 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.95895331864708,   5.87770027353615,
       -3.87853054905174, 0.95978365381150 ) ;

var
    i,j,ch,jIn,jOut : Integer ;
    iScan : Integer ;
    NumScansPerBlock : Integer ;
    NumScansInFile : Integer ;
    NumScansRead : Integer ;
    BufIn : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    BufOut : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    FPSize : Integer ;
    x : extended ;
    y : Array[0..EDRChannelLimit] of extended ;
    z : Array[0..EDRChannelLimit,0..NumCoeffs-1] of extended ;
    a : Array[0..NumCoeffs-1] of extended ;
    b : Array[0..NumCoeffs-1] of extended ;
     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;
     FilePointer : Integer ;
     iCutOff : Integer ;
     iBlock  : Integer ;
     FilterOperation : String ;
     OutFH : TCDRFileHeader ;
     ChannelMap : Array[0..MaxChannels-1] of Integer ;
begin

    // Get selected cut off frequency
    iCutOff := Integer(cbHPFilter.Items.Objects[cbHPFilter.ItemIndex]) ;

    // Type of filtering
    FilterOperation := format('[HP=%s]',[cbHPFilter.Text]) ;

    // Select coefficients for selected cut-off frequency
    Case iCutOff of
       1 : Begin
          // 0.0005
          for i := 0 to High(a) do a[i] := a001[i] ;
          for i := 0 to High(b) do b[i] := b001[i] ;
          end ;
       2 : Begin
          // 0.001
          for i := 0 to High(a) do a[i] := a002[i] ;
          for i := 0 to High(b) do b[i] := b002[i] ;
          end ;
       3 : Begin
          // 0.002
          for i := 0 to High(a) do a[i] := a003[i] ;
          for i := 0 to High(b) do b[i] := b003[i] ;
          end ;
       4 : Begin
          // 0.003
          for i := 0 to High(a) do a[i] := a004[i] ;
          for i := 0 to High(b) do b[i] := b004[i] ;
          end ;
       5 : Begin
          // 0.004
          for i := 0 to High(a) do a[i] := a005[i] ;
          for i := 0 to High(b) do b[i] := b005[i] ;
          end ;
       6 : Begin
          // 0.005
          for i := 0 to High(a) do a[i] := a006[i] ;
          for i := 0 to High(b) do b[i] := b006[i] ;
          end ;
       end ;

     FPSize := SizeOf(x) ;

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
     NumScansPerBlock := CDRFH.NumSamplesPerBlock div CDRFH.NumChannels ;
     NumScansInFile := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;

     FileSeek( TempHandle1, 0,0) ;
     for iBlock := 0 to CDRFH.NumBlocksInFile-1 do begin

         // Read A/D data from source file
         iScan := iBlock*NumScansPerBlock ;
         NumScansRead := ReadCDRBuffer(CdrFH,iScan,BufIn,NumScansPerBuf) ;
         if NumScansRead <= 0 then Break ;

         // Copy to temp file #1
         for j := 0 to NumScansRead*CDRFH.NumChannels-1 do begin
             x := BufIn[j] ;
             FileWrite( TempHandle1, x, FPSize ) ;
             end ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' HP Filter: Reading source signal %.3g%%',
         [(100.0*iScan)/NumScansInFile]) ;

         end ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     // Initialise filter
     FileSeek( TempHandle1, 0, 0 ) ;
     for ch := 0 to CDRFH.NumChannels-1 do begin
        FileRead( TempHandle1, x, FPSize ) ;
        y[ch] := 0.0 ;
        z[ch,NumCoeffs-1] := 0.0 ;
        for j := NumCoeffs-1 downto 1 do
            z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
        end ;

     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for iScan := 0 to NumScansInFile-1 do begin

          for ch := 0 to CDRFH.NumChannels-1 do begin

              // Read value
              FileRead( TempHandle1, x, FPSize ) ;

              y[ch] := b[0]*x + z[ch,0] ;
              z[ch,NumCoeffs-1] := 0.0 ;
              for j := 1 to NumCoeffs-1 do
                  z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;

              FileWrite( TempHandle2, y[ch], FPSize )

              end ;

          // Report progress
          if (iScan mod NumScansPerBuf) = 0 then
             Main.StatusBar.SimpleText := format(
             ' HP Filter: Applying forward filter %.3g%%',
             [(100.0*iScan)/NumScansInFile]) ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

     // Initialise filter
     iScan := NumScansInFile - CDRFH.NumChannels ;
     for ch := 0 to CDRFH.NumChannels-1 do begin
         FilePointer := ((iScan*CDRFH.NumChannels) + ch)*FPSize ;
         FileSeek( TempHandle2, FilePointer, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y[ch] := 0.0 ;
         z[ch,NumCoeffs-1] := 0.0 ;
         for j := NumCoeffs-1 downto 1 do
             z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
         end ;

     // Apply filter
     for iScan := NumScansInFile - CDRFH.NumChannels downto 0 do begin
         for ch := 0 to CDRFH.NumChannels-1do begin
             // Read value
             FilePointer := ((iScan*CDRFH.NumChannels) + ch)*FPSize ;
             FileSeek( TempHandle2, FilePointer, 0 ) ;
             FileRead( TempHandle2, x, FPSize ) ;
             // Apply filter
             y[ch] := b[0]*x + z[ch,0] ;
             z[ch,NumCoeffs-1] := 0.0 ;
             for j := 1 to NumCoeffs-1 do
                 z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
             // Write value
             FileSeek( TempHandle1, FilePointer, 0 ) ;
             FileWrite( TempHandle1, y[ch], FPSize ) ;
             end ;

         // Report progress
         if (iScan mod NumScansPerBuf) = 0 then
            Main.StatusBar.SimpleText := format(
            ' HP Filter: Applying reverse filter %.3g%%',
            [(100.0*iScan)/NumScansInFile]) ;

          end ;

     // Copy results to output EDR data file
     // ----------------------------------

     // Create output file
     OutFH := CDRFH ;
     OutFH.FileName := ANSIReplaceText( OutFH.FileName,
                                        '.edr',
                                        FilterOperation + '.edr') ;
     OutFH.FileHandle := FileCreate( OutFH.FileName ) ;

     FileSeek( TempHandle1, 0,0) ;

     // Set output channel mapping for filtered channels
     if rbNewChannel.Checked then begin
        // Output to extra channels
        for ch := 0 to CDRFH.NumChannels-1 do if UseChannel[ch] then begin
            Inc(OutFH.NumChannels) ;
            ChannelMap[ch] := OutFH.NumChannels-1 ;
            Channel[ChannelMap[ch]] := Channel[ch] ;
            Channel[ChannelMap[ch]].ChannelOffset := ChannelMap[ch] ;
            Channel[ChannelMap[ch]].ADCName := Channel[ch].ADCName + '(f)';
            end ;
        end
     else begin
        // Overwrite existing channels
        for ch := 0 to CDRFH.NumChannels-1 do ChannelMap[ch] := ch ;
        end ;

     for iBlock := 0 to CDRFH.NumBlocksInFile-1 do begin

         // Read A/D data from source file
         iScan := iBlock*NumScansPerBlock ;
         NumScansRead := ReadCDRBuffer(CdrFH,iScan,BufIn,NumScansPerBuf) ;
         if NumScansRead <= 0 then Break ;

         // Copy to new file
         jIn := 0 ;
         jOut := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to CDRFH.NumChannels-1 do begin
                 FileRead( TempHandle1, x, FPSize ) ;
                 BufOut[jOut+ch] := BufIn[jIn+ch] ;
                 if UseChannel[ch] then BufOut[jOut+ChannelMap[ch]] := Round(x) ;
                 end ;
             jOut := jOut + OutFH.NumChannels ;
             jIn := jIn + CDRFH.NumChannels ;
             end ;

         // Write updated buffer back to file
         WriteCDRBuffer(OutFH,iScan,BufOut,NumScansPerBuf) ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' HP Filter: Writing to file %.3g%%',
         [(100.0*iScan)/NumScansInFile]) ;

         end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Update O/P file header and close
     SaveCDRHeader( OutFH ) ;
     FileClose( OutFH.FileHandle ) ;

     // Close source file
     FileClose( CDRFH.FileHandle ) ;

     Main.StatusBar.SimpleText := 'Digital Filter: File created ' +
                                  OutFH.FileName ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     // Open filtered file
     Main.LoadDataFiles( OutFH.FileName ) ;

     end ;


procedure TDigFilterDlg.NotchFilter ;
// ----------------------------------------------
// Forward-backward butterworth notch filter
// ----------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 3 ;

var
    i,j,ch,k,jIn,jOut : Integer ;
    iScan : Integer ;
    NumScansPerBlock : Integer ;
    NumScansInFile : Integer ;
    NumScansRead : Integer ;
    BufIn : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    BufOut : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
    FBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of Extended ;
    FPSize : Integer ;
    x : extended ;
    y : Array[0..EDRChannelLimit] of extended ;
    z : Array[0..EDRChannelLimit,0..NumCoeffs-1] of extended ;
    a : Array[0..NumCoeffs-1] of extended ;
    b : Array[0..NumCoeffs-1] of extended ;
     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;
     FilePointer : Integer ;
     iCutOff : Integer ;
     iBlock  : Integer ;
         NyquistFreq : Extended ;
    CentreFreq : Extended ;
    Bandwidth : Extended ;
    HalfPi : Extended ;
    D1,E1 : Extended ;
    FilterOperation : String ;
    OutFH : TCDRFileHeader ;
    ChannelMap : Array[0..MaxChannels-1] of Integer ;
begin

    // Type of filtering
    FilterOperation := format('[NF=%.4gHz]',[edNFCutOffFreq.Value*edNFCutOffFreq.Scale]) ;

    // Get selected cut off frequency
    NyquistFreq := 1.0 / (CDRFH.dt*2.0) ;
    CentreFreq := edNFCutOffFreq.Value*2.0 ;
    Bandwidth := CentreFreq / 100.0 ;
    HalfPi := Pi/2.0 ;
    D1 := tan( HalfPi*BandWidth );
    E1 := (2.0*cos(HalfPi*CentreFreq*2.0)) / cos(HalfPi*BandWidth) ;

    b[0] := 1.0 / ( 1 + D1 ) ;
    b[1] := -E1 / ( 1 + D1 ) ;
    b[2] := b[0] ;

    a[0] := 1.0 ;
    a[1] := -E1 / ( 1 + D1 ) ;
    a[2] := (1 - D1) / ( 1 + D1 ) ;

     FPSize := SizeOf(x) ;

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
     NumScansPerBlock := CDRFH.NumSamplesPerBlock div CDRFH.NumChannels ;
     NumScansInFile := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;

     FileSeek( TempHandle1, 0,0) ;
     for iBlock := 0 to CDRFH.NumBlocksInFile-1 do begin

         // Read A/D data from source file
         iScan := iBlock*NumScansPerBlock ;
         NumScansRead := ReadCDRBuffer(CdrFH,iScan,BufIn,NumScansPerBuf) ;
         if NumScansRead <= 0 then Break ;

         // Copy to floating point buffer
         for j := 0 to NumScansRead*CDRFH.NumChannels-1 do begin
             FBuf[j] := BufIn[j] ;
             end ;

         // Write to temp file #1
         FileWrite( TempHandle1, FBuf, NumScansRead*CDRFH.NumChannels*FPSize ) ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' Notch Filter: Reading source signal %.3g%%',
         [(100.0*iScan)/NumScansInFile]) ;

         Application.ProcessMessages ;
         if bOK.Enabled then begin
            FileClose( TempHandle1 ) ;
            FileClose( TempHandle2 ) ;
            Exit ;
            end ;

         end ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     // Initialise filter
     FileSeek( TempHandle1, 0, 0 ) ;
     for ch := 0 to CDRFH.NumChannels-1 do begin
        FileRead( TempHandle1, x, FPSize ) ;
        y[ch] := x ;
        //z[ch,NumCoeffs-1] := 0.0 ;
        z[ch,NumCoeffs-1] := y[ch] - b[NumCoeffs-1]*x ;
        for j := NumCoeffs-1 downto 1 do
            z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
        end ;


     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for iScan := 0 to NumScansInFile-1 do begin

          for ch := 0 to CDRFH.NumChannels-1 do begin

              // Read value
              FileRead( TempHandle1, x, FPSize ) ;

              y[ch] := b[0]*x + z[ch,0] ;
              z[ch,NumCoeffs-1] := 0.0 ;
              for j := 1 to NumCoeffs-1 do
                  z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;

              FileWrite( TempHandle2, y[ch], FPSize )

              end ;

          // Report progress
          if (iScan mod NumScansPerBuf) = 0 then begin
             Main.StatusBar.SimpleText := format(
             ' Notch Filter: Applying forward filter %.3g%%',
             [(100.0*iScan)/NumScansInFile]) ;
             Application.ProcessMessages ;
            if bOK.Enabled then begin
               FileClose( TempHandle1 ) ;
               FileClose( TempHandle2 ) ;
               Exit ;
               end ;

             end ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

     // Initialise filter
     iScan := NumScansInFile - CDRFH.NumChannels ;
     for ch := 0 to CDRFH.NumChannels-1 do begin
         FilePointer := ((iScan*CDRFH.NumChannels) + ch)*FPSize ;
         FileSeek( TempHandle2, FilePointer, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y[ch] := x ;
         z[ch,NumCoeffs-1] := 0.0 ;
         for j := NumCoeffs-1 downto 1 do
             z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
         end ;

     // Apply filter
     for iScan := NumScansInFile - CDRFH.NumChannels downto 0 do begin
         for ch := 0 to CDRFH.NumChannels-1do begin
             // Read value
             FilePointer := ((iScan*CDRFH.NumChannels) + ch)*FPSize ;
             FileSeek( TempHandle2, FilePointer, 0 ) ;
             FileRead( TempHandle2, x, FPSize ) ;
             // Apply filter
             y[ch] := b[0]*x + z[ch,0] ;
             z[ch,NumCoeffs-1] := 0.0 ;
             for j := 1 to NumCoeffs-1 do
                 z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
             // Write value
             FileSeek( TempHandle1, FilePointer, 0 ) ;
             FileWrite( TempHandle1, y[ch], FPSize ) ;
             end ;

         // Report progress
         if (iScan mod NumScansPerBuf) = 0 then begin
            Main.StatusBar.SimpleText := format(
            ' Notch Filter: Applying reverse filter %.3g%%',
            [(100.0*iScan)/NumScansInFile]) ;
            Application.ProcessMessages ;
            if bOK.Enabled then begin
               FileClose( TempHandle1 ) ;
               FileClose( TempHandle2 ) ;
               Exit ;
            end ;

            end ;

          end ;

     // Copy results to output EDR data file
     // ----------------------------------

     // Create output file
     OutFH := CDRFH ;
     OutFH.FileName := ANSIReplaceText( OutFH.FileName,
                                        '.edr',
                                        FilterOperation + '.edr') ;
     OutFH.FileHandle := FileCreate( OutFH.FileName ) ;

     // Set output channel mapping for filtered channels
     if rbNewChannel.Checked then begin
        // Output to extra channels
        for ch := 0 to CDRFH.NumChannels-1 do if UseChannel[ch] then begin
            Inc(OutFH.NumChannels) ;
            ChannelMap[ch] := OutFH.NumChannels-1 ;
            Channel[ChannelMap[ch]] := Channel[ch] ;
            Channel[ChannelMap[ch]].ChannelOffset := ChannelMap[ch] ;
            Channel[ChannelMap[ch]].ADCName := Channel[ch].ADCName + '(f)';
            end ;
        end
     else begin
        // Overwrite existing channels
        for ch := 0 to CDRFH.NumChannels-1 do ChannelMap[ch] := ch ;
        end ;

     FileSeek( TempHandle1, 0,0) ;
     for iBlock := 0 to CDRFH.NumBlocksInFile-1 do begin

         // Read A/D data from source file
         iScan := iBlock*NumScansPerBlock ;
         NumScansRead := ReadCDRBuffer(CdrFH,iScan,BufIn,NumScansPerBuf) ;
         if NumScansRead <= 0 then Break ;

         // Copy to new file
         jIn := 0 ;
         jOut := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to CDRFH.NumChannels-1 do begin
                 FileRead( TempHandle1, x, FPSize ) ;
                 BufOut[jOut+ch] := BufIn[jIn+ch] ;
                 if UseChannel[ch] then BufOut[jOut+ChannelMap[ch]] := Round(x) ;
                 end ;
             jOut := jOut + OutFH.NumChannels ;
             jIn := jIn + CDRFH.NumChannels ;
             end ;

         // Write updated buffer to O/P file
         WriteCDRBuffer(OutFH,iScan,BufOut,NumScansPerBuf) ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' Notch Filter: Writing to file %.3g%%',
         [(100.0*iScan)/NumScansInFile]) ;

         Application.ProcessMessages ;
         if bOK.Enabled then begin
            FileClose( TempHandle1 ) ;
            FileClose( TempHandle2 ) ;
            Exit ;
            end ;

         end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Update O/P file header and close
     SaveCDRHeader( OutFH ) ;
     FileClose( OutFH.FileHandle ) ;

     // Close source file
     FileClose( CDRFH.FileHandle ) ;

     Main.StatusBar.SimpleText := 'Digital Filter: File created ' +
                                  OutFH.FileName ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     // Open filtered file
     Main.LoadDataFiles( OutFH.FileName ) ;

     end ;


procedure TDigFilterDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     if not bOK.Enabled then CanClose := False ;
     end;


procedure TDigFilterDlg.bCancelClick(Sender: TObject);
begin
     Abort := True ;
     end;

procedure TDigFilterDlg.SetChannelCheckBox(
          ckInUse : TCheckBox ;          { Channel in use check box }
          ChanNum : Integer              { Channel Number }
          ) ;
{ ----------------------------
  Set channel in use check box
  ----------------------------}
begin
     if CdrFH.NumChannels > ChanNum then begin
          ckInUse.caption := format('Ch.%d %s',[ChanNum,Channel[ChanNum].ADCName])  ;
          ckInUse.enabled := True ;
          ckInUse.visible := True ;
          ckInUse.checked := True ;

          end
     else begin
          ckInUse.enabled := False ;
          ckInUse.visible := False ;
          ckInUse.checked := False ;
          end ;

     // UseChannel is in channel scan sequence order
     UseChannel[Channel[ChanNum].ChannelOffset] := ckInUse.checked ;

     end ;


procedure TDigFilterDlg.ckInUse0Click(Sender: TObject);
{ ----------------------------
  Set channels to be filtered
  ----------------------------}
var
   ch,NumChannels : Integer ;
begin
     UseChannel[Channel[0].ChannelOffset] := ckInUse0.checked ;
     UseChannel[Channel[1].ChannelOffset] := ckInUse1.checked ;
     UseChannel[Channel[2].ChannelOffset] := ckInUse2.checked ;
     UseChannel[Channel[3].ChannelOffset] := ckInUse3.checked ;
     UseChannel[Channel[4].ChannelOffset] := ckInUse4.checked ;
     UseChannel[Channel[5].ChannelOffset] := ckInUse5.checked ;
     UseChannel[Channel[6].ChannelOffset] := ckInUse6.checked ;
     UseChannel[Channel[7].ChannelOffset] := ckInUse7.checked ;
     UseChannel[Channel[8].ChannelOffset] := ckInUse8.checked ;
     UseChannel[Channel[9].ChannelOffset] := ckInUse9.checked ;
     UseChannel[Channel[10].ChannelOffset] := ckInUse10.checked ;
     UseChannel[Channel[11].ChannelOffset] := ckInUse11.checked ;
     UseChannel[Channel[12].ChannelOffset] := ckInUse12.checked ;
     UseChannel[Channel[13].ChannelOffset] := ckInUse13.checked ;
     UseChannel[Channel[14].ChannelOffset] := ckInUse14.checked ;
     UseChannel[Channel[15].ChannelOffset] := ckInUse15.checked ;

     { Ensure at least one channel is always displayed }
     NumChannels := 0 ;
     for ch := 0 to CdrFH.NumChannels-1 do if UseChannel[ch] then Inc(NumChannels) ;
     if NumChannels <= 0 then begin
        ckInUse0.checked := True ;
        UseChannel[0] := ckInUse0.checked ;
        end ;

     end;


procedure TDigFilterDlg.rbLowPassClick(Sender: TObject);
// ----------------------
// Select low pass filter
// ----------------------
begin
     Filter.ActivePage := LPFilter ;
     end;

procedure TDigFilterDlg.rbHighPassClick(Sender: TObject);
// ----------------------
// Select high pass filter
// ----------------------
begin
     Filter.ActivePage := HPFilter ;
     end;


procedure TDigFilterDlg.rbNotchFilterClick(Sender: TObject);
// ----------------------
// Select low pass filter
// ----------------------
begin
     Filter.ActivePage := NFFilter ;
     end;

end.
