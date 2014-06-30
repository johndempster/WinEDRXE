unit Import;
{ ==============================================================
  WinEDR - General purpose Binary/ASCII data file import module
  (c) J. Dempster, University of Strathclyde, 1998
  25/6/98 Binary file import now works,
          Bug in text file import fixed
  17/10/99 32 bit version
  ==============================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, TabNotBk, maths, shared, global, fileio,
  ValEdit, ComCtrls, ValidatedEdit;

type
  TImportFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    SaveDialog: TSaveDialog;
    Page: TPageControl;
    TextFormat: TTabSheet;
    BinaryFormat: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbTUnits: TComboBox;
    edNumRowsIgnored: TValidatedEdit;
    edNumColumns: TValidatedEdit;
    edTimeColumn: TValidatedEdit;
    edASCII_DT: TValidatedEdit;
    ckTimeDataAvailable: TCheckBox;
    mePreview: TMemo;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    lbNumBytesToImport: TLabel;
    cbTUnitsBinary: TComboBox;
    edNumFileHeaderBytes: TValidatedEdit;
    edNumChannelsBinary: TValidatedEdit;
    edNumBytesToImport: TValidatedEdit;
    edScaleBy: TValidatedEdit;
    edOffsetBy: TValidatedEdit;
    edDTBinary: TValidatedEdit;
    ProgressGrp: TGroupBox;
    prProgress: TProgressBar;
    bStop: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbTUnitsChange(Sender: TObject);
    procedure cbTUnitsBinaryChange(Sender: TObject);
    procedure ckTimeDataAvailableClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
  private
    { Private declarations }
    procedure InspectFile ;
    procedure ImportText( FileName : string ) ;
    procedure ImportBinary( FileName : string ) ;
  public
    { Public declarations }
    FileName : string ;
  end;

var
  ImportFrm: TImportFrm;

implementation

uses Mdiform;

{$R *.DFM}


procedure TImportFrm.FormShow(Sender: TObject);
{ -----------------------------------
  Display first 10 lines of data file
  -----------------------------------}
begin

     cbTUnits.Clear ;
     cbTUnits.Items.Add('s') ;
     cbTUnits.Items.Add('ms') ;
     cbTUnits.Items.Add('us') ;
     cbTUnits.ItemIndex := 1 ;

     cbTUnitsBinary.Clear ;
     cbTUnitsBinary.Items.Add('s') ;
     cbTUnitsBinary.Items.Add('ms') ;
     cbTUnitsBinary.Items.Add('us') ;
     cbTUnitsBinary.ItemIndex := 1 ;

     if ckTimeDataAvailable.Checked then edTimeColumn.Visible := True
                                    else edTimeColumn.Visible := false ;

     { Initialise settings }
     InspectFile ;
     bStop.Enabled := False ;
     prProgress.Min := 0 ;
     prProgress.Max := 1 ;
     prProgress.Position := 0 ;

     end ;


procedure TImportFrm.InspectFile ;
{ ----------------------------------------------------------
  Inspect the data file to determine initial import settings
  ----------------------------------------------------------}
var
   F : TextFile ;
   nCols,nColsOld,FileHandle,nLines : Integer ;
   T,TOld : single ;
   Values : Array[0..20] of Single ;
   s : string ;
   Done : Boolean ;
begin

     Screen.Cursor := crHourglass ;

     { Open file for reading as text }
     AssignFile( F, ImportFrm.FileName ) ;
     Reset(F) ;

     { Set time conversion factor }
     if cbTUnits.Items[cbTUnits.ItemIndex] = 's' then edASCII_DT.Scale := 1.0
     else if cbTUnits.Items[cbTUnits.ItemIndex] = 'ms' then edASCII_DT.Scale := 1E3
     else edASCII_DT.Scale := 1E6 ;

     mePreview.Clear ;
     Done := False ;
     nLines := 0 ;
     nColsOld := 0 ;
     while (not EOF(F)) and (not Done) do begin

         { Read a line of text from file }
         ReadLn( F,s ) ;
         { Display first 20 lines into memo box }
         if nLines < 20 then mePreview.Lines.Add( s )
                        else Done := True ;
         Inc(nLines) ;

         { Number of columns }
         nCols := ExtractListOfFloats( s, Values, False ) ;
         if nCols = nColsOld then begin
            edNumColumns.Value := nCols ;
            end ;
         nColsOld := nCols ;

         { Sampling interval }
         if nCols > 1 then begin
            T := Values[0] ;
            edASCII_DT.Value := T-TOld ;
            TOld := T ;
            end ;

         end ;

     edNumRowsIgnored.Value := 0 ;

     { If only one column of data available, disable time column }
     if edNumColumns.Value <= 1 then begin
        ckTimeDataAvailable.Checked := False ;
        edTimeColumn.Value := -1 ;
        edTimeColumn.Visible := False ;
        end
     else begin
        edTimeColumn.Visible := True ;
        if ckTimeDataAvailable.Checked then edTimeColumn.Value := 1 ;
        end ;

     CloseFile(F) ;

     { Determine number of bytes in file (for binary import) }
     FileHandle := FileOpen( ImportFrm.FileName, fmOpenRead ) ;
     edNumBytesToImport.Value := FileSeek( FileHandle, 0, 2 ) ;
     FileClose( FileHandle ) ;

     { Initial settings of binary file import parameters }
     edNumFileHeaderBytes.Value := 0.0 ;
     edNumChannelsBinary.Value := 1.0 ;
     { Set time conversion factor }
     if cbTUnitsBinary.Items[cbTUnitsBinary.ItemIndex] = 's' then
        edDTBinary.Scale := 1.0
     else if cbTUnitsBinary.Items[cbTUnitsBinary.ItemIndex] = 'ms' then
        edDTBinary.Scale := 1E3
     else edDTBinary.Scale := 1E6 ;
     edDTBinary.Value := 1.0 ;
     edScaleBy.Value := 1.0 ;
     edOffsetBy.Value := 0.0 ;

     Screen.Cursor := crDefault ;
     end;


procedure TImportFrm.bOKClick(Sender: TObject);
{ ---------------------
  Import data from file
  ---------------------}
begin

     if Page.ActivePage = TextFormat then ImportText( FileName )
                                     else ImportBinary( FileName ) ;
     end;


procedure TImportFrm.ImportText(
          FileName : string
          ) ;
{ ----------------------
  Import ASCII data file
  ----------------------
  25/6/98 ... Imported data now placed correctly in data file }
const
     NumBlocksPerBuffer = 256 ;
var
   F : TextFile ;
   Scale,Values,MaxValues : Array[0..20] of Single ;
   s : string ;
   Col,nCols,nSamples,NumSamplesPerBuffer,ch,i,NumBlocks : Integer ;
   Buf : ^TSmallIntArray ;
   OK : Boolean ;
begin

     Screen.Cursor := crHourglass ;
     { Create buffer to hold samples }
     New(Buf) ;
     OK := True ;

     try

        if Round(edNumColumns.Value) > (ChannelLimit+2) then begin
           MessageDlg( format(' Only %d channels allowed',[ChannelLimit+2]),
                           mtWarning, [mbOK], 0 ) ;
           end ;

        { If only one column of data available, disable time column }
        if Round(edNumColumns.Value) = 1 then begin
           ckTimeDataAvailable.Checked := False ;
           edTimeColumn.Value := -1.0 ;
           edTimeColumn.Visible := False ;
           end
        else edTimeColumn.Visible := True ;

        { Close existing data file }
        if CdrFH.FileHandle >= 0 then begin
           FileClose( CdrFH.FileHandle ) ;
           CdrFH.FileHandle := -1 ;
           end ;

        { Create name of EDR file to hold imported data }
        CdrFH.FileName := ChangeFileExt( FileName, DataFileExtension ) ;
        { Make sure an existing data file is not overwritten, unintentionally }
        OK := FileOverwriteCheck( CdrFH.FileName ) ;
        { Create data file }
        if OK then OK := Main.CreateNewDataFile( CdrFH ) ;

        if OK then begin
           { Open file for reading as text }
           AssignFile( F, ImportFrm.FileName ) ;
           Reset(F) ;

           { Number of signal channels }
           CdrFH.NumChannels := Round(edNumColumns.Value) ;

           { If one of the data columns is time data, reduce channel count }
           if ckTimeDataAvailable.Checked
              and (Round(edTimeColumn.Value) > 0) then Dec(CdrFH.NumChannels)
                                                  else edTimeColumn.Value := -1.0 ;

           { Set sampling interval }
           if edASCII_DT.value = 0.0 then begin
              MessageDlg( ' Sampling interval not defined!',mtWarning, [mbOK], 0 ) ;
              edASCII_DT.value := 1.0 ;
              end ;
           CdrFH.dt := edASCII_DT.value ;

           { Determine range of values within data }
           for Col := 0 to High(MaxValues) do MaxValues[Col] := 0.0 ;
           while not EOF(F) do begin
               { Read a line of text from file }
               ReadLn( F,s ) ;
               { Extract samples from  row }
               nCols := ExtractListOfFloats( s, Values, False ) ;

               { Determine maximum absolute values within data columns }
               for Col := 0 to Round(edNumColumns.Value)-1 do
                   MaxValues[Col] := MaxFlt( [Abs(Values[Col]),MaxValues[Col]] ) ;
               end ;

           { Create suitable integer/real scaling factors }
           CdrFH.ADCVoltageRange := 1.0 ;
           Ch := 0 ;
           for Col := 0 to Round(edNumColumns.Value)-1 do
               if (not ckTimeDataAvailable.Checked)
               or (Col <> (Round(edTimeColumn.Value)-1)) then begin

               Channel[ch].ChannelOffset := ch ;
               Channel[ch].ADCZero := 0 ;
               Channel[ch].ADCUnits := '' ;
               Channel[ch].ADCName := Format( 'Ch.%d', [ch] ) ;
               Channel[ch].ADCMaxValue := Main.SESLabIO.ADCMaxValue ;
               { ASCII --> Integer scaling factor }
               Scale[Col] := Channel[ch].ADCMaxValue / (1.1*MaxValues[Col]) ;
               { Integer --> real scaling factor }
               Channel[Ch].ADCScale := 1.0 / Scale[Col] ;
               Channel[ch].ADCAmplifierGain := 1. ;
               Channel[ch].ADCCalibrationFactor := ADCScaleToCalibFactor(
                                                   CdrFH.ADCVoltageRange,
                                                   Channel[ch]) ;
               Inc(Ch) ;
               end ;

           { Import ASCII data into .EDR file format }
           Reset(F) ;
           nSamples := 0 ;
           NumBlocks := 0 ;
           CdrFH.NumSamplesInFile := 0 ;
           NumSamplesPerBuffer := NumBlocksPerBuffer*CdrFH.NumChannels ;

           { Read and discard rows to be ignored }
           for i := 1 to Round(edNumRowsIgnored.Value) do ReadLn( F,s ) ;

           while (not EOF(F)) do begin

               { Read in a row of text }
               ReadLn( F,s ) ;
               { Extract samples from  row }
               nCols := ExtractListOfFloats( s, Values, False ) ;
               nCols := MinInt( [nCols,Round(edNumColumns.Value)] ) ;

               { If at end of file, put last samples into buffer and request save }
               if EOF(F) then begin
                  for Col := 0 to nCols-1 do
                      if Col <> (Round(edTimeColumn.Value)-1) then begin
                      Buf^[nSamples] := Round( Values[Col]*Scale[Col] ) ;
                      Inc(nSamples) ;
                      end ;
                  end
               else begin
                  { Normal update of binary data buffer }
                  for Col := 0 to nCols-1 do
                      if Col <> (Round(edTimeColumn.Value)-1) then begin
                      Buf^[nSamples] := Round( Values[Col]*Scale[Col] ) ;
                      Inc(nSamples) ;
                      end ;
                  end ;

               { Copy to file when buffer is full }
               if nSamples >= NumSamplesPerBuffer then begin
                  WriteCDRBuffer( CdrFH, NumBlocks, Buf^, NumBlocksPerBuffer ) ;
                  NumBlocks := NumBlocks + NumBlocksPerBuffer ;
                  nSamples := 0 ;
                  end ;

               end ;

           CdrFH.NumSamplesInFile := NumBlocks*CdrFH.NumChannels ;
           SaveCDRHeader(CdrFH) ;

           WriteToLogFile( 'ASCII Data File : ' + ImportFrm.FileName ) ;
           WriteToLogFile( 'converted to WCD file : ' + CdrFH.FileName ) ;
           end ;

     finally
        { Close data file }
        if CdrFH.FileHandle >= 0 then begin
           FileClose( CdrFH.FileHandle ) ;
           CdrFH.FileHandle := -1 ;
           end ;
        { Close text file }
        CloseFile(F) ;
        Dispose(Buf) ;
        Screen.Cursor := crDefault ;
        end ;

     end ;


procedure TImportFrm.ImportBinary(
          FileName : string       { Name of file to import }
          ) ;
{ ----------------------
  Import binary data file
  ----------------------}
const
     NumBlocksPerBuf = 256 ;
var
   Filehandle,ch,i : Integer ;
   NumBuffersToCopy : Integer  ;   { No. buffers to be copied from import file }
   NumSamplesPerBuf : Integer  ; { No. A/ samples (2 byte) per buffer }
   NumBytesPerBuf : Integer  ;   { No. bytes per buffer }
   NumBlocks : Integer ;         { No. Multi-channel sample blocks copied }
   Buf : ^TSmallIntArray ;
   OK : Boolean ;
begin
     OK := True ;
     screen.cursor := crHourglass ;
     { Create buffer to hold samples }
     New(Buf) ;

     try

        { Number of signal channels }
        CdrFH.NumChannels := Round(edNumChannelsBinary.Value) ;
        if CdrFH.NumChannels > (ChannelLimit+1) then begin
           MessageDlg( format(' Only %d channels allowed',[ChannelLimit+1]),
                           mtWarning, [mbOK], 0 ) ;
           end ;
        CdrFH.NumChannels := MaxInt( [0,MinInt([ChannelLimit+1,CdrFH.NumChannels])] ) ;

        { Create name of EDR file to hold imported data }
        CdrFH.FileName := ChangeFileExt( FileName, DataFileExtension ) ;
        { Make sure an existing data file is not overwritten, unintentionally }
        OK := FileOverwriteCheck( CdrFH.FileName ) ;
        { Create data file }
        if OK then OK := Main.CreateNewDataFile( CdrFH ) ;

        if OK then begin
           { Channel calibration and scale factor settings }
           CdrFH.ADCVoltageRange := 1.0 ;
           for ch := 0 to CdrFH.NumChannels-1 do begin
               Channel[ch].ChannelOffset := ch ;
               Channel[Ch].ADCScale := 1.0 ;
               Channel[ch].ADCAmplifierGain := 1. ;
               Channel[ch].ADCCalibrationFactor := ADCScaleToCalibFactor(
                                                   CdrFH.ADCVoltageRange,
                                                   Channel[ch]) ;
               Channel[ch].ADCZero := 0 ;
               Channel[ch].ADCUnits := '' ;
               Channel[ch].ADCName := Format( 'Ch.%d', [ch] ) ;
               end ;

           { Set sampling interval }
           CdrFH.dt := edDTBinary.Value ;
           end ;

        { Open import file }
        FileHandle := FileOpen( FileName, fmOpenRead ) ;
        if FileHandle < 0 then begin
           MessageDlg(' Unable to open' + FileName, mtWarning, [mbOK], 0 ) ;
           OK := False ;
           end ;

        if OK then begin
           { Move file pointer to start of data in source file }
           FileSeek( FileHandle, Round(edNumFileHeaderBytes.Value), 0 ) ;
           { Move file pointer to start of data in destination file }
           FileSeek( CdrFH.FileHandle, CdrFH.NumBytesInHeader, 0 ) ;

           { Copy samples from binary file into EDR file }
           NumSamplesPerBuf := NumBlocksPerBuf*CdrFH.NumChannels ;
           NumBytesPerBuf := NumSamplesPerBuf*2 ;
           NumBuffersToCopy := (Round(edNumBytesToImport.Value)
                                - Round(edNumFileHeaderBytes.Value)) div NumBytesPerBuf ;
           NumBlocks := 0 ;
           prProgress.Position := 0 ;
           prProgress.Min := 0 ;
           prProgress.Max := NumBuffersToCopy ;
           bStop.Enabled := True ;

           while bStop.Enabled do begin

               { Read sample data }
               if FileRead(FileHandle,Buf^,NumBytesPerBuf) = NumBytesPerBuf then begin
                  { Do scaling and offset }
                  for i := 0 to NumSamplesPerBuf-1 do
                      Buf^[i] := Round(Buf^[i]*edScaleBy.Value + edOffsetBy.Value) ;

                  { Write data to file }
                  WriteCDRBuffer( CdrFH, NumBlocks, Buf^, NumBlocksPerBuf ) ;
                  NumBlocks := NumBlocks + NumBlocksPerBuf ;

                  prProgress.Position := prProgress.Position + 1 ;
                  if prProgress.Position >= prProgress.Max then bStop.Enabled := False ;
                  Application.ProcessMessages ;

                  end
               else bStop.Enabled := False ;

               end ;

           CdrFH.NumSamplesInFile := NumBlocks*CdrFH.NumCHannels ;

           { Update header information }
           SaveCDRHeader(CdrFH) ;

           WriteToLogFile( 'Binary Data File : ' + ImportFrm.FileName ) ;
           WriteToLogFile( 'converted to WCD file : ' + CdrFH.FileName ) ;

           end ;

     finally
        { Close import file }
        if FileHandle >=0 then FileClose( FileHandle ) ;
        { Close data file }
        if CdrFH.FileHandle >= 0 then FileClose( CdrFH.FileHandle ) ;

        Dispose(Buf) ;
        screen.cursor := crdefault ;
        end ;

     end ;


procedure TImportFrm.cbTUnitsChange(Sender: TObject);

begin
     { Set time conversion factor }
     if cbTUnits.Items[cbTUnits.ItemIndex] = 's' then edASCII_DT.Scale := 1.0
     else if cbTUnits.Items[cbTUnits.ItemIndex] = 'ms' then edASCII_DT.Scale := 1E3
     else edASCII_DT.Scale := 1E6 ;
     end;


procedure TImportFrm.cbTUnitsBinaryChange(Sender: TObject);
var
   dt : single ;
begin
     { Set time conversion factor }
     dt := edDTBinary.Value ;
     if cbTUnitsBinary.Items[cbTUnitsBinary.ItemIndex] = 's' then edDTBinary.Scale := 1.0
     else if cbTUnitsBinary.Items[cbTUnitsBinary.ItemIndex] = 'ms' then edDTBinary.Scale := 1E3
     else edDTBinary.Scale := 1E6 ;
     edDTBinary.Value := dt ;
     end;


procedure TImportFrm.ckTimeDataAvailableClick(Sender: TObject);
begin
     if ckTimeDataAvailable.Checked and (edNumColumns.Value > 1.0) then begin
        edTimeColumn.Visible := True ;
        if edTimeColumn.Value <= 1.0 then edTimeColumn.Value := 1.0 ;
        end
     else begin
        edTimeColumn.Visible := False ;
        end ;
     end;

procedure TImportFrm.bStopClick(Sender: TObject);
begin
     bStop.Enabled := False ;
     end;

end.
