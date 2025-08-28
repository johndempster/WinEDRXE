unit ImportRawUnit;
// ---------------------------------------
// Raw binary file import setup dialog box
// ---------------------------------------
// 19.03.04 Updated to support WinWCP
// 14.03.24 ... Form position saved to INI file
// 20.06.24 ... Extensively revised. Supports 2/4 byte integer, single precision float

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, maths, Grids, global, ADCDataFile, math, EDRFileUnit ;

type
  TImportRawFrm = class(TForm)
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label9: TLabel;
    edNumFileHeaderBytes: TValidatedEdit;
    edNumChannelsPerScan: TValidatedEdit;
    bOK: TButton;
    bCancel: TButton;
    gpChannels: TGroupBox;
    ChannelTable: TStringGrid;
    GroupBox3: TGroupBox;
    rbFloat: TRadioButton;
    rbInteger: TRadioButton;
    GroupBox4: TGroupBox;
    rbmsecs: TRadioButton;
    rbSecs: TRadioButton;
    rbMins: TRadioButton;
    Label1: TLabel;
    edNumBytesPerSample: TValidatedEdit;
    lbSamplingInterval: TLabel;
    edScanInterval: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbTimeScaleChange(Sender: TObject);
    procedure edNumBytesPerSampleKeyPress(Sender: TObject; var Key: Char);
    procedure edNumChannelsPerScanKeyPress(Sender: TObject; var Key: Char);
    procedure rbFloatClick(Sender: TObject);
    procedure rbmsecsClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure rbSecsClick(Sender: TObject);
    procedure rbMinsClick(Sender: TObject);
    procedure rbIntegerClick(Sender: TObject);
  private
    { Private declarations }
    YMax : Array[0..MaxChannels] of Single ; // Max absolute value of data in import
    procedure UpdateControls ;
    procedure AnalyseImportFileData ;
  public
    { Public declarations }
    ImportFile : TADCDataFile ;
    FileName : string ;
  end;

var
  ImportRawFrm: TImportRawFrm;

implementation

{$R *.dfm}

uses  MDIFORM;
const
     ChNum = 0 ;
     ChName = 1 ;
     chYMax = 2 ;
     ChScale = 3 ;
     ChUnits = 4 ;
     FPformat = 0 ;
     Intformat = 1 ;

procedure TImportRawFrm.FormHide(Sender: TObject);
// ---------
// Hide form
// ---------
begin
    // Save form position
    EDRFile.SaveFormPosition( Self ) ;

end;


procedure TImportRawFrm.FormShow(Sender: TObject);
// ------------------------------------------
// Initialise controls when form is displayed
// ------------------------------------------
var
     Row : Integer ;
     ch : Integer ;
begin

     // Ensure form shows all controls
     Self.ClientWidth := gpChannels.Left + gpChannels.Width + 8 ;
     Self.ClientHeight := bOK.Top + bOK.Height + 20 ;

    edNumFileHeaderBytes.Value := ImportFile.NumFileHeaderBytes ;
    edNumChannelsPerScan.Value := ImportFile.NumChannelsPerScan ;
    edNumBytesPerSample.Value := ImportFile.NumBytesPerSample ;

    if rbsecs.Checked then
       begin
       edScanInterval.Scale := 1.0 ;
       edScanInterval.Units := 's' ;
       end
    else if rbmsecs.Checked then
       begin
       edScanInterval.Scale := 1000.0 ;
       edScanInterval.Units := 'ms' ;
       end
    else
       begin
       edScanInterval.Scale := 1.0/60.0 ;
       edScanInterval.Units := 'min' ;
       end ;

    if ImportFile.ScanInterval = 0.0 then ImportFile.ScanInterval := 0.001 ;
    edScanInterval.Value := ImportFile.ScanInterval ;

    // Set sample format
    ImportFile.FloatingPointSamples := rbFloat.Checked ;
    if rbFloat.checked then edNumBytesPerSample.Value := SizeOf(Single)
                       else edNumBytesPerSample.Value := SizeOf(SmallInt) ;

    { Set channel calibration table }

    ChannelTable.RowCount := ImportFile.NumChannelsPerScan + 1;
    ChannelTable.ColCount := chUnits + 1 ;
    ChannelTable.cells[ChNum,0] := 'Ch.' ;
    ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
    ChannelTable.cells[ChName,0] := 'Name' ;
    ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
    ChannelTable.cells[ChYMax,0] := 'Ymax' ;
    ChannelTable.colwidths[ChYMax] := ChannelTable.DefaultColWidth ;
    ChannelTable.cells[ChScale,0] := 'V/Units' ;
    ChannelTable.colwidths[ChScale] := (4*ChannelTable.DefaultColWidth) div 3 ;
    ChannelTable.cells[ChUnits,0] := 'Units' ;
    ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;

    ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

    // Update controls on form
    UpdateControls ;

    end;

procedure TImportRawFrm.bOKClick(Sender: TObject);
// ------------------------------------------
// OK pressed - Update public variables and exit
// ------------------------------------------
var
    ch : Integer ;
begin

    ImportFile.NumFileHeaderBytes := Round(edNumFileHeaderBytes.Value)  ;
    ImportFile.NumChannelsPerScan := Round(edNumChannelsPerScan.Value) ;
    ImportFile.NumBytesPerSample := Round(edNumBytesPerSample.Value) ;
    ImportFile.ScanInterval := edScanInterval.Value ;

    // Floating point integer format
    ImportFile.FloatingPointSamples := rbFloat.Checked ;

    for ch := 0 to ImportFile.NumChannelsPerScan-1 do
         begin
         ImportFile.ChannelName[ch] := ChannelTable.cells[ChName,ch+1] ;
         ImportFile.ChannelCalibrationFactor[ch] := ExtractFloat(ChannelTable.cells[ChScale,ch+1],1.0) ;
         ImportFile.ChannelUnits[ch] := ChannelTable.cells[ChUnits,ch+1] ;
         end ;

    end;


procedure TImportRawFrm.UpdateControls ;
// --------------------------------------
// Update controls to account for changes
// --------------------------------------
var
    ch,Row : Integer ;
begin


    ImportFile.NumChannelsPerScan := Round(edNumChannelsPerScan.Value) ;
    ImportFile.ScanInterval := edScanInterval.Value ;
    ImportFile.NumBytesPerSample := Round(edNumBytesPerSample.Value) ;

    // Set integer sample size to signed 16 bit
    ImportFile.MaxADCValue := 32767 ;

    // Determine range of data values in file
    AnalyseImportFileData ;

    ChannelTable.RowCount := ImportFile.NumChannelsPerScan + 1 ;
    ChannelTable.ColCount := chUnits + 1 ;
    ChannelTable.cells[ChNum,0] := 'Ch.' ;
    ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;

     { Add details for new channels to table }
     for Row := 1 to ChannelTable.RowCount-1 do
         begin
         ch := Row-1 ;
         // Channel no.
         ChannelTable.cells[ChNum,Row] := format('%d',[ch]) ;
         // Channel name
         ChannelTable.cells[ChName,Row] := format('Ch.%d',[ch]) ; ;//ImportFile.ChannelName[ch] ;
         // Absolute range of data in import file
         ChannelTable.cells[ChYmax,Row] := format('%.4g',[YMax[ch]]) ;
         // Set A/D voltage range to standard input voltage range (+-10V)
         ImportFile.ChannelADCVoltageRange[ch] := 10.0 ;

         if rbFLoat.Checked then
            begin
            // Set calibration factor to place input data within middle 50% of range of  EDR file
            ImportFile.ChannelCalibrationFactor[ch] := (0.5*ImportFile.ChannelADCVoltageRange[ch])/YMax[ch] ;
            ChannelTable.cells[ChUnits,Row] := 'V'
            end
         else
            begin
            // Set calibration factor to display input data as ADc units
            ImportFile.ChannelCalibrationFactor[ch] := ImportFile.ChannelADCVoltageRange[ch]/ImportFile.MaxADCValue ;
            ChannelTable.cells[ChUnits,Row] := 'ADC' ;
            end;

         ChannelTable.cells[ChScale,Row] := format('%.4g',[ImportFile.ChannelCalibrationFactor[ch]]) ;

         end ;


    end ;


procedure TImportRawFrm.cbTimeScaleChange(Sender: TObject);
begin
     UpdateControls ;
     end;

procedure TImportRawFrm.edNumBytesPerSampleKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then UpdateControls ;
     end;


procedure TImportRawFrm.edNumChannelsPerScanKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// No. of signal channels changed
// ------------------------------
var
     Row : Integer ;
     ch : Integer ;
begin
     if Key = #13 then
        begin
        { Add details for new channels to table }
        ImportFile.NumChannelsPerScan := Round(edNumChannelsPerScan.Value) ;
        UpdateControls ;
        end;
     end;


procedure TImportRawFrm.rbFloatClick(Sender: TObject);
// ---------------------------------
// Floating point data file selected
// ---------------------------------
begin

    // Ensure no. of bytes / sample is size of single precision float
    if edNumBytesPerSample.Value <> SizeOf(Single) then edNumBytesPerSample.Value := SizeOf(Single) ;

    UpdateControls ;

    end;


procedure TImportRawFrm.rbIntegerClick(Sender: TObject);
// ----------------------------
// Integer data import selected
// ----------------------------
begin
    // Set no. of bytes / sample to defalt 2 byte integer
    if edNumBytesPerSample.Value <> SizeOf(SmallInt) then edNumBytesPerSample.Value := SizeOf(SmallInt) ;

    UpdateControls ;

end;


procedure TImportRawFrm.rbMinsClick(Sender: TObject);
// --------------------------
// Sampling interval in mins
// --------------------------
begin
     edScanInterval.Scale := 1.0/60.0 ;
     edScanInterval.Units := 'mins' ;
     UpdateControls ;
     end;


procedure TImportRawFrm.rbmsecsClick(Sender: TObject);
// --------------------------
// Sampling interval in msecs
// --------------------------
begin
     edScanInterval.Scale := 1000.0 ;
     edScanInterval.Units := 'ms' ;
     UpdateControls ;
     end;


procedure TImportRawFrm.rbSecsClick(Sender: TObject);
// --------------------------
// Sampling interval in secs
// --------------------------
begin
     edScanInterval.Scale := 1.0 ;
     edScanInterval.Units := 's' ;
     UpdateControls ;
     end;


procedure TImportRawFrm.AnalyseImportFileData ;
// --------------------------------------
// Determine range of data in import file
// --------------------------------------
const
    MaxBytes = 4000000 ;
var
    FileHandle : THandle ;
    NumDataBytes,FileDataStart : Int64 ;
    NumBytesPerSample,NumSamples,i,iMax,NumChannels,ch : Integer ;
    iBuf2 : PSmallIntArray ;
    iBuf4 : PLongIntArray ;
    fBuf4 : PSingleArrayDyn ;

begin

     FileHandle := FileOpen( FileName, fmOpenRead ) ;
     if FileHandle = INVALID_HANDLE_VALUE then
        begin
          ShowMessage( 'Unable to open: ' + FileName );
          Exit ;
        end;

     FileDataStart := Round(edNumFileHeaderBytes.Value) ;
     NumDataBytes := FileSeek( FileHandle, 0, 2 ) - FileDataStart ;
     NumBytesPerSample := Round(edNumBytesPerSample.Value) ;
     NumDataBytes := Min( NumDataBytes, MaxBytes ) ;
     NumDataBytes := (NumDataBytes div NumBytesPerSample) * NumBytesPerSample ;
     NumChannels := Round(edNumChannelsPerScan.value) ;

     if rbInteger.Checked then
        begin
        // ------------
        // Integer data
        // ------------
        case NumBytesPerSample of

            // 2 byte integers
            2 : begin

            iBuf2 := AllocMem( Min(NumDataBytes,MaxBytes)) ;
            FileSeek( FileHandle, FileDataStart, 0 ) ;
            FileRead( FileHandle, iBuf2^, NumDataBytes ) ;
            NumSamples := NumDataBytes div NumBytesPerSample ;
            iMax := 0 ;
            for i := 0 to NumSamples-1 do iMax := Max(Abs(iBuf2[i]),iMax) ;
            for ch := 0 to NumChannels-1 do YMax[ch] := iMax ;
            FreeMem( iBuf2 ) ;
            end;

            // 4 byte integers
            4 : begin
            iBuf4 := AllocMem( Min(NumDataBytes,MaxBytes)) ;
            FileSeek( FileHandle, FileDataStart, 0 ) ;
            FileRead( FileHandle, iBuf4^, NumDataBytes ) ;
            NumSamples := NumDataBytes div NumBytesPerSample ;
            iMax := 0 ;
            for i := 0 to NumSamples-1  do iMax := Max(Abs(iBuf4[i]),iMax) ;
            for ch := 0 to NumChannels-1 do YMax[ch] := iMax ;
            FreeMem( iBuf4 ) ;
            end;

            end;

        end
     else
       begin
        // -------------------
        // Floating point data
        // -------------------

       if (NumBytesPerSample <> 4) and (NumBytesPerSample <> 8)  then
          begin
          // Only 4 and 8 byte float supported
          NumBytesPerSample := 4 ;
          edNumBytesPerSample.Value := NumBytesPerSample ;
          end;

       case NumBytesPerSample of

            // 4 byte float
            4 : begin
            fBuf4 := AllocMem( Min(NumDataBytes,MaxBytes)) ;
            FileSeek( FileHandle, FileDataStart, 0 ) ;
            FileRead( FileHandle, fBuf4^, NumDataBytes ) ;
            NumSamples := NumDataBytes div NumBytesPerSample ;

            // Determine largest absolute value in file
            for ch := 0 to NumChannels-1 do YMax[ch] := 0.0 ;
            ch := 0 ;
            for i := 0 to NumSamples-1  do
                begin
                YMax[Ch] := Max(Abs(fBuf4[i]),YMax[ch]) ;
                ch := (ch + 1) mod NumChannels ;
                end;
            FreeMem( fBuf4 ) ;
            end;

            end;

       end;

     // Close file
     FileClose( FileHandle ) ;

end;


end.
