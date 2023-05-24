unit FilePropsUnit;
// ---------------------------------------
// Display / Edit EDR data file properties
// ---------------------------------------
// 25.07.06
// 14.06.10 More details and header text added to properties
// 04.02.11 Scroll bars added text windows, dialog box widened
// 16.05.18 V/Units in calibration table now shows actual scaling factor rather than X1 gain factor.
//

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, maths, ValidatedEdit, ComCtrls ;

type
  TFilePropsDlg = class(TForm)
    bCancel: TButton;
    bOK: TButton;
    PageControl1: TPageControl;
    TabProperties: TTabSheet;
    meProperties: TMemo;
    TabCalTable: TTabSheet;
    TabFileHeader: TTabSheet;
    meFileHeader: TMemo;
    ChannelTable: TStringGrid;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edSamplingInterval: TValidatedEdit;
    edADCVoltageRange: TValidatedEdit;
    edADCMaxValue: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilePropsDlg: TFilePropsDlg;

implementation

uses MDIForm, math , EDRFileUnit;

const
     ChNum = 0 ;
     ChName = 1 ;
     ChCal = 2 ;
     ChUnits = 3 ;

{$R *.dfm}

procedure TFilePropsDlg.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
    ch : Integer ;
//    Header : array[1..NumBytesInHeader] of ANSIchar ;
begin

     edSamplingInterval.Value := EDRFile.cdrfh.dt ;
     edADCVoltageRange.Value := EDRFile.cdrfh.ADCVoltageRange ;

     edADCMaxValue.Value := EDRFile.Channel[0].ADCMaxValue ;
     EDRFile.cdrfh.ADCMaxValue := Round(EDRFile.Channel[0].ADCMaxValue) ;

     { Set channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (5*ChannelTable.DefaultColWidth) div 4 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := EDRFile.cdrfh.NumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     for ch := 0 to EDRFile.cdrfh.NumChannels-1 do begin
         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := EDRFile.Channel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',[EDRFile.Channel[ch].ADCCalibrationFactor*EDRFile.Channel[ch].ADCAmplifierGain] ) ;
         ChannelTable.cells[ChUnits,ch+1] := EDRFile.Channel[ch].ADCUnits ;
         end ;

     meProperties.Lines.Add(format('File version: %.2f',[EDRFile.cdrfh.Version])) ;
     meProperties.Lines.Add(format('Date Created: %s',[EDRFile.cdrfh.CreationTime])) ;
     meProperties.Lines.Add(format('ID: %s',[EDRFile.cdrfh.IdentLine])) ;

     meProperties.Lines.Add(format('No. samples in file: %d',[EDRFile.cdrfh.NumSamplesInFile])) ;
     meProperties.Lines.Add(format('No. of channels: %d',[EDRFile.cdrfh.NumChannels])) ;
     meProperties.Lines.Add(format('No. of samples/channels: %d',
                                   [EDRFile.cdrfh.NumSamplesInFile div EDRFile.cdrfh.NumChannels])) ;
     meProperties.Lines.Add(format('File header size (bytes): %d',[EDRFile.cdrfh.NumBytesInHeader])) ;
     meProperties.Lines.Add(format('Sample value range: %d - %d',[-Round(EDRFile.Channel[0].ADCMaxValue)-1,Round(EDRFile.Channel[0].ADCMaxValue)])) ;


     // Display file header
     // Read ANSI text from first HeaderSize bytes of file header and load into Header StringList
  {   FileSeek( fHDR.FileHandle, 0, 0 ) ;
     pANSIBuf := AllocMem( MinBytesInHeader ) ;
     FileRead(fHDR.FileHandle, pANSIBuf^, HeaderSize ) ;
     pANSIBuf[HeaderSize-1] := #0 ;
     ANSIHeader := ANSIString( pANSIBuf ) ;}
{     Header.Text := String(ANSIHeader) ;

     FileSeek( EDRFile.cdrfh.FileHandle, 0,0) ;
     FillChar( Header, Sizeof(Header), 0) ;
     FileRead( EDRFile.cdrfh.FileHandle, Header, SizeOf(Header) ) ;}
     meFileHeader.Lines.Clear ;
     meFileHeader.Lines.Text := EDRFile.HeaderText ;

     end;

procedure TFilePropsDlg.bOKClick(Sender: TObject);
// -------------------------------
// Update changes to file settings
// -------------------------------
var
    ch : Integer ;
begin

     EDRFile.cdrfh.dt := edSamplingInterval.Value ;
     EDRFile.cdrfh.ADCVoltageRange := edADCVoltageRange.Value ;
     EDRFile.cdrfh.ADCMaxValue := Round(edADCMaxValue.Value) ;

     { Channel calibration }
     for ch := 0 to EDRFile.cdrfh.NumChannels-1 do begin
         EDRFile.Channel[ch].ADCMaxValue := EDRFile.cdrfh.ADCMaxValue ;
         EDRFile.Channel[ch].ADCName := ChannelTable.cells[ChName,ch+1] ;
         EDRFile.Channel[ch].ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             EDRFile.Channel[ch].ADCCalibrationFactor);
         if EDRFile.Channel[ch].ADCAmplifierGain <> 0.0 then
            EDRFile.Channel[ch].ADCCalibrationFactor := EDRFile.Channel[ch].ADCCalibrationFactor / EDRFile.Channel[ch].ADCAmplifierGain ;
         EDRFile.Channel[ch].ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
         end ;

     // Save to file header
     EDRFile.SaveHeader( EDRFile.CDRFH ) ;

     EDRFile.UpdateChannelScalingFactors ; ;

     Close ;

     end;


procedure TFilePropsDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TFilePropsDlg.bCancelClick(Sender: TObject);
begin
     Close ;
     end;

end.
