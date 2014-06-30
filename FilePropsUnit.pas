unit FilePropsUnit;
// ---------------------------------------
// Display / Edit EDR data file properties
// ---------------------------------------
// 25.07.06
// 14.06.10 More details and header text added to properties
// 04.02.11 Scroll bars added text windows, dialog box widened

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, global, maths, fileio, ValidatedEdit, ComCtrls ;

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

uses MDIForm, math ;

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
    Header : array[1..NumBytesInHeader] of ANSIchar ;
begin

     edSamplingInterval.Value := CDRFH.dt ;
     edADCVoltageRange.Value := CDRFH.ADCVoltageRange ;

     edADCMaxValue.Value := Channel[0].ADCMaxValue ;
     CDRFH.ADCMaxValue := Round(Channel[0].ADCMaxValue) ;

     { Set channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (5*ChannelTable.DefaultColWidth) div 4 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := CDRFH.NumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     for ch := 0 to CDRfH.NumChannels-1 do begin
         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := Channel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',[Channel[ch].ADCCalibrationFactor] ) ;
         ChannelTable.cells[ChUnits,ch+1] := Channel[ch].ADCUnits ;
         end ;

     meProperties.Lines.Add(format('File version: %.2f',[cdrfh.Version])) ;
     meProperties.Lines.Add(format('Date Created: %s',[cdrfh.CreationTime])) ;
     meProperties.Lines.Add(format('ID: %s',[cdrfh.IdentLine])) ;

     meProperties.Lines.Add(format('No. samples in file: %d',[cdrfh.NumSamplesInFile])) ;
     meProperties.Lines.Add(format('No. of channels: %d',[cdrfh.NumChannels])) ;
     meProperties.Lines.Add(format('No. of samples/channels: %d',
                                   [cdrfh.NumSamplesInFile div cdrfh.NumChannels])) ;
     meProperties.Lines.Add(format('File header size (bytes): %d',[cdrfh.NumBytesInHeader])) ;
     meProperties.Lines.Add(format('Sample value range: %d - %d',[-Round(Channel[0].ADCMaxValue)-1,Round(Channel[0].ADCMaxValue)])) ;


     // Display file header
     FileSeek( cdrfh.FileHandle, 0,0) ;
     FillChar( Header, Sizeof(Header), 0) ;
     FileRead( cdrfh.FileHandle, Header, SizeOf(Header) ) ;
     meFileHeader.Lines.Clear ;
     meFileHeader.Lines.Text := Header ;

     end;

procedure TFilePropsDlg.bOKClick(Sender: TObject);
// -------------------------------
// Update changes to file settings
// -------------------------------
var
    ch : Integer ;
begin

     CDRFH.dt := edSamplingInterval.Value ;
     CDRFH.ADCVoltageRange := edADCVoltageRange.Value ;
     CDRFH.ADCMaxValue := Round(edADCMaxValue.Value) ;

     { Channel calibration }
     for ch := 0 to CDRFH.NumChannels-1 do begin
         Channel[ch].ADCMaxValue := CDRFH.ADCMaxValue ;
         Channel[ch].ADCName := ChannelTable.cells[ChName,ch+1] ;
         Channel[ch].ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             Channel[ch].ADCCalibrationFactor);
         Channel[ch].ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
         end ;

     // Save to file header
     SaveCDRHeader( CDRFH ) ;

     Main.UpdateChannelScalingFactors ; ;

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
