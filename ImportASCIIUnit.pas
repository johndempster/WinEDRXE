unit ImportASCIIUnit;
// ----------------------------------
// ASCII file import setup dialog box
// ----------------------------------
// 21.11.03
// 19.02.04 ... Data exchanged using ImportFile public object
//              Channel name/units can now be set
// 16.07.05 ... Lines at beginning of file can now be skipped

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ADCDataFile, Grids, global ;

type
  TImportASCIIFrm = class(TForm)
    bCancel: TButton;
    bOK: TButton;
    GroupBox3: TGroupBox;
    ChannelTable: TStringGrid;
    meText: TMemo;
    GroupBox4: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox1: TGroupBox;
    lbScanInterval: TLabel;
    rbTimeDataInCol0: TRadioButton;
    rbUserDefined: TRadioButton;
    edScanInterval: TValidatedEdit;
    GroupBox5: TGroupBox;
    rbTab: TRadioButton;
    rbComma: TRadioButton;
    rbSpace: TRadioButton;
    rbmsecs: TRadioButton;
    rbSecs: TRadioButton;
    rbMins: TRadioButton;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    edNumTitleLines: TValidatedEdit;  procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure rbTimeDataInCol0Click(Sender: TObject);
    procedure rbTabClick(Sender: TObject);
    procedure rbmsecsClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateTimeUnits ;
  public
    { Public declarations }
    ImportFile : TADCDataFile ;
    FileName : String ;

  end;

var
  ImportASCIIFrm: TImportASCIIFrm;

implementation

{$R *.dfm}

const
     ChNum = 0 ;
     ChName = 1 ;
     ChUnits = 2 ;

const
    MSecScale = 1000.0 ;
    SecScale = 1.0 ;
    MinScale = 1.0/60.0 ;

procedure TImportASCIIFrm.FormShow(Sender: TObject);
// ---------------------------------------
// Initialise controls when form displayed
// ---------------------------------------
var
    ch : Integer ;
    Row : Integer ;
    F: TextFile;
    s : String ;
begin

     // Set time units
     if ImportFile.ASCIITimeUnits = 'ms' then rbmsecs.Checked := True
     else if ImportFile.ASCIITimeUnits = 's' then  rbsecs.Checked := True
     else rbMins.Checked := True ;
     UpdateTimeUnits ;

     // Column separating character
     if ImportFile.ASCIISeparator = ',' then rbComma.Checked := True
     else if ImportFile.ASCIISeparator = ' ' then rbSpace.Checked := True
     else rbTab.Checked := True ;

     rbTimeDataInCol0.Checked := ImportFile.ASCIITimeDataInCol0 ;
     rbUserDefined.Checked := not ImportFile.ASCIITimeDataInCol0 ;

     edScanInterval.Visible := not rbTimeDataInCol0.Checked ;
     lbScanInterval.Visible := edScanInterval.Visible ;

     edScanInterval.Value := ImportFile.ScanInterval ;

     { Set channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := EDRChannelLimit + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     { Add details for new channels to table }
     for Row := 1 to ChannelTable.RowCount-1 do begin
         ch := Row-1 ;
         ChannelTable.cells[ChNum,Row] := format('%d',[ch]) ;
         ChannelTable.cells[ChName,Row] := ImportFile.ChannelName[ch] ;
         ChannelTable.cells[ChUnits,Row] := ImportFile.ChannelUnits[ch] ;
         end ;

     // Display first 10 lines of file
     AssignFile( F, FileName ) ;
     Reset(F);
     meText.Clear ;
     While (not EOF(F)) and (meText.Lines.Count < 10) do begin
         Readln(F, s);
         meText.Lines.Add( s ) ;
         end ;
     CloseFile(F) ;

     end;


procedure TImportASCIIFrm.bOKClick(Sender: TObject);
// ------------------
// Exit and hide form
// ------------------
var
     ch : Integer ;
     Row : Integer ;
begin

     // Set time scaling factor
     if rbmsecs.Checked then begin
        ImportFile.ASCIITimeUnits := 'ms' ;
        ImportFile.ScanInterval := edScanInterval.Value*0.001 ;
        end
     else if rbsecs.Checked then begin
        ImportFile.ASCIITimeUnits := 's' ;
        ImportFile.ScanInterval := edScanInterval.Value ;
        end
     else begin
        ImportFile.ASCIITimeUnits := 'min' ;
        ImportFile.ScanInterval := edScanInterval.Value*60.0 ;
        end ;

     ImportFile.ASCIITimeDataInCol0 := rbTimeDataInCol0.Checked ;
     ImportFile.ASCIITitleLines :=  Round(edNumTitleLines.Value) ;

     // Column separator
     if rbComma.Checked then ImportFile.ASCIISeparator := ','
     else if rbSpace.Checked then ImportFile.ASCIISeparator := ' '
     else ImportFile.ASCIISeparator := #9 ;

     { Add details for new channels to table }
     for Row := 1 to ChannelTable.RowCount-1 do begin
         ch := Row-1 ;
         ImportFile.ChannelName[ch] := ChannelTable.cells[ChName,Row] ;
         ImportFile.ChannelUnits[ch] := ChannelTable.cells[ChUnits,Row] ;
         end ;

     end;


procedure TImportASCIIFrm.rbTimeDataInCol0Click(Sender: TObject);
begin
     edScanInterval.Visible := not rbTimeDataInCol0.Checked ;
     lbScanInterval.Visible := edScanInterval.Visible ;
     end;


procedure TImportASCIIFrm.UpdateTimeUnits ;

begin

     if rbmsecs.Checked then begin
        ImportFile.ASCIITimeUnits := 'ms' ;
        edScanInterval.Units := 'ms' ;
        end
     else if rbSecs.Checked then begin
        ImportFile.ASCIITimeUnits := 's' ;
        edScanInterval.Units := 's' ;
        end
     else begin
        ImportFile.ASCIITimeUnits := 'min' ;
        edScanInterval.Units := 'm' ;
        end ;
     end ;


procedure TImportASCIIFrm.rbTabClick(Sender: TObject);
begin
     UpdateTimeUnits ;
     end;

procedure TImportASCIIFrm.rbmsecsClick(Sender: TObject);
begin
     if rbmsecs.Checked then begin
        edScanInterval.Units := 'ms' ;
        edScanInterval.Scale := 1000.0 ;
        end
     else if rbsecs.Checked then begin
        edScanInterval.Units := 's' ;
        edScanInterval.Scale := 1.0 ;
        end
     else begin
        edScanInterval.Units := 'min' ;
        edScanInterval.Scale := 1.0/60.0 ;
        end

     end;

end.
