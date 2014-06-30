unit Copyrec;
{ ========================================================
  WinWCP - Setup form for copying image of  signal records
  (called by REPLAY module)
  2/5/97 ... Calibration bar values only shown for enabled channels
  ========================================================}
interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Spin, Grids, global, shared, sysutils;

type
  TCopyRecDlg = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edWidth: TEdit;
    edHeight: TEdit;
    GroupBox2: TGroupBox;
    CalibrationBarTable: TStringGrid;
    GroupBox5: TGroupBox;
    ckShowZeroLevels: TCheckBox;
    ckShowLabels: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    ckUseColor: TCheckBox;
    FontGrp: TGroupBox;
    Label7: TLabel;
    cbFontName: TComboBox;
    edFontSize: TEdit;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edLineThickness: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure edLineThicknessKeyPress(Sender: TObject; var Key: Char);
    procedure cbFontNameChange(Sender: TObject);
    procedure ckShowLabelsClick(Sender: TObject);
    procedure CalibrationBarTableKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure UpdateSettings ;
  public
    { Public declarations }
  end;

var
  CopyRecDlg: TCopyRecDlg;

implementation

{$R *.DFM}

uses maths ;

var
   Initialised : Boolean ;


procedure TCopyRecDlg.FormShow(Sender: TObject);
{ -------------------------------
  Initialise Copy Record(s) form
  -------------------------------}
var
   n,ch,Row : Integer ;
begin

     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := screen.fonts ;

     { Set Column widths }
     CalibrationBarTable.colwidths[0] := Canvas.TextWidth('XXXXX') ;
     CalibrationBarTable.colwidths[1] := Canvas.TextWidth('XXXXXXXXX') ;

     CalibrationBarTable.options := [goEditing,goHorzLine,goVertLine] ;
     CalibrationBarTable.RowCount := CdrfH.NumChannels + 1 ;
     CalibrationBarTable.cells[0,0] := 'Time ';

     { Adjust number of rows to number of enabled channels }
      n := 1 ;
      for ch := 0 to CdrfH.NumChannels-1 do if Channel[ch].InUse then Inc(n) ;
      CalibrationBarTable.RowCount := n ;

      { Update text box settings }
      Initialised := False ;
      UpdateSettings ;

     { Ensure calibration bars have focus when dialog box opens }
     CalibrationBarTable.SetFocus ;

     end;


procedure TCopyRecDlg.UpdateSettings ;
{ -----------------------------------
  Update text box/Settings parameters
  -----------------------------------}
var
   Row,ch : Integer ;
begin

     { Bit map width }
     if Initialised then Settings.BitmapWidth := ExtractInt(edWidth.text) ;
     edWidth.text := format( ' %d ',[Settings.BitmapWidth]);

     { Bit map height }
     if Initialised then Settings.BitmapHeight := ExtractInt(edHeight.text) ;
     edHeight.text := format( ' %d ',[Settings.BitmapHeight]);

     { Line thickness }
     if Initialised then Settings.Plot.LineThickness := ExtractInt(edLineThickness.text) ;
     edLineThickness.text := format( ' %d pts',[Settings.Plot.LineThickness]);

     { Font name }
     if Initialised then Settings.Plot.FontName := cbFontName.text ;
     cbFontName.itemindex := cbFontName.items.indexof(Settings.Plot.FontName) ;
     if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;

     { Font size (points) }
     if Initialised then Settings.Plot.FontSize := ExtractInt(edFontSize.text) ;
     edFontSize.text := format( ' %d pts',[Settings.Plot.FontSize]);

     { Show label on plot }
     if Initialised then Settings.ShowLabels := ckShowLabels.checked ;
     ckShowLabels.checked := Settings.ShowLabels ;

     { Show zero levels }
     if Initialised then Settings.ShowZeroLevels := ckShowZeroLevels.checked ;
     ckShowZeroLevels.checked := Settings.ShowZeroLevels ;

     { Use colour }
     if Initialised then Settings.Plot.UseColor := ckUseColor.checked ;
     ckUseColor.checked := Settings.Plot.UseColor ;

     { Time calibration bar }
     if Initialised then Settings.TBarValue := ExtractFloat(
                                               CalibrationBarTable.cells[1,0], 1. )
                                               * Settings.TUnscale ;
     CalibrationBarTable.cells[1,0] := Format( '%.4g %s',
                                       [Settings.TBarValue*Settings.TScale,
                                        Settings.TUnits])  ;

     { Channel calibration bars }
     Row := 1 ;
     for ch := 0 to CdrfH.NumChannels-1 do if Channel[ch].InUse then begin
         if Initialised then Channel[ch] := ExtractFloat(
                                            CalibrationBarTable.Cells[1,Row],10. ) ;
         CalibrationBarTable.cells[0,Row] := Channel[ch].ADCName ;
         CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                              [Settings.BarValue[ch],
                                               Channel[ch].ADCUnits]) ;
         Inc(Row) ;
         end ;

     Initialised := True ;

     end ;


procedure TCopyRecDlg.bOKClick(Sender: TObject);
{ -----------------------
  Update global settings
  ----------------------}
begin
     UpdateSettings ;
     end ;

procedure TCopyRecDlg.edLineThicknessKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;

procedure TCopyRecDlg.cbFontNameChange(Sender: TObject);
begin
     UpdateSettings ;
     end;

procedure TCopyRecDlg.ckShowLabelsClick(Sender: TObject);
begin
     UpdateSettings ;
     end;

procedure TCopyRecDlg.CalibrationBarTableKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;

end.
