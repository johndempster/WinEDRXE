unit PrintRec;
{ =================================================================
  WinEDR - Printer page/font setup form for printing signal records
  2/5/97 ... Calibration bar values only shown for enabled channels
  26/6/98 ... Text boxes updates now grouped together in UpdateSettings
  30/8/99 ... Display object (Scope or Chart) now updated by PrintRec
  23/2/04 ... Calibration bars now held internal within Display object
  09/8/12 ... Excess decimal places removed from calibration bars
  =================================================================}
interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls,Printers,Global,Shared, Grids, SysUtils, Spin,
  {PlotLib,} ValEdit,  ScopeDisplay, ChartDisplay, ValidatedEdit ;

type
  TDestination = (dePrinter,deClipboard) ;
  TPrintRecFrm = class(TForm)
    GroupBox2: TGroupBox;
    CalibrationBarTable: TStringGrid;
    GroupBox5: TGroupBox;
    ckShowZeroLevels: TCheckBox;
    ckShowLabels: TCheckBox;
    bPrint: TButton;
    bCancel: TButton;
    ckUseColor: TCheckBox;
    FontGrp: TGroupBox;
    Label7: TLabel;
    cbFontName: TComboBox;
    Page: TNotebook;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    Label8: TLabel;
    edWidth: TValidatedEdit;
    edHeight: TValidatedEdit;
    edLeftMargin: TValidatedEdit;
    edTopMargin: TValidatedEdit;
    edRightMargin: TValidatedEdit;
    edBottomMargin: TValidatedEdit;
    edFontSize: TValidatedEdit;
    Label5: TLabel;
    edLineThickness: TValidatedEdit;
    procedure bPrintClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CalibrationBarTableKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }

    Destination : TDestination ;
    Display : TScopeDisplay ;
  end;



var
  PrintRecFrm: TPrintRecFrm;

implementation

{$R *.DFM}
uses MDIform, maths , EDRFileUnit;

procedure TPrintRecFrm.FormShow(Sender: TObject);
{ --------------------------------
  Initialise Print Record(s) form
  -------------------------------}
var
   n,ch,Row : Integer ;
begin
     { Select appropriate settings page }
     if Destination = dePrinter then begin
        Caption := ' Print ' ;
        Page.PageIndex := 0 ;
        edFontSize.Units := 'pts' ;
        edLineThickness.Units := 'pts' ;
        end
     else begin
        Caption := ' Copy Image ' ;
        Page.PageIndex := 1 ;
        edFontSize.Units := 'pixels' ;
        edLineThickness.Units := 'pixels' ;
        end ;


     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;

     { Set Column widths }
     CalibrationBarTable.colwidths[0] := Canvas.TextWidth('XXXXX') ;
     CalibrationBarTable.colwidths[1] := Canvas.TextWidth('XXXXXXXXXXXX') ;

     CalibrationBarTable.options := [goEditing,goHorzLine,goVertLine] ;
     CalibrationBarTable.RowCount := Display.NumChannels + 1 ;
     CalibrationBarTable.cells[0,0] := 'Time ';

     { Adjust number of rows to number of enabled channels }
      n := 1 ;
      for ch := 0 to Display.NumChannels-1 do
         if Display.ChanVisible[ch] then Inc(n) ;
      CalibrationBarTable.RowCount := n ;

      { Update text box settings }
      edLeftMargin.Value := EDRFile.Settings.Plot.LeftMargin ;
      edRightMargin.Value := EDRFile.Settings.Plot.RightMargin ;
      edTopMargin.Value := EDRFile.Settings.Plot.TopMargin ;
      edBottomMargin.Value := EDRFile.Settings.Plot.BottomMargin ;
      edWidth.Value := EDRFile.Settings.Plot.MetafileWidth ;
      edHeight.Value := EDRFile.Settings.Plot.MetafileHeight ;
      edFontSize.Value := EDRFile.Settings.Plot.FontSize ;
      edLineThickness.Value := EDRFile.Settings.Plot.LineThickness ;
      cbFontName.itemindex := cbFontName.items.indexof(EDRFile.Settings.Plot.FontName) ;
      if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
      ckShowLabels.checked := EDRFile.Settings.ShowLabels ;
      ckShowZeroLevels.checked := EDRFile.Settings.ShowZeroLevels ;
      ckUseColor.checked := EDRFile.Settings.Plot.UseColor ;

      { Time calibration }
      if Display.TCalBar <= 0.0 then begin
         Display.TCalBar := (Display.xMax - Display.xMin)*0.1 ;
         end ;
      CalibrationBarTable.cells[1,0] := Format( '%.4g %s',
                                        [Display.TCalBar*Display.TScale,Display.TUnits])  ;

      { Channel calibration bars }
      Row := 1 ;
      for ch := 0 to Display.NumChannels-1 do if Display.ChanVisible[ch] then begin
         if Display.ChanCalBar[ch] <= 0.0 then
            Display.ChanCalBar[ch] := (Display.yMax[ch] - Display.yMin[ch])
                                      *Display.ChanScale[ch]*0.1 ;
         CalibrationBarTable.cells[0,Row] := Display.ChanName[ch] ;
         CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                             [Display.ChanCalBar[ch],
                                              Display.ChanUnits[ch]]) ;
         Inc(Row) ;
         end ;

      { Ensure calibration bars have focus when dialog box opens }
      CalibrationBarTable.SetFocus ;

     end;


procedure TPrintRecFrm.bPrintClick(Sender: TObject);
{ -----------------------
  Update global settings
  ----------------------}
var
   row,ch : Integer ;
begin

      { Update settings from text boxes }
      EDRFile.Settings.Plot.LeftMargin := edLeftMargin.Value ;
      EDRFile.Settings.Plot.RightMargin := edRightMargin.Value ;
      EDRFile.Settings.Plot.TopMargin := edTopMargin.Value ;
      EDRFile.Settings.Plot.BottomMargin := edBottomMargin.Value ;
      EDRFile.Settings.Plot.MetafileWidth := Round(edWidth.Value) ;
      EDRFile.Settings.Plot.MetafileHeight := Round(edHeight.Value) ;
      EDRFile.Settings.Plot.FontSize := Round(edFontSize.Value) ;
      EDRFile.Settings.Plot.LineThickness := Round(edLineThickness.Value) ;
      EDRFile.Settings.Plot.FontName :=  cbFontName.text ;

      EDRFile.Settings.ShowLabels := ckShowLabels.checked ;
      EDRFile.Settings.ShowZeroLevels := ckShowZeroLevels.checked ;
      EDRFile.Settings.Plot.UseColor := ckUseColor.checked ;

      { Time calibration }
      Display.TCalBar := ExtractFloat(CalibrationBarTable.cells[1,0], 1. ) /
                         Display.TScale  ;

      { Channel calibration bars }
      Row := 1 ;
      for ch := 0 to Display.NumChannels-1 do if Display.ChanVisible[ch] then begin
         Display.ChanCalBar[ch] := ExtractFloat(CalibrationBarTable.Cells[1,Row],10. ) ;
         Inc(Row) ;
         end ;

      { Copy data into display object }
      Display.PrinterLeftMargin := Round(EDRFile.Settings.Plot.LeftMargin) ;
      Display.PrinterRightMargin := Round(EDRFile.Settings.Plot.RightMargin) ;
      Display.PrinterTopMargin := Round(EDRFile.Settings.Plot.TopMargin) ;
      Display.PrinterBottomMargin := Round(EDRFile.Settings.Plot.BottomMargin) ;
      Display.PrinterFontName := EDRFile.Settings.Plot.FontName ;
      Display.PrinterFontSize := EDRFile.Settings.Plot.FontSize ;
      Display.PrinterPenWidth := EDRFile.Settings.Plot.LineThickness ;
      Display.MetafileWidth := EDRFile.Settings.Plot.MetaFileWidth ;
      Display.MetafileHeight := EDRFile.Settings.Plot.MetaFileHeight ;
      Display.PrinterShowZeroLevels := EDRFile.Settings.ShowZeroLevels ;
      Display.PrinterShowLabels := EDRFile.Settings.ShowLabels ;
      Display.PrinterDisableColor := not EDRFile.Settings.Plot.UseColor ;

     end ;


procedure TPrintRecFrm.CalibrationBarTableKeyPress(
          Sender: TObject;
          var Key: Char);
var
   Value : single ;
   ch,Row : Integer ;
begin
     if key = chr(13) then begin
         { Time calibration bar }
         Value := ExtractFloat( CalibrationBarTable.cells[1,0], 1. ) ;
         CalibrationBarTable.cells[1,0] := Format( '%.4g %s',
                                        [Value,Display.TUnits])  ;

         { Channel calibration bars }
         Row := 1 ;
         for ch := 0 to Display.NumChannels-1 do if Display.ChanVisible[ch] then begin
             Value := ExtractFloat(CalibrationBarTable.Cells[1,Row],10. ) ;
             CalibrationBarTable.cells[0,Row] := EDRFile.Channel[ch].ADCName ;
             CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                                 [Value,EDRFile.Channel[ch].ADCUnits]) ;
             Inc(Row) ;
             end ;
         end ;

     end;


end.
