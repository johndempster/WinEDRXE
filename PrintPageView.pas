unit PrintPageView;
{ =================================================================
  WinEDR - PageView display printing/copying settings dialog box
  12.12.02
  =================================================================}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RangeEdit, StdCtrls, ValidatedEdit, ExtCtrls, ScopeDisplay, global, printers ;

type
  TDestination = (dePrinter,deClipboard) ;
  TPrintPageViewFrm = class(TForm)
    Page: TNotebook;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLeftMargin: TValidatedEdit;
    edTopMargin: TValidatedEdit;
    edRightMargin: TValidatedEdit;
    edBottomMargin: TValidatedEdit;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    edWidth: TValidatedEdit;
    edHeight: TValidatedEdit;
    FontGrp: TGroupBox;
    Label9: TLabel;
    cbFontName: TComboBox;
    edFontSize: TValidatedEdit;
    edLineThickness: TValidatedEdit;
    Label10: TLabel;
    bPrint: TButton;
    GroupBox4: TGroupBox;
    rbCurrentPage: TRadioButton;
    rbWholeRecord: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    edVerticalCalBar: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    edHorizontalCalBar: TValidatedEdit;
    edVertCalBar: TValidatedEdit;
    bCancel: TButton;
    ckUseColor: TCheckBox;
    ckShowLabels: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Destination : TDestination ;
    Display : TScopeDisplay ;
    StartTime : Single ;            // Start time of first line of page display (s)
    EndTime : Single ;              // End time of page display lines (s)
  end;

var
  PrintPageViewFrm: TPrintPageViewFrm;

implementation

{$R *.dfm}

procedure TPrintPageViewFrm.FormShow(Sender: TObject);
// ------------------------------------------
// Initialise controls when form is displayed
// ------------------------------------------
begin

     { Select appropriate settings page }
     if Destination = dePrinter then begin
        Caption := ' Print ' ;
        bPrint.Caption := 'Print' ;
        Page.PageIndex := 0 ;
        edFontSize.Units := 'pts' ;
        edLineThickness.Units := 'pts' ;
        rbWholeRecord.Enabled := True ;
        rbRange.Enabled := True ;
        end
     else begin
        Caption := ' Copy Image ' ;
        bPrint.Caption := 'Copy' ;
        Page.PageIndex := 1 ;
        edFontSize.Units := 'pixels' ;
        edLineThickness.Units := 'pixels' ;
        rbWholeRecord.Enabled := False ;
        rbRange.Enabled := False ;
        end ;

     // Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;

     // Set limits of printer range
     rbCurrentPage.Checked := True ;
     edRange.LoValue := 0.0 ;
     edRange.HiLimit := 1E30 ;
     edRange.HiValue := CdrFH.RecordDuration ;
     edRange.HiLimit := edRange.HiValue ;

     // Set horizontal and vertical printer calibration bars
     if Settings.TBarValue = 0.0 then begin
        edHorizontalCalBar.Value := (Display.XMax - Display.XMin)*0.1*CdrFH.dt ;
        end ;
     edVertCalBar.Value := (Display.YMax[0] - Display.YMin[0])*0.1*Display.ChanScale[0] ;
     edVertCalBar.Units := Display.ChanUnits[0] ;

      { Update text box settings }
      edLeftMargin.Value := Settings.Plot.LeftMargin ;
      edRightMargin.Value := Settings.Plot.RightMargin ;
      edTopMargin.Value := Settings.Plot.TopMargin ;
      edBottomMargin.Value := Settings.Plot.BottomMargin ;
      edWidth.Value := Settings.Plot.MetafileWidth ;
      edHeight.Value := Settings.Plot.MetafileHeight ;
      edFontSize.Value := Settings.Plot.FontSize ;
      edLineThickness.Value := Settings.Plot.LineThickness ;
      cbFontName.itemindex := cbFontName.items.indexof(Settings.Plot.FontName) ;
      if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
      ckShowLabels.checked := Settings.ShowLabels ;
      ckUseColor.checked := Settings.Plot.UseColor ;


     end;

procedure TPrintPageViewFrm.bPrintClick(Sender: TObject);
{ -----------------------
  Update global settings
  ----------------------}
var
   ch : Integer ;
begin

      { Update settings from text boxes }
      Settings.Plot.LeftMargin := edLeftMargin.Value ;
      Settings.Plot.RightMargin := edRightMargin.Value ;
      Settings.Plot.TopMargin := edTopMargin.Value ;
      Settings.Plot.BottomMargin := edBottomMargin.Value ;
      Settings.Plot.MetafileWidth := Round(edWidth.Value) ;
      Settings.Plot.MetafileHeight := Round(edHeight.Value) ;
      Settings.Plot.FontSize := Round(edFontSize.Value) ;
      Settings.Plot.LineThickness := Round(edLineThickness.Value) ;
      Settings.Plot.FontName :=  cbFontName.text ;

      Settings.ShowLabels := ckShowLabels.checked ;
      Settings.Plot.UseColor := ckUseColor.checked ;

      Settings.TBarValue :=  edHorizontalCalBar.Value ;

      { Copy data into display object }
      Display.PrinterLeftMargin := Round(Settings.Plot.LeftMargin) ;
      Display.PrinterRightMargin := Round(Settings.Plot.RightMargin) ;
      Display.PrinterTopMargin := Round(Settings.Plot.TopMargin) ;
      Display.PrinterBottomMargin := Round(Settings.Plot.BottomMargin) ;
      Display.PrinterFontName := Settings.Plot.FontName ;
      Display.PrinterFontSize := Settings.Plot.FontSize ;
      Display.PrinterPenWidth := Settings.Plot.LineThickness ;
      Display.MetafileWidth := Settings.Plot.MetaFileWidth ;
      Display.MetafileHeight := Settings.Plot.MetaFileHeight ;
      Display.PrinterShowLabels := Settings.ShowLabels ;
      Display.PrinterDisableColor := not Settings.Plot.UseColor ;

      // Update channel calibration bar values
      for ch := 0 to Display.NumChannels-1 do
          Channel[ch].ADCCalibrationBar := edVertCalBar.Value ;

      // Set all calibration bars to zero except bottom one
      for ch := 0 to Display.NumChannels-2 do Display.ChanCalBar[ch] := 0.0 ;
      Display.ChanCalBar[Display.NumChannels-1] := Channel[0].ADCCalibrationBar ;
      Display.TCalBar := Settings.TBarValue/CdrFH.dt ;

      // Set range of times to be printed
      if rbWholeRecord.Checked then begin
         StartTime := 0.0 ;
         EndTime := CdrFH.RecordDuration ;
         end
      else if rbRange.Checked then begin
         StartTime := edRange.LoValue ;
         EndTime := edRange.HiValue ;
         end ;

      end ;



end.
