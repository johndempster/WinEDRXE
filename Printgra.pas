unit Printgra;
{ =======================================================
  Updates printer page settings in Settings.Plot
  21/2/00
  =======================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, Buttons, Global, Printers, Shared,
  ValEdit, XYPlotDisplay, ExtCtrls, ValidatedEdit ;

type
  TPrintGraphFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    FontGrp: TGroupBox;
    Label7: TLabel;
    cbFontName: TComboBox;
    edFontSize: TValidatedEdit;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edLineThickness: TValidatedEdit;
    Page: TNotebook;
    PrinterGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLeftMargin: TValidatedEdit;
    edRightMargin: TValidatedEdit;
    edTopMargin: TValidatedEdit;
    edBottomMargin: TValidatedEdit;
    MetafileGrp: TGroupBox;
    Label6: TLabel;
    Label9: TLabel;
    edBitmapWidth: TValidatedEdit;
    edBitmapHeight: TValidatedEdit;
    edMarkerSize: TValidatedEdit;
    Label8: TLabel;
    ckUseColor: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ToPrinter : Boolean ;
    Plot : TXYPlotDisplay ;

  end;

var
  PrintGraphFrm: TPrintGraphFrm;

implementation

{$R *.DFM}


procedure TPrintGraphFrm.FormShow(Sender: TObject);
{ --------------------------------------------
  Initialise edit boxes when form is displayed
  -------------------------------------------- }
begin
     { Select appropriate page for printer page margins or
       metafile image size }
     if ToPrinter then begin
        Page.PageIndex := 0 ;
        Caption := ' Print ' ;
        edFontSize.Units := 'pts' ;
        edMarkerSize.Units := 'pts' ;
        edLineThickness.Units := 'pts' ;
        end
     else begin
        Page.PageIndex := 1 ;
        Caption := ' Copy Image ' ;
        edFontSize.Units := 'pixels' ;
        edMarkerSize.Units := 'pixels' ;
        edLineThickness.Units := 'pixels' ;
        end ;

     edTopMargin.Value := Settings.Plot.TopMargin ;
     edBottomMargin.Value := Settings.Plot.BottomMargin ;
     edLeftMargin.Value := Settings.Plot.LeftMargin ;
     edRightMargin.Value := Settings.Plot.RightMargin ;
     edBitMapWidth.Value := Settings.BitMapWidth ;
     edBitMapHeight.Value := Settings.BitMapHeight ;

     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;
     edFontSize.Value := Settings.Plot.FontSize ;
     edLineThickness.Value := Settings.Plot.LineThickness ;
     edMarkerSize.Value := Settings.Plot.MarkerSize ;
     cbFontName.itemindex := cbFontName.items.indexof(Settings.Plot.FontName) ;
     if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
     ckUseColor.checked := Settings.Plot.UseColor ;

     end;


procedure TPrintGraphFrm.bOKClick(Sender: TObject);
begin
     { Save new settings }
     Settings.Plot.TopMargin := edTopMargin.Value ;
     Settings.Plot.BottomMargin := edBottomMargin.Value ;
     Settings.Plot.LeftMargin:= edLeftMargin.Value  ;
     Settings.Plot.RightMargin := edRightMargin.Value ;
     Settings.Plot.FontName := cbFontName.text ;
     Settings.Plot.FontSize := Round(edFontSize.Value) ;
     Settings.Plot.LineThickness := Round(edLineThickness.Value) ;
     Settings.Plot.MarkerSize := Round(edMarkerSize.Value) ;
     Settings.BitMapWidth := Round(edBitMapWidth.Value)  ;
     Settings.BitMapHeight := Round(edBitMapHeight.Value)  ;
     Settings.Plot.UseColor := ckUseColor.checked ;

     { Update settings in XYplot component }
     Plot.PrinterTopMargin := Round(Settings.Plot.TopMargin) ;
     Plot.PrinterBottomMargin := Round(Settings.Plot.BottomMargin) ;
     Plot.PrinterLeftMargin := Round(Settings.Plot.LeftMargin) ;
     Plot.PrinterRightMargin := Round(Settings.Plot.RightMargin) ;
     Plot.PrinterDisableColor := not Settings.Plot.UseColor ;
     Plot.PrinterFontSize := Settings.Plot.FontSize ;
     Plot.PrinterFontName := Settings.Plot.FontName ;
     Plot.PrinterLineWidth := Settings.Plot.LineThickness ;
     Plot.PrinterMarkerSize := Settings.Plot.MarkerSize ;
     Plot.MetafileWidth := Settings.BitMapWidth ;
     Plot.MetafileHeight := Settings.BitMapHeight ;

     end;


end.
