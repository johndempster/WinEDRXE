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

uses EDRFileUnit;


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

     edTopMargin.Value := EDRFile.Settings.Plot.TopMargin ;
     edBottomMargin.Value := EDRFile.Settings.Plot.BottomMargin ;
     edLeftMargin.Value := EDRFile.Settings.Plot.LeftMargin ;
     edRightMargin.Value := EDRFile.Settings.Plot.RightMargin ;
     edBitMapWidth.Value := EDRFile.Settings.BitMapWidth ;
     edBitMapHeight.Value := EDRFile.Settings.BitMapHeight ;

     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;
     edFontSize.Value := EDRFile.Settings.Plot.FontSize ;
     edLineThickness.Value := EDRFile.Settings.Plot.LineThickness ;
     edMarkerSize.Value := EDRFile.Settings.Plot.MarkerSize ;
     cbFontName.itemindex := cbFontName.items.indexof(EDRFile.Settings.Plot.FontName) ;
     if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
     ckUseColor.checked := EDRFile.Settings.Plot.UseColor ;

     end;


procedure TPrintGraphFrm.bOKClick(Sender: TObject);
begin
     { Save new settings }
     EDRFile.Settings.Plot.TopMargin := edTopMargin.Value ;
     EDRFile.Settings.Plot.BottomMargin := edBottomMargin.Value ;
     EDRFile.Settings.Plot.LeftMargin:= edLeftMargin.Value  ;
     EDRFile.Settings.Plot.RightMargin := edRightMargin.Value ;
     EDRFile.Settings.Plot.FontName := cbFontName.text ;
     EDRFile.Settings.Plot.FontSize := Round(edFontSize.Value) ;
     EDRFile.Settings.Plot.LineThickness := Round(edLineThickness.Value) ;
     EDRFile.Settings.Plot.MarkerSize := Round(edMarkerSize.Value) ;
     EDRFile.Settings.BitMapWidth := Round(edBitMapWidth.Value)  ;
     EDRFile.Settings.BitMapHeight := Round(edBitMapHeight.Value)  ;
     EDRFile.Settings.Plot.UseColor := ckUseColor.checked ;

     { Update settings in XYplot component }
     Plot.PrinterTopMargin := Round(EDRFile.Settings.Plot.TopMargin) ;
     Plot.PrinterBottomMargin := Round(EDRFile.Settings.Plot.BottomMargin) ;
     Plot.PrinterLeftMargin := Round(EDRFile.Settings.Plot.LeftMargin) ;
     Plot.PrinterRightMargin := Round(EDRFile.Settings.Plot.RightMargin) ;
     Plot.PrinterDisableColor := not EDRFile.Settings.Plot.UseColor ;
     Plot.PrinterFontSize := EDRFile.Settings.Plot.FontSize ;
     Plot.PrinterFontName := EDRFile.Settings.Plot.FontName ;
     Plot.PrinterLineWidth := EDRFile.Settings.Plot.LineThickness ;
     Plot.PrinterMarkerSize := EDRFile.Settings.Plot.MarkerSize ;
     Plot.MetafileWidth := EDRFile.Settings.BitMapWidth ;
     Plot.MetafileHeight := EDRFile.Settings.BitMapHeight ;

     end;


end.
