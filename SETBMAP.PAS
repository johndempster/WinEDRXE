unit Setbmap;
{ =========================================================
  Set dimensions and type face for a Windows metafile plot
  (c) J. Dempster 1999
  =========================================================
  20/1/99 Now includes font name and size }

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, shared, global, sysutils ;

type
  TSetBitmapFrm = class(TForm)
    GroupBox1: TGroupBox;
    edWidth: TEdit;
    Label1: TLabel;
    edHeight: TEdit;
    Label2: TLabel;
    bOK: TButton;
    bCancel: TButton;
    FontGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    cbFontName: TComboBox;
    edPointSize: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SetBitmapFrm: TSetBitmapFrm;

implementation

{$R *.DFM}

uses maths ;

procedure TSetBitmapFrm.FormShow(Sender: TObject);
begin
     edWidth.text := format( '%d pixels', [Settings.BitmapWidth]) ;
     edHeight.text := format( '%d pixels', [Settings.BitmapHeight]) ;

     { Font names }
     cbFontName.clear ;
     cbFontName.sorted := True ;
     cbFontName.items := screen.fonts ;
     cbFontName.ItemIndex := cbFontname.items.indexof( Settings.Plot.FontName ) ;
     edPointSize.text := format( ' %d', [Settings.Plot.FontSize]) ;

     end;

procedure TSetBitmapFrm.bOKClick(Sender: TObject);
begin
     Settings.BitmapWidth := MinInt([MaxInt([ExtractInt(edWidth.text),BitMapsMinSize]),
                                     BitmapsMaxSize]);
     Settings.BitmapHeight := MinInt([MaxInt([ExtractInt(edHeight.text),BitmapsMinSize]),
                                     BitmapsMaxSize]);

     { Text size (points) }
     Settings.Plot.FontSize := ExtractInt(edPointSize.text) ;
     edPointSize.text := format(' %d pts ',[Settings.Plot.FontSize]) ;
     { Font name }
     Settings.Plot.FontName := cbFontName.Text ;

     end;

end.
