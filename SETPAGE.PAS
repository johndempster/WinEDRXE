unit Setpage;
{ =======================================================
  Updates printer page settings in Settings.Plot
  (c) J. Dempster 1999
  =======================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, Buttons, Printers ;

type
  TSetPageFrm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLeftMargin: TEdit;
    edTopMargin: TEdit;
    edRightMargin: TEdit;
    edBottomMargin: TEdit;
    bOK: TButton;
    bCancel: TButton;
    FontGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    cbFontName: TComboBox;
    edPointSize: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure edRightMarginKeyPress(Sender: TObject; var Key: Char);
    procedure edTopMarginKeyPress(Sender: TObject; var Key: Char);
    procedure edBottomMarginKeyPress(Sender: TObject; var Key: Char);
    procedure edLeftMarginKeyPress(Sender: TObject; var Key: Char);
    procedure bCancelClick(Sender: TObject);
    procedure edPointSizeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure UpdateSettings ;

  public
    { Public declarations }

  end;

var
  SetPageFrm: TSetPageFrm;

implementation

{$R *.DFM}

uses maths, EDRFileUnit ;

var
   KeepSettings : TPageSettings ;

procedure TSetPageFrm.FormShow(Sender: TObject);
begin
     KeepSettings := EDRFile.Settings.Plot ;
     edTopMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.TopMargin]);
     edBottomMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.BottomMargin]);
     edLeftMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.LeftMargin]);
     edRightMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.RightMargin]);
     { Font names }
     cbFontName.clear ;
     cbFontName.sorted := True ;
     cbFontName.items := screen.fonts ;
     cbFontName.ItemIndex := cbFontname.items.indexof( EDRFile.Settings.Plot.FontName ) ;
     edPointSize.text := format( ' %d', [EDRFile.Settings.Plot.FontSize]) ;

     UpdateSettings ;
     end;


procedure TSetPageFrm.UpdateSettings ;
{ ---------------------------
  Update page margin settings
  ---------------------------}
begin
     { Top of page margin }
     EDRFile.Settings.Plot.TopMargin := ExtractFloat(edTopMargin.text,
                                EDRFile.Settings.Plot.TopMargin) ;
     edTopMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.TopMargin]);

     { Bottom of page margin }
     EDRFile.Settings.Plot.BottomMargin := ExtractFloat(edBottomMargin.text,
                                   EDRFile.Settings.Plot.BottomMargin) ;
     edBottomMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.BottomMargin]);

     { Left page margin }
     EDRFile.Settings.Plot.LeftMargin := ExtractFloat(edLeftMargin.text,
                                 EDRFile.Settings.Plot.LeftMargin) ;
     edLeftMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.LeftMargin]);

     { Right page margin }
     EDRFile.Settings.Plot.RightMargin := ExtractFloat(edRightMargin.text,
                                  EDRFile.Settings.Plot.RightMargin) ;
     edRightMargin.text := format( '%.1f cm',[EDRFile.Settings.Plot.RightMargin]);

     { Text size (points) }
     EDRFile.Settings.Plot.FontSize := ExtractInt(edPointSize.text) ;
     edPointSize.text := format(' %d pts ',[EDRFile.Settings.Plot.FontSize]) ;
     { Font name }
     EDRFile.Settings.Plot.FontName := cbFontName.Text ;

     end ;


procedure TSetPageFrm.bOKClick(Sender: TObject);
begin
     UpdateSettings ;
     end;


procedure TSetPageFrm.edRightMarginKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;


procedure TSetPageFrm.edTopMarginKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;


procedure TSetPageFrm.edBottomMarginKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;


procedure TSetPageFrm.edLeftMarginKeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;

procedure TSetPageFrm.bCancelClick(Sender: TObject);
begin
     { If operation has been cancelled, restore old settings }
     EDRFile.Settings.Plot := KeepSettings ;
     end;

procedure TSetPageFrm.edPointSizeKeyPress(Sender: TObject; var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;

end.
