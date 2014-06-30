unit About;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, shared ;

type
  TAboutDlg = class(TForm)
    Panel1: TPanel;
    ProductName: TLabel;
    edVersion: TLabel;
    Copyright: TLabel;
    Label1: TLabel;
    bOK: TButton;
    GroupBox1: TGroupBox;
    edSupplier: TEdit;
    EdModel: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.DFM}

uses global, Mdiform ;

procedure TAboutDlg.FormActivate(Sender: TObject);
begin
     edSupplier.text := Main.SESLabIO.LabInterfaceName ;
     edModel.text := Main.SESLabIO.LabInterfaceModel ;
     edVersion.caption := 'Version ' + Main.Version ;
end;

procedure TAboutDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree ;
     end;

end.

