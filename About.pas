unit About;
// =================================
// Display information about program
// =================================
//  14.03.24 ... Form position saved to INI file

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
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.DFM}

uses global, Mdiform , EDRFileUnit;

procedure TAboutDlg.bOKClick(Sender: TObject);
begin
    Close ;
end;

procedure TAboutDlg.FormActivate(Sender: TObject);
begin
     edSupplier.text := Main.SESLabIO.LabInterfaceName ;
     edModel.text := Main.SESLabIO.LabInterfaceModel ;
     edVersion.caption := 'Version ' + EDRFile.Version ;
end;

procedure TAboutDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree ;

    // Save form position
    EDRFile.SaveFormPosition( Self ) ;

end;

end.

