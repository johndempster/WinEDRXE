unit Zero;
{ -----------------------------------------------------------
  WinCDR (c) J.Dempster, University of Strathclyde 1996-97
  ViewWCP channel zero level setup module
  -----------------------------------------------------------
  28.8.12 ... Zero level no longer writes to CDRFH, now callable from any form including recording
  }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Global, Mask, Shared, fileio, ValidatedEdit, math ;

type
  TZeroFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    edChannel: TEdit;
    Label3: TLabel;
    edLevel: TValidatedEdit;
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ChSel : LongInt ;
    NewZeroAt : LongInt ;
    ZeroLevel : Integer ;
    ChanName : String
  end;

var
  ZeroFrm: TZeroFrm;

implementation

{$R *.DFM}

uses maths ;

procedure TZeroFrm.FormActivate(Sender: TObject);
begin
     edChannel.text := format('Ch.%d %s',[ChSel,ChanName]) ;
     edLevel.Value := ZeroLevel ;

     end ;


procedure TZeroFrm.bOKClick(Sender: TObject);
begin
    ZeroLevel := Round(edLevel.Value) ;
    end;


end.
