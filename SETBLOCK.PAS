unit Setblock;
{ =============================================================
  WinCDR (c) J. Dempster, University of Strathclyde, 1998
  Set Test/Background and Rejected status of a block of records
  (Called by PrwSpec.pas)
  20/7/98
  19/4/99 Now uses TRangeEdit component
  =============================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, shared, RangeEdit ;

type
  TSetBlockFrm = class(TForm)
    GroupBox1: TGroupBox;
    cbRecType: TComboBox;
    Label10: TLabel;
    ckRejected: TCheckBox;
    Label1: TLabel;
    bOK: TButton;
    bCancel: TButton;
    edRange: TRangeEdit;
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StartAt : LongInt ;
    EndAt : LongInt ;
  end;

var
  SetBlockFrm: TSetBlockFrm;

implementation

{$R *.DFM}

uses NoiseAnal ;


procedure TSetBlockFrm.FormShow(Sender: TObject);
{ --------------------------------------
  Initialisations when form is displayed
  --------------------------------------}
begin
     edRange.LoLimit := 1 ;
     edRange.HiLimit := EndAt+1 ;
     edRange.LoValue := 1 ;
     edRange.HiValue := EndAt+1  ;
     end;


procedure TSetBlockFrm.bOKClick(Sender: TObject);
{ --------------
  Exit form
  --------------}
begin
   { Get range of records to change and put into StartAt and EndAt
     for calling module to use }
   StartAt := Round(edRange.LoValue)-1 ;
   EndAt := Round(edRange.HiValue)-1 ;
   end;


end.
