unit Cdrzero;
{ -----------------------------------------------------------
  WinCDR (c) J.Dempster, University of Strathclyde 1996-97
  ViewCDR channel zero level setup module
  -----------------------------------------------------------}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, global, Shared, fileio, math, maths ;

type
  TCDRZeroFrm = class(TForm)
    edChannel: TEdit;
    edLevel: TEdit;
    Label3: TLabel;
    bOK: TButton;
    bCancel: TButton;
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ChSel : LongInt ;
  end;

var
  CDRZeroFrm: TCDRZeroFrm;

implementation

{$R *.DFM}

procedure TCDRZeroFrm.bOKClick(Sender: TObject);
begin
     Channel[ChSel].ADCZero := ExtractInt( edLevel.text ) ;
     Channel[ChSel].ADCZero := Max(-Channel[ChSel].ADCMaxValue-1,Channel[ChSel].ADCZero) ;
     Channel[ChSel].ADCZero := Min(Channel[ChSel].ADCMaxValue,Channel[ChSel].ADCZero) ;
     Channel[ChSel].ADCZeroAt := -1 ;
     SaveCDRHeader( CdrFH ) ;
     end;

procedure TCDRZeroFrm.FormShow(Sender: TObject);
begin
     edChannel.text := format('Ch.%d %s',[ChSel,Channel[ChSel].ADCName]) ;
     edLevel.text := IntToStr( Channel[ChSel].ADCZero ) ;
     end ;

end.
