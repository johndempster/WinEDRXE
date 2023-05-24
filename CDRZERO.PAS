unit Cdrzero;
{ -----------------------------------------------------------
  WinCDR (c) J.Dempster, University of Strathclyde 1996-97
  ViewCDR channel zero level setup module
  -----------------------------------------------------------}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, math, maths, edrfileunit ;

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
     EDRFile.Channel[ChSel].ADCZero := ExtractInt( edLevel.text ) ;
     EDRFile.Channel[ChSel].ADCZero := Max(-EDRFile.Channel[ChSel].ADCMaxValue-1,EDRFile.Channel[ChSel].ADCZero) ;
     EDRFile.Channel[ChSel].ADCZero := Min(EDRFile.Channel[ChSel].ADCMaxValue,EDRFile.Channel[ChSel].ADCZero) ;
     EDRFile.Channel[ChSel].ADCZeroAt := -1 ;
     EDRFile.SaveHeader( EDRFile.CdrFH ) ;
     end;


procedure TCDRZeroFrm.FormShow(Sender: TObject);
begin
     edChannel.text := format('Ch.%d %s',[ChSel,EDRFile.Channel[ChSel].ADCName]) ;
     edLevel.text := format( '%4g',[EDRFile.Channel[ChSel].ADCZero] ) ;
     end ;

end.
