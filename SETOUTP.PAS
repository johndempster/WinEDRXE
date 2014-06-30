unit Setoutp;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TDefSetFrm = class(TForm)
    DACGroup: TGroupBox;
    edVCommand: TEdit;
    Label1: TLabel;
    edVDivide: TEdit;
    Label2: TLabel;
    DIGGroup: TGroupBox;
    GroupBox2: TGroupBox;
    rbOn1: TRadioButton;
    rbOff1: TRadioButton;
    GroupBox5: TGroupBox;
    rbOn3: TRadioButton;
    rboff3: TRadioButton;
    GroupBox6: TGroupBox;
    rbon4: TRadioButton;
    rboff4: TRadioButton;
    GroupBox7: TGroupBox;
    rbon5: TRadioButton;
    rboff5: TRadioButton;
    GroupBox8: TGroupBox;
    rbon6: TRadioButton;
    rboff6: TRadioButton;
    GroupBox9: TGroupBox;
    rbon7: TRadioButton;
    rboff7: TRadioButton;
    GroupBox1: TGroupBox;
    rbOn2: TRadioButton;
    rbOff2: TRadioButton;
    Dig0: TGroupBox;
    RadioButton5: TRadioButton;
    rbOff0: TRadioButton;
    Label3: TLabel;
    Label4: TLabel;
    bOK: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure edVCommandKeyPress(Sender: TObject; var Key: Char);
    procedure edVDivideKeyPress(Sender: TObject; var Key: Char);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DefSetFrm: TDefSetFrm;

implementation

{$R *.DFM}

procedure TDefSetFrm.FormCreate(Sender: TObject);
var
   Bit : Word ;
begin
     { Get default digital O/P settings }
     Bit := Settings.DigitalPort.Value and 1 ;
     if Bit <> 0 then rbOn0.checked
                 else rbOff0.checked ;
     Bit := Settings.DigitalPort.Value and 2 ;
     if Bit <> 0 then rbOn1.checked
                 else rbOff1.checked ;
     Bit := Settings.DigitalPort.Value and 4 ;
     if Bit <> 0 then rbOn2.checked
                 else rbOff2.checked ;
     Bit := Settings.DigitalPort.Value and 8 ;
     if Bit <> 0 then rbOn3.checked
                 else rbOff3.checked ;
     Bit := Settings.DigitalPort.Value and 16 ;
     if Bit <> 0 then rbOn4.checked
                 else rbOff4.checked ;
     Bit := Settings.DigitalPort.Value and 32 ;
     if Bit <> 0 then rbOn5.checked
                 else rbOff5.checked ;
     Bit := Settings.DigitalPort.Value and 64 ;
     if Bit <> 0 then rbOn6.checked
                 else rbOff6.checked ;
     Bit := Settings.DigitalPort.Value and 128 ;
     if Bit <> 0 then rbOn7.checked
                 else rbOff7.checked ;
     { Get voltage settings }
     edVCommand.text := format(' %f mV',[Settings.VCommand.HoldingVoltage]) ;
     edVDivide.text := format(' %f ',[Settings.VCommand.DivideFactor]) ;
     end;

procedure TDefSetFrm.bOKClick(Sender: TObject);
var
   Value : Word ;
begin
     { Update default digital O/P settings }
     Value := 0 ;
     if rbOn0.checked then Value := Value or 1 ;
     if rbOn1.checked then Value := Value or 2 ;
     if rbOn2.checked then Value := Value or 4 ;
     if rbOn3.checked then Value := Value or 8 ;
     if rbOn4.checked then Value := Value or 16 ;
     if rbOn5.checked then Value := Value or 32 ;
     if rbOn6.checked then Value := Value or 64 ;
     if rbOn7.checked then Value := Value or 128 ;
     Settings.DigitalPort.Value := Value ;
     {Update voltage settings }
     Settings.VCommand.HoldingVoltage := ExtractFloat( edVCommand.text,
                                         Settings.VCommand.HoldingVoltage ) ;
     Settings.VCommand.DivideFactor := ExtractFloat( edVDivide.text,
                                         Settings.VCommand.DivideFactor ) ;
     close ;
     end;

procedure TDefSetFrm.edVCommandKeyPress(Sender: TObject; var Key: Char);
var
   Value : single ;
begin
     if key = char(13) then begin
        Value := ExtractFloat(edVCommand.text,Settings.VCommand.HoldingVoltage) ;
        edVCommand.text := format( ' %f mV ', [Value] ) ;
        end ;
     end;

procedure TDefSetFrm.edVDivideKeyPress(Sender: TObject; var Key: Char);
ar
   Value : single ;
begin
     if key = char(13) then begin
        Value := ExtractFloat(edVDivide.text,Settings.VCommand.DivideFactor) ;
        edVDdivide.text := format( ' %f mV ', [Value] ) ;
        end ;
     end;

procedure TDefSetFrm.bCancelClick(Sender: TObject);
begin
     close ;
     end;

end.
