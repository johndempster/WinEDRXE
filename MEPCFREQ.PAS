unit Mepcfreq;
{ =======================================================
  WinCDR (c) J. Dempster, University of Strathclyde, 1998
  MEPC Frequency computation module

  Computes MEPC frequency from skew and variance of a
  fluctuating current signal.

  =======================================================}


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, global, shared, ValEdit, ValidatedEdit ;

type
  TMEPCFreqFrm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    bOK: TButton;
    bCancel: TButton;
    edTauRise: TValidatedEdit;
    edTauDecay: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MEPCFreqFrm: TMEPCFreqFrm;

implementation

{$R *.DFM}

uses EDRFileUnit;


procedure TMEPCFreqFrm.FormShow(Sender: TObject);
{ ---------------------------
  Initialise controls on form
  ---------------------------}
begin
     if EDRFile.Settings.Variance.TauRise <= 0.0 then EDRFile.Settings.Variance.TauRise := EDRFile.cdrFH.dt ;
     edTauRise.Value := EDRFile.Settings.Variance.TauRise ;
     if EDRFile.Settings.Variance.TauDecay <= 0.0 then EDRFile.Settings.Variance.TauDecay := EDRFile.cdrFH.dt*50.0 ;
     edTauDecay.Value := EDRFile.Settings.Variance.TauDecay ;
     end;


procedure TMEPCFreqFrm.bOKClick(Sender: TObject);
begin
     EDRFile.Settings.Variance.TauRise := edTauRise.Value ;
     EDRFile.Settings.Variance.TauDecay := edTauDecay.Value ;
     end;

end.
