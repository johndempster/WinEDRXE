unit StimGen;
// ------------------------------------------
// WinEDR - Stimulus pulse editing dialog box
// ------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, global, ValidatedEdit, ExtCtrls ;

type
  TStimGenFrm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbSinglePulse: TRadioButton;
    rbPulseTrain: TRadioButton;
    PulseGrp: TGroupBox;
    edPulseWidth: TValidatedEdit;
    Label1: TLabel;
    Label2: TLabel;
    edPulseHeight: TValidatedEdit;
    edPeriod: TValidatedEdit;
    Label3: TLabel;
    bOK: TButton;
    bCancel: TButton;
    Label4: TLabel;
    edDivideFactor: TValidatedEdit;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edDelay: TValidatedEdit;
    PulseTrainPanel: TPanel;
    lbNumPulses: TLabel;
    edNumPulses: TValidatedEdit;
    lbPulseFrequency: TLabel;
    edPulseFrequency: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure rbSinglePulseClick(Sender: TObject);
    procedure rbPulseTrainClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StimGenFrm: TStimGenFrm;

implementation

{$R *.DFM}

procedure TStimGenFrm.FormShow(Sender: TObject);
{ --------------------
  Initialise controls
  -------------------- }
begin
     edPulseWidth.Value := Settings.Stimulator.PulseWidth ;
     edpulseheight.Value := Settings.Stimulator.pulseheight ;
     edPeriod.Value := Settings.Stimulator.Period ;
     edDelay.Value := Settings.Stimulator.Delay ;

     if Settings.Stimulator.NumPulses > 1 then begin
        rbSinglePulse.Checked := False ;
        rbPulseTrain.Checked := True ;
        PulseTrainPanel.Visible := True ;
        edNumPulses.Value := Settings.Stimulator.NumPulses ;
        edPulseFrequency.Value := Settings.Stimulator.PulseFrequency ;
        end
     else begin
        rbSinglePulse.Checked := True ;
        rbPulseTrain.Checked := False ;
        PulseTrainPanel.Visible := False ;
        end ;

     edDivideFactor.value := Settings.VCommand[0].DivideFactor ;
     end;


procedure TStimGenFrm.bOKClick(Sender: TObject);
{ --------------------------
  Update stimulator settings
  -------------------------- }
begin
     Settings.Stimulator.Period := edPeriod.Value ;
     Settings.Stimulator.PulseWidth := edPulseWidth.Value ;
     Settings.Stimulator.pulseheight := edpulseheight.Value ;
     Settings.Stimulator.Delay := edDelay.Value ;

     if rbSinglePulse.Checked then
        Settings.Stimulator.NumPulses := 1
     else Settings.Stimulator.NumPulses := Round(edNumPulses.Value) ;
     Settings.Stimulator.PulseFrequency := edPulseFrequency.Value ;
     Settings.VCommand[0].DivideFactor := edDivideFactor.value ;
     Settings.Stimulator.NewSettings := True ;
     
     Close ;
     end;

procedure TStimGenFrm.rbSinglePulseClick(Sender: TObject);
begin
    PulseTrainPanel.Visible := False ;
    end ;

procedure TStimGenFrm.rbPulseTrainClick(Sender: TObject);
begin
     PulseTrainPanel.Visible := True ;
     if edNumPulses.Value < 1 then edNumPulses.Value := 1.0 ;
     end;

procedure TStimGenFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TStimGenFrm.bCancelClick(Sender: TObject);
// ---------
// Exit form
// ---------
begin
     Close ;
     end;

end.
