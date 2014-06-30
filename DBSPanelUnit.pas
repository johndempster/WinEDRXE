unit DBSPanelUnit;
// -------------------------------------
// Direct Brain Stimulator Control Panel
// -------------------------------------
// V1.0 6/7/11

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, mmsystem ;

type
  TDBSPanelFrm = class(TForm)
    StimGrp: TGroupBox;
    edPulseWidth: TValidatedEdit;
    edPulseFrequency: TValidatedEdit;
    Label1: TLabel;
    Label2: TLabel;
    SystermPowerGrp: TGroupBox;
    rbActiveMode: TRadioButton;
    rbSleepMode: TRadioButton;
    GroupBox2: TGroupBox;
    rbStimulatorOn: TRadioButton;
    rbStimulatorOff: TRadioButton;
    bUpdateDBS: TButton;
    Timer: TTimer;
    meStatus: TMemo;
    GroupBox1: TGroupBox;
    procedure FormShow(Sender: TObject);
    procedure bUpdateDBSClick(Sender: TObject);
    procedure rbStimulatorOnClick(Sender: TObject);
    procedure rbStimulatorOffClick(Sender: TObject);
    procedure rbActiveModeClick(Sender: TObject);
    procedure rbSleepModeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edPulseWidthKeyPress(Sender: TObject; var Key: Char);
    procedure edPulseFrequencyKeyPress(Sender: TObject; var Key: Char);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    TSystemPowerEnable : Integer ;
  public
    { Public declarations }
  end;

var
  DBSPanelFrm: TDBSPanelFrm;

implementation

uses Mdiform;

{$R *.dfm}

procedure TDBSPanelFrm.FormShow(Sender: TObject);
begin
     ClientWidth := StimGrp.Left + StimGrp.Width + 5 ;
     ClientHeight := bUpdateDBS.Top + bUpdateDBS.Height + 10 ;

     edPulseWidth.Value := Main.SESLabIO.DBSPulseWidth ;
     edPulseFrequency.Value := Main.SESLabIO.DBSFrequency ;
     rbStimulatorOn.Checked := Main.SESLabIO.DBSStimulus ;
     rbStimulatorOff.Checked := not Main.SESLabIO.DBSStimulus ;
     rbSleepMode.Checked := Main.SESLabIO.DBSSleepMode ;
     rbActiveMode.Checked := not Main.SESLabIO.DBSSleepMode ;
     TSystemPowerEnable := 0 ;

     end;

procedure TDBSPanelFrm.bUpdateDBSClick(Sender: TObject);
begin
     Main.SESLabIO.DBSStimulus := rbStimulatorOn.Checked ;
     Main.SESLabIO.DBSFrequency := edPulseFrequency.Value ;
     Main.SESLabIO.DBSPulseWidth := edPulseWidth.Value ;
     end;

procedure TDBSPanelFrm.rbStimulatorOnClick(Sender: TObject);
begin
     Main.SESLabIO.DBSStimulus := rbStimulatorOn.Checked ;
     end;

procedure TDBSPanelFrm.rbStimulatorOffClick(Sender: TObject);
begin
     Main.SESLabIO.DBSStimulus := rbStimulatorOn.Checked ;
     end;

procedure TDBSPanelFrm.rbActiveModeClick(Sender: TObject);
// -------------------------------
// Set DBS device into active mode
// -------------------------------
begin
     Main.SESLabIO.DBSSleepMode := rbSleepMode.Checked ;
     rbSleepMode.Enabled := False ;
     rbActiveMode.Enabled := False ;
     TSystemPowerEnable := 10 ;
     end;


procedure TDBSPanelFrm.rbSleepModeClick(Sender: TObject);
// -------------------------------
// Set DBS device into sleep mode
// -------------------------------
begin
    Main.SESLabIO.DBSSleepMode := rbSleepMode.Checked ;
     rbSleepMode.Enabled := False ;
     rbActiveMode.Enabled := False ;
     TSystemPowerEnable := 10 ;
    end;


procedure TDBSPanelFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TDBSPanelFrm.edPulseWidthKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        Main.SESLabIO.DBSPulseWidth := edPulseWidth.Value ;
        edPulseWidth.Value := Main.SESLabIO.DBSPulseWidth ;
        end ;

     end;

procedure TDBSPanelFrm.edPulseFrequencyKeyPress(Sender: TObject;
  var Key: Char);
// ----------------------
// Pulse freqency changed
// ----------------------
begin
      if Key = #13 then begin
         Main.SESLabIO.DBSFrequency := edPulseFrequency.Value ;
         edPulseFrequency.Value := Main.SESLabIO.DBSFrequency ;
         end;
      end ;

procedure TDBSPanelFrm.TimerTimer(Sender: TObject);
// -------------------
// Status update timer
// -------------------
var
    s : String ;
begin

     // Re-enabled system power controls after dela
     if not rbActiveMode.Enabled then begin

        if TSystemPowerEnable <= 0 then begin
           rbActiveMode.Enabled := True ;
           rbSleepMode.Enabled := True ;
           end
        else Dec(TSystemPowerEnable) ;
        end ;

     meStatus.Lines.Clear ;
     if not Main.SESLabIO.ADCActive then begin
        s := 'Not available' ;
        meStatus.Lines.Add(s) ;
        Exit ;
        end ;

     s := format('Sampling rate: %.3g Hz',[Main.SESLabIO.DBSSamplingRate]) ;
     meStatus.Lines.Add(s) ;

     s := format('Stimulus: %.3gms pulse at %.3g Hz',
          [Main.SESLabIO.DBSPulseWidth*1000.0,
           Main.SESLabIO.DBSFrequency]);
     if Main.SESLabIO.DBSStimulus then s := s + ' ON'
                                  else s := s + ' OFF' ;
     meStatus.Lines.Add(s) ;

     if Main.SESLabIO.DBSNumFramesLost > 0 then begin
        s := format('No. of data frames lost: %d',[Main.SESLabIO.DBSNumFramesLost]) ;
        meStatus.Lines.Add(s) ;
        end ;

     end ;

end.
