unit SetComp;
// ------------------------------------------------
// Set Fluorescence/Capacity computation dialog box
// ------------------------------------------------
// 30.07.06 Now reads channel names from RecChannel
// 11.09.06 Now gets no. of channels from Settings.NumChannels
// 19.03.08 Event frequency calculation added
// 22.12.08 Real-time resistance calculation added
// 24.02.09 Option to invert conductance channels in capacity calculation added
// 27.05.09 ckGChannelsUseGainTelegraph added

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, ExtCtrls, ComCtrls, global, ValidatedEdit, math ;

type
  TSetCompFrm = class(TForm)
    PageControl: TPageControl;
    FluorescenceTab: TTabSheet;
    CapacityTab: TTabSheet;
    GroupBox1: TGroupBox;
    bOK: TButton;
    bCancel: TButton;
    GroupBox3: TGroupBox;
    edFThreshold: TValidatedEdit;
    Label2: TLabel;
    ConcResultsGrp: TGroupBox;
    edConcDisplayMax: TValidatedEdit;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    cbGImagChan: TComboBox;
    cbGRealChan: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    cbImChan: TComboBox;
    Label12: TLabel;
    cbVmChan: TComboBox;
    Label13: TLabel;
    GroupBox5: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    cbGsChan: TComboBox;
    cbGmChan: TComboBox;
    cbCmChan: TComboBox;
    GroupBox6: TGroupBox;
    edCmDisplayMax: TValidatedEdit;
    Label18: TLabel;
    edGmDisplayMax: TValidatedEdit;
    Label17: TLabel;
    GroupBox7: TGroupBox;
    edFrequency: TValidatedEdit;
    Label19: TLabel;
    edVRev: TValidatedEdit;
    Label20: TLabel;
    ckGmInUse: TCheckBox;
    ckGsInUse: TCheckBox;
    ckCmInUse: TCheckBox;
    cbConcChan: TComboBox;
    Label9: TLabel;
    edRatioDisplayMax: TValidatedEdit;
    Label8: TLabel;
    cbRatioChan: TComboBox;
    Label3: TLabel;
    ConcEquationGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edRMax: TValidatedEdit;
    edRMin: TValidatedEdit;
    edKeff: TValidatedEdit;
    ckRatioInUse: TCheckBox;
    ckConcInUse: TCheckBox;
    Label1: TLabel;
    cbNumerChan: TComboBox;
    cbDenomChan: TComboBox;
    Shape1: TShape;
    edGsDisplayMax: TValidatedEdit;
    Label21: TLabel;
    EventAnalysisTab: TTabSheet;
    GroupBox4: TGroupBox;
    Label22: TLabel;
    Label23: TLabel;
    edDetectionThreshold: TValidatedEdit;
    Label24: TLabel;
    edRunningMeanTime: TValidatedEdit;
    Label25: TLabel;
    edDeadTime: TValidatedEdit;
    cbChannel: TComboBox;
    ResistanceTab: TTabSheet;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Label26: TLabel;
    cbImRes: TComboBox;
    Label30: TLabel;
    cbVmRes: TComboBox;
    GroupBox10: TGroupBox;
    edResAmplitude: TValidatedEdit;
    edResDuration: TValidatedEdit;
    edResInterval: TValidatedEdit;
    Label27: TLabel;
    Label31: TLabel;
    Label28: TLabel;
    GroupBox11: TGroupBox;
    cbResPlot: TComboBox;
    ckInvertGReal: TCheckBox;
    ckGChannelsUseGainTelegraph: TCheckBox;
    ckInvertGImag: TCheckBox;
    GroupBox12: TGroupBox;
    Label29: TLabel;
    edRSeriesComp: TValidatedEdit;
    Label32: TLabel;
    edCellCapacityComp: TValidatedEdit;
    ckCapacityCompensationInUse: TCheckBox;
    Label33: TLabel;
    edEventCountingInterval: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    function SetChannel(ComboBox : TComboBox ; Chan : Integer) : Boolean ;
    function GetChannel(ComboBox : TComboBox ; InUse : Boolean) : Integer ;
    function GetResistanceStimulusUnits : String ;
    procedure SetResistanceStimulusUnits( Units : String ) ;
  public
    { Public declarations }
    procedure SelectPage( PageNum : Integer ) ;
    property ResistanceStimulusUnits : String
             read GetResistanceStimulusUnits write SetResistanceStimulusUnits ;

  end;

var
  SetCompFrm: TSetCompFrm;

implementation

uses Mdiform, Rec;

{$R *.DFM}

procedure TSetCompFrm.FormShow(Sender: TObject);
{ --------------------------------------------------
  Initialise control settings when form is displayed
  -------------------------------------------------- }
var
   ch : Integer ;
   Item : string ;
begin

     { Create channel lists }
     cbNumerChan.Clear ;
     cbDenomChan.Clear ;
     cbRatioChan.Clear ;
     cbConcChan.Clear ;
     cbGrealChan.Clear ;
     cbGimagChan.Clear ;
     cbImChan.Clear ;
     cbVmChan.Clear ;
     cbGsChan.Clear ;
     cbGmChan.Clear ;
     cbCmChan.Clear ;
     for ch := 0 to Settings.NumChannels-1 do begin
         Item := format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]]) ;
         cbNumerChan.items.add( Item ) ;
         cbDenomChan.items.add( Item ) ;
         cbRatioChan.items.add( Item ) ;
         cbConcChan.items.add( Item ) ;
         cbGrealChan.items.add( Item ) ;
         cbGimagChan.items.add( Item ) ;
         cbImChan.items.add( Item ) ;
         cbVmChan.items.add( Item ) ;
         cbGsChan.items.add( Item ) ;
         cbGmChan.items.add( Item ) ;
         cbCmChan.items.add( Item ) ;
         end ;

     { Set Fluorescence channel and parameters }
     SetChannel( cbNumerChan, Settings.Fluorescence.NumerChan ) ;
     SetChannel( cbDenomChan, Settings.Fluorescence.DenomChan ) ;
     ckRatioInUse.Checked := SetChannel( cbRatioChan, Settings.Fluorescence.RatioChan ) ;
     ckConcInUse.Checked := SetChannel( cbConcChan, Settings.Fluorescence.ConcChan ) ;

     edRMax.Value := Settings.Fluorescence.RMax ;
     edRMin.Value := Settings.Fluorescence.RMin ;
     edKeff.Value := Settings.Fluorescence.Keff ;
     edFThreshold.Value := Settings.Fluorescence.FThreshold ;
     edRatioDisplayMax.Value := Settings.Fluorescence.RatioDisplayMax ;
     edConcDisplayMax.Value := Settings.Fluorescence.ConcDisplayMax ;

     { Set capacity channels and parameters }
     SetChannel( cbGrealChan, Settings.Capacity.GrealChan ) ;
     SetChannel( cbGimagChan, Settings.Capacity.GimagChan ) ;
     SetChannel( cbImChan, Settings.Capacity.ImChan ) ;
     SetChannel( cbVmChan, Settings.Capacity.VmChan ) ;
     ckGsInUse.Checked := SetChannel( cbGsChan, Settings.Capacity.GsChan ) ;
     ckGmInUse.Checked := SetChannel( cbGmChan, Settings.Capacity.GmChan ) ;
     ckCmInUse.Checked := SetChannel( cbCmChan, Settings.Capacity.CmChan ) ;

     edCmDisplayMax.Value := Settings.Capacity.CmDisplayMax ;
     edGmDisplayMax.Value := Settings.Capacity.GmDisplayMax ;
     edGsDisplayMax.Value := Settings.Capacity.GsDisplayMax ;
     edFrequency.Value := Settings.Capacity.Frequency ;
     edVRev.Value := Settings.Capacity.VRev ;
     ckInvertGReal.Checked := Settings.Capacity.InvertGReal ;
     ckInvertGImag.Checked := Settings.Capacity.InvertGImag ;
     ckGChannelsUseGainTelegraph.Checked := Settings.Capacity.GChannelsUseGainTelegraph ;
     ckCapacityCompensationInUse.Checked := Settings.Capacity.CapacityCompensationInUse ;
     edRSeriesComp.Value := Settings.Capacity.RSeriesComp ;
     edCellCapacityComp.Value := Settings.Capacity.CellCapacityComp ;


     // Set event detection parameters
     cbChannel.Items.Clear ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
        cbChannel.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        end ;
     cbChannel.ItemIndex := Min( Settings.RTEventAnalysis.Channel,
                                 Main.SESLabIO.ADCNumChannels-1 ) ;

     edDetectionThreshold.Value := Settings.RTEventAnalysis.DetectionThreshold ;
     edDetectionThreshold.Units := Main.SESLabIO.ADCChannelUnits[Settings.RTEventAnalysis.Channel] ;
     edRunningMeanTime.LoLimit := Main.SESLabIO.ADCSamplingInterval ;
     edRunningMeanTime.Value := Settings.RTEventAnalysis.RunningMeanTime ;
     edDeadTime.LoLimit := Main.SESLabIO.ADCSamplingInterval ;
     edDeadTime.Value := Settings.RTEventAnalysis.DeadTime ;
     edEventCountingInterval.Value := Settings.RTEventAnalysis.CountingInterval ;

     // Set resistance parameters
     cbImRes.Items.Clear ;
     cbVmRes.Items.Clear ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do begin
        cbImRes.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        cbVmRes.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        end ;
     cbImRes.ItemIndex := Min( Settings.RTResistance.ImChannel,
                               Main.SESLabIO.ADCNumChannels-1 ) ;
     cbVmRes.ItemIndex := Min( Settings.RTResistance.VmChannel,
                               Main.SESLabIO.ADCNumChannels-1 ) ;

     edResAmplitude.Value := Settings.RTResistance.Amplitude ;
     edResDuration.Value := Settings.RTResistance.Duration ;
     edResInterval.Value := Settings.RTResistance.Interval ;
     cbResPlot.ItemIndex := Min(Max(Settings.RTResistance.Plot,0),cbResPlot.Items.Count-1) ;


     end;

procedure TSetCompFrm.SelectPage( PageNum : Integer ) ;
// -----------------------
// Select tab page to show
// -----------------------
begin

     // Select page in use
     case PageNum of
          0 : PageControl.ActivePage := FluorescenceTab ;
          1 : PageControl.ActivePage := CapacityTab ;
          2 : PageControl.ActivePage := EventAnalysisTab ;
          3 : PageControl.ActivePage := ResistanceTab ;
          end ;

     end ;


function TSetCompFrm.SetChannel(
         ComboBox : TComboBox ;
         Chan : Integer
         ) : Boolean ;
{ -----------------------------------------
  Set combo and check box state for channel
  ----------------------------------------- }
begin
     if Chan >= 0 then begin
        ComboBox.ItemIndex := Chan ;
        Result := True ;
        end
     else begin
        ComboBox.ItemIndex := -1 ;
        Result := False ;
        end ;
     end ;


function TSetCompFrm.GetChannel(
         ComboBox : TComboBox ;
         InUse : Boolean
         ) : Integer ;
{ -----------------------------------------
  Set combo and check box state for channel
  ----------------------------------------- }
begin
     if InUse then begin
        Result := ComboBox.ItemIndex ;
        end
     else begin
        Result := -1 ;
        end ;
     end ;



procedure TSetCompFrm.bOKClick(Sender: TObject);
{ -------------------------------------
  Update fluorescence settings and exit
  ------------------------------------- }
begin
     { Set fluorescence channel selections }
     Settings.Fluorescence.NumerChan := GetChannel( cbNumerChan, True ) ;
     Settings.Fluorescence.DenomChan := GetChannel( cbDenomChan, True ) ;
     Settings.Fluorescence.RatioChan :=  GetChannel( cbRatioChan, ckRatioInUse.Checked ) ;
     Settings.Fluorescence.ConcChan := GetChannel( cbConcChan, ckConcInUse.Checked ) ;

     Settings.Fluorescence.RMax := edRMax.Value ;
     Settings.Fluorescence.RMin := edRMin.Value ;
     Settings.Fluorescence.Keff := edKeff.Value ;
     Settings.Fluorescence.FThreshold := edFThreshold.Value ;
     Settings.Fluorescence.RatioDisplayMax := edRatioDisplayMax.Value ;
     Settings.Fluorescence.ConcDisplayMax := edConcDisplayMax.Value ;

     { Set capacity channel selections }
     Settings.Capacity.GrealChan := Getchannel( cbGrealChan, True ) ;
     
     Settings.Capacity.GimagChan := Getchannel( cbGimagChan, True  ) ;
     Settings.Capacity.ImChan := Getchannel( cbImChan, True  ) ;
     Settings.Capacity.VmChan := Getchannel( cbVmChan, True  ) ;
     Settings.Capacity.GsChan := Getchannel(cbGsChan,ckGsInUse.Checked) ;
     Settings.Capacity.GmChan := Getchannel( cbGmChan, ckGmInUse.Checked) ;
     Settings.Capacity.CmChan := Getchannel( cbCmChan, ckCmInUse.Checked) ;

     Settings.Capacity.CmDisplayMax := edCmDisplayMax.Value ;
     Settings.Capacity.GmDisplayMax := edGmDisplayMax.Value ;
     Settings.Capacity.GsDisplayMax := edGsDisplayMax.Value ;
     Settings.Capacity.Frequency := edFrequency.Value ;
     Settings.Capacity.VRev := edVRev.Value ;
     Settings.Capacity.InvertGReal := ckInvertGREal.Checked ;
     Settings.Capacity.InvertGImag := ckInvertGImag.Checked ;
     Settings.Capacity.GChannelsUseGainTelegraph := ckGChannelsUseGainTelegraph .Checked  ;
     Settings.Capacity.CapacityCompensationInUse := ckCapacityCompensationInUse.Checked ;
     Settings.Capacity.RSeriesComp := edRSeriesComp.Value ;
     Settings.Capacity.CellCapacityComp := edCellCapacityComp.Value ;

     // Set event detection parameters
     Settings.RTEventAnalysis.Channel := cbChannel.ItemIndex ;
     Settings.RTEventAnalysis.DetectionThreshold := edDetectionThreshold.Value ;
     Settings.RTEventAnalysis.RunningMeanTime := edRunningMeanTime.Value ;
     Settings.RTEventAnalysis.DeadTime := edDeadTime.Value ;
     Settings.RTEventAnalysis.CountingInterval := edEventCountingInterval.Value ;

     // Set resistance parameters
     Settings.RTResistance.ImChannel := cbImRes.ItemIndex ;
     Settings.RTResistance.VmChannel := cbVmRes.ItemIndex ;
     Settings.RTResistance.Amplitude := edResAmplitude.Value ;
     Settings.RTResistance.Duration := edResDuration.Value ;
     Settings.RTResistance.Interval := edResInterval.Value ;
     Settings.RTResistance.Plot := cbResPlot.ItemIndex ;

     Settings.NewCalculation := True ;

     Close ;

     RecordFrm.RestartADC

     end;


procedure TSetCompFrm.bCancelClick(Sender: TObject);
begin
     Close ;
     end;

procedure TSetCompFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree ;
     end;

function TSetCompFrm.GetResistanceStimulusUnits : String ;
begin
    Result := edResAmplitude.Units ;
    end ;

procedure TSetCompFrm.SetResistanceStimulusUnits( Units : String ) ;
begin
    edResAmplitude.Units := Units ;
    end ;

end.
