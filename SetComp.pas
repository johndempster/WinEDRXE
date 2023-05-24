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
// 09.01.16 Ratio computation updated. Ion name and units added
//          ratio channel selection rearranged.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, ExtCtrls, ComCtrls, global, ValidatedEdit, math ;

type
  TSetCompFrm = class(TForm)
    PageControl: TPageControl;
    FluorescenceTab: TTabSheet;
    CapacityTab: TTabSheet;
    bOK: TButton;
    bCancel: TButton;
    GroupBox3: TGroupBox;
    edFThreshold: TValidatedEdit;
    Label2: TLabel;
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
    ConcEquationGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edRMax: TValidatedEdit;
    edRMin: TValidatedEdit;
    edKeff: TValidatedEdit;
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
    Label34: TLabel;
    edIonName: TEdit;
    GroupBox1: TGroupBox;
    ConcResultsGrp: TGroupBox;
    Label8: TLabel;
    edRatioDisplayMax: TValidatedEdit;
    GroupBox13: TGroupBox;
    cbRatioChan: TComboBox;
    ckRatioInUse: TCheckBox;
    edConcDisplayMax: TValidatedEdit;
    cbConcChan: TComboBox;
    ckConcInUse: TCheckBox;
    Label4: TLabel;
    Label3: TLabel;
    edIonUnits: TEdit;
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

uses Mdiform, Rec, EDRFileUnit;

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
     for ch := 0 to EDRFile.Settings.NumChannels-1 do
         begin
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
     SetChannel( cbNumerChan, EDRFile.Settings.Fluorescence.NumerChan ) ;
     SetChannel( cbDenomChan, EDRFile.Settings.Fluorescence.DenomChan ) ;
     ckRatioInUse.Checked := SetChannel( cbRatioChan, EDRFile.Settings.Fluorescence.RatioChan ) ;
     ckConcInUse.Checked := SetChannel( cbConcChan, EDRFile.Settings.Fluorescence.ConcChan ) ;

     edRMax.Value := EDRFile.Settings.Fluorescence.RMax ;
     edRMin.Value := EDRFile.Settings.Fluorescence.RMin ;
     edKeff.Value := EDRFile.Settings.Fluorescence.Keff ;
     edFThreshold.Value := EDRFile.Settings.Fluorescence.FThreshold ;
     edRatioDisplayMax.Value := EDRFile.Settings.Fluorescence.RatioDisplayMax ;
     edConcDisplayMax.Value := EDRFile.Settings.Fluorescence.ConcDisplayMax ;
     edIonName.Text := EDRFile.Settings.Fluorescence.IonName ;
     edIonUnits.Text := EDRFile.Settings.Fluorescence.IonUnits ;

     { Set capacity channels and parameters }
     SetChannel( cbGrealChan, EDRFile.Settings.Capacity.GrealChan ) ;
     SetChannel( cbGimagChan, EDRFile.Settings.Capacity.GimagChan ) ;
     SetChannel( cbImChan, EDRFile.Settings.Capacity.ImChan ) ;
     SetChannel( cbVmChan, EDRFile.Settings.Capacity.VmChan ) ;
     ckGsInUse.Checked := SetChannel( cbGsChan, EDRFile.Settings.Capacity.GsChan ) ;
     ckGmInUse.Checked := SetChannel( cbGmChan, EDRFile.Settings.Capacity.GmChan ) ;
     ckCmInUse.Checked := SetChannel( cbCmChan, EDRFile.Settings.Capacity.CmChan ) ;

     edCmDisplayMax.Value := EDRFile.Settings.Capacity.CmDisplayMax ;
     edGmDisplayMax.Value := EDRFile.Settings.Capacity.GmDisplayMax ;
     edGsDisplayMax.Value := EDRFile.Settings.Capacity.GsDisplayMax ;
     edFrequency.Value := EDRFile.Settings.Capacity.Frequency ;
     edVRev.Value := EDRFile.Settings.Capacity.VRev ;
     ckInvertGReal.Checked := EDRFile.Settings.Capacity.InvertGReal ;
     ckInvertGImag.Checked := EDRFile.Settings.Capacity.InvertGImag ;
     ckGChannelsUseGainTelegraph.Checked := EDRFile.Settings.Capacity.GChannelsUseGainTelegraph ;
     ckCapacityCompensationInUse.Checked := EDRFile.Settings.Capacity.CapacityCompensationInUse ;
     edRSeriesComp.Value := EDRFile.Settings.Capacity.RSeriesComp ;
     edCellCapacityComp.Value := EDRFile.Settings.Capacity.CellCapacityComp ;

     // Set event detection parameters
     cbChannel.Items.Clear ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do
        begin
        cbChannel.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        end ;
     cbChannel.ItemIndex := Min( EDRFile.Settings.RTEventAnalysis.Channel,
                                 Main.SESLabIO.ADCNumChannels-1 ) ;

     edDetectionThreshold.Value := EDRFile.Settings.RTEventAnalysis.DetectionThreshold ;
     edDetectionThreshold.Units := Main.SESLabIO.ADCChannelUnits[EDRFile.Settings.RTEventAnalysis.Channel] ;
     edRunningMeanTime.LoLimit := Main.SESLabIO.ADCSamplingInterval ;
     edRunningMeanTime.Value := EDRFile.Settings.RTEventAnalysis.RunningMeanTime ;
     edDeadTime.LoLimit := Main.SESLabIO.ADCSamplingInterval ;
     edDeadTime.Value := EDRFile.Settings.RTEventAnalysis.DeadTime ;
     edEventCountingInterval.Value := EDRFile.Settings.RTEventAnalysis.CountingInterval ;

     // Set resistance parameters
     cbImRes.Items.Clear ;
     cbVmRes.Items.Clear ;
     for ch := 0 to Main.SESLabIO.ADCNumChannels-1 do
        begin
        cbImRes.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        cbVmRes.Items.Add(format('Ch.%d %s',[ch,Main.SESLabIO.ADCChannelName[ch]])) ;
        end ;
     cbImRes.ItemIndex := Min( EDRFile.Settings.RTResistance.ImChannel,Main.SESLabIO.ADCNumChannels-1 ) ;
     cbVmRes.ItemIndex := Min( EDRFile.Settings.RTResistance.VmChannel,Main.SESLabIO.ADCNumChannels-1 ) ;

     edResAmplitude.Value := EDRFile.Settings.RTResistance.Amplitude ;
     edResDuration.Value := EDRFile.Settings.RTResistance.Duration ;
     edResInterval.Value := EDRFile.Settings.RTResistance.Interval ;
     cbResPlot.ItemIndex := Min(Max(EDRFile.Settings.RTResistance.Plot,0),cbResPlot.Items.Count-1) ;

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
     EDRFile.Settings.Fluorescence.NumerChan := GetChannel( cbNumerChan, True ) ;
     EDRFile.Settings.Fluorescence.DenomChan := GetChannel( cbDenomChan, True ) ;
     EDRFile.Settings.Fluorescence.RatioChan :=  GetChannel( cbRatioChan, ckRatioInUse.Checked ) ;
     EDRFile.Settings.Fluorescence.ConcChan := GetChannel( cbConcChan, ckConcInUse.Checked ) ;

     EDRFile.Settings.Fluorescence.RMax := edRMax.Value ;
     EDRFile.Settings.Fluorescence.RMin := edRMin.Value ;
     EDRFile.Settings.Fluorescence.Keff := edKeff.Value ;
     EDRFile.Settings.Fluorescence.FThreshold := edFThreshold.Value ;
     EDRFile.Settings.Fluorescence.RatioDisplayMax := edRatioDisplayMax.Value ;
     EDRFile.Settings.Fluorescence.ConcDisplayMax := edConcDisplayMax.Value ;
     EDRFile.Settings.Fluorescence.IonName := edIonName.Text ;
     EDRFile.Settings.Fluorescence.IonUnits := edIonUnits.Text ;

     { Set capacity channel selections }
     EDRFile.Settings.Capacity.GrealChan := Getchannel( cbGrealChan, True ) ;
     
     EDRFile.Settings.Capacity.GimagChan := Getchannel( cbGimagChan, True  ) ;
     EDRFile.Settings.Capacity.ImChan := Getchannel( cbImChan, True  ) ;
     EDRFile.Settings.Capacity.VmChan := Getchannel( cbVmChan, True  ) ;
     EDRFile.Settings.Capacity.GsChan := Getchannel(cbGsChan,ckGsInUse.Checked) ;
     EDRFile.Settings.Capacity.GmChan := Getchannel( cbGmChan, ckGmInUse.Checked) ;
     EDRFile.Settings.Capacity.CmChan := Getchannel( cbCmChan, ckCmInUse.Checked) ;

     EDRFile.Settings.Capacity.CmDisplayMax := edCmDisplayMax.Value ;
     EDRFile.Settings.Capacity.GmDisplayMax := edGmDisplayMax.Value ;
     EDRFile.Settings.Capacity.GsDisplayMax := edGsDisplayMax.Value ;
     EDRFile.Settings.Capacity.Frequency := edFrequency.Value ;
     EDRFile.Settings.Capacity.VRev := edVRev.Value ;
     EDRFile.Settings.Capacity.InvertGReal := ckInvertGREal.Checked ;
     EDRFile.Settings.Capacity.InvertGImag := ckInvertGImag.Checked ;
     EDRFile.Settings.Capacity.GChannelsUseGainTelegraph := ckGChannelsUseGainTelegraph .Checked  ;
     EDRFile.Settings.Capacity.CapacityCompensationInUse := ckCapacityCompensationInUse.Checked ;
     EDRFile.Settings.Capacity.RSeriesComp := edRSeriesComp.Value ;
     EDRFile.Settings.Capacity.CellCapacityComp := edCellCapacityComp.Value ;

     // Set event detection parameters
     EDRFile.Settings.RTEventAnalysis.Channel := cbChannel.ItemIndex ;
     EDRFile.Settings.RTEventAnalysis.DetectionThreshold := edDetectionThreshold.Value ;
     EDRFile.Settings.RTEventAnalysis.RunningMeanTime := edRunningMeanTime.Value ;
     EDRFile.Settings.RTEventAnalysis.DeadTime := edDeadTime.Value ;
     EDRFile.Settings.RTEventAnalysis.CountingInterval := edEventCountingInterval.Value ;

     // Set resistance parameters
     EDRFile.Settings.RTResistance.ImChannel := cbImRes.ItemIndex ;
     EDRFile.Settings.RTResistance.VmChannel := cbVmRes.ItemIndex ;
     EDRFile.Settings.RTResistance.Amplitude := edResAmplitude.Value ;
     EDRFile.Settings.RTResistance.Duration := edResDuration.Value ;
     EDRFile.Settings.RTResistance.Interval := edResInterval.Value ;
     EDRFile.Settings.RTResistance.Plot := cbResPlot.ItemIndex ;

     EDRFile.Settings.NewCalculation := True ;

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
