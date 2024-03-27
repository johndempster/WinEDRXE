unit LabInterfaceSetup;
// -------------------------------------
// Laboratory Interface Setup Dialog Box
// -------------------------------------
// 15.06.11 New form for laboratory interface setup only
//          (previously handled in setupdlg form)
// 14.03.24 Form position saved to INI file

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, global, math, seslabio ;

type
  TLabInterfaceSetupFrm = class(TForm)
    cbLabInterface: TComboBox;
    NIPanel: TPanel;
    Label3: TLabel;
    Label13: TLabel;
    cbADCInputMode: TComboBox;
    cbDeviceNumber: TComboBox;
    Panel1: TPanel;
    edModel: TEdit;
    Label5: TLabel;
    cbADCVoltageRange: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbLabInterfaceChange(Sender: TObject);
    procedure cbDeviceNumberChange(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure cbADCInputModeChange(Sender: TObject);
  private
    { Private declarations }
    procedure FillOptionsLists ;
  public
    { Public declarations }
  end;

var
  LabInterfaceSetupFrm: TLabInterfaceSetupFrm;

implementation

uses MDIForm, AmpModule, EDRFileUnit;

{$R *.dfm}

procedure TLabInterfaceSetupFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
begin
     ClientWidth := cbLabInterface.Left + cbLabInterface.Width + 5 ;
     ClientHeight := bOK.Top + bOK.Height + 5 ;

     { Stop laboratory interface activity }
     if Main.SESLabIO.ADCActive then Main.SESLabIO.ADCStop ;
     if Main.SESLabIO.DACActive then Main.SESLabIO.DACStop ;

     FillOptionsLists ;

     end;

procedure TLabInterfaceSetupFrm.bOKClick(Sender: TObject);
begin
    { Update file header block with new settings }

    // A/D channel input mode
    Main.SESLABIO.ADCInputMode := cbADCInputMode.ItemIndex ;

    Main.SESLabIO.ADCVoltageRangeIndex := cbADCVoltageRange.ItemIndex ;

    Close ;

    end;


procedure TLabInterfaceSetupFrm.FillOptionsLists ;
// -----------------------------------
// Re-open lab. interface after change
// -----------------------------------
var
   i,iKeep : Integer ;
begin

     // Interface types
     Main.SESLABIO.GetLabInterfaceTypes( cbLabInterface.Items ) ;
     cbLabInterface.ItemIndex := cbLabInterface.Items.IndexofObject(TObject(Main.SESLABIO.LabInterfaceType)) ;

     // A/D channel input mode
     Main.SESLABIO.GetADCInputModes( cbADCInputMode.Items ) ;
     cbADCInputMode.ItemIndex := Min(Main.SESLABIO.ADCInputMode,cbADCInputMode.Items.Count-1) ;

     // Device list
     Main.SESLABIO.GetDeviceList( cbDeviceNumber.Items ) ;
     cbDeviceNumber.ItemIndex := Min(Main.SESLABIO.DeviceNumber-1,cbADCInputMode.Items.Count-1) ;

     if cbADCInputMode.Items.Count > 1 then begin
        NIPanel.Visible := True ;
        Panel1.top := NIPanel.Top + NIPanel.Height + 5
        end
     else begin
        NIPanel.Visible := False ;
        Panel1.top := NIPanel.Top ;
        end ;
     ClientHeight := Panel1.Top + Panel1.Height + 2 ;

     iKeep := Main.SESLabIO.ADCVoltageRangeIndex ;
     cbADCVoltageRange.clear ;
     for i := 0 to Main.SESLabIO.ADCNumVoltageRanges-1 do begin
         Main.SESLabIO.ADCVoltageRangeIndex := i ;
         cbADCVoltageRange.items.add(
           format(' +/-%.3g V ',[Main.SESLabIO.ADCVoltageRange] )) ;
         end ;
     Main.SESLabIO.ADCVoltageRangeIndex := iKeep ;
     cbADCVoltageRange.ItemIndex := Main.SESLabIO.ADCVoltageRangeIndex ;

    // Initialise channel display settings to minimum magnification
    Main.mnZoomOutAll.Click ;

     // If using VP500 as interface, amplifier must also be VP500
     if Main.SESLabIO.LabInterfaceType = vp500 then begin
        Amplifier.AmplifierType[0] := amVP500 ;
        Amplifier.AmplifierType[1] := amNone ;
        Amplifier.AmplifierType[2] := amNone ;
        Amplifier.AmplifierType[3] := amNone ;
        end ;

     // If using VP500 as interface, amplifier must also be VP500
     if Main.SESLabIO.LabInterfaceType = Triton then begin
        Amplifier.AmplifierType[0] := amTriton ;
        Amplifier.AmplifierType[1] := amNone ;
        Amplifier.AmplifierType[2] := amNone ;
        Amplifier.AmplifierType[3] := amNone ;
        end
     else begin
        // If not a Triton interface remove Triton from amplifiers list
        if Amplifier.AmplifierType[0] = amTriton then Amplifier.AmplifierType[0] := amNone ;
        end ;

     edModel.Text := Main.SESLabIO.LabInterfaceModel ;

     end;




procedure TLabInterfaceSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ------------------------------
// Procedures when form is closed
// ------------------------------
begin

    // Save form position to INI file
    EDRFile.SaveFormPosition( Self ) ;

    Action := caFree ;

    end;


procedure TLabInterfaceSetupFrm.cbLabInterfaceChange(Sender: TObject);
// ----------------------
// Lab. interface changed
// ----------------------
begin
     Screen.Cursor := crHourGlass ;
     Main.SESLabIO.LabInterfaceType := Integer(cbLabInterface.Items.Objects[cbLabInterface.ItemIndex]);
     FillOptionsLists ;
     Screen.Cursor := crDefault ;
     end;


procedure TLabInterfaceSetupFrm.cbDeviceNumberChange(Sender: TObject);
// ----------------------
// Device # changed
// ----------------------
begin
     Screen.Cursor := crHourGlass ;
     Main.SESLabIO.DeviceNumber := cbDeviceNumber.ItemIndex + 1 ;
     FillOptionsLists ;
     Screen.Cursor := crDefault ;
     end;

procedure TLabInterfaceSetupFrm.bCancelClick(Sender: TObject);
begin
     Close ;
     
     end;

procedure TLabInterfaceSetupFrm.cbADCInputModeChange(Sender: TObject);
// ----------------------
// A/D input mode changed
// ----------------------
begin
     Screen.Cursor := crHourGlass ;
     Main.SESLABIO.ADCInputMode := cbADCInputMode.ItemIndex ;
     FillOptionsLists ;
     Screen.Cursor := crDefault ;
     end;

end.
