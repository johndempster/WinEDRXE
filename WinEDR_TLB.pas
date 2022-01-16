unit WinEDR_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 16/01/2022 15:46:38 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Users\John\Documents\Embarcadero\Studio\Projects\WinEDRXE\WinEDR (1)
// LIBID: {1D3AA672-406C-4763-9645-84DE3DF7F1CA}
// LCID: 0
// Helpfile:
// HelpString: WinEDR Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WinEDRMajorVersion = 1;
  WinEDRMinorVersion = 0;

  LIBID_WinEDR: TGUID = '{1D3AA672-406C-4763-9645-84DE3DF7F1CA}';

  IID_IAUTO: TGUID = '{8ECE1EA2-3347-42A5-8119-3DF9B5448D55}';
  CLASS_AUTO: TGUID = '{D38DAF47-A6EC-4A66-8719-F0DB49A85CA3}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IAUTO = interface;
  IAUTODisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  AUTO = IAUTO;


// *********************************************************************//
// Interface: IAUTO
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8ECE1EA2-3347-42A5-8119-3DF9B5448D55}
// *********************************************************************//
  IAUTO = interface(IDispatch)
    ['{8ECE1EA2-3347-42A5-8119-3DF9B5448D55}']
    procedure NewFile(FileName: OleVariant); safecall;
    procedure CloseFile; safecall;
    procedure OpenFile(FileName: OleVariant); safecall;
    procedure StartRecording; safecall;
    procedure StopRecording; safecall;
    function Get_RecordDuration: OleVariant; safecall;
    procedure Set_RecordDuration(Value: OleVariant); safecall;
    function Get_TriggerMode: OleVariant; safecall;
    procedure Set_TriggerMode(Value: OleVariant); safecall;
    function Get_StimulusProtocol: OleVariant; safecall;
    procedure Set_StimulusProtocol(Value: OleVariant); safecall;
    procedure StartStimulus; safecall;
    procedure StopStimulus; safecall;
    function Get_HoldingVoltage: OleVariant; safecall;
    procedure Set_HoldingVoltage(Value: OleVariant); safecall;
    function Get_DACChannel: OleVariant; safecall;
    procedure Set_DACChannel(Value: OleVariant); safecall;
    function Get_NumTriggerSweeps: OleVariant; safecall;
    procedure Set_NumTriggerSweeps(Value: OleVariant); safecall;
    procedure StartSealTest; safecall;
    procedure StopSealTest; safecall;
    function Get_Cm: OleVariant; safecall;
    procedure Set_Cm(Value: OleVariant); safecall;
    function Get_Gm: OleVariant; safecall;
    procedure Set_Gm(Value: OleVariant); safecall;
    function Get_Ga: OleVariant; safecall;
    procedure Set_Ga(Value: OleVariant); safecall;
    function Get_RSeal: OleVariant; safecall;
    procedure Set_RSeal(Value: OleVariant); safecall;
    function Get_SealTestPulseAmplitude: OleVariant; safecall;
    procedure Set_SealTestPulseAmplitude(Value: OleVariant); safecall;
    function Get_SealTestPulseDuration: OleVariant; safecall;
    procedure Set_SealTestPulseDuration(Value: OleVariant); safecall;
    function Get_Status: OleVariant; safecall;
    procedure Set_Status(Value: OleVariant); safecall;
    function Get_PicoConfig: Integer; safecall;
    procedure Set_PicoConfig(Value: Integer); safecall;
    function Get_PicoEnableCFast: Integer; safecall;
    procedure Set_PicoEnableCFast(Value: Integer); safecall;
    function Get_PicoEnableCSlow: Integer; safecall;
    procedure Set_PicoEnableCSlow(Value: Integer); safecall;
    function Get_PicoEnableJP: Integer; safecall;
    procedure Set_PicoEnableJP(Value: Integer); safecall;
    function Get_PicoFilter: Integer; safecall;
    procedure Set_PicoFilter(Value: Integer); safecall;
    function Get_PicoGain: Integer; safecall;
    procedure Set_PicoGain(Value: Integer); safecall;
    function Get_PicoInput: Integer; safecall;
    procedure Set_PicoInput(Value: Integer); safecall;
    procedure PicoAutoCompCFast; safecall;
    procedure PicoAutoCompCSlow; safecall;
    procedure PicoAutoCompJP; safecall;
    function Get_PicoCFastComp: OleVariant; safecall;
    procedure Set_PicoCFastComp(Value: OleVariant); safecall;
    function Get_PicoCSlowComp: OleVariant; safecall;
    procedure Set_PicoCSlowComp(Value: OleVariant); safecall;
    function Get_PicoJPComp: OleVariant; safecall;
    procedure Set_PicoJPComp(Value: OleVariant); safecall;
    procedure PicoClearCompC; safecall;
    procedure PicoClearCompJP; safecall;
    function Get_SealTestNumAverages: OleVariant; safecall;
    procedure Set_SealTestNumAverages(Value: OleVariant); safecall;
    function Get_SealTestGaFromPeak: Integer; safecall;
    procedure Set_SealTestGaFromPeak(Value: Integer); safecall;
    property RecordDuration: OleVariant read Get_RecordDuration write Set_RecordDuration;
    property TriggerMode: OleVariant read Get_TriggerMode write Set_TriggerMode;
    property StimulusProtocol: OleVariant read Get_StimulusProtocol write Set_StimulusProtocol;
    property HoldingVoltage: OleVariant read Get_HoldingVoltage write Set_HoldingVoltage;
    property DACChannel: OleVariant read Get_DACChannel write Set_DACChannel;
    property NumTriggerSweeps: OleVariant read Get_NumTriggerSweeps write Set_NumTriggerSweeps;
    property Cm: OleVariant read Get_Cm write Set_Cm;
    property Gm: OleVariant read Get_Gm write Set_Gm;
    property Ga: OleVariant read Get_Ga write Set_Ga;
    property RSeal: OleVariant read Get_RSeal write Set_RSeal;
    property SealTestPulseAmplitude: OleVariant read Get_SealTestPulseAmplitude write Set_SealTestPulseAmplitude;
    property SealTestPulseDuration: OleVariant read Get_SealTestPulseDuration write Set_SealTestPulseDuration;
    property Status: OleVariant read Get_Status write Set_Status;
    property PicoConfig: Integer read Get_PicoConfig write Set_PicoConfig;
    property PicoEnableCFast: Integer read Get_PicoEnableCFast write Set_PicoEnableCFast;
    property PicoEnableCSlow: Integer read Get_PicoEnableCSlow write Set_PicoEnableCSlow;
    property PicoEnableJP: Integer read Get_PicoEnableJP write Set_PicoEnableJP;
    property PicoFilter: Integer read Get_PicoFilter write Set_PicoFilter;
    property PicoGain: Integer read Get_PicoGain write Set_PicoGain;
    property PicoInput: Integer read Get_PicoInput write Set_PicoInput;
    property PicoCFastComp: OleVariant read Get_PicoCFastComp write Set_PicoCFastComp;
    property PicoCSlowComp: OleVariant read Get_PicoCSlowComp write Set_PicoCSlowComp;
    property PicoJPComp: OleVariant read Get_PicoJPComp write Set_PicoJPComp;
    property SealTestNumAverages: OleVariant read Get_SealTestNumAverages write Set_SealTestNumAverages;
    property SealTestGaFromPeak: Integer read Get_SealTestGaFromPeak write Set_SealTestGaFromPeak;
  end;

// *********************************************************************//
// DispIntf:  IAUTODisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8ECE1EA2-3347-42A5-8119-3DF9B5448D55}
// *********************************************************************//
  IAUTODisp = dispinterface
    ['{8ECE1EA2-3347-42A5-8119-3DF9B5448D55}']
    procedure NewFile(FileName: OleVariant); dispid 201;
    procedure CloseFile; dispid 202;
    procedure OpenFile(FileName: OleVariant); dispid 203;
    procedure StartRecording; dispid 204;
    procedure StopRecording; dispid 205;
    property RecordDuration: OleVariant dispid 206;
    property TriggerMode: OleVariant dispid 207;
    property StimulusProtocol: OleVariant dispid 208;
    procedure StartStimulus; dispid 209;
    procedure StopStimulus; dispid 210;
    property HoldingVoltage: OleVariant dispid 211;
    property DACChannel: OleVariant dispid 212;
    property NumTriggerSweeps: OleVariant dispid 213;
    procedure StartSealTest; dispid 214;
    procedure StopSealTest; dispid 215;
    property Cm: OleVariant dispid 216;
    property Gm: OleVariant dispid 217;
    property Ga: OleVariant dispid 218;
    property RSeal: OleVariant dispid 219;
    property SealTestPulseAmplitude: OleVariant dispid 220;
    property SealTestPulseDuration: OleVariant dispid 221;
    property Status: OleVariant dispid 222;
    property PicoConfig: Integer dispid 223;
    property PicoEnableCFast: Integer dispid 224;
    property PicoEnableCSlow: Integer dispid 225;
    property PicoEnableJP: Integer dispid 226;
    property PicoFilter: Integer dispid 227;
    property PicoGain: Integer dispid 228;
    property PicoInput: Integer dispid 229;
    procedure PicoAutoCompCFast; dispid 230;
    procedure PicoAutoCompCSlow; dispid 231;
    procedure PicoAutoCompJP; dispid 232;
    property PicoCFastComp: OleVariant dispid 233;
    property PicoCSlowComp: OleVariant dispid 234;
    property PicoJPComp: OleVariant dispid 235;
    procedure PicoClearCompC; dispid 236;
    procedure PicoClearCompJP; dispid 237;
    property SealTestNumAverages: OleVariant dispid 238;
    property SealTestGaFromPeak: Integer dispid 239;
  end;

// *********************************************************************//
// The Class CoAUTO provides a Create and CreateRemote method to
// create instances of the default interface IAUTO exposed by
// the CoClass AUTO. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoAUTO = class
    class function Create: IAUTO;
    class function CreateRemote(const MachineName: string): IAUTO;
  end;

implementation

uses System.Win.ComObj;

class function CoAUTO.Create: IAUTO;
begin
  Result := CreateComObject(CLASS_AUTO) as IAUTO;
end;

class function CoAUTO.CreateRemote(const MachineName: string): IAUTO;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AUTO) as IAUTO;
end;

end.

