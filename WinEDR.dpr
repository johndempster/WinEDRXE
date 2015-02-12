program WinEDR;

uses
  Forms,
  WinEDR_TLB in 'WinEDR_TLB.pas',
  AutoUnit in 'AutoUnit.pas' {AUTO: CoClass},
  MDIFORM in 'MDIFORM.PAS' {Main},
  Maths in '..\Components\Maths.pas',
  ZERO in 'ZERO.PAS' {ZeroFrm},
  SETAXES in 'SETAXES.PAS' {SetAxesFrm},
  SETVAR in 'SETVAR.PAS' {SetVarFrm},
  PRINTREC in 'PRINTREC.PAS' {PrintRecFrm},
  SETPAGE in 'SETPAGE.PAS' {SetPageFrm},
  MEPCFREQ in 'MEPCFREQ.PAS' {MEPCFreqFrm},
  SETBLOCK in 'SETBLOCK.PAS' {SetBlockFrm},
  Printgra in 'Printgra.pas' {PrintGraphFrm},
  SetIgnor in 'SetIgnor.pas' {SetIgnoreFrm},
  SETFITPA in 'SETFITPA.PAS' {SetFitParsFrm},
  SetComp in 'SetComp.pas' {SetCompFrm},
  Digfilt in 'Digfilt.pas' {DigFilterDlg},
  export in 'export.pas' {ExportFrm},
  Invert in 'Invert.pas' {InvertDlg},
  ftest in 'ftest.pas' {FTestFrm},
  PrintPageView in 'PrintPageView.pas' {PrintPageViewFrm},
  EditMarkers in 'EditMarkers.pas' {EditMarkersFrm},
  ImportASCIIUnit in 'ImportASCIIUnit.pas' {ImportASCIIFrm},
  ImportRawUnit in 'ImportRawUnit.pas' {ImportRawFrm},
  SetFitPars1Unit in 'SetFitPars1Unit.pas' {SetFitPars1frm},
  StimModule in 'StimModule.pas' {Stimulator: TDataModule},
  REC in 'REC.PAS' {RecordFrm},
  Sealtest in 'Sealtest.pas' {SealTestFrm},
  About in 'About.pas' {AboutDlg},
  LOG in 'LOG.PAS' {LogFrm},
  DEFSET in 'DEFSET.PAS' {DefSetFrm},
  ViewSig in 'ViewSig.pas' {ViewSigFrm},
  Simmepc in 'Simmepc.pas' {SimMEPCFrm},
  CDRZERO in 'CDRZERO.PAS' {CDRZeroFrm},
  NoiseAnal in 'NoiseAnal.pas' {NoiseAnalFrm},
  SIMNOISE in 'SIMNOISE.PAS' {SimNoiseFrm},
  Simchan in 'Simchan.pas' {SimChanFrm},
  SingleChanAnal in 'SingleChanAnal.pas' {SingleChanAnalFrm},
  Ced1902u in 'Ced1902u.pas' {CED1902Frm},
  EventDetector in 'EventDetector.pas' {EventDetFrm},
  PageView in 'PageView.pas' {PageViewFrm},
  ECG in 'ECG.pas' {ECGFrm},
  VP500Panel in 'VP500Panel.pas' {VP500PanelFrm},
  FilePropsUnit in 'FilePropsUnit.pas' {FilePropsDlg},
  Global in 'Global.pas',
  RecPlotUnit in 'RecPlotUnit.pas' {RecPlotFrm},
  Fileio in 'Fileio.pas',
  SHARED in 'SHARED.PAS',
  DBSPanelUnit in 'DBSPanelUnit.pas' {DBSPanelFrm},
  TritonPanelUnit in 'TritonPanelUnit.pas' {TritonPanelFrm},
  LabInterfaceSetup in 'LabInterfaceSetup.pas' {LabInterfaceSetupFrm},
  InputChannelSetup in 'InputChannelSetup.pas' {InputChannelSetupFrm},
  AmpModule in 'AmpModule.pas' {Amplifier: TDataModule},
  EditProtocolUnit in 'EditProtocolUnit.pas' {EditProtocolFrm},
  HTMLHelpViewer in '..\Components\HTMLHelpViewer.pas',
  DirectorySelectUnit in 'DirectorySelectUnit.pas' {DirectorySelectFrm},
  EPC9PanelUnit in 'EPC9PanelUnit.pas' {EPC9PanelFrm},
  MATFileWriterUnit in 'MATFileWriterUnit.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'winedr.chm';
  Application.Title := 'WinEDR - Electrophysiology Disk Recorder';
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TZeroFrm, ZeroFrm);
  Application.CreateForm(TSetAxesFrm, SetAxesFrm);
  Application.CreateForm(TSetVarFrm, SetVarFrm);
  Application.CreateForm(TPrintRecFrm, PrintRecFrm);
  Application.CreateForm(TSetPageFrm, SetPageFrm);
  Application.CreateForm(TMEPCFreqFrm, MEPCFreqFrm);
  Application.CreateForm(TSetBlockFrm, SetBlockFrm);
  Application.CreateForm(TPrintGraphFrm, PrintGraphFrm);
  Application.CreateForm(TSetIgnoreFrm, SetIgnoreFrm);
  Application.CreateForm(TSetFitParsFrm, SetFitParsFrm);
  Application.CreateForm(TDigFilterDlg, DigFilterDlg);
  Application.CreateForm(TExportFrm, ExportFrm);
  Application.CreateForm(TInvertDlg, InvertDlg);
  Application.CreateForm(TFTestFrm, FTestFrm);
  Application.CreateForm(TPrintPageViewFrm, PrintPageViewFrm);
  Application.CreateForm(TEditMarkersFrm, EditMarkersFrm);
  Application.CreateForm(TImportASCIIFrm, ImportASCIIFrm);
  Application.CreateForm(TImportRawFrm, ImportRawFrm);
  Application.CreateForm(TSetFitPars1frm, SetFitPars1frm);
  Application.CreateForm(TStimulator, Stimulator);
  Application.CreateForm(TAmplifier, Amplifier);
  Application.CreateForm(TDirectorySelectFrm, DirectorySelectFrm);
  Application.Run;
end.
