�
 TRECORDFRM 0YI  TPF0
TRecordFrm	RecordFrmTagLeftkTop� CaptionRecord To DiskClientHeight<ClientWidth�Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 	FormStyle
fsMDIChild
KeyPreview	Position
poDesignedVisible	
OnActivateFormActivateOnClose	FormCloseOnCreate
FormCreate	OnDestroyFormDestroyOnDeactivateFormDeactivate	OnKeyDownFormKeyDownOnResize
FormResizeOnShowFormShow
TextHeight TLabelLabel4Left� TopWidth HeightCaptionIdent.Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TScopeDisplay	scDisplayLeft� Top Width�Heighta	OnMouseUpscDisplayMouseUpOnCursorChangescDisplayCursorChangeCursorChangeInProgressNumChannels	NumPoints	MaxPoints'XMin XMax�XOffset CursorsEnabled	TScale       ��?TUnitssTCalBar       ���ZoomDisableHorizontal	ZoomDisableVerticalDisableChannelVisibilityButtonPrinterFontSize PrinterPenWidth PrinterLeftMargin PrinterRightMargin PrinterTopMargin PrinterBottomMargin PrinterDisableColorPrinterShowLabels	PrinterShowZeroLevels	MetafileWidth MetafileHeight StorageModeRecordNumber�DisplayGrid	MaxADCValue�MinADCValue �NumBytesPerSampleFloatingPointSamplesFixZeroLevelsDisplaySelectedFontSize  	TGroupBox
ControlGrpLeftTopWidth� Height� Caption Record Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabel
lbFileSizeLeftHTophWidthHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabellbRecordingLeft_Top#WidthAHeightCaption	RecordingFont.CharsetDEFAULT_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TButtonbRecordLeftTopWidthQHeightHintStart Recording (F1)CaptionRecordFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrder OnClickbRecordClick  TButtonbStopLeft_TopWidthFHeightHintStop Recording (F2)CaptionStopEnabledFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClick
bStopClick  TPanelRecordPanelLeftTop2Width� Height� 
BevelOuterbvNoneTabOrder TLabelLabel3LeftTop	WidthbHeight	AlignmenttaRightJustifyAutoSizeCaptionNo. channelsFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel5Left Top WidthbHeight	AlignmenttaRightJustifyAutoSizeCaptionSampling IntervalFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel2Left TopfWidthBHeight	AlignmenttaRightJustifyCaption
No. sweeps  TValidatedEditedRecordDurationLefthTopPWidth<HeightHintDuration of recording to disk
OnKeyPressedRecordDurationKeyPressAutoSizeShowHint	Text 1.0 sValue       ��?Scale       ��?UnitssNumberFormat%.1fLoLimit     o��?HiLimit     ���b@  TValidatedEditedNumChannelsLefthTop	Width<HeightHint+No. of analog input channels to be acquired
OnKeyPressedNumChannelsKeyPressAutoSizeShowHint	Text 1 Value       ��?Scale       ��?NumberFormat%.0fLoLimit       ��?HiLimit     ���b@  TValidatedEditedSamplingIntervalLefthTop Width<HeightHint Analog channel sampling interval
OnKeyPressedSamplingIntervalKeyPressAutoSizeShowHint	Text 1 msValue     o��?Scale       �@UnitsmsNumberFormat%.4gLoLimit     �7��?HiLimit       �@  TValidatedEditedNumTriggerSweepsLefthTopgWidth<Height
OnKeyPressedNumTriggerSweepsKeyPressAutoSizeText 1 Value       ��?Scale       ��?NumberFormat%.0fLoLimit       ��?HiLimit     ���b@  TRadioButtonrbContinuousRecordingLeftTop=WidthZHeightHint-Record continuously until Stop button pressedCaption
ContinuousChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderTabStop	OnClickrbContinuousRecordingClick  TRadioButtonrbRecordingDurationLeftTopPWidthFHeightHint(Record a series of fixed duration sweepsCaptionDurationFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickrbRecordingDurationClick  	TCheckBoxckNewFilePerSweepLeftTop� Width� HeightHint8Repeated recording sweeps stored in separate data files.	AlignmenttaLeftJustifyCaptionNew File Per SweepFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickckNewFilePerSweepClick    	TGroupBox
TriggerGrpLeftTop�Width� Height}Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TBevelBevel1LeftTop� WidthqHeight  TPageControlSpecialPageLeftTopWidth� Heighti
ActivePageTrigTabTabOrder  	TTabSheetTrigTabCaptionTriggger Mode TRadioButton	rbFreeRunLeftTop WidthiHeightHintStart recording immediatelyCaptionFree RunChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrder TabStop	OnClickrbFreeRunClick  TRadioButtonrbExtTriggerLeftTopWidthqHeightHintStart recording immediatelyCaptionExternal TriggerFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickrbExtTriggerClick  	TGroupBoxExtTriggerGrpLeftTop%WidthiHeight#Caption Trigger Level Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TRadioButtonrbExtTriggerHighLeft	TopWidth2HeightCaption5VFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickrbExtTriggerHighClick  TRadioButtonrbExttriggerLowLeft8TopWidth:HeightCaption0VChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbExttriggerLowClick    	TTabSheet
SpecialTabCaptionSpecial
ImageIndex 	TCheckBox
ckCapacityLeftTopWidthiHeightHint!On-line cell capacity calculationCaptionCapacityEnabledFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickckCapacityClick  	TCheckBoxckFluorescenceLeftTopWidthiHeightHint&On-line fluorescence ratio calculationCaptionFluorescenceEnabledFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickckFluorescenceClick  	TCheckBoxckEventFrequencyLeftTop(Width� HeightHintOn-line event frequency plotCaptionEvent FrequencyFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickckEventFrequencyClick  	TCheckBoxckResistanceLeftTop8Width� HeightHintOn-line event frequency plotCaption
ResistanceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickckResistanceClick  TButtonbSpecialLefthTopWidth1HeightCaptionConfigFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbSpecialClick     TEditedIdentLeft� TopWidth�HeightAutoSizeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnChangeedIdentChange
OnKeyPressedIdentKeyPress  	TGroupBox	StatusGrpLeft� Top�WidthYHeight)Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TShapeshSeparatorLeftTopWidth Height  TEditedMarkerLeftXTopWidthQHeightAutoSizeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder 
OnKeyPressedMarkerKeyPress  TButtonbMarkLeftTopWidthIHeightHint$Add marker text to bottom of displayCaption
Mark ChartFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClick
bMarkClick  TEditEdNumMarkersLeft� TopWidthHeightHintNumber of markers remainingAutoSizeReadOnly	TabOrderTextEdNumMarkers  TPanelTDisplayPanelLeft� Top
WidthIHeight
BevelOuterbvNoneTabOrder TLabel
lbTDisplayLeftTopWidth[HeightCaptionDisplay DurationFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEdit
edTDisplayLeft� TopWidthKHeight
OnKeyPressedTDisplayKeyPressAutoSizeText 0.016667 minValue     � ��?Scale     ����?UnitsminNumberFormat%.5gLoLimit     ����?HiLimit      @�@  TButtonbTDisplayDoubleLeft2TopWidthHeightCaption4Font.CharsetSYMBOL_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameWebdings
Font.StylefsBold 
ParentFontTabOrderOnClickbTDisplayDoubleClick  TButtonbTDisplayHalfLeft� TopWidthHeightCaption3Font.CharsetSYMBOL_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameWebdings
Font.StylefsBold 
ParentFontTabOrderOnClickbTDisplayHalfClick  TRadioButtonrbTDisplayUnitsMSecsLeftaTopWidth(HeightCaptionmsChecked	Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbTDisplayUnitsMSecsClick  TRadioButtonrbTDisplayUnitsSecsLeft� TopWidthHeightCaptionsFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbTDisplayUnitsSecsClick  TRadioButtonrbTDisplayUnitsMinsLeft� TopWidth*HeightCaptionminFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbTDisplayUnitsMinsClick    	TGroupBoxStimulatorGrpLeftTop� Width� HeightZCaption Stimulator Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TButtonbStartStimulusLeftTop+WidthaHeightHintStart stimulus protocolCaptionStartFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrder OnClickbStartStimulusClick  TButtonbStopStimulusLeftnTop+Width9HeightHintStop stimulusn protocolCaptionStopFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickbStopStimulusClick  	TComboBoxcbStimProgramLeftTopWidth� HeightHint"Stimulation program in current useTabOrderTextcbStimProgramOnChangecbStimProgramChange  	TCheckBoxckStartStimOnRecordLeftTop@Width� HeightHint.Start stimulus protocol when recording startedCaptionStart on RecordFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickckStartStimOnRecordClick   	TGroupBox
ReadoutGrpLeftTop�Width� Height� Caption Signal Level Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder 
THTMLLabel	lbReadOutLeftTopWidth� HeightyCaptionLabel	AlignmenttaLeftJustifyLineSpacing       ��?Font.CharsetANSI_CHARSET
Font.ColorclBlueFont.Height�	Font.NameArial
Font.Style   	TCheckBoxckFixedZeroLevelsLeftTop� WidthyHeightCaptionFixed zero levelsFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder OnClickckFixedZeroLevelsClick   	TGroupBoxAOGrpLeftTop1Width� HeightQHint*Patch clamp command voltage divide factorsCaption Analog Outputs (AO) Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TPageControlAOPageLeftTopWidth� Height9
ActivePageAO0TabFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 	MultiLine	
ParentFontTabOrder TabWidth# 	TTabSheetAO0TabCaptionAO0 TLabelLabel29Left TopWidthQHeightAutoSizeCaptionHolding LevelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TValidatedEdit	edAO0HoldLeftPTopWidthDHeightHintStimulus holding level
OnKeyPressedAO0HoldKeyPressAutoSizeShowHint	Text -100 mVValue       ��Scale       ��?UnitsmVNumberFormat%.3gLoLimit     ���b�HiLimit     ���b@   	TTabSheetAO1TabCaptionAO1
ImageIndex TLabelLabel8Left TopWidthQHeightAutoSizeCaptionHolding LevelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TValidatedEdit	edAO1HoldLeftPTopWidthDHeightHintDAC Output 0 divide factors
OnKeyPressedAO1HoldKeyPressAutoSizeText	 -100 mV#Value       ��Scale       ��?UnitsmV#NumberFormat%.3gLoLimit     ���b�HiLimit     ���b@   	TTabSheetAO2TabCaptionAO2
ImageIndex TLabelLabel11Left TopWidthQHeightAutoSizeCaptionHolding LevelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TValidatedEdit	edAO2HoldLeftPTopWidthDHeightHintDAC Output 0 divide factors
OnKeyPressedAO2HoldKeyPressAutoSizeText	 -100 mV#Value       ��Scale       ��?UnitsmV#NumberFormat%.3gLoLimit     ���b�HiLimit     ���b@   	TTabSheetAO3TabCaptionAO3
ImageIndex TLabelLabel27Left TopWidthQHeightAutoSizeCaptionHolding LevelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  TValidatedEdit	edAO3HoldLeftPTopWidthDHeightHintDAC Output 0 divide factors
OnKeyPressedAO3HoldKeyPressAutoSizeText	 -100 mV#Value       ��Scale       ��?UnitsmV#NumberFormat%.3gLoLimit     ���b�HiLimit     ���b@     	TGroupBox
AmpGainGrpLeftTopWidth� Height� Caption Amplifier Gain / Mode  Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TButtonbUpdateGainLeftTophWidth� HeightCaptionUpdate GainFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickbUpdateGainClick  TPageControlAmpilifiersPageLeftTopWidth� HeightQ
ActivePageAmp0Tab	MultiLine	TabOrderTabWidth& 	TTabSheetAmp0TabCaption1. TLabelLabel7LeftTopWidthHeightCaptionGain  TValidatedEditedAmplifierGain0Left$TopWidthpHeight
OnKeyPressedAmplifierGain0KeyPressAutoSizeText 0 Scale       ��?NumberFormat%.6gLoLimit     ���b�HiLimit     ���b@  TRadioButton	rbVCLAMP0LeftTopWidth?HeightCaptionVClampChecked	Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbVCLAMP0Click  TRadioButton	rbICLAMP0LeftZTopWidthAHeightCaptionIClampFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbICLAMP0Click   	TTabSheetAmp1TabCaption2.
ImageIndex TLabelLabel9LeftTopWidthHeightCaptionGain  TValidatedEditedAmplifierGain1Left$TopWidthpHeight
OnKeyPressedAmplifierGain1KeyPressAutoSizeText 0 Scale       ��?NumberFormat%.6gLoLimit     ���b�HiLimit     ���b@  TRadioButton	rbICLamp1LeftXTopWidthAHeightCaptionIClampFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbICLamp1Click  TRadioButton	rbVClamp1LeftTopWidth?HeightCaptionVClampChecked	Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbVClamp1Click   	TTabSheetAmp2TabCaption3.
ImageIndex TLabelLabel10LeftTopWidthHeightCaptionGain  TValidatedEditedAmplifierGain2Left$TopWidthpHeight
OnKeyPressedAmplifierGain2KeyPressAutoSizeText 0 Scale       ��?NumberFormat%.6gLoLimit     ���b�HiLimit     ���b@  TRadioButton	rbVCLAMP2TagLeftTopWidth?HeightCaptionVClampChecked	Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbVCLAMP2Click  TRadioButton	RBICLAMP2TagLeftZTopWidthIHeightCaptionIClampFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickRBICLAMP2Click   	TTabSheetAmp3TabCaption4.
ImageIndex TLabelLabel12LeftTopWidthHeightCaptionGain  TValidatedEditedAmplifierGain3Left$TopWidthpHeight
OnKeyPressedAmplifierGain3KeyPressAutoSizeText 0 Scale       ��?NumberFormat%.6gLoLimit     ���b�HiLimit     ���b@  TRadioButton	rbVCLAMP3TagLeftTopWidth?HeightCaptionVClampChecked	Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderTabStop	OnClickrbVCLAMP3Click  TRadioButton	rbICLAMP3TagLeftZTopWidthIHeightCaptionIClampFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbICLAMP3Click     TTimerTimerEnabledInterval2OnTimer
TimerTimerLeft� Top�   