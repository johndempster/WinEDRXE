�
 TINPUTCHANNELSETUPFRM 0�"  TPF0TInputChannelSetupFrmInputChannelSetupFrmLeft�TopvBorderStylebsDialogCaption!Input Channels & Amplifiers SetupClientHeight	ClientWidtheColor	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 	FormStyle
fsMDIChildOldCreateOrder	PositionpoScreenCenterVisible	OnClose	FormCloseOnShowFormShowPixelsPerInch`
TextHeight TPageControlPageControlLeftTop WidthYHeight�
ActivePageAmplifiersPageTabOrder OnChangePageControlChange
OnChangingPageControlChanging 	TTabSheetChannelsPageCaptionInput Channels 
ImageIndex TStringGridChannelTableLeft TopWidthAHeight_Hint3Input channel scaling factors and calibration unitsColCountDefaultColWidth,DefaultRowHeight
FixedColorclInfoBkRowCount� Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont
ScrollBars
ssVerticalTabOrder 
OnKeyPressChannelTableKeyPress
RowHeights   	TGroupBox	GroupBox3LeftTophWidth<HeightQCaption Display GridTabOrder 	TGroupBox	TUnitsGrpLeftTopWidthYHeight7Caption Time units Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TRadioButtonrbTmsecsLeftTopWidth9HeightCaptionmsecs.Checked	TabOrder TabStop	OnClickrbTmsecsClick  TRadioButtonrbTSecsLeftTop Width1HeightCaptionsecs.TabOrderOnClickrbTSecsClick     	TTabSheetAmplifiersPageCaption Amplifiers
ImageIndex TTabControlAmplifiersTabLeft Top WidthFHeight	MultiLine	TabOrder Tabs.StringsAmplifier #1Amplifier #2Amplifier #3Amplifier #4 TabIndexOnChangeAmplifiersTabChange
OnChangingAmplifiersTabChanging  TPanelAmpPanelLeft TopWidthIHeight�TabOrder 	TComboBoxcbAmplifierLeftTopWidth9HeightHint:Patch/Voltage clamp amplifier attached to channels 0 and 1StylecsDropDownListParentShowHintShowHint	TabOrder OnChangecbAmplifierChange  	TGroupBoxModeGrpLeftTop$Width9Height� Caption Input Channels TabOrder TRadioButtonrbIClampLeftXTopWidthIHeightHint:Primary & secondary channels scaling in current-clamp modeCaptionIClampParentShowHintShowHint	TabOrder OnClickrbIClampClick  TRadioButtonrbVClampLeftTopWidthFHeightHint:Primary & secondary channels scaling in voltage-clamp modeCaptionVClampChecked	ParentShowHintShowHint	TabOrderTabStop	OnClickrbVClampClick  	TGroupBoxPrimaryChannelGrpLeftTop"Width)HeightPCaption Primary Channel TabOrder TLabelLabel3Left	TopWidth@Height	AlignmenttaRightJustifyCaptionScale factorFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel10Left� TopWidthHeight	AlignmenttaRightJustifyCaptionUnitsFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabellbPrimaryChannelLeftyTop0WidthWHeight	AlignmenttaCenterCaptionOn analog inputFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedPrimaryChannelScaleFactorLeftPTopWidthiHeightHint3Primary channel scaling factor at X1 gain (Units/V)
OnKeyPress#edPrimaryChannelScaleFactorKeyPressShowHint	Text 0 V/VScale       ��?UnitsV/VNumberFormat%.4gLoLimit     ���b�HiLimit     ���b@  TEditedPrimaryChannelUnitsLeft� TopWidth1HeightHintPrimary channel unitsParentShowHintShowHint	TabOrderTextedPrimaryChannelUnitsOnChangeedPrimaryChannelUnitsChange  	TComboBoxcbPrimaryChannelLeft� Top0WidthMHeightHint/Lab. interface analog input for primary channelStylecsDropDownListParentShowHintShowHint	TabOrder   	TGroupBoxSecondaryChannelGrpLeftTopvWidth)HeightPCaption Secondary Channel TabOrder TLabelLabel5Left	TopWidth@Height	AlignmenttaRightJustifyCaptionScale factorFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel11Left� TopWidthHeight	AlignmenttaRightJustifyCaptionUnitsFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabellbSecondaryChannelLeftyTop0WidthWHeight	AlignmenttaCenterCaptionOn analog inputFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedSecondaryChannelScaleFactorLeftPTopWidthiHeightHint5Secondary channel scaling factor at X1 gain (Units/V)
OnKeyPress%edSecondaryChannelScaleFactorKeyPressShowHint	Text 0 V/VScale       ��?UnitsV/VNumberFormat%.4gLoLimit     ���b�HiLimit     ���b@  TEditedSecondaryChannelUnitsLeft� TopWidth1HeightHintSecondary channel unitsParentShowHintShowHint	TabOrderTextedCurrentOutputUnitsOnChangeedSecondaryChannelUnitsChange  	TComboBoxcbSecondaryChannelLeft� Top0WidthMHeightHint1Lab. interface analog input for secondary channelStylecsDropDownListParentShowHintShowHint	TabOrder    	TGroupBoxVoltageCommandGrpLeftTop� Width9Height1Caption Voltage-clamp command channelTabOrder TLabelLabel2Left� TopWidth$Height	AlignmenttaRightJustifyCaptionOutputFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel4Left	TopWidth@Height	AlignmenttaRightJustifyCaptionScale factorFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBoxcbVoltageCommandChannelLeft� TopWidthMHeightHint>Lab. interface analog output for voltage clamp command signalsStylecsDropDownListParentShowHintShowHint	TabOrder   TValidatedEditedVoltageCommandScaleFactorLeftPTopWidthYHeightHint$Voltage clamp command scaling factorShowHint	Text 0 V/VScale       ��?UnitsV/VNumberFormat%.4gLoLimit     ���b�HiLimit     ���b@   	TGroupBoxCurrentCommandGrpLeftTop,Width9Height1Caption Current-clamp command channel TabOrder TLabelLabel8Left� TopWidth$Height	AlignmenttaRightJustifyCaptionOutputFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel9Left	TopWidth@Height	AlignmenttaRightJustifyCaptionScale factorFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBoxcbCurrentCommandChannelLeft� TopWidthMHeightHint>Lab. interface analog output for current clamp command signalsStylecsDropDownListParentShowHintShowHint	TabOrder   TValidatedEditedCurrentCommandScaleFactorLeftPTopWidthYHeightHint$Current clamp command scaling factorShowHint	Text 0 A/VScale       ��?UnitsA/VNumberFormat%.4gLoLimit     ���b�HiLimit     ���b@   	TGroupBoxTelegraphChannelsGrpLeftTop`Width9HeightMCaption Telegraph channels TabOrder TPanelGainTelegraphPanelLeftTopWidth2Height
BevelOuterbvNoneFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabellbTelegraphChannel1Left� Top WidthHeight	AlignmenttaRightJustifyCaptionGain Font.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBoxcbGainTelegraphChannelLeft� Top WidthMHeightStylecsDropDownListTabOrder    TPanelModeTelegraphPanelLeftTop,Width2Height
BevelOuterbvNoneFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TLabelLabel1LeftETop Width� Height	AlignmenttaRightJustifyCaptionVoltage/current clamp modeFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBoxcbModeTelegraphChannelLeft� Top WidthMHeightStylecsDropDownListTabOrder        TButtonbLoadDefaultSettingsLeft� Top�WidthnHeightCaptionDefault SettingsTabOrderOnClickbLoadDefaultSettingsClick  TButtonbSaveSettingsLeftzTop�WidthnHeightCaptionSave SettingsTabOrderOnClickbSaveSettingsClick  TButtonbLoadSettingsLeftTop�WidthnHeightCaptionLoad SettingsTabOrderOnClickbLoadSettingsClick  TOpenDialog
OpenDialogLeft`Toph  TSaveDialog
SaveDialogLeft`Top�    