�
 TNOISEANALFRM 0T  TPF0TNoiseAnalFrmNoiseAnalFrmTagLeftCTopCaptionNoise AnalysisClientHeight�ClientWidth�Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 	FormStyle
fsMDIChildOldCreateOrder	PositionpoScreenCenterVisible	
OnActivateFormActivateOnClose	FormCloseOnDeactivateFormDeactivateOnResize
FormResizeOnShowFormShowPixelsPerInch`
TextHeight TPageControlPageLeft Top Width�Height�
ActivePageDataTabFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder OnChange
PageChange 	TTabSheetDataTabCaptionReview/Edit Data Records TScopeDisplay	scDisplayLeft� TopWidth�HeightY	OnMouseUpscDisplayMouseUpOnCursorChangescDisplayCursorChangeCursorChangeInProgressNumChannels	NumPoints 	MaxPoints XMin�XMax XOffset CursorsEnabled	TScale       ��?TUnitssTCalBar       ���ZoomDisableHorizontal	ZoomDisableVerticalDisableChannelVisibilityButtonPrinterFontSize PrinterPenWidth PrinterLeftMargin PrinterRightMargin PrinterTopMargin PrinterBottomMargin PrinterDisableColorPrinterShowLabels	PrinterShowZeroLevels	MetafileWidth MetafileHeight StorageModeRecordNumber�DisplayGrid	MaxADCValue�MinADCValue �NumBytesPerSampleFloatingPointSamplesFixZeroLevelsDisplaySelectedFontSize  TEditedDisplayKeyPressSourceLeft TopWidthAHeightTabOrderTextedDisplayKeyPressSource	OnKeyDownedDisplayKeyPressSourceKeyDown  	TGroupBox
ControlGrpLeftTop Width� Height�Caption	 Records Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabelLabel10LeftTopVWidthHeight	AlignmenttaRightJustifyCaptionType  TLabelLabel5LeftTop<WidthHeight	AlignmenttaRightJustifyCaptionTime  
TScrollBarsbRecordLeftTop&Width� HeightPageSize TabOrder OnChangesbRecordChange  	TCheckBox
ckRejectedLeft8ToppWidthIHeightCaption	 RejectedFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickckRejectedClick  	TGroupBox	GroupBox2LeftTop� Width� Height� Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TLabelLabel1LeftTopWidthBHeightCaption
AC Channel  TLabelLabel2LeftTop<WidthDHeightCaption
DC Channel  TLabelLabel3LeftTophWidthBHeightCaptionRecord Size  TLabelLabel12LeftTop� Width*HeightCaptionOverlapFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBoxcbACChannelLeftTop WidthyHeightStylecsDropDownListTabOrder OnChangecbACChannelChange  	TComboBoxcbDCChannelLeftTopLWidthyHeightStylecsDropDownListTabOrderOnChangecbDCChannelChange  TValidatedEditedRecordSizeLeftTopxWidth2Height
OnKeyPressedRecordSizeKeyPressAutoSizeText 32 Value       �@Scale       ��?NumberFormat%.0fLoLimit       �@HiLimit       �@  TValidatedEditedRecordOverlapLeftTop� Width2Height
OnKeyPressedRecordOverlapKeyPressAutoSizeText 0 %Value     `B��?Scale       ��?Units%NumberFormat%.0fLoLimit     `B��?HiLimit       �@   	TComboBox	cbRecTypeLeft8TopVWidthYHeightStylecsDropDownListTabOrderOnChangecbRecTypeChangeItems.StringsTest
Background   TButtonbSetRecordStateLeft8Top� WidthYHeightHint1Set the Type/Rejected state of a block of recordsCaption	Set BlockFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontParentShowHintShowHint	TabOrderOnClickbSetRecordStateClick  
TRangeEditedRecordLeftTopWidth� Height
OnKeyPressedRecordKeyPressAutoSizeText 1 / 1.00000001504746624E30 LoValue       ��?HiValue     ���b@LoLimit       ��?HiLimit     ���b@Scale       ��?NumberFormat%.0f / %.0f  	TGroupBox	GroupBox3LeftTophWidth� Height-TabOrder 	TCheckBoxckFixedZeroLevelsLeftTopWidthyHeightCaptionFix Zero LevelsFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickckFixedZeroLevelsClick   TEditEdTimeLeft8Top<WidthYHeightReadOnly	TabOrderTextEdTime   	TGroupBoxDataGrpLeft� Top{Width�Height)Caption Data Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TLabelLabel6LeftTopWidth;HeightCaption	Mean (DC)  TLabelLabel7Left� TopWidthJHeightCaptionVariance (AC)  TEditedMeanLeftHTopWidthYHeightAutoSizeReadOnly	TabOrder Text 512  TEdit
edVarianceLeft TopWidthYHeightAutoSizeReadOnly	TabOrderText 512    	TTabSheet
AmpHistTabCaptionAmplitude Histogram
ImageIndex TXYPlotDisplay	plAmpHistLeft� TopWidthqHeight!MaxPointsPerLine XAxisMax       ��?	XAxisTick     ����?XAxisLawaxLinear
XAxisLabelX AxisXAxisAutoRangeYAxisMax       ��?	YAxisTick     ����?YAxisLawaxLinear
YAxisLabelY AxisYAxisAutoRangeYAxisLabelAtTopScreenFontNameArialScreenFontSize
	LineWidth
MarkerSize
	ShowLines	ShowMarkers	HistogramFullBordersHistogramFillColorclWhiteHistogramFillStylebsClearHistogramCumulativeHistogramPercentagePrinterFontSize
PrinterFontNameArialPrinterLineWidthPrinterMarkerSizePrinterLeftMargin PrinterRightMargin PrinterTopMargin PrinterBottomMargin PrinterDisableColorMetafileWidth�MetafileHeight�  	TGroupBox
AmpHistGrpLeftTop Width� Height�Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabelLabel17LeftTop� Width+HeightCaptionNo.binsFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TButtonbNewAmpHistLeftTopWidth� HeightCaptionNew HistogramFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickbNewAmpHistClick  	TGroupBox	GroupBox1LeftTop<Width� Height� Caption Data  TabOrder TRadioButtonrbAmpHistAllRecordsLeftTopHWidthaHeightCaptionAll RecordsChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	  TRadioButton
rbAmpRangeLeftTopXWidth9HeightCaption RangeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  
TRangeEditedAmpHistRangeLeftTophWidthOHeightAutoSizeText 1.00 - 10.00 LoValue       ��?HiValue       �@LoLimit       ��?HiLimit     ���b@Scale       ��?NumberFormat	%.f - %.f  	TGroupBoxChanGrpLeftTopWidthyHeight5Caption	 Channel Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TRadioButtonrbACChannelLeftTopWidthYHeightCaption
AC ChannelChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	  TRadioButtonrbDCChannelLeftTop WidthYHeightCaption
DC ChannelFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder    TButtonbAmpHistSetAxesLeftTop$Width� HeightCaptionSet AxesFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbAmpHistSetAxesClick  	TGroupBox	GroupBox4LeftToppWidth� Height)TabOrder TButtonbSetZeroLeftTopWidthyHeightCaptionSet ZeroFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickbSetZeroClick   TPanelBinRangePanelLeftTop� Width� HeightI
BevelOuterbvNoneTabOrder TLabelLabel13LeftTopWidth2Height	AlignmenttaRightJustifyCaption	Bin widthFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel19LeftTopWidth=Height	AlignmenttaRightJustifyCaption
Lower Lim.Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel22LeftTop3Width=Height	AlignmenttaRightJustifyCaption
Upper Lim.Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEdit
edBinWidthLeftOTopWidth@HeightHintNo. of bins in histogram
OnKeyPressedBinWidthKeyPressAutoSizeText 2 Value       � @Scale       ��?NumberFormat%.4gLoLimit     ���b�HiLimit     ���b@  TValidatedEditedBinsLowerLeftOTopWidth@HeightHintNo. of bins in histogram
OnKeyPressedBinsLowerKeyPressAutoSizeText 2 Value       � @Scale       ��?NumberFormat%.4gLoLimit     ���b�HiLimit     ���b@  TValidatedEditedBinsUpperLeftPTop3Width@HeightHintNo. of bins in histogram
OnKeyPressedBinsUpperKeyPressAutoSizeText 2 Value       � @Scale       ��?NumberFormat%.4gLoLimit     ���b�HiLimit     ���b@   TValidatedEdit	edNumBinsLeftSTop� Width@Height
OnKeyPressedNumBinsKeyPressAutoSizeText 100 Value       �@Scale       ��?NumberFormat%.3gLoLimit       ��?HiLimit       �@    	TTabSheetVarianceTabCaptionVariance Analysis TXYPlotDisplay	plVarPlotLeft� TopWidth�HeightMaxPointsPerLine XAxisMax       ��?	XAxisTick     ����?XAxisLawaxLinear
XAxisLabelX AxisXAxisAutoRangeYAxisMax       ��?	YAxisTick     ����?YAxisLawaxLinear
YAxisLabelY AxisYAxisAutoRangeYAxisLabelAtTopScreenFontNameArialScreenFontSize
	LineWidth
MarkerSize
	ShowLines	ShowMarkers	HistogramFullBordersHistogramFillColorclWhiteHistogramFillStylebsClearHistogramCumulativeHistogramPercentagePrinterFontSize
PrinterFontNameArialPrinterLineWidthPrinterMarkerSizePrinterLeftMargin PrinterRightMargin PrinterTopMargin PrinterBottomMargin PrinterDisableColorMetafileWidth�MetafileHeight�  	TGroupBoxVarGrpLeftTop Width� Height�Caption	 Records Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  	TGroupBox	GroupBox5LeftTop� Width� HeightqCaption Plot Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TLabelLabel9LeftTop<WidthHeightCaptionY AxisFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel8LeftTopWidthHeightCaptionX AxisFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  	TComboBox
cbVarYAxisLeftTopLWidthyHeightStylecsDropDownListTabOrder Items.Strings
Record No.Time	Mean (DC)St. Dev. [(AC)Variance (AC)	Skew (AC)Median Freq (AC)   	TComboBox
cbVarXAxisLeftTop WidthyHeightStylecsDropDownListTabOrderItems.Strings
Record No.Time	Mean (DC)St. Dev. [(AC)Variance (AC)	Skew (AC)Median Freq (AC)    TButtonbDoVarianceLeftTopWidth� HeightCaptionNew PlotFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbDoVarianceClick  	TGroupBox	GroupBox6LeftTopVWidth� HeightQCaption	 Records TabOrder TRadioButtonrbVarAllRecordsLeftTopWidthYHeightCaption All recordsChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	  TRadioButton
rbVarRangeLeftTop WidthYHeightCaption RangeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  
TRangeEdit
edVarRangeLeftTop4WidthYHeightAutoSizeText 1.00 - 1000.00 LoValue       ��?HiValue       �@LoLimit       ��?HiLimit       �@Scale       ��?NumberFormat	%.f - %.f   TButtonbVarSetAxesLeftTop&Width� HeightCaptionSet AxesFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbVarSetAxesClick  	TGroupBox
GroupBox12LeftTopWidth� Height)Caption	 Options Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder 	TCheckBoxckVarSubtractBackgroundLeftTopWidthyHeightCaption Subtr't Backgr'ndFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder    TButtonbStopVarianceLeftTop<WidthFHeightCaptionStopFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbStopVarianceClick   	TGroupBox	VarFitGrpLeft� TopWidth�Height� Caption
 Analysis Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TButtonbVarFitLeftTopWidthiHeightCaption	Fit CurveFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickbVarFitClick  	TComboBoxcbVarEquationLeftTop$WidthiHeightStylecsDropDownListTabOrderItems.StringsNoneLinearParabolaExponential   	TRichEditerVarResultsLeftxTopWidth!HeightqFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings     
ParentFont
ScrollBars
ssVerticalTabOrderZoomd  TButton
bSaveToLogLeftTop@WidthiHeightCaptionSave to LogFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbSaveToLogClick    	TTabSheetSpectrumTabCaptionSpectral Analysis TLabel
lbSpecFit0LeftTop� WidthHeightCaption|Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabel
lbSpecFit1LeftTop� WidthHeightCaption|Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TXYPlotDisplay
plSpecPlotLeft� TopWidth�Height� OnCursorChangeplSpecPlotCursorChangeMaxPointsPerLine  XAxisMax       ��?	XAxisTick     ����?XAxisLawaxLinear
XAxisLabelX AxisXAxisAutoRangeYAxisMax       ��?	YAxisTick     ����?YAxisLawaxLinear
YAxisLabelY AxisYAxisAutoRangeYAxisLabelAtTopScreenFontNameArialScreenFontSize
	LineWidth
MarkerSize	ShowLines	ShowMarkers	HistogramFullBordersHistogramFillColorclWhiteHistogramFillStylebsClearHistogramCumulativeHistogramPercentagePrinterFontSize
PrinterFontNameArialPrinterLineWidthPrinterMarkerSizePrinterLeftMargin PrinterRightMargin PrinterTopMargin PrinterBottomMargin PrinterDisableColorMetafileWidth�MetafileHeight�  TShapeshLineLeftTop Width� Height  TLabellbAreaLeftTop� Width$HeightCaptionlbAreaFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  	TGroupBoxspecGrpLeftTop Width� Height�Caption	 Records Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  	TGroupBox	GroupBox8LeftTop� Width� Height9Caption Time window Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder  TRadioButton
rbNoWindowLeftTopWidthAHeightCaption NoneChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	  TRadioButtonrbCosineWindowLeftTop WidthYHeightCaption 10% CosineFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder   TButtonbDoSpectrumLeftTopWidth� HeightCaptionNew SpectrumFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbDoSpectrumClick  	TGroupBox	GroupBox9LeftTop@Width� HeightQCaption	 Records TabOrder TRadioButtonrbSpecAllRecordsLeftTopWidthYHeightCaption All recordsChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	  TRadioButtonrbSpecRangeLeftTop WidthYHeightCaption RangeFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  
TRangeEditedSpecRangeLeftTop0WidthYHeightAutoSizeText 1.00 - 1000.00 LoValue       ��?HiValue       �@LoLimit       ��?HiLimit       �@Scale       ��?NumberFormat	%.f - %.f   TButtonbSpecSetAxesLeftTop$Width� HeightCaptionSet AxesFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbSpecSetAxesClick  	TGroupBox
GroupBox10LeftTop0Width� HeightaCaption Freq. Averaging TabOrder TRadioButtonrbNoFreqAveragingLeftTopWidth9HeightCaption NoneChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TabStop	OnClickrbNoFreqAveragingClick  TRadioButtonrbLogFreqAveragingLeftTop WidthaHeightCaption LogarithmicFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbLogFreqAveragingClick  TRadioButtonrbLinFreqAveragingLeftTop0Width9HeightCaption LinearFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickrbLinFreqAveragingClick  TPanelpanNumFreqAveragedLeftTopAWidthiHeight
BevelOuterbvNoneTabOrder TLabelLabel11LeftTopWidth9HeightAutoSizeCaption
No. Freqs.Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TValidatedEditedNumFreqAveragedLeft:TopWidth)HeightAutoSizeText 10.00 Value       �@Scale       ��?NumberFormat%.fLoLimit       ��?HiLimit     ���b@    	TGroupBox
GroupBox11LeftTop� Width� HeightiCaption	 Options Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TLabelLabel4LeftTop1WidthIHeightCaptionRemove Harmonics ofFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontWordWrap	  	TCheckBoxckRemoveHarmonicsLeftTop0WidthHeightCaption Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderWordWrap	  	TCheckBoxckSubtractTrendsLeftTopWidthiHeightCaption Subtr't TrendsFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder   	TCheckBoxckSpecSubtractBackgroundLeftTop WidthiHeightCaption Subtr't Backg'dFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TValidatedEditedBaseFrequencyLeftTopNWidthIHeightAutoSizeText 0 HzScale       ��?UnitsHzNumberFormat%.4gLoLimit     ��_�HiLimit     ��_@    	TGroupBox
SpecFitGrpLeft� Top#Width�Height� Caption Curve fitting Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TButtonbFitLorentzianLeftTopWidthiHeightCaption	Fit CurveFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder OnClickbFitLorentzianClick  	TComboBoxcbSpecEquationLeftTop$WidthiHeightStylecsDropDownListTabOrderItems.StringsNone
LorentzianLorentzian + 1/f2 Lorentzians
MEPC Noise   	TRichEditerSpecResultsLeftxTopWidth!HeightqFont.CharsetANSI_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings     
ParentFont
ScrollBars
ssVerticalTabOrderZoomd  TButtonbMEPCFrequencyLeftTop@WidthiHeightCaptionMEPC FrequencyEnabledFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbMEPCFrequencyClick  TButtonbSaveToLogSpectrumLeftTopXWidthiHeightCaptionSave To LogFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbSaveToLogSpectrumClick      