unit Convert;
{ ==========================================================================
  WinCDR - File importing module (c) J Dempster, 1996-98, All Rights Reserved
  7/6/97 ... AppendWCPFile added
  10/6/97 ... WCPTopCLAMP added
  24/8/97 ... ImportFromDataFile and CFSToWCP added
  7/3/97 ... Modified from WinWCP Convert.pas
  24/6/98 ... ImportFromCFS Bad points at end of data now eliminated
  21/4/99 ... Axon Binary file v1.5 (pCLAMP V7) now supported
  29/8/99 ... WCP import added
  2/9/99 ... QUB LDT import added
  17/10/99 ... WinCDR V2.3 import added
  7/3/01 ... ExportToAxon modified to support 16 bit WCP files
  9/1/02 ... (V2.2.6) ExportToASCII added
  26/2/02 ... (V2.3.0) ExportToASCII now works properly
  9.9.02  ... (V2.3.4) Progress now reported on main status bar
  13.2.03 ... (V2.3.7) ExportToEDR added
  ========================================================================== }
interface

uses global,fileio,SysUtils,shared,Dialogs,Messages,forms,controls, maths,
     import ;

type

    TpClampV5 = packed record { Note. FETCHEX format header block }
	par : Array[0..79] of single ;
	Comment : Array[1..77] of char ;
        Labels : Array[1..80] of char ;
        Reserved : Array[1..3] of char ;
        ChannelNames : Array[0..15,1..10] of char ;
	ADCOffset : Array[0..15] of single ;
	ADCGain : Array[0..15] of single ;
        ADCAmplification : Array[0..15] of single ;
	ADCShift : Array[0..15] of single ;
	Units : Array[0..15,1..8] of char ;
        end ;

    TABF = packed record
         { Group #1 }
         FileType : Array[1..4] of char ;
         FileVersionNumber : single ;
	 OperationMode : SmallInt ;
	 ActualAcqLength : LongInt ;
	 NumPointsIgnored : SmallInt ;
	 ActualEpisodes : LongInt ;
	 FileStartDate : LongInt ;
	 FileStartTime : LongInt ;
	 StopwatchTime : LongInt ;
	 HeaderVersionNumber : single ;
	 nFileType : SmallInt ;
	 MSBinFormat : SmallInt ;
         { Group #2 }
	 DataSectionPtr : LongInt ;
	 TagSectionPtr : LongInt ;
	 NumTagEntries : LongInt ;
	 ScopeConfigPtr : LongInt ;
	 NumScopes : LongInt ;
	 DACFilePtr : LongInt ;
	 DACFileNumEpisodes : LongInt ;
	 Unused68 : Array[1..4] of char ;
	 DeltaArrayPtr : LongInt ;
	 NumDeltas : LongInt ;
	 VoiceTagPtr : LongInt ;
	 VoiceTagEntries : LongInt ;
         Unused88 : LongInt ;
	 SynchArrayPtr : LongInt ;
	 SynchArraySize : LongInt ;
         DataFormat : SmallInt ;
         SimultaneousScan : SmallInt ;
         StatisticsConfigPtr : LongInt ;
	 Unused108 : Array[1..12] of char ;
         { Group #3 }
	 ADCNumChannels : SmallInt ;
	 ADCSampleInterval : single ;
	 ADCSecondSampleInterval : single ;
	 SynchTimeUnit : single ;
	 SecondsPerRun : single ;
	 NumSamplesPerEpisode : LongInt ;
	 PreTriggerSamples : LongInt ;
	 EpisodesPerRun : LongInt ;
	 RunsPerTrial : LongInt ;
	 NumberOfTrials : LongInt ;
	 AveragingMode  : SmallInt ;
	 UndoRunCount : SmallInt ;
	 FirstEpisodeInRun : SmallInt ;
	 TriggerThreshold : single ;
	 TriggerSource : SmallInt ;
	 TriggerAction : SmallInt ;
	 TriggerPolarity : SmallInt ;
	 ScopeOutputInterval : single ;
	 EpisodeStartToStart : single ;
	 RunStartToStart : single ;
	 TrialStartToStart : single ;
	 AverageCount : LongInt ;
         ClockChange : LongInt ;
         nAutoTriggerStrategy : SmallInt ;
         { Group #4 }
	 DrawingStrategy : SmallInt ;
	 TiledDisplay : SmallInt ;
	 nEraseStrategy : SmallInt ;
	 DataDisplayMode : SmallInt ;
	 DisplayAverageUpdate : LongInt ;
	 ChannelStatsStrategy : SmallInt ;
	 CalculationPeriod : LongInt ;
	 SamplesPerTrace : LongInt ;
	 StartDisplayNum : LongInt ;
	 FinishDisplayNum : LongInt ;
	 MultiColor : SmallInt ;
	 ShowPNRawData : SmallInt ;
         StatisticsPeriod : single ;
         StatisticsMeasurements : LongInt ;
         StatisticsSaveStrategy : SmallInt ;
         { Group #5}
	 ADCRange : single ;
	 DACRange : single ;
	 ADCResolution : LongInt ;
	 DACResolution : LongInt ;
         { Group #6 }
	 ExperimentType : SmallInt ;
	 AutosampleEnable : SmallInt ;
	 AutosampleADCNum : SmallInt ;
	 AutosampleInstrument : SmallInt ;
	 AutosampleAdditGain : single ;
	 AutosampleFilter : single ;
	 AutosampleMembraneCap : single ;
	 ManualInfoStrategy : SmallInt ;
	 CellID1 : single ;
	 CellID2 : single ;
	 CellID3 : single ;
	 CreatorInfo : Array[1..16] of char ;
	 FileComment : Array[1..56] of char ;
         FileStartMillisecs : SmallInt ;
	 Unused338 : Array[1..10] of char ;
         { Group #7 }
	 ADCPtoLChannelMap : Array[0..15] of SmallInt ;
	 ADCSamplingSeq : Array[0..15] of SmallInt ;
	 ADCChannelName : Array[0..15,1..10] of char ;
	 ADCUnits: Array[0..15,1..8] of char ;
	 ProgrammableGain : Array[0..15] of  single ;
	 DisplayAmplification : Array[0..15] of  single ;
	 DisplayOffset : Array[0..15] of  single ;
	 InstrumentScaleFactor : Array[0..15] of  single ;
	 InstrumentOffset : Array[0..15] of  single ;
	 SignalGain : Array[0..15] of  single ;
	 SignalOffset : Array[0..15] of  single ;
	 SignalLowPassFilter : Array[0..15] of  single ;
	 SignalHighPassFilter : Array[0..15] of  single ;

         DACChannelName : Array[0..3,1..10] of char ;
         DACChannelUnits : Array[0..3,1..8] of char ;
         DACScaleFactor : Array[0..3] of single ;
         DACHoldingLevel : Array[0..3] of single ;
         SignalType : SmallInt ;
         Unused1412 : Array[1..10] of char ;
         { Group #8 }
         OutEnable : SmallInt ;
         SampleNumberOUT1 : SmallInt ;
         SampleNumberOUT2 : SmallInt ;
         FirstEpisodeOUT : SmallInt ;
         LastEpisodeOut : SmallInt ;
         PulseSamplesOUT1 : SmallInt ;
         PulseSamplesOUT2 : SmallInt ;
         { group #9 }
         DigitalEnable : SmallInt ;
         WaveformSource : SmallInt ;
         ActiveDACChannel : SmallInt ;
         InterEpisodeLevel : SmallInt ;
         EpochType : Array[0..9] of SmallInt ;
         EpochInitLevel : Array[0..9] of single ;
         EpochLevelInc : Array[0..9] of single ;
         EpochInitDuration : Array[0..9] of SmallInt ;
         EpochDurationInc : Array[0..9] of SmallInt ;
         DigitalHolding : SmallInt ;
         DigitalInterEpisode : SmallInt ;
         DigitalValue : Array[0..9] of SmallInt ;
         Unavailable : Array[1..4] of char ;
         Unused1612 : Array[1..8] of char ;
         { Group 10 }
         DACFileStatus : single ;
         DACFileOffset : single ;
         Unused1628 : Array[1..2] of char ;
         DACFileEpisodeNum : SmallInt ;
         DACFileADCNum : SmallInt ;
         DACFilePath : Array[1..84] of char ;
         { Group 11 }
         ConditEnable : SmallInt ;
         ConditChannel : SmallInt ;
         ConditNumPulses : LongInt ;
         BaselineDuration : single ;
         BaselineLevel : single ;
         StepDuration : single ;
         StepLevel : single ;
         PostTrainPeriod : single ;
         PostTrainLevel : single ;
         Unused1750 : array[1..12] of char ;
         { Group 12 }
         ParamToVary : SmallInt ;
         ParamValueList : Array[1..80] of char ;
         { Group 13 }
         AutoPeakEnable : SmallInt ;
         AutoPeakPolarity : SmallInt ;
         AutoPeakADCNum : SmallInt ;
         AutoPeakSearchMode : SmallInt ;
         AutoPeakStart : LongInt ;
         AutoPeakEnd : LongInt ;
         AutoPeakSmoothing : SmallInt ;
         AutoPeakBaseline : SmallInt ;
         AutoPeakAverage : SmallInt ;
         Unavailable1866 : array[1..2] of char ;
         AutopeakBaselineStart : LongInt ;
         AutopeakBaselineEnd : LongInt ;
         AutopeakMeasurements : LongInt ;
         { Group #14 }
         ArithmeticEnable : SmallInt ;
         ArithmeticUpperLimit : single ;
         ArithmeticLowerLimit : single ;
         ArithmeticADCNumA : SmallInt ;
         ArithmeticADCNumB : SmallInt ;
         ArithmeticK1 : single ;
         ArithmeticK2  : single ;
         ArithmeticK3 : single ;
         ArithmeticK4 : single ;
         ArithmeticOperator : Array[1..2] of char ;
         ArithmeticUnits : Array[1..8] of char ;
         ArithmeticK5 : single ;
         ArithmeticK6 : single ;
         ArithmeticExpression : SmallInt ;
         Unused1930 : array[1..2] of char ;
         { Group #15 }
         PNEnable : SmallInt ;
         PNPosition : SmallInt ;
         PNPolarity : SmallInt ;
         PNNumPulses : SmallInt ;
         PNADCNum : SmallInt ;
         PNHoldingLevel : single ;
         PNSettlingTime : single ;
         PNInterPulse : single ;
         Unused1954 : array[1..12] of char ;
         { Group #16 }
         ListEnable : SmallInt ;
         BellEnable : Array[0..1] of SmallInt ;
         BellLocation : Array[0..1] of SmallInt ;
         BellRepetitions : Array[0..1] of SmallInt ;
         LevelHysteresis : SmallInt ;
         TimeHysteresis : LongInt ;
         AllowExternalTags : SmallInt ;
         LowpassFilterType : Array[0..15] of char ;
         HighpassFilterType : Array[0..15] of char ;
         AverageAlgorithm : SmallInt ;
         AverageWeighting : single ;
         UndoPromptStrategy : SmallInt ;
         TrialTriggerSource : SmallInt ;
         StatisticsDisplayStrategy : SmallInt ;
         ExternalTagType : SmallInt ;
         Unused2034 : Array[1..14] of char ;

         end ;

    TCDRHeader = packed record         { DOS CDR data file header }
            Nrecords : SmallInt ;
            MaxRecords : SmallInt ;
            RecordingTime : single ;
            Cell : array[0..77] of char ;
            FileName : array[0..11] of char ;
            dt : single ;
            BitCurrent : single ;
            RangeVolts : single ;
            YUnits : array[0..1] of char ;
            TUnits : array[0..1] of char ;
            CalCurrent : single ;
            CalTime : single ;
            iCalCurrent : SmallInt ;
            iCalRecord : SmallInt ;
            iCalCursor : SmallInt ;
            GainCurrent : single ;
            iZeroLevel : SmallInt ;
            TriggerLevel : single ;
            TriggerTime : single ;
            DeadTime : single ;
            RunningMean : single ;
            nEvents : SmallInt ;
            TypeList : array[0..29] of char ;
            nTypes : SmallInt ;
            InterfaceCard : SmallInt ;
            Signature : array[0..3] of char ;
            end ;

TLDTFileHeader = packed record
           Leader : Cardinal ;
           SamplingInterval : Word ;
           Scaling : Word ;
           end ;

TLDTSegmentHeader = packed record
                  Start : Cardinal ;
                  NumSamples : Cardinal ;
                  end ;

procedure ExportToAxon( FileName : string ; StartBlock : Integer ; EndBlock : Integer ) ;

procedure AppendEDRfile( FileName : string ) ;
procedure ImportFromCFS( FileName : string ) ;
procedure ExportToLDT( FileName : string ; StartBlock : Integer ; EndBlock : Integer ;
                       UseChannel : Integer ) ;
procedure ExportToASCII(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer ;             { Last multi-channel block in sequence }
          UseChannels : Array of Boolean   { Channel to be output }
          ) ;

procedure ExportToEDR(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer              { Last multi-channel block in sequence }
          ) ;


implementation

uses mdiform ;


procedure ExportToAxon(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer               { Last multi-channel block in sequence }
          ) ;
{ ---------------------------------------------
  Convert an EDR data file into a pCLAMP ABF data file
  ---------------------------------------------}
const
     NumBlocksPerBuf = 256 ;
var
   DataStartsAtByte,FilePointer,NumBufsToCopy,NumBufsDone,NumBytesToWrite : LongInt ;
   FileHandle,i,ch,BlockPointer : Integer ;
   OldRange : single ;
   Buf : ^TSmallIntArray ;
   PC6Header : ^TABF ;
   s : string ;
   Day,Month,Year,Min,Hour,Sec,Msec : Word ;
   lDay,lMonth,lYear,lMin,lHour,lSec : LongInt ;
   OK,Done : Boolean ;
begin

     OK := True ;
     New(Buf) ;
     New(pc6Header) ;

     try


        { Open new pClamp file }
        FileHandle := FileCreate( FileName ) ;
        if FileHandle < 0 then begin
           MessageDlg('Cannot create file ' + FileName, mtError,[mbOK],0) ;
           OK := False ;
           end ;

        if OK then begin

          pc6Header^.FileType[1] := 'A' ;
          pc6Header^.FileType[2] := 'B' ;
          pc6Header^.FileType[3] := 'F' ;
          pc6Header^.FileType[4] := ' ' ;

          pc6Header^.FileVersionNumber := 1.5 ;
          pc6Header^.OperationMode := 3 ; {Gap free mode }
          pc6Header^.NumPointsIgnored := 0 ;

          DecodeDate( Now, Year, Month, Day ) ;
          lDay := Day ;
          lMonth := Month ;
          lYear := Year ;
          pc6Header^.FileStartDate := lDay + 100*lMonth + 10000*lYear ;
          DecodeTime( Now, Hour, Min, Sec, MSec ) ;
          lHour := Hour ;
          lMin := Min ;
          lSec := Sec ;
          pc6Header^.FileStartTime := lHour*3600 + lMin*60 + lSec ;

          pc6Header^.StopwatchTime := 0 ;
          pc6Header^.HeaderVersionNumber := 1.5 ;
          pc6Header^.nFileType := 1 ;
          pc6Header^.MSBinFormat := 0 ;

          pc6Header^.DataSectionPtr := 4 ;
          pc6Header^.TagSectionPtr := 0 ;
          pc6Header^.NumTagEntries := 0 ;
          pc6Header^.ScopeConfigPtr := 0 ;
          pc6Header^.NumScopes := 0 ;
          pc6Header^.DACFilePtr := 0 ;
          pc6Header^.DACFileNumEpisodes := 0 ;
          pc6Header^.DeltaArrayPtr := 0 ;
          pc6Header^.NumDeltas := 0 ;
          pc6Header^.VoiceTagPtr := 0 ;
          pc6Header^.VoiceTagEntries := 0 ;
          pc6Header^.SynchArrayPtr := 0 ;
          pc6Header^.SynchArraySize := 0 ;
          pc6Header^.DataFormat := 0 ;

          pc6Header^.ADCNumChannels := CdrFH.NumChannels ;
          pc6Header^.ADCSampleInterval := (CdrFH.dt*1E6)/CdrFH.NumChannels ; {in microsecs}
          pc6Header^.ADCSecondSampleInterval := 0. ;
          pc6Header^.SynchTimeUnit := 0. ;
          pc6Header^.SecondsPerRun := 0 ;

          pc6Header^.NumSamplesPerEpisode := 512*pc6Header^.ADCNumChannels ;
          pc6Header^.PreTriggerSamples := 50 ;
          pc6Header^.EpisodesPerRun := 1 ;
          pc6Header^.RunsPerTrial := 1 ;
          pc6Header^.NumberofTrials := 1 ;
          pc6Header^.AveragingMode := 0 ;
          pc6Header^.UndoRunCount := -1 ;
          pc6Header^.FirstEpisodeInRun := 1 ;
          pc6Header^.TriggerThreshold := 100 ;
          pc6Header^.TriggerSource := -2 ;
          pc6Header^.TriggerAction := 0 ;
          pc6Header^.TriggerPolarity := 0 ;
          pc6Header^.ScopeOutputInterval := 0. ;
          pc6Header^.EpisodeStartToStart := 1. ;
          pc6Header^.RunStartToStart := 1. ;
          pc6Header^.TrialStartToStart := 1. ;
          pc6Header^.AverageCount := 1 ;

          pc6Header^.DrawingStrategy := 1 ;
          pc6Header^.TiledDisplay := 0 ;
          pc6Header^.DataDisplayMode := 1 ;
          pc6Header^.DisplayAverageUpdate := -1 ;
          pc6Header^.ChannelStatsStrategy := 1 ;
          pc6Header^.CalculationPeriod := 16384 ;
          pc6Header^.SamplesPerTrace := 512 ;
          pc6Header^.StartDisplayNum := 1 ;
          pc6Header^.FinishDisplayNum := 0 ;
          pc6Header^.MultiColor := 1 ;
          pc6Header^.ShowPNRawData := 0 ;

          pc6Header^.ADCRange := CdrFH.ADCVoltageRange ;
          pc6Header^.DACRange := 10.24 ;
          pc6Header^.ADCResolution := Main.SESLabIO.ADCMaxValue+1 ; ;
          pc6Header^.DACResolution := Main.SESLabIO.ADCMaxValue+1 ; ;
          pc6Header^.AutoSampleEnable := 0 ;
          pc6Header^.AutoSampleAddItGain := 1. ;
          pc6Header^.AutoSampleADCNum := 0 ;

          pc6Header^.ExperimentType := 0 ;
          pc6Header^.AutoSampleEnable := 0 ;
          pc6Header^.AutoSampleADCNum := 0 ;
          pc6Header^.AutoSampleInstrument := 0 ;
          pc6Header^.AutoSampleAddItGain := 1. ;
          pc6Header^.AutoSampleFilter := 100000. ;
          pc6Header^.AutoSampleMembraneCap := 1. ;

          pc6Header^.ManualInfoStrategy := 0 ;
          pc6Header^.CellID1 := 1. ;
          pc6Header^.CellID2 := 2. ;
          pc6Header^.CellID3 := 3. ;

          { Name of program which created file }
          s := 'WinEDR' ;
          for i := 1 to High(pc6Header^.CreatorInfo) do begin
              pc6Header^.CreatorInfo[i] := ' ' ;
              if i < Length(s) then pc6Header^.CreatorInfo[i] := s[i];
              end ;

          { Experiment ident information }
          for i := 1 to High(pc6Header^.FileComment) do begin
              pc6Header^.FileComment[i] := ' ' ;
              if i < Length(CdrFH.IdentLine) then
                 pc6Header^.FileComment[i] := CdrFH.IdentLine[i];
              end ;

          { Analog input channel settings }

          for ch := 0 to 15 do begin

	      pc6Header^.ADCPToLChannelMap[ch] := ch ;
	      pc6Header^.ADCSamplingSeq[ch] := -1 ;

              for i := 1 to 10 do pc6Header^.ADCChannelName[ch,i] := ' ' ;
              s := format( 'Ch%d ',[ch] ) ;
              for i := 1 to 4 do pc6Header^.ADCChannelName[ch,i] := s[i] ;

              for i := 1 to 8 do pc6Header^.ADCUnits[ch,i] := ' ' ;
	      pc6Header^.ADCUnits[ch,1] := 'm' ;
              pc6Header^.ADCUnits[ch,2] := 'V' ;

	      pc6Header^.ProgrammableGain[ch] := 1. ;
	      pc6Header^.DisplayAmplification[ch] := 1. ;
	      pc6Header^.DisplayOffset[ch] := 0. ;
	      pc6Header^.InstrumentScaleFactor[ch] := 1. ;
	      pc6Header^.InstrumentOffset[ch] := 0. ;
	      pc6Header^.SignalGain[ch] := 1. ;
	      pc6Header^.SignalOffset[ch] := 0. ;
	      pc6Header^.SignalLowpassFilter[ch] := 100000. ;
	      pc6Header^.SignalHighpassFilter[ch] := 0. ;
              end ;

          { Analog output channel settings }

          for ch := 0 to 3 do begin
              for i := 1 to 10 do pc6Header^.DACChannelName[ch,i] := ' ' ;
              for i := 1 to 8 do pc6Header^.DACChannelUnits[ch,i] := ' ' ;
              pc6Header^.DACScaleFactor[ch] := 1. ;
              pc6Header^.DACHoldingLevel[ch] := 0. ;
              end ;

          pc6Header^.SignalType := 0 ;

          pc6Header^.OutEnable := 0 ;
          pc6Header^.SampleNumberOUT1 := 0 ;
          pc6Header^.SampleNumberOUT2 := 0 ;
          pc6Header^.FirstEpisodeOUT := 0 ;
          pc6Header^.LastEpisodeOut := 0 ;
          pc6Header^.PulseSamplesOUT1 := 0 ;
          pc6Header^.PulseSamplesOUT2 := 0 ;

          pc6Header^.DigitalEnable := 0 ;
          pc6Header^.WaveformSource := 0 ;
          pc6Header^.ActiveDACChannel := 0 ;
          pc6Header^.InterEpisodeLevel := 0 ;
          for i := 0 to High(pc6Header^.EpochType) do begin
              pc6Header^.EpochType[i] := 0 ;
              pc6Header^.EpochInitLevel[i] := 0. ;
              pc6Header^.EpochLevelInc[i] := 0. ;
              pc6Header^.EpochInitDuration[i] := 0 ;
              pc6Header^.EpochDurationInc[i] := 0 ;
              end ;
          pc6Header^.DigitalHolding := 0 ;
          pc6Header^.DigitalInterEpisode := 0 ;
          for i := 0 to High(pc6Header^.DigitalValue) do
              pc6Header^.DigitalValue[i] := 0 ;

          pc6Header^.DACFileStatus := 1. ;
          pc6Header^.DACFileOffset := 0. ;
          pc6Header^.DACFileEpisodeNum := 0 ;
          pc6Header^.DACFileADCNum := 0 ;
          for i := 1 to High(pc6Header^.DACFilePath) do
              pc6Header^.DACFilePath[i] := ' ' ;

          pc6Header^.ConditEnable := 0 ;
          pc6Header^.ConditChannel := 0 ;
          pc6Header^.ConditNumPulses := 0 ;
          pc6Header^.BaselineDuration := 1. ;
          pc6Header^.BaselineLevel := 0. ;
          pc6Header^.StepDuration := 1. ;
          pc6Header^.StepLevel := 0. ;
          pc6Header^.PostTrainPeriod := 1. ;
          pc6Header^.PostTrainLevel := 1. ;

          pc6Header^.ParamToVary := 0 ;
          for i := 1 to High(pc6Header^.ParamValueList) do
              pc6Header^.ParamValueList[i] := ' ' ;

          pc6Header^.AutoPeakEnable := 0 ;
          pc6Header^.AutoPeakPolarity := 0 ;
          pc6Header^.AutoPeakADCNum := 0 ;
          pc6Header^.AutoPeakSearchMode := 0 ;
          pc6Header^.AutoPeakStart := 0 ;
          pc6Header^.AutoPeakEnd := 0 ;
          pc6Header^.AutoPeakSmoothing := 1 ;
          pc6Header^.AutoPeakBaseline := -2 ;
          pc6Header^.AutoPeakAverage := 0 ;

          pc6Header^.ArithmeticEnable := 0 ;
          pc6Header^.ArithmeticUpperLimit := 1. ;
          pc6Header^.ArithmeticLowerLimit := 0. ;
          pc6Header^.ArithmeticADCNumA := 0 ;
          pc6Header^.ArithmeticADCNumB := 0 ;
          pc6Header^.ArithmeticK1 := 1. ;
          pc6Header^.ArithmeticK2 := 2. ;
          pc6Header^.ArithmeticK3 := 3. ;
          pc6Header^.ArithmeticK4 := 4. ;
          pc6Header^.ArithmeticOperator[1] := '+' ;
          pc6Header^.ArithmeticOperator[2] := ' ' ;
          for i := 1 to High(pc6Header^.ArithmeticUnits) do
              pc6Header^.ArithmeticUnits[i] := ' ' ;
          pc6Header^.ArithmeticK5 := 5. ;
          pc6Header^.ArithmeticK6 := 6. ;
          pc6Header^.ArithmeticExpression := 0 ;

          pc6Header^.PNEnable := 0 ;
          pc6Header^.PNPosition := 0 ;
          pc6Header^.PNPolarity := 1 ;
          pc6Header^.PNNumPulses := 4 ;
          pc6Header^.PNADCNum := 0 ;
          pc6Header^.PNHoldingLevel := 0. ;
          pc6Header^.PNSettlingTime := 100. ;
          pc6Header^.PNInterPulse := 100. ;

          pc6Header^.ListEnable := 0 ;

          { Channel scaling/units information }

          for ch := 0 to CdrFH.NumChannels-1 do begin
              pc6Header^.ADCSamplingSeq[ch] := Channel[ch].ChannelOffset ;
              pc6Header^.SignalGain[ch] := 1. ;
              pc6Header^.InstrumentScaleFactor[ch] := Channel[ch].ADCCalibrationFactor /
                                                      Channel[ch].ADCAmplifierGain ;

              for i := 1 to 8 do if i <= Length(Channel[ch].ADCUnits) then
                  pc6Header^.ADCUnits[ch,i] := Channel[ch].ADCUnits[i] ;

              for i := 1 to 10 do if i <= Length(Channel[ch].ADCName) then
                  pc6Header^.ADCChannelName[ch,i] := Channel[ch].ADCName[i] ;

              end ;

          { Write header block }
          FileSeek( FileHandle, 0, 0 ) ;
          if FileWrite(FileHandle,pc6Header^,Sizeof(pc6Header^))
             <> Sizeof(pc6Header^) then
             MessageDlg( 'Error writing to ' + FileName,mtWarning, [mbOK], 0 ) ;

          { Copy signal records from WinEDR to Axon data file }

          { Get A/D voltage range from record }
          pc6Header^.ADCRange := CdrFH.ADCVoltageRange ;

          { Move destination file pointer to data section of ABF file }
          DataStartsAtByte := 4*512 ;
          FileSeek( FileHandle, DataStartsAtByte, 0 ) ;

          { Copy records }
          NumBufsToCopy := (EndBlock - StartBlock) div NumBlocksPerBuf ;
          BlockPointer := StartBlock ;
          Done := False ;
          while not Done do begin

              { Read buffer from EDR file }
              ReadCDRBuffer( CdrFH, BlockPointer, Buf^, NumBlocksPerBuf ) ;

              { Write A/D samples to pClamp file }
              NumBytesToWrite := 2*NumBlocksPerBuf*CdrFH.NumChannels ;
              if FileWrite(FileHandle,Buf^,NumBytesToWrite)
                 <> NumBytesToWrite then begin
                 Main.StatusBar.SimpleText := 'Export aborted : Error writing to ' + FileName ;
                 Done := True ;
                 end ;

              BlockPointer := BlockPointer + NumBlocksPerBuf ;

              // Buffer counter
              Inc(NumBufsDone) ;
              if NumBufsDone = NumBufsToCopy then Done := True ;

              Main.StatusBar.SimpleText := format(
              ' Exporting to %s %d/%d (ABF format)',
              [FileName,
               NumBufsDone*NumBlocksPerBuf*CdrFH.NumChannels,
               NumBufsToCopy*NumBlocksPerBuf*CdrFH.NumChannels]) ;

              end ;

         if NumBufsDone = NumBufsToCopy then begin
              // Final export report
              Main.StatusBar.SimpleText := format(
              ' Export Complete : %d samples exported to %s (ABF format)',
              [NumBufsDone*NumBlocksPerBuf*CdrFH.NumChannels,
               FileName]) ;
              end
         else begin
              // Error report
              Main.StatusBar.SimpleText := format(
              ' Export Aborted : %d/%d samples exported to %s (ABF format)',
              [NumBufsDone*NumBlocksPerBuf*CdrFH.NumChannels,
               NumBufsToCopy*NumBlocksPerBuf*CdrFH.NumChannels,
               FileName]) ;
              end ;

          { Update header with number of records written }
          pc6Header^.ActualAcqLength := (BlockPointer - StartBlock)
                                        *pc6Header^.ADCNumChannels ;
          pc6Header^.ActualEpisodes := 1 ;
          { Write header to Axon file }
          FileSeek( FileHandle, 0, 0 ) ;
          if FileWrite(FileHandle,pc6Header^,Sizeof(pc6Header^))
             <> Sizeof(pc6Header^) then
             MessageDlg( 'Error writing to file.',mtWarning, [mbOK], 0 ) ;

          WriteToLogFile( 'File : ' + CdrFH.FileName ) ;
          WriteToLogFile( 'converted to Axon (ABF) file : ' + FileName ) ;

          end ;
     finally
         Dispose(pc6Header) ;
         Dispose(Buf) ;
         { Close files }
         if FileHandle >= 0 then  FileClose( FileHandle ) ;
         end ;

     end ;


procedure ImportFromCFS( FileName : string ) ;
{ ------------------------------------------------------------------
  Convert a Cambridge Electronic Design data file into WCD data file
  ------------------------------------------------------------------
  24/6/98 ... Bad points at end of data now eliminated }
type
    TChannelDef = packed record
                ChanName : String[21] ;
                UnitsY : String[9] ;
                UnitsX : String[9] ;

                dType : Byte ;
	        dKind : Byte ;
	        dSpacing : Word ;
                OtherChan : Word ;
                end ;

    TChannelInfo = packed record
	        DataOffset : LongInt ; {offset to first point}
	        DataPoints : LongInt ; {number of points in channel}
	        scaleY : single ;
	        offsetY : single ;
	        scaleX : single ;
	        offsetX : single ;
                end ;

    TDataHeader = packed record
	lastDS : LongInt ;
	dataSt : LongInt ;
	dataSz : LongInt ;
	Flags : Word ;
      	Space : Array[1..8] of Word ;
        end ;

    TCFSFileHeader = packed record
	Marker : Array[1..8] of char ;
	Name : Array[1..14] of char ;
	FileSz : LongInt ;
        TimeStr : Array[1..8] of char ;
	DateStr : Array[1..8] of char ;
	DataChans : SmallInt ;
	FilVars : SmallInt ;
	DatVars : SmallInt ;
	fileHeadSz : SmallInt ;
	DataHeadSz : SmallInt ;
	EndPnt : LongInt ;
	DataSecs : SmallInt ;
	DiskBlkSize : SmallInt ;
	CommentStr : String[73] ;
	TablePos : LongInt ;
	Fspace : Array[1..20] of Word ;
        end ;

var
   ChannelDef : TChannelDef ;
   ChannelInfo : TChannelInfo ;
   CFSFileHeader : TCFSFileHeader ;
   RecHeader : TDataHeader ;
   FilePointer,DataPointer : LongInt ;
   Rec,MaxSamples,MinSamples,ChMax : LongInt ;
   FileHandle,i,LastChannel,Src,Dest,SrcStep,iOff,nSamples : Integer ;
   NumBytesInBuf,EndOfBuf,NumBytesInCEDBuf,EndOfCEDBuf,NumBytesDone  : Integer ;
   NumBufsToCopy,NumBufsDone : LongInt ;
   DataSecsDone : Integer ;
   ADCScale : single ;
   OK,Done : boolean ;
   nn,Ch : Integer ;
   Buf,CEDBuf : ^TIntArray ;
   NumSamplesInChannel :  Array[0..ChannelLimit] of LongInt ;
   DataOffset :  Array[0..ChannelLimit] of LongInt ;
   SampleSpacing :  Array[0..ChannelLimit] of LongInt ;
   SamplingInterval : Array[0..ChannelLimit] of Single ;
   x : single ;
   s : string ;
   TimeUnits : string ;
   TScale,ScaleBits : single ;
begin

     OK := True ;
     { Create buffers }
     New(Buf) ;
     New(CEDBuf) ;
     { Ensure file is closed }
     if CdrFH.FileHandle >= 0 then FileClose( CdrFH.FileHandle ) ;

     try

        { Make sure an existing data file is not overwritten, unintentionally }
        CdrFH.FileName := ChangeFileExt(FileName,DataFileExtension) ;
        OK := FileOverwriteCheck(CdrFH.FileName ) ;

        if OK then begin
           { Open CFS data file }
           FileHandle := FileOpen( FileName, fmOpenRead ) ;
           if FileHandle < 0 then begin
              OK := False ;
              MessageDlg( 'Unable to open ' + FileName ,mtWarning, [mbOK], 0) ;
              end ;
           end ;

        if OK then begin
           {  Read CFS file header block }
           FileSeek( FileHandle, 0, 0 ) ;
           if FileRead(FileHandle,CFSFileHeader,Sizeof(CFSFileHeader))
              <> Sizeof(CFSFileHeader) then begin
              MessageDlg( FileName + ' - CFS Header unreadable',mtWarning, [mbOK], 0) ;
              OK := False ;
              end ;
           end ;

        { Check that this is a CFS data file }
        if OK then begin
           s := '' ;
           for i := 1 to High(CFSFileHeader.Marker) do
               s := s + CFSFileHeader.Marker[i] ;
           if Pos('CEDFILE',s) = 0 then begin
              MessageDlg( FileName + ' : Not a CFS data file',mtWarning,[mbOK],0) ;
              OK := False ;
              end ;
           end ;

        { Get data from header block }
        if OK then begin
            { No. of analog input channels held in file }
            if CFSFileHeader.DataChans > (ChannelLimit+1) then
               MessageDlg( format('Input channels 7-%d ignored',
                                         [CFSFileHeader.DataChans-1]),
                                         mtWarning, [mbOK], 0 ) ;
            { Number of analog input channels }
            CdrFH.NumChannels := IntLimitTo( CFSFileHeader.DataChans,
                                             1, ChannelLimit+1 ) ;
            { Last channel number }
            LastChannel := CdrFH.NumChannels - 1 ;

            { Get experiment identification text }
            CdrFH.IdentLine := CFSFileHeader.CommentStr ;
            WriteToLogFile( 'CFS File : ' + FileName ) ;
	    WriteToLogFile( CdrFH.IdentLine ) ;
	    WriteToLogFile( CFSFileHeader.TimeStr ) ;
	    WriteToLogFile( CFSFileHeader.DateStr ) ;

            { A/D converter input voltage range }
	    CdrFH.ADCVoltageRange := 5.0 ;
            // Convert from 16 bit CFS format to 16 or 12 bit WCP
            ScaleBits := 32768.0 / (Main.SESLabIO.ADCMaxvalue+1) ;

            { Read Channel definition records }
	    for Ch := 0 to LastChannel do begin
                { Read signal channel definition record }
                if FileRead(FileHandle,ChannelDef,Sizeof(ChannelDef))
                   = Sizeof(ChannelDef) then begin
                   { Name of signal channel }
                   Channel[Ch].ADCName := ChannelDef.ChanName ;
                   { Units of signal channel }
                   Channel[Ch].ADCUnits := ChannelDef.UnitsY ;

                   Channel[Ch].ADCMaxValue := Main.SESLabIO.ADCMaxvalue ;

                   { Time units }
                   TimeUnits := ChannelDef.UnitsX ;
                   { Determine scaling to secs factor }
                   if Pos( 'us', TimeUnits ) > 0 then TScale := 1E-6
                   else if Pos( 'ms', TimeUnits ) > 0 then TScale := 1E-3
                                                      else TScale := 1. ;

                   SampleSpacing[Ch] := ChannelDef.dSpacing ;
                   end
                else MessageDlg(
                     format( 'Ch.%d definition record unreadable',[Ch]),
                    mtWarning, [mbOK], 0 ) ;
                end ;
            end ;

        { Create a data file to hold converted data }
        if OK then begin
            CdrFH.FileName := ChangeFileExt( FileName, DataFileExtension ) ;
            OK := Main.CreateNewDataFile( CdrFH ) ;
            end ;

        { Read data records from CFS file }
        if OK then begin

           Rec := 1 ;
           DataSecsDone := 0 ;
           While  DataSecsDone < CFSFileHeader.DataSecs do begin

                { Get pointer to start of data record #Rec }
                FileSeek( FileHandle,CFSFileHeader.TablePos + (Rec-1)*4, 0 ) ;
                FileRead(FileHandle,DataPointer,SizeOf(DataPointer)) ;

                { Read record data header }
                FileSeek( FileHandle, DataPointer, 0 ) ;
                FileRead(FileHandle,RecHeader,SizeOf(RecHeader)) ;

                { Read channel offset/scaling information records
                  which follow data record }
                MaxSamples := 0 ;
                MinSamples := High(MinSamples) ;
	        for Ch := 0 to LastChannel do begin

                    { Read channel definition record }
                    FileRead(FileHandle,ChannelInfo,SizeOf(ChannelInfo)) ;

                    { Derive WCP's calibration factors from first record
                      scaling factor }
                    If Rec = 1 then begin
                       { Get signal bits->units scaling factor }
                       Channel[ch].ADCScale := ChannelInfo.ScaleY * ScaleBits ;
                       { Note. Multiplcation  by ScaleBits to
                         account for conversion from
                         CED's +-32767 data range to +/-2048}
                       { Calculate calibration factors }
                       Channel[ch].ADCCalibrationFactor := CdrFH.ADCVoltageRange /
                                      ( Channel[ch].ADCScale * (Channel[Ch].ADCMaxValue+1) ) ;
                       Channel[ch].ADCAmplifierGain := 1. ;
                       Channel[ch].ADCScale := CalibFactorToADCScale(
                                               CdrFH.ADCVoltageRange,
                                               Channel[ch] ) ;
                       end ;

                    { Offset into groups of A/D samples for this channel }
                    Channel[ch].ChannelOffset := Ch ;

                    { Inter sample interval }
		    SamplingInterval[ch] := ChannelInfo.scaleX*TScale ;

                    { Number of samples in this channel }
		    NumSamplesInChannel[Ch] := ChannelInfo.DataPoints ;

                    { Determine maximum and minimum no. of channels
                      in record }
                    if NumSamplesInChannel[ch] > MaxSamples then begin
                       MaxSamples := NumSamplesInChannel[ch] ;
                       ChMax := ch ;
                       end ;
                    if NumSamplesInChannel[ch] < MinSamples then
                       MinSamples := NumSamplesInChannel[ch] ;

                    { Offset (bytes) of samples for this channel in
                      record data area }
		    DataOffset[Ch] := ChannelInfo.DataOffset ;

                    end ;


                if MaxSamples <> MinSamples then begin
                   CdrFH.NumChannels := 1 ;
                   NumSamplesInChannel[0] := NumSamplesInChannel[ChMax] ;
                   DataOffset[0] := DataOffset[ChMax] ;
                   SampleSpacing[0] := SampleSpacing[ChMax] ;
                   SamplingInterval[0] := SamplingInterval[ChMax] ;
                   Channel[0].ADCScale := Channel[ChMax].ADCScale ;
                   Channel[0].ADCCalibrationFactor :=
                                                   Channel[ChMax].ADCCalibrationFactor ;
                   Channel[0].ADCAmplifierGain := Channel[ChMax].ADCAmplifierGain ;
                   Channel[0].ADCName := Channel[ChMax].ADCName ;
                   Channel[0].ADCUnits := Channel[ChMax].ADCUnits ;
                   end ;

                for ch := 0 to CdrFH.NumChannels-1 do begin

                    { Read binary data from CED data file into CED record buffer }
                    FileSeek( FileHandle,RecHeader.DataSt+DataOffset[ch],0) ;

                    NumBytesInCEDBuf := 512 ;
                    EndOfCEDBuf := (NumBytesInCEDBuf div 2) - 1 ;
                    NumBytesInBuf := 512*CdrFH.NumChannels ;
                    EndOfBuf := (NumBytesInBuf div 2) - 1 ;
                    NumBufsToCopy := (RecHeader.DataSz div NumBytesInCEDBuf) ;
                    NumBufsDone := 0 ;
                    Done := False ;
                    Src := EndOfCEDBuf+1 ;
                    SrcStep := SampleSpacing[0] div 2 ;
                    Dest := Channel[ch].ChannelOffset ;
                    repeat
                          { Read from CFS file }
                          if Src > EndofCEDBuf then begin
                             if FileRead(FileHandle,CEDBuf^,NumBytesinCEDBuf)
                                <> NumBytesinCEDBuf then Done := True ;
                             Src := Src - EndOfCEDBuf - 1 ;
                             end ;

                          { Re-scale samples from 16 -> 12 bits }
                          Buf^[Dest] := Round( CEDBuf^[Src] / ScaleBits ) ;
                          Src := Src + SrcStep ;

                          { Write to WCD file }
                          Dest := Dest + CdrFH.NumChannels ;
                          if Dest > EndOfBuf then begin
                             FileWrite( CdrFH.FileHandle,Buf^,NumBytesInBuf ) ;
                             Dest := Dest - EndOfBuf - 1  ;
                             cdrFH.NumSamplesInFile := cdrFH.NumSamplesInFile
                                                       + (EndOfBuf+1) ;
                             Inc(NumBufsDone) ;
                             if NumBufsDone >= NumBufsToCopy then Done := True ;
                             end ;

                          until done ;
                    end ;

                Inc(Rec) ;
                Inc(DataSecsDone) ;

                // Report progress
                Main.StatusBar.SimpleText := format(
                ' Importing %d/%d data sectors from %s (CED CFS file)',
                [DataSecsDone,CFSFileHeader.DataSecs,FileName]);

                end ;

           // Final report
           Main.StatusBar.SimpleText := format(
           ' Import complete : %d data sectors from %s (CED CFS file)',
           [DataSecsDone,FileName]);

           CdrFH.dt := SamplingInterval[0] ;
           if CdrFH.dt = 0.0 then CdrFH.dt := 1.0 ;

           { Save file header }
           SaveCDRHeader( CdrFH ) ;

           WriteToLogFile( 'converted to : ' + CdrFH.FileName ) ;

           end ;

     finally
           { Close files }
           if FileHandle >= 0 then FileClose( FileHandle ) ;
           if CdrFH.FileHandle >= 0 then FileClose( CdrFH.FileHandle ) ;
           { Free buffers }
           Dispose(Buf) ;
           Dispose(CEDBuf) ;
           end ;
     end ;


procedure AppendEDRFile( FileName : string ) ;
{ --------------------------------------------------------
  Append data from an EDR file into the currently open one
  -------------------------------------------------------- }
Const
     NumBlocksPerBuf = 256 ;
var
   NumBuffersToAdd,NumBlocksRead,NumBlocksWrite : Integer ;
   NumBuffersDone : Integer ;
   OK, Done : boolean ;
   Buf : ^TSmallIntArray ;
   AppFH : TCDRFileHeader ;
begin

     OK := True ;
     New(Buf) ;
     AppFH.FileHandle := -1 ;
     if CdrFH.FileHandle >= 0 then begin
        FileClose( CdrFH.FileHandle ) ;
        CdrFH.FileHandle := -1 ;
        end ;

     try

        { Open data file to be appended }
        AppFH.FileName := FileName ;
        AppFH.FileHandle := FileOpen( AppFH.FileName, fmOpenRead ) ;
        if AppFH.FileHandle < 0 then begin
           OK := False ;
           MessageDlg( 'Unable to open ' + AppFH.FileName,mtWarning, [mbOK],0) ;
           end ;

        { Open data file to receive append }
        CdrFH.FileHandle := FileOpen( CdrFH.FileName, fmOpenReadWrite ) ;
        GetCDRHeader( CdrFH ) ;
        if CdrFH.FileHandle >= 0 then begin
           GetCDRHeader( CdrFH ) ;
           end
        else begin
           OK := False ;
           MessageDlg( 'Unable to open ' + CdrFH.FileName,mtWarning, [mbOK],0) ;
           end ;

        { Read EDR file header block from source file }
        if OK then begin
          GetCDRHeader( AppFH ) ;
          if (AppFH.NumChannels = CdrFH.NumChannels) and
             (AppFH.dt = CdrFH.dt) then OK := True
                                   else OK := False ;
          end ;

        if OK then begin

          NumBuffersToAdd := AppFH.NumSamplesInFile div
                             (AppFH.NumChannels*NumBlocksPerBuf) ;
          NumBlocksRead := 0 ;
          NumBlocksWrite := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
          NumBuffersDone := 0 ;
          Done := False ;
          While  not Done do begin

              { Write samples to destination file }
              ReadCDRBuffer( AppFH, NumBlocksRead, Buf^, NumBlocksPerBuf ) ;
              WriteCDRBuffer( CdrFH, NumBlocksWrite, Buf^, NumBlocksPerBuf ) ;
              NumBlocksRead := NumBlocksRead + NumBlocksPerBuf ;
              NumBlocksWrite := NumBlocksWrite + NumBlocksPerBuf ;
              Inc(NumBuffersDone) ;

             // Report progress
             Main.StatusBar.SimpleText := format(
             ' Appending samples %d/%d from %s',
             [NumBuffersDone*NumBlocksPerBuf,
              NumBuffersToAdd*NumBlocksPerBuf,
              FileName]) ;

              if NumBuffersDone >= NumBuffersToAdd then Done := True ;
              end ;

          { Save file header }
          CdrFH.NumSamplesInFile := NumBlocksWrite*CdrFH.NumChannels ;
          SaveCDRHeader( CdrFH ) ;
          WriteToLogFile( FileName + ' appended to ' + CdrFH.FileName ) ;

          // Report progress
          Main.StatusBar.SimpleText := format(
          ' %d samples appended from %s',
          [NumBuffersDone*NumBlocksPerBuf,
          FileName]) ;

          end ;

     finally
        { Close files }
        if CdrFH.FileHandle >= 0 then begin
           FileClose( CdrFH.FileHandle ) ;
           CdrFH.FileHandle := -1 ;
           end ;
        if AppFH.FileHandle >= 0 then begin
           FileClose( AppFH.FileHandle ) ;
           AppFH.FileHandle := -1 ;
           end ;
        Dispose(Buf) ;
        end ;

     end ;


procedure ExportToLDT(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer ;             { Last multi-channel block in sequence }
          UseChannel : Integer 
          ) ;
{ -----------------------
  Export data to LDT file
  ----------------------- }
Const
     NumSamplesPerRecord = 512 ;
     NumBytesPerRecord = NumSamplesPerRecord*2 ;

var
   StartAt,i,j,ch,NumBufsDone,NumBufsToCopy : Integer ;
   FileHandle : Integer ;
   OK,Done : Boolean ;
   Buf : ^TSmallIntArray ;
   LDTFileHeader : TLDTFileHeader ;
   LDTSegmentHeader : TLDTSegmentHeader ;
begin

     OK := True ;
     New(Buf) ;

     try

        { Open new LDT file }
        FileHandle := FileCreate( FileName ) ;
        if FileHandle < 0 then begin
           MessageDlg('Cannot create file ' + FileName, mtError,[mbOK],0) ;
           OK := False ;
           end ;

        { Write file header to LDT file }
        if OK then begin
           { Create LDT file header }
           LDTFileHeader.Leader := 1000 ;
           LDTFileHeader.SamplingInterval := Round(CdrFH.dt*SecsTous) ;
           Channel[UseChannel].ADCScale := CalibFactorToADCScale(
                                           CdrFH.ADCVoltageRange,
                                           Channel[UseChannel] ) ;

           LDTFileHeader.Scaling := Round(100.0/Channel[UseChannel].ADCScale) ;
           { Move pointer to start of O/P file }
           FileSeek( FileHandle, 0, 0 ) ;
           { Write file header block }
           if FileWrite(FileHandle,LDTFileHeader,SizeOf(LDTFileHeader))
              <> SizeOf(LDTFileHeader) then begin
              MessageDlg('Unable to write LDT file header ', mtError,[mbOK],0) ;
              OK := False ;
              end ;
           end ;

        { Write one segment containing A/D samples }
        if OK then begin
           { Write Segment header }
           LDTSegmentHeader.Start := 0 ;
           LDTSegmentHeader.NumSamples := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
           if FileWrite(FileHandle,LDTSegmentHeader,SizeOf(LDTSegmentHeader))
              <> SizeOf(LDTSegmentHeader) then
              WriteToLogFile('Unable to write LDT segment header to ' + FileName) ;

           { Read in segment }
           StartAt := 0 ;
           NumBufsToCopy := CDRfH.NumSamplesInFile
                            div (NumSamplesPerRecord*CdrFH.NumChannels) ;
           NumBufsDone := 0 ;
           Done := False ;
           While not Done do begin

               { Read buffer from EDR file }
               ReadCDRBuffer( CdrFH, StartAt, Buf^, NumSamplesPerRecord ) ;
               StartAt := StartAt + NumSamplesPerRecord ;

               { Extract selected channel }
               j := UseChannel ;
               for i:= 0 to NumSamplesPerRecord-1 do begin
                   Buf^[i] := Buf^[j] ;
                   j := j + CdrFH.NumChannels ;
                   end ;

               { Write record to LDT file }
               if FileWrite(FileHandle,Buf^,NumBytesPerRecord)
                  <> NumBytesPerRecord then
                  WriteToLogFile('Write error to ' + FileName) ;

               { Increment counters }
               Inc(NumBufsDone) ;
               if NumBufsDone >= NumBufsToCopy then Done := True ;

               // Report progress
               Main.StatusBar.SimpleText := format(
               ' Exporting records %d/%d to %s (LDT format)',
               [NumBufsDone*NumSamplesPerRecord,
                NumBufsToCopy*NumSamplesPerRecord,
                FileName]) ;


               end ;

           // Report progress
           Main.StatusBar.SimpleText := format(
           ' %d samples exported to %s (LDT format)',
           [NumBufsDone*NumSamplesPerRecord,
            FileName]) ;

           end ;

     finally
        if FileHandle >= 0 then FileClose(FileHandle) ;
        Dispose(Buf) ;
        end ;
     end ;


procedure ExportToASCII(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer ;              { Last multi-channel block in sequence }
          UseChannels : Array of Boolean  { Channel to be output }
          ) ;
{ -----------------------
  Export data to ASCII file
  ----------------------- }
Const
     NumSamplesPerBuffer = 512 ;

var
   SampleNum,i,ch : Integer ;
   LineText : String ;
   Buf : ^TSmallIntArray ;
   ExportFile : TextFile ;
   t : single ;
   Done,NewBufferNeeded : Boolean ;
begin

     New(Buf) ;

     { Open new ASCII file }
     AssignFile(ExportFile, FileName );
     Rewrite( ExportFile ) ;

     try

        SampleNum := StartBlock ;
        NewBufferNeeded := True ;
        t := 0.0 ;
        Done := False ;
        While  not Done do begin

            { Read buffer from EDR file }
            if NewBufferNeeded then begin
               ReadCDRBuffer( CdrFH, SampleNum, Buf^, NumSamplesPerBuffer ) ;

               { Increment counters }
               Main.StatusBar.SimpleText := format(
               ' Export : %.1f/%.1f s exported to %s (ASCII text format).',
               [SampleNum*CdrFH.dt,EndBlock*CdrFH.dt,FileName] ) ;

               Application.ProcessMessages ;
               NewBufferNeeded := False ;
               i := 0 ;
               end ;

            { Write sample values in buffer as tab-text table }
            LineText := format( '%.5g', [t*Settings.TScale] ) ;
            // Write selected channels
            for ch := 0 to CdrFH.NumChannels-1 do begin
                if UseChannels[ch] then
                   LineText := LineText + format( #9'%.5g',
                            [Channel[ch].ADCScale*(Buf^[i]-Channel[ch].ADCZero)]) ;
                Inc(i) ;
                end ;
            WriteLn( ExportFile, LineText ) ;

            Inc(SampleNum) ;
            if i >= (CdrFH.NumChannels*NumSamplesPerBuffer) then NewBufferNeeded := True ;
            t := t + CdrFH.dt ;

            if SampleNum > EndBlock then Done := True ;
            end ;

     finally

        CloseFile(ExportFile) ;
        Dispose(Buf) ;

        // Final status report
        Main.StatusBar.SimpleText := format(
        ' Export : %.1f-%.1f s (%d samples) exported to %s (ASCII text format).',
        [StartBlock*CdrFH.dt,EndBlock*CdrFH.dt,(EndBlock-StartBlock+1),FileName] ) ;

        end ;
     end ;


procedure ExportToEDR(
          FileName : string ;              { Output file name }
          StartBlock : Integer ;           { First multi-channel block }
          EndBlock : Integer               { Last multi-channel block in sequence }
          ) ;
{ -----------------------
  Export data to EDR file
  ----------------------- }
Const
     NumSamplesPerRecord = 256 ;
     NumBytesPerRecord = NumSamplesPerRecord*2 ;

var
   OK,Done : Boolean ;
   Buf : ^TSmallIntArray ;   // Data buffer
   OutFH : TCDRFileHeader ;  // File header for export destination file
   InPointer : Integer ;     // File read pointer
   OutPointer : Integer ;    // File write pointer
begin

     OK := True ;
     New(Buf) ;

     OutFH := CDRFH ;
     OutFH.FileHandle := -1 ;     // Note. Important to ensure file handle is invalid
     OutFH.NumSamplesInFile := 0 ; // No samples yet in export file
     OutFH.FileName := FileName ;

     try

        { Open new EDR file }
        OK := Main.CreateNewDataFile( OutFH ) ;

        { Write one segment containing A/D samples }
        if OK then begin

           { Copy block to export file }
           Done := False ;
           InPointer := StartBlock ;
           While not Done do begin

               { Read buffer from EDR file }
               if ReadCDRBuffer( CdrFH, StartBlock, Buf^, NumSamplesPerRecord )
                  >= NumSamplesPerRecord then begin
                  // Write to export file
                  WriteCDRBuffer( OutFH, OutPointer, Buf^, NumSamplesPerRecord ) ;
                  // Increment pointers
                  InPointer := InPointer + NumSamplesPerRecord ;
                  OutPointer := OutPointer + NumSamplesPerRecord ;
                  // Report progress
                  Main.StatusBar.SimpleText := format(
                  ' Export : Exporting %.3f/%.3fs to %s (EDR format)',
                  [InPointer*CDRFH.dt,EndBlock *CDRFH.dt,FileName]);

                  if InPointer > EndBlock then Done := True ;
                  end
               else Done := True ;

               end ;

           OutFH.NumSamplesInFile := OutPointer*OutFH.NumChannels ;
           SaveCDRHeader( OutFH ) ;

           Main.StatusBar.SimpleText := format(
           ' Export : %.3f-%.3fs exported to %s (EDR format)',
                  [StartBlock*CDRFH.dt,EndBlock*CDRFH.dt,FileName]);
           WriteToLogFile( Main.StatusBar.SimpleText ) ;

           end ;

     finally
        if OutFH.FileHandle >= 0 then FileClose(OutFH.FileHandle) ;
        Dispose(Buf) ;
        end ;
     end ;

end.
