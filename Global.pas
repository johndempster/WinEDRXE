unit Global;
{ ==============================================
  WinCDR - Global variables and type definitions
  (c) J. Dempster 1998
  ==============================================
  3.03.04 Channel limit increased to 11, defined by EDRChannelLimit
  06.08.04 Settings.ExternalTriggerActiveHigh added
  30.07.06 RecChannel added
  08.03.10 ... Settings.EventDetector.AvgFrequencyInterval added

  20.04.11 ... THistogram size increased to 4000 }
interface

uses sysUtils, Graphics, Classes, stdctrls, ced1401, maths, wintypes ;

const
     FileVersion = 7.1 ;
     EventFileExtension = '.EDE' ;
     DataFileExtension = '.EDR' ;
     MaxChannels = 16 ;
     EDRChannelLimit = MaxChannels-1 ;
     MinDT = 1.5E-5 ;
     NumDACChannels = 2 ;

     NumSamplesPerSector = 256 ;
     NumBytesPerSector = NumSamplesPerSector*2 ;
     NumBytesInHeader = 2048 ;
     MaxMarkers = 20 ;
     MaxTBuf = {29951} 32767 ;
     VoltsTomV = 1000. ;
     mVToVolts = 1E-3 ;
     AmpsTonA = 1E9 ;
     nAToAmps = 1E-9 ;
     nSToSeimens = 1E-9 ;
     msToSecs = 1E-3 ;
     SecsToms = 1E3 ;
     usToSecs = 1E-6 ;
     SecsTous = 1E6 ;
     pFToFarad = 1E-12 ;
     FaradTopF = 1E12 ;
     MinSingle = 1.5E-45 ;

     { WinWCP V3.0 constants }
     WCPChannelLimit = 7 ;
     NumAnalysisBytesPerWCPRecord = 1024 ;
     NumBytesInWCPFileHeader = 1024 ;
     VarLimit = 13 ;
     vRecord = 0 ;
     vGroup = 1 ;
     vTime = 2 ;
     vAverage = 3 ;
     vArea = 4 ;
     vPeak = 5 ;
     vVariance = 6 ;
     vRiseTime = 7 ;
     vRateofRise = 8 ;
     vLatency = 9 ;
     vT50 = 10 ;
     vT90 = 11 ;
     vInterval = 12 ;
     vBaseline = 13 ;

     FitVarLimit = 10 ;

     BitMapsMinSize = 16 ;
     BitMapsMaxSize = 10000 ;
     EmptyFlag = 32767 ;
     MaxSingle = 16382 ;

type
    TDestination = (ToPrinter,ToClipboard) ;
    TIntArray = Array[0..MaxTBuf] of SmallInt ;
    TSmallIntArray = Array[0..MaxTBuf] of SmallInt ;
    TSingleArray = Array[0..MaxTBuf] of Single ;
    TLongIntArray = Array[0..MaxTBuf] of LongInt ;
    TBooleanArray = Array[0..MaxTBuf] of Boolean ;
    TSmallIntArrayDyn = Array[0..99999999] of SmallInt ;
    PSmallIntArrayDyn = ^TSmallIntArrayDyn ;
    TSIngleArrayDyn = Array[0..99999999] of Single ;
    PSingleArrayDyn = ^TSIngleArrayDyn ;

    THeapBufferOp = (Allocate,Deallocate) ;
  {  TADCRange = string[20] ;}
    TSingleXY = record
                 x : single ;
                 y : single ;
                 end ;
    TSingleXYArray = Array[0..((MaxTBuf)-1)] of TSingleXY ;


{ WinEDR V2.0 data file header block }
TCDRFileHeader = record
            FileName : string ;
            WCPFileName : string ;
            FileHandle : integer ;
            FilePointer : Integer ;
            NumSamples : Integer ;
            NumChannels : Integer ;
            NumSamplesInFile : Integer ;
            NumBlocksInFile : Integer ;
            NumSamplesPerBlock : Integer ;
            NumBytesPerBlock : Integer ;
            NumBytesInHeader : Integer ;
            RecordDuration : single ;
            RecordNum : Integer ;
            dt : single ;
            ADCVoltageRange : single ;
            ADCMaxValue : Integer ;
            IdentLine : string ;
            Version : Single ;
            BackedUp : Boolean ;
            SaveHeader : Boolean ;
            CreationTime : string ;
            end ;


TString4 = string[4] ;
TString6 = string[6] ;
TString8 = string[8] ;
TChannel = record
         xMin : single ;
         xMax : single ;
         yMin : single ;
         yMax : single ;
         xScale : single ;
         yScale : single ;
         Left : Integer ;
         Right : Integer ;
         Top : Integer ;
         Bottom : Integer ;
         TimeZero : single ;
         ADCZero : Integer ;
         ADCZeroAt : Integer ;
         ADCSCale : single ;
         ADCCalibrationFactor : single ;
         ADCCalibrationBar : single ;
         ADCAmplifierGain : single ;
         ADCMaxValue : Integer ;
         ADCUnits : string ;
         ADCName : string ;
         InUse : Boolean ;
         ChannelOffset : Integer ;
         CursorIndex : Integer ;
         ZeroIndex : Integer ;
         CursorTime : single ;
         CursorValue : single ;
         Cursor0 : Integer ;
         Cursor1 : Integer ;
         TZeroCursor : Integer ;
         color : TColor ;
         end ;


THistCursorPos = record
           Enabled : Boolean ;
           Selected : Boolean ;
           xPix : Integer ;
           xVal : single ;
           index : Integer ;
           Lab : TLabel ;
           Color : TColor ;
           end ;

    { Histogram record }
    TBin = record
         Lo : Single ;
         Mid : Single ;
         Hi :  Single ;
         y : Single ;
         end ;


THistogram = class(TObject)
              StartAt : LongInt ;
              EndAt : LongInt ;
              RecordStart : LongInt ;
              RangeLo : single ;
              RangeHi : Single ;
              NumBins : Integer ;
              NumLogBinsPerDecade : Integer ;
              MaxBin : Integer ;
              Bins : Array[0..3999] of TBin ;
              TotalCount : single ;
              Available : Boolean ;
            {  Equation : TEquation ;}
              UnitCursor : THistCursorPos ;
              yHi : Single ;
              BinWidth : single ;
              BinScale : single ;
              TMin : single ;
              NewPlot : Boolean ;
              end ;

TSealTest = record
          CurrentChannel : Integer ;
          VoltageChannel : Integer ;
          HoldingVoltage1 : single ;
          HoldingVoltage2 : single ;
          HoldingVoltage3 : single ;
          PulseHeight : single ;
          PulseHeight1 : single ;
          PulseHeight2 : single ;
          PulseHeight3 : single ;
          PulseWidth : single ;
          Use : Integer ;
          AutoScale : Boolean ;
          DACNum : Integer ;
          FirstSweep : Boolean ;
          SmoothingFactor : Single ;
          end ;

TVCommand = record
          DivideFactor : single ;
          HoldingVoltage : single ;
          HoldingVoltageAlt : single ;
          HoldingVoltageIncrement : single ;
          end ;

//TDigitalPort = record
//             Value : LongInt ;
//             end ;

TEventDetector = record
               Channel : Integer ;
               yThreshold : single ;
               tThreshold : single ;
               DeadTime : single ;
               BaselineAverage : single ;
               RecordSize : Integer ;
               PreTriggerFraction : single ;
               AnalysisWindow : Single ;
               PositivePeaks : Boolean ;
               BaselineAtStart : Boolean ;
               SubtractBaseline : Boolean ;
               NumBaselinePoints : Integer ;
               NumBaselineGap : Integer ;
               TDecayPercent : Single ;
               TDecayFrom : Integer ;
               RisingEdgeWindow : Integer ;
               AvgFrequencyInterval : Single ;
               end ;

TPageSettings = record
              FontName : string ;
              FontSize : Integer ;
              LineThickness : Integer ;
              ShowLines : boolean ;
              MarkerSize : Integer ;
              ShowMarkers : boolean ;
              LeftMargin : single ;
              RightMargin : single ;
              TopMargin : single ;
              BottomMargin : single ;
              UseColor : boolean ;
              MetaFileWidth : Integer ;
              MetaFileHeight : Integer ;
              end ;

TVarianceSettings = record
                  RecordSize : Integer ;
                  RecordOverlap : Integer ;
                  TauRise : Single ;
                  TauDecay : single ;
                  end ;

TDwellTimesSettings = record
                   UnitCurrent : single ;
                   Threshold : single ;
                   ChanNum : Integer ;
                   RecordSize : Integer ;
                   TCritical : single ;
                   SampleRangeLo : Integer ;
                   SampleRangeHi : Integer ;
                   SampleBlockSize : Integer ;
                   EventRangeLo : Integer ;
                   EventRangeHi : Integer ;
                   EventBlockSize : Integer ;
                   NumChannelsPerPatch : Integer ;
                   end ;

TFluorescenceSettings = record
                      InUse : Boolean ;
                      MinChannels : Integer ;
                      RMax : single ;
                      RMin : single ;
                      KEff : single ;
                      FThreshold : single ;
                      NumerChan : Integer ;
                      DenomChan : Integer ;
                      RatioChan : Integer ;
                      ConcChan : Integer ;
                      RatioDisplayMax : single ;
                      ConcDisplayMax : single ;
                      end ;

TCapacitySettings = record
                      InUse : Boolean ;
                      MinChannels : Integer ;
                      GrealChan : Integer ;
                      GimagChan : Integer ;
                      ImChan : Integer ;
                      VmChan : Integer ;
                      GmChan : Integer ;
                      GsChan : Integer ;
                      CmChan : Integer ;
                      CmDisplayMax : single ;
                      GmDisplayMax : single ;
                      GsDisplayMax : single ;                      
                      Frequency : single ;
                      Vrev : single ;
                      InvertGREal : boolean ;
                      InvertGImag : boolean ;
                      GChannelsUseGainTelegraph : boolean ;
                      SineWaveRMS : Single ;
                      CapacityCompensationInUse : Boolean ;
                      RSeriesComp : Single ;
                      CellCapacityComp : Single ;
                      end ;

TRTEventAnalysisSettings = record
              InUse : Boolean ;
              Channel : Integer ;
              DetectionThreshold : Single ;
              RunningMeanTime : Single ;
              DeadTime : Single ;
              CountingInterval : Single ;
              end ;

TRTResistanceSettings = record
              InUse : Boolean ;
              ImChannel : Integer ;
              VmChannel : Integer ;
              Amplitude : Single ;
              Duration : Single ;
              Interval : Single ;
              Plot : Integer ;
              StimulusUnits : string ;
              end ;

TSimEPCSettings = record
    Duration : Single ;
    Amplitude : Single ;
    AmplitudeSD : Single ;
    TauRise : Single ;
    TauDecay : Single ;
    NoiseSD : Single ;
    Frequency : Single ;
    FrequencySD : Single ;
    Delay : Single ;
    RandomEvents : Boolean ;
    SineAmplitude : Single ;
    SineFrequency : Single ;
    ReleaseProbability : Single ;
    ReleasablePool : Single ;
    Depression : Single ;
    TauDepression : Single ;
    UnitsIndex : Integer ;
    end ;


TCED1902 = record
           ComPort : LongInt ;
           ComHandle : Integer ;
           InUse : Boolean ;
           Input : LongInt ;
           InputName : string[16] ;
           Gain : LongInt ;
           GainValue : Single ;
           HPFilter : LongInt ;
           HPFilterValue : Single ;
           LPFilter : LongInt ;
           LPFilterValue : Single ;
           NotchFilter : LongInt ;
           ACCoupled : LongInt ;
           DCOffset : LongInt ;
           DCOffsetVMax : single ;
           OverLapStructure : POverlapped ;
           end ;

TColors = record
        Cursors : TColor ;
        Grid : TColor ;
        end ;

TSettings = record
          Resolution16bit : Boolean ;
          TriggerMode : string ;
          ExternalTriggerActiveHigh : Boolean ;
          EventDetector : TEventDetector ;
          NumTriggerSweeps : Integer ;
          NumChannels : Integer ;
          RecordDuration : Single ;
          ADCSamplingInterval : Single ;
          ADCVoltageRangeIndex : Integer ;
         { NumBytesRequired : Integer ;}
          DisplayDuration : Single ;
          DisplayGrid : Boolean ;
//          NumHorizontalGridLines : Integer ;
//          NumVerticalGridLines : Integer ;
          FixedZeroLevels : Boolean ;
          Variance : TVarianceSettings ;
          DwellTimes : TDwellTimesSettings ;
          //UnitCurrent : single ;
          //VCommand : Array[0..1] of TVCommand ;
//          DigitalPort : TDigitalPort ;
  //        DigitalOutputs : Integer ;
          UpdateOutputs : boolean ;
          DACSelected : Integer ;
          TopMargin : Integer ;
          TopLine : Integer ;
          BottomMargin : Integer ;
          LeftMargin : Integer ;
          RightMargin : Integer ;
          Plot : TPageSettings ;
          ZeroLevels : boolean ;
          ShowLabels : boolean ;
          ShowZeroLevels : boolean ;
          CutOffFrequency : single ;
          MinDACInterval : single ;
          MinSamplingInterval : single ;
          MaxSamplingInterval : single ;
          SealTest : TSealTest ;
          TUnits : string ;
          TScale : single ;
          TUnScale : single ;
          TBarValue : single ;
          BitmapWidth : Integer ;
          BitmapHeight : Integer ;
          {SectorWriteTime : single ;}
          DataDirectory : string ;
          ProgDirectory : string ;
          VProtDirectory : string ;
          VProgramFileName : string ;
//          WavGenNoDisplay : boolean ;
          LaboratoryInterface : Integer ;
          DeviceNumber : Integer ;
          ADCInputMode : Integer ;

          CED1902 : TCED1902 ;
{          DD1200IOPort : Integer ;
          DD1200IRQ : Integer ;
          DD1200DMA : Integer ;
          NIDisableDMA : Boolean ;}
          RecentFiles : Array[0..3] of string ;
          //RecentFilesPointer : Integer ;
          Fluorescence : TFluorescenceSettings ;
          Capacity : TCapacitySettings ;
          RTEventAnalysis : TRTEventAnalysisSettings ;
          RTResistance : TRTResistanceSettings ;
          Colors : TColors ;
          PageViewLinesPerPage : Integer ;
          PageViewLineDuration : Single ;
          NewCalculation : Boolean ;
          SimEPC : TSimEPCSettings ;
          end ;

TBuf = class(TObject)
     Buf : array[0..MaxTBuf] of Integer ;
     end ;

Txy = record
    x : Single ;
    y : Single ;
    end ;

TMarkerShape = ( SquareMarker, CircleMarker ) ;
TxyBuf = class(TObject)
         NumPoints : Integer ;
         x : array[0..4096] of Single ;
         y : array[0..4096] of Single ;
         MarkerShape : TMarkerShape ;
         MarkerSize : Integer ;
         MarkerSolid : Boolean ;
         Color : TColor ;
         end ;

TViewBuf = record
         nPoints : Integer ;
         x : array[0..1000] of Single ;
         YMin : array[0..EDRChannelLimit,0..1000] of Integer ;
         YMax : array[0..EDRChannelLimit,0..1000] of Integer ;
         tDisplay : single ;
         xMin : single ;
         xMax : single ;
         ChannelSelected : Integer ;
         end ;


{ Global Variables }
var

//MinADCValue : Integer ;
//MaxADCValue : Integer ;
MinDACValue : Integer ;
MaxDACValue : Integer ;
//FH : TWCPFileHeader ;
CdrFH : TCDRFileHeader ;
HeaderArrayFull : Boolean ; // File header arrayb full flag
// File channel settings
Channel : array[0..EDRChannelLimit] of TChannel ;
//WCPChannel : Array[0..EDRChannelLimit] of TChannel ;

RecordTypes : TStringList ;
ChannelNames : TStringList ;
Settings : TSettings ;
CED1902A : TCED1902 ;
LogFileName : string ;
MarkerList : TStringList ;       // Chart markers list

implementation


end.
