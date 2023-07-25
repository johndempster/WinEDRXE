unit EDRFileUnit;
{ ======================================================================
  WinEDR - File Input/Output routines
  (c) J.Dempster, University of Strathclyde 1996-2003. All Rights Reserved
  ======================================================================
  4/6/97 Error in retaining Seal test amplitude fixed
  7/6/97 ADCAmplifierGain now correctly included in scale factor
  5/7/98 Settings.UnitCurrent added
  1/4/99 Size of file header increased to 2048
  27/8/99 ... WinEDR V2.0
              dt now stored as (s) Cal. factors as V/Units rather than mV/units
  17/10/99 ... FileOverwriteCheck transferred from Convert
  27/2/00 ... Bug ready .WCP files fixed
  12/4/00 ... V2.0.5 SaveWCPHeader and GetWCPHeader now has 1024 byte header space
  15/2/01 ... ADCMAX flag added to both WCP and EDR data file headers
              to distinguish between 12 and 16 bit data files
  23/10/01 ... Pad : Array[1..2] of Byte added to TEVENT
                to make record length 60 bytes to be
                compatible with V2.1.2 and earlier. .EDE
                Problem arose when going from Delphi V3 to V5 compiler
  8/12/01  ... ReadCDRBuffer and WriteCDRBuffer updated to fix Append File bug
  24/6/3 ..... Settings.DisplayDuration, Settings.NumHorizontalGridLines
               Settings.NumVerticalGridLines now saved in INI file
  26/6/6 ..... Marker list now saved to header
  30/6/6 ..... Stimulator settings added to INI file
  30/9/3 ..... MaxADCValue now set to SESLabIO.ADCMaxValue when a new data file created
  5/2/4 ...... Single-channel event file I/O procedures moved ton SingleChanAnal.pas
  6/8/4 ...... Settings.ExternalTriggerActiveHigh added INI file
  29/7/5 ..... Settings.ADCInputMode (LABADIP=) added to ini file
  5/1/6 .. 'DETAW=', Settings.EventDetector.AnalysisWindow added
  30/7/6 .. RecChannel settings now saved in INI file (instead of Channel settings)
  25/9/6 .. Error in LoadInitialisationFile when no EDR.INI fixed
  26/9/6 .. Only ADCVoltageRange passed to CalibFactorToADCScale and
            ADCScaleToCalibFactor instead of CDRFH
  19/03/08 .. Real-time event frequency settings added toi INI file
  09/02/09 .. EDR.INI file size increased to 10000 to avoid header full with very long file paths
  24/02/09 .. CAPINVG= added to INI file
  02/06/09 .. CAPRSCOMP= and CAPCCOMP=  added to INI file
  03/06/09 .. MakeBackupFile and RestoreFromBackupFile now read & write correctly without error messages
              'CAPCOMPINUSE=', 'CAPINUSE=' added
  15/02/10 .. Settings.EventDetector.RisingEdgeWindow added
  08/03/10 .. Settings.RTResistance parameters added
              Settings.EventDetector.AvgFrequencyInterval
  15/06/10 .. Creation time string CTIME= added
              Old WCP file procedures removed
  13/07/10 .. LABDEV= NI device number added to INI file
  26/07/10 .. Settings.DwellTimes.SampleRangeLo etc added to .EDR file
              Settings.RecordDuration now stored in INI file
  09.07.12 .. RecChannel settings removed from INI file
  08.08.12 .. 'EFCOUNTI=' Settings.RTEventAnalysis.CountInterval added to INI file
  30.04.13 .. SIMEPC settings added to INI file
  14.05.13 .. SIMEPC.UnitsIndex added to INI file
  15.05.13 .. SIMEPC.ReleaseProbability etc added  to INI file
  14.08.14 .. .log files now saved in C:\Users\Public\Documents\WinEDR instead of programs folder
  18.09.14 .. Amplifier settings no longer loaded from edr.ini file to avoid interference with
              amplifier settings.xml
  17.10.14 ... Settings.ADCVoltageRangeIndex no longer used to store selected A/D voltage range
  12.02.15 ... GetCDRHeader() now checks that NP= in header matches actual number of samples in file and allows correction.
  11/03/16 ..  'STZAPA=', Settings.SealTest.ZapAmplitude'STZAPD=', Settings.SealTest.ZapDuration added to INI file
  7/12/16 ...  fluoresecence ratio, resistance and event frequency special options in use now in INI file
  9.1.17 ....  'FLIONNAME=', Settings.Fluorescence.IonName and 'FLIONUNITS=', Settings.Fluorescence.IonUnits
               added to INI file
  24.04.18 ..  'STARTSTIMREC=', Settings.StartStimulusOnRecord added
  17.06.19 ..  Settings.Sea;Test settings copied from WinWCP
  07.07.19 ... FORMTOP= FORMLEFT=, FORMWIDTH=, FORMHEIGHT= added to INI file saving location of main form on screen
               'DETEBT=', Settings.EventDetector.EnableBaselineTracking added to EDR and INI files
  26.09.18 ... 'DETDM=', Settings.EventDetector.DetectionMode ) added to EDR file
  15.03.21 ... 'DETTAUR=' Settings.EventDetector.TauRise and 'DETTAUD=' Settings.EventDetector.TauDecay added to EDR file
  22.06.22 ... 'DETAMPSDSCALE=', Settings.EventDetector.AmpSDScale added to INI and EDR file
  02.05.23 ... Replaces fileio.pas, global.pas, shared.pas
  09.05.23 ... EDR File V6.2 File header size now increased when no. channels > 16, 1-16 = 2048, 17-32 = 4096 , ...
  10.07.23 ... 'DETDECTO', Settings.EventDetector.TDecayTo added
  }

interface

uses
  System.SysUtils, System.Classes, System.UITypes, stdctrls, wintypes, graphics, maths, VCL.grids, math ,
  Vcl.Dialogs, System.StrUtils, SESLabIO ;

const
     FileVersion = 7.1 ;
     EventFileExtension = '.EDE' ;
     DataFileExtension = '.EDR' ;
     MaxChannels = 128 ;
     EDRChannelLimit = MaxChannels-1 ;

     NumSamplesPerSector = 256 ;
     NumBytesPerSector = NumSamplesPerSector*2 ;
     MinBytesInHeader = 2048 ;
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
          CurrentChannel : LongInt ;
          VoltageChannel : LongInt ;
          HoldingVoltage1 : single ;
          HoldingVoltage2 : single ;
          HoldingVoltage3 : single ;
          PulseHeight : single ;
          PulseHeight1 : single ;
          PulseHeight2 : single ;
          PulseHeight3 : single ;
          PulseWidth : single ;
          Use : LongInt ;
          AutoScale : Boolean ;
          DisplayScale : LongInt ;
          FreeRun : Boolean ;
          FirstSweep : Boolean ;
          NumAverages : Integer ;
          ZapAmplitude : single ;
          ZapDuration : single ;
          GaFromPeak : Boolean ;
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
               DetectionMode : Integer ;
               yThreshold : single ;
               tThreshold : single ;
               DeadTime : single ;
               BaselineAveragingInterval : single ;
               TauRise : single ;
               TauDecay : single ;
               RecordSize : Integer ;
               PreTriggerFraction : single ;
               AnalysisWindow : Single ;
               Alignment : Integer ;
               PositivePeaks : Boolean ;
               Baseline : Integer ;
               SubtractBaseline : Boolean ;
               NumBaselinePoints : Integer ;
               NumBaselineGap : Integer ;
               TDecayLevel : Single ;
               TDecayFrom : Integer ;
               TDecayTo : Integer ;
               RisingEdgeWindow : Integer ;
               AvgFrequencyInterval : Single ;
               EnableBaselineTracking : Boolean ;
               AmpSDScale : Single ;
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
                      IonName : string ;
                      IonUnits : string ;
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
          StartStimulusOnRecord : Boolean ;        // Start stimulus protocol when record button pressed
          EventDetector : TEventDetector ;
          NumTriggerSweeps : Integer ;
          NumChannels : Integer ;
          ContinuousRecording: Boolean ;
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

TMarkerShape = ( SquareMarker, CircleMarker ) ;

  TEDRFile = class(TDataModule)
    SaveDialog: TSaveDialog;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    { Private declarations }
    LogFile : TextFile ;

{    function ANSIArrayToString( const CharArray : Array of ANSIChar ) : string ;
    Function AppendStringToANSIArray(
             var Dest : array of ANSIChar ; // Destination to append to
             Source : string                // Source to copy from
            ) : Boolean ;                  // TRUE = append successful  procedure CopyANSIArrayToString(

    Function CopyStringToANSIArray(
             var Dest : array of ANSIChar ; // Destination to append to
             Source : string                // Source to copy from
             ) : Boolean ;                  // TRUE = copy successful  procedure CopyANSIArrayToString(

    procedure CopyANSIArrayToString(
          var Dest : string ;
          var Source : array of ANSIchar ) ;}

  public
    { Public declarations }
    ProgVersion : string ;
    { Public declarations }

    Version : string ;
    ProgDirectory : string ;      // Program folder
    SettingsDirectory : String ;  // Settings folder
    SettingsFileName : String ;   // Settings file name
    DataDirectory : string ;
    VProtDirectory : string ;

    LogFileName : string ;        // Activity log file name
    LogFileAvailable : boolean ;

    MinDACValue : Integer ;
    MaxDACValue : Integer ;

    CdrFH : TCDRFileHeader ;
    HeaderText : String ;

    // File channel settings
    Channel : array[0..EDRChannelLimit] of TChannel ;

    RecordTypes : TStringList ;
    ChannelNames : TStringList ;
    Settings : TSettings ;
    CED1902A : TCED1902 ;
    MarkerList : TStringList ;       // Chart markers list

   // Cell parameters
   RSeal : Single ; // Seal Resistance (Ohms)
   Gm : Single ;    // Cell membrane conductance (S)
   Ga : Single ;    // Pipette access conductance (S)
   Cm : Single ;    // Cell capacity (F)
   Vm : Single ;    // Cell membrane voltage (V)
   Im : Single ;    // Cell membrane current (A)



    procedure SaveHeader( var fHDR : TCDRFileHeader ) ;
    procedure GetHeader( var fHDR : TCDRFileHeader ) ;
    function ADCScaleToCalibFactor(
         ADCVoltageRange : Single ;
         var Channel : TChannel )  : single ;
    function CalibFactorToADCScale(
         ADCVoltageRange : Single ;
         var Channel : TChannel )  : single ;

    function FileOverwriteCheck( var FileName : string ) : boolean ;


    function ReadBuffer(
         var FHdr : TCDRFileHeader ;
         BlockPointer : Integer ;
         var Buf : Array of SmallInt ;
         NumBlocksToRead : Integer ) : Integer ;

    function WriteBuffer(
         var FHdr : TCDRFileHeader ;
         BlockPointer : Integer ;
         var Buf : Array of SmallInt ;
         NumBlocksToWrite : Integer
         ) : Integer ;

    procedure LoadInitializationFile( const IniFileName : string ) ;
    procedure SaveInitializationFile( const IniFileName : string ) ;

    function CreateNewDataFile( var FHeader : TCDRFileHeader ) : Boolean ;
    procedure LoadDataFiles( FileName : string ) ;
    procedure CloseDataFile ;

    procedure UpdateChannelScalingFactors ;

    procedure OpenLogFile ;
    procedure WriteToLogFile( Line : string ) ;
    procedure WriteToLogFileNoDate( Line : string ) ;
    procedure CloseLogFile ;
    procedure FileCloseSafe( var FileHandle : Integer ) ;

    procedure MakeBackupFile ;
    procedure RestoreFromBackupFile ;

   function ExtractListOfFloats (
           const CBuf : string ;
           var Values : Array of Single ;
           PositiveOnly : Boolean
           ) : Integer ;


    Function GetFromEditBox(
           var ed : TEdit ;
           Default, Min, Max : Single ;
           const FormatString, Units : string ;
           Scale : single
           ) : Single ;
    procedure GetIntRangeFromEditBox(
            var ed : TEdit ;
            var Lo,Hi : LongInt ;
            Min,Max : LongInt
            ) ;
    Procedure GetRangeFromEditBox(
            const ed : TEdit ;
            var LoValue,HiValue : Single ;
            Min,Max : Single ;
            const FormatString : String ;
            const Units : String
            ) ;

{     function ReplaceFileEnding(
              FileName,
              Ending : string
              ) : string ;}

     function ExtractFileNameOnly(
             FilePath : string
             ) : string ;

     procedure PrintHeaderAndFooter ;
     procedure PrintPageTitle(
            Canvas : TCanvas ;
            EqnType : TEqnType ;
            const Results : TStringGrid ;
            var YEndOfText : Integer
            ) ;

     procedure PrintStringGrid( const Table : TStringGrid ) ;
     procedure CopyStringGrid(
          const Table : TStringGrid ;
          UseSelection : Boolean ) ;

     function PrinterPointsToPixels( PointSize : Integer ) : Integer ;
     function PrinterCmToPixels( const Axis : string ; cm : single ) : Integer ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : single        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : Integer        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : NativeInt        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : String        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : Boolean        // Value
                           ) ; Overload ;


   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : single       // Value
                         ) : Single ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : Integer       // Value
                         ) : Integer ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : NativeInt       // Value
                         ) : NativeInt ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : string       // Value
                         ) : string ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : Boolean       // Value
                         ) : Boolean ; Overload ;        // Return value


  function ExtractFloat ( CBuf : string ; Default : Single ) : extended ;
  function ExtractInt ( CBuf : string ) : longint ;

  end;

var
  EDRFile: TEDRFile;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses Mdiform , AmpModule, VCL.Forms, Windows, System.IOUtils, VCL.Printers, ClipBrd ;


function TEDRFile.FileOverwriteCheck(
         var FileName : string
         ) : boolean ;
{ ----------------------------------------------------------------------
  To avoid overwriting an existing data file, check whether the file
  "FileName" exists and give the user the chance to change the name or
  abandon the operation.
  Returns FALSE if the user has chosen to abandon the operation
  ---------------------------------------------------------------------}
const
     OK = True ;
begin
     { Check whether file exists }
     if FileExists(FileName) then
          begin
          { If it exists, let user change it's name }
          SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
          SaveDialog.DefaultExt := DataFileExtension ;
          SaveDialog.FileName := ExtractFileName( FileName ) ;
          SaveDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                    [DataFileExtension,DataFileExtension]) ;
         SaveDialog.Title := ExtractFileName(FileName)
                                   + ' already exists! Change Name? ';

          if Main.SaveDialog.execute then begin
             { Save data directory }
            DataDirectory := ExtractFilePath( SaveDialog.FileName ) ;
             { Use new file name entered by user }
             FileName := SaveDialog.FileName ;
             { User has clicked OK, tell calling routine to go ahead }
             Result := OK ;
             end
          else begin
               { User has clicked CANCEL, tell calling routine to give up }
               Result := not OK ;
               end ;
          end
     else begin
          { File doesn't exist, no overwrite possible }
          Result := OK ;
          end ;
     end ;


procedure TEDRFile.SaveHeader( var fHDR : TCDRFileHeader ) ;
{ ---------------------------------------
  Save file header data to CDR data file
  ---------------------------------------}
var
   Header : TStringList ;
   pANSIBuf : pANSIChar ;

   i : Integer ;
   ch : Integer ;

begin

     // Exit if file not open
     if fHDR.FileHandle < 0 then Exit ;

     // Create file header Name=Value string list
     Header := TStringList.Create ;

     AddKeyValue( Header, 'VER',fHDR.Version );

     AddKeyValue( Header, 'CTIME', fHDR.CreationTime ) ;

     // 13/2/02 Added to distinguish between 12 and 16 bit data files
     // 30/7/6 Now uses ADCMaxValue in Channel settings
     CDRFH.ADCMaxValue := Channel[0].ADCMaxValue ;
     AddKeyValue( Header, 'ADCMAX', CDRFH.ADCMaxValue ) ;

     { Number of bytes in file header
       Note 09.05.23 Header size increased when no. of channels exceeds 16 }
     fHDR.NumBytesInHeader := MinBytesInHeader*(1 + (fHDR.NumChannels-1) div 16) ;
     AddKeyValue( Header, 'NBH', fHDR.NumBytesInHeader ) ;

     fHDR.NumChannels := Max(fHDR.NumChannels,1) ;
     AddKeyValue( Header, 'NC', fHDR.NumChannels ) ;

     // A/D converter input voltage range
     AddKeyValue( Header, 'AD', fHDR.ADCVoltageRange ) ;

     { Determine number of samples in file from size of file
       - space taken up by header }
   {  fHDR.NumSamplesInFile := (FileSeek(fHDR.FileHandle,0,2)
                               - fHDR.NumBytesInHeader) div 2 ;}
     fHDR.NumSamplesPerBlock := fHDR.NumChannels*NumSamplesPerSector ;
     fHDR.NumBytesPerBlock := fHDR.NumSamplesPerBlock*2 ;
     fHDR.NumBlocksInFile := fHDR.NumSamplesInFile div fHDR.NumSamplesPerBlock ;

     AddKeyValue( Header, 'NP', fHDR.NumSamplesInFile ) ;

     AddKeyValue( Header, 'DT',fHDR.dt );

     { Time duration of recorded data }
     fHDR.RecordDuration := (fHDR.NumSamplesInFile*fHDR.dt) / fHDR.NumChannels ;

     { Event detector parameters }
     AddKeyValue( Header,   'DETCH', Settings.EventDetector.Channel ) ;
     AddKeyValue( Header,   'DETRS', Settings.EventDetector.RecordSize ) ;
     AddKeyValue( Header, 'DETYT', Settings.EventDetector.yThreshold ) ;
     AddKeyValue( Header, 'DETTT', Settings.EventDetector.tThreshold ) ;
     AddKeyValue( Header, 'DETDD', Settings.EventDetector.DeadTime ) ;
     AddKeyValue( Header, 'DETBAI', Settings.EventDetector.BaselineAveragIngInterval ) ;
     AddKeyValue( Header, 'DETPF', Settings.EventDetector.PreTriggerFraction ) ;
     AddKeyValue( Header, 'DETAW', Settings.EventDetector.AnalysisWindow ) ;
     AddKeyValue( Header, 'DETPOSPK', Settings.EventDetector.PositivePeaks ) ;
     AddKeyValue( Header, 'DETBASE', Settings.EventDetector.Baseline ) ;
     AddKeyValue( Header, 'DETALIGN', Settings.EventDetector.Alignment ) ;
     AddKeyValue( Header, 'DETBASSUB', Settings.EventDetector.SubtractBaseline ) ;
     AddKeyValue( Header, 'DETBASPTS', Settings.EventDetector.NumBaselinePoints ) ;
     AddKeyValue( Header, 'DETBASGAP', Settings.EventDetector.NumBaselineGap ) ;
     AddKeyValue( Header, 'DETTDECP', Settings.EventDetector.TDecayLevel ) ;
     AddKeyValue( Header, 'DETDECFR', Settings.EventDetector.TDecayFrom ) ;
     AddKeyValue( Header, 'DETDECTO', Settings.EventDetector.TDecayTo ) ;
     AddKeyValue( Header, 'DETREW', Settings.EventDetector.RisingEdgeWindow ) ;
     AddKeyValue( Header, 'DETEBT', Settings.EventDetector.EnableBaselineTracking ) ;
     AddKeyValue( Header, 'DETAMPSDSCALE', Settings.EventDetector.AmpSDScale ) ;

     AddKeyValue( Header, 'VARRS', Settings.Variance.RecordSize ) ;
     AddKeyValue( Header, 'VAROV', Settings.Variance.RecordOverlap ) ;
     AddKeyValue( Header, 'VARTR', Settings.Variance.TauRise ) ;
     AddKeyValue( Header, 'VARTD', Settings.Variance.TauDecay ) ;

     AddKeyValue( Header, 'UNITC', Settings.DwellTimes.UnitCurrent ) ;
     AddKeyValue( Header, 'DWTTH', Settings.DwellTimes.Threshold ) ;
     AddKeyValue( Header, 'DWTSARLO', Settings.DwellTimes.SampleRangeLo ) ;
     AddKeyValue( Header, 'DWTSARHI', Settings.DwellTimes.SampleRangeHi ) ;
     AddKeyValue( Header, 'DWTSABLK', Settings.DwellTimes.SampleBlockSize ) ;
     AddKeyValue( Header, 'DWTEVRLO', Settings.DwellTimes.EventRangeLo ) ;
     AddKeyValue( Header, 'DWTEVRHI', Settings.DwellTimes.EventRangeHi ) ;
     AddKeyValue( Header, 'DWTEVBLK', Settings.DwellTimes.EventBlockSize ) ;
     AddKeyValue( Header, 'DWTNCPP', Settings.DwellTimes.NumChannelsPerPatch ) ;

     for ch := 0 to fHDR.NumChannels-1 do begin
         AddKeyValue( Header, format('YO%d',[ch]), Channel[ch].ChannelOffset) ;
         AddKeyValue( Header, format('YU%d',[ch]), Channel[ch].ADCUnits ) ;
         AddKeyValue( Header, format('YN%d',[ch]), Channel[ch].ADCName ) ;
         //Channel[ch].ADCCalibrationFactor := ADCScaleToCalibFactor( FHDR, ch ) ;
         AddKeyValue(Header,format('YCF%d',[ch]),Channel[ch].ADCCalibrationFactor) ;
         AddKeyValue( Header, format('YAG%d',[ch]), Channel[ch].ADCAmplifierGain) ;
         AddKeyValue( Header, format('YZ%d',[ch]), Channel[ch].ADCZero) ;
         AddKeyValue( Header, format('YR%d',[ch]), Channel[ch].ADCZeroAt) ;
         end ;

     { Experiment identification line }
     AddKeyValue( Header, 'ID', fHDR.IdentLine ) ;

     { Save the name of any associated WCP data file }
     AddKeyValue( Header, 'WCPFNAM',  fHDR.WCPFileName ) ;

     { Save the original file backed up flag }
     AddKeyValue( Header, 'BAK', fHDR.BackedUp ) ;

     // Save markers to header
     AddKeyValue( Header, 'MKN', MarkerList.Count ) ;
     for i := 0 to MarkerList.Count-1 do
         begin
         AddKeyValue( Header, format('MKTIM%d',[i]), Single(MarkerList.Objects[i])) ;
         AddKeyValue( Header, format('MKTXT%d',[i]), MarkerList.Strings[i] ) ;
         end ;

     // Get ANSIstring copy of header text and write to file
     pANSIBuf := AllocMem( fHDR.NumBytesInHeader ) ;
     for i := 1 to Min(Length(Header.Text),fHDR.NumBytesInHeader-1) do
         begin
         pAnsiBuf[i-1] := ANSIChar(Header.Text[i]);
         end;

     if Length(Header.Text) >= fHDR.NumBytesInHeader then
        begin
        ShowMessage( fHDR.FileName + ' File header full!' ) ;
        end;

     // Write header to start of EDR data file
     FileSeek( fHDR.FileHandle, 0, 0 ) ;
     FileWrite( fHDR.FileHandle, pANSIBuf^, fHDR.NumBytesInHeader ) ;

     { Add Names of channels to list }
     ChannelNames.Clear ;
     for ch := 0 to fHDR.NumChannels-1 do
         ChannelNames.Add( format('Ch.%d %s',[ch,Channel[ch].ADCName]) ) ;

     // Save to public header text string
     HeaderText := Header.Text ;

     Header.Free ;
     FreeMem( pANSIBuf ) ;

     end ;


function TEDRFile.ADCScaleToCalibFactor( ADCVoltageRange : Single ;
                                var Channel : TChannel )  : single ;
begin
     if Channel.ADCAmplifierGain = 0.0 then Channel.ADCAmplifierGain := 1.0 ;
     if Channel.ADCScale = 0.0 then Channel.ADCScale := 0.001 ;
     Result := ADCVoltageRange /
              (Channel.ADCScale*Channel.ADCAmplifierGain *(Channel.ADCMaxValue+1) ) ;
     end ;


function TEDRFile.CalibFactorToADCScale( ADCVoltageRange : Single ;
                                var Channel : TChannel ) : single ;
begin
     if Channel.ADCAmplifierGain = 0.0 then Channel.ADCAmplifierGain := 1.0 ;
     if Channel.ADCCalibrationFactor = 0.0 then Channel.ADCCalibrationFactor := 0.001 ;
     Result := ADCVoltageRange /
              (Channel.ADCCalibrationFactor*Channel.ADCAmplifierGain *(Channel.ADCMaxValue+1) ) ;
     end ;


procedure TEDRFile.GetHeader( var fHDR : TCDRFileHeader ) ;
{ ------------------------------------------------------
  Read file header block from data file,
  decode parameter list, and put into FileHeader record
  ------------------------------------------------------}
var
   Header : TStringList ;
   pANSIBuf : PANSIChar ;
   ANSIHeader : ANSIString ;

   i,ch,OldValue : Integer ;
   NumMarkers,NumSamplesInFile : Integer ;
   MarkerTime : Single ;
   MarkerText : String ;
   SaveHeader : Boolean ;
begin

     // Create header parameter list
     Header := TStringList.Create ;

     // Read ANSI text from first HeaderSize bytes of file header and load into Header StringList
     FileSeek( fHDR.FileHandle, 0, 0 ) ;
     pANSIBuf := AllocMem( MinBytesInHeader ) ;
     FileRead(fHDR.FileHandle, pANSIBuf^, MinBytesInHeader ) ;
     pANSIBuf[MinBytesInHeader-1] := #0 ;
     ANSIHeader := ANSIString( pANSIBuf ) ;
     Header.Text := String(ANSIHeader) ;

     for i := 0 to Header.Count-1 do Header[i] := ReplaceText( Header[i], '==','=');
     for i := 0 to Header.Count-1 do outputdebugstring(pchar(Header[i]));

     SaveHeader := False ;

     // Get size of file header
     fHDR.NumBytesInHeader := GetKeyValue( Header, 'NBH', fHDR.NumBytesInHeader ) ;
     if fHDR.NumBytesInHeader > MinBytesInHeader then
        begin
        // If file header larger than default size reload
        FreeMem ( pANSIBuf ) ;
        pANSIBuf := AllocMem( fHDR.NumBytesInHeader ) ;
        FileSeek( fHDR.FileHandle, 0, 0 ) ;
        FileRead(fHDR.FileHandle, pANSIBuf^, fHDR.NumBytesInHeader ) ;
        pANSIBuf[fHDR.NumBytesInHeader-1] := #0 ;
        ANSIHeader := ANSIString( pANSIBuf ) ;
        Header.Text := String(ANSIHeader) ;
        end ;

     fHDR.Version := GetKeyValue( Header, 'VER',fHDR.Version );

     fHDR.CreationTime := GetKeyValue( Header, 'CTIME', fHDR.CreationTime ) ;

     // 14/2/02 Added to distinguish between 12 and 16 bit data files
     OldValue := Channel[0].ADCMaxValue ;
     Channel[0].ADCMaxValue := GetKeyValue( Header, 'ADCMAX', Channel[0].ADCMaxValue ) ;
     if Channel[0].ADCMaxValue = 0 then Channel[0].ADCMaxValue := 2047 ;
     for ch := 0 to High(Channel) do Channel[ch].ADCMaxValue := Channel[0].ADCMaxValue ;
     CDRFH.ADCMaxValue := Channel[0].ADCMaxValue ;

     if Channel[0].ADCMaxValue <> OldValue then
        begin
        for ch := 0 to High(Channel) do
             begin
             Channel[ch].yMin := -Channel[0].ADCMaxValue -1;
             Channel[ch].yMax := Channel[0].ADCMaxValue ;
             end ;
        end ;

     fHDR.NumChannels := GetKeyValue( Header, 'NC', fHDR.NumChannels ) ;

     fHDR.NumSamplesInFile := GetKeyValue( Header, 'NP', fHDR.NumSamplesInFile ) ;

     NumSamplesInFile := (FileSeek( fHDR.FileHandle, 0, 2 ) + 1 - fHDR.NumBytesInHeader) div 2 ;

     if fHDR.NumSamplesInFile <> NumSamplesInFile then
        begin
        if VCL.Dialogs.MessageDlg( format(
           'No. samples (%d) listed in file header does not match actual number in file (%d)! Correct file header?',
           [fHdr.NumSamplesInFile,NumSamplesInFile]),
           mtConfirmation,[mbYes,mbNo], 0 ) = mrYes then fHdr.NumSamplesInFile := NumSamplesInFile ;
           SaveHeader := True ;
        end ;

     fHDR.NumSamplesPerBlock := fHDR.NumChannels*NumSamplesPerSector ;
     fHDR.NumBytesPerBlock := fHDR.NumSamplesPerBlock*2 ;
     fHDR.NumBlocksInFile := fHDR.NumSamplesInFile div fHDR.NumSamplesPerBlock ;

     fHDR.ADCVoltageRange := GetKeyValue( Header, 'AD',fHDR.ADCVoltageRange);

     fHDR.dt := GetKeyValue( Header, 'DT', fHDR.dt );
     if fHDR.dt = 0.0 then
        begin
        fHDR.dt := 1.0 ;
        ShowMessage( 'Sampling Interval = 0.0 s. Changed to 1.0! (GetHeader) ' );
        end ;

     { Time duration of recorded data }
     fHDR.RecordDuration := (fHDR.NumSamplesInFile*fHDR.dt) / fHDR.NumChannels ;

          { Event detector parameters }
     Settings.EventDetector.Channel := GetKeyValue( Header, 'DETCH', Settings.EventDetector.Channel ) ;
     Settings.EventDetector.RecordSize := GetKeyValue( Header, 'DETRS', Settings.EventDetector.RecordSize ) ;
     Settings.EventDetector.yThreshold := GetKeyValue( Header, 'DETYT', Settings.EventDetector.yThreshold ) ;
     Settings.EventDetector.tThreshold := GetKeyValue( Header, 'DETTT', Settings.EventDetector.tThreshold ) ;
     Settings.EventDetector.DeadTime := GetKeyValue( Header, 'DETDD', Settings.EventDetector.DeadTime ) ;
     Settings.EventDetector.BaselineAveragingInterval := GetKeyValue( Header, 'DETBAI', Settings.EventDetector.BaselineAveragingInterval ) ;
     Settings.EventDetector.PreTriggerFraction := GetKeyValue( Header, 'DETPF', Settings.EventDetector.PreTriggerFraction ) ;
     Settings.EventDetector.AnalysisWindow := GetKeyValue( Header, 'DETAW', Settings.EventDetector.AnalysisWindow ) ;
     Settings.EventDetector.PositivePeaks := GetKeyValue( Header, 'DETPOSPK', Settings.EventDetector.PositivePeaks ) ;
     Settings.EventDetector.Baseline := GetKeyValue( Header, 'DETBASE', Settings.EventDetector.Baseline ) ;
     Settings.EventDetector.Alignment := GetKeyValue( Header, 'DETALIGN', Settings.EventDetector.Alignment ) ;
     Settings.EventDetector.SubtractBaseline := GetKeyValue( Header, 'DETBASSUB', Settings.EventDetector.SubtractBaseline ) ;
     Settings.EventDetector.NumBaselinePoints := GetKeyValue( Header, 'DETBASPTS', Settings.EventDetector.NumBaselinePoints ) ;
     Settings.EventDetector.NumBaselineGap := GetKeyValue( Header, 'DETBASGAP', Settings.EventDetector.NumBaselineGap ) ;
     Settings.EventDetector.TDecayLevel := GetKeyValue( Header, 'DETTDECP', Settings.EventDetector.TDecayLevel ) ;
     Settings.EventDetector.TDecayFrom := GetKeyValue( Header, 'DETDECFR', Settings.EventDetector.TDecayFrom ) ;
     Settings.EventDetector.TDecayTo := GetKeyValue( Header, 'DETDECTO', Settings.EventDetector.TDecayTo ) ;
     Settings.EventDetector.RisingEdgeWindow := GetKeyValue( Header, 'DETREW', Settings.EventDetector.RisingEdgeWindow ) ;
     Settings.EventDetector.EnableBaselineTracking := GetKeyValue( Header, 'DETEBT', Settings.EventDetector.EnableBaselineTracking ) ;
     Settings.EventDetector.AmpSDScale := GetKeyValue( Header, 'DETAMPSDSCALE', Settings.EventDetector.AmpSDScale ) ;

     // Variance analysis
     Settings.Variance.RecordSize := GetKeyValue( Header, 'VARRS', Settings.Variance.RecordSize ) ;
     Settings.Variance.RecordOverlap := GetKeyValue( Header, 'VAROV', Settings.Variance.RecordOverlap ) ;
     Settings.Variance.TauRise := GetKeyValue( Header, 'VARTR', Settings.Variance.TauRise ) ;
     Settings.Variance.TauDecay := GetKeyValue( Header, 'VARTD', Settings.Variance.TauDecay ) ;

     // Single-channel analysis
     Settings.DwellTimes.UnitCurrent := GetKeyValue( Header, 'UNITC', Settings.DwellTimes.UnitCurrent ) ;
     Settings.DwellTimes.Threshold := GetKeyValue( Header, 'DWTTH', Settings.DwellTimes.Threshold ) ;
     Settings.DwellTimes.SampleRangeLo := GetKeyValue( Header, 'DWTSARLO', Settings.DwellTimes.SampleRangeLo ) ;
     Settings.DwellTimes.SampleRangeHi := GetKeyValue( Header, 'DWTSARHI', Settings.DwellTimes.SampleRangeHi ) ;
     Settings.DwellTimes.SampleBlockSize := GetKeyValue( Header, 'DWTSABLK', Settings.DwellTimes.SampleBlockSize ) ;
     Settings.DwellTimes.EventRangeLo := GetKeyValue( Header, 'DWTEVRLO', Settings.DwellTimes.EventRangeLo ) ;
     Settings.DwellTimes.EventRangeHi := GetKeyValue( Header, 'DWTEVRHI', Settings.DwellTimes.EventRangeHi ) ;
     Settings.DwellTimes.EventBlockSize := GetKeyValue( Header, 'DWTEVBLK', Settings.DwellTimes.EventBlockSize ) ;
     Settings.DwellTimes.NumChannelsPerPatch := GetKeyValue( Header, 'DWTNCPP', Settings.DwellTimes.NumChannelsPerPatch ) ;

     for ch := 0 to fHDR.NumChannels-1 do
         begin

         Channel[ch].ChannelOffset := GetKeyValue( Header, format('YO%d',[ch]), Channel[ch].ChannelOffset) ;

         Channel[ch].ADCUnits := '??' ;
         Channel[ch].ADCUnits := GetKeyValue( Header, format('YU%d',[ch]) , Channel[ch].ADCUnits ) ;
         { Fix to avoid strings with #0 in them }
         if Channel[ch].ADCUnits[1] = chr(0) then Channel[ch].ADCUnits := '??' ;
         Channel[ch].ADCName := 'Ch' + IntToStr(ch) ;
         Channel[ch].ADCName := GetKeyValue( Header, format('YN%d',[ch]), Channel[ch].ADCName ) ;
         { Fix to avoid strings with #0 in them }
         if Channel[ch].ADCName[1] = chr(0) then Channel[ch].ADCName := '??' ;
         Channel[ch].ADCCalibrationFactor := GetKeyValue( Header, format('YCF%d',[ch]), Channel[ch].ADCCalibrationFactor) ;

         Channel[ch].ADCAmplifierGain := GetKeyValue( Header, format('YAG%d',[ch]), Channel[ch].ADCAmplifierGain) ;
         Channel[ch].ADCScale := CalibFactorToADCScale(FHDR.ADCVoltageRange,Channel[ch] ) ;

         Channel[ch].ADCZero := GetKeyValue( Header, format('YZ%d',[ch]), Channel[ch].ADCZero) ;
         Channel[ch].ADCZeroAt := GetKeyValue( Header, format('YR%d',[ch]), Channel[ch].ADCZeroAt) ;
         end ;

     { Experiment identification line }
     fHDR.IdentLine := GetKeyValue( Header, 'ID', fHDR.IdentLine ) ;

          { Read Markers }
     NumMarkers := 0 ;
     NumMarkers := GetKeyValue( Header, 'MKN', NumMarkers ) ;
     MarkerList.Clear ;
     MarkerTime := 0.0 ;
     MarkerText := '' ;
     for i := 0 to NumMarkers-1 do
          begin
          MarkerTime := GetKeyValue( Header, format('MKTIM%d',[i]), MarkerTime ) ;
          MarkerText := GetKeyValue( Header, format('MKTXT%d',[i]), MarkerText ) ;
          MarkerList.AddObject( MarkerText, TObject(MarkerTime) ) ;
          end ;

           { Add names of channels to list }
     ChannelNames.Clear ;
     for ch := 0 to fHDR.NumChannels-1 do
         ChannelNames.Add( format('Ch.%d %s',[ch,Channel[ch].ADCName] )) ;

     { Name of any associated WCP data file }
     fHDR.WCPFileName := '' ;
     fHDR.WCPFileName := GetKeyValue( Header, 'WCPFNAM', fHDR.WCPFileName ) ;

     { Save the original file backed up flag }
     fHDR.BackedUp := GetKeyValue( Header, 'BAK', fHDR.BackedUp ) ;

     // Save file header to file if changes have been made
     if SaveHeader then EDRFile.SaveHeader( fHDR ) ;

    // Save to public header text string
    HeaderText := Header.Text ;

    Header.Free ;
    FreeMem(pANSIBuf) ;

     end ;


function TEDRFile.Readbuffer(
         var FHdr : TCDRFileHeader ;     { Data file header }
         BlockPointer : Integer ;        { Sample block to start reading at }
         var Buf : Array of SmallInt ;   { Buffer to hold samples }
         NumBlocksToRead : Integer       { Number of sample blocks to read }
         ) : Integer ;
{ -----------------------------------------------
  Read a buffer of A/D samples from EDR data file
  ----------------------------------------------- }
var
   NumBytes : Integer ;
begin
     FHdr.FilePointer := FileSeek( FHdr.FileHandle,
                                   (BlockPointer*FHdr.NumChannels*2)
                                   + FHdr.NumBytesInHeader, 0 ) ;
     NumBytes := NumBlocksToRead*FHdr.NumChannels*2 ;
     Result := FileRead(FHdr.FileHandle,Buf,NumBytes) div (FHdr.NumChannels*2) ;
     end ;


function TEDRFile.WriteBuffer(
         var FHdr : TCDRFileHeader ;     { Data file header }
         BlockPointer : Integer ;        { Sample block to start Writeing at }
         var Buf : Array of SmallInt ;   { Buffer to hold samples }
         NumBlocksToWrite : Integer       { Number of sample blocks to Write }
         ) : Integer ;
{ -----------------------------------------------
  Write a buffer of A/D samples to EDR data file
  ----------------------------------------------- }
var
   NumBytes : Integer ;
begin
     FHdr.FilePointer := FileSeek( FHdr.FileHandle,
                                   (BlockPointer*FHdr.NumChannels*2)
                                   + FHdr.NumBytesInHeader, 0 ) ;
     NumBytes := NumBlocksToWrite*FHdr.NumChannels*2 ;
     Result := FileWrite(FHdr.FileHandle,Buf,NumBytes) div (FHdr.NumChannels*2) ;
     end ;


procedure TEDRFile.LoadInitializationFile( const IniFileName : string ) ;
{ ---------------------------------------------------------
  Read Initialization file to get initial program settings,
  e.g. the name of the last data file used
  ---------------------------------------------------------}
var
   Header : TStringList ;
   i : Integer ;
begin

     if not FileExists( IniFileName ) then Exit ;

     // Create file header Name=Value string list
     Header := TStringList.Create ;

     // Load list from file
     Header.LoadFromFile( IniFileName ) ;
     outputdebugstring(pchar(Header[0]));
     outputdebugstring(pchar(format('Count=%d',[Header.Count])));

     // Remove accidental double zeroes
     for i := 0 to Header.Count-1 do Header[i] := ReplaceText( Header[i], '==','=' ) ;


     // Record duration
     Settings.RecordDuration := GetKeyValue( Header, 'RECDUR', Settings.RecordDuration ) ;

     // Continuous Recording flag (overrides Settings.RecordDuration setting)
     Settings.ContinuousRecording := GetKeyValue( Header, 'RECCONT', Settings.ContinuousRecording ) ;

     { Get default no. channels }
     Settings.NumChannels := GetKeyValue( Header, 'NC', Settings.NumChannels ) ;
     Settings.NumChannels := Max( 1,Settings.NumChannels ) ;

     Settings.StartStimulusOnRecord := GetKeyValue( Header, 'STARTSTIMREC', Settings.StartStimulusOnRecord ) ;

     { CED 1902 amplifier settings }
     Amplifier.CED1902.Input := GetKeyValue( Header, 'CEDI', Amplifier.CED1902.Input ) ;
     Amplifier.CED1902.Gain := GetKeyValue( Header, 'CEDG', Amplifier.CED1902.Gain ) ;
     Amplifier.CED1902.GainValue := GetKeyValue( Header, 'CEDGV', Amplifier.CED1902.GainValue ) ;
     Amplifier.CED1902.LPFilter := GetKeyValue( Header, 'CEDLP', Amplifier.CED1902.LPFilter ) ;
     Amplifier.CED1902.HPFilter := GetKeyValue( Header, 'CEDHP', Amplifier.CED1902.HPFilter ) ;
     Amplifier.CED1902.ACCoupled := GetKeyValue( Header, 'CEDAC', Amplifier.CED1902.ACCoupled ) ;
     Amplifier.CED1902.DCOffset := GetKeyValue( Header, 'CEDDCO', Amplifier.CED1902.DCOffset ) ;
     Amplifier.CED1902.NotchFilter := GetKeyValue( Header, 'CEDNF', Amplifier.CED1902.NotchFilter ) ;
     Amplifier.CED1902.ComPort := GetKeyValue( Header, 'CEDPO', Amplifier.CED1902.ComPort ) ;

     Settings.Variance.RecordSize := GetKeyValue( Header, 'VARRS', Settings.Variance.RecordSize ) ;
     Settings.Variance.RecordOverlap := GetKeyValue( Header, 'VAROV', Settings.Variance.RecordOverlap ) ;
     Settings.Variance.TauRise := GetKeyValue( Header, 'VARTR', Settings.Variance.TauRise ) ;
     Settings.Variance.TauDecay := GetKeyValue( Header, 'VARTD', Settings.Variance.TauDecay ) ;

     Settings.DwellTimes.UnitCurrent := GetKeyValue( Header, 'UNITC', Settings.DwellTimes.UnitCurrent ) ;
     Settings.DwellTimes.Threshold := GetKeyValue( Header, 'DWTTH', Settings.DwellTimes.Threshold ) ;

           { *** Recording settings *** }
     { Recording trigger mode }
     Settings.TriggerMode := GetKeyValue( Header, 'TRG', Settings.TriggerMode ) ;
     Settings.ExternalTriggerActiveHigh := GetKeyValue( Header, 'EXTTRIGAH', Settings.ExternalTriggerActiveHigh ) ;
     { Number of records required (in free run/ext. trigger modes}
     Settings.NumTriggerSweeps := GetKeyValue( Header, 'NRQ', Settings.NumTriggerSweeps ) ;
     Settings.ADCSamplingInterval := GetKeyValue( Header, 'DT', Settings.ADCSamplingInterval );

     { Event detector parameters }
     Settings.EventDetector.Channel := GetKeyValue( Header, 'DETCH', Settings.EventDetector.Channel ) ;
     Settings.EventDetector.DetectionMode := GetKeyValue( Header, 'DETDM', Settings.EventDetector.DetectionMode ) ;
     Settings.EventDetector.RecordSize := GetKeyValue( Header, 'DETRS', Settings.EventDetector.RecordSize ) ;
     Settings.EventDetector.yThreshold := GetKeyValue( Header, 'DETYT', Settings.EventDetector.yThreshold ) ;
     Settings.EventDetector.tThreshold := GetKeyValue( Header, 'DETTT', Settings.EventDetector.tThreshold ) ;
     Settings.EventDetector.DeadTime := GetKeyValue( Header, 'DETDD', Settings.EventDetector.DeadTime ) ;
     Settings.EventDetector.BaselineAveragingInterval := GetKeyValue( Header, 'DETBAI', Settings.EventDetector.BaselineAveragingInterval ) ;
     Settings.EventDetector.TauRise := GetKeyValue( Header, 'DETTAUR', Settings.EventDetector.TauRise ) ;
     Settings.EventDetector.TauDecay := GetKeyValue( Header, 'DETTAUD', Settings.EventDetector.TauDecay ) ;
     Settings.EventDetector.PreTriggerFraction := GetKeyValue( Header, 'DETPF', Settings.EventDetector.PreTriggerFraction ) ;
     Settings.EventDetector.AnalysisWindow := GetKeyValue( Header, 'DETAW', Settings.EventDetector.AnalysisWindow ) ;
     Settings.EventDetector.RisingEdgeWindow := GetKeyValue( Header, 'DETREW', Settings.EventDetector.RisingEdgeWindow ) ;
     Settings.EventDetector.AvgFrequencyInterval := GetKeyValue( Header, 'DETAVFI', Settings.EventDetector.AvgFrequencyInterval ) ;
     Settings.EventDetector.EnableBaselineTracking := GetKeyValue( Header, 'DETEBT', Settings.EventDetector.EnableBaselineTracking ) ;
     Settings.EventDetector.AmpSDScale := GetKeyValue( Header, 'DETAMPSDSCALE', Settings.EventDetector.AmpSDScale ) ;

     { Default digital control port output byte setting }
     Settings.UpdateOutputs := True ;

     Settings.MinDACInterval := GetKeyValue( Header, 'DTMINDAC', Settings.MinDACInterval ) ;

     { Load time units (ms or s) }
     Settings.TUnits := GetKeyValue( Header, 'TUNITS', Settings.TUnits ) ;
     if Settings.TUnits = 's' then
        begin
        Settings.TScale := 1. ;
        Settings.TUnScale := 1. ;
        end
     else
        begin
        Settings.TUnits := 'ms' ;
        Settings.TScale := SecsToms ;
        Settings.TUnScale := MsToSecs ;
        end ;

      { Seal test pulse settings }
      Settings.SealTest.PulseHeight := GetKeyValue( Header, 'STPH', Settings.SealTest.PulseHeight ) ;
      Settings.SealTest.PulseHeight1 := GetKeyValue( Header, 'STPH1', Settings.SealTest.PulseHeight1 ) ;
      Settings.SealTest.PulseHeight2 := GetKeyValue( Header, 'STPH2', Settings.SealTest.PulseHeight2 ) ;
      Settings.SealTest.PulseHeight3 := GetKeyValue( Header, 'STPH3', Settings.SealTest.PulseHeight3 ) ;
      Settings.SealTest.HoldingVoltage1 := GetKeyValue( Header, 'STHV1', Settings.SealTest.HoldingVoltage1 ) ;
      Settings.SealTest.HoldingVoltage2 := GetKeyValue( Header, 'STHV2', Settings.SealTest.HoldingVoltage2 ) ;
      Settings.SealTest.HoldingVoltage3 := GetKeyValue( Header, 'STHV3', Settings.SealTest.HoldingVoltage3 ) ;
      Settings.SealTest.PulseWidth := GetKeyValue( Header, 'STPW', Settings.SealTest.PulseWidth ) ;
      Settings.SealTest.CurrentChannel := GetKeyValue( Header, 'STCCH', Settings.SealTest.CurrentChannel ) ;
      Settings.SealTest.VoltageChannel := GetKeyValue( Header, 'STVCH', Settings.SealTest.VoltageChannel ) ;
      Settings.SealTest.Use := GetKeyValue( Header, 'STUSE', Settings.SealTest.Use ) ;
      Settings.SealTest.DisplayScale := GetKeyValue( Header, 'STDSC', Settings.SealTest.DisplayScale ) ;
      Settings.SealTest.AutoScale := GetKeyValue( Header, 'STASC', Settings.SealTest.AutoScale ) ;
      Settings.SealTest.FreeRun := GetKeyValue( Header, 'STFRU', Settings.SealTest.FreeRun ) ;
      Settings.SealTest.NumAverages := GetKeyValue( Header, 'STNAV', Settings.SealTest.NumAverages ) ;
      Settings.SealTest.ZapAmplitude := GetKeyValue( Header, 'STZAPA', Settings.SealTest.ZapAmplitude ) ;
      Settings.SealTest.ZapDuration := GetKeyValue( Header, 'STZAPD', Settings.SealTest.ZapDuration ) ;
      Settings.SealTest.GaFromPeak := GetKeyValue( Header, 'STGAP', Settings.SealTest.GaFromPeak ) ;



     { Width/height of clipboard bitmaps }
     Settings.BitmapWidth := GetKeyValue( Header, 'BMAPW', Settings.BitmapWidth ) ;
     Settings.BitmapHeight := GetKeyValue( Header, 'BMAPH', Settings.BitmapHeight ) ;

     Settings.DisplayGrid := GetKeyValue( Header, 'DISPGRID', Settings.DisplayGrid ) ;
     Settings.DisplayDuration := GetKeyValue( Header, 'DISPDUR', Settings.DisplayDuration ) ;
     Settings.FixedZeroLevels := GetKeyValue( Header, 'FIXZERO', Settings.FixedZeroLevels) ;

     { Plotting page settings }
     Settings.Plot.TopMargin := GetKeyValue( Header, 'PLTPM',Settings.Plot.TopMargin ) ;
     Settings.Plot.BottomMargin := GetKeyValue( Header, 'PLBTM',Settings.Plot.BottomMargin ) ;
     Settings.Plot.LeftMargin := GetKeyValue( Header, 'PLLFM',Settings.Plot.LeftMargin ) ;
     Settings.Plot.RightMargin := GetKeyValue( Header, 'PLRTM',Settings.Plot.RightMargin ) ;
     Settings.Plot.FontName := GetKeyValue( Header, 'PLFNT',Settings.Plot.FontName ) ;
     Settings.Plot.FontSize := GetKeyValue( Header, 'PLFSI',Settings.Plot.FontSize ) ;
     Settings.Plot.LineThickness := GetKeyValue( Header, 'PLLTH',Settings.Plot.LineThickness ) ;
     Settings.Plot.ShowLines := GetKeyValue( Header, 'PLSHL',Settings.Plot.ShowLines ) ;
     Settings.Plot.MarkerSize := GetKeyValue( Header, 'PLMKS',Settings.Plot.MarkerSize ) ;
     Settings.Plot.ShowMarkers := GetKeyValue( Header, 'PLSHM',Settings.Plot.ShowMarkers ) ;
     Settings.Plot.UseColor := GetKeyValue( Header, 'PLCOL',Settings.Plot.UseColor ) ;

     // Page display module (PageView.pas) settings
     Settings.PageViewLinesPerPage := GetKeyValue( Header, 'PVLPP',Settings.PageViewLinesPerPage ) ;
     Settings.PageViewLineDuration := GetKeyValue( Header, 'PVLD',Settings.PageViewLineDuration ) ;

     DataDirectory := GetKeyValue( Header, 'DDIR', DataDirectory ) ;

     { Laboratory interface }
     Settings.LaboratoryInterface := GetKeyValue( Header, 'LABINT',Settings.LaboratoryInterface ) ;

     // Lab. interface device #
     Settings.DeviceNumber := GetKeyValue( Header, 'LABDEV',Settings.DeviceNumber ) ;

     // Lab. Interface A/D input mode
     Settings.ADCInputMode := 0 ;
     Settings.ADCInputMode := GetKeyValue( Header, 'LABADIP',Settings.ADCInputMode ) ;

     { Recently used data files }
     for i := 0 to High(Settings.RecentFiles) do Settings.RecentFiles[i] := '' ;
     Settings.RecentFiles[0] := GetKeyValue( Header, 'RF0', Settings.RecentFiles[0]) ;
     Settings.RecentFiles[1] := GetKeyValue( Header, 'RF1', Settings.RecentFiles[1]) ;
     Settings.RecentFiles[2] := GetKeyValue( Header, 'RF2', Settings.RecentFiles[2]) ;
     Settings.RecentFiles[3] := GetKeyValue( Header, 'RF3', Settings.RecentFiles[3]) ;

     // Capacity settings
     Settings.Capacity.CmDisplayMax := GetKeyValue( Header, 'CAPCMMX', Settings.Capacity.CmDisplayMax ) ;
     Settings.Capacity.GsDisplayMax := GetKeyValue( Header, 'CAPGSMX', Settings.Capacity.GsDisplayMax ) ;
     Settings.Capacity.GmDisplayMax := GetKeyValue( Header, 'CAPGMMX', Settings.Capacity.GmDisplayMax ) ;
     Settings.Capacity.Frequency := GetKeyValue( Header, 'CAPFREQ', Settings.Capacity.Frequency ) ;
     Settings.Capacity.VRev := GetKeyValue( Header, 'CAPVREV', Settings.Capacity.VRev ) ;
     Settings.Capacity.GrealChan := GetKeyValue( Header, 'CAPGR', Settings.Capacity.GrealChan) ;
     Settings.Capacity.GimagChan := GetKeyValue( Header, 'CAPGI', Settings.Capacity.GimagChan) ;
     Settings.Capacity.ImChan := GetKeyValue( Header, 'CAPIM', Settings.Capacity.ImChan) ;
     Settings.Capacity.VmChan := GetKeyValue( Header, 'CAPVM', Settings.Capacity.VmChan) ;
     Settings.Capacity.GmChan := GetKeyValue( Header, 'CAPGM', Settings.Capacity.GmChan) ;
     Settings.Capacity.GsChan := GetKeyValue( Header, 'CAPGS', Settings.Capacity.GsChan) ;
     Settings.Capacity.CmChan := GetKeyValue( Header, 'CAPCM', Settings.Capacity.CmChan) ;
     Settings.Capacity.InvertGReal := GetKeyValue( Header, 'CAPINVGR', Settings.Capacity.InvertGReal) ;
     Settings.Capacity.InvertGIMag := GetKeyValue( Header, 'CAPINVGI', Settings.Capacity.InvertGIMag) ;
     Settings.Capacity.RSeriesComp := GetKeyValue( Header, 'CAPRSCOMP', Settings.Capacity.RSeriesComp ) ;
     Settings.Capacity.CellCapacityComp := GetKeyValue( Header, 'CAPCCOMP', Settings.Capacity.CellCapacityComp ) ;
     Settings.Capacity.CapacityCompensationInUse := GetKeyValue( Header, 'CAPCOMPINUSE', Settings.Capacity.CapacityCompensationInUse) ;
     Settings.Capacity.InUse := GetKeyValue( Header, 'CAPINUSE', Settings.Capacity.InUse) ;

     // Fluorescence settings
     Settings.Fluorescence.InUse := GetKeyValue( Header, 'FLRINUSE', Settings.Fluorescence.InUse) ;
     Settings.Fluorescence.RMax := GetKeyValue( Header, 'FLRMAX', Settings.Fluorescence.RMax ) ;
     Settings.Fluorescence.RMin := GetKeyValue( Header, 'FLRMIN', Settings.Fluorescence.RMin ) ;
     Settings.Fluorescence.KEff := GetKeyValue( Header, 'FLKEFF', Settings.Fluorescence.KEff ) ;
     Settings.Fluorescence.FThreshold := GetKeyValue( Header, 'FLTHRE', Settings.Fluorescence.FThreshold ) ;
     Settings.Fluorescence.NumerChan := GetKeyValue( Header, 'FLNUM', Settings.Fluorescence.NumerChan ) ;
     Settings.Fluorescence.DenomChan := GetKeyValue( Header, 'FLDEN', Settings.Fluorescence.DenomChan ) ;
     Settings.Fluorescence.RatioChan := GetKeyValue( Header, 'FLRAT', Settings.Fluorescence.RatioChan ) ;
     Settings.Fluorescence.ConcChan := GetKeyValue( Header, 'FLCON', Settings.Fluorescence.ConcChan ) ;
     Settings.Fluorescence.RatioDisplayMax := GetKeyValue( Header, 'FLRDMX', Settings.Fluorescence.RatioDisplayMax ) ;
     Settings.Fluorescence.ConcDisplayMax := GetKeyValue( Header, 'FLCDMX', Settings.Fluorescence.ConcDisplayMax ) ;
     Settings.Fluorescence.IonName := 'Ca' ;
     Settings.Fluorescence.IonName := GetKeyValue( Header, 'FLIONNAME', Settings.Fluorescence.IonName ) ;
     Settings.Fluorescence.IonUnits := 'uM' ;
     Settings.Fluorescence.IonUnits := GetKeyValue( Header, 'FLIONUNITS', Settings.Fluorescence.IonUnits ) ;

     // Real time event frequency settings
     Settings.RTEventAnalysis.InUse := GetKeyValue( Header, 'EFINUSE', Settings.RTEventAnalysis.InUse) ;
     Settings.RTEventAnalysis.Channel := GetKeyValue( Header, 'EFCH', Settings.RTEventAnalysis.Channel ) ;
     Settings.RTEventAnalysis.DetectionThreshold := GetKeyValue( Header, 'EFTHRESH', Settings.RTEventAnalysis.DetectionThreshold ) ;
     Settings.RTEventAnalysis.RunningMeanTime := GetKeyValue( Header, 'EFRMEAN', Settings.RTEventAnalysis.RunningMeanTime ) ;
     Settings.RTEventAnalysis.DeadTime := GetKeyValue( Header, 'EFDEADT', Settings.RTEventAnalysis.DeadTime ) ;
     Settings.RTEventAnalysis.CountingInterval := GetKeyValue( Header, 'EFCOUNTI', Settings.RTEventAnalysis.CountingInterval ) ;

     // Real time resistance settings
     Settings.RTResistance.InUse := GetKeyValue( Header, 'RESINUSE', Settings.RTResistance.InUse) ;
     Settings.RTResistance.ImChannel := GetKeyValue( Header, 'RESICH', Settings.RTResistance.ImChannel ) ;
     Settings.RTResistance.VmChannel := GetKeyValue( Header, 'RESVCH', Settings.RTResistance.VmChannel ) ;
     Settings.RTResistance.Amplitude := GetKeyValue( Header, 'RESAMPLITUDE', Settings.RTResistance.Amplitude ) ;
     Settings.RTResistance.Duration := GetKeyValue( Header, 'RESDUR', Settings.RTResistance.Duration ) ;
     Settings.RTResistance.Interval:= GetKeyValue( Header, 'RESINT', Settings.RTResistance.Interval ) ;
     Settings.RTResistance.Plot := GetKeyValue( Header, 'RESPLT', Settings.RTResistance.Plot ) ;

     // Currently selected stimulus file
     Settings.VProgramFileName := GetKeyValue( Header, 'STIMFILE', Settings.VProgramFileName ) ;

     // Load post-synaptic current simulation settings
     Settings.SimEPC.Duration := GetKeyValue( Header, 'EPCDUR', Settings.SimEPC.Duration) ;
     Settings.SimEPC.Amplitude := GetKeyValue( Header, 'EPCAMP', Settings.SimEPC.Amplitude) ;
     Settings.SimEPC.AmplitudeSD := GetKeyValue( Header, 'EPCAMPSD', Settings.SimEPC.AmplitudeSD) ;
     Settings.SimEPC.TauRise := GetKeyValue( Header, 'EPCTRISE', Settings.SimEPC.TauRise) ;
     Settings.SimEPC.TauDecay := GetKeyValue( Header, 'EPCTDECAY', Settings.SimEPC.TauDecay) ;
     Settings.SimEPC.NoiseSD := GetKeyValue( Header, 'EPCNOISESD', Settings.SimEPC.NoiseSD) ;
     Settings.SimEPC.Frequency := GetKeyValue( Header, 'EPCFREQ', Settings.SimEPC.Frequency) ;
     Settings.SimEPC.FrequencySD := GetKeyValue( Header, 'EPCFREQSD', Settings.SimEPC.FrequencySD) ;
     Settings.SimEPC.Delay := GetKeyValue( Header, 'EPCDEL', Settings.SimEPC.Delay ) ;
     Settings.SimEPC.SineAmplitude := GetKeyValue( Header, 'EPCSINEAMP', Settings.SimEPC.SineAmplitude) ;
     Settings.SimEPC.SineFrequency := GetKeyValue( Header, 'EPCSINEFREQ', Settings.SimEPC.SineFrequency) ;
     Settings.SimEPC.RandomEvents := GetKeyValue( Header, 'EPCRAND', Settings.SimEPC.RandomEvents) ;
     Settings.SimEPC.UnitsIndex := GetKeyValue( Header, 'EPCUNITS', Settings.SimEPC.UnitsIndex) ;
     Settings.SimEPC.ReleaseProbability := GetKeyValue( Header, 'EPCPROB', Settings.SimEPC.ReleaseProbability) ;
     Settings.SimEPC.ReleasablePool := GetKeyValue( Header, 'EPCPOOL', Settings.SimEPC.ReleasablePool) ;
     Settings.SimEPC.Depression := GetKeyValue( Header, 'EPCDEP', Settings.SimEPC.Depression) ;
     Settings.SimEPC.TauDepression := GetKeyValue( Header, 'EPCTAUDEP', Settings.SimEPC.TauDepression) ;

     // Load main form size and position
     Main.Width := GetKeyValue( Header, 'FORMWIDTH', Main.Width ) ;
     Main.Height := GetKeyValue( Header, 'FORMHEIGHT', Main.Height ) ;
     Main.Top := GetKeyValue( Header, 'FORMTOP', Main.Top ) ;
     Main.Left := GetKeyValue( Header, 'FORMLEFT', Main.Left  ) ;

     Header.Free ;

     end ;


procedure TEDRFile.SaveInitializationFile( const IniFileName : string ) ;
{ --------------------------------------------
  Save program settings to Initialization file
  --------------------------------------------}
var
   Header : TStringList ;
   ch : Integer ;
begin


     // Create file header Name=Value string list
     Header := TStringList.Create ;

     // Record duration
     AddKeyValue( Header, 'RECDUR', Settings.RecordDuration ) ;

     AddKeyValue( Header, 'RECCONT', Settings.ContinuousRecording ) ;

     AddKeyValue( Header, 'STARTSTIMREC', Settings.StartStimulusOnRecord ) ;

     { Last raw data file used }
     //AddKeyValue( Header, 'FILE', CdrFH.FileName ) ;
     AddKeyValue( Header, '16BIT', Settings.Resolution16Bit ) ;

     AddKeyValue( Header, 'CEDI', Amplifier.CED1902.Input ) ;
     AddKeyValue( Header, 'CEDG', Amplifier.CED1902.Gain ) ;
     AddKeyValue( Header, 'CEDGV', Amplifier.CED1902.GainValue ) ;
     AddKeyValue( Header, 'CEDLP', Amplifier.CED1902.LPFilter ) ;
     AddKeyValue( Header, 'CEDHP', Amplifier.CED1902.HPFilter ) ;
     AddKeyValue( Header, 'CEDAC', Amplifier.CED1902.ACCoupled ) ;
     AddKeyValue( Header, 'CEDDCO', Amplifier.CED1902.DCOffset ) ;
     AddKeyValue( Header, 'CEDNF', Amplifier.CED1902.NotchFilter ) ;
     AddKeyValue( Header, 'CEDPO', Amplifier.CED1902.ComPort ) ;

     // Patch clamp amplifier data
     for ch := 1 to 2 do begin
         AddKeyValue( Header, format('AMP%d',[ch]),Amplifier.AmplifierType[ch] ) ;
         AddKeyValue( Header, format('AMPGAINCH%d',[ch]), Amplifier.GainTelegraphChannel[ch] ) ;
         AddKeyValue( Header, format('AMPMODECH%d',[ch]), Amplifier.ModeTelegraphChannel[ch] ) ;
         end ;

     { Recording settings }
     AddKeyValue( Header, 'TRG', Settings.TriggerMode ) ;
     AddKeyValue( Header, 'EXTTRIGAH', Settings.ExternalTriggerActiveHigh ) ;

     { Event detector parameters }
     AddKeyValue( Header, 'DETCH', Settings.EventDetector.Channel ) ;
     AddKeyValue( Header, 'DETDM', Settings.EventDetector.DetectionMode ) ;
     AddKeyValue( Header, 'DETRS', Settings.EventDetector.RecordSize ) ;
     AddKeyValue( Header, 'DETYT', Settings.EventDetector.yThreshold ) ;
     AddKeyValue( Header, 'DETTT', Settings.EventDetector.tThreshold ) ;
     AddKeyValue( Header, 'DETDD', Settings.EventDetector.DeadTime ) ;
     AddKeyValue( Header, 'DETBAI', Settings.EventDetector.BaselineAveragingInterval ) ;
     AddKeyValue( Header, 'DETTAUR', Settings.EventDetector.TauRise ) ;
     AddKeyValue( Header, 'DETTAUD', Settings.EventDetector.TauDecay ) ;
     AddKeyValue( Header, 'DETPTF', Settings.EventDetector.PreTriggerFraction ) ;
     AddKeyValue( Header, 'DETAW', Settings.EventDetector.AnalysisWindow ) ;
     AddKeyValue( Header, 'DETREW', Settings.EventDetector.RisingEdgeWindow ) ;
     AddKeyValue( Header, 'DETAVFI', Settings.EventDetector.AvgFrequencyInterval ) ;
     AddKeyValue( Header, 'DETEBT', Settings.EventDetector.EnableBaselineTracking ) ;
     AddKeyValue( Header, 'DETAMPSDSCALE', Settings.EventDetector.AmpSDScale ) ;

     AddKeyValue( Header, 'VARRS', Settings.Variance.RecordSize ) ;
     AddKeyValue( Header, 'VAROV', Settings.Variance.RecordOverlap ) ;
     AddKeyValue( Header, 'VARTR', Settings.Variance.TauRise ) ;
     AddKeyValue( Header, 'VARTD', Settings.Variance.TauDecay ) ;

     AddKeyValue( Header, 'UNITC', Settings.DwellTimes.UnitCurrent ) ;
     AddKeyValue( Header, 'DWTTH', Settings.DwellTimes.Threshold ) ;

     AddKeyValue( Header, 'NRQ', Settings.NumTriggerSweeps ) ;

//     AddKeyValue( Header, 'VCDIV', Settings.VCommand[0].DivideFactor ) ;
//     AddKeyValue( Header, 'VCHOLD', Settings.VCommand[0].HoldingVoltage ) ;
//     AddKeyValue( Header, 'VCHOLD2', Settings.VCommand[0].HoldingVoltageAlt ) ;

//     for i := 0 to High(Settings.VCommand) do begin
//         AddKeyValue( Header, format('VCDIV%d',[i]), Settings.VCommand[i].DivideFactor ) ;
//         AddKeyValue( Header, format('VCHOLD%d',[i]), Settings.VCommand[i].HoldingVoltage ) ;
//         AddKeyValue( Header, format('VCHOLD%d2',[i]), Settings.VCommand[i].HoldingVoltageAlt ) ;
//         end ;

 //    AddKeyValue( Header, 'DIGPORT', Settings.DigitalOutputs ) ;
     AddKeyValue( Header, 'DTMINDAC', Settings.MinDACInterval ) ;
     AddKeyValue( Header, 'TUNITS', Settings.TUnits ) ;

     { Pipette seal test settings }
     AddKeyValue( Header, 'STPH', Settings.SealTest.PulseHeight ) ;
     AddKeyValue( Header, 'STPH1', Settings.SealTest.PulseHeight1 ) ;
     AddKeyValue( Header, 'STPH2', Settings.SealTest.PulseHeight2 ) ;
     AddKeyValue( Header, 'STPH3', Settings.SealTest.PulseHeight3 ) ;
     AddKeyValue( Header, 'STHV1', Settings.SealTest.HoldingVoltage1 ) ;
     AddKeyValue( Header, 'STHV2', Settings.SealTest.HoldingVoltage2 ) ;
     AddKeyValue( Header, 'STHV3', Settings.SealTest.HoldingVoltage3 ) ;
     AddKeyValue( Header, 'STPW', Settings.SealTest.PulseWidth ) ;
     AddKeyValue( Header, 'STCCH', Settings.SealTest.CurrentChannel ) ;
     AddKeyValue( Header, 'STVCH', Settings.SealTest.VoltageChannel ) ;
     AddKeyValue( Header, 'STUSE', Settings.SealTest.Use ) ;
     AddKeyValue( Header, 'STDSC', Settings.SealTest.DisplayScale ) ;
     AddKeyValue( Header, 'STASC', Settings.SealTest.AutoScale ) ;
     AddKeyValue( Header, 'STFRU', Settings.SealTest.FreeRun ) ;
     AddKeyValue( Header, 'STNAV', Settings.SealTest.NumAverages ) ;
     AddKeyValue( Header, 'STZAPA', Settings.SealTest.ZapAmplitude ) ;
     AddKeyValue( Header, 'STZAPD', Settings.SealTest.ZapDuration ) ;
     AddKeyValue( Header, 'STGAP', Settings.SealTest.GaFromPeak ) ;

     AddKeyValue( Header, 'NC', Settings.NumChannels ) ;
     //AddKeyValue( Header, 'ADVRI', Settings.ADCVoltageRangeIndex ) ;
     AddKeyValue( Header, 'DT', Settings.ADCSamplingInterval ) ;

     { Width/height of clipboard bitmaps }
     AddKeyValue( Header, 'BMAPW', Settings.BitmapWidth ) ;
     AddKeyValue( Header, 'BMAPH', Settings.BitmapHeight ) ;

     AddKeyValue( Header, 'DISPGRID', Settings.DisplayGrid ) ;
//     AddKeyValue( Header, 'DISPNVG', Settings.NumVerticalGridLines ) ;
//     AddKeyValue( Header, 'DISPNHG', Settings.NumHorizontalGridLines ) ;
     AddKeyValue( Header, 'DISPDUR', Settings.DisplayDuration ) ;
     AddKeyValue( Header, 'FIXZERO', Settings.FixedZeroLevels) ;

     { Plotting page settings }
     AddKeyValue( Header, 'PLTPM',Settings.Plot.TopMargin ) ;
     AddKeyValue( Header, 'PLBTM',Settings.Plot.BottomMargin ) ;
     AddKeyValue( Header, 'PLLFM',Settings.Plot.LeftMargin ) ;
     AddKeyValue( Header, 'PLRTM',Settings.Plot.RightMargin ) ;
     AddKeyValue( Header, 'PLFNT',Settings.Plot.FontName ) ;
     AddKeyValue( Header, 'PLFSI',Settings.Plot.FontSize ) ;
     AddKeyValue( Header, 'PLLTH',Settings.Plot.LineThickness ) ;
     AddKeyValue( Header, 'PLSHL',Settings.Plot.ShowLines ) ;
     AddKeyValue( Header, 'PLMKS',Settings.Plot.MarkerSize ) ;
     AddKeyValue( Header, 'PLSHM',Settings.Plot.ShowMarkers ) ;
     AddKeyValue( Header, 'PLCOL',Settings.Plot.UseColor ) ;

     // Page display module (PageView.pas) settings
     AddKeyValue( Header, 'PVLPP',Settings.PageViewLinesPerPage ) ;
     AddKeyValue( Header, 'PVLD',Settings.PageViewLineDuration ) ;

     AddKeyValue( Header, 'DDIR', DataDirectory ) ;

     AddKeyValue( Header, 'LABINT',Settings.LaboratoryInterface ) ;
     AddKeyValue( Header, 'LABDEV',Settings.DeviceNumber ) ;
     AddKeyValue( Header, 'LABADIP',Settings.ADCInputMode ) ;

     { Recently used data files }
     AddKeyValue( Header, 'RF0', Settings.RecentFiles[0] ) ;
     AddKeyValue( Header, 'RF1', Settings.RecentFiles[1] ) ;
     AddKeyValue( Header, 'RF2', Settings.RecentFiles[2] ) ;
     AddKeyValue( Header, 'RF3', Settings.RecentFiles[3] ) ;

     AddKeyValue( Header, 'CAPCMMX', Settings.Capacity.CmDisplayMax ) ;
     AddKeyValue( Header, 'CAPGSMX', Settings.Capacity.GsDisplayMax ) ;
     AddKeyValue( Header, 'CAPGMMX', Settings.Capacity.GmDisplayMax ) ;
     AddKeyValue( Header, 'CAPFREQ', Settings.Capacity.Frequency ) ;
     AddKeyValue( Header, 'CAPVREV', Settings.Capacity.VRev ) ;
     AddKeyValue( Header, 'CAPGR', Settings.Capacity.GrealChan) ;
     AddKeyValue( Header, 'CAPGI', Settings.Capacity.GimagChan) ;
     AddKeyValue( Header, 'CAPIM', Settings.Capacity.ImChan) ;
     AddKeyValue( Header, 'CAPVM', Settings.Capacity.VmChan) ;
     AddKeyValue( Header, 'CAPGM', Settings.Capacity.GmChan) ;
     AddKeyValue( Header, 'CAPGS', Settings.Capacity.GsChan) ;
     AddKeyValue( Header, 'CAPCM', Settings.Capacity.CmChan) ;
     AddKeyValue( Header, 'CAPINVGR', Settings.Capacity.InvertGReal) ;
     AddKeyValue( Header, 'CAPINVGI', Settings.Capacity.InvertGImag) ;
     AddKeyValue( Header, 'CAPRSCOMP', Settings.Capacity.RSeriesComp ) ;
     AddKeyValue( Header, 'CAPCCOMP', Settings.Capacity.CellCapacityComp ) ;
     AddKeyValue( Header, 'CAPCOMPINUSE', Settings.Capacity.CapacityCompensationInUse) ;
     AddKeyValue( Header, 'CAPINUSE', Settings.Capacity.InUse) ;

     // Fluorescence settings
     AddKeyValue( Header, 'FLRINUSE', Settings.Fluorescence.InUse) ;
     AddKeyValue( Header, 'FLRMAX', Settings.Fluorescence.RMax ) ;
     AddKeyValue( Header, 'FLRMIN', Settings.Fluorescence.RMin ) ;
     AddKeyValue( Header, 'FLKEFF', Settings.Fluorescence.KEff ) ;
     AddKeyValue( Header, 'FLTHRE', Settings.Fluorescence.FThreshold ) ;
     AddKeyValue( Header, 'FLNUM', Settings.Fluorescence.NumerChan ) ;
     AddKeyValue( Header, 'FLDEN', Settings.Fluorescence.DenomChan ) ;
     AddKeyValue( Header, 'FLRAT', Settings.Fluorescence.RatioChan ) ;
     AddKeyValue( Header, 'FLCON', Settings.Fluorescence.ConcChan ) ;
     AddKeyValue( Header, 'FLRDMX', Settings.Fluorescence.RatioDisplayMax ) ;
     AddKeyValue( Header, 'FLCDMX', Settings.Fluorescence.ConcDisplayMax ) ;
     AddKeyValue( Header, 'FLIONNAME', Settings.Fluorescence.IonName ) ;
     AddKeyValue( Header, 'FLIONUNITS', Settings.Fluorescence.IonUnits ) ;

     // Real time event frequency settings
     AddKeyValue( Header, 'EFINUSE', Settings.RTEventAnalysis.InUse) ;
     AddKeyValue( Header, 'EFCH', Settings.RTEventAnalysis.Channel ) ;
     AddKeyValue( Header, 'EFTHRESH', Settings.RTEventAnalysis.DetectionThreshold ) ;
     AddKeyValue( Header, 'EFRMEAN', Settings.RTEventAnalysis.RunningMeanTime ) ;
     AddKeyValue( Header, 'EFDEADT', Settings.RTEventAnalysis.DeadTime ) ;
     AddKeyValue( Header, 'EFCOUNTI', Settings.RTEventAnalysis.CountingInterval ) ;

     // Real time resistance settings
     AddKeyValue( Header, 'RESINUSE', Settings.RTResistance.InUse) ;
     AddKeyValue( Header, 'RESICH', Settings.RTResistance.ImChannel ) ;
     AddKeyValue( Header, 'RESVCH', Settings.RTResistance.VmChannel ) ;
     AddKeyValue( Header, 'RESAMPLITUDE', Settings.RTResistance.Amplitude ) ;
     AddKeyValue( Header, 'RESDUR', Settings.RTResistance.Duration ) ;
     AddKeyValue( Header, 'RESINT', Settings.RTResistance.Interval ) ;
     AddKeyValue( Header, 'RESPLT', Settings.RTResistance.Plot ) ;

     // Currently selected stimulus file
     AddKeyValue( Header, 'STIMFILE', Settings.VProgramFileName ) ;

     // Save post-synaptic current simulation settings
     AddKeyValue( Header, 'EPCDUR', Settings.SimEPC.Duration) ;
     AddKeyValue( Header, 'EPCAMP', Settings.SimEPC.Amplitude) ;
     AddKeyValue( Header, 'EPCAMPSD', Settings.SimEPC.AmplitudeSD) ;
     AddKeyValue( Header, 'EPCTRISE', Settings.SimEPC.TauRise) ;
     AddKeyValue( Header, 'EPCTDECAY', Settings.SimEPC.TauDecay) ;
     AddKeyValue( Header, 'EPCNOISESD', Settings.SimEPC.NoiseSD) ;
     AddKeyValue( Header, 'EPCFREQ', Settings.SimEPC.Frequency) ;
     AddKeyValue( Header, 'EPCFREQSD', Settings.SimEPC.FrequencySD) ;
     AddKeyValue( Header, 'EPCDEL', Settings.SimEPC.Delay ) ;
     AddKeyValue( Header, 'EPCSINEAMP', Settings.SimEPC.SineAmplitude) ;
     AddKeyValue( Header, 'EPCSINEFREQ', Settings.SimEPC.SineFrequency) ;
     AddKeyValue( Header, 'EPCRAND', Settings.SimEPC.RandomEvents) ;
     AddKeyValue( Header, 'EPCUNITS', Settings.SimEPC.UnitsIndex) ;
     AddKeyValue( Header, 'EPCPROB', Settings.SimEPC.ReleaseProbability) ;
     AddKeyValue( Header, 'EPCPOOL', Settings.SimEPC.ReleasablePool) ;
     AddKeyValue( Header, 'EPCDEP', Settings.SimEPC.Depression) ;
     AddKeyValue( Header, 'EPCTAUDEP', Settings.SimEPC.TauDepression) ;

     // Save main form size and position
     AddKeyValue( Header, 'FORMTOP',Main.Top ) ;
     AddKeyValue( Header, 'FORMLEFT',Main.Left ) ;
     AddKeyValue( Header, 'FORMWIDTH',Main.Width ) ;
     AddKeyValue( Header, 'FORMHEIGHT',Main.Height ) ;

     // Save Name=Value list to INI file
     Header.SaveToFile( IniFileName ) ;

     // Free List
     Header.Free ;

     end ;


procedure TEDRFile.LoadDataFiles( FileName : string ) ;
{ -------------------------------------------
  Load EDR data file and any associated files
  -------------------------------------------}
var
   ch : Integer ;
   OK : Boolean ;
begin

     OK := False ;
     if FileExists( FileName ) then OK := True ;
     if OK then begin
        if (FileGetAttr(FileName) AND faReadOnly) <> 0 then begin
           OK := False ;
           ShowMessage( FileName + ' is READ-ONLY. Unable to open!' ) ;
           end ;
        end ;

     if OK then begin
        { Open data file }
        CdrFH.Filename := ChangeFileExt(FileName,DataFileExtension) ;
        CdrFH.FileHandle := FileOpen( CdrFH.FileName, fmOpenReadWrite ) ;
        Main.Caption := 'WinEDR ' + Version + ' ' + FileName ;
        if CdrFH.Filehandle >= 0 then begin
            { Load the raw file details }
            GetHeader( CdrFH ) ;
            { Make sure all channels are visible }
            for ch := 0 to CdrFH.NumChannels-1 do Channel[ch].InUse := True ;
            WriteToLogFile( CdrFH.FileName + ' opened' ) ;
            // Update open windows with new file info.
            Main.UpdateMDIWindows ;
            end
        else begin
             ShowMessage( format(' File Error =%d',[CdrFH.Filehandle]) ) ;
             OK := False ;
             end ;
        end ;

     // Close windows if file could not be opened
     if not OK then Main.CloseFormsAndDataFile( AllForms ) ;

     Main.SetMenus ;

     end ;


function TEDRFile.CreateNewDataFile(
         var FHeader : TCDRFileHeader
         ) : Boolean ;
{ --------------------------
  Create a new EDR data file
  --------------------------
  10/7/01 ... Associated .REC and .WCE files now deleted when new file created }
var
   ch : Integer ;
begin
     { Close any existing file }
     if FHeader.FileHandle >= 0 then
        begin
        FileClose( FHeader.FileHandle ) ;
        FHeader.FileHandle := -1 ;
        end ;

     { Create a new file }
     FHeader.FileHandle := FileCreate( FHeader.FileName ) ;
     if FHeader.FileHandle >= 0 then
        begin

        { No data points }
        FHeader.NumSamplesInFile := 0 ;
        { No saved file }
        FHeader.WCPFileName := '' ;
        { No back up file }
        FHeader.BackedUp := False ;

        FHeader.CreationTime := DateTimeToStr(Now) ;

        { Turn all channels on }
        for ch := 0 to FHeader.NumChannels-1 do Channel[ch].InUse := True ;
        // Clear markers
        MarkerList.Clear ;

        { Save initial values to header block }
        SaveHeader( FHeader ) ;
        // Delete event file associated with a previous data file of same name
        if FileExists(ChangeFileExt(FHeader.FileName,EventFileExtension)) then
           DeleteFile(PChar(ChangeFileExt(FHeader.FileName,EventFileExtension))) ;
        // Delete variance record file associated with a previous data file of same name
        if FileExists(ChangeFileExt(FHeader.FileName,'.rec')) then
           DeleteFile(PChar(ChangeFileExt(FHeader.FileName,'.rec'))) ;

        Result := True ;
        end
     else begin
        ShowMessage( 'Error: Could not create ' + FHeader.FileName ) ;
        Result := False ;
        end ;

     Main.Caption := 'File: ' + CDRFH.FileName ;
     end ;


procedure TEDRFile.DataModuleCreate(Sender: TObject);
// ---------------------------------------
// Initialise settings when module created
// ---------------------------------------
var
    ch : Integer ;
begin

      { Create channel names list }
      ChannelNames := TStringList.Create ;

      { Create default set of record types }
      RecordTypes := TStringList.Create ;
      RecordTypes.Add( 'ALL' ) ;
      RecordTypes.Add( 'EVOK' ) ;
      RecordTypes.Add( 'MINI' ) ;
      RecordTypes.Add( 'FAIL' ) ;
      RecordTypes.Add( 'TEST' ) ;
      RecordTypes.Add( 'LEAK' ) ;

     { Default values for channels }

     CdrFH.NumChannels := 1 ;
     CdrFH.NumSamplesInFile := 0 ;
     CdrFH.NumBytesInHeader := MinBytesInHeader ;
     CdrFH.ADCVoltageRange := 5.0 ;
     CdrFH.dt := 0.001 ;
     CdrFH.Version := 6.2 ;
     for ch := 0 to EDRChannelLimit do
         begin
         Channel[ch].TimeZero := 1. ;
         Channel[ch].ADCScale := 1. ;
         Channel[ch].CursorIndex := 128 ;
         Channel[ch].ZeroIndex := 0 ;
         Channel[ch].Cursor0 := 0 ;
         Channel[ch].Cursor1 := 256 ;
         { Zero levels fixed at hardware zero }
         Channel[ch].ADCZero := 0 ;
         Channel[ch].ADCZeroAt := -1 ;
         Channel[ch].ADCCalibrationFactor := 0.001 ;
         Channel[ch].ADCAmplifierGain := 1. ;
         Channel[ch].ADCCalibrationBar := 0.0 ;
         Channel[ch].ADCUnits := 'mV' ;
         Channel[ch].ADCName := format('Ch.%d',[ch]);
         Channel[ch].ChannelOffset := ch ;
         Channel[ch].color := clBlue ;
         Channel[ch].xMin := 0. ;
         Channel[ch].xMax := CdrFH.NumSamples-1 ;
         Channel[ch].yMin := -32768 ;
         Channel[ch].yMax := 32767 ;
         Channel[ch].InUse := True ;
         //Channel[ch] := Channel[ch] ;
         end ;

     CdrFH.WCPFileName := '' ;

     { Initialise to no laboratory interface }
     Settings.LaboratoryInterface := NoInterface12 ;
     Settings.DeviceNumber := 1 ;
     Settings.ADCSamplingInterval := 0.001 ;
     //Settings.ADCVoltageRangeIndex := 0 ;
     Settings.NumChannels := 1 ;
     Settings.StartStimulusOnRecord := False ;

     Settings.TriggerMode := 'F' ;
     Settings.ExternalTriggerActiveHigh := False ;
     Settings.EventDetector.Channel := 0 ;
     Settings.EventDetector.DetectionMode := 0 ;
     Settings.EventDetector.yThreshold := 0. ;
     Settings.EventDetector.tThreshold := CdrFH.dt*10.0 ;
     Settings.EventDetector.BaselineAveragingInterval := 1.0 ;
     Settings.EventDetector.PreTriggerFraction := 0.1 ;
     Settings.EventDetector.AnalysisWindow := 512 ;
     Settings.EventDetector.RecordSize := 512 ;
     Settings.EventDetector.DeadTime := CdrFH.dt*Settings.EventDetector.RecordSize ;
     Settings.EventDetector.TauRise := CdrFH.dt*2 ;
     Settings.EventDetector.TauDecay := CdrFH.dt*100.0 ;
     Settings.EventDetector.PositivePeaks := True ;
     Settings.EventDetector.Baseline := 0 ;
     Settings.EventDetector.SubtractBaseline := False ;
     Settings.EventDetector.NumBaselinePoints := 20 ;
     Settings.EventDetector.NumBaselineGap := 0 ;
     Settings.EventDetector.TDecayLevel := 90.0 ;
     Settings.EventDetector.TDecayFrom := 0 ;
     Settings.EventDetector.TDecayTo := 0 ;
     Settings.EventDetector.RisingEdgeWindow := 20 ;
     Settings.EventDetector.AvgFrequencyInterval := 1.0 ;
     Settings.EventDetector.EnableBaselineTracking := True ;
     Settings.EventDetector.Alignment := 0 ;
     Settings.EventDetector.AmpSDScale := 4.0 ;

     Settings.DwellTimes.ChanNum := 0 ;
     Settings.DwellTimes.RecordSize := 512 ;
     Settings.DwellTimes.Threshold := 0.5 ;

     Settings.ContinuousRecording := True ;
     Settings.NumTriggerSweeps := 1 ;
     Settings.RecordDuration := 10. ;
     Settings.DisplayDuration := 10.0 ;
     Settings.DisplayGrid := True ;
     Main.mnDisplayGrid.Checked := Settings.DisplayGrid ;

     Settings.FixedZeroLevels := False ;

     Settings.CutOffFrequency := 0. ;
     Settings.NewCalculation := False ;

     { Minimum interval for updating D/A converters when
       generating command voltage waveforms }
     Settings.MinDACInterval := 0.001 ;

     Settings.TUnits := 's' ;
     Settings.TScale := 1.0 ;
     Settings.TUnScale := 1.0 ;
     Settings.TBarValue := 0.0 ;

     { Name of command voltage protocol file in current use }
     Settings.VProgramFileName := '' ;

     { Divide factor that the patch/voltage clamp applies to its
       command voltage input. The D/A output voltage is thus scaled up
       by this factor }
//     Main.VProtDirectory := Settings.ProgDirectory + 'Stim\' ;

      { Get directory which contains EDR program }
      ProgDirectory := ExtractFilePath(ParamStr(0)) ;
      Application.HelpFile := ProgDirectory + 'WinEDR.chm';

     // Create settings directory (GetSpecialFolder(CSIDL_COMMON_DOCUMENTS))
     SettingsDirectory := TPath.GetSharedDocumentsPath + '\WinEDR\' ;

     if (not System.SysUtils.DirectoryExists(SettingsDirectory,True)) then
        begin
        if System.SysUtils.ForceDirectories(SettingsDirectory) then
           WriteToLogFile( 'Settings folder ' + SettingsDirectory + ' created.')
        else WriteToLogFile( 'Unable to create settings folder' + SettingsDirectory) ;
        end ;
     SettingsFileName := SettingsDirectory + 'winedr.ini' ;

     // Stimulus protocols folder
     VProtDirectory := TPath.GetSharedDocumentsPath + '\WinEDR\stim\';
      if not System.SysUtils.DirectoryExists(VProtDirectory) then begin
         if System.SysUtils.ForceDirectories(VProtDirectory) then
            WriteToLogFile( 'Protocols folder ' + VProtDirectory + ' created.')
         else WriteToLogFile( 'Unable to create protocols folder' + VProtDirectory) ;
         end ;

      // Create default data directory (in My Documents)
      DataDirectory := TPath.GetSharedDocumentsPath + '\WinEDR Data\';
      if not System.SysUtils.DirectoryExists(DataDirectory) then begin
         if System.SysUtils.ForceDirectories(DataDirectory) then
            WriteToLogFile( 'Data folder ' + DataDirectory + ' created.')
         else WriteToLogFile( 'Unable to create data folder' + DataDirectory) ;
         end ;

    { for i := 0 to High(Settings.VCommand) do begin
         Settings.VCommand[i].DivideFactor := 1. ;
         Settings.VCommand[i].HoldingVoltage := 0. ;
         Settings.VCommand[i].HoldingVoltageIncrement := 0. ;
         Settings.VCommand[i].HoldingVoltageAlt := 0. ;
         end ; }

  //   Settings.DigitalOutputs := 0 ;
     Settings.UpdateOutputs := True ;
     Settings.DACSelected := 0 ;

     { Default settings for seal test pulse }
     Settings.SealTest.Use := 1 ;
     Settings.SealTest.PulseHeight1 := 0.01 ;
     Settings.SealTest.HoldingVoltage1 := 0. ;
     Settings.SealTest.PulseHeight2 := 0.01 ;
     Settings.SealTest.HoldingVoltage2 := 0. ;
     Settings.SealTest.PulseHeight3 := 0.0 ;
     Settings.SealTest.HoldingVoltage3 := 0. ;

     Settings.SealTest.PulseWidth:= 0.03 ;
     Settings.SealTest.CurrentChannel := 0 ;
     Settings.SealTest.VoltageChannel := 1 ;
     Settings.SealTest.AutoScale := True ;
 //    Settings.SealTest.DACNum := 0 ;
     Settings.SealTest.NumAverages := 10 ;

     { Set flag indicating this is the first sweep, to force an autoscale }
     Settings.SealTest.FirstSweep := True ;

     Settings.Colors.Cursors := clWhite ;
     Settings.Colors.Grid := clAqua ;

     Settings.Plot.TopMargin := 50.0 ;
     Settings.Plot.LeftMargin := 50.0 ;
     Settings.Plot.BottomMargin := 50.0 ;
     Settings.Plot.RightMargin := 25.0 ;
     Settings.Plot.FontName := 'Arial' ;
     Settings.Plot.FontSize := 12 ;
     Settings.Plot.LineThickness := 2 ;
     Settings.Plot.MarkerSize := 5 ;
     Settings.Plot.ShowLines := True ;
     Settings.Plot.ShowMarkers := True ;
     Settings.Plot.MetafileWidth := 600 ;
     Settings.Plot.MetafileHeight := 500 ;

     { Bitmap size for images copied to clipboard }
     Settings.BitmapWidth := 600 ;
     Settings.BitmapHeight := 500 ;

     DataDirectory := '' ;

     { Settings for record hard copy plots }
     Settings.ShowLabels := True ;
     Settings.ShowZeroLevels := True ;

     { Default settings for fluorescence ratio analysis }
     Settings.Fluorescence.InUse := False ;
     Settings.Fluorescence.MinChannels := 3 ;
     Settings.Fluorescence.RMax := 1.0 ;
     Settings.Fluorescence.RMin := 0.0 ;
     Settings.Fluorescence.KEff := 1.0 ;
     Settings.Fluorescence.FThreshold := 0.0 ;
     Settings.Fluorescence.NumerChan := 0 ;
     Settings.Fluorescence.DenomChan := 1 ;
     Settings.Fluorescence.RatioChan := 2 ;
     Settings.Fluorescence.ConcChan := 3 ;
     Settings.Fluorescence.RatioDisplayMax := 10.0 ;
     Settings.Fluorescence.ConcDisplayMax := 1.0 ;
     Settings.Fluorescence.IonName := 'Ca' ;
     Settings.Fluorescence.IonUnits := 'uM' ;

     { Default settings for cell capacity analysis }
     Settings.Capacity.InUse := False ;
     Settings.Capacity.MinChannels := 7 ;
     Settings.Capacity.ImChan := 0 ;
     Settings.Capacity.VmChan := 1 ;
     Settings.Capacity.GrealChan := 2 ;
     Settings.Capacity.GimagChan := 3 ;
     Settings.Capacity.GmChan := 4 ;
     Settings.Capacity.GsChan := 5 ;
     Settings.Capacity.CmChan := 6 ;
     Settings.Capacity.CmDisplayMax := 200.0 ;
     Settings.Capacity.GmDisplayMax := 10.0 ;
     Settings.Capacity.GsDisplayMax := 500.0 ;
     Settings.Capacity.Frequency := 1000.0 ;
     Settings.Capacity.VRev := 0.0 ;
     Settings.Capacity.InvertGReal := False ;
     Settings.Capacity.InvertGImag := False ;
     Settings.Capacity.GChannelsUseGainTelegraph := False ;
     Settings.Capacity.SineWaveRMS := 0.01 ;
     Settings.Capacity.CapacityCompensationInUse := False ;
     Settings.Capacity.RSeriesComp := 0.0 ;
     Settings.Capacity.CellCapacityComp := 0.0 ;

     // Default settings for realtime event analysis
     Settings.RTEventAnalysis.InUse := False ;
     Settings.RTEventAnalysis.Channel := 0 ;
     Settings.RTEventAnalysis.DetectionThreshold := 10.0 ;
     Settings.RTEventAnalysis.RunningMeanTime := 1.0 ;
     Settings.RTEventAnalysis.DeadTime := 0.1 ;
     Settings.RTEventAnalysis.CountingInterval := 1.0 ;

     // Default settings for real-time resistance analysis
     Settings.RTResistance.InUse := False ;
     Settings.RTResistance.ImChannel := 0 ;
     Settings.RTResistance.VmChannel := 1 ;
     Settings.RTResistance.Amplitude := 0.01 ;
     Settings.RTResistance.Duration := 0.02 ;
     Settings.RTResistance.Interval := 1.0 ;
     Settings.RTResistance.Plot := 2 ;

     Settings.PageViewLinesPerPage := 8 ;
     Settings.PageViewLineDuration := 1.0 ;

    // Simulated post-synaptic current settings
    Settings.SimEPC.Duration := 10.0 ;
    Settings.SimEPC.Amplitude := 1.0 ;
    Settings.SimEPC.AmplitudeSD := 0.1 ;
    Settings.SimEPC.TauRise := 1E-4 ;
    Settings.SimEPC.TauDecay := 2E-3 ;
    Settings.SimEPC.NoiseSD := 0.05 ;
    Settings.SimEPC.Frequency := 10.0 ;
    Settings.SimEPC.FrequencySD := 0.0 ;
    Settings.SimEPC.Delay := 0.0 ;
    Settings.SimEPC.RandomEvents := True ;
    Settings.SimEPC.SineAmplitude := 0.0 ;
    Settings.SimEPC.SineFrequency := 50.0 ;
    Settings.SimEPC.UnitsIndex := 0 ;
    Settings.SimEPC.ReleaseProbability := 1.0 ;
    Settings.SimEPC.ReleasablePool := 100 ;
    Settings.SimEPC.Depression := 0.0 ;
    Settings.SimEPC.TauDepression := 1.0  ;

    Cm := 0.0 ;
    Gm := 0.0 ;
    Ga := 0.0 ;
    RSeal := 0.0 ;

     MarkerList := TStringList.Create ;

     { Set the file names and handles for all header blocks to null }
     CdrFH.FileHandle := -1 ;
     CdrFH.FileName := '' ;

     Main.SetMenus ;

     // Load initialization file to get name of last data file used }
     LoadInitializationFile( SettingsFileName ) ;
     if DataDirectory = '' then DataDirectory := TPath.GetDocumentsPath + '\WinEDR Data\';
     if VProtDirectory = '' then VProtDirectory := TPath.GetDocumentsPath + '\WinEDR\stim\';

end;


procedure TEDRFile.UpdateChannelScalingFactors ;
// ------------------------------
// Update channel scaling factors
// ------------------------------
var
   ch : Integer ;
begin
     for ch := 0 to CDRFH.NumChannels-1 do begin

         // Ensure that calibration factor is non-zero
         if Channel[ch].ADCCalibrationFactor = 0.0 then
            Channel[ch].ADCCalibrationFactor := 0.001 ;

         // Ensure that amplifier gain is non-zero
         if Channel[ch].ADCAmplifierGain = 0.0 then
            Channel[ch].ADCAmplifierGain := 1.0 ;

         // Calculate bits->units scaling factor
         Channel[ch].ADCScale := Abs(CDRFH.ADCVoltageRange) /
                                (Channel[ch].ADCCalibrationFactor*
                                 Channel[ch].ADCAmplifierGain
                                 *(Channel[ch].ADCMaxValue+1) ) ;
         end ;
     end ;


procedure TEDRFile.DataModuleDestroy(Sender: TObject);
// --------------------------------------
// Free variables when form is destroyed
// --------------------------------------
begin
//     RecordTypes.Free ;
     ChannelNames.Free ;
     MarkerList.Free ;

end;

procedure TEDRFile.CloseDataFile ;
// --------------------------------
// Close data file (if one is open)
// --------------------------------
begin
     if  CdrFH.FileHandle >= 0 then
         begin
         FileClose(CdrFH.FileHandle) ;
         CdrFH.FileHandle := -1 ;
         //CdrFH.FileName := '' ;
         end ;

     end;


procedure TEDRFile.MakeBackupFile ;
{ -------------------------------------------------
  Copy data file to backup file with .BAK extension
  ------------------------------------------------- }
var
   FileName : string ;
   FileHandle : Integer ;
   NumBytes,nRead : Integer ;
   Buf : Array[0..255] of SmallInt ;
   Done : Boolean ;
begin

     if CdrFH.BackedUp then Exit ;

     { Create backup file }
     FileName := ChangeFileExt( cdrFH.FileName, '.bak' ) ;
     FileHandle := FileCreate( FileName ) ;
     if FileHandle < 0 then begin
        ShowMessage( 'Error creating ' + FileName ) ;
        Exit ;
        end ;

     // Save latest header block data
     CdrFH.BackedUp := True ;
     SaveHeader( CdrFH ) ;

     { Copy data to backup }

     { No. of bytes to be copied }
     NumBytes := FileSeek( CdrFH.FileHandle, 0, 2 ) + 1 ;

     { Point to start of data files }
     CdrFH.FilePointer := FileSeek( CdrFH.FileHandle, 0, 0 ) ;
     FileSeek( FileHandle, 0, 0 ) ;

     Done := False ;
     while not Done do begin
           // Read from EDR file
           nRead := FileRead(CdrFH.FileHandle,Buf,Sizeof(Buf)) ;
           // Write to backup file
           if nRead > 0 then begin
              if FileWrite(FileHandle,Buf,nRead) < nRead then begin
                 ShowMessage( 'Backup: Error writing ' + FileName ) ;
                 WriteToLogFile( 'Backup: Error writing ' + FileName ) ;
                 Done := True ;
                 end ;
              end
           else Done := True ;

           NumBytes := NumBytes - nRead ;
           if NumBytes <= 0 then Done := True ;

           end ;

     { Close backup file }
     FileClose( FileHandle ) ;

     Main.mnRestoreOriginal.Enabled := CdrFH.BackedUp ;

     WriteToLogFile( cdrFH.FileName + ' saved to ' + FileName ) ;

     end ;


procedure TEDRFile.RestoreFromBackupFile ;
{ -------------------------------------------------
  Restore original data file from backup file
  ------------------------------------------------- }
var
   FileName : string ;
   FileHandle : Integer ;
   NumBytes,nRead : Integer ;
   Buf : Array[0..255] of SmallInt ;
   Done : Boolean ;
begin

     // Create back up file name
     FileName := ChangeFileExt( cdrFH.FileName, '.bak' ) ;

     if (not CdrFH.BackedUp) or (not FileExists(FileName)) then Exit ;

     { Open backup file }
     FileHandle := FileOpen( FileName, fmOpenRead ) ;
     if FileHandle < 0 then begin
        ShowMessage( 'Restore: Unable to open ' + FileName ) ;
        Exit ;
        end ;

     { No. of bytes to be copied }
     NumBytes := FileSeek( FileHandle, 0, 2 ) + 1 ;

     { Point to start of files }
     FileSeek( FileHandle, 0, 0 ) ;
     CdrFH.FilePointer := FileSeek( CdrFH.FileHandle, 0, 0 ) ;

     { Copy data from backup }
     Done := False ;
     while not Done do begin

           // Read data from backup
           nRead := FileRead(FileHandle,Buf,Sizeof(Buf)) ;

           // Write to EDR file
           if nRead > 0 then begin
              if FileWrite(CdrFH.FileHandle,Buf,nRead) <> nRead then begin
                 ShowMessage( 'Restore: Error writing ' + CdrFH.FileName ) ;
                 WriteToLogFile( 'Restore: Error writing ' + FileName ) ;
                 Done := True ;
                 end ;
              end
           else Done := True ;

           NumBytes := NumBytes - nRead ;
           if NumBytes <= 0 then Done := True ;

           end ;

     { Update header record with restored data }
     GetHeader( CdrFH ) ;

     { Close backup file }
     FileClose( FileHandle ) ;

     WriteToLogFile( cdrFH.FileName + ' restored from ' + FileName ) ;

     end ;




procedure TEDRFile.OpenLogFile ;
{ -------------
  Open log file
  ------------- }
begin

     { Create a log file using current date }
    {$IF CompilerVersion > 7.0} formatsettings.DateSeparator := '-';
     {$ELSE} DateSeparator := '-';
     {$IFEND}

     LogFileName := SettingsDirectory + DateToStr(Date)+'.log' ;
     LogFileAvailable := True ;
     AssignFile( LogFile, LogFileName ) ;
     try
           if FileExists( LogFileName ) then Append(LogFile)
                                        else ReWrite(LogFile) ;
     except
           on EInOutError do begin
              //MessageDlg( ' WinWCP - Cannot create Log File', mtWarning, [mbOK], 0 ) ;
              LogFileAvailable := False ;
              end ;
           end ;

     end ;


procedure TEDRFile.WriteToLogFile( Line : string ) ;
{ ---------------------------------------------
  Write a date-stamped line of text to log file
  --------------------------------------------- }
begin
     if LogFileAvailable then WriteLn( LogFile, TimeToStr(Time) + ' ' + Line ) ;
     end ;


procedure TEDRFile.WriteToLogFileNoDate( Line : string ) ;
{ --------------------------------
  Write a line of text to log file
  -------------------------------- }
begin
     if LogFileAvailable then WriteLn( LogFile, Line ) ;
     end ;


procedure TEDRFile.CloseLogFile ;
{ --------------
  Close log file
  -------------- }
begin
     if LogFileAvailable then begin
        try
           CloseFile(LogFile) ;
        except
              on EInOutError do begin
              ShowMessage( ' WinWCP - Error closing Log File') ;
              LogFileAvailable := False ;
              end ;
           end ;
        end ;
     end ;


function TEDRFile.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : single       // Value
                               ) : Single ;         // Return value
// ------------------------------
// Get Key=Single Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     // Remove any '=' in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := ExtractFloat( s, Value ) ;
        end
     else Result := Value ;

end;


function TEDRFile.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : Integer       // Value
                               ) : Integer ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     // Remove any '=' in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;


function TEDRFile.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : NativeInt       // Value
                               ) : NativeInt ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin
     // Remove any '=' in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;


function TEDRFile.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : string       // Value
                               ) : string ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin
     // Remove any '=' in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

      idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := s ;
        end
     else Result := Value ;

end;


function TEDRFile.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : Boolean       // Value
                               ) : Boolean ;        // Return value
// ------------------------------
// Get Key=Boolean Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin
     // Remove any '=' in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        if ContainsText(s,'T') then Result := True
                               else Result := False ;
        end
     else Result := Value ;

end;

procedure TEDRFile.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : single        // Value
                                 ) ;
// ---------------------
// Add Key=Single Value to List
// ---------------------
begin

     // Remove any '=' characters in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     List.Add( Keyword + format('=%.4g',[Value]) ) ;
end;


procedure TEDRFile.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : Integer        // Value
                                 ) ;
// ---------------------
// Add Key=Integer Value to List
// ---------------------
begin

     // Remove any '=' characters in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     List.Add( Keyword + format('=%d',[Value]) ) ;
end;

procedure TEDRFile.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : NativeInt        // Value
                                 ) ;
// ---------------------
// Add Key=NativeInt Value to List
// ---------------------
begin

     // Remove any '=' characters in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     List.Add( Keyword + format('=%d',[Value] )) ;
end;


procedure TEDRFile.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : string        // Value
                                 ) ;
// ---------------------
// Add Key=string Value to List
// ---------------------
begin

     // Remove any '=' characters in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     List.Add( Keyword + '=' + Value ) ;
end;


procedure TEDRFile.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : Boolean        // Value
                                 ) ;
// ---------------------
// Add Key=boolean Value to List
// ---------------------
begin

     // Remove any '=' characters in keyword
     Keyword := ReplaceText( Keyword, '=', '' ) ;

     if Value then List.Add( Keyword + '= T' )
              else List.Add( Keyword + '= F' ) ;
end;


function TEDRFile.ExtractFloat ( CBuf : string ; Default : Single ) : extended ;
{ Extract a floating point number from a string which
  may contain additional non-numeric text }

var CNum : string ;
i : SmallInt ;

begin
     CNum := ' ' ;
     for i := 1 to length(CBuf) do begin
         if CharInSet( CBuf[i], ['0'..'9', 'E', 'e', '+', '-', '.', ',' ] ) then
            CNum := CNum + CBuf[i]
         else CNum := CNum + ' ' ;
         end ;

     { Correct for use of comma/period as decimal separator }
     if (formatsettings.DECIMALSEPARATOR = '.') and (Pos(',',CNum) <> 0) then
        CNum[Pos(',',CNum)] := formatsettings.DECIMALSEPARATOR ;
     if (formatsettings.DECIMALSEPARATOR = ',') and (Pos('.',CNum) <> 0) then
        CNum[Pos('.',CNum)] := formatsettings.DECIMALSEPARATOR ;

     try
        ExtractFloat := StrToFloat( CNum ) ;
     except
        on E : EConvertError do ExtractFloat := Default ;
        end ;
     end ;

function TEDRFile.ExtractInt ( CBuf : string ) : longint ;
{ Extract a 32 bit integer number from a string which
  may contain additional non-numeric text }
Type
    TState = (RemoveLeadingWhiteSpace, ReadNumber) ;
var
   CNum : string ;
   i : integer ;
   Quit : Boolean ;
   State : TState ;

begin
     CNum := '' ;
     i := 1;
     Quit := False ;
     State := RemoveLeadingWhiteSpace ;
     while not Quit do begin

           case State of

           { Ignore all non-numeric ansicharacters before number }
           RemoveLeadingWhiteSpace : begin
               if CharInSet( CBuf[i], ['0'..'9','E','e','+','-','.'] ) then State := ReadNumber
                                                            else i := i + 1 ;
               end ;

           { Copy number into string CNum }
           ReadNumber : begin
                { End copying when a non-numeric ansicharacter
                or the end of the string is encountered }
                if CharInSet( CBuf[i], ['0'..'9','E','e','+','-','.'] ) then begin
                   CNum := CNum + CBuf[i] ;
                   i := i + 1 ;
                   end
                else Quit := True ;
                end ;
           else end ;

           if i > Length(CBuf) then Quit := True ;
           end ;
     try
        ExtractInt := StrToInt( CNum ) ;
     except
        ExtractInt := 1 ;
        end ;
     end ;



function TEDRFile.ExtractFileNameOnly( FilePath : string ) : string ;
{ -----------------------------------------------------
  Extract file name (without extension) from file path
  ----------------------------------------------------}
var
   FileName : string ;
   FileExt : string ;
begin
     FileName := ExtractFileName(FilePath) ;
     FileExt := ExtractFileExt(FileName) ;
     Result := ReplaceText( FileName, FileExt, '') ;
     end ;


Function TEDRFile.GetFromEditBox( var ed : TEdit ;
                         Default, Min, Max : Single ;
                         const FormatString,Units : string ;
                         Scale : single ) : Single ;
{ --------------------------------------------------------------------
  Get a number from an edit box, ensure that it is within valid limits,
  and update the box with the value used.
  ed ... Edit box to get text from
  Default ... value to use if box does not contain valid data
  Min ... Minimum valid value
  Max ... Maximum valid value
  FormatString ... format used to update box
  Units ... units of value
  Scale ... Factor for scaling display units
  --------------------------------------------------------------------}
var
   Value : single ;
begin
     Value := ExtractFloat( ed.text, Default*Scale ) / Scale ;
     if Value < Min then Value := Abs(Value) ;
     if Value < Min then Value := Min ;
     if Value > Max then Value := Max ;
     ed.text := format( FormatString, [Value*Scale] ) + ' ' + Units ;
     Result := Value ;
     end ;


procedure TEDRFile.GetIntRangeFromEditBox( var ed : TEdit ; var Lo,Hi : LongInt ;
                                  Min,Max : LongInt ) ;
var
   LoValue,HiValue : single ;
begin
     {if ed.text = '' then ed.text := format( ' %d-%d', [Lo,Hi]) ;}
     GetRangeFromEditBox( ed, LoValue,HiValue, Min, Max,'%.0f-%.0f','' ) ;
     Lo := Trunc( LoValue ) ;
     Hi := Trunc( HiValue ) ;
     end ;


procedure TEDRFile.GetRangeFromEditBox( const ed : TEdit ;
                               var LoValue,HiValue : Single ;
                               Min,Max : Single ;
                               const FormatString : String ;
                               const Units : String ) ;
var
   Values : Array[0..10] of Single ;
   Temp : Single ;
   nValues : Integer ;
begin
     LoValue := Min ;
     HiValue := Max ;
     nValues := ExtractListofFloats( ed.text, Values, True ) ;
     if nValues >=1 then LoValue := Values[0] ;
     if nValues >=2 then HiValue := Values[1] ;
     if LoValue > HiValue then begin
        Temp := LoValue ;
        LoValue := HiValue ;
        HiValue := Temp ;
        end ;
     ed.text := format( FormatString, [LoValue,HiValue] ) + ' ' + Units ;
     end ;


procedure TEDRFile.PrintStringGrid( const Table : TStringGrid ) ;
{ -----------------------------------------------
  Print the contents of a string grid spreadsheet
  -----------------------------------------------}
var
   CharWidth,CharHeight,ColHeight,Row,Col,w : Integer ;
   PageLeft,PageTop,PageBottom,Line,ColLeft,PageNum,LastPage : Integer ;
   ColWidth : Array[0..20] of Integer ;
begin

     Screen.Cursor := crHourglass ;

     { Set print font and size }
     Printer.Canvas.font.name := Settings.Plot.FontName ;
     Printer.Canvas.font.Size := 10 ;

     CharWidth := Printer.canvas.TextWidth('X') ;
     CharHeight := Printer.canvas.TextHeight('X') ;
     PageTop := CharHeight*5 ;
     PageBottom := printer.PageHeight - PageTop ;
     PageLeft := CharWidth*8 ;

     Printer.BeginDoc ;

     { Calculate column widths of table}
     for col := 0 to Table.ColCount-1 do begin
         ColWidth[Col] := 0 ;
         for row := 0 to Table.RowCount-1 do begin
             w := Printer.canvas.TextWidth(Table.cells[Col,Row]) ;
             if ColWidth[Col] < w then ColWidth[Col] := w ;
             end ;
         end ;
     for col := 0 to Table.ColCount-1 do ColWidth[Col] := ColWidth[Col] +
                                           2*CharWidth ;

     ColHeight := (12*Printer.canvas.TextHeight(Table.cells[0,0])) div 10 ;

     { Calculate number of pages to be printed }
     LastPage := 0 ;
     PageNum := 1 ;
     Line := PageTop + ColHeight*3 ;
     for row := 0 to Table.RowCount-1 do begin
         if LastPage <> PageNum then begin
            Line := PageTop + ColHeight*3 ;
            LastPage := PageNum ;
            end ;
         Line := Line + ColHeight ;
         if Line > PageBottom then Inc(PageNum) ;
         end ;

     { Print table
       ===========}

     PageNum := -1 ;
     for row := 0 to Table.RowCount-1 do begin
         {Print header lines on each new page }
         if Printer.PageNumber <> PageNum then begin
            PageNum := Printer.PageNumber ;
            Line := PageTop ;
            printer.canvas.textout(PageLeft,Line, 'File ... ' + CdrfH.FileName
                                   + format(' ( Page %d of %d )',
                                            [PageNum,LastPage])) ;
            Line := Line + ColHeight ;
            printer.canvas.textout(PageLeft,Line, CDRfH.IdentLine) ;
            Line := Line + ColHeight*2 ;
            //NewPage := False ;
            end ;

         { Print row }
         ColLeft := PageLeft ;
         Printer.Canvas.Pen.Width := 1 ;
         for col := 0 to Table.ColCount-1 do begin
             printer.canvas.rectangle( ColLeft,Line,ColLeft+ColWidth[Col],
                                       Line+ColHeight ) ;
             printer.canvas.textout( ColLeft + CharWidth,
                                     Line + CharHeight div 10,
                                     Table.cells[Col,Row] ) ;
             ColLeft := ColLeft + ColWidth[Col] ;
             end ;

         { New page when line crosses bottom margin }
         Line := Line + ColHeight ;
         if Line > PageBottom then Printer.NewPage ;

         end ;

     Printer.EndDoc ;

     Screen.Cursor := crDefault ;

     end ;

procedure TEDRFile.CopyStringGrid(
          const Table : TStringGrid ;
          UseSelection : Boolean ) ;
{ ---------------------------------------------------
  Copy the contents of a string grid to the clipboard
  --------------------------------------------------- }
var
   Row,Row0,Row1,Col,Col0,Col1,BufSize : Integer ;
   CopyBuf : PChar ;
begin

     if  ((Table.Selection.Bottom - Table.Selection.Top) > 1) or
         ((Table.Selection.Right - Table.Selection.Left) > 1) then
         UseSelection := True ;

     if UseSelection then begin
        Row0 := Table.Selection.Top ;
        Row1 := Table.Selection.Bottom ;
        Col0 := Table.Selection.Left ;
        Col1 := Table.Selection.Right ;
        end
     else begin
        Row0 := 0 ;
        Row1 := Table.RowCount-1 ;
        Col0 := 0 ;
        Col1 := Table.ColCount-1 ;
        end ;

     // Determine size of and allocate string buffer
     BufSize := 1 ;
     for Row := Row0 to Row1 do
         for Col := Col0 to Col1 do
             BufSize := BufSize + Length(Table.Cells[Col,Row]) + 2 ;
     CopyBuf := StrAlloc( BufSize ) ;

     // Open clipboard preventing others acceessing it
     Clipboard.Open ;

     try

       // Copy table into buffer
       StrCopy(CopyBuf,PChar('')) ;
       for Row := Row0 to Row1 do begin
           for Col := Col0 to Col1 do begin
               if Col < Col1 then StrCat(CopyBuf,PChar(Table.Cells[Col,Row]+#9))
                             else StrCat(CopyBuf,PChar(Table.Cells[Col,Row]+#13#10)) ;
               end ;
           end ;

       // Copy string buffer to clipboard
       ClipBoard.SetTextBuf( CopyBuf ) ;

     finally

       // Release clipboard
       Clipboard.Close ;
       { Dispose of buffers }
       StrDispose( CopyBuf ) ;
       end ;

     end ;


procedure TEDRFile.PrintHeaderAndFooter ;
{ -----------------------------------------------------
  Printer standard header and footer for a printed page
  -----------------------------------------------------}
var
   KeepSize,xPix,yPix,LineHeight : Integer ;
begin

     { File name and title always in 12 point }
     KeepSize := Printer.Canvas.font.size ;

     Printer.Canvas.font.size := 12 ;
     LineHeight := (Printer.Canvas.TextHeight('X')*12) div 10 ;

     { Print file name }
     xPix := Printer.PageWidth div 10 ;
     yPix := Printer.PageHeight div 60 ;
     Printer.Canvas.TextOut(xPix,yPix, 'File ... ' + CdrFH.FileName ) ;

     { Print ident line }
     yPix := yPix + LineHeight ;
     Printer.Canvas.TextOut( xPix, yPix, CdrFH.IdentLine ) ;

     Printer.Canvas.font.size := KeepSize ;

     end ;


procedure TEDRFile.PrintPageTitle(
          Canvas : TCanvas ;
          EqnType : TEqnType ;
          const Results : TStringGrid ;
          var YEndOfText : Integer
          ) ;
{ -----------------------------------------------------
  Print experiment identification and other information
  -----------------------------------------------------}
var
   xPix,yPix,LineHeight,Row : Integer ;
   OldFontName : String ;
   OldFontSize : Integer ;

begin
     { Save the current font settings }
     OldFontName := Canvas.Font.Name ;
     OldFontSize := Canvas.Font.Height ;

     { Select standard font name and size for text information }
     Canvas.Font.Name := 'Arial' ;
     Canvas.Font.Size := 10 ;

     //CharWidth := Canvas.TextWidth('X') ;
     LineHeight := (Canvas.TextHeight('X')*12) div 10 ;

     { Start printing a top-left of page }
     xPix := Printer.PageWidth div 10 ;
     yPix := Printer.PageHeight div 60 ;

     { File Name }
     Canvas.TextOut(xPix,yPix, 'File ... ' + CdrfH.FileName ) ;
     { Ident line }
     yPix := yPix + LineHeight ;
     Canvas.TextOut( xPix, yPix, CdrfH.IdentLine ) ;

     { If a curve has been fitted, print the best fit parameters }
     if EqnType <> None then begin
        for Row := 0 to Results.RowCount-1 do begin
            Canvas.TextOut( xPix, yPix, Results.Cells[0,Row] ) ;
            yPix := yPix + LineHeight ;
            end ;
        end ;

     { Return the vertical position of the bottom of the area used for text }
     YEndOfText := yPix + LineHeight ;

     { Restore the old font settings }
     Canvas.Font.Name := OldFontName ;
     Canvas.Font.Height := OldFontSize ;
     end ;


function TEDRFile.PrinterPointsToPixels(
         PointSize : Integer
         ) : Integer ;
var
   PixelsPerInch : single ;
begin

     { Get height and width of page (in mm) and calculate
       the size of a pixel (in cm) }
     PixelsPerInch := GetDeviceCaps( printer.handle, LOGPIXELSX ) ;
     PrinterPointsToPixels := Trunc( (PointSize*PixelsPerInch) / 72. ) ;
     end ;


function TEDRFile.PrinterCmToPixels(
         const Axis : string;
         cm : single
         ) : Integer ;
{ -------------------------------------------
  Convert from cm (on printer page) to pixels
  -------------------------------------------}
var
   PixelWidth,PixelHeight : single ;
begin
     { Get height and width of page (in mm) and calculate
       the size of a pixel (in cm) }
     if UpperCase(Axis) = 'H' then begin
        { Printer pixel width (mm) }
        PixelWidth := GetDeviceCaps( printer.handle, HORZSIZE ) ;
        Result := Trunc( ( 10. * cm * printer.pagewidth) / PixelWidth );
        end
     else begin
        { Printer pixel height (mm) }
        PixelHeight := GetDeviceCaps( printer.handle, VERTSIZE ) ;
        Result := Trunc( ( printer.pageheight * 10. * cm )/ PixelHeight ) ;
        end ;
     end ;


function TEDRFile.ExtractListOfFloats ( const CBuf : string ;
                                var Values : Array of Single ;
                                PositiveOnly : Boolean ) : Integer ;
{ -------------------------------------------------------------
  Extract a series of floating point number from a string which
  may contain additional non-numeric text
  ---------------------------------------}

var
   CNum : string ;
   i,nValues : integer ;
   EndOfNumber : Boolean ;
begin
     nValues := 0 ;
     CNum := '' ;
     for i := 1 to length(CBuf) do begin

         { If character is numeric ... add it to number string }
         if PositiveOnly then begin
            { Minus sign is treated as a number separator }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end
         else begin
            { Positive or negative numbers }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.', '-' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end ;

         { If all characters are finished ... check number }
         if i = length(CBuf) then EndOfNumber := True ;

         if (EndOfNumber) and (Length(CNum) > 0)
            and (nValues <= High(Values)) then begin
              try
                 Values[nValues] := StrToFloat( CNum ) ;
                 CNum := '' ;
                 Inc(nValues) ;
              except
                    on E : EConvertError do CNum := '' ;
                    end ;
              end ;
         end ;
     { Return number of values extracted }
     Result := nValues ;
     end ;


procedure TEDRFile.FileCloseSafe( var FileHandle : Integer ) ;
// -------------------------
// Safe close file procedure
// -------------------------
begin
     // Close file (if it is open)
     if FileHandle >= 0 then FileClose( FileHandle ) ;
     // File handle = -1 indicates closed file
     FileHandle := -1 ;
     end ;


end.
