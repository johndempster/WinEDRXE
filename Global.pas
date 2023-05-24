unit Global;
{ ==============================================
  WinCDR - Global variables and type definitions
  (c) J. Dempster 1998
  ==============================================
  3.03.04 Channel limit increased to 11, defined by EDRChannelLimit
  06.08.04 Settings.ExternalTriggerActiveHigh added
  30.07.06 RecChannel added
  08.03.10 ... Settings.EventDetector.AvgFrequencyInterval added

  20.04.11 ... THistogram size increased to 4000
  14.08.14 ... ProgDirectory etc. moved to Main.
  09.01.16 ... IonName and Ion Units added to TFluorescenceSettings
  15.03.21 ... TEventDetector TauRise and TauDecay settings added
  22.06.22 ... TEventDetector.AmpSDScale added
  01/02/23 ... ContinuousRecording flag added to Settings
  }

interface

uses sysUtils, Graphics, Classes, stdctrls, ced1401, maths, wintypes ;


{ Global Variables }
var

//MinADCValue : Integer ;
//MaxADCValue : Integer ;
MinDACValue : Integer ;
MaxDACValue : Integer ;
//FH : TWCPFileHeader ;
//CdrFH : TCDRFileHeader ;
//HeaderArrayFull : Boolean ; // File header arrayb full flag
// File channel settings
//Channel : array[0..EDRChannelLimit] of TChannel ;
//WCPChannel : Array[0..EDRChannelLimit] of TChannel ;

//RecordTypes : TStringList ;
//ChannelNames : TStringList ;
//Settings : TSettings ;
//CED1902A : TCED1902 ;
//LogFileName : string ;
//MarkerList : TStringList ;       // Chart markers list

implementation


end.
