unit Fileio;
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
  }


interface

uses messages,shared,global,sysUtils,dialogs,math, windows, UITYpes  ;

procedure SaveCDRHeader( var fHDR : TCDRFileHeader ) ;
procedure GetCDRHeader( var fHDR : TCDRFileHeader ) ;
function ADCScaleToCalibFactor(
         ADCVoltageRange : Single ;
         var Channel : TChannel )  : single ;
function CalibFactorToADCScale(
         ADCVoltageRange : Single ;
         var Channel : TChannel )  : single ;

function FileOverwriteCheck( var FileName : string ) : boolean ;


function ReadCDRBuffer(
         var FHdr : TCDRFileHeader ;
         BlockPointer : Integer ;
         var Buf : Array of SmallInt ;
         NumBlocksToRead : Integer ) : Integer ;

function WriteCDRBuffer(
         var FHdr : TCDRFileHeader ;
         BlockPointer : Integer ;
         var Buf : Array of SmallInt ;
         NumBlocksToWrite : Integer
         ) : Integer ;

procedure LoadInitializationFile( const IniFileName : string ) ;
procedure SaveInitializationFile( const IniFileName : string ) ;

procedure OpenLogFile ;
procedure WriteToLogFile( Line : string ) ;
procedure CloseLogFile ;
procedure MakeBackupFile ;
procedure RestoreFromBackupFile ;



implementation

uses mdiform, maths, ced1401, AmpModule ;

const
     HeaderSize = 2048 ;
     IniFileHeaderSize = 10000 ;

var
   LogFile : TextFile ;
   LogFileAvailable : boolean ;


function FileOverwriteCheck(
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
     if FileExists(FileName) then begin
          { If it exists, let user change it's name }
          Main.SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
          Main.SaveDialog.DefaultExt := DataFileExtension ;
          Main.SaveDialog.FileName := ExtractFileName( FileName ) ;
          Main.SaveDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                    [DataFileExtension,DataFileExtension]) ;
          Main.SaveDialog.Title := ExtractFileName(FileName)
                                   + ' already exists! Change Name? ';

          if Main.SaveDialog.execute then begin
             { Save data directory }
             Main.DataDirectory := ExtractFilePath( Main.SaveDialog.FileName ) ;
             { Use new file name entered by user }
             FileName := Main.SaveDialog.FileName ;
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


procedure SaveCDRHeader( var fHDR : TCDRFileHeader ) ;
{ ---------------------------------------
  Save file header data to CDR data file
  ---------------------------------------}
var
   Header : array[1..HeaderSize] of ANSIchar ;
   i : Integer ;
   ch : Integer ;

begin

     HeaderArrayFull := False ;
     { Initialise empty header buffer with zero bytes }
     for i := 1 to sizeof(Header) do Header[i] := chr(0) ;

     AppendFloat( Header, 'VER=',fHDR.Version );

     AppendString( Header, 'CTIME=', fHDR.CreationTime ) ;

     // 13/2/02 Added to distinguish between 12 and 16 bit data files
     // 30/7/6 Now uses ADCMaxValue in Channel settings
     CDRFH.ADCMaxValue := Channel[0].ADCMaxValue ;
     AppendInt( Header, 'ADCMAX=', CDRFH.ADCMaxValue ) ;

     { Number of bytes in file header }
     AppendInt( Header, 'NBH=', HeaderSize ) ;

     fHDR.NumChannels := Max(fHDR.NumChannels,1) ;
     AppendInt( Header, 'NC=', fHDR.NumChannels ) ;

     // A/D converter input voltage range
     AppendFloat( Header, 'AD=', fHDR.ADCVoltageRange ) ;

     { Determine number of samples in file from size of file
       - space taken up by header }
   {  fHDR.NumSamplesInFile := (FileSeek(fHDR.FileHandle,0,2)
                               - fHDR.NumBytesInHeader) div 2 ;}
     fHDR.NumSamplesPerBlock := fHDR.NumChannels*NumSamplesPerSector ;
     fHDR.NumBytesPerBlock := fHDR.NumSamplesPerBlock*2 ;
     fHDR.NumBlocksInFile := fHDR.NumSamplesInFile div fHDR.NumSamplesPerBlock ;

     AppendInt( Header, 'NP=', fHDR.NumSamplesInFile ) ;

     AppendFloat( Header, 'DT=',fHDR.dt );

     { Time duration of recorded data }
     fHDR.RecordDuration := (fHDR.NumSamplesInFile*fHDR.dt) /
                                 fHDR.NumChannels ;

     { Event detector parameters }
     AppendInt( Header,   'DETCH=', Settings.EventDetector.Channel ) ;
     AppendInt( Header,   'DETRS=', Settings.EventDetector.RecordSize ) ;
     AppendFloat( Header, 'DETYT=', Settings.EventDetector.yThreshold ) ;
     AppendFloat( Header, 'DETTT=', Settings.EventDetector.tThreshold ) ;
     AppendFloat( Header, 'DETDD=', Settings.EventDetector.DeadTime ) ;
     AppendFloat( Header, 'DETBAI=', Settings.EventDetector.BaselineAveragIngInterval ) ;
     AppendFloat( Header, 'DETPF=', Settings.EventDetector.PreTriggerFraction ) ;
     AppendFloat( Header, 'DETAW=', Settings.EventDetector.AnalysisWindow ) ;
     AppendLogical( Header, 'DETPOSPK=', Settings.EventDetector.PositivePeaks ) ;
     AppendInt( Header, 'DETBASE=', Settings.EventDetector.Baseline ) ;
     AppendInt( Header, 'DETALIGN=', Settings.EventDetector.Alignment ) ;
     AppendLogical( Header, 'DETBASSUB=', Settings.EventDetector.SubtractBaseline ) ;
     AppendInt( Header, 'DETBASPTS=', Settings.EventDetector.NumBaselinePoints ) ;
     AppendInt( Header, 'DETBASGAP=', Settings.EventDetector.NumBaselineGap ) ;
     AppendFloat( Header, 'DETTDECP=', Settings.EventDetector.TDecayPercent ) ;
     AppendInt( Header, 'DETDECFR=', Settings.EventDetector.TDecayFrom ) ;
     AppendInt( Header, 'DETREW=', Settings.EventDetector.RisingEdgeWindow ) ;
     AppendLogical( Header, 'DETEBT=', Settings.EventDetector.EnableBaselineTracking ) ;
     AppendFloat( Header, 'DETAMPSDSCALE=', Settings.EventDetector.AmpSDScale ) ;

     AppendInt( Header, 'VARRS=', Settings.Variance.RecordSize ) ;
     AppendInt( Header, 'VAROV=', Settings.Variance.RecordOverlap ) ;
     AppendFloat( Header, 'VARTR=', Settings.Variance.TauRise ) ;
     AppendFloat( Header, 'VARTD=', Settings.Variance.TauDecay ) ;

     AppendFloat( Header, 'UNITC=', Settings.DwellTimes.UnitCurrent ) ;
     AppendFloat( Header, 'DWTTH=', Settings.DwellTimes.Threshold ) ;
     AppendInt( Header, 'DWTSARLO=', Settings.DwellTimes.SampleRangeLo ) ;
     AppendInt( Header, 'DWTSARHI=', Settings.DwellTimes.SampleRangeHi ) ;
     AppendInt( Header, 'DWTSABLK=', Settings.DwellTimes.SampleBlockSize ) ;
     AppendInt( Header, 'DWTEVRLO=', Settings.DwellTimes.EventRangeLo ) ;
     AppendInt( Header, 'DWTEVRHI=', Settings.DwellTimes.EventRangeHi ) ;
     AppendInt( Header, 'DWTEVBLK=', Settings.DwellTimes.EventBlockSize ) ;
     AppendInt( Header, 'DWTNCPP=', Settings.DwellTimes.NumChannelsPerPatch ) ;

     for ch := 0 to fHDR.NumChannels-1 do begin
         AppendInt( Header, format('YO%d=',[ch]), Channel[ch].ChannelOffset) ;
         AppendString( Header, format('YU%d=',[ch]), Channel[ch].ADCUnits ) ;
         AppendString( Header, format('YN%d=',[ch]), Channel[ch].ADCName ) ;
         //Channel[ch].ADCCalibrationFactor := ADCScaleToCalibFactor( FHDR, ch ) ;
         AppendFloat(Header,format('YCF%d=',[ch]),Channel[ch].ADCCalibrationFactor) ;
         AppendFloat( Header, format('YAG%d=',[ch]), Channel[ch].ADCAmplifierGain) ;
         AppendInt( Header, format('YZ%d=',[ch]), Channel[ch].ADCZero) ;
         AppendInt( Header, format('YR%d=',[ch]), Channel[ch].ADCZeroAt) ;
         end ;

     { Experiment identification line }
     AppendString( Header, 'ID=', fHDR.IdentLine ) ;

     { Save the name of any associated WCP data file }
     AppendString( Header, 'WCPFNAM=',  fHDR.WCPFileName ) ;

     { Save the original file backed up flag }
     AppendLogical( Header, 'BAK=', fHDR.BackedUp ) ;

     // Save markers to header
     AppendInt( Header, 'MKN=', MarkerList.Count ) ;
     for i := 0 to MarkerList.Count-1 do begin
         AppendFloat( Header, format('MKTIM%d=',[i]), Single(MarkerList.Objects[i])) ;
         AppendString( Header, format('MKTXT%d=',[i]), MarkerList.Strings[i] ) ;
         end ;

     FileSeek( fHDR.FileHandle, 0, 0 ) ;
     if FileWrite(fHDR.FileHandle,Header,Sizeof(Header)) <> Sizeof(Header) then
        ShowMessage( ' File Header Write - Failed ' ) ;

     { Add Names of channels to list }
     ChannelNames.Clear ;
     for ch := 0 to fHDR.NumChannels-1 do ChannelNames.Add( format(
                                          'Ch.%d %s',[ch,Channel[ch].ADCName])) ;

     if HeaderArrayFull then
        ShowMessage( fHDR.FileName + ' file header parameter array full!' ) ;

     end ;


function ADCScaleToCalibFactor( ADCVoltageRange : Single ;
                                var Channel : TChannel )  : single ;
begin
     if Channel.ADCAmplifierGain = 0.0 then Channel.ADCAmplifierGain := 1.0 ;
     if Channel.ADCScale = 0.0 then Channel.ADCScale := 0.001 ;
     Result := ADCVoltageRange /
              (Channel.ADCScale*Channel.ADCAmplifierGain *(Channel.ADCMaxValue+1) ) ;
     end ;


function CalibFactorToADCScale( ADCVoltageRange : Single ;
                                var Channel : TChannel ) : single ;
begin
     if Channel.ADCAmplifierGain = 0.0 then Channel.ADCAmplifierGain := 1.0 ;
     if Channel.ADCCalibrationFactor = 0.0 then Channel.ADCCalibrationFactor := 0.001 ;
     Result := ADCVoltageRange /
              (Channel.ADCCalibrationFactor*Channel.ADCAmplifierGain *(Channel.ADCMaxValue+1) ) ;
     end ;


procedure GetCDRHeader( var fHDR : TCDRFileHeader ) ;
{ ------------------------------------------------------
  Read file header block from data file,
  decode parameter list, and put into FileHeader record
  ------------------------------------------------------}
var
   Header : array[1..HeaderSize] of ANSIchar ;
   i,ch,OldValue : Integer ;
   NumMarkers,NumSamplesInFile : Integer ;
   MarkerTime : Single ;
   MarkerText : String ;
   SaveHeader : Boolean ;
begin

     fHDR.FilePointer := FileSeek( fHDR.FileHandle, 0, 0 ) ;
     if FileRead( fHDR.FileHandle, Header, Sizeof(Header) )
        = Sizeof(Header) then begin

          SaveHeader := False ;

          { Get default size of file header }
          fHDR.NumBytesInHeader := HeaderSize ;
          { Get size of file header for this file }
          ReadInt( Header, 'NBH=', fHDR.NumBytesInHeader ) ;
          if fHDR.NumBytesInHeader <> HeaderSize then begin
             ShowMessage( 'File header size mismatch' ) ;
             end ;

          ReadFloat( Header, 'VER=',fHDR.Version );

          ReadString( Header, 'CTIME=', fHDR.CreationTime ) ;

          // 14/2/02 Added to distinguish between 12 and 16 bit data files
          OldValue := Channel[0].ADCMaxValue ;
          ReadInt( Header, 'ADCMAX=', Channel[0].ADCMaxValue ) ;
          if Channel[0].ADCMaxValue = 0 then Channel[0].ADCMaxValue := 2047 ;
          for ch := 0 to ChannelLimit do Channel[ch].ADCMaxValue := Channel[0].ADCMaxValue ;
          CDRFH.ADCMaxValue := Channel[0].ADCMaxValue ;

          if Channel[0].ADCMaxValue <> OldValue then begin
             for ch := 0 to ChannelLimit do begin
                Channel[ch].yMin := -Channel[0].ADCMaxValue -1;
                Channel[ch].yMax := Channel[0].ADCMaxValue ;
                end ;
             end ;

          ReadInt( Header, 'NC=', fHDR.NumChannels ) ;

          ReadInt( Header, 'NP=', fHDR.NumSamplesInFile ) ;

          NumSamplesInFile := (FileSeek( fHDR.FileHandle, 0, 2 ) + 1 - fHDR.NumBytesInHeader) div 2 ;

          if fHDR.NumSamplesInFile <> NumSamplesInFile then begin
             if Dialogs.MessageDlg( format(
                'No. samples (%d) listed file header does not match actual number in file (%d)! Correct file header?',
                [fHdr.NumSamplesInFile,NumSamplesInFile]),
                mtConfirmation,[mbYes,mbNo], 0 ) = mrYes then fHdr.NumSamplesInFile := NumSamplesInFile ;
                SaveHeader := True ;
             end ;

          fHDR.NumSamplesPerBlock := fHDR.NumChannels*NumSamplesPerSector ;
          fHDR.NumBytesPerBlock := fHDR.NumSamplesPerBlock*2 ;
          fHDR.NumBlocksInFile := fHDR.NumSamplesInFile div fHDR.NumSamplesPerBlock ;

          ReadFloat( Header, 'AD=',fHDR.ADCVoltageRange);

          ReadFloat( Header, 'DT=', fHDR.dt );
          if fHDR.dt = 0.0 then begin
             fHDR.dt := 1.0 ;
             ShowMessage( 'Sampling Interval = 0.0 s. Changed to 1.0! (GetCDRHeader) ' );
             end ;

          { Time duration of recorded data }
          fHDR.RecordDuration := (fHDR.NumSamplesInFile*fHDR.dt) / fHDR.NumChannels ;

          { Event detector parameters }
          ReadInt( Header, 'DETCH=', Settings.EventDetector.Channel ) ;
          ReadInt( Header, 'DETRS=', Settings.EventDetector.RecordSize ) ;
          ReadFloat( Header, 'DETYT=', Settings.EventDetector.yThreshold ) ;
          ReadFloat( Header, 'DETTT=', Settings.EventDetector.tThreshold ) ;
          ReadFloat( Header, 'DETDD=', Settings.EventDetector.DeadTime ) ;
          ReadFloat( Header, 'DETBAI=', Settings.EventDetector.BaselineAveragingInterval ) ;
          ReadFloat( Header, 'DETPF=', Settings.EventDetector.PreTriggerFraction ) ;
          ReadFloat( Header, 'DETAW=', Settings.EventDetector.AnalysisWindow ) ;
          ReadLogical( Header, 'DETPOSPK=', Settings.EventDetector.PositivePeaks ) ;
          ReadInt( Header, 'DETBASE=', Settings.EventDetector.Baseline ) ;
          ReadInt( Header, 'DETALIGN=', Settings.EventDetector.Alignment ) ;
          ReadLogical( Header, 'DETBASSUB=', Settings.EventDetector.SubtractBaseline ) ;
          ReadInt( Header, 'DETBASPTS=', Settings.EventDetector.NumBaselinePoints ) ;
          ReadInt( Header, 'DETBASGAP=', Settings.EventDetector.NumBaselineGap ) ;
          ReadFloat( Header, 'DETTDECP=', Settings.EventDetector.TDecayPercent ) ;
          ReadInt( Header, 'DETDECFR=', Settings.EventDetector.TDecayFrom ) ;
          ReadInt( Header, 'DETREW=', Settings.EventDetector.RisingEdgeWindow ) ;
          ReadLogical( Header, 'DETEBT=', Settings.EventDetector.EnableBaselineTracking ) ;
          ReadFloat( Header, 'DETAMPSDSCALE=', Settings.EventDetector.AmpSDScale ) ;

          ReadInt( Header, 'VARRS=', Settings.Variance.RecordSize ) ;
          ReadInt( Header, 'VAROV=', Settings.Variance.RecordOverlap ) ;
          ReadFloat( Header, 'VARTR=', Settings.Variance.TauRise ) ;
          ReadFloat( Header, 'VARTD=', Settings.Variance.TauDecay ) ;

          ReadFloat( Header, 'UNITC=', Settings.DwellTimes.UnitCurrent ) ;
          ReadFloat( Header, 'DWTTH=', Settings.DwellTimes.Threshold ) ;
          ReadInt( Header, 'DWTSARLO=', Settings.DwellTimes.SampleRangeLo ) ;
          ReadInt( Header, 'DWTSARHI=', Settings.DwellTimes.SampleRangeHi ) ;
          ReadInt( Header, 'DWTSABLK=', Settings.DwellTimes.SampleBlockSize ) ;
          ReadInt( Header, 'DWTEVRLO=', Settings.DwellTimes.EventRangeLo ) ;
          ReadInt( Header, 'DWTEVRHI=', Settings.DwellTimes.EventRangeHi ) ;
          ReadInt( Header, 'DWTEVBLK=', Settings.DwellTimes.EventBlockSize ) ;
          ReadInt( Header, 'DWTNCPP=', Settings.DwellTimes.NumChannelsPerPatch ) ;

          for ch := 0 to fHDR.NumChannels-1 do begin

              ReadInt( Header, format('YO%d=',[ch]), Channel[ch].ChannelOffset) ;

              Channel[ch].ADCUnits := '??' ;
              ReadString( Header, format('YU%d=',[ch]) , Channel[ch].ADCUnits ) ;
              { Fix to avoid strings with #0 in them }
              if Channel[ch].ADCUnits[1] = chr(0) then Channel[ch].ADCUnits := '??' ;
              Channel[ch].ADCName := 'Ch' + IntToStr(ch) ;
              ReadString( Header, format('YN%d=',[ch]), Channel[ch].ADCName ) ;
              { Fix to avoid strings with #0 in them }
              if Channel[ch].ADCName[1] = chr(0) then Channel[ch].ADCName := '??' ;
              ReadFloat( Header, format('YCF%d=',[ch]), Channel[ch].ADCCalibrationFactor) ;

              ReadFloat( Header, format('YAG%d=',[ch]), Channel[ch].ADCAmplifierGain) ;
              Channel[ch].ADCScale := CalibFactorToADCScale(
                                      FHDR.ADCVoltageRange,
                                      Channel[ch] ) ;

              ReadInt( Header, format('YZ%d=',[ch]), Channel[ch].ADCZero) ;
              ReadInt( Header, format('YR%d=',[ch]), Channel[ch].ADCZeroAt) ;
              end ;

          { Experiment identification line }
          ReadString( Header, 'ID=', fHDR.IdentLine ) ;

          { Read Markers }
          NumMarkers := 0 ;
          ReadInt( Header, 'MKN=', NumMarkers ) ;
          MarkerList.Clear ;
          for i := 0 to NumMarkers-1 do begin
              ReadFloat( Header, format('MKTIM%d=',[i]), MarkerTime ) ;
              ReadString( Header, format('MKTXT%d=',[i]), MarkerText ) ;
              MarkerList.AddObject( MarkerText, TObject(MarkerTime) ) ;
              end ;

           { Add names of channels to list }
          ChannelNames.Clear ;
          for ch := 0 to fHDR.NumChannels-1 do
              ChannelNames.Add( format('Ch.%d %s',[ch,Channel[ch].ADCName] )) ;

          { Name of any associated WCP data file }
          fHDR.WCPFileName := '' ;
          ReadString( Header, 'WCPFNAM=', fHDR.WCPFileName ) ;

          { Save the original file backed up flag }
          ReadLogical( Header, 'BAK=', fHDR.BackedUp ) ;

          // Save file header to file if changes have been made
          if SaveHeader then SaveCDRHeader( fHDR ) ;

          end
     else ShowMessage( ' File Header Read - Failed ' ) ;

     end ;


function ReadCDRBuffer(
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


function WriteCDRBuffer(
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


procedure LoadInitializationFile( const IniFileName : string ) ;
{ ---------------------------------------------------------
  Read Initialization file to get initial program settings,
  e.g. the name of the last data file used
  ---------------------------------------------------------}
var
   Header : array[1..IniFileHeaderSize] of ANSIchar ;
   IniFileHandle : Integer ;
   i : Integer ;
begin

     if not FileExists( IniFileName ) then Exit ;

     IniFileHandle := FileOpen( IniFileName, fmOpenReadWrite ) ;

     // Read file
     FileRead(IniFileHandle,Header,Sizeof(Header)) ;

     // Record duration
     ReadFloat( Header, 'RECDUR=', Settings.RecordDuration ) ;

     // Continuous Recording flag (overrides Settings.RecordDuration setting)
     ReadLogical( Header, 'RECCONT=', Settings.ContinuousRecording ) ;

     { Get default no. channels }
     ReadInt( Header, 'NC=', Settings.NumChannels ) ;
     Settings.NumChannels := Max( 1,Settings.NumChannels ) ;

     ReadLogical( Header, 'STARTSTIMREC=', Settings.StartStimulusOnRecord ) ;

     { CED 1902 amplifier settings }
     ReadInt( Header, 'CEDI=', Amplifier.CED1902.Input ) ;
     ReadInt( Header, 'CEDG=', Amplifier.CED1902.Gain ) ;
     ReadFloat( Header, 'CEDGV=', Amplifier.CED1902.GainValue ) ;
     ReadInt( Header, 'CEDLP=', Amplifier.CED1902.LPFilter ) ;
     ReadInt( Header, 'CEDHP=', Amplifier.CED1902.HPFilter ) ;
     ReadInt( Header, 'CEDAC=', Amplifier.CED1902.ACCoupled ) ;
     ReadInt( Header, 'CEDDCO=', Amplifier.CED1902.DCOffset ) ;
     ReadInt( Header, 'CEDNF=', Amplifier.CED1902.NotchFilter ) ;
     ReadInt( Header, 'CEDPO=', Amplifier.CED1902.ComPort ) ;

     ReadInt( Header, 'VARRS=', Settings.Variance.RecordSize ) ;
     ReadInt( Header, 'VAROV=', Settings.Variance.RecordOverlap ) ;
     ReadFloat( Header, 'VARTR=', Settings.Variance.TauRise ) ;
     ReadFloat( Header, 'VARTD=', Settings.Variance.TauDecay ) ;

     ReadFloat( Header, 'UNITC=', Settings.DwellTimes.UnitCurrent ) ;
     ReadFloat( Header, 'DWTTH=', Settings.DwellTimes.Threshold ) ;

           { Read global settings }

           { *** Recording settings *** }
     { Recording trigger mode }
     ReadString( Header, 'TRG=', Settings.TriggerMode ) ;
     ReadLogical( Header, 'EXTTRIGAH=', Settings.ExternalTriggerActiveHigh ) ;

     { Event detector parameters }
     ReadInt( Header, 'DETCH=', Settings.EventDetector.Channel ) ;
     ReadInt( Header, 'DETDM=', Settings.EventDetector.DetectionMode ) ;
     ReadInt( Header, 'DETRS=', Settings.EventDetector.RecordSize ) ;
     ReadFloat( Header, 'DETYT=', Settings.EventDetector.yThreshold ) ;
     ReadFloat( Header, 'DETTT=', Settings.EventDetector.tThreshold ) ;
     ReadFloat( Header, 'DETDD=', Settings.EventDetector.DeadTime ) ;
     ReadFloat( Header, 'DETBAI=', Settings.EventDetector.BaselineAveragingInterval ) ;
     ReadFloat( Header, 'DETTAUR=', Settings.EventDetector.TauRise ) ;
     ReadFloat( Header, 'DETTAUD=', Settings.EventDetector.TauDecay ) ;
     ReadFloat( Header, 'DETPF=', Settings.EventDetector.PreTriggerFraction ) ;
     ReadFloat( Header, 'DETAW=', Settings.EventDetector.AnalysisWindow ) ;
     ReadInt( Header, 'DETREW=', Settings.EventDetector.RisingEdgeWindow ) ;
     ReadFloat( Header, 'DETAVFI=', Settings.EventDetector.AvgFrequencyInterval ) ;
     ReadLogical( Header, 'DETEBT=', Settings.EventDetector.EnableBaselineTracking ) ;
     ReadFloat( Header, 'DETAMPSDSCALE=', Settings.EventDetector.AmpSDScale ) ;

     { Number of records required (in free run/ext. trigger modes}
     ReadInt( Header, 'NRQ=', Settings.NumTriggerSweeps ) ;

     { Default digital control port output byte setting }
     Settings.UpdateOutputs := True ;

     ReadFloat( Header, 'DTMINDAC=', Settings.MinDACInterval ) ;

     { Load time units (ms or s) }
     ReadString( Header, 'TUNITS=', Settings.TUnits ) ;
     if Settings.TUnits = 's' then begin
        Settings.TScale := 1. ;
        Settings.TUnScale := 1. ;
        end
     else begin
        Settings.TUnits := 'ms' ;
        Settings.TScale := SecsToms ;
        Settings.TUnScale := MsToSecs ;
        end ;

      { Seal test pulse settings }
      ReadFloat( Header, 'STPH=', Settings.SealTest.PulseHeight ) ;
      ReadFloat( Header, 'STPH1=', Settings.SealTest.PulseHeight1 ) ;
      ReadFloat( Header, 'STPH2=', Settings.SealTest.PulseHeight2 ) ;
      ReadFloat( Header, 'STPH3=', Settings.SealTest.PulseHeight3 ) ;
      ReadFloat( Header, 'STHV1=', Settings.SealTest.HoldingVoltage1 ) ;
      ReadFloat( Header, 'STHV2=', Settings.SealTest.HoldingVoltage2 ) ;
      ReadFloat( Header, 'STHV3=', Settings.SealTest.HoldingVoltage3 ) ;
      ReadFloat( Header, 'STPW=', Settings.SealTest.PulseWidth ) ;
      ReadInt( Header, 'STCCH=', Settings.SealTest.CurrentChannel ) ;
      ReadInt( Header, 'STVCH=', Settings.SealTest.VoltageChannel ) ;
      ReadInt( Header, 'STUSE=', Settings.SealTest.Use ) ;
      ReadInt( Header, 'STDSC=', Settings.SealTest.DisplayScale ) ;
      ReadLogical( Header, 'STASC=', Settings.SealTest.AutoScale ) ;
      ReadLogical( Header, 'STFRU=', Settings.SealTest.FreeRun ) ;
      ReadInt( Header, 'STNAV=', Settings.SealTest.NumAverages ) ;
      ReadFloat( Header, 'STZAPA=', Settings.SealTest.ZapAmplitude ) ;
      ReadFloat( Header, 'STZAPD=', Settings.SealTest.ZapDuration ) ;
      ReadLogical( Header, 'STGAP=', Settings.SealTest.GaFromPeak ) ;

     ReadFloat( Header, 'DT=', Settings.ADCSamplingInterval );

     { Width/height of clipboard bitmaps }
     ReadInt( Header, 'BMAPW=', Settings.BitmapWidth ) ;
     ReadInt( Header, 'BMAPH=', Settings.BitmapHeight ) ;

     ReadLogical( Header, 'DISPGRID=', Settings.DisplayGrid ) ;
     ReadFloat( Header, 'DISPDUR=', Settings.DisplayDuration ) ;
     ReadLogical( Header, 'FIXZERO=', Settings.FixedZeroLevels) ;
     
     { Plotting page settings }
     ReadFloat( Header, 'PLTPM=',Settings.Plot.TopMargin ) ;
     ReadFloat( Header, 'PLBTM=',Settings.Plot.BottomMargin ) ;
     ReadFloat( Header, 'PLLFM=',Settings.Plot.LeftMargin ) ;
     ReadFloat( Header, 'PLRTM=',Settings.Plot.RightMargin ) ;
     ReadString( Header, 'PLFNT=',Settings.Plot.FontName ) ;
     ReadInt( Header, 'PLFSI=',Settings.Plot.FontSize ) ;
     ReadInt( Header, 'PLLTH=',Settings.Plot.LineThickness ) ;
     ReadLogical( Header, 'PLSHL=',Settings.Plot.ShowLines ) ;
     ReadInt( Header, 'PLMKS=',Settings.Plot.MarkerSize ) ;
     ReadLogical( Header, 'PLSHM=',Settings.Plot.ShowMarkers ) ;
     ReadLogical( Header, 'PLCOL=',Settings.Plot.UseColor ) ;

     // Page display module (PageView.pas) settings
     ReadInt( Header, 'PVLPP=',Settings.PageViewLinesPerPage ) ;
     ReadFloat( Header, 'PVLD=',Settings.PageViewLineDuration ) ;

     ReadString( Header, 'DDIR=', Main.DataDirectory ) ;

     { Laboratory interface }
     ReadInt( Header, 'LABINT=',Settings.LaboratoryInterface ) ;

     // Lab. interface device #
     ReadInt( Header, 'LABDEV=',Settings.DeviceNumber ) ;

     // Lab. Interface A/D input mode
     Settings.ADCInputMode := 0 ;
     ReadInt( Header, 'LABADIP=',Settings.ADCInputMode ) ;

     { Recently used data files }
     for i := 0 to High(Settings.RecentFiles) do Settings.RecentFiles[i] := '' ;
     ReadString( Header, 'RF0=', Settings.RecentFiles[0] ) ;
     ReadString( Header, 'RF1=', Settings.RecentFiles[1] ) ;
     ReadString( Header, 'RF2=', Settings.RecentFiles[2] ) ;
     ReadString( Header, 'RF3=', Settings.RecentFiles[3] ) ;

     // Capacity settings
     ReadFloat( Header, 'CAPCMMX=', Settings.Capacity.CmDisplayMax ) ;
     ReadFloat( Header, 'CAPGSMX=', Settings.Capacity.GsDisplayMax ) ;
     ReadFloat( Header, 'CAPGMMX=', Settings.Capacity.GmDisplayMax ) ;
     ReadFloat( Header, 'CAPFREQ=', Settings.Capacity.Frequency ) ;
     ReadFloat( Header, 'CAPVREV=', Settings.Capacity.VRev ) ;
     ReadInt( Header, 'CAPGR=', Settings.Capacity.GrealChan) ;
     ReadInt( Header, 'CAPGI=', Settings.Capacity.GimagChan) ;
     ReadInt( Header, 'CAPIM=', Settings.Capacity.ImChan) ;
     ReadInt( Header, 'CAPVM=', Settings.Capacity.VmChan) ;
     ReadInt( Header, 'CAPGM=', Settings.Capacity.GmChan) ;
     ReadInt( Header, 'CAPGS=', Settings.Capacity.GsChan) ;
     ReadInt( Header, 'CAPCM=', Settings.Capacity.CmChan) ;
     ReadLogical( Header, 'CAPINVGR=', Settings.Capacity.InvertGReal) ;
     ReadLogical( Header, 'CAPINVGI=', Settings.Capacity.InvertGIMag) ;
     ReadFloat( Header, 'CAPRSCOMP=', Settings.Capacity.RSeriesComp ) ;
     REadFloat( Header, 'CAPCCOMP=', Settings.Capacity.CellCapacityComp ) ;
     ReadLogical( Header, 'CAPCOMPINUSE=', Settings.Capacity.CapacityCompensationInUse) ;
     ReadLogical( Header, 'CAPINUSE=', Settings.Capacity.InUse) ;

     // Fluorescence settings
     ReadLogical( Header, 'FLRINUSE=', Settings.Fluorescence.InUse) ;
     ReadFloat( Header, 'FLRMAX=', Settings.Fluorescence.RMax ) ;
     ReadFloat( Header, 'FLRMIN=', Settings.Fluorescence.RMin ) ;
     ReadFloat( Header, 'FLKEFF=', Settings.Fluorescence.KEff ) ;
     ReadFloat( Header, 'FLTHRE=', Settings.Fluorescence.FThreshold ) ;
     ReadInt( Header, 'FLNUM=', Settings.Fluorescence.NumerChan ) ;
     ReadInt( Header, 'FLDEN=', Settings.Fluorescence.DenomChan ) ;
     ReadInt( Header, 'FLRAT=', Settings.Fluorescence.RatioChan ) ;
     ReadInt( Header, 'FLCON=', Settings.Fluorescence.ConcChan ) ;
     ReadFloat( Header, 'FLRDMX=', Settings.Fluorescence.RatioDisplayMax ) ;
     ReadFloat( Header, 'FLCDMX=', Settings.Fluorescence.ConcDisplayMax ) ;
     Settings.Fluorescence.IonName := 'Ca' ;
     ReadString( Header, 'FLIONNAME=', Settings.Fluorescence.IonName ) ;
     Settings.Fluorescence.IonUnits := 'uM' ;
     ReadString( Header, 'FLIONUNITS=', Settings.Fluorescence.IonUnits ) ;

     // Real time event frequency settings
     ReadLogical( Header, 'EFINUSE=', Settings.RTEventAnalysis.InUse) ;
     ReadInt( Header, 'EFCH=', Settings.RTEventAnalysis.Channel ) ;
     ReadFloat( Header, 'EFTHRESH=', Settings.RTEventAnalysis.DetectionThreshold ) ;
     ReadFloat( Header, 'EFRMEAN=', Settings.RTEventAnalysis.RunningMeanTime ) ;
     ReadFloat( Header, 'EFDEADT=', Settings.RTEventAnalysis.DeadTime ) ;
     ReadFloat( Header, 'EFCOUNTI=', Settings.RTEventAnalysis.CountingInterval ) ;

     // Real time resistance settings
     ReadLogical( Header, 'RESINUSE=', Settings.RTResistance.InUse) ;
     ReadInt( Header, 'RESICH=', Settings.RTResistance.ImChannel ) ;
     ReadInt( Header, 'RESVCH=', Settings.RTResistance.VmChannel ) ;
     ReadFloat( Header, 'RESAMPLITUDE=', Settings.RTResistance.Amplitude ) ;
     ReadFloat( Header, 'RESDUR=', Settings.RTResistance.Duration ) ;
     ReadFloat( Header, 'RESINT=', Settings.RTResistance.Interval ) ;
     ReadInt( Header, 'RESPLT=', Settings.RTResistance.Plot ) ;

     // Currently selected stimulus file
     ReadString( Header, 'STIMFILE=', Settings.VProgramFileName ) ;

     // Load post-synaptic current simulation settings
     ReadFloat( Header, 'EPCDUR=', Settings.SimEPC.Duration) ;
     ReadFloat( Header, 'EPCAMP=', Settings.SimEPC.Amplitude) ;
     ReadFloat( Header, 'EPCAMPSD=', Settings.SimEPC.AmplitudeSD) ;
     ReadFloat( Header, 'EPCTRISE=', Settings.SimEPC.TauRise) ;
     ReadFloat( Header, 'EPCTDECAY=', Settings.SimEPC.TauDecay) ;
     ReadFloat( Header, 'EPCNOISESD=', Settings.SimEPC.NoiseSD) ;
     ReadFloat( Header, 'EPCFREQ=', Settings.SimEPC.Frequency) ;
     ReadFloat( Header, 'EPCFREQSD=', Settings.SimEPC.FrequencySD) ;
     ReadFloat( Header, 'EPCDEL=', Settings.SimEPC.Delay ) ;
     ReadFloat( Header, 'EPCSINEAMP=', Settings.SimEPC.SineAmplitude) ;
     ReadFloat( Header, 'EPCSINEFREQ=', Settings.SimEPC.SineFrequency) ;
     ReadLogical( Header, 'EPCRAND=', Settings.SimEPC.RandomEvents) ;
     ReadInt( Header, 'EPCUNITS=', Settings.SimEPC.UnitsIndex) ;
     ReadFloat( Header, 'EPCPROB=', Settings.SimEPC.ReleaseProbability) ;
     ReadFloat( Header, 'EPCPOOL=', Settings.SimEPC.ReleasablePool) ;
     ReadFloat( Header, 'EPCDEP=', Settings.SimEPC.Depression) ;
     ReadFloat( Header, 'EPCTAUDEP=', Settings.SimEPC.TauDepression) ;

     // Load main form size and position
     i := Main.Width ;
     ReadInt( Header, 'FORMWIDTH=', i ) ;
     Main.Width := i ;

     i := Main.Height ;
     ReadInt( Header, 'FORMHEIGHT=', i ) ;
     Main.Height := i ;

     i := Main.Top ;
     ReadInt( Header, 'FORMTOP=',i ) ;
     Main.Top := i ;

     i := Main.Left ;
     ReadInt( Header, 'FORMLEFT=', i ) ;
     Main.Left := i ;

     FileClose( IniFileHandle ) ;

     end ;


procedure SaveInitializationFile( const IniFileName : string ) ;
{ --------------------------------------------
  Save program settings to Initialization file
  --------------------------------------------}
var
   Header : array[1..IniFileHeaderSize] of ANSIchar ;
   i,ch : Integer ;
   IniFileHandle : Integer ;
begin
     IniFileHandle := FileCreate( IniFileName ) ;

     HeaderArrayFull := False ;
     { Initialise empty buffer with zero bytes }
     for i := 1 to sizeof(Header) do Header[i] := chr(0) ;

     // Record duration
     AppendFloat( Header, 'RECDUR=', Settings.RecordDuration ) ;

     AppendLogical( Header, 'RECCONT=', Settings.ContinuousRecording ) ;

     AppendLogical( Header, 'STARTSTIMREC=', Settings.StartStimulusOnRecord ) ;

     { Last raw data file used }
     //AppendString( Header, 'FILE=', CdrFH.FileName ) ;
     AppendLogical( Header, '16BIT=', Settings.Resolution16Bit ) ;

     AppendInt( Header, 'CEDI=', Amplifier.CED1902.Input ) ;
     AppendInt( Header, 'CEDG=', Amplifier.CED1902.Gain ) ;
     AppendFloat( Header, 'CEDGV=', Amplifier.CED1902.GainValue ) ;
     AppendInt( Header, 'CEDLP=', Amplifier.CED1902.LPFilter ) ;
     AppendInt( Header, 'CEDHP=', Amplifier.CED1902.HPFilter ) ;
     AppendInt( Header, 'CEDAC=', Amplifier.CED1902.ACCoupled ) ;
     AppendInt( Header, 'CEDDCO=', Amplifier.CED1902.DCOffset ) ;
     AppendInt( Header, 'CEDNF=', Amplifier.CED1902.NotchFilter ) ;
     AppendInt( Header, 'CEDPO=', Amplifier.CED1902.ComPort ) ;

     // Patch clamp amplifier data
     for ch := 1 to 2 do begin
         AppendInt( Header, format('AMP%d=',[ch]),Amplifier.AmplifierType[ch] ) ;
         AppendInt( Header, format('AMPGAINCH%d=',[ch]), Amplifier.GainTelegraphChannel[ch] ) ;
         AppendInt( Header, format('AMPMODECH%d=',[ch]), Amplifier.ModeTelegraphChannel[ch] ) ;
         end ;

     { Recording settings }
     AppendString( Header, 'TRG=', Settings.TriggerMode ) ;
     AppendLogical( Header, 'EXTTRIGAH=', Settings.ExternalTriggerActiveHigh ) ;

     { Event detector parameters }
     AppendInt( Header, 'DETCH=', Settings.EventDetector.Channel ) ;
     AppendInt( Header, 'DETDM=', Settings.EventDetector.DetectionMode ) ;
     AppendInt( Header, 'DETRS=', Settings.EventDetector.RecordSize ) ;
     AppendFloat( Header, 'DETYT=', Settings.EventDetector.yThreshold ) ;
     AppendFloat( Header, 'DETTT=', Settings.EventDetector.tThreshold ) ;
     AppendFloat( Header, 'DETDD=', Settings.EventDetector.DeadTime ) ;
     AppendFloat( Header, 'DETBAI=', Settings.EventDetector.BaselineAveragingInterval ) ;
     AppendFloat( Header, 'DETTAUR=', Settings.EventDetector.TauRise ) ;
     AppendFloat( Header, 'DETTAUD=', Settings.EventDetector.TauDecay ) ;
     AppendFloat( Header, 'DETPTF=', Settings.EventDetector.PreTriggerFraction ) ;
     AppendFloat( Header, 'DETAW=', Settings.EventDetector.AnalysisWindow ) ;
     AppendInt( Header, 'DETREW=', Settings.EventDetector.RisingEdgeWindow ) ;
     AppendFloat( Header, 'DETAVFI=', Settings.EventDetector.AvgFrequencyInterval ) ;
     AppendLogical( Header, 'DETEBT=', Settings.EventDetector.EnableBaselineTracking ) ;
     AppendFloat( Header, 'DETAMPSDSCALE=', Settings.EventDetector.AmpSDScale ) ;

     AppendInt( Header, 'VARRS=', Settings.Variance.RecordSize ) ;
     AppendInt( Header, 'VAROV=', Settings.Variance.RecordOverlap ) ;
     AppendFloat( Header, 'VARTR=', Settings.Variance.TauRise ) ;
     AppendFloat( Header, 'VARTD=', Settings.Variance.TauDecay ) ;

     AppendFloat( Header, 'UNITC=', Settings.DwellTimes.UnitCurrent ) ;
     AppendFloat( Header, 'DWTTH=', Settings.DwellTimes.Threshold ) ;

     AppendInt( Header, 'NRQ=', Settings.NumTriggerSweeps ) ;

//     AppendFloat( Header, 'VCDIV=', Settings.VCommand[0].DivideFactor ) ;
//     AppendFloat( Header, 'VCHOLD=', Settings.VCommand[0].HoldingVoltage ) ;
//     AppendFloat( Header, 'VCHOLD2=', Settings.VCommand[0].HoldingVoltageAlt ) ;

//     for i := 0 to High(Settings.VCommand) do begin
//         AppendFloat( Header, format('VCDIV%d=',[i]), Settings.VCommand[i].DivideFactor ) ;
//         AppendFloat( Header, format('VCHOLD%d=',[i]), Settings.VCommand[i].HoldingVoltage ) ;
//         AppendFloat( Header, format('VCHOLD%d2=',[i]), Settings.VCommand[i].HoldingVoltageAlt ) ;
//         end ;

 //    AppendInt( Header, 'DIGPORT=', Settings.DigitalOutputs ) ;
     AppendFloat( Header, 'DTMINDAC=', Settings.MinDACInterval ) ;
     AppendString( Header, 'TUNITS=', Settings.TUnits ) ;

     { Pipette seal test settings }
     AppendFloat( Header, 'STPH=', Settings.SealTest.PulseHeight ) ;
     AppendFloat( Header, 'STPH1=', Settings.SealTest.PulseHeight1 ) ;
     AppendFloat( Header, 'STPH2=', Settings.SealTest.PulseHeight2 ) ;
     AppendFloat( Header, 'STPH3=', Settings.SealTest.PulseHeight3 ) ;
     AppendFloat( Header, 'STHV1=', Settings.SealTest.HoldingVoltage1 ) ;
     AppendFloat( Header, 'STHV2=', Settings.SealTest.HoldingVoltage2 ) ;
     AppendFloat( Header, 'STHV3=', Settings.SealTest.HoldingVoltage3 ) ;
     AppendFloat( Header, 'STPW=', Settings.SealTest.PulseWidth ) ;
     AppendInt( Header, 'STCCH=', Settings.SealTest.CurrentChannel ) ;
     AppendInt( Header, 'STVCH=', Settings.SealTest.VoltageChannel ) ;
     AppendInt( Header, 'STUSE=', Settings.SealTest.Use ) ;
     AppendInt( Header, 'STDSC=', Settings.SealTest.DisplayScale ) ;
     AppendLogical( Header, 'STASC=', Settings.SealTest.AutoScale ) ;
     AppendLogical( Header, 'STFRU=', Settings.SealTest.FreeRun ) ;
     AppendInt( Header, 'STNAV=', Settings.SealTest.NumAverages ) ;
     AppendFloat( Header, 'STZAPA=', Settings.SealTest.ZapAmplitude ) ;
     AppendFloat( Header, 'STZAPD=', Settings.SealTest.ZapDuration ) ;
     AppendLogical( Header, 'STGAP=', Settings.SealTest.GaFromPeak ) ;

     AppendInt( Header, 'NC=', Settings.NumChannels ) ;
     //AppendInt( Header, 'ADVRI=', Settings.ADCVoltageRangeIndex ) ;
     AppendFloat( Header, 'DT=', Settings.ADCSamplingInterval ) ;

     { Width/height of clipboard bitmaps }
     AppendInt( Header, 'BMAPW=', Settings.BitmapWidth ) ;
     AppendInt( Header, 'BMAPH=', Settings.BitmapHeight ) ;

     AppendLogical( Header, 'DISPGRID=', Settings.DisplayGrid ) ;
//     AppendInt( Header, 'DISPNVG=', Settings.NumVerticalGridLines ) ;
//     AppendInt( Header, 'DISPNHG=', Settings.NumHorizontalGridLines ) ;
     AppendFloat( Header, 'DISPDUR=', Settings.DisplayDuration ) ;
     AppendLogical( Header, 'FIXZERO=', Settings.FixedZeroLevels) ;

     { Plotting page settings }
     AppendFloat( Header, 'PLTPM=',Settings.Plot.TopMargin ) ;
     AppendFloat( Header, 'PLBTM=',Settings.Plot.BottomMargin ) ;
     AppendFloat( Header, 'PLLFM=',Settings.Plot.LeftMargin ) ;
     AppendFloat( Header, 'PLRTM=',Settings.Plot.RightMargin ) ;
     AppendString( Header, 'PLFNT=',Settings.Plot.FontName ) ;
     AppendInt( Header, 'PLFSI=',Settings.Plot.FontSize ) ;
     AppendInt( Header, 'PLLTH=',Settings.Plot.LineThickness ) ;
     AppendLogical( Header, 'PLSHL=',Settings.Plot.ShowLines ) ;
     AppendInt( Header, 'PLMKS=',Settings.Plot.MarkerSize ) ;
     AppendLogical( Header, 'PLSHM=',Settings.Plot.ShowMarkers ) ;
     AppendLogical( Header, 'PLCOL=',Settings.Plot.UseColor ) ;

     // Page display module (PageView.pas) settings
     AppendInt( Header, 'PVLPP=',Settings.PageViewLinesPerPage ) ;
     AppendFloat( Header, 'PVLD=',Settings.PageViewLineDuration ) ;

     AppendString( Header, 'DDIR=', Main.DataDirectory ) ;

     AppendInt( Header, 'LABINT=',Settings.LaboratoryInterface ) ;
     AppendInt( Header, 'LABDEV=',Settings.DeviceNumber ) ;
     AppendInt( Header, 'LABADIP=',Settings.ADCInputMode ) ;

     { Recently used data files }
     AppendString( Header, 'RF0=', Settings.RecentFiles[0] ) ;
     AppendString( Header, 'RF1=', Settings.RecentFiles[1] ) ;
     AppendString( Header, 'RF2=', Settings.RecentFiles[2] ) ;
     AppendString( Header, 'RF3=', Settings.RecentFiles[3] ) ;

     AppendFloat( Header, 'CAPCMMX=', Settings.Capacity.CmDisplayMax ) ;
     AppendFloat( Header, 'CAPGSMX=', Settings.Capacity.GsDisplayMax ) ;
     AppendFloat( Header, 'CAPGMMX=', Settings.Capacity.GmDisplayMax ) ;
     AppendFloat( Header, 'CAPFREQ=', Settings.Capacity.Frequency ) ;
     AppendFloat( Header, 'CAPVREV=', Settings.Capacity.VRev ) ;
     AppendInt( Header, 'CAPGR=', Settings.Capacity.GrealChan) ;
     AppendInt( Header, 'CAPGI=', Settings.Capacity.GimagChan) ;
     AppendInt( Header, 'CAPIM=', Settings.Capacity.ImChan) ;
     AppendInt( Header, 'CAPVM=', Settings.Capacity.VmChan) ;
     AppendInt( Header, 'CAPGM=', Settings.Capacity.GmChan) ;
     AppendInt( Header, 'CAPGS=', Settings.Capacity.GsChan) ;
     AppendInt( Header, 'CAPCM=', Settings.Capacity.CmChan) ;
     AppendLogical( Header, 'CAPINVGR=', Settings.Capacity.InvertGReal) ;
     AppendLogical( Header, 'CAPINVGI=', Settings.Capacity.InvertGImag) ;
     AppendFloat( Header, 'CAPRSCOMP=', Settings.Capacity.RSeriesComp ) ;
     AppendFloat( Header, 'CAPCCOMP=', Settings.Capacity.CellCapacityComp ) ;
     AppendLogical( Header, 'CAPCOMPINUSE=', Settings.Capacity.CapacityCompensationInUse) ;
     AppendLogical( Header, 'CAPINUSE=', Settings.Capacity.InUse) ;

     // Fluorescence settings
     AppendLogical( Header, 'FLRINUSE=', Settings.Fluorescence.InUse) ;
     AppendFloat( Header, 'FLRMAX=', Settings.Fluorescence.RMax ) ;
     AppendFloat( Header, 'FLRMIN=', Settings.Fluorescence.RMin ) ;
     AppendFloat( Header, 'FLKEFF=', Settings.Fluorescence.KEff ) ;
     AppendFloat( Header, 'FLTHRE=', Settings.Fluorescence.FThreshold ) ;
     AppendInt( Header, 'FLNUM=', Settings.Fluorescence.NumerChan ) ;
     AppendInt( Header, 'FLDEN=', Settings.Fluorescence.DenomChan ) ;
     AppendInt( Header, 'FLRAT=', Settings.Fluorescence.RatioChan ) ;
     AppendInt( Header, 'FLCON=', Settings.Fluorescence.ConcChan ) ;
     AppendFloat( Header, 'FLRDMX=', Settings.Fluorescence.RatioDisplayMax ) ;
     AppendFloat( Header, 'FLCDMX=', Settings.Fluorescence.ConcDisplayMax ) ;
     AppendString( Header, 'FLIONNAME=', Settings.Fluorescence.IonName ) ;
     AppendString( Header, 'FLIONUNITS=', Settings.Fluorescence.IonUnits ) ;

     // Real time event frequency settings
     AppendLogical( Header, 'EFINUSE=', Settings.RTEventAnalysis.InUse) ;
     AppendInt( Header, 'EFCH=', Settings.RTEventAnalysis.Channel ) ;
     AppendFloat( Header, 'EFTHRESH=', Settings.RTEventAnalysis.DetectionThreshold ) ;
     AppendFloat( Header, 'EFRMEAN=', Settings.RTEventAnalysis.RunningMeanTime ) ;
     AppendFloat( Header, 'EFDEADT=', Settings.RTEventAnalysis.DeadTime ) ;
     AppendFloat( Header, 'EFCOUNTI=', Settings.RTEventAnalysis.CountingInterval ) ;

     // Real time resistance settings
     AppendLogical( Header, 'RESINUSE=', Settings.RTResistance.InUse) ;
     AppendInt( Header, 'RESICH=', Settings.RTResistance.ImChannel ) ;
     AppendInt( Header, 'RESVCH=', Settings.RTResistance.VmChannel ) ;
     AppendFloat( Header, 'RESAMPLITUDE=', Settings.RTResistance.Amplitude ) ;
     AppendFloat( Header, 'RESDUR=', Settings.RTResistance.Duration ) ;
     AppendFloat( Header, 'RESINT=', Settings.RTResistance.Interval ) ;
     AppendInt( Header, 'RESPLT=', Settings.RTResistance.Plot ) ;

     // Currently selected stimulus file
     AppendString( Header, 'STIMFILE=', Settings.VProgramFileName ) ;

     // Save post-synaptic current simulation settings
     AppendFloat( Header, 'EPCDUR=', Settings.SimEPC.Duration) ;
     AppendFloat( Header, 'EPCAMP=', Settings.SimEPC.Amplitude) ;
     AppendFloat( Header, 'EPCAMPSD=', Settings.SimEPC.AmplitudeSD) ;
     AppendFloat( Header, 'EPCTRISE=', Settings.SimEPC.TauRise) ;
     AppendFloat( Header, 'EPCTDECAY=', Settings.SimEPC.TauDecay) ;
     AppendFloat( Header, 'EPCNOISESD=', Settings.SimEPC.NoiseSD) ;
     AppendFloat( Header, 'EPCFREQ=', Settings.SimEPC.Frequency) ;
     AppendFloat( Header, 'EPCFREQSD=', Settings.SimEPC.FrequencySD) ;     
     AppendFloat( Header, 'EPCDEL=', Settings.SimEPC.Delay ) ;
     AppendFloat( Header, 'EPCSINEAMP=', Settings.SimEPC.SineAmplitude) ;
     AppendFloat( Header, 'EPCSINEFREQ=', Settings.SimEPC.SineFrequency) ;
     AppendLogical( Header, 'EPCRAND=', Settings.SimEPC.RandomEvents) ;
     AppendInt( Header, 'EPCUNITS=', Settings.SimEPC.UnitsIndex) ;
     AppendFloat( Header, 'EPCPROB=', Settings.SimEPC.ReleaseProbability) ;
     AppendFloat( Header, 'EPCPOOL=', Settings.SimEPC.ReleasablePool) ;
     AppendFloat( Header, 'EPCDEP=', Settings.SimEPC.Depression) ;
     AppendFloat( Header, 'EPCTAUDEP=', Settings.SimEPC.TauDepression) ;

     // Save main form size and position
     AppendInt( Header, 'FORMTOP=',Main.Top ) ;
     AppendInt( Header, 'FORMLEFT=',Main.Left ) ;
     AppendInt( Header, 'FORMWIDTH=',Main.Width ) ;
     AppendInt( Header, 'FORMHEIGHT=',Main.Height ) ;

     if FileWrite( IniFileHandle, Header, Sizeof(Header) ) <> Sizeof(Header) then
        ShowMessage( IniFileName + ' Write - Failed ' ) ;
     FileClose( IniFileHandle ) ;

     if HeaderArrayFull then
        ShowMessage( INIFileName + ' file header parameter array full!' ) ;

     end ;


procedure OpenLogFile ;
// -------------
// Open log file
// -------------
begin
     { Create a log file using current date }
     FormatSettings.DateSeparator := '-' ;
     LogFileName := Main.SettingsDirectory + DateToStr(Date)+'.log' ;
     LogFileAvailable := True ;
     AssignFile( LogFile, LogFileName ) ;
     try
        if FileExists( LogFileName ) then Append(LogFile)
                                     else ReWrite(LogFile) ;
     except
           on EInOutError do begin
              //ShowMessage( ' WinEDR - Cannot create Log File' ) ;
              LogFileAvailable := False ;
              end ;
           end ;
     { If file doesn't exist ... disable access }
     Main.InspectLogFile.Enabled := LogFileAvailable ;

     end ;


procedure WriteToLogFile( Line : string ) ;
begin
     if LogFileAvailable then WriteLn( LogFile, TimeToStr(Time) + ' ' + Line ) ;
     end ;


procedure CloseLogFile ;
begin

     if not LogFileAvailable then Exit ;

     try
        CloseFile(LogFile) ;
     except
        on EInOutError do begin
              ShowMessage( ' WinEDR - Error closing Log File' ) ;
              LogFileAvailable := False ;
              end ;
           end ;
     end ;


procedure MakeBackupFile ;
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
     SaveCDRHeader( CdrFH ) ;

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


procedure RestoreFromBackupFile ;
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
     GetCDRHeader( CdrFH ) ;

     { Close backup file }
     FileClose( FileHandle ) ;

     WriteToLogFile( cdrFH.FileName + ' restored from ' + FileName ) ;

     end ;



end.
