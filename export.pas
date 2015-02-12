unit export;
{ ================================================================
  WinEDR (c) J. Dempster, University of Strathclyde, 1998-99
  Data file export module
  ================================================================
  5/2/00
  28/2/00 ... Change File Name now works, channel captions updated
  9/2/02 ... V2.2.6 ASCII text export added
  26/2/02 ... V2.3.0 ASCII text export now works properly
              Progress reported to main status bar
  14/08/02 ... Bug which prevented export when output file did not exist fixed  (V2.3.4)
  1/12/03 .... Export now uses TADCDataFile component
  18/7/5  .... Support for Chart, WAV, Igor added
  14/12/6 .... Export to IGOR IBW files now works
  14/12/12 ... Zero level now subtracted from data when exported to ASCII text files.
  21/12/12 ... Individual channels can now be selected for export.
               Channels and record duration now added to export file name
  10/02/15 ... No. of selectable channels increased to 32
  12/02/15 ... Matlab export added
  }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, global, convert, ADCDataFile, maths, strutils, math, MatFileWriterUnit, SESLabIO, UITYpes ;

type
  TExportFrm = class(TForm)
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    ChannelsGrp: TGroupBox;
    ckCh0: TCheckBox;
    ckCh1: TCheckBox;
    ckCh2: TCheckBox;
    ckCh3: TCheckBox;
    ckCh4: TCheckBox;
    ckCh5: TCheckBox;
    ckCh6: TCheckBox;
    ckCh7: TCheckBox;
    GroupBox3: TGroupBox;
    edFileName: TEdit;
    GroupBox2: TGroupBox;
    rbABF: TRadioButton;
    rbCFS: TRadioButton;
    bChangeName: TButton;
    bOK: TButton;
    bCancel: TButton;
    SaveDialog: TSaveDialog;
    rbASCII: TRadioButton;
    rbEDR: TRadioButton;
    ExportFile: TADCDataFile;
    rbCHT: TRadioButton;
    rbWAV: TRadioButton;
    rbIBW: TRadioButton;
    ckCh8: TCheckBox;
    ckCh9: TCheckBox;
    ckCh10: TCheckBox;
    ckCh11: TCheckBox;
    ckCh12: TCheckBox;
    ckCh13: TCheckBox;
    ckCh14: TCheckBox;
    ckCh15: TCheckBox;
    ckCh16: TCheckBox;
    ckCh17: TCheckBox;
    ckCh18: TCheckBox;
    ckCh19: TCheckBox;
    ckCh20: TCheckBox;
    ckCh21: TCheckBox;
    ckCh22: TCheckBox;
    ckCh23: TCheckBox;
    ckCh24: TCheckBox;
    ckCh25: TCheckBox;
    ckCh26: TCheckBox;
    ckCh27: TCheckBox;
    ckCh28: TCheckBox;
    ckCh29: TCheckBox;
    ckCh30: TCheckBox;
    ckCh31: TCheckBox;
    rbMAT: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure rbABFClick(Sender: TObject);
    procedure rbLDTClick(Sender: TObject);
    procedure rbCFSClick(Sender: TObject);
    procedure rbASCIIClick(Sender: TObject);
    procedure rbEDRClick(Sender: TObject);
  private
    { Private declarations }
    ExportFileName : string ;
    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    function GetChannelsInUse( var UseChannel : Array of Boolean ): Integer ;
    procedure UpdateSettings ;
    procedure ExportToFile ;
    procedure ExportToIGORFile ;
    procedure ExportToMATFile ;
    function CreateExportFileName(
             FileName : string
             ) : String ;

  public
    { Public declarations }
  end;

var
  ExportFrm: TExportFrm;

implementation

uses Mdiform, fileio ;

{$R *.DFM}


procedure TExportFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
begin

     { Set block of EDR file to be exported }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiLimit := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)*cdrFH.dt ;
     edRange.HiValue := edRange.HiLimit ;

     { Update O/P file name channel selection options }
     ExportFileName := CDRFH.FileName ;
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;

     end;


procedure TExportFrm.SetChannel(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ---------------------------
// Set channel selection state
// ---------------------------
begin
     if ch < CDRFH.NumChannels then begin
        CheckBox.Visible := True ;
        CheckBox.Checked := Channel[ch].InUse ;
        CheckBox.Caption := Channel[ch].ADCName ;
        end
     else CheckBox.Visible := False ;
     end ;


procedure TExportFrm.bOKClick(Sender: TObject);
// ----------------------
// Export to output file
// ----------------------
begin

    if rbIBW.Checked then ExportToIGORFile
    else if rbMAT.Checked then ExportToMATFile
    else ExportToFile ;

    end ;




procedure TExportFrm.ExportToFile ;
// -------------------------------------------------
// Copy selected section of data file to export file
// -------------------------------------------------
const
   NumScansPerBuf = 512 ;
var
   StartAt,EndAt,ch,i,j,n : Integer ;
   UseChannel : Array[0..EDRChannelLimit] of Boolean ;
   InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   InScan : Integer ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   chOut : Integer ;
   Done : Boolean ;
   ExportType : TADCDataFileType ;
   s : string ;
begin

     s := '[' ;
     n := 0 ;
     for ch := 0 to CdrFH.NumChannels-1 do if UseChannel[ch] then begin
         s := s + Channel[ch].ADCName + ' ' ;
         Inc(n) ;
         end ;
     if n = CdrFH.NumChannels then s := '[' ;

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        StartAt := Round(edRange.LoValue/CdrFH.dt) ;
        EndAt := Round(edRange.HiValue/CdrFH.dt) ;
        s := s + format( '%.6g-%.6gs',[edRange.LoValue,edRange.HiValue]) ;
        end ;
     s := s + ']';

     if s <> '[]' then ExportFileName := ANSIReplaceText( ExportFileName,
                                                          ExtractFileExt(ExportFileName),
                                                          s + ExtractFileExt(ExportFileName)) ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Get channels to be exported
     GetChannelsInUse( UseChannel ) ;

     // Export file type
     if rbABF.Checked then ExportType := ftAxonABF
     else if rbASCII.Checked then ExportType := ftASC
     else if rbCFS.Checked then ExportType := ftCFS
     else if rbCHT.Checked then ExportType := ftCHT
     else if rbWAV.Checked then ExportType := ftWAV
     else if rbIBW.Checked then ExportType := ftIBW
     else ExportType := ftEDR ;

     // Create empty export data file
     ExportFile.CreateDataFile( ExportFileName,
                                ExportType ) ;

     // Set file parameters
     ExportFile.NumChannelsPerScan := CdrFH.NumChannels ;
     ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
     ExportFile.MaxADCValue := Channel[0].ADCMaxValue ;
     ExportFile.MinADCValue := -Channel[0].ADCMaxValue -1 ;
     ExportFile.ScanInterval := CdrFH.dt ;
     ExportFile.IdentLine := CdrFH.IdentLine ;
     ExportFile.RecordNum := 1 ;
     ExportFile.ABFAcquisitionMode := ftGapFree ;

     chOut := 0 ;
     for ch := 0 to CdrFH.NumChannels-1 do if UseChannel[ch] then begin
         ExportFile.ChannelOffset[chOut] := chOut ;
         ExportFile.ChannelADCVoltageRange[chOut] := CdrFH.ADCVoltageRange ;
         ExportFile.ChannelName[chOut] := Channel[ch].ADCName ;
         ExportFile.ChannelUnits[chOut] := Channel[ch].ADCUnits ;
         ExportFile.ChannelScale[chOut] := Channel[ch].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[chOut] := Channel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[chOut] := Channel[ch].ADCAmplifierGain ;
         Inc(chOut) ;
         end ;
     ExportFile.NumChannelsPerScan := chOut ;

     { Copy records }
     InScan := StartAt ;
     NumScansToCopy := EndAt - StartAt + 1 ;
     OutScan := 0 ;
     Done := False ;
     While not Done do begin

         // Read from buffer
         NumScansToRead := Min( NumScansToCopy,NumScansPerBuf ) ;
         NumScansRead := ReadCDRBuffer( CDRFH, InScan, InBuf, NumScansToRead ) ;
         if NumScansRead <= 0 then Done := True ;

         // Copy required channels
         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to CdrFH.NumChannels-1 do if UseChannel[ch] then begin
                 OutBuf[j] := InBuf[i*CdrFH.NumChannels+Channel[ch].ChannelOffset]
                              - Channel[ch].ADCZero ;
                 Inc(j) ;
                 end ;
             end ;

         // Write to export file
         //OutputDebugString(PChar(format('%d %d',[OutScan,NumScansRead])));
         ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
         OutScan := OutScan + NumScansRead ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' EXPORT: Exporting time points %d/%d to %s ',
         [InScan,EndAt,ExportFileName]) ;

         InScan := InScan + NumScansRead ;
         NumScansToCopy := NumScansToCopy - NumScansRead ;
         if NumScansToCopy <= 0 then Done := True ;

         end ;

     // Close export data file
     ExportFile.CloseDataFile ;

     // Final Report
     Main.StatusBar.SimpleText := format(
     ' EXPORT: %d scans exported to %s ',
     [EndAt-StartAt+1,ExportFileName]) ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     end;

function TExportFrm.GetChannelsInUse(
         var UseChannel : Array of Boolean ) : Integer ;
// -------------------------------
// Return channels to be exported
// -------------------------------
var
    i,n : Integer ;
begin

     // Channels to be exported
     for i := 0 to High(UseChannel) do UseChannel[i] := True ;
     UseChannel[0] :=  ckCh0.Checked ;
     UseChannel[1] :=  ckCh1.Checked ;
     UseChannel[2] :=  ckCh2.Checked ;
     UseChannel[3] :=  ckCh3.Checked ;
     UseChannel[4] :=  ckCh4.Checked ;
     UseChannel[5] :=  ckCh5.Checked ;
     UseChannel[6] :=  ckCh6.Checked ;
     UseChannel[7] :=  ckCh7.Checked ;
     UseChannel[8] :=  ckCh8.Checked ;
     UseChannel[9] :=  ckCh9.Checked ;
     UseChannel[10] := ckCh10.Checked ;
     UseChannel[11] := ckCh11.Checked ;
     UseChannel[12] := ckCh12.Checked ;
     UseChannel[13] := ckCh13.Checked ;
     UseChannel[14] := ckCh14.Checked ;
     UseChannel[15] := ckCh15.Checked ;
     UseChannel[16] :=  ckCh16.Checked ;
     UseChannel[17] :=  ckCh17.Checked ;
     UseChannel[18] :=  ckCh18.Checked ;
     UseChannel[19] :=  ckCh19.Checked ;
     UseChannel[20] :=  ckCh20.Checked ;
     UseChannel[21] :=  ckCh21.Checked ;
     UseChannel[22] :=  ckCh22.Checked ;
     UseChannel[23] :=  ckCh23.Checked ;
     UseChannel[24] :=  ckCh24.Checked ;
     UseChannel[25] :=  ckCh25.Checked ;
     UseChannel[26] := ckCh26.Checked ;
     UseChannel[27] := ckCh27.Checked ;
     UseChannel[28] := ckCh28.Checked ;
     UseChannel[29] := ckCh29.Checked ;
     UseChannel[30] := ckCh30.Checked ;
     UseChannel[31] := ckCh31.Checked ;

     n := 0 ;
     for i := 0 to CDRFH.NumChannels-1 do if UseChannel[i] then Inc(n) ;
     Result := n ;

     end ;


procedure TExportFrm.ExportToIGORFile ;
// -------------------------------------------------
// Export data to IGOR IBW file(s)
// -------------------------------------------------
const
   NumScansPerBuf = 256 ;
var
   StartAt,EndAt,ch,i,j : Integer ;
   UseChannel : Array[0..EDRChannelLimit] of Boolean ;
   InBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   InScan : Integer ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   Done : Boolean ;
   FileName : String ;
begin

     // Get time range to export
     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := CdrFH.NumSamplesInFile div CdrFH.NumChannels ;
        end
     else begin
        StartAt := Round(edRange.LoValue/CdrFH.dt) ;
        EndAt := Round(edRange.HiValue/CdrFH.dt) ;
        end ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Get channels to be exported
     GetChannelsInUse( UseChannel ) ;

     for ch := 0 to CdrFH.NumChannels-1 do if UseChannel[ch] then begin

         // Create empty export data file

         FileName := ANSIReplaceText( ExportFileName,
                                      '.ibw',
                                      format( '[%s].ibw',[Channel[ch].ADCName])) ;
         ExportFile.CreateDataFile( FileName, ftIBW ) ;

         // Set file parameters
         ExportFile.NumChannelsPerScan := 1 ;
         ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
         ExportFile.MaxADCValue := Channel[0].ADCMaxValue ;
         ExportFile.MinADCValue := -Channel[0].ADCMaxValue -1 ;
         ExportFile.ScanInterval := CdrFH.dt ;
         ExportFile.IdentLine := CdrFH.IdentLine ;
         ExportFile.RecordNum := 1 ;
         ExportFile.ABFAcquisitionMode := ftGapFree ;
         ExportFile.NumChannelsPerScan := 1 ;

         ExportFile.ChannelOffset[0] := 0 ;
         ExportFile.ChannelADCVoltageRange[0] := CdrFH.ADCVoltageRange ;
         ExportFile.ChannelName[0] := Channel[ch].ADCName ;
         ExportFile.ChannelUnits[0] := Channel[ch].ADCUnits ;
         ExportFile.ChannelScale[0] := Channel[ch].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[0] := Channel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[0] := Channel[ch].ADCAmplifierGain ;

         { Copy records }
         InScan := StartAt ;
         NumScansToCopy := EndAt - StartAt + 1 ;
         OutScan := 0 ;
         Done := False ;
         While not Done do begin

            // Read from buffer
            NumScansToRead := Min(NumScansToCopy,NumScansPerBuf) ;
            NumScansRead := ReadCDRBuffer( CDRFH, InScan, InBuf, NumScansToRead ) ;
            if NumScansRead <= 0 then Done := True ;

            // Copy required channel
            j := Channel[ch].ChannelOffset ;
            for i := 0 to NumScansRead-1 do begin
                OutBuf[i] := InBuf[j] ;
                j := j + CDRFH.NumChannels ;
                end ;

            // Write to export file
            ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
            OutScan := OutScan + NumScansRead ;

            // Report progress
            Main.StatusBar.SimpleText := format(
            ' EXPORT: Exporting sample time points %d/%d to %s ',
            [InScan,EndAt,FileName]) ;

            InScan := InScan + NumScansRead ;
            NumScansToCopy := NumScansToCopy - NumScansRead ;
            if NumScansToCopy <= 0 then Done := True ;

            end ;

         // Close export data file
         ExportFile.CloseDataFile ;

         // Final Report
         Main.StatusBar.SimpleText := format(
         ' EXPORT: %d sample time points exported to %s ',
         [EndAt-StartAt+1,FileName]) ;
         WriteToLogFile( Main.StatusBar.SimpleText ) ;

         end ;



     end;


procedure TExportFrm.ExportToMATFile ;
// -------------------------------------------------
// Copy selected records to MATLab .MAT file
// -------------------------------------------------
const
    NumScansPerBuf = 100000 ;
var
   ch,i,j : Integer ;
   InBuf : PSmallIntArray ;       // Source data buffer
   YBuf : PDoubleArray ;      // Output data buffer
   TBuf : PDoubleArray ;      // Output data buffer
   StartAt,EndAt : Integer ;                 // Record counter
   UseChannel : Array[0..EDRChannelLimit] of Boolean ;
   NumChannelsExported : Integer ;
   Writer : TMATFileWriter ;
   NumScans,NumScansRead,NumScansToRead,NumScansToExport : Integer ;
   FileName : String ;
begin

     // Get time range to export
     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (CdrFH.NumSamplesInFile div CdrFH.NumChannels) - 1 ;
        end
     else begin
        StartAt := Round(edRange.LoValue/CdrFH.dt) ;
        EndAt := Round(edRange.HiValue/CdrFH.dt) ;
        end ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Get channels to be exported
     GetChannelsInUse( UseChannel ) ;

     // Add record range to file name
     FileName := CreateExportFileName(ExportFileName) ;

     // Create buffers
     NumScansToExport := EndAt - StartAt + 1 ;
     GetMem( InBuf, NumScansPerBuf*CDRFH.NumChannels*2 ) ;
     GetMem( YBuf, NumScansToExport*CDRFH.NumChannels*8 ) ;
     GetMem( TBuf, NumScansToExport*8 ) ;

     // Open MAT file
     Writer := TMATFileWriter.Create();
     Writer.OpenMATFile( FileName ) ;
     Writer.WriteFileHeader;

     Try

     // Write time vector
     for i := 0 to NumScansToExport-1 do begin
         TBuf^[i] := i*CDRFH.dt ;
         end ;

     // Extract channels to copy to to YBuf
     NumScansToRead := NumScansToExport ;
     NumChannelsExported := 0 ;
     NumScansRead := 0 ;
     while NumScansToRead > 0 do begin
           NumScans := ReadCDRBuffer( CDRFH, StartAt, InBuf^,Min(NumScansToRead,NumScansPerBuf) ) ;
           NumChannelsExported := 0 ;
           for ch := 0 to CDRFH.NumChannels-1 do if UseChannel[ch] then begin
               j := NumChannelsExported*NumScansToExport + NumScansRead ;
               for i := 0 to NumScans-1 do begin
                   YBuf^[j] := (InBuf^[(i*CDRFH.NumChannels)+Channel[ch].ChannelOffset]
                                            - Channel[ch].ADCZero)*Channel[ch].ADCSCale ;
                   Inc(j) ;
                   end ;
               Inc(NumChannelsExported) ;
               end ;
           NumScansToRead := NumScansToRead - NumScans ;
           NumScansRead := NumScansRead + NumScans ;
           if NumScansRead = 0 then NumScansToRead := 0 ;
           StartAt := StartAt + NumScans ;
           end ;

     // Write to MAT file
     Writer.WriteDoubleMatrixHeader('T',NumScansRead,1);
     Writer.WriteDoubleMatrixValues( TBuf^,NumScansRead,1) ;
     Writer.WriteDoubleMatrixHeader('Y',NumScansRead,NumChannelsExported);
     Writer.WriteDoubleMatrixValues( YBuf^, NumScansRead,NumChannelsExported) ;

     // Final Report
     Main.StatusBar.SimpleText := format(
     ' EXPORT: %.5g - %.5g exported to %s ',
     [StartAt*CDRFH.dt,EndAt*CDRFH.dt,FileName]) ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     Finally
        // Free allocated buffers
        FreeMem( InBuf ) ;
        FreeMem( YBuf ) ;
        FreeMem( TBuf ) ;
        // Close export data file
        Writer.CloseMATFile;
        end ;

     end;


procedure TExportFrm.bChangeNameClick(Sender: TObject);
{ ------------------------------------------
  Change name/location of export destination
  ------------------------------------------ }
begin
     SaveDialog.DefaultExt := ExtractFileExt( ExportFileName ) ;
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.Filter := ' Files (*' + SaveDialog.DefaultExt + ')|*' +
                            SaveDialog.DefaultExt + '|' ;

     SaveDialog.FileName := ExportFileName ;
     SaveDialog.Title := 'Export File ' ;
     if Main.DataDirectory <> '' then
        SaveDialog.InitialDir := Main.DataDirectory ;

     if SaveDialog.Execute then ExportFileName := SaveDialog.FileName ;
     edFileName.text := ExportFileName ;
     end;


procedure TExportFrm.rbABFClick(Sender: TObject);
// ---------------------------------
// Axon Binary File option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportFrm.rbLDTClick(Sender: TObject);
// ---------------------------------
// Qub data file option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportFrm.rbCFSClick(Sender: TObject);
// ---------------------------------
// CED Filing System option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportFrm.UpdateSettings ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
begin

     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;
     SetChannel( ckCh8, 8 ) ;
     SetChannel( ckCh9, 9 ) ;
     SetChannel( ckCh10, 10 ) ;
     SetChannel( ckCh11, 11 ) ;
     SetChannel( ckCh12, 12 ) ;
     SetChannel( ckCh13, 13 ) ;
     SetChannel( ckCh14, 14 ) ;
     SetChannel( ckCh15, 15 ) ;
     SetChannel( ckCh16, 16 ) ;
     SetChannel( ckCh17, 17 ) ;
     SetChannel( ckCh18, 18 ) ;
     SetChannel( ckCh19, 19 ) ;
     SetChannel( ckCh20, 20 ) ;
     SetChannel( ckCh21, 21 ) ;
     SetChannel( ckCh22, 22 ) ;
     SetChannel( ckCh23, 23) ;
     SetChannel( ckCh24, 24 ) ;
     SetChannel( ckCh25, 25 ) ;
     SetChannel( ckCh26, 26 ) ;
     SetChannel( ckCh27, 27 ) ;
     SetChannel( ckCh28, 28 ) ;
     SetChannel( ckCh29, 29 ) ;
     SetChannel( ckCh30, 30 ) ;
     SetChannel( ckCh31, 31 ) ;

     if rbABF.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.abf' ) ;
     if rbCFS.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.cfs' ) ;
     if rbASCII.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.txt' ) ;
     if rbCHT.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.cht' ) ;
     if rbWAV.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.wav' ) ;
     if rbIBW.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.ibw' ) ;
     if rbMAT.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.mat' ) ;
     if rbEDR.Checked then begin
        ExportFileName := ChangeFileExt( ExportFileName, '.edr' ) ;
        if LowerCase(ExportFileName) = LowerCase(CdrFH.FileName) then begin
           ExportFileName := StringReplace( ExportFileName,
                                            '.edr',
                                            '(1).edr',
                                            [rfIgnoreCase] ) ;
           end ;
        end ;
     edFileName.text := ExportFileName ;
     end ;


procedure TExportFrm.rbASCIIClick(Sender: TObject);
// ---------------------------------
// ASCII text file option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportFrm.rbEDRClick(Sender: TObject);
// ---------------------------------
// WinEDR file option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


function TExportFrm.CreateExportFileName(
         FileName : string
         ) : String ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
var
    StartAt,EndAt : single ;
begin

     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;
     SetChannel( ckCh8, 8 ) ;
     SetChannel( ckCh9, 9 ) ;
     SetChannel( ckCh10, 10 ) ;
     SetChannel( ckCh11, 11 ) ;
     SetChannel( ckCh12, 12 ) ;
     SetChannel( ckCh13, 13 ) ;
     SetChannel( ckCh14, 14 ) ;
     SetChannel( ckCh15, 15 ) ;
     SetChannel( ckCh16, 16 ) ;
     SetChannel( ckCh17, 17 ) ;
     SetChannel( ckCh18, 18 ) ;
     SetChannel( ckCh19, 19 ) ;
     SetChannel( ckCh20, 20 ) ;
     SetChannel( ckCh21, 21 ) ;
     SetChannel( ckCh22, 22 ) ;
     SetChannel( ckCh23, 23 ) ;
     SetChannel( ckCh24, 24 ) ;
     SetChannel( ckCh25, 25 ) ;
     SetChannel( ckCh26, 26 ) ;
     SetChannel( ckCh27, 27 ) ;
     SetChannel( ckCh28, 28 ) ;
     SetChannel( ckCh29, 29 ) ;
     SetChannel( ckCh30, 30 ) ;
     SetChannel( ckCh31, 31 ) ;

     ChangeFileExt( FileName, '.tmp' ) ;

     // Add record range to file name
     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := (CdrFH.NumSamplesInFile div CdrFH.NumChannels)*CDRFH.dt ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // Set file extension to .tmp to locate end of file later
     FileName := ChangeFileExt( FileName, '.tmp' ) ;

     // Add record range
     FileName := ANSIReplaceText( FileName,
                                  '.tmp',
                                  format('[%.6g-%.6gs].tmp',[StartAt,EndAt]) ) ;

     if rbABF.Checked then FileName := ChangeFileExt( FileName, '.abf' ) ;
     if rbCFS.Checked then FileName := ChangeFileExt( FileName, '.cfs' ) ;
     if rbASCII.Checked then FileName := ChangeFileExt( FileName, '.txt' ) ;
     if rbEDR.Checked then FileName := ChangeFileExt( FileName, '.edr' ) ;
     if rbIBW.Checked then FileName := ChangeFileExt( FileName, '.ibw' ) ;
     if rbMAT.Checked then FileName := ChangeFileExt( FileName, '.mat' ) ;

     Result := FileName ;
     edFileName.Text := FileName ;

     end ;



end.
