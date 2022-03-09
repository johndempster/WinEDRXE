unit Mdiform;
{ =======================================================================
  WinEDR Master MDI form (c) John Dempster, 1996-2010 All Rights Reserved
  =======================================================================
  V1.0
  V1.2 15/6/98 Variance/Power spectrum now usable Print and Copy Data work
  V1.2a 23/6/98 ASCII data file import option added
  V1.2b 25/6/98 Binary file import added
                ASCII text import now imports data correctly
                Clipboard buffer limit reduced to 31000
  V1.3 20/7/98 Variance/Spectral Analysis module
                 DC and AC Channel selection now works correctly.
                 Set Block dialog box added allowing Test/Background and
                 rejected status to be set for a block of records.
                 Sum of 2 Lorentzian fit now works correctly
  V1.3a 21/7/98  Bug in Copy Data which caused  GPI fixed
  V2.0 24/6/99   32 bit version, name changed to WinEDR
       4/9/99    12/16 bit A/D resolution added
  V2.0.1 7/2/99 ... TProgressBar out of range fixed
                    Background subtraction now works in variance/mean plots
  V2.0.2 21/2/00 ... Copy Image and Print outputs improved
  V2.0.3 25/2/00 ... Individual exponential components now displayed (dweltime.pas)
  V2.0.4 14/3/00 ... Power spectrum of a range of records now works correctly
  V2.0.5 12/4/00 ... Export of WCP from Detect & Export now fixed
  V2.0.6 27/4/00 ... Support for NI boards up to 267 NIDAQ V6.5.1 added
  V2.0.7 29/6/00 ... Multi-channel A/D now works with PCI-1200
  V2.1.0 18/2/01 ... Support for Digidata 132X added
                     Can now read Axopatch gain telegraph
  V2.1.1 12/3/01 ... Axoutils32.dll now loaded from c:\axon\libs (not \windows\system)
                     (WinWCP now compatible with AxoScope)
  V2.1.2 27/3/01 ... Support for CED Power 1401's added
                     Warning now given when READ-ONLY files opened
                     DigFilt.pas bug fixed (channel cross-talk in 1st 256 samples).
  V2.1.3 ??
  V2.2.3 14/8/01 ... Dwell times summary table added (SingleChanAnal)
                     Single open times histogram now works (SingleChanAnal)
                     SealTest modified to prevent FP errors when test pulse is small
  V2.2.4 11/10/01 .. Error which prevented selection of No Lab Interface (16 bit) option
                     now fixed (SESLabIO)
                 ... All points in State amplitude histogram fixed (List Index Out or range error)
  V2.2.5 24/10/01 ... .EDE single-channel event record file now compatible
                      with earlier versions
                      Record To Disk (not working since V2.2.2) now fixed
                      (change to dd1320.pas in components)
    11/12/01 ... Stimulator now turned off when record form closed or de-activated (rec.pas)
  V2.2.6 8/1/02  ... now supports D/A output on PCI-6035E correctly
                     Changes to zero level in single-channel module now reflected in display module
                     9/1/02 ... (V2.2.6) ExportToASCII added
  V2.3.0 25/2/02 ... Updated Detect Events module added
  V2.3.1 9/4/02 .... RH.ADCVoltageRange now correctly takes account of
                     CDRFH.ADCAmplifierGain (detsigs.pas, eventdetector.pas)
         14/4/02 ... MatchTemplate detection criterion now aligned with peak signal
                     Export Gaps to EDR added to Detect Events  module
  V2.3.3 29.5.02 ... Support for N.I boards without D/A added
                     Selection of event detection channel in
                     multi-channel files now works correctly.
         6.6.02 .... Holding voltage increment now works (rec.pas)
  V2.3.4 30.7.02 ... Changes to detected event list now saved to file when form closed (eventdetector.pas)
         31.7.02 ... Occasional misplacement of inserted events fixed (eventdetector.pas)
  V2.3.6 17.12.02 ... Now tested with ITC-18 and ITC-18
                      Event detect module updated
  V2.3.7 31.01.03 ... Now works with ITC-16 and ITC-18 (old and new drivers)
         13.02.03 ... File name in command string can now be opened when WinEDR started
  V2.3.8 27.02.03 ... Page View can now compress digitised signals within line
                      allowing lines of any duration
  V2.3.9 8.04.03 .... Support for CED 1401s modified to have 10V A/D and D/As added
                      Failure to complete SealTest sweeo when A/D input at upper limit
                      now fixed.
                      Support for PCI-6014E added
  V2.4 29.05.03 ..... Amplifier gain now updated as soon as CED 1902 gain changed (ced1902u.pas)
  V2.4.1 12.06.03 ... Bug: Incorrect display of detection criteria trace when more than
                      one channel fixed (eventdetector.pas)
                      Display now updated automatically when channel changed (pageview.pas)
  V2.4.2 25.06.03 ... View Records display updated
         3.7.03 ..... ADCVoltageRange now picked up correctly (sealtest.pas)
         8.4.03 ..... NumEvents now restricted to limits of Events array
  V2.4.3 25/8/3 ..... Both \1401 and \1401\utils now checked for 1401 commands
                      (Sealtext.pas) Auto-display values now different for each channel
                      (ampmodule.pas) CED 1902 queried after each command sent
                      (fixes failure to set AC coupling with 29xx s/n 1902s)
                      Old Detect Signals module removed from program
  V2.4.4 7/9/3 .....  Floating point error in sealtest.pas fixed
  V2.4.5 21/1/4 .... Support for DaqCard 6024E added
  V2.4.5 19/2/4 .... Bug in UpdateChannelScaling which used
                     SESLabIO.ADCMaxValue instead of MaxADCValue fixed
  V2.4.6 3/3/4 ..... Whole record printout in PageView now works correctly
  11.3.04 ... Seal Test Floating point errors now trapped, avoiding message box.
  V2.4.8. 14/4/4 ... VP500 support added
                     Data files now have index numbers appended when new file created
  V2.4.9  09/08/04   Support for Multiclamp 700 A&B added
                     External Trigger now support Active High and Low polarity
  V2.5.0  13/09/04   Record module, Update Gain button added allowing user to force amp. gain update.
  V2.5.1  14/09/04   Capacitance calculation fixed (no memory violation errors)
  V2.5.2  19/12/04   Amplitude histogram ranges now set to min-max of data
                     Seal test no longer needs DAC 1 -> Ext. Trigger connection
                     VP500 Junction Potential can now be adjusted with arrow buttons
  V2.5.3  23/01/05   Baseline of detected events can now be plotted (Detect Events)
                     Single-channel current stability plots block duration can now be specified
                     No. of channels can be specified in single-channel current open probability  
  V2.5.4  09/03/05   Stimulus pulses now work again (CED 1401 & National Instruments)
  V2.5.5  15/04/05   Amplifier gain reading now works again with Digidata 1320 interface
                     National Instruments M Series boards now supported
  V2.5.6. 10/05/05   Channel calibration factor no longer changes when
                     Setup/Recording opened with a file
                     previously recorded using a different amplifier
  V2.5.7  16/05/05   A/D sampling now works again with Digidata 1200 (rec.pas)
  V2.5.8  18/07/05   Support for NPI SEC 05LX switch clamp added
                     Import/export to Strathclyde Chart added
                     Import from WAV files added
                     Import from ASCII files improved
  V2.6.1  01/08/05   NPI SEC 05LX gain now works correctly
  V2.6.3
  V2.6.4  19/10/05   Now works with CED 1401-plus again
  V2.6.5  16.01.06   Support for Digidata 1200 under Windows NT/2000/XP added
                     Baseline trends can now be subtracted from detected event waveforms
  V2.6.6  12/04/06   Support for Dagan 3900A added
  V2.6.7  09/06/06   Single-channel current stability plot added
                     Bug which caused loss of a/d samples when display resized
                      fixed
  V2.6.8 07.06.06 ... Bug which limited export event range to 1-1 fixed (eventdetector.pas)
  V2.6.9 13.08.06 ... Count-matched (selected n larged events) averaging option added (eventdetector.pas)
  V2.7.0 03.09.06 ... Suppport for Cairn Optopatch mode telegraph added
  V2.7.1 11.09.06
  V2.7.4 14.11.06
  V2.7.5 22.11.06 ... Decay time measurement % decay in event detection module can
                      be set by user.
                      Excluded margins in single-channel amplitude measurement now a
                      applied consistently throughout single-channel module. Manual
                      cursor-determined amplitudes and durations can now be exported
                      to file. Exported ABF files can now be filtered using CLAMPFIT V9
  V2.7.6 27.11.06 ... Linear fit added to Detected Events average
                      Event re-analysis now required after external event list loaded
                      Event list file now updated after events are delected or added
  V2.7.7 03.12.06 ... Digidata 1200 installation for Windows 2000 now works
  V2.7.8 12.12.06 ... Export to IGOR IBW files now works
                      Single-channel cursors measurements now plottable in
                      stability plots and amplitude histogram.
                      St. Dev. criterion added to Set Block function
  V2.7.9 02.02.07 ... Peak detection within detected event waveform analysis
                      can now be restricted to a-a cursor defined analysis area
                      eventdetector.pas
                      Imported data from WCP files now scaled correctly (adcdatafile.pas)
                      Channels with 0 points in CFS files now ignored (adcdatafile.pas)
                      Display grid now turned on/off more reliably
  V2.8.0 08.03.07 ... (EventDetector.pas) T.(x%) decay can now be relative to peak, mid-point of rise, or a0 cursor
                      Zero level can be at start of record or event
                      Decaying exponential fits added
                      Scaling factors of detected event exported to WCP files now correct
  V2.8.1 06.04.07 ... (sealtest.pas) Amplifier gain now set and stored correctly
                      when Amplifier= None or Manual
                      Now works with USB 6514 (digital I/O not supported errors ignored)
                      Incorrect sample at end of seal test with ITC16/18 fixed
  V2.8.2 17.04.07 ... ITC16/18 ncorrect sample at end of seal test with ITC16/18 fixed
                      for new drivers as well as old
  V2.8.3 05.06.07 ... New stimulus waveform generator
  V2.8.5 21.08.07 ... No. of points averaged to compute baseline
                      in EventDetector can now be changed
  V2.8.6 29.08.07 ... Data file clusters no longer pre-allocated before recording (Rec.pas)
                      Maximum number of ADC/DAC points now 1048576 with NIDAQ-MX interfaces
                      No. of increments in waveform protocols no longer changed when
                      single steps editted
  V2.8.7 04.09.07 ... Digidata 1320 Series no longer calibrates interface on startup
  V2.8.8 05.09.07 ... EDR files with spaces in them can now be opened by double clicking
                      on the data file icon. "Unable to create log file" warning no longer
                      occurs when more than one copy of WinEDR running.
  V2.8.9 07.10.07 ... Within-burst open probability now computed correctly (SingleCHanAnal.pas)
                      Detection criterion signal now limited to min/max A/D values
                      preventing wrap-round on overflow (EventDetector.pas)
  V2.9.0 11.03.08 ... Support for CED Power 1401 Mk2 added
                      Amplifier support for Axoclamp 2 and Dagan TEV200A added
                      Real-time event frequency graph added
  V2.9.1 28.04.08 ... Channel scaling factors now correct when importing
                      from Axon ABF V1.8 files.
  V2.9.2 01.05.08 ... Memory limit violation errors in event detection module
                      when display window duration increased fixed
  V2.9.3 10.08.08 ... Signal display windows improved.
                      Y axis ticks now now 1,2, or 5 step units
                      Individual channels can hidden. Y axis size of each channel can
                      be adjusted.
                      Bug in CFS import which caused repetition of data fixed
  V2.9.4 16.07.08 ... Gain telegraph channels in setup box now updated correctly
                      with selected amplifier (V2.9.3 problem)
                      Holding voltage now updated correctly when switching between
                      seal test and recording window with Digidata 132X series interfaces
  V2.9.5 25.07.08 ... Bug which caused incorrect storage of A/D resolution in EDR
                      file during recording fixed.
  V2.9.6 05.08.08 ... Support for Digidata 1440 added
  V2.9.7 19.08.08 ... Error which caused incorrect A/D sampling interval being reported
                  (possibly introduced WinEDR V2.9.4-6) with NIDAQ-MX
                  when sampling intervals shorter than board limits selected fixed.
  V2.9.8 20.08.08 ... Min. sampling/update interval for 6221/6229 boards now 1E-6s.
                      Event Detector: Buffer overflow error when exporting large events fixed
                      Exported event record limited to 65536 sample points per channel
  V2.9.9 12.09.08 CED 1401 (10V added) interface option added
                   NPI Turbo-Tec03X gain scaling now correct
  V3.0.0 18.09.08  Out of Memory error when files exported now fixed
  V3.0.1 26.09.08 Digidata 1440 interface now detected correctly
  V3.0.2 16.12.08 Cell resistance measurement plot added to recording window
                  Size of user-defined waveform stimuli no longer limited to 1024 points
                  and can now be loaded from a file.
                  Event detector list size increased to 1000000
  V3.0.3 09.02.09 File ident line now changeable in Record and Edit Markers / Line box
                  not view windows. Log file cannot be edited within WinEDR
                  Notes can be added to in log window.
                  Size of EDR.INI increased to accommodate large file paths
  V3.0.4 23.03.09 Min. display window duration reduced to 50ms
  V3.0.5 14.04.09 Event export bug where first record was duplicated in all exported records fixed
  V3.0.7 13.07.09 Compensation for Rs & Cslow cancellation added to capacity measurement
                  Greal & Gimag can be inverted, Tested with Scitec 420 lock-in amplifier
                  Mixing of multiple channels in first block of low pass filter fixed
                  Backup of raw data file when filtering no longer produces file error warning.
  V3.0.8 29.07.09 Dagan PCOne gain telegraph now read correctly over full gain range
  V3.0.9 03.09.09 CFS files can now exported.
                  CFS files with 4 byte int, 4 & 8 byte real data can be imported
  V3.1.0 14.09.09 Capacity measurement: Stored GReal and GImag data now inverted when inversion options selected
                  Notch filter now works correctly at centre frequencies which are
                  a small fraction of sampling frequency.
  V3.1.1 12.10.10 16 A/D channels now supported
                  No. bins, bin widths and ranges can be set for histograms
                  Support for Axoclamp 2 HS1, HS10 and HS0.1 headstages added
   V3.1.2 27.01.10 Access violation when event detector opened with data files containing
                   more than one channel fixed.
   V3.1.3 15.02.10 Access violations when Subt. Baseline Trend selected fixed. (eventdetector.pas)
                   Detected events now aligned at mid-point of rising phase
                   Size of rising edge detection window can be set by user
   V3.1.4 08.03.10 Average frequency plot now available as Frequency (avg) variable
                   in addition to instantaneous Frequency (Inst)
                   Average event frequency now correctly calculated over
                   specified range of counting intervals.
                   Real time resistance parameters now preserved in INI file
   V3.1.5 17.06.10 SIPBS Wireless EEG recorder now supported
                   Events exported from event detector in WCP V9.0 format
   V3.1.6 13.07.10 NI board device number can now be selected
                   PCI-61XX cards now work.
   V3.1.7 27.08.10 Spurious -10V pulse between seal test steps with Digidata 1440A fixed
                   Single channel analysis updated. Unit current now set using unit-c cursor.
                   Analysis sub-range settings now stored in EDR file.
   V3.1.8 08.12.10 Inter-cursor average can now be computed in View Recordings window
   V3.1.9 10.12.10 WinEDR.exe manifest now set to request upgrade to administrator
                   when run under Windows Vista or 7.
   V3.2.0 07.07.11 Export of events from event detector to WCP now scaled correctly
                   Max/min sampling intervals of NI cards now obtained from
                   device information when NIDAQmx supports it.
   V3.2.1 09.02.11 BUG FIX. Correct D/A voltage range now reported by .DACVoltageRange property
                     (No longer reports A/D voltage range)
                   device information when NIDAQmx supports it.
   V3.2.2 02.03.11 Support for USB 6008/9 added
                      National Instruments cards now traps errors when
                      no hardware is available for device number or
                      A/D input mode is not supported
   V3.2.3 20.04.11 ... SingleCHanANal.pas
              No. bins, bin width and bin range now all updated when any one changed,
               fixing bug where histograms were plotted using incorrect bin width when
               width was set below lower limit (lower limit also now set to amplitude resolution
               Max. no. of bins in histogram increased to 4000.
               Initial guesses for gaussian fits improved
               Stability plots now only plot complete averaging blocks.
  V3.2.4 10.05.11 AM Systems 2400 Current/voltage clamp mode telegraph now supported
  V3.2.5 24.06.11 Wireless DBS/EEG transceiver support updated. DBS control panel added
  V3.2.6 04.07.11 Error in amplitude scaling of imported V1.8+ ABF files with telegraphs
                  turned off fixed. (Bug reported by Trevor Smart)
                  Wireless DBS/EEG unit now has stimulus control panel
  V3.2.8 21.08.12 New stimulus editor (now uses XML stimulus protocol files like WinWCP)
                  Current stimuli supported. New Amplifier dialog.
                  Tecella PICO now supported.
                  Default graphs tick bar now integer steps
                  Some bugs fixed in single-channel analysis
                  Zero level now correct in ASCII text file exports
  V3.2.9 27.08.12 ITC-16/18 ITCMM_WriteDACsAndDigitalPort and ITCMM_ReadADC now disabled when ADCActive = TRUE
  V3.3.0 28.08.12 Fixed Zero Level option and Zero Level pop-up added to Record to Disk, Noise Analysis, Page View windows.
                  Fixed Zero Level option added to compressed view window.
  V3.3.1 18.9.12  EPC-800 command voltage scaling factor now 0.1 (rather than 0.02)
                  CED Micro 1401 Mk3 now recognised and no. of A/D and D/A channel set correctly
  V3.3.2 09.11.12 Noise Analysis module updated. (Record Overlap option now works.Record time added to Records panel
                  Absolute time now show on display.Display no longer goes into loop setting zero levels AC=DC channel)
  V3.3.3 15.02.13 Stimulator support added for Digidata 1440A interface
                  sealtest.pas Page showing Rm,Ra,Cm now displayed as alternative to Gm,Ga,Cm
                  Resistance/cell membrane parameters smoothing factor can now be set by user
  V3.3.4 11.03.13 FP error on import of .CHT files fixed
  V3.3.5 02.04.13 Free Run / External trigger setting now remembered when record form closed (rec.pas)
                  Invalid amplifier types now set to None when loaded from XML file and also when GetModelName called.
  V3.3.6 30.04.13 simmepc.pas Post synaptic current simulation now supports fixed intervals between events
                  and settings are saved in INI file.
   V3.3.7 15.05.13 simmepc.pas Binomial release probability and depression/facilitation added to fixed frequency trains
   V3.3.8 07.06.13 simmepc.pas Fixed frequency variation now present from start of trains.
   V3.3.9 06.11.13 A/D input channels can now be mapped to different physical inputs
                   for CED 1401s, Digidatas 1320 and 1440 and ITC-16 and ITC-18
   V3.4.0 18.11.13 Stimulus support for Digidata 132X added
                   SetCurrentDir() now ensure file dialog box opens in correct directory
                   CED 1902 now recognised again (Amp#s range from 0 to 3)
   V3.4.1 20.11.13 External trigger support for Digidata 1440 updated
   V3.4.2 02.12.13 D/A update buffering updated on Digidata 132X and CED 1401 interfaces
                   Corruption of long protocols (~150s) fixed
   V3.4.3 12.02.14 StimModule: Train waveshape now generate correct number of repetitions (previous n+1)
                   SESLabIO: AD/DA buffer size increases the 16777216 samples
   V3.4.4 26.06.14 Detect Events SaveEventList() removed from Insert/Delete events
//                 to avoid Error writing event file when buttons pressed to fast on slow systems
//                 Now works with Heka ITC-18 drivers.
//                 edr.ini file now stored in C:\Users\Public\Documents\WinEDR
//                 stim folder now in C:\Users\Public\Documents\WinEDR
// V3.4.5 16.07.14 SaveEventList() now executed from .timer() every time Insert/Delete events pressed
//                 to avoid events insertions/deletions being lost when NewFile() function called
// V3.4.6 25.07.14 AmpModule.pas & TritonPanelUnit.pas settings now saved in Settings directory (C:\Users\Public\Documents\WinEDR)
// V3.4.7 14.08.14 Support for USB-6000-6005 added.
//                 Amplifier settings now stored correctly in C:\Users\Public\Documents\WinEDR
// V3.4.8 15.08.14 ABF V2 files can now be imported
                   Now works with 18 bit NI interfaces (PCI-6281)
                   Heka EPC-9/10 and NPI ELC03X amplifier support added.
   V3.4.9 18.09.14 Current scaling factor for Axoclamp 2 current command channel now recognised currently
                   Current and Voltage command channels can now be updated by user.
                   Amplifier settings no longer loaded from edr.ini file to avoid interference with
                   amplifier settings.xml
   V3.5.0 15.10.14 Heka ITC-18 supported updated
   V3.5.1 17.10.14 Settings.ADCVoltageRangeIndex no longer used to store selected A/D voltage range
                   A/D voltage range no longer reset to 10V when Record to Disk opened
   V3.5.2          SESLabIO.pas Digidata 1320 D/A update timing error fixed???
   V3.5.3 03.12.14 ImportFromDataFile() now allocates a single large buffer to import file record
   V3.5.4 08.12.14 Current output scaling to AO1 now correct when using Axoclamp 2.
                   Rec.pas UpdateAOHold added. AO Hold scaling and units can now handle separate voltage
                   and current stimulation channels (e.g. Axoclamp 2)
          12.12.14 Changes to stimulus protocol element parameters no longer lost
                   when a new element dropped on waveform palette
   V3.5.5 10.02.15 No. of available channels increased to 128.
                   Buffer size of Tecella amplifiers increased to 8 Mbyte
                   Matlab .MAT files added to list of export formats
   V3.5.6 27.04.15 Dynamic clamp control panel added
                   Tecella Manual corrections to patch clamp current gain factors can now be applied by placing list of factors in file
                   C:\Users\Public\Documents\SESLABIO\tecella gain correction table.txt
   V3.5.7 09.06.15 Multiple files can be exported. Format now selected from drop-down liat.
   V3.5.8 10.07.15 Support for Digidata 1550 and 1550A added.
                   Protocol Editor: Open Protocol now always opens in ..\WinEDR\stim stimulus protocol folder
   V3.5.9 13.07.15 Axdd1440.dll updated to V2.0.2.1 to work under 64 bit Windows.
                      DD1550.DLL compiled with MT rather than MT DLL run-time library
                      to allow it to load under 64 bit windows.
   V3.6.0 13.07.15 Digidata 1550: 64/32 bit version of wdapi1140.dll created when O/S detected
   V3.6.1 03.08.15 Plotting Records: Signal records now plotted on printers and copied to clipboard
                   correctly (fixes. bug introduced in V3.5.8).
   V3.6.2 10.08.15 Plotting Records: Compression of signal displays with large numbers of points
                   into Ymin-YMax pairs now handled by scopedisplay component.
                   Changes to rec.pas, viewsig.pas, pageview.pas
   V3.6.3 12.08.15 dd1440.pas: Now ensures 32/64 bit version of wdapi1140.dll is created
                   allowing dd1440.dll to load.
   V3.6.4 26.08.15 Optopatch secondary channel gain and units now set correctly
                   Export file name list no longer mangled
                   axdd1400.dll, axdd1550.dll and axdd1550A.dll and associated files now
                   copied to WinEDR settings folder from AxoScope or PCLAMP program folders.
   V3.6.5 16.10.15 scopedisplay: Data arrays larger than 10000 points copied now min/max compressed
                   to less than 20000 points when copied to clipboard
   V3.6.6 08.01.16 SESLABIO.pas: Now ensures FADCNumChannels > 1 when settings loaded by LoadFromXMLFile(). XML settings file
                   now checked for LABINTERFACESETTINGS record to avoid access violation if not present.
                   Digidata 1440,1550,1550A: Now loads DLLs from C:\Users\Public\Documents\SESLABIO rather than (WinEDR program folder).
                   Amplifiers: XML settings file now checked for AMPLIFIERSETTINGS record to avoid access violation if not present.
   V3.6.7 09.02.16 File->Open and File->Import: File name now set to blank to ensure that available files are displayed in openfile dialog box.
   V3.6.8 09.03.16 Multiclamp 700A/B support updated to match WinWCP V5.1.5
                   Seal test now has zap button
   V3.6.9 05.04.16 amplifer settings.xml now correctly stoted in'C:\Users\Public\Documents\WinEDR' rather
                   than 'C:\Users\Public\Documents\WinEDR'
   V3.7.0 13.07.16 4 byte packing added to end TDD1550_Protocol & TDD1550A_Protocol record to avoid 'Error writing to device' error
                   when external trigger selectef.
   V3.7.1 17.08.16 Axoclamp 2: Default current gains now correct (10,1,0.1 V/nA) for (HS10,1,0.1)
   V3.7.2 30.09.16 Spport for Digidata 1550B added.
   V3.7.3 07.12.16 Fluoescence ratio option now stored in INI file
   V3.7.4 09.01.17 Computed fluorescence ratio updated and bugs fixed (rec.pas)
                   Computed channel readout now works correctly. Ch.0. can now be used as ratio channel
                   ion concentration now computed correctly. Ratio channel name now changed to
                   reflect channels ratioed. Computed ion name added to concentration channe.
   V3.7.5 28.04.17 Errors when selecting NI PCI-6010 card now fixed
   V3.7.8 30.10.17 Amplifiers: Analog input for secondary channel can now be remapped between
                   voltage- and current- clamp modes for amplifiers which require a different signal output
                   from the amplifier connected to the secondary channel in each clamp mode (applies to Axopatch 200 and AMS-2400)
                   ImportFromDataFile(): Multiple files can now be selected for import to WCP file format.
                   HekaUnit.pas code changed to try to get it working with EPC-9 USB
                   NPI ELC-03XS gain telegraph support updated fixed voltage scaling bug. Now supports alternative secondary channel analogue inputs
                   LIH88driver.dll added to program folder
   V3.7.9 21.11.17 D/A update of NI USB-600X devices now limited to 500 Hz or slower to avoid intermittent 5s delays in .ADCStop calls.
   V3.8.0 25.11.17 Unsupported function error prevented for non-600X boards fixing problem introduced V3.7.9
   V3.8.l 10.01.18 Files->Import GetMem replaced by AllocMem in ADCDataFile.pas to ensure all buffers initialized to zero.
   V3.8.2 18.01.18 AMS-2400: Now reads voltage gain in current clamp mode correctly
   V3.8.3 22.02.18 CHT File Import in ADCDATAFILE.pas: A/D voltage range now imported correctly. Fixes negative A/D voltage scales factors which
                   caused files exported as ABF V1.6 to be misidentified as ABF V2.0 with scaling factors misread and corrupted data points at beginning
   V3.8.4 24.04.18 Stimulus Start On Record option added allowing stimulus protocols to be initiated automatically when Record button pressed
   V3.8.5 08.05.18 Tecella Triton support updated. Junction potential offset determined in voltage-clamp mode now subtracted from membrane potential in
                     current-clamp mode. Current stimulus bias current offset can now be added to set stimulus current to zero current-clamp mode.
   V3.8.6 16.05.18 Amplifiers: PrimaryScaleFactorX1, SecondayScaleFactorX1 now set in correct units by Multiclamp and therefore appears correctly in Amplifiers Setup.
   V3.8.7 24.09.18 Amplifiers: Heka EPC-800 current command output now allocated to a different analog out channel since this amplifier has separate voltage- and current-clamp command inputs.
   V3.8.8 17.06.19 Support for Tecella Pico updated to match WinWCP.
                   SealTest More reliable calculation and averaging of cell parameters, less affected by invalid pulse signals.
   V3.8.9 29.07.19 Now reads WCP file scan interval and A/D voltage range correctly when only one record in WCP file
                   Baseline tracking can now be disabled in event detector
                   Variance plot and power spectrum calculation now faster by maing progress reporting more efficient
                   Application window size and location now preserved in INI file.
   V3.9.0 28.08.19 Digidata 1440-1550B Axoscope program folder on 64 bit systems searched for DLLs now correct
                   Detect Events: detection criterion display vertical scaling no longer shifts trace off scale when zero level adjusted by user.
   V3.9.1 26.09.19 Event Detector: Baseline tracking enabled/disabled option now works correctly
                   Detection mode,thresholds and other settings now preserved in EDR data file header
   V3.9.2 16.03.21 EventDetector:
                   Detected events aligned by mid-point of rising, irrespective of detection method used
                   First window of signal now fully displayed when detection window opened
                   Template match tau rise and decay times now preserved in EDR file
                   Rising edge window field removed, no longer required
  V3.9.3  26.04.21 Amplifiers: WPI EVC-4000 epithelial tissue voltage clamp added
                   Event Detector: Export Analysis button which exports measurements to csv files added
  V3.9.4  05.05.21 Event Detector: Bug which caused event detection point to be slightly misaligned from mid-point of rise for multi-channel recordings fixed
  V3.9.5  03.06.21 Event Detector: Events can now be aligned by either max. rate of rise or mid-point of rise.
                   Save to File added to Edit menu, allowed displayed signals/plots to be saved to as CSV file
  V3.9.6  09.08.21 AmpModule.pas copied from WinWCP. Axoclamp 900A module updated (not tested yet with actual hardware
                   Sealtest.pas imported from WinWCP, Pulse rate now faster. Default holding potential for AO channels selected by tick now updated.
  V3.9.7  27.08.21 Support for Tecella Triton X (16 channel patch clamp) added
  V3.9.8  24.11.21 Support for CED Micro 1401 Mk4 added. Digital stimulus pulses now work in Record To Disk window
                   Stimulus protocol files now correctly listed in Save/Open dialog boxes of protocol editor
  V3.9.9  16.01.22 COM Automation controls now working correctly again
  V4.0.0  06.02.22 Event Detector; Misalignment of mid-point of rise no longer misaligned for high frequency events
                   Event alignment point can now be left at detection point
  V4.0.1  09.03.22 Event Detector: Set Ampl. = 4*SD button now works
  =======================================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, Global, FileIo, Shared,
  PrintRec, Rec, SealTest, ViewSig,
  About, Log, defset, ced1902u, simmepc, SingleChanAnal,
  simnoise, simchan, NoiseAnal, digfilt, SESLabIO, ComCtrls,
  ADCDataFile, math, strutils, recplotunit, UITYpes, Vcl.HtmlHelpViewer, shlobj, epc9panelunit, tritonpanelunit  ;

const
    AllForms = True ;
    AllExceptRecordFrm = False ;  
type
  TMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Edit: TMenuItem;
    View: TMenuItem;
    Help1: TMenuItem;
    mnNew: TMenuItem;
    mnOpen: TMenuItem;
    mnPrint: TMenuItem;
    mnPrintSetup: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    mnZoomIn: TMenuItem;
    mnZoomOutAll: TMenuItem;
    Setup: TMenuItem;
    mnCED1902: TMenuItem;
    Analysis: TMenuItem;
    mnClose: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    mnStimulusGenerator: TMenuItem;
    CopyData: TMenuItem;
    CopyImage: TMenuItem;
    Record1: TMenuItem;
    mnRecord: TMenuItem;
    mnSealTest: TMenuItem;
    Simulations1: TMenuItem;
    mnMEPCSim: TMenuItem;
    mnImport: TMenuItem;
    mnExport: TMenuItem;
    N2: TMenuItem;
    Contents: TMenuItem;
    About: TMenuItem;
    InspectLogFile: TMenuItem;
    Windows: TMenuItem;
    mnDefaultSettings: TMenuItem;
    mnAppend: TMenuItem;
    mnViewSig: TMenuItem;
    mNDummy: TMenuItem;
    mnNoiseAnal: TMenuItem;
    mnSimNoise: TMenuItem;
    mnSimChan: TMenuItem;
    mnSingleChanAnal: TMenuItem;
    N1: TMenuItem;
    mnDigitalFilter: TMenuItem;
    mnRestoreOriginal: TMenuItem;
    mnRecentFileSeparator: TMenuItem;
    mnRecentFile0: TMenuItem;
    mnRecentFile1: TMenuItem;
    mnRecentFile2: TMenuItem;
    mnRecentFile3: TMenuItem;
    mnDisplayGrid: TMenuItem;
    mnInvert: TMenuItem;
    mnDetectEvent2: TMenuItem;
    StatusBar: TStatusBar;
    mnShowHints: TMenuItem;
    mnPageView: TMenuItem;
    N3: TMenuItem;
    mnEditMarkers: TMenuItem;
    mnECGFrm: TMenuItem;
    mnVP500: TMenuItem;
    mnZoomInCh0: TMenuItem;
    mnZoomInCh1: TMenuItem;
    mnZoomInCh2: TMenuItem;
    mnZoomInCh3: TMenuItem;
    mnZoomInCh4: TMenuItem;
    mnZoomInCh5: TMenuItem;
    mnZoomInCh6: TMenuItem;
    mnZoomInCh7: TMenuItem;
    mnZoomInCh8: TMenuItem;
    mnZoomInCh9: TMenuItem;
    mnZoomOut: TMenuItem;
    mnZoomOutCh0: TMenuItem;
    mnZoomOutCh1: TMenuItem;
    mnZoomOutCh2: TMenuItem;
    mnZoomOutCh3: TMenuItem;
    mnZoomOutCh4: TMenuItem;
    mnZoomOutCh5: TMenuItem;
    mnZoomOutCh6: TMenuItem;
    mnZoomOutCh7: TMenuItem;
    mnZoomOutCh8: TMenuItem;
    mnZoomOutCh9: TMenuItem;
    mnFileProperties: TMenuItem;
    ImportFile: TADCDataFile;
    mnDBSPanel: TMenuItem;
    mnTriton: TMenuItem;
    mnLaboratoryInterface: TMenuItem;
    mnInputChannelsAndAmplifiers: TMenuItem;
    SESLabIO: TSESLabIO;
    mnEPC9Panel: TMenuItem;
    mnResetMulticlamp: TMenuItem;
    mnDClamp: TMenuItem;
    mnSaveDataToFile: TMenuItem;
    procedure mnOpenClick(Sender: TObject);
    procedure mnZoomInClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnZoomOutAllClick(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
    procedure mnPrintClick(Sender: TObject);
    procedure mnPrintSetupClick(Sender: TObject);
    procedure mnRecordClick(Sender: TObject);
    procedure mnNewClick(Sender: TObject);
    procedure mnStimulusGeneratorClick(Sender: TObject);
    procedure CopyDataClick(Sender: TObject);
    procedure mnSealTestClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure InspectLogFileClick(Sender: TObject);
    procedure mnImportClick(Sender: TObject);
    procedure CopyImageClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnDefaultSettingsClick(Sender: TObject);
    procedure mnCED1902Click(Sender: TObject);
    procedure ContentsClick(Sender: TObject);
    procedure SearchforHelpon1Click(Sender: TObject);
    procedure HowtouseHelp1Click(Sender: TObject);
    procedure mnAppendClick(Sender: TObject);
    procedure mnExportClick(Sender: TObject);
    procedure mnViewSigClick(Sender: TObject);
    procedure mnMEPCSimClick(Sender: TObject);
    procedure mnNoiseAnalClick(Sender: TObject);
    procedure mnSimNoiseClick(Sender: TObject);
    procedure mnSimChanClick(Sender: TObject);
    procedure mnSingleChanAnalClick(Sender: TObject);
    procedure mnDigitalFilterClick(Sender: TObject);
    procedure mnRestoreOriginalClick(Sender: TObject);
    procedure mnRecentFile0Click(Sender: TObject);
    procedure mnDisplayGridClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnInvertClick(Sender: TObject);
    procedure mnDetectEvent2Click(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure mnShowHintsClick(Sender: TObject);
    procedure mnPageViewClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure mnEditMarkersClick(Sender: TObject);
    procedure mnECGFrmClick(Sender: TObject);
    procedure mnVP500Click(Sender: TObject);
    procedure mnZoomOutClick(Sender: TObject);
    procedure mnZoomInCh0Click(Sender: TObject);
    procedure mnZoomOutCh0Click(Sender: TObject);
    procedure ViewClick(Sender: TObject);
    procedure mnFilePropertiesClick(Sender: TObject);
    procedure SetupClick(Sender: TObject);
    procedure mnDBSPanelClick(Sender: TObject);
    procedure mnTritonClick(Sender: TObject);
    procedure mnLaboratoryInterfaceClick(Sender: TObject);
    procedure mnInputChannelsAndAmplifiersClick(Sender: TObject);
    procedure mnEPC9Click(Sender: TObject);
    procedure mnResetMulticlampClick(Sender: TObject);
    procedure mnDClampClick(Sender: TObject);
    procedure mnSaveDataToFileClick(Sender: TObject);
  private
    { Private declarations }

    function ImportFromDataFile : Boolean ;
    function CreateIndexedFileName( FileName : String ) : String ;
    procedure AppendEDRFile( FileName : string ) ;
    procedure UpdateRecentFilesList ;


  public
    { Public declarations }
    //SetupChanged : Boolean ;
    Version : string ;
    ProgDirectory : string ;      // Program folder
    SettingsDirectory : String ;  // Settings folder
    SettingsFileName : String ;   // Settings file name
    DataDirectory : string ;
    VProtDirectory : string ;

   // Cell parameters
   RSeal : Single ; // Seal Resistance (Ohms)
   Gm : Single ;    // Cell membrane conductance (S)
   Ga : Single ;    // Pipette access conductance (S)
   Cm : Single ;    // Cell capacity (F)
   Vm : Single ;    // Cell membrane voltage (V)
   Im : Single ;    // Cell membrane current (A)

    procedure SetCopyMenu( EnableCopyData, EnableCopyImage : boolean ) ;
    procedure UpdateWindows ;
    procedure SetMenus ;
    procedure ClearTicksInViewMenu ;
    procedure CopyAndPrintMenus( CopyEnabled,PrintEnabled : Boolean ) ;
    procedure CloseFormsAndDataFile( CloseAllForms : Boolean ) ;
    procedure LoadDataFiles( FileName : string ) ;
    procedure CloseDataFile ;
    function CreateNewDataFile( var FHeader : TCDRFileHeader ) : Boolean ;
    procedure SetRecentFileItem( MenuItem : TMenuItem ; FileName : string ) ;
    procedure UpdateViewSig ;
    procedure UpdateMDIWindows ;
    procedure UpdateChannelScalingFactors ;
    procedure UpdateAllDisplays ;
    function FormExists( FormName : String ) : Boolean ;
    function GetSpecialFolder(const ASpecialFolderID: Integer): string;
function ShowTritonPanel : Boolean ;



    end;

var
  Main: TMain;


implementation

uses AmpModule, Invert, EventDetector, PageView, EditMarkers,
  Import, ImportASCIIUnit, maths , ECG, ImportRawUnit, VP500Panel,
  FilePropsUnit, DBSPanelUnit, LabInterfaceSetup,
  InputChannelSetup, EditProtocolUnit , DCLAMPUnit, exportUnit;

{$R *.DFM}

procedure TMain.FormShow(Sender: TObject);
{ ---------------------------------------------------------------
  Initialize global program settings and load last data file used
  ---------------------------------------------------------------}
var
   i,ch : Integer ;
   FileName : String ;
begin
     Version := 'V4.0.1' ;
     Caption := 'WinEDR - Strathclyde Electrophysiology Data Recorder ' + Version ;

      { Get directory which contains EDR program }
      Main.ProgDirectory := ExtractFilePath(ParamStr(0)) ;
      Application.HelpFile := Main.ProgDirectory + 'WinEDR.chm';

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
     CdrFH.NumBytesInHeader := NumBytesInHeader ;
     CdrFH.ADCVoltageRange := 5.0 ;
     CdrFH.dt := 0.001 ;
     CdrFH.Version := 6.1 ;
     for ch := 0 to EDRChannelLimit do begin
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
     Settings.EventDetector.TDecayPercent := 90.0 ;
     Settings.EventDetector.TDecayFrom := 0 ;
     Settings.EventDetector.RisingEdgeWindow := 20 ;
     Settings.EventDetector.AvgFrequencyInterval := 1.0 ;
     Settings.EventDetector.EnableBaselineTracking := True ;
     Settings.EventDetector.Alignment := 0 ;

     Settings.DwellTimes.ChanNum := 0 ;
     Settings.DwellTimes.RecordSize := 512 ;
     Settings.DwellTimes.Threshold := 0.5 ;

     Settings.NumTriggerSweeps := 1 ;
     Settings.RecordDuration := 10. ;
     Settings.DisplayDuration := 10.0 ;
     Settings.DisplayGrid := True ;
     mnDisplayGrid.Checked := Settings.DisplayGrid ;
     
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

     // Create settings directory
     SettingsDirectory := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS) + '\WinEDR\';
     if not SysUtils.DirectoryExists(SettingsDirectory) then begin
        if SysUtils.ForceDirectories(SettingsDirectory) then
           WriteToLogFile( 'Settings folder ' + SettingsDirectory + ' created.')
        else WriteToLogFile( 'Unable to create settings folder' + SettingsDirectory) ;
        end ;
     SettingsFileName := SettingsDirectory + 'winedr.ini' ;

     // Stimulus protocols folder
     Main.VProtDirectory := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS) + '\WinEDR\stim\';
      if not SysUtils.DirectoryExists(Main.VProtDirectory) then begin
         if SysUtils.ForceDirectories(Main.VProtDirectory) then
            WriteToLogFile( 'Protocols folder ' + Main.VProtDirectory + ' created.')
         else WriteToLogFile( 'Unable to create protocols folder' + Main.VProtDirectory) ;
         end ;

      // Create default data directory (in My Documents)
      Main.DataDirectory := GetSpecialFolder(CSIDL_PERSONAL) + '\WinEDR Data\';
      if not SysUtils.DirectoryExists(Main.DataDirectory) then begin
         if SysUtils.ForceDirectories(Main.DataDirectory) then
            WriteToLogFile( 'Data folder ' + Main.DataDirectory + ' created.')
         else WriteToLogFile( 'Unable to create data folder' + Main.DataDirectory) ;
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

     Main.DataDirectory := '' ;

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

     SetMenus ;

     // Load initialization file to get name of last data file used }
     LoadInitializationFile( SettingsFileName ) ;
     if Main.DataDirectory = '' then
        Main.DataDirectory := Main.GetSpecialFolder(CSIDL_PERSONAL) + '\WinEDR Data\';
     if Main.VProtDirectory = '' then
        Main.VProtDirectory := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS) + '\WinEDR\stim\';

     { Name of last used data file returned in fH.FileName }

     // Initialise laboratory interface hardware
//     SESLabIO.OpenLabInterface( Settings.LaboratoryInterface,
//                                Settings.DeviceNumber,
//                                Settings.ADCInputMode ) ;

     // Set A/D input mode
//     SESLabIO.ADCInputMode := Settings.ADCInputMode ;

//     SESLabIO.ADCVoltageRange := CDRFH.ADCVoltageRange ;

     WindowMenu := Windows ;

     { Add names of recently accessed data files to Files menu }
     mnRecentFileSeparator.Visible := False ;
     SetRecentFileItem( mnRecentFile0, Settings.RecentFiles[0] ) ;
     SetRecentFileItem( mnRecentFile1, Settings.RecentFiles[1] ) ;
     SetRecentFileItem( mnRecentFile2, Settings.RecentFiles[2] ) ;
     SetRecentFileItem( mnRecentFile3, Settings.RecentFiles[3] ) ;

     { Open log file (contains log of program activity) }
     OpenLogFile ;
     WriteToLogFile( 'EDR Started' ) ;

     // Set channel display limits to whole ADC data range
     for ch := 0 to EDRChannelLimit do begin
         Channel[ch].xMin := 0. ;
         Channel[ch].xMax := CdrFH.NumSamples-1 ;
         Channel[ch].Cursor1 := CdrFH.NumSamples div 2 ;
         Channel[ch].yMin := SESLabIO.ADCMinValue ;
         Channel[ch].yMax := SESLabIO.ADCMaxValue ;
         end ;

     { Open a data file if one has been supplied in parameter string }
     FileName :=  '' ;
     for i := 1 to ParamCount do begin
         if i > 1 then FileName := FileName + ' ' ;
         FileName := FileName + ParamStr(i) ;
         end ;

     if ANSIContainsText( ExtractFileExt(FileName),'.edr') then begin
        if FileExists(FileName) then begin
           if (FileGetAttr(FileName) AND faReadOnly) = 0 then LoadDataFiles(FileName)
           else ShowMessage( FileName + ' is READ-ONLY. Unable to open!') ;

           end ;
        end ;

     end;


procedure TMain.SetRecentFileItem(
          MenuItem : TMenuItem ;    { Menu item to be updated }
          FileName : string         { Name of data file to be added }
          ) ;
{ ------------------------------------------------------
  Add the name of a recently used data file to File menu
  ------------------------------------------------------ }
begin
     if FileName <> '' then begin
        mnRecentFileSeparator.Visible := True ;
        MenuItem.Caption := FileName ;
        MenuItem.Visible := True ;
        end
     Else MenuItem.Visible := False ;
     end ;


procedure TMain.mnOpenClick(Sender: TObject);
{ -Menu item --------------------
  Open an existing EDR data file
  -------------------------------}
begin

     { Close existing data file }
     CloseFormsAndDataFile( AllForms ) ;

     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := DataFileExtension ;
     if Main.DataDirectory <> '' then begin
        OpenDialog.InitialDir := Main.DataDirectory ;
        OpenDialog.FileName := '' ;
        end ;
     OpenDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     OpenDialog.Title := 'Open File ' ;

     if OpenDialog.execute then begin
        CdrFH.FileName := OpenDialog.FileName ;
        LoadDataFiles( CdrFH.FileName ) ;

        { Save data directory }
        Main.DataDirectory := ExtractFilePath( CdrFH.FileName ) ;

        // Update list of recently used files
        UpdateRecentFilesList ;

        end ;

     SetMenus ;
     end;


procedure TMain.LoadDataFiles( FileName : string ) ;
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
            GetCDRHeader( CdrFH ) ;
            { Make sure all channels are visible }
            for ch := 0 to CdrFH.NumChannels-1 do Channel[ch].InUse := True ;
            WriteToLogFile( CdrFH.FileName + ' opened' ) ;
            // Update open windows with new file info.
            UpdateMDIWindows ;
            end
        else begin
             ShowMessage( format(' File Error =%d',[CdrFH.Filehandle]) ) ;
             OK := False ;
             end ;
        end ;

     // Close windows if file could not be opened
     if not OK then CloseFormsAndDataFile( AllForms ) ;

     SetMenus ;
     end ;


function TMain.CreateNewDataFile(
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
     if FHeader.FileHandle >= 0 then begin
        FileClose( FHeader.FileHandle ) ;
        FHeader.FileHandle := -1 ;
        end ;

     { Create a new file }
     FHeader.FileHandle := FileCreate( FHeader.FileName ) ;
     if FHeader.FileHandle >= 0 then begin

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
        SaveCDRHeader( FHeader ) ;
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

     Caption := 'File: ' + CDRFH.FileName ;
     end ;


procedure TMain.mnZoomInClick(Sender: TObject);
{ ---------------------
  Display zoom in menu
  --------------------- }
var
    i,ch : Integer ;
begin

     for i := 0 to mnZoomIn.Count-1 do mnZoomIn.Items[i].Visible := False ;
     if MDIChildCount <= 0 then Exit ;

     if ActiveMDIChild.Name = 'RecordFrm' then begin
        // Use Recording channels if recording form is active
        for ch := 0 to Settings.NumChannels-1 do begin
            mnZoomIn.Items[ch].Caption := format( 'Ch.%d (%s)',
                                         [ch,Main.SESLabIO.ADCChannelName[ch]]) ;
            mnZoomIn.Items[ch].Tag := ch ;
            mnZoomIn.Items[ch].Visible := True ;
            end ;
         end
     else begin
        // Otherwise use file channels
        for ch := 0 to CDRFH.NumChannels-1 do begin
            mnZoomIn.Items[ch].Caption := format( 'Ch.%d (%s)',
                                         [ch,Channel[ch].ADCName]) ;
            mnZoomIn.Items[ch].Tag := ch ;
            mnZoomIn.Items[ch].Visible := True ;
            end ;
        end ;

     end;


procedure TMain.Exit1Click(Sender: TObject);
{ --------------------------------
  Save state and terminate program
  -------------------------------- }
begin
     close ;
     end;


procedure TMain.FormDestroy(Sender: TObject);
begin
     RecordTypes.Free ;
     ChannelNames.Free ;
     end;


procedure TMain.mnZoomOutAllClick(Sender: TObject);
{ - Menu Item ---------------------------------
  Set display channels to minimum magnification
  ---------------------------------------------}
var
   i,ch : Integer ;
begin

     if MDIChildCount <= 0 then Exit ;

     for ch := 0 to EDRChannelLimit do begin
         Channel[ch].yMin := -Channel[ch].ADCMaxValue -1 ;
         Channel[ch].yMax := Channel[ch].ADCMaxValue ;
         Main.SESLabIO.ADCChannelYMin[ch] := -Main.SESLabIO.ADCMaxValue -1 ;
         Main.SESLabIO.ADCChannelYMax[ch] := Main.SESLabIO.ADCMaxValue ;
         end ;

     for i := 0 to MDIChildCount-1 do begin
          if MDIChildren[I].Name = 'SimMEPCFrm' then
             TSimMEPCFrm(MDIChildren[I]).ZoomOut
          else if MDIChildren[I].Name = 'SimNoiseFrm' then
             TSimNoiseFrm(MDIChildren[I]).ZoomOut
          else if MDIChildren[I].Name = 'SimChanFrm' then
             TSimChanFrm(MDIChildren[I]).ZoomOut
          else if MDIChildren[I].Name = 'NoiseAnal' then
             TNoiseAnalFrm(MDIChildren[I]).ZoomOutAll
          else if MDIChildren[I].Name = 'SingleChanAnalFrm' then
             TSingleChanAnalFrm(MDIChildren[I]).ZoomOutAll
          else if MDIChildren[I].Name = 'RecordFrm' then
             TRecordFrm(MDIChildren[I]).ZoomOutAll
          else if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ZoomOutAll
          else if MDIChildren[I].Name = 'SealTestFrm' then
             TSealTestFrm(MDIChildren[I]).ZoomOut
          else if MDIChildren[I].Name = 'EventDetFrm' then
             TEventDetFrm(MDIChildren[I]).ZoomOutAll ;
          end ;
    end;


procedure TMain.mnCloseClick(Sender: TObject);
{ -------------------------
  Close all forms and files
  -------------------------}
begin
     { Close all child windows and data files }
     CloseFormsAndDataFile( AllForms ) ;
     CdrFH.FileName := '' ;
     CdrFH.FileHandle := -1 ;

     end ;


procedure TMain.CloseFormsAndDataFile( CloseAllForms : Boolean ) ;
{ -------------------------
  Close all forms and files
  -------------------------}
var
   i : Integer ;
begin

     { Close all child windows }
     for i := 0 to MDIChildCount-1 do begin
         if CloseAllForms or
            ((MDICHildren[i].Name <> 'RecordFrm') and
             (MDICHildren[i].Name <> 'RecPlotFrm') and
             (MDICHildren[i].Name <> 'TritonPanelFrm')) then MDICHildren[i].Close
         end ;

     { Close files }
     CloseDataFile ;

     { Allow form close messages to be processed before continuing }
     Application.ProcessMessages ;

     SetMenus ;

     Caption := 'WinEDR - Strathclyde Electrophysiology Data Recorder ' + Version ;

     end ;


procedure TMain.CloseDataFile ;
// --------------------------------
// Close data file (if one is open)
// --------------------------------
begin
     if  CdrFH.FileHandle >= 0 then begin
         FileClose(CdrFH.FileHandle) ;
         CdrFH.FileHandle := -1 ;
         end ;
     end;


procedure TMain.UpdateRecentFilesList ;
// ----------------------------------
// Update list of recently used files
// ----------------------------------
var
    i : Integer ;
begin

     // If same file has been opened again, don't update
     if Settings.RecentFiles[0] = CDRFH.FileName then Exit ;

     // Shift list along
     for i := High(Settings.RecentFiles) downto 1 do
         Settings.RecentFiles[i] := Settings.RecentFiles[i-1] ;
     Settings.RecentFiles[0] := CDRFH.FileName ;

     { Update list in Files menu }
     mnRecentFileSeparator.Visible := False ;
     SetRecentFileItem( mnRecentFile0, Settings.RecentFiles[0] ) ;
     SetRecentFileItem( mnRecentFile1, Settings.RecentFiles[1] ) ;
     SetRecentFileItem( mnRecentFile2, Settings.RecentFiles[2] ) ;
     SetRecentFileItem( mnRecentFile3, Settings.RecentFiles[3] ) ;

     end ;


procedure TMain.mnPrintClick(Sender: TObject);
{ - Menu Item ----------------------------------------------------
  Print the record or graph displayed in the currently active form
  ----------------------------------------------------------------}
begin
     if ActiveMDIChild.Name = 'ViewSigFrm' then
        TViewSigFrm(ActiveMDIChild).PrintDisplay ;
     if ActiveMDIChild.Name = 'NoiseAnalFrm' then
        TNoiseAnalFrm(ActiveMDIChild).PrintDisplay ;
     if ActiveMDIChild.Name = 'SingleChanAnalFrm' then
        TSingleChanAnalFrm(ActiveMDIChild).PrintDisplay ;
     if ActiveMDIChild.Name = 'EventDetFrm' then
        TEventDetFrm(ActiveMDIChild).PrintDisplay ;
     if ActiveMDIChild.Name = 'PageViewFrm' then
        TPageViewFrm(ActiveMDIChild).PrintDisplay ;
     if ActiveMDIChild.Name = 'ECGFrm' then
        TPageViewFrm(ActiveMDIChild).PrintDisplay ;

     end;


procedure TMain.mnPrintSetupClick(Sender: TObject);
{ - Menu Item ------------------
  Standard printer setup dialog
  ------------------------------}
begin
     PrinterSetupDialog1.Execute ;
     end;


procedure TMain.mnRecordClick(Sender: TObject);
{ - Menu Item --------------
  Open record to disk module
  --------------------------}
begin
     if FormExists('RecordFrm') then begin
        if RecordFrm.WindowState = wsMinimized then RecordFrm.WindowState := wsNormal ;
        RecordFrm.BringToFront ;
        RecordFrm.SetFocus ;
        end
     else begin
        RecordFrm := TRecordFrm.Create(Self) ;
        RecordFrm.Left := 15 ;
        RecordFrm.Top := 15 ;
        RecordFrm.Height := ClientHeight - RecordFrm.Top - 30 ;
        RecordFrm.Width := Min( RecordFrm.Width,
                                 ClientWidth - RecordFrm.Left - 30 ) ;
        end ;

     end;


procedure TMain.mnNewClick(Sender: TObject);
{ - Menu Item --------
  Create new data file
  --------------------}
var
   OK : Boolean ;
begin

     { Close any existing data file }
     CloseFormsAndDataFile( AllExceptRecordFrm ) ;

     { Present user with standard Save File dialog box }
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := DataFileExtension ;
     SaveDialog.FileName := CreateIndexedFileName( Settings.RecentFiles[0] ) ;
     SaveDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     SaveDialog.Title := 'New Data File' ;

     if Main.DataDirectory <> '' then begin
        SaveDialog.InitialDir := Main.DataDirectory ;
        SetCurrentDir(Main.DataDirectory) ;
        end ;

     { Create new data file }
     if SaveDialog.execute then begin

        { Save data directory }
        Main.DataDirectory := ExtractFilePath( SaveDialog.FileName ) ;
        CdrFH.FileName := SaveDialog.FileName ;
        CdrFH.FileName := ChangeFileExt( CdrFH.FileName, DataFileExtension ) ;
        OK := CreateNewDataFile( CdrFH ) ;
        if OK then begin
           { Close again to permit re-opening by LoadDataFiles }
           CloseFormsAndDataFile( AllExceptRecordFrm ) ; ;
           WriteToLogFile( 'New file ' + CdrFH.FileName ) ;

           { Re-open data file }
           LoadDataFiles( CdrFH.FileName ) ;

           // Update recording window
           if FormExists('RecordFrm') then RecordFrm.NewFile ;

           // Update list of recently used files
           UpdateRecentFilesList ;

           end ;
        end ;
     end ;


procedure TMain.mnStimulusGeneratorClick(Sender: TObject);
//  ---------------------------------
// Open stimulus settings setup box
// ---------------------------------
begin

    if FormExists('EditProtocolFrm') then begin
       EditProtocolFrm.SetFocus ;
       end
    else begin
       EditProtocolFrm := TEditProtocolFrm.Create(Self) ;
       EditProtocolFrm.Left := 10 ;
       EditProtocolFrm.Top := 10 ;
       end ;
    end ;


procedure TMain.SetMenus ;
{ ---------------------------
  Activate usable menu items
  --------------------------}
begin
     View.Enabled := True ;
     Analysis.Enabled := False ;
     mnPrint.Enabled := False ;
     Edit.Enabled := True ;
     Setup.Enabled := True ;
     Record1.Enabled := True ;
     mnClose.Enabled := False ;
     mnAppend.Enabled := False ;
     mnFileProperties.Enabled := False ;
     mnExport.Enabled := False ;
     Simulations1.Enabled := False ;

     { Enable setup, recording and simulation ... if a data file is open }
     if CdrFH.FileHandle >= 0 then begin
           { Allow these functions when a data file is open }
           mnClose.Enabled := True ;
           mnFileProperties.Enabled := True ;
           Simulations1.Enabled := True ;
           Edit.Enabled := True ;
           mnAppend.Enabled := True ;

           { Enable these functions when file contains data records }
           if CdrFH.NumSamplesInFile > 0 then begin
              Analysis.Enabled := True ;
              mnPrint.Enabled := True ;
              mnExport.Enabled := True ;
              Edit.Enabled := True ;
              end ;

           end ;

     { If a back up is available, enabled restore menu option }
     mnRestoreOriginal.Enabled := CdrFH.BackedUp ;
     end ;


procedure TMain.SetCopyMenu( EnableCopyData, EnableCopyImage : boolean ) ;
{ --------------------------------------------------
  Enable/disable copy data and copy image menu items
  --------------------------------------------------}
begin
     CopyData.enabled := EnableCopyData ;
     CopyImage.enabled := EnableCopyImage ;
     end ;


procedure TMain.mnSaveDataToFileClick(Sender: TObject);
{ -----------------------------
  Copy data values to clipboard
  -----------------------------}
begin


//     if ActiveMDIChild.Name = 'SingleChanAnalFrm' then
//        TSingleChanAnalFrm(ActiveMDIChild).SaveDataToFile(FileName) ;
     if ActiveMDIChild.Name = 'ViewSigFrm' then TViewSigFrm(ActiveMDIChild).SaveDataToFile
      else if (ActiveMDIChild.Name = 'EventDetFrm') then TEventDetFrm(ActiveMDIChild).SaveDataToFile
      else if (ActiveMDIChild.Name = 'NoiseAnalFrm') then TNoiseAnalFrm(ActiveMDIChild).SaveDataToFile ;
//     if ActiveMDIChild.Name = 'ECGFrm' then
//        TECGFrm(ActiveMDIChild).SaveDataToFile(FileName) ;
//     if ActiveMDIChild.Name = 'RecPlotFrm' then
//        TRecPlotFrm(ActiveMDIChild).SaveDataToFile(FileName) ;

     end ;


procedure TMain.mnSealTestClick(Sender: TObject);
{ ----------------------------
  Menu - Open seal test module
  ----------------------------}
begin
     if FormExists('SealTestFrm') then begin
        if SealTestFrm.WindowState = wsMinimized then SealTestFrm.WindowState := wsNormal ;
        SealTestFrm.BringToFront ;
        SealTestFrm.SetFocus ;
        end
     else begin
        SealTestFrm := TSealTestFrm.Create(Self) ;
        SealTestFrm.Left := 15 ;
        SealTestFrm.Top := 15 ;
        end ;
     end;


procedure TMain.AboutClick(Sender: TObject);
begin
     AboutDlg := TAboutDlg.Create(Self) ;
     AboutDlg.ShowModal ;
     end;


procedure TMain.UpdateWindows ;
begin
    { ViewWCPChild.NewFile := True ;}
     end ;


procedure TMain.InspectLogFileClick(Sender: TObject);
begin
     LogFrm := TLogFrm.Create(Self) ;
     end;


procedure TMain.mnImportClick(Sender: TObject);
{ - Menu Item --------------------------------
  Import records from a foreign data file type
  --------------------------------------------}
begin

     CloseFormsAndDataFile( AllForms ) ;

     if ImportFromDataFile then begin ;
        { Open new data file }
        LoadDataFiles( CdrFH.FileName ) ;
        // Update list of recently used files
        UpdateRecentFilesList ;
        // Update windows
        UpdateMDIWindows ;
        SetMenus ;
        end ;

     end;


procedure TMain.CopyDataClick(Sender: TObject);
{ -----------------------------
  Copy data values to clipboard
  -----------------------------}
begin
     if ActiveMDIChild.Name = 'ViewSigFrm' then
        TViewSigFrm(ActiveMDIChild).scDisplay.CopyDataToClipBoard ;
     if ActiveMDIChild.Name = 'NoiseAnalFrm' then
        TNoiseAnalFrm(ActiveMDIChild).CopyDataToClipboard ;
     if ActiveMDIChild.Name = 'SingleChanAnalFrm' then
        TSingleChanAnalFrm(ActiveMDIChild).CopyDataToClipboard ;
     if ActiveMDIChild.Name = 'EventDetFrm' then
        TEventDetFrm(ActiveMDIChild).CopyDataToClipboard ;
     if ActiveMDIChild.Name = 'ECGFrm' then
        TECGFrm(ActiveMDIChild).CopyDataToClipboard ;
     if ActiveMDIChild.Name = 'RecPlotFrm' then
        TRecPlotFrm(ActiveMDIChild).CopyDataToClipboard ;

     end ;


procedure TMain.CopyImageClick(Sender: TObject);
{ ---------------------------------
  Copy displayed image to clipboard
  ---------------------------------}
begin
     if ActiveMDIChild.Name = 'ViewSigFrm' then
        TViewSigFrm(ActiveMDIChild).CopyImageToClipboard ;
     if ActiveMDIChild.Name = 'NoiseAnalFrm' then
        TNoiseAnalFrm(ActiveMDIChild).CopyImageToClipboard ;
     if ActiveMDIChild.Name = 'SingleChanAnalFrm' then
        TSingleChanAnalFrm(ActiveMDIChild).CopyImageToClipboard ;
     if ActiveMDIChild.Name = 'EventDetFrm' then
        TEventDetFrm(ActiveMDIChild).CopyImageToClipboard ;
     if ActiveMDIChild.Name = 'PageViewFrm' then
        TPageViewFrm(ActiveMDIChild).CopyImageToClipboard ;
     if ActiveMDIChild.Name = 'ECGFrm' then
        TECGFrm(ActiveMDIChild).CopyImageToClipboard ;


     end ;


procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
     if MessageDlg( 'Exit Program! Are you Sure? ', mtConfirmation,
        [mbYes,mbNo], 0 ) = mrYes then CanClose := True
                                  else CanClose := False ;
     end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin

        CloseFormsAndDataFile( AllForms ) ;

        { Close log file }
        CloseLogFile ;
        { Save initialization file }
        SaveInitializationFile( SettingsFileName ) ;

        // Dispose of marker list
        MarkerList.Free ;

        end;

{ **** MENU ITEMS **************************************************** }


procedure TMain.ClearTicksInViewMenu ;
begin
     {mnViewWCP.checked := false ;}
     end ;


procedure TMain.mnDefaultSettingsClick(Sender: TObject);
// --------------------------------
// Display default settings window
// --------------------------------
begin
     if FormExists('DefSetFrm') then begin
        if DefSetFrm.WindowState = wsMinimized then DefSetFrm.WindowState := wsNormal ;
        DefSetFrm.BringToFront ;
        DefSetFrm.SetFocus ;
        end
     else begin
        DefSetFrm := TDefSetFrm.Create(Self) ;
        DefSetFrm.Left := 10 ;
        DefSetFrm.Top := 10 ;
        end ;

     end;


procedure TMain.mnCED1902Click(Sender: TObject);
// ----------------------------------------
// Display CED 1902 amplifier control panel
// ----------------------------------------
begin
     if FormExists('CED1902Frm') then begin
        if CED1902Frm.WindowState = wsMinimized then CED1902Frm.WindowState := wsNormal ;
        CED1902Frm.BringToFront ;
        CED1902Frm.SetFocus ;
        end
     else begin
        CED1902Frm := TCED1902Frm.Create(Self) ;
        CED1902Frm.Left := 10 ;
        CED1902Frm.Top := 10 ;
        end ;

     end;


procedure TMain.ContentsClick(Sender: TObject);
// ---------------
// Show Help Files
// ---------------
begin
     application.HelpContext(10);
     end;


procedure TMain.SearchforHelpon1Click(Sender: TObject);
begin
     application.helpcommand( HELP_PARTIALKEY, 0 ) ;

     end;


procedure TMain.HowtouseHelp1Click(Sender: TObject);
begin
     application.helpcommand( HELP_HELPONHELP, 0 ) ;
     end;


procedure TMain.mnAppendClick(Sender: TObject);
{ - Menu Item -------------------------------------------
  Append records from another EDR data file (convert.pas)
  -------------------------------------------------------}
begin

     { Get the name of a data file from user }
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := DataFileExtension ;
     if Main.DataDirectory <> '' then begin
        OpenDialog.InitialDir := Main.DataDirectory ;
        SetCurrentDir(Main.DataDirectory) ;
        end ;
     OpenDialog.Filter := format( ' EDR Files (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     OpenDialog.Title := 'Append File ' ;

     if not OpenDialog.execute then Exit ;

     if OpenDialog.FileName = CdrFH.FileName then begin
        MessageDlg( 'APPEND: Can''t append file to itself',mtWarning, [mbOK], 0 ) ;
        Exit ;
        end ;

     AppendEDRFile( OpenDialog.FileName ) ;

     SetMenus ;
     end;


procedure TMain.mnExportClick(Sender: TObject);
{ - Menu Item --------------------------------
  Export records to a foreign data file type
  --------------------------------------------}
begin
     ExportFrm.ShowModal ;
     end ;


procedure TMain.mnViewSigClick(Sender: TObject);
{ --------------------------
  Open signal display window
  -------------------------- }
begin
     if FormExists('ViewSigFrm') then begin
        if ViewSigFrm.WindowState = wsMinimized then ViewSigFrm.WindowState := wsNormal ;
        ViewSigFrm.BringToFront ;
        ViewSigFrm.SetFocus ;
        end
     else begin
        ViewSigFrm := TViewSigFrm.Create(Self) ;
        ViewSigFrm.Left := 15 ;
        ViewSigFrm.Top := 15 ;
        ViewSigFrm.Height := ClientHeight - ViewSigFrm.Top - 10 ;
        ViewSigFrm.Width := Min( ViewSigFrm.Width,
                                 ClientWidth - ViewSigFrm.Left - 30 ) ;
        end ;
     end;


procedure TMain.mnMEPCSimClick(Sender: TObject);
{ ---------------------------
  Open MEPC simulation window
  --------------------------- }
begin
     if FormExists('simmepcfrm') then begin
        if simmepcfrm.WindowState = wsMinimized then simmepcfrm.WindowState := wsNormal ;
        simmepcfrm.BringToFront ;
        simmepcfrm.SetFocus ;
        end
     else begin
        simmepcfrm := Tsimmepcfrm.Create(Self) ;
        simmepcfrm.Left := 20 ;
        simmepcfrm.Top := 20 ;
        end ;
     end;


procedure TMain.mnNoiseAnalClick(Sender: TObject);
{ -----------------------------------
  Open variance/power spectrum window
  ----------------------------------- }
begin
     if FormExists('NoiseAnalFrm') then begin
        if NoiseAnalFrm.WindowState = wsMinimized then NoiseAnalFrm.WindowState := wsNormal ;
        NoiseAnalFrm.BringToFront ;
        NoiseAnalFrm.SetFocus ;
        end
     else begin
        NoiseAnalFrm := TNoiseAnalFrm.Create(Self) ;
        NoiseAnalFrm.Left := 20 ;
        NoiseAnalFrm.Top := 20 ;
        end ;
     end;


procedure TMain.mnSimNoiseClick(Sender: TObject);
{ ----------------------------
  Ion channel noise simulation
  ----------------------------}
begin
     if FormExists('SimNoiseFrm') then begin
        if SimNoiseFrm.WindowState = wsMinimized then SimNoiseFrm.WindowState := wsNormal ;
        SimNoiseFrm.BringToFront ;
        SimNoiseFrm.SetFocus ;
        end
     else begin
        SimNoiseFrm := TSimNoiseFrm.Create(Self) ;
        SimNoiseFrm.Left := 20 ;
        SimNoiseFrm.Top := 20 ;
        end ;
     end;


procedure TMain.CopyAndPrintMenus( CopyEnabled,PrintEnabled : Boolean ) ;
{ -------------------------------------------
  Enable/disable copying and printingf menus
  -------------------------------------------}
begin
     CopyData.Enabled := CopyEnabled ;
     CopyImage.Enabled := CopyEnabled  ;
     mnPrint.Enabled := PrintEnabled ;
     end ;


procedure TMain.mnSimChanClick(Sender: TObject);
{ ----------------------------------
  Single-channel currents simulation
  ----------------------------------}
begin
     if FormExists('SimChanFrm') then begin
        if SimChanFrm.WindowState = wsMinimized then SimChanFrm.WindowState := wsNormal ;
        SimChanFrm.BringToFront ;
        SimChanFrm.SetFocus ;
        end
     else begin
        SimChanFrm := TSimChanFrm.Create(Self) ;
        SimChanFrm.Left := 20 ;
        SimChanFrm.Top := 20 ;
        end ;
     end;


procedure TMain.mnSingleChanAnalClick(Sender: TObject);
{ -----------------------------------
  Open single-channel analysis window
  ----------------------------------- }
begin
     if FormExists('singlechananalfrm') then begin
        if singlechananalfrm.WindowState = wsMinimized then singlechananalfrm.WindowState := wsNormal ;
        singlechananalfrm.BringToFront ;
        singlechananalfrm.SetFocus ;
        end
     else begin
        singlechananalfrm := Tsinglechananalfrm.Create(Self) ;
        singlechananalfrm.Left := 20 ;
        singlechananalfrm.Top := 20 ;
        end ;
     end;


procedure TMain.mnDigitalFilterClick(Sender: TObject);
{ - Menu Item --------------------------------
  Open digital filter dialog form (digfilt.pas)
  --------------------------------------------}
begin
     DigFilterDlg.ShowModal ;
     if DigFilterDlg.ModalResult = mrOK then UpdateViewSig
     end;


procedure TMain.mnResetMulticlampClick(Sender: TObject);
// -------------------------------------------
// Reset communications with Multiclamp 700A/B
// -------------------------------------------
begin
    Amplifier.ResetMultiClamp700 ;
    end;


procedure TMain.mnRestoreOriginalClick(Sender: TObject);
{ - Menu Item --------------------------
  Restore orginal data from back up file
  -------------------------------------- }
begin
     Screen.Cursor := crHourGlass ;
     RestoreFromBackupFile ;
     Screen.Cursor := crDefault ;
     UpdateViewSig ;
     end ;


procedure TMain.UpdateViewSig ;
var
   i : Integer ;
begin
     { Update Signal display window if it is open }
     for i := 0 to MDIChildCount-1 do
         if MDIChildren[I].Name = 'ViewSigFrm' then TViewSigFrm(MDIChildren[I]).NewData ;
     end;


procedure TMain.UpdateMDIWindows ;
var
   i : Integer ;
   ViewSigExists : Boolean ;
begin

     SetMenus ;

     ViewSigExists := False ;
     for i := 0 to MDIChildCount-1 do begin
         if MDIChildren[I].Name = 'ViewSigFrm' then begin
            TViewSigFrm(MDIChildren[I]).DisplayFromFile ;
            ViewSigExists := True ;
            end ;
         end ;

     { If no replay form ... create one }
     if (CdrfH.NumSamplesInFile > 0) and (not ViewSigExists) then
        ViewSigFrm := TViewSigFrm.Create(Self) ;

     end;


procedure TMain.mnRecentFile0Click(Sender: TObject);
{ - Menu Item -------------------
  Load a recently used data file
  ------------------------------- }
var
   FileName : string ;
begin

     { Close existing data file }
     CloseFormsAndDataFile( AllForms ) ;
     
     { Get file name. Note, the .Tag property of the menu item was set
       at design time to point to the appropriate RecentFiles array item }
     FileName := Settings.RecentFiles[TMenuItem(Sender).Tag] ;
     if (FileName <> '') and FileExists(FileName) then begin

       { OldNumSamples := RawFH.NumSamples ;}

        LoadDataFiles(FileName) ;

        { Set display magnification to minimum }
{        if CdrFH.NumRecords > 0 then begin
           if OldNumSamples <> RawFH.NumSamples then mnZoomOut.Click ;
           mnShowRaw.Click ;
           end ;}
        SetMenus ;
        end ;

     end;



procedure TMain.mnDisplayGridClick(Sender: TObject);
{ -----------------------------------------------------
  Enable/display grid overlay on oscilloscope displays
  ----------------------------------------------------- }
var
   i : Integer ;
begin
     Settings.DisplayGrid := not Settings.DisplayGrid ;
     mnDisplayGrid.Checked := Settings.DisplayGrid ;
     for i := 0 to MDIChildCount-1 do begin
          if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SimNoiseFrm' then
             TSimNoiseFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SimChanFrm' then
             TSimChanFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'NoiseAnalFrm' then
             TNoiseAnalFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SingleChanAnalFrm' then
             TSingleChanAnalFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'RecordFrm' then
             TRecordFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SealTestFrm' then
             TSealTestFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'PageViewFrm' then
             TPageViewFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'EventDetFrm' then
             TEventDetFrm(MDIChildren[I]).ChangeDisplayGrid ;
          end ;

     end;


procedure TMain.UpdateAllDisplays ;
{ -----------------------------------------------
  Update all open forms when data file is changed
  ----------------------------------------------- }
var
   i : Integer ;
begin

     for i := 0 to MDIChildCount-1 do begin
          if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SimNoiseFrm' then
             TSimNoiseFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SimChanFrm' then
             TSimChanFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'NoiseAnalFrm' then
             TNoiseAnalFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SingleChanAnalFrm' then
             TSingleChanAnalFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'RecordFrm' then begin
             TRecordFrm(MDIChildren[I]).RestartADC ;
             TRecordFrm(MDIChildren[I]).ChangeDisplayGrid
             end
          else if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ChangeDisplayGrid
          else if MDIChildren[I].Name = 'SealTestFrm' then
             TSealTestFrm(MDIChildren[I]).ChangeDisplayGrid ;
          end ;
     end;

procedure TMain.UpdateChannelScalingFactors ;
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


procedure TMain.mnInvertClick(Sender: TObject);
 // - Menu Item --------------------------------
 // Open Invert channel dialog box (invert.pas)
 // --------------------------------------------
begin
     InvertDlg.ShowModal ;
     if InvertDlg.ModalResult = mrOK then UpdateViewSig
     end;

procedure TMain.mnDetectEvent2Click(Sender: TObject);
{ ---------------------------
  Open event detection window
  --------------------------- }
begin
     if FormExists('EventDetFrm') then begin
        if EventDetFrm.WindowState = wsMinimized then EventDetFrm.WindowState := wsNormal ;
        EventDetFrm.BringToFront ;
        EventDetFrm.SetFocus ;
        end
     else begin
        EventDetFrm := TEventDetFrm.Create(Self) ;
        EventDetFrm.Left := 20 ;
        EventDetFrm.Top := 20 ;
        end ;
     end;


procedure TMain.EditClick(Sender: TObject);
// --------------------------------------
// Enable/disable Copy items on Edit menu
// --------------------------------------
begin

     CopyData.Enabled := False ;
     CopyImage.Enabled := False ;
     mnSaveDataToFile.Enabled := False ;

     if MDIChildCount <= 0 then Exit ;

     if (ActiveMDIChild.Name = 'ViewSigFrm') then begin
        CopyData.Enabled := True ;
        CopyImage.Enabled := True ;
        mnSaveDataToFile.Enabled := True ;
        end
     else if (ActiveMDIChild.Name = 'NoiseAnalFrm') then begin
        CopyData.Enabled :=NoiseAnalFrm.IsClipboardDataAvailable ;
        CopyImage.Enabled := NoiseAnalFrm.IsClipboardDataAvailable ;
        mnSaveDataToFile.Enabled := NoiseAnalFrm.IsClipboardDataAvailable ;
        end
     else if (ActiveMDIChild.Name = 'SingleChanAnalFrm') then begin
        CopyData.Enabled := True ;
        CopyImage.Enabled := True ;
        end
     else if (ActiveMDIChild.Name = 'EventDetFrm') then begin
        CopyData.Enabled := EventDetFrm.IsClipboardDataAvailable ;
        CopyImage.Enabled := EventDetFrm.IsClipboardDataAvailable ;
        mnSaveDataToFile.Enabled := EventDetFrm.IsClipboardDataAvailable ;
        end
     else if (ActiveMDIChild.Name = 'ECGFrm') then begin
        CopyData.Enabled := ECGFrm.IsClipboardDataAvailable ;
        CopyImage.Enabled := ECGFrm.IsClipboardDataAvailable ;
        end
     else if (ActiveMDIChild.Name = 'PageViewFrm') then begin
        CopyData.Enabled := False ;
        CopyImage.Enabled := True ;
        end
     else if (ActiveMDIChild.Name = 'RecPlotFrm') then begin
        CopyData.Enabled := True ;
        CopyImage.Enabled := False ;
        end ;

     if CDRFH.FileHandle >= 0 then mnEditMarkers.Enabled := True
                              else mnEditMarkers.Enabled := False ;

     end;


procedure TMain.mnShowHintsClick(Sender: TObject);
{ --------------------
  Enable/Disable hints
  --------------------}
begin
     Application.ShowHint := not Application.ShowHint ;
     mnShowHints.Checked := Application.ShowHint ;
     end;

procedure TMain.mnPageViewClick(Sender: TObject);
// ------------------------------------------
// Open Multi-line page signal display window
// ------------------------------------------
begin
     if FormExists('PageViewFrm') then begin
        if PageViewFrm.WindowState = wsMinimized then PageViewFrm.WindowState := wsNormal ;
        PageViewFrm.BringToFront ;
        PageViewFrm.SetFocus ;
        end
     else begin
        PageViewFrm := TPageViewFrm.Create(Self) ;
        PageViewFrm.Left := 15 ;
        PageViewFrm.Top := 15 ;
        end ;

     end;


procedure TMain.File1Click(Sender: TObject);
// -----------------------------------
// Enable/disable available menu items
// -----------------------------------
var
    i : Integer ;
begin

     if MDIChildCount > 0 then begin

        if FormExists( 'RecordFrm' ) then begin
              for i := 0 to File1.Count-1 do begin
                 if RecordFrm.Recording then File1.Items[i].Enabled := False
                                        else File1.Items[i].Enabled := True ;
                 end ;
           end ;

        if ActiveMDIChild.Name = 'EventDetFrm' then begin
           mnPrint.Enabled := False ;
           if TEventDetFrm(ActiveMDIChild).IsClipboardDataAvailable then
              mnPrint.Enabled := True ;
           end
        else if ActiveMDIChild.Name = 'ECGFrm' then begin
           mnPrint.Enabled := False ;
           if TECGFrm(ActiveMDIChild).IsClipboardDataAvailable then
              mnPrint.Enabled := True ;
           end
        else if ActiveMDIChild.Name = 'PageViewFrm' then begin
           mnPrint.Enabled := True ;
           end ;

        end ;

     end;


procedure TMain.mnEditMarkersClick(Sender: TObject);
// --------------------------------------
//  Display edit chart markers dialog box
// --------------------------------------
begin
     // Display dialog box
     EditMarkersFrm.Left :=Main.Left + 15 ;
     EditMarkersFrm.Top :=Main.Top +  55 ;
     EditMarkersFrm.showmodal ;

     // Save settings
     SaveCDRHeader( CDRFH ) ;

     // Update display
     if FormExists('ViewSigFrm') then ViewSigFrm.DisplayFromFile ;
     if FormExists('PageViewFrm') then PageViewFrm.NewFile ;

     end ;


function TMain.ImportFromDataFile : Boolean ;
{ -------------------------------------------
  Import data from another data file format
  -------------------------------------------}
const
   NumScansPerBuf = 512 ;
Type
     TFilter = record
              Ext : string ;
              FType : TADCDataFileType ;
              end ;
var
   Filters : Array[1..17] of TFilter ;
   FileName : String ;
   FileType : TADCDataFileType ;
   i : Integer ;
   Buf : PSmallIntArray ;
   NumScansTotal : Integer ;
   NumRead : Integer ;
   iRec,ch : Integer ;
begin

     Result := False ;

     Filters[1].Ext := 'Axon Files (*.DAT,*.ABF)|*.DAT;*.ABF|' ;
     Filters[1].FType := ftUnknown ;
     Filters[2].Ext := 'CED Files (*.DAT,*.CFS)|*.DAT;*.CFS|' ;
     Filters[2].FType := ftCFS ;
     Filters[3].Ext := 'PAT Files (*.SCD)|*.SCD|' ;
     Filters[3].FType := ftSCD ;
     Filters[4].Ext := 'CDR Files (*.CDR)|*.CDR|' ;
     Filters[4].FType := ftCDR ;
     Filters[5].Ext := 'WCP Files (*.WCP)|*.WCP|' ;
     Filters[5].FType := ftWCP ;
     Filters[6].Ext := 'WinCDR Files (*.WCD)|*.WCD|' ;
     Filters[6].FType := ftWCD ;
     Filters[7].Ext := 'SPAN Files (*.SPA)|*.SPA|' ;
     Filters[7].FType := ftSPA ;
     Filters[8].Ext := 'SCAN Files (*.SCA)|*.SCA|' ;
     Filters[8].FType := ftSCA ;
     Filters[9].Ext := 'Physionet WFDB Files (*.HEA)|*.HEA|' ;
     Filters[9].FType := ftWFDB ;
     Filters[10].Ext := 'IGOR Binary Files (*.IBW)|*.IBW|' ;
     Filters[10].FType := ftIBW ;
     Filters[11].Ext := 'PoNeMah Files (*.PRO)|*.PRO|' ;
     Filters[11].FType := ftPNM ;
     Filters[12].Ext := 'Chart Files (*.CHT)|*.cht|' ;
     Filters[12].FType := ftCHT ;
     Filters[13].Ext := 'WAV Files (*.WAV, *.DAT)|*.wav;*.dat|' ;
     Filters[13].FType := ftCHT ;
     Filters[14].Ext := 'ASCII Text (*.TXT,*.ASC,*.CSV)|*.TXT;*.ASC;*.CSV|' ;
     Filters[14].FType := ftASC ;
     Filters[15].Ext := 'Raw Binary (*.dat,*.raw)|*.dat;*.raw|' ;
     Filters[15].FType := ftRaw ;
     Filters[16].Ext := 'HEKA Pulse (*.asc)|*.asc|' ;
     Filters[16].FType := ftHEK ;
     Filters[17].Ext := 'All Files (*.*)|*.*|' ;
     Filters[17].FType := ftUnknown ;

     OpenDialog.Filter := '' ;
     for i := 1 to High(Filters) do
         OpenDialog.Filter := OpenDialog.Filter + Filters[i].Ext ;
     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := 'DAT' ;
     OpenDialog.FileName := '' ;

     OpenDialog.Title := 'Import File ' ;
     if Main.DataDirectory <> '' then begin
        OpenDialog.InitialDir := Main.DataDirectory ;
        SetCurrentDir(Main.DataDirectory) ;
        end ;

     if not OpenDialog.execute then Exit ;

     Main.DataDirectory := ExtractFilePath( OpenDialog.FileName ) ;
     FileType := Filters[OpenDialog.FilterIndex].FType ;
     FileName := OpenDialog.FileName ;

     { Close existing data file }
     if CdrFH.FileHandle >= 0 then begin
        FileClose( CdrFH.FileHandle ) ;
        CdrFH.FileHandle := -1 ;
        end ;

     { Create name of EDR file to hold imported file }
     CdrFH.FileName := ChangeFileExt( FileName, DataFileExtension ) ;
     { Make sure an existing data file is not overwritten, unintentionally }
     if not FileOverwriteCheck( CdrFH.FileName ) then Exit ;

     // Report progress
     Main.StatusBar.SimpleText := format(
     ' IMPORT: Importing data from %s ',[FileName] ) ;

     if FileType = ftASC then begin
        // ASCII format data files
        ImportASCIIFrm.ImportFile := ImportFile ;
        ImportASCIIFrm.FileName := FileName ;
        if ImportASCIIFrm.ShowModal <> mrOK Then Exit ;
        if not Main.ImportFile.OpenDataFile( FileName, FileType ) then Exit ;
        end
     else if FileType = ftRaw then begin
        // Raw binary data files
        ImportRawFrm.ImportFile := ImportFile ;
        if ImportRawFrm.ShowModal <> mrOK Then Exit ;
        if not Main.ImportFile.OpenDataFile( FileName, FileType ) then Exit ;
        end
     else  if FileType = ftHEK then begin
        if not Main.ImportFile.OpenDataFile( FileName, FileType ) then Exit ;
        end
     else begin
        // All other data files
        FileType := ImportFile.FindFileType( FileName ) ;
        if not Main.ImportFile.OpenDataFile( FileName, FileType ) then Exit ;
        end ;

     { Create EDR data file to hold import}
     if not CreateNewDataFile( CdrFH ) then Exit ;

     CdrFH.NumChannels := Main.ImportFile.NumChannelsPerScan ;

     // Reset display magnification
     for ch := 0 to EDRChannelLimit do begin
         Channel[ch].ADCMaxValue := Main.ImportFile.MaxADCValue ;
         Channel[ch].yMin := -Channel[ch].ADCMaxValue -1 ;
         Channel[ch].yMax := Channel[ch].ADCMaxValue ;
         end ;

     CdrFH.IdentLine := Main.ImportFile.IdentLine ;

     // Allocate buffer
     GetMem( Buf, Main.ImportFile.NumScansPerRecord*Main.ImportFile.NumChannelsPerScan*2 ) ;

     { Copy records }
     NumScansTotal := 0 ;
     for iRec := 1 to Main.ImportFile.NumRecords do begin

         Main.ImportFile.RecordNum := iRec ;
         CdrFH.ADCVoltageRange := Main.ImportFile.ChannelADCVoltageRange[0] ;
         for ch := 0 to Main.ImportFile.NumChannelsPerScan-1 do begin
             Channel[ch].ChannelOffset := Main.ImportFile.ChannelOffset[ch] ;
             Channel[ch].ADCName := Main.ImportFile.ChannelName[ch] ;
             Channel[ch].ADCUnits := Main.ImportFile.ChannelUnits[ch] ;
             Channel[ch].ADCSCale := Main.ImportFile.ChannelScale[ch] ;
             Channel[ch].ADCCalibrationFactor := Main.ImportFile.ChannelCalibrationFactor[ch] ;
             Channel[ch].ADCAmplifierGain := Main.ImportFile.ChannelGain[ch] ;
             end ;

         NumRead := Main.ImportFile.LoadADCBuffer( 0, Main.ImportFile.NumScansPerRecord, Buf^ ) ;
         if NumRead <= 0 then Break ;

         { Copy sampling interval and A/D range
           NOTE. Must be after ImportFile.LoadADCBuffer() to ensure scan interval and ADCVoltageRange valid}

         CdrFH.dt := Main.ImportFile.ScanInterval ;
         CdrFH.ADCVoltageRange := Main.ImportFile.ChannelADCVoltageRange[0] ;

         // Correct calibration factor for any differences between
         for ch := 0 to Main.ImportFile.NumChannelsPerScan-1 do begin
             Channel[ch].ADCCalibrationFactor := Main.ImportFile.ChannelCalibrationFactor[ch]*
                                                 (Main.ImportFile.ChannelADCVoltageRange[0]/
                                                  Main.ImportFile.ChannelADCVoltageRange[ch])  ;
             end ;

         { Write A/D samples to EDR file }
         WriteCDRBuffer( CdrFH,NumScansTotal,Buf^,NumRead ) ;

         NumScansTotal := NumScansTotal + NumRead ;

         // Report progress
         Main.StatusBar.SimpleText := format(
         ' IMPORT: Importing %d/%d (%d channel) scans from record %d/%d from %s ',
         [NumRead,
          Main.ImportFile.NumScansPerRecord,
          Main.ImportFile.NumChannelsPerScan,
          iRec,Main.ImportFile.NumRecords,FileName]) ;

         end ;

     FreeMem(Buf) ;

     CDRfH.NumSamplesInFile := NumScansTotal*CdrFH.NumChannels ;
     for ch := 0 to CdrFH.NumChannels-1 do
         Channel[ch].ADCScale := CalibFactorToADCScale(
                                 CdrFH.ADCVoltageRange,
                                 Channel[ch] ) ;

     // Final progress
     Main.StatusBar.SimpleText := format(
     'IMPORT: %d records (%dx%d channel scans) imported from %s to %s',
     [Main.ImportFile.NumRecords,
      Main.ImportFile.NumScansPerRecord,
      Main.ImportFile.NumChannelsPerScan,
      FileName,
      CDRFH.FileName]) ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     { Save file header }
     SaveCDRHeader( CdrFH ) ;

     { Close source file }
     Main.ImportFile.CloseDataFile ;

     { Close dest. file }
     if CdrFH.FileHandle >= 0 then begin
        FileClose( CdrFH.FileHandle ) ;
        CdrFH.FileHandle := -1 ;
        end ;

     Result := True ;

     end ;


procedure TMain.AppendEDRFile( FileName : string ) ;
{ ------------------------------------------------
  Append an EDR data file on to end of current one
  ------------------------------------------------}
const
   NumScansPerBuf = 512 ;
var
   Buf : Array[0..NumScansPerBuf*(EDRChannelLimit+1)-1] of SmallInt ;
   NumScans : Integer ;
   NumScansTotal : Integer ;
   NumScansOriginal : Integer ;
   NumScansToRead : Integer ;
   NumRead : Integer ;
   NumWrite : Integer ;
   iRec : Integer ;
begin

     { Open data file to be imported }
     if not Main.ImportFile.OpenDataFile( FileName, ftEDR ) then Exit ;

     if (CdrFH.NumChannels <> Main.ImportFile.NumChannelsPerScan) then begin
        ShowMessage( 'APPEND: Unable to append files with different numbers of channels!');
        Exit ;
        end ;

     if Channel[0].ADCMaxValue <> Main.ImportFile.MaxADCValue then begin
        ShowMessage( 'APPEND: Unable to append files with different A/D resolutions!');
        Exit ;
        end ;

     if CDRFH.dt <> Main.ImportFile.ScanInterval then begin
        ShowMessage( 'APPEND: Unable to append files with different sampling intervals!');
        Exit ;
        end ;

     { Copy records }
     NumScansOriginal := CDRFH.NumSamplesInFile div CDRFH.NumChannels ;
     NumScansTotal := NumScansOriginal ;
     for iRec := 1 to Main.ImportFile.NumRecords do begin

         Main.ImportFile.RecordNum := iRec ;

         NumScans := 0 ;
         NumScansToRead := Main.ImportFile.NumScansPerRecord ;
         while NumScansToRead > 0 do begin

             // Read A/D sample from source file
             NumRead := Main.ImportFile.LoadADCBuffer( NumScans, NumScansPerBuf, Buf ) ;

             { Write A/D samples to EDR file }
             NumWrite := Min(NumRead,NumScansToRead) ;
             WriteCDRBuffer( CdrFH,NumScansTotal,Buf,NumWrite ) ;

             NumScans := NumScans + NumRead ;
             NumScansToRead := NumScansToRead - NumRead ;
             NumScansTotal := NumScansTotal + NumWrite ;

             // Report progress
             Main.StatusBar.SimpleText := format(
             ' APPEND: Add %d/%d scans (%d ch.) from record %d/%d from %s ',
             [ NumScans,
               Main.ImportFile.NumScansPerRecord,
               Main.ImportFile.NumChannelsPerScan,
               iRec,
               Main.ImportFile.NumRecords,
               FileName]) ;

             end ;

         end ;

     // Final progress
     Main.StatusBar.SimpleText := format(
     'APPEND: %d records (%dx%d channel scans) imported from %s',
     [Main.ImportFile.NumRecords,
      NumScansTotal - NumScansOriginal,
      CdrFH.NumChannels,
      FileName]) ;
     WriteToLogFile( Main.StatusBar.SimpleText ) ;

     CDRfH.NumSamplesInFile := NumScansTotal*CdrFH.NumChannels ;

     { Save file header }
     SaveCDRHeader( CdrFH ) ;

     WriteToLogFile( FileName + ' appended to ' + CdrFH.FileName ) ;

     { Close source file }
     Main.ImportFile.CloseDataFile ;

     // Update windows
     UpdateMDIWindows ;

     end ;

procedure TMain.mnECGFrmClick(Sender: TObject);
// ------------------------
//  Open ECG analysis window
// ------------------------
begin
     ECGFrm := TECGFrm.Create(Self) ;
     end;


procedure TMain.mnVP500Click(Sender: TObject);
// -------------------------------
//  Display VP500 control window
// ------------------------------- }
begin
     if FormExists('VP500PanelFrm') then begin
        if VP500PanelFrm.WindowState = wsMinimized then VP500PanelFrm.WindowState := wsNormal ;
        VP500PanelFrm.BringToFront ;
        VP500PanelFrm.SetFocus ;
        end
     else begin
        VP500PanelFrm := TVP500PanelFrm.Create(Self) ;
        VP500PanelFrm.Left := 10 ;
        VP500PanelFrm.Top := 10 ;
        end ;
     end ;


function TMain.CreateIndexedFileName(
         FileName : String ) : String ;
// ---------------------------------------------------
// Append an (incremented) index number to end of file
// ---------------------------------------------------
var
     i : Integer ;
     ExtensionStart : Integer ;
     IndexNumberStart : Integer ;
     IndexNum : Integer ;
     sDate,NewFileName : String ;
     FileStem : String ;
begin

     // Create new file name based on date

     DateTimeToString( sDate, 'yymmdd', Date() ) ;
     FileName := ExtractFilePath(FileName) + sDate + '.edr' ;
     if not DirectoryExists(ExtractFilePath(FileName)) then begin
        FileName := sDate + '.edr' ;
        end ;

     // Find '_nnn' index number (if it exists)
     i := Length(FileName) ;
     ExtensionStart := Length(FileName)+1 ;
     IndexNumberStart := -1 ;
     While (i > 0) do begin
         if FileName[i] = '.' then ExtensionStart := i ;
         if FileName[i] = '_' then begin
            IndexNumberStart := i ;
            Break ;
            end ;
         Dec(i) ;
         end ;

     if ((ExtensionStart - IndexNumberStart) > 4)
        or (ExtensionStart < 5) then IndexNumberStart := -1 ;

     // Find next available (lowest) index number for this file name

     FileStem :=  '' ;
     if IndexNumberStart > 0 then begin
        for i := 1 to IndexNumberStart-1 do
            FileStem :=  FileStem + FileName[i] ;
        end
     else begin
        for i := 1 to ExtensionStart-1 do
            FileStem :=  FileStem + FileName[i] ;
        end ;

     IndexNum := 0 ;
     repeat
          Inc(IndexNum) ;
          NewFileName := FileStem + format('_%.3d',[IndexNum]) ;
          for i := ExtensionStart to Length(FileName) do
              NewFileName := NewFileName + FileName[i] ;
          until not FileExists(NewFileName) ;

     // Return name
     Result := NewFileName ;

     end ;


procedure TMain.mnZoomOutClick(Sender: TObject);
// ----------------
// Display zoom out
// ----------------
var
    i,ch : Integer ;
begin

     for i := 0 to mnZoomOut.Count-1 do mnZoomOut.Items[i].Visible := False ;
     if MDIChildCount <= 0 then Exit ;

     if ActiveMDIChild.Name = 'RecordFrm' then begin
        // Use Recording channels if recording form is active
        for ch := 0 to Settings.NumChannels-1 do begin
            mnZoomOut.Items[ch].Caption := format( 'Ch.%d (%s)',
                                         [ch,Main.SESLabIO.ADCChannelName[ch]]) ;
            mnZoomOut.Items[ch].Tag := ch ;
            mnZoomOut.Items[ch].Visible := True ;
            end ;
         end
     else begin
        // Otherwise use file channels
        for ch := 0 to CDRFH.NumChannels-1 do begin
            mnZoomOut.Items[ch].Caption := format( 'Ch.%d (%s)',
                                         [ch,Channel[ch].ADCName]) ;
            mnZoomOut.Items[ch].Tag := ch ;
            mnZoomOut.Items[ch].Visible := True ;
            end ;
        end ;
     end ;


procedure TMain.mnEPC9Click(Sender: TObject);
// -------------------------------
//  Display EPC9/10 control window
// ------------------------------- }
begin

     if FormExists( 'EPC9PanelFrm' ) then begin
        // Make form visible, active and on top
        if EPC9PanelFrm.WindowState = wsMinimized then EPC9PanelFrm.WindowState := wsNormal ;
        EPC9PanelFrm.BringToFront ;
        EPC9PanelFrm.SetFocus ;
        end
     else begin
        EPC9PanelFrm := TEPC9PanelFrm.Create(Self) ;
        EPC9PanelFrm.Top := 10 ;
        EPC9PanelFrm.Left := Width - EPC9PanelFrm.Width - 50 ;
        end ;

     end;

procedure TMain.mnZoomInCh0Click(Sender: TObject);
// --------------------------------------
// Vertically magnify selected channel X2
// --------------------------------------
var
    i : Integer ;
begin

     for i := 0 to MDIChildCount-1 do begin
          if MDIChildren[I].Name = 'NoiseAnalFrm' then
             TNoiseAnalFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'SingleChanAnalFrm' then
             TSingleChanAnalFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'RecordFrm' then
             TRecordFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'PageViewFrm' then
             TPageViewFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'EventDetFrm' then
             TEventDetFrm(MDIChildren[I]).ZoomIn(TMenuItem(Sender).Tag) ;
          end ;
    end;


procedure TMain.mnZoomOutCh0Click(Sender: TObject);
// --------------------------------------
// Vertically reduce selected channel X2
// --------------------------------------
var
    i : Integer ;
begin

     for i := 0 to MDIChildCount-1 do begin
          if MDIChildren[I].Name = 'NoiseAnal' then
             TNoiseAnalFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'SingleChanAnalFrm' then
             TSingleChanAnalFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'RecordFrm' then
             TRecordFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'ViewSigFrm' then
             TViewSigFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'PageViewFrm' then
             TPageViewFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag)
          else if MDIChildren[I].Name = 'EventDetFrm' then
             TEventDetFrm(MDIChildren[I]).ZoomOut(TMenuItem(Sender).Tag) ;
          end ;

    end;

procedure TMain.ViewClick(Sender: TObject);
// ------------------------------
// Enable/disable View menu items
// ------------------------------
begin

     mnDisplayGrid.Checked := Settings.DisplayGrid ;

     if (CDRFH.NumSamplesInFile > 0) and (CDRFH.FileHandle >=0) then begin
        mnViewSig.Enabled := True ;
        mnPageView.Enabled := True ;
        end
     else begin
        mnViewSig.Enabled := False ;
        mnPageView.Enabled := False ;
        end ;

     end;

procedure TMain.mnFilePropertiesClick(Sender: TObject);
// - Menu Item -------------------------------
//  File properties dialog (fileprops.pas)
//  -------------------------------------------}
begin
     if FormExists('FilePropsDlg') then begin
        if FilePropsDlg.WindowState = wsMinimized then FilePropsDlg.WindowState := wsNormal ;
        FilePropsDlg.BringToFront ;
        FilePropsDlg.SetFocus ;
        end
     else begin
        FilePropsDlg := TFilePropsDlg.Create(Self) ;
        FilePropsDlg.Left := 10 ;
        FilePropsDlg.Top := 10 ;
        end ;
     end ;


function TMain.FormExists( FormName : String ) : Boolean ;
// --------------------------
// Return TRUE if form exists
// --------------------------
var
    i : Integer ;
begin
     Result := False ;
     for i := 0 to MDIChildCount-1 do
         if ANSIContainsText(MDIChildren[i].Name,FormName) then Result := True ;
     end ;


procedure TMain.SetupClick(Sender: TObject);
var
    i : Integer ;
begin

     mnVP500.Enabled := False ;
     for i := 0 to 3 do
       if Amplifier.AmplifierType[i] = amVP500 then mnVP500.Enabled := True ;

     mnCED1902.Enabled := False ;
     for i := 0 to 3 do
       if Amplifier.AmplifierType[i] = amCED1902 then mnCED1902.Enabled := True ;

     // Enable/disable Multiclamp reset item
     mnResetMulticlamp.Enabled := False ;
     for i := 0 to 3 do begin
         if (Amplifier.AmplifierType[i] = amMulticlamp700A) or
            (Amplifier.AmplifierType[i] = amMulticlamp700B) then mnResetMulticlamp.Enabled := True ;
         end;

     end;

procedure TMain.mnDBSPanelClick(Sender: TObject);
// -------------------------------
//  Display DBS stimulator control window
// -------------------------------
begin
     if FormExists('DBSPanelFrm') then begin
        if DBSPanelFrm.WindowState = wsMinimized then DBSPanelFrm.WindowState := wsNormal ;
        DBSPanelFrm.BringToFront ;
        DBSPanelFrm.SetFocus ;
        end
     else begin
        DBSPanelFrm := TDBSPanelFrm.Create(Self) ;
        DBSPanelFrm.Left := 10 ;
        DBSPanelFrm.Top := 10 ;
        end ;
     end ;

procedure TMain.mnDClampClick(Sender: TObject);
// --------------------------------------------
// Display DCLAMP - Dynamic Clamp control panel
// --------------------------------------------
begin
     if FormExists( 'DCLAMPFrm' ) then begin
        // Make form visible, active and on top
        if DefSetFrm.WindowState = wsMinimized then DefSetFrm.WindowState := wsNormal ;
        DCLAMPFrm.BringToFront ;
        DCLAMPFrm.SetFocus ;
        end
     else begin
        DCLAMPFrm := TDCLAMPFrm.Create(Self) ;
        DCLAMPFrm.Top := 10 ;
        DCLAMPFrm.Left := 10 ;
        end ;

     end;

procedure TMain.mnTritonClick(Sender: TObject);
// -------------------------------
//  Display Triton control window
// ------------------------------- }
begin

     if FormExists( 'TritonPanelFrm' ) then begin
        // Make form visible, active and on top
        if TritonPanelFrm.WindowState = wsMinimized then TritonPanelFrm.WindowState := wsNormal ;
        TritonPanelFrm.BringToFront ;
        TritonPanelFrm.SetFocus ;
        end
     else begin
        TritonPanelFrm := TTritonPanelFrm.Create(Self) ;
        TritonPanelFrm.Top := 10 ;
        TritonPanelFrm.Left := 650 ;
        end ;

     end;



procedure TMain.mnLaboratoryInterfaceClick(Sender: TObject);
{ - Menu Item -------------------------------
  Record parameters set-up dialog (setup.pas)
  -------------------------------------------}
begin
     if FormExists( 'LabInterfaceSetupFrm' ) then begin
        // Make form visible, active and on top
        if LabInterfaceSetupFrm.WindowState = wsMinimized then LabInterfaceSetupFrm.WindowState := wsNormal ;
        LabInterfaceSetupFrm.BringToFront ;
        LabInterfaceSetupFrm.SetFocus ;
        end
     else begin
        LabInterfaceSetupFrm := TLabInterfaceSetupFrm.Create(Self) ;
        LabInterfaceSetupFrm.Top := 10 ;
        LabInterfaceSetupFrm.Left := 10 ;
        end ;

     end;

procedure TMain.mnInputChannelsAndAmplifiersClick(Sender: TObject);
{ - Menu Item -------------------------------
  Record parameters set-up dialog (setup.pas)
  -------------------------------------------}
begin
     if FormExists( 'InputChannelSetupFrm' ) then begin
        // Make form visible, active and on top
        if InputChannelSetupFrm.WindowState = wsMinimized then InputChannelSetupFrm.WindowState := wsNormal ;
        InputChannelSetupFrm.BringToFront ;
        InputChannelSetupFrm.SetFocus ;
        end
     else begin
        InputChannelSetupFrm := TInputChannelSetupFrm.Create(Self) ;
        InputChannelSetupFrm.Top := 10 ;
        InputChannelSetupFrm.Left := 10 ;
        end ;

     end;


function TMain.GetSpecialFolder(const ASpecialFolderID: Integer): string;
// --------------------------
// Get Windows special folder
// --------------------------
var
  //vSFolder :  pItemIDList;
  vSpecialPath : array[0..MAX_PATH] of Char;
begin

    SHGetFolderPath( 0, ASpecialFolderID, 0,0,vSpecialPath) ;
//  SHGetSpecialFolderLocation(0, ASpecialFolderID, vSFolder);

//  SHGetPathFromIDList(vSFolder, vSpecialPath);

  Result := StrPas(vSpecialPath);

  end;

function TMain.ShowTritonPanel : Boolean ;
// -------------------------------
//  Display Triton control window
// ------------------------------- }
begin
     // Display Triton control panel if it is not open
     Result := False ;
     case Main.SESLabIO.LabInterfaceType of
          Triton : begin
             if not Main.FormExists( 'TritonPanelFrm' ) then
                begin
                // Create form
                TritonPanelFrm := TTritonPanelFrm.Create(Self) ;
                TritonPanelFrm.Top := 15 ;
                TritonPanelFrm.Left := 710 ;
                end
             else
                begin
                // Make form visible, active and on top
                if TritonPanelFrm.WindowState = wsMinimized then TritonPanelFrm.WindowState := wsNormal ;
                TritonPanelFrm.BringToFront ;
                TritonPanelFrm.SetFocus ;
                end ;
             Result := True ;
             end ;
          end;
end;



end.


