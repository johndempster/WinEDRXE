unit SetIgnor;
{ ================================================================
  WinEDR (c) J. Dempster, University of Strathclyde, 1998-2001
  Change Ignore setting block of events in channel state list
  ================================================================
  3/7/01 Updated to allow simultaneous amplitude/duration criteria
  20/1/03 Now sets all events correctly within selected block
  5/2/04  NumEvents now passed to SetIgnor
  21/12/06 Standard deviation criterion added }


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, global, fileio, ComCtrls, ValEdit, ValidatedEdit ;

type
  TSetIgnoreFrm = class(TForm)
    GroupBox6: TGroupBox;
    cbChannelState: TComboBox;
    Label1: TLabel;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    rbIgnored: TRadioButton;
    rbNotIgnored: TRadioButton;
    ckDuration: TCheckBox;
    ckAmplitude: TCheckBox;
    edAmplitudeLo: TValidatedEdit;
    edAmplitudeHi: TValidatedEdit;
    edDurationLo: TValidatedEdit;
    edDurationHi: TValidatedEdit;
    Label6: TLabel;
    Label7: TLabel;
    ckSD: TCheckBox;
    edSDLo: TValidatedEdit;
    Label2: TLabel;
    edSDHi: TValidatedEdit;
    GroupBox9: TGroupBox;
    rbAllEvents: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SelectedChannel : Integer ;
  end;

var
  SetIgnoreFrm: TSetIgnoreFrm;

implementation

uses Mdiform, SingleChanAnal;

{$R *.DFM}

const
     stClosed = 0 ;
     stOpen = 1 ;
     stAll = 2 ;


procedure TSetIgnoreFrm.FormShow(Sender: TObject);
{ ------------------------------------------
  Initialise controls when form is displayed
  ------------------------------------------ }
begin

     { Set selected state to ALL }
     cbChannelState.ItemIndex := stAll ;

     edRange.HiLimit := SingleChanAnalFrm.EventFile.NumEvents ;
     edRange.HiValue := edRange.HiLimit ;

     { Initialise duration range }
     if edDurationHi.Value = edDurationHi.HiLimit then begin
        edDurationLo.Scale := Settings.TScale ;
        edDurationLo.Units := Settings.TUnits ;
        edDurationHi.Value := 1E-3 ;
        edDurationLo.Value := 0.0 ;
        end ;

     { Initialise amplitude range }
     edAmplitudeLo.Units := Channel[SelectedChannel].ADCUnits ;
     edAmplitudeHi.Units := Channel[SelectedChannel].ADCUnits ;
     if edAmplitudeHi.Value = edAmplitudeHi.HiLimit then begin
        edAmplitudeHi.Value := 1.0 ;
        edAmplitudeLo.Value := 0.0 ;
        end ;

     { Initialise standard dev. range }
     edSDLo.Units := Channel[SelectedChannel].ADCUnits ;
     edSDHi.Units := Channel[SelectedChannel].ADCUnits ;
     if edAmplitudeHi.Value = edAmplitudeHi.HiLimit then begin
        edAmplitudeHi.Value := 1.0 ;
        edAmplitudeLo.Value := 0.0 ;
        end ;

     end;


procedure TSetIgnoreFrm.bOKClick(Sender: TObject);
{ ----------------------------------------------------
  Set a selected block of event into the ignored state
  ---------------------------------------------------- }
var
   StartAt,EndAt,EventNum,SelectedState,NumHits : Integer ;
   Event : TEvent ;
   DurationMatch,AmplitudeMatch,SDMatch : Boolean ;
   SetState : String ;
begin

     Screen.Cursor := crHourglass ;

     // Set range of events to be scanned
     if rbRange.Checked then begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end
     else begin
        StartAt := Round(edRange.LoLimit) ;
        EndAt := Round(edRange.HiLimit) ;
        end ;

     { Selected channel state (Closed,Open,All) }
     SelectedState := cbChannelState.ItemIndex ;

     if rbIgnored.Checked then SetState := 'IGNORED'
                          else SetState := 'NOT IGNORED' ;

     NumHits := 0 ;
     Main.StatusBar.SimpleText :=
     'Single-channel Analysis (Set Block):' ;

     for EventNum := StartAt to EndAt do begin

         { Read event data from file }
         SingleChanAnalFrm.ReadEventFromFile( SingleChanAnalFrm.EventFile,
                                              EventNum,
                                              Event ) ;

         if Event.Available then begin

            // State duration criterion
            if ckDuration.Checked then begin
               if (edDurationLo.Value <= Event.Duration) and
                  (Event.Duration <= edDurationHi.Value) and
                  ((Event.ChannelState = SelectedState) or (SelectedState = stAll)) then
                  DurationMatch := True
               else DurationMatch := False ;
               end
            else DurationMatch := True ;

            // State amplitude criterion
            if ckAmplitude.Checked then begin
               if (edAmplitudeLo.Value <= Event.Average) and
                  (Event.Average <= edAmplitudeHi.Value) and
                  ((Event.ChannelState = SelectedState) or (SelectedState = stAll)) then
                  AmplitudeMatch := True
               else AmplitudeMatch := False ;
               end
            else AmplitudeMatch := True ;

            // State amplitude standard deviation criterion
            if ckSD.Checked then begin
               if (edSDLo.Value <= Sqrt(Event.Variance)) and
                  (Sqrt(Event.Variance) <= edSDHi.Value) and
                  ((Event.ChannelState = SelectedState) or (SelectedState = stAll)) then
                  SDMatch := True
               else SDMatch := False ;
               end
            else SDMatch := True ;

            if AmplitudeMatch and DurationMatch and SDMatch then begin
                Event.Ignore := rbIgnored.Checked ;
                Inc(NumHits) ;
                Main.StatusBar.SimpleText := format(
                'Single-channel Analysis (Set Block): Event %d/%d set to %s ',
                [EventNum,EndAt,SetState]) ;
                Application.ProcessMessages ;
                end ;

            { Save back to file }
            SingleChanAnalFrm.WriteEventToFile( SingleChanAnalFrm.EventFile,
                                                EventNum,
                                                Event ) ;
            end
         else Break ;

         end ;

     Main.StatusBar.SimpleText := format(
     'Single-channel Analysis (Set Block): %d events set to %s ',
     [NumHits,SetState]) ;

     Screen.Cursor := crDefault ;

     end;

end.
