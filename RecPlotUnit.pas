unit RecPlotUnit;
//  ----------------------------------------
// On-line analysis time course plot module
// ----------------------------------------
// 22.08.07
// 03.02.09 Resistance,conductance, current and voltage plot added
// 08.08.12 Frequency count period now set in config dialog box
// 09.08.12 Resistance Scale units now selected correctly for current
//          Axes ticks now automatically set to integer values.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XMultiYPlot, ValidatedEdit, global, math, mmsystem,
  XYPlotDisplay, HTMLLabel, ExtCtrls, strutils ;

const
    ptUnknown = 0 ;
    ptFrequency = 1 ;
    ptResistance = 2 ;
    ptConductance = 3 ;
    ptVoltage = 4 ;
    ptCurrent = 5 ;
type

  TRecPlotData = Record
      PlotNum : Integer ;
      LineNum : Integer ;
      VarNum : Integer ;
      ChanNum : Integer ;
      YLabel : String ;
      ListEntry : String ;
      end ;

  TRecPlotFrm = class(TForm)
    ControlsGrp: TGroupBox;
    bClearPoints: TButton;
    plPlot: TXYPlotDisplay;
    ConductancePanel: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    cbConductanceUnits: TComboBox;
    ResistancePanel: TPanel;
    Label2: TLabel;
    Panel3: TPanel;
    cbResistanceUnits: TComboBox;
    CurrentPanel: TPanel;
    Label4: TLabel;
    Panel5: TPanel;
    cbCurrentUnits: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure bClearPointsClick(Sender: TObject);
    procedure cbConductanceUnitsChange(Sender: TObject);
    procedure cbResistanceUnitsChange(Sender: TObject);
  private
    { Private declarations }
    NumEvents : Integer ;
    ResetTimeZero : Boolean ;
    TZero : Integer ;
    TimeStartedAt : String ;
    yMax : Single ;
    yMin : Single ;
    tMax : Single ;
    YScale : Single ;

    procedure ClearPoints ;
    procedure SavePlotDataToFile ;
    procedure UpdatePlotAxes(
              t : single ;
              y : Single
              ) ;

  public
    { Public declarations }
    PlotType : Integer ;
    procedure PlotFrequency( Frequency : Single ) ;
//              EventAtScan : Integer ;
//              var StartOfCountPeriod : Integer ;
//              var EndOfCountPeriod : Integer
            //  ) ;
    procedure PlotResistance(
              Resistance : Single
              ) ;
    procedure PlotConductance(
              Conductance : Single
              ) ;
    procedure PlotVoltage(
              Voltage : Single
              ) ;
    procedure PlotCurrent(
              Current : Single
              ) ;
    function TickSpacing( Range : Single ) : Single ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;

  end;

var
  RecPlotFrm: TRecPlotFrm;

implementation

uses MDIForm, Printgra;

{$R *.dfm}

procedure TRecPlotFrm.FormShow(Sender: TObject);
// -------------------------------------
// Initialisation when form is displayed
// -------------------------------------
begin

     // Set X/Y plot available flag to no plot
     { Create X/Y plot cursor }
     plPlot.ClearVerticalCursors ;
     plPlot.AddVerticalCursor( clGreen, '?r' ,0) ;

     { Plot graph of currently selected variables }
     plPlot.xAxisAutoRange := False ;
     plPlot.XAxisMin := 0.0 ;
     plPlot.XAxisMax := 60.0 ;
     plPlot.XAxisTick := plPlot.XAxisMax/6.0 ;
     tMax := 60.0 ;

     plPlot.yAxisAutoRange := False ;
     plPlot.YAxisMin := 0.0 ;
     plPlot.YAxisMax := 1.0 ;
     plPlot.YAxisTick := plPlot.YAxisMax*0.2 ;

     plPlot.xAxisLabel := 's' ;
     plPlot.yAxisLabel := 'Events/s' ;

     // Ensure there is enough space allocated for line
     plPlot.MaxPointsPerLine := 100000 ;
     // Add`new line to plot
     plPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     NumEvents := 0 ;
     yMax := 0.0 ;
     yMin := 0.0 ;     
     YScale := 1.0 ;

     TZero := TimeGetTime ;

     ResetTimeZero := True ;
     PlotType := ptUnknown ;

     ClearPoints ;

     Resize ;

     end;


procedure TRecPlotFrm.PlotFrequency( Frequency : Single ) ;
// ------------------------------
// Plot event frequency vs time
// ------------------------------
var
    y,t : single ;
begin

    if PlotType <> ptFrequency then begin
       Caption := 'Event Frequency' ;
       plPlot.YAxisLabel := 'Events/s' ;
       ResistancePanel.Visible := False ;
       ConductancePanel.Visible := False ;
       CurrentPanel.Visible := False ;
       PlotType := ptFrequency ;
       end ;


    t := (TimeGetTime - TZero)*0.001 ;
    y := Frequency ;

    UpdatePlotAxes(t,y) ;

    // Plot point
    plPlot.AddPoint( 0, t, y ) ;
    plPlot.VerticalCursors[0] := t ;

    end ;


procedure TRecPlotFrm.PlotResistance(
          Resistance : Single
          ) ;
// ------------------------------
// Plot resistance values vs time
// ------------------------------
var
    y,t : single ;

begin

    if PlotType <> ptResistance then begin
       Caption := 'Resistance' ;
       case cbResistanceUnits.ItemIndex of
          0 : begin
              YScale := 1E-9 ;
              plPlot.YAxisLabel := 'Rm (GOhms)' ;
              end ;
          1 : begin
              YScale := 1E-6 ;
              plPlot.YAxisLabel := 'Rm (MOhms)' ;
              end ;
          2 : begin
              YScale := 1E-3 ;
              plPlot.YAxisLabel := 'Rm (KOhms)' ;
              end ;
          3 : begin
              YScale := 1.0 ;
              plPlot.YAxisLabel := 'Rm (Ohms)' ;
              end ;
          end ;
       ResistancePanel.Visible := True ;
       ConductancePanel.Visible := False ;
       CurrentPanel.Visible := False ;
       PlotType := ptResistance ;
       ClearPoints ;
       end ;

    t := (TimeGetTime - TZero)*0.001 ;

    y := Resistance*YScale ;

    UpdatePlotAxes(t,y) ;

    // Plot point
    plPlot.AddPoint( 0, t, y ) ;
    plPlot.VerticalCursors[0] := t ;

    end ;


procedure TRecPlotFrm.PlotConductance(
          Conductance : Single
          ) ;
// ------------------------------
// Plot Conductance values vs time
// ------------------------------
var
    y,t : single ;

begin

    if PlotType <> ptConductance then begin
       Caption := 'Conductance' ;
       case cbConductanceUnits.ItemIndex of
          0 : begin
              YScale := 1E12 ;
              plPlot.YAxisLabel := 'Gm (pS)' ;
              end ;
          1 : begin
              YScale := 1E9 ;
              plPlot.YAxisLabel := 'Gm (nS)' ;
              end ;
          2 : begin
              YScale := 1E6 ;
              plPlot.YAxisLabel := 'Gm (uS)' ;
              end ;
          3 : begin
              YScale := 1E3 ;
              plPlot.YAxisLabel := 'Gm (mS)' ;
              end ;
          4 : begin
              YScale := 1.0 ;
              plPlot.YAxisLabel := 'Gm (S)' ;
              end ;
          end ;
       ResistancePanel.Visible := False ;
       ConductancePanel.Visible := True ;
       CurrentPanel.Visible := False ;
       PlotType := ptConductance ;
       ClearPoints ;
       end ;

    t := (TimeGetTime - TZero)*0.001 ;

    y := Conductance*YScale ;

    UpdatePlotAxes(t,y) ;

    // Plot point
    plPlot.AddPoint( 0, t, y ) ;
    plPlot.VerticalCursors[0] := t ;

    end ;


procedure TRecPlotFrm.PlotVoltage(
          Voltage : Single
          ) ;
// ------------------------------
// Plot Voltage values vs time
// ------------------------------
var
    y,t : single ;

begin

    if PlotType <> ptVoltage then begin
       Caption := 'Voltage' ;
       YScale := 1E3 ;
       plPlot.YAxisLabel := 'Vpulse (mV)' ;
       ResistancePanel.Visible := False ;
       ConductancePanel.Visible := False ;
       CurrentPanel.Visible := False ;
       PlotType := ptVoltage ;
       ClearPoints ;
       end ;

    t := (TimeGetTime - TZero)*0.001 ;

    y := Voltage*YScale ;

    UpdatePlotAxes(t,y) ;

    // Plot point
    plPlot.AddPoint( 0, t, y ) ;
    plPlot.VerticalCursors[0] := t ;

    end ;


procedure TRecPlotFrm.PlotCurrent(
          Current : Single
          ) ;
// ------------------------------
// Plot Current values vs time
// ------------------------------
var
    y,t : single ;

begin

    if PlotType <> ptCurrent then begin
       Caption := 'Current' ;
       case cbCurrentUnits.ItemIndex of
          0 : begin
              YScale := 1E12 ;
              plPlot.YAxisLabel := 'Im (pA)' ;
              end ;
          1 : begin
              YScale := 1E9 ;
              plPlot.YAxisLabel := 'Im (nA)' ;
              end ;
          2 : begin
              YScale := 1E6 ;
              plPlot.YAxisLabel := 'Im (uA)' ;
              end ;
          3 : begin
              YScale := 1E3 ;
              plPlot.YAxisLabel := 'Im (mA)' ;
              end ;
          4 : begin
              YScale := 1.0 ;
              plPlot.YAxisLabel := 'Im (A)' ;
              end ;
          end ;
       ResistancePanel.Visible := False ;
       ConductancePanel.Visible := False ;
       CurrentPanel.Visible := True ;
       PlotType := ptCurrent ;
       ClearPoints ;
       end ;

    t := (TimeGetTime - TZero)*0.001 ;

    y := Current*YScale ;

    UpdatePlotAxes(t,y) ;

    // Plot point
    plPlot.AddPoint( 0, t, y ) ;
    plPlot.VerticalCursors[0] := t ;

    end ;


procedure TRecPlotFrm.UpdatePlotAxes(
          T : single ;
          y : Single
          ) ;
// ------------------------
// Update plot X and Y axes
// ------------------------
begin

    if y > yMax then begin
       yMax := y ;
       plPlot.YAxisMax := yMax*1.1 ;
       plPlot.YAxisTick := TickSpacing(Max(Abs(yMax),Abs(yMin))) ;
       end ;

    if y < yMin then begin
       yMin := y ;
       plPlot.YAxisMin := yMin*1.1 ;
       plPlot.YAxisTick := TickSpacing(Max(Abs(yMax),Abs(yMin))) ;
       end ;

    if t > (0.95*tMax) then begin
       tMax := tMax + 60.0 ;
       plPlot.XAxisMax := tMax ;
       plPlot.XAxisTick := TickSpacing(tMax) ;
       end ;

    end ;


function TRecPlotFrm.TickSpacing( Range : Single ) : Single ;
//
// Find a suitable integer axis tick spacing
const
    TickMultipliers : array[0..6] of Integer = (1,2,5,10,20,50,100) ;
var
    TickBase,TickSize : Single ;
    i : Integer ;
begin
    TickBase := 0.01*exp(Round(Log10(Abs(Range)))*ln(10.0)) ;
    for i := 0 to High(TickMultipliers) do begin
        TickSize := TickBase*TickMultipliers[i] ;
        if (Range/TickSize) <= 10 then Break ;
        end ;
    Result := TickSize ;
    end ;


procedure TRecPlotFrm.SavePlotDataToFile ;
// -------------------------
// Save data in plot to file
// -------------------------
var
    OutFile : TextFile ;
    i : Integer ;
    x,y : Single ;
    FileName : String ;
begin

     if CdrFH.FileName <> '' then FileName := ChangeFileExt(CdrFH.FileName,'.txt')
                             else FileName := Settings.DataDirectory + DateToStr(Date) + '.txt' ;
     case PlotType of
        ptFrequency : FileName := ANSIReplaceText( FileName, '.txt','(F).txt') ;
        ptResistance : FileName := ANSIReplaceText( FileName, '.txt','(R).txt') ;
        ptConductance : FileName := ANSIReplaceText( FileName, '.txt','(G).txt') ;
        ptCurrent : FileName := ANSIReplaceText( FileName, '.txt','(I).txt') ;
        ptVoltage : FileName := ANSIReplaceText( FileName, '.txt','(V).txt') ;
        else FileName := ANSIReplaceText( FileName, '.txt','(U).txt')
        end ;

     AssignFile( OutFile, FileName ) ;

     if FileExists( FileName ) then begin
        Append(OutFile) ;
        end
     else begin
        ReWrite(OutFile) ;
        end ;

     WriteLn( OutFile, TimeStartedAt ) ;
     WriteLn( OutFile,'Time (s)' + chr(9) + plPlot.YaxisLabel ) ;


     for i := 0 to plPlot.GetNumPointsInLine(0)-1 do begin
         plPlot.GetPoint(0,i,x,y) ;
         WriteLn( OutFile,format('%.6g%s%.6g',[x,chr(9),y])) ;
         end ;

     CloseFile(OutFile) ;

     end ;

procedure TRecPlotFrm.FormClose(Sender: TObject; var Action: TCloseAction);
//
//
begin

     SavePlotDataToFile ;

     Action := caFree ;

     end;

procedure TRecPlotFrm.FormResize(Sender: TObject);
// -----------------------------------------
// Update controls on form when form resized
// -----------------------------------------
begin

     ControlsGrp.Top := ClientHeight - ControlsGrp.Height - 5 ;
     plPlot.Height := Max(ControlsGrp.Top - plPlot.Top - 2,2) ;
     plPlot.Width := Max(ClientWidth - plPlot.Left - 5,2) ;
     ControlsGrp.Width := plPlot.Width ;
     bClearPoints.Left := ControlsGrp.ClientWidth - bClearPoints.Width - 5 ;

     end;


procedure TRecPlotFrm.bClearPointsClick(Sender: TObject);
begin
     ClearPoints ;
     PlotType := ptUnknown ;
    end ;


procedure TRecPlotFrm.ClearPoints ;
// ------------------------
// Clear all data from plot
// ------------------------
begin

     plPlot.ClearAllLines ;
     plPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     NumEvents := 0 ;
     YMax := 0.0 ;
     YMin := 0.0 ;
     plPlot.YAxisMin := 0.0 ;
     plPlot.YAxisMax := 1.0 ;
     plPlot.YAxisTick := plPlot.YAxisMax*0.2 ;

     plPlot.XAxisMin := 0.0 ;
     plPlot.XAxisMax := 60.0 ;
     plPlot.YAxisTick := plPlot.YAxisMax/6.0 ;
     tMax := 60.0 ;

     TZero := TimeGetTime ;

     // Date/time started
     TimeStartedAt := 'Started at: ' + DateToStr(Date) + ' ' + TimeToStr(Time) ;

     ResetTimeZero := True ;

     plPlot.Invalidate ;

     end;


procedure TRecPlotFrm.CopyDataToClipBoard ;
{ ---------------------------------------------
  Copy the graph plot(s) data to the clipboard
  --------------------------------------------- }
begin
     plPlot.CopyDataToClipboard ;
     end ;


procedure TRecPlotFrm.Print ;
{ ------------------------------
  Print graph plot(s) on display
  ------------------------------ }
begin
//     PrintGraphFrm.Plot := plPlot ;
//     PrintGraphFrm.ToPrinter := True ;
//     PrintGraphFrm.MultiYPlot := True ;
//     PrintGraphFrm.ShowModal ;
//     if PrintGraphFrm.ModalResult = mrOK then begin
        plPlot.ClearPrinterTitle ;
        plPlot.AddPrinterTitleLine( ' File : ' + CDRFH.FileName ) ;
        plPlot.AddPrinterTitleLine( ' ' + CDRFH.IdentLine ) ;
        plPlot.Print ;
//        end ;
     end ;


procedure TRecPlotFrm.CopyImageToClipboard ;
{ ------------------------------------------------------------
  Copy image of graph plot(s) to clipboard as Windows metafile
  ------------------------------------------------------------ }
begin
//     PrintGraphFrm.Plot := plPlot ;
//     PrintGraphFrm.ToPrinter := False ;
//     PrintGraphFrm.MultiYPlot := True ;
//     PrintGraphFrm.ShowModal ;
//     if PrintGraphFrm.ModalResult = mrOK then
     plPlot.CopyImageToClipboard ;
     end ;





procedure TRecPlotFrm.cbConductanceUnitsChange(Sender: TObject);
// -------------------------
// Conductance units changed
// -------------------------
begin
    // Clear plot and update axes
    bClearPoints.Click ;
    end;

procedure TRecPlotFrm.cbResistanceUnitsChange(Sender: TObject);
// -------------------------
// Resistance units changed
// -------------------------
begin
    // Clear plot and update axes
    bClearPoints.Click ;
    end;


end.
