unit Custhist;
{ ----------------------------------
  WinWCP - Customise histogram plot
  ---------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, Global, Shared, ExtCtrls ;

type
  TCustHistFrm = class(TForm)
    XAxisGrp: TGroupBox;
    lbXMin: TLabel;
    lbxMax: TLabel;
    Label1: TLabel;
    edXlo: TEdit;
    edXHi: TEdit;
    edXTic: TEdit;
    cbXAxisType: TComboBox;
    YAxisGrp: TGroupBox;
    lbYMin: TLabel;
    lbYMax: TLabel;
    Label4: TLabel;
    edYLo: TEdit;
    edYHi: TEdit;
    EdYTic: TEdit;
    cbYAxisType: TComboBox;
    TraceGrp: TGroupBox;
    ckBorders: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    LabelsGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EdXLab: TEdit;
    edYLab: TEdit;
    edTitle: TEdit;
    cbFillStyle: TComboBox;
    ColorBox: TShape;
    Label8: TLabel;
    Label9: TLabel;
    EdLineThickness: TEdit;
    procedure bOKClick(Sender: TObject);
    procedure ColorBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure edXloKeyPress(Sender: TObject; var Key: Char);
    procedure cbFillStyleChange(Sender: TObject);
    procedure ckBordersClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateSettings ;
  public
    { Public declarations }
    SetPlot : TPlot ;
  end;

var
  CustHistFrm: TCustHistFrm;

implementation

{$R *.DFM}
var
   Initialised : Boolean ;
   OldPlot : TPlot ;


procedure TCustHistFrm.FormShow(Sender: TObject);
{ ----------------
  Initialise form
  ----------------}
begin
     { Keep a copy of settings, in case changes are cancelled }
     OldPlot := SetPlot ;
     Initialised := False ;
     UpdateSettings ;
     end ;


procedure TCustHistFrm.UpdateSettings ;
{ --------------------
  Update plot settings
  --------------------}
begin
     { X Axis }
     if Initialised then SetPlot.XAxis.Lo := ExtractFloat( edXlo.text,
                                                           SetPlot.XAxis.Lo ) ;
     edXlo.text := format(' %.3g ',[SetPlot.XAxis.Lo]) ;

     if Initialised then SetPlot.XAxis.Hi := ExtractFloat( edXHi.text,
                                                           SetPlot.XAxis.Hi ) ;
     edXHi.text := format(' %.3g ',[SetPlot.XAxis.Hi]) ;

     if Initialised then SetPlot.XAxis.Tic := ExtractFloat( edXTic.text,
                                                            SetPlot.XAxis.Tic ) ;
     edXTic.text := format(' %.3g ',[SetPlot.XAxis.Tic]) ;

     if not Initialised then begin
        if SetPlot.XAxis.log then cbXAxisType.itemIndex := 1
                             else cbXAxisType.itemIndex := 0 ;
        end ;
     if cbXAxisType.itemIndex = 0 then SetPlot.XAxis.log := false
                                  else SetPlot.XAxis.log := True ;

     { Y Axis }
     if Initialised then SetPlot.YAxis.Lo := ExtractFloat( edYlo.text,
                                                           SetPlot.YAxis.Lo ) ;
     edYlo.text := format(' %.3g ',[SetPlot.YAxis.Lo]) ;

     if Initialised then SetPlot.YAxis.Hi := ExtractFloat( edYHi.text,
                                                           SetPlot.YAxis.Hi ) ;
     edYhi.text := format(' %.3g ',[SetPlot.YAxis.Hi]) ;

     if Initialised then SetPlot.YAxis.Tic := ExtractFloat( edYTic.text,
                                                            SetPlot.YAxis.Tic ) ;
     edYTic.text := format(' %.3g ',[SetPlot.YAxis.Tic]) ;

     if not Initialised then begin
        if SetPlot.YAxis.log then cbYAxisType.itemIndex := 1
                             else cbYAxisType.itemIndex := 0 ;
        end ;
     if cbYAxisType.itemIndex = 0 then SetPlot.YAxis.log := false
                                  else SetPlot.YAxis.log := True ;

     { Labels }
     if Initialised then SetPlot.XAxis.Lab := edXLab.text
                    else  edXLab.text := SetPlot.XAxis.Lab ;

     if Initialised then SetPlot.YAxis.Lab := edYLab.text
                    else edYLab.text := SetPlot.YAxis.Lab ;

     if Initialised then SetPlot.Title := edTitle.text
                    else edTitle.text := SetPlot.Title ;

     { Histogram bin fill style }
     if not Initialised then begin
          cbFillStyle.Clear ;
          cbFillStyle.Items.AddObject( ' Empty ', TObject(bsClear) ) ;
          cbFillStyle.Items.AddObject( ' Solid ', TObject(bsSolid) ) ;
          cbFillStyle.Items.AddObject( ' Hatched ', TObject(bsBDiagonal) ) ;
          cbFillStyle.ItemIndex := cbFillStyle.IndexOfObject(
                                              TObject(SetPlot.BinFillStyle)) ;
          end ;
     else SetPlot.BinFillStyle := TBrushStyle(
                                  cbFillStyle.Objects[cbFillStyle.ItemIndex]) ;

     { Line thickness }
     if Initialised then Settings.Plot.LineThickness := ExtractInt(
                                                        edLineThickness.text) ;
     edLineThickness.text := format(' %d pts',[Settings.Plot.LineThickness]);

     { Borders round bins }
     if not Initialised then ckBorders.checked := SetPlot.BinBorders
                        else SetPlot.BinBorders := ckBorders.checked ;

     { Bin fill colour }
     if Initialised then SetPlot.BinFillColor := ColorBox.brush.color
                    else ColorBox.brush.color := SetPlot.BinFillColor ;

     Initialised := True ;
     end ;


procedure TCustHistFrm.bOKClick(Sender: TObject);
begin
     UpdateSettings ;
     end;

procedure TCustHistFrm.ColorBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     if ColorDialog.Execute then ColorBox.brush.color := ColorDialog.color ;
     UpdateSettings ;
     end;


procedure TCustHistFrm.edXloKeyPress(Sender: TObject; var Key: Char);
begin
     if key = chr(13) then UpdateSettings ;
     end;

procedure TCustHistFrm.cbFillStyleChange(Sender: TObject);
begin
     UpdateSettings ;
     end;

procedure TCustHistFrm.ckBordersClick(Sender: TObject);
begin
     UpdateSettings ;
     end;

procedure TCustHistFrm.bCancelClick(Sender: TObject);
begin
     { Changes cancelled - Restore old plot settings }
     SetPlot := OldPlot ;
     end;

end.
