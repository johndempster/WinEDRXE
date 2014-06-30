unit ftest;
{ ===============================================================================
  WinEDR (c) J. Dempster 1998-2001
  F-Test form. Calculates whether adding additional exponential components
  to a dwell time p.d.f. produces a signficant reduction residual sum of squares
  (Called from
  ===============================================================================
  8/7/01 }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ValEdit, Maths, Grids, Printers, global, math ;

const
    MaxExps = 5 ;
    MaxPars = 20 ;

type
  TResult = record
       Available : Boolean ;
       ResSSQ : single ;
       NumPars : Integer ;
       NumDegFree : Integer ;
       Pars : Array[0..MaxPars-1] of Single ;
       ParNames : Array[0..MaxPars-1] of String ;
       ParUnits : Array[0..MaxPars-1] of String ;
       end ;

  TFTestFrm = class(TForm)
    TableGrp: TGroupBox;
    sgTable: TStringGrid;
    bPrint: TButton;
    bClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bClearClick(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Results : Array[1..MaxExps] of TResult ;
    FTitle : TStringList ;
  public
    { Public declarations }
    procedure AddResult(
              NumExps : Integer ;
              ResSD : single ;
              NumPars : Integer ;
              NumDegFree : Integer ;
              Pars : Array of Single ;
              ParUnits : Array of string ) ;

    procedure AddPrinterTitleLine( Line : string );
    procedure ClearPrinterTitleLines ;

  end;

var
  FTestFrm: TFTestFrm;

implementation

{$R *.DFM}

uses shared ;

const
    cNumExps = 0 ;
    cResSSQ = 1 ;
    cNumPars = 2 ;
    cDegFree = 3 ;
    cFStatistic = 4 ;
    cProb = 5 ;
    cPars = 6 ;

procedure TFTestFrm.FormCreate(Sender: TObject);
var
    i : Integer ;
begin

     for i := 1 to MaxExps do Results[i].Available := False ;
     FTitle := TStringList.Create ;

     end;


procedure TFTestFrm.FormShow(Sender: TObject);
var
   SSQA,SSQB,F,P : Single ;
   iExp,DegFreeA,DegFreeB,NumParsA,NumParsB,Col,Row,Size,NumExpsInTable,i : Integer ;
   ParRow,MaxWidth : Integer ;
begin
     sgTable.RowCount := cProb + MaxExps*2 ;
     sgTable.ColCount := MaxExps + 1 ;

     sgTable.Cells[0,cNumExps] :=     ' Exponentials ' ;
     sgTable.Cells[0,cResSSQ] :=      ' Residual SSQ' ;
     sgTable.Cells[0,cNumPars] :=    ' No. Parameters ' ;
     sgTable.Cells[0,cDegFree] :=    ' Degrees of Freedom ' ;
     sgTable.Cells[0,cFStatistic] := ' F     ' ;
     sgTable.Cells[0,cProb] :=       ' p     ' ;

     sgTable.DefaultRowHeight := sgTable.Canvas.TextHeight('X') + 1 ;
     NumExpsInTable := 0 ;
     for iExp := 1 to MaxExps do begin

         // Write fit quality results to table
         if Results[iExp].Available then begin
            sgTable.Cells[iExp,cNumExps] := format(' %d ',[iExp] ) ;
            sgTable.Cells[iExp,cResSSQ] := format(' %.4g ',[Results[iExp].ResSSQ] ) ;
            sgTable.Cells[iExp,cNumPars] := format(' %d ',[Results[iExp].NumPars] ) ;
            sgTable.Cells[iExp,cDegFree] := format(' %d ',[Results[iExp].NumDegFree] ) ;
            for i := 0 to Results[iExp].NumPars-1 do begin
                 ParRow := cPars+i ;
                 sgTable.Cells[0,ParRow] := Results[iExp].ParNames[i] ;
                 sgTable.Cells[iExp,ParRow] := format(' %.4g %s ',
                 [Results[iExp].Pars[i],Results[iExp].ParUnits[i]] ) ;
                 end ;

            NumExpsInTable := iExp ;
            end ;

         // Calculate F statistic between n and n-1 exponential fits
         if Results[iExp].Available and Results[iExp-1 ].Available
            and (iExp > 1) then begin

            SSQA := Results[iExp-1].ResSSQ ;
            DegFreeA := Results[iExp-1].NumDegFree ;
            NumParsA := Results[iExp-1].NumPars ;

            SSQB := Results[iExp].ResSSQ ;
            DegFreeB := Results[iExp].NumDegFree ;
            NumParsB := Results[iExp].NumPars ;

            // F statistic
            F := ((SSQA - SSQB) / SSQB)*(DegFreeB/NumParsA) ;
            // F distribution tail probability
            P := FProb( F, NumParsA, DegFreeB ) ;

            sgTable.Cells[iExp,cFStatistic] := format( '%.4g',[F] ) ;
            sgTable.Cells[iExp,cProb] := format( '%.4f',[P] ) ;
            end ;
         end ;

     // Size table
     sgTable.Canvas.Font.Name := 'Arial ' ;
     sgTable.Canvas.Font.Size :=  10 ;
     sgTable.DefaultRowHeight := sgTable.Canvas.TextHeight('X') + 1 ;
     for Col := 0 to sgTable.ColCount-1 do begin
         MaxWidth := 0 ;
         for Row := 0 to sgTable.RowCount-1 do begin
            MaxWidth := Max(MaxWidth,sgTable.Canvas.TextWidth(sgTable.Cells[Col,Row])) ;
            end ;
         sgTable.ColWidths[Col] := MaxWidth ; ;
         end ;

     Size := 0 ;
     for Col := 0 to sgTable.ColCount-1 do Size := Size + sgTable.ColWidths[Col] ;
     sgTable.Width := Size + 5 ;
     TableGrp.Width := 2*sgTable.Left + sgTable.Width ;

     sgTable.RowCount := NumExpsInTable*2 + cPars ;
     sgTable.Height := sgTable.DefaultRowHeight*(sgTable.RowCount+1) ;
     TableGrp.height := sgTable.Top + sgTable.Height + bPrint.Height + 8 ;
     bPrint.Top :=  TableGrp.height - bPrint.Height - 4 ;
     bClear.Top := bPrint.Top ;
     ClientHeight := TableGrp.Top + TableGrp.Height + 5 ;
     ClientWidth :=  TableGrp.Left + TableGrp.Width + 5 ;

     end;

procedure TFTestFrm.AddResult(
          NumExps : Integer ;
          ResSD : single ;
          NumPars : Integer ;
          NumDegFree : Integer ;
          Pars : Array of Single ;
          ParUnits : Array of string ) ;
var
    i : Integer ;
begin

     // Add new fit result
     if (NumExps >=1) and (NumExps <= MaxExps) then begin
        Results[NumExps].Available := True ;
        Results[NumExps].NumPars := NumPars ;
        Results[NumExps].NumDegFree := NumDegFree ;
        Results[NumExps].ResSSQ := ResSD*ResSD*NumDegFree ;
        for i := 0 to NumPars-1 do begin
            Results[NumExps].Pars[i] := Pars[i] ;
            Results[NumExps].ParUnits[i] := ParUnits[i] ;
            if (i mod 2) = 0 then
               Results[NumExps].ParNames[i] := format(' A(%d)',[(i div 2)+1])
            else
               Results[NumExps].ParNames[i] := format(' Tau(%d)',[(i div 2)+1]) ;
            end ;
        end ;
     end ;


procedure TFTestFrm.AddPrinterTitleLine(
          Line : string
          );
{ ---------------------------
  Add a line to printer title
  ---------------------------}
begin
     FTitle.Add( Line ) ;
     end ;


procedure TFTestFrm.ClearPrinterTitleLines ;
{ ---------------------------
  Clear printer title lines
  ---------------------------}
begin
     FTitle.Clear ;
     end ;


procedure TFTestFrm.bClearClick(Sender: TObject);
var
    i,Row,Col : Integer ;
begin
     for i := 1 to MaxExps do Results[i].Available := False ;
     for Row := 0 to sgTable.RowCount-1 do begin
         for Col := 1 to sgTable.ColCount-1 do sgTable.Cells[Col,Row] := '' ;
         end ;
     end;

procedure TFTestFrm.bPrintClick(Sender: TObject);
{ -------------------------
  Print the results table
  -------------------------}
var
   CharWidth,CharHeight,ColHeight,Row,Col,w,i : Integer ;
   PageLeft,PageTop,Line,ColLeft : Integer ;
   ColWidth : Array[0..20] of Integer ;

begin

     { Set print font and size }
     Printer.Canvas.font.name := Settings.Plot.FontName ;
     Printer.Canvas.Font.Size := 10 ;

     CharWidth := Printer.canvas.TextWidth('X') ;
     CharHeight := Printer.canvas.TextHeight('X') ;
     PageTop := CharHeight*5 ;
     PageLeft := CharWidth*8 ;

     Printer.BeginDoc ;

     { Calculate column widths of Table}
     for col := 0 to sgTable.ColCount-1 do begin
         ColWidth[Col] := 0 ;
         for row := 0 to sgTable.RowCount-1 do begin
             w := Printer.canvas.TextWidth(sgTable.cells[Col,Row]) ;
             if ColWidth[Col] < w then ColWidth[Col] := w ;
             end ;
         end ;
     for col := 0 to sgTable.ColCount-1 do ColWidth[Col] := ColWidth[Col] +
                                           2*CharWidth ;

     ColHeight := (12*Printer.canvas.TextHeight(sgTable.cells[0,0])) div 10 ;

     Line := PageTop ;
     for i := 0 to FTitle.Count-1 do begin
         Printer.Canvas.Textout( PageLeft, Line, FTitle.Strings[i] ) ;
         Line := Line + CharHeight ;
         end ;

    Line := Line + CharHeight ;

     { Print Table
       ===========}

     for row := 0 to sgTable.RowCount-1 do begin

         { Print row }
         ColLeft := PageLeft ;
         Printer.Canvas.Pen.Width := 1 ;
         for col := 0 to sgTable.ColCount-1 do begin
             printer.canvas.rectangle( ColLeft,Line,ColLeft+ColWidth[Col],
                                       Line+ColHeight ) ;
             printer.canvas.textout( ColLeft + CharWidth,
                                     Line + CharHeight div 10,
                                     sgTable.cells[Col,Row] ) ;
             ColLeft := ColLeft + ColWidth[Col] ;
             end ;

         { New page when line crosses bottom margin }
         Line := Line + ColHeight ;

         end ;

     Printer.EndDoc ;

     end;

procedure TFTestFrm.FormDestroy(Sender: TObject);
begin
     FTitle.Free ;
     end;

end.
