unit SetFitPars1Unit;
// ----------------------------
// Set curve fitting parameters
// ----------------------------
// (For use with CurveFitter component
// 14.08.06

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HTMLLabel, ValidatedEdit, CurveFitter ;

type
  TSetFitPars1frm = class(TForm)
    ParametersGrp: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    ckFixed0: TCheckBox;
    ckFixed1: TCheckBox;
    ckFixed2: TCheckBox;
    ckFixed3: TCheckBox;
    ckFixed4: TCheckBox;
    ckFixed5: TCheckBox;
    ckFixed6: TCheckBox;
    ckFixed7: TCheckBox;
    edPar0: TValidatedEdit;
    edPar1: TValidatedEdit;
    edPar2: TValidatedEdit;
    edPar3: TValidatedEdit;
    edPar4: TValidatedEdit;
    edPar5: TValidatedEdit;
    edPar6: TValidatedEdit;
    edPar7: TValidatedEdit;
    edpar8: TValidatedEdit;
    ckfixed8: TCheckBox;
    edpar9: TValidatedEdit;
    ckfixed9: TCheckBox;
    lbPar0: THTMLLabel;
    lbPar1: THTMLLabel;
    lbPar2: THTMLLabel;
    lbPar3: THTMLLabel;
    lbPar4: THTMLLabel;
    lbPar5: THTMLLabel;
    lbPar6: THTMLLabel;
    lbPar7: THTMLLabel;
    lbPar8: THTMLLabel;
    lbPar9: THTMLLabel;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    rbAutomaticGuess: TRadioButton;
    rbManual: TRadioButton;
    bInitialise: TButton;
    procedure FormShow(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bInitialiseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CVFit : TCurveFitter ;
    procedure SetParameterLine(
          var lbPar : THTMLLabel ;
          var ed : TValidatedEdit ;
          var ck : TCheckBox ;
          ParNum : Integer
          ) ;
    procedure GetParameterLine(
          const ed : TValidatedEdit ;
          const ck : TCheckBox ;
          ParNum : LongInt
          ) ;

  end;

var
  SetFitPars1frm: TSetFitPars1frm;

implementation

{$R *.dfm}

procedure TSetFitPars1frm.FormShow(Sender: TObject);
{ ---------------------------------------
  Initialisation when dialog form is shown
  ---------------------------------------}
var
    i : Integer ;
begin

        for i := 0 to CVFit.NumParameters-1 do CVFit.InitialGuess(i) ;

        SetParameterLine( lbPar0, edPar0,  ckFixed0, 0 ) ;
        SetParameterLine( lbPar1, edPar1,  ckFixed1, 1) ;
        SetParameterLine( lbPar2, edPar2,  ckFixed2, 2 ) ;
        SetParameterLine( lbPar3, edPar3,  ckFixed3, 3 ) ;
        SetParameterLine( lbPar4, edPar4,  ckFixed4, 4 ) ;
        SetParameterLine( lbPar5, edPar5,  ckFixed5, 5 ) ;
        SetParameterLine( lbPar6, edPar6,  ckFixed6, 6 ) ;
        SetParameterLine( lbPar7, edPar7,  ckFixed7, 7 ) ;
        SetParameterLine( lbPar8, edPar8,  ckFixed8, 8 ) ;
        SetParameterLine( lbPar9, edPar9,  ckFixed9, 9 ) ;
        end;


procedure TSetFitPars1frm.SetParameterLine(
          var lbPar : THTMLLabel ;
          var ed : TValidatedEdit ;
          var ck : TCheckBox ;
          ParNum : Integer
          ) ;
{ -------------------------------------------
  Create parameter line on form from Equation
  -------------------------------------------}
begin

     if ParNum < CVFit.NumParameters then begin
        lbPar.Caption := CVFit.ParNames[ParNum] ;
        if Pos( 'ms', CVFit.ParUnits[ParNum] ) > 0 then ed.Scale := 1000.
                                                   else ed.Scale := 1. ;

        if rbAutomaticGuess.Checked and (not CVFit.FixedParameters[ParNum]) then
           ed.Value := CVFit.InitialGuess(ParNum)
        else ed.Value := CVFit.Parameters[ParNum]  ;

        ed.Units := CVFit.ParUnits[ParNum] ;
        ed.enabled := True ;
        ck.Enabled := True ;
        if CVFit.FixedParameters[ParNum] then ck.Checked := True
                                         else ck.Checked := False ;
        lbPar.Visible := True ;
        end
     else begin
        lbPar.visible := False ;
        ed.text := '' ;
        ed.enabled := False ;
        ck.enabled := False ;
        ck.checked := False ;
        end ;
     end ;



procedure TSetFitPars1Frm.GetParameterLine(
          const ed : TValidatedEdit ;
          const ck : TCheckBox ;
          ParNum : LongInt
          ) ;
{ -----------------------------------------------
  Read parameter line on form and update Equation
  -----------------------------------------------}
begin
     if ed.enabled then  begin
        CVFit.Parameters[ParNum] := ed.Value ;
        CVFit.FixedParameters[ParNum] := ck.Checked
        end ;
     end ;



procedure TSetFitPars1frm.bCancelClick(Sender: TObject);
begin
     hide ;
     end;


procedure TSetFitPars1frm.bOKClick(Sender: TObject);
// -----------------
// OK button clicked
// -----------------
begin

     // Transfer equation parameters from form to MathFunc

     GetParameterLine( edPar0, ckFixed0, 0 ) ;
     GetParameterLine( edPar1, ckFixed1, 1 ) ;
     GetParameterLine( edPar2, ckFixed2, 2 ) ;
     GetParameterLine( edPar3, ckFixed3, 3 ) ;
     GetParameterLine( edPar4, ckFixed4, 4 ) ;
     GetParameterLine( edPar5, ckFixed5, 5 ) ;
     GetParameterLine( edPar6, ckFixed6, 6 ) ;
     GetParameterLine( edPar7, ckFixed7, 7 ) ;
     GetParameterLine( edPar8, ckFixed8, 8 ) ;
     GetParameterLine( edPar9, ckFixed9, 9 ) ;

     // Indicate that parameters have been set
     CVFit.ParametersSet := True ;

     // Exit
     hide ;

     end;



procedure TSetFitPars1frm.bInitialiseClick(Sender: TObject);
// -----------------------------------------
// Create a set of initial parameter guesses
// -----------------------------------------
var
     i : Integer ;
begin

     // Get parameter data from form
     GetParameterLine( edPar0, ckFixed0, 0 ) ;
     GetParameterLine( edPar1, ckFixed1, 1 ) ;
     GetParameterLine( edPar2, ckFixed2, 2 ) ;
     GetParameterLine( edPar3, ckFixed3, 3 ) ;
     GetParameterLine( edPar4, ckFixed4, 4 ) ;
     GetParameterLine( edPar5, ckFixed5, 5 ) ;
     GetParameterLine( edPar6, ckFixed6, 6 ) ;
     GetParameterLine( edPar7, ckFixed7, 7 ) ;
     GetParameterLine( edPar8, ckFixed8, 8 ) ;
     GetParameterLine( edPar9, ckFixed9, 9 ) ;

     // Determine best initial guesses
     for i := 0 to CVFit.NumParameters-1 do
         if not CVFit.FixedParameters[i] then begin
            CVFit.Parameters[i] := CVFit.InitialGuess(i) ;
            end ;

     // Update form with new values
     SetParameterLine( lbPar0, edPar0,  ckFixed0, 0 ) ;
     SetParameterLine( lbPar1, edPar1,  ckFixed1, 1) ;
     SetParameterLine( lbPar2, edPar2,  ckFixed2, 2 ) ;
     SetParameterLine( lbPar3, edPar3,  ckFixed3, 3 ) ;
     SetParameterLine( lbPar4, edPar4,  ckFixed4, 4 ) ;
     SetParameterLine( lbPar5, edPar5,  ckFixed5, 5 ) ;
     SetParameterLine( lbPar6, edPar6,  ckFixed6, 6 ) ;
     SetParameterLine( lbPar7, edPar7,  ckFixed7, 7 ) ;
     SetParameterLine( lbPar8, edPar8,  ckFixed8, 8 ) ;
     SetParameterLine( lbPar9, edPar9,  ckFixed9, 9 ) ;

     end;


end.
