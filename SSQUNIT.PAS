unit Ssqunit;
{ ===============================================================
  WinCDR - Non-linear curve fitting module (c) J.Dempster 1996-97
  ===============================================================
  25/6/98 Invalid FP error on first curve fit (caused by uninitialised
          PowerSpectrum.Equation.ParSD) fixed
  4/7/98 Gaussian functions added (used by Amphist)
         GetEquationList now stores EqnType as object in TStringList
  8/9/98 Exponential PDFs added
  16/9/98 Log scaling of parameters added
          Histogram bin widths can now be accessed for pdfs }


interface
uses classes,dialogs,SysUtils,maths ;
const
     LastParameter = 8 ;
     ChannelLimit = 5 ;

type
    TWork = Array[0..12000] of Single ;
    TWorkArray = Array[0..1023] of Single ;
 


procedure FitCurve(
          var Data : TXYData ;
          nPoints : LongInt ;
          var Equation : TEquation
          ) ;


procedure Func( Const Pars : TPars ;
                nPoints,nPars : LongInt ;
                Var Residuals : Array of Single ;
                iStart : LongInt ;
                Equation : TEquation ;
                Const Data : TXYData ) ;


procedure SsqMin (
          var Pars : Tpars ;
          nPoints,nPars,ItMax,NumSig,NSiqSq : LongInt ;
          Delta : Single ;
          var W,SLTJJ : Array of Single ;
          var ICONV,ITER : LongInt ;
          var SSQ : Single ;
          var F : Array of Single ;
          var Equation : TEquation ;
          Const Data : TXYData) ;

procedure STAT(nPoints,nPars :LongInt ;
               var F,Y,W,SLT : Array of Single ;
               var SSQ : Single;
               var SDPars : Array of Single ;
               var SDMIN,R : Single ;
               var XPAR : Array of Single ) ;

procedure ScaleData(
          var Data : TXYData ;
          nPoints : LongInt ;
          var Equation : TEquation
           ) ;

procedure UnScaleParameters(
          var Equation : TEquation
           ) ;

function SSQCAL(
         const Pars : TPars ;
         nPoints,nPars : LongInt ;
         var Residuals : Array of Single ;
         iStart : LongInt ;
         const W : Array of Single ;
         const Equation : TEquation ;
         const Data : TXYData ) : Single ;

procedure MINV(
          var A : Array of Single ;
          N : LongInt ;
          var D : Single ;
          var L,M : Array of LongInt ) ;

FUNCTION SQRT1 ( R : Real ) : Real ;
function erf(x : Single ) : Single ;

implementation

const
     MaxSingle = 1E38 ;
var
     MathFunc : TMathFunc ;

procedure FitCurve(
          var Data : TXYData ;         { Data to be fitted to }
          nPoints : LongInt ;          { No. of data points }
          var Equation : TEquation ) ; { Equation to be fitted }
var
   NumSig,nSigSq,iConv,Maxiterations,iTer,i : Integer ;
   nVar,nFixed : Integer ;
   SSQ,DeltaMax,R : Single ;
   F,W : ^TWorkArray ;
   sltjj : Array[0..300] of Single ;
   Pars : TPars ;

begin

     New(F) ;
     New(W) ;

     { Define the function to be fitted }
     MathFunc := TMathFunc.Create ;
     MathFunc.Setup( Equation.EqnType, ' ', ' ' ) ;

     { Determine an initial set of parameter guesses }
     if not Equation.ParametersSet then begin
        for i := 0 to MathFunc.NumParameters-1 do begin
           { Find reasonable initial guesses for parameters which are not fixed }
           if not Equation.ParFixed[i] then
              Equation.Par[i] := MathFunc.InitialGuess( Data, nPoints, i ) ;
           end ;
        end ;

     { Scale X & Y data into 0-1 numerical range }
     ScaleData( Data, nPoints, Equation ) ;

     { Re-arrange parameters putting fixed parameters at end of array }
     nVar := 0 ;
     nFixed := MathFunc.NumParameters ;
     for i := 0 to MathFunc.NumParameters-1 do begin
         if Equation.ParSD[i] = -1. then begin
            Pars.Value[nFixed] := Equation.Par[i] ;
            Pars.Map[nFixed] := i ;
            Dec(nFixed) ;
            end
         else begin
            Inc(nVar) ;
            Pars.Value[nVar] := Equation.Par[i] ;
            Pars.Map[nVar] := i ;
            end ;
         end ;

     { Set weighting array to unity }
     for i := 1 to nPoints do W^[i] := 1. ;

     NumSig := 4 ;
     nSigSq := 4 ;
     deltamax := 1E-16 ;
     maxiterations := 100 ;
     iconv := 0 ;
     if nVar > 0 then begin
        try
           ssqmin ( Pars , nPoints, nVar, maxiterations,NumSig,NSigSq,DeltaMax,
                 W^,SLTJJ,iConv,iTer,SSQ,F^,Equation, Data ) ;
        except
               on EOverFlow do MessageDlg( ' Fit abandoned -FP Overflow !',
                                  mtWarning, [mbOK], 0 ) ;
               on EUnderFlow do MessageDlg( ' Fit abandoned -FP Underflow !',
                                  mtWarning, [mbOK], 0 ) ;
               on EZeroDivide do MessageDlg( ' Fit abandoned -FP Zero divide !',
                                  mtWarning, [mbOK], 0 ) ;
               end ;


        { Calculate parameter and residual standard deviations
          (If the fit has been successful) }
        if iConv > 0 then begin
           STAT(nPoints,nVar,F^,Data.y,W^,SLTJJ,SSQ,
                Pars.SD,Equation.ResidualSD,R,Pars.Value ) ;
           for i := 1 to nVar do Equation.Par[Pars.Map[i]] := Pars.Value[i] ;
           for i := 1 to nVar do Equation.ParSD[Pars.Map[i]] := Pars.SD[i] ;
           Equation.Available := True ;
           Equation.DegreesFreedom := nPoints-MathFunc.NumParameters ;
           Equation.ParametersSet := False ;
           end
        else Equation.Available := False ;
        end
     else Equation.Available := True ;


     UnScaleParameters( Equation ) ;
     Equation.NumIterations := Iter ;

     Dispose(W) ;
     Dispose(F) ;
     MathFunc.Free ;
     end ;




procedure ScaleData(
          var Data : TXYData ;
          nPoints : LongInt ;
          var Equation : TEquation
          );
{ ----------------------------------------------------------
  Scale Y data to lie in same range as X data
  (The iterative fitting routine is more stable when
  the X and Y data do not differ too much in numerical value)
  ----------------------------------------------------------}
var
   i,iEnd : Integer ;
   xMax,yMax,xScale,yScale,x,y,ySum : Single ;
begin
     iEnd := nPoints - 1 ;
     { Find absolute value limits of data }
     xMax := -MaxSingle ;
     yMax := -MaxSingle ;
     ySum := 0.0 ;
     for i := 0 to iEnd do begin
         x := Abs(Data.x[i]) ;
         y := Abs(Data.y[i]) ;
         if xMax < x then xMax := x ;
         if yMax < y then yMax := y ;
         ySum := ySum + y ;
         end ;

     {Calc. scaling factor}
     if xMax > 0. then xScale := 1./xMax
                  else xScale := 1. ;
     if yMax > 0. then yScale := 1./yMax
                  else yScale := 1. ;

     xScale := 1.0 ;
     yScale := 1.0 ;

     { Scale data to lie in same numerical range as X data }
     for i := 0 to iEnd do begin
         Data.x[i] := xScale * Data.x[i] ;
         Data.y[i] := yScale * Data.y[i] ;
         end ;

     { Set parameter scaling factors which adjust for
       data normalisation to 0-1 range }
     MathFunc.SetupNormalisation( xScale, yScale ) ;

     { Scale equation parameters }
     for i := 0 to MathFunc.NumParameters-1 do begin
         Equation.Par[i] := MathFunc.NormaliseParameter(i,Equation.Par[i]) ;
         end ;

     end ;


procedure UnScaleParameters(
          var Equation : TEquation
          ) ;
{ ----------------------------------------------------
  Correct best-fit parameters for effects of Y scaling
  ----------------------------------------------------}

var
   i : Integer ;
   UpperLimit,LowerLimit : single ;
begin
     for i := 0 to MathFunc.NumParameters-1 do begin

         { Don't denormalise a fixed parameter }
         if not Equation.ParFixed[i] then begin
            UpperLimit := MathFunc.DenormaliseParameter(i,
                          Equation.Par[i]+Equation.ParSD[i]) ;
            LowerLimit := MathFunc.DenormaliseParameter(i,
                          Equation.Par[i]-Equation.ParSD[i]) ;
            Equation.ParSD[i] := Abs(UpperLimit - LowerLimit)*0.5 ;

            end
         else begin
           { Equation.ParSD[i] := MathFunc.DenormaliseParameter(i,Equation.ParSD[i]) ;}
            end ;
         { Denormalise parameter }
         Equation.Par[i] := MathFunc.DenormaliseParameter(i,Equation.Par[i]) ;

         end ;
     end ;


procedure SsqMin (
          var Pars : TPars ;
          nPoints,nPars,ItMax,NumSig,NSiqSq : LongInt ;
          Delta : Single ;
          var W,SLTJJ : Array of Single ;
          var ICONV,ITER : LongInt ;
          var SSQ : Single ;
          var F : Array of Single ;
          var Equation : TEquation ;
          Const Data : TXYData) ;

{
  SSQMIN routine based on an original FORTRAN routine written by
  Kenneth Brown and modified by S.H. Bryant

C
C       Work buffer structure
C
C      (1+N(N-1)/2)
C      :---------:------N*M----------:-----------:
C      1         JACSS               GRADSS      GRDEND
C
C                :--N--:             :----M------------:---N----:
C                      DELEND        FPLSS             DIAGSS   ENDSS
C                                                                :--->cont.
C                                                                FMNSS
C
C       :-------M------:--N-1---:
C       FMNSS          XBADSS   XBEND
C
C
C
C
C               SSQMIN   ------   VERSION II.
C
C       ORIGINAL SOURCE FOR SSQMIN WAS GIFT FROM K. BROWN, 3/19/76.
C       PROGRAM WAS MODIFIED A FOLLOWS:
C
C       1.      WEIGHTING VECTOR W(1) WAS ADDED SO THAT ALL RESIDUALS
C	EQUAL F(I) * SQRT1(W(I)).
C
C       2.      THE VARIABLE KOUT WHICH INDICATED ON EXIT WHETHER F(I)
C       WAS CALCULATED FROM UPDATED X(J) WAS REMOVED. IN
C       CONDITIONS WHERE KOUT =0 THE NEW F(I)'S AND AN UPDATED
C       SSQ IS OUTPUTTED . SSQ ( SUM WEIGHTED F(I) SQUARED )
C       WAS PUT INTO THE CALL STRING.
C
C       3.      A NEW ARRAY SLTJJ(K) WHICH CONTAINS THE SUPER LOWER
C       TRIANGLE OF JOCOBIAN (TRANSPOSE)*JACOBIAN WAS ADDED TO THE
C       CALL STRING. IT HAS THE SIZE N*(N+1)/2 AND IS USED FOR
C       CALCULATING THE STATSTICS OF THE FIT. STORAGE OF
C       ELEMENTS IS AS FOLLOWS:C(1,1),C(2,1),C(2,2),C(3,2),C(3,3),
C       C(4,1)........
C       NOTE THE AREA WORK (1) THROU WORK (JACM1) IN WHICH SLTJJ
C       IS INITALLY STORED IS WRITTEN OVER (DO 52) IN CHOLESKY
C       AND IS NOT AVAILABLE ON RETURN.
C
C       4.      A BUG DUE TO SUBSCRIPTING W(I) OUT OF BOUNDS WAS
C       CORRECTED IN MAY '79. THE CRITERION FOR SWITCHING FROM
C       FORWARD DIFFERENCES (ISW=1) TO CENTRAL DIFFERENCES
C       (ISW = 2) FOR THE PARTIAL DERIVATIVE ESTIMATES IS SET
C       IN STATEMENT 27 (ERL2.LT.GRCIT).GRCIT IS INITALIZED
C       TO 1.E-3 AS IN ORIGINAL PROGRAM. THE VARIABLE T IN
C       CHOLESKY WAS MADE TT TO AVIOD CONFUSION WITH ARRAY T.
C
C       SSQMIN -- IS A FINITE DIFFERENCE LEVENBERG-MARQUARDT LEAST
C       SQUARES ALGORTHM. GIVEN THE USER SUPPLIED INITIAL
C       ESTIMATE FOR X, SSQMIN FINDS THE MINIMUM OF
C       SUM ((F (X ,....,X ) ) ** 2)   J=1,2,.....M
C              J  1       N   J
C       BY A MODIFICATION OF THE LEVENBERG-MARQUARDT ALGORITHM
C       WHICH INCLUDES INTERNAL SCALING AND ELIMINATES THE
C       NEED FOR EXPLICIT DERIVATIVES. THE F (X ,...,X )
C                           J  1      N
C       CAN BE TAKEN TO BE THE RESIDUALS OBTAINED WHEN FITTING
C       NON-LINEAR MODEL, G, TO DATA Y IN THE LEAST SQUARES
C       SENSE ..., I.E.,TAKE
C               F (X ,...,X ) = G (X ,...,X ) - Y
C                J  1      N     J  1      N
C       REFERENCES:
C
C       BROWN,K.M. AND DENNIS,J.S. DERIVATIVE FREE ANALOGS OF
C       THE LEVENBERG-MARQUARDT AND GAUSS ALGORITHMS FOR
C       NON-LINEAR LEAST SQUARES APPROXIMATION. NUMERISCHE
C       MATHEMATIK 18:289 -297  (1972).
C       BROWN,K.M.  COMPUTER ORIENTED METHODS FOR FITTING
C       TABULAR DATA IN THE LINEAR AND NON-LINEAR LEAST SQUARES
C       SENSE.  TECHNICIAL REPORT NO. 72-13. DEPT..COMPUTER &
C       INFORM. SCIENCES; 114 LIND HALL, UNIVERSITY OF
C       MINNESOTA, MINNEAPOLIS, MINNESOTA  5545.
C
C       PARAMETERS :
C
C       X       REAL ARRAY WITH DIMENSION N.
C               INPUT --- INITIAL ESTIMATES
C               OUTPUT -- VALUES AT MIN (OR FINAL APPROXIMATION)
C
C       M       THE NUMBER OF RESIDUALS (OBSERVATIONS)
C
C       N       THE NUMBER OF UNKNOWN PARAMETERS
C
C       ITMAX   THE MAXIMUM NUMBER OF ITERATIONS TO BE ALLOWED
C               NOTE-- THE MAXIMUM NUMBER OF FUNCTION EVALUATIONS
C               ALLOWED IS ROUGHLY (N+1)*ITMAX  .
C
C       IPRINT  AN OUTPUT PARAMETER. IF IPRINT IS NON ZERO CONTROL
C               IS PASSED ONCE DURING EACH ITERATION TO SUBROUTINE
C               PRNOUT WHICH PRINTS INTERMEDIATE RESULTS (SEE BELOW)
C               IF IPRINT IS ZERO NO CALL IS MADE.
C
C       NUMSIG  FIRST CONVERGENCE CRITERION. CONVERGENCE CONDITION
C               SATISFIED IF ALL COMPONENTS OF TWO SUCCESSIVE
C               ITERATES AGREE TO NUMSIG DIGITS.
C
C       NSIGSQ  SECOND CONVERGENCE CRITERION. CONVERGENCE CONDITIONS
C               SATISFIED IF SUM OF SQUARES OF RESIDUALS FOR TWO
C               SUCCESSIVE ITERATIONS AGREE TO NSIGSQ DIGITS.
C
C       DELTA   THIRD CONVERGENCE CRITERION. CONVERGENCE CONDITIONS
C               SATISFIED IF THE EUCLIDEAN NORM OF THE APPROXIMATE
C               GRADIENT VECTOR IS LESS THAN DELTA.
C
C         ***************  NOTE  ********************************
C
C               THE ITERATION WILL TERMIATE ( CONVERGENCE WILL CONSIDERED
C               ACHIEVED ) IF ANY ONE OF THE THREE CONDITIONS IS SATISFIED.
C
C       RMACH   A REAL ARRAY OF LENGTH TWO WHICH IS DEPENDENT
C               UPON THE MACHINE SIGNIFICANCE;
C               SIG (MAXIMUM NUMBER OF SIGNIFICANT
C               DIGITS ) AND SHOULD BE COMPUTED AS FOLLOWS:
C
C               RMACH(1)= 5.0*10.0 **(-SIG+3)
C               RMACH(2)=10.0 **(-(SIG/2)-1)
C
C          WORK SCRATCH ARRAY OF LENGTH 2*M+(N*(N+2*M+9))/2
C               WHOSE CONTENTS ARE
C
C       1 TO JACM1      N*(N+1)/2       LOWER SUPER TRIANGLE OF
C                               JACOBIAN( TRANSPOSED )
C                               TIMES JACOBIAN
C
C       JACESS TO GRDM1         N*M     JACOBIAN MATRIX
C
C       JACSS TO DELEND         N       DELTA X
C
C       GRADSS TO GRDEND        N       GRADIENT
C
C       GRADSS TO DIAGM1        M       INCREMENTED FUNCTION VECTOR
C
C       DIAGSS TO ENDSS N       SCALING VECTOR
C
C       FMNSS TO XBADSS-1       M       DECREMENTED FUNCTION VECTOR
C
C       XBADSS TO XBEND N       LASTEST SINGULAR POINT
C
C               NOTE:
C               SEVERAL WORDS ARE USED FOR TWO DIFFERENT QUANTITIES (E.G.,
C               JACOBIAN AND DELTA X) SO THEY MAY NOT BE AVAILABLE
C               THROUGHOUT THE PROGRAM.
C
C       W       WEIGHTING VECTOR OF LENGTH M
C
C       SLTJJ   ARRAY OF LENGTH N*(N+1)/2 WHICH CONTAINS THE LOWER SUPER
C               TRIANGLE OF J(TRANS)*J RETAINED FROM WORK(1) THROUGH
C               WORK(JACM1) IN DO 30. ELEMENTS STORED SERIALLY AS C(1,1),
C               C(2,1),C(2,2),C(3,1),C(3,2),...,C(N,N). USED IN STATISTICS
C               SUBROUTINES FOR STANDARD DEVIATIONS AND CORRELATION
C               COEFFICIENTS OF PARAMETERS.
C
C       ICONV   AN INTEGER OUTPUT PARAMETER INDICATING SUCCESSFUL
C               CONVERGENCE OR FAILURE
C
C               .GT.  0  MEANS CONVERGENCE IN ITER ITERATION
C                  =  1  CONVERGENCE BY FIRST CRITERION
C                  =  2  CONVERGENCE BY SECOND CRITERION
C                  =  3  CONVERGENCE BY THIRD CRITERION
C               .EQ.  0  MEANS FAILURE TO CONVERGE IN ITMAX ITERATIONS
C               .EQ. -1  MEANS FAILURE TO CONVERGE IN ITER ITERATIONS
C                BECAUSE OF UNAVOIDABLE SINGULARITY WAS ENCOUNTERED
C
C          ITER AN INTEGER OUTPUT PARAMETER WHOSE VALUE IS THE NUMBER OF
C               ITERATIONS USED. THE NUMBER OF FUNCTION EVALUATIONS USED
C               IS ROUGHLY (N+1)*ITER.
C
C          SSQ  THE SUM OF THE SQUARES OF THE RESIDUALS FOR THE CURRENT
C               X AT RETURN.
C
C          F    A REAL ARRAY OF LENGTH M WHICH CONTAINS THE FINAL VALUE
C               OF THE RESIDUALS (THE F(I)'S) .
C
C
C       EXPLANATION OF PARAMETERS ----
C
C               X       CURRENT X VECTOR
C               N       NUMBER OF UNKNOWNS
C               ICONV   CONVERGENCE INDICATOR (SEE ABOVE)
C               ITER    NUMBER OF THE CURRENT ITERATION
C               SSQ     THE NUMBER OF THE SQUARES OF THE RESIDUALS FOR THE
C               CURRENT X
C               ERL2    THE EUCLICEAN NORM OF THE GRADIENT FOR THE CURRENT X
C               GRAD    THE REAL ARRAY OF LENGTH N CONTAINING THE GRADIENT
C               AT THE CURRENT X
C
C               NOTE ----
C
C               N AND ITER MUST NOT BE CHANGED IN PRNOUT
C               X AND ERL2 SHOULD NOT BE CAPRICIOUSLY CHANGED.
C
C
C
C       S.H. BRYANT ---- REVISION MAY 12, 1979  ----
C
C       DEPARTMENT OF PHARACOLOGY AND CELL BIOPHYSICS,
C       COLLEGE OF MEDICINE,
C       UNIVERSITY OF CINCINNATI,
C       231 BETHESDA AVE.,
C       CINCINNATI,
C       OHIO. 45267.
C       TELEPHONE 513/ 872-5621. }

{       Initialisation }


var
   i,j,jk,k,kk,l,jacss,jacm1,delend,GRADSS,GRDEND,GRDM1,FPLSS,FPLM1 : LongInt ;
   DIAGSS,DIAGM1,ENDSS,FMNSS,XBADSS,XBEND,IBAD,NP1,ISW : LongInt ;
   Iis,JS,LI,Jl,JM,KQ,JK1,LIM,JN,MJ : LongInt ;
   PREC,REL,DTST,DEPS,RELCON,RELSSQ,GCrit,ERL2,RN,OldSSQ,HH,XDABS,ParHold,SUM,TT : Single ;
   RHH,DNORM : Single ;
   Quit,Singular,retry,Converged : Boolean ;
   Work : ^TWork ;
begin

{ Set machine precision constants }
      PREC := 0.01 ;
      REL := 0.005 ;
      DTST := SQRT1(PREC) ;
      DEPS := SQRT1(REL) ;

      { Set convergence limits }
    {  RELCON := 10.**(-NUMSIG) ;
      RELSSQ := 10.**(-NSIGSQ) ; }
       RELCON := 1E-4 ;
       RELSSQ := 1E-4 ;

      { Set up pointers into WORK buffer }

        JACSS := 1+(nPars*(nPars+1)) div 2 ;
        JACM1 := JACSS-1 ;
        DELEND := JACM1 + nPars ;
        { Gradient }
        GRADSS := JACSS+nPars*nPoints ;
        GRDM1 := GRADSS-1 ;
        GRDEND := GRDM1 + nPars ;
        { Forward trial residuals }
        FPLSS := GRADSS ;
        FPLM1 := FPLSS-1 ;
        { Diagonal elements of Jacobian }
        DIAGSS := FPLSS + nPoints ;
        DIAGM1 := DIAGSS - 1 ;
        ENDSS := DIAGM1 + nPars ;
        { Reverse trial residuals }
        FMNSS := ENDSS + 1 ;
        XBADSS := FMNSS + nPoints ;
        XBEND := XBADSS + nPars - 1 ;
        ICONV := -5 ;
        ERL2 := 1.E35 ;
        GCRIT := 1.E-3 ;
        IBAD := -99 ;
        RN := 1. / nPars ;
        NP1 := nPars + 1 ;
        ISW := 1 ;
        ITER := 1 ;

        New( Work ) ;

        { Iterative loop to find best fit parameter values }

        Quit := False ;
        While Not Quit do begin

            { Compute sum of squares
              SSQ :=  W * (Ydata - Yfunction)*(Ydata - Yfunction) }
            SSQ := SSQCAL(Pars,nPoints,nPars,F,1,W,Equation,Data) ;

            { Convergence test - 2 Sum of squares nPointsatch to NSIGSQ figures }
            IF ITER <> 1 then begin {125}
                 IF ABS(SSQ-OLDSSQ) <= (RELSSQ*MaxFlt([ 0.5,SSQ])) then begin
                       ICONV := 2 ;
                       break ;
                       end ;
                 end ;
            OLDSSQ := SSQ ;{125}

            { Compute trial residuals by incrementing
              and decrementing X(j) by HH j := 1...N
              R  :=  Zi (Y(i) - Yfunc(i)) i := 1...M }
            K := JACM1 ;
            for J := 1 to nPars do begin

                  { Compute size of increment in parameter }
                  XDABS := ABS(Pars.Value[J]) ;
                  HH := REL*XDABS ;
                  if ISW = 2 then HH := HH*1.E3 ;
                  if HH <= PREC then HH := PREC ;

                  { Compute forward residuals Rf  :=  X(J)+dX(J) }
                  ParHold := Pars.Value[J] ;
                  Pars.Value[j] := Pars.Value[j] + HH ;
                  FUNC(Pars, nPoints, nPars, Work^,FPLSS,Equation,Data) ;
                  Pars.Value[j] := ParHold ;

                  { ISW = 1 then skip reverse residuals }
                  IF ISW <> 1 then begin {GO TO 16 }
                         { Compute reverse residual Rr  :=  Pars[j]  -  dPars[j] }
                       Pars.Value[j] := ParHold - HH ;
                       FUNC(Pars, nPoints, nPars, Work^,FMNSS, Equation, Data ) ;
                       Pars.Value[j] := ParHold ;

                       { Compute gradients (Central differences)
                       Store in JACSS  -  GRDM1
 		       SQRT1(W(j))(Rf(j)  -  Rr)j))/2HH
                       for j := 1..M and  X(i) i := 1..N }

                       L := ENDSS ;
                       RHH := 0.5/HH ;
                       KK := 0 ;
                       for I := FPLSS to DIAGM1 do begin
                           L := L + 1 ;
                           K := K + 1 ;
                           KK := KK + 1 ;
			   Work^[K] := SQRT1(W[KK])*(Work^[I] - Work^[L])*RHH ;
                           end ;
                       end
                  else begin
                        { 16 }
                       { Case of no reverse residuals
                       Forward difference
                       G := SQRT1(W(j)(Rf(j)  -  Ro(j))/HH
                       j := 1..M X(i) i := 1..N }

                       L := FPLM1 ;
                       RHH := 1./HH ;
                       for I := 1 to nPoints do begin
                           K := K + 1 ;
                           L := L + 1 ;
			   Work^[K] := (SQRT1(W[I])*Work^[L] - F[I])*RHH ;
                           end ;
                       end ;
                  end ;
        {20 }
{22      CONTINUE}

{C
C       G2 :=  Z W(j)* ((Rf(j) - Rr(j))/2HH) * Ro(j)
C          j := 1..M
C
C       ERL2  :=  Z G2
C          i := 1..N
C }
            ERL2 := 0. ;
            K := JACM1 ;
            for I := GRADSS to GRDEND do begin
                  SUM := 0. ;
                  for  J := 1 to nPoints do begin
                        K := K + 1 ;
                        SUM := SUM + Work^[K]*F[J] ;
                        end ;
                  Work^[I] := SUM ;
                  ERL2 := ERL2 + SUM*SUM ;
                  end ;

            ERL2 := SQRT1(ERL2) ;

            { Convergence test - 3 Euclidian norm < DELTA }
            IF(ERL2 <= DELTA) then begin
                 ICONV := 3 ;
                 break ;
                 end ;
            IF(ERL2 < GCRIT) then ISW := 2 ;

            { Compute summed cross - products of residual gradients
              Sik  :=  Z Gi(j) * Gk(j)   (i,k := 1...N)
             j := 1...M S11,S12,S22,S13,S23,S33,..... }
            repeat
                  Retry := False ;
                  L := 0 ;
                  Iis := JACM1 - nPoints ;
                  for I := 1 to nPars do begin
                      Iis := Iis + nPoints ;
                      JS := JACM1 ;
                      for J := 1 to I do begin
                          L := L + 1 ;
                          SUM := 0. ;
                          for K := 1 to nPoints do begin
                                LI := Iis + K ;
                                JS := JS + 1 ;
                                SUM := SUM + Work^[LI]*Work^[JS] ;
                                end ;
                          SLTJJ[L] := SUM ;
                          Work^[L] := SUM ;
                          end ;
                      end ;

                  { Compute normalised diagonal matrix
                   SQRT1(Sii)/( SQRT1(Zi (Sii)**2) ) i := 1..N }

                  L := 0 ;
                  J := 0 ;
                  DNORM := 0. ;
                  for I := DIAGSS to ENDSS do begin {34}
                      J := J + 1 ;
                      L := L + J ;
                      Work^[I] := SQRT1(Work^[L]) ;
                      DNORM := DNORM + Work^[L]*Work^[L] ;
                      end ;
                  DNORM := 1./SQRT1(MinFlt([DNORM,3.4E38])) ;
                  for I := DIAGSS to ENDSS do Work^[I] := Work^[I]*DNORM ;

                  { Add ERL2 * Nii i := 1..N
                    Diagonal elements of summed cross - products }

                  L := 0 ;
                  K := 0 ;
                  for J := DIAGSS to ENDSS do begin
                      K := K + 1 ;
                      L := L + K ;
                      Work^[L] := Work^[L] + ERL2*Work^[J] ;
                      IF(IBAD > 0) then Work^[L] := Work^[L]*1.5 + DEPS ;
                      end ;

                  JK := 1 ;
                  Singular := False ;
                  JK1 := 0 ;
                  for I := 1 to nPars do begin {52}
                      JL := JK ;
                      JM := 1 ;
                      for J := 1 to I do begin {52}
                          TT := Work^[JK] ;
                          IF(J <> 1) then begin
                               for K := JL to JK1 do begin
                                   TT := TT - Work^[K]*Work^[JM] ;
                                   JM := JM + 1 ;
                                   end ;
                               end ;
                          IF(I = J) then begin
                               IF (Work^[JK] + TT*RN) <= Work^[JK] then
                                  Singular := True ;{GO TO 76}
		               Work^[JK] := 1./SQRT1(TT) ;
                               end
                          else Work^[JK] := TT*Work^[JM] ;
                          JK1 := JK ;
                          JM := JM + 1 ;
                          JK := JK + 1 ;
                          end ;
                          if Singular then Break ;
                      end ;

                  if Singular then begin

                     { Singularity processing 76 }
                     IF IBAD >= 2 then ReTry := False {GO TO 92}
                     else if iBad < 0 then begin
                          iBad := 0 ;
                          ReTry := True ;
                          {IF(IBAD) 81,78,78 }
                          end
                     else begin
                          J := 0 ; {78}
                          ReTry := False ;
                          for I := XBADSS to XBEND do begin{80}
                              J := J + 1 ;
                              IF(ABS(Pars.Value[j] - Work^[I]) > MaxFlt(
                                          [DTST,ABS(Work^[I])*DTST]) ) then
                                              ReTry := True ;
                              end ; {80}
                          end ;
                     end ;

                  if ReTry then begin
                     J := 0 ; {82}
                     for I := XBADSS to XBEND do begin
                         J := J + 1 ;
                         Work^[I] := Pars.Value[j]
                         end ;
                     IBAD := IBAD + 1 ;
                     end ;
                  until not ReTry ;

            JK := 1 ;
            JL := JACM1 ;
            KQ := GRDM1 ;
            for I := 1 to nPars do begin {60}
                  KQ := KQ + 1 ;
                  TT := Work^[KQ] ;
                  IF JL <> JACM1 then begin
                     JK := JK + JL - 1 - JACM1 ;
                     LIM := I - 1 + JACM1 ;
                     for J := JL to LIM do begin
                         TT := TT - Work^[JK]*Work^[J] ;
                         JK := JK + 1 ;
                         end ;
                     end
                  else begin
                     IF(TT <> 0. ) then JL := JACM1 + I ;
                     JK := JK + I - 1 ;
                     end ;
                  Work^[JACM1 + I] := TT*Work^[JK] ;
                  JK := JK + 1 ;
                  end ; {60}

            for I := 1 to nPars do begin{66}
                  J := NP1 - I + JACM1 ;
                  JK := JK - 1 ;
                  JM := JK ;
                  JN := NP1 - I + 1 ;
                  TT := Work^[J] ;
                  IF (nPars >= JN) then begin {GO TO 64}
                     LI := nPars + JACM1 ;
                     for MJ := JN to nPars do begin
                         TT := TT - Work^[JM]*Work^[LI] ;
                         LI := LI - 1 ;
                         JM := JM - LI + JACM1 ;
                         end ;
                     end ; {64}
                  Work^[J] := TT*Work^[JM] ;
                  end ; {66}

            IF (IBAD <>  - 99 ) then IBAD := 0 ;
            J := JACM1 ;
            for I := 1 to nPars do begin {68}
                  J := J + 1 ;
                  Pars.Value[I] := Pars.Value[I] - Work^[J] ;
                  end ; {68}

            { Convergence condition  -  1
             Xnew  :=  Xold to NUMSIG places 5E - 20 V1.1 .5 in V1. }
            Converged := True ;
            J := JACM1 ;
            for I := 1 to nPars do begin {70}
                  J := J + 1 ;
                  IF ABS(Work^[J]) > (RELCON*MaXFlt([0.5,ABS(Pars.Value[I])])) then
                                  Converged := False ;
                  end ;

            if Converged then begin
                 ICONV := 1 ;
                 Quit := True ;
                 end ;

            ITER := ITER + 1 ;
            IF (ITER > ITMAX) then Quit := True ;
            end ;

        SSQ := SSQCAL(Pars,nPoints,nPars,F,1,W,Equation,Data) ;
        Dispose(Work) ;
        end ;


function SSQCAL( const Pars : TPars ;
                 nPoints,nPars : LongInt ;
                 var Residuals : Array of Single ;
                 iStart : LongInt ;
                 const W : Array of Single ;
                 const Equation : TEquation ;
                 const Data : TXYData ) : Single ;

       { Compute sum of squares of residuals }
       { Enter with :
         Pars = Array of function parameters
         nPoints = Number of data points to be fitted
         nPars = Number of parameters in Pars
         Residuals = array of residual differences
         W = array of weights
         Equation = Equation to be fitted to data
         Data = Data to be fitted (array of x,y points) }
var
   I : LongInt ;
   SSQ : single ;
begin
	Func(Pars,nPoints,nPars,Residuals,iStart,Equation, Data ) ;
        SSQ := 0. ;
        for I := 1 to nPoints do begin
		Residuals[I] := SQRT1(W[I])*Residuals[iStart+I-1] ;
                SSQ := SSQ + Sqr(Residuals[iStart+I-1]) ;
                end ;
        SSQCAL := SSQ ;
        end ;


procedure Func(
          Const Pars :TPars ;
          nPoints,nPars : LongInt ;
          Var Residuals : Array of Single ;
          iStart : LongInt ;
          Equation : TEquation ;
          Const Data : TXYData ) ;
var
   i : Integer ;
begin
     { Un-map parameters from compressed array to normal }
     for i := 1 to nPars do Equation.Par[Pars.Map[i]] := Pars.Value[i] ;
     { Convert log-transformed parameters to normal }
     for i := 0 to nPars-1 do if MathFunc.IsLogParameters[i] then
         Equation.Par[i] := Exp(Equation.Par[i]) ;

     { Set function parameters }
     for i := 0 to nPars-1 do MathFunc.Parameters[i] := Equation.Par[i] ;
     
     if Equation.UseBinWidths then begin
        { Apply bin width multiplier when fitting probability density
          functions to histograms }
        for i := 0 to nPoints-1 do
            Residuals[iStart+I] := Data.y[I]
                                   - (Data.BinWidth[i]*MathFunc.Value(Data.x[I])) ;
        end
     else begin
        { Normal curve fitting }
        for i := 0 to nPoints-1 do
            Residuals[iStart+I] := Data.y[I]
                                   - MathFunc.Value(Data.x[I]) ;
        end ;
     end ;


procedure STAT(
          nPoints,nPars :LongInt ;
          var F,Y,W,SLT : Array of Single ;
          var SSQ : Single;
          var SDPars : Array of Single ;
          var SDMIN,R : Single ;
          var XPAR : Array of Single ) ;
{C
C       J.DEMPSTER 1 - FEB - 82
C       Adapted from STAT by S.H. Bryant
CC      Subroutine to supply statistics for non - linear least -
C       squares fit of tabular data  by SSQMIN.
C       After minminsation takes J(TRANSPOSE)*J matrix from
C       ssqmin which is stored serially as a lower super tr -
C       angle in SLTJJ(1) through SLTJJ(JACM1). Creates full
C       matrix in C(N,N) which is then inverted to give the var -
C       iance/covariance martix from which standard deviances
C       and correlation coefficients are calculated by the
C       methods of Hamilton (1964).  Hamilton's R is calculated from
C       the data and theoretical values
C
C       Variables in call string:
C
C       M        - Integer no. of residuals (observations)
C       N        - Integer no. of fitted parameters
C       F        - Real array of length M which contains the
C                final values of the residuals
C       Y        - Real array of length M containing Y data
C       W        - Real weighting array of length M
C       SLT      - Real array of length N*(N + 1)/2
C                on input stores lower super triangle of
C                J(TRANS)*J from SLTJJ in SSQMIN
C                on return contains parameter corr. coeffs.
C                as CX(1,1),CX(2,1),CX(2,2),CX(3,1)....CX(N,N)
C       SSQ      - Final sum of squares of residuals
C       SDX      - REal array of length N containing the % standard
C                deviations of each parameter X
C       SDMIN    -        Minimised standard deviation
C       R        -        Hamilton's R
C       XPAR     -        Fitted parameter array
C
C
C       Requires matrix inversion srtn. MINV
C
        DIMENSION Y(M),SLT(1),SDX(N),C(8,8),A(64)
C
        REAL F(M),W(M),XPAR(N)
	INTEGER LROW(8),MCOL(8)
 }
 var
        I,J,L : LongInt ;
        LROW,MCOL : Array[0..8] of LongInt ;
        C : Array[1..8,1..8] of Single ;
        A : Array[0..80] of Single ;
        SUMP,YWGHT,DET : Single ;
 begin
	SDMIN := SQRT1( (SSQ/(nPoints - nPars)) ) ;
        SUMP := 0. ;
        for I := 1 to nPoints do begin
	    YWGHT := Y[I-1]*SQRT1(W[I]) ;
            SUMP := SUMP + Sqr(F[I] + YWGHT) ;
            end ;
        R := SQRT1(MinFlt([3.4E38,SSQ])/SUMP) ;

        { Restore J(TRANSP)*J and place in C(I,J) }

        L := 0 ;
        for I := 1 to nPars do begin
            for J := 1 to I do begin
                L := L + 1 ;
                C[I,J]  :=  SLT[L] ;
                end ;
            end ;

        for I := 1 to nPars do
            for J := 1 to nPars do
                IF (I < J) then C[I,J]  :=  C[J,I] ;

        { Invert C(I,J) }
        L := 0 ;
        for J := 1 to nPars do begin
            for I := 1 to nPars do begin
                L := L + 1 ;
                A[L] := C[I,J] ;
                end ;
            end ;

        MINV (A,nPars,DET,LROW,MCOL) ;

        L := 0 ;
        for J := 1 to nPars do begin
            for I := 1 to nPars do begin
                L := L + 1 ;
                C[I,J] := A[L] ;
                end ;
            end ;

        { Calculate std. dev. Pars[j] }

        for J  :=  1 to nPars do SDPars[j]  :=  SDMIN * SQRT1(ABS(C[J,J])) ;


{C	*** REMOVED since causing F.P. error and not used
C       Calculate correlation coefficients for
C       X(1) on Pars[j]. Return in lower super
C       triangle as X(1,1),X(2,2),X(3,1),X(3,2) ....
C

C	 L := 0
C	 DO 7 I := 1 to N
C	 DO 7 J := 1,I
C	 L := L + 1
C	 SLT(L) := C(I,J)/SQRT1(C(I,I)*C(J,J))
C7	 CONTINUE
	 RETURN}
         end ;


procedure MINV(var A : Array of Single ;
               N : LongInt ;
               var D : Single ;
               var L,M : Array of LongInt ) ;
{
C           A  -  INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C               RESULTANT INVERSE.
C           N  -  ORDER OF MATRIX A
C           D  -  RESULTANT DETERMINANT
C           L  -  Work^ VECTOR OF LENGTH N
C           M  -  Work^ VECTOR OF LENGTH N
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C
C        METHOD
C           THE STANDARD GAUSS - JORDAN METHOD IS USED. THE DETERMINANT
C           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
C           THE MATRIX IS SINGULAR.
C
}
var
   NK,K,I,J,KK,IJ,IK,IZ,KI,KJ,JP,JQ,JR,JI,JK : LongInt ;
   BIGA,HOLD : Single ;
begin

      D := 1.0 ;
      NK :=  -N ;
      for K := 1 to N do begin {80}
          NK := NK + N ;
          L[K] := K ;
          M[K] := K ;
          KK := NK + K ;
          BIGA := A[KK] ;
          for J := K to N do begin{20}
              IZ := N*(J - 1) ;
              for I := K to N do begin {20}
                  IJ := IZ + I ;
                  IF( ABS(BIGA) -  ABS(A[IJ])) < 0. then begin {15,20,20}
                      BIGA := A[IJ] ;
                      L[K] := I ;
                      M[K] := J ;
                      end ;
                  end ;
              end ;

          { INTERCHANGE ROWS }

          J := L[K] ;
          IF(J - K) > 0. then begin {35,35,25}
               KI := K - N ;
               for I := 1 to N do begin {30}
                   KI := KI + N ;
                   HOLD :=  - A[I] ;
                   JI := KI - K + J ;
                   A[KI] := A[JI] ;
                   A[JI]  := HOLD ;
                   end ; {30}
               end ;

          { INTERCHANGE COLUMNS }

          I := M[K] ; {35}
          IF(I - K) > 0. then begin
               JP := N*(I - 1) ;
               for J := 1 to N do begin {40}
                   JK := NK + J ;
                   JI := JP + J ;
                   HOLD :=  - A[JK] ;
                   A[JK] := A[JI] ;
                   A[JI]  := HOLD
                   end ;{40}
               end ;

         { DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
          CONTAINED IN BIGA }

          IF BIGA = 0. then begin
                  D := 0.0 ;
                  break ;
                  end ;

          for I := 1 to N do begin {55}
              IF(I - K) <> 0 then begin {50,55,50}
                   IK := NK + I ;
                   A[IK] := A[IK]/( -BIGA) ;
                   end ;
              end ; {55}

         { REDUCE MATRIX }

         for I := 1 to N do begin {65}
             IK := NK + I ;
             HOLD := A[IK] ;
             IJ := I - N ;
             for J := 1 to N do begin {65}
                 IJ := IJ + N ;
                 IF(I - K) <> 0 then begin {60,65,60}
                      IF(J - K) <> 0 then begin {62,65,62}
                           KJ := IJ - I + K ;
                           A[IJ] := HOLD*A[KJ] + A[IJ] ;
                           end ;
                      end ;
                 end ;
             end ; {65}

         { DIVIDE ROW BY PIVOT }

         KJ := K - N ;
         for J := 1 to N do begin {75}
             KJ := KJ + N ;
             IF(J <> K) then {70,75,70} A[KJ] := A[KJ]/BIGA ;
             end ; {75}

        { PRODUCT OF PIVOTS }

        D := D*BIGA ;

        { REPLACE PIVOT BY RECIPROCAL }

        A[KK] := 1.0/BIGA ;
        end ;

      { FINAL ROW AND COLUMN INTERCHANGE }

      K := N - 1 ;
      while (K>0) do begin {150,150,105}
              I := L[K] ;{105}
              IF(I - K) > 0 then begin {120,120,108}
                   JQ := N*(K - 1) ; {108}
                   JR := N*(I - 1) ;
                   for J := 1 to N do begin {110}
                       JK := JQ + J ;
                       HOLD := A[JK] ;
                       JI := JR + J ;
                       A[JK] :=  -A[JI] ;
                       A[JI] := HOLD ;
                       end ; {110}
                   end ;
              J := M[K] ;{120}
              IF(J - K) > 0 then begin {100,100,125}
                   KI := K - N ; {125}
                   for I := 1 to N do begin {130}
                       KI := KI + N ;
                       HOLD := A[KI] ;
                       JI := KI - K + J ;
                       A[KI] :=  -A[JI] ;
                       A[JI]  := HOLD ;
                       end ; {130}
                   end ;
              K := (K - 1) ;
              end ;

      end ;


FUNCTION SQRT1 ( R : Real ) : Real ;
begin
     SQRT1  :=  SQRT( MINFlt([R,MaxSingle]) ) ;
     end ;



function erf(x : Single ) : Single ;
{ --------------
  Error function
  --------------}
var
   t,z,y,erfx : single ;
begin
        if x < 10. then begin
	   z := abs( x )  ;
	   t := 1./( 1. + 0.5*z ) ;
	   y := t*exp( -z*z - 1.26551223 +
      	        t*(1.00002368 + t*(0.37409196 + t*(0.09678418 +
      	        t*( -0.18628806 + t*(0.27886807 + t*( -1.13520398 +
      	        t*(1.48851587 + t*( -0.82215223 + t*0.17087277 ))))))))) ;

           if ( x < 0. ) then y := 2. - y ;
	   erfx := 1. - y ;
           end
        else erfx := 1. ;
        erf := erfx ;
        end ;

end.
