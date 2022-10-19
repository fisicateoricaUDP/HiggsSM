(*************************************************************************)
(** This routine implements the dimensional regularization of the Higgs **)
(** self-energy amplitudes produced by the code 2_SelfenergyAmps.m      **)
(*************************************************************************)

PrependTo[$Path, ToFileName[{"/Path","to","FeynCalc"}]];

$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;

Off[DeleteFile::nffil];
Off[ParallelCombine::nopar1];
Off[Simplify::time];


LaunchKernels[5];
Print[" Number of Kernels: ", Length[ Kernels[] ] ]


ParallelEvaluate[

PrependTo[$Path, ToFileName[{"/Path","to","FeynCalc"}]];

$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;

Off[DeleteFile::nffil];
Off[ParallelCombine::nopar1];
Off[Simplify::time];

];


TempDirName = ToFileName[{"/Path","to","outputs","HiggsSE"}];

Amps[l_] := Get["FCAmpsTop"<>ToString[l],
		Path->{ToFileName[{TempDirName, ToString[Top]<>ToString[l]}]}];

Changesmom = Module[ {a}, 
                         Off[Part::pspec];
                         {Momentum[a__, D] :> If[Length[a] == 0,
						 Momentum[a, D], 
                                               Plus @@ Map[Momentum[a[[#]], D]&, 
                                               Range[Length[a]]]]}];

Changes1 = {Momentum[q2, D] -> Momentum[q2, D] + Momentum[q3, D], 
            Momentum[q2, D] -> -Momentum[q2, D], 
            Momentum[q2, D] -> -Momentum[q2, D], 
            Momentum[q3, D] -> Momentum[q3, D] + Momentum[q1, D], 
            Momentum[q2, D] -> Momentum[q2, D] + Momentum[q1, D], 
            Momentum[q2, D] -> Momentum[q2, D] + Momentum[q1, D], 
            Momentum[q2, D] -> -Momentum[q2, D], 
            Momentum[q3, D] -> Momentum[q3, D] + Momentum[q2, D], 
            Momentum[q2, D] -> -Momentum[q2, D] + Momentum[q1, D], 
            Momentum[q3, D] -> -Momentum[q3, D] - Momentum[q1, D]};
  
Changes2 = {Momentum[q3, D] -> Momentum[q3, D] - Momentum[q2, D], 
            Momentum[q3, D] -> -Momentum[q3, D], 
            Momentum[q2, D] -> Momentum[q2, D], 
            Momentum[q2, D] -> Momentum[q2, D], 
            Momentum[q3, D] -> Momentum[q3, D] + Momentum[q2, D], 
            Momentum[q3, D] -> Momentum[q3, D] + Momentum[q2, D], 
            Momentum[q2, D] -> Momentum[q2, D], 
            Momentum[q3, D] -> -Momentum[q3, D], 
            Momentum[q3, D] -> -Momentum[q3, D] + Momentum[q1, D], 
            Momentum[q2, D] -> -Momentum[q2, D]};

Changes3 = {{},
	    {},
	    {},
	    {},
	    {Momentum[q1, D] -> Momentum[q4, D], 
             Momentum[q2, D] -> Momentum[q5, D], 
             Momentum[q4, D] -> Momentum[q2, D], 
             Momentum[q5, D] -> Momentum[q1, D]},
	    {Momentum[q1, D] -> Momentum[q4, D],
	     Momentum[q2, D] -> Momentum[q5, D], 
             Momentum[q4, D] -> Momentum[q2, D], 
             Momentum[q5, D] -> Momentum[q1, D]},
	    {},
	    {},
	    {Momentum[q1, D] -> Momentum[q4, D],
	     Momentum[q4, D] -> -Momentum[q1, D]},
	    {}};

AmpsChanged = 
  Map[ Off[ReplaceRepeated::rrlim]; 
   ReplaceRepeated[
     Amps[#] /. Changesmom /. Changes1[[#]] /. Changes2[[#]], 
     Changes3[[#]], MaxIterations -> 2] &, Range[Length[Changes2]]];


  P1[n_, m_] := DeleteCases[AmpsChanged[[n,m]], FeynAmpDenominator[__] | DiracTrace[__]]

  P2[n_, m_] := Cases[AmpsChanged[[n,m]], DiracTrace[a__] :> a] /. {MB -> 0}

  P3[n_, m_] := Part[Cases[AmpsChanged[[n,m]], FeynAmpDenominator[__]], 1]


    SetOptions[DiracSimplify, DiracCanonical -> True, 
                              DiracSubstitute67 -> True, 
                              Expanding -> True, 
                              Factoring -> False];

    SetOptions[DiracTrace, DiracTraceEvaluate -> False];

    $BreitMaison = False;

    ParallelEvaluate[

		     SetOptions[DiracSimplify, DiracCanonical -> True, 
		                               DiracSubstitute67 -> True, 
		                               Expanding -> True, 
		                               Factoring -> False];

                     SetOptions[DiracTrace, DiracTraceEvaluate -> False];

		     $BreitMaison = False;
		     
		     ];

RegulGamma5 = {DiracGamma[5] -> 0, DiracGamma[5] -> 0, 
   DiracGamma[5] -> DiracGamma[5], DiracGamma[5] -> 0, 
   DiracGamma[5] -> 0, DiracGamma[5] -> 0, DiracGamma[5] -> 0, 
   DiracGamma[5] -> 0, DiracGamma[5] -> 0, DiracGamma[5] -> 0};

RegulAmps =
  ParallelTable[
	Times[P1[i,j], Apply[Times,
			     Map[DiracTrace[DiracSimplify[P2[i,j]][[#]] /. {RegulGamma5[[i]]}, 
			                    DiracTraceEvaluate->True]&,
			     Range[Length[P2[i,j]]]]], P3[i,j]],
	{i, 1, Length[Changes1]}, {j, 1, Length[AmpsChanged[[i]]]}];

Print[ " Regul Amps Generated !! " ];


        Table[
	      Put[ RegulAmps[[j]],
		   ToFileName[{TempDirName, "Top"<>ToString[j] }, "RegulAmps"<>ToString[j] ] ],
	      {j, 1, Length[RegulAmps]} ];


Print[ " Regul Amps Saved !! " ];

Print["The End!"]

CloseKernels[];

Quit[];
