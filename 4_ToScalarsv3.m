#!/Path/to/MathematicaScript -script

(**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++**)
(** In this routine the numerators of the Feynman integrals are rewritten     **)
(** in terms of inverse propagators. Partial fractioning is done when is      **)
(** required. A canonical basis for planar and non planar integrals is chosen.**) 
(** Besides, the obtained scalar integrals are exported to the language       **)
(** of Reduze (https://reduze.hepforge.org/).                                 **)
(**---------------------------------------------------------------------------**)


PrependTo[$Path, ToFileName[{"/Path","to","FeynCalc"}]];

$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;

Off[DeleteFile::nffil];
Off[ParallelCombine::nopar1];
Off[Simplify::time];
SetOptions[Simplify, TimeConstraint -> 10];



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
SetOptions[Simplify, TimeConstraint -> 10];

];


na1 = ToExpression[$ScriptCommandLine[[2]]]; (* Topology Number *)
na2 = ToExpression[$ScriptCommandLine[[3]]]; (* planar = 0, nonplanar = 1 *) 


TempDirName = ToFileName[{"/Path","to","outputs","HiggsSE"}];


Amps[l_] := Get["RegulAmps"<>ToString[l],
		Path->{ToFileName[{TempDirName, 
		                   ToString[Top]<>ToString[l]}]}];

Prop[amp_] := 
 If[amp === 0, 0, Part[Cases[amp, FeynAmpDenominator[__]], 1] ]

PFandBasis[amp_] := 
 Module[ {kpm, ChangetoBasis},
  kpm = {k1, k2, k3, k4, k5, k6, k7, k8, k9}; 
  ChangetoBasis = {Momentum[q1, D] -> Momentum[kpm[[1]], D], 
    Momentum[q2, D] -> Momentum[kpm[[2]], D], 
    Momentum[q3, D] -> Momentum[kpm[[3]], D], 
    Momentum[q1, D] + Momentum[q2, D] -> Momentum[kpm[[4]], D], 
    Momentum[q1, D] + Momentum[q3, D] -> Momentum[kpm[[5]], D], 
    Momentum[q2, D] - Momentum[q3, D] -> Momentum[kpm[[6]], D], 
    Momentum[p, D] + Momentum[q1, D] -> Momentum[kpm[[7]], D], 
    Momentum[p, D] - Momentum[q2, D] -> Momentum[kpm[[8]], D], 
    Momentum[p, D] - Momentum[q3, D] -> Momentum[kpm[[9]], D]};
  ReplaceAll[
     Prop[amp] /. {PropagatorDenominator[a__, b_] :> 
         PropagatorDenominator[Simplify[Abs[a]], b]} // 
      Apart2 , {PropagatorDenominator[Abs[a_], b_] :> 
       PropagatorDenominator[a, b]}] //. ChangetoBasis // Expand ]

MissingProp[amp_] := 
 Module[ {kpm},
  kpm = {k1, k2, k3, k4, k5, k6, k7, k8, k9}; 
  Flatten[Cases[
    Complement[kpm, 
     Union@Cases[PFandBasis[amp], 
       PropagatorDenominator[Momentum[a_, D], _] :> a, Infinity]], 
    a_ :> PropagatorDenominator[Momentum[a, D], 0]^{\[Nu]}, 
    Infinity]]]


(*************************************************************************************)
(** In this version (v3) the Higgs mass in the propagators is disregarded, Mh -> 0, **)
(** and we have assumed the Landau gauge.                                           **)
(*************************************************************************************)

  ListProp[amp_] := 
  Cases[PFandBasis[amp] /. {GaugeXi[_] -> 0, MB -> 0, Mh -> 0}, 
        PropagatorDenominator[_, _], Infinity]

AllProp[amp_] := 
 Union[Cases[ListProp[amp], 
   PropagatorDenominator[p_, m_] :> 
    Power[(PropagatorDenominator[p, m]), \[Nu] + 
      Count[ListProp[amp], PropagatorDenominator[p, m]]], Infinity], 
  MissingProp[amp]]


Mark[amp_] := Module[{j, MarkP, MarkNP},
  MarkP[j_] := 
   Times @@ 
    Cases[AllProp[j], 
     PropagatorDenominator[Momentum[k_, D], a_]^(b_) :> 
      ToExpression[
         ToString[Pr] <> 
          ToString[Position[AllProp[j], k][[1, 1]]]] @@ {a}^(-b)];
  MarkNP[j_] := 
   Times @@ 
    Cases[AllProp[j], 
     PropagatorDenominator[Momentum[k1_, D] + Momentum[k2_, D], 
        c_]^(d_) :> 
      ToExpression[
         ToString[PrNP] <> 
          ToString[
           Position[AllProp[j], Momentum[k1, D] + Momentum[k2, D]][[1,1]]]] @@ {c}^(-d)];
		     
   MarkP[amp]*MarkNP[amp] ]
		     

ChangesNumij[prop_] :=
  Module[{Propij, Propj, Massij, Massj, den, a, b, c, x, n, m}, 
  Propij[n_, m_] := 
   ToExpression[(ToString[Pr] <> 
      ToString[
       Plus @@ ToExpression[
         StringCases[
          StringCases[ToString[n], x_ -> x][[2]] <> 
           StringCases[ToString[m], x_ -> x][[2]] <> ToString[1], 
          x_ -> x]]])];
  Propj[n_] := 
   ToExpression[(ToString[Pr] <> 
      StringCases[ToString[n], x_ -> x][[2]])];
  Massij[n_, m_] := 
   Cases[prop, Propij[n, m] @@ {c_} :> c, Infinity][[1]];
  Massj[n_] := 
   Cases[prop, Propj[n] @@ {c_} :> c, Infinity][[1]];

     {Pair[Momentum[a_, D], Momentum[b_, D]] :> (1/2)*(-1)^(ToExpression[
         StringCases[ToString[a], x_ -> x][[2]]] + 
        1)*(Propij[a, b] @@ {Massij[a, b]} - Propj[a] @@ {Massj[a]} - 
       Propj[b] @@ {Massj[b]} + Massij[a, b]^2 - Massj[a]^2 - 
       Massj[b]^2)}]

ChangesNumj[prop_] :=
 Module[{Propj, Massj, a, c, x, n},
  Propj[n_] := 
   ToExpression[(ToString[Pr] <> 
      StringCases[ToString[n], x_ -> x][[2]])];
  Massj[n_] := Cases[prop, Propj[n] @@ {c_} :> c, Infinity][[1]];
   {Pair[Momentum[a_, D], 
     Momentum[a_, D]] :> (Propj[a] @@ {Massj[a]} + Massj[a]^2)}]

ChangesNumpj[prop_] :=
  Module[{sign, Proppj, Propj, Masspj, Massj, den, a, b, c, x, n, m},
  sign = {1, -1, -1};
  Proppj[n_] := 
   ToExpression[(ToString[Pr] <> 
      ToString[
       Plus @@ ToExpression[
         StringCases[
          StringCases[ToString[n], x_ -> x][[2]] <> ToString[6], x_ -> x]]])];
  Propj[n_] := 
   ToExpression[(ToString[Pr] <> 
      StringCases[ToString[n], x_ -> x][[2]])];
  Masspj[n_] := Cases[prop, Proppj[n] @@ {c_} :> c, Infinity][[1]];
  Massj[n_] := 
   Cases[prop, Propj[n] @@ {c_} :> c, Infinity][[
    1]]; {Pair[Momentum[p, D], 
     Momentum[b_, 
      D]] :> (1/2)*(sign[[
       ToExpression[
        StringCases[ToString[b], x_ -> x][[2]]]]])*(Proppj[
         b] @@ {Masspj[b]} - Propj[b] @@ {Massj[b]} + Masspj[b]^2 - 
       Massj[b]^2 - Pair[Momentum[p, D], Momentum[p, D]])}]

ChangesNumNP[prop_] :=
  Module[{Proppja, Proppjb, Proppjc, Masspja, Masspjb, Masspjc, den, a,
    c, x, n, m},
  Proppja[n_] := 
   ToExpression[(ToString[PrNP] <> 
      ToString[
       Plus @@ ToExpression[
         StringCases[
          StringCases[ToString[n], x_ -> x][[2]]<>ToString[8], x_ -> x]]])];
  Proppjb[n_] := 
   ToExpression[(ToString[Pr] <> 
      ToString[
       Plus @@ ToExpression[
         StringCases[
          StringCases[ToString[n], x_ -> x][[2]] <> ToString[7], x_ -> x]]])];
  Proppjc[n_] := 
   ToExpression[(ToString[Pr] <> 
      ToString[
       Plus @@ ToExpression[
         StringCases[
          StringCases[ToString[n], x_ -> x][[2]] <> ToString[2], x_ -> x]]])];
  Masspja[n_] := Cases[prop, Proppja[n] @@ {c_} :> c, Infinity][[1]];
  Masspjb[n_] := Cases[prop, Proppjb[n] @@ {c_} :> c, Infinity][[1]];
  Masspjc[n_] := Cases[prop, Proppjc[n] @@ {c_} :> c, Infinity][[1]];

   {Pair[Momentum[p, D], Momentum[q2, D]] :> (1/2)*(Proppja[q2] @@ {Masspja[q2]} - 
       Proppjb[q2] @@ {Masspjb[q2]} - Proppjc[q2] @@ {Masspjc[q2]} + 
       Masspja[q2]^2 - Masspjb[q2]^2 - Masspjc[q2]^2 - 
       Pair[Momentum[p, D], Momentum[q1, D]] + 
       Pair[Momentum[q1, D], Momentum[q3, D]] + 
       Pair[Momentum[q2, D], Momentum[q3, D]])}    ]

Num[amp_] := 
 If[amp === 0, 0, DeleteCases[amp, FeynAmpDenominator[__]] ]

Identifier[c1_, c2_] :=
 Module[{i, j, x, y},
  i = ToExpression[StringCases[ToString[c1], x_ -> x]];
  i = Last[i];
  j = ToExpression[StringCases[ToString[c2], y_ -> y]];
  j = Last[j];
  If[ DigitQ[ToString[i]] == True,
   If[i == j, ToString[ChangesNumj], ToString[ChangesNumij]],
   ToString[ChangesNumpj]] ]

RulesNum[amp_] := 
 Module[{list1, list2, list3, changes, z, a, b},
  list1 = Union@Cases[Num[amp] /. {GaugeXi[_] -> 0, MB -> 0}, Pair[__, __], Infinity];
  list2 = DeleteCases[list1, Pair[Momentum[p, D], Momentum[p, D]]]; 
  changes = {Pair[Momentum[a_, D], Momentum[b_, D]] :> (Pair[Momentum[a, D], Momentum[b, D]] /. 
        ToExpression[Identifier[a, b]]@Mark[amp]) /; 
        DigitQ[Last[StringCases[ToString[b], z_ -> z]]] == True};

	Map[list2[[#]] -> (list2[[#]] /. changes) &, Range[Length[list2]]]    ]


ToScalars[amp_] := ToScalars[amp] =
  Module[{ampp, listt, listf, listp, factor, a, b, j},
   ampp = Times[Num[amp] /. {GaugeXi[_] -> 0, MB -> 0} /. RulesNum[amp] 
       /. {Pair[Momentum[b_, D], Momentum[b_, D]] :> b^2, 
        Eps[___] -> 0} , Mark[amp]] // Expand;
   listt[j_] := Cases[ampp[[j]], 
     ToExpression[(ToString[Pr] <> ToString[a_] <> ToString[__])]];
   listf[j_] := 
    DeleteCases[listt[j], 
     ToExpression[(ToString[Pr] <> ToString[a_])][__]^(b_ /; NumberQ[b] == False)];
   factor[j_] := Times @@ listf[j];
   listp[j_] := Complement[listt[j], listf[j]];
   ampp = Plus @@ Map[
      factor[#]*
        INT @@ Cases[listp[#], 
          ToExpression[(ToString[Pr] <> ToString[__])][a_]^(b_) :> {a, -b /. {\[Nu] -> 0} }] &,
      Range[Length[ampp]]];
   ampp = 
    Collect[ampp, Union@Cases[ampp, INT[__], Infinity], Simplify];
   ampp  ]


ToScalarsNP[amp_] := ToScalarsNP[amp] =
  Module[{ampp, listt, listf, listp, listpNP, factor, a, b, j},
   ampp = Times[Num[amp] /. {GaugeXi[_] -> 0, MB -> 0} /. ChangesNumNP[Mark[amp]] /. RulesNum[amp] 
       /. {Pair[Momentum[b_, D], Momentum[b_, D]] :> b^2, 
        Eps[___] -> 0} , Mark[amp]] // Expand;
   listt[j_] := 
    Cases[ampp[[j]], 
     ToExpression[(ToString[Pr] <> ToString[a_] <> ToString[__])]];
   listf[j_] := 
    DeleteCases[listt[j], 
     ToExpression[(ToString[Pr] <> ToString[a_])][__]^(b_ /; NumberQ[b] == False)];
   factor[j_] := Times @@ listf[j];
   listp[j_] := Complement[listt[j], listf[j]];
   listpNP[j_] := DeleteCases[listp[j], Pr8[__]^(__), Infinity]; 
   ampp = Plus @@ Map[
      factor[#]*
        INT @@ Cases[listpNP[#], 
          ToExpression[(ToString[Pr] <> ToString[__])][a_]^(b_) :> {a, -b /. {\[Nu] -> 0} }] &,
      Range[Length[ampp]]];
   ampp = 
    Collect[ampp, Union@Cases[ampp, INT[__], Infinity], Simplify];
   ampp ]

(*************************************************************************************)
(** The function FCtoReduze[] changes the integrals in the FeynCalc notation to the **)
(** language of Reduze.                                                             **)
(*************************************************************************************)

FCtoReduze[amp_] :=
  Module[{INTtoReduze, lists, masses, FamDiff, listn, name, rules,
          a, b, c, d, e, f, g, h, i, j, k, l, m, n, r, s, t, u,v, w, q},
  lists = Union@Cases[amp, INT[__], Infinity]; 
  masses[b_] := DeleteCases[Union @@ Cases[{lists[[b]]},
        INT[{i_, _}, {j_, _}, {k_, _}, {l_, _}, {m_, _}, {n_, _}, {r_, _}, {s_, _}, {t_, _}]
					   :> {i, j, k, l, m, n, r, s, t}, Infinity], 0];
  FamDiff[b_, i_, j_, k_, l_, m_, n_, r_, s_, t_] :=   
   Map[Flatten @ Position[{i, j, k, l, m, n, r, s, t}, masses[b][[#]]] &, 
    Range[Length[masses[b]]]];
  listn[b_] :=    
    lists[[b]] /. {INT[{i_, _}, {j_, _}, {k_, _}, {l_, _}, {m_, _}, {n_, _}, {r_, _}, {s_, _}, {t_, _}] :> 
                   FamDiff[b, i, j, k, l, m, n, r, s, t]};
   name[b_] := 
   ToString[na2] <> StringJoin @ Table[ ToString[masses[b][[c]]]<>"_"<>StringJoin@Map[ToString[listn[b][[c, #]]] &,
        Range[Length[listn[b][[c]]]]] , {c, 1, Length[masses[b]]}];
  INTtoReduze[b_] := 
   lists[[b]] /. {INT[{i_, d_}, {j_, e_}, {k_, f_}, {l_, g_}, {m_, h_}, {n_, q_}, {r_, u_}, {s_, v_}, {t_, w_}] :>  
      INT @@ {name[b], 
          Length[Cases[{d, e, f, g, h, q, u, v, w}, p_ /; Positive[p],Infinity]],
          Sum[Power[2, 
            Flatten[Position[Sign[{d, e, f, g, h, q, u, v, w}], 1]][[a]] - 1],
           {a, 1, Length[Position[Sign[{d, e, f, g, h, q, u, v, w}], 1]]}],
          Sum[Cases[{d, e, f, g, h, q, u, v, w}, p_ /; Positive[p], Infinity][[a]],
           {a, 1, Length[Cases[{d, e, f, g, h, q, u, v, w}, p_ /; Positive[p], Infinity]]}],
          Abs[Sum[Cases[{d, e, f, g, h, q, u, v, w}, p_ /; Negative[p], Infinity][[a]],
           {a, 1, Length[Cases[{d, e, f, g, h, q, u, v, w}, p_ /; Negative[p], Infinity]]}]], 
          {d, e, f, g, h, q, u, v, w}} };
  rules = Map[lists[[#]] -> INTtoReduze[#] &, Range[Length[lists]]];
    Collect[amp /. rules, INT[__], Simplify]    ]


AmpsReduced[num_] := AmpsReduced[num] =
    If[ num == 0,
	ParallelTable[ FCtoReduze[ToScalars[Amps[na1][[j]] ]], {j, 1, Length[Amps[na1]]} ] ,
	If[ num == 1,
	    ParallelTable[ FCtoReduze[ToScalarsNP[Amps[na1][[j]] ]], {j, 1, Length[Amps[na1]]} ] ,
	    Print[" Choose a value beetween 0 and 1 ! "]
	    ]
	]

	   Put[ AmpsReduced[na2],
		ToFileName[{TempDirName, "Top"<>ToString[na1] }, "ScalarAmpsLandauv3"<>ToString[na1] ]];	   

           Put[ Union@Cases[AmpsReduced[na2], INT[__], Infinity],
                ToFileName[{TempDirName, "Top"<>ToString[na1] }, "ListScalarsLandauv3"<>ToString[na1] ]];

           Put[ Union@Cases[AmpsReduced[na2], INT[a_, ___] :> a, Infinity],
                ToFileName[{TempDirName, "Top"<>ToString[na1] }, "FamiliesLandauv3"<>ToString[na1] ]];


Print["The End!"]

CloseKernels[];

Quit[];

