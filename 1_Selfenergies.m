(* ::Package:: *)

(****************************************************************************************)
(** We need the Mathematica packages FeynCalc and FeynArts. We call FeynArts patched   **)
(** to Feyncalc and avoid the alert messages.                                          **)
(** Please specify the path where Feyncalc package has been saved                      **)
(****************************************************************************************)
 
PrependTo[$Path, ToFileName[{"/path","to","FeynCalc"}]];

$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;
Off[Paint::nolevel]

(***********************************************************************************)
(** In order to parallelize some applications in the routine we use the function  **) 
(** LaunchKernels[ ].                                                             **)
(***********************************************************************************)  

LaunchKernels[10]; Print["Number of Kernels: ", Length[ Kernels[] ] ]

ParallelEvaluate[

PrependTo[$Path, ToFileName[{"/path","to","FeynCalc"}]];

$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;
Off[Paint::nolevel]

];


(*****************************************************************************************)
(** Generation of diagrams: creation of topologies, definition and insertion of fields  **)
(*****************************************************************************************)


TP = CreateTopologies[3, 1 -> 1, ExcludeTopologies -> Internal, Adjacencies -> 3];
Print["Number of Topologies = ",Length[TP]]


(* (1) *)
(********************************************************************************)
(** We define the function InsFTP[] which will insert the fields on each line  **)
(** with the hel of the FeynArts function InsertFields[__] !                   **)
(********************************************************************************)

EFields = {S[1]};
$short1=Length[EFields];

InsFTP[topol_,field1_,field2_,selfields_,excludefields_] := InsertFields[ topol,
									  field1 -> field2,
									  Model -> "MSSMCT",
                                       InsertionLevel -> Particles,
									  ExcludeParticles -> excludefields,
									  LastSelections -> selfields]
DistributeDefinitions[InsFTP]


selfields={};

excludefields={F[1, _], F[2, _], F[11, _], F[12, _], F[15, _],
	       S[2], S[3], S[5], S[11], S[12], S[13], S[14],
	       V[1], V[2], V[3], V[5], V[6], U[1 | 2 | 3 | 4 | 5 ]}; 
	       (** The above fields are excluded **)


(* (2) *)
(*********************************************************************************************)
(***  Patterns are defined here, they correspond to the fields involved in the computation ***)
(*********************************************************************************************)

tpatt = F[3, {3, _}] | -F[3, {3, _}];
bpatt = F[4, {3, _}] | -F[4, {3, _}];
  
qpatt= Flatten[ tpatt | bpatt ];

(** The above fields correspond to the quarks top and bottom **)


(* (3) *)
(***********************************************************************)
(** Selection rules for 3L Self-Energy diagrams.                      **)
(** Basically we specify which fields can be inserted on each line    **)
(***********************************************************************)

SESelRules = {
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[4] -> tpatt] && MemberQ[#, Field[6] -> tpatt])&,
  (MemberQ[#, Field[3|9] -> tpatt] && MemberQ[#, Field[7|10] -> tpatt])&,
  (MemberQ[#, Field[3|10] -> tpatt] && MemberQ[#, Field[5|7] -> tpatt])&,
  (MemberQ[#, Field[3|10] -> tpatt] && MemberQ[#, Field[4|6] -> tpatt])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[6] -> tpatt])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[8] -> qpatt])&,
  (MemberQ[#, Field[3|7] -> tpatt] && MemberQ[#, Field[5|10] -> tpatt])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[4] -> tpatt] && MemberQ[#, Field[6|7] -> tpatt])&,
  (MemberQ[#, Field[4] -> tpatt] && MemberQ[#, Field[10] -> tpatt])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[6] -> tpatt])&
              };


(* (4) *)
(*************************************************************)
(** Choosing the diagrams for which the function InsFTP[]   **)
(** with the given criteria in'SESelRules' yields True.     **)
(*************************************************************)


DiagSelHiggsSE[i_,j_] := Parallelize[MapThread[
					        DiagramSelect[
						InsFTP[ Take[TP,{#1}], EFields[[i]], EFields[[j]], 
						        selfields, excludefields], #2]&,
                                {Range[Length[TP]], SESelRules}]]

HiggsSEdiag = Flatten[ Table[ DiagSelHiggsSE[i,j], {i,1,1}, {j,1,1}], 1]

Print["Number of Higgs SE = ", Length[HiggsSEdiag[[1]]]];
DistributeDefinitions[HiggsSEdiag];



(***********************************************************)
(* Drawing the Selfenergy Topologies and Exporting to *.ps *)
(***********************************************************)


(** Please define a path where you are going to save the outputs **)
TempDirName = ToFileName[{"/path","to","save","outputs"}];


If[!DirectoryQ[ToFileName[{TempDirName,ToString[SETOP]}]], 
    CreateDirectory[ToFileName[{TempDirName,ToString[SETOP]}]]];

Print["Directory ?/SETOP generated"]

Paint[TP,FieldNumbers->True,ColumnsXRows->{5,5},
Numbering-> Simple, SheetHeader-> "Topologies for the Higgs self-energies", 
DisplayFunction->(Export[ToFileName[{TempDirName,"SETOP"},"SETopologies"<>".ps"],#]&)];


Print["File SETopologies.ps saved in ?/SETOP"]


(* (5) *)
(******************************************************************)
(* Drawing the desired self-energy diagrams and Exporting to *.ps *)
(******************************************************************)


Table[If[ !DirectoryQ[ ToFileName[{TempDirName,"HiggsSE",StringJoin["Top",ToString[l]]}]],
          CreateDirectory[ToFileName[{TempDirName,"HiggsSE", StringJoin["Top",ToString[l]]}]]], 
          {l, 1, Length[SESelRules]}];


PaintHiggsSE[dir1_,dir2_,dir3_,name_,top_,level_, tittle_] := Paint[ top,
								      PaintLevel -> level,
								      FieldNumbers -> True,
								      ColumnsXRows -> {5,5},
								      Numbering -> Simple,
								      SheetHeader -> tittle,
								      DisplayFunction -> (Export[ToFileName[{dir1,dir2,dir3}, 
								                                 StringJoin[dir2,name]],#]&)]

DistributeDefinitions[PaintHiggsSE];


Parallelize[MapThread[
		      PaintHiggsSE[TempDirName, "HiggsSE", StringJoin["Top",ToString[#1]],
				    "Top"<>ToString[#1]<>".ps", #2, Particles,
				    "Three-loop Higgs Selfenergy Eiagrams"]&,
		      {Range[Length[SESelRules]], HiggsSEdiag[[1]] }]];


Print["Files *Top?.ps saved in /SETOP/HiggsSE/Top?"]


(* 6 *)
(**************************************************************************)
(*** Generation of the Amplitudes. The amplitudes are saved in txt files **)
(**************************************************************************)

TxtFile[dir1_,dir2_, dir3_, Amp_, name_] := Put[Amp, ToFileName[{dir1,dir2,dir3},StringJoin[dir2,name]]];
DistributeDefinitions[TxtFile];

Parallelize[MapThread[
		      TxtFile[TempDirName,"HiggsSE",StringJoin["Top",ToString[#1]],
                       CreateFeynAmp[#2, AmplitudeLevel -> {Particles}, 
                                         GaugeRules -> {}, 
                                         Truncated -> True] /. {SB -> CA},
			          ToString[Amps]<>ToString[#1]]&,
		              {Range[Length[SESelRules]], HiggsSEdiag[[1]]}] ];

Print["Amps Generated !!"]


(************************************************)
(** Closing the kernels and leaving the script **)
(************************************************)
  
CloseKernels[];

Print["The End!"];

Quit[];
