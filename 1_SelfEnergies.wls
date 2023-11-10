#!/usr/bin/env wolframscript

(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCD contributions to the Higgs Boson Mass															*)

(*
	
	This software is covered by the GNU General Public License 3.
	Copyright (C) 2022 Edilson A. Reyes R.
	
*)

(* :Summary:	This is a Mathematica package for the calculation and evaluation
		of the Higgs boson mass corrections at three-loop level in the 
		mixed top Yukawa - QCD sector of the SM.   *)

(* ------------------------------------------------------------------------ *)

$FeynCalcStartupMessages = False;
$LoadAddOns={"FeynArts"};
<<FeynCalc`;
$FAVerbose = 0;
Off[Paint::nolevel];


LaunchKernels[4]; 
Print["Number of Kernels: ", Length[ Kernels[] ] ]

ParallelEvaluate[
$FeynCalcStartupMessages = False;
$LoadAddOns={"FeynArts"};
<<FeynCalc`;
$FAVerbose = 0;
Off[Paint::nolevel]
];


TP = CreateTopologies[3, 1 -> 1,
                         ExcludeTopologies -> Internal,
			             Adjacencies -> 3];


Print["Number of Topologies = ",Length[TP]]


Paint[TP,FieldNumbers-> True,ColumnsXRows->{5,2}]


EFields = {S[1]};
$short1=Length[EFields];

InsFTP[top_,f1_,f2_,self_,excludef_] := InsertFields[ top,
				                      f1 -> f2,
						              Model -> "SMQCD",
                                      InsertionLevel -> Particles,
						              ExcludeParticles -> excludef,
						              LastSelections -> self]


selfields={};
excludefields={F[1, _], F[2, _],        
	       V[1], V[2], V[3], 
	       U[1 | 2 | 3 | 4 ]};


tpatt = F[3, {3, _}] | -F[3, {3, _}];
bpatt = F[4, {3, _}] | -F[4, {3, _}];  
qpatt = Flatten[ tpatt | bpatt ];
gpatt = V[5, _];
spatt = S[1] | S[2] | S[3];


SESelRules = {
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[4] -> tpatt] && FreeQ[#, Field[8]-> qpatt]
   && MemberQ[#, Field[5|8] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[10] -> tpatt] 
   && MemberQ[#, Field[6|8] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[5] -> tpatt] 
   && MemberQ[#, Field[4|6|8|9] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3|10] -> tpatt] && MemberQ[#, Field[4|6] -> tpatt]
   && MemberQ[#, Field[5|9] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[7|9] -> gpatt] 
  && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[8]-> qpatt] && FreeQ[#, Field[9] -> bpatt]
   && MemberQ[#, Field[5|7|9|10] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[5] -> tpatt]
   && MemberQ[#, Field[6|9] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[4] -> tpatt] && MemberQ[#, Field[6] -> tpatt]
   && MemberQ[#, Field[5|8] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[6|9] -> gpatt] 
  && MemberQ[{Count[LoopFields[##],V[5,_]]},1])&,
  (MemberQ[#, Field[3] -> tpatt] && MemberQ[#, Field[6] -> tpatt]
   && MemberQ[#, Field[5|9] -> gpatt] && MemberQ[{Count[LoopFields[##],V[5,_]]},1])& };


DiagSelHiggsSE[i_,j_] := Parallelize[MapThread[
					                 DiagramSelect[ InsFTP[ Take[TP,{#1}], 
					                                EFields[[i]], 
					                                EFields[[j]], 
					                                selfields, 
					                                excludefields], #2]&,
                                                    {Range[Length[TP]], SESelRules}]]


Print["This wls script is in: ", Directory[]]
SetDirectory[Directory[]];
If[!DirectoryQ["outputs"], CreateDirectory["outputs"]];
Print["Directorio outputs creado"]


Table[If[!DirectoryQ[ ToFileName[{"outputs","HiggsSE",StringJoin["Top",ToString[l]]}]],
          CreateDirectory[ToFileName[{"outputs","HiggsSE", StringJoin["Top",ToString[l]]}]]], 
          {l, 1, Length[SESelRules]}];


PaintHiggsSE[dir1_,dir2_,dir3_,name_,top_,level_, tittle_] := Paint[ top,
								      PaintLevel -> level,
								      FieldNumbers -> True,
								      ColumnsXRows -> {5,5},
								      Numbering -> Simple,
								      SheetHeader -> tittle,
								      DisplayFunction -> (Export[ToFileName[{dir1,dir2,dir3}, StringJoin[dir2,name]],#]&)]

DistributeDefinitions[PaintHiggsSE];


HiggsSE=DiagSelHiggsSE[1,1];
Parallelize[MapThread[
		      PaintHiggsSE["outputs", "HiggsSE", 
		                   StringJoin["Top",ToString[#1]],
				           "Top"<>ToString[#1]<>".ps", #2, 
				           Particles,"Three-loop Higgs Selfenergy Diagrams"]&,
		      {Range[Length[SESelRules]], HiggsSE}]];


TxtFile[dir1_,dir2_,dir3_,Amp_,name_] := Put[Amp, ToFileName[{dir1,dir2,dir3},
                                                                StringJoin[dir2,name]]];
DistributeDefinitions[TxtFile];


Amps=ParallelMap[CreateFeynAmp[#,AmplitudeLevel -> {Particles},
                              PreFactor -> 1,
                              GaugeRules -> {},
                              Truncated -> True]&,
                  HiggsSE];


Print["There are in total ",
      Sum[Length[Amps[[j]]],{j,1,Length[SESelRules]}],
      " amplitude diagrams to evaluate! "];


Map[TxtFile["outputs","HiggsSE", 
            StringJoin["Top",ToString[#]],
            Amps[[#]], "Amps"<>ToString[#]]&,
		    Range[Length[Amps]]];

Print["FA amplitudes saved !!"]


CloseKernels[];

Print["The End!"];

Quit[];
