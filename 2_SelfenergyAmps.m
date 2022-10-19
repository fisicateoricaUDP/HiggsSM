#!/Path/to/MathematicaScript -script

PrependTo[$Path, ToFileName[{"/Path","to","FeynCalc"}]];


$FeynCalcStartupMessages = False;
$LoadFeynArts = True;
Get["FeynCalc.m"];
$FAVerbose = 0;


Off[DeleteFile::nffil];
Off[ParallelCombine::nopar1];
Off[Simplify::time];


<< SimplificationDefinitionsB`;
(* This Mathematica package can be consulted in http://www.feynhiggs.de/ *)

var = ToExpression[$ScriptCommandLine[[2]]]; 
LaunchKernels[var];
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

<< SimplificationDefinitionsB`;

];

                
      (*******************************************************************)
      (** The regularization procedure is implemented in the next lines **)   
      (*******************************************************************)


TempDirName = ToFileName[{"/Path","to","outputs","HiggsSE"}];

Amp[l_] := Get["HiggsSEAmps"<>ToString[l], 
                Path->{ToFileName[{TempDirName, ToString[Top]<>ToString[l]}]}];


changes = { Mh0tree -> Mh, 
            EL -> (2*MW*SW/MT)*(yt/Sqrt[2]),  
            FASUNT[Index[Gluon, g1_], Index[Gluon, g2_], Index[Colour, o2_], Index[Colour, o1_]] :> 
            FASUNT[Index[Gluon,g1],Index[Colour,o2],c].FASUNT[Index[Gluon,g2],c,Index[Colour,o1]],
            FASUNTSum[Index[Colour, o2_],Index[Colour, o1_],Index[Colour, o4_],Index[Colour, o3_]] :>
            FASUNT[x,Index[Colour,o2],Index[Colour,o1]].FASUNT[x,Index[Colour,o4],Index[Colour,o3]]
          };

SetOptions[FCFAConvert, DropSumOver -> True, 
                        UndoChiralSplittings -> True, 
                        ChangeDimension -> D];
                        
SetOptions[DiracSimplify, DiracSubstitute67 -> True, 
                          Expanding -> True, 
                          Factoring -> True];

ParallelEvaluate[

SetOptions[FCFAConvert,  DropSumOver -> True, 
                         UndoChiralSplittings-> True, 
                         ChangeDimension -> D];
                         
SetOptions[DiracSimplify, DiracSubstitute67 -> True, 
                          Expanding -> True, 
                          Factoring-> True];

                ];
                

SEFC[l_] := FCFAConvert[ Amp[l] //. changes , 
                         IncomingMomenta -> {p}, 
                         OutgoingMomenta -> {p}, 
                         LoopMomenta -> {q1,q2,q3}];

TxtFile[dir1_,dir2_, Amp_, name_] := Put[Amp, ToFileName[{dir1,dir2}, StringJoin[name,dir2] ] ];

ParallelMap[TxtFile[TempDirName, StringJoin[ToString[Top],ToString[#]], 
                                 SEFC[#], 
                                 ToString[FCAmps]]&, 
                                 Range[1,10] ];


Print[ " FC Amps Saved !! " ];



Print["The End!"]

CloseKernels[];

Quit[];
