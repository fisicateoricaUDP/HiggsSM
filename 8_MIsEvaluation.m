#!/usr/local/Wolfram/Mathematica/8.0/SystemFiles/Kernel/Binaries/Linux-x86-64/MathematicaScript -script

PrependTo[$Path, ToFileName[{"/home","edilson","myprograms","fiesta","FIESTA5"}]];
Get["FIESTA5.m"];

TempDirName = DirectoryName[FindFile["MIsEval.m"]];
SetDirectory[TempDirName];

masters = Get["Masterspnp"];

Qin = ToExpression[$ScriptCommandLine[[2]]];
Q = 0.01*Qin;

Family[n_] := Union@Flatten@Union@Cases[masters[[n]],
			     G[a_,b__]:>ToExpression@Characters[ToString[a]]];
Sector[n_] := Cases[masters[[n]], G[a_,b__]:>b];
  
Rules[n_] := Block[{masses},
               masses = Map[ToExpression["m"<>ToString[#]] -> 0 &, Range[9]];
               Map[(masses[[#]] = masses[[#]] /. {0 -> t^2}) &, Family[n]];
               masses ];


SetOptions[FIESTA, "NumberOfSubkernels" -> 10, "NumberOfLinks" -> 5, 
	   "ComplexMode" -> True, DataPath -> TempDirName]


listrules = Table[

SDEvaluate[UF[{q1, q2, q3}, {q1^2 - m1, q2^2 - m2, q3^2 - m3,
               (q1 - q2)^2 - m4, (q1 - q3)^2 - m5, (q2 - q3)^2 - m6,
               (q1 + p)^2 - m7, (q2 + p)^2 - m8, (q3 + p)^2 - m9} /. Rules[i],
               {p -> 0.125, t -> 0.173}],
	       Sector[i][[j]], 0, ComplexMode -> False, OnlyPrepare -> True, 
               DataPath -> TempDirName];

SDIntegrate[];

masters[[i,j]]-> Block[{res},
               res = GenerateAnswer[];
               res = Normal@Series[res*Normal@Series[Exp[3*ep*Log[Q^2]], {ep, 0, 3}], {ep, 0, 0}];
	       res = Collect[res,ep,Re[#]& ] /. {Re[a_]:>0};
	       res ]

, {i,1,Length[masters]},{j, 1, Length[Sector[i]]}]

Print[" Numeric Result Generated ! "]

Put[listrules, ToFileName[{TempDirName},"numrulesp_"<>ToString[Q]]];

Quit[];
