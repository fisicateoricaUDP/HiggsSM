#!/Path/to/MathematicaScript -script

PrependTo[$Path, ToFileName[{"/Path","to","FIESTA5"}]];
Get["FIESTA5.m"];

TempDirName = DirectoryName[FindFile["MIsEvaluation.m"]];
SetDirectory[TempDirName];

masters = Get["Masters"];

Qin = ToExpression[$ScriptCommandLine[[2]]];
Q = 0.01*Qin;

Family[n_] := Union@Flatten@Union@Cases[masters[[n]],
			     G[a_,b__]:>ToExpression@Characters[ToString[a]]];
Sector[n_] := Cases[masters[[n]], G[a_,b__]:>b];
  
Rules[n_] := Block[{masses},
               masses = Map[ToExpression["m"<>ToString[#]] -> 0 &, Range[9]];
               Map[(masses[[#]] = masses[[#]] /. {0 -> t^2}) &, Family[n]];
               masses ];


SetOptions[FIESTA, "NumberOfSubkernels" -> 5, "NumberOfLinks" -> 5, 
	   "ComplexMode" -> True, DataPath -> TempDirName]


listrules = Table[

(** The numerical evauation is done with FIESTA at p=0.125TeV and M_t = 0.173 TeV. **)
SDEvaluate[UF[{q1, q2, q3}, {q1^2 - m1, q2^2 - m2, q3^2 - m3,
               (q1 - q2)^2 - m4, (q1 - q3)^2 - m5, (q2 - q3)^2 - m6,
               (q1 + p)^2 - m7, (q2 + p)^2 - m8, (q3 + p)^2 - m9} /. Rules[i],
               {p -> 0.125, t -> 0.173}],
	       Sector[i][[j]], 0, ComplexMode -> False, OnlyPrepare -> True, 
               DataPath -> TempDirName];

SDIntegrate[];

(** The RGE of the masters is done with the help of the next function: **)
masters[[i,j]]-> Block[{res},
               res = GenerateAnswer[];
               res = Normal@Series[res*Normal@Series[Exp[3*ep*Log[Q^2]], {ep, 0, 3}], {ep, 0, 0}];
	       res = Collect[res,ep,Re[#]& ] /. {Re[a_]:>0};
	       res ]

, {i,1,Length[masters]},{j, 1, Length[Sector[i]]}]

Print[" Numeric Result Generated ! "]

Put[listrules, ToFileName[{TempDirName},"numrulesp_"<>ToString[Q]]];

Quit[];

(** For the numerical evaluation of the self-energy amplitudes the **)
(** following functions are required:                              **)
  
Goodamps[n_] := Get["Goodamp"<>ToString[n]<>".txt", Path -> {TempDirName}]
Ampsp2[a_] := Collect[FactorTerms[-I*4096*Pi^12*Coefficient[Goodamps[a], S]], {T,y^6}]
Ampp2 = Map[Ampsp2[#] &, Range[9]];

(** You must import the data for the RGE of M_t and y_t as for instance: **)
datosMt = Import["DatosMt.dat", "Table"];
dataMt = Table[{datosMt[[k, 1]], datosMt[[k, j]]/1000},
               {j, 2,Length[datosMt[[1]]]}, {k, 1, Length[datosMt]}];
datayt = Import["Datosyt.dat", "Table"];

(** You must call the rules produced with the script 8_MIsEvaluation.m: **)
NumRules[n_] := Get["numrulesp_"<>ToString[n]]

(** The numerical evauation for one kinematical point is done by: **)
Amps[j_, o_] := Block[{amps, rules},
                   rules = Flatten@{NumRules[0.01*j], s -> 0.125^2, t -> 0.173^2};
                   amps = Ampsp2 /. rules;
                   amps = y^6*Collect[Coefficient[Plus@@amps, y, 6], {T}, Expand];
                   amps = amps /. {y -> datayt[[j,2]], T -> dataMt[[10,j]][[2]]^2 };
                   amps = (-4096*Pi^12)^(-1)*Coefficient[amps,ep,o]*(10^12)*(0.125^2);
                   amps = Sqrt@Abs[amps] // N;                                                                                                                                        
                   {10*j, amps}]

(** Note that the function Amps[n,o] produces the numerical value of the    **)
(** dependent term from the sum of the 3L self-energies amps at order y_t^6 **)
(** in GeV.                                                                 **)


