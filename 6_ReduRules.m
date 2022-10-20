(* ::Package:: *)

TempDirName = ToFileName[{"/Path","to","reductions"}];


PrependTo[$Path,TempDirName];


ListScalars[j_] := Get["ListScalarsv3"<>ToString[j]] 


Reductions[n_]:= Get["intredu"<>ToString[n]<>"v3"]


Rules[m_] := Map[#1 -> (#1 /. Reductions[m]) &, ListScalars[m]  ]


Block[ {j},
            SetDirectory[TempDirName];
            Table[Put[Rules[j],"rules"<>ToString[j]],{j,1,10}];
            Print[" Reduze Rules Generated ! "]
             ]


Name[var_]:=
ToExpression@
  StringJoin[
   ToString /@ Select[ToExpression@Characters[var], IntegerQ]]


RulesFire[m_] := Rules[m] /. {INT[name_, _, _, _, _, indices__] :> G@@{Name[name], indices}}


Block[ {j},
            SetDirectory[TempDirName];
            Table[Put[RulesFire[j],"rulesfire"<>ToString[j]],{j,1,10}];
            Print[" Fire Rules Generated ! "]
             ]


ReduzeRules[j_] := Get["rules"<>ToString[j]] 


FireScalars[j_] := Union@Cases[ListScalars[j] /. ReduzeRules[j], 
                               INT[__,a_,_,_,b_,__] /; a>=6 && b>0, Infinity]


Block[ {j},
            SetDirectory[TempDirName];
            Table[Put[FireScalars[j],"FireScalars"<>ToString[j]],{j,1,10}];
            Print[" Fire Scalares Generated ! "]
             ]


Quit[];
