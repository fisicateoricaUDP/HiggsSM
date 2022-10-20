(* ::Package:: *)

TempDirName = ToFileName[{"/Path","to","outputs"}];

Amps[n_] := Get["ScalarAmpsLandauv3"<>ToString[n],Path->{TempDirName}]

Rulesfire[n_] := Get["rulesfire"<>ToString[n],Path->{ToFileName[{TempDirName,"reductions"}]}]


GoodRules[n_] := Block[{rules, srules,res},
                       rules = Select[Association[Rulesfire[n]],#=!=0&];
                       rules = Map[(# // Factor)&,rules];                                                
                       srules = Select[rules,TrueQ[(Denominator[# // Factor] /. {d->4}) =!=0]&];
                       res = srules /. {d->4}
                                           ]

Zerosectors[n_]:= Select[ Association[Rulesfire[n]],#==0&]


Name[var_]:=
ToExpression@StringJoin[ToString /@ Select[ToExpression@Characters[var], IntegerQ]]



Topamp[n_]:=Block[{changes, amp},
                                 changes = {INT[name_, _, _, _, _, indices__] :> G@@{Name[name], indices},
                                            p -> Sqrt[S], MT -> Sqrt[T], yt -> y};                                    
                                 amp = Amps[n] /. changes /. Zerosectors[n] ;
                                 amp = y^6*Collect[FactorTerms[
                                                   Coefficient[Plus@@amp,y,6], G[___]],{S,T},Simplify];
                   amp ]

Goodamp[n_]:= Goodamp[n] = Block[{scalars,masters},
                                  scalars =  Union@Cases[Topamp[n],G[__],Infinity];
                                  masters =  Union@Cases[scalars /. Rulesfire[n],G[__],Infinity];
                                  If[Length[scalars]<Length[masters],
                                  Topamp[n],
                                  Topamp[n] /. GoodRules[n]] ]

Goodmasters[n_] := Union@Cases[Goodamp[n],G[__],Infinity]

Negativefire[int_]:=Length[Cases[Negative[Flatten@@Cases[{int},G[_,a_]:>a]],True]]

Mastersn[n_]:=Select[Goodmasters[n], (Negativefire[#]!=0)&]

SetDirectory[TempDirName];

Table[Put[Goodamp[j],"Goodamp"<>ToString[j]],{j,1,10}];

Print[" Good Amps Generated !"]

Table[Put[Goodmasters[j],"Goodmasters"<>ToString[j]],{j,1,10}];

Print[" Good Masters Generated !"]


Print[" The End! "]

Quit[];
