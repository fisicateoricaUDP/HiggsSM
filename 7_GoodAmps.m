(* ::Package:: *)

(* ::Input:: *)
(*TempDirName = ToFileName[{"/home","edilson","Escritorio","Servidor","SMHiggs","outputs"}];*)


(* ::Input:: *)
(*Amps[n_]:=Get["ScalarAmpsLandauv3"<>ToString[n],Path->{TempDirName}]*)


(* ::Input:: *)
(*Rulesfire[n_] := Get["rulesfire"<>ToString[n],Path->{ToFileName[{TempDirName,"reductions"}]}]*)


(* ::Input:: *)
(*GoodRules[n_] := Block[{rules, srules,res},*)
(*                                                rules = Select[Association[Rulesfire[n]],#=!=0&]; *)
(*                                                rules = Map[(# // Factor)&,rules];*)
(*                                                srules = Select[rules,TrueQ[(Denominator[# // Factor]/. {d->4}) =!=0]&];*)
(*                                                res = srules /. {d->4} *)
(*                                                 ]*)


(* ::Input:: *)
(*Zerosectors[n_]:= Select[ Association[Rulesfire[n]],#==0&]*)


(* ::Input:: *)
(*Name[var_]:=*)
(*ToExpression@StringJoin[ToString /@ Select[ToExpression@Characters[var], IntegerQ]]*)


(* ::Input:: *)
(*Topamp[n_]:=Block[{changes,amp},*)
(*                                      changes ={INT[name_, _, _, _, _, indices__] :> G@@{Name[name], indices}, *)
(*                                                         p -> Sqrt[S], MT -> Sqrt[T], yt -> y};*)
(*                                      amp = Amps[n] /. changes /. Zerosectors[n] ;*)
(*                                      amp = y^6*Collect[FactorTerms[*)
(*                                                      Coefficient[Plus@@amp,y,6], G[___]],{S,T},Simplify];*)
(*                                      amp ]*)


(* ::Input:: *)
(*Goodamp[n_]:= Goodamp[n]=Block[{scalars,masters},*)
(*                            scalars =  Union@Cases[Topamp[n],G[__],Infinity];*)
(*                            masters =  Union@Cases[scalars /. Rulesfire[n],G[__],Infinity];*)
(*                            If[Length[scalars]<Length[masters],*)
(*                          Topamp[n],*)
(*                          Topamp[n] /. GoodRules[n]] ]*)


(* ::Input:: *)
(*Goodmasters[n_]:=Union@Cases[Goodamp[n],G[__],Infinity]*)


(* ::Input:: *)
(*Negativefire[int_]:=Length[Cases[Negative[Flatten@@Cases[{int},G[_,a_]:>a]],True]]*)


(* ::Input:: *)
(*Mastersn[n_]:=Select[Goodmasters[n], (Negativefire[#]!=0)&]*)


(* ::Input:: *)
(*SetDirectory[TempDirName];*)


(* ::Input:: *)
(*Table[Put[Goodamp[j],"Goodamp"<>ToString[j]],{j,1,10}];*)


(* ::Input:: *)
(*Print[" Good Amps Generated !"]*)


(* ::Input:: *)
(*Table[Put[Goodmasters[j],"Goodmasters"<>ToString[j]],{j,1,10}];*)


(* ::Input:: *)
(*Print[" Good Masters Generated !"]*)


(* ::Input:: *)
(*Print[" The End! "]*)


(* ::Input:: *)
(*Quit[];*)
