TempDirName = ToFileName[{"/Path","to","outputs"}];

ScalarInt[n_] := Get["ListScalarsLandauv3"<>ToString[n],
		      Path->{ToFileName[{TempDirName,"HiggsSE",ToString[Top]<>ToString[n]}]}];

NegativeNum[int_] := 
 Length[Cases[
   Negative[Flatten @@ Cases[{int}, INT[_, _, _, _, _, a_] :> a]], 
   True]]

  ListScalars[m_] := Select[ScalarInt[m], NegativeNum[#] > 0 &]
  Basis[m_] := Select[ScalarInt[m], NegativeNum[#] == 0 &]
  Sectors[n_] := Union@Cases[ListScalars[n], INT[a_, _, b_, _, _, __] :> S @@ {a, b} ]

  Table[Put[ ListScalars[j],
	     ToFileName[{TempDirName, "reductiontop"<>ToString[j] }, 
	     "ListScalarsv3"<>ToString[j] ]], {j,1,10}];

  Print[" Scalar Integrals Generated ! "]

  Table[Put[ Sectors[j],
             ToFileName[{TempDirName, "reductiontop"<>ToString[j] }, 
             "Sectorsv3"<>ToString[j] ]], {j,1,10}];

  Print[" Sectors Generated ! "]


   Table[Put[ Basis[j],
             ToFileName[{TempDirName, "reductiontop"<>ToString[j] }, 
             "Basisv3"<>ToString[j] ]], {j,1,10}];

  Print[" Basis Integrals Generated ! "]

      
Print["The End!"]
