BeginPackage["LogTicks`",{"LogScale`"}];

(* Usage messages *)

LogTicks::usage =
"LogTicks[logXmin, logXmax] produces tick marks (including labels in exponential \
form) in the range logXmin .. logXmax\n\
LogTicks[logXmin, logXmax, Stepsize] puts only every Stepsize'th label";

LogTicksNoLabel::usage =
"LogTicksNoLabel[logXmin, logXmax] produces tick marks (without labels) in the range \
logXmin .. logXmax";


(* LogTicks *)

Begin["`Private`"]

LogTicks[Min_, Max_] := (Module[{},
   MyTicks = LogScale[Min, Max, Ceiling[Max - Min]];
   NLabels = Length[Cases[MyTicks, {_, _}]];
   Return[
    MapAt[{#[[1]], "\!\(10\^" <> ToString[Round[#[[1]]]] <> "\)"} &, 
     MyTicks, List /@ Range[1, NLabels]]];
   ])

LogTicks[Min_, Max_, Stepsize_] := (Module[{},
   MyTicks = LogScale[Min, Max, Ceiling[Max - Min]];
   NLabels = Length[Cases[MyTicks, {_, _}]];

   MyTicks = MapAt[{#[[1]], "\!\(10\^" <> ToString[Round[#[[1]]]] <> "\)"} &, 
                   MyTicks, List /@ Range[1, NLabels, Stepsize]]];
   MyTicks = MapAt[{#[[1]], ""} &,
                   MyTicks, List /@ Complement[Range[1, NLabels], Range[1, NLabels, Stepsize]]];
   Return[MyTicks];
)

LogTicksNoLabel[Min_, Max_] := (Module[{},
   MyTicks = LogScale[Min, Max, Ceiling[Max - Min]];
   NLabels = Length[Cases[MyTicks, {_, _}]];
   Return[MapAt[{#[[1]], ""} &, MyTicks, List /@ Range[1, NLabels]]];
   ])

End[ ] (* Private *)

EndPackage[]

