BeginPackage["MyGrid`"];

(* Usage messages *)

MyGrid::usage =
"MyGrid[xmin, xmax] generates positions for grid lines in a logarithmic plot";


(* MyGrid *)

Begin["`Private`"]

MyGrid[min_, max_] := Module[{ThisPosition, TheGrid, Digits},
   TheGrid = {};
   ThisPosition = min;
   Digits = RealDigits[ThisPosition, 10, 1];
   Digits = {{Digits[[1]][[1]]}, Digits[[2]]};
   While[ThisPosition <= max,
     AppendTo[TheGrid, N[ThisPosition]];
     Digits = RealDigits[ThisPosition, 10, 1];
     If [Digits[[1]][[1]] < 9,
       Digits = {{Digits[[1]][[1]] + 1}, Digits[[2]]},
       (* Else *)
       Digits = {{1}, Digits[[2]] + 1};
     ];
     ThisPosition = FromDigits[Digits];
   ];
   Return[TheGrid];
];

End[ ] (* Private *)

EndPackage[]

