BeginPackage["LogScale`"];

(* Usage messages *)

LinearScale::usage =
"LinearScale[xmin, xmax] gives a list of \"nice\" values between xmin and xmax \
suitable for use as tick mark positions. LinearScale[xmin, xmax, n] attempts \
to find n such values.";

LogScale::usage =
"LogScale[xmin, xmax] gives a list of \"nice\" values between xmin and xmax \
suitable for use as tick mark positions on a logarithmic scale. \
LogScale[xmin, xmax, n] attempts to find n such values.";

LogGridMinor::usage =
"LogGridMinor[xmin, xmax] gives a list of \"nice\" values between xmin and \
xmax suitable for use as grid line positions on a logarithmic scale. The \
positions are the same as those for major and minor tick marks from LogScale. \
LogGridMinor[xmin, xmax, n] attempts to find n such values.";

LogGridMajor::usage =
"LogGridMajor[xmin, xmax] gives a list of \"nice\" values between xmin and \
xmax suitable for use as grid line positions on a logarithmic scale. The \
positions are the same as those for major tick marks from LogScale. \
LogGridMajor[xmin, xmax, n] attempts to find n such values.";

GenGrid::usage = "";
MinorLogTicks::usage="";
MinorAux1::usage="";
MinorAux2::usage="";

(* LogScale *)

Begin["`Private`"]

LogScale[min_, max_, n_Integer:6] :=
        (Module[{pts} ,
        pts = GenGrid[ min, max, n] ;
        Join[ Map[ LogTicks, pts ], MinorLogTicks[pts]]
    ] /; N[min] < N[max])


LogGridMajor[ min_, max_, n_Integer:6] :=
        (Module[{pts} ,
                pts = GenGrid[ min, max, n] ;
                Map[ Log[10, #]& , pts ]
        ] /; N[min] < N[max])

LogGridMinor[ min_, max_, n_Integer:6] :=
        (Module[{pts} ,
                pts = GenGrid[ min, max, n] ;
        Union[ Map[ Log[10., #]&,pts],
                       Map[ First, MinorLogTicks[pts]]]
        ] /; N[min] < N[max])

GenGrid[min_, max_, n_Integer:6] :=
        Module[{nmin=N[min], nmax=N[max], imin, imax, nper, t, tl} ,
            imin=Round[nmin] ;
            imax=Round[nmax] ;
            If[imin == imax, imax+=1];
            nper = Floor[n/(imax - imin)] ;
            If[nper > 0,
                    t = 10.^Range[imin, imax] ;
                    tl = Take[ $LogPreferances,
                                Min[nper, Length[$LogPreferances]] ] ;
                    t = Flatten[Outer[Times, t, tl]] ;
                    t = Sort[t] ,
                    (* else *)
                    nper = Ceiling[(imax - imin)/n] ;
                    t = 10.^Range[imin, imax, nper]
            ] ;
        Map[ If[ Log[10., #] < 5 && # == Round[#] , Round[#], #,#]&, t]
        ]

LogTicks[x_] :=
    {Log[10., x], x}

(* revised this function slightly on 3 May 95. Did not change the
   algorithm, per se, but did revise a bit of the code. Fixed in
   MinorAux1 the technique equivalent to finding the first non-zero
   digit in the input value; it was being computed mathematically,
   but ran into problems on machines with different precision (e.g.,
   040 Macintosh). Cleaned code for MinorLogTicks and MinorAux1; I
   think this entire function could be cleaned further, but didn't
   want to fiddle with the algorithm any more at the moment. *)
MinorLogTicks[pts_] :=
    Flatten[ Map[ MinorAux2,
              Transpose[{ Drop[pts, -1],
                     Map[ MinorAux1, Drop[pts, 1] - Drop[pts, -1] ]
              }]
         ],
         1
   ]



MinorAux2[{xst_, {del_ , n_}}] :=
        Module[{xfin = xst+del*(n-1),pts,x},
                pts = Table[x, {x, xst+del, xfin, del}] ;
                Map[ {Log[10., #], "", {0.6/160., 0.},
                                {Thickness[0.001]}}&, pts ]
        ]


(*MinorAux1[x_] := {x/#, #}&[RealDigits[ N[x] ][[1,1]]]*)

(* JK, 2010-02-28: Modified to avoid roundoff errors that happened for strongly *)
(* negative exponents (10^-40 or so) *)
MinorAux1[x_] := {x/#, #}&[RealDigits[ SetPrecision[N[x],10] ][[1,1]]]

$LogPreferances = {1, 5, 2, 3, 1.5, 7, 4, 6, 1.2, 8, 9, 1.3, 2.5, 1.1, 1.4}
{1, 5, 2, 3, 1.5, 7, 4, 6, 1.2, 8, 9, 1.3, 2.5, 1.1, 1.4}

End[ ] (* Private *)

EndPackage[]

