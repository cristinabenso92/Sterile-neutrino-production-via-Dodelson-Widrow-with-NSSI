(*BeginPackage["KPTools`"];*)

Clear[Global`SaveFigures];

(* Reduce history length to save memory *)
$HistoryLength = 5;

(* Useful packages *)
<< ErrorBarPlots`;
<< LogTicks`;
<< MyGrid`;

(* Commands for plot legends *)
LegendLine[Style_] := Graphics[{Style,Line[{{-1,0},{1,0}}]},AspectRatio->0.6,ImageSize->30];

LegendBox[Style_]  := Graphics[{Style,Rectangle[{0,0}]},AspectRatio->0.6,ImageSize->30];

LegendBox[Style_,EdgeStyle_] :=
  Graphics[{Style,EdgeForm[EdgeStyle],Rectangle[{0,0}]},AspectRatio->0.6,ImageSize->30];

LegendBand[CurveStyle_, BandStyle_] := Graphics[{BandStyle, Rectangle[{0, 0}], CurveStyle,
   Line[{{0, .5}, {1, .5}}]}, AspectRatio -> 0.6, ImageSize -> 30];

LegendBound[Style_] := 
  Graphics[{Style,Arrowheads[.6], AbsoluteThickness[1.5],Line[{{0,-1},{0,1}}],
            Arrow[{{0,0},{-1,0}}]},AspectRatio->0.6,ImageSize->30];

LegendPoint[Style_] :=
  ListPlot[{{0,0}}, Frame->False, Axes->False, ImageSize->30, PlotStyle->Style];

Options[LegendDataPoint] = {
  HorizontalError -> True,
  PlotStyle -> Directive[Black,AbsoluteThickness[1]],
  Frame     -> False,
  Axes      -> False,
  ImageSize -> 30
};
Options[LegendDataPoint] = Union[Flatten[{
    {Evaluate[FilterRules[Options[ErrorListPlot], Except[Options[LegendDataPoint]]]]},
  Options[LegendDataPoint]
}]];
LegendDataPoint[opts:OptionsPattern[]] :=
  ErrorListPlot[{{{0,0}, If[OptionValue[HorizontalError], ErrorBar[1,1], ErrorBar[1]]}},
    Evaluate[Sequence @@ Evaluate[
      {FilterRules[Union[Evaluate[FilterRules[Options[LegendDataPoint], Except[{opts}]]], {opts}],
                  Options[ErrorListPlot]]}]] ];
(*LegendDataPoint[Style_, opts:OptionsPattern[]] :=
  ErrorListPlot[{{{0,0},ErrorBar[1,1]}}, Frame->False, ImageSize->30];*)

(* Taken from http://forums.wolfram.com/mathgroup/archive/1999/Apr/msg00028.html
   (slightly modified) *)
JHEPPlot[grObj_Graphics] := Module[{fulopts, tcklst, th = 0.004},
   fulopts = FullOptions[grObj];
   tcklst = FrameTicks /. fulopts /. {loc_, lab_, len : {_, _}, {sty : ___}}
          :> {loc, lab, 2*len, {Directive[sty, Thickness[th]]}};
   grObj /. Graphics[prims_, (opts__)?OptionQ] :> 
            Graphics[prims, FrameTicks -> tcklst, FrameTicksStyle -> Thickness[th],
                     FrameStyle -> Thickness[th], opts]
];

(* Common options for plotting functions *)
MyOptions = {
  BaseStyle   -> {FontFamily -> "Times", FontSize -> 18, TextAlignment -> Left},
  PlotRange   -> All,
  Axes        -> False,
  Frame       -> True,
  AspectRatio -> 0.9,
  ImageSize   -> 400
};
Off[SetOptions::optnf];
SetOptions[Show,MyOptions];
SetOptions[Plot,MyOptions];
SetOptions[LogPlot,MyOptions];
SetOptions[LogLinearPlot,MyOptions];
SetOptions[LogLogPlot,MyOptions];
SetOptions[Plot3D,MyOptions];
SetOptions[ListPlot,MyOptions];
SetOptions[ListLogPlot,MyOptions];
SetOptions[ListLogLinearPlot,MyOptions];
SetOptions[ListLogLogPlot,MyOptions];
SetOptions[ListPlot3D,MyOptions];
SetOptions[Histogram,MyOptions];
SetOptions[ContourPlot,MyOptions];
SetOptions[ListContourPlot,MyOptions];
SetOptions[DensityPlot,MyOptions];
SetOptions[ListDensityPlot,MyOptions];
SetOptions[RegionPlot,MyOptions];
SetOptions[ParametricPlot,MyOptions];
SetOptions[ArrayPlot,MyOptions];
On[SetOptions::optnf];


(* Numerical constants *)
NN = {
   (* Astrophysics *)
   GN -> 6.708*^-39/1*^18,    (* eV^-2, Newton's constant *)
   MPl -> 1.22093*^19 GeV,    (* Planck mass, PDG 2013 *)
   Msun -> 1.989*^30 kg,
   Rsun -> 6.9551*^8 meter,
   MMW -> 7*^11 Msun,         (* Mass of Milky way, Wikipedia article "Milky Way" *)

   (* cosmology *)
   H0 -> h*100 km/sec/Mpc,    (* Hubble constant *)
   h -> 0.688,                (* according to Planck, see Wikipedia *)
   T0 -> 2.73 Kelvin,         (* temperature of the Universe at time t_0 (today) *)
   T\[Nu]0 -> (4/11)^(1/3) T0,(* neutrino temperature at time t_0 *)
   \[CapitalOmega]m -> 0.14/h^2, (* total matter energy density *)
   \[CapitalOmega]b -> 0.022/h^2, (* baryon number density *)
   zeq -> 3370,               (* redshift of matter-radiation equality (MRE), see Planck 2015 *)

   (* Particle physics *)
   \[Alpha] -> 1/137, GF -> 1.16637*^-5 / GeV^2,
   MZ -> 91.19*^9, MW -> 80.398 GeV,
   MH -> 125. GeV, vev -> 246. GeV, vH -> vev, (* Higgs mass and vev *)
   mp -> 938*^6,              (* Proton mass *)
   amu -> 931.494061 MeV,     (* Atomic mass unit *)
   me -> 511.*^3, mmu -> 105.658367 MeV, mtau -> 1776.82 MeV,  (* Charged lepton masses *)
   mb -> 4.65 GeV,            (* b quark pole mass *)
   mbMSbar -> 4.18 GeV,       (* b quark MSbar mass *)
   mt -> 173.21 GeV,          (* Top quark pole mass *)
   mtMSbar -> 160 GeV,        (* Top quark MSbar mass *)
   GammaTop -> 1.5 GeV,       (* Top quark width (SM theory), from 0808.2167 *)
   BRWlnu -> 0.2155,          (* BR(W -> e \nu) + BR(W -> \mu \nu) *)
   Mu -> 931.494028 MeV,      (* Atomatic mass unit *)
   a0 -> 0.529177*^-10 meter, (* Bohr radius *)

   (* Properties of chemical elements *) 
   QXe -> 54, MXe -> 131.293 mp,
   QGe -> 32, MGe -> 72.64 mp,
   QSi -> 14, MSi -> 28.0855 mp,

   (* Material properties *)
   \[Rho]NaI -> 3.67 grams/cm^3,
   \[Rho]Xe  -> 3.1  grams/cm^3,

   (* Unit conversion *)
   eV -> 1., keV -> 1*^3, MeV -> 1*^6, GeV -> 1*^9, TeV -> 1*^12,
   Kelvin -> (1/1.1604505*^4) eV,
   Joule -> 1/(1.60218*^-19) eV, Watt -> Joule/sec,
   erg -> 1*^-7 Joule,
   Tesla -> 1/(1.4440271*^-3) eV^2,
   Gauss -> 1*^-4 Tesla,
   pb -> 2.5766*^-27, (* eV^-2 *)
   fb -> 1*^-3 pb,
   meter -> 5.076*^6, km -> 1000*meter,
   cm -> 5.076*^4, nm -> 1*^-9 meter, fm -> 1*^-15 meter,
   pc -> 30.857*^15 meter, kpc -> 1*^3 pc, Mpc -> 1*^3 kpc,
   sec -> 1.523*^15, hours -> 3600 sec, days -> 24 hours, yrs -> 365 days,
   kg -> 5.62*^35 eV, grams -> 10^-3 kg,
   c -> 2.9979*^10 (* cm/s *)
};


(* Likelihood for Poisson distributed random variable *)
PoissonLikelihood[obs_, th_] := 2.0 * (th - obs + If[obs > 0, obs*Log[obs/th], 0])

(* Likelihood for Gauss distributed random variable *)
GaussLikelihood[obs_, th_, sigma_] := (th - obs)^2/sigma^2


(* Compute p value corresponding to given number of sigmas (2-sided) *)
PValue[sigmas_] := Module[{},
    Return[2 NIntegrate[1/(Sqrt[2 \[Pi]]) Exp[-x^2/2], {x, sigmas, \[Infinity]}]];
];

(* Compute chi^2 value for given number of DOF and given p value *)
\[Chi]2[dof_, pvalue_] := Module[{},
    Return[x /. FindRoot[1 - CDF[ChiSquareDistribution[dof]][x] == pvalue, {x, 10}]];
];


(* My own histogram plotting function *)
Options[KPHistogram] = {
  ScalingFactor      -> 1,
  Joined             -> True,
  InterpolationOrder -> 0,
  ReturnRawData      -> False,
  LogScale           -> False,
  Weights            -> {}
};
Options[KPHistogram] = Union[Flatten[{
    {Evaluate[FilterRules[Options[ListPlot], Except[Options[KPHistogram]]]]},
  Options[KPHistogram]
}]];
KPHistogram[Data_List, xmin_?NumericQ, xmax_?NumericQ, xbins_Integer, opts:OptionsPattern[]] := Module[
    { dx, BinBoundaries, BinWidths, BinCenters, BinnedData, Result },

  Off[OptionValue::nodef,OptionValue::optnf];
  dx = (xmax - xmin) / xbins;
  BinBoundaries = Range[xmin, xmax, dx];
  BinWidths     = Differences[BinBoundaries];
  BinCenters    = BinBoundaries[[1;;-2]] + BinWidths / 2.;

  If [Length[OptionValue[Weights]] > 0,
    BinnedData    = BinLists[Transpose[{Data, OptionValue[Weights]}], {BinBoundaries},
                             {{-Infinity, Infinity}}];
    BinnedData    = Total[#[[All, 2]]] & /@ Flatten[BinnedData, {1, 2}],
  (* Else *)
    BinnedData    = BinCounts[Data, {BinBoundaries}];
  ];

  If[Length[BinnedData] == 0, BinnedData = ConstantArray[0, Length[BinBoundaries]-1]];
  BinnedData    = Transpose[{BinBoundaries[[1;;-2]],
                             OptionValue[ScalingFactor]*BinnedData}];
  AppendTo[BinnedData, {BinBoundaries[[-1]], BinnedData[[-1, 2]]}];
  If[OptionValue[LogScale],
    BinnedData = { #[[1]], Log10[#[[2]]] } & /@ BinnedData;
  ];
  
  Result = ListPlot[BinnedData, Evaluate[Sequence @@ Evaluate[
            {FilterRules[Union[Evaluate[FilterRules[Options[KPHistogram], Except[{opts}]]], {opts}],
                  Options[ListPlot]]}]]];
  If[OptionValue[ReturnRawData],
    Result = { Result, BinnedData }
  ];

  On[OptionValue::nodef,OptionValue::optnf];
  Return[Result];
];


(* Preparing data points for logarithmic ErrorListPlots *)
(* The argument should be a list of the form { {x, y, -delta-, delta+}, ... },
   (NOT log(x), log(y), ...). Note also that delta- *)
LogErrorList[l_List] := N[{
  { #[[1]],
    Log10[Max[ 1*^-100, #[[2]] ]] }, 
  ErrorBar[{Log10[Max[ 1*^-100, #[[2]] + #[[3]] ]] - Log10[Max[ 1*^-100, #[[2]] ]], 
            Log10[Max[ 1*^-100, #[[2]] + #[[4]] ]] - Log10[Max[ 1*^-100, #[[2]] ]]}]
}] & /@ l;

LogLogErrorList[l_List] := N[{
  { Log10[#[[1]]],
    Log10[Max[ 1*^-100, #[[2]] ]] }, 
  ErrorBar[{Log10[Max[ 1*^-100, #[[2]] + #[[3]] ]] - Log10[Max[ 1*^-100, #[[2]] ]], 
            Log10[Max[ 1*^-100, #[[2]] + #[[4]] ]] - Log10[Max[ 1*^-100, #[[2]] ]]}]
}] & /@ l;


(* Remove ``spider webs'' from contour plots (courtesy of Paddy Fox) *)
(* this seems not always to work - use cleanContourPlot instead *)
(*OptimizeContourPlot[p_] :=  p /. { { EdgeForm[], RGBColor[i1_, i2_, i3_], ii___}
                               -> {EdgeForm[RGBColor[i1, i2, i3]], RGBColor[i1, i2, i3], ii} };*)

(* http://mathematica.stackexchange.com/questions/3190/saner-alternative-to-contourplot-fill *)
(* NOTE: This function clashes with FeynArts, since FeynArts overwrites the Graph function *)
cleanContourPlot[cp_] :=
 Module[{points, groups, regions, lines},
  groups = 
   Cases[cp, {style__, g_GraphicsGroup} :> {{style}, g}, Infinity];
  points = 
   First@Cases[cp, GraphicsComplex[pts_, ___] :> pts, Infinity];
  regions = Table[
    Module[{group, style, polys, edges, cover, graph},
     {style, group} = g;
     polys = Join @@ Cases[group, Polygon[pt_, ___] :> pt, Infinity];
     edges = Join @@ (Partition[#, 2, 1, 1] & /@ polys);
     cover = Cases[Tally[Sort /@ edges], {e_, 1} :> e];
     graph = Graph[UndirectedEdge @@@ cover];
     {Sequence @@ style, 
      FilledCurve[
       List /@ Line /@ First /@ 
          Map[First, 
           FindEulerianCycle /@ (Subgraph[graph, #] &) /@ 
             ConnectedComponents[graph], {3}]]}
     ],
    {g, groups}];
  lines = Cases[cp, _Tooltip, Infinity];
  Graphics[GraphicsComplex[points, {regions, lines}], 
   Sequence @@ Options[cp]]
];


(* Partition flat lists into multi-dimensional sublists (taken from Mathematica documentation) *)
Unflatten[e_, {d__?((IntegerQ[#] && Positive[#]) &)}] := 
 Fold[Partition, e, 
     Take[{d}, {-1, 2, -1}]] /; (Length[e] === Times[d]);

(* An abbreviation for RegularExpression *)
RegExp = RegularExpression;

(*Begin["`Private`"]

End[ ] (* Private *)

EndPackage[]*)

