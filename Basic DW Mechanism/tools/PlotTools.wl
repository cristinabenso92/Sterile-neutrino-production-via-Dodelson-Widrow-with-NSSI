(* ::Package:: *)

(*BeginPackage["PlotTools`"];*)

Clear[Global`SaveFigures];

(* Reduce history length to save memory *)
$HistoryLength = 5;

(* Useful packages *)

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
  BaseStyle   -> {FontFamily -> "Times", FontSize -> 14, TextAlignment -> Center},
  PlotRange   -> All,
  Axes        -> False,
  Frame       -> True,
  AspectRatio -> 0.9,
  ImageSize   -> 450,
  FrameStyle->{Style,16,Black,FontFamily->Times}
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

