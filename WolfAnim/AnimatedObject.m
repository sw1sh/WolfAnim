Package["WolfAnim`"]

PackageExport["AnimatedObject"]

PackageScope["$AnimatedObjectDefaultDirective"]


Options[AnimatedObject] = {"GraphicsOptions" -> Sequence[]};

$AnimatedObjectProperties = {"Primitives", "Directive", "Effects", "GraphicsOptions"};

$AnimatedObjectDefaultDirective = {LightBlue};


graphicsDirectiveQ[x_] := ResourceFunction["GraphicsDirectiveQ"][x] || MatchQ[x, _LinearGradientFilling | _RadialGradientFilling | _ConicGradientFilling] ||
    VectorQ[x, graphicsDirectiveQ] && ! MatchQ[x, {}]

graphicsPrimitiveQ[x_] := ResourceFunction["GraphicsPrimitiveQ"][x] || VectorQ[x, graphicsPrimitiveQ] && ! MatchQ[x, {}]

directiveQ[x_] := graphicsDirectiveQ[x]

primitiveQ[x_] := ResourceFunction["GraphicsPrimitiveQ"][x] || RegionQ[x] || MatchQ[x, _AnimatedObject] || VectorQ[x, primitiveQ] && ! MatchQ[x, {}]


animatedObjectDataQ[data_] :=
    AllTrue[$AnimatedObjectProperties, KeyExistsQ[data, #] &] &&
    primitiveQ[data["Primitives"]] &&
    directiveQ[data["Directive"]]


AnimatedObject[g_, dir_ : $AnimatedObjectDefaultDirective, opts : OptionsPattern[]] /; primitiveQ[g] && directiveQ[dir] :=
    AnimatedObject[<|"Primitives" -> g, "Directive" -> dir, "Effects" -> {}, "GraphicsOptions" -> {
        OptionValue["GraphicsOptions"], PlotRangePadding -> Scaled[0.1], Background -> Black}|>
    ]

AnimatedObject[s_String, dir_ : $AnimatedObjectDefaultDirective, opts : OptionsPattern[]] /; directiveQ[dir] :=
    AnimatedObject[ Cases[MaTeX`MaTeX[s], _ ? primitiveQ, {4}] /. {
        curve_FilledCurve :> GeometricFunctions`DecodeFilledCurve[curve],
        curve_JoinedCurve :> GeometricFunctions`DecodeJoinedCurve[curve]
    },
        dir, opts
    ]["Apply", "Stretch", Automatic, 1]["Centralize"]

AnimatedObject[g_Graphics, dir_ : Nothing, opts : OptionsPattern[]] :=
    AnimatedObject[Cases[g, _ ? graphicsPrimitiveQ, {1, 2}], Append[Cases[g, _ ? graphicsDirectiveQ, {1, 2}], dir], opts]


AnimatedObject[data_]["Primitives"] := data["Primitives"]

AnimatedObject[data_]["Directive"] := Replace[data["Directive"], {ds__} :> Directive[ds]]

AnimatedObject[data_]["Effects"] := data["Effects"]

AnimatedObject[data_]["GraphicsOptions"] := data["GraphicsOptions"]


obj_AnimatedObject["Graphics"] := {obj["Directive"], obj["Primitives"] /. o_AnimatedObject :> o["Graphics"]}

obj_AnimatedObject["Duration"] := Total[Cases[obj["Primitives"], o_AnimatedObject :> o["Duration"], All]] + Total[#["Duration"] & /@ obj["Effects"]]


(obj : AnimatedObject[data_])["Update", T_ : None] := First @ FoldWhile[{
        #2["Function"] @ <|
            "Object" -> #1[[1]],
            "T" -> T,
            "t" -> Min[T - #1[[2]], #2["Duration"]]
        |>,
        #1[[2]] + #2["Duration"]} &,
    {obj["MapPrimitives", ReplaceAll[o_AnimatedObject :> o["Update", T]]], 0},
    data["Effects"],
    #[[2]] < T &
]


obj_AnimatedObject["Render", opts : OptionsPattern[Graphics] | OptionsPattern[Graphics3D]] := With[{
    dim = obj["EmbeddingDimension"]
},
    Which[
        dim < 3, Graphics,
        dim == 3, Graphics3D,
        True,
        Failure["UnsupportedDimension", "Only dimensions less or equal to 3 are supported."]
    ][obj["Graphics"], Sequence @@ obj["GraphicsOptions"], opts]
]


regionPrimitive[withDirectives_][x_] := Which[
    RegionQ @ x, x,
    MatchQ[x, _AnimatedObject],
    If[withDirectives, Prepend[x["Directive"]], Identity] @ x["RegionPrimitives"],
    True, DiscretizeGraphics @ x
]

obj_AnimatedObject["RegionPrimitives", withDirectives_ : False, flatten_ : True] :=
    If[flatten, Flatten, Identity] @
    If[ ListQ @ obj["Primitives"],
        regionPrimitive[withDirectives] /@ obj["Primitives"],
        {regionPrimitive[withDirectives] @ obj["Primitives"]}
    ]

obj_AnimatedObject["Region"] := With[{regions = obj["RegionPrimitives"]}, If[ListQ @ regions, RegionUnion @@ Flatten[regions], regions]]

obj_AnimatedObject["EmbeddingDimension"] := Max[RegionEmbeddingDimension /@ obj["RegionPrimitives"]]

obj_AnimatedObject["Dimension"] :=  Max[RegionDimension /@ obj["RegionPrimitives"]]

obj_AnimatedObject["Center"] := Mean /@ obj["Bounds"] // Chop

obj_AnimatedObject["Bounds"] := RegionBounds @ Region @ obj["Region"] // Chop

obj_AnimatedObject["Corners"] := Module[{xmin, xmax, ymin, ymax},
    {{xmin, xmax}, {ymin, ymax}} = obj["Bounds"];
    <|{-1, -1} -> {xmin, ymin}, {1, -1} -> {xmax, ymin}, {1, 1} -> {xmax, ymax}, {-1, 1} -> {xmin, ymax}|>
]

obj_AnimatedObject["Width"] := ReverseApplied[Subtract] @@ obj["Bounds"][[1]]

obj_AnimatedObject["Height"] := ReverseApplied[Subtract] @@ obj["Bounds"][[2]]


AnimatedObject /: MakeBoxes[obj : AnimatedObject[data_ ? animatedObjectDataQ], form_] := Module[{
    above, below
},
    above = {
        {BoxForm`SummaryItem[{"Dimension", ":", obj["Dimension"]}]},
        {BoxForm`SummaryItem[{"Duration", ":", Quantity[obj["Duration"], "Seconds"]}]}
    };
    below = {
        {BoxForm`SummaryItem[{"EmbeddingDimension", ":", obj["EmbeddingDimension"]}]},
        {BoxForm`SummaryItem[{"Center", ":", obj["Center"]}]},
        {BoxForm`SummaryItem[{"Bounds", ":", obj["Bounds"]}]}
    };
    BoxForm`ArrangeSummaryBox[
        AnimatedObject,
        obj, obj["Dynamic"],
        above, below,
        form,
        "Interpretable" -> Automatic
    ]
]

obj_AnimatedObject["MapData", f_] := MapAt[f, obj, {1}]

obj_AnimatedObject["SetPrimitives", g_] := obj["MapData", Append["Primitives" -> g]]

obj_AnimatedObject["MapPrimitives", f_] := obj["MapData", MapAt[f, Key["Primitives"]]]

obj_AnimatedObject["TransformPrimitives", f_] := obj["MapPrimitives", ReplaceAll @ {
    o_AnimatedObject :> o["TransformPrimitives", f],
    r_ /; RegionQ[r] :> TransformedRegion[r, f],
    g_ /; 
      ResourceFunction["GraphicsPrimitiveQ"][g] :> (g /. 
       points : {{__Real} ..} :> f[points])
    }
]

obj_AnimatedObject["SetDirective", d_] := obj["MapData", Append["Directive" -> d]]

obj_AnimatedObject["MapDirective", f_] := obj["MapData", MapAt[f, Key["Directive"]]]

obj_AnimatedObject["Boundary"] := obj["MapPrimitives", Map[Replace[{
    r_ ? RegionQ :> RegionBoundary[r],
    FilledCurve[a__] :> JoinedCurve[a]}], #, {0, 1}] &
]

obj_AnimatedObject["Centralize"] := obj["TransformPrimitives", TranslationTransform[- obj["Center"]]]


obj_AnimatedObject["MeshRegion", n_ : 100] := With[{reg = obj["Region"]},
    DiscretizeRegion[reg, MaxCellMeasure -> {RegionDimension[reg] -> RegionMeasure[reg] / n}]
]


Options[partialMeshRegion] = {"SortBy" -> None};

partialMeshRegion[reg_MeshRegion, start_ : 0, end_ : 1, OptionsPattern[]] /; 0 <= start <= end <= 1 := Module[{
    dim, primitives, size, from, to
},
    dim = RegionDimension @ reg;
    primitives = MeshPrimitives[reg, dim];
    If[ OptionValue["SortBy"] =!= None,
        primitives = primitives[[
            OrderingBy[AnnotationValue[{reg, dim}, MeshCellCentroid], OptionValue["SortBy"]]
        ]]
    ];
    size = Length @ primitives;
    from = start (size - 1);
    to = end (size - 1);
    primitives[[Ceiling[from] + 1 ;; Floor[to] + 1]]
]

obj_AnimatedObject["Partial", start_ : 0, end_ : 1, sortBy_ : None] /; 0 <= start <= end <= 1 :=
    If[end - start == 1, obj, obj["SetPrimitives", partialMeshRegion[obj["MeshRegion"], start, end, "SortBy" -> sortBy]]]

obj_AnimatedObject["Play", f_AnimationEffect] := obj["MapData", MapAt[Append[f], "Effects"]]

obj_AnimatedObject["Play", name_String, args___] := obj["Play", AnimationEffect[name, args]]

obj_AnimatedObject["Apply", name_String, args___] := With[{eff = AnimationEffect[name, args]},
    eff["Function"][<|"Object" -> obj, "t" -> eff["Duration"], "T" -> obj["Duration"] + eff["Duration"]|>]
]

obj_AnimatedObject["Wait", args___] := obj["Play", "Wait", args]


Options[dynamicGraphics] = Merge[{Options[Graphics], Options[Graphics3D]}, First];

dynamicGraphics[obj_AnimatedObject, repeating_ : False, opts : OptionsPattern[]] := DynamicModule[{
    t, begin, end = obj["Duration"], init
},
    init[] := (t = 0; begin = AbsoluteTime[]);
    init[];

    Dynamic[
        Refresh[
            If[t < end, t = AbsoluteTime[] - begin, If[repeating, init[], t = end]];
            EventHandler[
                obj["Update", t]["Render", opts, 
                PlotRange -> obj["Bounds"]], {{"MouseDown", 1} :> init[]}
            ],
            TrackedSymbols :> {t}, UpdateInterval -> Infinity
        ]
    ]
]

obj_AnimatedObject["Dynamic", opts : OptionsPattern[dynamicGraphics]] := dynamicGraphics[obj, False, opts]


obj_AnimatedObject["Video", opts : OptionsPattern[VideoGenerator]] := With[{
    bounds = obj["Bounds"]
},
    Video[
        VideoGenerator[
            obj["Update", #]["Render", PlotRange -> bounds] &,
            obj["Duration"],
            opts
        ],
        Appearance -> "Minimal"
    ]
]
