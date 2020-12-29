Package["WolfAnim`"]

PackageExport["AnimationEffect"]



Options[AnimationEffect] = {"Duration" -> 1}

$AnimationEffectProperties = {"Duration", "Function"};


animationEffectDataQ[data_] :=
    AllTrue[$AnimationEffectProperties, KeyExistsQ[data, #] &] &&
    NumericQ[data["Duration"]]

AnimationEffect[f_Function, OptionsPattern[]] := AnimationEffect[<|"Function" -> f, "Duration" -> OptionValue["Duration"]|>]

AnimationEffect[data_]["Function"] := data["Function"]

AnimationEffect[data_]["Duration"] := data["Duration"]


eff_AnimationEffect[obj_AnimatedObject] := obj["AddEffect", eff]


Options[creationEffect] = {Method -> "Partial", "Delay" -> 0};

creationEffect[duration_, opts : OptionsPattern[]] := Module[{
    method, args, delay = OptionValue["Delay"]
},
    If[ ListQ[OptionValue[Method]],
        method = First @ OptionValue[Method];
        args = Association @ Rest @ OptionValue[Method],

        method = OptionValue[Method];
        args = <||>
    ];
    Switch[method,
        "Partial",
        AnimationEffect[#Object["Partial", 0, Ramp[(#t - delay)/duration]] &, duration + delay],
        "Gradient",
        AnimationEffect[#Object["MapDirective",
            dir |-> Map[
                Replace[c_ ? ColorQ :>
                    LinearGradientFilling[
                        {Ramp[2 (#t - delay) / duration - 1], Min[Ramp[2 (#t - delay) / duration], 1]} -> {c, Transparent}, Lookup[args, "Direction", 0]
                    ]
                ],
                dir,
                {0, 1}]
            ] &,
            duration + delay
        ]
    ]
]


AnimationEffect["Scale", factor_, Optional[p : Except[OptionsPattern[]], Automatic], opts : OptionsPattern[]] := With[{
    duration = OptionValue[AnimationEffect, {opts}, "Duration"]
},
    AnimationEffect[
        Function @ With[{scale = If[ListQ @ factor, factor, Table[factor, #Object["EmbeddingDimension"]]], t = #t},
            #Object["TransformPrimitives", ScalingTransform[(1 + (If[# > 0, t, duration - t] / duration) (Abs[#] - 1)) & /@ scale, p /. Automatic -> #Object["Center"]]]
        ],
    opts
    ]
]

AnimationEffect["Rotate", angle_, Optional[p : Except[OptionsPattern[]], Automatic], opts : OptionsPattern[]] := With[{
    duration = OptionValue[AnimationEffect, {opts}, "Duration"]
},
    AnimationEffect[#Object["TransformPrimitives",
        RotationTransform[#t / duration angle, p /. Automatic -> #Object["Center"]]] &,
        opts
    ]
]

AnimationEffect["Translate", v_, opts : OptionsPattern[]] := With[{
    duration = OptionValue[AnimationEffect, {opts}, "Duration"]
},
    AnimationEffect[#Object["TransformPrimitives",
        TranslationTransform[#t / duration (v /. {Left -> {-1, 0}, Right -> {1, 0}, Down -> {0, -1}, Up -> {0, 1}})]] &,
        opts
    ]
]

AnimationEffect["Stretch", width_, height_ : Automatic, opts : OptionsPattern[]] := Enclose @ AnimationEffect[
    Function @ Module[{w, h},
        If[ width =!= Automatic,
            w = width / #Object["Width"];
            h = (height /. Automatic -> w #Object["Height"]) / #Object["Height"],
            ConfirmAssert[height =!= Automatic];
            h = height / #Object["Height"];
            w = (width /. Automatic -> h #Object["Width"]) / #Object["Width"]
        ];
        AnimationEffect["Scale", {w, h}, opts]["Function"][#]
    ],
    opts
]

AnimationEffect["Creation", opts : OptionsPattern[Join[Options[AnimationEffect], Options[creationEffect]]]] :=
    creationEffect[OptionValue["Duration"], Sequence @@ FilterRules[opts, Options[creationEffect]]]

AnimationEffect["Wait", opts : OptionsPattern[]] := AnimationEffect[#Object &, opts]


AnimationEffect[effects : {__AnimationEffect}] := AnimationEffect[
    Fold[<|"Object" -> #2["Function"][#1], "t" -> #1["t"], "T" -> #1["T"]|> &, #, effects]["Object"] &,
    "Duration" -> Max[#["Duration"] & /@ effects]
]



AnimationEffect /: MakeBoxes[eff : AnimationEffect[data_ ? animationEffectDataQ], form_] := Module[{
    above, below
},
    above = {{BoxForm`SummaryItem[{"Duration", ":", Quantity[eff["Duration"], "Seconds"]}]}};
    below = {};
    BoxForm`ArrangeSummaryBox[
        AnimationEffect,
        eff,
        AnimatedObject[RegularPolygon[3]]["AddEffect", eff]["Dynamic"],
        above, below,
        form,
        "Interpretable" -> Automatic
    ]
]