Package["WolfAnim`"]

PackageExport["AnimationEffect"]



Options[AnimationEffect] = {"Duration" -> 1, "Reverse" -> False, "Rate" -> "Linear"}

$AnimationEffectProperties = {"Function", "Duration", "Reverse", "Rate"};


animationEffectDataQ[data_] :=
    AllTrue[$AnimationEffectProperties, KeyExistsQ[data, #] &] &&
    NumericQ[data["Duration"]]

AnimationEffect[f_Function, OptionsPattern[]] :=
    AnimationEffect[<|"Function" -> f, "Duration" -> OptionValue["Duration"], "Reverse" -> OptionValue["Reverse"], "Rate" -> OptionValue["Rate"]|>]

AnimationEffect[data_ ? animationEffectDataQ][prop_ /; MemberQ[$AnimationEffectProperties, prop]] := data[prop]


eff_AnimationEffect[obj_AnimatedObject] := obj["Play", eff]


animationRateFunction[rate_String] := rate /. {"Linear" -> Function[#], "Quadratic" -> Function[# ^ 2], "Exponential" -> Function[Exp[#] / E]}

animationRateFunction[rate_Function] := rate


Options[creationEffect] = {Method -> "Partial", "Delay" -> 0};

creationEffect[duration_, rate_, opts : OptionsPattern[]] := Module[{
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
        AnimationEffect[#Object["Partial", 0, Ramp[(rate[#t / duration] - delay / duration)]] &, "Duration" -> duration + delay],
        "Gradient",
        AnimationEffect[#Object["MapDirective",
            dir |-> Map[
                Replace[c_ ? ColorQ :>
                    LinearGradientFilling[
                        {Ramp[2 (rate[#t / duration] - delay / duration) - 1], Min[Ramp[2 (rate[#t / duration] - delay / duration)], 1]} -> {c, Transparent}, Lookup[args, "Direction", 0]
                    ]
                ],
                dir,
                {0, 1}]
            ] &,
            "Duration" -> duration + delay
        ]
    ]
]


AnimationEffect["Scale", factor_, Optional[p_List, Automatic], opts : OptionsPattern[]] := With[{
    duration = OptionValue["Duration"], rate = animationRateFunction @ OptionValue["Rate"]
},
    AnimationEffect[
        Function @ With[{scale = If[ListQ @ factor, factor, Table[factor, #Object["EmbeddingDimension"]]]},
            #Object["TransformPrimitives", ScalingTransform[(1 + rate[#t / duration] (scale - 1)), p /. Automatic -> #Object["Center"]]]
        ],
    opts
    ]
]

AnimationEffect["Rotate", angle_, Optional[p_List, Automatic], opts : OptionsPattern[]] := With[{
    duration = OptionValue["Duration"], rate = animationRateFunction @ OptionValue["Rate"]
},
    AnimationEffect[#Object["TransformPrimitives",
        RotationTransform[rate[#t / duration] angle, p /. Automatic -> #Object["Center"]]] &,
        opts
    ]
]

AnimationEffect["Translate", v_, opts : OptionsPattern[]] := With[{
    duration = OptionValue["Duration"], rate = animationRateFunction @ OptionValue["Rate"]
},
    AnimationEffect[#Object["TransformPrimitives",
        TranslationTransform[rate[#t / duration] (v /. {Left -> {-1, 0}, Right -> {1, 0}, Down -> {0, -1}, Up -> {0, 1}})]] &,
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
    creationEffect[OptionValue["Duration"], animationRateFunction @ OptionValue["Rate"], Sequence @@ FilterRules[opts, Options[creationEffect]]]

AnimationEffect["Wait", opts : OptionsPattern[]] := AnimationEffect[#Object &, opts]


AnimationEffect[effects : {__AnimationEffect}] := AnimationEffect[
    Fold[<|"Object" -> #1["Object"][#2, #1["t"], #1["T"]], "t" -> #1["t"], "T" -> #1["T"]|> &, #, effects]["Object"] &,
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
        AnimatedObject[RegularPolygon[3]]["Play", eff]["Dynamic"],
        above, below,
        form,
        "Interpretable" -> Automatic
    ]
]