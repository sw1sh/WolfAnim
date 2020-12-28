Package["WolfAnim`"]

PackageExport["AnimationEffect"]



$AnimationEffectProperties = {"Duration", "Function"};


animationEffectDataQ[data_] :=
    AllTrue[$AnimationEffectProperties, KeyExistsQ[data, #] &] &&
    NumericQ[data["Duration"]]

AnimationEffect[f_Function, duration_ : 1] := AnimationEffect[<|"Function" -> f, "Duration" -> duration|>]

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


AnimationEffect["Scale", factor_, duration_ : 1] := AnimationEffect[
    Function @ With[{scale = If[ListQ @ factor, factor, Table[factor, #Object["EmbeddingDimension"]]], t = #t},
        #Object["TransformPrimitives", ScalingTransform[(1 + (If[# > 0, t, duration - t] / duration) (Abs[#] - 1)) & /@ scale]]
    ],
    duration
]

AnimationEffect["Rotate", angle_, duration_ : 1] := AnimationEffect[#Object["TransformPrimitives",
    RotationTransform[#t angle, #Object["Center"]]] &,
    duration
]

AnimationEffect["Translate", v_, duration_ : 1] := AnimationEffect[#Object["TransformPrimitives",
    TranslationTransform[#t v /. {Left -> {-1, 0}, Right -> {1, 0}, Down -> {0, -1}, Up -> {0, 1}}]] &,
    duration
]

AnimationEffect["Creation", Optional[duration : Except[OptionsPattern[]], 1], opts : OptionsPattern[creationEffect]] := creationEffect[duration, opts]

AnimationEffect["Wait", duration_ : 1] := AnimationEffect[#Object &, duration]



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