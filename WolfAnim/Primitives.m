Package["WolfAnim`"]

PackageExport["Brace"]


Options[Brace] = {"Direction" -> Down, "WidthMultiplier" -> 2, "Buffer" -> 0.2}

Brace[obj_AnimatedObject, dir_ : $AnimatedObjectDefaultDirective, OptionsPattern[]] := Module[{
    direction, angle, corners, left, right, width, numQuads, text
},
    direction = OptionValue["Direction"] /. {Left -> {-1, 0}, Right -> {1, 0}, Down | Bottom -> {0, -1}, Up | Top -> {0, 1}};
    angle = ArcTan @@ Reverse[direction] - Pi;
    corners = obj["TransformPrimitives", RotationTransform[angle]]["Corners"];
    {left, right} = Values@corners[[{Key[{-1, -1}], Key[{1, -1}]}]];
    width = First[right] - First[left];
    numQuads = Clip[width OptionValue["WidthMultiplier"], {2, 15}];
    text = AnimatedObject[StringTemplate["\\underbrace{``}"][StringJoin @ Table["\\qquad", numQuads]]]["Apply", "Stretch", width];
    text["Apply", "Translate", left - text["Corners"][{-1, 1}] + {0, -OptionValue["Buffer"]}]["TransformPrimitives", RotationTransform[-angle]]
]
