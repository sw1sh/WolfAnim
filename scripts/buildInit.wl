Needs["PacletManager`"];

$repoRoot = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]];
pacletName = FileBaseName[$repoRoot];
$pacletRoot = FileNameJoin[{$repoRoot, pacletName}];

packPaclet[] := Module[{pacletFileName},
  pacletFileName = CreatePacletArchive[$pacletRoot, $repoRoot];
  If[TrueQ[FileExistsQ[FileNames[pacletName <> "*.paclet"][[1]]]],
      Print[FileNames[pacletName <> "*.paclet"][[-1]] <> " ... OK"],
      Print["Paclet not produced"]
    ]
]
