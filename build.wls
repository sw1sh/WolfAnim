#!/usr/bin/env wolframscript

Check[
    Get[FileNameJoin[{DirectoryName[$InputFileName], "scripts", "buildInit.wl"}]];
    deleteBuildDirectory[];,

    Exit[1];
];

Check[
  packPaclet[];
  Print["Build done."]
  ,
  Print["Build failed."];
  Exit[1]
];

