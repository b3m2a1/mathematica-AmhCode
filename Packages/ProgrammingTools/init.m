(* ::Package:: *)

LoadProgrammingTool::usage="";

Begin["`Private`"];
$ContextFile=$InputFileName;
With[{d=DirectoryName@$InputFileName},
	(*Get@FileNameJoin@{d,"init.m"};*)
	LoadProgrammingTool[toolName_]:=
		PackageExecute[
			If[toolName===All,
				SetDirectory@`Private`d;
				(Get@FileNameJoin@#&)/@FileNames[Except["init.m"]];
				ResetDirectory[];,
				Get@FileNameJoin@{`Private`d,toolName<>".m"}
				];
			End[];
			]
		];
End[];
