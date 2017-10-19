(* ::Package:: *)

(*

Loads the DatabaseWrapper classes

*)

LoadDatabaseType::usage="Loads a database class";

`Private`$LoadContext=$Context;

Begin["`Private`"];

$ContextFile=$InputFileName

Needs["DatabaseLink`"];

With[{
	d=FileNameJoin[{DirectoryName@$InputFileName,"_core"}]
	},
	Get@FileNameJoin@{d,"database.m"};
	LoadDatabaseType[extension_]:=
		PackageExecute[
			$LoadContext,
			If[extension===All,
				SetDirectory@d;
				(Get@FileNameJoin@#&)/@FileNames[Except["database.m"]];
				ResetDirectory[];,
				Get@FileNameJoin@{d,extension<>".m"}
				];
			]
		];

End[];
