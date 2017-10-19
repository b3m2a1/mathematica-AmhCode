(* ::Package:: *)

(*

Loads the BackwardsCompatibility packages

*)

LoadBackwardsCompatible::usage=
	"Loads a quick imp of a backwards compatible symbol";

`Private`$LoadContext=$Context;

Begin["`Private`"];

$ContextFile=$InputFileName;

LoadBackwardsCompatible::nosym="Couldn't load symbol `` in version ``";
With[{dir=FileNameJoin[{DirectoryName@$InputFileName, "_core"}]},
LoadBackwardsCompatible[symbol_,version_:9]:=
	PackageExecute[
		$LoadContext,
		If[
			Get@Replace[FileNameJoin@{dir,symbol<>".m"},
					f_:>
					If[Not@FileExistsQ[f],
						FileNameJoin@{dir,ToString@version,symbol<>".m"}
						]
				]===$Failed,
			Message[BackwardsCompatibility::nosym,symbol,version];
			End[];
			$Failed,
			End[];
			Symbol[symbol]
			];
		];
];

End[];
