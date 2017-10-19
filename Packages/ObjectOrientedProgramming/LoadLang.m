(* ::Package:: *)

LoadLanguage::usage="";

`Private`$LoadContext=$Context;

Begin["`Private`"];

$BaseLanguage="Python";

$ObjectDirectory =
	FileNameJoin@{
		DirectoryName[$InputFileName],
		"Saved Objects"
	};
$ContextFile=$InputFileName;
If[Context@Association=!="System`",
	LoadBackwardsCompatible["Association"]
];

LoadLanguage::nolang="Couldn't load class of language ``";
LoadLanguage[language_:$BaseLanguage]:=
	PackageExecute[
		$LoadContext,
	ClearAll/@
		Select[
			Names["LoadContext"<>"`*"],
			!StringEndsQ["$BaseLanguage"|"$MMADir"|"LoadLanguage"]
			];
	If[Get@FileNameJoin@{DirectoryName@$$ContextFile,"Languages",language<>".m"}=!=$Failed,
		$BaseLanguage=language,
		Message[LoadLanguage::nolang,language];
		$Failed
		]
	);

EndPackage[];
