(* ::Package:: *)

LoadGUIExtension::usage="";

Begin["`Private`"]

`Private`$ContextFile=$InputFileName;

With[{GUIToolsDirectory=$InputFileName//DirectoryName},
LoadGUIExtension[extensionName_]:=
	PackageExecute[
		$LoadContext,
		Get@FileNameJoin@{GUIToolsDirectory,StringTrim[extensionName,".m"]<>".m"};
		]
]

];

End[];
