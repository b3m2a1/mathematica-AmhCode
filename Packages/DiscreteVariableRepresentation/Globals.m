(* ::Package:: *)


IgnoreDVRFunctionShadowing::usage=
	"Macro for ignoring DVR shadowing";
LoadDVR::usage=
	"Function for loading a DVR";

$DVRRoot::usage="The main directory to find files"
$DVRDirectory::usage="Specifies the root directory for finding DVR config files"
$KineticMatrixDirectory::usage="Specifies the directory for finding saved kinetic matrices"
$PotentialFileDirectory::usage="Specifies the directory for finding calculated potentials "
$WavefunctionFileDirectory::usage="Specifies where to save wave functions"
BeginDVRExtension::usage="Begins a DVR extension file"
EndDVRExtension::usage="Ends a DVR extension file"

`Private`$BaseContext::usage="The base context for the package"
`Private`$BaseContext=$Context;

Begin["`Private`"]

$ContextFile=$InputFileName;

IgnoreDVRFunctionShadowing[command_,context_]:=
(*
With[{shortnames={"$DVRDimension","$PointLabels","$FormatGrid","$GridPointsFunction","$KineticMatrixFunction","$PotentialMatrixFunction","$WavefunctionsFunction","$PlotFunction"}},
With[{shadowings=ToExpression@(#<>"::shdw"&/@(Names["*`"<>#]&/@shortnames))},
	Print@shadowings;
	Quiet[command,shadowings]
	]
]
*)
	(*Quiet[*)command;(*,General::shdw]*)
SetAttributes[IgnoreDVRFunctionShadowing,HoldFirst];

$extensionStack={};
LoadDVR[dvrName_,baseDir:_String|Automatic:Automatic,context:_String|Automatic:Automatic,reload:True|False:False]:=
With[{cont=Replace[context,Automatic:>$BaseContext]<>dvrName<>"`"},
	If[reload||Length@Names@(cont<>"*")==0,
	IgnoreDVRFunctionShadowing[Get@FileNameJoin@{
		Replace[baseDir,
			Automatic:>DirectoryName@$InputFileName
			],dvrName<>".m"},
		cont]];
	If[`Private`$extensionStack!={},AppendTo[$ContextPath,cont]];
	];

BeginDVRExtension[dvrName:_String|None:None]:=
	(
	BeginPackage[$BaseContext,{"ObjectOrientedProgramming`"}];
	(*ClearAll@Evaluate@("`"<>dvrName<>"`*");*)
	If[MatchQ[dvrName,_String],Begin@("`"<>dvrName<>"`")];
	AppendTo[$ContextPath,$Context];
	Quiet[AppendTo[`Private`$extensionStack,$Context]];
	);
EndDVRExtension[]:=(
	`Private`$extensionStack=Replace[`Private`$extensionStack,{o___,_}:>{o}];
	If[$Context!=$BaseContext,End[]];EndPackage[];
	)

$DVRRoot=DirectoryName@$InputFileName;
$ConfigFileDirectory=FileNameJoin[{$DVRRoot,"Resources","DVR Files"}];
$KineticMatrixDirectory=FileNameJoin[{$DVRRoot,"Resources","Kinetic Matrices"}];
$PotentialFileDirectory=FileNameJoin[{$DVRRoot,"Resources","Potential Files"}];
$WavefunctionFileDirectory=FileNameJoin[{$DVRRoot,"Resources","Calculated Wavefunctions"}];

End[];
