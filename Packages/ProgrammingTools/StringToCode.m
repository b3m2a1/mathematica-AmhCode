(* ::Package:: *)

(* ::Section:: *)
(*StringToCode tool*)


(* ::Subsection::Closed:: *)
(*Exposed*)


StringBlock::usage="A block where variables are specified as strings"
StringFunction::usage="A function where variables are specified as strings"


(* ::Subsection::Closed:: *)
(*Internal*)


Begin["`Private`"];


StringBlock[inList_List,args_]:=
	Block[{setvars,l=Hold[inList]},
		l=l/.HoldPattern[Set[s_,v_]]:>(s<>"="<>ToString@Unevaluated[v])//ReleaseHold;
		Replace[(Hold[Block[{setvars},args]]/.setvars->Join@@MakeExpression@l),{Hold[Block[{HoldComplete[x__]},a_]]:>Hold[Block[{x},a]]}
	]//ReleaseHold
	];
SetAttributes[StringBlock,HoldAll]
StringFunction[l_,args_String]:=
		Block[{setvars,repR},ReplaceAll[(Hold[Function[setvars,repR]]/.{setvars->MakeExpression@ToString@l,repR->MakeExpression@args}),
				{HoldComplete[a__]:>a}]
			]//ReleaseHold;
StringFunction[l_,args_]:=
		Block[{setvars,repR},
			ReplaceAll[(Hold[Function[setvars,repR]]/.{setvars->MakeExpression@ToString@l,repR->MakeExpression@ToString@Unevaluated[args]}),
				{HoldComplete[a_]:>a}]
			]//ReleaseHold;
SetAttributes[StringFunction,HoldAll]


(* ::Subsection::Closed:: *)
(*EndTool*)


End[]
