(* ::Package:: *)

(* ::Section:: *)
(*Debugging Tools*)


(* ::Subsection:: *)
(*Exposed*)


traceView2::usage="A bit of web-sourced code to trace evaluations"
safeTest::usage="Tests value type against unevaluated own values"
findSymbols::usage="Uses safeTest to find symbols in specified context that pass the test"
trueTest::usage="Always returns true, good for finding all symbols"
findContexts::usage="Gets all contexts defined by a parent context"
symbolCatalog::usage="Uses findContexts to make a panel to look at symbol definitions"
panThrough::usage="Pans through a list"
ignoreProtect::usage="Runs a command without protection"
deletePackage::usage="Removes a package"


(* ::Subsection:: *)
(*Internal*)


Begin["`Private`"];


Clear[safeTest];
safeTest[u_Symbol,test_:NumberQ,nullResponse_:False]:=
With[{O=Quiet[With[{U=OwnValues[u]},If[Length[$MessageList]>0,{},U]]]},If[Length@O>0,With[{E=Extract[O,{1,2},Unevaluated]},E//test],nullResponse]];
SetAttributes[safeTest,HoldFirst]
trueTest[u_]:=True;
SetAttributes[trueTest,HoldFirst];
findSymbols[contString_String,OptionsPattern[{Evaluate->NumberQ,Context->"`*"}]]:=
With[{test=OptionValue@Evaluate,N=Names@(contString<>OptionValue@Context)},
	{Length@N,
	With[{R=Reap[Do[With[{r=ToExpression[n,InputForm,Unevaluated]},
		If[safeTest[r,test],Sow@(n->Evaluate@r)]],{n,N}]][[2]]},
	If[Length@R>0,Flatten[R,1],R]//If[StringTake[contString,{1}]==="*",
					Block[{a=<||>,r=#},
						Do[With[{c=With[{str=StringSplit[s[[1]],"`"][[1]]},If[str==s[[1]],"System|Global",str]]},
										If[MemberQ[Keys@a,c],AppendTo[a[c],s],a[c]={s}]],{s,r}];a],#]&
	]
	}
];
findContexts[contString_String,OptionsPattern[{Null->False,Evaluate->None,Context->"`*"}]]:=
With[{cString=contString<>"`",b=OptionValue@Null,sTest=If[StringLength@contString>0,StringTake[contString,{1}]!="*",False]},
With[{N=Names@(contString~~OptionValue@Context)},
	Block[{a=<||>,r=With[{t=OptionValue@Evaluate},If[t===None,N,Select[N,With[{E=ToExpression[#,InputForm,Unevaluated]},safeTest[E,t,b]]&]]]},
		Do[With[{c=With[{str=StringSplit[If[sTest,StringTrim[s,cString],s],"`"][[1]]},If[str==s,"System|Global",str]]},
				If[MemberQ[Keys@a,c],AppendTo[a[c],s],a[c]={s}]],{s,r}];
					Do[a[c]=Sort[a[c]],{c,Keys@a}];
					a]
	]
];
symbolCatalog::nosym="No symbols found matching ``";
symbolCatalog[(contString:(_?StringQ):""),ops:OptionsPattern[{Null->False,Evaluate->None,Context->"`*"}]]:=
With[{f=findContexts[contString,ops]},
	With[{k=Keys@f//Sort},
		If[Length@k>0,
		DynamicModule[{c=k[[1]],d,r=Null},
			{If[Length@k>1,Control@{{c,k[[1]],"Context"},k,ControlType->PopupMenu},{"Context",Spacer[10],Style[c,Medium]}//Row],
			Dynamic[If[Length@f[c]>1,Control@{{d,f[c][[1]],"Symbol"},f[c],ControlType->PopupMenu},{"Symbol",Spacer[10],Style[d=f[c][[1]],Medium]}//Row],TrackedSymbols:>{c}],
			Dynamic[With[{o=ToExpression[d,InputForm,Unevaluated],e=ToExpression[d]},If[Head@e===Symbol,
																	({{{Spacer[15],HoldForm[e]}//Row,SpanFromLeft},
																	{Spacer[25],"Down Values:",Spacer[10],DownValues@o//Column//Panel,SpanFromLeft},
																	{Spacer[25],"Up Values:",Spacer[10],UpValues@o//Column//Panel,SpanFromLeft},
																	{Spacer[25],"Sub Values:",Spacer[10],SubValues@o//Column//Panel,SpanFromLeft},
																	{Spacer[25],"Format Values:",Spacer[10],FormatValues@o//Column//Panel,SpanFromLeft},
																	{Spacer[25],"N Values:",Spacer[10],NValues@o//Column//Panel,SpanFromLeft}}//Grid[#,Alignment->Left]&),
																	{Spacer[15],HoldForm[e],Spacer[150],Head@e}//Row]]//Pane]}//Column//Panel],
		Message[symbolCatalog::nosym,contString<>OptionValue@Context];
		$Failed
		]
	]
];
symbolCatalog[contList_List,ops:OptionsPattern[{Null->False,Evaluate->None,Context->"`*"}]]:=
	symbolCatalog[Context->Alternatives@@(StringJoin[#,OptionValue@Context]&/@contList),FilterRules[{ops},Except[Context]]];
frontEndModule[varSpec_,body_]:=Block[{Manipulate`Dump`$eDynamicModule=True},DynamicModule[varSpec,body]]

(*capDict=Capitalize/@DictionaryLookup["*"];
pairDict[dec2_,dec1_:0,append_:None,inputDict_:capDict]:=With[{p1=If[dec1>1,dec1,1+Floor@(Length[inputDict]*dec1)],p2=If[dec2>1,dec2,Floor@(Length[inputDict]*dec2)]},Table[If[append=!=None,append<>s1<>s2,s1<>s2],{s1,inputDict},{s2,inputDict[[p1;;p2]]}]//Flatten
]
Table[s1<>s2,{s1,capDict[[1;;10]]},{s2,capDict[[1;;10]]}]
Clear@currentValueTest
currentValueTest[dec2_,dec1_:0,append_:None,inputDict_:capDict,evAgainst_:None]:=
With[{D=pairDict[dec2,dec1,append,inputDict]},{Length@D,Reap[Do[With[{v=If[evAgainst===None,CurrentValue[d],CurrentValue[$FrontEnd,d]]},If[v=!=$Failed,Sow@(d\[Rule]v)]],{d,D}]
][[2]]//Flatten}]*)
panThrough[l_,f_:Row,h_:None]:=
With[{L=Length@l},
	If[h===None//Not,
	Manipulate[f@(l[[i]]),
		{{i,1,"Index"},1,L,1},
		{{i,1,"Name"},Block[{j=1},Table[j++->h[n],{n,l}]],PopupMenu},
		Row@{Button["<-",If[--i==0,i++]],Button["->",If[++i>L,i--]]}
	],
	Manipulate[f@(l[[i]]),
		{{i,1,"Index"},1,L,1},
		Row@{Button["<-",If[--i==0,i++]],Button["->",If[++i>L,i--]]}
	]]
];

SetAttributes[ignoreProtect,HoldAll]
ignoreProtect[sym_,cmd_]:=Block[{ignoreProtectReturnValue=None},Unprotect@sym;ignoreProtectReturnValue=cmd;Protect@sym;ignoreProtectReturnValue];
deletePackage[pkg_String]:=With[{s=StringTrim[pkg,"`"]<>"`"},
If[pkg!="ProgrammingTools`",Quiet[Remove@(Evaluate[s<>"*"])]];
ignoreProtect[$Packages,$Packages=DeleteCases[$Packages,s]];
If[pkg=="ProgrammingTools`",Quiet[Remove@(Evaluate[s<>"*"])]];
];


(* ::Subsection::Closed:: *)
(*End Tool*)


End[]
