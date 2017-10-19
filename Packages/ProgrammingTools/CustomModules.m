(* ::Package:: *)

(* ::Section:: *)
(*Custom Modules*)


(* ::Subsection:: *)
(*Exposed*)


SynchronizedModule::usage="A dynamic module that synchronizes across the front end (not preserved across sessions)"
frontEndModule::usage="A module where only dynamic variables are localized (lightweight dynamic module)"
TrackedModule::usage="A dynamic module that supports symbol tracking"
OutputtingModule::usage="A dynamic module that interprets to a dynamic local symbol"
OutputTrackedSymbol::usage="Tracks a dynamic symbol"
OutputTrackedKey::usage="Tracks a dynamic module's serial no"
GetTrackedModuleInfo::usage="Gets module info from a TrackedModule"


(* ::Subsection:: *)
(*Internal*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Tracked Module*)


TrackedModule`tracker={};
TrackedModule`track[num_Integer]:=If[(FilterRules[TrackedModule`tracker,num]//Length)==0,TrackedModule`tracker=TrackedModule`tracker~Join~{num->EvaluationCell[][[1]],EvaluationCell[][[1]]->num}];
TrackedModule`trackerFunction[trackValue_Integer]:=trackValue;
TrackedModule`trackerFunction[trackingSymbol_Symbol]:=
	With[{s=StringSplit[ToString@trackingSymbol,"`",3]},
		If[s[[1]]=="FE"&&s[[2]]=="TrackedModule",
			With[{n=StringSplit[s[[3]],"$$"][[2]]//ToExpression},
				TrackedModule`track[n];n],trackingSymbol
			]
		];
Options[TrackedModule]=Options@DynamicModule;
TrackedModule[{vars__},body_,ops:OptionsPattern[]]:=
	ReplaceAll[
		ReplaceAll[
			HoldComplete[
				DynamicModule[
					TrackedModule`varsetter,
						{(
							{
							Dynamic[TrackedModule`stateTracker=
								With[{n=TrackedModule`trackerFunction[TrackedModule`trackVar]},
									If[Head@n===Integer,n,TrackedModule`stateTracker]
								]
							],
				Dynamic[TrackedModule`tracker]
					}~Style~.01),body}//Row,
				ops
				]
			],TrackedModule`varsetter->TrackedModule`trackHold[{TrackedModule`trackVar,TrackedModule`stateTracker,HoldComplete[vars]}]
		],TrackedModule`trackHold[{TrackedModule`trackVar,TrackedModule`stateTracker,HoldComplete[a___]}]:>{TrackedModule`trackVar,TrackedModule`stateTracker,a}
	]//ReleaseHold;
SetAttributes[TrackedModule,Attributes[DynamicModule]];TrackedModule//Unprotect;
GetTrackedModuleInfo[module_DynamicModule]:=
	With[{n=(module//Delete[{2,1,2}]//Setting)[[1,1,1]]},
		If[Head[n[[1]]]===Integer,
			{n[[1]],CellObject@Replace[n[[1]],n[[2]]]},
			$Failed
		]
	];
CellToTrackedModuleKey[cellObject_CellObject,number_:1]:=With[{o=FilterRules[TrackedModule`tracker,cellObject[[1]]]},If[Length@o>=number,o[[number,2]],$Failed]];
OutputCell[param_:None,ops:OptionsPattern@Options@Dynamic]:=If[param===None,Dynamic[EvaluationCell[],ops],Dynamic[param@EvaluationCell[],ops]]
getDMKey[cell_,number_]:=Module[{m=CellToTrackedModuleKey[cell,number]},If[m===$Failed,m=CellToTrackedModuleKey[cell,number],m]]
OutputTrackedKey[number_:1,ops:OptionsPattern@Options@Dynamic]:=OutputCell[getDMKey[#,number]&,ops]
OutputTrackedSymbol[symbol_,number_:1,ops:OptionsPattern@Options@Dynamic]:=With[{s=ToString@Unevaluated[symbol]},OutputCell[Function[o,ToExpression["FE`"<>s<>"$$"<>ToString@getDMKey[o,number],StandardForm,Unevaluated]],ops]]
SetAttributes[OutputTrackedSymbol,HoldFirst];
OutputtingModule[{vars__},sym_,body_,ops:OptionsPattern[Flatten@{Options@TrackedModule,Evaluate->True}]]:=Interpretation[TrackedModule[{vars},body,ops],With[{s=OutputTrackedSymbol[sym]//Setting},If[OptionValue@Evaluate,s,Unevaluated@s]]]
SetAttributes[OutputtingModule,Attributes[TrackedModule]];
SyntaxInformation[TrackedModule]=Flatten@{SyntaxInformation@DynamicModule,"LocalVariables"->{"Module",All}}


(* ::Subsubsection:: *)
(*Synchronized Module*)


Clear@SynchronizedModule;
Unprotect@`SynchronizedModule`Hold;
ClearAll@"SynchronizedModule`*";
SetAttributes[$edmHold,HoldAll];
SynchronizedModule[{args___},
	body_,
	keyNumber:(_?(MatchQ[#,(None|Null)]&)):None,
	merge:_?(MatchQ[#,True|False|Not[Alternatives[___]]|{___String}]&):False]:=
	With[{m=$ModuleNumber,ar=$edmHold[args]},
		(*With[{e=Replace[Replace[Hold[Module[{$edmArgs},body]],{$edmArgs}->ar],{$edmHold[]:>Null,$edmHold[a___]:>a}]//ReleaseHold},*)
		SynchronizedModule[{args},
			Evaluate@(ReplaceAll[ReplaceAll[Hold[Module[{$edmArgs},body]],{$edmArgs$}->{ar}],{$edmHold[a___]}:>{a}]//ReleaseHold),
			m,merge
		(*]*)
		]
	];
SynchronizedModule[{args___},body_,keyNumber_Integer]:=SynchronizedModule[{args},body,keyNumber,False];
SetAttributes[SynchronizedModule,HoldAll];
Clear@`SynchronizedModule`Inject;
SetAttributes[`SynchronizedModule`Hold,HoldAll];
Protect@`SynchronizedModule`Hold;
`SynchronizedModule`vars[sm_SynchronizedModule,toStrings_:False]:=
With[{v=Thread[Extract[sm,1,HoldPattern]]/.{HoldPattern[Set[a_,b_]]:>a,_Sequence->{}}},
	If[toStrings&&Length@v>0,ToString/@Thread[ReplacePart[Thread[v,HoldPattern],0->Unevaluated]],v]
	];
`SynchronizedModule`values[sm_SynchronizedModule,unEval_:False]:=
	With[{v=`SynchronizedModule`vars[sm,True],k=ToString@`SynchronizedModule`key[sm]},
			ToExpression[#<>"$"<>k&/@v,StandardForm,If[unEval,Unevaluated,Evaluate]]
		];
`SynchronizedModule`getvalue[sm_SynchronizedModule,sym_String]:=
	ToExpression[sym<>"$"<>`SynchronizedModule`key[sm]];
`SynchronizedModule`setvalue[sm_SynchronizedModule,sym_String,val_]:=
	With[{set=ToExpression[sym<>"$"<>ToString[`SynchronizedModule`key[sm]],StandardForm,HoldPattern]},
			set=val];
`SynchronizedModule`body[sm_SynchronizedModule,repFlag_:False]:=(Extract[sm,2,`SynchronizedModule`Hold]/.If[repFlag//TrueQ,`SynchronizedModule`replacevars[sm,False],None->None]);
`SynchronizedModule`key[sm_SynchronizedModule]:=Extract[sm,3];
`SynchronizedModule`replacevars[sm_SynchronizedModule,fromVars_:True,replaceUnique_:False]:=
	With[{vars=`SynchronizedModule`vars[sm],(*body=SynchronizedModule`body[sm],*)key=`SynchronizedModule`key[sm]},
		If[fromVars,
			With[{r=Evaluate@If[Length@vars>0,
						ToExpression[
							(#<>If[replaceUnique,"\[NumberSign]"<>ToString@key<>"$","$"]<>ToString@key&)/@(
								ToString/@Thread[ReplacePart[Thread[vars,HoldPattern],0->Unevaluated]]
									),StandardForm,`SynchronizedModule`Hold],{}]},
					Thread[vars:>r]
				],
			With[{vars2=Evaluate@If[Length@vars>0,
							ToExpression[
							(#<>"$"<>ToString@key&)/@(
								ToString/@Thread[ReplacePart[Thread[vars,HoldPattern],0->Unevaluated]]
									),StandardForm,HoldPattern],
									{}]},
				If[replaceUnique,
					With[{r=Evaluate@ToExpression[(#<>"\[NumberSign]"<>ToString@key&)/@(ToString/@Thread[ReplacePart[Thread[vars,HoldPattern],0->Unevaluated]]),StandardForm,`SynchronizedModule`Hold]},
						{Thread[vars2:>r],Thread[vars:>r]}
						],
					With[{r=Block[{HoldPattern=`SynchronizedModule`Hold},vars]},
						Thread[vars2:>r]
						]
					]
				]
			]
		];
allowTest[s_]:=MatchQ[Unevaluated[s],(_Symbol|_Set)];
SetAttributes[allowTest,HoldAll];
`SynchronizedModule`Inject[syncMod_SynchronizedModule,vars:{__?allowTest}|_Symbol|_Set|None,
											body_:Null,join_:CompoundExpression]:=
	(With[{module=syncMod,
			Old=(`SynchronizedModule`vars[syncMod,True]),
			New=Replace[#,{
					Hold[None]:>Sequence[],
					Hold[Set[sym_,val_]]:>With[{s=ToString[Unevaluated[sym]]},
											`SynchronizedModule`setvalue[syncMod,s,val];s],
					Hold[o_]:>ToString[Unevaluated[o]]
					}]&/@Replace[Unevaluated[vars],{l_List:>Thread[Hold[l]],e_:>{Hold[e]}}]
					
			},
		
		With[{m=ToString@module[[3]],R=Old~Join~New},
			With[{E=((#<>"$"<>m)&/@R)~ToExpression~Sequence[StandardForm,Unevaluated]},
				ReplacePart[module,{
					{1}->ToExpression[Old,StandardForm,`SynchronizedModule`Hold]
							~Join~
						ReplaceAll[New,HoldPattern->`SynchronizedModule`Hold],
					{2}->(Replace[join,CompoundExpression->If[body===Null,First,CompoundExpression]][
							`SynchronizedModule`body[module],
							`SynchronizedModule`Hold[body]/.Thread[ToExpression[R,StandardForm,HoldPattern]:>E]])
					}](*~Replace~(o_\[RuleDelayed](Print@o;o))*)
				]
		]
	]//.`SynchronizedModule`Hold[a___]:>Unevaluated[a])
SetAttributes[`SynchronizedModule`Inject,HoldAll];

Clear@`SynchronizedModule`Copy;
`SynchronizedModule`Copy[syncMod_SynchronizedModule,newKey_:None,copyVars_:False]:=
	Module[{oList,nList,rList,o=syncMod[[3]],
		m=If[newKey===None,$ModuleNumber+1,newKey],r,
			syms=`SynchronizedModule`vars[syncMod,True]},
		oList=ToExpression[#<>"$"<>ToString@o&/@syms,StandardForm,HoldPattern];
		nList=ToExpression[#<>"$"<>ToString@m&/@syms,StandardForm,Unevaluated];
		rList=ToExpression[#<>"$"<>ToString@m&/@syms,StandardForm,`SynchronizedModule`Hold];
		r=ReplacePart[syncMod/.Thread[oList->rList],{3->m}]/.`SynchronizedModule`Hold[a___]:>a;
		If[copyVars,MapThread[(#1=ReleaseHold@#2)&,{nList,oList}]];
		r//ReleaseHold
	];

`SynchronizedModule`Combine[syncMod1_SynchronizedModule,syncMod2_SynchronizedModule,join_:CompoundExpression,mergeModuleVariables_:Automatic,key_:Automatic]:=
	With[{m=If[Head@key===Integer,key,$ModuleNumber+1],k2=syncMod2[[3]],
			mergeVariables=If[MatchQ[mergeModuleVariables,(True|False|_List|_Not)],mergeModuleVariables,syncMod2[[4]]]},
	With[{sm2=If[m=!=k2,`SynchronizedModule`Copy[syncMod2,m,True],syncMod2],
			sm1=If[m=!=syncMod1[[3]],`SynchronizedModule`Copy[syncMod1,m,True],syncMod1]},
		With[{v1=`SynchronizedModule`vars[sm1,True],
				v2=`SynchronizedModule`vars[sm2,True]
				(*/.(`SynchronizedModule`replacevars[sm2,False])*)},
			(*Print@{b1,b2};*)
			With[{vl=Switch[mergeVariables,
						True,DeleteDuplicates[v1~Join~v2],
						_List,v1~Join~Intersection[v2,ToString/@mergeVariables]~Join~Block[{c=Complement[v2,mergeVariables]},Table[r->r<>"$key"<>ToString@k2<>"key",{r,c}]],
						_Not,v1
								~Join~
							Cases[v2,ReplacePart[mergeVariables,0->Except]]
								~Join~
							Block[{c=DeleteCases[v2,ReplacePart[mergeVariables,0->Except]]},
									Table[r->r<>"$key"<>ToString@k2<>"key",{r,c}]],
						_,Block[{L=v1~Join~Complement[v2,v1],i=Intersection[v1,v2]},
							Do[AppendTo[L,r->r<>"$key"<>ToString@k2<>"key"],{r,i}];L]
						]},
			With[{b1=`SynchronizedModule`body[sm1],b2=`SynchronizedModule`body[sm2]},
				With[{bJ=If[mergeVariables//TrueQ,
							join[b1,b2],
							With[{r=(ToExpression[#[[1]]<>"$"<>ToString@m,StandardForm,HoldPattern]->
								With[{s=#[[2]]<>"$"<>ToString@m,
										val=#[[1]]<>"$"<>ToString@k2},
									Evaluate@ToExpression[s,StandardForm,HoldPattern]=ToExpression@val;
									ToExpression[s,StandardForm,`SynchronizedModule`Hold]
								])&/@Select[vl,Head@#===Rule&]},
								With[{bz=b2/.r},
									Thread[join[b1,bz],Hold]/.`SynchronizedModule`Hold[a___]:>a
									]
								]]},
					(ReleaseHold@Thread[
							ReplacePart[sm1,
								{1->Thread[(ToExpression[#,StandardForm,Hold]&/@Table[If[MatchQ[r,_String],r,r[[2]]],{r,vl}]),Hold],
								2->bJ}],Hold])//.`SynchronizedModule`Hold[a___]:>a
					]
				]
				]
			]
		]
		];

`SynchronizedModule`TemplateCombine[template_,syncMods:(_SynchronizedModule|{___SynchronizedModule}),mergeVariables_:Automatic,key_:Automatic]:=
	Block[{syncMod1=If[Head@template===SynchronizedModule,template,SynchronizedModule[{},template]],
			smList=If[Head@syncMods===List,syncMods,{syncMods}],i=1},
		With[{mergeList=Switch[mergeVariables,
								Automatic,#[[4]]&/@smList,
								_List,mergeVariables,
								_,{mergeVariables}]},
		Do[
		With[{k1=If[Head@key===Integer,key,`SynchronizedModule`key[syncMod1]],
				k2=`SynchronizedModule`key[syncMod2],
				merge=mergeList[[Mod[i++,Length@mergeList,1]]]},
			syncMod1=`SynchronizedModule`Combine[syncMod1,syncMod2,(#1/.k2->#2)&,merge,k1];
			],
			{syncMod2,smList}
			]
		(*MapThread[(#2=#1)&,vLists];*)
		];
		syncMod1
		]


`SynchronizedModule`Column[sm1_SynchronizedModule,sm2_SynchronizedModule,ops:OptionsPattern[Options@Column]]:=
		`SynchronizedModule`Combine[sm1,sm2,Column[{##},ops]&]
`SynchronizedModule`Row[sm1_SynchronizedModule,sm2_SynchronizedModule,ops:OptionsPattern[Options@Row]]:=
		`SynchronizedModule`Combine[sm1,sm2,Column[{##},ops]&]
Format[o:SynchronizedModule[{args___},body_,keyNumber_?(#=!=None&),mergeKey_?(MatchQ[#,True|False|Not[Alternatives[___]]|{___String}]&)]]:=Interpretation[body,o(*SynchronizedModule[HoldForm@{args},body,keyNumber]*)];


(* ::Subsection::Closed:: *)
(*End Tool*)


End[];
