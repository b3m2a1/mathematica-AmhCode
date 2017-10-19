(* ::Package:: *)

(* ::Section:: *)
(*Event Handlers and UI Elements*)


(* ::Subsection::Closed:: *)
(*Exposed*)


KeyEventField::usage="An input field with simplified event bindings"
ReturnField::usage="A KeyEventField with return newlines by default"
ComplexEventHandler::usage="An event handler that simplifies event handling syntax"
eventParse::usage="Parses a supplied event"
eventTest::usage="Provides a test for a given event"
eventList::usage="Turns supplied events into a list of handler-appropriate events"


(* ::Subsection:: *)
(*Internal*)


Begin["`Private`"];


ClearAll[KeyEventField]
SetAttributes[pHold,HoldAll];
unevalList[l_,head_:List]:=Hold[Block[{pHold=Unevaluated,disV=List},#]&@Thread[pHold[l],head]]/.disV:>head//ReleaseHold;
SetAttributes[unevalList,HoldAll];
Options[KeyEventField]=Flatten@{EventData->{},Options@InputField};
eventParse[e_]/;Head[Unevaluated[e]]=!=pHold:=eventParse[pHold[e]];
eventParse[pHold[e_]][s_]:=eventParse[s,e];
eventParse[s_,e_]:=
		With[{h=Unevaluated[s]},
		(*Print@Head@h;*)
		Which[
			SymbolName@Head[h]=="Event",StringBlock[{ToString@Head[h]=List},If[Length@h==1,Extract[h,{1},ToString],Prepend[Extract[h,List/@Range[2,Length@h]],Extract[h,1]]]]:>e,
			SymbolName@Head[h]=="Click",StringBlock[{ToString@Head[h]=List},Switch[Length@h,0,eventParse[Event["MouseClicked"],e],1,With[{v=h[[1]]},eventParse[Event["MouseClicked",v],e]],2,With[{v=h[[1]],c=h[[2]]},eventParse[Event["MouseClicked",v]&&"MouseClickCount"==c,e]]]],
			Head[h]===Equal,With[{n=Extract[h,{1},Unevaluated],v=Extract[h,{2},Unevaluated]},eventParse[ProgrammingTools`Private`Event[n],If[CurrentValue[n]==v,e]]],
			Head@h===String,{"KeyDown",Evaluate@h}:>e,
			Head@h===Or,With[{l=unevalList[h,Or],par=eventParse[Unevaluated[e]]},Sequence@@(par/@l)],
			Head@h===Plus,Block[{Plus=List},With[{n=Extract[h,{1},Unevaluated],v=Extract[h,{2},Unevaluated]},eventParse[v,If[eventTest[n],e]]]],
			Head@h===And,Block[{And=List},With[{v=Extract[h,{1},Unevaluated],n=Extract[h,{2},Unevaluated]},eventParse[v,If[eventTest[n],e]]]],
			True,h:>e
			]
		];
SetAttributes[eventParse,HoldAll];
eventTest[s_]:=
		With[{h=Unevaluated[s]},
		Which[
			SymbolName@Head[h]=="Event",CurrentValue@Extract[h,{1},ToString],
			Head[h]===Equal,With[{n=Extract[h,{1},Unevaluated],v=Extract[h,{2},Unevaluated]},CurrentValue[n]==v],
			Head@h===String,{"KeyDown",Evaluate@h}:>e,
			Head@h===Or,Thread[eventTest[h],Or],
			Head@h===And,Thread[eventTest[h],And],
			True,False
			]
		];
SetAttributes[eventTest,HoldAll];
eventList[l_]:=
	Module[{keyArr=<||>},
	Do[With[{kp=With[{h=Extract[k,{1,1},Unevaluated],e=Extract[k,{1,2},Unevaluated]},eventParse[h,e]]},
		With[{h=Extract[kp,1],e=Extract[kp,2,Unevaluated]},
			With[{f=Extract[keyArr,Key[h],Unevaluated]},
				If[MemberQ[Keys@keyArr,h],keyArr[h]:=(f;e),keyArr[h]:=e]
			]
		]
		],
	{k,l}];
	keyArr
	];
SetAttributes[eventList,HoldAll];
ComplexEventHandler[expr_,eventRules:{__},ops:OptionsPattern[Options@EventHandler]]:=
	With[{l=Thread[Hold[eventRules]]},
	EventHandler[expr,eventList[Block[{Hold=Unevaluated},l]],ops]
	]
SetAttributes[ComplexEventHandler,HoldAll];
KeyEventField[spec:(_?NoRuleQ):Null,type:(_?NoRuleQ):Expression,
		ops:OptionsPattern[]]:=
	InputField[spec,type,
		FilterRules[{ops},Options@InputField]
		]~EventHandler~eventList[
							Block[{Hold=Unevaluated},Thread@OptionValue[Automatic,Automatic,EventData,Hold]]
							];
SetAttributes[KeyEventField,HoldAll]

NoRuleQ[O_]:=!MatchQ[Head[O],(Rule|RuleDelayed)]
ClearAll[ReturnField]
ReturnField[spec:(_?NoRuleQ):Null,type:(_?NoRuleQ):Expression,
		ops:OptionsPattern[Flatten@{EventData->{},Options@InputField}]]:=
	Block[{evpat,Event},
	Hold[Block[{evpat=Event},
	With[{ke=OptionValue@EventData,names=Extract[#,1]&/@(OptionValue@EventData)},
		KeyEventField[spec,type,EventData->	
			Evaluate@
				If[MemberQ[names,Event["ReturnKeyDown"]],
					ke,
					Prepend[ke,
						Event["ReturnKeyDown"]:>FrontEndExecute[{NotebookWrite[InputNotebook[],"\n",After]}]
					]
				],Evaluate@FilterRules[{ops},Except[EventData]]
		]
	]
	]]//ReleaseHold[ReplaceAll[#,evpat->With[{g=Evaluate@Symbol[$Context<>"Event"]},Print@g;g]]]&
	];
SetAttributes[ReturnField,HoldAll]

ClearAll@traceView2
traceView2[expr_]:=Module[{steps={},stack={},pre,post,show,dynamic},pre[e_]:=(stack={steps,stack};steps={});post[e_,r_]:=(steps=First@stack~Join~{show[e,HoldForm[r],steps]};stack=stack[[2]]);SetAttributes[post,HoldAllComplete];show[e_,r_,steps_]:=Grid[steps/.{{}->{{"Expr  ",Row[{e," ",Style["inert",{Italic,Small}]}]}},_->{{"Expr  ",e},{"Steps",steps/.{{}->Style["no definitions apply",Italic],_:>OpenerView[{Length@steps,dynamic@Column[steps]}]}},{"Result",r}}},Alignment->Left,Frame->All,Background->{{LightCyan},None}];TraceScan[pre,expr,___,post];Deploy@Pane[steps[[1]]/.dynamic->Dynamic,ImageSize->10000]]
SetAttributes[traceView2,{HoldAllComplete}]


(* ::Subsection::Closed:: *)
(*End Tool*)


End[]
