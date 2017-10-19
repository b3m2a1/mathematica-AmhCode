(* ::Package:: *)

(* ::Subsection:: *)
(*Clearing and Setting Attributes*)


Unprotect@Association;
ClearAll@Association;


(* ::Subsection:: *)
(*Internal*)


(* ::Subsubsection:: *)
(*Making the Head*)


System`Association::usage="Association[Subscript[key, 1]->Subscript[val, 1],Subscript[key, 2]->Subscript[val, 2],\[Ellipsis]] or <|Subscript[key, 1]->Subscript[val, 1],Subscript[key, 2]->Subscript[val, 2],\[Ellipsis]|> 
represents an association between keys and values."
Begin["`AssociationPrivate`"];
allowTest[s_]:=MatchQ[Unevaluated[s],_Rule|_RuleDelayed];
assPattern=Association[___?allowTest];


(* ::Subsubsection:: *)
(*Defining Standard Association Rules*)


Association/:HoldPattern[Normal[a:assPattern]]:=ReplacePart[a,0->List];
Association/:HoldPattern[(a:assPattern)[k_]]:=
	With[{l=Normal@a},
		Replace[Replace[Cases[l,(k->v_):>v],{}->Cases[l,(k:>v_):>v],
			{{}->Missing["KeyAbsent",k],l_:>l[[1]]}]]
		];
Set/:HoldPattern[(a:assPattern)[k_]=v_]:=
	With[{l=Normal@a},
		Replace[Cases[l,(Rule[k,_]|RuleDelayed[k,_])->1],{
			{}->Append[a],
			_:>a/.((Rule[k,_]|RuleDelayed[k,_])->Rule[k,v])
				}]
		];


(* ::Subsection:: *)
(*Protecting and Resetting Attributes*)


End[];
Protect@Association;
