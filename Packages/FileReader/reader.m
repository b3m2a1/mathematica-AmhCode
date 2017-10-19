(* ::Package:: *)

(* ::Chapter:: *)
(*File Reader*)

(* ::Section:: *)
(*Exposed Code*)


(* ::Subsection::Closed:: *)
(*Parameters*)


FileReaderTypes::usage="An association of file extensions and reader types"


(* ::Subsection::Closed:: *)
(*Structures*)


LinePattern::usage="A pattern for line matching"
LineBlock::usage="A collection of line patterns"
FileReader::usage="A collection of line patterns and line blocks for file parsing"


(* ::Subsection::Closed:: *)
(*Functions*)


LoadReaders::usage="Loads file readers from a file"
ReadFile::usage="A function to automatically load a reader and parse a file"


(* ::Section:: *)
(*Internals*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Reader*)


(* ::Subsubsection::Closed:: *)
(*LinePattern structure*)


LinePattern/:HoldPattern[
	Set[l_List,
		LinePattern[linPat_,linFunction:_Symbol|_Function|Automatic:Automatic,linNum:_Integer:1]
		]
	]:=Set[l,
		{linPat,
			Replace[linFunction,Automatic:>Replace[linPat,{_Rule|_RuleDelayed->None,_->Identity}]],
			linNum}
		]


(* ::Subsubsection::Closed:: *)
(*LineBlock structure*)


SetAttributes[LineBlock,HoldAllComplete];
LineBlock[start_,linePatterns__LinePattern,end_:Sequence[],repeats:_Integer:1,
	sep:("Separator"->_):("Separator"->Sequence[])]:=
	LineBlock[start,HoldComplete[linePatterns],end,repeats,sep]
LineBlock[start_,lineList_List,end_:Sequence[],repeats:_Integer:1,
	sep:("Separator"->_):("Separator"->Sequence[])]:=
	With[{lb=Replace[HoldComplete[lineList],HoldComplete[{l___}]:>HoldComplete[l]]},
		LineBlock[start,lb,end,repeats,sep]
	];


(* ::Subsubsection::Closed:: *)
(*FileReader function*)


SetAttributes[parseFail,HoldAll];
	parseLine[line_,pattern_,function_:None]:=
	Replace[line,{
		If[function===None,
			Replace[pattern,{
					_Rule|_RuleDelayed->pattern,
					_StringExpression->({pattern}:>line),
					_->(pattern:>line)
					}],
			pattern:>function[line]
			],
		_String:>
			With[{strPat=Replace[pattern,(Rule|RuleDelayed)[p_,_]:>(p/.pat_Pattern:>pat[[2]])]},
				If[StringMatchQ[line,strPat],
					StringCases[line,
						If[function===None,
							Replace[pattern,{
								Except[_Rule|_RuleDelayed]->(pattern:>line)
								}],
						pattern:>function[line]
						]],
					parseFail]
				],
		_->parseFail
	}]


FileReader::nofi="No file: ``"


parseFile[file_String,fileStructure_HoldComplete,importForm_:"Table"]:=
	Block[{parsingFile=file,
		fileStream=OpenRead[file],patterns=fileStructure,currentPattern=None,
		storedPatterns={},blockPath={},blockReps=0,blockStarted=True,
		currentFunction,currentBlockLen,line,result
		},
		If[!FileExistsQ@file,Message[FileReader::nofi,file];Return@$Failed];
		result=Replace[Reap[Do[
			line=Replace[ReadLine[fileStream],{
				s_String:>(Replace[ImportString[s,importForm],
				l_?(importForm=="Table"&):>l[[1]]]),
				EndOfFile:>Return[EndOfFile]
				}];
			Replace[
				Do[
				(*Get the next pattern*)
				If[blockStarted&&currentPattern===None||currentBlockLen==0,
					If[!MemberQ[{HoldComplete[],{}},patterns],
						Replace[patterns[[1]],{
							b:LineBlock[start_,lines:HoldComplete[__],end_,repeats_,"Separator"->sep_]:>(
								AppendTo[blockPath,b];
								AppendTo[storedPatterns,Delete[patterns,1]];
								patterns=b[[2]];
								blockStarted=False;
								blockReps=repeats),
							p_LinePattern:>(
								{currentPattern,currentFunction,currentBlockLen}=p;
								patterns=Delete[patterns,1];
								)
							}],
						Replace[blockPath,{
							{p___,b:LineBlock[start_,lines:HoldComplete[__],end_,repeats_,"Separator"->sep_],c_}:>(
								blockPath={p,b};
								patterns=storedPatterns[[-1]];
								storedPatterns=Delete[storedPatterns,-1];
								blockStarted=False;
								blockReps=repeats),
							{}|{b_}:>Return[End]
							}]
						]
					];
				With[{b=If[Length@blockPath>0,
					blockPath[[-1]],
					(*If there are no blocks, set a simple loop through everything*)
					blockStarted=True;blockReps=0;{_,{},_Sequence,_,_}]},
					(*Test to start the block*)
					If[blockStarted||(blockStarted=
						Replace[line,
							{Replace[Replace[b[[1]],LinePattern[p_,__]:>p],
								{
									(h:Rule|RuleDelayed)[k_,f_]:>h[k,f;True],
									p_:>(p->True)
								}],
							_->False}]),
						(*If the line matches the line block end,
							either loop through the line block again or
							select the next of pattern outside the block*)
						If[MatchQ[line,b[[-3]]],
							If[blockReps===0,
								patterns=HoldComplete[],
								patterns=b[[2]];
								currentPattern=None;
								blockStarted=False;
								Quiet[Sow@b[[-1,2]]];
								blockReps--],
						(*Try to parse the line.
							In case of failure,
							 either move to the next line (if a positive number of matching lines was supplied),
							or move to the next pattern (if a negative one, i.e. an arbitrary number of matching lines, was)
							*)
							Replace[parseLine[line,currentPattern,currentFunction],{
								parseFail:>Which[
									currentBlockLen<0,currentBlockLen=0,
									currentBlockLen>0,Return[Continue]
									],
							   res_:>(currentBlockLen--;Sow@res;Return[Continue])
								}]
							],
						Return[Continue]
						]
					],
				{i,\[Infinity]}],
				Except[Continue]:>Return[]
				],
				{fileLine,\[Infinity]}]
				][[2]],
		{
			{res_}:>res,
			{}:>(Message[FileReader::impfrm,file];$Failed)}
		];
	Close[fileStream];
	result
	]


SetAttributes[FileReader,HoldAllComplete];
FileReader::impfrm="Couldn't parse file ``: incorrect format";
FileReader[blockVars:_List:{},fileStructure:__?(MatchQ[#,_LinePattern|_LineBlock]&),
	postProcessing:_Symbol|_Function:Identity,impPat:_String:"Table"]:=With[{fList=HoldComplete[fileStructure]},
	FileReader[blockVars,fList,postProcessing,impPat]
	];
FileReader[blockVars:_List:{},lineList_List,
	postProcessing:_Symbol|_Function:Identity,impPat:_String:"Table"]:=With[{fileStructure=Replace[HoldComplete[lineList],HoldComplete[{l___}]:>HoldComplete[l]]},
	FileReader[blockVars,fileStructure,postProcessing,impPat]
	];
FileReader[blockVars:_List:{},
	fileStructure_HoldComplete,
	postProcessing:_Symbol|_Function:Identity,
	impPat:_String:"Table"][file_String]:=Block[blockVars,
		postProcessing@parseFile[file,fileStructure,impPat]
	];


(* ::Subsection:: *)
(*Pre-Defined file types*)


(* ::Subsubsection::Closed:: *)
(*fakeAssociation*)


ClearAll@fakeAssociation;
SetAttributes[`fakeAssociation`HoldPattern,HoldAllComplete]
SetAttributes[`fakeAssociation`Pattern,HoldAllComplete]
SetAttributes[`fakeAssociation`Alts,HoldAllComplete]
fakeAssociation/:HoldPattern[Set[sym_Symbol,f_fakeAssociation]]/;(`fakeAssociation`setOverride=!=True):=(
	Block[{`fakeAssociation`setOverride=True},Set[sym,f]];
	sym/:HoldPattern[(s:Set|SetDelayed)[sym[key_],val_]]:=Block[{`fakeAssociation`setOverride=True},
		sym=With[{ass=Evaluate[sym]},s[ass[key],val]]];
	sym/:HoldPattern[(s:Set|SetDelayed)[sym,val:Except[_fakeAssociation]]]:=(
		UpValues[sym]=With[{uv=UpValues[sym]/.{
			HoldPattern:>`fakeAssociation`HoldPattern,
			Pattern:>`fakeAssociation`Pattern,
			Alternatives:>`fakeAssociation`Alts
			}},
			DeleteCases[uv,(
				`fakeAssociation`HoldPattern[
					`fakeAssociation`HoldPattern[
						`fakeAssociation`Pattern[_,`fakeAssociation`Alts[Set,SetDelayed]][
								sym[`fakeAssociation`Pattern[key,_]]
								`fakeAssociation`Pattern[_,_]
								]
							]
						]:>_
						)|(
				`fakeAssociation`HoldPattern[`fakeAssociation`HoldPattern[
					`fakeAssociation`Pattern[_,`fakeAssociation`Alts[Set,SetDelayed]][
								sym,`fakeAssociation`Pattern[_,_]
							]
						]
					]

				)]
			];
		Block[{`fakeAssociation`setOverride=True},s[sym,val]])
	);
fakeAssociation/:HoldPattern[(s:Set|SetDelayed)[(fa:fakeAssociation[rules___])[key_],val_]]:=
Block[{f=fa},
	Do[f=
		With[{r=If[s===Set,k->val,k:>val]},
			Replace[
				Do[Replace[Extract[f,i],
					(Rule|RuleDelayed)[_?(MatchQ[key,#]&),_]:>Return@ReplacePart[f,i->r]
					],
				{i,Length@f}],{
				Null:>Append[f,r]
			}]],
		{k,Replace[key,{_Alternatives:>ReplacePart[key,0->List],Except[_List]->{key}}]}
	];
	f];
fakeAssociation/:HoldPattern[fakeAssociation[rules___][key_]]:=
Replace[
Do[
Replace[r,(Rule|RuleDelayed)[key,val_]:>Return[{key,val}]],{r,{rules}}],{
{key,val_}:>val,
_:>Missing["KeyAbsent",key]
}];
fakeAssociation/:HoldPattern[Keys@fakeAssociation[rules___]]:=First/@{rules};
fakeAssociation/:HoldPattern[Normal@fakeAssociation[rules___]]:={rules};


(* ::Subsubsection::Closed:: *)
(*Automatic Reader Detection*)


FileReader::ftyp="Couldn't determine FileReader to use for file ``";
FileReaderTypes=fakeAssociation[];
FileReader[file_String]:=
	Replace[FileReaderTypes[StringReplace[file,___~~"."~~ext__:>ext]],
_Missing:>(Message[FileReader::ftyp,file];$Failed)
]


(* ::Subsubsection::Closed:: *)
(*ReadFile function*)


ReadFile[file_String]:=Replace[FileReader[file],f_FileReader:>f[file]]


(* ::Subsubsection:: *)
(*Custom Readers*)


`LoadReaders`loadContext=$Context;
LoadReaders::nofi="No file: ``";
LoadReaders::nord="No reader found in file ``";
LoadReaders::nold="Can't load file ``: unsupported type";
LoadReaders[dir_String]/;DirectoryQ@dir:=(
	SetDirectory@dir;
	Begin[`LoadReaders`loadContext];
	Block[{Reader,Extension},
		Do[
			If[MemberQ[{"m","nb","mx","wl"},FileExtension@f],
				Replace[FileExtension@f,
					{"nb":>Cases[Get@f,Cell[b:BoxData[__],"Input",___]:>ToExpression@b,\[Infinity]],
						_:>Get@f}];
				If[MatchQ[{Extension,Reader},{_String|_Alternatives,_FileReader}],
					With[{ext=Extension,rd=Reader},FileReaderTypes[ext]=rd],
					Message[LoadReaders::nord,f]],
				If[FileExtension@f!="",Message[LoadReaders::nold,f]]
				];,
			{f,FileNames[]}]
		];
	End[];
	ResetDirectory[];);
With[{infDir=$InputFileName//DirectoryName},
LoadReaders[fileBase_String]:=With[
	{file=Replace[fileBase,_?(!FileExistsQ@fileBase&):>
			FileNameJoin@{infDir,"_readers",fileBase}
			]},
If[FileExistsQ@file,
	Begin[`LoadReaders`loadContext];
		Block[{Reader,Extension},
			If[MemberQ[{"m","nb","mx","wl"},FileExtension@file],
				Replace[FileExtension@file,
					{"nb":>Cases[Get@file,Cell[b:BoxData[__],"Input",___]:>ToExpression@b,\[Infinity]],
						_:>Get@file}];
				If[MatchQ[{Extension,Reader},{_String|_Alternatives,_FileReader}],
					With[{ext=Extension,rd=Reader},FileReaderTypes[ext]=rd],
					Message[LoadReaders::nord,file]
					],
			If[FileExtension@file!="",Message[LoadReaders::nold,file]]
			]
		];
	End[];,
	Message[LoadReaders::nofi,file];$Failed
	]
]
];


LoadReaders@FileNameJoin@{DirectoryName@$InputFileName,"_readers"}


(* ::Subsection::Closed:: *)
(*End Package*)


End[];
