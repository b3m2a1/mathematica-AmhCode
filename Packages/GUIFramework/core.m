(* ::Package:: *)

(* ::Chapter:: *)
(*GUI Package*)


(*Begin["`GUIBaseObjects`"];
Insert[$ContextPath,$Context<>"`GUIBaseObjects`"];*)


(* ::Section:: *)
(*Exposed Code*)


(* ::Subsection::Closed:: *)
(*Exposed Classes*)


(*ClearAll/@{GUIBaseWidgetClass,GUIRootClass,GUIGeometryManagerClass,GUIWidgetClass}*)


GUIBaseWidgetClass::usage="Base class for all GUI objects. Specifies universal methods, although these are often overridden"
GUIRootClass::usage="Root class on which to pack GUIObjects."
GUIGeometryManagerClass::usage="Geometry manager class."
GUIWidgetClass::usage="Basic GUI widget. Extended as pseudoclasses via constructors."


(* ::Subsection::Closed:: *)
(*Exposed Constructors*)


(*ClearAll/@{GUIRoot,GUIWidget,GUIFrame,GUILabel,GUIButton}*)


GUIRoot::usage="Constructor for a GUIRootClass. Simplifies input syntax."
GUIWidget::usage="Constructor for a general GUIWidget"
GUIFrame::usage="Constructor for a frame using GUIWidget"
GUILabel::usage="Constructor for a label using GUIWidget"
GUIButton::usage="Constructor for a button using GUIWidget"


(* ::Subsection::Closed:: *)
(*Exposed Parameters*)


$Widget::usage="The widget generated by a constructor function"


(* ::Section:: *)
(*Internal Code*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Base Widget Class*)


(* ::Subsubsection::Closed:: *)
(*GUIBaseWidgetClass*)


GUIBaseWidgetClass:=BeginClass[];


(* ::Subsubsection:: *)
(*Bound method: Init*)


InitializationFunction[self_,name_,id_,ops:OptionsPattern[{___}]]:=
(self::ChildList=None;
self::PackLayout=None;
self::Settings=Association[ops];
self::Bindings=<||>;
self::GridLayout=None;
self::PlaceLayout=None;
self::GeometryMode="Pack";
self::Container=False;
self::Visible=False;
self::ID=id;
self::UpdateSymbol="update$"<>ToString@id;
self::Name=name;);


(* ::Subsubsection:: *)
(*Bound method: UpdateSettings*)


BoundMethod[UpdateSettings,self_,ops:OptionsPattern[{__}]]:=
Module[{settings=self::Settings,new=Thread[Hold[{ops}]]},
Do[Replace[n,{
		Hold[Set[key_,value_]]:>(settings[key]=value),
		Hold[SetDelayed[key_,value_]]:>(settings[key]:=value),
		Hold[Rule[key_,value_]]:>(settings[key]=value),
		Hold[RuleDelayed[key_,value_]]:>(settings[key]:=value)
		}],{n,new}];
self::Settings=settings;
self::Root::Update[];
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Bind*)


BoundMethod[Bind,self_,eventList__?(MatchQ[#,(_Rule|_RuleDelayed)]&)]:=
Module[{events=self::Bindings,new=(Thread[Hold[{eventList}]]/.{$Widget->self})},
Do[events[Extract[n,{1,1},Unevaluated]]:=Evaluate@Extract[n,{1,2},Unevaluated],{n,new}];
self::Bindings=events;
self::Root::Update[];
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Unbind*)


BoundMethod[Unbind,self_,keys__]:=
With[{b=self::Bindings//Normal,k=(Thread[{keys}:>c_]~Join~Thread[{keys}->c_])},
	self::Bindings=DeleteCases[b,Alternatives@@k]//Association;
	self::Root::Update[];
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Display*)


BoundMethod[Display,self_,NotebookObject->True]:=self::Root::Display[NotebookObject->OptionValue@NotebookObject];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection:: *)
(*Root Class*)


(* ::Subsubsection::Closed:: *)
(*GUIRootClass*)


GUIRootClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends GUIBaseWidgetClass*)


ParentClasses={GUIBaseWidgetClass};


(* ::Subsubsection:: *)
(*Bound method: Init*)


InitializationFunction[self_,name_:"GUIRoot",ops:OptionsPattern[{___}]]:=
(SuperclassDictionary[self,"Init"][self,name,$ModuleNumber];
self::Widgets=<||>; 
self::Settings=Association@@Flatten@{{ops},
	WindowTitle->name,
	Padding->{2,2,2,2},
	ImageSize->{300,300}};
self::Container=True;
self::UpdateInterval=.01;
self::UpdateFlag=False;
(*self::Evaluating=False;*)
self::Root=self;
self::GeometryManager=GUIGeometryManagerClass::New[];);


(* ::Subsubsection::Closed:: *)
(*Bound method: NameToWidget*)


BoundMethod[NameToWidget,self_,name_]:=self::Widgets[name];


(* ::Subsubsection::Closed:: *)
(*Bound method: Display*)


(* ::Text:: *)
(*If NotebookObject is True, displays in a new pane. Uses the objects UpdateSymbol as the dynamic object. Done for Dynamic efficiency.*)


BoundMethod[Display,self_,NotebookObject->True]:=
With[{s=ToExpression[self::UpdateSymbol,StandardForm,SynchronizedModule`Hold],int=self::UpdateInterval},
	With[{h=Hold[s=self::Expression;
		Dynamic[If[self::UpdateFlag,self::Update[]];s,UpdateInterval->int,TrackedSymbols:>{}]]/.SynchronizedModule`Hold[s_]:>s,S=self::Settings//Normal},
			Switch[OptionValue@NotebookObject,
				False,
					self::Visible=True;
					h//ReleaseHold,
				True,
					With[{o=CreateDialog[h//ReleaseHold]},
						SetOptions[o,NotebookEventActions->{"WindowClose":>(self::Destroy[])}~Join~CurrentValue[o,NotebookEventActions],
							FilterRules[S,AbsoluteOptions@o~Join~Options@CreateWindow]];
							self::Visible=o
						],
				_NotebookObject,
					With[{o=CreateDialog[h//ReleaseHold,OptionValue@NotebookObject]},
						SetOptions[o,NotebookEventActions->{"WindowClose":>(self::Destroy[])}~Join~CurrentValue[o,NotebookEventActions],
							FilterRules[S,AbsoluteOptions@o~Join~Options@CreateWindow]];
						self::Visible=o
						]
				]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound property: Children*)


BoundProperty[Children,self_]:=With[{W=Normal@self::Widgets,C=self::ChildList},Thread[C->(C/.W)]];


(* ::Subsubsection::Closed:: *)
(*Bound method: Destroy*)


(* ::Text:: *)
(*If displaying as a new window, destroys that and sets visible to false. Sets the UpdateSymbol to Null;*)


BoundMethod[Destroy,self_]:=
With[{v=self::Visible,s=ToExpression[self::UpdateSymbol,StandardForm,Unevaluated]},
s=Null;
If[MatchQ[v,_NotebookObject],NotebookClose[v]];
self::Visible=False;
];


(* ::Subsubsection:: *)
(*Bound property: Expression*)


(* ::Text:: *)
(*Uses its geometry manager to calcuate the appropriate layout and then merges all child widgets into a single synchronized module. Done for potential state-sharing.*)


BoundProperty[Expression,self_]:=
(*If[self::Evaluating===False,
self::Evaluating=True;*)
With[{b=Normal@self::Bindings,e=self::GeometryManager::ManageGeometry[self]},
	(*self::Evaluating=False;*)
	If[Length@b>0,
		SynchronizedModule`Inject[e,None,Null,(ComplexEventHandler[#,b]&)],
		e
		]
	];(*,
	With[{id=self::ID},
		SynchronizedModule[{},Panel["",FilterRules[self::Settings,Options@Panel]]//Deploy,id]
		]
];*)


(* ::Subsubsection:: *)
(*Bound method: Update*)


(* ::Text:: *)
(*Saves current variable values, has the GeometryManager update the current layout, displays that, and then copies variable values back.*)


BoundMethod[Update,self_]:=
With[{s=ToExpression[self::UpdateSymbol,StandardForm,Unevaluated]},
	If[self::Visible=!=False,
		With[{vLists={SynchronizedModule`values[Evaluate@s,True],SynchronizedModule`values[Evaluate@s]}},
			s=With[{b=Normal@self::Bindings,e=self::GeometryManager::ManageGeometry[self(*,Update*)]},
				If[Length@b>0,
					SynchronizedModule`Inject[e,None,Null,(ComplexEventHandler[#,b]&)],
					e
					]
				];
			MapThread[If[#1=!=#2,#1=#2]&,vLists];
			s
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: UpdateSettings*)


(* ::Text:: *)
(*Calls the base widget UpdateSettings process and then applies further styling to any displayed window.*)


BoundMethod[UpdateSettings,self_,ops:OptionsPattern[{___}]]:=
(SuperclassDictionary[self,"UpdateSettings"][self,ops];
With[{o=self::Visible},
If[MatchQ[o,_NotebookObject],
SetOptions[o,FilterRules[self::Settings,AbsoluteOptions@o~Join~Options@CreateWindow]]]
]
);


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*Geometry Manager Class*)


(* ::Subsubsection::Closed:: *)
(*GUIGeometryManagerClass*)


GUIGeometryManagerClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Class method: ManageGeometry*)


(* ::Text:: *)
(*Takes a widget and management mode and creates an appropriate SynchronizedModule with the correct graphical layout.*)
(*The idea is to calculate a geometry template as a grid for grid or pack or nothing for place*)
(*This grid will should be nested such that each subwidget has its own grid*)
(*The overall template should present the proposed width and height as its second element*)
(**)
(*Adjustments need to be made for nested widets, though, as their positions should not be scaled by the primary positions, but rather relative to the heights and widths of the subwidget grid*)
(**)
(*The idea for this is to take a position grid like *)
(*{*)
(* {{1,1}, {1,2}, 	       {1,3},	 {1,4}},*)
(* {{2,1}, {2,2}, {{ {{1,1}, {1,2}} }}, {2,4}},*)
(*		 {{2,1}, {2,2}}*)
(* {{3,1}, {3,2},	      {3,3}, 	 {3,4}}*)
(*} *)
(*and get everything to fit appropriately*)


SetAttributes[geometryManagementInsetCommand,HoldAllComplete];
ClassMethod[ManageGeometry,cls_,widget_,mode_:Automatic]:=
With[{replacements=With[{lessThan=#1,size=#2,template=#3},{
geometryManagementInsetCommand[replaceObject_,replaceCommand_]:=
	HoldComplete[{gridPosition=Replace[Position[template,replaceObject],{{}:>{All},g_:>g[[1(*;;-2*)]]}]},
		HoldComplete[{xGridPos=Delete[gridPosition,1],yGridPos=Delete[gridPosition,-1]},
			HoldComplete[{gr=size[[1]]},
				(*Print@{gr,interiorGrid,gridPosition};*)
				(*0*)
				replaceCommand
				]~ReplacePart~(0->Block)
			]~ReplacePart~(0->With)
		]~ReplacePart~(0->With);
(*Defines replacements for scaled*)
			
(*If the x position is defined as scaled on a coordinate range,
return the x position of the columns up to that point,
and then the scaling times the coordinate range column sum*)
(*Inset[ob_,{GUIManagerScaled[parent_,w_,{jm_,jM_}],h_},o___]:>
	With[{xPos=
		geometryManagementInsetCommand[
			ob,
			Total@Table[Replace[
								lessThan[position,"x",gr],
									{i_Integer:>((*gr=gr[[position]];*)i),what_:>Print@what}],
							{position,gridPosition}
							]+w*lessThan[{jm,jM},"x",interiorGrid]
		]},
		Inset[ob,{xPos,h},o]
		],*)
(*If the x position is defined as scaled on a coordinate, return the column sum up to that point and then multiply by the scale on the last coordinate*)
Inset[ob_,{GUIManagerScaled[parent_,w_,j_],h_},o___]:>
	With[{xPos=
		geometryManagementInsetCommand[
			ob,
				w*lessThan[{Replace[j,{Except[{_,_}|All]:>{j,j+1}}]},"x",gr]
				+lessThan[xGridPos,"x",gr]
		]},
		Inset[ob,{xPos,h},o]
		],
(*If the y position is defined as scaled on a coordinate range,
return the y position of the columns up to that point, and then the scaling times the coordinate range row sum*)
(*Inset[ob_,{w_,GUIManagerScaled[inside_,h_,{im_,iM_}]},o___]:>
	With[{yPos=
		geometryManagementInsetCommand[
			ob,
			h*lessThan[{im,iM},"y",interiorGrid]+
			Total@Table[
					Replace[lessThan[position,"y",gr],
							{i_Integer:>(gr=gr[[position]];i),else_:>Print@else}],
					{position,gridPosition}]
		]},
		Inset[ob,{w,size[[2,2]]-yPos},o]
		],*)
(*If the y position is defined as scaled on a coordinate, return the row sum up to that point and then multiply by the scale on the last coordinate*)
Inset[ob_,{w_,GUIManagerScaled[inside_,h_,i_]},o___]:>
	With[{yPos=
		geometryManagementInsetCommand[
			ob,
			h*(lessThan[{Replace[i,{Except[{_,_}|All]:>{i,i+1}}]},"y",gr])
			+lessThan[yGridPos,"y",gr]
		]},
		Inset[ob,{w,size[[2,2]]-yPos},o]
		],
(*If the object width is defined as coordinate range,
return the scaling times the column sum in that range.
If it's a coordinate, return the width of that column.*)
Inset[ob_,o__,{GUIManagerScaled[inside_,w_,j_],h_},m___]:>
	With[{xWidth=
		geometryManagementInsetCommand[
			ob,
			w*lessThan[{Replace[j,{Except[{_,_}|All]:>{j,j+1}}]},"x",gr]
		]},
		Inset[ob,o,{xWidth,h},m]
		],
(*If the object height is defined as coordinate range,
return the scaling times the row sum in that range,
else if it's a coordinate, return the height of that row*)
Inset[ob_,o__,{w_,GUIManagerScaled[inside_,h_,i_]},m___]:>
	With[{yWidth=
		geometryManagementInsetCommand[
			ob,
			h*(lessThan[{Replace[i,{Except[{_,_}|All]:>{i,i+1}}]},"y",gr])
		]},
		Inset[ob,o,{w,yWidth},m]
		],
(*If a pure scaled appears, apply scale by total height or total width respectively*)
GUIManagerScaled[inside_,p_]:>GUIManagerScaled[inside,p,All]}
]&,
lessThan=With[{baseFunction=
	Function[{p,k,baseGrid},(*Make recursive*)
			
			With[{grid=Replace[baseGrid,{
							{}->ConstantArray[{0,0},Replace[p,{All->1,i_Integer:>i,{i1_Integer,i2_Integer}:>(i2-i1)}]],
							{ix_Integer,iy_Integer}:>{{{ix,iy}}},
							l:{{_Integer,_Integer}..}:>{l}
							}]},
			Switch[k,
				"x",Total@Replace[Array[
						Max@Cases[grid[[All,#]],{n1_Integer,_Integer}:>n1]&,Sequence@@
						Switch[p,
							_Integer,{p-1},
							All,{Length@grid[[1]]},
							_,{p[[2]]-p[[1]],p[[1]]}
						]],{{}->{0}(*,o_\[RuleDelayed](Print@{k,p,o};o)*)}],
				"y",Total@Replace[Array[
						Max@Cases[grid[[#]],{_Integer,n_Integer}:>n]&,Sequence@@
						Switch[p,
							_Integer,{p-1},
							All,{(Length@grid),1},
							_,{p[[2]]-p[[1]],p[[1]]}
						]],{{}->{0}(*,o_\[RuleDelayed](Print@{k,p,o};o)*)}]
				]~Replace~{_?(!NumericQ[#]&):>0,DirectedInfinity[___]:>0}]
				]},
	Function[{posList,key,readGrid},
		Block[{tabGrid=readGrid},
			Total@MapThread[(
				(*Print@{#1,key,tabGrid};*)
				baseFunction[#1,key,tabGrid=tabGrid[[ Replace[#2,Except[_Integer]:>All] ]]]
					)&,
					{posList,Prepend[Delete[Replace[posList,i_Integer:>{i}],-1],All]} ]] 
			] ]
	},

If[mode=!=Update,
	With[{T=Replace[cls::Template[widget,mode],i_Integer:>{{(*i*)},{(*Inset[i]*)}}],
		S=Normal@widget::Settings,
		M=With[{r=widget::Root},(*Append[*)Normal@r::Widgets(*,r::ID\[Rule]r]*)]},
		If[MatchQ[T[[1]],{___Integer}],
			(*if a template array is given*)
			With[{G=Graphics[T[[2]],FilterRules[S,Options@Graphics]],
				W=Table[GetAttribute[m,"Expression"],{m,Cases[T[[1]],_Integer,\[Infinity]]/.M}]},
				widget::Template={T};
				SynchronizedModule`TemplateCombine[G,W,Automatic,widget::ID]
				],
			(*if a template grid is given*)
			With[{classes=Replace[Cases[T[[1]],n_Integer:>n,\[Infinity]],c_:>Thread[c->(c/.M)]]},
				With[{W=Table[GetAttribute[m,"Expression"],{m,#[[2]]&/@classes}],size=cls::CalculateSize[widget,T[[1]],M]},
					(*Print@size;*)
					widget::Template={T,size[[1]]};
					(*CellPrint[{size,lessThan,T[[2]]}];*)
					With[{replace=replacements[lessThan,size,T[[1]]]},
						
						With[{insets=T[[2]]//.replace},
						(*Print@classes;*)
						Do[With[{inst=keyHW[[1]]/.classes},(*Print@inst;*)inst::Settings[ImageSize]=keyHW[[2]]],{keyHW,Cases[insets,Inset[key_,pos_,off_,{x_,y_},___]:>{key,{x,y}}]}];
						With[{G=Graphics[insets,
								ImageSize->(size[[2]]),
								FilterRules[S,Options@Graphics]]},
								SynchronizedModule`TemplateCombine[G,W,Automatic,widget::ID]
							]]]]
				]]
		],
(*Assuming the template is {{key grid,replacements}, hwGrid}, simply map the key grid to the hwGrid and apply this as a replacement list to the new generated template. 
		This attempts to prevent any unnecessary rasterization to determine object size*)
	With[{S=widget::Settings,
		T=widget::Template,
		T2=Replace[cls::Template[widget,Automatic],i_Integer:>{{(*i*)},{(*Inset[i]*)}}],
		M=With[{r=widget::Root},(*Append[Normal@*)r::Widgets(*,r::ID\[Rule]r]*)]},
		If[MatchQ[T2[[1]],{___Integer}],
			(*if a template array is given*)
			With[{G=Graphics[T2,FilterRules[S,Options@Graphics]],W=Table[GetAttribute[m,"Expression"],{m,Cases[T2[[1]],n_Integer:>n,\[Infinity]]/.M}]},
				SynchronizedModule`TemplateCombine[G,W,Automatic,widget::ID]
				],
			(*if a template grid is given*)
			With[{classes=Cases[T2[[1]],n_Integer:>n,\[Infinity]]/.M,reps=With[{P=Position[T[[1,1]],_Integer]},
				Thread[Table[T[[1,1,## ]]&@@p,{p,P}]->Table[T[[2,## ]]&@@p,{p,P}]]
				]//Dispatch},
				With[{W=Table[GetAttribute[m,"Expression"],{m,classes}],size=cls::CalculateSize[widget,cls::HeightWidthGrid[widget,T2[[1]]/.reps,M]]
				},
					With[{replace=replacements[lessThan,size,T2[[1]]]},
						With[{insets=T2[[2]]//.replace},
						Do[Evaluate[keyHW[[1]]/.classes]::Settings[ImageSize]=keyHW[[2]],{keyHW,Cases[insets,Inset[key_,pos_,off_,{x_,y_}]:>{key,{x,y}}]}];
						With[{G=Graphics[T2[[2]]//.replace,
								ImageSize->size[[2]],
								FilterRules[S,Options@Graphics]]},
								SynchronizedModule`TemplateCombine[G,W,Automatic,widget::ID]
							]]]]
				]
			]
		]
	]
];


(* ::Subsubsection::Closed:: *)
(*Class method: Template*)


(* ::Text:: *)
(*Generates a template grid to generate the graphical layout from.*)
(*Modes Pack, Grid, and Place are more or less the same*)


ClassMethod[Template,cls_,widget_,mode_:Automatic]:=
Switch[mode,
	"Pack",With[{L=widget::PackLayout,T=widget::Root::NameToWidget,c=cls::Template,S=widget::Settings},
		If[L=!=None,
			Module[{grid,replacements,R=Table[
				With[{gr=c[T[k]]},
					k->gr
					],{k,L//Keys}]},
				{grid,replacements}=cls::Pack[widget,widget::ID];
				grid=grid/.R;
				{grid,replacements}
				],
			widget::ID
			]
		],
	"Grid",With[{L=widget::GridLayout,T=widget::Root::NameToWidget,c=cls::Template},
			If[L=!=None,
				Module[{grid,replacements,R=Table[With[{gr=c[T[k]]},k->gr],{k,L//Keys}]},
					{grid,replacements}=cls::Grid[widget,widget::ID];
					grid=grid/.R;
					{grid,replacements}
					],
				widget::ID
				]
			],
	"Place",With[{L=widget::PlaceLayout,T=widget::Root::NameToWidget,c=cls::Template},
			If[L=!=None,
				With[{R=Table[k->c[T[k]],{k,L//Keys}]},
					cls::Place[widget,widget::ID]/.R
					],
				widget::ID
				]
			],
	_,With[{m=widget::GeometryMode},cls::Template[widget,m]]
	];


(* ::Subsubsection::Closed:: *)
(*Class method: HeightWidthGrid*)


(* ::Text:: *)
(*Takes a template grid and calculates item sizes based on padding and the widget base expressions*)
(**)
(*If a widget is not a container and has child widgets, its grid will be {widget grid, child grid}*)


ClassMethod[HeightWidthGrid,cls_,widget_,grid_,widgets_:None]:=
	Module[{H=0,W=0,P=widget::Settings[Padding],
		E=If[widgets===None,Normal@widget::Root::Widgets,widgets]},
		P=Replace[P,
			{p_Integer:>ConstantArray[p,4],
			{n_Integer,m_Integer}:>{n,n,m,m}
			}];
		E=Table[With[{inst=k/.E},
					inst::Settings[ImageSize]=Automatic;
					k->ImageDimensions@Rasterize[
						inst::Expression,
						ImageResolution->Automatic
					]],{k,Cases[Flatten[grid,1],_Integer]}];
		Table[Table[
			Switch[e,
				_Graphics,ImageDimensions@e,
				{_Integer,_Integer},e,
				_And,{(e[[1]]/.E)+{P[[1]]+P[[2]],P[[3]]+P[[4]]},cls::CalculateSize[widget,e[[2]],widgets]},
				_List,cls::CalculateSize[widget,e,widgets],
				Null,{0,0},
				_,(e/.E)+{P[[1]]+P[[2]],P[[3]]+P[[4]]}
				],{e,r}],{r,grid}](*~Replace~(o_\[RuleDelayed](Print@o;o))*)
		];


(* ::Subsubsection::Closed:: *)
(*Class method: CalculateSize*)


(* ::Text:: *)
(*Takes the height width grid and finds the overall graphics size required to accomodate all the elements*)
(**)
(*If a row has elements that aren't integer pairs, the max is calculated by applying CalculateSize to that grid*)


ClassMethod[CalculateSize,cls_,widget_,grid_,widgets_:None]:=
Module[{H=0,W=0,G=If[MatrixQ[widgets],widget,cls::HeightWidthGrid[widget,grid,widgets]]},
	(*Print@G;*)
	H=Total@Table[Max[
		Cases[r,{w_?NumberQ,h_?NumberQ}:>h]
			~Join~
		Cases[r,other:Except[{w_?NumberQ,h_?NumberQ}]:> cls::CalculateSize[widget,other,widgets][[2,2]]]
		],{r,G}];
	W=Total@Array[Max@
			With[{c=G[[All,#]]},
				Cases[c,{w_?NumberQ,h_?NumberQ}:>w]
					~Join~
				Cases[c,other:Except[{w_?NumberQ,h_?NumberQ}]:> cls::CalculateSize[widget,other,widgets][[2,1]]]
			]&,Length@G[[1]] ];
	(*Print@{W,H};*)
	{G,{W,H}}
	];


(* ::Subsubsection::Closed:: *)
(*Static method: Pack*)


(* ::Text:: *)
(*Takes the PackLayout of a widget and assigns grid spots based on item positions*)
(*The specifications can be Position: Left, Right, Top, Bottom, Center*)
(*and Filling: x, y, or both*)
(**)
(*By specifying what to pack inside, the manager function can later determine how coordinates should be determined*)


StaticMethod[Pack,widget_,inside_]:=
Module[{grid,replacements,i=1,j=1,T,M=widget::Children,S=widget::Settings,P=widget::PackLayout},
	If[P===None,Return[widget::ID]];
		T=Table[With[{O=W[[2]],K=W[[1]]},
				{Position,Filling,K}/.O~Join~{Position->"Center",Filling->"None"}
				],
				{W,Normal@P}];
		With[{B=Select[T,#[[1]]=="Bottom"&],R=Select[T,#[[1]]=="Right"&],
			C=Select[T,#[[1]]=="Center"&],L=Select[T,#[[1]]=="Left"&],
			A=Select[T,#[[1]]=="Top"&]},
			With[{LH=L~Join~C~Join~R//Length,LV=Length@(A~Join~B)+1},
				grid=ConstantArray[Null,{LV,LH}];
				replacements=Inset[#1,Sequence@@#2,With[{w=#1/.M},FilterRules[Normal@w::Settings,Options@Inset]]]&@@@
								Join@@{
									Table[
										grid[[j,Ceiling[LH/2]]]=g[[3]];
										{g[[3]],First@{Switch[g[[2]],
													"X"|"x",
														{{Scaled[.5],Scaled[.5,j]},Center,{Scaled[1],Automatic},Automatic},
													"Y"|"y",
														{{Scaled[.5],Scaled[.5,j]},Center,{Automatic,Scaled[1,j]},Automatic},
													"Both"|"XY"|"xy"|"both",
														{{Scaled[.5],Scaled[.5,j]},Center,{Scaled[1],Scaled[1,j]},Automatic},
													_,
														{{Scaled[.5],Scaled[.5,j]},Center,Automatic,Automatic}
													]},j++},{g,A}],
									Table[
										grid[[j,i]]=g[[3]];
										{g[[3]],First@{Switch[g[[2]],
													"X"|"x",
														{{Scaled[.5,i],Scaled[.5,j]},Center,{Scaled[1,i],Automatic},Automatic},
													"Y"|"y",
														{{Scaled[.5,i],Scaled[.5,j]},Center,{Automatic,Scaled[1,j]},Automatic},
													"Both"|"XY"|"xy"|"both",
														{{Scaled[.5,i],Scaled[.5,j]},Center,{Scaled[1,i],Scaled[1,j]},Automatic},
													_,
														{{Scaled[.5,i],Scaled[.5,j]},Center,Automatic,Automatic}
													]},i++},{g,L~Join~C~Join~R}],
								If[i>1,j++];Table[
									grid[[j,Ceiling[LH/2]]]=g[[3]];
									{g[[3]],First@{Switch[g[[2]],
													"X"|"x",
														{{Scaled[.5],Scaled[.5,j]},Center,{Scaled[1],Automatic},Automatic},
													"Y"|"y",
														{{Scaled[.5],Scaled[.5,j]},Center,{Automatic,Scaled[1,j]},Automatic},
													"Both"|"XY"|"xy"|"both",
														{{Scaled[.5],Scaled[.5,j]},Center,{Scaled[1],Scaled[1,j]},Automatic},
													_,
														{{Scaled[.5],Scaled[.5,j]},Center,Automatic,Automatic}
													]},j++},{g,B}]
								};
			]
	];
	replacements=replacements/.Scaled[a___]:>GUIManagerScaled[inside,a];
	(*If[widget::Container//TrueQ,
		{grid,replacements},*)
		{If[Not@IsInstance[widget,GUIRootClass],widget::ID&&grid,grid],replacements}(*]*)
	];


(* ::Subsubsection:: *)
(*Static method: Grid*)


(* ::Text:: *)
(*Takes the GridLayout of a widget and produces an appropriate grid and inset templates for eventual formatting by Template and ManageGeometry*)


StaticMethod[Grid,widget_,inside_]:=
Module[{grid,replacements,i=1,j=1,T={},M=widget::Children,P=widget::GridLayout},
	If[P===None,Return[widget::ID]];
		T=Table[With[{O=W[[2]],K=W[[1]]},
			{Row,Column,SpanFromLeft,SpanFromAbove,"Sticky",K}/.O~Join~{Row:>i++,Column:>j++,SpanFromLeft->1,SpanFromAbove->1,"Sticky"->""}
			],{W,Normal@P}];
	grid=ConstantArray[Null,{Max[T[[All,1]]],Max[T[[All,2]]]}];
	replacements=Inset[#1,Sequence@@#2,With[{w=#1/.M},FilterRules[Normal@w::Settings,Options@Inset]]]&@@@
	Table[
		With[{r=g[[1]],c=g[[2]],cs=g[[3]],rs=g[[4]],f=g[[5]],k=g[[6]]},
			{k,grid[[r,c]]=k;
			With[{C=Characters@ToLowerCase[ToString@f]},
				{{
				Which[
					MemberQ[C,"w"]&&MemberQ[C,"e"],Scaled[.5,{c,c+cs}],
					MemberQ[C,"w"],Scaled[0,c],
					MemberQ[C,"e"],Scaled[1,c+cs-1],
					True,Scaled[.5,{c,c+cs}]],
				Which[
					MemberQ[C,"n"]&&MemberQ[C,"s"],Scaled[.5,{r,r+rs}],
					MemberQ[C,"n"],Scaled[0,r],
					MemberQ[C,"s"],Scaled[1,r+rs-1],
					True,Scaled[.5,{r,r+rs}]
					]},
			{Which[
				MemberQ[C,"w"],Left,
				MemberQ[C,"e"],Right,
				True,Center],
			Which[
				MemberQ[C,"n"],Top,
				MemberQ[C,"s"],Bottom,
				True,Center]
			},
			{If[MemberQ[C,"w"]&&MemberQ[C,"e"],
				Scaled[1,{c,c+cs}],
				Automatic
				],
			If[MemberQ[C,"n"]&&MemberQ[C,"s"],
				Scaled[1,{r,r+rs}],
				Automatic
				]
			},
			Automatic}
			]}
			],
			{g,T}
			];
	replacements=replacements/.Scaled[a___]:>GUIManagerScaled[inside,a];
	(*If[widget::Container//TrueQ,
		{grid,replacements},
		{widget::ID&&grid,replacements}]*)
	{If[Not@IsInstance[widget,GUIRootClass],widget::ID&&grid,grid],replacements}
	];


(* ::Subsubsection:: *)
(*Static method: Place*)


(* ::Text:: *)
(*Takes the GridLayout of a widget and produces an appropriate grid and inset templates for eventual formatting by Template and ManageGeometry.*)


StaticMethod[Place,widget_,inside_]:=
Module[{grid,replacements,T,M=widget::Children,P=widget::PlaceLayout},
	If[P===None,Return[widget::ID]];
		T=Table[With[{O=W[[2]],K=W[[1]]},
			{"X","Y","RelativeX","RelativeY","Height","Width","RelativeHeight","RelativeWidth","Anchor",K}/.O~Join~{"X"->0,"Y"->0,"RelativeX"->0,"RelativeY"->0,"Height"->Automatic,"Width"->Automatic,"RelativeHeight"->0,"RelativeWidth"->0,"Anchor"->"NW"}
			],
			{W,Normal@P}];
		grid={};
		replacements=Inset[#1,Sequence@@#2,With[{W=#1/.M},FilterRules[Normal@W::Settings,Options@Inset]]]&@@@
			Table[
				With[{x=g[[1]],y=g[[2]],rx=g[[3]],ry=g[[4]],h=g[[5]],w=g[[6]],
					rh=g[[7]],rw=g[[8]],a=g[[9]],k=g[[10]]},
					{AppendTo[grid,k];k,
					{{x+Scaled[rx],y+Scaled[ry]},
					With[{c=Characters@a},
						{Which[MemberQ[c,"W"|"w"],Left,
								MemberQ[c,"E"|"e"],Right,
								_,Center
							],
						Which[MemberQ[c,"N"|"n"],Top,
								MemberQ[c,"S"|"s"],Bottom]
						}],
					{If[NumberQ@rw&&rw=!=0,
						If[w===Automatic,Scaled[rw],w+Scaled[rw]],
						w
						],
					If[NumberQ@rh&&rh=!=0,
						If[h===Automatic,Scaled[rh],h+Scaled[rh]],
						h
						]},
					Automatic
					}
					}],
				{g,T}];
		replacements=replacements/.Scaled[a___]:>GUIManagerScaled[inside,a];
(*If[widget::Container//TrueQ,
{grid,replacements},
{widget::ID&&grid,replacements}]*)
		{If[Not@IsInstance[widget,GUIRootClass],widget::ID&&grid,grid],replacements}
];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*Widget Class*)


(* ::Subsubsection::Closed:: *)
(*GUIWidgetClass*)


GUIWidgetClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends GUIBaseWidgetClass*)


ParentClasses={GUIBaseWidgetClass};


(* ::Subsubsection:: *)
(*Bound method: Init*)


(* ::Text:: *)
(*Initializes the Widget such that it has a parent widget and an expression which gets wrapped in a SynchronizedModule if it is not already in one*)
(*Its properties come from calling Init on GUIBaseWidgetClass*)


InitializationFunction[self_,parentWidget_,expr_:Pane[Null],ops:OptionsPattern[{___}]]:=
With[{m=(If[MatchQ[expr, _SynchronizedModule],
SynchronizedModule`Inject[expr,None,Null,If[MatchQ[Unevaluated[#],_SynchronizedModule`Hold],
#,
SynchronizedModule`Hold[#]]&],
SynchronizedModule[{},SynchronizedModule`Hold[expr]]])},
self::BaseExpression=m;
SuperclassDictionary[self,"Init"][self,parentWidget::Name<>"."<>ToString@m[[3]],m[[3]],ops];
self::Parent=parentWidget;
If[parentWidget::ChildList===None,parentWidget::ChildList={}];
AppendTo[parentWidget::ChildList,self::ID];
self::Layout=None;
self::GeometryMode="Pack";
self::Root=If[IsInstance[parentWidget,GUIRootClass],parentWidget,parentWidget::Root];
self::Root::Widgets[self::ID]=self;
];


(* ::Subsubsection::Closed:: *)
(*Bound property: Representation*)


(* ::Text:: *)
(*Generates a dynamically updating form of Expression. Not all that useful for real GUI work.*)


BoundProperty[Representation,self_]:=
With[{s=ToExpression[self::UpdateSymbol,StandardForm,Unevaluated]},
Dynamic[If[Evaluate@s//TrueQ,self::LastDisplayed=self::Expression,self::LastDisplayed],TrackedSymbols:>{s}]
];


(* ::Subsubsection::Closed:: *)
(*Bound property: Expression*)


(* ::Text:: *)
(*This is the real visual gui object seen. For container widgets, this will generally be null.*)


BoundProperty[Expression,self_]:=
With[{b=Normal@self::Bindings,e=self::BaseExpression/.{$Widget->self}},
If[Length@b>0,
	SynchronizedModule`Inject[e,None,Null,(ComplexEventHandler[Evaluate[#1/.SynchronizedModule`Hold[a_]:>a,b]]&)],
	e/.SynchronizedModule`Hold[a_]:>a
	]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: NameToWidget*)


(* ::Text:: *)
(*Takes a widget name/id key and gets the widget object from the root object's stored widgets*)


BoundMethod[NameToWidget,self_,name_]:=self::Root::NameToWidget[name];


(* ::Subsubsection::Closed:: *)
(*Bound method: SetOptions*)


(* ::Text:: *)
(*Sets widget options and then calls update on the root object which will, eventually, respond as a queued process rather than immediately, decreasing the danger of calling too often.*)


BoundMethod[SetOptions,self_,ops___]:=self::Settings=Merge[{Association[ops],self::Settings},First];


(* ::Subsubsection:: *)
(*Bound method: Pack*)


(* ::Text:: *)
(*Adds appropriate specs to the parent widget's PackLayout. Also calls GridForget and PlaceForget and if Update is true, calls the root widget's update function.*)


BoundMethod[Pack,self_,Position->"Center",Filling->"None",Update->True]:=
Block[{parentWidget,lastLayout=self::Layout,pa=$PassedArguments,keywordDict=KeyDrop[$KeyWordArguments,Update]},
	parentWidget=self::Parent;
	Replace[lastLayout,{
		{"Grid",___}:>self::GridForget[Update->False],
		{"Place",___}:>self::PlaceForget[Update->False],
		{"Pack",a_}:>(keywordDict=Merge[{pa,a,keywordDict},First])
			}];
	(*self::Visible="Pack";*)
	parentWidget::GeometryMode="Pack";
	self::Layout={"Pack",keywordDict};
	If[parentWidget::PackLayout===None,
		parentWidget::PackLayout=<|self::ID->Normal@keywordDict|>,
		parentWidget::PackLayout[self::ID]=Normal@keywordDict
	];
	If[OptionValue@Update,self::Root::Update[];]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: PackForget*)


(* ::Text:: *)
(*If the widget's visibility state is pack, removes the widget from the parent's PackLayout and sets widget state to invisible. If Update is True, calls the root widget's update function.*)


BoundMethod[PackForget,self_,Update->True]:=
With[{p=self::Parent},
If[self::Visible==="Pack",
With[{L=p::PackLayout},
p::PackLayout=KeyDrop[L,self::ID];
If[OptionValue@Update,self::Root::Update[]];
self::Visible=False;
]
];
];


(* ::Subsubsection:: *)
(*Bound method: Grid*)


(* ::Text:: *)
(*Adds appropriate specs to the parent widget's GridLayout. Also calls PackForget and PlaceForget and if Update is true, calls the root widget's update function.*)


BoundMethod[Grid,self_,Update->True,Row->1,Column->1,SpanFromLeft->1,SpanFromAbove->1,"Sticky"->""]:=
Block[{parentWidget,lastLayout=self::Layout,pa=$PassedArguments,keywordDict=KeyDrop[$KeyWordArguments,Update]},
	parentWidget=self::Parent;
	Replace[lastLayout,{
		{"Pack",___}:>self::PackForget[Update->False],
		{"Place",___}:>self::PlaceForget[Update->False],
		{"Grid",a_}:>(keywordDict=Merge[{pa,a,keywordDict},First])
			}];
	(*self::Visible==="Grid";*)
	parentWidget::GeometryMode="Grid";
	self::Layout={"Grid",keywordDict};
	If[parentWidget::GridLayout===None,
		parentWidget::GridLayout=<|self::ID->Normal@keywordDict|>,
		parentWidget::GridLayout[self::ID]=Normal@keywordDict
		];
	If[OptionValue@Update,self::Root::Update[];]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: GridForget*)


(* ::Text:: *)
(*PackForget analog for Grid.*)


BoundMethod[GridForget,self_,Update->True]:=
With[{p=self::Parent},
If[self::Visible==="Grid",
With[{L=p::GridLayout},
p::GridLayout=KeyDrop[L,self::ID];
If[OptionValue@Update,self::Root::Update[]];
self::Visible=False;
];
]
];


(* ::Subsubsection:: *)
(*Bound method: Place*)


(* ::Text:: *)
(*Adds appropriate specs to the parent widget's PlaceLayout. Also calls GridForget and PackForget and if Update is true, calls the root widget's update function.*)


BoundMethod[Place,self_,"X"->0,"Y"->0,"RelativeX"->0,"RelativeY"->0,"Width"->Automatic,"Height"->Automatic,"RelativeWidth"->0,"RelativeHeight"->0,"Anchor"->"NW",Update->True]:=
Block[{parentWidget,lastLayout=self::Layout,pa=$PassedArguments,keywordDict=KeyDrop[$KeyWordArguments,Update]},
	parentWidget=self::Parent;
	Replace[lastLayout,{
		{"Pack",___}:>self::PackForget[Update->False],
		{"Grid",___}:>self::GridForget[Update->False],
		{"Place",a_}:>(keywordDict=Merge[{pa,a,keywordDict},First])
			}];
	(*self::Visible==="Place";*)
	parentWidget::GeometryMode="Place";
	self::Layout={"Place",keywordDict};
	If[parentWidget::PlaceLayout===None,
		parentWidget::PlaceLayout=<|self::ID->Normal@keywordDict|>,
		parentWidget::PlaceLayout[self::ID]=Normal@keywordDict
		];
	If[OptionValue@Update,self::Root::Update[];]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: PlaceForget*)


(* ::Text:: *)
(*See GridForget*)


BoundMethod[PlaceForget,self_,Update->True]:=
With[{p=self::Parent},
If[self::Visible==="Place",
With[{L=p::PlaceLayout},
p::PlaceLayout=KeyDrop[L,self::ID];
If[OptionValue@Update,self::Root::Update[]];
self::Visible=False;
];
]
];


(* ::Subsubsection::Closed:: *)
(*Bound property: Children*)


(* ::Text:: *)
(*Returns all the child widget objects stored as keys in the widget's ChildList attribute*)


BoundProperty[Children,self_]:=With[{C=self::ChildList,W=self::Root::Widgets},Thread[C->(C/.W)]];


(* ::Subsubsection::Closed:: *)
(*End Class*)


GUIWidgetClass=EndClass[];


(* ::Subsection::Closed:: *)
(*GUI Constructor Functions*)


rootPattern=(_?(IsInstance[#,GUIRootClass]&)|None);


(* ::Subsubsection::Closed:: *)
(*GUI Root Constructor*)


(* ::Text:: *)
(*Makes a GUI root object*)


GUIRoot[name_:"root",ops:OptionsPattern[{___}]]:=GUIRootClass::New[name,ops]


(* ::Subsubsection:: *)
(*GUI Widget Constructor*)


(* ::Text:: *)
(*Makes a general GUIWidget object from the variables and expression given. If no root is provided, it makes one.*)


GUIWidget[rootWidget:rootPattern:None,{vars___},baseExpression_,ops:OptionsPattern[{___}]]:=
With[{root=If[rootWidget===None,GUIRoot[],rootWidget],
	sm=SynchronizedModule[{vars},{vars},Evaluate[Alternatives@@(((Not/.FilterRules[{ops},Not])/.Not->{}))//Not]]},
		(*Print@sm;*)
		With[{n=GUIWidgetClass::New[root,sm,FilterRules[{ops},Except[Not]]]},
			With[{expr=HoldComplete[baseExpression]/.{$Widget->n}},
				n::BaseExpression=SynchronizedModule`Inject[Evaluate[n::BaseExpression],None,expr,((#2//ReleaseHold)&)];
				n
				]
			]
	];
SetAttributes[GUIWidget,HoldAll]


(* ::Subsubsection:: *)
(*GUI Frame Constructor*)


(* ::Text:: *)
(*Makes a GUI frame for packing in other widgets*)


GUIFrame[rootWidget:rootPattern:None,ops:OptionsPattern[]]:=
With[{root=If[rootWidget===None,GUIRoot[]]},
	GUIWidget[root,Pane[Null]]
	]


(* ::Subsubsection:: *)
(*GUI Label Constructor*)


(* ::Text:: *)
(*Makes an updating label which uses the widget settings value Label to update*)


GUILabel[rootWidget:rootPattern:None,labelVariable:_Symbol:GUILabelVar,textvarORtext:Except[_Symbol]:"",ops:OptionsPattern[{___}]]:=
With[
{G=GUIWidget[rootWidget,{labelVariable=textvarORtext},
Dynamic@
	With[{S=$Widget::Settings},
		Replace[S[Label],l:Except[None|_Missing]:>(labelVariable=l;$Widget::Settings[Label]=None;)];
			With[{f=FilterRules[Normal@S,Options@Style]},
				Style[labelVariable,f]
				]
			],
			Not->{Replace[ToString@Unevaluated[labelVariable],s_?(!StringMatchQ[#,"GUILabelVar"]&)->Sequence[]]},
			ops
			]},
		G::Settings[Label]=textvarORtext;
		G
		];
SetAttributes[GUILabel,HoldAll]


(* ::Subsubsection:: *)
(*GUI Button Constructor*)


(* ::Text:: *)
(*Makes a GUI button that uses the label specified by Label and the function specified by ButtonFunction*)


GUIButton[rootWidget:rootPattern:None,buttonLabel_,buttonCmd_,ops:OptionsPattern[{___}]]:=
With[{G=GUIWidget[rootWidget,{GUILabelVar=buttonLabel},
	Dynamic@
		With[{S=$Widget::Settings},
			If[MemberQ[S//Keys,Label],GUILabelVar=S[Label],GUILabelVar=""];
				With[{f=FilterRules[Normal@S,Options@Style],b=FilterRules[Normal@S,Options@Button]},
					Button[Style[GUILabelVar,f],S[ButtonFunction],b]
					]
			],
			Not->{"GUILabelVar"},
			ops]},
			G::Settings[Label]=buttonLabel;
			With[{F=Hold[buttonCmd]/.$Widget->G},
				G::Settings[ButtonFunction]:=F//ReleaseHold
				];
			G
	];
SetAttributes[GUIButton,HoldAll]


(* ::Section:: *)
(*End Package*)


End[];


(*End[];*)
