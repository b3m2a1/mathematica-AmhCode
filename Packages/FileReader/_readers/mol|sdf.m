(* ::Package:: *)

Reader=FileReader[{atomCount,bondCount},
	LineBlock[{atNo_Integer,bondNo_Integer,__,_?(MatchQ[#,_String]&&StringMatchQ[#,"V"~~NumberString]&)}:>{atomCount=atNo,bondCount=bondNo}(*]*),
		LinePattern[{x_Real,y_Real,z_Real,elem_String,massDiff_Integer,charge_Integer,hydrogenCount_Integer,___}:>
			{elem,{x,y,z},"Mass Difference"->massDiff,"Charge"->charge,"H Count"->hydrogenCount},atomCount],
		LinePattern[{a_Integer,b_Integer,c_,0...}:>{a,b,c},bondCount],
		LinePattern[{"M","END"},1],
		{"M","END"},
		-1,
		"Separator"->"M_END"
		]
];
Extension="mol"|"sdf"
