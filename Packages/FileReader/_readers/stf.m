(* ::Package:: *)

Reader=FileReader[
	LinePattern[{s_String}:>(readerVars["Name"]=s;)],
	LinePattern[{i_Integer}:>(readerVars["atomNum"]=i;)],
	LinePattern[{aNum_Integer,0|dref_Integer,0|aref_Integer,0|href_Integer,0.|dist_Real,0.|ang_Real,0.|dihed_Real,m_Real}:>
		{aNum,{dref,dist,aref,ang,href,dihed},m},
		readerVars["atomNum"]]
	];
Extension="stf"
