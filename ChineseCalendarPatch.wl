(* ::Package:: *)

System`DateCalendarsDump`$chineseCelestialStems=Characters@"\:7532\:4e59\:4e19\:4e01\:620a\:5df1\:5e9a\:8f9b\:58ec\:7678"


System`DateCalendarsDump`$chineseTerrestrialBranches=Characters@"\:5b50\:4e11\:5bc5\:536f\:8fb0\:5df3\:5348\:672a\:7533\:9149\:620c\:4ea5"


System`DateCalendarsDump`$chineseQuarterNumber = Characters@"\:521d\:4e00\:4e8c\:4e09";


System`DateCalendarsDump`monthList["Chinese"] := {
	"\:6b63\:6708", "\:4e8c\:6708", "\:4e09\:6708", "\:56db\:6708", "\:4e94\:6708", "\:516d\:6708", "\:4e03\:6708", "\:516b\:6708",
	"\:4e5d\:6708", "\:5341\:6708", "\:51ac\:6708", "\:814a\:6708"
};


System`DateCalendarsDump`chineseYearName[DateCalendarsDump`year_] :=
	StringJoin[System`DateCalendarsDump`chineseSexagesimalName @ DateCalendarsDump`year, "\:5e74"]


System`DateCalendarsDump`makeChineseDateString[{DateCalendarsDump`cycle_, DateCalendarsDump`year_, DateCalendarsDump`month_, ___}] :=
	StringJoin[
   	System`DateCalendarsDump`makeChineseDateString @ {DateCalendarsDump`cycle, DateCalendarsDump`year},
   	" ",
   	If[System`DateCalendarsDump`leapVariantQ[DateCalendarsDump`month], "\:95f0", ""],
   	Part[System`DateCalendarsDump`monthList @ "Chinese", System`DateCalendarsDump`leapValue @ DateCalendarsDump`month]
	];


System`DateCalendarsDump`makeChineseDateString[{DateCalendarsDump`cycle_}] :=
	If[DateCalendarsDump`cycle>=0,
		StringJoin[
			"\:897f\:5143\:540e\:7b2c ",
			ToString@DateCalendarsDump`cycle,
			" \:8f6e"
		],
		StringJoin[
			"\:897f\:5143\:524d\:7b2c ",
			ToString@-DateCalendarsDump`cycle,
			" \:8f6e"
		]
	];


System`DateCalendarsDump`makeChineseDateString[System`DateCalendarsDump`date:{_, _, _, System`DateCalendarsDump`day_}] :=
	StringJoin[
		System`DateCalendarsDump`makeChineseDateString@Most@System`DateCalendarsDump`date,
		" ",
		StringJoin[
			System`DateCalendarsDump`makeChineseDayString@#1,
			" ",
			System`DateCalendarsDump`makeChineseTimeString@#2
		]&@@MixedFractionParts@System`DateCalendarsDump`day
	];


System`DateCalendarsDump`makeChineseTimeString[System`DateCalendarsDump`time_Real?NonNegative] :=
	StringJoin[
		System`DateCalendarsDump`$chineseTerrestrialBranches[[#1 + 1]],
		"\:65f6 ",
		If[#2 >= 0.5, "\:6b63", "\:521d"],
		System`DateCalendarsDump`$chineseQuarterNumber[[Mod[Floor[8#2], 4] + 1]],
		"\:523b"
	]&@@MixedFractionParts@Mod[12System`DateCalendarsDump`time + 0.5, 12]


System`DateCalendarsDump`makeChineseTimeString[System`DateCalendarsDump`time_] := $Failed


System`DateCalendarsDump`makeChineseDayString[System`DateCalendarsDump`day_Integer?NonNegative] :=
	Which[
		System`DateCalendarsDump`day <= 9,
			"\:521d"<>IntegerName[System`DateCalendarsDump`day + 1, "Chinese"],
		System`DateCalendarsDump`day <= 18,
			IntegerName[System`DateCalendarsDump`day + 1, "Chinese"],
		System`DateCalendarsDump`day <= 28,
			"\:5eff"<>IntegerName[System`DateCalendarsDump`day - 19, "Chinese"],
		System`DateCalendarsDump`day == 29,
			"\:4e09\:5341",
		True,
			$Failed
	]
