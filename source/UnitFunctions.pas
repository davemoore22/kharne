{ UnitFunctions

  Copyright (c) 2007-2009 Dave Moore 

  Miscellaneous Game Functions

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is all code apart where indicated.

  The Initial Developer of the Original Code is Dave Moore (starbog@gmail.com)

  Portions created by Dave Moore are Copyright (C) Dave Moore. All Rights Reserved.

  Contributor(s):

}


unit UnitFunctions;

interface

uses Windows, SysUtils, Classes, Controls, Graphics, Forms, Contnrs, Types,
  Math, Gradlabl, StdCtrls, ComCtrls, DateUtils, Messages, HotLog, 
  UnitDefines, UnitConst, UnitMonster, UnitItem, UnitCreature;

{ Convert a Real Number to a String }
function RealToStr(Value: Real): String;

{ Generate a random name }
function GenerateName: String;

{ Generate a random background }
function GenerateBackground: String;

{ Convert a skill number to a rating }
function ConvertNumberToRating(Number: Integer): String;

{ Convert subrace enum to string }
function SubRaceToString(SubRace: crSubRace): String;

{ Convert gender enum to string }
function GenderToString(Gender: crGender): String;

{ Convert class enum to string }
function ClassToString(CClass: crClass): String;

{ Load an image list with images from a bitmap }
function LoadImageList(ImageList: TImageList; BitmapPath: String; 
  ImageIndex: Integer; Transparent: Boolean): Boolean;
  
{ Return first unused position in an ObjectList }
function ReturnUnusedPosition(ObjectList: TObjectList; 
  MaxPosition: Integer): Integer;
  
{ Shuffle the contents of an array }
procedure ShuffleArray(var ArrayToShuffle: Array of String);

{ Shuffle the name of a string }
procedure ShuffleName(var Shuffle: String);

{ Make a name out of random words }
function MakeRandomName: String;

{ Fill an array with random characters }
procedure FillArrayWithRandom(var ArrayToFill: Array of String);

{ Scale down an int to a byte }
function IntToByte(IntToConvert:Integer):Byte;

{ Check if two points are adjecent to one another }
function IsAdjacent(Point1: TPoint; Point2: TPoint): Boolean;

{ Find the offset between two points, i.e. the difference in x and y 
  coordinates }
function FindOffsets(Point1: TPoint; Point2: TPoint): TPoint;

{ Return a random colour }
function GetRandomColor: TColor;

{ Scale a colour }
function GetRelatedColor(Color: TColor; Range: Integer): TColor;

{ Get the Level Offset for a particular Dungeon Branch, i.e. the starting
  difficulty level of the top level fo the Dungeon Branch }
function GetLeveLOffset(LevelTheme: Integer): Integer;

{ Return the Dungeon Branch as a String }
function GetEcology(LevelTheme: Integer): String;

{ Return the Euclidean distance between two points }
function Dist(Point1: TPoint; Point2: TPoint): Integer;

{ Return the four corners of the viewport }
function GetTopLeft(CurrentDrawMode: Integer; 
  CurrentWindowSize: TPoint): TPoint;
function GetBottomLeft(CurrentDrawMode: Integer; 
  CurrentWindowSize: TPoint): TPoint;
function GetTopRight(CurrentDrawMode: Integer; 
  CurrentWindowSize: TPoint): TPoint;
function GetBottomRight(CurrentDrawMode: Integer; 
  CurrentWindowSize: TPoint): TPoint;

{ Get the physical x/y of the centre of the drawing surface }
function GetPlayerScreenLoc(CurrentDrawMode: Integer; 
  CurrentWindowSize: TPoint): TPoint;

{ Check if a set of coordinates are inside the valid area inside the dungeon }
function InBounds(Point: TPoint): Boolean; 

{ Get the position to display a popup window }
function GetWindowPosX: Integer;
function GetWindowPosY: Integer;

{ Get the Monster Category as a String }
function GetMonsterCategory(MonsterType: Integer): String;

{ Convert the Player Race into a String }
function GetRace(Subrace: crSubrace): String;

{ Convert the Player Class into a string}
function GetClass(CClass: crClass): String;

{ Check if a point is on screen }
function IsViewBoundary(PointToCheck: TPoint): Boolean;

{ Set the colour of a skill label depending on the value - negative values
  go progressively redder, positive values go progressively light greener }
procedure SetSkillFontColor(SkillValue: Integer; 
  ComponentToColour: TGradLabel); overload;
procedure SetSkillFontColor(SkillValue: Integer; SkillValueMax: Integer; 
  ComponentToColour: TLabel); overload;
  
{ Add coloured text to a rich edit }
procedure AddText(szText: String; clColor: TColor; RichEdit: TRichEdit);

{ Character dump }
function CharDump(Dead: Boolean): String;

{ Routines used in populating the character dump }
procedure AddWornItemToFile(var MorgueFile: TStringList; ItemSlot: Integer; 
  SlotDescription: String);
procedure AddInventoryItems(var MorgueFile: TStringList; Dead: Boolean);
procedure DumpDisplay(PlayerX: Integer; PlayerY: Integer; 
  var MorgueFile: TStringList);
procedure GetDelves(var MorgueFile: TStringList);

{ Convert a TPoint to a string }
function PointToStr(Point: TPoint): String;

{ Check if we can display a level feeling }
function CanFeelLevel: Boolean;

{ Get the Elapsed Game Time }
function GetElapsedGameTime: String;

{ Animate a character }
procedure AnimateMissileWeapon(Point1: TPoint; Point2: TPoint;
  Character: Char; Colour: TColor; ProjectileType: tProjectile = prMissile);

{ Get a random description of how tasty food is when easting }
function GetFoodDescription: String;

{ Monster can drop an item }
function MonsterDropItem(Monster: TMonster): Boolean;

{ Returns true if the first character of a string is a vowel } 
function Vowel(StringToCheck: String): Boolean;

{ Convert the first character of a strong to lowercase }
function Lower(StringToChange: String): String;

{ Random function - equivalent of if (Rand(Chance) = 0) }
function OneChanceIn(Chance: Integer): Boolean;

{ Log a message to the Wizard Screen }
procedure WizardLog(StringToDisplay: String);

{ Functions to return random monster types }
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
  MonsterEcology: String = ''; MonsterOccurrence: String = ''; 
  LooseMatching: Boolean = False): TMonsterArchetype; overload;
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
  MonsterOccurrence: String = ''; LooseMatching: 
  Boolean = False): TMonsterArchetype; overload;
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
  LooseMatching:Boolean = False): TMonsterArchetype; overload;
function GetRandomMonsterForEcology(Ecology: String; 
  MonsterOccurrence: String): TMonsterArchetype;
function GetRandomUniqueMonster(MonsterLevel: Integer; 
  var MonsterFound: Boolean): TMonsterArchetype;
  
{ Given an ID, return the associated Monster type }  
function GetMonsterArchetype(ID: Integer): TMonsterArchetype;

{ Internal Sorting Routine for Item Lists }
function CompareMagnitudes(Item1, Item2: Pointer): Integer;

{ Item Archetype functions }
function GetRandomItemArchetype: TItemArchetype;
function GetSpecificItemArchetype(ObjectList: TObjectList; 
  ArchetypeName: String): TItemArchetype;
function GetRandomTypeItemArchetype
  (ArchetypeType: crItemType): TItemArchetype;

{ Get a specific enchantment }
function GetSpecificEnchantment(EnchantmentName: String): TItemEnchantment;

{ Generate Item Descriptors }
function GenerateKnownDescriptor(ItemLevel: Integer; 
  ItemType: crItemType): String;
function GenerateUnknownDescriptor(ItemQuality: crItemQuality): String;

{ Return a random item list type }
function ReturnRandomItemList: TObjectList;

{ Return the material enum from a string }
function GetMaterial(Material: String): crMaterial;

{ Return the item slot enum from a string }
function GetSlot(ItemSlot: String): crItemSlot;

{ Return a colour from a string }
function GetColour(ItemColour: String): TColor;

{ Dice-rolling routine for character creation }
function RollStartingDice(RacialModifier: Integer; var StatModifier: Integer;
  MinimumValue: Integer): Integer;

{ Create a character }
function CreateCharacter(var Character: TCreature): String;

implementation

uses UnitEngine, UnitDisplay, UnitVars, UnitDungeon, UnitHiScores, UnitWizard,
  UnitGame;

{ Scale down an int to a byte }
function IntToByte(IntToConvert:Integer):Byte;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.IntToByte()');

  { Default result }
  Result := 0;

  try
    { Check for boundaries }
    if IntToConvert > 255 then
      Result:= 255
    else if IntToConvert < 0 then 
      Result:= 0 
    else 
      Result:= IntToConvert;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end; 

{ Fill an array with random characters }
procedure FillArrayWithRandom(var ArrayToFill: Array of String);
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.FillArrayWithRandom()');

  try
  	{ Fill Array with names }
		for Loop := Low(ArrayToFill) to High(ArrayToFill) do
		  ArrayToFill[Loop] := MakeRandomName;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Make a name out of random words }
function MakeRandomName: String;
var
  CurrentWord: String;
  Loop: Integer;
  NumberOfWords: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.MakeRandomName()');

  { Default result }
  Result := '';
  
  try
  	{ Reinitialise the RND }
  	Randomize;
  	
  	{ Select the number of words to use, which can be 1, 2 or 3 }
  	NumberOfWords := Random(2) + 1;

	  { Generate each word }
  	for Loop := 1 to NumberOfWords do
  	begin
  		{ Get the Word }
    	CurrentWord := UpperCase(GenerateName);
    	
    	{ Shuffle it }
    	ShuffleName(CurrentWord);
    	
    	{ Add it to the result }
    	Result := Format('%s%s ', [Result, CurrentWord]);
  	end;
  
  	{ Get rid of any extraneous spaces }
  	Result := Trim(Result);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


{ Shuffle the name of a string }
procedure ShuffleName(var Shuffle: String);
var
  n1, n2, n3: integer;
  s1: Char;
  nIntensity: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ShuffleName()');

  try
  	{ Reinitialise the RND }
  	Randomize;
  	
  	{ Perform the Shuffle }
  	nIntensity := Length(Shuffle);  	
  	for n1 := 1 to nIntensity do
  	begin
			n2 := Random(nIntensity) + 1;
			n3 := Random(nIntensity) + 1;
			s1 := Shuffle[n3];
			Shuffle[n3] := Shuffle[n2];
			Shuffle[n2] := s1;
  	end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;
  
{ Shuffle the contents of an array }
procedure ShuffleArray(var ArrayToShuffle: Array of String);
var
  n1, n2, n3: integer;
  s1: string;
  nIntensity: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ShuffleArray()');
  
  try
  	{ Reinitialise the RND }
  	Randomize;
  	
  	{ Perform the Shuffle }
  	nIntensity := High(ArrayToShuffle); 
		for n1 := 0 to nIntensity do
		begin
			n2 := Random(nIntensity + 1);
			n3 := Random(nIntensity + 1);
			s1 := ArrayToShuffle[n3];
			ArrayToShuffle[n3] := ArrayToShuffle[n2];
			ArrayToShuffle[n2] := s1;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return first unused position in an ObjectList }
function ReturnUnusedPosition(ObjectList: TObjectList; 
  MaxPosition: Integer): Integer;
var
  Loop: Integer;
  Position: Integer;
  PositionFound: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ReturnUnusedPosition()');

  { Default result }
  Result := 0;
  
  try
		{ Reinitialise the RND }
		Randomize;

    { Try and find the first free positon in the list }
		repeat
			PositionFound := False;
			Position := Random(MaxPosition);
			for Loop := 0 to ObjectList.Count - 1 do
				if (ObjectList.Items[Loop] as TItemArchetype).Position = Position then 
				  PositionFound := True;
		until PositionFound = False;
		
		{ Return the position found }
		Result := Position;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;    
end;

{ Convert a Real Number to a String }
function RealToStr(Value: Real): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.RealToStr()');

  { Default result }
  Result := '';
  
  try
  	{ Format the result appropriately }
  	if Value <> 0 then
      Result := FormatFloat('#.#', Value)
  	else
      Result := '0';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate a random name }
function GenerateName: String;
var
  Sy1: TStringList;
  Sy2: TStringList;
  Sy3: TStringList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GenerateName()');

  { Default result }
  Result := '';
  
  try
  	{ Build up the syllables used to generate the name } 
  	
  	{ First syllable }
  	Sy1 := TStringList.Create;
  	Sy1.DelimitedText := '"Ab","Ac","Ad","Af","Agr","Ast","As","Al","Adw",' +
  	  '"Adr","Ar","B","Br","C","Cr","Ch","Cad","D","Dr","Dw","Ed","Eth",' +
  	  '"Et","Er","El","Eow","F","Fr","G","Gr","Gw","Gal","Gl","H","Ha",'+
  	  '"Ib","Jer","K","Ka","Ked","L","Loth","Lar","Leg","M","Mir","N",' +
  	  '"Nyd","Ol","Oc","On","P","Pr","R","Rh","S","Sev","T","Tr","Th","V",'+
  	  '"Y", "Z", "W", "Wic"';
  	  
  	 { Second syllable }
  	Sy2 := TStringList.Create;
  	Sy2.DelimitedText := '"a","ae","au","ao","are","ale","ali","ay","ardo",' +
  	  '"e","ei","ea","eri","era","ela","eli","enda","erra","i","ia","ie",' +
  	  '"ire","ira","ila","ili","ira","igo","o","oa","oi","oe","ore","u","y"';

  	{ Third syllable }  
		Sy3 := TStringList.Create;
		Sy3.DelimitedText := '"a","and","b","bwyn","baen","bard","c","ctred",' +
			'"cred","ch","can","d","dan","don","der","dric","dfrid","dus","f","g",' +
			'"gord","gan","l","li","lgrin","lin","lith","lath","loth","ld","ldric",' +
			'"ldan","m","mas","mos","mar","mond","n","nydd","nidd","nnon","nwan",' +
			'"nyth","nad","nn","nnor","nd","p","r","ron","rd","s","sh","seth",' +
			'"sean","t","th","tha","tlan","trem","tram","v","vudd","w","wan","win",' +
      '"wyn","wyr","wyr","wyth"';
			
		{ Reinitialise the RND }
		Randomize;
  	
  	{ Generate the name }
  	Result := Format('%s%s%s', [Sy1[Random(Sy1.Count - 1)], 
  		Sy2[Random(Sy2.Count - 1)], Sy3[Random(Sy3.Count - 1)]]); 
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate a random background }
function GenerateBackground: String;
var
  ChildStringList: TStringList;
  ClassStringList: TStringList;
  ParentStringList: TStringList;
  CreditStringList: TStringList;
  BackgroundStringList: TStringList;
  EyeTypeStringList: TStringList;
  EyeColourStringList: TStringList;
  HairStyleStringList: TStringList;
  HairColourStringList: TStringList;
  ComplexionStringList: TStringList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GenerateBackground()');

  { Default result }
  Result := '';
  
  try
  	{ Allocate space for the consitutent parts of the name }
		ChildStringList := TStringList.Create;
		ClassStringList := TStringList.Create;
		ParentStringList := TStringList.Create;
		BackgroundStringList := TStringList.Create;
		CreditStringList := TStringList.Create;
		EyeTypeStringList := TStringList.Create;
		EyeColourStringList := TStringList.Create;
		HairStyleStringList := TStringList.Create;
		HairColourStringList := TStringList.Create;
		ComplexionStringList := TStringList.Create;
		
		try
			{ Populate the various parts }
			ChildStringList.DelimitedText := '"an only child","one of two children",' +
				'"one of many children","the only surviving child"';
			ClassStringList.DelimitedText := '"lower-class", "middle-class",' +
				'"upper-class"';
			ParentStringList.DelimitedText := '"mercenary","merchant","businessman",' +
				'"craftsman","soldier","templar","priest"';
			BackgroundStringList.DelimitedText := '"contented","peaceful",' +
				'"troubled","settled","disturbed"';
			CreditStringList.DelimitedText := '"a credit to","a disgrace to",' +
				'"the black sheep of"';
			EyeTypeStringList.DelimitedText := '"dull","unusually piercing",' +
				'"piercing","striking"';
			EyeColourStringList.DelimitedText := '"grey","violet","green","blue",' +
				'"brown"';
			HairStyleStringList.DelimitedText := '"wavy","curly","straight","short",' +
				'"long"';
			HairColourStringList.DelimitedText := '"auburn","blonde","black","dark",' +
				'"ginger","grey"';
			ComplexionStringList.DelimitedText := '"an average","a sallow","a fair",' +
				'"a dark","a light"';

			{ Reinitialise the RND }
			Randomize;

			{ Build up the result }
			Result := Format('You are %s of a %s %s. You had a %s upbringing and you ' +
				'are %s the family. You have %s %s eyes, %s %s hair, and %s complexion.',
				[ChildStringList[Random(ChildStringList.Count - 1)],
				ClassStringList[Random(ClassStringList.Count - 1)],
				ParentStringList[Random(ParentStringList.Count - 1)], 
				BackgroundStringList[Random(BackgroundStringList.Count - 1)],			
				CreditStringList[Random(CreditStringList.Count - 1)],
				EyeTypeStringList[Random(EyeTypeStringList.Count - 1)],
				EyeColourStringList[Random(EyeColourStringList.Count - 1)],
				HairStyleStringList[Random(HairStyleStringList.Count - 1)],
				HairColourStringList[Random(HairColourStringList.Count - 1)],
				ComplexionStringList[Random(ComplexionStringList.Count - 1)]]);
			
		finally
			{ In all cases, free any memory used }		
			ChildStringList.Free;
			ClassStringList.Free;
			ParentStringList.Free;
			BackgroundStringList.Free;
			CreditStringList.Free;
			EyeTypeStringList.Free;
			EyeColourStringList.Free;
			HairStyleStringList.Free;
			HairColourStringList.Free;
			ComplexionStringList.Free;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert a skill number to a rating }
function ConvertNumberToRating(Number: Integer): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ConvertNumberToRating()');

  { Default result }
  Result := '';
  
  try
  	if Number < 5 then 
  		Result := 'Atrocious'
		else if Number < 15 then
			Result := 'Abysmal'
		else if Number < 25 then
			Result := 'Bad'
		else if Number < 40 then
			Result := 'Poor'
		else if Number < 50 then 
			Result := 'Mediocre'
		else if Number < 65 then 
			Result := 'Fair'
		else if Number < 80 then 
			Result := 'Good'
		else if Number < 95 then 
			Result := 'Excellent'
		else 
			Result := 'Superb';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert subrace enum to string }
function SubRaceToString(SubRace: crSubRace): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.SubRaceToString()');

  { Default result }
  Result := '';
  
  try
  	{ Convert enum to string }
		case SubRace of
			srNone: Result := 'None';
			srHuman: Result := 'Human';
			srHalfing: Result := 'Halfing';
			srOrc: Result := 'Orc';
			srDwarf: Result := 'Dwarf';
			srElf: Result := 'Elf';
		else 
			Result := 'None';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert gender enum to string }
function GenderToString(Gender: crGender): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GenderToString()');

  { Default result }
  Result := '';
  
  try
    { Convert enum to string }
		case Gender of
			gMale: Result := 'Male';
			gFemale: Result := 'Female';
			gOther: Result := 'Other';
		else 
		  Result := 'None';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert class enum to string }
function ClassToString(CClass: crClass): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ClassToString()');

  { Default result }
  Result := '';
  
  try
  	{ Convert enum to string }
		case CClass of
			cThief: Result := 'Thief';
			cKnight: Result := 'Knight';
			cMage: Result := 'Mage';
			cPriest: Result := 'Priest';
			cWarrior: Result := 'Warrior';
		else 
			Result := 'None';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Given a bitmap, load it, crack it open into various images (or just the one 
  specified by the imageindex, and populate the specificed ImageList with 
  them/it }
function LoadImageList(ImageList: TImageList; BitmapPath: String; 
  ImageIndex: Integer; Transparent: Boolean): Boolean;
var
  Bitmap: TBitmap;
  LocalImageList: TImageList;
  LocalBitmap: TBitmap;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.LoadImageList()');

  { Default result }
  Result := False;
  
  try
  	{ Check if the bitmap file specified exists }
		if (FileExists(ExtractFilePath(Application.ExeName) + '\gfx\' + 
			BitmapPath)) then
		begin
			{ Create a temporary bitmap }
			Bitmap := TBitmap.Create;
			
			{ Create a placeholder bitmap for use during loading of individual 
			  images }
			LocalBitmap := TBitmap.Create;
			
			{ Create an imagelist and parse the temporary bitmap into it }
			LocalImageList := TImageList.CreateSize(32, 32);
			
			try
				{ Load the bitmap file specified into the temporary bitmap }
				Bitmap.LoadFromFile(ExtractFilePath(Application.ExeName) + '\gfx\' + 
					BitmapPath);

				{ Set up the Image List }
				LocalImageList.BkColor := clNone;
				LocalImageList.Masked := Transparent;
				LocalImageList.Add(Bitmap, nil);

				{ Reinitialise the imagelist }
				ImageList.Clear;

				{ Add the associated bitmap }
				if ImageIndex <> -1 then
				begin
					{ Get the bitmap to load }
					LocalImageList.GetBitmap(ImageIndex, LocalBitmap);

					{ Handle transparent images }
					if Transparent = True then 
						ImageList.AddMasked(LocalBitmap, clDefault)
					else 
						ImageList.Add(LocalBitmap, nil);
				end
				else
				begin
					{ Load each bitmap into the imagelist }
					for Loop := 0 to LocalImageList.Count - 1 do
					begin
						{ Get the bitmap to load }
						LocalImageList.GetBitmap(Loop, LocalBitmap);

						{ Handle transparent images }
						if Transparent = True then 
							ImageList.AddMasked(LocalBitmap, clDefault)
						else 
							ImageList.Add(LocalBitmap, nil);
					end;
				end;

				{ If we've reached this far, then we've succeeded }
				Result := True;
			
			finally
				{ Free any bitmaps created }
				LocalBitmap.Free;
				LocalImageList.Free;
				Bitmap.Free;
			end;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if two points are adjecent to one another }
function IsAdjacent(Point1: TPoint; Point2: TPoint): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.IsAdjacent()');

  { Default result }
  Result := False;
  
  try
  	{ If the distance in squares between the two points specified is greater
  	  then one, then the two points are not adjacent }
  	Result := not(Dist(Point1, Point2) > 1);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Find the offset between two points, i.e. the difference in x and y 
  coordinates }
function FindOffsets(Point1: TPoint; Point2: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.FindOffsets()');

  { Default result }
  Result := Point(0,0);
  
  try
  	{ Get the difference for the X Coordinate }
  	Result.X := Point1.X - Point2.X;
  	
  	{ Limit the answer to -1/0/1 }
  	if Result.X < -1 then 
  		Result.X := -1
  	else if Result.X > 1 then 
  	  Result.X := 1;
  	  
  	{ Get the difference for the Y Coordinate } 
  	Result.Y := Point1.Y - Point2.Y;
  	
  	{ Limit the answer to -1/0/1 }
  	if Result.Y < -1 then 
  		Result.Y := -1
  	else if Result.Y > 1 then 
  		Result.Y := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return a random colour }
function GetRandomColor: TColor;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomColor()');

  { Default result }
  Result := clWhite;
  
  try
  	{ Get a random colour }
  	Result := RGB(Random(256), Random(256), Random(256));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Scale a colour }
function GetRelatedColor(Color: TColor; Range: Integer): TColor;
var
  R, G, B: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.GetRelatedColor()');

  { Default result }
  Result := Color;
  
  try
  	{ Get random offsets for Red, Green and Blue parts of the colour }
		R := (Color and $ff) + Random(Range * 2) - Range;
		G := ((Color and $ff00) shr 8) + Random(Range * 2) - Range;
		B := ((Color and $ff0000) shr 16) + Random(Range * 2) - Range;
		
		{ Ensure the results are sensible }
		if R > 255 then 
		  R := 255
		else if R < 0 then 
		  R := 0;
		if G > 255 then 
		  G := 255
		else if G < 0 then 
		  G := 0;
		if B > 255 then 
		  B := 255
		else if B < 0 then 
		  B := 0;
		  
		{ Format the result }
		Result := RGB(R, G, B);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Level Offset for a particular Dungeon Branch, i.e. the starting
  difficulty level of the top level fo the Dungeon Branch }
function GetLeveLOffset(LevelTheme: Integer): Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetLeveLOffset()');

  { Default result }
  Result := 0;
  
  try
  	{ Get the offsets }
		case LevelTheme of
			D_ABYSS: Result := 15;
			D_EARTH: Result := 10;
			D_AIR: Result := 10;
			D_FIRE: Result := 10;
			D_WATER: Result := 10;
			D_KEEP: Result := 5;
			D_CRYPT: Result := 5;
		else 
		  Result := 0;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the Dungeon Branch as a String }
function GetEcology(LevelTheme: Integer): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetEcology()');

  { Default result }
  Result := '';
  
  try
  	{ Get the Theme }
		case LevelTheme of
			D_ABYSS: Result := 'Abyss';
			D_EARTH: Result := 'Earth';
			D_AIR: Result := 'Air';
			D_FIRE: Result := 'Fire';
			D_WATER: Result := 'Water';
			D_KEEP: Result := 'Keep';
			D_CRYPT: Result := 'Mausoleum';
			D_FORTRESS: Result := 'Fortress';
			D_WILDERNESS: Result := 'Wilderlands';
		else 
			Result := '';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the Euclidean distance between two points }
function Dist(Point1: TPoint; Point2: TPoint): Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.Dist()');

  { Default result }
  Result := 0;
  
  try
  	{ Get the distance between two points, using the standard cartesian metric
  	  for flat Euclidean space }
  	Result := Trunc(Sqrt((Power((Point2.X - Point1.X), 2)) + 
  		(Power((Point2.Y - Point1.Y), 2))));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Returns the X/Y coordinates of various corners of the current viewport }
function GetTopLeft(CurrentDrawMode: Integer; 
	CurrentWindowSize: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetTopLeft()');

  { Default result }
  Result := Point(0, 0);
  
  try
  	{ Get the coordinates of the top left corner }
  	Result.X := 0 - ((CurrentWindowSize.X div ScalingFactorX) div 2);
  	Result.Y := 0 + ((CurrentWindowSize.Y div ScalingFactorY) div 2) + 2;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Returns the X/Y coordinates of various corners of the current viewport }
function GetBottomLeft(CurrentDrawMode: Integer; 
	CurrentWindowSize: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetBottomLeft()');

  { Default result }
  Result := Point(0, 0);
  
  try  	
    { Get the coordinates of the bottom left corner }
  	Result.X := 0 - ((CurrentWindowSize.X div ScalingFactorX) div 2);
  	Result.Y := 0 - ((CurrentWindowSize.Y div ScalingFactorY) div 2);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Returns the X/Y coordinates of various corners of the current viewport }
function GetTopRight(CurrentDrawMode: Integer; 
	CurrentWindowSize: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetTopRight()');

  { Default result }
  Result := Point(0, 0);
  
  try
    { Get the coordinates of the top right corner }
  	Result.X := 0 + ((CurrentWindowSize.X div ScalingFactorX) div 2);
  	Result.Y := 0 + ((CurrentWindowSize.Y div ScalingFactorY) div 2) + 2;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Returns the X/Y coordinates of various corners of the current viewport }
function GetBottomRight(CurrentDrawMode: Integer; 
	CurrentWindowSize: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetBottomRight()');

  { Default result }
  Result := Point(0, 0);
  
  try
    { Get the coordinates of the bottom right corner }
  	Result.X := 0 + ((CurrentWindowSize.X div ScalingFactorX) div 2);
  	Result.Y := 0 - ((CurrentWindowSize.Y div ScalingFactorY) div 2);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the physical x/y of the centre of the drawing surface }
function GetPlayerScreenLoc(CurrentDrawMode: Integer; 
	CurrentWindowSize: TPoint): TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetPlayerScreenLoc()');

  { Default result }
  Result := Point(0, 0);
  
  try
  	{ Return the centre of the viewport, where the player is located }
  	Result.X := 
  		((CurrentWindowSize.X div ScalingFactorX) div 2) * ScalingFactorX;
  	Result.Y := 
  		CurrentWindowSize.Y - (((CurrentWindowSize.Y div ScalingFactorY) div 2) 
  		* ScalingFactorY);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if a set of coordinates are inside the valid area inside the dungeon }
function InBounds(Point: TPoint): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.InBounds()');

  { Default result }
  Result := False;
  
  try
  	{ Check the Point given is valid, i.e. is within the bounds of the various
  	  tile arrays for the TDungeonLevel class }
  	Result := (Point.X >= 1) and (Point.X <= DUNGEONSIZEX) and 
  		(Point.Y >= 1) and (Point.Y <= DUNGEONSIZEY);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the X position to display a popup window }
function GetWindowPosX: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetWindowPosX()');

  { Default result }
  Result := 2;
  
  try
  	{ Check for the edge of the screen }
		if FormDisplay.Width > Screen.Width - 30 then 
			Result := -32;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Y position to display a popup window }
function GetWindowPosY: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetWindowPosY()');

  { Default result }
  Result := 2;
  
  try  	
    { Check for the edge of the screen }
  	if FormDisplay.Height > Screen.Height - 30 then 
  	  Result := -32;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Monster Category as a String }
function GetMonsterCategory(MonsterType: Integer): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetMonsterCategory()');

  { Default result }
  Result := '';
  
  try
  	{ Convert enum to string }
		case MonsterType of
			1: Result := 'Animal';
			2: Result := 'Humanoid';
			3: Result := 'Construct';
			4: Result := 'Demon';
			5: Result := 'Dragon';
			6: Result := 'Elemental';
			7: Result := 'Plant';
			8: Result := 'Undead';
			9: Result := 'Giant';
			10: Result := 'Ooze';
			11: Result := 'Goblinoid';
			12: Result := 'Outsider';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert the Player Race into a String }
function GetRace(Subrace: crSubrace): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRace()');

  { Default result }
  Result := '';
  
  try
    { Convert enum to string }
  	case (Ord(Subrace)) of
    	0: Result := '';
    	1: Result := 'Human';
    	2: Result := 'Halfling';
    	3: Result := 'Orc';
    	4: Result := 'Dwarf';
    	5: Result := 'Elf';
  	end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert the Player Class into a string}
function GetClass(CClass: crClass): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetClass()');

  { Default result }
  Result := '';
  
  try
  	{ Convert enum to string }
		case (Ord(CClass)) of
			0: Result := '';
			1: Result := 'Thief';
			2: Result := 'Knight';
			3: Result := 'Mage';
			4: Result := 'Priest';
			5: Result := 'Warrior';
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if a point is on screen }
function IsViewBoundary(PointToCheck: TPoint): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.IntToByte()');

  { Default result }
  Result := False;
  
  try
  	{ Check if the point is inside the current viewport, i.e. on the screen }
  	if ((PointToCheck.X = 
  		FormDisplay.Game.PlayerX + FormDisplay.Game.TopLeft.X) or 
  		(PointToCheck.X = 
  		FormDisplay.Game.PlayerX + FormDisplay.Game.BottomRight.X)) or
     ((PointToCheck.Y = 
     	FormDisplay.Game.PlayerY + FormDisplay.Game.TopLeft.Y) 	or 
     	(PointToCheck.Y = FormDisplay.Game.PlayerY + 
     	FormDisplay.Game.BottomRight.Y)) then
  		Result := True;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set the colour of a skill label depending on the value - negative values
  go progressively redder, positive values go progressively light greener }
procedure SetSkillFontColor(SkillValue: Integer; ComponentToColour: TGradLabel);
var
  R, G, B: Integer;
  Temp: TColor;
  OffSet: Integer;
  ColourtoUse: TColor;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.SetSkillFontColor()');
  
  try
  	{ Find the Offset }
  	OffSet := Abs(SkillValue * 10);
  	if (Offset > 160) then 
    	Offset := 160;

		{ Deal with positive and negative skill values }
		if SkillValue < 0 then
		begin
			Temp := $008080FF;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;
			Dec(G, Offset);
			Dec(B, Offset);
		end
		else if SkillValue > 0 then
		begin
			Temp := $0080FF80;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;
			Dec(R, Offset);
			Dec(B, Offset);
		end
		else
		begin
			Temp := clWhite;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;  
		end;
		
		{ Reconstitute the adjusted colour }
  	ColourToUse := RGB(R, G, B);
  	
  	{ Set the Component to the new colour }
  	ComponentToColour.StartColor := ColourToUse;
  	ComponentToColour.EndColor := ColourToUse;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set the colour of a skill label depending on the value - negative values
  go progressively redder, positive values go progressively light greener }
procedure SetSkillFontColor(SkillValue: Integer; SkillValueMax: Integer; 
	ComponentToColour: TLabel);
var
  R, G, B: Integer;
  Temp: TColor;
  OffSet: Integer;
  Proportion: Double;
  ColourtoUse: TColor;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.SetSkillFontColor()');
 
  try
  	{ Find the Proportion }
  	Proportion := (SkillValueMax - SkillValue) / SkillValueMax; 
  	
  	{ Find the Offset }
  	OffSet := Trunc(Proportion * 100);
  	if (Offset > 100) then 
  	  Offset := 100
  	else if (Offset < -100) then 
  	  Offset := -100;

		{ Deal with positive and negative offsets }
		if Offset < 0 then
		begin
			Temp := $008080FF;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;
			Dec(G, Offset);
			Dec(B, Offset);
		end
		else if Offset > 0 then
		begin
			Temp := $0080FF80;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;
			Dec(R, Offset);
			Dec(B, Offset);
		end
		else
		begin
			Temp := clWhite;
			R := (Temp and $ff);
			G := (Temp and $ff00) shr 8;
			B := (Temp and $ff0000) shr 16;  
		end;
		
		{ Reconstitute the adjusted colour }
  	ColourToUse := RGB(R, G, B);
  	
  	{ Set the Component to the new colour }
  	ComponentToColour.Font.Color := ColourToUse;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Add coloured text to a rich edit }
procedure AddText(szText: String; clColor: TColor; RichEdit: TRichEdit);
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.AddText()');

  try
  	{ Set the selected text color }
  	RichEdit.SelAttributes.Color := clColor;
  	
  	{ Add the text }  	
  	RichEdit.SelText := szText;
  	
  	{ Set the colour for the next text added back to the default }
  	RichEdit.SelAttributes.Color := clWindowText;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Produce a character dump - the Dead parameter indicates if the player is
  dead, because in this circumstance it forces all carried items to be
  identified and changes the output text slightly }
function CharDump(Dead: Boolean): String;
var
  MorgueFile: TStringList;
  MorgueFileName: String;
  Loop: Integer;
  TotalKills: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.CharDump()');

  { Default result }
  Result := '';
  
  try
		{ Get the Character Dump Template file }
		MorgueFileName := ExtractFilePath(Application.ExeName) + 'info\morgue.dat';
		
		{ Create the local stringlist to write the dump to }
		MorgueFile := TStringList.Create;
		try
			{ Unavoidable 'with' here as all the information we are looking for is
			  stored in the FormDisplay.Game object }
			with FormDisplay.Game do
			begin
				{ Calculate the score }
				Score := CalculateScore(FormDisplay.Game.Player);
				
				{ Load the template file }
				MorgueFile.LoadFromFile(MorgueFileName);
				
				{ Start formatting the character dump }
				MorgueFile[0] := Format(MorgueFile[0], ['------------']);
				MorgueFile[1] := Format(MorgueFile[1], [VersionString]);
				MorgueFile[2] := Format(MorgueFile[2], ['------------']);
				
				{ Output Player Info }			
				MorgueFile[4] := Format(MorgueFile[4], [Player.Name, Player.Levels, 
					GetRace(Player.Subrace), Player.FriendlyClassName,
          GetClass(Player.CClass)]);
				MorgueFile[6] := Format(MorgueFile[6], [DateTimeToStr(GameStartTime)]);
				
				{ Handle dead/not dead }
				if Dead then
				begin
					MorgueFile[7] := Format(MorgueFile[7], [Turns, DateTimeToStr(Now), 
						GetElapsedGameTime]);
					MorgueFile[8] := Format(MorgueFile[8], [Killed, Dungeon.LevelDepth, 
						Dungeon.Name]);						
				end
				else
				begin
					MorgueFile[7] := Format('Has spent %d turns in the Multiverse ' +
						'(taking %s)', [Turns, GetElapsedGameTime]);
					MorgueFile[8] := Format(MorgueFile[8], ['Is currently', 
						Dungeon.LevelDepth, Dungeon.Name]);						
				end;

				{ Statistics }
				MorgueFile[16] := Format(MorgueFile[16], [Player.HP, Player.MaxHP]);
				MorgueFile[17] := Format(MorgueFile[17], [Player.MP, Player.MaxMP]);
				MorgueFile[18] := Format(MorgueFile[18], [Player.AC]);
				MorgueFile[19] := Format(MorgueFile[19], [Player.EV]);
				MorgueFile[20] := Format(MorgueFile[20], [Player.Speed]);

				{ Gold }
				MorgueFile[22] := Format(MorgueFile[22], [Player.Gold]);

				{ Experience }
				MorgueFile[24] := Format(MorgueFile[24], [Player.XP, Player.NextLevel]);

				{ Attributes }
				MorgueFile[30] := Format(MorgueFile[30], [Player.Strength, 
					Player.StrengthMod]);
				MorgueFile[31] := Format(MorgueFile[31], [Player.Agility, 
					Player.AgilityMod]);
				MorgueFile[32] := Format(MorgueFile[32], [Player.Endurance, 
					Player.EnduranceMod]);
				MorgueFile[33] := Format(MorgueFile[33], [Player.Intelligence, 
					Player.IntelligenceMod]);
				MorgueFile[34] := Format(MorgueFile[34], [Player.Resolve, 
					Player.ResolveMod]);
				MorgueFile[35] := Format(MorgueFile[35], [Player.Charisma, 
					Player.CharismaMod]);

				{ Combat Stats }
				MorgueFile[41] := Format(MorgueFile[41], [Player.Accuracy]);
				MorgueFile[42] := Format(MorgueFile[42], [Player.DamageBonus]);
				MorgueFile[43] := Format(MorgueFile[43], [Player.Blocking]);
				MorgueFile[44] := Format(MorgueFile[44], [Player.Deflection]);

				{ Mundance Skills }
				MorgueFile[50] := Format(MorgueFile[50], 
					[Player.Skills[SK_FIGHTING]]);
				MorgueFile[51] := Format(MorgueFile[51], [Player.Skills[SK_MELEE]]);
				MorgueFile[52] := Format(MorgueFile[52], [Player.Skills[SK_RANGED]]);
				MorgueFile[53] := Format(MorgueFile[53], [Player.Skills[SK_UNARMED]]);
				MorgueFile[55] := Format(MorgueFile[55], [Player.Skills[SK_DEFENSE]]);
				MorgueFile[56] := Format(MorgueFile[56], [Player.Skills[SK_HEAVY]]);
				MorgueFile[57] := Format(MorgueFile[57], [Player.Skills[SK_MEDIUM]]);
				MorgueFile[58] := Format(MorgueFile[58], [Player.Skills[SK_LIGHT]]);
				MorgueFile[60] := Format(MorgueFile[60], 
					[Player.Skills[SK_SUBTERFUGE]]);
				MorgueFile[61] := Format(MorgueFile[61], [Player.Skills[SK_STEALTH]]);
				MorgueFile[62] := Format(MorgueFile[62], 
					[Player.Skills[SK_THIEVERY]]);

				{ Magic Skills }
				MorgueFile[68] := Format(MorgueFile[68], [Player.Skills[SK_MAGIC]]);
				MorgueFile[69] := Format(MorgueFile[69], [Player.Skills[SK_FIRE]]);
				MorgueFile[70] := Format(MorgueFile[70], [Player.Skills[SK_WATER]]);
				MorgueFile[71] := Format(MorgueFile[71], [Player.Skills[SK_AIR]]);
				MorgueFile[72] := Format(MorgueFile[72], [Player.Skills[SK_EARTH]]);
				MorgueFile[73] := Format(MorgueFile[73], [Player.Skills[SK_NATURE]]);
				MorgueFile[74] := Format(MorgueFile[74], [Player.Skills[SK_HEALING]]);
				MorgueFile[75] := Format(MorgueFile[75], [Player.Skills[SK_CURSING]]);
				MorgueFile[76] := Format(MorgueFile[76], [Player.Skills[SK_COMBAT]]);
				MorgueFile[77] := Format(MorgueFile[77], 
					[Player.Skills[SK_PROTECTION]]);
				MorgueFile[78] := Format(MorgueFile[78], [Player.Skills[SK_LORE]]);

				{ Resistances }
				MorgueFile[84] := Format(MorgueFile[84], [Player.Resistance]);
				MorgueFile[85] := Format(MorgueFile[85], [Player.FireResistance]);
				MorgueFile[86] := Format(MorgueFile[86], [Player.EarthResistance]);
				MorgueFile[87] := Format(MorgueFile[87], [Player.AirResistance]);
				MorgueFile[88] := Format(MorgueFile[88], [Player.WaterResistance]);
				MorgueFile[89] := Format(MorgueFile[89], [Player.PoisonResistance]);
				MorgueFile[90] := Format(MorgueFile[90], 
					[Player.LifeDrainingResistance]);

				{ More formatting }
				MorgueFile.Add('');
				MorgueFile.Add('---------');
				MorgueFile.Add('Inventory');
				MorgueFile.Add('---------');
				MorgueFile.Add('');

				{ Add worn/wielded item details }
				AddWornItemToFile(MorgueFile, S_HEAD, 'Head: ');
				AddWornItemToFile(MorgueFile, S_NECK, 'Neck: ');
				AddWornItemToFile(MorgueFile, S_CHEST, 'Chest: ');
				AddWornItemToFile(MorgueFile, S_ARMS, 'Arms: ');
				AddWornItemToFile(MorgueFile, S_HANDS, 'Hands: ');
				AddWornItemToFile(MorgueFile, S_LEGS, 'Legs: ');
				AddWornItemToFile(MorgueFile, S_FEET, 'Feet: ');
				AddWornItemToFile(MorgueFile, S_MAINHAND, 'Mainhand: ');
				AddWornItemToFile(MorgueFile, S_OFFHAND, 'Offhand: ');
				AddWornItemToFile(MorgueFile, S_RANGED, 'Ranged: ');
				AddWornItemToFile(MorgueFile, S_BACK, 'Back: ');
				AddWornItemToFile(MorgueFile, S_LEFTFINGER, 'Left Finger: ');
				AddWornItemToFile(MorgueFile, S_RIGHTFINGER, 'Right Finger: ');
				
				{ Add carried/inventory items }
				MorgueFile.Add('');
				MorgueFile.Add('Carried:');
				MorgueFile.Add('');
				AddInventoryItems(MorgueFile, Dead);

				{ Add levels explored }
				MorgueFile.Add('');
				MorgueFile.Add('-----------');
				MorgueFile.Add('Exploration');
				MorgueFile.Add('-----------');
				MorgueFile.Add('');
				GetDelves(MorgueFile);
				
				{ Current display }
				MorgueFile.Add('');
				MorgueFile.Add('-------');
				MorgueFile.Add('Dungeon');
				MorgueFile.Add('-------');
				MorgueFile.Add('');
				DumpDisplay(PlayerX, PlayerY, MorgueFile);

        { Deal with visible monsters }
        if FormDisplay.ListViewMonsters.Items <> nil then
        begin
          if FormDisplay.ListViewMonsters.Items.Count > 0 then
          begin
            MorgueFile.Add('');
            if Dead then
              MorgueFile.Add('You could see: ')
            else
              MorgueFile.Add('You can see: ');
            for Loop := 0 to FormDisplay.ListViewMonsters.Items.Count - 1 do
              MorgueFile.Add(Format(' %s',
                [FormDisplay.ListViewMonsters.Items[Loop].Caption]));
          end;
        end;

				{ Add last few messages }
				MorgueFile.Add('');
				MorgueFile.Add('--------');
				MorgueFile.Add('Messages');
				MorgueFile.Add('--------');
				MorgueFile.Add('');
        if FormDisplay.UpdateLog.Lines.Count < 20 then
        begin
          for Loop := 0 to FormDisplay.UpdateLog.Lines.Count do
            MorgueFile.Add(Trim(FormDisplay.UpdateLog.Lines[Loop]));
        end
        else
        begin
          for Loop := FormDisplay.UpdateLog.Lines.Count - 20 to
            FormDisplay.UpdateLog.Lines.Count do
            MorgueFile.Add(Trim(FormDisplay.UpdateLog.Lines[Loop]));
        end;

        { Handle score }
        if Dead then
          if Player.Gender = gMale then
				    MorgueFile[10] := Format('He scored %d points', [Score])
          else
            MorgueFile[10] := Format('She scored %d points', [Score])
        else
          if Player.Gender = gMale then
				    MorgueFile[10] := Format('He has scored %d points so far', [Score])
          else
            MorgueFile[10] :=
              Format('She has scored %d points so far', [Score]);

        { Add list of kills }
        MorgueFile.Add('');
				MorgueFile.Add('-------------');
				MorgueFile.Add('Defeated Foes');
				MorgueFile.Add('-------------');
				MorgueFile.Add('');
        TotalKills := 0;
        for Loop := 0 to FormDisplay.Game.MonsterKills.Count - 1 do
        begin
          if FormDisplay.Game.MonsterKillNumbers[Loop] = 1 then
            MorgueFile.Add(Format(' %d %s',
              [FormDisplay.Game.MonsterKillNumbers[Loop],
              FormDisplay.Game.MonsterKills[Loop]]))
          else
            MorgueFile.Add(Format(' %d %s',
              [FormDisplay.Game.MonsterKillNumbers[Loop],
              FormDisplay.Game.MonsterKillsPlural[Loop]]));
          Inc(TotalKills, FormDisplay.Game.MonsterKillNumbers[Loop]);
        end;
        MorgueFile.Add('');
        MorgueFile.Add(Format('Total: %d creatures defeated', [TotalKills]));

				{ Notes and Milestones }
				MorgueFile.Add('');
				MorgueFile.Add('-----');
				MorgueFile.Add('Notes');
				MorgueFile.Add('-----');
				MorgueFile.Add('');
				MorgueFile.Add(Player.Notes);		
			end;

			{ Return the formatted template }
			Result := MorgueFile.Text;
		finally;
			{ In all cases, free the memory allocated }
			MorgueFile.Clear;
			MorgueFile.Free;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Add inventory item details to the character dump }
procedure AddInventoryItems(var MorgueFile: TStringList; Dead: Boolean);
var
  Counter: Integer;
  Description: TStringList;
  Game: TGame;
  LocalItem: TItem;
  Loop: Integer;
  Player: TCreature;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.AddInventoryItems()');
  
  try
  	{ Get a shortcut to the player and game, without using the accursed 'with' }
    Game := FormDisplay.Game;
		Player := FormDisplay.Game.Player;
		
		{ Create a local stringlist to hold the item details }
		Description := TStringList.Create;
		
		try
      { If the character is dead, make the scrolls and potions carried known }
      if Dead then
      begin
        { Reset the scroll IDs to known }
        For Counter := Low(Game.ScrollIDed) to High(Game.ScrollIDed) do
          Game.ScrollIDed[Counter] := stKnown;

        { Reset the potion IDs to known }
        For Counter := Low(Game.PotionIDed) to High(Game.PotionIDed) do
          Game.PotionIDed[Counter] := poKnown;
      end;

			{ Iterate through each inventory slot }
			for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
			begin
				Description.Clear;
				
				{ Check we have an item in this slot }
				if Player.Inventory[Loop] > 0 then
				begin
					{ Get the item }
					LocalItem := GItemList[Player.Inventory[Loop]] as TItem;
					
					{ If the player is dead then identify all items }
					if Dead then
						LocalItem.Known := True;

					{ Add the name of the item }	
					MorgueFile.Add(Format('%s (%d gp)', [LocalItem.Name,
						LocalItem.Value]));
					
					{ If it is a magical item add the effects }
				  if (LocalItem.ItemQuality >= iSuperb) and (LocalItem.Known) then
          //if (LocalItem.Known) then
					begin
						LocalItem.GetItemEffects(Description);
						MorgueFile.Add('  ' + Description.Text);
					end;
				end;
				{ Don't bother displaying anything for empty slots }
			end;
		finally
			{ Free the stringlist in all circumstances }
			Description.Free;
			Description := nil;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Add worn item details to the character dump }
procedure AddWornItemToFile(var MorgueFile: TStringList; ItemSlot: Integer; 
	SlotDescription: String);
var
  Description: TStringList;
  LocalItem: TItem;
  Player: TCreature;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.AddWornItemToFile()');
  
  try
  	{ Get a shortcut to the player, without using the accursed 'with' }
		Player := FormDisplay.Game.Player;
		
		{ Create a local stringlist to hold the item details }
		Description := TStringList.Create;
		try
			{ Check we have an item in this slot }
			if Player.Inventory[ItemSlot] > 0 then
			begin
				{ Get the item }
				LocalItem := GItemList[Player.Inventory[ItemSlot]] as TItem;
				
				{ Add the name of the item }
				MorgueFile.Add(Format('%s%s (%d gp)', [SlotDescription, LocalItem.Name,
				  LocalItem.Value]));
				
				{ If it is a magical item add the effects }
				if LocalItem.ItemQuality >= iSuperb then
				begin
					LocalItem.GetItemEffects(Description);
					MorgueFile.Add('  ' + Description.Text);
				end;
			end
			else
				MorgueFile.Add(SlotDescription + '[EMPTY]');
		finally
			{ Free the stringlist in all circumstances }
			Description.Free;
			Description := nil;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get information on what the player has explored }
procedure GetDelves(var MorgueFile: TStringList);
var
  Count: Integer;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetDelves()');
  
  try
		Count := 0;
		
		{ Find the total levels visted }
		for Loop := 1 to 9 do
			Inc(Count, FormDisplay.Game.GDepthDelved[Loop]);
			
		{ Add this to the dump }
		MorgueFile.Add(Format('You visited %d unique levels of the Multiverse:',
		  [Count]));
		MorgueFile.Add('');
		
		{ Check each dungeon branch for the amount of levels visited }
		for Loop := 1 to 9 do
			if FormDisplay.Game.GDepthDelved[Loop] > 0 then
				MorgueFile.Add(Format('%s : %d', [GDungeonNameArray[Loop],
					FormDisplay.Game.GDepthDelved[Loop]]));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Dump the current display to a StringList, one line at a time }
procedure DumpDisplay(PlayerX: Integer; PlayerY: Integer; 
	var MorgueFile: TStringList);
var
  X: Integer;
  Y: Integer;
  Line: String;
  Theme: Integer;
  ObjectID: Integer;
  EffectID: Integer;
  MonsterID: Integer;
  Zone: Integer;
  Terrain: Integer;
  LocalMonster: TMonster;
  LocalItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.DumpDisplay()');
  
  try
  	{ Build this up in reverse order to the way it is done on screen, one 
  	  line at a time }
		for Y := PlayerY + 6 downto PlayerY - 6 do
		begin
			{ Reset the current line }
			Line := '';
			for X := PlayerX - 10 to PlayerX + 10 do
			begin			
				{ Check for valid map cell }
				if not(InBounds(Point(X, Y))) then
				begin
				  Line := Line + ' ';
					continue; 
				end;

				{ Get what is present at this map cell }
				ObjectID := FormDisplay.Game.Dungeon.Objects[X, Y];
				EffectID := FormDisplay.Game.Dungeon.Effects[X, Y];
				MonsterID := FormDisplay.Game.Dungeon.Monsters[X, Y];
				Zone := FormDisplay.Game.Dungeon.Zone[X, Y];
				Terrain := FormDisplay.Game.Dungeon.Terrain[X, Y];

        { Deal with zones }
				if (Zone > 0) then 
					Theme := Zone 
				else 
					Theme := FormDisplay.Game.Dungeon.LevelTheme;

				{ Write the contents out, in order of priority }
				if (X = PlayerX) and (Y = PlayerY) then
					Line := Line + '@'
				else if MonsterID > 0 then
				begin
					{ Get the monster present }
					LocalMonster := (GMonsterList.Items[MonsterID] as TMonster);
					Line := Line + LocalMonster.Char;
				end
				else if ObjectID > 0 then
				begin
					{ If there is an object present, display just the top item }
					if ObjectID >= ITEM_GOLD then 
						Line := Line + '$'
					else
					begin
						LocalItem := (GItemList.Items[ObjectID] as TItem);
						Line := Line + LocalItem.ItemSymbol;
					end;
				end
				else if EffectID > 0 then	
					{ Deal with effects }
					Line := Line +  GDungeonEffectArray[Theme]
				else
				begin
					{ Only if none of the conditions above is fulfilled, display the 
					  terrain }
					if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
						{ Handle zones and the town level }
						if (Zone > 0) and 
							((FormDisplay.Game.Dungeon.Terrain[X, Y] = (T_FLOOR_ROOM)) or 
							(FormDisplay.Game.Dungeon.Terrain[X, Y] 
							= (T_FLOOR_CORRIDOR))) then
							Line := Line + GDungeonEffectArray[Terrain]
						else
							Line := Line + GStandardASCII[Terrain]
					else
						Line := Line + GStandardTownASCII[Terrain];
				end;
			end;
			
			{ Add the built up line to the output }
			MorgueFile.Add(Line);
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert a TPoint to a string }
function PointToStr(Point: TPoint): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.PointToStr()');

  { Default result }
  Result := ' (0/0) ';
  
  try
  	Result := Format(' (%d/%d) ', [Point.X, Point.Y]);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if we can display a level feeling }
function CanFeelLevel: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.CanFeelLevel()');

  { Default result }
  Result := False;
  
  try
  	{ Don't display level feelings too often }
  	Result :=
      (FormDisplay.Game.Turns - FormDisplay.Game.LastLevelFeeling)
      > LEVEL_FEELING_INTERVAL;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Elapsed Game Time }
function GetElapsedGameTime: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetElapsedGameTime()');

  { Default result }
  Result := '';
  
  try
  	 {Build up the elapsed time }
  	 Result := Format('%dh %dm %ds', [HourOf(Now - 
  	 	FormDisplay.Game.GameStartTime), MinuteOf(Now - 
  	 	FormDisplay.Game.GameStartTime), SecondOf(Now - 
  	 	FormDisplay.Game.GameStartTime)]); 
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;


{ This function draw a line from x1, y1 to x2, y2, printing the projectile 
  specified at each part }
procedure AnimateMissileWeapon(Point1: TPoint; Point2: TPoint; 
	Character: Char; Colour: TColor; ProjectileType: tProjectile = prMissile);
var
  d, x, y, ax, ay, sx, sy, dx, dy:integer;
  temp: boolean;
  X1: Integer;
  X2: Integer;
  Y1: Integer;
  Y2: Integer;
  PreviousLocX: Integer;
  PreviousLocY: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.AnimateMissileWeapon()');

  try
  	{ Display refresh whilst this is being drawn }
		//FormDisplay.Timer.Enabled := False;
		//FormDisplay.TimerAnimate.Enabled := False;

	  { Initialise }
    PreviousLocX := Point1.X;
    PreviousLocY := Point1.Y;
		X1 := Point1.X;
		X2 := Point2.X;
		Y1 := Point1.Y;
		Y2 := Point2.Y;
		temp := false;
		
		{ Precalculate directions and offsets }
		dx := x2 - x1;
		dy := y2 - y1;
		ax := abs(dx) * 2;
		ay := abs(dy) * 2;
		if dx >= 0 then 
			sx := 1
		else 
			sx := -1;
		if dy >= 0 then 
			sy := 1 
		else 
			sy := -1;
		x := x1;
		y := y1;
		
		{ Check if we are mainly horizontal or vertical }
		if ax > ay then
		begin
			{ x direction dominant }
			d := ay - (ax div 2);
			repeat
				{ Note x and y here are the absolute dungeon co-ordinates }

        { Blank previous projectile if necessary }
        if ProjectileType = prMissile then
        begin
          FormDisplay.Game.Dungeon.Projectiles[PreviousLocX, PreviousLocY] := ' ';
          FormDisplay.Game.Dungeon.ProjectilesColour[PreviousLocX,
            PreviousLocY] := clBlack;
        end;

        { Draw projectile in new square }
        FormDisplay.Game.Dungeon.Projectiles[X, Y] := Character;
        FormDisplay.Game.Dungeon.ProjectilesColour[X, Y] := Colour;

        { Keep track of projectile path }
        PreviousLocX := X;
        PreviousLocY := Y;

				//FormDisplay.DrawCharacterToSurface(MainRect, FormDisplay.ScreenMain,
				//	Point(X, Y), Character, Colour);

				{ break out if necessary }
				if x = x2 then break;
				
				{ Move to next square }
				if d > 0 then
				begin
					inc(y, sy);
					dec(d, ax);
				end;
				inc(x, sx);
				inc(d, ay);
        
        { Now do the animation }
        case ProjectileType of
          prMissile: Sleep(ANIMATION_DELAY);
          prBeam: Sleep(ANIMATION_DELAY div 2);
          prBolt: Sleep(ANIMATION_DELAY);
        end;
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
				
				{ Keep going }
			until temp = true;
		end
		else
		begin
			{ y direction dominant }
			d:= ax - (ay div 2);
			repeat
				{ Note x and y here are the absolute dungeon co-ordinates }

        { Blank previous projectile if necessary }
        if ProjectileType = prMissile then
        begin
          FormDisplay.Game.Dungeon.Projectiles[PreviousLocX, PreviousLocY] := ' ';
          FormDisplay.Game.Dungeon.ProjectilesColour[PreviousLocX,
            PreviousLocY] := clBlack;
        end;

        { Draw projectile in new square }
        FormDisplay.Game.Dungeon.Projectiles[X, Y] := Character;
        FormDisplay.Game.Dungeon.ProjectilesColour[X, Y] := Colour;

        { Keep track of projectile path }
        PreviousLocX := X;
        PreviousLocY := Y;

				{ break out if necessary }
				if y = y2 then break;
				
				{ Move to next square }
				if d >= 0 then
					begin
						inc(x, sx);
						dec(d, ay);
					end;
				inc(y, sy);
				inc(d, ax);

        { Now do the animation }
        case ProjectileType of
          prMissile: Sleep(ANIMATION_DELAY);
          prBeam: Sleep(ANIMATION_DELAY div 2);
          prBolt: Sleep(ANIMATION_DELAY);
        end;
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

				{ Keep going }
			until temp=true;
		end;

    { At the end, remove the last position of the missile projectile if needed }
    if ProjectileType = prMissile then
    begin
      FormDisplay.Game.Dungeon.Projectiles[PreviousLocX, PreviousLocY] := ' ';
      FormDisplay.Game.Dungeon.ProjectilesColour[PreviousLocX,
        PreviousLocY] := clBlack;
    end
    else if ProjectileType = prBeam then
    begin
      { Add an extra delay for visibility }
      Sleep(ANIMATION_DELAY * 3);

      { Blank all projectiles }
      for X := 1 to DUNGEONSIZEX do
      begin
				for Y := 1 to DUNGEONSIZEY do
				begin
          FormDisplay.Game.Dungeon.Projectiles[X, Y] := ' ';
          FormDisplay.Game.Dungeon.ProjectilesColour[X, Y] := clBlack;
        end;
      end;
    end;
		
		{ Enable refresh of display }
		//FormDisplay.Timer.Enabled := True;
		//FormDisplay.TimerAnimate.Enabled := True;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random description of how tasty food is when easting }
function GetFoodDescription: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetFoodDescription()');

  { Default result }
  Result := '';
  
  try
  	{ Return a random mixture of descriptions }
  	Result := Trim(GFoodFeelingsPrefix[Random(High(GFoodFeelingsPrefix)) + 1] 
  		+ ' ' +  GFoodFeelings[Random(High(GFoodFeelings)) + 1]); 
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;


{ Monster can drop an item }
function MonsterDropItem(Monster: TMonster): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.MonsterDropItem()');

  { Default result }
  Result := False;

  try
    { If the monster has a weapon }
    if Monster.Weapon > NO_ITEM then
    begin
      { If no item is present then drop it }
      if (FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] = 0) or
        (FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] >=
        ITEM_GOLD) then
      begin
        (GItemList[Monster.Weapon] as TItem).Location := iFloor;
        FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] :=
          Monster.Weapon;
        Monster.Weapon := NO_ITEM;
        Result := True;
      end
      else
      begin
        { If there is an item already present, add it to the pile - note that
          this will not work if there are already is more than one item
          present! }
        (GItemList[FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y]] as
          TItem).NextItem := Monster.Weapon;
        (GItemList[Monster.Weapon] as TItem).Location := iFloor;
        Monster.Weapon := NO_ITEM;
        Result := True;
      end;
    end;
    if (Monster.Armour > -1) then
    begin
      begin
        { If no item is present then drop it }
        if (FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] = 0) or
          (FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] >=
          ITEM_GOLD) then
        begin
          (GItemList[Monster.Armour] as TItem).Location := iFloor;
          FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y] :=
            Monster.Armour;
          Monster.Armour := NO_ITEM;
          Result := True;
        end
        else
        begin
          { If there is an item already present, add it to the pile - note that
          this will not work if there are already is more than one item
          present! }
          (GItemList[FormDisplay.Game.Dungeon.Objects[Monster.X, Monster.Y]] as
            TItem).NextItem :=  Monster.Armour;
          (GItemList[Monster.Armour] as TItem).Location := iFloor;
          Monster.Armour := NO_ITEM;
          Result := True;
        end;
      end;
    end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Returns true if the first character of a string is a vowel }
function Vowel(StringToCheck: String): Boolean;
var
  NameString: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.Vowel()');

  { Default result }
  Result := False;
  
  try
  	{ Check if the first character is a vowel }
		NameString := Trim(StringToCheck);
		if Length(NameString) > 0 then
			Result := (NameString[1] in ['a','e','i','o','u','A','E','I','O','U']);
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Convert the first character of a strong to lowercase }
function Lower(StringToChange: String): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.Lower()');

  try
    { Get the string to change }
    Result := StringToChange;

  	{ Convert the first character of the input string to lowercase }
    Result[1] := LowerCase(Copy(Result, 1, 1))[1];
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Random function - equivalent of if (Rand(Chance) = 0) }
function OneChanceIn(Chance: Integer): Boolean;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.OneChanceIn()');

  { Default result }
  Result := False;
  
  try
  	{ Get the random chance }
    if Chance > 0 then
  	  Result := Random(Chance) = 0;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Return a random monster type }
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
	MonsterEcology: String = ''; MonsterOccurrence: String = '';
	LooseMatching: Boolean = False): TMonsterArchetype; overload;
var
  Counter: Integer;
  RandomMonsterArchetype: TMonsterArchetype;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomMonsterArchetype()');

  { Default result }
  Result := nil;
  
  try
  	{ Safety condition }
		RandomMonsterArchetype := nil;
		
		{ Find the desired monster type }	
		if Length(Trim(MonsterEcology)) > 0 then
		begin
			if Length(Trim(MonsterOccurrence)) > 0 then
			begin
				{ Use a paranoia counter just in case we can't find any appropriate
				  monsters }			
				Counter := 0;
				repeat
					inc(Counter);
					RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
						LooseMatching);
				until ((RandomMonsterArchetype.Occurrence = MonsterOccurrence) and 
					(Pos(MonsterEcology, RandomMonsterArchetype.Ecology) > 0)) or
					(Counter > MONSTER_PARANOIA);
					
				{ If we haven't found a suitable monster then do a looser search }
				if (Counter > MONSTER_PARANOIA) then
				begin
					repeat
						RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
							LooseMatching);
					until (Pos(MonsterEcology, RandomMonsterArchetype.Ecology) > 0);
				end;
			end
			else
			begin
				{ If we haven't found a suitable monster then do a looser search }
				repeat
					RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
						LooseMatching);
				until (Pos(MonsterEcology, RandomMonsterArchetype.Ecology) > 0);
			end;
		end
		else
		begin
			{ Do a looser search }
			if (Length(Trim(MonsterOccurrence)) > 0) then
			begin
				repeat
					RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
					LooseMatching);
				until RandomMonsterArchetype.Occurrence = MonsterOccurrence;
			end
			else
			begin
				{ Loosest search of all }
				RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
				LooseMatching);
			end;
		end;

		{ Assign the result }
		Result := RandomMonsterArchetype;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Return a random monster type }
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
	MonsterOccurrence: String = ''; 
	LooseMatching: Boolean = False): TMonsterArchetype;
var
  RandomMonsterArchetype: TMonsterArchetype;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomMonsterArchetype()');

  { Default result }
  Result := nil;
  
  try
  	{ Safety condition }
		RandomMonsterArchetype := nil;
		
		{ Find the desired monster type }	
		if (Length(Trim(MonsterOccurrence)) > 0) then
		begin
			{ Search through all monsters looking for a specific type }
			repeat
				RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
					LooseMatching);
			until (RandomMonsterArchetype.Occurrence = MonsterOccurrence);
		end
		else
		begin
			{ Do a looser search }
			RandomMonsterArchetype := GetRandomMonsterArchetype(MonsterLevel, 
				LooseMatching);
		end;
		
		{ Assign the result }
		Result := RandomMonsterArchetype;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random monster according to the ecology specified }
function GetRandomMonsterForEcology(Ecology: String; 
	MonsterOccurrence: String): TMonsterArchetype;
var
  Counter: Integer;
  RandomMonsterArchetype: TMonsterArchetype;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomMonsterForEcology()');

  { Default result }
  Result := nil;
  
  try
  	{ Safety condition }
		RandomMonsterArchetype := nil;
		
		{ Find the desired monster type }
		ObjectList := GMonsterTypeList;
		
		{ Use a paranoia counter just in case we can't find any appropriate
			monsters }	
		Counter := 0;
		repeat
			inc(Counter);
			RandomMonsterArchetype := 
				ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
		until ((RandomMonsterArchetype.Occurrence = MonsterOccurrence) and 
			(Pos(Ecology, RandomMonsterArchetype.Ecology) > 0))
			or (Counter > MONSTER_PARANOIA);
			
		{ If we haven't found a suitable monster then do a looser search }
		if Counter > MONSTER_PARANOIA then
		begin
			repeat
				RandomMonsterArchetype := 
					ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
			until (Pos(Ecology, RandomMonsterArchetype.Ecology) > 0)
		end;

		{ Assign the result }
		Result := RandomMonsterArchetype;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random monster archetype that can have a unique monster } 
function GetRandomUniqueMonster(MonsterLevel: Integer; 
	var MonsterFound: Boolean): TMonsterArchetype;
var
  Counter: Integer;
  RandomMonsterArchetype: TMonsterArchetype;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomUniqueMonster()');

  { Default result }
  Result := nil;
  MonsterFound := False;
  
  try
  	{ Set up the list to be accessed }
  	ObjectList := GMonsterTypeList;

  	{ Safety condition }
  	RandomMonsterArchetype := nil;

		{ Use a paranoia counter just in case we can't find any appropriate
			monsters }
  	Counter := 0; 
		repeat
			inc(Counter);
			RandomMonsterArchetype := 
				ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
		until ((RandomMonsterArchetype.Level >= MonsterLevel - 3) and 
			(RandomMonsterArchetype.Level <= MonsterLevel + 3)
			or (Counter > MONSTER_PARANOIA)) and (RandomMonsterArchetype.CanBeUnique);

		{ Sometimes there are no unique monsters to be found but if there are,
		  flag this back }
		if Counter < MONSTER_PARANOIA then
		begin
			Result := RandomMonsterArchetype;
			MonsterFound := True;
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Internal function to get a random monster }
function GetRandomMonsterArchetype(MonsterLevel: Integer; 
	LooseMatching: Boolean = False): TMonsterArchetype;
var
  Counter: Integer;
  RandomMonsterArchetype: TMonsterArchetype;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomMonsterArchetype()');

  { Default result }
  Result := nil;
  
  try
    { Set up the list to be accessed }
		ObjectList := GMonsterTypeList;

		{ Safety condition }
		RandomMonsterArchetype := nil;
		
		{ Loose matching enables searching over a wider range of levels }
		if LooseMatching then
		begin
			{ Use a paranoia counter just in case we can't find any appropriate
				monsters }
			Counter := 0;
			repeat
				inc(Counter);
				RandomMonsterArchetype := 
					ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
			until ((RandomMonsterArchetype.Level >= MonsterLevel - 1) and 
				(RandomMonsterArchetype.Level <= MonsterLevel + 1) or
        (Counter > MONSTER_PARANOIA));
				
			{ Go to wider matching if we can't find an exact monster type }	
			if (Counter > MONSTER_PARANOIA) then
			begin
				repeat
					RandomMonsterArchetype := 
						ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
				until (RandomMonsterArchetype.Level >= MonsterLevel - 6) 
					and (RandomMonsterArchetype.Level <= MonsterLevel + 6)
			end;
		end
		else
		begin
			{ Find an exact monster }
			repeat
				RandomMonsterArchetype := ObjectList.Items[Random(ObjectList.Count)] as TMonsterArchetype;
			until RandomMonsterArchetype.Level = MonsterLevel;
		end;
		
		{ Assign the result }		
		Result := RandomMonsterArchetype;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Given an ID, return the associated Monster type } 
function GetMonsterArchetype(ID: Integer): TMonsterArchetype;
var
  Loop: Integer;
  LocalMonsterArchetype: TMonsterArchetype;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetMonsterArchetype()');

  { Default result }
  Result := nil;
  
  try
    { Set up the list to be accessed }		
		ObjectList := GMonsterTypeList;
		
		{ Iterate through the monster archetypes looking for the one we want }
		for Loop := 0 to ObjectList.Count - 1 do
		begin
			LocalMonsterArchetype := ObjectList.Items[Loop] as TMonsterArchetype;
			if LocalMonsterArchetype.ID = ID then
			begin
				{ We've found it so exit now }
				Result := ObjectList.Items[Loop] as TMonsterArchetype;
				Exit;
			end;
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate known item descriptors for certain types of item only }
function GenerateKnownDescriptor(ItemLevel: Integer; ItemType: crItemType): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GenerateKnownDescriptor()');

  { Default result }
  Result := '';
  
  try
  	{ Only do this for Rings or Amulets }
		if (ItemType = iRing) or (ItemType = iAmulet) then
			{ Now check the item level }
			if ItemLevel = 0 then
				Result := 
					GNonMagicalItemDescriptorArray[Random(High
					(GNonMagicalItemDescriptorArray)) + 1]
			else if ItemLevel < (MAX_LEVEL div 2) then
				Result := 
					GLowLevelMagicalItemDescriptorArray[Random(High
					(GLowLevelMagicalItemDescriptorArray)) + 1]
			else if ItemLevel < MAX_LEVEL then
				Result := 
					GMediumLevelMagicalItemDescriptorArray[Random(High
					(GMediumLevelMagicalItemDescriptorArray)) + 1]
			else
				Result := GHighLevelMagicalItemDescriptorArray[Random(High
					(GHighLevelMagicalItemDescriptorArray)) + 1];
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate unknown item descriptors for all items }
function GenerateUnknownDescriptor(ItemQuality: crItemQuality): String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GenerateUnknownDescriptor()');

  { Default result }
  Result := '';
  
  try
  	{ Only magical items have unknown descriptors }
		case ItemQuality of
			iUnknownQuality: Result := ''; 
			iWorthless: Result := '';
			iCommon: Result := ''; 
			iSuperb: Result := GLowLevelMagicalItemDescriptorArray[Random(High
				(GLowLevelMagicalItemDescriptorArray)) + 1];
			iLegendary: Result := GMediumLevelMagicalItemDescriptorArray[Random(High
				(GMediumLevelMagicalItemDescriptorArray)) + 1];
			iEpic: Result := GHighLevelMagicalItemDescriptorArray[Random(High
				(GHighLevelMagicalItemDescriptorArray)) + 1];
			iArtifact: Result := 'Ancient';
			iSpecial: Result := GLowLevelMagicalItemDescriptorArray[Random(High
				(GLowLevelMagicalItemDescriptorArray)) + 1];
		end;
	except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Internal Sorting Routine for Item Lists }
function CompareMagnitudes(Item1, Item2: Pointer): Integer;
var
  Enchantment1: TItemEnchantment;
  Enchantment2: TItemEnchantment;
  EnchantmentMag1: Real;
  EnchantmentMag2: Real;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.CompareMagnitudes()');

  { Default result }
  Result := 0;
  
  try
  	{ Compare Items using Scaling Factors/Magnitudes }
		Enchantment1 := Item1;
		Enchantment2 := Item2;
		EnchantmentMag1 := Enchantment1.Magnitude * Enchantment1.ScalingFactor;
		EnchantmentMag2 := Enchantment2.Magnitude * Enchantment2.ScalingFactor;

		{ There is a fixed set of results defined by TObjectList }
		if EnchantmentMag1 = EnchantmentMag2 then 
			Result := 0
		else if EnchantmentMag1 > EnchantmentMag2 then 
			Result := -1
		else 
			Result := 1;
		except
			{ in case of error, log the Exception }
			on E: Exception do hLog.AddException(E);
		end;
end;

{ Get an random item type }
function GetRandomItemArchetype: TItemArchetype;
var
  ObjectList: TObjectList;
  RandomItemArchetype: TItemArchetype;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomItemArchetype()');

  { Default result }
  Result := nil;
  
  try
		{ So, what do we want to go for }
		ObjectList := ReturnRandomItemList;

		{ Whatever list we've chosen, get an item from it }
		RandomItemArchetype := 
			ObjectList.Items[Random(ObjectList.Count)] as TItemArchetype;
		
		{ Return the item }
		Result := RandomItemArchetype;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a specific enchantment }
function GetSpecificEnchantment(EnchantmentName: String): TItemEnchantment;
var
  Loop: Integer;
  RandomEnchantment: TItemEnchantment;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetSpecificEnchantment()');

  { Default result }
  Result := nil;
  
  try
		{ Iterate through the enchantmentlist looking for the enchantment we are
		  after }
		for Loop := 1 to GEnchantmentList.Count - 1 do
		begin
			RandomEnchantment := GEnchantmentList.Items[Loop] as TItemEnchantment;
			if RandomEnchantment.Name = EnchantmentName then
			begin
				{ Break if we've found it }
				Result := RandomEnchantment;
				Break;
			end;
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a specific item type }
function GetSpecificItemArchetype(ObjectList: TObjectList; 
	ArchetypeName: String): TItemArchetype;
var
  Loop: Integer;
  RandomItemArchetype: TItemArchetype;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetSpecificItemArchetype()');

  { Default result }
  Result := nil;
  
  try
  	{ Iterate through the itemlist looking for the itemtype we are after }
		for Loop := 0 to ObjectList.Count - 1 do
		begin
			RandomItemArchetype := ObjectList.Items[Loop] as TItemArchetype;
			if RandomItemArchetype.ItemName = ArchetypeName then
			begin
				{ Break if we've found it }
				Result := RandomItemArchetype;
				Break;
			end;
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random item from a specified archetype type }
function GetRandomTypeItemArchetype(ArchetypeType: crItemType): TItemArchetype;
var
  RandomItemArchetype: TItemArchetype;
  ObjectList: TObjectList;
begin

  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.GetRandomTypeItemArchetype()');

  { Default result }
  Result := nil;
  
  try		
		{ Depending on the itemtype passed in, access the appropriate list }
		case ArchetypeType of
			iWeapon: ObjectList := GWeaponList;
			iArmour: ObjectList := GArmourList;
			iRing: ObjectList := GJewelleryList;
			iAmulet: ObjectList := GJewelleryList;
			iPotion: ObjectList := GPotionList;
			iWand: ObjectList := GWandList;
			iScroll: ObjectList := GScrollList;
			iConsumable: ObjectList := GFoodList;
		else
			ObjectList := nil;
		end;
		
		{ If we have a list } 
		if Assigned(ObjectList) then
		begin
			{ Try and find a random item }
			RandomItemArchetype := nil;
			repeat
				RandomItemArchetype := 
					ObjectList.Items[Random(ObjectList.Count)] as TItemArchetype;
			until RandomItemArchetype.ItemType = ArchetypeType;

			{ Return the item }
			Result := RandomItemArchetype;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random item type }
function ReturnRandomItemList: TObjectList;
var
  Chance: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.ReturnRandomItemList()');

  { Default result }
  Result := nil;
  
  try
  	{ Get a random list }
		Chance := Random(6) + 1;
		case Chance of
			1: Result := GWeaponList;
			2: Result := GArmourList;
			3: Result := GJewelleryList;
			4: Result := GPotionList;
			5: Result := GWandList;
			6: Result := GScrollList;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the item material enum from a string }
function GetMaterial(Material: String): crMaterial;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} UnitFunctions.GetMaterial(''%s'')', [Material]));

  { Default result }
  Result := mOther;

  try
   if (Material = 'Metal') then
    Result := mMetal
  else if (Material = 'Cloth') then
    Result := mCloth
  else if (Material = 'Wood') then
    Result := mWooden
  else if (Material = 'Leather') then
    Result := mLeather
  else if (Material = 'Stone') then
    Result := mStone;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the item slot enum from a string }
function GetSlot(ItemSlot: String): crItemSlot;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} UnitFunctions.GetSlot(''%s'')', [ItemSlot]));

  { Default result }
  Result := iInventory;

  try
    if (ItemSlot = 'Shield') then
      Result := iOffhand
    else if (ItemSlot = 'Head Armour') then
      Result := iHead
    else if (ItemSlot = 'Chest Armour') then
      Result := iChest
    else if (ItemSlot = 'Wrist Armour') then
      Result := iArms
    else if (ItemSlot = 'Hand Armour') then
      Result := iHands
    else if (ItemSlot = 'Leg Armour') then
      Result := iLegs
    else if (ItemSlot = 'Feet Armour') then
      Result := iFeet
    else if (ItemSlot = 'Ring') then
      Result := iFinger
    else if (ItemSlot = 'Amulet') then
      Result := iNeck
    else if (ItemSlot = 'Melee Weapon') then
      Result := iMainhand
    else if (ItemSlot = 'Ranged Weapon') then
      Result := iRanged;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


{ Return a colour from a string }
function GetColour(ItemColour: String): TColor;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} UnitFunctions.GetColour(''%s'')', [ItemColour]));

  { Default result }
  Result := clWhite;

  try
    if (ItemColour = 'Red') then
      Result := clRed
    else if (ItemColour = 'Yellow') then
      Result := clYellow
    else if (ItemColour = 'Red') then
      Result := clRed
    else if (ItemColour = 'Gray') then
      Result := clGray
    else if (ItemColour = 'Green') then
      Result := clGreen
    else if (ItemColour = 'Orange') then
      Result := $000080FF
    else if (ItemColour = 'Lime') then
      Result := clLime
    else if (ItemColour = 'Brown') then
      Result := $00004080;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Write a message to the wizard log }
procedure WizardLog(StringToDisplay: String);
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.WizardLog()');

  try
    if Assigned(FormWizard) then
      if FormWizard.Visible = True then
        FormWizard.MemoLog.Lines.Add(StringToDisplay);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


{ Dice-rolling routine for character creation }
function RollStartingDice(RacialModifier: Integer; var StatModifier: Integer;
  MinimumValue: Integer): Integer;
var
  Stat: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.RollStartingDice()');

  { Default result }
  Result := 0;
  
  try
    { Generate a starting stat from 2d8 + 1d4 }
    repeat
      Stat := ((Random(8) + 1) + (Random(8) + 1) + (Random(4) + 1)) +
        RacialModifier;
    until (Stat > (MinimumValue + RacialModifier)) and (Stat < 21);

    { The modifier is the stat minus 10 }
    StatModifier := (Stat - 10);

    { Set the result }
    Result := Stat;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


{ Create a character - this routine needs refactoring as it is a copy of all the
  functionality that was previously contained in the character creation DLL }
function CreateCharacter(var Character: TCreature): String;
var
  StatOrderCondition: Boolean;
  TempTotal: Integer;
  Intelligence: Integer;
  IntelligenceMod: Integer;
  IntelligenceRac: Integer;
  Charisma: Integer;
  CharismaMod: Integer;
  CharismaRac: Integer;
  Endurance: Integer;
  EnduranceMod: Integer;
  EnduranceRac: Integer;
  Strength: Integer;
  StrengthMod: Integer;
  StrengthRac: Integer;
  Agility: Integer;
  AgilityMod: Integer;
  AgilityRac: Integer;
  Resolve: Integer;
  ResolveMod: Integer;
  ResolveRac: Integer;
  AC: Integer;
  Evasion: Integer;
  HP: Integer;
  MP: Integer;
  Res: Integer;
  Speed: Integer;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitFunctions.CreateCharacter()');

  { Default result }
  Result := '';
  
  try
    { Work out stat adjustments }
    case Character.SubRace of
      srHuman:
      begin
        IntelligenceRac := 0;
        CharismaRac := 0;
        EnduranceRac := 0;
        StrengthRac := 0;
        AgilityRac := 0;
        ResolveRac := 0;
      end;
      srDwarf:
      begin
        IntelligenceRac := 0;
        CharismaRac := 0;
        EnduranceRac := 1;
        StrengthRac := 0;
        AgilityRac := -2;
        ResolveRac := 1;
      end;
      srElf:
      begin
        IntelligenceRac := 1;
        CharismaRac := 1;
        EnduranceRac := 0;
        StrengthRac := -2;
        AgilityRac := 0;
        ResolveRac := 0;
      end;
      srOrc:
      begin
        IntelligenceRac := -2;
        CharismaRac := 0;
        EnduranceRac := 0;
        StrengthRac := 2;
        AgilityRac := 0;
        ResolveRac := 0;
      end;
      srHalfing:
      begin
        IntelligenceRac := 0;
        CharismaRac := 0;
        EnduranceRac := 0;
        StrengthRac := -2;
        AgilityRac := 1;
        ResolveRac := 1;
      end;
    end;



    { Generate stats from class }
    case Character.CClass of
      cKnight:
      begin
        { Stat Allocation Order is End, Res, Str, Cha, Agi, Int }
        repeat
          StatOrderCondition := False;
          
          Intelligence := RollStartingDice(IntelligenceRac, IntelligenceMod, 3);
          Charisma := RollStartingDice(CharismaRac, CharismaMod, 3);
          Endurance := RollStartingDice(EnduranceRac, EnduranceMod, 3);
          Strength := RollStartingDice(StrengthRac, StrengthMod, 3);
          Agility := RollStartingDice(AgilityRac, AgilityMod, 3);
          Resolve := RollStartingDice(ResolveRac, ResolveMod, 3);

          if (Endurance >= Resolve) and (Resolve >= Strength) and
           (Strength >= Charisma) and (Charisma >= Agility) and
           (Agility >= Intelligence) then
            StatOrderCondition := True;

          TempTotal := Intelligence + Charisma + Endurance + Strength +
            Agility + Resolve;

        until (TempTotal = 84) and (StatOrderCondition = True);
      end;
      cMage:
      begin
        { Stat Allocation Order is Int, Res, Cha, Agi, End, Str }
        repeat
          StatOrderCondition := False;

          Intelligence := RollStartingDice(IntelligenceRac, IntelligenceMod, 3);
          Charisma := RollStartingDice(CharismaRac, CharismaMod, 3);
          Endurance := RollStartingDice(EnduranceRac, EnduranceMod, 3);
          Strength := RollStartingDice(StrengthRac, StrengthMod, 3);
          Agility := RollStartingDice(AgilityRac, AgilityMod, 3);
          Resolve := RollStartingDice(ResolveRac, ResolveMod, 3);

          if (Intelligence >= Resolve) and
             (Resolve >= Charisma) and
             (Charisma >= Agility) and
             (Agility >= Endurance) and
             (Endurance >= Strength) then
          begin
            StatOrderCondition := True;
          end;

          TempTotal := Intelligence + Charisma + Endurance + Strength +
            Agility + Resolve;

        until (TempTotal = 84) and (StatOrderCondition = True);
      end;
      cPriest:
      begin
        { Stat Allocation Order is Res, Int, Cha, Str, End, Agi }
        repeat
          StatOrderCondition := False;

          Intelligence := RollStartingDice(IntelligenceRac, IntelligenceMod, 3);
          Charisma := RollStartingDice(CharismaRac, CharismaMod, 3);
          Endurance := RollStartingDice(EnduranceRac, EnduranceMod, 3);
          Strength := RollStartingDice(StrengthRac, StrengthMod, 3);
          Agility := RollStartingDice(AgilityRac, AgilityMod, 3);
          Resolve := RollStartingDice(ResolveRac, ResolveMod, 3);

          if (Resolve >= Intelligence) and
             (Intelligence >= Charisma) and
             (Charisma >= Strength) and
             (Strength >= Endurance) and
             (Endurance >= Agility) then
          begin
            StatOrderCondition := True;
          end;

          TempTotal := Intelligence + Charisma + Endurance + Strength +
            Agility + Resolve;

        until (TempTotal = 84) and (StatOrderCondition = True);
      end;
      cThief:
      begin
        { Stat Allocation Order is Agi, Res, Int, Str, Cha, End }
        repeat
          StatOrderCondition := False;

          Intelligence := RollStartingDice(IntelligenceRac, IntelligenceMod, 3);
          Charisma := RollStartingDice(CharismaRac, CharismaMod, 3);
          Endurance := RollStartingDice(EnduranceRac, EnduranceMod, 3);
          Strength := RollStartingDice(StrengthRac, StrengthMod, 3);
          Agility := RollStartingDice(AgilityRac, AgilityMod, 3);
          Resolve := RollStartingDice(ResolveRac, ResolveMod, 3);

          if (Agility >= Resolve) and
             (Resolve >= Intelligence) and
             (Intelligence >= Strength) and
             (Strength >= Charisma) and
             (Charisma >= Endurance) then
          begin
            StatOrderCondition := True;
          end;

          TempTotal := Intelligence + Charisma + Endurance + Strength +
            Agility + Resolve;

        until (TempTotal = 84) and (StatOrderCondition = True);
      end;
      cWarrior:
      begin
        { Stat Allocation Order is Str, End, Agi, Res, Int, Cha }
        repeat
          StatOrderCondition := False;

          Intelligence := RollStartingDice(IntelligenceRac, IntelligenceMod, 3);
          Charisma := RollStartingDice(CharismaRac, CharismaMod, 3);
          Endurance := RollStartingDice(EnduranceRac, EnduranceMod, 3);
          Strength := RollStartingDice(StrengthRac, StrengthMod, 3);
          Agility := RollStartingDice(AgilityRac, AgilityMod, 3);
          Resolve := RollStartingDice(ResolveRac, ResolveMod, 3);

          if (Strength >= Endurance) and
             (Endurance >= Agility) and
             (Agility >= Resolve) and
             (Resolve >= Intelligence) and
             (Intelligence >= Charisma) then
          begin
            StatOrderCondition := True;
          end;

          TempTotal := Intelligence + Charisma + Endurance + Strength +
            Agility + Resolve;

        until (TempTotal = 84) and (StatOrderCondition = True);
      end;
    end;

    { Calculate the attributes and abilities }
    HP := 0;
    MP := 0;
    Res := Resolve div 2;
    MP := MP + Intelligence div 3;
    AC := 0;
    Evasion := Agility div 3;
    Speed := Agility div 3;

    case Character.CClass of
      cThief:
      begin
        HP := Endurance div 3 + 6;
        MP := MP + 0;
      end;
      cKnight:
      begin
        HP := Endurance div 3 + 10;
        MP := MP + 1;
      end;
      cMage:
      begin
        HP := Endurance div 3 + 4;
        MP := MP + 4;
      end;
      cPriest:
      begin
        HP := Endurance div 3 + 8;
        MP := MP + 3;
      end;
      cWarrior:
      begin
        HP := Endurance div 3 + 12;
        MP := MP + 0;
      end;
    end;

    if MP < 0 then
      MP := 1;
    if HP < 3 then
      HP := 3;

    { Set the abilities and attributes }
    Character.Intelligence := Intelligence;
    Character.Charisma := Charisma;
    Character.Endurance := Endurance;
    Character.Strength := Strength;
    Character.Agility := Agility;
    Character.Resolve := Resolve;
    Character.MP := MP;
    Character.HP := HP;
    Character.BaseMaxMP := MP;
    Character.BaseMaxHP := HP;
    Character.BaseSpeed := Speed;
    Character.Levels := 1;

    { To begin with, set all skills to zero }
    For Loop := 0 to High(Character.Skills) do
      Character.Skills[Loop] := 0;

    { Work out Race Modifiers to Skills }
    case Character.SubRace of
      srHuman:
      begin
        Character.Skills[SK_DEFENSE] := 1;
        Character.Skills[SK_SUBTERFUGE] := 1;
      end;
      srElf:
      begin
        Character.Skills[SK_RANGED] := 1;
        Character.Skills[SK_MAGIC] := 1;
      end;
      srHalfing:
      begin
        Character.Skills[SK_RANGED] := 1;
        Character.Skills[SK_SUBTERFUGE] := 1;
      end;
      srOrc:
      begin
        Character.Skills[SK_FIGHTING] := 1;
        Character.Skills[SK_MELEE] := 1;
      end;
      srDwarf:
      begin
        Character.Skills[SK_FIGHTING] := 1;
        Character.Skills[SK_HEAVY] := 1;
      end;
    end;

    { Work out Class Modifers to skills }
    case Character.CClass of
      cThief:
      begin
        if Character.AgilityMod > 0 then
          Inc(Character.Skills[SK_SUBTERFUGE], Character.AgilityMod div 4);
        if Character.StrengthMod > 0 then
          Inc(Character.Skills[SK_FIGHTING], Character.StrengthMod div 4);

        Inc(Character.Skills[SK_SUBTERFUGE], 2);
        Inc(Character.Skills[SK_DEFENSE], 1);

        Inc(Character.Skills[SK_RANGED], 1);
        Inc(Character.Skills[SK_LIGHT], 1);
        Inc(Character.Skills[SK_STEALTH], 1);
        Inc(Character.Skills[SK_THIEVERY], 2);
      end;
      cKnight:
      begin
        if Character.CharismaMod > 0 then
          Inc(Character.Skills[SK_MAGIC], Character.CharismaMod div 4);
        if Character.StrengthMod > 0 then
          Inc(Character.Skills[SK_FIGHTING], Character.StrengthMod div 4);

        Inc(Character.Skills[SK_FIGHTING], 2);
        Inc(Character.Skills[SK_DEFENSE], 1);

        Inc(Character.Skills[SK_MELEE], 1);
        Inc(Character.Skills[SK_HEAVY], 1);
        Inc(Character.Skills[SK_MEDIUM], 1);
        Inc(Character.Skills[SK_COMBAT], 1);
        Inc(Character.Skills[SK_PROTECTION], 1);
      end;
      cMage:
      begin
        if Character.IntelligenceMod > 0 then
          Inc(Character.Skills[SK_MAGIC], Character.IntelligenceMod div 4);
        if Character.AgilityMod > 0 then
          Inc(Character.Skills[SK_SUBTERFUGE], Character.AgilityMod div 4);

        Inc(Character.Skills[SK_MAGIC], 2);
        Inc(Character.Skills[SK_DEFENSE], 1);

        Inc(Character.Skills[SK_FIRE], 1);
        Inc(Character.Skills[SK_EARTH], 1);
        Inc(Character.Skills[SK_AIR], 1);
        Inc(Character.Skills[SK_WATER], 1);
        Inc(Character.Skills[SK_LORE], 1);
      end;
      cPriest:
      begin
        if Character.ResolveMod > 0 then
          Inc(Character.Skills[SK_MAGIC],Character.ResolveMod div 4);
        if Character.StrengthMod > 0 then
          Inc(Character.Skills[SK_FIGHTING], Character.StrengthMod div 4);

        Inc(Character.Skills[SK_MAGIC], 2);
        Inc(Character.Skills[SK_FIGHTING], 1);

        Inc(Character.Skills[SK_COMBAT], 1);
        Inc(Character.Skills[SK_MEDIUM], 1);
        Inc(Character.Skills[SK_HEALING], 2);
        Inc(Character.Skills[SK_PROTECTION], 1);
      end;
      cWarrior:
      begin
        if Character.StrengthMod > 0 then
          Inc(Character.Skills[SK_FIGHTING],Character.StrengthMod div 4);
        if Character.AgilityMod > 0 then
          Inc(Character.Skills[SK_DEFENSE],Character.AgilityMod div 4);

        Inc(Character.Skills[SK_FIGHTING], 2);
        Inc(Character.Skills[SK_DEFENSE], 1);

        Inc(Character.Skills[SK_RANGED], 1);
        Inc(Character.Skills[SK_MELEE], 2);
        Inc(Character.Skills[SK_MEDIUM], 1);
        Inc(Character.Skills[SK_HEAVY], 1);
      end;
    end;

    { Add extra points }
    case Character.CClass of
      cKnight:
      begin
        Inc(Character.Skills[SK_FIGHTING], 1);
        Inc(Character.Skills[SK_MAGIC], 1);

        Inc(Character.Skills[SK_MELEE], 1);
        Inc(Character.Skills[SK_HEAVY], 1);
        Inc(Character.Skills[SK_PROTECTION], 1);
        Inc(Character.Skills[SK_COMBAT], 1);
      end;
      cMage:
      begin
        Inc(Character.Skills[SK_MAGIC], 2);

        Inc(Character.Skills[SK_FIRE], 1);
        Inc(Character.Skills[SK_EARTH], 1);
        Inc(Character.Skills[SK_AIR], 1);
        Inc(Character.Skills[SK_WATER], 1);
      end;
      cPriest:
      begin
        Inc(Character.Skills[SK_FIGHTING], 1);
        Inc(Character.Skills[SK_MAGIC], 1);

        Inc(Character.Skills[SK_HEALING], 2);
        Inc(Character.Skills[SK_PROTECTION], 1);
        Inc(Character.Skills[SK_MELEE], 1);
      end;
      cThief:
      begin
        Inc(Character.Skills[SK_DEFENSE], 1);
        Inc(Character.Skills[SK_SUBTERFUGE], 1);

        Inc(Character.Skills[SK_LIGHT], 1);
        Inc(Character.Skills[SK_STEALTH], 1);
        Inc(Character.Skills[SK_THIEVERY], 1);
        Inc(Character.Skills[SK_RANGED], 1);
      end;
      cWarrior:
      begin
        Inc(Character.Skills[SK_FIGHTING], 2);

        Inc(Character.Skills[SK_MELEE], 2);
        Inc(Character.Skills[SK_HEAVY], 2);
      end;
    end;

    { And save the player file }
    Character.Save(ExtractFilePath(Application.ExeName) + 'save\');

    { Return the character name }
    Result := Character.Name;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


end.
