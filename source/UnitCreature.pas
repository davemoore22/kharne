{ UnitCreature

  Copyright (c) 2007-2009 Dave Moore 

  Player Class and Player Functions

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

unit UnitCreature;

interface

uses Math, Contnrs, Classes, SysUtils, Graphics, HotLog, UnitDefines, UnitDice, 
	UnitItem, UnitInterfaces, UnitConst, UnitMonster, UnitDungeon;

{ TCreature is the Player }
type TCreature = class(TInterfacedObject, ICommonObject)

protected
	{ Protected members }
  function GetCurrentIntelligenceMod: Integer;
  function GetCurrentCharismaMod: Integer;
  function GetCurrentEnduranceMod: Integer;
  function GetCurrentStrengthMod: Integer;
  function GetCurrentAgilityMod: Integer;
  function GetCurrentResolveMod: Integer;
  function GetCurrentEV: Integer;
  function GetCurrentBurden: Integer;
  function GetCurrentAC: Integer;
  function GetCurrentStrength: Integer;
  function GetCurrentAgility: Integer;
  function GetCurrentEndurance: Integer;
  function GetCurrentIntelligence: Integer;
  function GetCurrentResolve: Integer;
  function GetCurrentCharisma: Integer;
  function GetCurrentSpeed: Integer;
  function GetCurrentResistance: Integer;
  function GetcurrentMaxHP: Integer;
  function GetCurrentMaxMP: Integer;
  function GetCurrentHP: Integer;
  function GetCurrentMP: Integer;
  function GetCurrentAccuracy: Integer;
  function GetCurrentDamageBonus: Integer;
  function GetCurrentBlocking: Integer;
  function GetCurrentDeflection: Integer;
  function GetCurrentResistanceToFire: Integer;
  function GetCurrentResistanceToAir: Integer;
  function GetCurrentResistanceToEarth: Integer;
  function GetCurrentResistanceToWater: Integer;
  function GetCurrentResistanceToPoison: Integer;
  function GetCurrentResistanceToLifeDrain: Integer;
  function GetFoodStatus: String;
  function GetFoodColour: TColor;
  function GetPoisonStatus: String;
  function GetPoisonColour: TColor;
  function GetAlertness: Integer;
  function SkillProgress(Skill: Integer): Integer;
  function GetSkillLimit(Skill: Integer): Integer;
  function IsStarving: Boolean; 
  procedure SetBackground(Value: String);
  function GetBackground: String;
  function GetMaxBurden: Integer;
  function GetNextLevelXP: Integer;
  function GetNotes: String;
  function GetFriendlyClassName: String;

private
	{ Private Members }
  FName: String;
  FGender: crGender;
  FBackground: TStringList;
  FSubRace: CrSubRace;
  FRace: crRace;
  FClass: crClass;
  FSize: crSize;
  FLevel: Integer;
  FHP: Integer;
  FMaxHP: Integer;
  FMP: Integer;
  FMaxMP: Integer;
  FSpeed: Integer;
  FResistance: Integer;
  FArmourClass: Integer;
  FEvasion: Integer;
  FFood: Integer;
  FAlertness: Integer;
  FIcon: Integer;
  FIntelligence: Integer;
  FCharisma: Integer;
  FEndurance: Integer;
  FStrength: Integer;
  FAgility: Integer;
  FResolve: Integer;
 	FGold: Integer;
  FCurrentXP: Integer;
  FSelectedSpellSchool: Integer;
  FSelectedSpell: Integer;
  FRegeneration: Integer;
  FNotes: TStringList;
  FPoison: Integer;
public
	{ Public Members }
  Skills: Array [0..40] of Integer;
  SkillsProgress: Array [0..40] of Integer;
  Magic: Array [0..15, 0..8] of Boolean;
  Inventory: Array [0..40] of Integer;
  InventoryCount: Array [0..40] of Integer;
  Hotbar: Array [0..7] of Integer;
  TempAlertness: Integer;
  KilledBy: String;
  Dead: Boolean;
  Status: Array [0..40] of Integer;

	{ Constructors }
  constructor Create;
  
  { Destructors }
  destructor Destroy; override;

	{ Accessors }
  property Name: String read FName write FName;
  property Gender: crGender read FGender write FGender;
  property Race: crRace read FRace write FRace;
  property SubRace: crSubRace read FSubRace write FSubrace;
  property CClass: crClass read FClass write FClass;
  property FriendlyClassName: String read GetFriendlyClassName;
  property Size: crSize read FSize write FSize;
  property Levels: Integer read FLevel write FLevel;
  property NextLevel: Integer read GetNextLevelXP;
  property Background: String read GetBackground write SetBackground;
  property Icon: Integer read FIcon write FIcon;
  property EV: Integer read GetCurrentEV;
  property Burden: Integer read GetCurrentBurden;
  property MaxBurden: Integer read GetMaxBurden;
  property AC: Integer read GetCurrentAC;
  property Speed: Integer read GetCurrentSpeed;
  property Resistance: Integer read GetCurrentResistance;
  property Food: Integer read FFood write FFood;
  property FoodStatus: String read GetFoodStatus;
  property FoodColour: TColor read GetFoodColour;
  property PoisonStatus: String read GetPoisonStatus;
  property PoisonColour: TColor read GetPoisonColour;
  property Starving: Boolean read IsStarving;
  property Alertness: Integer read GetAlertness;
  property BaseEV: Integer read FEvasion write FEvasion;
  property BaseAC: Integer read FArmourClass write FArmourClass;
  property BaseResistance: Integer read FResistance write FResistance;
  property BaseSpeed: Integer read FSpeed write FSpeed;
  property HP: Integer read GetCurrentHP write FHP;
  property MP: Integer read GetCurrentMP write FMP;
  property BaseMaxHP: Integer read FMaxHP write FMaxHP;
  property BaseMaxMP: Integer read FMaxMP write FMaxMP;
  property MaxHP: Integer read GetCurrentMaxHP;
  property MaxMP: Integer read GetCurrentMaxMP;
  property Accuracy: Integer read GetCurrentAccuracy;
  property DamageBonus: Integer read GetCurrentDamageBonus;
  property Blocking: Integer read GetCurrentBlocking;
  property Deflection: Integer read GetCurrentDeflection;
  property FireResistance: Integer read GetCurrentResistanceToFire;
  property AirResistance: Integer read GetCurrentResistanceToAir;
  property WaterResistance: Integer read GetCurrentResistanceToWater;
  property EarthResistance: Integer read GetCurrentResistanceToEarth;
  property PoisonResistance: Integer read GetCurrentResistanceToPoison;
  property LifeDrainingResistance: Integer read 
  	GetCurrentResistanceToLifeDrain;
  property Intelligence: Integer read GetCurrentIntelligence 
  	write FIntelligence;
  property Charisma: Integer read GetCurrentCharisma write FCharisma;
  property Endurance: Integer read GetCurrentEndurance write FEndurance;
  property Strength: Integer read GetCurrentStrength write FStrength;
  property Agility: Integer read GetCurrentAgility write FAgility;
  property Resolve: Integer read GetcurrentResolve write FResolve;
  property IntelligenceMod: Integer read GetCurrentIntelligenceMod;
  property CharismaMod: Integer read GetCurrentCharismaMod;
  property EnduranceMod: Integer read GetCurrentEnduranceMod;
  property StrengthMod: Integer read GetCurrentStrengthMod;
  property AgilityMod: Integer read GetCurrentAgilityMod;
  property ResolveMod: Integer read GetCurrentResolveMod;
  property Gold: Integer read FGold write FGold;
  property XP: Integer read FCurrentXP write FCurrentXP;
  property SelectedSpellSchool: Integer  read FSelectedSpellSchool 
  	write FSelectedSpellSchool;
  property SelectedSpell: Integer read FSelectedSpell write FSelectedSpell;
  property Regeneration: Integer read FRegeneration write FRegeneration;
  property Poison: Integer read FPoison write FPoison;
  property Notes: String read GetNotes;


	{ Public Functions }
	
	{ Get the graphic for the player }
  function ReturnPlayerIconIndex(PRace: crSubRace; PClass: crClass): Integer;

	{ Get the cost of a spell }
  function GetSpellCost(SpellCastingSkill: Integer; SchoolSkill: Integer;
    SpellLevel: Integer; Intelligence: Integer): Integer;

	{ Get the success chance of casting a spell }
  function GetSpellProbability(SpellCastingSkill: Integer; SchoolSkill: 
  	Integer; SpellLevel: Integer; Intelligence: Integer): Integer;

	{ Learn the specified skill }
  procedure LearnSkill(SkillToLearn: Integer; SkillJump: Integer = 1);

	{ Save the character to a file }
  procedure Save(Path: String);

	{ Load the character from a file }  
  procedure Load(CharFile: String);

	{ Get the value of an enchantment from items carried by the Player }
  function GetEnchantmentValue(Enchantment: String): Integer;
  
  { Get bonus damage the player has against a creature type }
  function GetBonusDamage(MonsterRace: Integer): Integer;

	{ Get the progress of a skill to the next skill level as a string }
  function SkillProgressString(Skill: Integer): String;

  { Add an Item to the first available slot in the backpack }
  function AddItem(ItemIndex: Integer): Integer;

  { Get the first free slot for an item }
  function ReturnFirstAvailableSlot(Item: TItem): Integer;

	{ Get the number of slots without any items in them }
  function GetNumberUnusedSlots: Integer;

  { Take Damage }
  function TakeDamage(Monster: TMonster; DamageTaken: Integer): Boolean;

	{ Feed }
  procedure Feed(FoodValue: Integer);

	{ Gain XP }
  procedure GainXP(XPToGain: Integer);

	{ Gain a level }  
  procedure GainALevel;
  
  { Work out what armours are being worn of a certain type }
  function ReturnNumberOfArmourTypeWorn(ArmourType: crMaterial): Integer;
  
  { Work out the overall ratio of armour types worn in various slots }
  procedure ReturnArmourRatio(var Heavy: Integer; var Medium: Integer; 
  	var Light: Integer);
  	
  { Learn armour skills }
  procedure LearnSkillArmour;

	{ Take a note }
  procedure TakeNote(Turns: Integer; Note: String; NoteType: tCharacterNote; 
  	Dungeon: TDungeonLevel);

	{ Generate starting equipment }
  procedure GenerateAndEquipStartingEquipment;

	{ See if the player can cast spells }
  function HasMagic: Boolean;

  { Return the letters of the spell schools the character has access to to }
  function GetSpellSchools: String;

  { Get a numberical value of how powerful the magical items that the character
    has equipped }
  function GetMagicalLoad: Integer;

  { Test for a Status Condition }
  function Has(Condition: Integer): Boolean;

	{ Interface Method for Persistance }
  function GetStringValue: String;
end;

implementation

uses Dialogs, Controls, UnitEngine, UnitWizard, UnitFunctions, UnitDisplay,
  UnitVars;

{ TCreature }

{ Standard Constructor }
constructor TCreature.Create;
var
  Loop: Integer;
begin
  inherited Create;
  
  { Logging }
	hLog.Add('{now} {lNum} TCreature.Create()');
	
  try
  	{ Set the basic class properties }
		FName := '';
		FGender := gNone;
		FSubRace := srNone;
		FRace := rNone;
		FClass := cNone;
		FSize := sizNone;
		FLevel := 0;
		FHP := 0;
		FMP := 0;
		FSelectedSpellSchool := -1;
		FSelectedSpell := -1;
		FRegeneration := 100;
    FPoison := 0;
		FBackground := TStringList.Create;
		FNotes := TStringList.Create;

    for Loop := Low(Status) to High(Status) do
      Status[Loop] := 0;

		{ Add a note }
		FNotes.Add(Format('%-7.7s %-25.25s %s', ['Turn', 'Place', 'Note']));
		FNotes.Add('----------------------------------------------------' +
			'----------');
		
		{ We're not dead }
		Dead := False;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Destructor }
destructor TCreature.Destroy;
begin
  inherited Destroy;
  
  { Logging }
	hLog.Add('{now} {lNum} TCreature.Destroy()');
	
  try
  	{ Free any created stringlists }
		FNotes.Free;
		FNotes := nil;
		FBackground.Free;
		FBackground := nil;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Item ArcheType as a String }
function TCreature.GetStringValue: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetStringValue()');
  
  Result := 'Creature';
end;

{ Get the maximum weight that the character can carry without it affecting
  his/her speed }
function TCreature.GetMaxBurden: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetMaxBurden()');
  
  Result := FStrength * 10;
end;

{ Get the character's agility modifier }
function TCreature.GetCurrentAgilityMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentAgilityMod()');

  Result := FAgility - 10;
end;

{ Get the character's charisma modifier }
function TCreature.GetCurrentCharismaMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentCharismaMod()');
  
  Result := FCharisma - 10;
end;

{ Get the character's endurance modifier }
function TCreature.GetCurrentEnduranceMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentEnduranceMod()');
 
  Result := FEndurance - 10;
end;

{ Get the character's intelligence modifier }
function TCreature.GetCurrentIntelligenceMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentIntelligenceMod()');
 
  Result := FIntelligence - 10;
end;

{ Get the character's strength modifier }
function TCreature.GetCurrentStrengthMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentStrengthMod()');
 
  Result := FStrength - 10;
end;

{ Get the character's resolve modifier }
function TCreature.GetCurrentResolveMod: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResolveMod()');

  Result := FResolve - 10;
end;

{ Get the character's background }
function TCreature.GetBackground: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetBackground()');
 
  Result := FBackground.Text;
end;

{ Set the character's background }
procedure TCreature.SetBackground(Value: String);
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.SetBackground()');
  
  FBackground.Text := Value;
end;

{ Get the relevant icon for the player }
function TCreature.ReturnPlayerIconIndex(PRace: crSubRace;
  PClass: crClass): Integer;
var
  ClassMult: Integer;
  RaceMult: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.ReturnPlayerIconIndex()');

	{ Default Result }
	Result := 0;
	
	try
		{ Get Race }
		case PRace of
			srHuman: RaceMult := HUMAN;
			srDwarf: RaceMult := DWARF;
			srElf: RaceMult := ELF;
			srHalfing: RaceMult := HALFING;
			srOrc: RaceMult := ORC;
		end;
		
		{ Get Class }
		case PClass of
			cKnight: ClassMult := KNIGHT;
			cMage: ClassMult := MAGE;
			cPriest: ClassMult := PRIEST;
			cThief: ClassMult := THIEF;
			cWarrior: ClassMult := WARRIOR;
		end;
		
		{ Return item index }
		Result := (RaceMult * 8) + ClassMult;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;   
end;

{ Load the character from a file }
procedure TCreature.Load(CharFile: String);
var
  InnerLoop: Integer;
  Loop: Integer;
  PFile: TextFile;
  Temp: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.Load()');

	try
		{ Assign the File }
		AssignFile(PFile, CharFile);
		
		{ Start reading }
		Reset(PFile);

		try
			{ TODO: need to check for dead or alive }
			ReadLn(PFile, Temp);
			
			{ Get the name }
			ReadLn(PFile, FName);
			
			{ Read gender }
			ReadLn(PFile, Temp);
			case (StrToInt(Temp)) of
				0:  FGender := gNone;
				1:  FGender := gMale;
				2:  FGender := gFemale;
				3:  FGender := gOther;
			end;
			
			{ Read background }
			ReadLn(PFile, Temp);
			FBackground.DelimitedText := Temp;

			{ Read race }
			ReadLn(PFile, Temp);
			case (StrToInt(Temp)) of
				0: FSubRace := srNone;
				1: FSubRace := srHuman;
				2: FSubRace := srHalfing;
				3: FSubRace := srOrc;
				4: FSubRace := srDwarf;
				5: FSubRace := srElf;
			end;

			{ Read race type }
			ReadLn(PFile, Temp);
			case (StrToInt(Temp)) of
				0: FRace := rNone;
				1: FRace := rAnimal;
				2: FRace := rHumanoid;
				3: FRace := rConstruct;
				4: FRace := rDemon;
				5: FRace := rDragon;
				6: FRace := rElemental;
				7: FRace := rPlant;
				8: FRace := rUndead;
			end;

			{ Read class }
			ReadLn(PFile, Temp);
			case (StrToInt(Temp)) of
				0: FClass := cNone;
				1: FClass := cThief;
				2: FClass := cKnight;
				3: FClass := cMage;
				4: FClass := cPriest;
				5: FClass := cWarrior;
			end;

			{ Read size }
			ReadLn(PFile, Temp);
			case (StrToInt(Temp)) of
				0: FSize := sizNone;
				1: FSize := sizFine;
				2: FSize := sizDiminutive;
				3: FSize := sizTiny;
				4: FSize := sizSmall;
				5: FSize := sizMedium;
				6: FSize := sizLarge;
				7: FSize := sizHuge;
				8: FSize := sizGargantuan;
				9: FSize := sizColossal;
			end;

			{ Read other attributes and abilities }
			ReadLn(PFile, FLevel);
			ReadLn(PFile, FGold);
			ReadLn(PFile, FCurrentXP);
			ReadLn(PFile, FHP);
			ReadLn(PFile, FMP);
			ReadLn(PFile, FMaxHP);
			ReadLn(PFile, FMaxMP);
			ReadLn(PFile, FSpeed);
			ReadLn(PFile, FResistance);
			ReadLn(PFile, FArmourClass);
			ReadLn(PFile, FEvasion);
			ReadLn(PFile, FIcon);
			ReadLn(PFile, FIntelligence);
			ReadLn(PFile, FCharisma);
			ReadLn(PFile, FEndurance);
			ReadLn(PFile, FStrength);
			ReadLn(PFile, FAgility);
			ReadLn(PFile, FResolve);
			
			{ Get skills }
			for Loop := Low(Skills) to High(Skills) do
				ReadLn(PFile, Skills[Loop]);

			{ Get magic }
			for Loop := Low(Magic) to High(Magic) do
			begin
				for InnerLoop := 0 to 8 do
				begin
					ReadLn(PFile, Temp);
					if StrToInt(Temp) = 1 then
						Magic[Loop, InnerLoop] := True
					else
						Magic[Loop, InnerLoop] := False;
				end;
			end;

      { Get status }
			for Loop := Low(Status) to High(Status) do
				ReadLn(PFile, Status[Loop]);
			
			{ Get inventory }
			for Loop := Low(Inventory) to High(Inventory) do
			begin
				ReadLn(PFile, Inventory[Loop]);
				ReadLn(PFile, InventoryCount[Loop]);
			end;

      { Load Poison }
      ReadLn(PFile, FPoison);
			
			{ Get hotbar }
			for Loop := Low(HotBar) to High(HotBar) do
				ReadLn(PFile, HotBar[Loop]);

		finally
			{ And finally close the file }		
			CloseFile(PFile);
		end;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end; 
end;

{ Save the character to a file }
procedure TCreature.Save(Path: String);
var
  InnerLoop: Integer;
  Loop: Integer;
  PFile: TextFile;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.Save()');

	try
		{ Assign the File }
		AssignFile(PFile, Path + FName + '.CHR');
		
		{ Start writing }
		Rewrite(PFile);

		try
			{ Handle dead or alive }
			if FHP > 0 then 
				WriteLn(PFile, 'Alive')
			else 
				WriteLn(PFile, 'Dead');
				
			{ Write various properties of the character } 
			WriteLn(PFile, FName);
			WriteLn(PFile, IntToStr(Ord(FGender)));
			WriteLn(PFile, FBackground.DelimitedText);
			WriteLn(PFile, IntToStr(Ord(FSubRace)));
			WriteLn(PFile, IntToStr(Ord(FRace)));
			WriteLn(PFile, IntToStr(Ord(FClass)));
			WriteLn(PFile, IntToStr(Ord(FSize)));
			WriteLn(PFile, IntToStr(FLevel));
			WriteLn(PFile, IntToStr(FGold));
			WriteLn(PFile, IntToStr(FCurrentXP));
			WriteLn(PFile, IntToStr(FHP));
			WriteLn(PFile, IntToStr(FMP));
			WriteLn(PFile, IntToStr(FMaxHP));
			WriteLn(PFile, IntToStr(FMaxMP));
			WriteLn(PFile, IntToStr(FSpeed));
			WriteLn(PFile, IntToStr(FResistance));
			WriteLn(PFile, IntToStr(FArmourClass));
			WriteLn(PFile, IntToStr(FEvasion));
			WriteLn(PFile, IntToStr(FIcon));
			WriteLn(PFile, IntToStr(FIntelligence));
			WriteLn(PFile, IntToStr(FCharisma));
			WriteLn(PFile, IntToStr(FEndurance));
			WriteLn(PFile, IntToStr(FStrength));
			WriteLn(PFile, IntToStr(FAgility));
			WriteLn(PFile, IntToStr(FResolve));
			
			{ Write skills }
			for Loop := Low(Skills) to High(Skills) do
				WriteLn(PFile, IntToStr(Skills[Loop]));
				
			{ Write magic }
			for Loop := Low(Magic) to High(Magic) do
				for InnerLoop := 0 to 8 do
				begin
					if (Magic[Loop, InnerLoop] = False) then WriteLn(PFile,'0');
					if (Magic[Loop, InnerLoop] = True) then WriteLn(PFile,'1');
				end;

      { Write status }
			for Loop := Low(Status) to High(Status) do
        WriteLn(PFile, IntToStr(Status[Loop]));
				
			{ Write inventory }
			for Loop := Low(Inventory) to High(Inventory) do
			begin
				WriteLn(PFile, IntToStr(Inventory[Loop]));
				WriteLn(PFile, IntToStr(InventoryCount[Loop]));
			end;

      { Write Poison }
      WriteLn(PFile, IntToStr(FPoison));
			
			{ Write hotbar }
			for Loop := Low(HotBar) to High(HotBar) do
				WriteLn(PFile, HotBar[Loop]);
				
		finally
			{ And finally close the file }
			CloseFile(PFile);
		end; 
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Add an Item to the First available slot in the backpack }
function TCreature.AddItem(ItemIndex: Integer): Integer;
var
  ItemSlot: Integer;
  LocalItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.AddItem()');

	{ Default Result }
	Result := 0;

	try
		{ Get the item }
		LocalItem := (GItemList[ItemIndex] as TItem);
		
		{ Find the first available slot (or if the item stacks return the 
		  appropriate slot }
		ItemSlot := ReturnFirstAvailableSlot(LocalItem);
		
		{ If we have a slot }
		if ItemSlot > -1 then
		begin
			{ If we are stacking an item }
			if InventoryCount[ItemSlot] > 0 then
			begin
				{ Increase the count of the inventory slots by the item count }
				Inc(InventoryCount[ItemSlot], LocalItem.Count);
				(GItemList[Inventory[ItemSlot]] as TItem).Count := 
					(GItemList[Inventory[ItemSlot]] as TItem).Count + LocalItem.Count;
			end
			else
			{ If we are not stacking an item }
			begin
				Inventory[ItemSlot] := ItemIndex;
				Inc(InventoryCount[ItemSlot]);
			end;
			
			{ Set the location of the item }
			LocalItem.Location := iInventory;
		end;
		
		{ Return the slot added to }
		Result := ItemSlot;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the first free slot for an item }
function TCreature.ReturnFirstAvailableSlot(Item: TItem): Integer;
var
  ID: String;
  ItemFound: Boolean;
  ItemInSlot: Integer;
  LocalItem: TItem;
  Slot: Integer;
  Stackable: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.ReturnFirstAvailableSlot()');

	{ Default Result }
	Result := -1;

	try
		{ The behaviour for this routine is different depending on whether or not 
		  the item we're working with is stackable.

			If it isn't, then we look for the first free inventory slot and return 
			it.
			
			If it is, then we look first for any existing items of this type, and 
			if we find some, return that slot, else we look for the first free slot
			as above }
		ID := Item.ID;
		Stackable := Item.Stackable;
		
		{ Start looking through each inventory slot }
		Slot := S_INVENTORY_BEGIN;
		
		{ We haven't yet found the item yet }
		ItemFound := False;

		{ Check for stackable items first }
		if Stackable then
		begin
			repeat
				{ Keep going until we've found an extant version of this item in the
				  current inventory that has the same identification status }
				ItemInSlot := Inventory[Slot];
				if ItemInSlot > 0 then
				begin
					LocalItem := (GItemList[ItemInSlot] as TItem);
					{ TODO: We may need to check for and handle PseudoID here as well }
					if (LocalItem.ID = Item.ID) and (LocalItem.Known = Item.Known) then
						ItemFound := True;
				end;
				if not(ItemFound) then 
					Inc(Slot);
			until (Slot > S_INVENTORY_END) or (ItemFound = True);
		end;
		
		{ If we've not found an item, or the item is not stackable, look for the
			first free slot }
		if ItemFound = False then
		begin
			Slot := S_INVENTORY_BEGIN;
			repeat
				if (Inventory[Slot] = 0) then
					ItemFound := True
				else
					inc(Slot);
			until (Slot > S_INVENTORY_END) or (ItemFound = True);
		end;

		{ If we get here then we have no free slots }
		if ItemFound = True then
			{ Return a slot }
			Result := Slot;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the current evasion }
function TCreature.GetCurrentEV: Integer;
var
  ArmourCapacity: Real;
  BaseEvasion: Real;
  CalculatedEvasion: Real;
  LocalArmourCapacity: Real;
  LocalItem: TItem;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentEV()');

	{ Default Result }
	Result := 0;

	try
		{ Work out Base Evasion from current Dexterity and DEFENSE Skill - this is
		  a logarithmic function }
		BaseEvasion := (EVASIONCAP - ((EVASIONCAP - FAgility + 1) *
			(EVASIONCAP - FAgility + 1) / (EVASIONCAP)) + 2.04) * (25/27);
			
		{ Add the defense skill value }
		BaseEvasion := BaseEvasion + Skills[SK_DEFENSE];
		
		{ Cap the evasion }
		if BaseEvasion > EVASIONCAP then 
			BaseEvasion := EVASIONCAP;

		{ Reset the armour capacity }
		ArmourCapacity := 0;
		
		{ Now we work out armour capacity from armor worn and armor skill }
		for Loop := S_FEET to S_BACK do
		begin
			{ For each armour item worn... }
			if Inventory[Loop] > 0 then
			begin
				LocalItem := GItemList[Inventory[Loop]] as TItem;
				if (LocalItem.ItemType = iArmour) then
				begin
					case LocalItem.ItemMaterial of
						mMetal:
						begin
							{ Heavy metal armour is harder to wear than lighter items }
							if LocalItem.ItemWeight > FStrength then
								LocalArmourCapacity := 4
							else
								LocalArmourCapacity := 2.5;
								
							{ Skills reduce how much armour affects your evasion } 
							LocalArmourCapacity := LocalArmourCapacity -
								(LocalArmourCapacity * (Skills[SK_HEAVY] / SKILLCAP));
						end;
						mLeather:
						begin
							{ Medium armour is easier to wear than heavy armour }
							LocalArmourCapacity := 1.25;
							LocalArmourCapacity := LocalArmourCapacity -
								(LocalArmourCapacity * (Skills[SK_MEDIUM] / SKILLCAP));
						end;
						mCloth:
							begin
							{ Cloth armour is the easiest at all to wear }
								LocalArmourCapacity := 0.25;
								LocalArmourCapacity := LocalArmourCapacity -
									(LocalArmourCapacity * (Skills[SK_LIGHT] / SKILLCAP));
							end;
					end;
					ArmourCapacity := ArmourCapacity + LocalArmourCapacity;
				end;
			end;
		end;

		{ Scale Evasion according to Armour Capacity }
		CalculatedEvasion := ((ARMOURCAPACITYCAP - (ArmourCapacity * 
			(ArmourCapacity / ARMOURCAPACITYCAP))) * (1 / ARMOURCAPACITYCAP));
			
		{ Armour affects evasion considerably }
		if CalculatedEvasion < 0.5 then 
			CalculatedEvasion := 0.5;
			
		{ Set the base evasion }
		CalculatedEvasion := CalculatedEvasion * BaseEvasion;

		{ Add any bonuses from magical armour }
		CalculatedEvasion := CalculatedEvasion + 
			(GetEnchantmentValue('ENCH_EVASION') div 5);

		{ Rescale calculated evasion down }
		Result := Trunc(CalculatedEvasion / 1.5);
		
		{ Make sure we don't return a nonsensical value }
		if Result < 0 then 
			Result := 0;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the weight of items the character is carrying and wearing }
function TCreature.GetCurrentBurden: Integer;
var
  LocalItem: TItem;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentBurden()');

	{ Default Result }
	Result := 0;

	try
		{ Check each inventory slot }
		for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
		begin
			if (Inventory[Loop] > 0) then
			begin
				{ Handle multiple items }
				LocalItem := GItemList[Inventory[Loop]] as TItem;
				Result := Result + (LocalItem.ItemWeight * InventoryCount[Loop]);
			end;
		end;
		
		{ Check each equipped slot }
		for Loop := S_FEET to S_BACK do
		begin
			if (Inventory[Loop] > 0) then
			begin
				{ Handle multiple items }
				LocalItem := GItemList[Inventory[Loop]] as TItem;
				Result := Result + (LocalItem.ItemWeight * InventoryCount[Loop]);
			end;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;    
end;

{ Get current AC }
function TCreature.GetCurrentAC: Integer;
var
  ACTemp: Integer;
  LocalItem: TItem;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentAC()');

	{ Default Result }
	Result := 0;

	try
		{ Check each equipped slot }
		for Loop := S_FEET to S_BACK do
		begin
			{ If there is an item in the slot }
			if Inventory[Loop] > 0 then
			begin
				{ Get the item }
				LocalItem := GItemList[Inventory[Loop]] as TItem;
				
				{ Get the armour rating and add it to the runnnig total }
				ACTemp := LocalItem.Armour;
				Result := Result + ACTemp;
			end;
		end;
		
		{ We can't have negative armour }
		if Result < 0 then 
			Result := 0;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current agility }
function TCreature.GetCurrentAgility: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentAgility()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FAgility;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_AGI');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current charisma }
function TCreature.GetCurrentCharisma: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentCharisma()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FCharisma;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_CHA');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current endurance } 
function TCreature.GetCurrentEndurance: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentEndurance()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FEndurance;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_EMD');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current intelligence }
function TCreature.GetCurrentIntelligence: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentIntelligence()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FIntelligence;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_INT');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current resolve }
function TCreature.GetCurrentResolve: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResolve()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FResolve;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_RES');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current strength }
function TCreature.GetCurrentStrength: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentStrength()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FStrength;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_STR');

    { Check for drained status }
    if Has(DRAINED) then
      Result := Result div 2;
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance }
function TCreature.GetCurrentResistance: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentEV()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base attribute }
		Result := FResistance;
		
		{ Add any items that affect the attribute }
		Result := Result + GetEnchantmentValue('ENCH_RESIST');
		
		{ Minimum attribute is 1 }
		if Result < 0 then 
			Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current speed }
function TCreature.GetCurrentSpeed: Integer;
var
  CurrentTotal: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentSpeed()');

	{ Default Result }
	Result := 0;

	try
		{ Get the base speed }
  	CurrentTotal := FSpeed;
  	
  	{ Adjust the speed for encumbrence }
		if GetCurrentBurden > GetMaxBurden * 2 then
			CurrentTotal := CurrentTotal div 3
		else if GetCurrentBurden > GetMaxBurden then
			CurrentTotal := (CurrentTotal div 3) * 2;

		{ Add any equipped items that affect speed }
  	CurrentTotal := CurrentTotal + (GetEnchantmentValue('ENCH_HASTE') div 2);

		{ We can't have negative haste }
  	if CurrentTotal < 0 then
  		CurrentTotal := 1;

    { If we're stealthed, reduce the speed dramatically }
    if Has(STEALTHED) then
      CurrentTotal := 1;

    { If hasted, triple the speed }
    if Has(HASTED) then
      CurrentTotal := CurrentTotal * 3;

  Result := CurrentTotal;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current maximum hitpoints }
function TCreature.GetCurrentMaxHP: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentMaxHP()');

	{ Default Result }
	Result := 1;

	try
		{ Start off with the base HP }
  	Result := FMaxHP;
  	
  	{ Add any items that affect health }
  	Result := Result + GetEnchantmentValue('ENCH_HP');

    { TODO: check for death when taking off a HP enchant item at low health }

		{ TODO: handle negative health }
  	if Result < 0 then 
  		Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magicpoints }
function TCreature.GetCurrentMaxMP: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentMaxMP()');

	{ Default Result }
	Result := 0;

	try
		{ Start off with the base MP }
  	Result := FMaxMP;
  	
  	{ Add any items that affect health }
  	Result := Result + GetEnchantmentValue('ENCH_MANA');

		{ TODO: handle negative mana }
  	if Result < 0 then 
  		Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magicpoints }
function TCreature.GetCurrentMP: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentMP()');

	{ Default Result }
	Result := 0;

	try
		{ Start off with the current MP }
  	Result := FMP;
  	
  	{ Add any items that affect mana }
  	Result := Result + GetEnchantmentValue('ENCH_MANA');

		{ TODO: handle negative mana }
  	if Result < 0 then 
  		Result := 0;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current hitpoints }
function TCreature.GetCurrentHP: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentHP()');

	{ Default Result }
	Result := 0;

	try
		{ Start off with the current HP }
  	Result := FHP;
  	
  	{ Add any items that affect HP }
  	Result := Result + GetEnchantmentValue('ENCH_HP');

		{ TODO: handle negative HP }
  	if Result < 0 then 
  		Result := 0;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current combat accuracy }
function TCreature.GetCurrentAccuracy: Integer;
var
  CurrentTotal: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetCurrentAccuracy()');

	{ Default Result }
	Result := 0;

	try
		{ Accuracy is based upon several stats }
		CurrentTotal := (GetCurrentAgility - 10);
		Currenttotal := CurrentTotal div 4;
		Currenttotal := CurrentTotal + (Skills[SK_FIGHTING] div 2);

		{ And is adjusted by worn items }
		CurrentTotal := CurrentTotal + GetEnchantmentValue('ENCH_HIT');

		{ Return the total }
		Result := CurrentTotal;

    { Check for potion }
    if Has(MASTERY) then
      Result := Result * 5;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current combat damage bonus }
function TCreature.GetCurrentDamageBonus: Integer;
var
  CurrentTotal: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentDamageBonus()');

	{ Default Result }
	Result := 0;

	try
		{ Damage bonus is based upon several stats }
  	CurrentTotal := (GetCurrentStrength - 10);
  	CurrentTotal := CurrentTotal div 4;
  	
  	{ And is adjusted by worn items }
  	CurrentTotal := CurrentTotal + GetEnchantmentValue('ENCH_DAM');

    { Check for potion }
    if Has(ENRAGED) then
      Result := Result * 3;

		{ Return the total }
  	Result := CurrentTotal;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the value of an enchantment from items worn }
function TCreature.GetEnchantmentValue(Enchantment: String): Integer;
var
  Loop: Integer;
  LocalItem: TItem;
  Magnitude: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetEnchantmentValue()');

	{ Default Result }
	Result := 0;

	try
		{ Check each equipped slot }
		for Loop := S_FEET to S_BACK do
		begin
			{ If we have an item in the slot }
			if Inventory[Loop] > 0 then
			begin
				{ Get the item }
				LocalItem := GItemList[Inventory[Loop]] as TItem;
				
				{ Get the magnitude of the enchantment }
				Magnitude := LocalItem.ReturnEnchantmentMagnitude(Enchantment);
				
				{ Cursed items decrease the enchantment value instead of increasing
				  it }
				if LocalItem.Cursed then
					Dec(Result, Magnitude)
				else
					Inc(Result, Magnitude);
			end;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current combat deflection }
function TCreature.GetCurrentDeflection: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentDeflection()');

	{ Default Result }
	Result := 0;

	try
		{ Deflection is based upon several variables }
		Result := Skills[SK_DEFENSE];
		Result := Result + GetEnchantmentValue('ENCH_DEFLECTION');

		{ Add in some deflection for shields - for now, if we're using an off-hand
		  item, which are all currently shields, then add a flat 10% bonus }
		if Inventory[S_OFFHAND] > 0 then 
			Inc(Result, 10);

		{ Reduce effect of deflection by half }
		Result := Result div 2;

    { Check for potion }
    if Has(REFLEXES) then
      Result := Result * 5;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current combat blocking }
function TCreature.GetCurrentBlocking: Integer;
var
  CurrentTotal: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentBlocking()');

	{ Default Result }
	Result := 0;

	try
		{ Blocking is based upon several variables }
		CurrentTotal := Skills[SK_DEFENSE];
		CurrentTotal := CurrentTotal + GetEnchantmentValue('ENCH_BLOCK');

		{ Add in some blocking for shields - for now, if we're using an off-hand 
		  item, which are all currently shields, then add a flat 15% bonus }
		if Inventory[S_OFFHAND] > 0 then 
			Inc(CurrentTotal, 15);
			
		{ Reduce effect of blocking by half }
		Result := CurrentTotal div 2;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to air spells }
function TCreature.GetCurrentResistanceToAir: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToAir()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_AIR');

    { Add extra resistance for potion effects }
    if Has(RESISTING_AIR) then
      Result := Result + RESISTANCE_BONUS;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to earth spells }
function TCreature.GetCurrentResistanceToEarth: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToEarth()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_EARTH');

    { Add extra resistance for potion effects }
    if Has(RESISTING_EARTH) then
      Result := Result + RESISTANCE_BONUS;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to fire spells }
function TCreature.GetCurrentResistanceToFire: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToFire()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_FIRE');

    { Add extra resistance for potion effects }
    if Has(RESISTING_FIRE) then
      Result := Result + RESISTANCE_BONUS;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to lifedraining spells }
function TCreature.GetCurrentResistanceToLifeDrain: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToLifeDrain()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_LIFEFORCE');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to poison spells }
function TCreature.GetCurrentResistanceToPoison: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToPoison()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_POISON');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get current magical resistance to water spells }
function TCreature.GetCurrentResistanceToWater: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetCurrentResistanceToWater()');

	{ Default Result }
	Result := 0;

	try
		{ Use the base magic resistance }
  	Result := GetCurrentResistance;
  	
  	{ Add any extra from equipped items }
  	Result := Result + GetEnchantmentValue('ENCH_WATER');

    { Add extra resistance for potion effects }
    if Has(RESISTING_WATER) then
      Result := Result + RESISTANCE_BONUS;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the cost of a spell }
function TCreature.GetSpellCost(SpellCastingSkill, SchoolSkill, SpellLevel,
  Intelligence: Integer): Integer;
var
  Cost: Real;
  Difference: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetSpellCost()');

	{ Default Result }
	Result := 1;

	try
		{ Spell Cost is a function of various factors }

		{ Base Cost }
		Cost := SpellLevel * 1.5;

		{ This is modified by Spell Casting Skill - if Spell Casting Skill is 
			beneath the Spell Level then the Spell Cost is increased - if above then
			the Cost is decreased }
		Difference := SpellCastingSkill - SpellLevel;
		if Difference < 0 then
			Cost := Cost * (1 + (0.2 * Abs(Difference)))
		else
			Cost := Cost * (1 - (0.03 * Difference));

		{ Ensure we don't have negative costs }
		if Cost < 0 then 
			Cost := 0;

		{ Likewise we modify the cost by the Spell School Skill }
		Difference := SchoolSkill - SpellLevel;
		if Difference < 0 then
			Cost := Cost * (1 + (0.2 * Abs(Difference)))
		else
			Cost := Cost * (1 - (0.06 * Difference));

		{ Ensure we don't have negative costs }
		if Cost < 0 then 
			Cost := 0;

		{ Intelligence alters the cost depending on the modifier }
		Difference := (Intelligence - 10) div 2;
		if Difference < 0 then
			Cost := Cost * (1 + (0.02 * Abs(Difference)))
		else
			Cost := Cost * (1 - (0.02 * Difference));;

		{ Ensure we don't have negative costs }
		if Cost < 0 then 
			Cost := 1;

		{ Return the result as a whole number }
  	Result := Trunc(Cost);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the success chance of casting a spell }
function TCreature.GetSpellProbability(SpellCastingSkill, SchoolSkill,
  SpellLevel, Intelligence: Integer): Integer;
var
  Difference: Integer;
  Probability: Real;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetSpellProbability()');

	{ Default Result }
	Result := 0;

	try
		{ Base Probability is 95% }
		Probability := 95;

		{ This is modified by Spell Casting Skill - if Spell Casting Skill is 
		  beneath the Spell Level then the Spell Probability is decreased }
		Difference := SpellCastingSkill - SpellLevel;
		if Difference < 0 then
			Probability := Probability * (1 - (0.15 * Abs(Difference)));

		{ Ensure we don't have negative probability }
		if Probability < 0 then 
			Probability := 0;

		{ Likewise with the Spell School - also a decrease per point of 
			difference }
		Difference := SchoolSkill - SpellLevel;
		if Difference < 0 then
			Probability := Probability * (1 - (0.15 * Abs(Difference)));

		{ Ensure we don't have negative probability }
		if Probability < 0 then 
			Probability := 0;

		{ And with Intelligence a slight modifier as well }
		Difference := (Intelligence - 10) div 2;
		if Difference < 0 then
			Probability := Probability * (1 - (0.03 * Abs(Difference)));
			
		{ Ensure we don't have negative probability }
		if Probability < 0 then 
			Probability := 0;

		{ Return the result as a whole number }
  	Result := Trunc(Probability);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Take Damage }
function TCreature.TakeDamage(Monster: TMonster; DamageTaken: Integer): Boolean;
var
  ProportionBefore: Real;
  ProportionAfter: Real;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.TakeDamage()');

	{ Default Result }
	Result := True;

	try
		{ If we have wizard mode, don't take damage }
		if (Assigned(FormWizard)) then
			if FormWizard.Visible = True then
				DamageTaken := 0;
				
		{ Keep all damage inflicted beneath the maximum hp - 1 to avoid one-shots
			at FULL health } 
		if DamageTaken > FHP + GetEnchantmentValue('ENCH_HP') then
			DamageTaken := FHP + GetEnchantmentValue('ENCH_HP') - 1;

		{ Keep a track of the health beforehand }
		ProportionBefore := (FHP + GetEnchantmentValue('ENCH_HP')) / FMaxHP;
		
		{ Take the damage }
		Dec(FHP, DamageTaken);
		
		{ Keep a track of the health after }
		ProportionAfter := (FHP + GetEnchantmentValue('ENCH_HP')) / FMaxHP;

		{ Work out if this is a big enough chunk of health to warn the player }
		if ProportionBefore - ProportionAfter > 0.50 then
			UnitEngine.UpDateLog('Ouch! That really hurt!', mesYouKill);

		{ Check if we've crossed the warning HP threshhold and if so, warn the 
		  player }
		if (ProportionBefore >= HPTHRESHOLD) and 
			(ProportionAfter < HPTHRESHOLD) then
			UnitEngine.UpDateLog('*** LOW HITPOINT WARNING ***', mesError);

		{ Check for death }
		if FHP + GetEnchantmentValue('ENCH_HP') <=0 then
		begin
			{ If wizard mode is on, allow the player not to die }
			if FormWizard.Visible then
			begin
				if MessageDlg('Die?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
				begin
					{ Restore Health }
					FHP := FMaxHP - GetEnchantmentValue('ENCH_HP');
					
					{ Don't die }
					Result := True;
				end
				else
					{ Die }
					Result := False;
			end
			else
				{ Die }
				Result := False;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the number of slots without any items in them }
function TCreature.GetNumberUnusedSlots: Integer;
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetNumberUnusedSlots()');

	{ Default Result }
	Result := 0;

	try
		{ Check each inventory slot }
		for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
		begin
			{ If there are no items in the slot }
			if InventoryCount[Loop] = 0 then 
				Inc(Result);
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Gain XP }
procedure TCreature.GainXP(XPToGain: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GainXP()');

	try
		{ Add the XP }
		Inc(FCurrentXP, XPToGain);

		{ Check to see if we have gained a level }
		if FCurrentXP >= GetNextLevelXP then 
			GainALevel;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the XP for the creatures next level }
function TCreature.GetNextLevelXP: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetNextLevelXP()');

	{ Default Result }
	Result := 0;

	try
		{ XP per level is currently linear }
  	Result := (FLevel * 170) * FLevel;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Gain an experience level }
procedure TCreature.GainALevel;
var
  HPGained: Integer;
  MPGained: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GainALevel()');

	try
		{ Increase the level }
		Inc (FLevel);
		
		{ Alert the player }
		UpDateLog('Your hard work and perseverance has paid off. You have gained '
			+ 'a level. Welcome to Level ' + InTToStr(FLevel), mesLevelUp);
			
		{ Work how how much HP and MP are gained }
		HPGained := FEndurance div 3 + Skills[SK_FIGHTING];
		MPGained := FIntelligence div 3 + Skills[SK_MAGIC];
		
		{ Alert the player }
		UpDateLog('You have gained ' + IntToStr(HPGained) + ' HP and ' + 
			IntToStr(MPGained) + ' MP.', mesLevelUp);
		
		{ Gain the HP and MP calculated }
		Inc(FHP, HPGained);
		Inc(FMP, MPGained);
		Inc(FMaxHP, HPGained);
		Inc(FMaxMP, MPGained);

		{ Gain skills }
		case CClass of  
			cThief: 
			begin
				inc(Skills[SK_SUBTERFUGE]);
				inc(Skills[SK_FIGHTING]);
			end;
			cKnight:
			begin
				inc(Skills[SK_FIGHTING]);
				inc(Skills[SK_MAGIC]);			
			end;
			cMage:
			begin
				inc(Skills[SK_MAGIC]);
				inc(Skills[SK_SUBTERFUGE]);		
			end;
			cPriest:
			begin
				inc(Skills[SK_MAGIC]);
				inc(Skills[SK_DEFENSE]);		
			end;
			cWarrior:
			begin
				inc(Skills[SK_FIGHTING]);
				inc(Skills[SK_DEFENSE]);		
			end;
		end;

    { Let the player note that they have gained new skills }
    UpDateLog('You have gained some skills', mesLevelUp);

		{ Make a note of the level gain }
  	TakeNote(FormDisplay.Game.Turns, '', nDing, FormDisplay.Game.Dungeon);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get bonus damage the player has against a creature type }
function TCreature.GetBonusDamage(MonsterRace: Integer): Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetBonusDamage()');

	{ Default Result }
	Result := 0;

	try
		{ For each monster race, check the damage bonus for that race }
		case MonsterRace of
			1: Result := GetEnchantmentValue('ENCH_SLAYANIMAL');
			2: Result := GetEnchantmentValue('ENCH_SLAYHUMANOID');
			3: Result := GetEnchantmentValue('ENCH_SLAYCONSTRUCT');
			4: Result := GetEnchantmentValue('ENCH_SLAYDEMON');
			5: Result := GetEnchantmentValue('ENCH_SLAYDRAGON');
			6: Result := GetEnchantmentValue('ENCH_SLAYELEMENTAL');
			7: Result := GetEnchantmentValue('ENCH_SLAYPLANT');
			8: Result := GetEnchantmentValue('ENCH_SLAYUNDEAD');
			9: Result := GetEnchantmentValue('ENCH_SLAYGIANT');
			10: Result := GetEnchantmentValue('ENCH_SLAYAOOZE');
			11: Result := GetEnchantmentValue('ENCH_SLAYGOBLINOID');
			12: Result := GetEnchantmentValue('ENCH_SLAYOUTSIDER');
			else Result := 0;
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Feed }
procedure TCreature.Feed(FoodValue: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.Feed()');

	try
		{ Feeding is also used for negative values, i.e. hunger, so if we have
		  wizard mode on, don't do anything }
		if Assigned(FormWizard) then
			if FormWizard.Visible = True then
				Exit;
	
		{ Increase/Decrease the character hunger }
		inc(FFood, FoodValue);
		
		{ Deal with the bounds of hunger }
		if FFood > (FEndurance * 100) then 
			FFood := FEndurance * 100;
		if FFood < (0 - (FEndurance * 100)) then 
			FFood := 0 - (FEndurance * 100);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check the food level to see if the character is so hungry he/she is 
	starving }
function TCreature.IsStarving: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.IsStarving()');

	{ Default Result }
	Result := False;

	try
		{ Check if the food is at the minimum }
  	Result := FFood = (0 - (FEndurance * 100));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Change the hunger level into a friendly description }
function TCreature.GetFoodStatus: String;
var
  MinFoodValue: Integer;
  MaxFoodValue: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetFoodStatus()');

	{ Default Result }
	Result := '';

	try
		{ Get min and max food values }
		MinFoodValue := 0 - (FEndurance * 100);
		MaxFoodValue := FEndurance * 100;
		
		{ Convert the current hunger into a string }
		if FFood = MinFoodValue then 
			Result := 'Starving'
		else if FFood = MaxFoodValue then 
			Result := 'Full'
		else if FFood < MinFoodValue + Abs(MinFoodValue div 2) then 
			Result := 'Hungry'
		else if FFood > MaxFoodValue div 2 then 
			Result := 'Sated'
		else 
			Result := '';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Change the hunger level into a friendly colour }
function TCreature.GetFoodColour: TColor;
var
  MinFoodValue: Integer;
  MaxFoodValue: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetFoodColour()');

	{ Default Result }
	Result := clWhite;

	try
		{ Get min and max food values }
		MinFoodValue := 0 - (FEndurance * 100);
		MaxFoodValue := FEndurance * 100;
		
		{ Convert the current hunger into a colour }
		if FFood = MinFoodValue then 
			Result := clRed
		else if FFood = MaxFoodValue then 
			Result := clLime
		else if FFood < MinFoodValue + Abs(MinFoodValue div 2) then 
			Result := clYellow
		else if FFood > MaxFoodValue div 2 then 
			Result := clGreen
		else 
			Result := clBlack;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the amount of times a skill needs to be practised before skill up }
function TCreature.GetSkillLimit(Skill: Integer): Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetSkillLimit()');

	{ Default Result }
	Result := 0;

	try
		{ If the skill isn't zero, then the amount it takes to level is different }
		if Skill > 0 then
		begin
			{ Learning is based upon intelligence }
			Result := (30 - Intelligence) * 3;
			
			{ With a minimum value }
			if Result < 30 then 
				Result := 30;
				
			{ And increase it to a reasonable value }
			Result := Result + ((30 - Intelligence) * 3);
		end
		else
			Result := (30 - Intelligence) * 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the progress of a skill to the next skill level as a string }
function TCreature.SkillProgressString(Skill: Integer): String;
var
  Progress: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.SkillProgressString()');

	{ Default Result }
	Result := '';

	try
		{ Get the progress }
  	Progress := SkillProgress(Skill);
  	
  	{ Convert the progress into a string }
  	if Progress = 0 then 
  		Result := '' 
  	else 
  		Result := IntToStr(Progress) + '%';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the progress of a skill to the next skill level as an integer }
function TCreature.SkillProgress(Skill: Integer): Integer;
var
  SkillProgress: Double;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.SkillProgress()');

	{ Default Result }
	Result := 0;

	try
		{ Work out the percentage }
		SkillProgress := (SkillsProgress[Skill] / 
			GetSkillLimit(SkillsProgress[Skill])) * 100;
			
		{ Return the percentage as a whole number }
		Result := Trunc(SkillProgress);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Learn a skill }
procedure TCreature.LearnSkill(SkillToLearn: Integer; SkillJump: Integer = 1);
var
  CurrentSkillLevel: Integer;
  CurrentSkillProgress: Integer;
  SkillLimit: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.LearnSkill()');

	try
		{ Get the current skill level }
		CurrentSkillLevel := Skills[SkillToLearn];
		
		{ Get the current skill progrss }
		CurrentSkillProgress := SkillsProgress[SkillToLearn];
		
		{ Get the skill threshhold }
		SkillLimit := GetSkillLimit(CurrentSkillLevel);
		
		{ Increase the progress }
		Inc(CurrentSkillProgress, SkillJump);
		SkillsProgress[SkillToLearn] := CurrentSkillProgress;
		
		{ If we have learned enough to bump up a skill } 
		if CurrentSkillProgress > SkillLimit then
		begin
			{ Increase the skill }
			Inc(Skills[SkillToLearn]);
			
			{ Reset the progress }
			SkillsProgress[SkillToLearn] := 0;
			
			{ If we have learned a new skill (went from 0 to 1) then add an
			  appropriate message to the message log }
			if Skills[SkillToLearn] = 1 then 
				UnitEngine.UpDateLog('You have learned the ' + 
					GSkillName[SkillToLearn] + ' skill!', mesSkill )
			else 
				{ Else update the message log anyway }
				UnitEngine.UpDateLog('Your ' + GSkillName[SkillToLearn] + 
					' skill has increased to level ' + IntToStr(Skills[SkillToLearn]) 
					+ '!',mesSkill );

			{ Take a note if we have gained significant value in a skill }
			if (Skills[SkillToLearn] mod 5) = 0 then
				TakeNote(FormDisplay.Game.Turns, 
					Format('%d in %s', [Skills[SkillToLearn], 
					GSkillName[SkillToLearn]]), nSkill, FormDisplay.Game.Dungeon);
		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Work out what armours are being worn of a certain type }
function TCreature.ReturnNumberOfArmourTypeWorn(ArmourType: crMaterial): Integer;
var
  InventoryLoop: Integer;
  LocalItem: TItem;
  LocalMaterial: crMaterial;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.ReturnNumberOfArmourTypeWorn()');

	{ Default Result }
	Result := 0;

	try
    { Look in every equipped slot for a specific type of armour }
    for InventoryLoop := S_FEET to S_HEAD do
    begin
      { If there is an item in the slot }
      if Inventory[InventoryLoop] > 0 then
      begin
        { Get the item }
        LocalItem := (GItemList[Inventory[InventoryLoop]] as TItem);

        { Get the material }
        LocalMaterial := LocalItem.ItemMaterial;

        { Check if it is the material we're after }
        if LocalMaterial = ArmourType then
          inc(Result);
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Work out the overall ratio of armour types worn in various slots }
procedure TCreature.ReturnArmourRatio(var Heavy: Integer; var Medium: Integer;
  var Light: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.ReturnArmourRatio()');

	try
    { Initialise the armour type }
    Heavy := 0;
    Medium := 0;
    Light := 0;

    { Return the number of armour items worn }
    Heavy := ReturnNumberOfArmourTypeWorn(mMetal);
    Medium := ReturnNumberOfArmourTypeWorn(mLeather);
    Light := ReturnNumberOfArmourTypeWorn(mCloth);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Learn armour skills }
procedure TCreature.LearnSkillArmour;
var
  Light: Integer;
  Medium: Integer;
  Heavy: Integer;
  Total: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.LearnSkillArmour()');

	try
    { Work out what types of armour the character is wearing }
    ReturnArmourRatio(Heavy, Medium, Light);

    { Now work out how many slots in total are armoured }
    Total := Heavy + Medium + Light;

    { If we are wearing armour then learn skills = the more armour that is worn
      the better }
    if Heavy > 0 then
    begin
      { Do additonal armour skills for classes }
      if FormDisplay.Game.Player.CClass = cWarrior then
        LearnSkill(SK_HEAVY, Heavy)
      else
        LearnSkill(SK_HEAVY, Heavy div 3);
    end;

    { It is progressively easier to learn lighter armour }
    if Medium > 0 then
      LearnSkill(SK_MEDIUM, Medium div 2);

    { Light armour is the easiest to learn }
    if Light > 0 then
      LearnSkill(SK_LIGHT, Light div 1);

    { Learning armour leads to learning the defense skill }
    if Total > 0 then
      LearnSkill(SK_DEFENSE, Total div 6);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the current alertness radius }
function TCreature.GetAlertness: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetAlertness()');

	{ Default Result }
	Result := 0;

	try
    { Find the current alertness of the character, i.e. the radius of the FOV }
    Result := LIGHTRADIUS + TempAlertness + GetEnchantmentValue('ENCH_ALERT');

    { Put bounds on it }
    if Result < 3then
      Result := 3;
    if Result > 10 then
      Result := 10;

    { Set up some FOV variables }
    FormDisplay.Game.mv := Result;
    FormDisplay.Game.mw := FormDisplay.Game.mv * FormDisplay.Game.mv;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the notes for the character }
function TCreature.GetNotes: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetNotes()');

	{ Default Result }
	Result := '';

	try
    { Return all the character notes }
    Result := FNotes.Text;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Take a note }
procedure TCreature.TakeNote(Turns: Integer; Note: String; NoteType: tCharacterNote; Dungeon: TDungeonLevel);
var
  SubNote: String;
  NoteString: String;
  PlaceName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.TakeNote()');

	try
    { Find the current dungeon name and depth }
    PlaceName := Format('%s %d', [Dungeon.Name, Dungeon.LevelDepth]);

    { Add the note, the format and contents of which differ according to the
      type }
    case NoteType of
      nBegin: SubNote :=
        Format('%s the %s %s started to explore the Multiverse', [FName,
        SubRaceToString(FSubRace), ClassToString(FClass)]);
      nDing: SubNote := Format('Reached Level %d, HP: %d/%d, MP: %d/%d',
        [FLevel, GetCurrentHP, GetCurrentMaxHP, GetCurrentMP, GetCurrentMaxMP]);
      nReach: SubNote := Format('Explored Level %d of %s',
        [Dungeon.LevelDepth, Dungeon.Name]);
      nItem: SubNote := Format('Identified %s', [Note]);
      nNotice: SubNote := Format('Noticed %s', [Note]);
      nMonster: SubNote := Format('Killed %s', [Note]);
      nUnique: SubNote := Format('Killed %s', [Note]);
      nSkill: SubNote := Format('Gained Skill Level %s', [Note]);
      nDeath: SubNote := Note;
    end;

  NoteString := Format('%-7.7d %-25.25s %s', [Turns,
                                              PlaceName,
                                              SubNote]);
  FNotes.Add(NoteString);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate starting equipment }
procedure TCreature.GenerateAndEquipStartingEquipment;
var
  DesiredMaterial: crMaterial;
  Temp: Integer;
  SkillArray: Array [1..3] of Integer;
  Item: TItem;

  { Inline function }
  function Find_Max(Arr: array of Integer): Integer;
  var
    i, M, p: Integer;
  begin
    M := Arr[Low(Arr)];
    p := 1;
    for i := 1 to High(Arr) do
      if Arr[i] >= M then
      begin
        M := Arr[i];
        p := i + 1;
      end;
    Result := p;
  end;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GenerateAndEquipStartingEquipment()');
  
	try
    { We want to give all classes some armour (of various types), a melee weapon
      and some food. Some classes also get a shield or a ranged weapon}

    { First, work out what the best armour skill the player has and generate
      some starting armour items based upon that }
    DesiredMaterial := mCloth;
    SkillArray[1] := Skills[SK_LIGHT];
    SkillArray[2] := Skills[SK_MEDIUM];
    SkillArray[3] := Skills[SK_HEAVY];
    Temp := Find_Max(SkillArray);
    case Temp of
      1: DesiredMaterial := mCloth;
      2: DesiredMaterial := mLeather;
      3: DesiredMaterial := mMetal;
    end;

    { In new games, these items will be the very first items added to the global
      object list (GItemList). We add aake Item to start the item list - oh woe
      to 0-index lists! }
    Item := TItem.Create(iCommon, 1, iArmour, False);
    GItemList.Add(Item);
  
    { Now we generate armour - we want chest, helm, boots, legs }
    repeat
       Item := TItem.Create(iCommon, 1, iArmour, False);
    until (Item.ItemMaterial = DesiredMaterial) and (Item.ItemSlot = iChest);
    Item.Location := iChest;
    Item.Known := True;
    GItemList.Add(Item);
    Inventory[S_CHEST] := GItemList.Count - 1;
    InventoryCount[S_CHEST] := 1;
    repeat
       Item := TItem.Create(iCommon, 1, iArmour, False);
    until (Item.ItemMaterial = DesiredMaterial) and (Item.ItemSlot = iHead);
    Item.Location := iHead;
    Item.Known := True;
    GItemList.Add(Item);
    Inventory[S_HEAD] := GItemList.Count - 1;
    InventoryCount[S_HEAD] := 1;
    repeat
       Item := TItem.Create(iCommon, 1, iArmour, False);
    until (Item.ItemMaterial = DesiredMaterial) and (Item.ItemSlot = iFeet);
    Item.Location := iFeet;
    Item.Known := True;
    GItemList.Add(Item);
    Inventory[S_FEET] := GItemList.Count - 1;
    InventoryCount[S_FEET] := 1;
    repeat
       Item := TItem.Create(iCommon, 1, iArmour, False);
    until (Item.ItemMaterial = DesiredMaterial) and (Item.ItemSlot = iLegs);
    Item.Location := iLegs;
    Item.Known := True;
    GItemList.Add(Item);
    Inventory[S_LEGS] := GItemList.Count - 1;
    InventoryCount[S_LEGS] := 1;

    { Now give some food }
    Item := TItem.Create(iCommon, 1, iConsumable, False);
    Item.Location := iInventory;
    Item.Known := True;
    GItemList.Add(Item);
    AddItem(GItemList.Count - 1);
    Item := TItem.Create(iCommon, 1, iConsumable, False);
    Item.Location := iInventory;
    Item.Known := True;
    GItemList.Add(Item);
    AddItem(GItemList.Count - 1);
    Item := TItem.Create(iCommon, 1, iConsumable, False);
    Item.Location := iInventory;
    Item.Known := True;
    GItemList.Add(Item);
    AddItem(GItemList.Count - 1);

    { Now a weapon(s) and possibly a shield This is dependent upon class }
    case CClass of
      cThief:
      begin
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iMainhand) and
          (Item.ItemHandedNess = ONE_HANDED);
        Item.Location := iMainhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_MAINHAND] := GItemList.Count - 1;
        InventoryCount[S_MAINHAND] := 1;
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iRanged);
        Item.Location := iRanged;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_RANGED] := GItemList.Count - 1;
        InventoryCount[S_RANGED] := 1;
      end;
      cKnight:
      begin
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iMainhand) and
          (Item.ItemHandedNess = TWO_HANDED);
        Item.Location := iMainhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_MAINHAND] := GItemList.Count - 1;
        InventoryCount[S_MAINHAND] := 1;
      end;
      cMage:
      begin
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iMainhand) and
          (Item.ItemHandedNess = TWO_HANDED);
        Item.Location := iMainhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_MAINHAND] := GItemList.Count - 1;
        InventoryCount[S_MAINHAND] := 1;
      end;
      cPriest:
      begin
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iMainhand) and
          (Item.ItemHandedNess = ONE_HANDED);
        Item.Location := iMainhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_MAINHAND] := GItemList.Count - 1;
        InventoryCount[S_MAINHAND] := 1;
        repeat
           Item := TItem.Create(iCommon, 1, iArmour, False);
        until (Item.ItemSlot = iOffHand);
        Item.Location := iOffhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_OFFHAND] := GItemList.Count - 1;
        InventoryCount[S_OFFHAND] := 1;
      end;
      cWarrior:
      begin
        repeat
           Item := TItem.Create(iCommon, 1, iWeapon, False);
        until (Item.ItemSlot = iMainhand) and
          (Item.ItemHandedNess = ONE_HANDED);
        Item.Location := iMainhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_MAINHAND] := GItemList.Count - 1;
        InventoryCount[S_MAINHAND] := 1;
        repeat
           Item := TItem.Create(iCommon, 1, iArmour, False);
        until (Item.ItemSlot = iOffHand);
        Item.Location := iOffhand;
        Item.Known := True;
        GItemList.Add(Item);
        Inventory[S_OFFHAND] := GItemList.Count - 1;
        InventoryCount[S_OFFHAND] := 1;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ See if the player can cast spells }
function TCreature.HasMagic: Boolean;
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.HasMagic()');

	{ Default Result }
	Result := False;

	try
    { check to see if this character has any magic skills }
    for Loop := SK_MAGIC to SK_TRAVEL do
    begin
      if Skills[Loop] > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the letters of the spell schools the character has access to to }
function TCreature.GetSpellSchools: String;
var
  School: Char;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetSpellSchools()');

	{ Default Result }
	Result := '';

	try
    School := 'a';

    { Check the spell schools }
    for Loop := SK_MAGIC to SK_TRAVEL do
    begin
      if Skills[Loop] > 0 then
        Result := Result + School;

      inc(School);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a numberical value of how powerful the magical items that the character
  has equipped }
function TCreature.GetMagicalLoad: Integer;
var
  Loop: Integer;
  LocalItem: TItem;
  ItemPowerRating: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TCreature.GetMagicalLoad()');

	{ Default Result }
	Result := 0;

	try
    { Check every equipped slot for magical items, the greater quality of
      magical items, the greater the magical "load" }
    for Loop := S_FEET to S_BACK do
		begin
			{ If we have an item worn/wielded... }
			if Inventory[Loop] > 0 then
			begin
        { Get the item }
				LocalItem := GItemList[Inventory[Loop]] as TItem;

        { Add the power rating of the item - iCommon items have a value of 2
          thus we have to subtract 2 since they shouldn't affect the rating }
        ItemPowerRating := (Ord(LocalItem.ItemQuality) - 2) * 2;

        { Cursed items dramatically affect the item power rating }
        if LocalItem.Cursed then
          inc(ItemPowerRating, ItemPowerRating);

        { Increase the total }
        Inc(Result, ItemPowerRating);
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the friendly class name }
function TCreature.GetFriendlyClassName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetFriendlyClassName()');

	{ Default Result }
	Result := '';

  try
    { Convert the class level and type to its friendly name }
    Result := ClassNames[Ord(CClass), (FLevel div 5) + 1];
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the poison status }
function TCreature.GetPoisonStatus: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetPoisonStatus()');

	{ Default Result }
	Result := '';

  try
    { Check if we're poisoned }
    if FPoison > 0 then
      Result := 'Poisoned';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;


{ Change the poison level into a friendly colour }
function TCreature.GetPoisonColour: TColor;
var
  Quartile: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.GetPoisonColour()');

	{ Default Result }
	Result := clBlack;

	try
    if FPoison > 0 then
    begin
      Quartile := Trunc(FPoison) div 25;

			{ The colour the bar returned changes depending on poison level }
			case Quartile of
				0: Result := clYellow;
				1: Result := $000080FF;
				2: Result := clRed;
				3: Result := $000000B9;
			end;
    end;

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Status Conidition Accessor }
function TCreature.Has(Condition: Integer): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TCreature.Has()');

	{ Default Result }
	Result := False;

	try
    { Check for turns left }
    Result := Status[Condition] > 0;

    { Or check for permanent statuses }
    if Status[Condition] < 0 then
      Result := True;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
