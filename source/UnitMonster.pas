{ UnitMonster

  Copyright (c) 2007-2009 Dave Moore 

  Monster Class and Monster Functions

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

unit UnitMonster;

interface

uses Graphics, Contnrs, SysUtils, Math, Classes, HotLog, UnitInterfaces,
  UnitDefines, UnitVars, UnitConst;

{ TMonsterArchetype is the class that holds the monster type definitions - the
  TMonster class represents actual instances of Monsters, and the appropriate
  TMonsterArchetype is used during the TMonster constructor to populate its
  values and details }
type TMonsterArchetype = Class (TInterfacedObject, ICommonObject)

private
  { Private Members }
  FID: Integer;
  FName: String;
  FPluralName: String;
  FLevel: Integer;
  FDisplayChar: Char;
  FDisplayColour: TColor;
  FResistances: String;
  FHitDice: Integer;
  FAttacks: Integer;
  FAttackBonus: Integer;
  FAttackType: String;
  FAttackDamage: Integer;
  FAC: Integer;
  FEV: Integer;
  FSpeed: Integer;
  FMonsterType: String;
  FFlags: String;
  FBehaviour: String;
  FClasses: String;
  FEcology: String;
  FTreasure: String;
  FPopulation: Integer;
  FOccurrence: String;
  FSpeechFrequency: Integer;
  FSpeech: String;
  FSpeechActions: String;
  FActions: String;
  FCanBeUnique: Boolean;
  FUseItems: Boolean;
  FVerbs: String;
  FRangedVerb: String;
  FProjectileChar: Char;
  FProjectileColour: TColor;
  FPoisonLevel: Integer;

public
  { Constructors }
  constructor Create; overload;
  constructor Create(ID: Integer;
                     Name: String;
                     Level: Integer;
                     DisplayChar: Char;
                     DisplayColour: TColor;
                     Resistances: String;
                     HitDice: Integer;
                     Attacks: Integer;
                     AttackBonus: Integer;
                     AttackType: String;
                     AttackDamage: Integer;
                     AC: Integer;
                     EV: Integer;
                     Speed: Integer;
                     PoisonLevel: Integer;
                     MonsterType: String;
                     Flags: String;
                     Behaviour: String;
                     Classes: String;
                     Ecology: String;
                     Treasure: String;
                     Population: Integer;
                     Occurrence: String;
                     SpeechFrequency: Integer = 0;
                     Speech: String = '';
                     SpeechActions: String = '';
                     Actions: String = '';
                     Verbs: String = '';
                     CanBeUnique: Boolean = False;
                     UseItems: Boolean = False); overload;

  { Interface Method for Persistance }
  function GetStringValue: String;

  { Accessors }
  property ID: Integer read FID;
  property Name: String read FName write FName;
  property PluralName: String read FPluralName write FPluralName;
  property Level: Integer read FLevel write FLevel;
  property DisplayChar: Char read FDisplayChar write FDisplayChar;
  property DisplayColour: TColor read FDisplayColour write FDisplayColour;
  property Resistances: String read FResistances write FResistances;
  property HitDice: Integer read FHitDice write FHitDice;
  property Attacks: Integer read FAttacks write FAttacks;
  property AttackBonus: Integer read FAttackBonus write FAttackBonus;
  property AttackType: String read FAttackType write FAttackType;
  property AttackDamage: Integer read FAttackDamage write FAttackDamage;
  property AC: Integer read FAC write FAC;
  property EV: Integer read FEV write FEV;
  property Speed: Integer read FSpeed write FSpeed;
  property MonsterType: String read FMonsterType write FMonsterType;
  property Flags: String read FFlags write FFlags;
  property Behaviour: String read FBehaviour write FBehaviour;
  property Classes: String read FClasses write FClasses;
  property Ecology: String read FEcology write FEcology;
  property Treasure: String read FTreasure write FTreasure;
  property Population: Integer read FPopulation write FPopulation;
  property Occurrence: String read FOccurrence write FOccurrence;
  property SpeechFrequency: Integer read FSpeechFrequency
    write FSpeechFrequency;
  property Actions: String read FActions;
  property Speech: String read FSpeech;
  property SpeechActions: String read FSpeechActions;
  property Verbs: String read FVerbs;
  property RangedVerb: String read FRangedVerb write FRangedVerb;
  property CanBeUnique: Boolean read FCanBeUnique write FCanBeUnique;
  property UseItems: Boolean read FUseItems write FUseItems;
  property ProjectileChar: Char read FProjectileChar write FProjectileChar;
  property ProjectileColour: TColor read FProjectileColour
    write FProjectileColour;
  property PoisonLevel: Integer read FPoisonLevel write FPoisonLevel;  
end;

{ TMonster represents individual monsters }
type TMonster = Class

private
  { Private Members }
  FGUID: TGuid;
  FHostile: Boolean;
  FName: String;
  FPluralName: String;
  FID: Integer;
  FMaxHP: Integer;
  FCondition: String;
  FColour: TColor;
  FChar: Char;
  FCurrentBehaviour: String;
  FHomeEcology: String;
  FCategory: String;
  FRace: Integer;
  FLevel: Integer;
  FAwake: Boolean;
  FAlive: Boolean;
  FEnergy: Integer;
  FX: Integer;
  FY: Integer;
  FSpeed: Integer;
  FCurrentHP: Integer;
  FAttackType: String;
  FAttackNumber: Integer;
  FXP: Integer;
  FTurnsSinceLastHealthMessage: Integer;
  FUniqueName: String;
  FUseItems: Boolean;
  FWeapon: Integer;
  FArmour: Integer;
  FHasBeenSeen: Boolean;
  FLastKnownX: Integer;
  FLastKnownY: Integer;
  FActions: String;
  FSpeechActions: String;
  FSpeech: String;
  FSpeechFrequency: Integer;
  FVerbs: String;
  FRangedVerb: String;
  FAttackBonus: Integer;
  FAttackDamage: Integer;
  FAC: Integer;
  FEV: Integer;
  FProjectileChar: Char;
  FProjectileColour: TColor;
  FPoisonLevel: Integer;

  { Private functions to support class properties defined below }
  function GetDescription: String;
  function GetDamageLevel(var TextColour: TColor): String;
  function GetPrefix: string;
  function GetSinglePrefix: string;
  function GetAttackBonus: Integer;
  function GetDamageBonus: Integer;
  function GetEvasion: Integer;
  function GetAC: Integer;
  function GetRandomVerb: String;

public
  { Constructors }
  constructor Create; overload;
  constructor Create(ID: Integer;  Unique: Boolean = False;
    Boosted: Boolean = False); overload;

  { Accessors }
  property GUID: TGuid read FGUID;
  property Hostile: Boolean read FHostile write FHostile;
  property Name: String read FName write FName;
  property PluralName: String read FPluralName write FPluralName;
  property ID: Integer read FID write FID;
  property MaxHP: Integer read FMaxHP write FMaxHP;
  property Condition: String read FCondition;
  property Colour: TColor read FColour write FColour;
  property Char: Char read FChar write FChar;
  property CurrentBehaviour: String read FCurrentBehaviour;
  property HomeEcology: String read FHomeEcology;
  property Category: String read FCategory;
  property Race: Integer read FRace;
  property Level: Integer read FLevel write FLevel;
  property Awake: Boolean read FAwake write FAwake;
  property Alive: Boolean read FAlive write FAlive;
  property Energy: Integer read FEnergy;
  property X: Integer read FX write FX;
  property Y: Integer read FY write FY;
  property Speed: Integer read FSpeed write FSpeed;
  property CurrentHP: Integer read FCurrentHP write FCurrentHP;
  property AttackType: String read FAttackType;
  property AttackNumber: Integer read FAttackNumber;
  property XP: Integer read FXP write FXP;
  property TurnsSinceLastHealthMessage: Integer
    read FTurnsSinceLastHealthMessage;
  property UniqueName: String read FUniqueName;
  property UseItems: Boolean read FUseItems;
  property Weapon: Integer read FWeapon write FWeapon;
  property Armour: Integer read FArmour write FArmour;
  property HasBeenSeen: Boolean read FHasBeenSeen;
  property LastKnownX: Integer read FLastKnownX;
  property LastKnownY: Integer read FLastKnownY;
  property Actions: String read FActions;
  property SpeechActions: String read FSpeechActions;
  property Speech: String read FSpeech;
  property SpeechFrequency: Integer read FSpeechFrequency;
  property Verbs: String read FVerbs;
  property RangedVerb: String read FRangedVerb;
  property AttackBonus: Integer read GetAttackBonus write FAttackBonus;
  property AttackDamage: Integer read GetDamageBonus write FAttackDamage;
  property EV: Integer read GetEvasion write FEV;
  property AC: Integer read GetAC write FAC;
  property Description: String read GetDescription;
  property Prefix: string read GetPrefix;
  property SinglePrefix: String read GetSinglePrefix;
  property BeenSeen: Boolean read FHasBeenSeen write FHasBeenSeen;
  property Verb: String read GetRandomVerb;
  property ProjectileChar: Char read FProjectileChar write FProjectileChar;
  property ProjectileColour: TColor read FProjectileColour
    write FProjectileColour;
  property PoisonLevel: Integer read FPoisonLevel write FPoisonLevel;
  
  { Public Functions }

  { Compares One Monster to Another }
  function Equals(Monster: TMonster): Boolean;

  { If a Monster is carrying a Weapon or an Armour that boosts a stat or
    attribute, then return the value }
  function GetEnchantmentValue(Enchantment: String): Integer;

  { Spend Energy }
  function SpendEnergy(EnergyToSpend: Integer): Integer;

  { Get the current health of the monster in words }
  function GetMonsterHealthText(var MessageReturned: String;  var TextColor: TColor): Boolean;

  { Get the 'Con' status of the Monster, i.e. if its an overcon or an undercon }
  function GetRelativeDescription(CurrentDungeonLevel: Integer; CurrentPlayerLevel: Integer): String;

  { Boost the Monster's Level, increasing many of its stats and effectiveness }
  procedure BoostLevel(LevelGain: Integer);
end;

implementation

uses UnitFunctions, UnitItem, UnitEngine, UnitDisplay, UnitDungeon;

{ TMonsterArchetype }

{ Default Constructor }
constructor TMonsterArchetype.Create;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonsterArchetype.Create()');

  try
    { Does nothing }
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Usual Constructor }
constructor TMonsterArchetype.Create(ID: Integer; Name: String; Level: Integer;
  DisplayChar: Char; DisplayColour: TColor; Resistances: String; HitDice,
  Attacks, AttackBonus: Integer; AttackType: String; AttackDamage, AC, EV,
  Speed: Integer; PoisonLevel: Integer; MonsterType: String; Flags, Behaviour, Classes, Ecology,
  Treasure: String; Population: Integer; Occurrence: String; SpeechFrequency: Integer;
  Speech: String; SpeechActions: String; Actions: String; Verbs: String;
  CanBeUnique: Boolean; UseItems: Boolean);
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TMonsterArchetype.Create(%d, %s)', [ID, Name]));

  try
    { Load the private member data }
    FID := ID;
    FName := Name;
    FLevel := Level;
    FDisplayChar := DisplayChar;
    FDisplayColour := DisplayColour;
    FResistances := Resistances;
    FHitDice := HitDice;
    FAttacks := Attacks;
    FAttackBonus := AttackBonus;
    FAttackType := AttackType;
    FAttackDamage := AttackDamage;
    FAC := AC;
    FEV := EV;
    FSpeed := Speed;
    FPoisonLevel := PoisonLevel;
    FMonsterType := MonsterType;
    FFlags := Flags;
    FBehaviour := Behaviour;
    FClasses := Classes;
    FEcology := Ecology;
    FTreasure := Treasure;
    FPopulation := Population;
    FOccurrence := Occurrence;
    FSpeechFrequency := SpeechFrequency;
    FSpeech := Speech;
    FSpeechActions := SpeechActions;
    FActions := Actions;
    FVerbs := Verbs;
    FRangedVerb := RangedVerb;
    FCanBeUnique := CanBeUnique;
    FUseItems := UseItems;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Monster ArcheType as a String }
function TMonsterArchetype.GetStringValue: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonsterArchetype.GetStringValue()');

  Result := Format('%d %s', [FID, FName]);
end;

{ TMonster }

{ Default Constructor }
constructor TMonster.Create;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.Create()');

  try
    { Does nothing }
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Preferred Constructor }
constructor TMonster.Create(ID: Integer; Unique: Boolean; Boosted: Boolean);
var
  MonsterType: TMonsterArchetype;
  Item: TItem;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TMonster.Create(%d, %d)', [ID, Ord(Unique)]));

  try
    { Assign the Archetype ID }
    FID := ID;

    { Load the Archetype Information }
    MonsterType := GetMonsterArchetype(FID);

    { Create a new GUID }
    CreateGUID(FGUID);

    { By default, monsters do not carry weapons or armour }
    FWeapon := NO_ITEM;
    FArmour := NO_ITEM;

    { Load the monster properties from the Archetype }
    FColour := MonsterType.DisplayColour;
    FChar := MonsterType.DisplayChar;
    FName := MonsterType.Name;
    FPluralName := MonsterType.PluralName;
    FCategory := GetMonsterCategory(StrToIntDef(MonsterType.MonsterType, 0));
    FRace := StrToIntDef(MonsterType.MonsterType, 0);
    FLevel := MonsterType.Level;
    FUseItems := MonsterType.UseItems;
    FCondition := '';
    FCurrentBehaviour := MonsterType.Behaviour;
    FHomeEcology := MonsterType.Ecology;
    FHasBeenSeen := False;
    FAwake := False;
    FAlive := True;
    FEnergy := MONSTER_STARTING_ENERGY;
    FX := 0;
    FY := 0;
    FLastKnownX := 0;
    FLastKnownY := 0;
    FSpeech := MonsterType.Speech;
    FSpeechActions := MonsterType.SpeechActions;
    FActions := MonsterType.Actions;
    FSpeechFrequency := MonsterType.SpeechFrequency;
    FTurnsSinceLastHealthMessage := 0;
    FVerbs := MonsterType.Verbs;
    FRangedVerb := MonsterType.RangedVerb;
    FAC := MonsterType.AC;
    FEV := MonsterType.EV;
    FSpeed := MonsterType.Speed;
    FPoisonLevel := MonsterType.PoisonLevel;
    FMaxHP := MonsterType.HitDice * MONSTER_HITDICE_HP * 2;

    { Some stats are keyed off monster level }
    FMaxHP := FMaxHP + Random(FLevel) - FLevel div 2;
    if (FMaxHP < 1) then
      FMaxHP := 1;
    FXP := Trunc(Power(FLevel, 2)) + MaxHP + Random(FLevel + 1);
    
    FCurrentHP := FMaxHP;
    FAttackType := MonsterType.AttackType;
    FAttackBonus := MonsterType.AttackBonus;
    FAttackDamage := (MonsterType.AttackDamage * 4) div 3;
    FAttackNumber := MonsterType.Attacks;

    FProjectileChar := MonsterType.ProjectileChar;
    FProjectileColour := MonsterType.ProjectileColour;

    { Unique Monsters are more difficult, have better stats, etc }
    if (Unique) then
    begin
      { Boost the Monster's level }
      BoostLevel(UNIQUE_MONSTER_LEVEL_BOOST);

      { Unique monsters have different types of names }
      FUniqueName := Format('%s the %s', [GenerateName,
        GMonsterSuffix[Random(High(GMonsterSuffix))]]);
      FName := UniqueName;
    end
    else if (Boosted) then
    begin
      { Boosted creatures are leaders etc }
      BoostLevel(1);
      case FRace of
        1: FName := 'Massive ' + FName; // Animal
        2: FName := FName + ' Captain'; // Humanoid
        3: FName := 'Gigantic ' + FName; // Construct
        4: FName := FName + ' Vindicator'; // Demon
        5: FName := 'Elder ' + FName; // Dragon
        6: FName := 'Elder ' + FName; // Elemental
        7: FName := 'Large ' + FName; // Plant
        8: FName := FName + ' Reaver'; // Undead
        9: FName := FName + ' Chieftan'; // Giant
        10: FName := 'Primal ' + FName; // Ooze
        11: FName := FName + ' Captain'; // Goblinoid
        12: FName := 'Imperious ' + FName; // Outsider
      end;
    end
    else
    begin
      { Standard Monster }
      FUniqueName := '';
    end;

    { Usually monsters are hostile, although townspeople are a special set of
      monsters who aren't }
    Hostile := True;

    { If a monster archetype can use items, then there is a chance of them being
      generated with either an item or an armour, which could be magical }
    if (FUseItems and Unique) then
    begin
      { Uniques always carry at least one item which is magical }
      if OneChanceIn(COIN_FLIP) then
      begin
        { Generate a magical weapon }
        Item := TItem.Create(iSuperb, FLevel, iWeapon, False);

        { Load the monster with the weapon }
        Item.Location := ionMonster;
        GItemList.Add(Item);
        FWeapon := GItemList.Count - 1;
      end
      else
      begin
        { Or generate a magical armour }
        Item := TItem.Create(iSuperb, FLevel, iArmour, False);

        { Load the monster with the weapon }
        Item.Location := ionMonster;
        GItemList.Add(Item);
        FArmour := GItemList.Count - 1;
      end;
    end
    else if (FUseItems) then
    begin
      { Standard monsters have a lower chance of carrying items }
      if OneChanceIn(MONSTER_CARRYING_ITEM_CHANCE) then
      begin
        if OneChanceIn(COIN_FLIP) then
        begin
          { Generate a weapon which only occasionally is magical }
          if OneChanceIn(MONSTER_MAGICAL_ITEM_CHANCE) then
            Item := TItem.Create(iSuperb, FLevel, iWeapon, False)
          else
            Item := TItem.Create(iCommon, FLevel, iWeapon, False);

          { Load the monster with the weapon }
          Item.Location := ionMonster;
          GItemList.Add(Item);
          FWeapon := GItemList.Count - 1;
        end
        else
        begin
          { Generate an armour which only occasionally is magical }
          if OneChanceIn(MONSTER_MAGICAL_ITEM_CHANCE) then
            Item := TItem.Create(iSuperb, FLevel, iArmour, False)
          else
            Item := TItem.Create(iCommon, FLevel, iArmour, False);

          { Load the monster with the weapon }
          Item.Location := ionMonster;
          GItemList.Add(Item);
          FArmour := GItemList.Count - 1;
        end;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the textual description of the monster }
function TMonster.GetDescription: String;
var
  PercentHealth: Real;
  Quartile: Integer;
  Stem: String;
  Behaviour: String;
  Health: String;
  Ecology: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetDescription()');

  { Default result }
  Result := '';

  { Monster Descriptions are built up out of a number of different attributes }
  try
    { If this is a townsperson, do a special description }
    if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
    begin
      { Build up the return value }
      Result := Trim(Format('This is %s %s.', [Prefix, Name]));
    end
    else
    begin
      { First, the creature category }
      Stem := Format('This %s creature', [FCategory]);

      { Then check its behaviour, if awake}
      if (not(Awake)) then
        Behaviour := 'hasn''t noticed you yet'
      else
      begin
        if (CurrentBehaviour = 'Aggressive') then
          Behaviour := 'will attack you'
        else if (CurrentBehaviour = 'Cowardly') then
          Behaviour := 'will attack you, although it looks slightly scared of you'
        else if (CurrentBehaviour = 'Ranged') then
          Behaviour := 'will attack you mainly from a distance'
        else if (CurrentBehaviour = 'Reckless') then
          Behaviour := 'will attack you single-mindedly'
        else if (CurrentBehaviour = 'Swoop') then
          Behaviour := 'will attack you before retreating to safety'
        else
          Behaviour := '';
      end;

      { Now its health status - we cannot use GetHealthLevel for this since we are
        not interested in the colour associated with the Health Level and also we
        need to indicate if it is unharmed }
      PercentHealth := (FCurrentHP / FMaxHP) * 100;
      Quartile := Trunc(PercentHealth) div 25;

      { Convert a percentage health into a quartile value }
      case Quartile of
        0: Health := 'It is almost dead';
        1: Health := 'It is heavily wounded';
        2: Health := 'It is wounded';
        3: Health := 'It is slightly wounded';
        4: Health := 'It is unharmed';
      end;

      { And finally its ecology }
      if (FHomeEcology = 'Common') then
        Ecology := 'It is generally found throughout the multiverse'
      else
        Ecology := Format('It is native to the %s', [FHomeEcology]);

      { And build up the return value }
      Result := Trim(Format('%s %s. %s. %s.', [Stem, Behaviour, Health,
        Ecology]));
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Spend Energy }
function TMonster.SpendEnergy(EnergyToSpend: Integer): Integer;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TMonster.SpendEnergy(%d)', [EnergyToSpend]));

  { Default result }
  Result := MONSTER_STARTING_ENERGY;

  try
    { Reduce the monster's energy }
    Dec(FEnergy, EnergyToSpend);

    { Return the new energy level }
    Result := Energy;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the relative con descriptor }
function TMonster.GetRelativeDescription(CurrentDungeonLevel: Integer;
  CurrentPlayerLevel: Integer): String;
var
  Con: String;
  Relative: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetRelativeDescription()');

  { Default result }
  Result := '';

  try
    { First the OODness }
    if (CurrentDungeonLevel > FLevel) then
      Relative := 'It is normally found at shallower depths'
    else if (CurrentDungeonLevel = FLevel) then
      Relative := 'It is normally found at this depth'
    else
      Relative := 'It is normally found at deeper depths';

    { Now the Con }
    if (CurrentPlayerLevel > FLevel) then
      Con := 'This should not be too difficult for you to defeat'
    else if (CurrentPlayerLevel = FLevel) then
      Con := 'This would be an even fight for you'
    else
      Con := 'This creature is dangerous and would likely kill you in a fight';

    { And build up the return value }
    Result := Trim(Format('%s. %s.', [Relative, Con]));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if one monster equals another }
function TMonster.Equals(Monster: TMonster): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.Equals()');

  { Default result }
  Result := False;

  try
    { Compare the GUIDs }
    Result := (GUIDToString(GUID) = GUIDToString(Monster.GUID));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return how damaged the monater is, as a qualitative string }
function TMonster.GetDamageLevel(var TextColour: TColor): String;
var
  PercentHealth: Real;
  Quartile: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetDamageLevel()');

  { Default result }
  Result := '';
  TextColour := clWhite;

  try
    { Unlike the method we used in GetDescription, we don't want to return
      anything if the monster is unharmed }
    if (CurrentHP < MaxHP) then
    begin
      PercentHealth := (CurrentHP/MaxHP) * 100;
      Quartile := Trunc(PercentHealth) div 25;

      { Convert a percentage health into a quartile value }
      case Quartile of
        0: Result := 'almost dead';
        1: Result := 'heavily wounded';
        2: Result := 'wounded';
        3: Result := 'slightly wounded';
      end;

      { Set the Text Colour as well }
      case Quartile of
        0: TextColour := clRed;
        1: TextColour := $000080FF;
        2: TextColour := clYellow;
        3: TextColour := clLime;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Periodically, we need to output a monster health status message - if we do,
  we return true from this function, along with the message to display and the
  colour to display it in the message log }
function TMonster.GetMonsterHealthText(var MessageReturned: String;
  var TextColor: TColor): Boolean;
var
  DamageLevel: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetMonsterHealthText()');

  { Default result }
  Result := False;
  MessageReturned := '';
  TextColor := clWhite;

  try
    { Only display health status messages if the monster is in sight }
    if (XCanSeeY(FormDisplay.Game.Dungeon, FormDisplay.Game.PlayerX,
      FormDisplay.Game.PlayerY, X, Y)) then
    begin
      { Check to see we don't display health messages too often }
      if (TurnsSinceLastHealthMessage > MONSTER_MESSAGE_HEALTH_INTERVAL) then
      begin
        { Get the Damage Level and the Text Colour }
        DamageLevel:= GetDamageLevel(TextColor);

        { Only display health status messages if the monster has been harmed }
        if (DamageLevel <> '') then
        begin
          { Build up the message }
          MessageReturned := Trim(Format('%s%s is %s', [Prefix, FName,
            DamageLevel]));
          { Flag that we're returning a message }
          Result := True;

          { Reset the turn counter }
          FTurnsSinceLastHealthMessage := 0;
        end
      end
    end
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the Monster Prefix (which can be 'the') }
function TMonster.GetPrefix: string;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetPrefix()');

  { Default result }
  Result := '';

  try
    { Uniques do not have a predix }
    if (Length(Trim(FUniqueName)) <> 0) then
      Result := ' '
    else
      Result := 'the ';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the singular form of the prefix }
function TMonster.GetSinglePrefix: string;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetSinglePrefix()');

  { Default result }
  Result := '';

  try
    { Again, Uniques do not have prefixes }
    if (Length(Trim(FUniqueName)) <> 0) then
      Result := ' '
    else
    begin
      { Deal with vowels }
      if (Vowel(FName)) then
        Result := 'an '
      else
        Result := 'a ';
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Armour Class is the default armour class plus any AC given by carried items }
function TMonster.GetAC: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetAC()');

  { Default result }
  Result := FAC;

  try
    Result := FAC + GetEnchantmentValue('ENCH_AC');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Attack Bonus is the default attack bonus plus any attack bonus given by
  carried items }
function TMonster.GetAttackBonus: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetAttackBonus()');

  { Default result }
  Result := FAttackBonus;

  try
    Result := FAttackBonus + GetEnchantmentValue('ENCH_HIT');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Damage Bonus is the default damage bonus plus any damage bonus given by
  carried items }
function TMonster.GetDamageBonus: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetDamageBonus()');

  { Default result }
  Result := FAttackDamage;

  try
    Result := FAttackDamage + GetEnchantmentValue('ENCH_DAM');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Evasion is the default evasion plus any evasion given by carried items }
function TMonster.GetEvasion: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetEvasion()');

  { Default result }
  Result := FEV;

  try
    Result := FEV + GetEnchantmentValue('ENCH_EVASION');
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Given an enchantment type, return the magnitude of those enchantments upon
  weapons or armour worn by monsters }
function TMonster.GetEnchantmentValue(Enchantment: String): Integer;
var
  LocalItem: TItem;
  Magnitude: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TMonster.GetEnchantmentValue()');

  { Default result }
  Result := 0;

  try
    { If the monster is carrying a weapon }
    if (FWeapon > NO_ITEM) then
    begin
      { Get a pointer to the weapon it is carrying }
      LocalItem := GItemList[FWeapon] as TItem;

      { Get the magnitude of the enchantment concerned }
      Magnitude := LocalItem.ReturnEnchantmentMagnitude(Enchantment);

      { Handle cursed items }
      if (LocalItem.Cursed) then
        Dec(Result, Magnitude)
      else
        Inc(Result, Magnitude);
    end;

    { If the monster is wearing armour }
    if (FArmour > NO_ITEM) then
    begin
      { Get a pointer to the armour it is wearing }
      LocalItem := GItemList[FArmour] as TItem;

      { Get the magnitude of the enchantment concerned }
      Magnitude := LocalItem.ReturnEnchantmentMagnitude(Enchantment);

      { Handle cursed items }
      if (LocalItem.Cursed) then
        Dec(Result, Magnitude)
      else
        Inc(Result, Magnitude);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random action verb (if defined) }
function TMonster.GetRandomVerb: String;
var
  VerbList: TStringList;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetRandomVerb()');

  { Default result }
  Result := 'hits';

  try
    { If we have verbs defined }
    if (Length(Trim(FVerbs)) > 0) then
    begin
      { Weapons can only 'hit' }
      if (FWeapon > 0) then
        Result := 'hits'
      else
      begin
        { Parse the verb list and break it down into parts }
        VerbList := TStringList.Create;
        VerbList.Delimiter := ',';
        VerbList.QuoteChar := '''';
        try
          { Choose a random verb }
          VerbList.DelimitedText := FVerbs;
          Result := VerbList[Random(VerbList.Count)];
        finally
          VerbList.Free;
        end;
      end;
    end;
    except
      { in case of error, log the Exception }
      on E: Exception do hLog.AddException(E);
    end;
end;

{ Boost a monster up by a number of levels }
procedure TMonster.BoostLevel(LevelGain: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetAC()');

  try
    { Increase the relevant stats }
    Inc(FLevel, LevelGain);
    Inc(FAC, (LevelGain * 2) div 3);
    Inc(FSpeed, LevelGain);
    Inc(FAttackBonus, (LevelGain * 2) div 3);
    Inc(FAttackDamage, (LevelGain * 2) div 3);
    Inc(FMaxHP, (LevelGain * MONSTER_HITDICE_HP) div 2);
    FCurrentHP := FMaxHP;
    if LevelGain < 0 then
      Dec(FXP, Trunc(Power(Abs(LevelGain) + (Random(MONSTER_HITDICE_HP) + 2),
        1.25)))
    else
      Inc(FXP, Trunc(Power(LevelGain + (Random(MONSTER_HITDICE_HP) + 2), 1.25)));
    FMaxHP := FMaxHP div 5;
    FCurrentHP := FCurrentHP div 5;

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
