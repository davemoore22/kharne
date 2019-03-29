{ UnitItem

  Copyright (c) 2007-2009 Dave Moore 

  Item Class and Item Functions

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

unit UnitItem;

interface

uses Contnrs, SysUtils, Dialogs, Graphics, Classes, HotLog, UnitDefines,
  UnitInterfaces, UnitInit, UnitConst;

{ TItemEnchantment defines the magical properties that go onto items - these are
  loaded from the Database and held in an object list }
type TItemEnchantment = Class (TInterfacedObject, ICommonObject)

private
  { Private Members }
  FEnchantmentName: String;
  FEnchantmentLevel: Integer;
  FItemSuffix: String;
  FEnchantmentType: Integer;
  FPlayerDescription: String;
  FPositiveEffect: String;
  FNegativeEffect: String;
  FAccurateDescription: String;
  FActive: Boolean;
  FCursed: Boolean;
  FMagnitude: Integer;
  FScalingFactor: Real;
  FHidden: Boolean;
  FValue: Integer;

  { Private functions to support class properties defined below }
  function GetWearEffect: String;
  function GetRemoveEffect: String;
  function GetModifierText: String;

public
  { Constructors }
  constructor Create(EName: String;
                     Suffix: String;
                     Description: String;
                     PositiveText: String;
                     NegativeText: String;
                     ForceCursed: Boolean = False;
                     Magnitude: Integer = 0;
                     ScalingFactor: Real = 0;
                     Cost: Integer = 0;
                     Hidden: Boolean = False);

  { Interface Method for Persistance }
  function GetStringValue: String;

  { Accessors }
  property Name: String read FEnchantmentName write FEnchantmentName;
  property Level: Integer read FEnchantmentLevel write FEnchantmentLevel;
  property Description: String read FPlayerDescription write FPlayerDescription;
  property EnchantmentType: Integer read FEnchantmentType write FEnchantmentType;
  property ModifierText: String read GetModifierText;
  property ItemSuffix: String read FItemSuffix write FItemSuffix;
  property Magnitude: Integer read FMagnitude write FMagnitude;
  property ScalingFactor: Real read FScalingFactor;
  property FullDescription: String read FAccurateDescription write FAccurateDescription;
  property Active: Boolean read FActive write FActive;
  property Cursed: Boolean read FCursed write FCursed;
  property PositiveEffect: String read FPositiveEffect write FPositiveEffect;
  property NegativeEffect: String read FNegativeEffect write FNegativeEffect;
  property WearEffect: String read GetWearEffect;
  property RemoveEffect: String read GetRemoveEffect;
  property Value: Integer read FValue;
end;

{ TItem Archetype holds the item definitions }
type TItemArchetype = Class (TInterfacedObject, ICommonObject)

private
  { Private Members }
  FItemName: String;
  FItemNamePlural: String;
  FItemType: crItemType;
  FItemWeight: Integer;
  FItemMaterial: crMaterial;
  FItemSlot: crItemSlot;
  FItemModifier: String;
  FItemHandedNess: Integer;
  FItemBaseDurability: Integer;
  FItemDescription: String;
  FItemCost: Integer;
  FItemSymbol: String;    
  FItemColour: TColor;
  FPosition: Integer;
  FPrefix: String;
  FArchetypeID: String;
  FStackable: Boolean;
  FValue: Integer;
  FIded: Boolean;
  FTried: Boolean;
public
  { Public Members }
  List: TObjectList;

  { Constructor }
  constructor Create(ItemName: String; ItemNamePlural: String;
    ItemType: crItemType; ItemWeight: Integer; ItemMaterial: crMaterial;
    ItemSlot: crItemSlot; ItemModifier: String; ItemHandedNess: Integer;
    ItemValue: Integer = 0; ItemDurability: Integer = -1);

  { Interface Method for Persistance }
  function GetStringValue: String;

  { Accessors }
  property ItemName: String read FItemName write FItemName;
  property ItemNamePlural: String read FItemNamePlural write FItemNamePlural;
  property ItemType: crItemType read FItemType write FItemType;
  property ItemWeight: Integer read FItemWeight write FItemWeight;
  property ItemMaterial: crMaterial read FItemMaterial write FItemMaterial;
  property ItemSlot: crItemSlot read FItemSlot write FItemSlot;
  property ItemModifier: String read FItemModifier write FItemModifier;
  property ItemHandedNess: Integer read FItemHandedNess write FItemHandedNess;
  property ItemDurability: Integer read FItemBaseDurability write FItemBaseDurability;
  property ItemDescription: String read FItemDescription write FItemDescription;
  property ItemSymbol: String read FItemSymbol write FItemSymbol;
  property ItemColour: TColor read FItemColour write FItemColour;
  property Position: Integer read FPosition write FPosition;
  property Prefix: String read FPrefix write FPrefix;
  property ArcheTypeID: String read FArcheTypeID write FArcheTypeID;
  property Stackable: Boolean read FStackable write FStackable;
  property Value: Integer read FValue write FValue;
  property Ided: Boolean read FIded write FIded;
  property Tried: Boolean read FTried write FTried;
end;

{ TItem represents individual items, and inherits from TItemArcheType }
type TItem = Class (TItemArchetype, ICommonObject)

private
  { Private Members }
  FEnchantmentList: TObjectList;
  FItemQuality: crItemQuality;
  FMaterial: String;
  FSellable: Boolean;
  FLevel: Integer;
  FDurability: Integer; 
  FIdentified: Boolean;
  FLocation: crItemSlot;
  FKnownDescriptor: String;
  FUnknownDescriptor: String;
  FGivenName: String;
  FName: String;
  FID: String;
  FCount: Integer;
  FValue: Integer;
  FNextItem: Integer;
  FIDCounter: Integer;
  FPseudoIDed: Boolean;

  { Private functions to support class properties defined below }
  function IsBroken: Boolean;
  function IsCursed: Boolean;
  function IsKnown: Boolean;
  function GetCurrentDurability: Integer;
  function GetDurability: Integer;
  function GetName: String;
  function GetSingleName: String;
  function GetWearEffect: String;
  function GetRemoveEffect: String;
  function GetDescriptor: String;
  function GetSuffix: String;
  function GetMaterial: String;
  function GetColour: TColor;
  function GetCost: Integer;
  function GetShortName: String;
  function GetAC: String;
  function GetArmour: Integer;
  function GetDamage: String;
  function GetBonusDamage: String;
  function GetBonusAC: String;
  function GetBaseAC: String;
  function GetBaseDamage: String;
  function GetSpellEnchant: String;
  function GetDamageAsInt: Integer;
  function GetBonusDamageAsInt: Integer;
  function GetSpellCharges: Integer;
  function ConstructName(ShortName: Boolean; Plural: Boolean): String;
public
  { Constructors }
  constructor Create(ItemQuality: crItemQuality;
                     ItemLevel: Integer;
                     ItemType: crItemType;
                     ForceCursed: Boolean = False;
                     ForceDistribution: Boolean = False);

  { Destructor }                    
  destructor Destroy; override;

  { Accessors }
  property Broken: Boolean read IsBroken;
  property Cursed: Boolean read IsCursed;
  property Known: Boolean read IsKnown write FIdentified;
  property Cost: Integer read GetCost;
  property Durability: Integer read GetDurability write FDurability;
  property Name: String read GetName write FName;
  property SingleName: String read GetSingleName;
  property ShortName: String read GetShortName;
  property Location: crItemSlot read FLocation write FLocation;
  property PlayerGivenName: String read FGivenName write FGivenName;
  property ItemName: String read FName;
  property WearEffect: String read GetWearEffect;
  property RemoveEffect: String read GetRemoveEffect;
  property Suffix: String read GetSuffix;
  property KnownDescriptor: String read FKnownDescriptor write FKnownDescriptor;
  property UnknownDescriptor: String read FUnknownDescriptor write FUnknownDescriptor;
  property ItemQuality: crItemQuality read FItemQuality write FItemQuality;
  property Sellable: Boolean read FSellable write FSellable;
  property Level: Integer read FLevel write FLevel;
  property Material: String read FMaterial write FMaterial;
  property ID: String read FID write FID;
  property Count: Integer read FCount write FCount;
  property TextColour: TColor read GetColour;
  property Value: Integer read GetCost write FValue;
  property Damage: Integer read GetDamageAsInt;
  property BonusDamage: Integer read GetBonusDamageAsInt;
  property NextItem: Integer read FNextItem write FNextItem;
  property Armour: Integer read GetArmour;
  property IDCounter: Integer read FIDCounter write FIDCounter;
  property PseudoIDed: Boolean read FPseudoIDed write FPseudoIDed;

  { Get the full description of the item }
  procedure Description(var Desc: TStringList);

  { Get the full effects of the item }
  procedure GetItemEffects(var Desc: TStringList);

  { Add a random enchantment to an item }
  procedure AddRandomEnchantment(EnchantmentLevel: Integer; Cursed: Boolean;
    Chance: Integer = PERCENTAGE);

  { Add a specified enchanbtment to an item }
  procedure AddSpecificEnchantment(EnchantmentName: String;
    EnchantmentLevel: Integer; Cursed: Boolean;  Chance: Integer = PERCENTAGE;
    Hidden: Boolean = False);

  { Add an enchantment to an item - instead of the enchantment type, this will
    add the actual TItemEnchantment }
  procedure AddEnchantment(Enchantment: TItemEnchantment);

  { Check if an item has a specific enchantment }
  function HasEnchantment(Enchantment: TItemEnchantment): Boolean;

  { Return how powerful an enchantment is }
  function ReturnEnchantmentMagnitude(EnchantmentName: String): Integer;

  { Uncurse an item }
  procedure UnCurse;

  { Curse an item }
  procedure Curse;
end;

implementation

uses UnitEngine, UnitFunctions, UnitVars, UnitDisplay;

{ TItemArchetype }

{ Standard Constructor }
constructor TItemArchetype.Create(ItemName: String; ItemNamePlural: String;
  ItemType: crItemType; ItemWeight: Integer; ItemMaterial: crMaterial;
  ItemSlot: crItemSlot; ItemModifier: String; ItemHandedNess: Integer;
  ItemValue: Integer = 0; ItemDurability: Integer = -1);
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TItemArchetype.Create(''%s'')', [ItemName]));

  try
    { Load the private member data }
    FItemName := ItemName;
    FItemNamePlural := ItemNamePlural;
    FItemType := ItemType;
    FItemWeight := ItemWeight;
    FItemMaterial := ItemMaterial;
    FItemSlot := ItemSlot;
    FItemModifier := ItemModifier;
    FItemHandedNess := ItemHandedNess;
    FItemBaseDurability := ItemDurability;
    FValue := ItemValue;
    FIded := False;
    FTried := False;

    { Setup the Item Lists }
    case ItemType of
      iAny: List := nil;
      iWeapon: List := GWeaponList;
      iArmour: List := GArmourList;
      iRing: List := GJewelleryList;
      iWand: List := GWandList;
      iReageant: List := nil;
      iJunk: List := nil;
      iAmulet: List := GJewelleryList;
      iPotion: List := GPotionList;
      iSpellbook: List := nil;
      iScroll: List := GScrollList;
      iMiscellaneous: List := nil;
      iConsumable: List := GFoodList;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Item ArcheType as a String }
function TItemArchetype.GetStringValue: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemArchetype.GetStringValue()');

  Result := ArcheTypeID;
end;

{ TItemEnchantment }

{ Standard Constructor }
constructor TItemEnchantment.Create(EName: String; Suffix, Description,
  PositiveText, NegativeText: String; ForceCursed: Boolean = False;
  Magnitude: Integer = 0; ScalingFactor: Real = 0; Cost: Integer = 0;
  Hidden: Boolean = False);
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TItemEnchantment.Create(''%s'')', [EName]));

  try
    { Load the private member data }
    FEnchantmentName := EName;
    FPlayerDescription := Description;
    FPositiveEffect := PositiveText;
    FNegativeEffect := NegativeText;
    FCursed := ForceCursed;
    FMagnitude := Magnitude;
    FItemSuffix := Suffix;
    FScalingFactor := ScalingFactor;
    FHidden := Hidden;
    FValue := Cost;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Item Modifier }
function TItemEnchantment.GetModifierText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemEnchantment.GetModifierText()');

  { Default result }
  Result := '';

  try
    { Modifier is the Item Suffix } 
    Result := FItemSuffix;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ If you take off an item that is magical in some way, we can optionally
  display an appopriate atmospheric message }
function TItemEnchantment.GetRemoveEffect: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemEnchantment.GetRemoveEffect()');

  { Default result }
  Result := '';

  try
    { The effects when you take off an item depend on whither or not it is
      cursed }
    if FCursed = True then
      Result := FPositiveEffect
    else
      Result := FNegativeEffect;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ If you wear an item that is magical in some way, we can optionally display an
  appopriate atmospheric message }
function TItemEnchantment.GetWearEffect: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemArchetype.GetWearEffect()');

  { Default result }
  Result := '';

  try
    { The effects when you wear an item depend on whither or not it is
      cursed }
    if FCursed = True then
      Result := FNegativeEffect
    else
      Result := FPositiveEffect;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Enchantment as a String }
function TItemEnchantment.GetStringValue: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemEnchantment.GetStringValue()');

  try
    Result := Name;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ TItem }

{ The standard TItem Constructor - we pass in a number of parameters which can
  control the type of item we create }
constructor TItem.Create(ItemQuality: crItemQuality; ItemLevel: Integer;
  ItemType: crItemType; ForceCursed: Boolean; ForceDistribution: Boolean);
var
  Chance: Integer;
  Wearable: Boolean;
  RandomQuality: Integer;
  RandomItemArchetype: TItemArchetype;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.Create()');
  try
    { Magical Items have an associated list of TItemEnchantments }
    FEnchantmentList := TObjectList.Create(True);

    { Create a single, unstacked item }
    FCount := 1;
    FNextItem := NO_NEXT_ITEM;

    { If we haven't passed in a desired Quality level then randomly assign a
      quality. Lower quality items are more common }
    if ItemQuality = iUnknownQuality then
    begin
      Chance := Random(PERCENTAGE);
      if Chance < ITEM_CHANCE_ARTIFACT then
        FItemQuality := iArtifact
      else if Chance < ITEM_CHANCE_LEGENDARY then
        FItemQuality := iLegendary
      else if Chance < ITEM_CHANCE_EPIC then
        FItemQuality := iEpic
      else if Chance < ITEM_CHANCE_SUPERB then
        FItemQuality := iSuperb
      else
        FItemQuality := iCommon;
    end
    else
      FItemQuality := ItemQuality;

    { Optionally generate a random level for the item as well, if it has not
      been passed in }
    if ItemLevel = NO_ITEM_LEVEL then
    begin
      FLevel := Random(MAX_LEVEL) + 1;
    end
    else
    begin
      FLevel := ItemLevel;
    end;

    { Optionally retrieve a random object type}
    if ItemType = iAny then
      RandomItemArchetype := GetRandomItemArchetype
    else if not(ForceDistribution) then
      RandomItemArchetype := GetRandomTypeItemArchetype(ItemType)
    else
    begin
      if ItemType = iScroll then
        RandomItemArchetype := GetSpecificItemArchetype(GScrollList,
          'Scroll of Identify');
      if ItemType = iPotion then
        if OneChanceIn(2) then
          RandomItemArchetype := GetSpecificItemArchetype(GPotionList,
            'Potion of Healing')
        else 
          RandomItemArchetype := GetSpecificItemArchetype(GPotionList,
            'Potion of Extra Healing')
      else
        RandomItemArchetype := GetRandomTypeItemArchetype(ItemType);
    end;
      
    { Load the base (TItemArchetype) class's private members } 
    FItemName := RandomItemArchetype.ItemName;
    FItemNamePlural := RandomItemArchetype.ItemNamePlural;
    FItemType := RandomItemArchetype.ItemType;
    FItemWeight := RandomItemArchetype.ItemWeight;
    FItemMaterial := RandomItemArchetype.ItemMaterial;
    FItemSlot := RandomItemArchetype.ItemSlot;
    FItemQuality := ItemQuality;
    FItemModifier := RandomItemArchetype.ItemModifier;
    FItemHandedNess := RandomItemArchetype.ItemHandedNess;
    FItemBaseDurability := RandomItemArchetype.ItemDurability;
    FItemDescription := RandomItemArchetype.ItemDescription;
    FItemSymbol := RandomItemArchetype.ItemSymbol;
    FItemColour := RandomItemArchetype.ItemColour;
    FPrefix := RandomItemArchetype.Prefix;
    FID := RandomItemArchetype.ArcheTypeID;
    FValue := RandomItemArchetype.Value;

    { For some items types, we have to deal with the random mangling of names,
      for other types we don't }
    case ItemType of
      iRing: FPosition := Random(NUMBER_OF_RINGS);
      iAmulet: FPosition := Random(NUMBER_OF_AMULETS);
      iPotion: FPosition := RandomItemArchetype.Position;
      iWand: FPosition := Random(NUMBER_OF_WANDS);
      iScroll: FPosition := RandomItemArchetype.Position;
    else
      FPosition := RandomItemArchetype.Position;
    end;

    { Some items are stackable }
    case ItemType of
      iPotion: FStackable := True;
      iScroll: FStackable := True;
      iConsumable: FStackable := True;
    else
      FStackable := False;
    end;

    { Now, some item types are of fixed qualities which overrides the quality
      passed in }
    if (FItemType = iScroll) or (FItemType = iPotion) or
      (FItemType = iWand) then
      FItemQuality := iSuperb;

    { Ensure that all Rings and Amulets are magical }
    if (FItemType = iRing) or (FItemType = iAmulet) then
      if (FItemQuality = iWorthless) or (FItemQuality = iCommon) then
        FItemQuality := iSuperb;

    { Set the Material of the item }
    FMaterial := GetMaterial;

    { Some items are wearable }
    Wearable := (FItemType = iWeapon) or (FItemType = iArmour) or
      (FItemType = iAmulet) or (FItemType = iRing);

    { Default Properties for items }
    FIdentified := False;
    FSellable := True;
    FIDCounter := 0;
    FPseudoIDed := False;

    { Now, dependent upon the Item Quality, add some Enchantments and set some
      other properties }
    case FItemQuality of
      iWorthless:
        begin
          { Worthless items are mainly just for atmosphere but are always
            identified and cannot be sold } 
          FIdentified := True;
          FSellable := False;
        end;
      iCommon:
        begin
          { Common items have no magical properties }
        end;
      iSuperb:
        begin
          { Superb items have a single enchantment }
          if Wearable then
          begin
            { Weapons and Armour are set to have specific enchantments }
            if ItemType = iWeapon then
              if OneChanceIn(COIN_FLIP) then
                AddSpecificEnchantment('ENCH_DAM', Trunc(Level *
                  SUPERB_ITEM_SCALING), ForceCursed, PERCENTAGE, True)
              else
                AddSpecificEnchantment('ENCH_HIT', Trunc(Level *
                  SUPERB_ITEM_SCALING), ForceCursed, PERCENTAGE, True)
            else if ItemType = iArmour then
              AddSpecificEnchantment('ENCH_AC', Trunc(Level *
                SUPERB_ITEM_SCALING), ForceCursed, PERCENTAGE, True)
            { Amulets or Rings can have any enchantments }
            else if (ItemType = iAmulet) or (ItemType = iRing)  then
              AddRandomEnchantment(Trunc(Level * SUPERB_ITEM_SCALING),
                ForceCursed);
          end;

          { Wands are special }
          if ItemType = iWand then
            AddSpecificEnchantment('ENCH_SPELL', Trunc(Level *
              SUPERB_ITEM_SCALING), False, PERCENTAGE, True);
        end;
      iLegendary:
        begin
          { Legendary items can have more than one enchantment }
          if Wearable then
          begin
            { Weapons have both damage and hit enchantments }
            if ItemType = iWeapon then
            begin
              AddSpecificEnchantment('ENCH_DAM', Trunc(Level *
                SUPERB_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
              AddSpecificEnchantment('ENCH_HIT', Trunc(Level *
                SUPERB_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
            end
            { Legendary armour is more protective than superb-quality armour }
            else if ItemType = iArmour then
              AddSpecificEnchantment('ENCH_AC', Trunc(Level *
                LEGENDARY_ITEM_SCALING), ForceCursed, PERCENTAGE, True)
            { Amulets or Rings can have any enchantments }
            else if (ItemType = iAmulet) or (ItemType = iRing) then
              AddRandomEnchantment(Trunc(Level * LEGENDARY_ITEM_SCALING),
                ForceCursed);

            { Optionally add a second enchantment }
            if Random(PERCENTAGE) < CHANCEMAGICAL then
              AddRandomEnchantment(Trunc(Level * LEGENDARY_ITEM_SCALING),
                ForceCursed);
          end;
        end;
      iEpic:
      begin
        { Epic items always have more than one enchantment }
        if Wearable then
        begin
          if ItemType = iWeapon then
          begin
            { Weapons have both damage and hit enchantments }
            AddSpecificEnchantment('ENCH_DAM', Trunc(Level *
              LEGENDARY_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
            AddSpecificEnchantment('ENCH_HIT', Trunc(Level *
              LEGENDARY_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
          end
          else if ItemType = iArmour then
            { Epic armour is more protective than legendary-quality armour }
            AddSpecificEnchantment('ENCH_AC',
              Trunc(Level * LEGENDARY_ITEM_SCALING), ForceCursed, PERCENTAGE,
              True)
          { Amulets or Rings can have any enchantments }
          else if (ItemType = iAmulet) or (ItemType = iRing) then
            AddRandomEnchantment(Level, ForceCursed);

          { And add the extra random enchantment }
          AddRandomEnchantment(Trunc(Level * EPIC_ITEM_SCALING), ForceCursed);
        end;
      end;
      iArtifact:
      begin
        { Artifacts have lots of enchantments, and are the most powerful items
          in the game - they are designed to be game-changing }
        if Wearable then
        begin
          if ItemType = iWeapon then
          begin
            { Weapons have both damage and hit enchantments }
            AddSpecificEnchantment('ENCH_DAM', Trunc(Level *
              EPIC_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
            AddSpecificEnchantment('ENCH_HIT', Trunc(Level *
              EPIC_ITEM_SCALING), ForceCursed, PERCENTAGE, True);
          end
          { Artifact armour is extremely protective }
          else if ItemType = iArmour then
            AddSpecificEnchantment('ENCH_AC', Trunc(Level *
              ARTIFACT_ITEM_SCALING), ForceCursed, PERCENTAGE, True)
          else if (ItemType = iAmulet) or (ItemType = iRing)  then
            AddRandomEnchantment(Level, ForceCursed);

          { Add more enchantments }
          AddRandomEnchantment(Level * ARTIFACT_ITEM_SCALING , ForceCursed);
          if OneChanceIn(COIN_FLIP) then
            AddRandomEnchantment(Level * ARTIFACT_ITEM_SCALING, ForceCursed);
          if OneChanceIn(COIN_FLIP) then
            AddRandomEnchantment(Level * ARTIFACT_ITEM_SCALING, ForceCursed);
        end;
      end;
    end;

    { Artifacts have proper names}
    if FItemQuality = iArtifact then
      FName := GenerateName
    else
      FName := RandomItemArchetype.ItemName;

    { Get the descriptors - these interrogate the enchantment types and thus
      these must be set after they are added }
    FKnownDescriptor := GenerateKnownDescriptor(FLevel, FItemType);
    FUnknownDescriptor := GenerateUnknownDescriptor(FItemQuality);

    { Now, in the case of potions and scrolls, ascertain whither or not they
      are currently unknown, tried, or known, as set them }
    if (ItemType = iScroll) and Assigned(FormDisplay.Game) then
      Known := FormDisplay.Game.ScrollIDed[FPosition] in [stKnown]
    else if (ItemType = iPotion) and Assigned(FormDisplay.Game) then
      Known := FormDisplay.Game.PotionIDed[FPosition] in [poKnown];
      
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Destructor }
destructor TItem.Destroy;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.Destroy()');

  try
    { Destroy the Enchantment List }
    FEnchantmentList.Free;
    FEnchantmentList := nil;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the item's current durability }
function TItem.GetDurability: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetDurability()');

  { Default result }
  Result := 0;

  try
    Result := FDurability;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ When we implement durability, this will tell us if an item is broken }
function TItem.IsBroken: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.IsBroken()');

  { Default result }
  Result := False;

  try
    { Check for no durability left }
    Result := (FDurability < 1);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if an item is cursed }
function TItem.IsCursed: Boolean;
var
  LocalEnchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TItem.IsCursed()');

  { Default result }
  Result := False;

  try
    { Look for any cursed enchantments }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment }
      LocalEnchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Check if its cursed }
      if LocalEnchantment.Cursed = True then
      begin
        { If its cursed stop the loop }
        Result := True;
        Break;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Uncurse an item }
procedure TItem.UnCurse;
var
  LocalEnchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.UnCurse()');

  try
    { Look for any cursed enchantments }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment }
      LocalEnchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Check if its cursed }
      if LocalEnchantment.Cursed = True then
      begin
        { If its cursed uncurse it }
        LocalEnchantment.Cursed := False;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if the item is identified }
function TItem.IsKnown: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.IsKnown()');

  { Default result }
  Result := False;

  try
    { Use the FIdentified value }
    Result := FIdentified = True;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the effect that occurs whenever an item is worn or wielded }
function TItem.GetWearEffect: String;
var
  Enchantment: TItemEnchantment;
  Loop: Integer;
  Effect: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetWearEffect()');

  { Default result }
  Result := '';

  try
    { Sort the enchantment list }
    FEnchantmentList.Sort(@CompareMagnitudes);

    { Iterate through the enchantment list returning the player description of
      each enchantment }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      Enchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;
      Effect := 'You feel ' + Enchantment.WearEffect;
      Result := Result + Effect + #13;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Armour Class string for an item }
function TItem.GetAC: String;
var
  ACTotal: Integer;
  BaseAC: Integer;
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetAC()');

  { Default result }
  Result := '';

  try
    { Keep some running totals }
    ACTotal := 0;
    BaseAC := 0;

    { Get the Armour rating }
    if FItemType = iArmour then
      BaseAC := StrToInt(FItemModifier);


    { Iterate through the list of enchantments looking for any armour
      enchantments, and then add these to the base armour }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      Enchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Armour enchantments }
      if Enchantment.Name = 'ENCH_AC' then
        { Handle cursed enchantments }
        if Enchantment.Cursed then
          Dec(ACTotal, Enchantment.Magnitude)
        else
          Inc(ACTotal, Enchantment.Magnitude);
    end;

    { Add more for cursing }
    if Cursed then
      ACTotal := 0 - ACTotal;

    { Format the result }
    if ACTotal > -1 then
      Result := IntToStr(BaseAC) + ', +' + IntToStr(ACTotal)
    else if ACTotal < 0 then
      Result := IntToStr(BaseAC) + ', ' + IntToStr(ACTotal)
    else
      Result :=  IntToStr(BaseAC);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the base armour class of an item }
function TItem.GetBaseAC: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetAttackBonus()');

  { Default result }
  Result := '';

  try
    { Only for armour }
    if FItemType = iArmour then
      Result := FItemModifier;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the damage string }
function TItem.GetDamage: String;
var
  BaseDamage: String;
  DamageTotal: Integer;
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetDamage()');

  { Default result }
  Result := '';

  { Keep a running total }
  DamageTotal := 0;

  try
    { Get the base weapon type, then add any damage enchantments to the string }
    if FItemType = iWeapon then
      BaseDamage := FItemModifier;

    { Look for damage enchantments }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);
      if (Enchantment.Name = 'ENCH_DAM') then
        { Handle cursed enchantments }
        if (Enchantment.Cursed) then
          Dec(DamageTotal, Enchantment.Magnitude)
        else
          Inc(DamageTotal, Enchantment.Magnitude);
    end;

    { Format the result }
    if DamageTotal > -1 then
      Result := Format('%s+%d', [BaseDamage, DamageTotal])
    else if DamageTotal < 0 then
      Result := Format('%s%d', [BaseDamage, DamageTotal])
    else
      Result := Format('%s', [BaseDamage]);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the base damage string of the weapon }
function TItem.GetBaseDamage: String;
var
  BaseDamage: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetBaseDamage()');

  { Default result }
  Result := '';

  try
    { Only get it for weapons }
    if FItemType = iWeapon then
      Result := FItemModifier;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the suffix of the item }
function TItem.GetSuffix: String;
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetSuffix()');

  { Default result }
  Result := '';

  try
    { Sort the enchantment list into highest-magnitude first order }
    FEnchantmentList.Sort(@CompareMagnitudes);

    { Return the first non-hidden enchantment }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      if (FEnchantmentList.Items[Loop] as TItemEnchantment).FHidden = False then
      begin
        Result := (FEnchantmentList.Items[Loop] as TItemEnchantment).ItemSuffix;
        Break;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Construct the name of the item - note that this should never be called by any
  external routine outside of TItem }
function TItem.ConstructName(ShortName: Boolean; Plural: Boolean): String;
var
  ItemPrefix: String;
  Descriptor: String;
  Material: String;
  ItemName: String;
  ItemSuffix: String;
  Cursed: String;
  Pseudo: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.ConstructName()');

  { Default result }
  Result := '';

  try
    { Build the name out of various optional components so we initialise these }
    ItemPrefix := '';
    Descriptor := '';
    Material := '';
    ItemName := '';
    ItemSuffix := '';
    Cursed := '';
    Pseudo := '';

    { Handle item descriptors }
    if not(FIdentified) and (FItemType in [iWeapon, iArmour, iRing, iAmulet,
      iWand]) then
      Descriptor := Format('%s ', [FUnknownDescriptor]);

    if Descriptor = ' ' then
      Descriptor := '';

    { Handke item material }
    if not(ShortName) and (FItemType in [iWeapon, iArmour, iRing, iAmulet,
      iWand]) then
      Material := Format('%s ', [FMaterial]);

    { Now the item names, which can be special }
    if (Plural) and (FCount > 1) then
    begin
      case FItemType of
        { Handle potions }
        iPotion:
        begin
          case FormDisplay.Game.PotionIDed[FPosition] of
            poUnknown: ItemName := Format('%s %s Potions', [Trim
              (GPotionItemDescriptorArray[FPosition]),
              Trim(GPotionColourItemDescriptorArray[FPosition])]);
            poTried: ItemName := Format('%s %s Potions {tried}', [Trim
              (GPotionItemDescriptorArray[FPosition]),
              Trim(GPotionColourItemDescriptorArray[FPosition])]);
            poKnown: ItemName := Format('%s ', [FItemNamePlural]);
          end
        end;
        { Handle scroll identification }
        iScroll:
        begin
          case FormDisplay.Game.ScrollIDed[FPosition] of
            stUnknown: ItemName := Format('Scrolls labelled "%s"', [Trim
            (FormDisplay.Game.ScrollNameArray[FPosition])]);
            stTried: ItemName := Format('Scrolls labelled "%s" {tried}',
              [Trim(FormDisplay.Game.ScrollNameArray[FPosition])]);
            stKnown: ItemName := Format('%s ', [FItemNamePlural]);
          end
        end
      else
        ItemName := Format('%s ', [FItemNamePlural]);
      end;
    end
    else
    begin
      case FItemType of
        { Handle potions }
        iPotion:
        begin
          case FormDisplay.Game.PotionIDed[FPosition] of
            poUnknown: ItemName := Format('%s %s Potion', [Trim
              (GPotionItemDescriptorArray[FPosition]),
              Trim(GPotionColourItemDescriptorArray[FPosition])]);
            poTried: ItemName := Format('%s %s Potion {tried}', [Trim
              (GPotionItemDescriptorArray[FPosition]),
              Trim(GPotionColourItemDescriptorArray[FPosition])]);
            poKnown: ItemName := Format('%s ', [FItemName]);
          end;
        end;
        { Handle scroll identification }
        iScroll:
        begin
          case FormDisplay.Game.ScrollIDed[FPosition] of
            stUnknown: ItemName := Format('Scroll labelled "%s"', [Trim
            (FormDisplay.Game.ScrollNameArray[FPosition])]);
            stTried: ItemName := Format('Scroll labelled "%s" {tried}',
              [Trim(FormDisplay.Game.ScrollNameArray[FPosition])]);
            stKnown: ItemName := Format('%s ', [FItemName]);
          end
        end
      else
        ItemName := Format('%s ', [FItemName]);
      end;
    end;

    { Now the item suffix }
    if FIdentified then
    begin
      case FItemType of
        iWeapon: ItemSuffix := Format('[%s] %s ', [GetBonusDamage, GetSuffix]);
        iArmour: ItemSuffix := Format('[%s] %s ', [GetBonusAC, GetSuffix]);
        iRing: ItemSuffix := Format('%s ', [GetSuffix]);
        iAmulet: ItemSuffix := Format('%s ', [GetSuffix]);
        iWand: ItemSuffix := Format('of %s [%d] ', [GetSpellEnchant,
          GetSpellCharges]);
      end;
    end;

    { And finally the cursed indicator }
    if (IsCursed) and (FIdentified) then
      Cursed := 'Cursed ';

    { And build up the items, apart from artifacts, in which case we discard
      all we've done so far and give them a proper name }
    if (FItemQuality = iArtifact) and (FIdentified) then
      if ShortName then
        Result := Format('%s', [FName])
      else
        Result := Format('the ancient artifact "%s"', [FName])
    else
    begin
      if ShortName then
        Result := Trim(Format('%s%s%s', [Cursed, ItemName, ItemSuffix]))
      else
      begin
        if Cursed = 'Cursed ' then
          Cursed := '{Cursed} ';
        Result := Trim(Format('%s%s%s%s%s', [Descriptor, Material, ItemName,
          ItemSuffix, Cursed]));
      end;
    end;

    { Handle plural items }
    if (Plural) and (FCount > 1) then
      ItemPrefix := Format('%d ', [FCount])
    { Handle the prefixes of normal items }
    else if FItemQuality <> iArtifact then
    begin
      if (ShortName) then
        if (FPrefix = 'a') and Vowel(Result) then
          ItemPrefix := 'an '
        else if (FPrefix = 'an') and Vowel(Result) then
          ItemPrefix := 'an '
        else if (FPrefix <> 'some') then
          ItemPrefix := 'a '
        else
          ItemPrefix := 'some '
      else
        if (FPrefix = 'a') and Vowel(Result) then
          ItemPrefix := 'an '
        else if (FPrefix = 'an') and Vowel(Result) then
          ItemPrefix := 'an '
        else if (FPrefix <> 'some') then
          ItemPrefix := 'a '
        else
          ItemPrefix := 'some ';
    end
    { Handle the prefixes of unidentified artifacts }
    else if not(FIdentified) then
      ItemPrefix := 'an ';

    { Change the first letter to uppercase }
    Result := Trim(Format('%s%s', [ItemPrefix, Result]));
    Result[1] := UpperCase(Copy(Result, 1, 1))[1];

    { And finally, if we have an unidentified, pseudoided magical item, add the
      pseudoid suffix }
    if (not(FIdentified)) and (PseudoIDed) and (FItemQuality in [iSuperb,
      iLegendary, iEpic, iArtifact]) then
    begin
      { Get the category }
      case FItemQuality of
        iCommon: Pseudo := '{average}';
        iSuperb: Pseudo := '{superb}';
        iLegendary: Pseudo := '{legendary}';
        iEpic: Pseudo := '{epic}';
        iArtifact: Pseudo := '{artifact}';
      end;

      { If we have a suffix, add it }
      if Pseudo <> '' then
        Result := Format('%s %s', [Result, Pseudo]);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the single version of the name }
function TItem.GetSingleName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TMonster.GetAttackBonus()');

  { Default result }
  Result := '';

  try
    { Return the single name }
    Result := ConstructName(False, False);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the full name of the item }
function TItem.GetName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetName()');

  { Default result }
  Result := '';

  try
    { Return the name }
    Result := ConstructName(False, True);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{  Return the tense name of the item }
function TItem.GetShortName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetShortName()');

  { Default result }
  Result := '';

  try
    { Return the shortname }
    Result := ConstructName(True, True);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the effect that occurs whenever an item is removed from being worn or
  wielded }
function TItem.GetRemoveEffect: String;
var
  Enchantment: TItemEnchantment;
  Loop: Integer;
  Effect: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetRemoveEffect()');

  { Default result }
  Result := '';

  try
    { Sort the enchantment list }
    FEnchantmentList.Sort(@CompareMagnitudes);

    { Iterate through the enchantment list returning the player description of
      each enchantment }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);
      Effect := 'You feel ' + Enchantment.RemoveEffect;
      Result := Result + Effect + #13;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the current item durability }
function TItem.GetCurrentDurability: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetCurrentDurability()');

  { Default result }
  Result := 0;

  try
    { Get the current durability }
    Result := Durability;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Descriptor }
function TItem.GetDescriptor: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetDescriptor()');

  { Default result }
  Result := '';

  try
    { Deal with Known/Unknown items }
    if FIdentified then
      Result := FKnownDescriptor
    else
      Result := FUnknownDescriptor;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the material an item is made out of }
function TItem.GetMaterial: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetMaterial()');

  { Default result }
  Result := '';

  try
    { Return the material as a string }
    case ItemMaterial of
      mWooden: Result := GWoodenItemDescriptorArray[Random(High(
        GWoodenItemDescriptorArray) + 1)];
      mLeather: Result := GLeatherItemDescriptorArray[Random(High(
        GLeatherItemDescriptorArray) + 1)];
      mStone: Result := GStoneItemDescriptorArray[Random(High(
        GStoneItemDescriptorArray) + 1)];
      mMetal: Result := GMetalItemDescriptorArray[Random(High(
        GMetalItemDescriptorArray) + 1)];
      mCloth: Result := GClothItemDescriptorArray[Random(High(
        GClothItemDescriptorArray) + 1)];
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Work out the cost of an item }
function TItem.GetCost: Integer;
var
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetCost()');

  { Default result }
  Result := FValue;

  try
    { Enchantments add value }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      Enchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Calculate the Value of the enchantment }
      Result := Result + Enchantment.Value * (Enchantment.Magnitude +
          Level * 2);

      { Deal with Cursed Items }
      if Enchantment.Cursed then
        Result := Result div 3;
    end;

    { Scale the Item Value up by the Item Quality }
    case ItemQuality of
      iUnknownQuality: Result := Result * 1;
      iWorthless: Result := Result * 1;
      iCommon: Result := Result * 1;
      iSuperb: Result := (Result * 3) div 2;
      iLegendary: Result := Result * 2;
      iEpic: Result := (Result * 5) div 2;
      iArtifact: Result := Result * 3;
      iSpecial: Result := Result * 1;
    end;

    { Reduce the Item Cost by a fixed factor }
    Result := Trunc(Result div ITEM_COST_FACTOR);

    { Make sure items cost at least 1 gp }
    if Result = 0 then
      Result := 1;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Item Effects }
procedure TItem.GetItemEffects(var Desc: TStringList);
var
  Loop: Integer;
  Cursed: Boolean;
  Description: String;
  Enchantment: TItemEnchantment;
  Magnitude: String;
  ActionText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetItemEffects()');

  try
    if FIdentified then
    begin
      { If we have any effects, add a blank line to space them out }
      FEnchantmentList.Sort(@CompareMagnitudes);
      if FEnchantmentList.Count <> 0 then
        Desc.Add('');

      { Iterate through the enchantment lists returning the player description
        of each }
      for Loop := 0 to FEnchantmentList.Count - 1 do
      begin
        { Load the enchantment for examination }
        Enchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

        { Handle Cursed Enchantments }
        if Enchantment.Cursed then
        begin
          ActionText := 'decreases';
          if Enchantment.ScalingFactor <> 0 then
            Magnitude := Format('(-%d)', [Enchantment.Magnitude])
          else
            Magnitude := '';
        end
        else
        begin
          ActionText := 'increases';
          if Enchantment.ScalingFactor <> 0 then
            Magnitude := Format('(+%d)', [Enchantment.Magnitude])
          else
            Magnitude := '';
        end;

        { Build up the effect line }
        if Enchantment.Magnitude = 0 then
          Magnitude := '';

        Description := Format(Enchantment.Description + ' %s', [ActionText, Magnitude]);

        { Add it }
        Desc.Add(Description);
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate the Item Description (which includes the item effects) }
procedure TItem.Description(var Desc: TStringList);
var
  Loop: Integer;
  Cursed: Boolean;
  Description: String;
  Enchantment: TItemEnchantment;
  Magnitude: String;
  ActionText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.Description()');

  try
    { Clear the Item Description Buffer }
    Desc.Clear;

    { First thing we add is the location }
    if FLocation = iInventory then
      Desc.Add('Location: Backpack')
    else if FLocation = iFloor then
      Desc.Add('Location: Floor')
    { If its not the inventory or the floor then the item must be worn or
      wielded }
    else if ItemType = iWeapon then
      Desc.Add('Location: Wielded')
    else
      Desc.Add('Location: Worn');

    { Now add the Item Type Informaton - this is different for the various item
      types }
    case ItemType of
      iWeapon:
        begin
          { Weapon has damage and type }
          if ItemSlot = iRanged then
          begin
            { Ranged Weapons }
            Desc.Add('Type: Ranged Weapon');
            Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
            Desc.Add('Slot: Ranged/Throwing');
          end
          else
          begin
            { Melee Weapons }
            Desc.Add(Format('Type: %d-handed Weapon', [FItemHandedNess]));
            Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
            Desc.Add('Slot: Main Hand');
          end;

          { Add Damage if the item is known }
          if FIdentified then
            Desc.Add(Format('Damage: %s', [GetDamage]))
          else
            Desc.Add(Format('Damage: %s', [GetBaseDamage]))
        end;
      iArmour:
        begin
          { Armour has armour and slot }
          Desc.Add('Type: Armour/Clothing');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));

          { Deal with the individual slots }
          case FItemSlot of
            iHead: Desc.Add('Slot: Head');
            iChest: Desc.Add('Slot: Chest');
            iHands: Desc.Add('Slot: Hands');
            iArms: Desc.Add('Slot: Arms');
            iLegs: Desc.Add('Slot: Legs');
            iFeet: Desc.Add('Slot: Feet');
            iOffhand: Desc.Add('Slot: Offhand');
            iRanged: Desc.Add('Slot: Ranged/Throwning');
          end;

          { If the item gives armour class then display that information }
          if ((StrToIntDef(GetAC, 1) <> 0) or
            (StrToIntDef(GetBaseAC, 1) <> 0)) then
            if (FIdentified) then
              Desc.Add(Format('Armour Rating: %s', [GetAC]))
            else
              Desc.Add(Format('Armour Rating: %s', [GetBaseAC]));
        end;
      iRing:
        begin
          { Jewellery doesn't have much other information }
          Desc.Add('Type: Jewellery');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
          Desc.Add('Slot: Fingers');
        end;
      iWand:
        begin
          { Wands aren't implemented properly yet }
          Desc.Add('Type: Wand');
          Desc.Add('Slot: Usable');

          { Deal with IDed and UnIDed items }
          if FIdentified then
          begin
            Desc.Add('Spell:');
            Desc.Add('Maximum Charges:');
            Desc.Add('Charges Left:');
          end
          else
          begin
            Desc.Add('Spell: Unknown');
            Desc.Add('Maximum Charges:  Unknown');
            Desc.Add('Charges Left:  Unknown');
          end;
        end;
      iAmulet:
        begin
          { Jewellery doesn't have much other information }
          Desc.Add('Type: Jewellery');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
          Desc.Add('Slot: Neck');
        end;
      iPotion:
        begin
          { Potions don't have much information }
          Desc.Add('Type: Potion');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
          Desc.Add('Slot: Usable');

          { Deal with IDed and UnIDed items }
          if FIdentified then
            Desc.Add('Effect:')
          else
            Desc.Add('Effect: Unknown');
        end;
      iScroll:
        begin
          { Now handle Scrolls }
          Desc.Add('Type: Scroll');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
          Desc.Add('Slot: Usable');

          { Deal with IDed and UnIDed items }
          if FIdentified then
            Desc.Add('Effect:')
          else
            Desc.Add('Effect: Unknown');
        end;
      iConsumable:
        begin
          { Food }
          Desc.Add('Type: Food/Drink');
          Desc.Add(Format('Weight: %d lbs', [FItemWeight]));
          Desc.Add('Slot: Usable');
          Desc.Add('Effect: Satisfies Hunger');
        end;
    end;

    { Now add the magical effects, if we have identified the item }
    GetItemEffects(Desc);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the colour used to display the item }
function TItem.GetColour: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TItem.GetColour()');

  { Default result }
  Result := clWhite;

  try
    { Known items are coloured differently }
    if (FIdentified) then
    begin
      { Go through the various item qualities }
      case FItemQuality of
        iWorthless: Result := clSilver;
        iCommon: Result := clWhite;
        iSuperb: Result := clGreen;
        iLegendary: Result := clblue;
        iEpic: Result := $00FF0080;
        iArtifact: Result := $000080FF;
      else
        Result := clSilver;
      end;
    end
    else
    begin
      { Unknown items are dependent upon the material they are made from }
      case ItemMaterial of
        mMetal: Result := $00808040;
        mLeather: Result := $002C5594;
        mCloth: Result := $00FE81CC;
        mWooden: Result := $0009B6CE;
        mOrganic: Result := clGreen;
        mStone: Result := $00FF8080;
      else
        Result := clSilver;
      end;
    end;

    { Overwrite these for Potions }
    if FItemType = iPotion then
      Result := GPotionColourArray[FPosition]
    else if FItemType = iConsumable then
      Result := FItemColour;

    { Cursed items are displayed in red }
    if (Cursed) and (FIdentified) then
      Result := clRed;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the fancy version of the extra hot and damage an item can do }
function TItem.GetBonusDamage: String;
var
  DamageTotal: Integer;
  Enchantment: TItemEnchantment;
  HitTotal: Integer;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetBonusDamage()');

  { Default result }
  Result := '';

  try
    { Working Variables }
    HitTotal := 0;
    DamageTotal := 0;

    { Do the first half of the result }

    { Iterate through the list of enchantments looking for any hit
      enchantments, and then add these to the hit total }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { Handle cursed items }
      if Enchantment.Name = 'ENCH_HIT' then
        if Enchantment.Cursed then
          Dec(HitTotal, Enchantment.Magnitude)
        else
          Inc(HitTotal, Enchantment.Magnitude);
    end;

    { Format the first half of the Result }
    if HitTotal > -1 then
      Result := Format('+%d', [HitTotal])
    else
      Result := Format('%d', [HitTotal]);

    { Do the second half of the result }

    { Iterate through the list of enchantments looking for any damage
      enchantments, and then add these to the damage total }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { Handle cursed items }
      if (Enchantment.Name = 'ENCH_DAM') then
      begin
        if (Enchantment.Cursed) then
          Dec(DamageTotal, Enchantment.Magnitude)
        else
          Inc(DamageTotal, Enchantment.Magnitude);
      end;
    end;

    { Format the second half of the Result and add it to the first half}
    if (DamageTotal > -1) then
      Result := Format('%s, +%d', [Result, DamageTotal])
    else
      Result := Format('%s, %d', [Result, DamageTotal]);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the bonus damage of an item in a form we can actually use for
  calculations }
function TItem.GetBonusDamageAsInt: Integer;
var
  DamageTotal: Integer;
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetBonusDamageAsInt()');

  { Default result }
  Result := 0;

  try
    { Iterate through the list of enchantments looking for any armour
      enchantments, and then add this to the base armour }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { Handle cursed items }
      if Enchantment.Name = 'ENCH_DAM' then
        if Enchantment.Cursed then
          Dec(Result, Enchantment.Magnitude)
        else
          Inc(Result, Enchantment.Magnitude);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Bonus AC of an item as a string }
function TItem.GetBonusAC: String;
var
  ACTotal: Integer;
  BaseAC: Integer;
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetBonusAC()');

  { Default result }
  Result := '';

  try
    { Working Variables }
    ACTotal := 0;
    BaseAC := 0;

    { Only Armour Items have any sort of Base Armour rating }
    if FItemType = iArmour then
      BaseAC := StrToInt(FItemModifier);

    { Iterate through the list of enchantments looking for any armour
      enchantments, and then add this to the base armour }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { The Enchantment magnitude is the additional armour rating }
      if Enchantment.Name = 'ENCH_AC' then
        if Enchantment.Cursed then
          Dec(ACTotal, Enchantment.Magnitude)
        else
          Inc(ACTotal, Enchantment.Magnitude);
    end;

    { Format the result }
    if ACTotal > -1 then
      Result := Format('%d, +%d', [BaseAC, ACTotal])
    else
      Result := Format('%d, %d', [BaseAC, ACTotal]);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the armour of an item }
function TItem.GetArmour: Integer;
var
  ACTotal: Integer;
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TItem.GetArmour()');

  { Default result }
  Result := 0;

  try
    { Only Armour Items have any sort of Base Armour rating }
    if FItemType = iArmour then
      Result := StrToInt(FItemModifier);

    { Iterate through the list of enchantments looking for any armour
      enchantments, and then add this to the base armour value }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { The Enchantment magnitude is the additional armour rating }
      if Enchantment.Name = 'ENCH_AC' then
        { Handle cursed items as well }
        if Enchantment.Cursed then
          Dec(ACTotal, Enchantment.Magnitude)
        else
          Inc(ACTotal, Enchantment.Magnitude);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the number of charges on a wand }
function TItem.GetSpellCharges: Integer;
var
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetSpellCharges()');

  { Default result }
  Result := 0;

  try
    { Look for a Spell Enchant }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { The Enchantment magnitude is the charges }
      if Enchantment.Name = 'ENCH_SPELL' then
        Result :=  Result + Enchantment.Magnitude;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the spell enchant (used in wands) from this item }
function TItem.GetSpellEnchant: String;
var
  Enchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetSpellEnchant()');

  { Default result }
  Result := '';

  try
    { Iterate through the enchantments for this item }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      Enchantment := (FEnchantmentList.Items[Loop] as TItemEnchantment);

      { Check the enchantment }
      if Enchantment.Name = 'ENCH_SPELL' then
        Result := Result + Enchantment.ItemSuffix;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;    
end;

{ Return the Bonus Damage on this item as an Integer }
function TItem.GetDamageAsInt: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.GetDamageAsInt()');

  { Default result }
  Result := 0;

  try
    { Get the first part of the modifier text }
    Result := StrToIntDef(Copy(FItemModifier, 3, 3), 0);

    { Handle curses }
    if Cursed then
      Result := 0 - Result;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end; 
end;

{ Add an enchantment to an item }
procedure TItem.AddEnchantment(Enchantment: TItemEnchantment);
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.AddEnchantment()');

  try
    { Add the enchantment }
    FEnchantmentList.Add(Enchantment);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end; 
end;

{ Determine the total magnitude of all the enchantments of a certain type }
function TItem.ReturnEnchantmentMagnitude(EnchantmentName: String): Integer;
var
  LocalEnchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  // hLog.Add(Format('{now} {lNum} TItem.ReturnEnchantmentMagnitude(''%s'')',
  //  [EnchantmentName]));

  { Default result }
  Result := 0;

  try
    { Iterate through all enchantments }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment for examination }
      LocalEnchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Check the enchantment }
      if LocalEnchantment.FEnchantmentName = EnchantmentName then
        Inc(Result, LocalEnchantment.Magnitude);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end; 
end;

{ Determines if an Item has a specific enchantment }
function TItem.HasEnchantment(Enchantment: TItemEnchantment): Boolean;
var
  LocalEnchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.HasEnchantment()');

  { Default result }
  Result := False;

  try
    { Start the beginning }
    Loop := 0;

    { Iterate through the Item's Enchantment List looking for the specified
      enchantment }
    repeat
      { Load the enchantment for examination }
      LocalEnchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Check more than one field just in case there are multiple enchantments
        of the same type on the item }
      if (LocalEnchantment.Name = Enchantment.Name) and
         (LocalEnchantment.EnchantmentType = Enchantment.EnchantmentType) and
         (LocalEnchantment.Level = Enchantment.Level) then
        Result := True;
      Inc(Loop);
    until Result or (Loop>= FEnchantmentList.Count - 1);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end; 
end;

{ Add a random enchantment to an item }
procedure TItem.AddRandomEnchantment(EnchantmentLevel: Integer;
  Cursed: Boolean; Chance: Integer);
var
  LevelToUse: Real;
  EnchantToUse: TItemEnchantment;
  MyEnchantment: TItemEnchantment;
  MagnitudeScalingFactor: Real;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.AddRandomEnchantment()');

  try
    { We can optionally give a percentage of adding this enchantment }
    if Random(PERCENTAGE) < Chance then
    begin
      { Get a random enchantment (ENCH_SPELL is used only for Wands and thus we
        do not want to use it here }
      repeat
        EnchantToUse := (GEnchantmentList.Items[Random(GEnchantmentList.Count)])
          as TItemEnchantment;
      until EnchantToUse.FEnchantmentName <> 'ENCH_SPELL';

      { Some enchantments vary in effectiveness and strength }
      MagnitudeScalingFactor := EnchantToUse.ScalingFactor;

      { Magnitude is based upon level, or +/- 1 }
      LevelToUse := (EnchantmentLevel + (Random(3) - 1)) *
        MagnitudeScalingFactor;
      if (LevelToUse < 1) then
        LevelToUse := 1;

      { Create the enchantment }
      MyEnchantment := TItemEnchantment.Create(EnchantToUse.Name,
        EnchantToUse.ModifierText, EnchantToUse.Description,
        EnchantToUse.PositiveEffect, EnchantToUse.NegativeEffect, Cursed,
        Round(LevelToUse), MagnitudeScalingFactor, EnchantToUse.FValue);

      { Add the enchantment to the item }
      FEnchantmentList.Add(MyEnchantment);

      { Set the ID }
      FID := FID + EnchantToUse.Name;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end; 
end;

{ Add a specific enchantment to an item - used mainly for weapons (+hit, +dam)
  or armour (+ac) }
procedure TItem.AddSpecificEnchantment(EnchantmentName: String;
  EnchantmentLevel: Integer; Cursed: Boolean; Chance: Integer = PERCENTAGE;
  Hidden: Boolean = False);
var
  Charges: Integer;
  LevelToUse: Real;
  Loop: Integer;
  EnchantToUse: TItemEnchantment;
  MyEnchantment: TItemEnchantment;
  MagnitudeScalingFactor: Real;
  SchoolToUse: Integer;
  Spell: String;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TItem.AddSpecificEnchantment(''%s'')',
    [EnchantmentName]));

  try
    { We can optionally give a percentage of adding this enchantment }
    if (Random(PERCENTAGE) < Chance) then
    begin
      { Iterate through the list of enchantments looking for the particular
        enchantment to add }
      for Loop := 0 to GEnchantmentList.Count - 1 do
      begin
        EnchantToUse := (GEnchantmentList.Items[Loop]) as TItemEnchantment;
        if EnchantToUse.Name = EnchantmentName then
        begin
          { For spell enchants (i.e. wands), we do things differently }
          if EnchantmentName = 'ENCH_SPELL' then
          begin
            { School Selection is random - note that SCH_TRAVEL is the highest
              school at the moment }
            SchoolToUse := Random(SCH_TRAVEL + 1);

            { Here enchantment level indicates the spell }
            Dec(EnchantmentLevel);
            if (EnchantmentLevel > 8) then
              EnchantmentLevel := 8;
            if (EnchantmentLevel < 1) then
              EnchantmentLevel := 1;

            { Set the spell and charges }
            Spell := GMagic[SchoolToUse, EnchantmentLevel];
            Charges := Random(STARTING_WAND_CHARGES) + MINIMUM_WAND_CHARGES;

            { Enchantment isn't really meant to be used for Spell Enchants on
              Wands, but we can use it anyway }
            MyEnchantment := TItemEnchantment.Create(EnchantToUse.Name, Spell,
              '', '', '', False, Charges, EnchantmentLevel,
              EnchantToUse.FValue * Charges, True);

            { Add the enchantment }
            FEnchantmentList.Add(MyEnchantment);

            { Set the ID }
            FID := FID + Spell;

            { Break out }
            Break;
          end
          else
          begin
            { Set the Enchant Level }
            MagnitudeScalingFactor := EnchantToUse.ScalingFactor;
            LevelToUse := (EnchantmentLevel + (Random(3) - 1)) *
              MagnitudeScalingFactor;
            if (LevelToUse < 1) then
              LevelToUse := 1;

            { Add the enchantment }
            MyEnchantment := TItemEnchantment.Create(EnchantToUse.Name,
              EnchantToUse.ModifierText, EnchantToUse.Description,
              EnchantToUse.PositiveEffect, EnchantToUse.NegativeEffect, Cursed,
              Round(LevelToUse), MagnitudeScalingFactor, EnchantToUse.FValue,
              Hidden);

            { Add the enchantment }
            FEnchantmentList.Add(MyEnchantment);

            { Set the ID }
            FID := FID + EnchantToUse.Name;

            { Break out }
            Break;
          end;
        end;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Curse an item }
procedure TItem.Curse;
var
  LocalEnchantment: TItemEnchantment;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItem.Curse()');

  try
    { Look for any cursed enchantments }
    for Loop := 0 to FEnchantmentList.Count - 1 do
    begin
      { Load the enchantment }
      LocalEnchantment := FEnchantmentList.Items[Loop] as TItemEnchantment;

      { Check if its cursed }
      if LocalEnchantment.Cursed = False then
      begin
        { If its not cursed curse it }
        LocalEnchantment.Cursed := True;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
