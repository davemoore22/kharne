{ UnitInit

  Copyright (c) 2007-2009 Dave Moore 

  Initialisation Routines

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

unit UnitInit;

interface

uses Contnrs, Forms, SysUtils, Classes, Dialogs, Math, Controls, Graphics,
  Hotlog, UnitDefines, UnitConst, UnitDataModule;

{ Create the Global Data Structure }
procedure CreateGlobalData;

{ Populate the Global Data Structure }
procedure PopulateGlobalData(UseDB: Boolean = False);

{ Empty the Global Data Structure }
procedure FreeGlobalData;

{ Set up Dungeons }
procedure SetupDungeons;

{ Set up Magic }
procedure SetupMagic;

implementation

uses UnitItem, UnitDungeon, UnitMonster, UnitVault, UnitDisplay, UnitFunctions,
  UnitVars;

{ Set up Magic }
procedure SetupMagic;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitInit.SetupMagic()');

  try
    { School of Fire }
    GMagicColour[SCH_FIRE] := clRed;
    GMagic[SCH_FIRE, 0] := 'Tome of Flames';
    GMagic[SCH_FIRE, 1] := 'Fire Bolt';
    GMagic[SCH_FIRE, 2] := 'Lesser Fire Resistance';
    GMagic[SCH_FIRE, 3] := 'Fire Blast';
    GMagic[SCH_FIRE, 4] := 'Fireball';
    GMagic[SCH_FIRE, 5] := 'FireStorm';
    GMagic[SCH_FIRE, 6] := 'Greater Fire Resistance';
    GMagic[SCH_FIRE, 7] := 'Weakness to Fire';
    GMagic[SCH_FIRE, 8] := 'Flaming Agony';
    GMagicDescription[SCH_FIRE, 0] := 'Spells that manipulate the raw elemental fury of fire, often causing great offensive damage';
    GMagicDescription[SCH_FIRE, 1] := 'Launches a bolt of fire that causes a small amount of fire damage to a creature';
    GMagicDescription[SCH_FIRE, 2] := 'Temporarily increases slightly your resistance to elemental fire';
    GMagicDescription[SCH_FIRE, 3] := 'Launches a blast of fire that causes a medium amount of fire damage to a creature';
    GMagicDescription[SCH_FIRE, 4] := 'Launches a ball of fire that causes a medium amount of fire damage over a large area';
    GMagicDescription[SCH_FIRE, 5] := 'Causes fire to rain down over an area causing a large amount of fire damage to creatures within';
    GMagicDescription[SCH_FIRE, 6] := 'Temporarily increases greatly your resistance to elemental fire';
    GMagicDescription[SCH_FIRE, 7] := 'Temporarily decreases greatly a creature''s resistance to elemental fire';
    GMagicDescription[SCH_FIRE, 8] := 'Wracks a creature in elemental fire, causing a massive amount of fire damage';

    { School of Air }
    GMagicColour[SCH_AIR] := clWhite;
    GMagic[SCH_AIR, 0] := 'Tome of Aether';
    GMagic[SCH_AIR, 1] := 'Blast of Wind';
    GMagic[SCH_AIR, 2] := 'Fog';
    GMagic[SCH_AIR, 3] := 'Whirlwind';
    GMagic[SCH_AIR, 4] := 'Lightning Bolt';
    GMagic[SCH_AIR, 5] := 'Lightning Blast';
    GMagic[SCH_AIR, 6] := 'Hurricane';
    GMagic[SCH_AIR, 7] := 'Vacuum';
    GMagic[SCH_AIR, 8] := 'Mass Vacuum';
    GMagicDescription[SCH_AIR, 0] := 'Spells that manipulate the raw elemental fury of air, often causing great offensive damage';
    GMagicDescription[SCH_AIR, 1] := 'Launches a bolt of hardened fire that causes a small amount of physical damage to a creature';
    GMagicDescription[SCH_AIR, 2] := 'Blankets an area in fog making things hard to see';
    GMagicDescription[SCH_AIR, 3] := 'Summons a whirlwind that paralyzes a creaturet for a short period of time';
    GMagicDescription[SCH_AIR, 4] := 'Launches a lighting bolt that causes a medium amount of electrical damage to a creature';
    GMagicDescription[SCH_AIR, 5] := 'Launches a lighting bolt that causes a high amount of electrical damage to a creature';
    GMagicDescription[SCH_AIR, 6] := 'Summons a massive hurricane that paralyzes all creatures within a certain area for a short period of time';
    GMagicDescription[SCH_AIR, 7] := 'Sucks the air out of a creature causing a massive amount of physical damage';
    GMagicDescription[SCH_AIR, 8] := 'Sucks the air out of all creatures in an area causing massive amounts of physical damage';

    { School of Earth }
    GMagicColour[SCH_EARTH] := $00004080;
    GMagic[SCH_EARTH, 0] := 'Tome of Earth';
    GMagic[SCH_EARTH, 1] := 'Mud';
    GMagic[SCH_EARTH, 2] := 'Dig';
    GMagic[SCH_EARTH, 3] := 'Sinking Sands';
    GMagic[SCH_EARTH, 4] := 'Slam';
    GMagic[SCH_EARTH, 5] := 'Earthquake';
    GMagic[SCH_EARTH, 6] := 'Unmoving Flesh';
    GMagic[SCH_EARTH, 7] := 'Earth Punch';
    GMagic[SCH_EARTH, 8] := 'Flesh to Stone';
    GMagicDescription[SCH_EARTH, 0] := 'Spells that manipulate the raw elemental fury of earth, often causing great offensive damage';
    GMagicDescription[SCH_EARTH, 1] := 'Douses a creature in mud, slowing it for a short period of time';
    GMagicDescription[SCH_EARTH, 2] := 'Excavates rock quickly';
    GMagicDescription[SCH_EARTH, 3] := 'Turns rock to quicksand temporarily within an area, paralysing creatures for a short period of time';
    GMagicDescription[SCH_EARTH, 4] := 'Slams a creature hard into the nearest hard ceiling, causing a large amount of physical damage';
    GMagicDescription[SCH_EARTH, 5] := 'Shakes the ground violently all around you, causing a large amount of physical damage to nearby creatures';
    GMagicDescription[SCH_EARTH, 6] := 'Hardens the flesh of a creature, paralysing it for a long period of time';
    GMagicDescription[SCH_EARTH, 7] := 'Slams rocks into a creature at high speed, causing a massive amount of physical damage';
    GMagicDescription[SCH_EARTH, 8] := 'Hardens the flesh of a creature, paralysing it for a long period of time and causing massive amounts of physical damage';

    { School of Water }
    GMagicColour[SCH_WATER] := $00FF8000;
    GMagic[SCH_WATER, 0] := 'Tome of Torrents';
    GMagic[SCH_WATER, 1] := 'Steam Clouds';
    GMagic[SCH_WATER, 2] := 'Waterwalk';
    GMagic[SCH_WATER, 3] := 'Boil';
    GMagic[SCH_WATER, 4] := 'Acid Cloud';
    GMagic[SCH_WATER, 5] := 'Tsunami';
    GMagic[SCH_WATER, 6] := 'Dehydrate';
    GMagic[SCH_WATER, 7] := 'Drown';
    GMagic[SCH_WATER, 8] := 'Wither';
    GMagicDescription[SCH_WATER, 0] := 'Spells that manipulate the raw elemental fury of water, often causing great offensive damage';
    GMagicDescription[SCH_WATER, 1] := 'Blankets an area in steam making things hard to see';
    GMagicDescription[SCH_WATER, 2] := 'You can walk through water just as quickly as if you were walking though air';
    GMagicDescription[SCH_WATER, 3] := 'Boils the water within a creature causing a large amount of physical damage';
    GMagicDescription[SCH_WATER, 4] := 'Creates a rolling cloud of acid that inflicts a large amount of water damage to creatures within';
    GMagicDescription[SCH_WATER, 5] := 'Summons a tumultous torrent of water that inflicts a large amount of water damage to creatures within';
    GMagicDescription[SCH_WATER, 6] := 'Forcibly removes the water within a creature causing a large amount of physical damage';
    GMagicDescription[SCH_WATER, 7] := 'Tries to drown a creature from within, paralysing it for a long period of time and causing massive amounts of physical damage';
    GMagicDescription[SCH_WATER, 8] := 'Forcibly removes the water within a creature causing a massive amount of physical damag';

    { School of Nature }
    GMagicColour[SCH_NATURE] := clGreen;
    GMagic[SCH_NATURE, 0] := 'Tome of the Wild';
    GMagic[SCH_NATURE, 1] := 'Calm Beast';
    GMagic[SCH_NATURE, 2] := 'Entangle';
    GMagic[SCH_NATURE, 3] := 'Invisibility to Beasts';
    GMagic[SCH_NATURE, 4] := 'Nature''s Ally Minor';
    GMagic[SCH_NATURE, 5] := 'Nature''s Wrath';
    GMagic[SCH_NATURE, 6] := 'Nature''s Ally Major';
    GMagic[SCH_NATURE, 7] := 'Befriend Beast';
    GMagic[SCH_NATURE, 8] := 'Decay';
    GMagicDescription[SCH_NATURE, 0] := 'Spells that manipulate the primal power of nature, and dealing with animals and plants';
    GMagicDescription[SCH_NATURE, 1] := 'Calms a natural beast for a short period of time, making it neutral to you';
    GMagicDescription[SCH_NATURE, 2] := 'Weeds reach out and grasp a creature, stopping it moving for a short period of time';
    GMagicDescription[SCH_NATURE, 3] := 'Makes you invisible to nearby beasts for a short period of time';
    GMagicDescription[SCH_NATURE, 4] := 'Summons a small natural creature to fight for you';
    GMagicDescription[SCH_NATURE, 5] := 'Causes a large amount of damage to a creature as nature itself strikes in fury';
    GMagicDescription[SCH_NATURE, 6] := 'Summons a poweful natural creature to fight for you';
    GMagicDescription[SCH_NATURE, 7] := 'Permanently befriends a natural creature, making it into an ally';
    GMagicDescription[SCH_NATURE, 8] := 'Ages a creature horribly, causing a massive amount of of physical damage';

    { School of Healing }
    GMagicColour[SCH_HEALING] := clAqua;
    GMagic[SCH_HEALING, 0] := 'Tome of Salving';
    GMagic[SCH_HEALING, 1] := 'Minor Healing';
    GMagic[SCH_HEALING, 2] := 'Cure';
    GMagic[SCH_HEALING, 3] := 'Medium Healing';
    GMagic[SCH_HEALING, 4] := 'Life Ward';
    GMagic[SCH_HEALING, 5] := 'Major Healing';
    GMagic[SCH_HEALING, 6] := 'Lesser Lifeforce';
    GMagic[SCH_HEALING, 7] := 'Greater Lifeforce';
    GMagic[SCH_HEALING, 8] := 'Death Ward';
    GMagicDescription[SCH_HEALING, 0] := 'Spells that deal with healing wounds, aiding the sick and curing the infirm';
    GMagicDescription[SCH_HEALING, 1] := 'Heals a small amount of damage';
    GMagicDescription[SCH_HEALING, 2] := 'Cleans you of any toxins and poisons';
    GMagicDescription[SCH_HEALING, 3] := 'Heals a medium amount of damage';
    GMagicDescription[SCH_HEALING, 4] := 'Energises your body to reactively heal some of the damage that you take';
    GMagicDescription[SCH_HEALING, 5] := 'Heals a large amount of damage';
    GMagicDescription[SCH_HEALING, 6] := 'Temporarily increases your resistance to lifedraining and death magics by a small amount';
    GMagicDescription[SCH_HEALING, 7] := 'Temporarily increases your resistance to lifedraining and death magics by a large amounte';
    GMagicDescription[SCH_HEALING, 8] := 'Gives you a chance of avoiding even the clutches of death';

    { School of Cursing }
    GMagicColour[SCH_CURSING] := $00FF0080;
    GMagic[SCH_CURSING, 0] := 'Tome of Doom';
    GMagic[SCH_CURSING, 1] := 'Hand of Fate';
    GMagic[SCH_CURSING, 2] := 'Sloth';
    GMagic[SCH_CURSING, 3] := 'Weakness';
    GMagic[SCH_CURSING, 4] := 'Lesser Doom';
    GMagic[SCH_CURSING, 5] := 'Cackhanded';
    GMagic[SCH_CURSING, 6] := 'Vulnerability';
    GMagic[SCH_CURSING, 7] := 'Clumsiness';
    GMagic[SCH_CURSING, 8] := 'Doom';
    GMagicDescription[SCH_CURSING, 0] := 'Spells that adversely affect probability and chance';
    GMagicDescription[SCH_CURSING, 1] := 'One creature is temporarily deemed unlucky in all its physical endeavours';
    GMagicDescription[SCH_CURSING, 2] := 'Makes one creature temporarily slightly more stupid or weak or fragile or frail';
    GMagicDescription[SCH_CURSING, 3] := 'Makes one creature temporarily slightly more stupid and weak and fragile and frail';
    GMagicDescription[SCH_CURSING, 4] := 'One creature is temporarily deemed extremely unlucky in all its physical endeavours';
    GMagicDescription[SCH_CURSING, 5] := 'All skills of one creature are temporarily stripped from it';
    GMagicDescription[SCH_CURSING, 6] := 'One creature is made much more vulnerable to offensive magics';
    GMagicDescription[SCH_CURSING, 7] := 'One creature is temporarily deemed extremely unlucky in all its physical endeavours';
    GMagicDescription[SCH_CURSING, 8] := 'An unfortunate creature is doomed and the multiverse works against it for a time in a myriad of ways';

    { School of Combat }
    GMagicColour[SCH_COMBAT] := clSilver;
    GMagic[SCH_COMBAT, 0] := 'Tome of Battle';
    GMagic[SCH_COMBAT, 1] := 'Keen Edge';
    GMagic[SCH_COMBAT, 2] := 'Aid';
    GMagic[SCH_COMBAT, 3] := 'Counterattack';
    GMagic[SCH_COMBAT, 4] := 'Bleeding Edge';
    GMagic[SCH_COMBAT, 5] := 'Fate';
    GMagic[SCH_COMBAT, 6] := 'Battle Sense';
    GMagic[SCH_COMBAT, 7] := 'Battle Roar';
    GMagic[SCH_COMBAT, 8] := 'Death March';
    GMagicDescription[SCH_COMBAT, 0] := 'Spells that give an edge in battle, and help the deserving win battles that much more easily';
    GMagicDescription[SCH_COMBAT, 1] := 'All physical damage you inflict for a short period of time is increased slightly';
    GMagicDescription[SCH_COMBAT, 2] := 'Your attack skills are boosted slightly for a short period of time';
    GMagicDescription[SCH_COMBAT, 3] := 'You automatically attack the next creature to attack you in melee';
    GMagicDescription[SCH_COMBAT, 4] := 'All physical damage you inflict for a short period of time is increased substiantially';
    GMagicDescription[SCH_COMBAT, 5] := 'Your next attacks are all guaranteed to be critical hits';
    GMagicDescription[SCH_COMBAT, 6] := 'Your attack skills are boosted considerably for a short period of time';
    GMagicDescription[SCH_COMBAT, 7] := 'Creatures within your vicinity are frightened and run away';
    GMagicDescription[SCH_COMBAT, 8] := 'For a period of time, in a frenzy of battle lust you counterattack anything that attacks you remorselessly';

    { School of Protection }
    GMagicColour[SCH_PROTECTION] := clGray;
    GMagic[SCH_PROTECTION, 0] := 'Tome of Defense';
    GMagic[SCH_PROTECTION, 1] := 'Lesser Protection';
    GMagic[SCH_PROTECTION, 2] := 'Determination';
    GMagic[SCH_PROTECTION, 3] := 'Stubbornness';
    GMagic[SCH_PROTECTION, 4] := 'Greater Protection';
    GMagic[SCH_PROTECTION, 5] := 'Watchfulness';
    GMagic[SCH_PROTECTION, 6] := 'Bloody-mindedness';
    GMagic[SCH_PROTECTION, 7] := 'Stalwart';
    GMagic[SCH_PROTECTION, 8] := 'Battle Shield';
    GMagicDescription[SCH_PROTECTION, 0] := 'Spells that protect and shield';
    GMagicDescription[SCH_PROTECTION, 1] := 'Temporarily, all incoming damage is slightly lessened';
    GMagicDescription[SCH_PROTECTION, 2] := 'Your defense increases for a short period of time, increasing the effect of the armour you wear';
    GMagicDescription[SCH_PROTECTION, 3] := 'You resist harmful and baleful magics more for a short period of time';
    GMagicDescription[SCH_PROTECTION, 4] := 'Temporarily, all incoming damage is lessened';
    GMagicDescription[SCH_PROTECTION, 5] := 'Your defense increases considerably for a short period of time, increasing the effect of the armour you wear';
    GMagicDescription[SCH_PROTECTION, 6] := 'You resist harmful and baleful magics considerably more for a short period of time';
    GMagicDescription[SCH_PROTECTION, 7] := 'You become immune to harmful and baleful magics for a fleeting moment in time';
    GMagicDescription[SCH_PROTECTION, 8] := 'You become immune to physical attakcs for a fleeting moment in time';

    { School of Genius }
    GMagicColour[SCH_LORE] := $0080FFFF;
    GMagic[SCH_LORE, 0] := 'Tome of Genius';
    GMagic[SCH_LORE, 1] := 'Detect Magic';
    GMagic[SCH_LORE, 2] := 'Identify';
    GMagic[SCH_LORE, 3] := 'Adept';
    GMagic[SCH_LORE, 4] := 'Detect Curse';
    GMagic[SCH_LORE, 5] := 'Interrogate';
    GMagic[SCH_LORE, 6] := 'Remove Curse';
    GMagic[SCH_LORE, 7] := 'Awareness';
    GMagic[SCH_LORE, 8] := 'Mass Identify';
    GMagicDescription[SCH_LORE, 0] := 'Spells of Diviation and seeking out Knowledge, both forbidden and permitted';
    GMagicDescription[SCH_LORE, 1] := 'Determines if any items in your posession are magical';
    GMagicDescription[SCH_LORE, 2] := 'Identifies an item, revealing all its properties';
    GMagicDescription[SCH_LORE, 3] := 'The cost of your next spell is reduced';
    GMagicDescription[SCH_LORE, 4] := 'Determines if any items in your posession are cursed';
    GMagicDescription[SCH_LORE, 5] := 'Reveals all the vital statistics of a monster';
    GMagicDescription[SCH_LORE, 6] := 'Removes any curses from any items in your posession';
    GMagicDescription[SCH_LORE, 7] := 'Locates all nearby items, creatures or points of interest';
    GMagicDescription[SCH_LORE, 8] := 'Identifies all items in your posession, revealing all their properties';


    { Extra schools that haven't been implemented in-game yet }

    GMagicColour[SCH_NECROMANCY] := $008000FF;
    GMagic[SCH_NECROMANCY, 0] := 'Tome of Death';
    GMagic[SCH_NECROMANCY, 1] := 'Drain';
    GMagic[SCH_NECROMANCY, 2] := 'Sliver of Life';
    GMagic[SCH_NECROMANCY, 3] := 'Undead Bane';
    GMagic[SCH_NECROMANCY, 4] := 'Summon Minor Undead';
    GMagic[SCH_NECROMANCY, 5] := 'Greater Drain';
    GMagic[SCH_NECROMANCY, 6] := 'Summon Major Undead';
    GMagic[SCH_NECROMANCY, 7] := 'Cheat Death';
    GMagic[SCH_NECROMANCY, 8] := 'Semblance of Unlife';

    GMagicColour[SCH_DEMONOLOGY] := $00800040;
    GMagic[SCH_DEMONOLOGY, 0] := 'Tome of the Abyss';
    GMagic[SCH_DEMONOLOGY, 1] := 'Summon Imp';
    GMagic[SCH_DEMONOLOGY, 2] := 'Smite';
    GMagic[SCH_DEMONOLOGY, 3] := 'Summon Terror';
    GMagic[SCH_DEMONOLOGY, 4] := 'Communion';
    GMagic[SCH_DEMONOLOGY, 5] := 'Wrath';
    GMagic[SCH_DEMONOLOGY, 6] := 'Summon Swarm';
    GMagic[SCH_DEMONOLOGY, 7] := 'Leech';
    GMagic[SCH_DEMONOLOGY, 8] := 'Sacrifice';

    GMagicColour[SCH_MIND] := $00808040;
    GMagic[SCH_MIND, 0] := 'Tome of the Mind';
    GMagic[SCH_MIND, 1] := 'Frightening Visage';
    GMagic[SCH_MIND, 2] := 'Demoralise';
    GMagic[SCH_MIND, 3] := 'Mind Snatch';
    GMagic[SCH_MIND, 4] := 'Nightmares';
    GMagic[SCH_MIND, 5] := 'Terror';
    GMagic[SCH_MIND, 6] := 'Psychosis';
    GMagic[SCH_MIND, 7] := 'Mind Cage';
    GMagic[SCH_MIND, 8] := 'Mind Theft';

    GMagicColour[SCH_ENCHANTMENT] := $008080FF;
    GMagic[SCH_ENCHANTMENT, 0] := 'Tome of Enchantment';
    GMagic[SCH_ENCHANTMENT, 1] := 'Imbue';
    GMagic[SCH_ENCHANTMENT, 2] := 'Enchant';
    GMagic[SCH_ENCHANTMENT, 3] := 'Minor Disenchant';
    GMagic[SCH_ENCHANTMENT, 4] := 'Transmute';
    GMagic[SCH_ENCHANTMENT, 5] := 'Major Disenchant';
    GMagic[SCH_ENCHANTMENT, 6] := 'Instill';
    GMagic[SCH_ENCHANTMENT, 7] := 'Augment';
    GMagic[SCH_ENCHANTMENT, 8] := 'Greater Augment';

    GMagicColour[SCH_TRAVEL] := clOlive;
    GMagic[SCH_TRAVEL, 0] := 'Tome of the Road';
    GMagic[SCH_TRAVEL, 1] := 'Light';
    GMagic[SCH_TRAVEL, 2] := 'Speed of the Hare';
    GMagic[SCH_TRAVEL, 3] := 'Sustenance';
    GMagic[SCH_TRAVEL, 4] := 'Sunlight';
    GMagic[SCH_TRAVEL, 5] := 'Speed of the Sparrow';
    GMagic[SCH_TRAVEL, 6] := 'Spirit Walk';
    GMagic[SCH_TRAVEL, 7] := 'Speed of the Eagle';
    GMagic[SCH_TRAVEL, 8] := 'Stasis';
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set up the dungeons }
procedure SetupDungeons;
var
  InnerLoop: Integer;
  LocalImageList: TImageList;
  Loop: Integer;
  DS: TDungeonBranch;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitInit.SetupDungeons()');

  try
    { Set up the Dungeon Data }
    ObjectList := GDungeonBranchList;

    { Set up individual dungeon branches }
    for Loop := 0 to 9 do
    begin
      { Create branch }
      DS := TDungeonBranch.Create;
      DS.BranchName := GDungeonNameArray[Loop];
      DS.BranchTheme := Loop;
      DS.BranchFileName := GDungeonTypeArray[Loop];

      { Although graphics currently aren't implemented, we can handle them }
      LocalImageList := TImageList.CreateSize(32, 32);
      LocalImageList.Masked := False;
      LoadImageList(LocalImageList, DS.BranchFileName, -1, False);
      DS.BranchGraphics.Assign(LocalImageList);

      { Set up Ascii Colours }
      for InnerLoop := 0 to High(GASCIIColours) do
        DS.BranchASCIIColours[InnerLoop] :=
          GASCIIColours[DS.BranchTheme, InnerLoop];

      { Set up internal data for each branch }
      DS.BranchDepth := BRANCHDEPTH;
      DS.BranchStartingLevel := MIN_LEVEL;
      DS.BranchEndingLevel := MAX_LEVEL;
      DS.BranchDungeonBalance := GDungeonBalance[Loop];
      DS.BranchDungeonRoomsize := GDungeonRoomSize[Loop];
      DS.BranchCorridorLength := GDungeonCorridorLength[Loop];
      DS.BranchRecurseDepth := GDungeonRecurseDepth[Loop];
      DS.BranchStairCount := GDungeonStairCount[Loop];
      DS.BranchCrossCorridors := GDungeonCrossCorridors[Loop];
      DS.BranchCorridorDoorChance := GDungeonCorridorDoorChance[Loop];

      { Add the dungeon to the dungeon list }
      LocalImageList.Free;
      LocalImageList := nil;
      ObjectList.Add(DS);
    end;

    { Set up the temporary storage dungeons for the current branch }
    for InnerLoop := 1 to 10 do
    begin
      BranchDungeons[InnerLoop] := TDungeonLevel.Create(0, D_NONE);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;  
end;

{ Set up the global data }
procedure CreateGlobalData;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitInit.CreateGlobalData()');
  try
   { Set up the global storage lists }
    GEnchantmentList := TObjectList.Create(True);
    GItemList := TObjectList.Create(True);
    GDungeonBranchList := TObjectList.Create(True);
    GVaultList := TObjectList.Create(True);
    GWeaponList := TObjectList.Create(True);
    GArmourList := TObjectList.Create(True);
    GJewelleryList := TObjectList.Create(True);
    GWandList := TObjectList.Create(True);
    GPotionList := TObjectList.Create(True);
    GScrollList := TObjectList.Create(True);
    GFoodList := TObjectList.Create(True);
    GMonsterTypeList := TObjectList.Create(True);
    GMonsterList := TObjectList.Create(True);
    GTownsPeopleList := TObjectList.Create(True);

    { This is a copy of certain monsters from the GMonsterList but don't own
      the memory }
    ActiveMonsterList := TObjectList.Create(False);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Load the global data in from the database }
procedure PopulateGlobalData(UseDB: Boolean = False);
var
  Enchantment: TItemEnchantment;
  ItemContainer: TItemArchetype;
  FileLineStringList: TStringList;
  Monster: TMonsterArchetype;
  ObjectList: TObjectList;
  MonsterChar: Char;
  MonsterColor: TColor;
  MonsterLevel: Integer;
  Vault: TVault;
  SingleName: String;
  PluralName: String;
  Weight: Integer;
  AC: Integer;
  HandedNess: Integer;
  Value: Integer;
  CanBeUnique: Boolean;
  UseItems: Boolean;
  ItemType: crItemType;
  ItemMaterial: crMaterial;
  ItemSlot: crItemSlot;
  ItemQuality: crItemQuality;
  ItemColour: String;
  Projectile: String;
  RangedAttack: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitInit.PopulateGlobalData()');

  try
    { Initialise the Loading Buffer }
    FileLineStringList := TStringList.Create;
    FileLineStringList.Delimiter := ',';
    FileLineStringList.QuoteChar := '"';

    { Retrieve Information from Database if we've chosen this method }
    if (UseDB = True) then
    begin
      { Create the datamodule - which initialises the database connection }
      DataModuleMain := TDataModuleMain.Create(nil);
      with DataModuleMain do
      begin
        { Get the monster data }
        SLTable := SlDatabase.GetTable('SELECT * FROM MONSTERS');
        ObjectList := GMonsterTypeList;

        { Create the monster data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Retrieve some monster archetype details }
          MonsterLevel := StrToIntDef(
            SLTable.FieldAsString(SLTable.FieldIndex['MLEVEL']), -1);
          MonsterChar := SLTable.FieldAsString(
            SLTable.FieldIndex['DISPLAYCHAR'])[1];
          MonsterColor := StringToColor('$02' + Copy(
            SLTable.FieldAsString(SLTable.FieldIndex['DISPLAYCOLOUR']), 2, 6));
          CanBeUnique := SLTable.FieldAsString(
            SLTable.FieldIndex['CANBEUNIQUE']) = 'Yes';
          UseItems := SLTable.FieldAsString(
            SLTable.FieldIndex['USEITEMS']) = 'Yes';

          { Create the monster archetype }
          Monster := TMonsterArchetype.Create(
            SLTable.FieldAsInteger(SLTable.FieldIndex['ID']),
            SLTable.FieldAsString(SLTable.FieldIndex['MONSTERNAME']),
            MonsterLevel, MonsterChar, MonsterColor,
            SLTable.FieldAsString(SLTable.FieldIndex['RESISTANCES']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['HITDICE']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['ATTACKNUM']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['ATTACKBONUS']),
            SLTable.FieldAsString(SLTable.FieldIndex['ATTACKTYPE']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['ATTACKDAMAGE']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['AC']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['EV']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['SPEED']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['POISONLEVEL']),
            SLTable.FieldAsString(SLTable.FieldIndex['MONSTERTYPE']),
            SLTable.FieldAsString(SLTable.FieldIndex['MONSTERFLAGS']),
            SLTable.FieldAsString(SLTable.FieldIndex['BEHAVIOUR']),
            SLTable.FieldAsString(SLTable.FieldIndex['CLASSES']),
            SLTable.FieldAsString(SLTable.FieldIndex['ECOLOGY']),
            SLTable.FieldAsString(SLTable.FieldIndex['TREASURE']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['POPULATION']),
            SLTable.FieldAsString(SLTable.FieldIndex['OCCURRENCE']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['SPEECHFREQUENCY']),
            SLTable.FieldAsString(SLTable.FieldIndex['SPEECH']),
            SLTable.FieldAsString(SLTable.FieldIndex['SPEECHACTIONS']),
            SLTable.FieldAsString(SLTable.FieldIndex['ACTIONS']),
            SLTable.FieldAsString(SLTable.FieldIndex['VERBS']), CanBeUnique,
            UseItems);

          { Add the extra data }
          Projectile := SLTable.FieldAsString(SLTable.FieldIndex['PROJECTILE']);
          if Projectile = '' then
            Projectile := ' ';
          Monster.ProjectileChar := Projectile[1];
          Projectile :=
            SLTable.FieldAsString(SLTable.FieldIndex['PROJECTILECOLOUR']);
          if Projectile = '' then
            Projectile := 'clBlack';
          Monster.ProjectileColour := StringToColor
            (Projectile);
          RangedAttack :=
            SLTable.FieldAsString(SLTable.FieldIndex['RANGEDVERB']);
          Monster.RangedVerb := RangedAttack;
          Monster.PluralName :=
            SLTable.FieldAsString(SLTable.FieldIndex['MONSTERNAMEPLURAL']);

          { Load the monster data }
          ObjectList.Add(Monster);

          { Move onto the next record }
          SLTable.Next;
        end;

        { Get the vault data }
        SLTable := SlDatabase.GetTable('SELECT * FROM VAULT');
        ObjectList := GVaultList;

        { Create the vault data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Create the vault type }
          Vault := TVault.Create(
            SLTable.FieldAsInteger(SLTable.FieldIndex['ID']),
            SLTable.FieldAsString(SLTable.FieldIndex['VAULTNAME']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['VAULTLEVEL']),
            SLTable.FieldAsString(SLTable.FieldIndex['VAULTECOLOGY']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['VAULTX']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['VAULTY']),
            SLTable.FieldAsString(SLTable.FieldIndex['VAULTMAP']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['VAULTSCORE']));

          { Load the vault data }
          ObjectList.Add(Vault);

          { Move onto the next record }
          SLTable.Next;
        end;


        { Get the enchantment data}
        SLTable := SlDatabase.GetTable('SELECT * FROM ENCHANTMENTS');
        ObjectList := GEnchantmentList;

        { Create the enchantment data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Create the enchantment }
          Enchantment := TItemEnchantment.Create(
            SLTable.FieldAsString(SLTable.FieldIndex['NAME']),
            SLTable.FieldAsString(SLTable.FieldIndex['DESCRIPTION']),
            SLTable.FieldAsString(SLTable.FieldIndex['EFFECT']),
            SLTable.FieldAsString(SLTable.FieldIndex['POSEFFECT']),
            SLTable.FieldAsString(SLTable.FieldIndex['NEGEFFECT']), False, 0,
            SLTable.FieldAsDouble(SLTable.FieldIndex['SCALING']),
            SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']));

          { Load the enchantment data }  
          ObjectList.Add(Enchantment);

          { Move onto the next record }
          SLTable.Next;
        end;

        { Get the armour data }
        SLTable := SlDatabase.GetTable('SELECT * FROM ARMOUR');
        ObjectList := GArmourList;

        { Set the Item Type appropriately }
        ItemType := iArmour;

        { Create the armour data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));
          ItemSlot :=
            GetSlot(SLTable.FieldAsString(SLTable.FieldIndex['SLOT']));
          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight := StrToInt(SLTable.FieldAsString(
            SLTable.FieldIndex['WEIGHT']));
          AC := StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['AC']));

          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, IntToStr(AC), 0, Value);

          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.Position := ObjectList.Count;
          ItemContainer.ItemColour := clWhite;
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the armour data }
          ObjectList.Add(ItemContainer);

          { Move onto the next record }
          SLTable.Next;
        end;     

        { Get the weapon data }
        SLTable := SlDatabase.GetTable('SELECT * FROM WEAPONS');
        ObjectList := GWeaponList;

        { Set the Item Type appropriately }
        ItemType := iWeapon;

        { Create the monster data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));
          ItemSlot :=
            GetSlot(SLTable.FieldAsString(SLTable.FieldIndex['SLOT']));
          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight := StrToInt(
            SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));
          HandedNess := StrToInt(
            SLTable.FieldAsString(SLTable.FieldIndex['HANDS']));

          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', HandedNess, Value, 0);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.Position := ObjectList.Count;
          ItemContainer.ItemColour := clWhite;
          ItemContainer.ItemModifier :=
            SLTable.FieldAsString(SLTable.FieldIndex['DAMAGE']);
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the weapon data }
          ObjectList.Add(ItemContainer);

          { Move onto the next record }
          SLTable.Next;
        end;

        { Get the jewellery data }
        SLTable := SlDatabase.GetTable('SELECT * FROM JEWELLERY');
        ObjectList := GJewelleryList;

        { Set the Item Type appropriately }
        ItemType := iRing;
        ItemSlot := iInventory;

        { Create the monster data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));

          { Deal with both rings and amulets }
          if SLTable.FieldAsString(SLTable.FieldIndex['SLOT']) = 'Ring' then
          begin
            ItemType := iRing;
            ItemSlot := iFinger;
          end
          else if SLTable.FieldAsString(SLTable.FieldIndex['SLOT']) = 'Amulet' then
          begin
            ItemType := iAmulet;
            ItemSlot := iNeck;
          end;

          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight :=
            StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));

          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', 0, Value);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.Position := ObjectList.Count;
          ItemContainer.ItemColour := clWhite;
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the jewellery data }
          ObjectList.Add(ItemContainer);

          { Move onto the next record }
          SLTable.Next;
        end;


        { Get the potion data }
        SLTable := SlDatabase.GetTable('SELECT * FROM POTIONS');
        ObjectList := GPotionList;

        { Set the Item Type appropriately }
        ItemType := iPotion;

        { Create the potion data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));
          ItemSlot := iInventory;
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight :=
            StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));
          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);

          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', 0, Value);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.ItemColour := clWhite;
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the potion data }
          ObjectList.Add(ItemContainer);

          { Set the position here, after the add, to avoid 0 indexing }
          ItemContainer.Position := ObjectList.Count;

          { Move onto the next record }
          SLTable.Next;
        end;


        { Get the wand data }
        SLTable := SlDatabase.GetTable('SELECT * FROM WANDS');
        ObjectList := GWandList;

        { Set the Item Type appropriately }
        ItemType := iWand;

        { Create the wand data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));
          ItemSlot := iInventory;
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight :=
            StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));
            
          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', 0, BASE_WAND_COST);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.Position := ObjectList.Count;
          ItemContainer.ItemColour := clWhite;
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the wand data }
          ObjectList.Add(ItemContainer);

          { Move onto the next record }
          SLTable.Next;
        end;

        { Get the scroll data }
        SLTable := SlDatabase.GetTable('SELECT * FROM SCROLLS');
        ObjectList := GScrollList;

        { Set the Item Type appropriately }
        ItemType := iScroll;

        { Create the scroll data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial :=
            GetMaterial(SLTable.FieldAsString(SLTable.FieldIndex['MATERIAL']));
          ItemSlot := iInventory;
          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight :=
            StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));

          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', 0, Value);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.ItemColour := clWhite;
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the scroll data }
          ObjectList.Add(ItemContainer);

          { Set the position here, after the add, to avoid 0 indexing }
          ItemContainer.Position := ObjectList.Count;

          { Move onto the next record }
          SLTable.Next;
        end;


        { Get the food and other consumable data }
        SLTable := SlDatabase.GetTable('SELECT * FROM FOOD');
        ObjectList := GFoodList;

        { Set the Item Type appropriately }
        ItemType := iConsumable;

        { Create the consumable data from the database entries }
        while (not(SLTable.EOF)) do
        begin
          { Get and set the Item Archetype Data }
          ItemMaterial := mOrganic;
          ItemSlot := iInventory;
          SingleName := SLTable.FieldAsString(SLTable.FieldIndex['NAME']);
          PluralName := SLTable.FieldAsString(SLTable.FieldIndex['NAMES']);
          Weight :=
            StrToInt(SLTable.FieldAsString(SLTable.FieldIndex['WEIGHT']));
          Value := SLTable.FieldAsInteger(SLTable.FieldIndex['VALUE']);
          
          { Create the Item Archetype }
          ItemContainer := TItemArchetype.Create(SingleName, PluralName,
            ItemType, Weight, ItemMaterial, ItemSlot, '', 0, Value);
            
          { Set additional details about the item }
          ItemContainer.ItemSymbol :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']);
          ItemContainer.Position := ObjectList.Count;
          ItemContainer.ItemColour :=
            GetColour(SLTable.FieldAsString(SLTable.FieldIndex['TEXTCOLOUR']));
          ItemContainer.Prefix :=
            SLTable.FieldAsString(SLTable.FieldIndex['PREFIX']);
          ItemContainer.ArcheTypeID :=
            SLTable.FieldAsString(SLTable.FieldIndex['SYMBOL']) +
            IntToStr(ObjectList.Count);

          { Load the consumable data }
          ObjectList.Add(ItemContainer);
          
          { Move onto the next record }
          SLTable.Next;
        end;
      end;
      { Free the datamodule }
      DataModuleMain.Free;
      DataModuleMain := nil;
    end;

    { Free the buffer }
    FileLineStringList.Free;
    FileLineStringList := nil;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Clear up all the global data }
procedure FreeGlobalData;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitInit.FreeGlobalData()');

  try
    { Free the global data }
    GEnchantmentList.Free;
    GItemList.Free;
    GDungeonBranchList.Free;
    GVaultList.Free;
    GWeaponList.Free;
    GArmourList.Free;
    GJewelleryList.Free;
    GWandList.Free;
    GPotionList.Free;
    GScrollList.Free;
    GFoodList.Free;
    GMonsterList.Free;
    GMonsterTypeList.Free;
    ActiveMonsterList.Free;
    GTownsPeopleList.Free;
    GEnchantmentList := nil;
    GItemList := nil;
    GDungeonBranchList := nil;
    GVaultList := nil;
    GWeaponList := nil;
    GArmourList := nil;
    GJewelleryList := nil;
    GWandList := nil;
    GPotionList := nil;
    GScrollList := nil;
    GFoodList := nil;
    GMonsterList := nil;
    GMonsterTypeList := nil;
    GTownsPeopleList := nil;
    ActiveMonsterList := nil;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.

