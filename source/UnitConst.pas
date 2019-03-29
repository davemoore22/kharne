{ UnitConst

  Copyright (c) 2007-2009 Dave Moore 

  Global Constants

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


unit UnitConst;

interface

uses Graphics;

const
  { Persistance Seperator }
  SEPERATOR = '|';

  { Terrain Types }
  T_BLANK = 0;
  T_HARDWALL = 1;
  T_SOFTWALL = 2;
  T_FLOOR_ROOM = 3;
  T_FLOOR_CORRIDOR = 4;
  T_DOOR_OPEN = 5;
  T_DOOR_CLOSED = 6;
  T_STAIRS_DOWN = 7;
  T_STAIRS_UP = 8;
  T_TERRAIN = 9;
  T_ARCH = 10;
  T_SPECIAL = 11;
  T_ROOMWALL = 12;
  T_FOUNTAIN = 13;
  T_FOUNTAIN_USED = 14;

  { Dungeon Themes } 
  D_NONE = -1;
  D_TOWN = 0;
  D_ABYSS = 1;
  D_CRYPT = 2;
  D_FORTRESS = 3;
  D_KEEP = 4;
  D_WILDERNESS = 5;
  D_EARTH = 6;
  D_AIR = 7;
  D_FIRE = 8;
  D_WATER = 9;
  D_MAX = 9;
  D_TILE_TOWN = 0;
  D_TILE_ABYSS = 12;
  D_TILE_CRYPT = 10;
  D_TILE_FORTRESS = 13;
  D_TILE_KEEP = 14;
  D_TILE_WILDERNESS = 11;
  D_TILE_EARTH = 16;
  D_TILE_AIR = 18;
  D_TILE_FIRE = 15;
  D_TILE_WATER = 17;
  D_TILE_DO_NOT_USE = 19;
  D_TILE_SPARKLE = 20;
  D_TILE_SHOP = 21;
  D_TILE_CLOSED_DOOR = 22;
  D_TILE_OPEN_DOOR = 23;
  D_TILE_HOUSE = 24;

  { Zones }
  Z_ABYSS = 1;
  Z_NEGATIVE = 2;
  Z_EARTH = 6;
  Z_AIR = 7;
  Z_FIRE = 8;
  Z_WATER = 9;

  { Zone dimensions }
  ZONE_SIZE = 10;
  ZONE_TILES_MAX = 100;

  { Vault Marker }
  Z_VAULT = 100;

  { Vault Categories }
  SMALLEST_GREATER_VAULT = 5;
  LARGEST_ENTRY_VAULT = 8;

  { Paranoia counters }
  VAULT_PARANOIA = 100;
  MONSTER_PARANOIA = 100;

  { Terrain Costs }
  TC_IMPASSABLE = 255;
  TC_PASSABLE = 100;
  TC_ITEM = 1;
  TC_MONSTER = 255;
  TC_NONE = 0;
  TC_VISITED = -1;

  { Terrain Effects }
  E_NONE = 0;
  E_SPECIALEFFECT = 1;
  E_STANDARDEFFECT = 2;
  E_DARKEREFFECT = 3;
  E_MINERAL = 4;
  E_GROUNDEFFECT = 101;
  E_LIGHTSQUARE = 110;

  { HP Warning Threshold }
  HPTHRESHOLD = 0.30;

  { Flag to indicate an item isn't stacked with anything beneath it }
  NO_NEXT_ITEM = -1;

  { Length of turns it takes to pseduo-id items carried }
  PSEUDOID_TURNS = 100;

  { Inventory Slots }
  S_INVENTORY_BEGIN = 1;
  S_INVENTORY_END = 26;
  S_FEET = 27;
  S_LEGS = 28;
  S_HANDS = 29;
  S_ARMS = 30;
  S_CHEST = 31;
  S_HEAD = 32;
  S_NECK = 33;
  S_MAINHAND = 34;
  S_OFFHAND = 35;
  S_LEFTFINGER = 36;
  S_RIGHTFINGER = 37;
  S_RANGED = 38;
  S_BACK = 39;

  { Skill caps used in logarithmic adjustments }
  ARMOURCAPACITYCAP = 25;
  EVASIONCAP = 25;
  SKILLCAP = 25;

  { Font Settings }
  //FontName = 'Bitstream Vera Sans Mono';
  //FontName = 'Garamond';
  //FontName = '16x24x';
  FontName = '12x18x';
  FontNameMedium = '8x16x';
  FontNameSmall = '6x10x';
  FontNamePlayer = '12x18x';
  FontSize = 11;
  FontSizePlayer = 13;
  ScalingFactorX = 12; //16; //12;
  ScalingFactorY = 24;
  BarWidth = ScalingFactorX / 100;

  { General Dungeon Preferences - used in dungeon generation }
  NORTH = 1;
  SOUTH = 2;
  EAST = 3;
  WEST = 4;
  SWIDTH = 0;
  SHEIGHT = 1;
  DUNGEONSIZEX = 400;
  DUNGEONSIZEY = 400;
  SPECIALLEVELSIZEX = 160;
  SPECIALLEVELSIZEY = 140;
  MAXDUNGEONSTACKDEPTH = 10;
  DUNGEONCLOSEDDOORCHANCE = 10;
  DUNGEONOPENDOORCHANCE = 40;
  DUNGEONARCHWAYCHANCE = 80;
  MAXSTAIRS = 10;
  MIN_LEVEL = 1;
  MAX_LEVEL = 20;
  BRANCHDEPTH = 10;
  BLOCK_HGT = 11;
  BLOCK_WID	= 11;
  BLOCK_NUM_X = 20;
  BLOCK_NUM_Y = 15;
  PANEL_HGT = 11;
  PANEL_WID = 33;
  DUNGEON_HGT = 165;
  DUNGEON_WID = 220;
  DUN_ROOMS = 14;
  DUN_TUN_RND = 10;
  DUN_TUN_CHG = 30;
  DUN_TUN_CON = 15;
  DUN_TUN_PEN = 25;
  DUN_TUN_JCT = 25;
  MAX_ROOMS_ROW = 18;
  MAX_ROOMS_COL = 18;
  IN_BOUNDS_BORDER = 40;
  ROOM_NORMAL = 0;
  ROOM_OVERLAPPING = 1;

  { Graphic Types }
  DGRAPHICS = 0;
  DASCIISTANDARD = 1;
  DASCIICOLOURED = 2;

  { Stair direction values }
  D_UP = -1;
  D_DOWN = 1;

  { Item Generation }
  CHANCECURSED = 25;
  CHANCEMAGICAL = 25;
  CHANCE_ARTIFACT = 3;
  CHANCE_EPIC = 15;
  CHANCE_LEGENDARY = 36;
  CHANCE_WEAPON = 33;
  CHANCE_ARMOUR = 66;
  CHANCE_RING = 82;
  ITEM_CHANCE_ARTIFACT = 2;
  ITEM_CHANCE_LEGENDARY = 7;
  ITEM_CHANCE_EPIC = 16;
  ITEM_CHANCE_SUPERB = 30;
  NO_ITEM_LEVEL = -1;
  ITEM_COST_FACTOR = 5;             
  STARTING_WAND_CHARGES = 20;
  MINIMUM_WAND_CHARGES = 5;
  SUPERB_ITEM_SCALING = 0.75;
  LEGENDARY_ITEM_SCALING = 1.5;
  EPIC_ITEM_SCALING = 2.5;
  ARTIFACT_ITEM_SCALING = 4;
  BASE_WAND_COST = 250;
  CHANCE_NODE_ITEM = 5;

  { Combat and Monsters }
  RANGED_CHANCE_GF_MELEE = 70;
  SET_NO_ENERGY = 1000;
  FUMBLE_ENERGY_COST_FACTOR = 3;
  MONSTER_ATTACK_ENERGY_COST_FACTOR = 5;

  { Probability }
  COIN_FLIP = 2;
  PERCENTAGE = 101;

  { Item properties }
  ONE_HANDED = 1;
  TWO_HANDED = 2;

  { Item munging constants - these must be kept in sync with the numbers of
    their equivalent items in the item datanaseyo }
  NUMBER_OF_RINGS = 16;
  NUMBER_OF_AMULETS = 16;
  NUMBER_OF_WANDS = 7;

  { Score adjustments }
  INVENTORY_SCORE_ITEM_DIVISOR = 500;
  WORN_SCORE_ITEM_DIVISOR = 100;

  { Visiblity }
  PREVIOUSLY_VISIBLE = 2;
  CURRENTLY_VISIBLE = 1;
  NOT_YET_VISIBLE = 0;
  MAGIC_VISIBLE = 3;

  { Money values }
  GOLD_AMOUNT = 50;
  SILVER_AMOUNT = 100;
  BRONZE_AMOUNT = 200;

  { How many walls surround a totally surrounded square }
  TOTALLY_SURROUNDED = 8;

  { Wizard mode }
  WIZARD_MODE_XP_GAIN_STEP = 200;
  WIZARD_MODE_GOLD_GAIN = 1000;

  { Monster constants }
  MONSTER_MAGICAL_ITEM_CHANCE = 10;
  MONSTER_CARRYING_ITEM_CHANCE = 5;
  MONSTER_HITDICE_HP = 5;
  MONSTER_STARTING_ENERGY = -6;
  ZONE_MONSTER_LEVEL_BOOST = 1;
  UNIQUE_MONSTER_LEVEL_BOOST = 3;
  MONSTER_MESSAGE_HEALTH_INTERVAL = 5;

  { Sting output constants }  
  LINE_BREAK = #10#13;

  { Display constants }
  STATUS_FONT_SIZE = 14;
  CONDITION_FONT_SIZE = 11;
  MAXIMUM_ITEM_LENGTH = 27;

  { Inventory features }
  NO_ITEM = -1;
  NO_ITEM_IN_SLOT = 0;

  { Monster definitions }
  NO_MONSTER = -1;

  { Turns it takes to dig }
  DIGGING_TURNS = 25;

  { As it says, the last character before the start of ascii }
  START_OF_ASCII_LETTERS = 64;

  { Internal IDs of the variuous tabs }
  MAINDISPLAY = 0;
  INVENTORY = 1;
  SKILLS = 2;
  MAGIC = 3;
  HELP = 4;
  DUMP = 5;
  VERSION = 6;
  MONSTER = 11;
  ITEM = 10;

  { Town starting location }
  TOWNSTARTX = 203;
  TOWNSTARTY = 205;

  { Pigment levels }
  SMALL_COLOUR_CHANGE = 25;
  LARGE_COLOUR_CHANGE = 75;

  { Turns before you get a level feeling }
  LEVEL_FEELING_INTERVAL = 100;

  { Default FOV radius }
  LIGHTRADIUS = 7;

  { Character Icon Display Constants - used to load appopriate race and class
    icon from the player graphics file - to get appropriate image use (
    RACE * 8) + CLASS - not implemmented yet though }
  HUMAN = 0;
  DWARF = 3;
  ELF = 4;
  HALFING = 8;
  ORC = 10;
  KNIGHT = 3;
  MAGE = 1;
  THIEF = 6;
  PRIEST = 4;
  WARRIOR = 7;

  { Minimum and maximum dungeon levels }
  MINDUNGEONLEVEL = 1;
  MAXDUNGEONLEVEL = 20;

  { Nutrition from food }
  FOOD_NUTRITION_VALUE = 400;
  HUNGER_STEP_NORMAL = -1;
  HUNGER_STEP_LARGE = -3;
  HUNGER_STEP_MAXIMUM = -5;

  { Regeneration }
  MINIMUM_REGENERATION = 30;

  { Fountains }
  CHANCE_MAGICAL = 10;
  CHANCE_BLESSED = 15;
  CHANCE_CURSED = 25;
  CHANCE_GUARDED = 30;
  MAXIMUM_EXTRA_HEALTH = 10;
  MAXIMUM_EXTRA_MANA = 4;
  FOUNTAIN_HUNGER = -100;
  FOUNTAIN_REFRESHMENT = 100;
  FOUNTAIN_REGENERATION_BOOST = 10;

  { Monster Speech Types }
  NONE = -1;
  SPEECH = 0;
  VERBALACTION = 1;
  ACTION = 2;

  { Character Stats }
  STRENGTH = 0;
  AGILITY = 1;
  ENDURANCE = 2;
  INTELLIGENCE = 3;
  RESOLVE = 4;
  CHARISMA = 5;

  { Character Skills }
  SKILL_START = 0;
  SK_FIGHTING = 0;
  SK_MELEE = 1;
  SK_RANGED = 2;
  SK_UNARMED = 3;
  SK_DEFENSE = 4;
  SK_HEAVY = 5;
  SK_MEDIUM = 6;
  SK_LIGHT = 7;
  SK_SUBTERFUGE = 10;
  SK_STEALTH = 11;
  SK_THIEVERY = 12;
  SK_MAGIC = 20;
  SK_FIRE = 21;
  SK_AIR = 22;
  SK_WATER = 23;
  SK_EARTH = 24;
  SK_NATURE = 25;
  SK_HEALING = 26;
  SK_CURSING = 27;
  SK_COMBAT = 28;
  SK_PROTECTION = 29;
  SK_LORE = 30;
  SK_NECROMANCY = 31;
  SK_DEMONOLOGY = 32;
  SK_MIND = 33;
  SK_ENCHANTMENT = 34;
  SK_TRAVEL = 35;
  SKILL_END = 35;
  UNSKILLED = 0;

  { Number of hi scores displayed }
  MAXHISCORESDISPLAYED = 22;
  
  { Magic Schools }
  SCH_FIRE = 0;
  SCH_AIR = 1;
  SCH_WATER = 2;
  SCH_EARTH = 3;
  SCH_NATURE = 4;
  SCH_HEALING = 5;
  SCH_CURSING = 6;
  SCH_COMBAT = 7;
  SCH_PROTECTION = 8;
  SCH_LORE = 9;
  SCH_NECROMANCY = 10;
  SCH_DEMONOLOGY = 11;
  SCH_MIND = 12;
  SCH_ENCHANTMENT = 13;
  SCH_TRAVEL = 14;

  { Hacks to allow handling of pseudo-items like gold }
  ITEM_GOLD = MAXINT - 3;
  ITEM_SILVER = MAXINT - 2;
  ITEM_BRONZE = MAXINT - 1;
  CHAR_GOLD = '$';

  { Version String }
  VersionString = 'Alpha 24';

  { Scroll types }
  ENCHANT_ARMOUR = 1;
  ENCHANT_WEAPON = 2;
  REMOVE_CURSE = 3;
  TELEPORTATION = 4;
  IDENTIFY = 5;
  MAGIC_MAPPING = 6;
  FORGETFULNESS = 7;
  BLANK_SCROLL = 8;
  CURSING = 9;

  { Potion Types }
  HEALING = 1;
  SPEED = 2;
  RESTORE_ABILITIES = 3;
  CONFUSION = 4;
  BLINDNESS = 5;
  SEE_INVISIBLE = 6;
  EXTRA_HEALING = 7;
  PARALYSIS = 8;
  FREE_ACTION = 9;
  COMBAT_MASTERY = 10;
  COMBAT_REFLEXES = 11;
  MIGHT = 12;
  RESIST_FIRE = 13;
  RESIST_EARTH = 14;
  RESIST_AIR = 15;
  RESIST_WATER = 16;

  { NPCS }
  GUARDS = 0;
  SHOPKEEPERS = 1;
  HOUSEKEEPER = 11;
  PETCAT = 12;
  NUMBER_OF_GUARDS = 6;

  { Animation delay in milliseconds }
  ANIMATION_DELAY = 40;

  { Status Conditions }
  STEALTHED = 30;
  ENRAGED = 31;
  REGENERATING = 32;
  HASTED = 33;
  MASTERY = 34;
  REFLEXES = 35;
  FREEA = 36;
  SEEINV = 37;
  CONFUSED = 20;
  BLINDED = 21;
  DRAINED = 22;
  HELD = 23;
  RESISTING_FIRE = 26;
  RESISTING_EARTH = 27;
  RESISTING_AIR = 28;
  RESISTING_WATER = 29;

  { Amount of Stealth bonus to add to rolls when sneaking }
  STEALTH_BONUS = 15;

  { Resistance bonus for potions }
  RESISTANCE_BONUS = 40;

  { Colour Constants }
  STEALTH_COLOUR = clGray;
  BLIND_COLOUR = $00000000;
  FADE_COLOUR = $00404040;
  MONOCHROME_COLOUR = clWhite;
  RAGE_COLOUR = clRed;

  { Potion Duration }
  POTION_DURATION = 50;
implementation

end.
