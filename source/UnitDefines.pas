{ UnitDefines

  Copyright (c) 2007-2009 Dave Moore 

  Enums

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

unit UnitDefines;

interface

uses Contnrs, Graphics, UnitConst;

{ Character Class }
type crClass = (cNone = 0, cThief = 1, cKnight = 2, cMage = 3, cPriest = 4,
  cWarrior = 5);

{ Creature Type - note that Characters are currently automatically humanoids }
type crRace = (rNone = 0, rAnimal = 1, rHumanoid = 2, rConstruct = 3,
  rDemon = 4, rDragon = 5, rElemental = 6, rPlant = 7, rUndead = 8, rGiant = 9,
  rOoze = 10, rGoblinoid = 11, rOutsider = 12);

{ Character Race }
type crSubRace = (srNone = 0, srHuman = 1, srHalfing = 2, srOrc = 3,
  srDwarf = 4, srElf = 5);

{ Character Gender }
type crGender = (gNone = 0, gMale = 1, gFemale = 2, gOther = 3);

{ Creature Size - note that Characters are either medium or small depending on
  race }
type crSize = (sizNone = 0, sizFine = 1, sizDiminutive = 2, sizTiny = 3,
  sizSmall = 4, sizMedium = 5, sizLarge = 6, sizHuge = 7, sizGargantuan = 8,
  sizColossal = 9);

{ Monster con }
type monStatus = (monNormal = 0, monElite = 1, monUnique = 2);

{ Item Quality }
type crItemQuality = (iUnknownQuality = 0, iWorthless = 1, iCommon = 2,
  iSuperb = 3, iLegendary = 4, iEpic = 5, iArtifact = 6, iSpecial = 7);

{ Item type }
type crItemType = (iAny = 0, iWeapon = 1, iArmour = 2, iRing = 3, iWand = 4,
  iReageant = 5, iJunk = 6, iAmulet = 7, iPotion = 8, iSpellbook = 9,
  iScroll = 10, iMiscellaneous = 11, iConsumable = 12);

{ Item materials }
type crMaterial = (mMetal = 0, mLeather = 1, mCloth = 2, mWooden = 3,
  mStone = 4, mPaper = 5, mOrganic = 6, mOther = 7);

{ Item intended slot }
type crItemSlot = (iHead = 0, iNeck = 1, iChest = 2, iHands = 3, iArms = 4,
  iLegs = 5, iFeet = 6, iMainhand = 7, iOffhand = 8, iRanged = 9, iFinger = 10,
  iInventory = 11, iNone = 12, iFloor = 13, iBack = 14, iOnMonster = 15);

{ Attack results }
type aResult = (iNormal = 0, iFumble = 1, iCritical = 2);

{ Attack types }
type aType = (iMelee = 0, iRange = 1, iMagic = 2);

{ Message types }
type mType = (mesDefault = clSilver, mesFloorDescription = clSilver,
  mesYouCombatHit = clWhite, mesYouCombatMiss = clGray,
  mesMonCombatHit = $009393FF, mesMonCombatMiss = clGray,
  mesMonComeIntoView = clRed, mesYouKill = $000080FF,
  mesItemManipulation = $00FFFF7D, mesLevelUp = clLime, mesDoor = clWhite,
  mesStairsandPortals = clSilver, mesSkill = clGreen, mesError = clRed,
  mesFountain = clBlue, mesDiscoverMagicItem = clLime,
  mesMonsterAction = clWhite, mesMonsterSpeech = clWhite,
  mesStealth = clSilver, mesStatus = clLime);

{ Targeting interface modes}
type tTargetMode = (tgNone, tgDig, tgRanged);

{ Instruction type, used in keyboard and mouse handling }
type tInstructionType = (insQuit, insGet, insWizardMode, insSkills,
  insInventory, insDrop, insMagic, insDig, insDump, insHelp, insVersion,
  insQuaff, insSuicide, insHotBar, insItem, insEat, insRead, insIdentify,
  insStealth, insMonster, insSpell);

{ Unique status - held at the TDungeon level }
type tUniqueStatus = (uNone, uAlive, uKilled);

{ Character log note types }
type tCharacterNote = (nBegin, nReach, nDing, nItem, nNotice, nMonster, nUnique,
  nSkill, nDeath);

{ Inventroy status - this is set to allow the same inventory screen to be used
  for different things and to be responsive to different keypresses }
type tInventoryStatus = (invNormal, invMultidrop, invEat, invRead, invIdentify,
  invDrink);

{ Scroll statuses }
type tScrollStatus = (stUnknown = 0, stTried = 1, stKnown = 2);

{ Potion statuses }
type tPotionStatus = (poUnknown = 0, poTried = 1, poKnown = 2);

{ Scroll types - these should be identical to those listed in the SCROLLS table
  in the database }
type tScrollType = (scEnchantArmour = 1, scEnchantWeapon = 2, scRemoveCurse = 3,
  scTeleportation = 4, scIdentify = 5, scMagicMapping = 6, scForgetfulness = 7,
  scBlank = 8, scCursing = 9);

{ Projectile Type }
type tProjectile = (prMissile = 1, prBeam = 2, prBolt = 3);

implementation


end.
