{ UnitEngine

  Copyright (c) 2007-2009 Dave Moore 

  Implements the main game engine

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

unit UnitEngine;

interface


uses  Graphics, KeyboardHandler, Windows, Dialogs, Controls, Classes, SysUtils,
  ComCtrls, StdCtrls, Forms, ExtCtrls, Messages, Contnrs, Math, ShellAPI,
  HotLog, UnitDefines, UnitInit, UnitDungeon, UnitDisplay, UnitConst, UnitVars,
  UnitItem, UnitVersion, UnitInterfaces, UnitCreature, UnitMonster, UnitWizard,
  UnitFunctions, UnitHiScores, UnitTimer;

{ Main routine for handling keyboard input across the entire game }
procedure HandleKeyboardInput(KH: TKeyboardHandler; TargetMode:
	tTargetMode = tgNone);

{ Main routine for handling mouse input across the entire game }
procedure HandleMouseInput(Instruction: tInstructionType);

{ Main routine for handling screen input }
procedure HandleScreenInput(X: Integer; Y: Integer; Button: TMouseButton);

{ Move the player in a specified direction, also handles melee combat }
procedure MovePlayer(X: Integer; Y: Integer);

{ Dig in a specified direction }
procedure Dig(X: Integer; Y: Integer);

{ OpenDoor is the routine used to open doors as part of moving, i.e. OpenDoors
  is triggered by pressing 'o' and will affect all local doors whereas OpenDoor
  will only open a door in the direction of movement }
procedure OpenDoor(X: Integer; Y: Integer);

{ Close one or more doors adjacent to a square }
procedure CloseDoor(X: Integer; Y: Integer);

{ Open one or more doors adjacent to a square }
procedure OpenDoors(X: Integer; Y: Integer);

{ Generate and display the text that is added to the message log when the player
  moves over an object or objects on the ground; this also picks up coin }
procedure DisplayObjectText(ObjectID: Integer);

{ Handle displaying of dungeon terrain effects in the message log }
procedure DisplayEffectText(DungeonTheme: Integer);

{ Handle displaying of dungeon terrain effects in the message log }
procedure DisplayDarkerEffectText(DungeonTheme: Integer);

{ Move up and down between dungeons via stairs }
procedure TraverseDungeon(Direction: Integer);

{ Add text to the message log }
procedure UpDateLog(MessageToAdd: String; messageType: mType = mesDefault);

{ Add text to the message log in a specified colour }
procedure UpDateLogColor(MessageToAdd: String; messageColor: TColor = clWhite);

{ Pick up an item }
procedure GetAnItem(ItemID: Integer);

{ Load the inventory contents into the inventory screen }
procedure LoadInventoryDetails(Player: TCreature);

{ Handle highlighting of slots on the inventory }
procedure InventoryHighlightDestination(SenderItem: TItem);

{ Load and display the skills }
procedure LoadSkills(Player: TCreature);

{ Load and display magic }
procedure LoadMagic(Player: TCreature);

{ Display spells belonging to a school of magic }
procedure ShowSchoolSpells(Player: TCreature; School: String);

 { Pass a game turn }
procedure PassATurn;

{ Handle drinking from fountains }
procedure DrinkFromFountain;

{ Rest for a number of turns }
procedure Rest(TurnsToRest: Integer);

{ Do monster AI }
procedure ProcessCreatures(Dungeon: TDungeonLevel);

{ Do townspeople AI }
procedure ProcessTownsPeople(Dungeon: TDungeonLevel);

{ Recalculate terrain costs for pathing }
procedure ReCalculateTerrainCosts(Dungeon: TDungeonLevel; PlayerX: Integer; 
	PlayerY: Integer);
	
{ Awaken nearby creatures }
procedure AwakenCreatures(SourceList: TObjectlist; ActiveList: TObjectList; 
	Dungeon: TDungeonLevel; PlayerX: Integer; PlayerY: Integer);
	
{ Try and draw a digital line between two points to check for uninterrupted
  vision }
function XCanSeeY(Dungeon: TDungeonLevel; x1:integer; y1:integer; x2:integer; 
	y2:integer):boolean;
	
{ Find a path between two points }
function GetPathXToY(Dungeon: TDungeonLevel; x1:integer; y1:integer; 
	x2:integer; y2:integer; var xloc: Integer; var yloc: Integer):boolean;
	
{ Check if a move is permitted }
function CanMoveXtoY(Dungeon: TDungeonLevel; newx: integer; newy: integer; 
	playerx: integer; playery: integer): Boolean; overload;
	
{ Move one square towards a specific direction }
function MoveXtoY(Dungeon: TDungeonLevel; Monster: TMonster; newx: integer; 
	newy: Integer): Boolean;
	
{ Check if a move is permitted }
function CanMoveXtoY(Dungeon: TDungeonLevel; newx: integer; 
	newy: integer): Boolean;  overload;
	
{ Get a random direction }
function GetRandomDirection(Dungeon: TDungeonLevel; Monster: TMonster; 
	PlayerX: Integer; PlayerY: Integer; var OffSetX: Integer; 
	var OffSetY: Integer): Boolean;
	
{ Melee attack rolls }
function XCanHitY(LocalMonster: TMonster; LocalPlayer: TCreature; 
	var Attack: String; var AttackResult: aResult; AttackType: aType = iMelee):
  Boolean; overload;
	
{ Melee attack rolls }
function XCanHitY(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Attack: String; var AttackResult: aResult):
  Boolean; overload;
	
{ Ranged/Magic Attack Rolls }
function XCanHitYRanged(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Attack: String; var AttackResult: aResult): Boolean; overload;
	
{ Handle player physical attacks }
function XMeleeDamagesY(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Damage: String; AttackResult: aResult): Integer; overload;

{ Handle player missile attacks }
function XMissileDamagesY(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Damage: String; AttackResult: aResult): Integer; overload;
	
{ Handle physical attacks from Monsters }
function XMeleeDamagesY(LocalMonster: TMonster; LocalPlayer: TCreature; 
	var Damage: String; AttackResult: aResult): Integer; overload;
	
{ Handle Magic Attacks from Monsters }
function XMagicDamagesY(LocalMonster: TMonster; LocalPlayer: TCreature; 
	var Damage: String; AttackResult: aResult; 
	MagicSchool: Integer): Integer; overload;

{ Handle Poison Attacks from Monsters }
function XPoisonsY(LocalMonster: TMonster; LocalPlayer: TCreature): Boolean;
	
{ Handle Monster Speech/Actions }
function MonsterSpeaks(Monster: TMonster; Dungeon: TDungeonLevel; 
	MonsterKnown: Boolean = True): Boolean;
	
{ Handle stealth and hiding }
function CanDetect(LocalMonster: TMonster; LocalPlayer: TCreature): Boolean;

{ Handle pseudoid }
procedure PseduoIDTick(LocalPlayer: TCreature);

{ Read scrolls }
function ReadScroll(ScrollToRead: TItem; LocalPlayer: TCreature): String;

{ Drink Potions }
function DrinkPotion(PotionToDrink: TItem; LocalPlayer: TCreature): String;

var
  TownDownX: Integer;
  TownDownY: Integer;
  
implementation

uses UnitOtherFuncs;

{ Main routine for handling keyboard input across the entire game - in many
  places in this routine we have to disable the main keyboard handling routine
  while we're in it to make sure it is not called again whilst the previous
  call is being dealt with }
procedure HandleKeyboardInput(KH: TKeyboardHandler; TargetMode:
	tTargetMode = tgNone);
var
  KeyPressed: Boolean;
  Morgue: TStringList;
  KeyToCheck: Char;
  InventorySlot: Integer;
  LocalItem: TItem;
  Description: TStringList;
  FunctionKey: Integer;
  ColouredItemText: String;
  CharacterName: String;
  ComponentName: String;
  ItemPanel: TComponent;
  FloorItem: TItem;
  Loop: Integer;
  ItemToDrop: TItem;
  ItemToDropIndex: Integer;
  ItemToEat: TItem;
  ItemToRead: TItem;
  ItemToIdentify: TItem;
  ItemToDrink: TItem;
  PotionResult: String;
  ScrollResult: String;
  ViableMagicKeys: String;
  Key: Char;
  LowerKey: Char;
  SpellSchool: Integer;
  Letter: Char;
begin
	{ Logging }
  // hLog.Add('{now} {lNum} UnitEngine.HandleKeyboardInput()');

  try
    { Make sure that we don't respond to keys if the main window is minimised
      or otherwise blocked }
    if not(application.Active) or BlockKeyBoardInput then
      Exit;

    { Set the Target Mode }
    if Assigned(FormDisplay.Game) then
      FormDisplay.Game.CurrentTargetMode := TargetMode;

    { Handle character creation }
    if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetCharacterCreate then
    begin
      { Handle first name screen }
      if FormDisplay.PageControlCharacterCreate.ActivePage =
        FormDisplay.TabSheetChar1 then
      begin
        { CTRL creates a random name }
        if (KH.VirtualKeyDown[VK_LCONTROL]) or
          (KH.VirtualKeyDown[VK_RCONTROL]) then
          FormDisplay.EditName.Text := GenerateName;

        { TAB continues to the next section but only if a name has been given }
        if (KH.VirtualKeyDown[VK_TAB]) and
          (Length(Trim(FormDisplay.EditName.Text)) > 0)
        then
        begin
          { Set the name }
          FormDisplay.NewCharacter.Name := FormDisplay.EditName.Text;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar2;
        end;

        { ESC cancels character creation }
        if KH.VirtualKeyDown[VK_ESCAPE] then
        begin
          { Reset the character }
          FormDisplay.NewCharacter.Name := '';
          FormDisplay.EditName.Text := '';
          FormDisplay.MemoBackground.Lines.Clear;
          FormDisplay.MemoBackground.Text := '';

          { Change back to the main screen }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar1;
          FormDisplay.PageControlMain.ActivePage := FormDisplay.TabSheetIntro;
          FormDisplay.Caption := ' Kharne ' + GetVersion;
        end;
      end
      { Handle second screen gender }
      else if FormDisplay.PageControlCharacterCreate.ActivePage =
        FormDisplay.TabSheetChar2 then
      begin
        { 'a' chooses male }
        if (KH.VirtualKeyDown[Ord('a')]) or (KH.VirtualKeyDown[Ord('A')]) then
        begin
          { Set the gender }
          FormDisplay.NewCharacter.Gender := gMale;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar3;
        end;

        { ESC cancels character creation }
        if KH.VirtualKeyDown[VK_ESCAPE] then
        begin
          { Reset the character }
          FormDisplay.NewCharacter.Gender := gNone;

          { Change back to to the previous screen }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar1;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;
        end;

        { 'b' chooses female }
        if (KH.VirtualKeyDown[Ord('b')]) or (KH.VirtualKeyDown[Ord('B')]) then
        begin
          { Set the gender }
          FormDisplay.NewCharacter.Gender := gFemale;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar3;
        end;
      end
      { Handle third screen race }
      else if FormDisplay.PageControlCharacterCreate.ActivePage =
        FormDisplay.TabSheetChar3 then
      begin
        { Set the standard options }
        FormDisplay.NewCharacter.Race := rHumanoid;

        { ESC cancels character creation }
        if KH.VirtualKeyDown[VK_ESCAPE] then
        begin
          { Reset the character }
          FormDisplay.NewCharacter.Gender := gNone;

          { Change back to to the previous screen }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar2;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;
        end;

        { 'a' chooses human }
        if (KH.VirtualKeyDown[Ord('a')]) or (KH.VirtualKeyDown[Ord('A')]) then
        begin
          { Set the race }
          FormDisplay.NewCharacter.SubRace := srHuman;

          { Set the size }
          FormDisplay.NewCharacter.Size := sizMedium;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;
        end;

        { 'b' chooses dwarf }
        if (KH.VirtualKeyDown[Ord('b')]) or (KH.VirtualKeyDown[Ord('B')]) then
        begin
          { Set the race }
          FormDisplay.NewCharacter.SubRace := srDwarf;

          { Set the size }
          FormDisplay.NewCharacter.Size := sizMedium;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;
        end;

        { 'c' chooses elf}
        if (KH.VirtualKeyDown[Ord('c')]) or (KH.VirtualKeyDown[Ord('C')]) then
        begin
          { Set the race }
          FormDisplay.NewCharacter.SubRace := srElf;

          { Set the size }
          FormDisplay.NewCharacter.Size := sizMedium;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;
        end;

        { 'd' chooses orc }
        if (KH.VirtualKeyDown[Ord('d')]) or (KH.VirtualKeyDown[Ord('D')]) then
        begin
          { Set the race }
          FormDisplay.NewCharacter.SubRace := srOrc;

          { Set the size }
          FormDisplay.NewCharacter.Size := sizMedium;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;
        end;

        { 'e' chooses halfling }
        if (KH.VirtualKeyDown[Ord('e')]) or (KH.VirtualKeyDown[Ord('E')]) then
        begin
          { Set the race }
          FormDisplay.NewCharacter.SubRace := srHalfing;

          { Set the size }
          FormDisplay.NewCharacter.Size := sizSmall;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;
        end;
      end
      { Handle fourth screen class }
      else if FormDisplay.PageControlCharacterCreate.ActivePage =
        FormDisplay.TabSheetChar4 then
      begin
        { 'a' chooses knight }
        if (KH.VirtualKeyDown[Ord('a')]) or (KH.VirtualKeyDown[Ord('A')]) then
        begin
          { Set the class }
          FormDisplay.NewCharacter.CClass := cKnight;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;
          
          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar5;
        end;

        { 'b' chooses dwarf }
        if (KH.VirtualKeyDown[Ord('b')]) or (KH.VirtualKeyDown[Ord('B')]) then
        begin
          { Set the class }
          FormDisplay.NewCharacter.CClass := cMage;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar5;
        end;

        { 'c' chooses priest }
        if (KH.VirtualKeyDown[Ord('c')]) or (KH.VirtualKeyDown[Ord('C')]) then
        begin
          { Set the class }
          FormDisplay.NewCharacter.CClass := cPriest;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar5;
        end;

        { 'd' chooses thief }
        if (KH.VirtualKeyDown[Ord('d')]) or (KH.VirtualKeyDown[Ord('D')]) then
        begin
          { Set the class }
          FormDisplay.NewCharacter.CClass := cThief;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar5;
        end;

        { 'e' chooses warrior }
        if (KH.VirtualKeyDown[Ord('e')]) or (KH.VirtualKeyDown[Ord('E')]) then
        begin
          { Set the class }
          FormDisplay.NewCharacter.CClass := cWarrior;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;

          { Move to the next tab }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar5;
        end;

        { ESC cancels character creation }
        if KH.VirtualKeyDown[VK_ESCAPE] then
        begin
          { Reset the character }
          FormDisplay.NewCharacter.CClass := cNone;

          { Change back to to the previous screen }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar3;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;
        end;
      end
      { Handle fifth and final screen background }
      else if FormDisplay.PageControlCharacterCreate.ActivePage =
        FormDisplay.TabSheetChar5 then
      begin
        { CTRL creates a random background }
        if (KH.VirtualKeyDown[VK_LCONTROL]) or
          (KH.VirtualKeyDown[VK_RCONTROL]) then
          FormDisplay.MemoBackground.Text := GenerateBackground;

        { ESC cancels character creation }
        if KH.VirtualKeyDown[VK_ESCAPE] then
        begin
          { Reset the character }
          FormDisplay.MemoBackground.Lines.Clear;
          FormDisplay.MemoBackground.Text := '';
          FormDisplay.NewCharacter.Background := '';

          { Change back to to the previous screen }
          FormDisplay.PageControlCharacterCreate.ActivePage :=
            FormDisplay.TabSheetChar4;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(500);
          Screen.Cursor := crDefault;
        end;

        { Handle accepting a character }
        if KH.VirtualKeyDown[VK_TAB] then
        begin
          { Set the background (which is optional }
          FormDisplay.NewCharacter.Background := FormDisplay.MemoBackground.Text;

          { Reset the displayed background }
          FormDisplay.MemoBackground.Lines.Clear;

          { Now create the character }
          CharacterName := CreateCharacter(FormDisplay.NewCharacter);

          { And start a new game! }
          if Length(Trim(CharacterName)) > 0 then
            FormDisplay.Initialise(CharacterName, True);
        end;
      end
    end
    { Handle item keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetItem then
    begin
      { ESC exits the item screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        { Go back to the inventory }
        HandleMouseInput(insItem);
      end;
    end
    { Handle monster keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetMonster then
    begin
      { ESC exits the item screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        { Go back to the inventory }
        HandleMouseInput(insMonster);
      end;
    end
    { Handle character skills keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetSkills then
    begin
      { ESC exits the character skills screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insQuit);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end;
    end
    { Handle magic keypresses }
    else if (FormDisplay.PageControlMain.ActivePage = 
      FormDisplay.TabSheetMagic) then
    begin
      { ESC exits the character magic screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insQuit);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end
      else
      begin
        { Handle other keypresses to select spellschool }

        { First of all, find out how many schools of magic are castable from }
        ViableMagicKeys := FormDisplay.Game.Player.GetSpellSchools;

        { If we can cast spells }
        if Length(ViableMagicKeys) > 0 then
        begin
          { Iterate through the available spell schools checking to see if the
            appropriate key has been pressed }
          for Loop := 1 to Length(ViableMagicKeys) do
          begin
            LowerKey := ViableMagicKeys[Loop];
            Key := UpCase(ViableMagicKeys[Loop]);

            if (KH.VirtualKeyDown[Ord(LowerKey)] = True) or
              (KH.VirtualKeyDown[Ord(Key)] = True) then
            begin
              { Select the spell school based upon the fixed letter by finding
                the letter of the item listed in the list }
              for SpellSchool := 0 to
                FormDisplay.ListViewAvailableSchools.Items.Count - 1 do
              begin
                Letter := (FormDisplay.ListViewAvailableSchools.Items[SpellSchool] as TListItem).
              end;
            end;
          end;
        end;

        
        //buildupalist


        

              // FINDFINDTODODOTOTOTO   


      //else if (KH.VirtualKeyDown[Ord('S')] = True) or
      //  (KH.VirtualKeyDown[Ord('s')] = True) then




      begin
        HandleMouseInput(insSpell);
      end
      end
    end
    { Handle help keypresses }
    else if (FormDisplay.PageControlMain.ActivePage = 
      FormDisplay.TabSheetHelp) then
    begin
      { ESC exits the help screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insQuit);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end;
    end
    { Handle character dump keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetDump then
    begin
      { ESC exits the character dump screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        { Since character dump can take place in either the characrer death
          screen or anytime ingame (by pressing 'a') then we have to handle both
          circumstances }
        if FormDisplay.Game.Player.Dead then
        begin
          FormDisplay.Timer.Enabled := False;
          FormDisplay.PageControlMain.ActivePage := 
            FormDisplay.TabSheetHiScores;
          EmptyKeyQueue;
          FormDisplay.Timer.Enabled := True;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(100);
          Screen.Cursor := crDefault;
        end
        else
        begin
          FormDisplay.Timer.Enabled := False;
          HandleMouseInput(insQuit);
          EmptyKeyQueue;
          FormDisplay.Timer.Enabled := True;

          { Sleep to avoid keypress reptition }
          Screen.Cursor := crHourGlass;
          Sleep(100);
          Screen.Cursor := crDefault;
        end;
      end
      { S saves the character dump }
      else if (KH.VirtualKeyDown[Ord('S')] = True) or
        (KH.VirtualKeyDown[Ord('s')] = True) then
      begin
        Morgue := TStringList.Create;
        try
          { Generate the character dump and save it to disc with the filename
            specifiec by the user }
          Morgue.Text := CharDump(FormDisplay.Game.Player.Dead);
          if FormDisplay.SaveDumpDialog.Execute then
            Morgue.SaveToFile(FormDisplay.SaveDumpDialog.FileName);
        finally
          Morgue.Free;
        end;
      end;
    end
    { Handle version log keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetVersion then
    begin
      { ESC exits the version log screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insQuit);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(500);
        Screen.Cursor := crDefault;
      end;
    end
    { Handle intro screen keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetIntro then
    begin
      { V displays the high scores }
      if (KH.VirtualKeyDown[Ord('V')] = True) or
        (KH.VirtualKeyDown[Ord('v')] = True) then
      begin
        FormDisplay.Timer.Enabled := False;
        EmptyKeyQueue;
        FormDisplay.RichEditHiScores.Clear;
        DisplayHiScores(FormDisplay.RichEditHiScores, '', False);
        FormDisplay.PageControlMain.ActivePage := FormDisplay.TabSheetHiScores;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end
      { S starts a new game } 
      else if (KH.VirtualKeyDown[Ord('S')] = True) or
        (KH.VirtualKeyDown[Ord('s')] = True) then
        FormDisplay.Starta1Click(nil)
      { C loads a an existing game }
      else if (KH.VirtualKeyDown[Ord('C')] = True) or
        (KH.VirtualKeyDown[Ord('c')] = True) then
        FormDisplay.ContinueanExistingGame1Click(nil)
      { T displays the tutorial and help }
      else if (KH.VirtualKeyDown[Ord('T')] = True) or
        (KH.VirtualKeyDown[Ord('t')] = True) then
        ShellExecute(FormDisplay.Handle,'open', 
          PChar(ExtractFilePath(Application.ExeName) + '\docs\README.TXT'), 
          nil, nil, SW_SHOWNORMAL)
      { Q quits }
      else if (KH.VirtualKeyDown[Ord('Q')] = True) or
        (KH.VirtualKeyDown[Ord('q')] = True) then
        FormDisplay.Exit1Click(nil);
    end
    { Handle hi scores keypresses }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetHiScores then
    begin
      { ESC exits the hi score screen }
      if (KH.VirtualKeyDown[VK_ESCAPE] = True) then
      begin
        FormDisplay.Timer.Enabled := False;
        EmptyKeyQueue;
        FormDisplay.PageControlMain.ActivePage := FormDisplay.TabSheetIntro;
        FormDisplay.Caption := ' Kharne ' + GetVersion;
        InGame := False;
        
        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end
      { I, if we are on the character death version of the hiscores screen will
        display the character inventory }
      else if ((KH.VirtualKeyDown[Ord('I')] = True) or
       (KH.VirtualKeyDown[Ord('i')] = True)) and InGame then
      begin
        { TODO: Yet to do inventory view }
      end
      { S, if we are on the character death version of the hiscores screen will
        save the character dump out to a specified file }
      else if ((KH.VirtualKeyDown[Ord('S')] = True) or
        (KH.VirtualKeyDown[Ord('s')] = True)) and InGame then
      begin
        Morgue := TStringList.Create;
        try
          Morgue.Text := CharDump(True);
          if FormDisplay.SaveDumpDialog.Execute then
          begin
            Morgue.SaveToFile(FormDisplay.SaveDumpDialog.FileName);
          end;
        finally
          Morgue.Free;
        end;
      end
      { V, if we are on the character death version of the hiscores screen will
        view the character dump }
      else if ((KH.VirtualKeyDown[Ord('V')] = True) or
        (KH.VirtualKeyDown[Ord('v')] = True)) and InGame then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insDump);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end
    end
    { Now handle pressing of keys on the inventory screen }
    else if FormDisplay.PageControlMain.ActivePage =
      FormDisplay.TabSheetInventory then
    begin
      { ESC exits the inventory screen }
      if KH.VirtualKeyDown[VK_ESCAPE] = True then
      begin
        FormDisplay.Timer.Enabled := False;
        HandleMouseInput(insQuit);
        EmptyKeyQueue;
        FormDisplay.Timer.Enabled := True;

        { Sleep to avoid keypress reptition }
        Screen.Cursor := crHourGlass;
        Sleep(100);
        Screen.Cursor := crDefault;
      end
      else
      { Check to see if an item key has been pressed }
      begin
        { Reset the multidrop for each keypress }
        InventorySlot := 0;

        { Find the corresponding inventory slot to the letter }
        for KeyToCheck := 'a' to 'z' do
          if (KH.VirtualKeyDown[Ord(UpperCase(KeyToCheck)[1])] = True) or
          (KH.VirtualKeyDown[Ord(KeyToCheck)] = True) then
            InventorySlot := Ord(UpperCase(KeyToCheck)[1]) -
              START_OF_ASCII_LETTERS;

        { Find the corresponding equipped slot to the function key }
        for FunctionKey := VK_F1 to VK_F12 do
          if KH.VirtualKeyDown[FunctionKey] then
          begin
            case FunctionKey of
              VK_F1: InventorySlot := S_HEAD;
              VK_F2: InventorySlot := S_NECK;
              VK_F3: InventorySlot := S_CHEST;
              VK_F4: InventorySlot := S_ARMS;
              VK_F5: InventorySlot := S_HANDS;
              VK_F6: InventorySlot := S_LEGS;
              VK_F7: InventorySlot := S_FEET;
              VK_F8: InventorySlot := S_MAINHAND;
              VK_F9: InventorySlot := S_OFFHAND;
              VK_F10: InventorySlot := S_RANGED;
              VK_F11: InventorySlot := S_LEFTFINGER;
              VK_F12: InventorySlot := S_RIGHTFINGER;
            else
              InventorySlot := 0;
            end;

            { Break if we've found the key and hence the item }
            if InventorySlot > 0 then
              Break;
          end;

        { If we have an item in the slot and we're not dropping }
        if InventoryStatus = invNormal then
        begin
          if InventorySlot > 0 then
          begin
            { If there is an item in the inventory slot}
            if FormDisplay.Game.Player.Inventory[InventorySlot] > 0 then
            begin
              { Get the item }
              LocalItem :=
                GItemList[FormDisplay.Game.Player.Inventory[InventorySlot]]
                as TItem;

              { Clear the description view }
              FormDisplay.RichEditItem.Clear;

              { Generate the item description and add it }
              Description := TStringList.Create;
              try
                LocalItem.Description(Description);
                FormDisplay.RichEditItem.Lines.DelimitedText :=
                  Description.DelimitedText;
              finally
                Description.Free;
                Description := nil;
              end;

              { Scroll to the beginning }
              FormDisplay.RichEditItem.SelStart := 0;
              FormDisplay.RichEditItem.Perform(EM_SCROLLCARET, 0, 0);

              { Add the caption }
              ColouredItemText := Trim(LocalItem.Name);

              { Change the font colour }
              FormDisplay.RichEditItem.SelAttributes.Color := LocalItem.TextColour;

              { Add the text }
              FormDisplay.RichEditItem.SelText := ColouredItemText + #10;

              { Set the selected text back to the default }
              FormDisplay.RichEditItem.SelAttributes.Color := clWindowText;

              { Add a line }
              FormDisplay.RichEditItem.Lines.Insert(1, '');

              { Swap to the item view }
              FormDisplay.Timer.Enabled := False;
              FormDisplay.PageControlMain.ActivePage :=
                FormDisplay.TabSheetItem;
              EmptyKeyQueue;
              FormDisplay.Timer.Enabled := True;

              { Sleep to avoid keypress reptition }
              Screen.Cursor := crHourGlass;
              Sleep(100);
              Screen.Cursor := crDefault;

              { Remove focus from the description }
              FormDisplay.PanelItemTopBorder.SetFocus;
            end;
          end;
        end
        { Handle reading of scrolls }
        else if InventoryStatus = invRead then
        begin
          { Make sure we have an item }
          if (InventorySlot > 0) and
            (FormDisplay.Game.Player.Inventory[InventorySlot] > 0) then
          begin
            { Get the item }
            ItemToRead := GItemList
              [FormDisplay.Game.Player.Inventory[InventorySlot]] as TItem;

            { Check it is a valid scroll }
            if ItemToRead.ItemType = iScroll then
            begin
              if FormDisplay.Game.Player.Has(ENRAGED) or
                FormDisplay.Game.Player.Has(CONFUSED) or
                FormDisplay.Game.Player.Has(BLINDED) then
              begin
                { Alert that we can't do this }
                UnitEngine.UpdateLog('You can''t do that right now', mesError);

                { Reset the inventory status so the next time we go into the
                  inventory it is the basic view }
                InventoryStatus := invNormal;

                { Return the main screen }
                FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
                FormDisplay.
                  DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              end
              else
              begin
                { Read the scroll }
                ScrollResult := ReadScroll(ItemToRead, FormDisplay.Game.Player);

                { Blank paper isn't consumed }
                if ItemToRead.Position <> BLANK_SCROLL then
                begin
                  { Handle reading stacked items }
                  if ItemToRead.Count > 1 then
                    ItemToRead.Count := ItemToRead.Count - 1;
                  Dec(FormDisplay.Game.Player.InventoryCount[InventorySlot]);

                  { If we have read the last item in the stack empty it }
                  if FormDisplay.Game.Player.InventoryCount[InventorySlot] = 0 then
                    FormDisplay.Game.Player.Inventory[InventorySlot] := 0;
                end;

                { Handle identify scrolls }
                if  ItemToRead.Position <> IDENTIFY then
                begin
                  { Update the message log }
                  UnitEngine.UpdateLog(ScrollResult, mesItemManipulation);

                  { Pass a turn }
                  PassATurn;

                  { Refresh the inventory }
                  LoadInventoryDetails(FormDisplay.Game.Player);

                  { Reset the inventory status so the next time we go into the
                    inventory it is the basic view }
                  InventoryStatus := invNormal;

                  { Get rid of the item }
                  ItemToRead.ItemSlot := iNone;

                  { Return the main screen }
                  FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
                  FormDisplay.
                    DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
                end;

                { Sleep to avoid keypress reptition }
                Screen.Cursor := crHourGlass;
                Sleep(100);
                Screen.Cursor := crDefault;
              end;
            end;
          end;
        end { Handle drinking }
        else if InventoryStatus = invDrink then
        begin
          { Make sure we have an item }
          if (InventorySlot > 0) and
            (FormDisplay.Game.Player.Inventory[InventorySlot] > 0) then
          begin
            { Make sure we can drink }
            if FormDisplay.Game.Player.Has(ENRAGED) then
            begin
              { Display a suitable alert }
              UnitEngine.UpdateLog('You can''t do that right now', mesError);

              { Reset the inventory status so the next time we go into the
                inventory it is the basic view }
              InventoryStatus := invNormal;

              { Return the main screen }
              FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
              FormDisplay.
                DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
            end
            else
            begin
              { Get the item }
              ItemToDrink := GItemList
                [FormDisplay.Game.Player.Inventory[InventorySlot]] as TItem;

              { Check it is a valid consumable }
              if ItemToDrink.ItemType = iPotion then
              begin
                { Drink the potion }
                PotionResult := DrinkPotion(ItemToDrink, FormDisplay.Game.Player);

                { Check for drinking success }
                if PotionResult = '' then
                begin
                  { Handle drinking stacked items }
                  if ItemToDrink.Count > 1 then
                    ItemToDrink.Count := ItemToDrink.Count - 1;
                  Dec(FormDisplay.Game.Player.InventoryCount[InventorySlot]);

                  { If we have drunk the last item in the stack empty the stack }
                  if FormDisplay.Game.Player.InventoryCount[InventorySlot] = 0 then
                    FormDisplay.Game.Player.Inventory[InventorySlot] := 0;

                  { Update the message log }
                  UnitEngine.UpdateLog(PotionResult, mesItemManipulation);

                  { Pass a turn }
                  PassATurn;
                end;

                { Refresh the inventory }
                LoadInventoryDetails(FormDisplay.Game.Player);

                { Reset the inventory status so the next time we go into the
                  inventory it is the basic view }
                InventoryStatus := invNormal;

                { Get rid of the item }
                ItemToDrink.ItemSlot := iNone;

                { Return the main screen }
                FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
                FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

                { Sleep to avoid keypress reptition }
                Screen.Cursor := crHourGlass;
                Sleep(100);
                Screen.Cursor := crDefault;
              end;
            end;
          end;
        end
        { Handle eating }
        else if InventoryStatus = invEat then
        begin
          { Make sure we have an item }
          if (InventorySlot > 0) and
            (FormDisplay.Game.Player.Inventory[InventorySlot] > 0) then
          begin
            { Get the item }
            ItemToEat := GItemList
              [FormDisplay.Game.Player.Inventory[InventorySlot]] as TItem;

            { Check it is a valid consumable }
            if ItemToEat.ItemType = iConsumable then
            begin
              { Handle eating stacked items }
              if ItemToEat.Count > 1 then
                ItemToEat.Count := ItemToEat.Count - 1;
              Dec(FormDisplay.Game.Player.InventoryCount[InventorySlot]);

              { If we have ea ten the last item in the stack empty the stack }
              if FormDisplay.Game.Player.InventoryCount[InventorySlot] = 0 then
                FormDisplay.Game.Player.Inventory[InventorySlot] := 0;

              { Provide some nutrition to the player }
              FormDisplay.Game.Player.Feed(FOOD_NUTRITION_VALUE);

              { Update the message log }
              UnitEngine.UpdateLog(Format('You eat %s - mmmmm, %s!',
                [Lower(ItemToEat.SingleName), GetFoodDescription]),
                mesItemManipulation);

              { Pass a turn }
              PassATurn;

              { Refresh the inventory }
              LoadInventoryDetails(FormDisplay.Game.Player);

              { Reset the inventory status so the next time we go into the
                inventory it is the basic view }
              InventoryStatus := invNormal;

              { Get rid of the item }
              ItemToEat.ItemSlot := iNone;

              { Return the main screen }
              FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
            end;
          end;
        end
        { Handle identifying an item }
        else if InventoryStatus = invIdentify then
        begin
          { Make sure we have an item }
          if (InventorySlot > 0) and
            (FormDisplay.Game.Player.Inventory[InventorySlot] > 0) then
          begin
            { Use the scroll }
            UpdateLog('As you read the scroll, it crumbles to dust',
              mesItemManipulation);

            { Get the item }
            ItemToIdentify := GItemList
              [FormDisplay.Game.Player.Inventory[InventorySlot]] as TItem;

            { Check it is an unknown item }
            if ItemToIdentify.Known = False then
            begin
              { Set the item to known }
              ItemToIdentify.Known := True;

              { Check for scrolls }
              if ItemToIdentify.ItemType = iScroll then
              begin
                FormDisplay.Game.SetScrollStatus(ItemToIdentify.Position,
                  stKnown);

                { Set the scroll result for identify }
                if FormDisplay.Game.ScrollIDed[ItemToIdentify.Position] <> stKnown then
                begin
                  FormDisplay.Game.SetScrollStatus(IDENTIFY, stKnown);
                  UnitEngine.UpdateLog('This is Scroll of Identify',
                    mesItemManipulation);
                end;
              end;

              { Check for potions }
              if ItemToIdentify.ItemType = iPotion then
                FormDisplay.Game.SetPotionStatus(ItemToIdentify.Position,
                  poKnown);

              { Output an appropriate message }
              UpdateLog(Format('This is %s', [Lower(ItemToIdentify.Name)]));
            end;

            { Pass a turn }
            PassATurn;

            { Refresh the inventory }
            LoadInventoryDetails(FormDisplay.Game.Player);

            { Reset the inventory status so the next time we go into the
              inventory it is the basic view }
            InventoryStatus := invNormal;

            { Return the main screen }
            FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
            FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
          end;
        end
        { Handle selection of items in multidrop }
        else if InventoryStatus = invMultidrop then
        begin
          { Only allow filled slots to be dropped }
          if (InventorySlot > 0) and
            (FormDisplay.Game.Player.Inventory[InventorySlot] > 0) then
          begin
            { Get the panel }
            ComponentName := 'BackPanel' + IntToStr(InventorySlot);
            ItemPanel := FormDisplay.FindComponent(ComponentName);

            { Set the slot panel to indicate that its been selected }
            if (ItemPanel as TPanel).Color = $00800040 then
            begin
              { Unselect and item to drop }
              ItemsToDrop.Delete(ItemsToDrop.IndexOf(IntToStr(InventorySlot)));

              { Reset the colour back to default }
              (ItemPanel as TPanel).Color := $00000000;
            end
            else
            begin
              { Select and item to drop }
              ItemsToDrop.Add(IntToStr(InventorySlot));

              { Reset the colour back to default }
              (ItemPanel as TPanel).Color := $00800040;
            end;
          end;
        end;

        { Handle multidropping of items }
        if (KH.VirtualKeyDown[VK_RETURN]) and
          (InventoryStatus = invMultidrop) then
        begin
          { Check we have items to drop }
          if ItemsToDrop.Count > 0 then
          begin
            { Check we're not in a shop }
            if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and
              (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
              FormDisplay.Game.PlayerY] = D_TILE_SHOP) then
              beep
            else
            begin
              { For each item we want to drop }
              for Loop := 0 to ItemsToDrop.Count - 1 do
              begin
                { Get each item }
                ItemToDropIndex := FormDisplay.Game.Player.Inventory
                  [StrToInt(ItemsToDrop[Loop])];
                ItemToDrop := GItemList[ItemToDropIndex] as TItem;

                { Set the item location to the floor }
                ItemToDrop.Location := iFloor;

                { Reset any pseudoID progress }
                ItemToDrop.IDCounter := 0;

                { See if there already is an item on the tile - will always be
                  true after the first item drop }
                if FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
                  FormDisplay.Game.PlayerY] > 0 then
                begin
                  { Get the last item on the tile }
                  FloorItem := FormDisplay.Game.Dungeon.GetLastItemOnTile
                    (Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY));

                  { Reestablish the link }
                  FloorItem.NextItem := ItemToDropIndex;

                  { Clear the inventory slot }
                  FormDisplay.Game.Player.
                    InventoryCount[StrToInt(ItemsToDrop[Loop])] := 0;
                  FormDisplay.Game.Player.
                    Inventory[StrToInt(ItemsToDrop[Loop])] := 0;

                  { Update the message log }
                  UnitEngine.UpdateLog(Format('You drop %s',
                    [Lower(ItemToDrop.Name)]), mesItemManipulation);
                end
                else
                begin
                  { Place the item on the ground }
                  FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
                    FormDisplay.Game.PlayerY] := ItemToDropIndex;

                  { Clear the inventory slot }
                  FormDisplay.Game.Player.
                    InventoryCount[StrToInt(ItemsToDrop[Loop])] := 0;
                  FormDisplay.Game.Player.
                    Inventory[StrToInt(ItemsToDrop[Loop])] := 0;

                  { Update the message log }
                  UnitEngine.UpdateLog(Format('You drop %s',
                    [Lower(ItemToDrop.Name)]), mesItemManipulation);
                end;
              end;

              { For now, only pass 1 turn }
              PassATurn;

              { Let monsters react }
              ProcessCreatures(FormDisplay.Game.Dungeon);

              { Clear the items to drop list }
              ItemsToDrop.Clear;

              { And go back to the main display }
              LoadInventoryDetails(FormDisplay.Game.Player);
              FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              InventoryStatus := invNormal; 
            end;
          end;
        end;
      end;
    end
    { Now handle digging, which can only take place on the main screen }
    else if (TargetMode = tgDig) and
      (FormDisplay.PageControlMain.ActivePageIndex = MAINDISPLAY) then
    begin
      { Handle only the digging keys - and use ESC to escape from digging }
      repeat
        { Refresh the screen }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

        { Reinitialise the keyboard queue }
        Application.ProcessMessages;
        KeyPressed := False;

        { Check for North Movement }
        if (KH.VirtualKeyDown[VK_UP] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD8] = True) or
          (KH.VirtualKeyDown[Ord('K')] = True) or
          (KH.VirtualKeyDown[Ord('k')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(0, 1);
          KeyPressed := True;
        end
        { Check for South Movement }
        else if (KH.VirtualKeyDown[VK_DOWN] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD2] = True) or
          (KH.VirtualKeyDown[Ord('J')] = True) or
          (KH.VirtualKeyDown[Ord('j')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(0, -1);
          KeyPressed := True;
        end
        { Check for West Movement }
        else if (KH.VirtualKeyDown[VK_LEFT] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD4] = True) or
          (KH.VirtualKeyDown[Ord('H')] = True) or
          (KH.VirtualKeyDown[Ord('h')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(-1, 0);
          KeyPressed := True;
        end
        { Check for East Movement }
        else if (KH.VirtualKeyDown[VK_RIGHT] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD6] = True) or
          (KH.VirtualKeyDown[Ord('L')] = True) or
          (KH.VirtualKeyDown[Ord('l')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(1, 0);
          KeyPressed := True;
        end
        { Check for North-West Movement }
        else if (KH.VirtualKeyDown[VK_HOME] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD7] = True) or
          (KH.VirtualKeyDown[Ord('Y')] = True) or
          (KH.VirtualKeyDown[Ord('y')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(-1, 1);
          KeyPressed := True;
        end
        { Check for North-East Movement }
        else if (KH.VirtualKeyDown[VK_PRIOR] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD9] = True) or
          (KH.VirtualKeyDown[Ord('U')] = True) or
          (KH.VirtualKeyDown[Ord('u')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(1, 1);
          KeyPressed := True;
        end
        { Check for South-West Movement }
        else if (KH.VirtualKeyDown[VK_END] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD1] = True) or
          (KH.VirtualKeyDown[Ord('B')] = True) or
          (KH.VirtualKeyDown[Ord('b')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(-1, -1);
          KeyPressed := True;
        end
        { Check for South-East Movement }
        else if (KH.VirtualKeyDown[VK_NEXT] = True) or
          (KH.VirtualKeyDown[VK_NUMPAD3] = True) or
          (KH.VirtualKeyDown[Ord('N')] = True) or
          (KH.VirtualKeyDown[Ord('n')] = True) then
        begin
          { Dig in the specified direction (using X/Y offsets }
          Dig(1, -1);
          KeyPressed := True;
        end
        { Check for cancel }
        else if KH.VirtualKeyDown[VK_ESCAPE] = True then
          KeyPressed := True;
      until KeyPressed = True;

      { Clear the target mode }
      TargetMode := tgNone;
      FormDisplay.Game.CurrentTargetMode := TargetMode;

      { Redisplay the screen }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

      { Reinitialise keyboard handling }
      EmptyKeyQueue;
      FormDisplay.Timer.Enabled := True;

      { Sleep to avoid keypress reptition }
      Screen.Cursor := crHourGlass;
      Sleep(100);
      Screen.Cursor := crDefault;
    end
    else
    begin
      { Now handle the main key controls for the game }
      FormDisplay.Timer.Enabled := False;

      { Deal with switching on and off minimal view }
      if KH.VirtualKeyDown[VK_F1] then
      begin
        FormDisplay.PanelInfo.Width := 0;
        FormDisplay.PanelRightSplitter.Width := 0;
        FormDisplay.FormResize(nil);
      end;
      if KH.VirtualKeyDown[VK_F2] then
      begin
        FormDisplay.PanelInfo.Width := 327;
        FormDisplay.PanelRightSplitter.Width := 16;
        FormDisplay.FormResize(nil);
      end;

      { Handle North Movement }
      if (KH.VirtualKeyDown[VK_UP] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD8] = True) or
        (KH.VirtualKeyDown[Ord('K')] = True) or
        (KH.VirtualKeyDown[Ord('k')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(0, 1)
      { Handle South Movement }
      else if (KH.VirtualKeyDown[VK_DOWN] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD2] = True) or
        (KH.VirtualKeyDown[Ord('J')] = True) or
        (KH.VirtualKeyDown[Ord('j')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(0, -1)
      { Handle West Movement }
      else if (KH.VirtualKeyDown[VK_LEFT] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD4] = True) or
        (KH.VirtualKeyDown[Ord('H')] = True) or
        (KH.VirtualKeyDown[Ord('h')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(-1, 0)
      { Handle East Movement }
      else if (KH.VirtualKeyDown[VK_RIGHT] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD6] = True) or
        (KH.VirtualKeyDown[Ord('L')] = True) or
        (KH.VirtualKeyDown[Ord('l')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(1, 0)
      { Handle North-West Movement }
      else if (KH.VirtualKeyDown[VK_HOME] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD7] = True) or
        (KH.VirtualKeyDown[Ord('Y')] = True) or
        (KH.VirtualKeyDown[Ord('y')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(-1, 1)
      { Handle North-East Movement }
      else if (KH.VirtualKeyDown[VK_PRIOR] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD9] = True) or
        (KH.VirtualKeyDown[Ord('U')] = True) or
        (KH.VirtualKeyDown[Ord('u')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(1, 1)
      { Handle South-West Movement }
      else if (KH.VirtualKeyDown[VK_END] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD1] = True) or
        (KH.VirtualKeyDown[Ord('B')] = True) or
        (KH.VirtualKeyDown[Ord('b')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(-1, -1)
      { Handle South-East Movement }
      else if (KH.VirtualKeyDown[VK_NEXT] = True) or
        (KH.VirtualKeyDown[VK_NUMPAD3] = True) or
        (KH.VirtualKeyDown[Ord('N')] = True) or
        (KH.VirtualKeyDown[Ord('n')] = True) then
        { Move in the specified direction (using X/Y offsets }
        MovePlayer(1, -1)
      { For some reason, the Delphi implementation of windows keycodes is
        incomplete and doesn't have < and > (i.e. they are not in Windows.Pas
        from line #18207 onwards thus we need to use the integer values instead
        of the friendly VK_* versions }
      { > to go down stairs }
      else if (KH.VirtualKeyDown[190] = True)  and
        (KH.VirtualKeyDown[VK_SHIFT] = True) then
        TraverseDungeon(D_DOWN)
      { < to go up stairs }
      else if (KH.VirtualKeyDown[188] = True) and
        (KH.VirtualKeyDown[VK_SHIFT] = True) then 
        TraverseDungeon(D_UP)
      { c to close nearby doors }
      else if (KH.VirtualKeyDown[Ord('C')] = True) or
        (KH.VirtualKeyDown[Ord('c')] = True) then 
        CloseDoor(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)
      { o to open nearby doors (which can also be opened by moving into them }
      else if (KH.VirtualKeyDown[Ord('O')] = True) or
        (KH.VirtualKeyDown[Ord('o')] = True) then
        OpenDoors(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)
      { g to get an item }
      else if (KH.VirtualKeyDown[Ord('G')] = True) or
        (KH.VirtualKeyDown[Ord('g')] = True) then
        HandleMouseInput(insGet)
      { w to toggle on and off wizard mode }
      else if (KH.VirtualKeyDown[Ord('W')] = True) or
        (KH.VirtualKeyDown[Ord('w')] = True) then
        HandleMouseInput(insWizardMode)
      { s to toggle on and off stealth mode }
      else if (KH.VirtualKeyDown[Ord('S')] = True) or
        (KH.VirtualKeyDown[Ord('s')] = True) then
        HandleMouseInput(insStealth)
      { 5 to rest for a while }
      else if (KH.VirtualKeyDown[Ord(VK_NUMPAD5)] = True) or
        (KH.VirtualKeyDown[Ord('5')] = True) then 
        Rest(50)
      { 0 to rest for a single turn }
      else if (KH.VirtualKeyDown[Ord(VK_NUMPAD0)] = True) or
         (KH.VirtualKeyDown[Ord('0')] = True) then
        Rest(1)
      {else if (KH.VirtualKeyDown[Ord(VK_NUMPAD4)] = True) or
        (KH.VirtualKeyDown[Ord('4')] = True) then
      begin
        FormDisplay.Game.SampleTimer := TGameTimer.Create(timMight,
          30, UnitFunctions.PotionOfMightTick, UnitFunctions.PotionOfMightStart,
          UnitFunctions.PotionOfMightEnd);
      end }
      { m to display skills }
      else if ((KH.VirtualKeyDown[Ord('M')] = True) or
        (KH.VirtualKeyDown[Ord('m')] = True)) then 
        HandleMouseInput(insSkills)
      { i to display inventory }
      else if ((KH.VirtualKeyDown[Ord('I')] = True) or
        (KH.VirtualKeyDown[Ord('i')] = True)) then 
        HandleMouseInput(insInventory)
      { d to drop an item }
      else if ((KH.VirtualKeyDown[Ord('D')] = True) or
        (KH.VirtualKeyDown[Ord('d')] = True)) then
        HandleMouseInput(insDrop)
      { e to eat a consumable }
      else if ((KH.VirtualKeyDown[Ord('E')] = True) or
        (KH.VirtualKeyDown[Ord('e')] = True)) then
        HandleMouseInput(insEat)
      { r to read a scroll }
      else if ((KH.VirtualKeyDown[Ord('R')] = True) or
        (KH.VirtualKeyDown[Ord('r')] = True)) then
        HandleMouseInput(insRead)
      { z to display magic }
      else if ((KH.VirtualKeyDown[Ord('Z')] = True) or
        (KH.VirtualKeyDown[Ord('z')] = True)) then 
        HandleMouseInput(insMagic)
      { t to tunnel/dig, but not on the top town level, where it makes no sense
        to allow digging }
      else if ((KH.VirtualKeyDown[Ord('T')] = True) or
        (KH.VirtualKeyDown[Ord('t')] = True)) then
      begin
        if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
          HandleMouseInput(insDig)
        else
          UpdateLog('You cannot dig here...');
      end
      { a to display character dump }
      else if (KH.VirtualKeyDown[Ord('A')] = True) or
        (KH.VirtualKeyDown[Ord('a')] = True) then
        HandleMouseInput(insDump)
      { x for help }
      else if (KH.VirtualKeyDown[Ord('X')] = True) or
        (KH.VirtualKeyDown[Ord('x')] = True) then
        HandleMouseInput(insHelp)
      { v for version information }
      else if (KH.VirtualKeyDown[Ord('V')] = True) or
        (KH.VirtualKeyDown[Ord('v')] = True) then
        HandleMouseInput(insVersion)
      { q to quaff, for example, whilst standing on a fountain }
      else if (KH.VirtualKeyDown[Ord('Q')] = True) or
        (KH.VirtualKeyDown[Ord('q')] = True) then 
        HandleMouseInput(insQuaff);
    end;

    { Now that we have handled the keypress, empty the key buffer and switch on
      key handling }
    EmptyKeyQueue;

    FormDisplay.Timer.Enabled := True;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Main routine for handling mouse input across the entire game - note that
  HandleKeyboardInput is somewhat of a wrapper for this, and this is somewhat of
  a legacy piece of code back to the days when you could click on icons to do
  actions }
procedure HandleMouseInput(Instruction: tInstructionType);
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.HandleMouseInput()');

  try
    { Disable standard key checks whilst in this routine }
    FormDisplay.Timer.Enabled := False;

    { Handle digging }
    if Instruction = insDig then
    begin
      UpdateLog('Digging');
      HandleKeyBoardInput(FormDisplay.KeyboardHandler, tgDig);
    end;

    { Handling leaving subscreens via ESC }
    if Instruction = insQuit then
    begin
      { ESC only works on the various subscreens, not on the main page }
      if FormDisplay.PageControlMain.ActivePageIndex <> MAINDISPLAY then
      begin
        { Abort the drop if any }
        InventoryStatus := invNormal; 
        ItemsToDrop.Clear;

        { Do additional processing for the magic screen, where we need to find
          the spell that is currently selected and store it }
        if FormDisplay.PageControlMain.ActivePage = FormDisplay.TabSheetMagic
        then
        begin
          { Get the selected spell school }
          if FormDisplay.ListViewAvailableSchools.SelCount <> 0 then
            FormDisplay.Game.Player.SelectedSpellSchool := 
              StrToInt(FormDisplay.ListViewAvailableSchools.Selected.SubItems[1])
          else
            FormDisplay.Game.Player.SelectedSpellSchool := -1;

          { Get the selected spell }
          if FormDisplay.ListViewSpellsKnown.SelCount <> 0 then
            FormDisplay.Game.Player.SelectedSpell :=  
              StrToInt(FormDisplay.ListViewSpellsKnown.Selected.Caption)
          else
            FormDisplay.Game.Player.SelectedSpell := -1;
        end;

        { Switch back to the main screen and refresh the view }
        FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end;
    end;
    
    { Handle leaving item screen back to inventory }
    if Instruction = insItem then
    begin
      { ESC only works on the various subscreens, not on the main page }
      if FormDisplay.PageControlMain.ActivePageIndex = ITEM then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);
        
        { Refresh the screen }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end
    end;

    { Handle leaving monster screen}
    if Instruction = insMonster then
    begin
      { ESC only works on the various subscreens, not on the main page }
      if FormDisplay.PageControlMain.ActivePageIndex = MONSTER then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end
    end;

    { Handle quitting }
    if Instruction = insSuicide then
      FormDisplay.PlayerDie(nil);

    { Toggle on and off Stealth Mode }
    if Instruction = insStealth then
    begin
      { Toggle on or off stealth }
      if FormDisplay.Game.Player.Has(STEALTHED) then
      begin
        FormDisplay.Game.Player.Status[STEALTHED] := 0;
        UnitEngine.UpdateLog('You are no longer stealthed', mesStealth);
      end
      else
      begin
        { Can only stealth if no awake monsters nearby }
        if FormDisplay.Game.CanStealth then
        begin
          FormDisplay.Game.Player.Status[STEALTHED] := -1;
          UnitEngine.UpdateLog('You hide in the shadows!', mesStealth);
        end
        else
        begin
          UnitEngine.UpdateLog('You can''t stealth here!', mesStealth);
        end;
      end;

      { Changing stealth status passes a turn }
      PassATurn;

      { Process any monsters }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh the dungeon }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end;

    { Toggle on and off Wizard Mode }
    if Instruction = insWizardMode then
    begin
      { If the wizard form isn't displayed, display it }
      if FormWizard.Visible = False then
      begin
        { Locate the wizard form to the right edge of the main form by default }
        if FormWizard.CurrentLeft = 0 then
        begin
          { Default position }
          FormWizard.Left := FormDisplay.Left + FormDisplay.Width + 
            GetWindowPosX;
          FormWizard.Top := FormDisplay.Top + 50;;
        end
        else
        begin
          { If the form has been moved previously, then redisplay it at its
            previous position }
          FormWizard.Left := FormWizard.CurrentLeft;
          FormWizard.Top := FormWizard.CurrentTop;
        end;

        { Show the form and refresh it }
        FormWizard.Show;
        FormWizard.BringToFront;
        FormWizard.RefreshDisplay;
      end
      else
        { If the form is currently visible, hide it }
        FormWizard.Hide;
    end;

    { Get an Item }
    if Instruction = insGet then
    begin
      { Check of course that an item is actually present }
      if (FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
        FormDisplay.Game.PlayerY] > 0) then
      begin
        { For now, just pick up the top item, and shuffle everything else up }
        GetAnItem(FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
          FormDisplay.Game.PlayerY]);

        { Getting an item passes a turn }
        PassATurn;

        { Process any monsters }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Refresh the dungeon }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end
      else
        { Alert the player if there is nothing to pickup }
        UpdateLog('There is nothing here!', mesItemManipulation);
    end;

    { Handle drinking from fountains }
    if Instruction = insQuaff then
    begin
      { Check for the wet fountain terrain }
      if FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] = T_FOUNTAIN then
      begin
        { Drink from the fountain }
        DrinkFromFountain;

        { Replace the wet fountain with a dry fountain }
        FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX, 
          FormDisplay.Game.PlayerY] := T_FOUNTAIN_USED;

        { Fountains provide some extra refreshment }
        FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT);

        { Drinking from the fountain passes a turn }
        PassATurn;

        { Process any monsters }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Refresh the dungeon }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end
      else if FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] = T_FOUNTAIN_USED then
      begin
        { Alert the player that the fountain is dry }
        UpdateLog('The fountain is dry', mesFountain);

        { Trying to drink from the fountain passes a turn }
        PassATurn;

        { Process any monsters }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Refresh the dungeon }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end
      else
      begin
        { Handle drinking potions }
        { Check to make sure we don't try this whilst on the same page }
        if FormDisplay.PageControlMain.ActivePageIndex <> INVENTORY then
        begin
          { Set the drop state }
          InventoryStatus := invDrink; 

          { Set up the correct keys }
          FormDisplay.LabelKeysInventory.Visible := False;
          FormDisplay.LabelKeysDrop.Visible := False;
          FormDisplay.LabelKeysEat.Visible := False;
          FormDisplay.LabelKeysRead.Visible := False;
          FormDisplay.LabelKeysIdentify.Visible := False;
          FormDisplay.LabelKeysDrink.Visible := True;

          { Switch the tab over to the correct tab }
          FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

          { Load the inventory details }
          LoadInventoryDetails(FormDisplay.Game.Player);

          { Refresh the display }
          FormDisplay.GradLabelInventoryTitle.Repaint;
          FormDisplay.LabelBackpack.Repaint;
          FormDisplay.LabelWearingWielding.Repaint;
        end;
      end;
    end;

    { Show the Inventory }
    if Instruction = insInventory then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> INVENTORY then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

        { Set up the correct keys }
        FormDisplay.LabelKeysInventory.Visible := True;
        FormDisplay.LabelKeysDrop.Visible := False;
        FormDisplay.LabelKeysEat.Visible := False;
        FormDisplay.LabelKeysRead.Visible := False;
        FormDisplay.LabelKeysIdentify.Visible := False;
        FormDisplay.LabelKeysDrink.Visible := False;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);
        
        { Refresh the screen }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end;
    end;

    { Show Help }
    if Instruction = insHelp then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> HELP then
        { Switch the tab over to the correct tab } 
        FormDisplay.PageControlMain.ActivePageIndex := HELP;
    end;

    { Show Version Information }
    if Instruction = insVersion then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> VERSION then
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := VERSION;
    end;

    { Show Character Dump }
    if Instruction = insDump then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> DUMP then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := DUMP;

        { Load the character dump }
        FormDisplay.RichEditDump.Text := CharDump(FormDisplay.Game.Player.Dead);

        { Refresh the display }
        FormDisplay.RichEditDump.Repaint;
      end;
    end;

    { Show Inventory for Dropping purposesm }
    if Instruction = insDrop then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if (FormDisplay.PageControlMain.ActivePageIndex <> INVENTORY) then
      begin
        { Set the drop state }
        InventoryStatus := invMultiDrop; 

        { Set up the correct keys }
        FormDisplay.LabelKeysInventory.Visible := False;
        FormDisplay.LabelKeysDrop.Visible := True;
        FormDisplay.LabelKeysEat.Visible := False;
        FormDisplay.LabelKeysRead.Visible := False;
        FormDisplay.LabelKeysIdentify.Visible := False;
        FormDisplay.LabelKeysDrink.Visible := False;

        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);

        { Refresh the display }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end;
    end;

    { Show Inventory for Eating purposesm }
    if Instruction = insEat then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> INVENTORY then
      begin
        { Set the drop state }
        InventoryStatus := invEat; 

        { Set up the correct keys }
        FormDisplay.LabelKeysInventory.Visible := False;
        FormDisplay.LabelKeysDrop.Visible := False;
        FormDisplay.LabelKeysEat.Visible := True;
        FormDisplay.LabelKeysRead.Visible := False;
        FormDisplay.LabelKeysIdentify.Visible := False;
        FormDisplay.LabelKeysDrink.Visible := False;

        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);

        { Refresh the display }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end;
    end;

    { Show Inventory for identify purposesm }
    if Instruction = insIdentify then
    begin
      { Check to make sure we are still in the inventory }
      if FormDisplay.PageControlMain.ActivePageIndex = INVENTORY then
      begin
        { Set the inventory state }
        InventoryStatus := invIdentify; 

        { Set up the correct keys }
        FormDisplay.LabelKeysInventory.Visible := False;
        FormDisplay.LabelKeysDrop.Visible := False;
        FormDisplay.LabelKeysEat.Visible := False;
        FormDisplay.LabelKeysRead.Visible := False;
        FormDisplay.LabelKeysIdentify.Visible := True;
        FormDisplay.LabelKeysDrink.Visible := False;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);

        { Refresh the display }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end;
    end;

    { Show Inventory for reading scroll purposes }
    if Instruction = insRead then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> INVENTORY then
      begin
        { Set the drop state }
        InventoryStatus := invRead; 

        { Set up the correct keys }
        FormDisplay.LabelKeysInventory.Visible := False;
        FormDisplay.LabelKeysDrop.Visible := False;
        FormDisplay.LabelKeysEat.Visible := False;
        FormDisplay.LabelKeysRead.Visible := True;
        FormDisplay.LabelKeysIdentify.Visible := False;
        FormDisplay.LabelKeysDrink.Visible := False;

        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := INVENTORY;

        { Load the inventory details }
        LoadInventoryDetails(FormDisplay.Game.Player);

        { Refresh the display }
        FormDisplay.GradLabelInventoryTitle.Repaint;
        FormDisplay.LabelBackpack.Repaint;
        FormDisplay.LabelWearingWielding.Repaint;
      end;
    end;

    { Show Skills }
    if Instruction = insSkills then
    begin
      { Check to make sure we don't try this whilst on the same page }
      if FormDisplay.PageControlMain.ActivePageIndex <> SKILLS then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := SKILLS;

        { Load the skills }
        LoadSkills(FormDisplay.Game.Player);
      end;
    end;

    { Show Magic }
    if Instruction = insMagic then
    begin
      { Check if we can actually display magic, because some characters can't
        cast any spells }
      if not(FormDisplay.Game.Player.HasMagic) then
        { Alert the player this is the case }
        UpdateLog('You don''t know any spells!')
      { Check to make sure we don't try this whilst on the same page }
      else if FormDisplay.PageControlMain.ActivePageIndex <> MAGIC then
      begin
        { Switch the tab over to the correct tab }
        FormDisplay.PageControlMain.ActivePageIndex := MAGIC;

        { Load magic }
        LoadMagic(FormDisplay.Game.Player);
      end;
    end;

    { Empty the mouse queue to avoid repetition of any events }
    EmptyMouseQueue;

    { Switch back on normal keyboard handling }
    FormDisplay.Timer.Enabled := True;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Main routine for handling screen input, i.e. mouse clicks }
procedure HandleScreenInput(X: Integer; Y: Integer; Button: TMouseButton);
var
  AttackString: String;
  AttackResult: aResult;
  DamageCaused: Integer;
  DamageString: String;
  InX: Integer;
  InY: Integer;
  OutX: Integer;
  OutY: Integer;
  LocalItem: TItem;
  LocalMonster: TMonster;
  ScreenRect: TRect;
  PointX: Real;
  PointY: Real;
  XP: Integer;
  Offset: Integer;
  SneakAttackMultiplier: Integer;
  EffectiveDungeonLevel: Integer;
  MonsterDescription: String;
  ItemText: String;
  MonsterColour: TColor;
  MonsterText: String;
  MonsterType: Integer;
  NumberKilled: Integer;
  MoreInfoText: String;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.HandleScreenInput()');

  try
    { Check for right-hand mouse button }

    { TODO: some of this may be depreciated already by the new mouse-over
      behaviour - eventually we want to use the mouse for quick-casting spells
      instead }
    if Button = mbRight then
    begin
      { Work out the map co-ordinates from the screen coordinates - first we
        have to adjust just in case only part of a cell is visible }
      Offset := FormDisplay.ScreenMain.Height -
        FormDisplay.ScreenMain.Constraints.MinHeight;

      { We don't use integer division here as it can cause rounding errors }
      PointX := (X / ScalingFactorX) + FormDisplay.Game.TopLeftX;
      PointY := FormDisplay.Game.BottomLeftY + ((FormDisplay.ScreenMain.Height
        - (Y - OffSet)) / ScalingFactorY);

      { And finally produce the map co-ordinates }
      InX := Round(PointX);
      InY := Round(PointY);

      { Refresh the display to get rid of any previous marks }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

      { Convert map co-ordinates back into screen co-ordinates for drawing the
        cursor }
      OutX := (InX - FormDisplay.Game.topleftx) * ScalingFactorX;
      OutY := FormDisplay.ScreenMain.Height - 
        ((InY - FormDisplay.Game.bottomlefty) * ScalingFactorY);

      { Draw a cursor around the selected square }
      ScreenRect := Rect(OutX, OutY, OutX + ScalingFactorX, 
        OutY + ScalingFactorY);
      FormDisplay.ScreenMain.Canvas.Brush.Color := clWhite;
      FormDisplay.ScreenMain.Canvas.FrameRect(ScreenRect);

      { Switch on animations }
      FormDisplay.TimerCycleColours.Enabled := True;

      { If there is a monster at this location, this has priority to display the
        monster tooltip, else display the item tooltip else display some terrain
        information }
      if FormDisplay.Game.Dungeon.Visible[Inx, InY] = CURRENTLY_VISIBLE then
      begin
        { Check for the character }
        if (InX = FormDisplay.Game.PlayerX) and
          (InY = FormDisplay.Game.PlayerY) then
          { TODO: eventually we want to display some cursory health status
            information, e.g. "You see Wibble the Knight, who is currently
            slightly injured" }
          { Flag to the player that this is the character }
          UpdateLog('You see you!', mesFloorDescription )
        { There is a monster here }
        else if (FormDisplay.Game.Dungeon.Monsters[InX, InY] > 0) and
          (FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN) then
        begin
          { Handle monster memory }

          { Make sure we don't try and go to the monster page whilst on a monster }
          if FormDisplay.PageControlMain.ActivePageIndex <> MONSTER then
          begin

            { Get the monster }
            LocalMonster :=
              (GMonsterList.Items[FormDisplay.Game.Dungeon.Monsters[InX, InY]] as
              TMonster);

            { Make sure the monster title is always displayed }
            MonsterColour := LocalMonster.Colour;
            if MonsterColour = clBlack then
              MonsterColour := clWhite;

            { Now start adding the text }
            FormDisplay.RichEditMonster.Clear;

            { Change the font colour using the mType array entries }
            FormDisplay.LabelMonsterName.Font.Color := MonsterColour;

            { Get the name }
            MonsterText := Trim(Format('%s: %s%s', [LocalMonster.Char,
              UpperCase(LocalMonster.SinglePrefix[1]) +
              Copy(LocalMonster.SinglePrefix, 2,
              Length(LocalMonster.SinglePrefix)), LocalMonster.Name]));

            { Add the monster name first }
            FormDisplay.LabelMonsterName.Caption := MonsterText;

            { Now add the description }
            if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
              MonsterDescription := LocalMonster.Name
            else
              MonsterDescription := LocalMonster.Description;
            FormDisplay.RichEditMonster.Lines.Add(MonsterDescription);
            FormDisplay.RichEditMonster.Lines.Add('');

            { Work out the monster difficulty }
            EffectiveDungeonLevel := FormDisplay.Game.Dungeon.LevelDepth div 2 +
              FormDisplay.Game.Dungeon.LevelOffset;
            if EffectiveDungeonLevel < MINDUNGEONLEVEL then
              EffectiveDungeonLevel := MINDUNGEONLEVEL;
            if EffectiveDungeonLevel > MAXDUNGEONLEVEL then
              EffectiveDungeonLevel := MAXDUNGEONLEVEL;

            { Now add the relative strength }
            FormDisplay.RichEditMonster.Lines.Add(
              LocalMonster.GetRelativeDescription(EffectiveDungeonLevel,
              FormDisplay.Game.Player.Levels));
            FormDisplay.RichEditMonster.Lines.Add('');

            { Check if it is wearing armour or wielding a weapon }
            if LocalMonster.Armour > 0 then
            begin
              LocalItem := (GItemList[LocalMonster.Armour] as TItem);
              ItemText := 'It is wearing ' + Lower(LocalItem.Name) + '.';
            end
            else if LocalMonster.Weapon > 0 then
            begin
              LocalItem := (GItemList[LocalMonster.Weapon] as TItem);
              ItemText := 'It is wielding ' + Lower(LocalItem.Name) + '.';
            end
            else ItemText := 'It is not carrying anything of importance';

            FormDisplay.RichEditMonster.Lines.Add(ItemText);
            FormDisplay.RichEditMonster.Lines.Add('');

            { Now add monster memory if we have killed any of these creatures }
            MonsterType :=
              FormDisplay.Game.MonsterKills.IndexOf(LocalMonster.Name);

            { If we have found the name, increment the appropriate count }
            if MonsterType <> -1 then
              FormDisplay.RichEditMonster.Lines.Add('You have defeated ' +
                IntToStr(FormDisplay.Game.MonsterKillNumbers[MonsterType])
                + ' of these creatures.')
            else
              FormDisplay.RichEditMonster.Lines.Add('You have not defeated any'
                + ' of these creatures.');
            FormDisplay.RichEditMonster.Lines.Add('');

            { Now display some information about these depending on how many
              have been killed }
            if MonsterType <> -1 then
            begin
              NumberKilled := FormDisplay.Game.MonsterKillNumbers[MonsterType];
              if NumberKilled >= 5 then
              begin
                MoreInfoText := Format('It has %d Hit Dice and %d Armour Class.',
                  [LocalMonster.MaxHP div 8, LocalMonster.AC]);
                FormDisplay.RichEditMonster.Lines.Add(MoreInfoText);
              end;
              if NumberKilled >= 10 then
              begin
                MoreInfoText := Format('It has %d Evasion and can attack %d ' +
                  'Times per turn.', [LocalMonster.EV, LocalMonster.AttackNumber]);
                FormDisplay.RichEditMonster.Lines.Add(MoreInfoText);
              end;
              if NumberKilled >= 20 then
              begin
                MoreInfoText := Format('Each attack can hit for up to %d Points ' +
                  ' of Damage, and it moves and attacks with a Speed of %d.',
                  [LocalMonster.AttackDamage, LocalMonster.Speed]);
                FormDisplay.RichEditMonster.Lines.Add(MoreInfoText);
              end;

              { Nothing is known about this creature }
              if NumberKilled < 5 then
                FormDisplay.RichEditMonster.Lines.Add('No other information about'
                 + ' this creature is yet known');
              FormDisplay.RichEditMonster.Lines.Add('');
            end;

            { Switch the tab over to the correct tab }
            FormDisplay.PageControlMain.ActivePageIndex := MONSTER;
          end;

          // gfgfgf
          //{ Get the monster information }
          //if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
          //  LocalMonster :=
          //    (GTownsPeopleList.Items[FormDisplay.Game.Dungeon.Monsters[InX,
          //    InY]] as TMonster)
          //else
          //  LocalMonster :=
          //    (GMonsterList.Items[FormDisplay.Game.Dungeon.Monsters[InX, InY]]
          //    as TMonster);
          //
          //{ Alert the player to the monster }
          //UpdateLog('You see a ' + LocalMonster.Name + '...',
          //  mesFloorDescription);
        end
        { There are items here }
        else if (FormDisplay.Game.Dungeon.Objects[InX, InY] > 0) and 
          (FormDisplay.Game.Dungeon.Objects[InX, InY] <> ITEM_GOLD) then
          { Get the local item }
          LocalItem :=
            (GItemList.Items[FormDisplay.Game.Dungeon.Objects[InX, InY]] as
            TItem)
        { Handle coins }
        else if FormDisplay.Game.Dungeon.Objects[InX, InY] >= ITEM_GOLD then
          UpdateLog('You see a pile of coins here...', mesFloorDescription)
        { Handle special terrain effects }
        else if FormDisplay.Game.Dungeon.Effects[InX, InY] in
          [E_SPECIALEFFECT, E_STANDARDEFFECT] then
          { Handle special terrain inside of a zone }
          if FormDisplay.Game.Dungeon.Zone[InX, InY] > 0 then
            UpdateLog('You see ' + 
              GDungeonEffectDescriptionArray[FormDisplay.Game.Dungeon.Zone
              [InX, InY]] + ' here...', mesFloorDescription)
          { Handle special terrain not in zones }
          else
            UpdateLog('You see ' +
              GDungeonEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
                + ' here...', mesFloorDescription)
        { Handle special terrain effects }
        else if FormDisplay.Game.Dungeon.Effects[InX, InY] in
          [E_DARKEREFFECT] then
          { Handle special terrain inside of a zone }
          if FormDisplay.Game.Dungeon.Zone[InX, InY] > 0 then
            UpdateLog('You see ' +
              GDungeonDarkerEffectDescriptionArray[
              FormDisplay.Game.Dungeon.Zone[InX, InY]] + ' here...', 
              mesFloorDescription)
          { Handle special terrain not in zones }
          else
            UpdateLog('You see ' + GDungeonDarkerEffectDescriptionArray[
              FormDisplay.Game.Dungeon.LevelTheme] + ' here...', 
              mesFloorDescription)
        else
        { Handle normal terrain }
        begin
          case FormDisplay.Game.Dungeon.Terrain[InX, InY] of
            T_BLANK: UpdateLog('You see nothing here...', mesFloorDescription);
            T_HARDWALL: UpdateLog('You see especially hard rock here...', 
              mesFloorDescription);
            T_SOFTWALL: UpdateLog('You see a wall here...', 
              mesFloorDescription);
            T_FLOOR_ROOM: UpdateLog('You see the ground here...', 
              mesFloorDescription);
            T_FLOOR_CORRIDOR: UpdateLog('You see the ground here...', 
              mesFloorDescription);
            T_DOOR_OPEN: UpdateLog('You see an open door here...',
              mesFloorDescription);
            T_DOOR_CLOSED: UpdateLog('You see a closed door here....', 
              mesFloorDescription);
            T_STAIRS_DOWN: UpdateLog('You see ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading downwards here....', mesStairsandPortals );
            T_STAIRS_UP: UpdateLog('You see ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading upwards here....', mesStairsandPortals);
          end;
        end;
      end
      { Handle previously visible, i.e. darkened squares in which monsters,
        items, gold etc can't be seen }
      else if FormDisplay.Game.Dungeon.Visible[InX, InY] =
        PREVIOUSLY_VISIBLE then
      begin
        { Ignore the town level for now }
        if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
        begin
          { Handle normal terrain }
          case FormDisplay.Game.Dungeon.Terrain[InX, InY] of
            T_BLANK: UpdateLog('You see nothing here...', mesFloorDescription);
            T_HARDWALL: UpdateLog('You see an imprenetrable barrier here...', 
              mesFloorDescription);
            T_SOFTWALL: UpdateLog('You see a wall here...',
              mesFloorDescription);
            T_FLOOR_ROOM: UpdateLog('You see the ground here...', 
              mesFloorDescription);
            T_FLOOR_CORRIDOR: UpdateLog('You see the ground here...',
              mesFloorDescription);
            T_DOOR_OPEN: UpdateLog('You see an open door here...', 
              mesFloorDescription);
            T_DOOR_CLOSED: UpdateLog('You see a closed door here....',
              mesFloorDescription);
            T_STAIRS_DOWN: UpdateLog('You see ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading downwards here....',
              mesStairsandPortals);
            T_STAIRS_UP: UpdateLog('You see ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading upwards here....', mesStairsandPortals);
          end;
        end;
      end;
    end
    { Now handle left mouse button, which is currently used as a shortcut for
      firing ranged weapons }
    else if (Button = mbLeft) then
    begin
      { Work out the map co-ordinates from the screen coordinates - first we
        have to adjust just in case only part of a cell is visible }
      Offset := FormDisplay.ScreenMain.Height -
        FormDisplay.ScreenMain.Constraints.MinHeight;

      { We don't use integer division here as it can cause rounding errors }
      PointX := (X / ScalingFactorX) + FormDisplay.Game.TopLeftX;
      PointY := FormDisplay.Game.BottomLeftY + ((FormDisplay.ScreenMain.Height
        - (Y - OffSet)) / ScalingFactorY);
        
      { And finally produce the map co-ordinates }
      InX := Round(PointX);
      InY := Round(PointY);

      { Check if we have a ranged weapon equipped }
      if FormDisplay.Game.Player.Inventory[S_RANGED] > 0 then
      begin
        { TODO: when we implement ammunition, then we will have to handle that }

        { Can only fire ranged missiles at a visible square }
        if FormDisplay.Game.Dungeon.Visible[InX, InY] = 1 then
        begin
          if XCanSeeY(FormDisplay.Game.Dungeon,
            FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, InX, InY) then
          begin
            { And only at a monster that is visible }
            if FormDisplay.Game.Dungeon.Monsters[InX, InY] > 0 then
            begin
              { Get the monster }
              LocalMonster := 
                (GMonsterList.Items[FormDisplay.Game.Dungeon.Monsters
                [InX, InY]] as TMonster);

              { Animate the missile weapon }
              AnimateMissileWeapon(Point(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY), Point(InX, InY), '(', clAqua);

                //LocalMonster.ProjectileChar, LocalMonster.ProjectileColour);

              { Check to see if the weapon has hit }
              if XCanHitYRanged(FormDisplay.Game.Player, LocalMonster,
                AttackString, AttackResult) then
              begin
                { If so, work out the damage caused }
                DamageCaused := XMissileDamagesY(FormDisplay.Game.Player, 
                  LocalMonster, DamageString, AttackResult);

                { Update the message log }
                UpdateLog(AttackString + DamageString, mesYouCombatHit);

                { Apply bonuses for sneak attacks }
                if not(LocalMonster.Awake) then
                begin
                  { Base damage for sneak attacks is double }
                  SneakAttackMultiplier := 2;

                  { Thieves get an added bonus to damage - for every 3 ranks in
                    the subterfuge skill, add extra damage }
                  if FormDisplay.Game.Player.CClass = cThief then
                    Inc(SneakAttackMultiplier,
                      FormDisplay.Game.Player.Skills[SK_SUBTERFUGE] div 3);

                  { Work out the new damage }
                  DamageCaused := DamageCaused * SneakAttackMultiplier;

                  { Update the message log for sneak attacks}
                  UpdateLog('That was a sneaky blow!', mesYouCombatHit);

                  { Learn skills for doing this successfully }
                end;

                { Damage the monster }
                LocalMonster.CurrentHP := LocalMonster.CurrentHP - DamageCaused;

                { Check if the monster is still alive }
                if LocalMonster.CurrentHP < 1 then
                begin
                  { Check for the type of monster and print an appropriate death
                    message if dead }
                  if LocalMonster.Category = 'Undead' then
                    { Dead, so update the messae log }
                    UpdateLog('You have destroyed ' +
                      Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
                      mesYouKill)
                  else if LocalMonster.Category = 'Outsider' then
                    { Dead, so update the messae log }
                    UpdateLog('You have banished ' +
                      Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
                      mesYouKill)
                  else
                    { Dead, so update the message log }
                    UpdateLog('You have killed ' +
                      Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
                      mesYouKill);

                  { If it is a unique we've just killed, make a note of it }
                  if LocalMonster.UniqueName <> '' then
                    FormDisplay.Game.Player.TakeNote(FormDisplay.Game.Turns, 
                    LocalMonster.UniqueName, nUnique, FormDisplay.Game.Dungeon);

                  { Update the kill count }
                  FormDisplay.Game.LogMonsterKill(LocalMonster);

                  { Monsters have an XP value }
                  XP := LocalMonster.XP;

                  { Increase XP as we go down the dungeon }
                  XP := XP + Trunc(Power(FormDisplay.Game.Dungeon.LevelDepth, 
                    1.5));

                  { Gain the XP }
                  FormDisplay.Game.Player.GainXP(XP);

                  { If the monster is carrying an item, drop it }
                  MonsterDropItem(LocalMonster);

                  { Get rid of the monster from the map and set the monster to
                    being dead }
                  FormDisplay.Game.Dungeon.RemoveMonsterFromMap(Point(InX, InY));
                  LocalMonster.Alive := False;
                  LocalMonster.Awake := False;

                  { Refresh the display, which will get rid of the monster in
                    the monster preview display }
                  FormDisplay.UpdateStatus;
                end
                else
                  { The monster is damaged but still alive, so alert the player
                    to this }
                  UpdateLog(AttackString + DamageString, mesYouCombatHit);
              end
              else
                { Alert the player that the ranged attack has missed }
                UpdateLog(AttackString, mesYouCombatMiss);
            end;
          end
          { Firing at an invalid square }
          else
            beep;
        end
        { Firing at an invalid square }
        else
          beep;

        { Ranged combat takes a turn }
        PassATurn;

        { Process creature AI }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Redraw the dungeon }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
      end;
    end;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Move up and down between dungeons via stairs }
procedure TraverseDungeon(Direction: Integer);
var
  Loop: Integer;
  NewDungeonLevel: TDungeonLevel;
  StairsUsed: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.TraverseDungeon()');

  try
    { Deal with travelling up and down stairs }
    case Direction of
      D_UP:
      begin
        { Set the cursor to busy }
        Screen.Cursor := crHourGlass;

        { Alert the player }
        UpdateLog('Climbing up stairs...', mesStairsandPortals);

        { Take normal upstairs }
        if (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX, 
          FormDisplay.Game.PlayerY] = T_STAIRS_UP) and 
          (FormDisplay.Game.Dungeon.LevelDepth > 1) then
        begin
          { Work out which set of stairs we have taken }
          StairsUsed := 
            FormDisplay.Game.Dungeon.FindStairsTaken(FormDisplay.Game.PlayerX, 
            FormDisplay.Game.PlayerY, Direction);
          FormDisplay.Game.Dungeon.StairsTaken := StairsUsed;

          { Save the level we're moving off }
          FormDisplay.Game.Dungeon.SaveLevel
            (BranchDungeons[FormDisplay.Game.Dungeon.LevelDepth]);

          { Load the level we're moving to }
          FormDisplay.Game.Dungeon.LoadLevel
            (BranchDungeons[FormDisplay.Game.Dungeon.LevelDepth + Direction]);

          { Move to the corresponding set of stairs on the level we're going
            to }
          FormDisplay.Game.PlayerX := 
            FormDisplay.Game.Dungeon.StairsDownX[StairsUsed];
          FormDisplay.Game.PlayerY := 
            FormDisplay.Game.Dungeon.StairsDownY[StairsUsed];

          { Calculate the viewport boundaries }
          FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
            FormDisplay.ScreenMain.Height);

          { Set the viewport boundaries }
          FormDisplay.Game.SetViewPort;

          { Set the visible region }
          FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

          { Refresh the display }
          FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

          { Climbing stairs takes a turn }
          PassATurn;

          { Add an appropriate message to the message log }
          UpdateLog('You have climbed the stairs.', mesStairsandPortals);

          { Optionally, return the level feeling }
          UpdateLog(FormDisplay.Game.Dungeon.GetLevelFeeling,
            mesStairsandPortals);

          { Always report any Unique Monster on the level }
          if FormDisplay.Game.Dungeon.Unique <> nil then
            UpdateLog(FormDisplay.Game.Dungeon.GetUniqueMonsterFeeling,
              mesStairsandPortals);

          { Clear the active monster list }
          ActiveMonsterList.Clear;
        end
        { Handle going back to the town }
        else if (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX, 
          FormDisplay.Game.PlayerY] = T_STAIRS_UP) and 
          (FormDisplay.Game.Dungeon.LevelDepth = 1) then
        begin
          { Load the town level - note we don't save the previous level since
            by going back to the town level we are explicitly resetting the
            dungeon branch we came from }
          FormDisplay.Game.Dungeon.loadLevel(FormDisplay.Game.TownDungeon);

          { Restock Shops }
          FormDisplay.Game.TownDungeon.ReStockShops
            (FormDisplay.Game.Player.Levels - 2);

          { Reinitialise the other dungeons }
          for Loop := 1 to 10 do
            if (Assigned(BranchDungeons[Loop])) then 
              BranchDungeons[Loop].initialise;
              
          {FormDisplay.ImageListTerrain.Assign
            ((GDungeonBranchList[D_TOWN] as TDungeonBranch).BranchGraphics); }

          { Set the player in an appropriate location }
          FormDisplay.Game.PlayerX := TownDownX;
          FormDisplay.Game.PlayerY := TownDownY;

          { Calculate the viewport boundaries }
          FormDisplay.Game.SetViewBoundaries
            (FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height);
            
          { Set the viewport boundaries }
          FormDisplay.Game.SetViewPort;

          { Set the visible region }
          FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
            FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

          { Refresh the display }
          FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

          { Climbing stairs takes a turn }
          PassATurn;

          { Add an appropriate message to the message log }
          UpdateLog('You have returned to The Nexus...', mesStairsandPortals);

          { Clear the active monster list }
          ActiveMonsterList.Clear;
        end;

        { Set the cursor to busy }
        Screen.Cursor := crDefault;
      end;
      D_DOWN:
      begin
        { Take normal down stairs }
        if FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY] = T_STAIRS_DOWN then
        begin
          { Alert the player }
          UpdateLog('Climbing down...', mesStairsandPortals);

          { Check if we need just-in-time level generation or we can use a level
            we have previously visited }
          if (FormDisplay.Game.Dungeon.LevelDepth + Direction > 
            FormDisplay.Game.GDepthDelved[
            FormDisplay.Game.Dungeon.LevelTheme]) then
          begin
            { Get the stairs we used to go down }
            StairsUsed := FormDisplay.Game.Dungeon.
              FindStairsTaken(FormDisplay.Game.PlayerX, 
              FormDisplay.Game.PlayerY, D_DOWN);
            FormDisplay.Game.Dungeon.StairsTaken := StairsUsed;

            { Save the level we moved off }
            FormDisplay.Game.Dungeon.SaveLevel
              (BranchDungeons[FormDisplay.Game.Dungeon.LevelDepth]);

            { Generate a new dungeon level to explore }
            NewDungeonLevel := 
              TDungeonLevel.Create(FormDisplay.Game.Dungeon.LevelDepth + 
              Direction, FormDisplay.Game.Dungeon.LevelTheme);

            { Load this freshly created level }
            FormDisplay.Game.Dungeon.LoadLevel(NewDungeonLevel);

            { Increase the maximum depth delved }
            FormDisplay.Game.GDepthDelved[FormDisplay.Game.Dungeon.LevelTheme] 
              := FormDisplay.Game.Dungeon.LevelDepth;

            { Add a characrer note }
            FormDisplay.Game.Player.TakeNote(FormDisplay.Game.Turns, '', 
              nReach, FormDisplay.Game.Dungeon);
          end
          else
          begin
            { Get the stairs we used to go down }
            StairsUsed := FormDisplay.Game.Dungeon.
              FindStairsTaken(FormDisplay.Game.PlayerX, 
              FormDisplay.Game.PlayerY, D_DOWN);
            FormDisplay.Game.Dungeon.StairsTaken := StairsUsed;

            { Save the level we moved off }
            FormDisplay.Game.Dungeon.SaveLevel
              (BranchDungeons[FormDisplay.Game.Dungeon.LevelDepth]);

            { Load a previously visited level }
            FormDisplay.Game.Dungeon.LoadLevel
              (BranchDungeons[FormDisplay.Game.Dungeon.LevelDepth + Direction]);

            { Although in this circumstance we are loading an already created
              dungeon level, we need to create a blank dungeon level so we can
              free a dungeon level in all circumstances later on }
            NewDungeonLevel := TDungeonLevel.Create(0, D_NONE);
          end;

          { Move to the corresponding set of stairs on the level we're going
            to }
          FormDisplay.Game.PlayerX := 
            FormDisplay.Game.Dungeon.StairsUpX[StairsUsed];
          FormDisplay.Game.PlayerY := 
            FormDisplay.Game.Dungeon.StairsUpY[StairsUsed];

          { Calculate the viewport boundaries }
          FormDisplay.Game.SetViewBoundaries
            (FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height);

          { FormDisplay.Game.TopLeft :=
            GetTopLeft(FormDisplay.Game.CurrentDrawMode, 
            Point(FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height));
          FormDisplay.Game.TopRight := 
            GetTopRight(FormDisplay.Game.CurrentDrawMode, 
            Point(FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height));
          FormDisplay.Game.BottomLeft := 
            GetBottomLeft(FormDisplay.Game.CurrentDrawMode, 
            Point(FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height));
          FormDisplay.Game.BottomRight := 
            GetBottomRight(FormDisplay.Game.CurrentDrawMode, 
            Point(FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height)); }

          { Set the viewport boundaries }
          FormDisplay.Game.SetViewPort;

          { Set the visible region }
          FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX, 
            FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

          { Refresh the display }
          FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

          { TODO: Again, not sure what this does }
          if Assigned(NewDungeonLevel) then
            NewDungeonLevel.Free;

          { Climbing stairs takes a turn }
          PassATurn;

          { Add an appropriate message to the message log }
          UpdateLog('You have climbed down the stairs.', mesStairsandPortals);

          { Display level feeling if we can }
          if FormDisplay.Game.Dungeon.LevelDepth > 1 then
            UpdateLog(FormDisplay.Game.Dungeon.GetLevelFeeling, 
            mesStairsandPortals);

          { Always report any Unique Monster on the level }
          if FormDisplay.Game.Dungeon.Unique <> nil then
            UpdateLog(FormDisplay.Game.Dungeon.GetUniqueMonsterFeeling,
              mesStairsandPortals);

          { Clear the active monster list }
          ActiveMonsterList.Clear;
        end
        { Now handle entering a new dungeon stack }
        else if (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY] > (D_TILE_CRYPT - 1)) and
          (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) then
        begin
          { Save the town level }
          FormDisplay.Game.Dungeon.saveLevel(FormDisplay.Game.TownDungeon);

          { Make a note of where we left the town level }
          TownDownX := FormDisplay.Game.PlayerX;
          TownDownY := FormDisplay.Game.PlayerY;

          { Now figure out what dungeon branch we entered and create a new level
            of that type }
          case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS: NewDungeonLevel := TDungeonLevel.Create(1, D_ABYSS);
            D_TILE_CRYPT: NewDungeonLevel := TDungeonLevel.Create(1, D_CRYPT);
            D_TILE_FORTRESS: NewDungeonLevel := TDungeonLevel.Create(1, D_FORTRESS);
            D_TILE_KEEP: NewDungeonLevel := TDungeonLevel.Create(1, D_KEEP);
            D_TILE_WILDERNESS: NewDungeonLevel := TDungeonLevel.Create(1, D_WILDERNESS);
            D_TILE_EARTH: NewDungeonLevel := TDungeonLevel.Create(1, D_EARTH);
            D_TILE_AIR: NewDungeonLevel := TDungeonLevel.Create(1, D_AIR);
            D_TILE_FIRE: NewDungeonLevel := TDungeonLevel.Create(1, D_FIRE);
            D_TILE_WATER: NewDungeonLevel := TDungeonLevel.Create(1, D_WATER);
          end;

          { Load the created level }
          FormDisplay.Game.Dungeon.LoadLevel(NewDungeonLevel);

          { Set up the Branch graphics }
          {case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS: FormDisplay.ImageListTerrain.Assign
              ((GDungeonBranchList[D_ABYSS] as TDungeonBranch).BranchGraphics);
            D_TILE_CRYPT: FormDisplay.ImageListTerrain.Assign
              ((GDungeonBranchList[D_CRYPT] as TDungeonBranch).BranchGraphics);
            D_TILE_FORTRESS: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_FORTRESS] as TDungeonBranch).BranchGraphics);
            D_TILE_KEEP: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_KEEP] as TDungeonBranch).BranchGraphics);
            D_TILE_WILDERNESS: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_WILDERNESS] as TDungeonBranch).BranchGraphics);
            D_TILE_EARTH: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_EARTH] as TDungeonBranch).BranchGraphics);
            D_TILE_AIR: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_AIR] as TDungeonBranch).BranchGraphics);
            D_TILE_FIRE: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_FIRE] as TDungeonBranch).BranchGraphics);
            D_TILE_WATER: FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_WATER] as TDungeonBranch).BranchGraphics);
          end; }

          { Get the starting location }
          FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
          FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];

          { Set the delving for this branch if appropriate }
          if FormDisplay.Game.Dungeon.LevelDepth >
            FormDisplay.Game.GDepthDelved[
            FormDisplay.Game.Dungeon.LevelTheme] then
            FormDisplay.Game.GDepthDelved[
              FormDisplay.Game.Dungeon.LevelTheme] :=
              FormDisplay.Game.Dungeon.LevelDepth;

          { Get the viewport boundaries }
          FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width,
            FormDisplay.ScreenMain.Height);

          { Set the viewport boundaries }
          FormDisplay.Game.SetViewPort;

          { Set up the visible area }
          FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

          { Going down stairs passes a turn }  
          PassATurn;

          { Refresh dungeon }
          FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

          { Free the temporary created dungeon level }
          NewDungeonLevel.Free;

          { Clear the active monster list }
          ActiveMonsterList.Clear;

          { case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_ABYSS);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_ABYSS] as TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);

              FormDisplay.Game.SetViewPort;


              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_CRYPT:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_CRYPT);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_CRYPT] as TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);

              FormDisplay.Game.SetViewPort;

              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_FORTRESS:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_FORTRESS);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_FORTRESS] as 
                TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;


              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);

              FormDisplay.Game.SetViewPort;

              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_KEEP:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_KEEP);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_KEEP] as
                TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);

              FormDisplay.Game.SetViewPort;

              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_WILDERNESS:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_WILDERNESS);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign
                ((GDungeonBranchList[D_WILDERNESS] as 
                TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved
                  [FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);


              FormDisplay.Game.SetViewPort;

              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_EARTH:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_EARTH);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_EARTH] 
                as TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);


              FormDisplay.Game.SetViewPort;



              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_AIR:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_AIR);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_AIR] as 
                TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height); 

              FormDisplay.Game.SetViewPort;



              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_FIRE:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_FIRE);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_FIRE] 
                as TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);

              FormDisplay.Game.SetViewPort;


              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;
            D_TILE_WATER:
            begin
              NewDungeonLevel := TDungeonLevel.Create(1, D_WATER);
              FormDisplay.Game.Dungeon.loadLevel(NewDungeonLevel);
              FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_WATER] 
                as TDungeonBranch).BranchGraphics);
              FormDisplay.Game.PlayerX := FormDisplay.Game.Dungeon.StairsUpX[0];
              FormDisplay.Game.PlayerY := FormDisplay.Game.Dungeon.StairsUpY[0];
              if FormDisplay.Game.Dungeon.LevelDepth > 
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] then
                FormDisplay.Game.GDepthDelved[
                  FormDisplay.Game.Dungeon.LevelTheme] := 
                  FormDisplay.Game.Dungeon.LevelDepth;

              FormDisplay.Game.SetViewBoundaries(FormDisplay.ScreenMain.Width, 
                FormDisplay.ScreenMain.Height);
              FormDisplay.Game.SetViewPort;


              FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
                FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

              PassATurn;
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
              NewDungeonLevel.Free;

              ActiveMonsterList.Clear;
            end;  }
          end;

          { Update the message log that the player has entered a new dungeon }
          if FormDisplay.Game.Dungeon.LevelDepth = 1 then
            UpdateLog('You have travelled through the portal to ' +
              FormDisplay.Game.Dungeon.Name, mesStairsandPortals);

          { Get a level feeling but not on the first level of the dungeon to
            avoid stair-scumming }
          if FormDisplay.Game.Dungeon.LevelDepth > 1 then
            UpdateLog(FormDisplay.Game.Dungeon.GetLevelFeeling, 
              mesStairsandPortals);

          { Always report any Unique Monster on the level }
          if FormDisplay.Game.Dungeon.Unique <> nil then
            UpdateLog(FormDisplay.Game.Dungeon.GetUniqueMonsterFeeling,
              mesStairsandPortals);
              
          { Clear the active monster list }
          ActiveMonsterList.Clear;
        end;
    end;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Add text to the message log in a specified colour }
procedure UpDateLogColor(MessageToAdd: String; messageColor: TColor = clWhite);
var
  Initial: String;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.UpDateLogColor()');

  try
    { Check to see if we have a string to add }
    if Length(Trim(MessageToAdd)) > 0 then
    begin
      { Camel-Case the text just to be sure }
      Initial :=  MessageToAdd[1];
      MessageToAdd := UpperCase(Initial) + Copy(MessageToAdd, 2, 
        Length(MessageToAdd));

      { Change the font colour }
      FormDisplay.UpdateLog.SelAttributes.Color := messageColor;

      { Add the text }
      FormDisplay.UpdateLog.SelText := MessageToAdd + #10;

      { Set the selected text back to the default }
      FormDisplay.UpdateLog.SelAttributes.Color := clWindowText;

      { Scroll down }
      FormDisplay.UpdateLog.SelStart := Length(FormDisplay.UpdateLog.Text);
      SendMessage(FormDisplay.UpdateLog.handle,EM_ScrollCaret,0,0);

      { Reset the focus }
      if FormDisplay.PageControlMain.ActivePageIndex = MAINDISPLAY then
        FormDisplay.PanelDisplay.SetFocus;
    end;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Add text to the message log }
procedure UpDateLog(MessageToAdd: String; messageType: mType = mesDefault);
var
  Initial: String;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.UpDateLog()');

  try
    { Check to see if we have a string to add }
    if Length(Trim(MessageToAdd)) > 0 then
    begin
      { Camel-Case the text just to be sure }
      Initial :=  MessageToAdd[1];
      MessageToAdd := UpperCase(Initial) + Copy(MessageToAdd, 2,
        Length(MessageToAdd));

      { Change the font colour using the mType array entries }
      FormDisplay.UpdateLog.SelAttributes.Color := Ord(messageType);

      { Add the text }
      FormDisplay.UpdateLog.SelText := MessageToAdd + #10;

      { Set the selected text back to the default }
      FormDisplay.UpdateLog.SelAttributes.Color := clWindowText;

      { Scroll down }
      FormDisplay.UpdateLog.SelStart := Length(FormDisplay.UpdateLog.Text);
      SendMessage(FormDisplay.UpdateLog.handle,EM_ScrollCaret,0,0);

      { Reset the focus }
      if FormDisplay.PageControlMain.ActivePageIndex = MAINDISPLAY then
        FormDisplay.PanelDisplay.SetFocus;
    end;
	except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Close one or more doors adjacent to a square }
procedure CloseDoor(X: Integer; Y: Integer);
var
  A, B: Integer;
  DoorsClosed: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.CloseDoor()');

  try
    { Keep a count of the number of doors to close }
    DoorsClosed := 0;

    { Find any visible doors within 1 square to close }
    for A := (X - 1) to (X + 1) do
    begin
      for B := (Y - 1) to (Y + 1) do
      begin
        { Only do visible doors }
        if FormDisplay.Game.Dungeon.Visible[A, B] = CURRENTLY_VISIBLE then
        begin
          { Don't do doors on the town level here because they use a different
            tile }
          if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
          begin
            { Check for DOOR_OPEN tiles }
            if FormDisplay.Game.Dungeon.Terrain[A, B] = T_DOOR_OPEN then
            begin
              { Swap these for DOOR_CLOSED tiles }
              FormDisplay.Game.Dungeon.Terrain[A, B] := T_DOOR_CLOSED;

              { Make the terrain not walkable }
              FormDisplay.Game.Dungeon.Walkable[A, B] := False;

              { Increase the number of doors closed }
              Inc(DoorsClosed);
            end;
          end
          else
          begin
            { Check for DOOR_OPEN tiles on the town level }
            if FormDisplay.Game.Dungeon.Terrain[A, B] = D_TILE_OPEN_DOOR then
            begin
              { Swap these for DOOR_CLOSED tiles }
              FormDisplay.Game.Dungeon.Terrain[A, B] := D_TILE_CLOSED_DOOR;

              { Make the terrain not walkable }
              FormDisplay.Game.Dungeon.Walkable[A, B] := True;

              { Increase the number of doors closed }
              Inc(DoorsClosed);
            end;
          end;
        end;
      end;
    end;

    { If there are doors to be closed then pass a turn and allow monsters to
      react if they can }
    if DoorsClosed > 0 then
    begin
      { Reset the visible area }
      FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX, 
        FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

      { Check the thievery skill to allow a chance of not passing a turn, i.e.
        not allowing the monsters to react }
      if Random(PERCENTAGE) >
        FormDisplay.Game.Player.Skills[SK_THIEVERY] * 5 then
      begin
        { Alert the player that a door or doors have been closed }
        if DoorsClosed = 1 then
          UpDateLog('You close the door', mesDoor)
        else
          UpDateLog('You close the doors', mesDoor);

        { Pass a turn }
        PassATurn;
      end
      else
      begin
        { Alert the player that a door or doors have been closed }
        if DoorsClosed = 1 then
          UpDateLog('You close the door quickly before anyone or anything ' +
            'notices', mesDoor)
        else
          UpDateLog('You close the doors quickly before anyone or anything ' +
            'notices', mesDoor);

        { Handle skill learning }
				if OneChanceIn(3) then
					FormDisplay.Game.Player.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
        FormDisplay.Game.Player.LearnSkill(SK_THIEVERY, Random(3) + 3);
      end;

      { Process creatures }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh dungeon }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end
    else
    begin
      { No doors to close }
      UpDateLog('There are no doors here to close!', mesDoor)
    end;
	except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Open one or more doors adjacent to a square }
procedure OpenDoors(X: Integer; Y: Integer);
var
  A, B: Integer;
  DoorsOpened: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.OpenDoors()');

  try
    { Keep a count of the number of doors to open }
    DoorsOpened := 0;

    { Find any visible doors within 1 square to close }
    for A := (X - 1) to (X + 1) do
    begin
      for B := (Y - 1) to (Y + 1) do
      begin
        { Only do visible doors }
        if FormDisplay.Game.Dungeon.Visible[A, B] = CURRENTLY_VISIBLE then
        begin
          { Don't do doors on the town level here because they use a different
            tile }
          if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
          begin
            { Check for DOOR_CLOSED tiles }
            if FormDisplay.Game.Dungeon.Terrain[A, B] = T_DOOR_CLOSED then
            begin
              { Swap these for DOOR_OPEN tiles }
              FormDisplay.Game.Dungeon.Terrain[A, B] := T_DOOR_OPEN;

              { Make the terrain walkable }
              FormDisplay.Game.Dungeon.Walkable[A, B] := True;

              { Increase the number of doors opened }
              Inc(DoorsOpened);
            end;
          end
          else
          begin
            { Check for DOOR_CLOSED tiles on the town level }
            if FormDisplay.Game.Dungeon.Terrain[A, B] = D_TILE_CLOSED_DOOR then
            begin
              { Swap these for DOOR_OPEN tiles }
              FormDisplay.Game.Dungeon.Terrain[A, B] := D_TILE_OPEN_DOOR;

              { Make the terrain walkable }
              FormDisplay.Game.Dungeon.Walkable[A, B] := True;

              { Increase the number of doors opened }
              Inc(DoorsOpened);
            end;
          end;
        end;
      end;
    end;

    { If there are doors to be opened then pass a turn and allow monsters to
      react if they can }
    if DoorsOpened > 0 then
    begin
      { Reset the visible area }
      FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

      { Check the thievery skill to allow a chance of not passing a turn, i.e.
        not allowing the monsters to react }
      if Random(PERCENTAGE) >
        FormDisplay.Game.Player.Skills[SK_THIEVERY] * 5 then
      begin
        { Alert the player that a door or doors have been opened }
        if DoorsOpened = 1 then
          UpDateLog('You open the door', mesDoor)
        else
          UpDateLog('You open the doors', mesDoor);

        { Pass a turn }
        PassATurn;
      end
      else
      begin
        { Alert the player that a door or doors have been closed }
        if DoorsOpened = 1 then
          UpDateLog('You open the door quickly before anyone or anything ' +
            'notices', mesDoor)
        else
          UpDateLog('You open the doors quickly before anyone or anything ' +
            'notices', mesDoor);

        { Handle skill learning }
				if OneChanceIn(3) then
					FormDisplay.Game.Player.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
        FormDisplay.Game.Player.LearnSkill(SK_THIEVERY, Random(3) + 3);
      end;

      { Process creatures }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh dungeon }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end
    else
    begin
      { No doors to open }
      UpDateLog('There are no doors here to open!', mesDoor)
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate and display the text that is added to the message log when the player
  moves over an object or objects on the ground; this also picks up coin }
procedure DisplayObjectText(ObjectID: Integer);
var
  MyItem: TItem;
  TextToDisplay: String;
  Gold: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.OpenDoors()');

  try
    { If we have some coins (of whatever type }
    if ObjectID >= ITEM_GOLD then
    begin
      { Default amount of gold picked up }
      Gold := 0;

      { Gold coins }
      if ObjectID = ITEM_GOLD then
      begin
        { Random amount of gold coins }
        Gold := Random(GOLD_AMOUNT) + FormDisplay.Game.Player.Charisma;

        { Update the log }
        UpdateLog('You pick up ' + IntToStr(Gold) + ' gold pieces',
          mesItemManipulation);
      end
      { Silver coins }
      else if ObjectID = ITEM_SILVER then
      begin
        { Random amount of silver  coins }
        Gold := Random(SILVER_AMOUNT) + FormDisplay.Game.Player.Charisma * 2;

        { Update the log }
        UpdateLog('You pick up ' + IntToStr(Gold) + ' silver pieces',
          mesItemManipulation);

        { 10 silver coins to 1 gold coin }
        Gold := Gold div 10;

        { Make sure we always have enough silver coins for one gold coin }
        if Gold = 0 then
          Gold := 1;
      end
      { Bronze coins }
      else if ObjectID = ITEM_BRONZE then
      begin
        { Random amount of bronze coins }
        Gold := Random(BRONZE_AMOUNT) + GOLD_AMOUNT +
          FormDisplay.Game.Player.Charisma * 3;

        { Update the log }
        UpdateLog('You pick up ' + IntToStr(Gold) + ' bronze pieces',
          mesItemManipulation);

        { 10 bronze coins to 1 silver coin }
        Gold := Gold div 100;

        { Make sure we always have enough bronze coins for one gold coin }
        if Gold = 0 then
          Gold := 1;
      end;

      { Add the coin to the player's stash }
      FormDisplay.Game.Player.Gold := FormDisplay.Game.Player.Gold + Gold;

      { Remove the coin from the dungeon }
      FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] := 0;

      { Refresh the display }
      FormDisplay.UpdateStatus;

      { Pass a turn }
      PassATurn;
    end
    else
    { MovePlayer handles multiple items - this routine is only called if there
      is one item on the tile }
    begin
      { Get the local item }
      MyItem := (GItemList[ObjectID] as TItem);

      { Get the text to display, making sure to lowercase the first character }
      TextToDisplay := Lower(MyItem.Name);

      { If we are in a shop, display the price as well }
      if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and
        (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] = D_TILE_SHOP) then
        { Display a message to the player }
        UpDateLog('You see ' + TextToDisplay + ' for sale (' +
          IntToStr(MyItem.Cost) + ' gp)', mesFloorDescription)
      else if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and
        (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] = D_TILE_HOUSE) then
        { Handle the character's house }
        UpDateLog('You see ' + TextToDisplay + ' stored here',
          mesFloorDescription)
      else
        { Display a message to the player }
        UpDateLog('You see ' + TextToDisplay + ' here', mesFloorDescription);
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ OpenDoor is the routine used to open doors as part of moving, i.e. OpenDoors
  is triggered by pressing 'o' and will affect all local doors whereas OpenDoor
  will only open a door in the direction of movement }
procedure OpenDoor(X: Integer; Y: Integer);
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.OpenDoor()');

  try
    { Replace the terrain with an open door }
    if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
      FormDisplay.Game.Dungeon.Terrain[X, Y] := T_DOOR_OPEN
    else
      FormDisplay.Game.Dungeon.Terrain[X, Y] := D_TILE_OPEN_DOOR;

    { Make the terrain walkable }
    FormDisplay.Game.Dungeon.Walkable[x, y] := True;

    { Check the thievery skill to allow a chance of not passing a turn, i.e.
      not allowing the monsters to react }
    if Random(PERCENTAGE) >
      FormDisplay.Game.Player.Skills[SK_THIEVERY] * 5 then
    begin
      { Alert the player that a door has been opened }
      UpDateLog('You open the door', mesDoor);

      { Reset the visible area }
      FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

      { Pass a turn }
      PassATurn;

      { Let monsters react }
      ProcessCreatures(FormDisplay.Game.Dungeon);
    end
    else
    begin
      { Alert the player that a door has been opened }
      UpDateLog('You open the door quickly before anyone or anything notices',
        mesDoor);

      { Reset the visible area }
      FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

      { Handle skill learning }
      if OneChanceIn(3) then
        FormDisplay.Game.Player.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
      FormDisplay.Game.Player.LearnSkill(SK_THIEVERY, Random(3) + 3);
    end;

    { Refresh the display }
    FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Dig in a specified direction }
procedure Dig(X: Integer; Y: Integer);
var
  Effects: Integer;
  Loop: Integer;
  Terrain: Integer;
  ItemQualityRatio: Integer;
  ItemQuality: crItemQuality;                                   
  ItemType: crItemType;
  LocalItem: TItem;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.Dig()');

  try
    { Check for the different types of wall we can dig through }
    Terrain := FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX + x,
      FormDisplay.Game.PlayerY + y];
    Effects := FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + x,
      FormDisplay.Game.PlayerY + y];

    if (Terrain in [T_SOFTWALL, T_ROOMWALL]) or (Effects in [E_SPECIALEFFECT,
      E_STANDARDEFFECT, E_DARKEREFFECT]) then
    begin
      { Digging takes a certain number of turns, and abort if we cannot dig for
        all those turns }
      for Loop := 1 to DIGGING_TURNS do
      begin
        { Process Creature AI }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Abort if there are monsters visible }
        if not(FormDisplay.Game.CanRest) then
        begin
          { Alert the player }
          UpdateLog('There are monsters nearby. Digging aborted...');

          { Exit completely without changing the terrain }
          Exit;
        end;

        { Pass a turn }
        PassATurn;
      end;

      { Change the terrain to a corridor }
      FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] := T_FLOOR_CORRIDOR;

      { Remove any effects }
      if FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] in [E_SPECIALEFFECT,
        E_STANDARDEFFECT, E_DARKEREFFECT] then
        FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] := E_NONE;

      { Make the terrain walkable }
      FormDisplay.Game.Dungeon.Walkable[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] := True;

      { Alert to a successful dig }
      UpdateLog('You have removed the obstacle successfully!', mesDefault);

      { If we had a node effect, place either gold or an item on the ground }
      if (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] = E_MINERAL) then
      begin
        { Get rid of the effect now we've dug through it }
        FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + x,
          FormDisplay.Game.PlayerY + y] := E_NONE;

        { Place either a magical item on the now excavated square... }
        if (Random(PERCENTAGE) < CHANCE_NODE_ITEM) then
        begin
          { Figure out what type of item to grant the player }
          ItemQualityRatio := Random(PERCENTAGE) + 1;

          { Only hand out very decent quality items }
          if ItemQualityRatio < CHANCE_ARTIFACT then
            ItemQuality := iArtifact
          else if ItemQualityRatio < CHANCE_EPIC then
            ItemQuality := iEpic
          else
            ItemQuality := iLegendary;

          { Decide what type of items to give out }
          ItemQualityRatio := Random(PERCENTAGE) + 1;

          { Only certain types of items can be handed out }
          if ItemQualityRatio < CHANCE_WEAPON then
            ItemType := iWeapon
          else if ItemQualityRatio < CHANCE_ARMOUR then
            ItemType := iArmour
          else if ItemQualityRatio < CHANCE_RING then
            ItemType := iRing
          else
            ItemType := iAmulet;

          { Create the item }
          LocalItem := TItem.Create(ItemQuality,
            FormDisplay.Game.Player.Levels, ItemType, False);

          { Add the item to the dungeon }
          LocalItem.Location := iFloor;
          GItemList.Add(LocalItem);
          FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX + x,
            FormDisplay.Game.PlayerY + y] := GItemList.Count - 1;

          { Alert the player to what has happened }
          UpdateLog('You have found something buried here!', mesDefault);
        end
        else
        begin
          { ...Or place some gold here }
          FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX + x,
            FormDisplay.Game.PlayerY + y] := ITEM_GOLD;

          { Alert the player to what has happened }
          UpdateLog('You have found some treasure buried here!',
            mesDefault);
        end;
      end;

      { Reset the view }
      FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

      { Refresh the display }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end
    { We can't dig through some walls }
    else if Terrain = T_HARDWALL then
    begin
      { Alert the user }
      UpdateLog('You cannot dig through this...', mesError);

      { Refresh the display }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

      { Be kind and don't pass a turn or allow monsters to react }
    end
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Move the player in a specified direction, also handles melee combat }
procedure MovePlayer(X: Integer; Y: Integer);
var
  AttackResult: AResult;
  AttackString: String;
  DamageCaused: Integer;
  DamageString: String;
  LocalMonster: TMonster;
  MonsterID: Integer;
  NPCTalk: String;
  NPCType: Integer;
  XP: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.MovePlayer()');

  try
    { Check for confusion, which changes the direction randomly }
    if FormDisplay.Game.Player.Has(CONFUSED) then
    begin
      X := Random(3) - 1;
      Y := Random(3) - 1;
    end;

    { First check to make sure we're not moving off the edge of the map  }
    if ((FormDisplay.Game.PlayerX + X) < 10) or
      ((FormDisplay.Game.PlayerX + X) > DUNGEONSIZEX - 10) then
      Exit;
    if ((FormDisplay.Game.PlayerY + Y) < 10) or
      ((FormDisplay.Game.PlayerY + Y) > DUNGEONSIZEY - 10) then
      Exit;

    { Check if we're doing melee combat by bumping into a monster}
    MonsterID := FormDisplay.Game.Dungeon.Monsters[FormDisplay.Game.PlayerX + X,
      FormDisplay.Game.PlayerY + Y];

    { Handle hostile creatures only here }
    if ((MonsterID > 0) and (FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN)) then
    begin
      { Get the monster }
      LocalMonster := GMonsterList.Items[MonsterID] as TMonster;

      { See if we can hit the monster in melee combat }
      if XCanHitY(FormDisplay.Game.Player, LocalMonster, AttackString,
        AttackResult) then
      begin
        { We can, so get the damage caused }
        DamageCaused := XMeleeDamagesY(FormDisplay.Game.Player, LocalMonster,
          DamageString, AttackResult);

        { Update the message log with the attack result }
        UpdateLog(AttackString + DamageString, mesYouCombatHit);

        { Decrease the monsters HP }
        LocalMonster.CurrentHP := LocalMonster.CurrentHP - DamageCaused;

        { Check if the monster is dead }
        if LocalMonster.CurrentHP < 1 then
        begin
          { Check for the type of monster and print an appropriate death
            message if dead }
          if LocalMonster.Category = 'Undead' then
            { Dead, so update the messae log }
            UpdateLog('You have destroyed ' +
              Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
              mesYouKill)
          else if LocalMonster.Category = 'Outsider' then
            { Dead, so update the messae log }
            UpdateLog('You have banished ' +
              Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
              mesYouKill)
          else
            { Dead, so update the messae log }
            UpdateLog('You have killed ' +
              Trim(LocalMonster.Prefix + LocalMonster.Name) + '!',
              mesYouKill);

          { Check if we have killed a unique monster and if so, take a note }
          if LocalMonster.UniqueName <> '' then
            FormDisplay.Game.Player.TakeNote(FormDisplay.Game.Turns,
              LocalMonster.UniqueName, nUnique, FormDisplay.Game.Dungeon);

          { Update the kill count }
          FormDisplay.Game.LogMonsterKill(LocalMonster);

          { Monsters have an XP value }
          XP := LocalMonster.XP;

          { Increase XP as we go down the dungeon }
          XP := XP + Trunc(Power(FormDisplay.Game.Dungeon.LevelDepth, 1.5));

          { Gain XP }
          FormDisplay.Game.Player.GainXP(XP);

          { If the monster is carrying an item, drop it }
          MonsterDropItem(LocalMonster);

          { Remove the monster from the map }
          FormDisplay.Game.Dungeon.RemoveMonsterFromMap(
            Point(FormDisplay.Game.PlayerX + X, FormDisplay.Game.PlayerY + Y));

          { Make the monster dead }
          LocalMonster.Alive := False;
          LocalMonster.Awake := False;

          { Refresh the status (for gaining XP) }
          FormDisplay.UpdateStatus;
        end;
      end;

      { In all cases, allow monsters a turn }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh the display }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end
    else if not(FormDisplay.Game.Player.Has(HELD)) then
    begin
      { Check for Opening Doors - we do this before the walkable check since
        closed doors aren't walkable by default, but opening a door makes it
        walkable }
      if ((FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and
        (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] = D_TILE_CLOSED_DOOR)) or
        ((FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y] = T_DOOR_CLOSED) and
        (FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN)) then
      begin
        { Open the individual door - the OpenDoor routine takes care of allowing
          monsters to react and so on }
        OpenDoor(FormDisplay.Game.PlayerX + x, FormDisplay.Game.PlayerY + y);

        { Set the visible area }
        FormDisplay.Game.Dungeon.Setvisible(FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

        { Refresh the display }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
        Exit;
      end;

      { Check if the square we're moving into is walkable, if not, do nothing }
      if FormDisplay.Game.Dungeon.Walkable[FormDisplay.Game.PlayerX+x,
        FormDisplay.Game.PlayerY+y] = False then
      begin
        { Add a message to say this is a wall }
        if (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX+x,
          FormDisplay.Game.PlayerY+y] in [E_SPECIALEFFECT, E_STANDARDEFFECT]) then
          UpdateLog
            (GDungeonEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
            + ' stops you for now...', mesDefault)
        else
          UpdateLog('There''s a wall here...', mesDefault);

        { Pass a turn }
        PassATurn;

        { Allow monsters to react }
        ProcessCreatures(FormDisplay.Game.Dungeon);

        { Refresh the display }
        FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

        { And exit without doing anything else }
        Exit;
      end;

      { Handle townspeople }
      if ((MonsterID > 0) and (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN)) then
      begin
        { Get the monster }
        LocalMonster := GTownsPeopleList.Items[MonsterID] as TMonster;

        { Notify that we're pushing past the creature }
        if (LocalMonster.ID < HOUSEKEEPER) then
        begin
          UpdateLog('You push past the ' + Trim(LocalMonster.Name), mesMonsterAction);
        end
        else
        begin
          UpdateLog('You push past ' + Trim(LocalMonster.Name),
            mesMonsterAction);
        end;

        { Say something }
        NPCType := LocalMonster.ID;
        if (NPCType < HOUSEKEEPER) then
        begin
          NPCTalk := Format('The %s %s "%s"', [LocalMonster.Name,
            NPCSpeech[NPCType], NPCSays[NPCType, Random(3) +1]]);
          UpdateLog(NPCTalk, mesMonsterAction);
        end
        else
        begin
          NPCTalk := Format('%s %s "%s"', [LocalMonster.Name,
            NPCSpeech[NPCType], NPCSays[NPCType, Random(3) +1]]);
          UpdateLog(NPCTalk, mesMonsterAction);
        end;

        { Remove the monster from the map from the square you're entering }
        FormDisplay.Game.Dungeon.Monsters[FormDisplay.Game.PlayerX + X,
          FormDisplay.Game.PlayerY + Y] := 0;

        { Add the monster back in to the current square }
        FormDisplay.Game.Dungeon.Monsters[FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY] := MonsterID;
      end;

      { Just moving whilst wearing armour can exercise the armour skill }
      if OneChanceIn(6) then
        FormDisplay.Game.Player.LearnSkillArmour;

      { Handle the text for flavour }
      if (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] = E_GROUNDEFFECT) and
        (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + X,
        FormDisplay.Game.PlayerY + Y] <> E_GROUNDEFFECT) then
        UpdateLog('You leave the ' +
          GDungeonBackgroundEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme],
          mesDefault);
      if (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX + X,
        FormDisplay.Game.PlayerY + Y] = E_GROUNDEFFECT) and
        (FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] <> E_GROUNDEFFECT) then
        UpdateLog('You enter a ' +
          GDungeonBackgroundEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme],
          mesDefault);

      { Move the viewport window };
      Inc(FormDisplay.Game.topleftx, x);
      Inc(FormDisplay.Game.toplefty, y);
      Inc(FormDisplay.Game.toprightx, x);
      Inc(FormDisplay.Game.toprighty, y);
      Inc(FormDisplay.Game.bottomleftx, x);
      Inc(FormDisplay.Game.bottomlefty, y);
      Inc(FormDisplay.Game.bottomrightx, x);
      Inc(FormDisplay.Game.bottomrighty, y);

      { Reset the visible map area }
      FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX + x,
        FormDisplay.Game.PlayerY + y,FormDisplay.Game.Player.Alertness);

      { Now the player is no longer standing on a square, reset its terrain
        cost to be the same as any other squares }
      FormDisplay.Game.dungeon.TerrainCost[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] := TC_NONE;

      { Move the player }
      Inc(FormDisplay.Game.PlayerX, x);
      inc(FormDisplay.Game.PlayerY, y);

      { Make the player's square impassable }
      FormDisplay.Game.dungeon.TerrainCost[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] := TC_IMPASSABLE;

      { Pass a turn }
      PassATurn;

      { Allow creatures to have a go }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh the display }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

      { Update Log with what is visible on this square }
      if (FormDisplay.Game.Dungeon.Zone[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] > 0) or (FormDisplay.Game.Dungeon.LevelTheme
        = D_TOWN) then
      begin
        { Handle stairs/portals }
        case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY] of

          T_STAIRS_DOWN: UpdateLog('There is ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading downwards here....',
              mesStairsandPortals);
          T_STAIRS_UP: UpdateLog('There is ' +
              GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
              + ' leading upwards here....', mesStairsandPortals);
          D_TILE_ABYSS:
            UpDateLog('There is a portal leading to the Abyss here',
              mesStairsandPortals );
          D_TILE_CRYPT:
            UpDateLog('There is a portal leading to the Mausoleum here',
              mesStairsandPortals );
          D_TILE_FORTRESS:
            UpDateLog('There is a portal leading to the Fortress of Ablach here',
              mesStairsandPortals );
          D_TILE_KEEP:
            UpDateLog('There is a portal leading to Rancar''s Keep here',
              mesStairsandPortals );
          D_TILE_WILDERNESS:
            UpDateLog('There is a portal leading to the Wilderlands here',
              mesStairsandPortals );
          D_TILE_EARTH:
            UpDateLog('There is a portal leading to Plane of Earth here',
              mesStairsandPortals );
          D_TILE_AIR:
            UpDateLog('There is a portal leading to Plane of Air here',
              mesStairsandPortals );
          D_TILE_FIRE:
            UpDateLog('There is a portal leading to Plane of Fire here',
              mesStairsandPortals );
          D_TILE_WATER:
            UpDateLog('There is a portal leading to Plane of Water here',
              mesStairsandPortals );
          T_HARDWALL:
          begin
            if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
              UpDateLog('You are standing on the edge of the Planar Expanse',
                mesStairsandPortals);
          end;
        end;

        { Handle messages that indicate if the player is ready to explore a
          particular dungeon branch }
        if FormDisplay.Game.Player.Levels < 6 then
        begin
          case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_CRYPT:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_KEEP:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_EARTH:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_AIR:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_FIRE:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_WATER:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
          end;
        end
        else if FormDisplay.Game.Player.Levels < 11 then
        begin
          case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_EARTH:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_AIR:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_FIRE:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
            D_TILE_WATER:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
          end;
        end
        else if FormDisplay.Game.Player.Levels < 16 then
        begin
          case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] of
            D_TILE_ABYSS:
              UpDateLog('You feel you are not ready to explore this plane yet',
                mesStairsandPortals );
          end;
        end;
      end
      else
      { Handle standard terrain types }
      begin
        case FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] of
        T_STAIRS_DOWN: UpdateLog('There is ' +
          GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
          + ' leading downwards here....',
          mesStairsandPortals);
        T_STAIRS_UP: UpdateLog('There is ' +
          GDungeonStairsDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
          + ' leading upwards here....', mesStairsandPortals);
        T_FOUNTAIN:
          UpDateLog('There is a fountain here...',
            mesStairsandPortals );
        T_FOUNTAIN_USED:
          UpDateLog('There is a dried-up fountain here...',
            mesStairsandPortals );
        end;
      end;

      { Handle gold and items }
      if (FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] > 0) and
        (FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] < ITEM_GOLD) then
      begin
        { If there is only one item on the tile }
        if FormDisplay.Game.Dungeon.GetNumberofItemsOnTile(
          Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)) = 1 then
          DisplayObjectText
            (FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY])
        else
        begin
          { If there are multiple items on the tile }
          UpDateLog('There are a number of items here:', mesFloorDescription);
          UpDateLog(FormDisplay.Game.Dungeon.GetDecriptionOfItemsOnTile(
            Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)),
            mesFloorDescription);
        end
      end
      { Automatically pick up gold }
      else if FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] >= ITEM_GOLD then
        DisplayObjectText(FormDisplay.Game.Dungeon.Objects
          [FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY])
      else
      { Handle special terrain effects }
      begin
        case FormDisplay.Game.Dungeon.Effects[FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY] of
          E_SPECIALEFFECT, E_STANDARDEFFECT:
          begin
            { Check for zones }
            if FormDisplay.Game.Dungeon.Zone[FormDisplay.Game.PlayerX,
              FormDisplay.Game.PlayerY] > 0 then
            DisplayEffectText(FormDisplay.Game.Dungeon.Zone
              [FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY])
            else
              DisplayEffectText(FormDisplay.Game.Dungeon.LevelTheme);
          end;
          E_DARKEREFFECT:
          begin
            { Check for zones }
            if FormDisplay.Game.Dungeon.Zone[FormDisplay.Game.PlayerX,
              FormDisplay.Game.PlayerY] > 0 then
            DisplayDarkerEffectText(FormDisplay.Game.Dungeon.Zone
              [FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY])
            else
              DisplayDarkerEffectText(FormDisplay.Game.Dungeon.LevelTheme);
          end;
        end;
      end;
    end
    else
    begin
      { Held, and thus cannot move }
      UpDateLog('You cannot move!', mesStatus);
      
      { Pass a turn }
      PassATurn;

      { Allow creatures to have a go }
      ProcessCreatures(FormDisplay.Game.Dungeon);

      { Refresh the display }
      FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Pick up an item }
procedure GetAnItem(ItemID: Integer);
var
  ItemCost: Integer;
  SlotAddedTo: Integer;
  CurrentItem: TItem;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.GetAnItem()');

  try
    { Handle buying of items in the town level }
    if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and 
      (FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX, 
      FormDisplay.Game.PlayerY] = D_TILE_SHOP) then
    begin
      { Get the item cost }
      ItemCost := (GItemList[ItemID] as TItem).Cost;

      { See if the character can afford it }
      if ItemCost > FormDisplay.Game.Player.Gold then
        UpdateLog('You cannot afford this item!', mesItemManipulation)
      else
      begin
        { Get the inventory slot the item would go into }
        SlotAddedTo := FormDisplay.Game.Player.AddItem(ItemID);

        { If the inventory isn't full then add it }
        if SlotAddedTo > -1 then
        begin
          { Set the item location to the inventory }
          (GItemList[ItemID] as TItem).Location := iInventory;

          { Update the message log with the buying message }
          UpdateLog('You have bought ' + Lower((GItemList[ItemID] as TItem).Name)
            +	' for ' + IntToStr(ItemCost) + ' gp', mesItemManipulation );

          { Check for items that stack }
          if (GItemList[FormDisplay.Game.Player.Inventory[SlotAddedTo]] as
            TItem).Count > 1 then
            UpdateLog('You now have ' + 
              (GItemList[FormDisplay.Game.Player.Inventory[SlotAddedTo]] 
              as TItem).Name, mesItemManipulation );

          { Reduce the character's gold }
          FormDisplay.Game.Player.Gold := FormDisplay.Game.Player.Gold -
            ItemCost;

          { Remove the item from the shop }
          FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
            FormDisplay.Game.PlayerY] := 0;

          { Update the status to reflect the new gold total }
          FormDisplay.UpdateStatus;

          { Pass a turn }
          PassATurn;
        end
        else
          { No inventory space to add the item }
          UpdateLog('You cannot carry anymore items!', mesItemManipulation );
      end;
    end
    else if ItemID >= ITEM_GOLD then
    begin
      { This is never reached since pickup of gold is done on move }
    end
    else
    { For now, just pickup the top item even if there are multiple items on
        the tile } 
    begin
      { Get a slot }
      SlotAddedTo := FormDisplay.Game.Player.AddItem(ItemID);

      { See if the item has been successfully picked up }
      if (SlotAddedTo > -1) then
      begin
        { Alert the player that he/she has picked up an item }
        UpdateLog('You pick up ' + Lower((GItemList[ItemID] as TItem).Name),
          mesItemManipulation);

        { Set the items location }
        (GItemList[ItemID] as TItem).Location := iInventory;

        { Handle item stacking }
        if (GItemList[FormDisplay.Game.Player.Inventory[SlotAddedTo]] as
          TItem).Count > 1 then
          UpdateLog('You now have ' + 
            (GItemList[FormDisplay.Game.Player.Inventory[SlotAddedTo]] as 
            TItem).Name,mesItemManipulation );

        { Handle multiple items on a tile }
        CurrentItem := GItemList[ItemID] as TItem;

        { Shuffle the other items on the tile to the top if necessary }
        if CurrentItem.NextItem <> NO_NEXT_ITEM then
          FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
            FormDisplay.Game.PlayerY] := CurrentItem.NextItem
        else
          FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY] := 0;

        { Break the linkage on the item just picked up }
        CurrentItem.NextItem := NO_NEXT_ITEM;

        { Update the status }
        FormDisplay.UpdateStatus;

        { Pass a turn }
        PassATurn;
      end
      else
        { Alert the player that he can't pick up any more items }
        UpdateLog('You cannot carry anymore items!', mesItemManipulation );
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Load the inventory contents into the inventory screen }
procedure LoadInventoryDetails(Player: TCreature);
var
  ComponentName: String;
  ItemText: TComponent;
  ItemPanel: TComponent;
  LocalItem: TItem;
  Loop: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.LoadInventoryDetails()');

  try
    { Iterate through the inventory slots }
    for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
    begin
      { Find the component to load the slot into }
      ComponentName := 'LabelItem' + IntToStr(Loop);
      ItemText := FormDisplay.FindComponent(ComponentName);
      ComponentName := 'BackPanel' + IntToStr(Loop);
      ItemPanel := FormDisplay.FindComponent(ComponentName);

      { Set the component colour }
      (ItemPanel as TPanel).Color := $00000000;

      { Check if the inventory slot is occupied }
      if Player.Inventory[Loop] > 0 then
      begin
        { Get the item details }
        LocalItem := GItemList[Player.Inventory[Loop]] as TItem;

        { Set the displayed properties }
        (ItemText as TLabel).Caption := LocalItem.Name;
        (ItemText as TLabel).Font.Color := LocalItem.TextColour;

        { Override for Consumables }
        if LocalItem.ItemType = iConsumable then
          (ItemText as TLabel).Font.Color := clSilver;

        { Override for Scrolls }
        if LocalItem.ItemType = iScroll then
          (ItemText as TLabel).Font.Color := clWhite;

        { Check if we need to highlight consumables if we've come in to eat
          something }
        if InventoryStatus = invEat then
        begin
          if LocalItem.ItemType = iConsumable then
            (ItemPanel as TPanel).Color := $00800040;
        end
        else
        if InventoryStatus = invRead then
        begin
          if LocalItem.ItemType = iScroll then
            (ItemPanel as TPanel).Color := $00800040;
        end
        else if InventoryStatus = invIdentify then
        begin
          if LocalItem.Known = False then
            (ItemPanel as TPanel).Color := $00800040;
        end
        else if InventoryStatus = invDrink then
        begin
          if LocalItem.ItemType = iPotion then
            (ItemPanel as TPanel).Color := $00800040;
        end
        else
        begin
          { Handle known/unknown items }
          if LocalItem.Known then
            (ItemPanel as TPanel).Color := $00000000
          else
            (ItemPanel as TPanel).Color := $00002000;
        end;
      end
      else
        { Empty inventory slots }
        (ItemText as TLabel).Caption := '';
    end;

    { Iterate through the equipped slots }
    for Loop := S_FEET to S_BACK do
    begin
      { Find the component to load the slot into }
      ComponentName := 'LabelItem' + IntToStr(Loop);
      ItemText := FormDisplay.FindComponent(ComponentName);
      ComponentName := 'BackPanel' + IntToStr(Loop);
      ItemPanel := FormDisplay.FindComponent(ComponentName);

      { Set the component colour }
      (ItemPanel as TPanel).Color := $00000000;

      { Check if the equipped slot is occupied }
      if Player.Inventory[Loop] > 0 then
      begin
        { Get the item details }
        LocalItem := GItemList[Player.Inventory[Loop]] as TItem;

        { Set the displayed properties }
        (ItemText as TLabel).Caption := LocalItem.Name;
        (ItemText as TLabel).Font.Color := LocalItem.TextColour;

        { Handle known/unknown items }
        if LocalItem.Known then
          (ItemPanel as TPanel).Color := $00000000
        else
          (ItemPanel as TPanel).Color := $00002000;
      end
      else
        { Empty equipped slots }
        (ItemText as TLabel).Caption := '';
    end;

    { Display the amount carried }
    FormDisplay.GradLabelBurden.Caption := IntToStr(Player.Burden) + '/' +
      IntToStr(Player.MaxBurden) + ' lbs (' +
      IntToStr(26 - Player.GetNumberUnusedSlots) + '/26 slots)';

    { Refresh the display }
    FormDisplay.UpdateStatus;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle highlighting of slots on the inventory }
procedure InventoryHighlightDestination(SenderItem: TItem);
var
  DestinationSlot: TComponent;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.InventoryHighlightDestination()');

  try
    { Highlight the available slot this item can be moved to, if indeed it can
      be moved to a slot on the person }
    if SenderItem <> nil then
    begin
      { We have an item - find out what slot it would normally inhabit }
      case SenderItem.ItemSlot of
        iHead: DestinationSlot := FormDisplay.BackPanel32;
        iNeck: DestinationSlot := FormDisplay.BackPanel33;
        iChest: DestinationSlot := FormDisplay.BackPanel31;
        iHands: DestinationSlot := FormDisplay.BackPanel29;
        iArms: DestinationSlot := FormDisplay.BackPanel30;
        iLegs: DestinationSlot := FormDisplay.BackPanel28;
        iFeet: DestinationSlot := FormDisplay.BackPanel27;
        iMainhand: DestinationSlot := FormDisplay.BackPanel34;
        iOffhand: DestinationSlot := FormDisplay.BackPanel35;
        iRanged: DestinationSlot := FormDisplay.BackPanel38;
        iFinger: DestinationSlot := FormDisplay.BackPanel36; 
        iBack: DestinationSlot := FormDisplay.BackPanel39;
      else
        DestinationSlot := nil;
      end;

      { Do the highlighting }
      if DestinationSlot <> nil then
      begin
        (DestinationSlot as TPanel).Color := clGray;

        { Handle rings, since we can carry two rings }
        if DestinationSlot = FormDisplay.BackPanel36 then
          FormDisplay.BackPanel37.Color := clGray;

        { Set up a timer to change the colours back }
        FormDisplay.TimerColorReset.Enabled := True;
      end;
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Load and display the skills }
procedure LoadSkills(Player: TCreature);
var
  Temp: String;
  Stealth: Integer;
  HeavyArmour: Integer;
  LightArmour: integer;
  MediumArmour: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.LoadSkills()');

  try
    { TODO: Find a less monotonous way of doing this }

    { Handle skills }
    SetSkillFontColor(Player.Skills[SK_FIGHTING],
      FormDisplay.GradLabelSkill0);
    FormDisplay.GradLabelSkill0.Caption := 
      Format('%d', [Player.Skills[SK_FIGHTING]]);
    FormDisplay.GradLabelSkill0Progress.Caption := 
      Player.SkillProgressString(SK_FIGHTING);

    SetSkillFontColor(Player.Skills[SK_MELEE],  
      FormDisplay.GradLabelSkill1);
    FormDisplay.GradLabelSkill1.Caption := 
      Format('%d', [Player.Skills[SK_MELEE]]);
    FormDisplay.GradLabelSkill1Progress.Caption := 
      Player.SkillProgressString(SK_MELEE);

    SetSkillFontColor(Player.Skills[SK_RANGED],  
      FormDisplay.GradLabelSkill2);
    FormDisplay.GradLabelSkill2.Caption := 
      Format('%d', [Player.Skills[SK_RANGED]]);
    FormDisplay.GradLabelSkill2Progress.Caption := 
      Player.SkillProgressString(SK_RANGED);

    SetSkillFontColor(Player.Skills[SK_UNARMED],  
      FormDisplay.GradLabelSkill3);
    FormDisplay.GradLabelSkill3.Caption := 
      Format('%d', [Player.Skills[SK_UNARMED]]);
    FormDisplay.GradLabelSkill3Progress.Caption := 
      Player.SkillProgressString(SK_UNARMED);

    FormDisplay.GradLabelSkill4.Caption := 
      Format('%d', [Player.Skills[SK_DEFENSE]]);
    SetSkillFontColor(Player.Skills[SK_DEFENSE],  
      FormDisplay.GradLabelSkill4);
    FormDisplay.GradLabelSkill4Progress.Caption := 
      Player.SkillProgressString(SK_DEFENSE);

    FormDisplay.GradLabelSkill5.Caption := 
      Format('%d', [Player.Skills[SK_HEAVY]]);
    SetSkillFontColor(Player.Skills[SK_HEAVY],  
      FormDisplay.GradLabelSkill5);
    FormDisplay.GradLabelSkill5Progress.Caption := 
      Player.SkillProgressString(SK_HEAVY);

    FormDisplay.GradLabelSkill6.Caption := 
      Format('%d', [Player.Skills[SK_MEDIUM]]);
    SetSkillFontColor(Player.Skills[SK_MEDIUM],  
      FormDisplay.GradLabelSkill6);
    FormDisplay.GradLabelSkill6Progress.Caption := 
      Player.SkillProgressString(SK_MEDIUM);

    FormDisplay.GradLabelSkill7.Caption := 
      Format('%d', [Player.Skills[SK_LIGHT]]);
    SetSkillFontColor(Player.Skills[SK_LIGHT],  
      FormDisplay.GradLabelSkill7);
    FormDisplay.GradLabelSkill7Progress.Caption := 
      Player.SkillProgressString(SK_LIGHT);

    FormDisplay.GradLabelSkill10.Caption := 
      Format('%d', [Player.Skills[SK_SUBTERFUGE]]);
    SetSkillFontColor(Player.Skills[SK_SUBTERFUGE],  
      FormDisplay.GradLabelSkill10);
    FormDisplay.GradLabelSkill10Progress.Caption := 
      Player.SkillProgressString(SK_SUBTERFUGE);

    { Check for Stealth }
    Stealth := Player.Skills[SK_STEALTH];

    { Reduce Stealth for wearing heavy and medium armour }
    FormDisplay.Game.Player.ReturnArmourRatio(HeavyArmour, MediumArmour,
      LightArmour);
    Dec(Stealth, HeavyArmour * 2);
    Dec(Stealth, MediumArmour * 2);
    if Stealth < 0 then
      Stealth := 0;

    if Player.Has(STEALTHED) then
      FormDisplay.GradLabelSkill11.Caption :=
        Format('%d', [Stealth + STEALTH_BONUS])
    else
      FormDisplay.GradLabelSkill11.Caption :=
        Format('%d', [Stealth]);
    SetSkillFontColor(Player.Skills[SK_STEALTH],  
      FormDisplay.GradLabelSkill11);
    FormDisplay.GradLabelSkill11Progress.Caption := 
      Player.SkillProgressString(SK_STEALTH);

    FormDisplay.GradLabelSkill12.Caption := 
      Format('%d', [Player.Skills[SK_THIEVERY]]);
    SetSkillFontColor(Player.Skills[SK_THIEVERY],  
      FormDisplay.GradLabelSkill12);
    FormDisplay.GradLabelSkill12Progress.Caption := 
      Player.SkillProgressString(SK_THIEVERY);

    FormDisplay.GradLabelSkill20.Caption := 
      Format('%d', [Player.Skills[SK_MAGIC]]);
    SetSkillFontColor(Player.Skills[SK_MAGIC],  
      FormDisplay.GradLabelSkill20);
    FormDisplay.GradLabelSkill20Progress.Caption := 
      Player.SkillProgressString(SK_MAGIC);

    FormDisplay.GradLabelSkill21.Caption := 
      Format('%d', [Player.Skills[SK_FIRE]]);
    SetSkillFontColor(Player.Skills[SK_FIRE],  
      FormDisplay.GradLabelSkill21);
    FormDisplay.GradLabelSkill21Progress.Caption := 
      Player.SkillProgressString(SK_FIRE);

    FormDisplay.GradLabelSkill22.Caption := 
      Format('%d', [Player.Skills[SK_AIR]]);
    SetSkillFontColor(Player.Skills[SK_AIR],  
      FormDisplay.GradLabelSkill22);
    FormDisplay.GradLabelSkill22Progress.Caption := 
      Player.SkillProgressString(SK_AIR);

    FormDisplay.GradLabelSkill23.Caption := 
      Format('%d', [Player.Skills[SK_WATER]]);
    SetSkillFontColor(Player.Skills[SK_WATER],  
      FormDisplay.GradLabelSkill23);
    FormDisplay.GradLabelSkill23Progress.Caption := 
      Player.SkillProgressString(SK_WATER);

    FormDisplay.GradLabelSkill24.Caption := 
      Format('%d', [Player.Skills[SK_EARTH]]);
    SetSkillFontColor(Player.Skills[SK_EARTH],  
      FormDisplay.GradLabelSkill24);
    FormDisplay.GradLabelSkill24Progress.Caption := 
      Player.SkillProgressString(SK_EARTH);

    FormDisplay.GradLabelSkill25.Caption := 
      Format('%d', [Player.Skills[SK_NATURE]]);
    SetSkillFontColor(Player.Skills[SK_THIEVERY],  
      FormDisplay.GradLabelSkill25);
    FormDisplay.GradLabelSkill25Progress.Caption := 
      Player.SkillProgressString(SK_NATURE);

    FormDisplay.GradLabelSkill26.Caption := 
      Format('%d', [Player.Skills[SK_HEALING]]);
    SetSkillFontColor(Player.Skills[SK_HEALING],  
      FormDisplay.GradLabelSkill26);
    FormDisplay.GradLabelSkill26Progress.Caption := 
      Player.SkillProgressString(SK_HEALING);

    FormDisplay.GradLabelSkill27.Caption := 
      Format('%d', [Player.Skills[SK_CURSING]]);
    SetSkillFontColor(Player.Skills[SK_CURSING],  
      FormDisplay.GradLabelSkill27);
    FormDisplay.GradLabelSkill27Progress.Caption := 
      Player.SkillProgressString(SK_CURSING);

    FormDisplay.GradLabelSkill28.Caption := 
      Format('%d', [Player.Skills[SK_COMBAT]]);
    SetSkillFontColor(Player.Skills[SK_COMBAT],  
      FormDisplay.GradLabelSkill28);
    FormDisplay.GradLabelSkill28Progress.Caption := 
      Player.SkillProgressString(SK_COMBAT);

    FormDisplay.GradLabelSkill29.Caption := 
      Format('%d', [Player.Skills[SK_PROTECTION]]);
    SetSkillFontColor(Player.Skills[SK_PROTECTION],  
      FormDisplay.GradLabelSkill29);
    FormDisplay.GradLabelSkill29Progress.Caption := 
      Player.SkillProgressString(SK_PROTECTION);

    FormDisplay.GradLabelSkill30.Caption := 
      Format('%d', [Player.Skills[SK_LORE]]);
    SetSkillFontColor(Player.Skills[SK_LORE],  
      FormDisplay.GradLabelSkill30);
    FormDisplay.GradLabelSkill30Progress.Caption := 
      Player.SkillProgressString(SK_LORE);

    { Handle ablities and attributes }
    FormDisplay.GradLabelAC.Caption := Format('%d', [Player.AC]);
    FormDisplay.GradLabelEV.Caption := Format('%d', [Player.EV]);
    FormDisplay.GradLabelSP.Caption := Format('%d', [Player.Speed]);
    FormDisplay.GradLabelSTR.Caption := Format('%d', [Player.Strength]);
    FormDisplay.GradLabelAGI.Caption := Format('%d', [Player.Agility]);
    FormDisplay.GradLabelEND.Caption := Format('%d', [Player.Endurance]);
    FormDisplay.GradLabelRESO.Caption := Format('%d', [Player.Resolve]);
    FormDisplay.GradLabelINT.Caption := Format('%d', [Player.Intelligence]);
    FormDisplay.GradLabelCHA.Caption := Format('%d', [Player.Charisma]);

    FormDisplay.GradLabelACC.Caption := Format('%d', [Player.Accuracy]);
    SetSkillFontColor(Player.Accuracy,  FormDisplay.GradLabelACC);
    FormDisplay.GradLabelDam.Caption := Format('%d', [Player.DamageBonus]);
    SetSkillFontColor(Player.DamageBonus,  FormDisplay.GradLabelDam);
    FormDisplay.GradLabelBlock.Caption := Format('%d', [Player.Blocking]);
    SetSkillFontColor(Player.Blocking,  FormDisplay.GradLabelBlock);
    FormDisplay.GradLabelDeflect.Caption := Format('%d', [Player.Deflection]);
    SetSkillFontColor(Player.Deflection,  FormDisplay.GradLabelDeflect);

    { Handle resistances }
    FormDisplay.GradLabelRESBase.Caption := Format('%d', [Player.Resistance]);
    SetSkillFontColor(Player.Resistance,  FormDisplay.GradLabelRESBase);
    FormDisplay.GradLabelRESF.Caption := Format('%d', [Player.FireResistance]);
    SetSkillFontColor(Player.FireResistance,  FormDisplay.GradLabelRESF);
    FormDisplay.GradLabelRESA.Caption := Format('%d', [Player.AirResistance]);
    SetSkillFontColor(Player.AirResistance,  FormDisplay.GradLabelRESA);
    FormDisplay.GradLabelRESE.Caption := Format('%d', [Player.EarthResistance]);
    SetSkillFontColor(Player.EarthResistance,  FormDisplay.GradLabelRESE);
    FormDisplay.GradLabelRESW.Caption := Format('%d', [Player.WaterResistance]);
    SetSkillFontColor(Player.WaterResistance,  FormDisplay.GradLabelRESW);
    FormDisplay.GradLabelRESP.Caption := Format('%d', [Player.PoisonResistance]);
    SetSkillFontColor(Player.PoisonResistance,  FormDisplay.GradLabelRESP);
    FormDisplay.GradLabelRESL.Caption := 
      Format('%d', [Player.LifeDrainingResistance]);
    SetSkillFontColor(Player.LifeDrainingResistance,  FormDisplay.GradLabelRESL);

    { Handle vital statistics }
    FormDisplay.LabelExperience.Caption := 
      Format('%d/%d', [Player.XP, Player.NextLevel]);
    FormDisplay.LabelGold.Caption := Format('%d', [Player.Gold]);
    FormDisplay.LabelPTurns.Caption := Format('%d', [FormDisplay.Game.Turns]);

    FormDisplay.GradLabelHP.Caption := 
      Format('%d/%d', [Player.HP, Player.MaxHP]);
    SetSkillFontColor(Player.HP, Player.MaxHP, FormDisplay.GradLabelHP);
    FormDisplay.GradLabelMP.Caption := 
      Format('%d/%d', [Player.MP, Player.MaxMP]);
    SetSkillFontColor(Player.MP, Player.MaxMP, FormDisplay.GradLabelMP);

    { Handle player details }
    case (Ord(Player.SubRace)) of
      0: Temp := '';
      1: Temp := 'Human ';
      2: Temp := 'Halfling ';
      3: Temp := 'Orc ';
      4: Temp := 'Dwarf ';
      5: Temp := 'Elf ';
    end;

    Temp := Temp + Player.FriendlyClassName+ ' ';

    case (Ord(Player.CClass)) of
      0: Temp := Temp + '';
      1: Temp := Temp + '(Thief)';
      2: Temp := Temp + '(Knight)';
      3: Temp := Temp + '(Mage)';
      4: Temp := Temp + '(Priest)';
      5: Temp := Temp + '(Warrior)';
    end;
    Temp := Player.Name + ' (Level ' + IntToStr(Player.Levels) + ' ' + Temp + ')';
    FormDisplay.GradLabelName.Caption := Temp;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle displaying of dungeon terrain effects in the message log }
procedure DisplayEffectText(DungeonTheme: Integer);
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.DisplayEffectText()');

  try
    { Output a description of any special terrain the character is staying on }
    UpdateLog('You see ' +
      GDungeonEffectDescriptionArray[DungeonTheme] + ' here...',
      mesFloorDescription);
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle displaying of dungeon terrain effects in the message log }
procedure DisplayDarkerEffectText(DungeonTheme: Integer);
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.DisplayDarkerEffectText()');

  try
    { Output a description of any special terrain the character is staying on }
    UpdateLog('You see ' +
      GDungeonDarkerEffectDescriptionArray[DungeonTheme] + ' here...',
      mesFloorDescription);
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Load and display magic }
procedure LoadMagic(Player: TCreature);
var
  Loop: Integer;
  MyListItem: TListItem;
  School: Char;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.LoadMagic()');

  try
    { Clear the list of spell schools }
    FormDisplay.ListViewAvailableSchools.Items.Clear;

    { Set up the keyboard shortcuts }
    School := 'a';

    { Iterate through each spell school }
    for Loop := SK_FIRE to SK_TRAVEL do
    begin
      { If the character has skill in a spell school }
      if Player.Skills[Loop] > 0 then
      begin
        { Add a spell school icon }
        MyListItem := FormDisplay.ListViewAvailableSchools.Items.Add;
        MyListItem.Caption := School + ')';
        MyListItem.SubItems.Add(GMagic[(Loop - SK_FIRE), 0]);
        MyListItem.SubItems.Add(IntToStr(Loop - SK_FIRE));
      end;

      { Set up the next keyboard shortcut }
      Inc(School);
    end;

    { Clear the school spells and descriptions }
    FormDisplay.ListViewSpellsKnown.Items.Clear;
    FormDisplay.MemoSpellDescription.Lines.Clear;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Display spells belonging to a school of magic }
procedure ShowSchoolSpells(Player: TCreature; School: String);
var
  Loop: Integer;
  MagicSchool: Integer;
  MyListItem: TListItem;
  SpellName: String;
  LetterSchool: Char;
  Temp: String;
  Value: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.ShowSchoolSpells()');

  try
    { Clear the list of spells }
    FormDisplay.ListViewSpellsKnown.Items.Clear;

    { Get the school }
    MagicSchool := StrToInt(School);

    { Set up the keyboard shortcuts }
    LetterSchool := '1';

    { Iterate through each spell in the school }
    for Loop := 1 to 8 do
    begin
      { Add a spell icon }
      SpellName := GMagic[MagicSchool, Loop];
      FormDisplay.ListViewSpellsKnown.ViewStyle := vsSmallIcon;
      MyListItem := FormDisplay.ListViewSpellsKnown.Items.Add;
      MyListItem.ImageIndex := Loop - 1;
      MyListItem.Caption := LetterSchool;
      MyListItem.SubItems.Add(SpellName);

      { Work out the cost}
      Value := Player.GetSpellCost(Player.Skills[SK_MAGIC],
        Player.Skills[MagicSchool + SK_FIRE], Loop, Player.Intelligence);                          ;
      MyListItem.SubItems.Add(IntToStr(Value));

      { Store it }
      GMagicCost[MagicSchool + SK_FIRE, Loop] := Value;

      { Work out thecasting probability }
      Value := Player.GetSpellProbability(Player.Skills[SK_MAGIC],
        Player.Skills[MagicSchool + SK_FIRE], Loop, Player.Intelligence);

      { Store it }
      GMagicProbability[MagicSchool + SK_FIRE, Loop] := Value;

      { Change the probability to friendly text }   
      if Value < 10 then
        Temp := 'Very Low'
      else if Value < 20 then
        Temp := 'Low'
      else if Value < 40 then
        Temp := 'Poor'
      else if Value < 60 then
        Temp := 'Iffy'
      else if Value < 80 then
        Temp := 'Fair'
      else if Value < 95 then
        Temp := 'Good'
      else Temp := 'Excellent';
      MyListItem.SubItems.Add(Temp);

      { Add extra items }
      MyListItem.SubItems.Add(IntToStr(Loop));

      { Reset the icon style to avoid cropping }
      FormDisplay.ListViewSpellsKnown.ViewStyle := vsReport;

      { Set up the next keyboard shortcut }
      Inc(LetterSchool);
    end;
  except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Try and draw a digital line between two points to check for uninterrupted
  vision }
function XCanSeeY(Dungeon: TDungeonLevel; x1:integer; y1:integer; x2:integer; 
	y2:integer):boolean;
var
  d, x, y, ax, ay, sx, sy, dx, dy:integer;
  temp, unabletosee:boolean;
begin
	{ Logging }
  // hLog.Add('{now} {lNum} UnitEngine.GetPathXToY()');

  { Default result }
  Result := False;
  
  try
		{ Initialise }
		temp := False;
		unabletosee := False;
		
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
			
			{ Note x and y here are the absolute dungeon co-ordinates }
			repeat
				{ Check for blockages }
				if Dungeon.walkable[x,y] = false then
					unabletosee := true;
				if Dungeon.terrain[x,y] = T_DOOR_CLOSED then
					unabletosee := true;
					
				{ break out if necessary }	
				if x = x2 then 
					break;
					
				{ Move to next square }
				if d > 0 then
				begin
					inc(y, sy);
					dec(d, ax);
				end;
				inc(x, sx);
				inc(d, ay);
				
				{ Keep going }
			until temp = true;
		end
		else
		begin
			{ y direction dominant }
			d:= ax - (ay div 2);
			repeat
				{ Note x and y here are the absolute dungeon co-ordinates }
			
				{ Check for blockages }
				if Dungeon.walkable[x,y] = false then
					unabletosee := true;
				if Dungeon.terrain[x,y] = T_DOOR_CLOSED then
					unabletosee := true;
				
				{ break out if necessary }
				if y = y2 then 
					break;
					
				{ Move to next square }	
				if d >= 0 then
				begin
					inc(x, sx);
					dec(d, ay);
				end;
				inc(y, sy);
				inc(d, ax);
				
				{ Keep going }
			until temp = true;
		end;
		
		{ Return the result }
		Result := not(unabletosee);


		{ Put a limit on the distance monsters can see, else the whole level would
		  be after our brave hero }
		if (Dist(Point(X1, Y1), Point(X2, Y2)) > 
			FormDisplay.Game.Player.Alertness + 3) and (Result)
		then 
			Result := False;
  	
  except	
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Find a path between two points }
function GetPathXToY(Dungeon: TDungeonLevel; x1:integer; y1:integer; 
	x2:integer; y2:integer; var xloc: Integer; var yloc: Integer):boolean;
begin
	{ Logging }
  // hLog.Add('{now} {lNum} UnitEngine.GetPathXToY()');

  { Default result }
  Result := False;
  
  try
  	{ Get the start point }
		FormDisplay.Game.StartCell := Point(x1, y1);
		
		{ Get the end point }
		FormDisplay.Game.EndCell := Point(x2, y2);
		
		{ Set up the Pathfinding component }
		with FormDisplay do
		begin
			{ Reset the path }
			SimplePathPlanner.Reset;
			
			{ Load the path points }
			SimplePathPlanner.SetStart(FormDisplay.Game.StartCell);
			SimplePathPlanner.SetEnd(FormDisplay.Game.EndCell);
			
			{ Find the first step on the path }
			SimplePathPlanner.FindPath(1);
			
			{ If no path found, return the current square }
			if SimplePathPlanner.Path.Count = 0 then
			begin
				xloc := x1;
				xloc := y1;
				Result := False;
			end
			else
			begin
				try
					{ Get the first step of the path }
					if SimplePathPlanner.Path.Count > 0 then
					begin
						xloc := SimplePathPlanner.Path[1].X;
						yloc := SimplePathPlanner.Path[1].Y;
						result := true;
					end
					else
					begin
						xloc := x1;
						xloc := y1;
						Result := true;
					end;
				except
					Result := False;
				end;
			end;
		end;
  except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Do townspeople AI }
procedure ProcessTownsPeople(Dungeon: TDungeonLevel);
var
  LocalMonster: TMonster;
  MonsterID: Integer;
  NPCTalk: String;
  NPCType: Integer;
  OffsetX: Integer;
  OffsetY: Integer;
  X: Integer;
  Y: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.ProcessTownsPeople()');

  try
    { Move every creature on the townspeople list }
    for x := 1 to DUNGEONSIZEX  do
		begin
			for y := 1 to DUNGEONSIZEY do
			begin
				{ Ignore none-walkable tiles }
				if Dungeon.Walkable[X, Y] = False then
					continue;

				{ Check if we have a monster }
				if ((Dungeon.Monsters[X, Y] > 0) and
          (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN)) then
        begin
					{ Get the Monster }
					MonsterID := Dungeon.Monsters[X, Y];
					LocalMonster := GTownsPeopleList[MonsterID] as TMonster;

          { Now, townspeople only do something every other turn }
          if OneChanceIn(2) then
          begin
            { Check if they say something or move in a random direction }
            if OneChanceIn(25) then
            begin
              if OneChanceIn(2) then
              begin
                { Say something }
                NPCType := LocalMonster.ID;
                if (NPCType < HOUSEKEEPER) then
                begin
                  NPCTalk := Format('The %s %s "%s"', [LocalMonster.Name,
                    NPCSpeech[NPCType], NPCSays[NPCType, Random(3) +1]]);
                  UpdateLog(NPCTalk, mesMonsterAction);
                end
                else
                begin
                  NPCTalk := Format('%s %s "%s"', [LocalMonster.Name,
                    NPCSpeech[NPCType], NPCSays[NPCType, Random(3) +1]]);
                  UpdateLog(NPCTalk, mesMonsterAction);
                end;
              end;
            end
            else
            begin
              { Move randomly if possible }
              if (GetRandomDirection(FormDisplay.Game.Dungeon, LocalMonster,
                FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, OffsetX,
                OffSetY)) then
              begin
                MoveXtoY(FormDisplay.Game.Dungeon, LocalMonster, LocalMonster.X
                  + OffsetX, LocalMonster.Y + OffSetY);
              end;
            end;
          end;
        end;
      end;
    end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Do monster AI }
procedure ProcessCreatures(Dungeon: TDungeonLevel);
var
  AttackLoop: Integer;
  AttackString: String;
  AttackResult: aResult;
  DamageCaused: Integer;
  DamageString: String;
  cdir, rdir: Integer;
  NumberOfActions: Integer;
  MonsterSpeed: Integer;
  LocalMonster: TMonster;
  Loop: Integer;
  NewX: Integer;
  NewY: Integer;
  SpeedIncrement: Double;
  DoNothing: Boolean;
  Offset: TPoint;
  CheckMonster: TMonster;
  MonsterHealth: String;
  HealthColour: TColor;
  Item: TItem;
  ProjectileType: tProjectile;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.ProcessCreatures()');

  try
    { If we're on the town level, do the townspeople as well }
    if Dungeon.LevelTheme = D_TOWN then
      ProcessTownsPeople(Dungeon);

		{ First, awaken any sleeping monsters on the level that can see the 
		  player }
		AwakenCreatures(GMonsterList, ActiveMonsterList, Dungeon,
			FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY);

		{ Then, before moving any monsters, we need to recalculate terrain costs }
		ReCalculateTerrainCosts(Dungeon, FormDisplay.Game.PlayerX,
			FormDisplay.Game.PlayerY);

		{ Now, process any active monsters }                      
		for Loop := 0 to ActiveMonsterList.Count - 1 do
		begin
			{ Add a check into make sure that we exit if the Player dies - this is to
			  stop deaths happening multiple times if the Player is attacked by 
			  multiple monsters }
			if FormDisplay.Game.Player.Dead then 
				Exit;

			{ Get each monster }
			LocalMonster := ActiveMonsterList[Loop] as TMonster;

			{ Only process monsters that are alive }
			if LocalMonster.Alive then
			begin
				{ Do a couple of aanity checks }
				if Dungeon.Monsters[LocalMonster.X, LocalMonster.Y] = 0 then
				begin
					{ If we fail the sanity check, kill and then remove the monster from
					  the map }
					LocalMonster.Alive := False;
					LocalMonster.Awake := False;
					LocalMonster.SpendEnergy(SET_NO_ENERGY);
					Continue;
				end;
				CheckMonster := 
					(GMonsterList.Items[Dungeon.Monsters[LocalMonster.X,
					LocalMonster.Y]])	as TMonster;
				if not(LocalMonster.Equals(CheckMonster)) then
				begin
					LocalMonster.Alive := False;
					LocalMonster.Awake := False;
					LocalMonster.SpendEnergy(SET_NO_ENERGY);
					Continue;
				end;

				{ Monster Health }
				if LocalMonster.GetMonsterHealthText(MonsterHealth, HealthColour)	then
					UpdateLogColor(MonsterHealth, HealthColour);

				{ Monster Speech/Actions }
				MonsterSpeaks(LocalMonster, Dungeon);

				{ Decide what to do for each monster }
				MonsterSpeed := (LocalMonster.Speed div 5) + 1;
				SpeedIncrement := (FormDisplay.Game.Player.Speed div 5) + 1;
				SpeedIncrement := FormDisplay.Game.Player.Speed * 
					(MonsterSpeed / SpeedIncrement);

        { Allow monsters to act always to stop a freeze occurring }
        if SpeedIncrement = 0 then
          SpeedIncrement := 1;

        { Get the number of attacks }
				NumberOfActions := LocalMonster.AttackNumber;

				{ Give each monster energy }
				LocalMonster.SpendEnergy(0 - Trunc(SpeedIncrement)); 
				
				{ Keep going whilst we have energy to spend or until we can't actually
				  do anything }
				repeat
					{ Flag to indicate if the monster actually does something }
					DoNothing:= False;

					{ If monster has enough energy, it can act }
					if LocalMonster.Energy >= 0 then
					begin
            { First, make sure that if the monster does something, the player
              character is no longer stealthed }
            if FormDisplay.Game.Player.Has(STEALTHED) then
            begin
              FormDisplay.Game.Player.Status[STEALTHED] := 0;
              UnitEngine.UpdateLog('You are no longer stealthed', mesStealth);

              { Refresh the display }
              FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
            end;

            { Handle pickups of items }
            if (LocalMonster.UseItems) and (Dungeon.Objects[LocalMonster.X,
					    LocalMonster.Y] > 0) and
              (not(IsAdjacent(Point(FormDisplay.Game.PlayerX,
              FormDisplay.Game.PlayerY),Point(LocalMonster.X, LocalMonster.Y))))
              then
            begin
              { First check if the creature is already wielding/wearing
                something }
              if (LocalMonster.Armour < 1) and (LocalMonster.Weapon < 1) then
              begin
                { Check if the item is a weapon or an armour }
                Item := (GItemList[Dungeon.Objects[LocalMonster.X,
                  LocalMonster.Y]] as TItem);
                if Item.ItemType = iWeapon then
                begin
                  { Pick the weapon up }
                  LocalMonster.Weapon := Dungeon.Objects[LocalMonster.X,
                    LocalMonster.Y];
                  Dungeon.Objects[LocalMonster.X, LocalMonster.Y] := -1;

                  { Handle stacks of items }
                  if Item.NextItem <> NO_NEXT_ITEM then
                  begin
                    Dungeon.Objects[LocalMonster.X, LocalMonster.Y] :=
                      Item.NextItem;
                    Item.NextItem := NO_NEXT_ITEM;
                  end;

                  { Alert the player }
                  UpdateLog(Trim(LocalMonster.SinglePrefix + LocalMonster.Name
                    + ' picks up ' + Lower(Item.Name)), mesMonComeIntoView );
                end;
                if Item.ItemType = iArmour then
                begin
                  { Pick the armour up }
                  LocalMonster.Armour := Dungeon.Objects[LocalMonster.X,
                    LocalMonster.Y];
                  Dungeon.Objects[LocalMonster.X, LocalMonster.Y] := -1;

                  { Handle stacks of items }
                  if Item.NextItem <> NO_NEXT_ITEM then
                  begin
                    Dungeon.Objects[LocalMonster.X, LocalMonster.Y] :=
                      Item.NextItem;
                    Item.NextItem := NO_NEXT_ITEM;
                  end;

                  { Alert the player }
                  UpdateLog(Trim(LocalMonster.SinglePrefix + LocalMonster.Name
                    + ' picks up ' + Lower(Item.Name)), mesMonComeIntoView );
                end;
              end;
            end;

						{ Handle melee attacks }
						if LocalMonster.AttackType = 'Melee' then
						begin
							{ move all non-adjacent melee monsters towards the player }
							if not(IsAdjacent(Point(FormDisplay.Game.PlayerX, 
								FormDisplay.Game.PlayerY), 
								Point(LocalMonster.X, LocalMonster.Y))) then  
							begin
								{ try and find a path to the player }
								if GetPathXToY(Dungeon, LocalMonster.X, LocalMonster.Y, 
									FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, NewX, 
									NewY) then
								begin
									{ try and move towards the player }
									if CanMoveXtoY(Dungeon, NewX, NewY, FormDisplay.Game.PlayerX,
										FormDisplay.Game.PlayerY) then
									begin
										{ Move towards the player }
										MoveXtoY(Dungeon, LocalMonster, NewX, NewY); 

										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
									end
									else
									begin
										{ Can't move directly towards the player, therefore we try
										  to move randomly }
										if GetRandomDirection(Dungeon, LocalMonster, 
											FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, 
											cdir, rdir) then 
										begin
											{ Move towards the player }
											MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + cdir, 
												LocalMonster.Y + rdir);

											{ And spend energy for doing something }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										end
										else
										begin 
											{ Can't move and not adjacent to the player thus do 
											  nothing for now }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
											DoNothing := True;
										end;
									end;
								end;
							end
							else
							begin 
								{ Attempt melee attacks }
								for AttackLoop := 1 to NumberOfActions do
								begin
									{ See if the monster can attack the player successfully }
									if XCanHitY(LocalMonster, FormDisplay.Game.Player, 
										AttackString, AttackResult) then
									begin
										{ On hit, work out the damage }
										DamageCaused := 
											XMeleeDamagesY(LocalMonster, FormDisplay.Game.Player, 
											DamageString, AttackResult);
											
										{ Update the message log }
										UpdateLog(AttackString + DamageString, mesMonCombatHit );
										
										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										
										{ Deal with the effects of the attack }
										FormDisplay.Game.Player.TakeDamage(LocalMonster, 
											DamageCaused);
											
										{ Check for death }
										if FormDisplay.Game.Player.HP < 1 then 
											FormDisplay.PlayerDie(LocalMonster);
									end
									else
									begin 
										{ On a miss, tell the player }
										UpdateLog(AttackString, mesMonCombatMiss);
										
										{ And spend energy for doing something }
										if AttackResult = iFumble then 											
											LocalMonster.SpendEnergy
												(FormDisplay.Game.Player.Speed *
                        FUMBLE_ENERGY_COST_FACTOR)
										else 
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
									end;
								end;
							end;
						end
						{ Handle ranged attacks }
						else if (LocalMonster.AttackType = 'Ranged') or
              (LocalMonster.AttackType = 'Breath') then
						begin
							{ Check for non-adjacent monsters to player }
							if not(IsAdjacent(Point(FormDisplay.Game.PlayerX, 
								FormDisplay.Game.PlayerY), 
								Point(LocalMonster.X, LocalMonster.Y))) then
							begin
								{ if the monster can't see the player }
								if not(XCanSeeY(Dungeon, LocalMonster.X, LocalMonster.Y, 
									FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)) then
								begin
									{ try and find a path to the player }
									if GetPathXToY(Dungeon, LocalMonster.X, LocalMonster.Y, 
										FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, NewX, 
										NewY) then
									begin
										{ try and move towards the player }
										if CanMoveXtoY(Dungeon, NewX, NewY, 
											FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY) then
										begin
											{ Move towards the player }
											MoveXtoY(Dungeon, LocalMonster, NewX, NewY);

											{ And spend energy for doing something }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										end
										else
										begin
											{ Can't move directly towards the player, therefore we 
											  try to move randomly }
											if GetRandomDirection(Dungeon, LocalMonster, 
												FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, 
												cdir, rdir) then
											begin
												{ Move randomly }
												MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + cdir, 
													LocalMonster.Y + rdir);

												{ And spend energy for doing something }
												LocalMonster.
													SpendEnergy(FormDisplay.Game.Player.Speed);
											end
											else
											begin
												{ Can't see the player and can't move towards so do 
													nothing }
												LocalMonster.
													SpendEnergy(FormDisplay.Game.Player.Speed); 
												DoNothing := True;
											end;
										end;
									end;
								end
								else
								begin
									{ A monster with ranged attacks can see the player but it is
									  out of range and we always want ranged attackers to be 
									  visible }
									if Dist(Point(LocalMonster.X, LocalMonster.Y), 
										Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)) > 
										FormDisplay.Game.Player.Alertness then
									begin
										{ Move towards the player }
										MoveXtoY(Dungeon, LocalMonster, NewX, NewY); 

										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed); 
									end
									else
									begin
										{ Attack the player with a projectile } 
										for AttackLoop := 1 to NumberOfActions do
										begin
											{ Animate the projectile }
											
											{ TODO: change this for each type of monster }
                      if LocalMonster.AttackType = 'Breath' then
                        ProjectileType := prBeam
                      else
                        ProjectileType := prMissile;
											AnimateMissileWeapon(Point(LocalMonster.X, 
												LocalMonster.Y), Point(FormDisplay.Game.PlayerX, 
												FormDisplay.Game.PlayerY), LocalMonster.ProjectileChar,
                        LocalMonster.ProjectileColour, ProjectileType);
												
											{ See if the monster can attack the player successfully }	
											if XCanHitY(LocalMonster, FormDisplay.Game.Player, 
												AttackString, AttackResult, iRange) then
											begin
												{ On hit, work out the damage }
												DamageCaused := XMeleeDamagesY(LocalMonster, 
													FormDisplay.Game.Player, DamageString, 
													AttackResult);
													
												{ Update the message log }
												UpdateLog(AttackString + DamageString, 
													mesMonCombatHit);

												{ And spend energy for doing something }
												LocalMonster.SpendEnergy
													(FormDisplay.Game.Player.Speed * 5);
													
												{ Deal with the effects of the attack }
												FormDisplay.Game.Player.TakeDamage(LocalMonster, 
													DamageCaused);
													
												{ Check for death }
												if FormDisplay.Game.Player.HP < 1 then 
													FormDisplay.PlayerDie(LocalMonster);
											end
											else
											begin
												{ On a miss, tell the player }
												UpdateLog(AttackString, mesMonCombatMiss);

												{ And spend energy for doing something }
												if AttackResult = iFumble then													
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed *
                            FUMBLE_ENERGY_COST_FACTOR *
                            MONSTER_ATTACK_ENERGY_COST_FACTOR)
												else 
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed *
                            MONSTER_ATTACK_ENERGY_COST_FACTOR);
											end;
										end;
									end;
								end;
							end
							else
							begin
								{ If we're standing next to the player, 70% chance of moving 
								  away or 30% chance of attacking in melee }
								if (random(PERCENTAGE) < RANGED_CHANCE_GF_MELEE) then
								begin
									{ Find direction from monster to player, reverse it, and 
									  pick an arbitrary point to aim for in that direction 
									  and then move towards it }
									Offset := FindOffsets(Point(LocalMonster.X, LocalMonster.Y), 
										Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY));
										
									{ See if we can move in that direction }
									if (CanMoveXtoY(Dungeon, LocalMonster.X + Offset.X, 
										LocalMonster.Y + Offset.Y)) then
									begin
										{ Move in that direction }
										MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + Offset.X, 
											LocalMonster.Y + Offset.Y);

										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
									end
									else
									begin 
										{ Can't move away so try and move randomly }
										if GetRandomDirection(Dungeon, LocalMonster, 
											FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, cdir, 
											rdir) then
										begin
											{ Move randomly }
											MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + cdir, 
												LocalMonster.Y + rdir);

											{ And spend energy for doing something }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										end
										else
										begin
											{ Can't find a direction to move so attack }
											for AttackLoop := 1 to NumberOfActions do
											begin
												{ See if the monster can attack the player 
												  successfully }
												if (XCanHitY(LocalMonster, FormDisplay.Game.Player, 
													AttackString, AttackResult)) then 
												begin 
													{ On hit, work out the damage }
													DamageCaused := XMeleeDamagesY(LocalMonster, 
														FormDisplay.Game.Player, DamageString, 
														AttackResult);
													
													{ Update the message log }
													UpdateLog(AttackString + DamageString, 
														mesMonCombatHit);
														
													{ And spend energy for doing something }
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed);
													
													{ Deal with the effects of the attack }
													FormDisplay.Game.Player.TakeDamage(LocalMonster, 
														DamageCaused);
														
													{ Check for death }
													if FormDisplay.Game.Player.HP < 1 then 
														FormDisplay.PlayerDie(LocalMonster);
												end
												else
												begin 
													{ On a miss, tell the player }
													UpdateLog(AttackString, mesMonCombatMiss);

													{ And spend energy for doing something }
													if (AttackResult = iFumble) then													
														LocalMonster.SpendEnergy
															(FormDisplay.Game.Player.Speed *
                              FUMBLE_ENERGY_COST_FACTOR)
													else 
														LocalMonster.SpendEnergy
															(FormDisplay.Game.Player.Speed);
												end;
											end;
										end;
									end;
								end
								else
								begin
									{ See if the monster can attack the player successfully }
									if (XCanHitY(LocalMonster, FormDisplay.Game.Player, 
										AttackString, AttackResult)) then
									begin 
										{ On hit, work out the damage }
										DamageCaused := XMeleeDamagesY(LocalMonster, 
											FormDisplay.Game.Player, DamageString, AttackResult);
											
										{ Update the message log }
										UpdateLog(AttackString + DamageString, mesMonCombatHit);
										
										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										
										{ Deal with the effects of the attack }
										FormDisplay.Game.Player.TakeDamage
											(LocalMonster, DamageCaused);
											
										{ Check for death }
										if (FormDisplay.Game.Player.HP < 1) then 
											FormDisplay.PlayerDie(LocalMonster);
									end
									else
									begin
										{ On a miss, tell the player }
										UpdateLog(AttackString, mesMonCombatMiss );
										
										{ And spend energy for doing something }
										if (AttackResult = iFumble) then
											LocalMonster.SpendEnergy
												(FormDisplay.Game.Player.Speed *
                        FUMBLE_ENERGY_COST_FACTOR)
										else
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										
									end;

								end;
							end;
						end
						{ Handle magic attacks }
						else if (LocalMonster.AttackType = 'Magic') then
						begin														
							{ TODO: choose spells to cast }
							
							{ Check for non-adjacent monsters to player }
							if not(IsAdjacent(Point(FormDisplay.Game.PlayerX, 
								FormDisplay.Game.PlayerY), Point(LocalMonster.X, 
								LocalMonster.Y))) then
							begin
								{ if the monster can't see the player }
								if not(XCanSeeY(Dungeon, LocalMonster.X, LocalMonster.Y, 
									FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)) then
								begin
									{ try and find a path to the player }
									if GetPathXToY(Dungeon, LocalMonster.X, LocalMonster.Y, 
										FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, NewX, 
										NewY) then
									begin
										{ try and move towards the player }
										if CanMoveXtoY(Dungeon, NewX, NewY, 
											FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY) then
										begin
											{ Move towards the player }
											MoveXtoY(Dungeon, LocalMonster, NewX, NewY); 

											{ And spend energy for doing something }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed); 
										end
										else
										begin
											{ Can't move directly towards the player, therefore we 
											  try to move randomly }
											if (GetRandomDirection(Dungeon, LocalMonster, 
												FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, cdir, 
												rdir)) then 
											begin
												{ Move randomly }
												MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + cdir, 
													LocalMonster.Y + rdir);
													
												{ And spend energy for doing something }
												LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed); 
											end
											else
											begin
												{ Can't see the player and can't move towards so do 
													nothing }
												LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
												DoNothing := True;
											end;
										end;
									end;
								end
								else
								begin 
									{ A monster with magic attacks can see the player but it is
										out of range and we always want ranged attackers to be 
									  visible }
									if Dist(Point(LocalMonster.X, LocalMonster.Y), 
										Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY)) > 
										FormDisplay.Game.Player.Alertness then
									begin
										{ Move towards the player }
										MoveXtoY(Dungeon, LocalMonster, NewX, NewY);

										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										DoNothing := False;
									end
									else
									begin
										{ Attack the player with a spell }
										for AttackLoop := 1 to NumberOfActions do
										begin
											{ Animate the projectile }
											
											{ TODO: change this for each type of monster }
											AnimateMissileWeapon(Point(LocalMonster.X, 
											  LocalMonster.Y), Point(FormDisplay.Game.PlayerX, 
												FormDisplay.Game.PlayerY), LocalMonster.ProjectileChar,
                        LocalMonster.ProjectileColour);
												
											{ See if the monster can attack the player successfully }
											if XCanHitY(LocalMonster, FormDisplay.Game.Player,
												AttackString, AttackResult, iMagic) then
											begin 
												{ On hit, work out the damage }
												DamageCaused := XMagicDamagesY(LocalMonster, 
													FormDisplay.Game.Player, DamageString, AttackResult, 
													0);
												
												{ TODO: use a spell instead }
												
												{ Update the message log }
												UpdateLog(AttackString + DamageString, 
													mesMonCombatHit);
												
												{ And spend energy for doing something }
												LocalMonster.SpendEnergy
													(FormDisplay.Game.Player.Speed * 5);
													
												{ Deal with the effects of the attack }	
												FormDisplay.Game.Player.TakeDamage(LocalMonster, 
													DamageCaused);
													
												{ Check for death }
												if (FormDisplay.Game.Player.HP < 1) then 
													FormDisplay.PlayerDie(LocalMonster);
											end
											else
											begin
												{ On a miss, tell the player }
												UpdateLog(AttackString, mesMonCombatMiss);
												
												{ And spend energy for doing something }
												if (AttackResult = iFumble) then 
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed *
                            FUMBLE_ENERGY_COST_FACTOR *
                            MONSTER_ATTACK_ENERGY_COST_FACTOR)
												else 
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed *
                            MONSTER_ATTACK_ENERGY_COST_FACTOR);
											end;
										end;
									end;
								end;
							end
							else
							begin
								{ If we're standing next to the player, 70% chance of moving 
								  away or 30% chance of attacking in melee }
                if (random(PERCENTAGE) < RANGED_CHANCE_GF_MELEE) then
								begin
									{ Find direction from monster to player, reverse it, and 
										pick an arbitrary point to aim for in that direction 
									  and then move towards it }
									Offset := FindOffsets(Point(LocalMonster.X, LocalMonster.Y), 
										Point(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY));
										
									{ See if we can move in that direction }	
									if CanMoveXtoY(Dungeon, LocalMonster.X + Offset.X, 
										LocalMonster.Y + Offset.Y) then
									begin
										{ Move in that direction }
										MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + Offset.X, 
											LocalMonster.Y + Offset.Y);
											
										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
									end
									else
									begin 
										{ Can't move away so try and move randomly }
										if GetRandomDirection(Dungeon, LocalMonster, 
											FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY, cdir, 
											rdir) then
										begin
											{ Move randomly }
											MoveXtoY(Dungeon, LocalMonster, LocalMonster.X + cdir, 
												LocalMonster.Y + rdir);

											{ And spend energy for doing something }
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);  
										end
										else
										begin
											{ Can't find a direction to move so attack }
											for AttackLoop := 1 to NumberOfActions do
											begin
												{ See if the monster can attack the player 
												  successfully }											
												if XCanHitY(LocalMonster, FormDisplay.Game.Player, 
													AttackString, AttackResult) then
												begin
													{ On hit, work out the damage }
													DamageCaused := XMeleeDamagesY(LocalMonster, 
														FormDisplay.Game.Player, DamageString, 
														AttackResult);
														
													{ Update the message log }
													UpdateLog(AttackString + DamageString, 
														mesMonCombatHit);
														
													{ And spend energy for doing something }	
													LocalMonster.SpendEnergy
														(FormDisplay.Game.Player.Speed);
													
													{ Deal with the effects of the attack }
													FormDisplay.Game.Player.TakeDamage(LocalMonster, 
														DamageCaused);
														
													{ Check for death }	
													if (FormDisplay.Game.Player.HP < 1) then 
														FormDisplay.PlayerDie(LocalMonster);
												end
												else
												begin
													{ On a miss, tell the player }
													UpdateLog(AttackString, mesMonCombatMiss);
													
													{ And spend energy for doing something }
													if (AttackResult = iFumble) then 
														LocalMonster.SpendEnergy
															(FormDisplay.Game.Player.Speed *
                              FUMBLE_ENERGY_COST_FACTOR)
													else 
														LocalMonster.SpendEnergy
															(FormDisplay.Game.Player.Speed);
												end;
											end;
										end;
									end;
								end
								else
								begin
									{ See if the monster can attack the player successfully }
									if XCanHitY(LocalMonster, FormDisplay.Game.Player, 
										AttackString, AttackResult) then
									begin 
										{ On hit, work out the damage }
										DamageCaused := XMeleeDamagesY(LocalMonster, 
											FormDisplay.Game.Player, DamageString, AttackResult);
											
										{ Update the message log }
										UpdateLog(AttackString + DamageString, mesMonCombatHit);
										
										{ And spend energy for doing something }
										LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
										
										{ Deal with the effects of the attack }
										FormDisplay.Game.Player.TakeDamage(LocalMonster, 
											DamageCaused);
										
										{ Check for death }
										if FormDisplay.Game.Player.HP < 1 then 
											FormDisplay.PlayerDie(LocalMonster);
									end
									else
									begin 
										{ On a miss, tell the player }
										UpdateLog(AttackString, mesMonCombatMiss);
										
										{ And spend energy for doing something }
										if (AttackResult = iFumble) then 
											LocalMonster.SpendEnergy
												(FormDisplay.Game.Player.Speed *
                        FUMBLE_ENERGY_COST_FACTOR)
										else 
											LocalMonster.SpendEnergy(FormDisplay.Game.Player.Speed);
									end;
								end;
							end;
						end;
					end;
				until (LocalMonster.Energy < 0) or (DoNothing);
				
				{ If the monster hasn't done anything, then spend energy anyway to 
				  prevent queueing of monster actions }
				if DoNothing then 
					LocalMonster.SpendEnergy(Trunc(SpeedIncrement));
			end;
		end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if a move is permitted }
function CanMoveXtoY(Dungeon: TDungeonLevel; newx: integer; newy: integer; 
	playerx: integer; playery: integer): Boolean;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.CanMoveXtoY()');

  { Default result }
  Result := False;
  
  try
  	{ Check for walkable square without the player }
		Result := ((Dungeon.Walkable[NewX, NewY] = true) and
      (Dungeon.Monsters[NewX, NewY] = 0) and (not((NewX = PlayerX)
      and (NewY = PlayerY))));
 	except	
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if a move is permitted }
function CanMoveXtoY(Dungeon: TDungeonLevel; newx: integer; 
	newy: integer): Boolean;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.CanMoveXtoY()');

  { Default result }
  Result := False;
  
  try
    { Check for walkable square without monsters }
		Result := ((Dungeon.Walkable[NewX, NewY] = true) and 
			(Dungeon.Monsters[NewX, NewY] = 0));
 	except	
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Move one square towards a specific direction }
function MoveXtoY(Dungeon: TDungeonLevel; Monster: TMonster; newx: integer; 
	newy: Integer): Boolean;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.MoveXtoY()');

  { Default result }
  Result := False;
  
  try
  	{ Check for being inbounds }
    if (NewX > 1) and (NewX <= DUNGEONSIZEX) and (NewY > 1) or 
    	(NewY <= DUNGEONSIZEY) then
    begin
    	{ Move the creature and set terrain costs }
			Dungeon.Monsters[NewX, NewY] := Dungeon.Monsters[Monster.X, Monster.Y];
			Dungeon.TerrainCost[NewX, NewY] := TC_MONSTER;
			Dungeon.Monsters[Monster.X, Monster.Y] := 0;
			Dungeon.TerrainCost[Monster.X, Monster.Y] := TC_NONE;
			Monster.X := NewX;
			Monster.Y := NewY;
			Result := True;
		end;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random direction }
function GetRandomDirection(Dungeon: TDungeonLevel; Monster: TMonster; 
	PlayerX: Integer; PlayerY: Integer; var OffSetX: Integer; 
	var OffSetY: Integer): Boolean;
var
  Direction: Integer;
  Paranoia: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.GetRandomDirection()');

  { Default result }
  Result := False;
  
  try
  	{ Set up a paranoia counter }
		Paranoia := 0;
		repeat
		 	{ Get a random direction }			
			Direction := Random(4);
			
			{ Get x and y offsets for that random direction }
			OffSetX := ddx_ddd[direction];
			OffSetY := ddy_ddd[direction];
			inc(Paranoia);
			{ Keep going until we can find an empty square or until we hit our
			  paranoia counter }
		until ((Dungeon.Walkable[Monster.X + OffSetX, Monster.Y + OffSetY] = true)
		  and (Dungeon.Monsters[Monster.X + OffSetX, Monster.Y + OffSetY] = 0) and
			(not((Monster.X + OffSetX = PlayerX) and (Monster.Y + OffSetY = 
			PlayerY)))) or (Paranoia >= 20);
			
		{ Return the success of our search }
		Result := Paranoia < 20;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Recalculate terrain costs for pathing }
procedure ReCalculateTerrainCosts(Dungeon: TDungeonLevel; PlayerX: 
	Integer; PlayerY: Integer);
var
  X: Integer;
  Y: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.ReCalculateTerrainCosts()');

	try
		{ Recalculate terrain costs across the entire dungeon }
		
		{ TODO Optimise this }
		for x := 1 to DUNGEONSIZEX  do
		begin
			for y := 1 to DUNGEONSIZEY do
			begin
				{ Walkable squares are all low cost apart from monster-occupied 
					squares }
				if Dungeon.Walkable[X, Y] = True then
				begin
					Dungeon.TerrainCost[X, Y] := TC_PASSABLE;
					
					{ Deal with monster-occupied squares }
					if Dungeon.Monsters[X, Y] > 0 then
						Dungeon.TerrainCost[X, Y] := TC_MONSTER;

          { Item squares are attractive }
          if Dungeon.Objects[X, Y] > 0 then
            Dungeon.TerrainCost[X, Y] := TC_ITEM;
				end
				else
				begin
					Dungeon.TerrainCost[X, Y] := TC_IMPASSABLE;
				end;
			end;
		end;
		
		{ The player's square is impassable }
		Dungeon.TerrainCost[PlayerX, PlayerY] := TC_IMPASSABLE;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Awaken nearby creatures }
procedure AwakenCreatures(SourceList: TObjectlist; ActiveList: 
	TObjectList; Dungeon: TDungeonLevel; PlayerX: Integer; PlayerY: Integer);
var
  X: Integer;
  Y: Integer;
  LocalMonster: TMonster;
  MonsterID: Integer;
  Item: TItem;
  StealthFlag: String;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.AwakenCreatures()');

	try
		{ TODO: optimise by only looking within a certain number of squares }
		
		{ Iterate through the dungeon looking for monsters to wake up }
		for x := 1 to DUNGEONSIZEX  do
		begin
			for y := 1 to DUNGEONSIZEY do
			begin
				{ Ignore none-walkable tiles }
				if Dungeon.Walkable[X, Y] = False then
					continue;

				{ Check if we have a monster }
				if ((Dungeon.Monsters[X, Y] > 0) and
          (FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN)) then
        begin
					{ Get the Monster }
					MonsterID := Dungeon.Monsters[X, Y];
					LocalMonster := SourceList[MonsterID] as TMonster;

					{ Only alive monsters can wake up }
					if LocalMonster.Alive then
					begin
						{ Check for line-of-sight }
						if (XCanSeeY(Dungeon, X, Y, PlayerX, PlayerY)) and 
							(Dungeon.Visible[X, Y] = 1)  then
						begin
              { Stealth check }
              if FormDisplay.Game.Player.Has(STEALTHED) then
                StealthFlag := ' sees you!'
              else
                StealthFlag := ' appears!';

							{ Check for newly discovered monsters }
							if not(LocalMonster.BeenSeen) then          
							begin
								{ Now handle awakening messages }
								if LocalMonster.Armour > 0 then
								begin
									{ The monster is wearing some armour }
									Item := (GItemList[LocalMonster.Armour] as TItem);
									UpdateLog(Trim(LocalMonster.SinglePrefix + LocalMonster.Name +
                     StealthFlag + ' It is wearing ' + Lower(Item.Name)),
                    mesMonComeIntoView );
								end
								else if LocalMonster.Weapon > 0 then
								begin
									{ The monster is carrying a weapon }
									Item := (GItemList[LocalMonster.Weapon] as TItem);
									UpdateLog(Trim(LocalMonster.SinglePrefix + LocalMonster.Name +
			    					 StealthFlag + ' It is wielding ' + Lower(Item.Name)),
                     mesMonComeIntoView);
								end
								else
								begin
									{ It has not got weapon or armour }
									UpdateLog(Trim(LocalMonster.SinglePrefix + LocalMonster.Name
										+ StealthFlag), mesMonComeIntoView);
								end;

								{ If the monster is a unique, add a discovery note }
								if LocalMonster.UniqueName <> '' then
								begin
									FormDisplay.Game.Player.TakeNote
										(FormDisplay.Game.Turns, LocalMonster.UniqueName, nNotice, 
										Dungeon);
								end;

								{ Make the monster has having been seen }
								LocalMonster.BeenSeen := True;
							end;

							{ Now wake the monster up }
							if (not(LocalMonster.Awake)) and (LocalMonster.Alive) and
								(CanDetect(LocalMonster, FormDisplay.Game.Player)) then
							begin
								{ Add the monster to the currently active monster list }
								if Dungeon.Visible[X, Y] = 1 then
								begin
									LocalMonster.Awake := True;
									ActiveList.Add(LocalMonster);

                  { Also force the character out of stealth }
                  if FormDisplay.Game.Player.Status[STEALTHED] <> 0 then
                  begin
                    FormDisplay.Game.Player.Status[STEALTHED] := 0;
                    UnitEngine.UpdateLog('You are no longer stealthed',
                      mesStealth);
                  end;
								end
								else
								begin
									{ Do some speech for monsters that the player cannot see }
									LocalMonster.Awake := True;
									ActiveList.Add(LocalMonster);
									MonsterSpeaks(LocalMonster, Dungeon, False);
								end;
							end
              else
              begin
                { Handle the case where the player can see the monster but the
                  monster can't - this is caused by the player using recursive
                  shadowcasting for his/her view but the monster just using
                  digital lines }
                if (not(LocalMonster.Awake)) and (LocalMonster.Alive) and
                  (Dungeon.Visible[X, Y] = 1) then
                begin
									LocalMonster.Awake := True;
									ActiveList.Add(LocalMonster);
								end;
              end;
						end;
					end;
				end;
			end;
		end;
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Melee attack rolls }
function XCanHitY(LocalMonster: TMonster; LocalPlayer: TCreature; 
	var Attack: String; var AttackResult: aResult; AttackType: aType = iMelee):
  Boolean;
var
  AttackRoll: Integer;
  HeavyArmour: Integer;
  LightArmour: Integer;
  MediumArmour: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XCanHitY()');

  { Default result }
  Result := False;
  
  try
  	{ Roll a d20 }
		AttackRoll := Random(20) + 1;
		
		{ Deal with critical hits or misses }
		case AttackRoll of
			1:  begin
						{ A 1 is always a miss }
						AttackResult := iFumble;

            { Deal with different attack types }
            case AttackType of
              iMelee: Attack := Format(' %s you but misses badly',
                [LocalMonster.Verb]);
              iRange: Attack := Format(' %s you but misses badly',
                [LocalMonster.RangedVerb]);
              iMagic: Attack := ' miscasts';
            end;

						Result := False; 
					end;
			{ 20: begin
						{ A 20 is always a hit }
						{ AttackResult := iCritical; }

            { Deal with different attack types }
            { case AttackType of
              iMelee: Attack := Format(' really %s you!', [LocalMonster.Verb]);
              iRange: Attack := Format(' really %s you!',
                [LocalMonster.RangedVerb]);
              iMagic: Attack := ' almost knocks you off your feet';
            end;
            
						Result := True; 
					end; }
			else
			begin
				{ Just a normal hit }
				AttackResult := iNormal;
				
				{ Add attack bonus }
				Inc(AttackRoll, LocalMonster.AttackBonus);

				{ TODO: Check for items worn or wielded that increase the attack }

				{ Check for a hit }
				Result := AttackRoll >= LocalPlayer.EV;
				
				{ Check for blocking and deflection }
				if Result then
				begin
					{ Handle deflection }
					Result := (Result and (Random(PERCENTAGE) > LocalPlayer.Deflection)); 
					
					{ Attacks which aren't deflected can be blocked }
					if Result then
					begin
						Result := (Result and (Random(PERCENTAGE) > LocalPlayer.Blocking));
						
						{ Build the attack string for successful attacks} 
						if Result then
            begin
              case AttackType of
                iMelee: Attack := Format(' %s you', [LocalMonster.Verb]);
                iRange: Attack := Format(' %s you', [LocalMonster.RangedVerb]);
                iMagic: Attack := Format(' %s you', [LocalMonster.RangedVerb]);
              end;
            end
						else
            begin
							{ Build the attack string for blocked attacks}
              case AttackType of
                iMelee: Attack := Format(' %s you but you block its attack',
                  [LocalMonster.Verb]);
                iRange: Attack := Format(' %s you but you block its attack',
                  [LocalMonster.RangedVerb]);
                iMagic: Attack := Format(' %s you but you block its attack',
                  [LocalMonster.RangedVerb]);
              end;
            end;
					end
					else
          begin
						{ Build the attack string for deflected attacks }
            case AttackType of
              iMelee: Attack := Format(' %s you but you deflect its attack',
                [LocalMonster.Verb]);
              iRange: Attack := Format(' %s you but you deflect its attack',
                [LocalMonster.RangedVerb]);
              iMagic: Attack := Format(' %s you but you deflect its attack',
                [LocalMonster.RangedVerb]);
            end;
          end;
				end
				else
        begin
					{ Build the attack string for missed attacks }
          case AttackType of
            iMelee: Attack := Format(' %s you but misses',
              [LocalMonster.Verb]);
            iRange: Attack := Format(' %s you but misses',
              [LocalMonster.RangedVerb]);
            iMagic: Attack := Format(' %s you but misses',
              [LocalMonster.RangedVerb]);
          end;
        end;
			end;
		end;
		
		{ Substitute in the attack string }
		Attack := Trim(LocalMonster.Prefix + LocalMonster.Name + Attack);
		
		{ Handle skill learning }
    if FormDisplay.Game.Player.CClass = cWarrior then
    begin
      if OneChanceIn(2) then
			  FormDisplay.Game.Player.LearnSkill(SK_DEFENSE);
    end
    else
    begin
		  if OneChanceIn(5) then
			  FormDisplay.Game.Player.LearnSkill(SK_DEFENSE);
    end;

    { If the character isn't wearing any armour then give a bonus to defense
      learning by way of compensation }
    FormDisplay.Game.Player.ReturnArmourRatio(HeavyArmour, MediumArmour,
      LightArmour);
    if HeavyArmour + MediumArmour + LightArmour = 0 then
      FormDisplay.Game.Player.LearnSkill(SK_DEFENSE, Random(2) + 1);
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Melee attack rolls }
function XCanHitY(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Attack: String; var AttackResult: aResult): Boolean;
var
  AttackRoll: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XCanHitY()');

  { Default result }
  Result := False;
  
  try
  	{ Roll a d20 }
		AttackRoll := Random(20) + 1;

		{ Deal with critical hits or misses }
		case AttackRoll of
			1:  begin
						{ A 1 is always a miss }
						AttackResult := iFumble;
						Attack := ' miss badly ';
						Result := False;
					end;
			20: begin
						{ A 20 is always a hit }
						AttackResult := iCritical;
						Attack := ' critically hit ';
						Result := True;
					end;
			else
			begin
				{ Just a normal hit }
				AttackResult := iNormal;
				
				{ Take into account fighting skill }
				Inc(AttackRoll, LocalPlayer.Skills[SK_FIGHTING] div 2);
				
				{ Handle skill learning }
				if OneChanceIn(3) then
					LocalPlayer.LearnSkill(SK_FIGHTING);
					
				{ Deal with armed/unarmed skills and bonuses }
				if (LocalPlayer.Inventory[S_MAINHAND] > 0) then
				begin
					Inc(AttackRoll, LocaLPlayer.Skills[SK_MELEE] div 2);
					LocalPlayer.LearnSkill(SK_MELEE);
				end
				else
				begin
					Inc(AttackRoll, LocaLPlayer.Skills[SK_UNARMED] div 2);
					LocalPlayer.LearnSkill(SK_UNARMED);
				end;
				
				{ Take into account any accuracy bonuses }
				Inc(AttackRoll, LocalPlayer.Accuracy div 2);
				
				{ Check for a hit }
				Result := AttackRoll >= LocalMonster.EV;
				
				{ Build the attack string }
				if Result then
					Attack := ' hit '
				else 
					Attack := ' attack but miss ';
			end;
		end;
		
		{ Substitute in the attack string }
		Attack := 'You' + Attack + Trim(LocalMonster.Prefix + LocalMonster.Name);

		{ Add hunger for attacking }
		FormDisplay.Game.Player.Feed(-5);
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Ranged Attack Rolls }
function XCanHitYRanged(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Attack: String; var AttackResult: aResult): Boolean; overload;
var
  AttackRoll: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XCanHitYRanged()');

  { Default result }
  Result := False;
  
  try
		{ Roll a d20 }
		AttackRoll := Random(20) + 1;

		{ Deal with critical hits or misses }
		case AttackRoll of
			1:  begin
						{ A 1 is always a miss }
						AttackResult := iFumble;
						Attack := ' miss badly ';         
						Result := False;
					end;
			20: begin
						{ A 20 is always a hit }
						AttackResult := iCritical;
						Attack := ' critically hit ';          
						Result := True;
					end;
			else
			begin
				{ Just a normal hit }
				AttackResult := iNormal;
				
				{ Take into account fighting skill }
				Inc(AttackRoll, LocalPlayer.Skills[SK_FIGHTING] div 2);
				
				{ Take into account ranged fighting skill }
				Inc(AttackRoll, LocaLPlayer.Skills[SK_RANGED] div 2);
				
				{ Handle skill learning }
				if OneChanceIn(3) then
					LocalPlayer.LearnSkill(SK_FIGHTING);				
				LocalPlayer.LearnSkill(SK_RANGED, Random(3) + 3);
				
				{ Take into account any accuracy bonuses }
				Inc(AttackRoll, LocalPlayer.Accuracy div 2);
				
				{ Check for a hit }
				Result := AttackRoll >= LocalMonster.EV;
				
				{ Built the attack string }
				if Result then
					Attack := ' hit '
				else 
					Attack := ' attack but miss ';
			end;
		end;
		
		{ Substitute in the attack string }
		Attack := 'You' + Attack + Trim(LocalMonster.Prefix + LocalMonster.Name);

		{ Add hunger for attacking, though not as much as for melee attacks }
		FormDisplay.Game.Player.Feed(-3);
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle player missile attacks }
function XMissileDamagesY(LocalPlayer: TCreature; LocalMonster: TMonster; 
	var Damage: String; AttackResult: aResult): Integer; overload;
var
  Absorption: Real;
  DamageInflicted: Real;
  WeaponBaseDamage: Integer;
  WeaponBonusDamage: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XMissileDamagesY()');

  { Default result }
  Result := 0;
  
  try
		{ Calculate the proportion of damage absorbed by armour }
		Absorption := (LocalMonster.AC / (ARMOURCAPACITYCAP * 4));

		{ Get ranged weapon damage }
		WeaponBaseDamage := 
			(GItemList.Items[LocalPlayer.Inventory[S_RANGED]] as TItem).Damage;
		WeaponBonusDamage := 
			(GItemList.Items[LocalPlayer.Inventory[S_RANGED]] as TItem).BonusDamage;

		{ Handle critical attacks }
		if AttackResult = iCritical then 
			DamageInflicted := WeaponBaseDamage * 2 + WeaponBonusDamage + 
				LocalPlayer.DamageBonus
		else 
			DamageInflicted := Random(WeaponBaseDamage) + 1 + WeaponBonusDamage + 
				LocalPlayer.DamageBonus;

		{ Deal with any racial bonuses to damage }
		DamageInflicted := DamageInflicted + 
			LocalPlayer.GetBonusDamage(LocalMonster.Race);

		{ Reduce the damage for armour }
		DamageInflicted := DamageInflicted * (1 - Absorption);
		
		{ Round the damage down }
		Result := Trunc(DamageInflicted);

    { Check for any error conditions that may occur and correct them }
    if Result < 0 then
      Result := 0;
		
		{ Built the attack string }
		if Result = 0 then 
			Damage := ' but its armour absorbs the blow'
		else 
			Damage := '';
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle player physical attacks }
function XMeleeDamagesY(LocalPlayer: TCreature; LocalMonster: TMonster;
	var Damage: String; AttackResult: aResult): Integer; overload;
var
  Absorption: Real;
  DamageInflicted: Real;
  WeaponBaseDamage: Integer;
  WeaponBonusDamage: Integer;
  SneakAttackMultiplier: Integer;
  SneakAttack: Boolean;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XMeleeDamagesY()');

  { Default result }
  Result := 0;
  
  try
  	{ Calculate the proportion of damage absorbed by armour }
		Absorption := (LocalMonster.AC / (ARMOURCAPACITYCAP * 4));

		{ Deal with armed or unarmed attacks }
		if LocalPlayer.Inventory[S_MAINHAND] > 0 then
		begin
			{ Calculate weapon damage }
			WeaponBaseDamage := 
				(GItemList.Items[LocalPlayer.Inventory[S_MAINHAND]] as TItem).Damage;
			WeaponBonusDamage := 
				(GItemList.Items[LocalPlayer.Inventory[S_MAINHAND]]
				as TItem).BonusDamage;
		end
		else
		begin
			{ Unarmed damage is fixed, but is modified by the unarmed skill }
			WeaponBaseDamage := 6;
			WeaponBonusDamage := LocalPlayer.Skills[SK_UNARMED];
		end;
		
		{ Handle critical attacks }
		if AttackResult = iCritical then 
			DamageInflicted := WeaponBaseDamage * 2 + WeaponBonusDamage + 
				LocalPlayer.DamageBonus
		else 
			DamageInflicted := Random(WeaponBaseDamage) + 1 + WeaponBonusDamage + 
			LocalPlayer.DamageBonus;

		{ Deal with any racial bonuses to damage }
		DamageInflicted := DamageInflicted + 
			LocalPlayer.GetBonusDamage(LocalMonster.Race);

		{ Reduce the damage for armour }
		DamageInflicted := DamageInflicted * (1 - Absorption);

    { Apply bonuses for sneak attacks }
    if not(LocalMonster.Awake) then
    begin
      { This is a sneak attack }
      SneakAttack := True;

      { Base damage for sneak attacks is double }
      SneakAttackMultiplier := 2;

      { Thieves get an added bonus to damage - for every 3 ranks in
        the subterfuge skill, add extra damage }
      if FormDisplay.Game.Player.CClass = cThief then
        Inc(SneakAttackMultiplier,
          FormDisplay.Game.Player.Skills[SK_SUBTERFUGE] div 3);

      { Work out the new damage }
      DamageInflicted := DamageInflicted * SneakAttackMultiplier;

      { Learn skills for doing this successfully }
      LocalPlayer.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
    end
    else
      { This is not a sneak attack }
      SneakAttack := False;
		
		{ Round the damage down }
		Result := Trunc(DamageInflicted);

    { Check for any error conditions that may occur and correct them }
    if Result < 0 then
      Result := 0;
		
		{ Built the attack string }

    { Check for sneak attacks first }
    if SneakAttack then
      Damage := ' sneakily';

		if Result = 0 then 
			Damage := Damage + ' but its armour absorbs the blow'
		else 
			Damage := Damage + '';
	except	
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle physical attacks from Monsters }
function XMeleeDamagesY(LocalMonster: TMonster; LocalPlayer: TCreature;
	var Damage: String; AttackResult: aResult): Integer; overload;
var
  Absorption: Real;
  DamageInflicted: Real;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XMeleeDamagesY()');

  { Default result }
  Result := 0;
  
  try
  	{ TODO: deal with Weapon Damage from picked up weapons at some point }
  	
  	{ Calculate the proportion of damage absorbed by armour }
		Absorption := LocalPlayer.AC / (ARMOURCAPACITYCAP * 12);
		
		{ Handle critical attacks }
		{if AttackResult = iCritical then
			DamageInflicted := LocalMonster.AttackDamage + 
				Random(LocalMonster.AttackDamage) + 1
		else  }
    DamageInflicted := Random(LocalMonster.AttackDamage) + 1;
		 	
		{ Reduce the damage for armour }
		DamageInflicted := DamageInflicted * (1 - Absorption);

		{ Never one-shot the player }
		if DamageInflicted >= LocalPlayer.MaxHP then
			DamageInflicted := LocalPlayer.MaxHP - 1;

		{ Round the damage down }
		Result := Trunc(DamageInflicted);

    { Check for any error conditions that may occur and correct them }
    if Result < 0 then
      Result := 0;

		{ Built the attack string }
		if Result = 0 then 
			Damage := ' but your armour absorbs the blow'
		else 
			Damage := '';

    { Deal with poisons }
    if XPoisonsY(LocalMonster, LocalPlayer) then
      UpdateLog('You are poisoned!', mesMonCombatHit);
 	except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle Magic Attacks from Monsters }
function XMagicDamagesY(LocalMonster: TMonster; LocalPlayer: TCreature; 
	var Damage: String; AttackResult: aResult; MagicSchool: Integer): Integer; 
	overload;
var
  Absorption: Real;
  DamageInflicted: Real;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XMagicDamagesY()');

  { Default result }
  Result := 0;
  
  try
  	{ Take into account any base resistance }
		Absorption := LocalPlayer.Resistance;
		
		{ Add any specific resistance }
		case MagicSchool of
			SCH_FIRE: Absorption := Absorption + LocalPlayer.FireResistance;
			SCH_AIR: Absorption := Absorption + LocalPlayer.AirResistance;
			SCH_WATER: Absorption := Absorption + LocalPlayer.WaterResistance;
			SCH_EARTH: Absorption := Absorption + LocalPlayer.EarthResistance;
			SCH_NECROMANCY: Absorption := Absorption + 
				LocalPlayer.LifeDrainingResistance;
		end;
		
		{ Work out the amount of absorption }
		Absorption := Absorption / 100;

		{ Handle Critical Attacks }
		{if AttackResult = iCritical then
			DamageInflicted := LocalMonster.AttackDamage + 
				Random(LocalMonster.AttackDamage) + 1
		 else }
    DamageInflicted := Random(LocalMonster.AttackDamage) + 1;
		 	
		{ Take off resistance }
		DamageInflicted := DamageInflicted * (1 - Absorption);

		{ Never one-shot the player }
		if DamageInflicted >= LocalPlayer.MaxHP then
			DamageInflicted := LocalPlayer.MaxHP - 1;

		{ Round the damage down }
		Result := Trunc(DamageInflicted);

    { Check for any error conditions that may occur and correct them }
    if Result < 0 then
     Result := 0;
		
		{ Built the attack string }
		if Result = 0 then
			Damage := ' but you resist'
		else 
			Damage := '';
	except	
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;		
end;

{ Handle stealth and hiding }
function CanDetect(LocalMonster: TMonster; LocalPlayer: TCreature): Boolean;
var
  MonsterRoll: Integer;
  PlayerRoll: Integer;
  HeavyArmour: Integer;
  MediumArmour: Integer;
  LightArmour: Integer;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.CanDetect()');

  { Default result }
  Result := True;
  
	try
		{ Only handle sleeping monsters - wakened monsters can always see through
		  stealth }
		if not(LocalMonster.Awake) then
		begin
			{ Monster attempts are modified only by level }
			MonsterRoll := Random(20) + 1 + LocalMonster.Level;
			
			{ Players get to add Skills }
			PlayerRoll := Random(20) + 1 + LocalPlayer.Skills[SK_SUBTERFUGE] +
				LocalPlayer.Skills[SK_STEALTH];

      { If Stealthed, then add a stealth bonus }
      if LocalPlayer.Has(STEALTHED) then
        Inc(PlayerRoll, STEALTH_BONUS);

      { Reduce Stealth for wearing heavy and medium armour }
      LocalPlayer.ReturnArmourRatio(HeavyArmour, MediumArmour, LightArmour);
      Dec(PlayerRoll, HeavyArmour * 2);
      Dec(PlayerRoll, MediumArmour);

      { Increase or decrease for the distance }
      Inc(PlayerRoll, Dist(Point(FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY), Point(LocalMonster.X, LocalMonster.Y)) - 3);

			{ Learn skills if successful }
			if PlayerRoll > MonsterRoll then
			begin
        if LocalPlayer.CClass = cThief then
        begin
          if OneChanceIn(2) then
					  LocalPlayer.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
          LocalPlayer.LearnSkill(SK_STEALTH, Random(6) + 3);
        end
        else
        begin
          if OneChanceIn(4) then
					  LocalPlayer.LearnSkill(SK_SUBTERFUGE, Random(3) + 3);
				  LocalPlayer.LearnSkill(SK_STEALTH, Random(3) + 3);
        end;
			end;
			
			{ Return the result }
			Result := MonsterRoll >= PlayerRoll;
		end;
	except	
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Pass a game turn }
procedure PassATurn;
var
  HPInterval: Double;
  Loop: Integer;
  MPInterval: Double;
  Regeneration: Integer;
  MagicCarried: Integer;
  HPToRemove: Integer;
  PoisonInterval: Integer;
  StatusText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.PassATurn()');
  
  try 
  	{ Increase the Turn Count }
		Inc(FormDisplay.Game.Turns);
		
		{ Work out Hit Point Regeneration Rate }
		Regeneration := FormDisplay.Game.Player.ReGeneration - 
			FormDisplay.Game.Player.GetEnchantmentValue('ENCH_REGENERATION');
			
		{ Regeneration has a minimum rate }
		if Regeneration < MINIMUM_REGENERATION then
      Regeneration := MINIMUM_REGENERATION;
		
		{ Regeneration can be affected by Endurance (in the case of HP) and 
		  Intelligence (in the case of MP }
		HPInterval := ((MINIMUM_REGENERATION - FormDisplay.Game.Player.Endurance)
      / 2) * (Regeneration / 100);
			
		{ Magic Point regeneration does not use Hit Point Regeneration }
		MPInterval := ((MINIMUM_REGENERATION - FormDisplay.Game.Player.Intelligence)
       / 2);

		{ If we are not starving, increase HP }
		if (FormDisplay.Game.Turns mod (Trunc(HPInterval)) = 0) and
			 (FormDisplay.Game.Player.HP < FormDisplay.Game.Player.MaxHP) and
			 (not(FormDisplay.Game.Player.Starving))
		then FormDisplay.Game.Player.TakeDamage(nil, -1);
		
		{ If we are not starving, increase MP }
		if (FormDisplay.Game.Turns mod (Trunc(MPInterval)) = 0) and
			 (FormDisplay.Game.Player.MP < FormDisplay.Game.Player.MaxMP) and
			 (not(FormDisplay.Game.Player.Starving))
		then FormDisplay.Game.Player.MP := FormDisplay.Game.Player.MP + 1;

		{ Now calculate hunger - which is based upon how much magic the creature is
      wearing/wielding }
    MagicCarried := FormDisplay.Game.Player.GetMagicalLoad;

    { Hunger calculated from magic items is based upon endurance and resolve }
    if MagicCarried < (FormDisplay.Game.Player.Resolve +
      FormDisplay.Game.Player.Endurance + FormDisplay.Game.Player.Levels) then
		  FormDisplay.Game.Player.Feed(HUNGER_STEP_NORMAL)
    else if MagicCarried < (2 * (FormDisplay.Game.Player.Resolve +
      FormDisplay.Game.Player.Endurance + FormDisplay.Game.Player.Levels)) then
		  FormDisplay.Game.Player.Feed(HUNGER_STEP_LARGE)
    else
      FormDisplay.Game.Player.Feed(HUNGER_STEP_MAXIMUM);

    { Handle pseudoIDing items }
    PseduoIDTick(FormDisplay.Game.Player);

    { Decrease applicable status effects }
    for Loop := Low(FormDisplay.Game.Player.Status) to
      High(FormDisplay.Game.Player.Status) do
    begin
      if FormDisplay.Game.Player.Status[Loop] > 0 then
      begin
        if FormDisplay.Game.Player.Status[Loop] = 1 then
        begin
          { Alert the player that a status is ending }
          StatusText := '';
          case Loop of
            ENRAGED: StatusText := 'You no longer feel so angry!';
            REGENERATING: StatusText := 'You are no longer regenerating!';
            HASTED: StatusText := 'You no longer feel so speedy!';
            CONFUSED: StatusText := 'You no longer feel so confused!';
            BLINDED: StatusText := 'You can now see again!';
            DRAINED: StatusText := 'You no longer feel so fragile!';
            HELD: StatusText := 'You can now move again!';
            SEEINV: StatusText := 'You can no longer see into the Ethereal!';
            MASTERY: StatusText := 'You can no longer fight as well!';
            FREEA: StatusText := 'You can no longer move so freely!';
            REFLEXES: StatusText := 'You no longer dodge blows so easily';
            RESISTING_FIRE: StatusText := 'You no feel so resistant to Fire Magics';
            RESISTING_AIR: StatusText := 'You no feel so resistant to Air Magics';
            RESISTING_WATER: StatusText := 'You no feel so resistant to Water Magics';
            RESISTING_EARTH: StatusText := 'You no feel so resistant to Earth Magics';
          end;
          UnitEngine.UpdateLog(StatusText, mesStatus);
        end;
        Dec(FormDisplay.Game.Player.Status[Loop]);
      end;
    end;

    { Make sure to reset visibility }
    FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
      FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

    { Handle Poison - note we never actually kill the character with poison -
      poison will never put the character under 1 hp }
    if (FormDisplay.Game.Player.HP > 1) and
      (FormDisplay.Game.Player.Poison > 0) then
    begin
      { Wear off the poison }
      FormDisplay.Game.Player.Poison := FormDisplay.Game.Player.Poison - 1;

      { Check for a decrease }
      PoisonInterval := 15 - ((100 - FormDisplay.Game.Player.Poison) div 10);

      if FormDisplay.Game.Turns mod PoisonInterval = 0 then
      begin
        { Work out how many hp to remove }
        HPToRemove := FormDisplay.Game.Player.MaxHP div 10;
        if HPToRemove = 0 then
          HPToRemove := 1;

        { Take damage }
        FormDisplay.Game.Player.TakeDamage(nil, HPToRemove);
      end;
    end;

  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;		
end;

{ Rest for a number of turns }
procedure Rest(TurnsToRest: Integer);
var
  Ecology: String;
  Loop: Integer;
  Monster: TMonster;
  MonsterType: TMonsterArchetype;
  MonsterVisible: Boolean;
  OffSetX: Integer;
  OffSetY: Integer;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.Rest()');

	try
		{ Check to see if any monsters are visible }
		MonsterVisible := not(FormDisplay.Game.CanRest);

		{ Can't rest when monsters are visible }
		if (MonsterVisible) then
			UpdateLog('You cannot rest now, there are enemies nearby', mesDefault)
		else if (FormDisplay.Game.Player.Starving) then
      UpdateLog('You''re too hungry to rest', mesDefault)
    else
		begin
			{ Rest for a number of turns, checking each turn to ensure we can still
			  rest }
			for Loop := 1 to TurnsToRest do
			begin
				{ Pass the Turn }
				PassATurn;
				
				{ Process any nearby creatures }
				ProcessCreatures(FormDisplay.Game.Dungeon);


        { Check for a random passing monster - the chance of this happening is
          dependent upon level depth }
        if (Random(PERCENTAGE) < FormDisplay.Game.Dungeon.LevelDepth) and
         (FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN) then
        begin
          { Get a suitable monster }
          Ecology:= GetEcology(FormDisplay.Game.Dungeon.LevelTheme);
          MonsterType := GetRandomMonsterArchetype(FormDisplay.Game.Player.Levels, 
            Ecology, 'Common', True);

          { Create Monster }
          Monster := TMonster.Create(MonsterType.ID);
          X := -1;
          Y := -1;

          { locate and place down the monster near to the player }
          for OffsetX := FormDisplay.Game.PlayerX - 2 to
            FormDisplay.Game.PlayerX + 2 do
          begin
            for OffSetY := FormDisplay.Game.PlayerY - 2 to
              FormDisplay.Game.PlayerY + 2 do
            begin
              if (FormDisplay.Game.Dungeon.Monsters[OffsetX, OffsetY] = 0) and
                 (FormDisplay.Game.Dungeon.Walkable[OffsetX, OffsetY] = True) and
                 (OffsetX <> FormDisplay.Game.PlayerX) and 
                 (OffSetY <> FormDisplay.Game.PlayerY) then
              begin
                X := OffsetX;
                Y := OffsetY;
                break;
              end
            end;
          end;

          { Add the Monster }
          if (X <> -1) and (Y <> -1) then
          begin
            Monster.X := X;
            Monster.Y := Y;
            FormDisplay.Game.Dungeon.Monsters[X, Y] := GMonsterList.Count;
            GMonsterList.Add(Monster);
          end;
        end;

				{ Again, check for any nearby visible monsters }
				MonsterVisible := not(FormDisplay.Game.CanRest);

				{ Break out if there are }
				if (MonsterVisible) then
				begin
					UpdateLog('You have been interrupted - something is nearby!', mesDefault );
					Break;
				end;
			end;

			{ Display an appropriate message if we have had uninterrupted rest }
			if (not(MonsterVisible)) and (TurnsToRest > 1) then 
				UpdateLog('You have rested successfully!', mesDefault);
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;		
end;

{ Handle drinking from fountains }
procedure DrinkFromFountain;
var
  FountainChance: Integer;
  X: Integer;
  Y: Integer;
  ItemQualityRatio: Integer;
  ItemQuality: crItemQuality;                                   
  ItemType: crItemType;
  LocalItem: TItem;
  OffsetX: Integer;
  OffsetY: Integer;
  MonsterType: TMonsterArchetype;
  Monster: TMonster;
  Ecology: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.DrinkFromFountain()');

	try
		{ Randomly decide what type of fountain we find }
		FountainChance := Random(100) + 1;
		
		{ Display drinking message }
		UpdateLog('You take a draft from the fountain', mesFountain);
		
		{ Only a small proportion of fountains have magical effects }
		if (FountainChance <= CHANCE_MAGICAL) then
		begin
			{ There are many different types of magical fountains }
			FountainChance := Random(4) + 1;
			
			{ Handle each type of fountain }
			case FountainChance of
				1:  begin
							{ This type of fountain gives the player back any lost mana and
							  also gives you some bonus mana - the amount of bonus mana added
							  is dependent upon whither or not the player has maximum mana to
							  begin with }
							if FormDisplay.Game.Player.MP = 
								FormDisplay.Game.Player.MaxMP then
							begin
								FormDisplay.Game.Player.BaseMaxMP := 
									FormDisplay.Game.Player.BaseMaxMP +
                  Random(MAXIMUM_EXTRA_HEALTH) + 1
							end
							else
							begin
								FormDisplay.Game.Player.MP := 
									FormDisplay.Game.Player.MP + (Random(MAXIMUM_EXTRA_MANA) + 1)
                    * FormDisplay.Game.Player.Levels;
									
								if FormDisplay.Game.Player.MP > 
									FormDisplay.Game.Player.MaxMP then
									FormDisplay.Game.Player.MP := FormDisplay.Game.Player.MaxMP;
							end;
							
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel magically re-invigorated', mesDefault);
						end;
				2:  begin
							{ This type of fountain gives the player magic mapping for the 
							  entire level }
							for X := 1 to DUNGEONSIZEX do
								for Y := 1 to DUNGEONSIZEY do
									{ The CountWalls call is important to only allow visibility
									  of rock near a space - cells completely surrounded by rock
									  should never become visible }
									if (FormDisplay.Game.Dungeon.Visible[X, Y] = NOT_YET_VISIBLE)
                    and (FormDisplay.Game.Dungeon.CountWalls(X, Y) <> 8) then
										FormDisplay.Game.Dungeon.Visible[X, Y] :=
                      PREVIOUSLY_VISIBLE;

							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel magically aware of your surroundings', 
								mesDefault);
						end;
				3:  begin
							{ This type of fountain gives a magical item }
							
							{ This is just a hack to avoid handling multiple items on the
							  same tile }							  
							if FormDisplay.Game.Dungeon.Objects
								[FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY] = 0 then
							begin
								{ Figure out what type of item to grant the player }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								
								{ Only hand out very decent quality items }
								if ItemQualityRatio < CHANCE_ARTIFACT then 
									ItemQuality := iArtifact
								else if ItemQualityRatio < CHANCE_EPIC then 
									ItemQuality := iEpic
								else 
									ItemQuality := iLegendary;

								{ Decide what type of items to give out }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								
								{ Only certain types of items can be handed out }
								if ItemQualityRatio < CHANCE_WEAPON then 
									ItemType := iWeapon
								else if ItemQualityRatio < CHANCE_ARMOUR then 
									ItemType := iArmour
								else if ItemQualityRatio < CHANCE_RING then 
									ItemType := iRing
								else 
									ItemType := iAmulet;

								{ Create the item }
								LocalItem := TItem.Create(ItemQuality, 
									FormDisplay.Game.Player.Levels, ItemType, False);
									
								{ Add the item to the dungeon }
								LocalItem.Location := iFloor;
								GItemList.Add(LocalItem);
								FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
									FormDisplay.Game.PlayerY] := GItemList.Count - 1;
								
								{ Alert the player to what has happened }
								UpdateLog('You feel something in the fountain!', mesDefault);
							end
							else
							begin
								{ An inert fountain, that does nothing but slightly fills you }

								{ Alert the player to what has happened }
								UpdateLog('You drink from the fountain. The water refreshes you', 
									mesFountain);

								{ Drinking decreases hunger }
								FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT);
							end;
						end;
				4:  begin
							{ This type of fountain increases hit point regeneration but with
							  a small hunger penalty }
							FormDisplay.Game.Player.Regeneration := 
								FormDisplay.Game.Player.Regeneration +
                FOUNTAIN_REGENERATION_BOOST;
							
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel re-invigorated', mesDefault);
						end;
			end;
		end
		else if (FountainChance <= CHANCE_BLESSED) then
		begin
			{ There are several different types of blessed fountains }
			FountainChance := Random(4) + 1;
			
			{ Handle each type of fountain }
			case FountainChance of
				1:  begin
							{ This type of fountain gives the player back any lost hp and
								also gives you some bonus hp - the amount of bonus hp added
								is dependent upon whither or not the player has maximum hp to
							  begin with }
							if (FormDisplay.Game.Player.HP = 
								FormDisplay.Game.Player.MaxHP) then
							begin
								FormDisplay.Game.Player.BaseMaxHP := 
									FormDisplay.Game.Player.BaseMaxHP +
                  Random(MAXIMUM_EXTRA_HEALTH) + 1;
							end
							else
							begin
								FormDisplay.Game.Player.HP := FormDisplay.Game.Player.HP + 
									(Random(MAXIMUM_EXTRA_MANA) + 1) *
                  FormDisplay.Game.Player.Levels;
								if FormDisplay.Game.Player.HP > 
									FormDisplay.Game.Player.MaxHP then
									FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;
							end;
								
							{ Drinking decreases hunger very much }
							FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT * 5);

							{ Alert the player to what has happened }
							UpdateLog('You feel refreshed', mesDefault);
						end;
				2:  begin
							{ This type of fountain increases hit point regeneration but
							  without the hunger penalty }
							FormDisplay.Game.Player.Regeneration := 
								FormDisplay.Game.Player.Regeneration +
                FOUNTAIN_REGENERATION_BOOST;
							
							{ Drinking decreases hunger very much }
							FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT * 5);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel re-invigorated', mesDefault);
						end;
				3:  begin
							{ This type of fountain increases endurance }
							FormDisplay.Game.Player.Endurance := 
								FormDisplay.Game.Player.Endurance + 1;
							
							{ Drinking decreases hunger very much }
							FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT * 5);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel hearty', mesDefault);
						end;
				4:  begin
							{ This type of fountain increases intelligence }
							FormDisplay.Game.Player.Intelligence := 
								FormDisplay.Game.Player.Intelligence + 1;
							
							{ Drinking decreases hunger very much }
							FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT * 5);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel clever', mesDefault);
						end;
			end;
		end
		else if (FountainChance <= CHANCE_CURSED) then
		begin
			{ Cursed fountains }
			
			{ There are many different types of cursed  fountains }
			FountainChance := Random(5) + 1;
			
			{ Handle each type of fountain }
			case FountainChance of
				1:  begin
							{ This type of fountain decreases hit point regeneration }
							FormDisplay.Game.Player.Regeneration := 
								FormDisplay.Game.Player.Regeneration -
                FOUNTAIN_REGENERATION_BOOST;
							
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER * 3);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel frailer', mesDefault);
						end;
				2:  begin
							{ This type of fountain gives a cursed magical item }
														
							{ This is just a hack to avoid handling multiple items on the
							  same tile }							  		
							if FormDisplay.Game.Dungeon.Objects
								[FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY] = 0 then
							begin
							{ Figure out what type of item to grant the player }
								ItemQualityRatio := Random(PERCENTAGE) + 1;

								{ Only hand out very decent quality items }
								if ItemQualityRatio < CHANCE_ARTIFACT then 
									ItemQuality := iArtifact
								else if ItemQualityRatio < CHANCE_EPIC then 
									ItemQuality := iEpic
								else 
									ItemQuality := iLegendary;

								{ Decide what type of items to give out }
								ItemQualityRatio := Random(PERCENTAGE) + 1;

								{ Only certain types of items can be handed out }
								if ItemQualityRatio < CHANCE_WEAPON then 
									ItemType := iWeapon
								else if ItemQualityRatio < CHANCE_ARMOUR then 
									ItemType := iArmour
								else if ItemQualityRatio < CHANCE_RING then 
									ItemType := iRing
								else 
									ItemType := iAmulet;

								{ Create the item }
								LocalItem := TItem.Create(ItemQuality, 
									FormDisplay.Game.Player.Levels, ItemType, True);

								{ Add the item to the dungeon }
								LocalItem.Location := iFloor;
								GItemList.Add(LocalItem);
								FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
								FormDisplay.Game.PlayerY] := GItemList.Count - 1;

                { Drinking increases hunger }
							  FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER * 3);
							  UpdateLog('You feel weakened', mesDefault);
															
								{ Alert the player to what has happened }
								UpdateLog('You find something in the fountain!', mesDefault);
							end;
						end;
				3:  begin							
						  { This type of fountain gives the player back any lost hp but
								also removes some bonus hp - the amount of hp removed is
								dependent upon whither or not the player has maximum hp to
							  begin with }
							if (FormDisplay.Game.Player.HP = 
								FormDisplay.Game.Player.MaxHP) then
							begin
								FormDisplay.Game.Player.BaseMaxHP := 
									FormDisplay.Game.Player.BaseMaxHP -
                  Random(MAXIMUM_EXTRA_HEALTH) + 1;
								FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;

                { Do a check for death just in case}
                FormDisplay.Game.Player.TakeDamage(nil, 0);
							end
							else
							begin
								FormDisplay.Game.Player.BaseMaxHP := 
									FormDisplay.Game.Player.BaseMaxHP -
                  Random(MAXIMUM_EXTRA_HEALTH) + 1;
								if (FormDisplay.Game.Player.HP > 
									FormDisplay.Game.Player.MaxHP) then
									FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;

                { Do a check for death just in case}
                FormDisplay.Game.Player.TakeDamage(nil, 0);
							end;
								
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER * 3);
							UpdateLog('You feel weakened', mesDefault);
						end;
				4:  begin
							{ This type of fountain gives the player amnesia for the entire 
							  level }
							for X := 1 to DUNGEONSIZEX do
								for Y := 1 to DUNGEONSIZEY do
								begin
									{ The CountWalls call is important to only allow visibility
										of rock near a space - cells completely surrounded by rock
									  should never become visible }
										if FormDisplay.Game.Dungeon.Visible[X, Y] =
                      PREVIOUSLY_VISIBLE
											then FormDisplay.Game.Dungeon.Visible[X, Y] :=
                        NOT_YET_VISIBLE;
								end;
							
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER * 3);
							
							{ Alert the player to what has happened }
							UpdateLog('You forget where you are!', mesDefault);
						end;
				5:  begin
							{ This type of fountain decreases endurance }
							FormDisplay.Game.Player.Endurance := 
								FormDisplay.Game.Player.Endurance - 1;
							
							{ Drinking increases hunger }
							FormDisplay.Game.Player.Feed(FOUNTAIN_HUNGER * 5);
							
							{ Alert the player to what has happened }
							UpdateLog('You feel weak', mesDefault);
						end;
			end;

		end
		else if (FountainChance <= CHANCE_GUARDED) then
		begin
			{ This type of function summons a guardian monster }
			
			{ Get a suitable monster }
			Ecology:= GetEcology(FormDisplay.Game.Dungeon.LevelTheme);
			MonsterType := GetRandomMonsterArchetype(FormDisplay.Game.Player.Levels, 
				Ecology, 'Common', True);

			{ Create Monster }
			Monster := TMonster.Create(MonsterType.ID);
			X := -1;
			Y := -1;

			{ locate and place down the monster near to the player }
			for OffsetX := FormDisplay.Game.PlayerX - 1 to 
				FormDisplay.Game.PlayerX + 1 do
			begin
				for OffSetY := FormDisplay.Game.PlayerY - 1 to 
					FormDisplay.Game.PlayerY + 1 do
				begin
					if (FormDisplay.Game.Dungeon.Monsters[OffsetX, OffsetY] = 0) and
						 (FormDisplay.Game.Dungeon.Walkable[OffsetX, OffsetY] = True) and
						 (OffsetX <> FormDisplay.Game.PlayerX) and 
						 (OffSetY <> FormDisplay.Game.PlayerY) then
					begin
						X := OffsetX;
						Y := OffsetY;
						break;
					end
				end;
			end;

			{ Add the Monster }
			if (X <> -1) and (Y <> -1) then
			begin
				Monster.X := X;
				Monster.Y := Y;
				FormDisplay.Game.Dungeon.Monsters[X, Y] := GMonsterList.Count;
				GMonsterList.Add(Monster);

				{ Alert the player to what has happened }
				UpdateLog('The fountain summons a guardian!', mesFountain);
			end;
		end
		else
		begin
			{ An inert fountain, that does nothing but slightly fills you }
		
			{ Alert the player to what has happened }
			UpdateLog('You drink from the fountain. The water refreshes you', 
				mesFountain);
				
			{ Drinking decreases hunger }
			FormDisplay.Game.Player.Feed(FOUNTAIN_REFRESHMENT);
		end;  
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ This routine is used to generate both monster "speech" and monster actions
  and will update the messagelog }
function MonsterSpeaks(Monster: TMonster;  Dungeon: TDungeonLevel; 
	MonsterKnown: Boolean): Boolean;
var
  ActionType: Integer;
  MonsterSpeech: String;
  SpeechCounter: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.MonsterSpeaks()');

  { Default result }
  Result := False;
  ActionType := NONE;
  
  try
		{ Dead or sleeping monsters can't speak }
		if (not(Monster.Awake)) or (not(Monster.Alive)) then
			Result := False
		{ Monsters that can't see the player won't speak }
		else if (not(XCanSeeY(Dungeon, FormDisplay.Game.PlayerX, 
			FormDisplay.Game.PlayerY, Monster.X, Monster.Y))) then
			Result := False
		else
		begin
			{ Unique monsters are more talkable }
			if Monster.UniqueName <> '' then
				SpeechCounter := Monster.SpeechFrequency div 2
			else
				SpeechCounter := Monster.SpeechFrequency;

			{ Now check if we can do something }
			begin
				if OneChanceIn(SpeechCounter) then
				begin
					{ Now select a random action to perform }
					ActionType := Random(3);

					{ Verbal action, if the monster has this type of action defined }
					if (ActionType = VERBALACTION) and (Monster.SpeechActions <> '') then
					begin
						{ If the monster has been seen, format the speech differently }
						if MonsterKnown then
							MonsterSpeech := Trim(Format('%s%s %s', [Monster.Prefix,
								Monster.Name, Monster.SpeechActions]))
						else
							MonsterSpeech := Format('%s %s!', ['Something nearby',
								Monster.SpeechActions]);
						Result := True;
					end
					{ Verbal speech, if the monster has this type of action defined }
					else if (ActionType = SPEECH) and (Monster.Speech <> '') then
					begin
						{ If there is a unique alive on the level then occasionally allow 
						  it to cheer it on else if the Unique is dead, then do different 
						  speech }
						if Assigned(Dungeon.Unique) and OneChanceIn(COIN_FLIP) and 
							(Monster.UniqueName = '') then
						begin
							{ If the Unique is alive }
							if Dungeon.Unique.Alive then
							begin
								{ If the monster has been seen, format the speech differently }
								if MonsterKnown then
									MonsterSpeech := Trim(Format('%s%s %s "%s %s!"', ['The ',
										Monster.Name, SpeechType[Random(High(SpeechType) + 1)],
										Dungeon.Unique.Name, 
										GUniqueCheers[Random(High(GUniqueCheers))]]))
								else
									MonsterSpeech := Format('%s %s "%s %s!"', 
									  ['Something nearby', 
									  SpeechType[Random(High(SpeechType) + 1)], 
									  Dungeon.Unique.Name, 
										GUniqueCheers[Random(High(GUniqueCheers))]]);
								Result := True;
							end
							else							
							begin
								{ If the monster has been seen, format the speech differently }
								if MonsterKnown then
									MonsterSpeech := Trim(Format('%s%s %s "%s %s!"', ['The ',
										Monster.Name, SpeechType[Random(High(SpeechType) + 1)],
										Dungeon.Unique.Name, 
										GUniqueMourns[Random(High(GUniqueMourns))]]))
								else
									MonsterSpeech := Format('%s %s "%s %s!"', 
										['Something nearby', 
										SpeechType[Random(High(SpeechType) + 1)],
										Dungeon.Unique.Name,
										GUniqueMourns[Random(High(GUniqueMourns))]]);
								Result := True;
							end
            end
						else
						begin
						  { Just do standard speech }
							if MonsterKnown then
								MonsterSpeech := Trim(Format('%s%s %s "%s"', [Monster.Prefix,
									Monster.Name, SpeechType[Random(High(SpeechType) + 1)],
									Monster.Speech]))
							else
								MonsterSpeech := Format('%s %s "%s"', ['Something nearby',
									SpeechType[Random(High(SpeechType) + 1)], Monster.Speech]);
							Result := True;
						end;
					end
					else if (ActionType = ACTION) and (Monster.Actions <> '') then
					begin
					  { Monsters can also do actions }
						if MonsterKnown then
						begin
							MonsterSpeech := Trim(Format('%s%s %s', [Monster.Prefix,
								Monster.Name, Monster.Actions]));
							Result := True;
						end;
					end;
				end
				else
				begin
				  { Handle monster actions }
					if OneChanceIn(SpeechCounter) then
					begin
						ActionType := ACTION;
						if Monster.Actions <> '' then
							if MonsterKnown then
							begin
								MonsterSpeech := Format('%s%s %s', [Monster.Prefix, 
									Monster.Name, Monster.Actions]);
								Result := True;
							end;
					end;
				end;
			end;
		end;

		{ Output the Speech to the Message Log }
		if (Result = True) then
		begin
			MonsterSpeech := Trim(MonsterSpeech);
			case ActionType of
				VERBALACTION: UnitEngine.UpdateLog(MonsterSpeech, mesMonsterAction);
				SPEECH: UnitEngine.UpdateLog(MonsterSpeech, mesMonsterSpeech);
				ACTION: UnitEngine.UpdateLog(MonsterSpeech, mesMonsterAction);
			end;
		end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;
            
{ Handle pseudoid - this is called every turn}
procedure PseduoIDTick(LocalPlayer: TCreature);
var
  Loop: Integer;
  LocalItem: TItem;
  Pseudo: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.PseduoIDTick()');

  try
    { On each turn, advance the tick of unidentified magical items in the
      player inventory by one }
    for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
    begin
      { Check if there is a item in each slot }
      if LocalPlayer.Inventory[Loop] > 0 then
      begin
        { Get each item }
        LocalItem := GItemList[LocalPlayer.Inventory[Loop]] as TItem;

        { Check if the item is magical and unidentified and not already
          pseudoided }
        if (not(LocalItem.Known)) and (LocalItem.ItemQuality in [iSuperb,
          iLegendary, iEpic, iArtifact]) and (not(LocalItem.PseudoIDed)) then
        begin
          { Increase the counter but not for scrolls and potions }
          if not(LocalItem.ItemType in [iScroll, iPotion]) then
            LocalItem.IDCounter := LocalItem.IDCounter + 1
          else
            LocalItem.IDCounter := -1;

          { Check if we can pseudoid it - the amount of time it takes to
            identify something is dependent upon the quality of the item }
          if LocalItem.IDCounter >
            (Ord(LocalItem.ItemQuality) * PSEUDOID_TURNS) then
           begin
            case LocalItem.ItemQuality of
              iSuperb: Pseudo := 'superb';
              iLegendary: Pseudo := 'legendary';
              iEpic: Pseudo := 'epic';
              iArtifact: Pseudo := 'artifact';
            end;

            { Alert the player }
            UpdateLog(Format('You can tell that %s you are carrying is of %s' +
              ' quality!', [Lower(LocalItem.Name), Pseudo]), mesItemManipulation);

            { PseudoID the item - do the flag here after the message has been
              displayed so that we don't display the pseudoid brand twice }
            LocalItem.PseudoIDed := True;
           end;
        end;
      end;
    end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Drink potions }
function DrinkPotion (PotionToDrink: TItem; LocalPlayer: TCreature): String;
var
  PotionType: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.DrinkPotion()');

  { Default Result }
  Result := '';

  try
    { Now drink potions }
    PotionType := PotionToDrink.Position;
    case PotionToDrink.Position of
      HEALING:
      begin
        { Identify this potion if its not already identified }
        FormDisplay.Game.Player.HP := FormDisplay.Game.Player.HP +
          FormDisplay.Game.Player.MaxHP div 2;
        if FormDisplay.Game.Player.HP >	FormDisplay.Game.Player.MaxHP then
          FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;
        UnitEngine.UpdateLog('You feel better', mesItemManipulation);

        { Handle Poison Effects }
        if FormDisplay.Game.Player.Poison > 0 then
        begin
          FormDisplay.Game.Player.Poison := 0;
          UnitEngine.UpdateLog('You are no longer poisoned',
            mesItemManipulation);
        end;

        { Handle ID by using }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
        begin
          FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
          UnitEngine.UpdateLog('This is a Potion of Healing',
            mesItemManipulation);
        end;
      end;
      SPEED:
      begin
        { If under the effects of speed already, can't drink it }
        if FormDisplay.Game.Player.Has(HASTED) then
          UnitEngine.UpdateLog('You can''t drink that now', mesItemManipulation)
        else
        begin
          UnitEngine.UpdateLog('You feel faster', mesItemManipulation);
          FormDisplay.Game.Player.Status[HASTED] := POTION_DURATION;
        end;

        { Always identify this potion }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
        begin
          FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
          UnitEngine.UpdateLog('This is a Potion of Speed',
            mesItemManipulation);
        end;

        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      RESTORE_ABILITIES:
      begin
        { Identify this potion only we've lost stats }
        if FormDisplay.Game.Player.Has(DRAINED) then
        begin
          { Restore Abilities }
          FormDisplay.Game.Player.Status[DRAINED] := 0;
          UnitEngine.UpdateLog('You feel normal again', mesItemManipulation);

          { Handle ID by using }
          if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          begin
            FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
            UnitEngine.UpdateLog('This is a Potion of Restore Abilities',
              mesItemManipulation);
          end;
        end
        else
        begin
          { Nothing happens }
          if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
            FormDisplay.Game.SetPotionStatus(PotionType, poTried);
          UnitEngine.UpdateLog('Nothing happens', mesItemManipulation);
        end;
      end;
      CONFUSION:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You feel befuddled!', mesItemManipulation);
        FormDisplay.Game.Player.Status[CONFUSED] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Confusion!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      BLINDNESS:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You suddenly can''t see!', mesItemManipulation);
        FormDisplay.Game.Player.Status[BLINDED] := POTION_DURATION;
        FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
          FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Blindness!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      SEE_INVISIBLE:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You can now see into the ethereal!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[SEEINV] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of See Invisible!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      EXTRA_HEALING:
      begin
        { Identify this potion if its not already identified }
        FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;
        UnitEngine.UpdateLog('You feel better', mesItemManipulation);

        { Handle Poison Effects }
        if FormDisplay.Game.Player.Poison > 0 then
        begin
          FormDisplay.Game.Player.Poison := 0;
          UnitEngine.UpdateLog('You are no longer poisoned',
            mesItemManipulation);
        end;

        { Handle other effects }
        if FormDisplay.Game.Player.Has(CONFUSED) then
        begin
          FormDisplay.Game.Player.Status[CONFUSED] := 0;
          UnitEngine.UpdateLog('You are no longer confused',
            mesItemManipulation);
        end;
        if FormDisplay.Game.Player.Has(BLINDED) then
        begin
          FormDisplay.Game.Player.Status[BLINDED] := 0;
          UnitEngine.UpdateLog('You can now see again!', mesItemManipulation);
        end;
        if FormDisplay.Game.Player.Has(DRAINED) then
        begin
          FormDisplay.Game.Player.Status[DRAINED] := 0;
          UnitEngine.UpdateLog('You no longer feel so fragile!',
            mesItemManipulation);
        end;
        if FormDisplay.Game.Player.Has(HELD) then
        begin
          FormDisplay.Game.Player.Status[HELD] := 0;
          UnitEngine.UpdateLog('You can now move again!', mesItemManipulation);
        end;
        
        { Handle ID by using }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
        begin
          FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
          UnitEngine.UpdateLog('This is a Potion of Extra-Healing',
            mesItemManipulation);
        end;
      end;
      PARALYSIS:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You suddenly can''t move', mesItemManipulation);
        FormDisplay.Game.Player.Status[HELD] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Paralysis!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      FREE_ACTION:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You can now move much more freely!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[FREEA] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Free Action!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      RESIST_FIRE:
      begin
      { Handle ID by using }
        UnitEngine.UpdateLog('You feel more resistant to Fire Magics!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[RESISTING_FIRE] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Resist Fire!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      RESIST_EARTH:
      begin
      { Handle ID by using }
        UnitEngine.UpdateLog('You feel more resistant to Earth Magics!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[RESISTING_EARTH] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Resist Earth!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      RESIST_WATER:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You feel more resistant to Water Magics!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[RESISTING_WATER] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Resist Water!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      RESIST_AIR:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You feel more resistant to Air Magics!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[RESISTING_AIR] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Resist Air!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      COMBAT_MASTERY:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You can now fight much better!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[MASTERY] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Combat Mastery!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      COMBAT_REFLEXES:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You can now dodge blows much easier!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[REFLEXES] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Combat Reflexes!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
      MIGHT:
      begin
        { Handle ID by using }
        UnitEngine.UpdateLog('You feel very angry!',
          mesItemManipulation);
        FormDisplay.Game.Player.Status[ENRAGED] := POTION_DURATION;

        { Identify this potion always }
        if FormDisplay.Game.PotionIDed[PotionType] <> poKnown then
          UnitEngine.UpdateLog('This is a Potion of Might!',
              mesItemManipulation);
        FormDisplay.Game.SetPotionStatus(PotionType, poKnown);
      end;
    end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;
  
{ Read scrolls }
function ReadScroll(ScrollToRead: TItem; LocalPlayer: TCreature): String;
var
  ScrollType: Integer;
  Counter: Integer;
  LocalItem: TItem;
  InventorySlot: Integer;
  CursedItems: Integer;
  Loop: Integer;
  X: Integer;
  Y: Integer;
  ItemsUnknown: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitEngine.ReadScroll()');

  { Default Result }
  Result := '';

  try
    { Get the scroll type }
    ScrollType := ScrollToRead.Position;

    { Handle the different scrolls }
    case Ord(ScrollType) of
      ENCHANT_ARMOUR:
      begin
        { Enchant an item of worn armour with an AC enhancement }

        { Set up some initial variables }
        LocalItem := nil;
        Counter := 0;

        { First, find a random worn armour item }
        repeat
          InventorySlot := S_FEET + Random((S_NECK - S_FEET) + 1);
          if LocalPlayer.Inventory[InventorySlot] > 0 then
            LocalItem := (GItemList[LocalPlayer.Inventory[InventorySlot]]) as
              TItem;

          { Increase paranoia counter }
          inc(Counter);
        until (LocalItem <> nil) or (Counter > 200);

        { If we have an item found }
        if Assigned(LocalItem) then
        begin
          { Use the scroll }
          UpdateLog('As you read the scroll, it crumbles to dust',
            mesItemManipulation);

          { Output an appropriate message here before the item is changed }
          Result := Format('%s momentarily glows a vibrant blue',
            [LocalItem.Name]);

          { Set the item quality if necessary to reflect the new enchantment }
          if LocalItem.ItemQuality = iCommon then
            LocalItem.ItemQuality := iSuperb;
            
          { Add an enchantment to the item of the current player level + 3 }
          LocalItem.AddSpecificEnchantment('ENCH_AC', LocalPlayer.Levels + 3,
            False);

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Enchant Armour',
              mesItemManipulation);
          end;
        end
        else
        begin
          { Use the scroll }
          UpdateLog('As you read the scroll, it crumbles to dust',
            mesItemManipulation);

          { No item to use it on }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      ENCHANT_WEAPON:
      begin
        { Enchant the current wielded weapon with either a +hit or +dam
          enhancement }

        { Set up some initial variables }
        LocalItem := nil;
        Counter := 0;

        { First, find a random wielded weapon item }
        repeat
          InventorySlot := S_MAINHAND + Random((S_OFFHAND - S_MAINHAND) + 1);
          if LocalPlayer.Inventory[InventorySlot] > 0 then
          begin
            { Make sure its a weapon }
            LocalItem := (GItemList[LocalPlayer.Inventory[InventorySlot]]) as
              TItem;
            if LocalItem.ItemType <> iWeapon then
              LocalItem := nil;
          end;

          { Increase paranoia counter }
          inc(Counter);
        until (LocalItem <> nil) or (Counter > 200);

        { If we have an item found }
        if Assigned(LocalItem) then
        begin
          { Use the scroll }
          UpdateLog('As you read the scroll, it crumbles to dust',
            mesItemManipulation);

          { Output an appropriate message here before the item is changed }
          Result := Format('%s momentarily glows a vibrant silver',
            [LocalItem.Name]);

          { Set the item quality if necessary to reflect the new enchantment }
          if LocalItem.ItemQuality = iCommon then
            LocalItem.ItemQuality := iSuperb;

          { Add an enchantment to the item of the current player level + 3 }
          if OneChanceIn(2) then
            LocalItem.AddSpecificEnchantment('ENCH_HIT', LocalPlayer.Levels + 3,
              False)
          else
            LocalItem.AddSpecificEnchantment('ENCH_DAM', LocalPlayer.Levels + 3,
              False);

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Enchant Weapon',
              mesItemManipulation);
          end;
        end
        else
        begin
          { Use the scroll }
          UpdateLog('As you read the scroll, it crumbles to dust',
            mesItemManipulation);

          { No item to use it on }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      REMOVE_CURSE:
      begin
        { Remove all curses from carried and wielded items }

        { Use the scroll }
        UpdateLog('As you read the scroll, it crumbles to dust',
          mesItemManipulation);

        { Reset the cursed item type }
        CursedItems := 0;

        { Get the number of cursed items carried }
        for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
        begin
          { Get the item }
          LocalItem := (GItemList[LocalPlayer.Inventory[Loop]]) as TItem;

          { See if it's cursed }
          if LocalItem.Cursed then
            inc(CursedItems);
        end;

        { Get the number of cursed items worn }
        for Loop := S_FEET to S_BACK do
        begin
          { Get the item }
          LocalItem := (GItemList[LocalPlayer.Inventory[Loop]]) as TItem;

          { See if it's cursed }
          if LocalItem.Cursed then
            Inc(CursedItems);
        end;

        { If we have cursed items then uncurse all of them}
        if CursedItems > 0 then
        begin
          { Look at carried items }
          for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
          begin
            { Get the item }
            LocalItem := (GItemList[LocalPlayer.Inventory[Loop]]) as TItem;

            { See if it's cursed and if so, uncurse it }
            if LocalItem.Cursed then
              LocalItem.UnCurse;
          end;

          { Look at wielded/worn items }
          for Loop := S_FEET to S_BACK do
          begin
            { Get the item }
            LocalItem := (GItemList[LocalPlayer.Inventory[Loop]]) as TItem;

            { See if it's cursed and if so, uncurse it }
            if LocalItem.Cursed then
              LocalItem.UnCurse;
          end;

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Remove Curse',
              mesItemManipulation);
          end;

          { Return an appropriate message }
          Result := 'You feel something is helping you';
        end
        else
        begin
          { No cursed items }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      TELEPORTATION:
      begin
        { Use the scroll }
        UpdateLog('As you read the scroll, it crumbles to dust',
          mesItemManipulation);

        { Cannot be used on the town level }
        if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
        begin
          { Teleport randomly to another part of the level }
          Result := 'You feel weirdly unstable';

          { Find another spot to teleport to }
          repeat
            X := Random(DUNGEONSIZEX) + 1;
            Y := Random(DUNGEONSIZEY) + 1;
          until (FormDisplay.Game.Dungeon.Terrain[X, Y] = T_FLOOR_ROOM) or
            (FormDisplay.Game.Dungeon.Terrain[X, Y] = T_FLOOR_CORRIDOR);

          { Move to the new location }
          FormDisplay.Game.PlayerX := X;
          FormDisplay.Game.PlayerY := Y;

          { Calculate the viewport boundaries }
          FormDisplay.Game.SetViewBoundaries
            (FormDisplay.ScreenMain.Width, FormDisplay.ScreenMain.Height);

          { Set the viewport boundaries }
          FormDisplay.Game.SetViewPort;

          { Set the visible region }
          FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
            FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

          { Refresh the display }
          FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Teleportation',
              mesItemManipulation);
          end;
        end
        else
        begin
          { Tried to teleport on the town level }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      IDENTIFY:
      begin
        { Need to loop through inventory checking if any items need to be
          identified first }
        if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
        begin
          { Reset Counter }
          ItemsUnknown := 0;

          { Iterate through the inventory slots }
          for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
          begin
            { Check if the inventory slot is occupied }
            if FormDisplay.Game.Player.Inventory[Loop] > 0 then
            begin
              { Get the item details }
              LocalItem := GItemList[FormDisplay.Game.Player.Inventory[Loop]] as
                TItem;

              { Increment counter for unknown items }
              if LocalItem.Known then
                Inc(ItemsUnknown);
            end;
          end;

          { Iterate through the equipped slots }
          for Loop := S_FEET to S_BACK do
          begin
            { Check if the inventory slot is occupied }
            if FormDisplay.Game.Player.Inventory[Loop] > 0 then
            begin
              { Get the item details }
              LocalItem := GItemList[FormDisplay.Game.Player.Inventory[Loop]] as
                TItem;

              { Increment counter for unknown items }
              if LocalItem.Known then
                Inc(ItemsUnknown);
            end;
          end;

          { Now check if there's no unindentified items }
          if ItemsUnknown = 0 then
          begin
            { Tried to identify nothing }
            Result := 'Nothing happens as you read the scroll';

            { Set the status for an unsuccessful try }
            if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
              FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
          end
          else
          begin
            { We have unidentified items }
            HandleMouseInput(insIdentify);

            { Handle scroll by using }
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Identify',
              mesItemManipulation);
          end;
        end;

        { Because the identify needs a further key selectrion, handle it
          differently from the usual way }
        HandleMouseInput(insIdentify);
      end;
      MAGIC_MAPPING:
      begin
        { Use the scroll }
        UpdateLog('As you read the scroll, it crumbles to dust',
          mesItemManipulation);

        { Cannot be used on the town level }
        if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
        begin
          { Update the entire dungeon as previously visible }
          for X := 1 to DUNGEONSIZEX do
            for Y := 1 to DUNGEONSIZEY do
              { The CountWalls call is important to only allow visibility
                of rock near a space - cells completely surrounded by rock
                should never become visible }
              if (FormDisplay.Game.Dungeon.Visible[X, Y] = NOT_YET_VISIBLE)
                and (FormDisplay.Game.Dungeon.CountWalls(X, Y) <> 8) then
                FormDisplay.Game.Dungeon.Visible[X, Y] :=
                  PREVIOUSLY_VISIBLE;

          { Map the entire level }
          Result := 'You gain awareness of your surroundings';

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Magic Mapping',
              mesItemManipulation);
          end;
        end
        else
        begin
          { Used on the town level }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      FORGETFULNESS:
      begin
        { Use the scroll }
        UpdateLog('As you read the scroll, it crumbles to dust',
          mesItemManipulation);

        { Cannot be used on the town level }
        if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
        begin
          for X := 1 to DUNGEONSIZEX do
            for Y := 1 to DUNGEONSIZEY do
            begin
              { The CountWalls call is important to only allow visibility
                of rock near a space - cells completely surrounded by rock
                should never become visible }
                if FormDisplay.Game.Dungeon.Visible[X, Y] =
                  PREVIOUSLY_VISIBLE
                  then FormDisplay.Game.Dungeon.Visible[X, Y] :=
                    NOT_YET_VISIBLE;
            end;

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Forgetfulness',
              mesItemManipulation);
          end;

          { Forget the entire level }
          Result := 'You forget where you are';
        end
        else
        begin
          { Used on the town level }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
      BLANK_SCROLL:
      begin
        { Does nothing }
        Result := 'This scroll is blank';

        { Set the scroll result }
        FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
      end;
      CURSING:
      begin
        { Sticky-curse a number of worn and wielded items }

        { Use the scroll }
        UpdateLog('As you read the scroll, it crumbles to dust',
          mesItemManipulation);

        { Set up some initial variables }
        LocalItem := nil;
        Counter := 0;

        { First, find a random worn armour item }
        repeat
          InventorySlot := S_FEET + Random((S_NECK - S_FEET) + 1);
          if LocalPlayer.Inventory[InventorySlot] > 0 then
          begin
            { Don't curse already cursed items }
            if not(((GItemList[LocalPlayer.Inventory[InventorySlot]]) as TItem).
              Cursed) then
              LocalItem := (GItemList[LocalPlayer.Inventory[InventorySlot]]) as
                TItem;
          end;
          
          { Increase paranoia counter }
          inc(Counter);
        until (LocalItem <> nil) or (Counter > 200);

        { If we have an item found }
        if not(Assigned(LocalItem)) then
        begin
          { Set up some initial variables }
          LocalItem := nil;
          Counter := 0;

          { First, find a random wielded item }
          repeat
            InventorySlot := S_MAINHAND + Random((S_OFFHAND - S_MAINHAND) + 1);
            if LocalPlayer.Inventory[InventorySlot] > 0 then
            begin
            { Don't curse already cursed items }
            if not(((GItemList[LocalPlayer.Inventory[InventorySlot]]) as TItem).
              Cursed) then
              LocalItem := (GItemList[LocalPlayer.Inventory[InventorySlot]]) as
                TItem;
            end;

            { Increase paranoia counter }
            inc(Counter);
          until (LocalItem <> nil) or (Counter > 200);
        end;

        { We have an item so curse it }
        if Assigned(LocalItem) then
        begin
          { Output an appropriate message before the item is cursed }
          Result := Format('%s momentarily pulses a deep black',
            [LocalItem.Name]);

          { If the item is a common item, add an enchantment so it can be 
            cursed! }
          if LocalItem.ItemQuality = iCommon then
          begin
            { Set the item quality }
            LocalItem.ItemQuality := iSuperb;
            
            { Add the enchantments }
            if LocalItem.ItemType = iWeapon then
            begin
              if OneChanceIn(2) then
                LocalItem.AddSpecificEnchantment('ENCH_HIT', LocalPlayer.Levels
                  + 3, False)
              else
                LocalItem.AddSpecificEnchantment('ENCH_DAM', LocalPlayer.Levels
                  + 3, False);
            end
            else if LocalItem.ItemType = iArmour then
            begin
              LocalItem.AddSpecificEnchantment('ENCH_AC', LocalPlayer.Levels + 3
                , False)
            end;
          end;

          { Curse the item }
          LocalItem.Curse;

          { Handle ID by using }
          if FormDisplay.Game.ScrollIDed[ScrollType] <> stKnown then
          begin
            FormDisplay.Game.SetScrollStatus(ScrollType, stKnown);
            UnitEngine.UpdateLog('This is a Scroll of Cursing',
              mesItemManipulation);
          end;
        end
        else
        begin
          { No items to curse, which means no items are worn or carried }
          Result := 'Nothing happens as you read the scroll';

          { Set the status for an unsuccessful try }
          if FormDisplay.Game.ScrollIDed[ScrollType] = stUnknown then
            FormDisplay.Game.ScrollIDed[ScrollType] := stTried;
        end;
      end;
    end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle Poison Attacks from Monsters }
function XPoisonsY(LocalMonster: TMonster; LocalPlayer: TCreature): Boolean;
begin
	{ Logging }
  hLog.Add('{now} {lNum} UnitEngine.XPoisonsY()');

  { Default result }
  Result := False;

  try
    { Check if the monster can poison }
    if LocalMonster.PoisonLevel > 0 then
    begin
      { If so then check to see if it has poisoned }
      if random(PERCENTAGE) < LocalMonster.PoisonLevel then
      begin
        { Check the poison resistance }
        if random(PERCENTAGE) > LocalPlayer.PoisonResistance then
        begin
          { Poisons are cumulative }
          LocalPlayer.Poison := LocalPlayer.Poison + LocalMonster.PoisonLevel;

          { Don't add too much poison }
          if LocalPlayer.Poison > 100 then
            LocalPlayer.Poison := 100;

          { We have poisoned the character }
          Result := True;
        end;
      end;
    end;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

end.
