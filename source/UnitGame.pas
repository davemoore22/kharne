{ UnitGame

  Copyright (c) 2007-2009 Dave Moore 

  Holds the Game class definition

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

unit UnitGame;

interface

uses Types, Classes, SysUtils,Contnrs, Hotlog, UnitDefines, UnitCreature,
  UnitDungeon,  UnitTimer, UnitMonster;

{ TGame holds the transient game info }
type
  TGame = class

public
  { Just keep things in this class all public for the sake of brevity as this
    class is pretty much ubiquitous across all other units }

  { The character }
  Player: TCreature;

  { The current dungeon }
  Dungeon: TDungeonLevel;

  { The town level }
  TownDungeon: TDungeonLevel;

  { Current graphics mode, currently only ASCII monochrome or coloured }
  CurrentDrawMode: Integer;

  { Current list of monsters used for the visible monster list }
  CurrentMonsters: Array[0..500] of Integer;

  { Depth delved in each dungron branch }
  GDepthDelved: Array [0..9] of Integer;

  { Scroll randomisation }
  ScrollNameArray: Array[1..9] of String;

  { Scroll identification }
  ScrollIDed: Array[1..9] of tScrollStatus;

  { Potion randomisation }
  PotionNameArray: Array[1..16] of String;

  { Potion identification }
  PotionIDed: Array[1..16] of tPotionStatus;

  { Path-finding variables }
  EndCell: TPoint;                          
  StartCell: TPoint;
  Visited: TBits;
  Selected: Integer;

  { Timing variables }
  T1: TDateTime;
  T2: TDateTime;
  GameStartTime: TDateTime;

  { Blocking Timer Variable }
  Block: Boolean;

  { Start New Game/Loaded Existing Game flag }
  GameNew: Boolean;

  { FOV variables }
  mv: integer;
  mw: integer;

  { Player location }
  PlayerX: Integer;
  PlayerY: Integer;

  { Viewport boundaries }
  topleftx: Integer;
  toplefty: Integer;
  toprightx: Integer;
  toprighty: Integer;
  bottomrightx: Integer;
  bottomrighty: Integer;
  bottomleftx: Integer;
  bottomlefty: Integer;
  TopLeft: TPoint;
  TopRight: TPoint;
  BottomRight: TPoint;
  BottomLeft: TPoint;

  { Number of turns passed }
  Turns: Integer;

  { Turns since the last level feeling }
  LastLevelFeeling: Integer;

  { Current targeting mode }
  CurrentTargetMode: tTargetMode;

  { Killed by }
  Killed: String;

  { Game score }
  Score: Integer;

  { Monster Kills }
  MonsterKillNumbers: Array[0..1000] of Integer;
  MonsterKills: TStringList;
  MonsterKillsPlural: TStringList;

  { Game timers }
  TimerList: TObjectList;

  { Tooltips }
  Tooltip: String;

  { Default constructor }
  constructor Create; overload;

  { Standard constructor }
  constructor Create(NewGame: Boolean; CharacterName: String); overload;

  { Default destructor }
  destructor Destroy; override;

  { See if there are any monsters within line of sight }
  function CanRest: Boolean;

  { See if there are any aware monsters within line of sight }
  function CanStealth: Boolean;

  { Set the view port }
  procedure SetViewPort;

  { Set the view boundaries }
  procedure SetViewBoundaries(ScreenWidth: Integer; ScreenHeight: Integer);

  { Add a monster kill to the monster kill list }
  procedure LogMonsterKill(LocalMonster: TMonster);

  { Set scroll status }
  procedure SetScrollStatus(ScrollType: Integer; ScrollStatus: tScrollStatus);

  { Set potion status }
  procedure SetPotionStatus(PotionType: Integer; PotionStatus: tPotionStatus);

  { Set up potions }
  procedure BuildAndShufflePotions;
end;

implementation

{ TGame }

uses UnitConst, UnitDisplay, UnitVars, UnitFunctions, UnitItem;

{ Default constructor }
constructor TGame.Create;
begin
end;

{ Standard constructor }
constructor TGame.Create(NewGame: Boolean; CharacterName: String);
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TGame.Create(''%s'')', [CharacterName]));

  try
    { Set up FOV stuff }
    mv := LIGHTRADIUS;
    mw := mv * mv;

    { Set up the game timers }
    TimerList := TObjectList.Create(True);

    { Switch off blocking timer until needed }
    Block := false;

    { Reset the depth delved }
    for Loop := Low(GDepthDelved) to High(GDepthDelved) do
      GDepthDelved[Loop] := 0;

    { Reset the monster kill numbers }
    for Loop := Low(MonsterKillNumbers) to High(MonsterKillNumbers) do
      MonsterKillNumbers[Loop] := 0;

    { Create the monster kill names }
    MonsterKills := TStringList.Create;
    MonsterKillsPlural := TStringList.Create;

    { If we are starting a new game... }
    if NewGame then
    begin
      { Create the character }
      Player := TCreature.Create;

      { Load the character from disk }
      Player.Load(CharacterName);
      
      { Reset the scroll munging }
      FillArrayWithRandom(ScrollNameArray);

      { Shuffle the Potions }
      BuildAndShufflePotions;

      { Reset the scroll IDs }
      For Loop := Low(ScrollIDed) to High(ScrollIDed) do
        ScrollIDed[Loop] := stUnknown;

      { Reset the potion IDs }
      For Loop := Low(PotionIDed) to High(PotionIDed) do
        PotionIDed[Loop] := poUnknown;


      { Reset the spells }
      Player.SelectedSpellSchool := -1;
      Player.SelectedSpell := -1;

      { Create the starting town dungeon }
      Dungeon := TDungeonLevel.Create(1, D_TOWN);

      { Create the space for storing the town dungeon }
      TownDungeon := TDungeonLevel.Create(1, 0);

      { Assign the images }
      FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_TOWN] as
        TDungeonBranch).BranchGraphics);

      { Generate the character's starting equipment }
      Player.GenerateAndEquipStartingEquipment;

      { Restock the shops }
      Dungeon.RestockShops(Player.Levels - 2);

      { Get the current start time }
      GameStartTime := Now;

      { Reset game turns }
      Turns := 0;

      { Reset the level feeling counter }
      LastLevelFeeling := 0;

      { Set the starting position }
      PlayerX := TOWNSTARTX;
      PlayerY := TOWNSTARTY;

      { Take a starting note }
      Player.TakeNote(Turns, '', nBegin, Dungeon);

      { Flag that we're now ingame }
      InGame := True;
    end
    else
    { Loading an existing game }
    begin
      { TODO: when saving and loading is implemented, this will change, but for
        now, mimic the new game code }

      { Create the character }
      Player := TCreature.Create;

      { Load the character from disk }
      Player.Load(CharacterName);

      { Reset the scroll munging }
      FillArrayWithRandom(ScrollNameArray);

      { Reset the scroll IDs }
      For Loop := Low(ScrollIDed) to High(ScrollIDed) do
        ScrollIDed[Loop] := stUnknown;

      { Reset the spells }
      Player.SelectedSpellSchool := -1;
      Player.SelectedSpell := -1;

      { Generate the character's starting equipment }
      Player.GenerateAndEquipStartingEquipment;

      { Get the current start time }
      GameStartTime := Now;

      { Reset game turns }
      Turns := 0;

      { Reset the level feeling counter }
      LastLevelFeeling := 0;

      { Create the starting town dungeon }
      Dungeon := TDungeonLevel.Create(1, D_TOWN);

      { Create the space for storing the town dungeon }
      TownDungeon := TDungeonLevel.Create(1, 0);

      { Restock the shops }
      Dungeon.RestockShops(1);

      { Assign the images }
      FormDisplay.ImageListTerrain.Assign((GDungeonBranchList[D_TOWN] as
        TDungeonBranch).BranchGraphics);

      { Set the starting position }
      PlayerX := TOWNSTARTX;
      PlayerY := TOWNSTARTY;

      { Flag that we're now ingame }
      InGame := True;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Default destructor }
destructor TGame.Destroy;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.Destroy');

  try
    { Free anything we've created }
    Player.Free;
    Player := nil;

    TimerList.Free;
    TimerList := nil;

    MonsterKills.Free;
    MonsterKillsPlural.Free;

    inherited;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ See if there are any monsters within line of sight }
function TGame.CanRest: Boolean;
var
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.CanRest');

  { Default result }
  Result := True;

  try
    { Look for nearby monsters }
    for X := topleftx to toprightx do
      for Y := bottomlefty to toplefty do
      begin
        { Skip tiles off the map }
        if X < 1 then
          continue;
        if X > DUNGEONSIZEX then
          continue;
        if Y < 1 then
          continue;
        if Y > DUNGEONSIZEY then
          continue;

        { If there is a monster visible then we can't rest }  
        if (Dungeon.Visible[X, Y] = 1) and (Dungeon.Monsters[X, Y] > 0) then
        begin
          Result := False;
          Break;
        end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ See if there are any awake monsters within line of sight }
function TGame.CanStealth: Boolean;
var
  LocalMonster: TMonster;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.CanStealth');

  { Default result }
  Result := True;

  try
    { Can't stealth in the town level }
    if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
    begin
      Result := False;
      Exit;
    end;

    { Look for nearby monsters }
    for X := topleftx to toprightx do
      for Y := bottomlefty to toplefty do
      begin
        { Skip tiles off the map }
        if X < 1 then
          continue;
        if X > DUNGEONSIZEX then
          continue;
        if Y < 1 then
          continue;
        if Y > DUNGEONSIZEY then
          continue;

        { If there is a monster visible and awake then we can't stealth }  
        if (Dungeon.Visible[X, Y] = 1) and (Dungeon.Monsters[X, Y] > 0) then
        begin
          LocalMonster := GMonsterList[Dungeon.Monsters[X, Y]] as TMonster;
          if (LocalMonster.Awake) and (LocalMonster.Alive) then
          begin
            Result := False;
            Break;
          end;
        end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set the view port }
procedure TGame.SetViewPort;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.SetViewPort');

  try
    { Set the current viewport boundaries }
    TopLeftX := PlayerX + TopLeft.X;
    TopLeftY := PlayerY + TopLeft.Y;
    TopRightX := PlayerX + TopRight.X;
    TopRightY := PlayerY + TopRight.Y;
    BottomLeftX := PlayerX + BottomLeft.X;
    BottomLeftY := PlayerY + BottomLeft.Y;
    BottomRightX := PlayerX + BottomRight.X;
    BottomRightY := PlayerY + BottomRight.Y;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set the view boundaries }
procedure TGame.SetViewBoundaries(ScreenWidth, ScreenHeight: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.SetViewBoundaries');

  try
    { Calculate the viewport boundaries }
    TopLeft := GetTopLeft(CurrentDrawMode, Point(ScreenWidth, ScreenHeight));
    TopRight := GetTopRight(CurrentDrawMode, Point(ScreenWidth, ScreenHeight));
    BottomLeft := GetBottomLeft(CurrentDrawMode, Point(ScreenWidth, ScreenHeight));
    BottomRight := GetBottomRight(CurrentDrawMode, Point(ScreenWidth, ScreenHeight));
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Add a monster kill to the monster kill list }
procedure TGame.LogMonsterKill(LocalMonster: TMonster);
var
  MonsterType: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.LogMonsterKill');

  try
    { Try and find the monster name in the previously killed list - we use this
      instead of uniquename to avoid counting uniques as different monsters }
    MonsterType := MonsterKills.IndexOf(LocalMonster.Name);

    { If we have found the name, increment the appropriate count }
    if MonsterType <> -1 then
      Inc(MonsterKillNumbers[MonsterType])
    else
    begin
      { A new monster therefore add it to the monster kill list }
      MonsterKills.Add(LocalMonster.Name);
      MonsterKillsPlural.Add(LocalMonster.PluralName);
      Inc(MonsterKillNumbers[MonsterKills.Count - 1]);
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set scroll status }
procedure TGame.SetScrollStatus(ScrollType: Integer; ScrollStatus:
  tScrollStatus);
var
  Loop: Integer;
  LoopItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.SetScrollStatus');

  try
    { First, iterate through any scrolls held in the inventory and set their
      IDed status - this is purely for display purposes }
    for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
    begin
      { Get the item }
      LoopItem := (GItemList[Player.Inventory[Loop]]) as TItem;

      { Check if its a scroll }
      if LoopItem.ItemType = iScroll then
      begin
        { Set the scroll status for the right scrolls only }
        if LoopItem.Position = ScrollType then
        begin
          if ScrollStatus in [stUnknown, stTried] then
            LoopItem.Known := False
          else if ScrollStatus = stKnown then
            LoopItem.Known := True;
        end;
      end;
    end;

    { Now iterate through the global item list }
    for Loop := 0 to GItemList.Count - 1 do
    begin
      { Get the item }
      LoopItem := (GItemList[Loop]) as TItem;

      { Check if its a scroll }
      if LoopItem.ItemType = iScroll then
      begin
        { Set the scroll status for the right scrolls only }
        if LoopItem.Position = ScrollType then
        begin
          if ScrollStatus in [stUnknown, stTried] then
            LoopItem.Known := False
          else if ScrollStatus = stKnown then
            LoopItem.Known := True;
        end;
      end;
    end;

    { Flag the scroll type }
    ScrollIDed[ScrollType] := ScrollStatus;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set up potions }
procedure TGame.BuildAndShufflePotions;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.BuildAndShufflePotions');

  try

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Set potion status }
procedure TGame.SetPotionStatus(PotionType: Integer;
  PotionStatus: tPotionStatus);
var
  Loop: Integer;
  LoopItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGame.SetPotionStatus');

  try
    { First, iterate through any scrolls held in the inventory and set their
      IDed status - this is purely for display purposes }
    for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
    begin
      { Get the item }
      LoopItem := (GItemList[Player.Inventory[Loop]]) as TItem;

      { Check if its a scroll }
      if LoopItem.ItemType = iPotion then
      begin
        { Set the potion status for the right potions only }
        if LoopItem.Position = PotionType then
        begin
          if PotionStatus in [poUnknown, poTried] then
            LoopItem.Known := False
          else if PotionStatus = poKnown then
            LoopItem.Known := True;
        end;
      end;
    end;

    { Now iterate through the global item list }
    for Loop := 0 to GItemList.Count - 1 do
    begin
      { Get the item }
      LoopItem := (GItemList[Loop]) as TItem;

      { Check if its a potion }
      if LoopItem.ItemType = iPotion then
      begin
        { Set the scroll status for the right scrolls only }
        if LoopItem.Position = PotionType then
        begin
          if PotionStatus in [poUnknown, poTried] then
            LoopItem.Known := False
          else if PotionStatus = poKnown then
            LoopItem.Known := True;
        end;
      end;
    end;

    { Flag the scroll type }
    PotionIDed[PotionType] := PotionStatus;  
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
