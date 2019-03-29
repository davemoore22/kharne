{ UnitDungeon

  Copyright (c) 2007-2009 Dave Moore 

  Dungeon Class and Dungeon Functions

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

unit UnitDungeon;

interface

uses SysUtils, Dialogs, Math, Graphics, Forms, Classes, Controls, Contnrs,
  Types, HotLog, UnitDefines, UnitConst, UnitVars, UnitItem, UnitMonster,
  UnitVault, UnitOtherFuncs;

{ TDigger is used to build cave levels }

{ TODO: replace with TPoint }
type
  TDigger = class                                  
public
	{ Public Members }
  X: Integer;
  Y: Integer;

  { Constructors }
  constructor Create(X1, Y1: Integer);
end;

{ TDungeonBranch holds the information that defines each dungeon stack }
type
  TDungeonBranch = class

public
	{ Public Members }
  BranchName: String;
  BranchTheme: Integer;
  BranchFileName: String;
  BranchGraphics: TImageList;
  BranchASCIIColours: Array[0..11] of TColor;
  BranchDepth: Integer;
  BranchStartingLevel: Integer;
  BranchEndingLevel: Integer;
  BranchDungeonBalance: Integer;
  BranchDungeonRoomsize: Integer;
  BranchCorridorLength: Integer;
  BranchRecurseDepth: Integer;
  BranchStairCount: Integer;
  BranchCrossCorridors: Boolean;
  BranchCorridorDoorChance: Integer;
  BranchMonsterDensity: Integer;
  BranchItemDensity: Integer;
  BranchItemCursed: Integer;
  BranchUniqueMonsterChance: Integer;
  BranchVaultChance: Integer;

  { Constructors }
  constructor Create;
  
  { Destructor }
  destructor Destroy; override;
  
  { Load Graphics associated with this branch }
  function LoadImageList(ImageList: TImageList; BitmapPath: String; 
  	ImageIndex: Integer = -1; Transparent: Boolean = True): Boolean;
end;
     

{ This class represents dungeon levels. There is always one instance created,
  which is the current dungeon level, called Dungeon - and others created
  for all the other levels, and these are copied into and out from Dungeon }
type
  TDungeonLevel = class

public
	{ Public Members }
	
	{ Level Name }
  Name: String;
  
  { Level Depth in the Stack }
  LevelDepth: Integer;
  
  { Level Offset - more difficult levels are offset by a certain value }
  LevelOffset: Integer;
  
  { Level Theme }
  LevelTheme: Integer;
  
  { Level Rating }
  LevelRating: Integer;
  
  { Resident Unique Monster }
  Unique: TMonster;
    
  { Map Layer Definitions }
  Visible: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Terrain: array [1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Effects: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Objects: array [1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Zone: array [1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Monsters: array [1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Walkable: array [1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of boolean;
  People: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  TerrainCost: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of integer;
  Projectiles: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of char;
  ProjectilesColour: array[1..DUNGEONSIZEX, 1..DUNGEONSIZEY] of TColor;

  { Stair Locations }
  StairsUpX: array[0..MAXSTAIRS] of Integer;
  StairsUpY: array[0..MAXSTAIRS] of Integer;
  StairsDownX: array[0..MAXSTAIRS] of Integer;
  StairsDownY: array[0..MAXSTAIRS] of Integer;
  StairsTaken: Integer;
  
private
	{ Private Members }

	{ Various arrays and variables used in level construction }
	
	{ The level constructon algorithm for creating standard levels is based upon
	  the one used in Angband }

  { Block Array - split the dungeon map into a certain number of blocks, and 
    put a room into each one - this is to ensure a fairly even distrubution 
    of rooms. This array contains an integer pointing to the Room coordinates 
    below }
  Block: array[0..BLOCK_NUM_X, 0..BLOCK_NUM_Y] of Integer;

  { Room Array }
  Rooms: array[0..DUN_ROOMS] of TPoint;
  RoomCount: Integer;

  { Hold Centre Points of Rooms created }
  Cent: array[0..DUN_ROOMS] of TPoint;
  CentCount: Integer;

  { Tunnel Array - used in level construction to hold candidate squares for
    tunnels }
  Tunnel: array[0..1000] of TPoint;
  TunnelCount: Integer;

  { Wall Array - used in level construction to hold candidate squares for
    walls }
  Wall: array[0..1000] of TPoint;
  WallCount: Integer;

  { Door Array - used in level construction to hold candidate squares for
    walls }
  Door: array[0..1000] of TPoint;
  DoorCount: Integer;

	{ Digger List - used in level construction for cave type levels}
  Digger: TObjectList;

	{ Place a room on the map }
  function PlaceRoom(RoomType: Integer; RoomSegment: Integer = -1): Boolean;

	{ Build a room at a specified location }
  function BuildRoom(roomx: Integer; roomy: Integer; roomtype: Integer;
    segment: Integer; var roomcentre: TPoint): Boolean;

	{ Build a tunnel from one location to another }
  procedure BuildTunnel(var row1: Integer; var col1: Integer; 
  	var row2: Integer; var col2: Integer);

	{ Count the number of adjacent corridor tiles to a square }
  function CountAdjacentCorridors(x1: Integer; y1: Integer): Integer;

	{ Count the number of adjacent floor tiles to a square }
  function CountAdjacentFloors(x1, y1: Integer): Integer;

  { Reset any twists and turns or kinks during tunnel creation }
  procedure FindCorrectDirection(var rdir: Integer; var cdir: Integer;
    y1: Integer; x1: Integer; y2: Integer; x2: Integer);

	{ Check if we're not trying to go off the edges of the map }
  function InDungeonBounds(x: Integer; y: Integer): Boolean;

	{ Pick a random direction and return the offsets }
  procedure PickRandomDirection(var rdir: Integer; var cdir: Integer);

	{ Try and place a door at a location }
  function TryPlaceDoor(x: Integer; y: Integer): Boolean;

public
	{ Standard constructor }
  constructor Create(ldepth:integer; ltheme: Integer);
   
  { Load the town level }
  procedure LoadTownLevel;
  
  { Reset the level }
  procedure Initialise;

  { Zone handling }
  procedure DrawZone(x:integer; y:integer; zoneid: integer; zonesize: integer;
  	maxtilesused: integer);
  	
  { Effect Zone creation }
  procedure DrawEffectZone(x:integer; y:integer; zoneid: integer; 
  	zonesize: integer; maxtilesused: integer);

  { Put Items on Level }
  procedure PlaceItemsOnLevel(ItemsWanted: integer);

  { Put Monsters on Level }
  procedure PlaceMonstersOnLevel(MonstersWanted: Integer);

  { Put Zones on Level }
  procedure PlaceZonesonLevel(ZonesWanted: Integer);

  { Put Vaults on Level }
  procedure PlaceVaultsonLevel(PercentageChanceOfGreaterVault: Integer);
  
  { Create and place and entry vault on the Level }
  procedure PlaceEntryVault(var StairLoc: TPoint);

  { Place effects on Level }
  procedure PlaceEffectsOnLevel(LevelTheme: Integer; EffectsWanted: integer);

  { Place Fountains }
  procedure PlaceFountainsOnlevel(FountainsWanted: Integer);

  { Level Feeling }
  function GetLevelFeeling: String;

  { Get the Unique Monster Text for the Level Feeling }
  function GetUniqueMonsterFeeling: String;

  { FOV Handling }
  procedure SetVisible(xpos: integer; ypos: integer; radius: integer);

	{ FOV Routines }
  procedure RecursiveVisibility(xpos1,ypos1: Integer; oct, depth: integer; 
  	slopeA, slopeB: single);
  function GetVisDistance(x1, y1, x2, y2: integer): integer;
  function GetSlopeInv(x1, y1, x2, y2: single): single;
  function GetSlopeStd(x1, y1, x2, y2: single): single;

  { Load the level from another TDungeonLevel object }
  procedure LoadLevel(Sourcelevel:TDungeonLevel);
  
  { Save the level to another TDungeonLevel object }
  procedure SaveLevel(Destinationlevel:TDungeonLevel);

	{ Remove a monster from the map after it has been killed }
  procedure RemoveMonsterFromMap(Loc: TPoint);
  
  { Find what stair was used previously to reach this level }
  function FindStairsTaken(X: Integer; Y: Integer; 
  	Direction: Integer): Integer;

	{ Get the number of items lying on a tile }
  function GetNumberofItemsOnTile(Location: TPoint): Integer;
  
  { Get the last item (i.e. bottommost) lying on a tile }
  function GetLastItemOnTile(Location: TPoint): TItem;
  
  { Get the description of all items lying on the tile }
  function GetDecriptionOfItemsOnTile(Location: TPoint): String;

	{ Restock the town level shops }
  procedure RestockShops(LevelToUse: Integer);

  { Regenerate the towns people }
  procedure RegenerateTownsPeople;

  { Count the number of adjacent walls to a square }
  function CountWalls(x, y: Integer): Integer;

  { Save any adjacent monsters }
  function SaveAdjacentMonsters: Integer;

  { Load any previous adjacent monsters from the previous level }
  procedure LoadAdjacentMonsters(PreviousLevel: TDungeonLevel;
    MonstersToPutDown: Integer);

end;

var
	{ TODO: move to UnitVars }
  BranchDungeons: array[1..10] of TDungeonLevel;
  CX: Integer;
  CY: Integer;
  CanCrossCorridors: Boolean;
  CorridorDoorChance: Integer;
  BaseRoomSize: Integer;
  BaseCorridorLength: Integer;
  NumberOfStairs: Integer;

implementation

uses UnitDisplay, Unitengine, UnitWizard, UnitFunctions;

{ TDungeonBranch }

{ Standard Constructor }
constructor TDungeonBranch.Create;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.Create()');

	try
		{ Set up the branch graphics }
  	BranchGraphics := TImageList.CreateSize(32, 32);
  	BranchGraphics.Masked := False;
  	
  	{ TODO: remember and assign images }
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Standard Destructor }
destructor TDungeonBranch.Destroy;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.Destroy()');
  
  try
		BranchGraphics.Free;
		BranchGraphics := nil;
		inherited Destroy;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;		
end;

{ Given a bitmap, load it, crack it open into various images (or just the one
  specified by the imageindex, and populate the specificed ImageList with 
  them/it }
function TDungeonBranch.LoadImageList(ImageList: TImageList;
  BitmapPath: String; ImageIndex: Integer; Transparent: Boolean): Boolean;
var
  Bitmap: TBitmap;
  LocalImageList: TImageList;
  LocalBitmap: TBitmap;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.LoadImageList()');
  
  { Default Result}
  Result := True;
  
  try
  	{ Check if the bitmap file exists }
		if FileExists(Format('%s\gfx\%s', [ExtractFilePath(Application.ExeName),
			BitmapPath])) then 
		begin
			{ Create the bitmap }
			Bitmap := TBitmap.Create;
			
			{ Load the bitmap from the file }
			Bitmap.LoadFromFile(Format('%s\gfx\%s', 
				[ExtractFilePath(Application.ExeName), BitmapPath]));
				
			{ Create a temporary imagelist to hold the graphics }
			LocalImageList := TImageList.CreateSize(32, 32);
			
			{ Set the temporary imagelist options }
			LocalImageList.BkColor := clNone;
			LocalImageList.Masked := False;
			
			{ Add the bitmap to the temporary imagelist }
			LocalImageList.Add(Bitmap, nil);
			
			{ Create a placeholder bitmap }
			LocalBitmap := TBitmap.Create;
			
			{ Clear the imagelist }
			ImageList.Clear;
			
			{ Check if a particular image has been specified }
			if ImageIndex <> -1 then
			begin
				{ Get that bitmap }
				LocalImageList.GetBitmap(ImageIndex, LocalBitmap);
				
				{ Check for transparency and add the bitmap to the imagelist }
				if Transparent = True then 
					ImageList.AddMasked(LocalBitmap, clDefault)
				else 
					ImageList.Add(LocalBitmap, nil);
			end
			else
			begin
				{ Load all bitmaps }
				for Loop := 0 to LocalImageList.Count - 1 do
				begin
					{ Get that bitmap }
					LocalImageList.GetBitmap(Loop, LocalBitmap);
					
					{ Check for transparency and add the bitmap to the imagelist }
					if Transparent = True then 
						ImageList.AddMasked(LocalBitmap, clDefault)
					else 
						ImageList.Add(LocalBitmap, nil);
				end;
			end;
			
			{ And free up the buffer and temporary bitmaps }
			LocalBitmap.Free;
			LocalBitmap := nil;
			LocalImageList.Free;
			LocalImageList := nil;
			Bitmap.Free;
			Bitmap := nil;
			Result := True;
		end
		else
			{ File to load doesn't exist so return an error condition }
			Result := False;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Standard Level Constructor }
constructor TDungeonLevel.Create(ldepth:integer; ltheme: Integer);
var
  x,y:integer;
  adjacent_corridors: Integer;
  door_loop: Integer;
  room_loop: Integer;
  count: Integer;
  cxfrom: Integer;
  cyfrom: Integer;
  cx: Integer;
  cy: integer;
  cells_created: Integer;
  random_digger: Integer;
  DunB: TDungeonBranch;
  Stair_Loop: Integer;
  DiggerPoint: TDigger;
  DiggerPos: TDigger;
  walls_around: Integer;
  dx, dy: Integer;
  pick1, pick2: Integer;
  randompoint: TPoint;
  NewStairLoc: TPoint;
  TempX: Integer;
  TempY: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.Create()');
  
	try
    { Set the cursor to busy }
    Screen.Cursor := crHourGlass;

		{ Reset the level }
    if LTheme <> D_NONE then
      Initialise;
	
		{ Set the unique for this level to nil }
    Unique := nil;

		{ We haven't taken any stairs yet, obviously }
    StairsTaken := -1;
    
   	{ Now handle building up the different types of level }
    if LTheme = D_TOWN then
    begin
    	{ This is the town level - hardcoded and loaded from a file }
      LoadTownLevel;
      
      { Load the dungeonbranch }
      DunB := GDungeonBranchList[LTheme] as TDungeonBranch;
      
      { Set the level name }
      Name := DunB.BranchName;
    end
    { Handle dungeon levels of various types }
    else if LTheme > D_TOWN then
    begin
    	{ First, we need to retrieve the info necessary from the appropriate
    		dungeon branch defintion objects }
    	DunB := GDungeonBranchList[LTheme] as TDungeonBranch;

    	{ Set up some of the properties of the dungeon level }
    	LevelDepth := LDepth;
    	Name := DunB.BranchName;
    	LevelTheme := LTheme;
    	LevelRating := 0;
    	LevelOffset := GetLeveLOffset(LevelTheme);
    	
    	{ Reset the stairs }
			for X := Low(Stairsdownx) To High(Stairsdownx) Do
			begin
				Stairsdownx[X] := 0;
				Stairsdowny[X] := 0;
			end;

			{ Check if we have the Wizard visble, if so, clear the log }
			if Assigned(FormWizard) then
				if FormWizard.Visible = True then
					FormWizard.MemoLog.Lines.Clear;

			{ Build the level. What type of level generation we use depends on the
			  level type }
			if (ltheme = D_KEEP) or (ltheme = D_FORTRESS) or (ltheme = D_CRYPT) then
			begin
				{ Angband-style Dungeon Generation }
				
				{ Reset the tunnel and room count }
				TunnelCount := 0;
				RoomCount := 0;

      	{ Fill the dungeon with hard walls }
				{for x:= 1 to DUNGEON_WID do
					for y:= 1 to DUNGEON_HGT do
						Terrain[x,y] := T_HARDWALL;  }

				{ Fill the inside area of the dungeon with softer (diggable) walls }
        if LevelTheme = D_ABYSS then
        begin
          for x:= 1 to DUNGEON_WID do
          begin
            for y:= 1 to DUNGEON_HGT do
            begin
              {if InDungeonBounds(x, y) then
                Terrain[x,y] := T_SOFTWALL; }

              { The abyss has a softer, less regular edge }
              if LevelTheme = D_ABYSS then
              begin
                if (((x = IN_BOUNDS_BORDER) or
                  (x = DUNGEON_WID - IN_BOUNDS_BORDER)) and (OneChanceIn(3))) then
                    Terrain[x,y] := T_SOFTWALL;
                if (((y = IN_BOUNDS_BORDER) or
                  (y = DUNGEON_HGT - IN_BOUNDS_BORDER)) and (OneChanceIn(3))) then
                    Terrain[x,y] := T_SOFTWALL;
              end;
            end;
          end;
        end;

				{ Place some rooms, one segment at a time. This will update the Rooms
					array and RoomCount }
				for room_loop := 0 to DUN_ROOMS do
				begin
					{ 33% chance of placing an overlapping room else place a nornal
						room }
					if OneChanceIn(3) then
						PlaceRoom(ROOM_OVERLAPPING)
					else
						PlaceRoom(ROOM_NORMAL);
				end;

				{ Reset the door count }
				DoorCount := 0;
				for x := Low(Door) to High(Door) do
				begin
					Door[x].X := 0;
					Door[x].Y := 0;
				end;

				{ Scramble the Room Order to make the corridors appear more random }
				for x := 0 to CentCount - 1 do
				begin
					pick1 := random(CentCount);
					pick2 := random(CentCount);
					randompoint := Cent[pick1];
					Cent[pick1] := Cent[pick2];
					Cent[pick2] := randompoint;
				end;

				{ Now connect the rooms together }
				cy := Cent[CentCount - 1].Y;
				cx := Cent[CentCount - 1].X;
				for room_loop := 0 to Centcount - 1 do
				begin
					cyfrom := Cent[room_loop].y;
					cxfrom := Cent[room_loop].x;
					BuildTunnel(cyfrom, cxfrom, cy, cx);
					cy := Cent[room_loop].y;
					cx := Cent[room_loop].x;
				end;

				{ Place the doors }
				for door_loop := 0 to DoorCount do
				begin
					{ Get the coordinate }
					x := Door[door_loop].X;
					y := Door[door_loop].Y;

					{ Try to place a door }
					
					{ TODO: check to see if this is placing too many doors }
					TryPlaceDoor(x - 1, y);
					TryPlaceDoor(x + 1, y);
					TryPlaceDoor(x, y - 1);
					TryPlaceDoor(x, y + 1);
				end;

				{ Make terrain walkable where appropriate }
				for x:= 1 to DUNGEONSIZEX  do
        begin
					for y:= 1 to DUNGEONSIZEY do
					begin
            { Skip over terrain we're not interested in }
            if Terrain[x, y] = T_SOFTWALL then
              continue;

						{ If we have a floor or an open door, make this walkable }
						if ((Terrain[x,y] = T_FLOOR_ROOM) or
						  (Terrain[x,y] = T_FLOOR_CORRIDOR) or 
						  (Terrain[x,y] = T_DOOR_OPEN)) then
						  Walkable[x, y] := True;

						{ Reset the wall types }
						if Terrain[x, y] = T_ROOMWALL then
							Terrain[x, y] := T_SOFTWALL;
						if ((InDungeonBounds(x, y)) and (Terrain[x, y] = T_HARDWALL)) then
              Terrain[x, y] := T_SOFTWALL;
					end;
        end;

				{ Scatter items on the level }
				PlaceItemsOnLevel(30);
				
				{ Scatter some optional zones about }
				PlaceZonesonLevel(random(2));
				
				{ Put monsters on the level }
				PlaceMonstersOnLevel(60);

				{ Place some effects on the level }
				PlaceEffectsOnLevel(LevelTheme, 60 + LDepth * 20);
				
				{ Place some vaults on the level (this is a percentage chance }
				PlaceVaultsonLevel(LDepth * 10);
				
				{ Place some optional fountains on the level }
				PlaceFountainsOnlevel(random(3));

      	{ Get the number of stairs to add }
      	NumberOfStairs := DunB.BranchStairCount;

				{ Now we place some stairs }
				for Stair_Loop := 0 to NumberOfStairs do
				begin
					{ Find somewhere to draw them on the map }
					repeat
						TempX := Random(DUNGEONSIZEX) + 1;
						TempY := Random(DUNGEONSIZEY) + 1;
					until (Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
						(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR);
						
					{ Place the up stairs }
					Terrain[TempX, TempY] := T_STAIRS_UP;
					Effects[TempX, TempY] := 0;
					
					{ Hack to zap any monster on the stair }
					RemoveMonsterFromMap(Point(TempX, TempY));
					
					{ Store the location of the stairs }
					StairsUpX[Stair_Loop] := TempX;
					StairsUpY[Stair_Loop] := TempY;

					{ And place some down stairs if we're not at the bottom level }
					if LDepth < MAXDUNGEONSTACKDEPTH then
					begin
						{ Find somewhere to draw them on the map }
						repeat
							TempX := Random(DUNGEONSIZEX) + 1;
							TempY := Random(DUNGEONSIZEY) + 1;
						until ((Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
							(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR));
							
						{ Place the down stairs }	
						Terrain[TempX, TempY] := T_STAIRS_DOWN;
						Effects[TempX, TempY] := 0;
						
						{ Hack to zap any monster on the stair }
						RemoveMonsterFromMap(Point(TempX, TempY));
						
						{ Store the location of the stairs }
						StairsDownX[Stair_Loop] := TempX;
						StairsDownY[Stair_Loop] := TempY;
					end;
				end;

				{ Place the entry vault on level 1 but only the first time we enter
				  a dungeon branch. Note that the fortress is the only starting level
				  of this type that can have an entry vault }
				if (LevelTheme = D_FORTRESS) and 
					(FormDisplay.Game.GDepthDelved[D_FORTRESS] = 0) then
				begin
					{ Place the vault around the stairs }
					NewStairLoc := Point(StairsUpX[0], StairsUpY[0]);
					PlaceEntryVault(NewStairLoc);
					
					{ The stair coordinates might need adjusting because of the vault
					  format }
					StairsUpX[0] := NewStairLoc.X;
					StairsUpY[0] := NewStairLoc.Y;
				end;
    	end
    	{ Cave-style dungeon generation } 	
			else if (ltheme = D_WILDERNESS) or (ltheme = D_ABYSS) or 
				(ltheme = D_AIR) or (ltheme = D_FIRE) or (ltheme = D_EARTH) or
				(ltheme = D_WATER) then
			begin
        { Create the Digger List }
        Digger := TObjectList.Create(False);
        
        { Now keep digging levels until we get a level that we want }
        repeat
          { Fill the dungeon with hard walls }
          for x := 1 to DUNGEON_WID do
            for y := 1 to DUNGEON_HGT do
              Terrain[x,y] := T_HARDWALL;

          { Fill the inside area of the dungeon with softer (diggable) walls }
          for x := 1 to DUNGEON_WID  do
            for y := 1 to DUNGEON_HGT do
              if (InDungeonBounds(x, y) = true) then
                Terrain[x,y] := T_SOFTWALL;
                
          { Keep a count of the cells dug out }
          cells_created := 0;

          { Start in the middle of the level and excavate out a cell }
          DiggerPoint := TDigger.Create(DUNGEON_WID div 2, DUNGEON_HGT div 2);

          { Add the cell to the list of seed cells }
          Digger.Add(DiggerPoint);

          { Dig out the cell }
          Terrain[DiggerPoint.X, DiggerPoint.Y] := T_FLOOR_ROOM;

          { Increment the cell dug counter }
          inc(cells_created);

          { Whilst we have seed cells to dig }
          while (Digger.Count <> 0) do
          begin
            { Get a random cell from the list of seed cells }
            random_digger := Random(Digger.Count);

            { Use it to dig out more cells }
            DiggerPos := Digger.Items[random_digger] as TDigger;

            { Delete the cell from the list of seed cells now we've used it }
            Digger.Delete(random_digger);

            { Check if the cell is inbounds }
            if InDungeonBounds(DiggerPos.X, DiggerPos.Y) then
            begin
              { Get the number of walls (unexcavated cells) around this square }
              walls_around := 0;
              for X := -1 to 1 do
              begin
                for Y := -1 to 1 do
                begin
                  { Don't check the cell itself }
                  if ((X = 0) and (Y = 0)) then
                    continue;

                  { Check all adjacent cells }
                  if Terrain[DiggerPos.X + X, DiggerPos.Y + Y] = T_SOFTWALL then
                    inc(walls_around);
                end;
              end;

              { The Abyss is excavated much wider then other levels }
              if ltheme = D_ABYSS then
                { Only continue to excavate if we have more than a certain number
                  of walls surrounding this cell }
              begin
                if walls_around < 5 then
                  continue;
              end
              else
              begin
               { Only continue to excavate if we have more than a certain number
                  of walls surrounding this cell}
                if walls_around < (5 + random(2)) then
                  continue;
              end;

              { Dig out two cells next to an excavated cell }
              count := 0;

              { Find two cells if possible }
              while count < 2 do
              begin
                { Find an adjacent cell }
                dx := DiggerPos.X + (random(3) - 1);
                dy := DiggerPos.Y + (random(3) - 1);

                { Check if the cell is undug }
                if (Terrain[dx, dy] = T_SOFTWALL) then
                begin
                  { Add the cell to the list of seed cells }
                  DiggerPoint := TDigger.Create(dx, dy);
                  Digger.Add(DiggerPoint);

                  { Dig the cell out }
                  Terrain[dx, dy] := T_FLOOR_ROOM;

                  { Keep a track of the number of cells we've dug out }
                  inc(count);
                  inc(cells_created);
                end;
              end;
            end;
          end;

          wizardlog(IntToStr(cells_created));

          Digger.Clear;

        { Keep going until 500 cells have been dug out }
        until (cells_created > 500);

        { Clear the digger buffer }
        Digger.Free;
        Digger := nil;

        { Now do some tidyup and remove most isolated unexcavated cells }
        for x:= 1 to DUNGEONSIZEX  do
        begin
          for y:= 1 to DUNGEONSIZEY do
          begin
            if InDungeonBounds(x, y) then
            begin
              { If we have an undug cell }
              if Terrain[X, Y] = T_SOFTWALL then
              begin
                { Get the number of adjacent corridors }
                adjacent_corridors := countadjacentfloors(x, y);

                { dig out isolated cells }
                if adjacent_corridors > 2 then
                  Terrain[x,y] := T_FLOOR_ROOM;
              end;
            end;
          end;
        end;

        { Iterate over the freshly dug map making all excavated terrain
          walkable }
        for x:= 1 to DUNGEONSIZEX  do
        begin
          for y:= 1 to DUNGEONSIZEY do
          begin
            { If we have a floor or an open door, make this walkable }
            if ((Terrain[x,y] = T_FLOOR_ROOM) or
                (Terrain[x,y] = T_FLOOR_CORRIDOR) or
                (Terrain[x,y] = T_DOOR_OPEN)) then
              Walkable[x, y] := True;

            { Change one to standard type of wall }
            if Terrain[x, y] = T_ROOMWALL then
              Terrain[x, y] := T_SOFTWALL;

            { Remove any spurious undiggable walls }
            if InDungeonBounds(x, y) then
              if (Terrain[x, y] = T_HARDWALL) then
                Terrain[x, y] := T_SOFTWALL;
          end;
        end;

				{ Scatter items on the level }
				PlaceItemsOnLevel(cells_created div 60);
				
				{ Scatter some optional zones about }
				PlaceZonesonLevel(random(2));
				
				{ Put monsters on the level }
				PlaceMonstersOnLevel(cells_created div 60);
				
				{ Place some effects on the level }
				PlaceEffectsOnLevel(LevelTheme, 60 + LDepth * 20);

				{ Place some vaults on the level (this is a percentage chance with the
				  Abyss having more chance of vaults }
				if LevelTheme = D_ABYSS then 
					PlaceVaultsonLevel(LDepth * 20) 
				else 
					PlaceVaultsonLevel(LDepth * 10);
					
				{ Place some optional fountains on the level, but not on Elemental
				  levels }
				if (LevelTheme = D_ABYSS) or (LevelTheme = D_WILDERNESS) then 
					PlaceFountainsOnlevel(random(3));

				{ Get the number of stairs to add }
				NumberOfStairs := DunB.BranchStairCount;

      	{ Now we place some stairs }
      	for Stair_Loop := 0 to NumberOfStairs do
      	begin
        	{ Find somewhere to draw them on the map }
        	repeat
          	TempX := Random(DUNGEONSIZEX) + 1;
          	TempY := Random(DUNGEONSIZEY) + 1;
        	until (Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
        		(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR);
        		
        	{ Place the up stairs }
        	Terrain[TempX, TempY] := T_STAIRS_UP;
        	Effects[TempX, TempY] := 0;
        	
        	{ Hack to zap any monster on the stair }
        	RemoveMonsterFromMap(Point(TempX, TempY));
        	
        	{ Store the location of the stairs }
        	StairsUpX[Stair_Loop] := TempX;
        	StairsUpY[Stair_Loop] := TempY;

       		{ And place some down stairs if we're not at the bottom level }
					if LDepth < MAXDUNGEONSTACKDEPTH then
					begin
						{ Find somewhere to draw them on the map }
						repeat
							TempX := Random(DUNGEONSIZEX) + 1;
							TempY := Random(DUNGEONSIZEY) + 1;
						until (Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
							(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR);
						
						{ Place the down stairs }	
						Terrain[TempX, TempY] := T_STAIRS_DOWN;
						Effects[TempX, TempY] := 0;
						
						{ Hack to zap any monster on the stair }
						RemoveMonsterFromMap(Point(TempX, TempY));
						
						{ Store the location of the stairs }
						StairsDownX[Stair_Loop] := TempX;
						StairsDownY[Stair_Loop] := TempY;
					end;
				end;

      	{ Place the entry vault on level 1 but only the first time we enter
				  a dungeon branch. Note that the wildrness is the only starting level
				  of this type that can have an entry vault }
				if (LevelTheme = D_WILDERNESS) and 
					(FormDisplay.Game.GDepthDelved[D_WILDERNESS] = 0) then
				begin
					{ Place the vault around the stairs }
					NewStairLoc := Point(StairsUpX[0], StairsUpY[0]);
					PlaceEntryVault(NewStairLoc);
					
					{ The stair coordinates might need adjusting because of the vault
					  format }
					StairsUpX[0] := NewStairLoc.X;
					StairsUpY[0] := NewStairLoc.Y;
				end;
    	end
    end
    else
    { Default empty dungeon creation }
    begin
      { Do nothing here }
    end;

    { Set the cursor back to normal }
    Screen.Cursor := crDefault;

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Put Zones on Level }
procedure TDungeonLevel.PlaceZonesonLevel(ZonesWanted: Integer);
var
  Loop: Integer;
  TempX: Integer;
  TempY: Integer;
  ZoneToUse: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceZonesonLevel()');
  
	try
		{ First randomly figure out a zone type to use }
		for Loop := 1 to ZonesWanted do
		begin
			repeat
				ZoneToUse := Random(6) + 1;
				if ZoneToUse > 2 then
					{ See Z_ Constants in UnitConst for why we increase these ones} 
					Inc(ZoneToUse, 3); 
			until ZoneToUse <> LevelTheme;

			{ Find a good starting place for this }
			repeat
				TempX := Random(DUNGEONSIZEX) + 1;
				TempY := Random(DUNGEONSIZEY) + 1;
			until (Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
				(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR);

			{ Draw the zone }
			DrawZone(TempX, TempY, ZoneToUse, Random(ZONE_SIZE) + 8, ZONE_TILES_MAX);

			{ Add the zone info if we have the wizard screen displayed }
			WizardLog(Format('Placing Zone of Type %d at %d/%d', [ZoneToUse, TempX, 
				TempY]));
  	end;
  except
	    { in case of error, log the Exception }
	    on E: Exception do hLog.AddException(E);
  end;
end;

{ Put Monsters on Level }
procedure TDungeonLevel.PlaceMonstersOnLevel(MonstersWanted: Integer);
var
  Counter: Integer;
  Ecology: String;
  LevelOffset: Integer;
  Loop: Integer;
  MonsterType: TMonsterArchetype;
  Monster: TMonster;
  MonsterNamePrefix: String;
  Population: Integer;
  PopulationLoop: Integer;
  X: Integer;
  XOffset: Integer;
  Y: Integer;
  YOffset: Integer;
  ZoneName: String;
  UniqueFound: Boolean;
  LevelDifference: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceMonstersOnLevel()');
  
	try		
		{ Get the level ecology and difficulty }
		Ecology := GetEcology(LevelTheme);
		LevelOffset := GetLeveLOffset(LevelTheme);

		{ 90% of monsters placed are the common ones }
		for Loop := 0 to (MonstersWanted div 10) * 9 do
		begin
			{ 80% of these monsters are specific to the ecology }
			if Random(PERCENTAGE) < 80 then
				MonsterType := GetRandomMonsterArchetype(LevelOffset + 
					(LevelDepth div 2), Ecology, 'Common', True)
			{ 20% of these monsters are common }
			else
				MonsterType := GetRandomMonsterArchetype(LevelOffset + 
					(LevelDepth div 2), 'Common', 'Common', True);

			{ Figure out how many of each monster we have to place, and adjust  }
			Population := MonsterType.Population * 2;
			
			{ The Suffix is used whenever we place out-of-branch monsters into zones
			  and we need to differentiate them from normal monsters of that type }
			MonsterNamePrefix := '';

			{ For each monster we need to place }
			for PopulationLoop := 1 to (Random(Population div 2) + Population) do
			begin
				{ First deal with the base monster, and then other monsters in the
				  population are placed relative to this }
				if PopulationLoop = 1 then
				begin
					{ Find a good place to put this monster }
					repeat
						X := Random(DUNGEONSIZEX) + 1;
						Y := Random(DUNGEONSIZEY) + 1;
					until ((Terrain[X, Y] = T_FLOOR_ROOM) or
						(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Monsters[X, Y] = 0);

					{ Override the monster type slightly for zones }
					if Zone[X, Y] > 0 then
					begin
						{ Find the zone type }
						case Zone[X, Y] of
							Z_ABYSS: ZoneName := 'Abyss';
							Z_NEGATIVE: ZoneName := 'Mausoleum';
							Z_EARTH: ZoneName := 'Earth';
							Z_AIR: ZoneName := 'Air';
							Z_FIRE: ZoneName := 'Fire';
							Z_WATER: ZoneName := 'Water';
						end;

						{ Figure out the common/rare distribution as before }
						if Random(PERCENTAGE) < 80 then
							MonsterType := GetRandomMonsterForEcology(ZoneName, 'Common')
						else
							MonsterType := GetRandomMonsterForEcology(ZoneName, 'Rare');

						{ Give the monster a suitable prefix }
						if (Random(2) = 0) then 
							MonsterNamePrefix := ' Vanguard' 
						else 
							MonsterNamePrefix := ' Intruder';
					end;

					{ Create the Monster }
          if (Population > 2) and (OneChanceIn(4)) then
            Monster := TMonster.Create(MonsterType.ID, False, True)
          else
            Monster := TMonster.Create(MonsterType.ID);
					
					{ Add the zone suffix if necessary }
					Monster.Name := Monster.Name + MonsterNamePrefix;

					{ If it is a zone monster we need to scale it }
					if (Zone[X, Y] > 0) then
          begin
            { Work out how much we need to scale the monster to make it suitable
              for the current zone }
            if Monster.Level > (LevelDepth + LevelOffset) then
              LevelDifference :=  0 - (Monster.Level - (LevelDepth + LevelOffset))
            else
              LevelDifference := (LevelDepth + LevelOffset) - Monster.Level;

            { And scale the monster for the zone }
            Monster.BoostLevel(LevelDifference);
          end;

					{ Add the Monster }
					Monster.X := X;
					Monster.Y := Y;
					Monsters[X, Y] := GMonsterList.Count;
					GMonsterList.Add(Monster);
				end
				else
				begin
					{ Now for any other monsters, we add them in a random position 
						nearby if we can }
						
					{ Set up a Paranoia Counter }
					Counter := 0;
					repeat
						{ Try and clump them within 3 squares of the original monster }
						XOffset := X + Random(7) - 3; 
						YOffset := Y + Random(7) - 3;
						inc (Counter); 
					until (((Terrain[X, Y] = T_FLOOR_ROOM) or 
						(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Monsters[X, Y] = 0)
						or (Counter > 100));
						
					{ If we've been successful in finding a space, add the monster }
					if Counter < 100 then
					begin

						{ Override for zones - don't bother with additional monsters since
						  zone monsters are harder than normal }
						if Zone[X, Y] = 0 then
						begin
							{ Create the monster }
							Monster := TMonster.Create(MonsterType.ID);
							
							{ Add the monster }
							Monster.X := XOffset;
							Monster.Y := YOffset;
							Monsters[XOffset, YOffset] := GMonsterList.Count;
							GMonsterList.Add(Monster);
						end;
					end;
				end;
			end
		end;

		{ 10% of monsters placed are rares }
		for Loop := 0 to (MonstersWanted div 10) * 1 do
		begin
			{ Rare monsters are always solo }
			MonsterType := 
				GetRandomMonsterArchetype(LevelOffset + (LevelDepth div 2), Ecology, 
				'Rare', True);

			{ Create the Monster }
			Monster := TMonster.Create(MonsterType.ID);

			{ Locate and place the monster }
			repeat
				X := Random(DUNGEONSIZEX) + 1;
				Y := Random(DUNGEONSIZEY) + 1;
			until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
				(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Monsters[X, Y] = 0);

			{ Add the Monster }
			Monster.X := X;
			Monster.Y := Y;
			Monsters[X, Y] := GMonsterList.Count;
			GMonsterList.Add(Monster);
		end;

		{ There is a 25% chance of generating a unique per level }
		if OneChanceIn(4) then
		begin
			{ Get the monster type, which can be unique on a level basis }
			MonsterType := GetRandomUniqueMonster(LevelOffset + (LevelDepth div 2), 
				UniqueFound);
				
			{ Not all monster types have uniques, but if we do }
			if UniqueFound then
			begin
				{ Create the Monster }
				Monster := TMonster.Create(MonsterType.ID, True);

				{ Locate and place the monster }
				repeat
					X := Random(DUNGEONSIZEX) + 1;
					Y := Random(DUNGEONSIZEY) + 1;
				until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
					(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Monsters[X, Y] = 0);

				{ Add the Monster }
				Monster.X := X;
				Monster.Y := Y;
				Monsters[X, Y] := GMonsterList.Count;
				GMonsterList.Add(Monster);

				{ Set the unique monster of the dungeon to this monster }
				Unique := Monster;

				{ Add the monster info if we have the wizard screen displayed }
				WizardLog(Format('Generated Unique: %s at %d/%d', [Monster.Name, X, Y]));
			end;
		end;
  except
	  { in case of error, log the Exception }
	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Put Items on Level }
procedure TDungeonLevel.PlaceItemsOnLevel(ItemsWanted: Integer);
var
  Cursed: Boolean;
  Item: TItem;
  ItemLevel: Integer;
  ItemQuality: crItemQuality;
  ItemQualityRatio: Integer;
  ItemType: crItemType;
  LevelOffset: Integer;
  Loop: Integer;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceItemsOnLevel()');
  
	try	
		{ TODO: this can be refactored further }
	
		{ Work out the base item level we work with }
  	LevelOffset := GetLeveLOffset(LevelTheme);

		{ Generate weapons - 10% of items created }
		for Loop := 0 to (ItemsWanted div 10) * 1 do
		begin
			{ Find a location for the item }
			repeat
				X := Random(DUNGEONSIZEX) + 1;
				Y := Random(DUNGEONSIZEY) + 1;
			until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
				(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

			{ Calculate a random quality }
			ItemQualityRatio := Random(100) + 1;
			
			{ See if we want a cursed item }
			Cursed := (Random(100) + 1) < CHANCECURSED;
			
			{ Generate the item level }
			ItemLevel := LevelOffset + LevelDepth + (Random(3) - 1);
			
			{ Make sure we have a lower bound on item level }
			if ItemLevel = 0 then 
				ItemLevel := 1;

			{ Set the item quality }
			if ItemQualityRatio = 1 then 
				ItemQuality := iArtifact
			else if ItemQualityRatio < 5 then 
				ItemQuality := iEpic
			else if ItemQualityRatio < 12 then 
				ItemQuality := iLegendary
			else if ItemQualityRatio < 25 then 
				ItemQuality := iSuperb
			else 
			begin
				ItemQuality := iCommon;
				ItemLevel := 0;
			end;
			
			{ Set the item type }
			ItemType := iWeapon;

			{ Create the item }
			Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed);
			
			{ Increase the level rating (used for level feelings }
			case Item.ItemQuality of
				iSuperb: Inc(LevelRating, 1);
				iLegendary: Inc(LevelRating, 50);
				iEpic: Inc(LevelRating, 250);
				iArtifact: Inc(LevelRating, 1000);
			end;
			
			{ Place the item onto the level }
			Item.Location := iFloor;
			GItemList.Add(Item);
			Objects[X, Y] := GItemList.Count - 1;

			{ Add the item info if we have the wizard screen displayed }
			if (Item.ItemQuality = iArtifact) then
				WizardLog(Format('Generated Artifact: %s at %d/%d', 
				[Item.Name, X, Y]));
		end;
		
	{ Generate armour - 10% of items created }
	for Loop := 0 to (ItemsWanted div 10) * 1 do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

		{ Calculate a random quality }
		ItemQualityRatio := Random(PERCENTAGE) + 1;

		{ See if we want a cursed item }
		Cursed := (Random(PERCENTAGE) + 1) < CHANCECURSED;

		{ Generate the item level }
		ItemLevel := LevelOffset + LevelDepth + (Random(3) - 1);

		{ Make sure we have a lower bound on item level }
		if ItemLevel = 0 then 
			ItemLevel := 1;

		{ Set the item quality }
		if ItemQualityRatio = 1 then 
			ItemQuality := iArtifact
		else if ItemQualityRatio < 5 then 
			ItemQuality := iEpic
		else if ItemQualityRatio < 12 then 
			ItemQuality := iLegendary
		else if ItemQualityRatio < 25 then 
			ItemQuality := iSuperb
		else 
		begin
			ItemQuality := iCommon;
			ItemLevel := 0;
		end;

		{ Set the item type }
		ItemType := iArmour;

		{ Create the item }
		Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed);

		{ Increase the level rating (used for level feelings }
		case Item.ItemQuality of
			iSuperb: Inc(LevelRating, 1);
			iLegendary: Inc(LevelRating, 50);
			iEpic: Inc(LevelRating, 250);
			iArtifact: Inc(LevelRating, 1000);
		end;

		{ Place the item onto the level }
		Item.Location := iFloor;
		GItemList.Add(Item);
		Objects[X, Y] := GItemList.Count - 1;

		{ Add the item info if we have the wizard screen displayed }
		if (Item.ItemQuality = iArtifact) then
			WizardLog(Format('Generated Artifact: %s at %d/%d', 
			[Item.Name, X, Y]));
		end;	
		
	{ Generate jewellery - 0 to 2 per level }
	for Loop := 0 to Random(3) do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

		{ Calculate a random quality }
		ItemQualityRatio := Random(PERCENTAGE) + 1;

		{ See if we want a cursed item }
		Cursed := (Random(PERCENTAGE) + 1) < CHANCECURSED;

		{ Generate the item level }
		ItemLevel := LevelOffset + LevelDepth + (Random(3) - 1);

		{ Make sure we have a lower bound on item level }
		if ItemLevel = 0 then 
			ItemLevel := 1;

		{ Set the item quality }
		if ItemQualityRatio = 1 then 
			ItemQuality := iArtifact
		else if ItemQualityRatio < 5 then 
			ItemQuality := iEpic
		else if ItemQualityRatio < 12 then 
			ItemQuality := iLegendary
		else
			ItemQuality := iSuperb;

		{ Set the item type }
		if OneChanceIn(2) then 
			ItemType := iRing 
		else 
			ItemType := iAmulet;

		{ Create the item }
		Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed);

		{ Increase the level rating (used for level feelings }
		case Item.ItemQuality of
			iSuperb: Inc(LevelRating, 1);
			iLegendary: Inc(LevelRating, 50);
			iEpic: Inc(LevelRating, 250);
			iArtifact: Inc(LevelRating, 1000);
		end;

		{ Place the item onto the level }
		Item.Location := iFloor;
		GItemList.Add(Item);
		Objects[X, Y] := GItemList.Count - 1;

		{ Add the item info if we have the wizard screen displayed }
		if (Item.ItemQuality = iArtifact) then
			WizardLog(Format('Generated Artifact: %s at %d/%d', 
			[Item.Name, X, Y]));
		end;

	{ Generate potions - 20% of items created }
	for Loop := 0 to (ItemsWanted div 10) * 3 do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

		{ Potions have fixed properties }
		Cursed := False;
		ItemLevel := 0;
		ItemQuality := iCommon;

		{ Set the item type }
		ItemType := iPotion;

		{ Create a random potion }
    if Loop mod 2 = 0 then
		  Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed)
    else
      Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed, True);

		{ Place the item onto the level }
		Item.Location := iFloor;
		GItemList.Add(Item);
		Objects[X, Y] := GItemList.Count - 1;
	end;	

	{ TODO: implement generation of Wands }

	{ Generate scrolls - 20% of items created }
	for Loop := 0 to (ItemsWanted div 10) * 3 do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

		{ Potions have fixed properties }
		Cursed := False;
		ItemLevel := 0;
		ItemQuality := iCommon;

		{ Set the item type }
		ItemType := iScroll;

		{ Create the item }
    if Loop mod 2 = 0 then
		  Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed)
    else
      Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed, True);

		{ Place the item onto the level }
		Item.Location := iFloor;
		GItemList.Add(Item);
		Objects[X, Y] := GItemList.Count - 1;
	end;	

	{ Generate food - 5 to 10 on each level }
	for Loop := 0 to (Random(5) + 5) do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR )) and (Objects[X, Y] = 0);

		{ Potions have fixed properties }
		Cursed := False;
		ItemLevel := 0;
		ItemQuality := iCommon;

		{ Set the item type }
		ItemType := iConsumable;

		{ Create the item }
		Item := TItem.Create(ItemQuality, ItemLevel, ItemType, Cursed);


		{ Food is always identified }
		Item.Known := True;
		
		{ Place the item onto the level }
		Item.Location := iFloor;
		GItemList.Add(Item);
		Objects[X, Y] := GItemList.Count - 1;
	end;			

	{ Generate cash - how much is dependent upon charisma }
	
	{ Gold }
	for Loop := 0 to (FormDisplay.Game.Player.Charisma div 3) do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Objects[X, Y] = 0);

		{ Set up the item on the floor, but since this is an abstract item, we
		  don't need to set up any further properties }
		Objects[X, Y] := ITEM_GOLD;
	end;

	{ Silver }
	for Loop := 0 to (FormDisplay.Game.Player.Charisma div 2) do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Objects[X, Y] = 0);
			
		{ Set up the item on the floor, but since this is an abstract item, we
		  don't need to set up any further properties }
		Objects[X, Y] := ITEM_SILVER;
	end;

	{ Bronze }
	for Loop := 0 to (FormDisplay.Game.Player.Charisma) do
	begin
		{ Find a location for the item }
		repeat
			X := Random(DUNGEONSIZEX) + 1;
			Y := Random(DUNGEONSIZEY) + 1;
		until ((Terrain[X, Y] = T_FLOOR_ROOM) or 
			(Terrain[X, Y] = T_FLOOR_CORRIDOR)) and (Objects[X, Y] = 0);
			
		{ Set up the item on the floor, but since this is an abstract item, we
		  don't need to set up any further properties }
		Objects[X, Y] := ITEM_BRONZE;
	end;
  except
	  { in case of error, log the Exception }
	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Load the town level }
procedure TDungeonLevel.LoadTownLevel;
var
  FileLine: String;
  FileLineStringList: TStringList;
  Column: Integer;
  Row: Integer;
  StartX: Integer;
  StartY: Integer;
  TerrainToSet: Integer;
  TownFile: Text;
  TownFileName: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.LoadTownLevel()');
  
	try	
		{ Generate the starting point }
		StartX := (DUNGEONSIZEX - SPECIALLEVELSIZEX) div 2;
		StartY := (DUNGEONSIZEY - SPECIALLEVELSIZEY) div 2;
		
		{ Generate the buffer to store the town data }
		FileLineStringList := TStringList.Create;

		{ Get the filename }
		TownFileName := ExtractFilePath(Application.ExeName) + 'info\nexus.dat';
		
		{ Open the file }
		AssignFile(TownFile, TownFileName);
		Reset(TownFile);
		Row := 0;
		
		{ For each line of the file }
		while not(Eof(TownFile)) do
		begin
			{ Read in each line of the file }
			ReadLn(TownFile, FileLine);
			
			{ Skip Comment Lines }
			if Pos('//', FileLine) = 0 then
			begin
				{ Skip Blank Lines }
				if Trim(FileLine) <> '' then
				begin
					{ Break down each line into elements }
					FileLineStringList.Clear;
					FileLineStringList.CommaText := FileLine;
					
					{ For each element in each line }
					for Column := 0 to FileLineStringList.Count - 1 do
					begin
						{ The town is all visible by default }					
						Visible[StartX + Column, StartY + Row] := 1;
						
						{ If we have a value then set the terrain }
						if Length(Trim(FileLineStringList[Column])) <> 0 then
						begin
							{ Do the background }
							if StrToInt(FileLineStringList[Column]) = 0 then
							begin
								{ Handle planar sparkles in the space around the town }
								if OneChanceIn(5) then 
									TerrainToSet := D_TILE_SPARKLE
								else 
									TerrainToSet := StrToInt(FileLineStringList[Column]);
							end
							else
								TerrainToSet := StrToInt(FileLineStringList[Column]);
								
							{ Set the terrain }
							Terrain[StartX + Column, StartY + Row] := TerrainToSet;
							
							{ Set certain areas to be walkable }
							if ((TerrainToSet = 2) or	((TerrainToSet > 8) and 
								(TerrainToSet < 19)) or (TerrainToSet > 20)) then
								Walkable[StartX + Column, StartY + Row] := True;
						end
						{ If no value specified, blank out the terrain }
						else
							Terrain[StartX + Column, StartY + Row] := 0;
					end;
					Inc(Row);
				end;
			end;
		end;
		
		{ Free the buffer }
		FileLineStringList.Free;
		FileLineStringList := nil;  
  except
	  { in case of error, log the Exception }
	   on E: Exception do hLog.AddException(E);
  end;
end;

{ FOV Handling - uses Recursive Shadowcasting }
procedure TDungeonLevel.SetVisible(xpos: integer; ypos: integer; 
	radius: integer);
var
  x, y:integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.SetVisible()');
  
	try	
		{ Don't bother do this on the town level } 	
		if LevelTheme <> D_TOWN then
		begin
      { Check for player character blindness }
      if FormDisplay.Game.Player.Has(BLINDED) then
      begin
        { First set anything currently visible to 0 }
        for x := FormDisplay.Game.topleftx to FormDisplay.Game.toprightx do
          for y := FormDisplay.Game.bottomlefty to FormDisplay.Game.toplefty do
            Visible[x,y] := 0;

        { Set the current square to visible and that's it}
        Visible[xpos,ypos] := 1;
      end
      else
      begin
        { First set anything currently visible to 2 }
        for x := FormDisplay.Game.topleftx to FormDisplay.Game.toprightx do
        begin
          for y := FormDisplay.Game.bottomlefty to FormDisplay.Game.toplefty do
          begin
            if InDungeonBounds(X, Y) then
              if Visible[x,y] = 1 then
                Visible[x,y] := 2;
          end;
        end;

        { Now work out what we can see octant by octant }
        RecursiveVisibility(xpos, ypos, 1, 1, 1, 0);
        RecursiveVisibility(xpos, ypos, 6, 1, -1, 0);
        RecursiveVisibility(xpos, ypos, 8, 1, 1, 0);
        RecursiveVisibility(xpos, ypos, 7, 1, -1, 0);
        RecursiveVisibility(xpos, ypos, 2, 1, -1, 0);
        RecursiveVisibility(xpos, ypos, 5, 1, 1, 0);
        RecursiveVisibility(xpos, ypos, 3, 1, -1, 0);
        RecursiveVisibility(xpos, ypos, 4, 1, 1, 0);

        { Set the current square to visible }
        Visible[xpos,ypos] := 1;
      end;
   	end; 

  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

 { Load the level from another TDungeonLevel object }
procedure TDungeonLevel.LoadLevel(Sourcelevel: TDungeonLevel);
var
  x, y: integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.LoadLevel()');
  
	try	
		{ Load the basic properties of the level }
		Name := Sourcelevel.Name;
		LevelDepth := Sourcelevel.LevelDepth;
		LevelTheme := Sourcelevel.LevelTheme;
		LevelRating := SourceLevel.LevelRating;

		{ Load the stairs }
		StairsTaken := SourceLevel.StairsTaken;

		{ Load the unique }
		Unique := SourceLevel.Unique;

		{ Load the map layers }
		for x:=1 to (DUNGEONSIZEX) do
    begin
			for y:=1 to (DUNGEONSIZEY) do
      begin
        Visible[x,y] := SourceLevel.Visible[x,y];
        Terrain[x,y] := SourceLevel.Terrain[x,y];
        Monsters[x,y] := SourceLevel.Monsters[x,y];
        Objects[x,y] := SourceLevel.Objects[x,y];
        Walkable[x,y] := SourceLevel.Walkable[x,y];
        Effects[x,y] := SourceLevel.Effects[x,y];
        People[x,y] := SourceLevel.People[x,y];
        Zone[x,y] := SourceLevel.Zone[x,y];
        TerrainCost[x,y] := SourceLevel.TerrainCost[x,y];
        Projectiles[x,y] := SourceLevel.Projectiles[x,y];
        ProjectilesColour[x,y] := SourceLevel.ProjectilesColour[x,y];
      end;
    end;

		{ Load the stairs }
		for x:=0 to MAXSTAIRS do
		begin
			StairsUpX[x] := SourceLevel.StairsUpX[x];
			StairsUpY[x] := SourceLevel.StairsUpY[x];
			StairsDownX[x] := SourceLevel.StairsDownX[x];
			StairsDownY[x] := SourceLevel.StairsDowny[x];
		end;  
  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Save the level to another TDungeonLevel object }
procedure TDungeonLevel.SaveLevel(DestinationLevel: TDungeonLevel);
var
  x,y:integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.SaveLevel()');
  
	try
		{ Save the basic properties of the level }
		DestinationLevel.Name := Name;
		DestinationLevel.LevelDepth := LevelDepth;
		DestinationLevel.LevelTheme := LevelTheme;
		DestinationLevel.LevelRating := LevelRating;

		{ Save the stairs }
		DestinationLevel.StairsTaken := StairsTaken;

		{ Save the unique }
		DestinationLevel.Unique := Unique;

		{ Save the map layers }
		for x:=1 to (DUNGEONSIZEX) do
    begin
			for y:=1 to (DUNGEONSIZEY) do
      begin
        DestinationLevel.Visible[x,y]:=Visible[x,y];
        DestinationLevel.Terrain[x,y]:=Terrain[x,y];
        DestinationLevel.Monsters[x,y]:=Monsters[x,y];
        DestinationLevel.Objects[x,y]:=Objects[x,y];
        DestinationLevel.Walkable[x,y]:=Walkable[x,y];
        DestinationLevel.Effects[x,y]:=Effects[x,y];
        DestinationLevel.People[x,y]:=People[x,y];
        DestinationLevel.Zone[x,y]:=Zone[x,y];
        DestinationLevel.TerrainCost[x,y] := TerrainCost[x,y];
        DestinationLevel.Projectiles[x,y] := Projectiles[x,y];
        DestinationLevel.ProjectilesColour[x,y] := ProjectilesColour[x,y];
      end;
    end;

		{ Save the stairs }
		for x:=0 to MAXSTAIRS do
		begin
			DestinationLevel.StairsUpX[x] := StairsUpX[x];
			DestinationLevel.StairsUpY[x] := StairsUpY[x];
			DestinationLevel.StairsDownX[x] := StairsDownX[x];
			DestinationLevel.StairsDowny[x] := StairsDownY[x];
		end;
  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Unique Monster Text for the Level Feeling }
function TDungeonLevel.GetUniqueMonsterFeeling: String;
var
  Unique1: String;
  Unique2: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.GetUniqueMonsterFeeling()');

	{ Default Result }
	Result := '';
	try
    { Now check for a unique }
    if FormDisplay.Game.Dungeon.Unique <> nil then
    begin
      Unique1 := UniqueAlertsTypes[Random(High(UniqueAlertsTypes)) + 1];
      Unique2 := UniqueAlerts[Random(High(UniqueAlerts)) + 1];
      Result := Format(Unique1, [Format(Unique2,
        [FormDisplay.Game.Dungeon.Unique.Name])]);
    end;
  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Calculate and return the Level Feeling }
function TDungeonLevel.GetLevelFeeling: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.GetLevelFeeling()');

	{ Default Result }
	Result := '';

	try
		{ Don't allow level feelings to be generated too option to avoid stair
		  scumming }
		if CanFeelLevel then
		begin
			{ Level feeling is affected by items }
			if LevelRating > 999 then 
				Result := GLevelFeelings[4]
			else if LevelRating > 500 then 
				Result := GLevelFeelings[3]
			else if LevelRating > 250 then
				Result := GLevelFeelings[2]
			else if LevelRating > 100 then 
				Result := GLevelFeelings[2]
			else 
				Result := GLevelFeelings[0];
				
			{ Set the level feeling }
			FormDisplay.Game.LastLevelFeeling := FormDisplay.Game.Turns;
		end;
  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Place effects on Level }
procedure TDungeonLevel.PlaceEffectsOnLevel(LevelTheme: Integer; EffectsWanted: integer);
var
  Loop: Integer;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceEffectsOnLevel()');
  
	try
		{ Depending on the level, add different types of special background effects
		  to open ground }
		if LevelTheme in [D_EARTH, D_AIR, D_FIRE, D_WATER, D_WILDERNESS] then
		begin
			{ For each effect wanted }
			for Loop := 0 to EffectsWanted div 7 do
			begin
				{ Find a suitable location }
				repeat
					X := Random(DUNGEONSIZEX) + 1;
					Y := Random(DUNGEONSIZEY) + 1;
				until (Terrain[X, Y] in [T_FLOOR_ROOM, T_FLOOR_CORRIDOR]) and 
					(Effects[X, Y] = 0);
					
				{ Set the tile }
				Effects[X, Y] := E_SPECIALEFFECT;
        Walkable[X, Y] := False;
			end;
		end
		else if LevelTheme = D_ABYSS then
		begin			
			{ For each effect wanted }
			for Loop := 0 to EffectsWanted * 10 do
			begin
				{ Find a suitable location }
				repeat
					X := Random(DUNGEONSIZEX) + 1;
					Y := Random(DUNGEONSIZEY) + 1;
				until (Terrain[X, Y] in [T_FLOOR_ROOM, T_FLOOR_CORRIDOR]) and 
					(Effects[X, Y] = 0);
					
				{ Set the tile }
				Effects[X, Y] := E_SPECIALEFFECT;
        Walkable[X, Y] := False;
			end;
		end
		else if LevelTheme <> D_TOWN then
		begin
			{ For each effect wanted }
			for Loop := 0 to EffectsWanted div 8 do
			begin
				{ Find a suitable location }
				repeat
					X := Random(DUNGEONSIZEX) + 1;
					Y := Random(DUNGEONSIZEY) + 1;
				until (Terrain[X, Y] in [T_FLOOR_ROOM, T_FLOOR_CORRIDOR]) and 
					(Effects[X, Y] = 0);
					
				{ Set the tile }
				Effects[X, Y] := E_STANDARDEFFECT;
        Walkable[X, Y] := False;
			end;
		end;

    { Now place mineral nodes }
    for Loop := 0 to EffectsWanted div 8 do
    begin
      { Find a suitable location }
      repeat
        X := Random(DUNGEONSIZEX) + 1;
        Y := Random(DUNGEONSIZEY) + 1;
      until (Terrain[X, Y] in [T_SOFTWALL]) and
        (Effects[X, Y] = 0) and ((CountAdjacentCorridors(X, Y) > 0) or
        (CountAdjacentFloors(X, Y) > 0));

      { Set the tile }
      Effects[X, Y] := E_MINERAL;
    end;

		{ Draw the ground zones }
		for Loop := 0 to EffectsWanted div 3 do
		begin
			{ Find a suitable location }
			repeat
				X := Random(DUNGEONSIZEX) + 1;
				Y := Random(DUNGEONSIZEY) + 1;
			until (Terrain[X, Y] in [T_FLOOR_ROOM, T_FLOOR_CORRIDOR]) and 
				(Effects[X, Y] = 0);
				
			{ Draw the zone }
			DrawEffectZone(X, Y, E_GROUNDEFFECT, random(4) + 2, 6);
		end;
  except
 	  { in case of error, log the Exception }
 	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Place a room on the map }
function TDungeonLevel.PlaceRoom(RoomType: integer; 
	RoomSegment: integer = -1): Boolean;
const
  MAX_TRY_COUNT = 50;
var
  segmentX: Integer;
  segmentY: Integer;
  startX: Integer;
  startY: Integer;
  trycounter: Integer;
  roomcentre: TPoint;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceRoom()');

	{ Default Result }
	Result := False;
	
	try
		{ Initialise with default values }
		Result := False;
		trycounter := 0;

		{ Try to place the room in an unused segment }
		while (trycounter < MAX_TRY_COUNT) do
		begin
			{ Get random location inside our dungeon well away from edges }
			repeat
				repeat
					startX := random(DUNGEON_WID) + 1;
					startY := random(DUNGEON_HGT) + 1;
				until InDungeonBounds(startX, startY);

				{ Find the block from the location }
				segmentX := startX div BLOCK_WID;
				segmentY := starty div BLOCK_HGT;
			until ((segmentX > 1) and (segmentX < BLOCK_NUM_X - 1)) and
						((segmentY > 1) and (segmentY < BLOCK_NUM_Y - 1));

			{ Check if there already is a room in this block }
			if Block[segmentX, segmentY] = 0 then
			begin
				{ If there isn't, try to build a room here }
				if BuildRoom(startX, startY, RoomType, 0, roomcentre) = true then
				begin
					{ Mark the Block as used }
					Block[segmentX, segmentY] := RoomCount;

					{ Store the Room }
					Rooms[RoomCount].X := startX;
					Rooms[RoomCount].Y := startY;
					Inc(RoomCount);

					{ Store the Room Centre }
					Cent[CentCount] := roomcentre;
					Inc(Centcount);

					{ Break out of the loop if successful }
					Result := True;
					Break;
				end;
			end;
			inc(trycounter);
		end;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;  
end;

  { Count the number of adjacent walls to a square }
function TDungeonLevel.CountWalls(x, y: Integer): Integer;
var
  wallcount: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.CountWalls()');

	{ Default Result }
	Result := 0;
	
	try
		{ Check each tile for walls surrounding the tile specified }
		if (Terrain[x, y + 1] = T_SOFTWALL) or 
			(Terrain[x, y + 1] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x, y - 1] = T_SOFTWALL) or 
			(Terrain[x, y - 1] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x + 1, y] = T_SOFTWALL) or 
			(Terrain[x + 1, y] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x - 1, y] = T_SOFTWALL) or 
			(Terrain[x - 1, y] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x + 1, y + 1] = T_SOFTWALL) or 
			(Terrain[x + 1, y + 1] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x + 1, y - 1] = T_SOFTWALL) or 
			(Terrain[x + 1, y - 1] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x - 1, y + 1] = T_SOFTWALL) or 
			(Terrain[x - 1, y + 1] = T_HARDWALL) then 
			inc(Result);
		if (Terrain[x - 1, y - 1] = T_SOFTWALL) or 
			(Terrain[x - 1, y - 1] = T_HARDWALL) then 
			inc(Result);
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Reset any twists and turns or kinks during tunnel creation }
procedure TDungeonLevel.FindCorrectDirection(var rdir, cdir: Integer; y1,
  x1, y2, x2: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.FindCorrectDirection()');

	try
		{ Get Horizontal and Vertical Directions }
		if y1 = y2 then 
			rdir := 0 
		else if y1 < y2 then 
			rdir := 1 
		else 
			rdir := -1;			
		if x1 = x2 then 
			cdir := 0 
		else if x1 < x2 then 
			cdir := 1 
		else 
			cdir := -1;

		{ Never move diagonally }
		if abs(cdir) = abs(rdir) then
			if OneChanceIn(2) then 
				rdir := 0 
			else 
				cdir := 0;
  except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Pick a random direction and return the offsets }
procedure TDungeonLevel.PickRandomDirection(var rdir, cdir: Integer);
var
  direction: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PickRandomDirection()');

	try
		{ Get a random direction }
		direction := random(4);

		{ Extract the dx/dy coordinates }
		cdir := ddx_ddd[direction];
		rdir := ddy_ddd[direction];
	except
	 	{ in case of error, log the Exception }
	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Build a tunnel from one location to another }
procedure TDungeonLevel.BuildTunnel(var row1, col1, row2, col2: Integer);
var
  col_dir: Integer;
  door_flag: Boolean;
  row_dir: Integer;
  loop: Integer;
  loop_count: Integer;
  start_row: Integer;
  start_col: Integer;
  temp_col: Integer;
  temp_row: Integer;
  x: Integer;
  y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PickRandomDirection()');

	try
		{ Reset the door indicator }
		door_flag := False;

		{ Save the starting location }
		start_row := row1;
		start_col := col1;

		{ Set up some working variables }
		loop_count := 0;
		temp_col := 0;
		temp_row := 0;
		col_dir := 0;
		row_dir := 0;

		{ Initialise the tunnel-building arrays }
		TunnelCount := 0;
		for loop := Low(Tunnel) to High(Tunnel) do
		begin
			Tunnel[Loop].X := 0;
			Tunnel[Loop].Y := 0;
		end;
		WallCount := 0;
		for loop := Low(Wall) to High(Wall) do
		begin
			Wall[Loop].X := 0;
			Wall[Loop].Y := 0;
		end;

		{ Start out in the correct direction }
		FindCorrectDirection(row_dir, col_dir, row1, col1, row2, col2);

		{ Keep going until we're done }
		while ((row1 <> row2) or (col1 <> col2)) do
		begin
			{ Increase the iterator }
			inc(loop_count);
			
			{ Just in case we run into trouble }
			if loop_count > 2000 then 
				break;

			{ Allow bends in the tunnel }
			if random(PERCENTAGE) < DUN_TUN_CHG then
			begin
				{ Get the correct direction }
				FindCorrectDirection(row_dir, col_dir, row1, col1, row2, col2);

				{ Set out on a random direction }
				if random(PERCENTAGE) < DUN_TUN_RND then 
					PickRandomDirection(row_dir, col_dir);
			end;

			{ Get the next location }
			temp_row := row1 + row_dir;
			temp_col := col1 + col_dir;

			{ Ensure we don't go over the edges of the dungeon }
			while not(InDungeonBounds(temp_col, temp_row)) do
			begin
				{ Get the correct direction }
				FindCorrectDirection(row_dir, col_dir, row1, col1, row2, col2);

				{ Set out on a random direction }
				if random(PERCENTAGE) < DUN_TUN_RND then
					PickRandomDirection(row_dir, col_dir);

				{ Get the next location }
				temp_row := row1 + row_dir;
				temp_col := col1 + col_dir;
			end;

			{ Now we need to avoid heading out of the dungeon! }
			if Terrain[temp_col, temp_row] = T_HARDWALL then 
				continue;

			{ If we find a wall of a room we pierce it }
			if Terrain[temp_col, temp_row] = T_ROOMWALL then
			begin
				{ Get the next location }
				x := temp_col + col_dir;
				y := temp_row + row_dir;

				{ Avoid going out of the dungeon }
				if (Terrain[x, y] = T_HARDWALL) then 
					continue;

				{ Accept we're going to use this location }
				row1 := temp_row;
				col1 := temp_col;

				{ Save the entry point }
				if (TunnelCount < High(Tunnel)) then
				begin
					Wall[WallCount].X := col1;
					Wall[WallCount].Y := row1;
					inc(WallCount);
				end;

				{ Don't allow more tunnel entrances near this one }
				for y := row1 - 1 to row1 + 1 do
        begin
					for x := col1 - 1 to col1 + 1 do
          begin
						if (Terrain[x, y] = T_ROOMWALL) then
              Terrain[x, y] := T_HARDWALL;
          end;
        end;
			end
			{ travel quickly through rooms }
			else if Terrain[temp_col, temp_row] = T_FLOOR_ROOM then
			begin
				{ Accept we're going to use this location }
				row1 := temp_row;
				col1 := temp_col;
			end
			{ tunnel through other walls }
			else if terrain[temp_col, temp_row] = T_SOFTWALL then
			begin
				{ Accept we're going to use this location }
				row1 := temp_row;
				col1 := temp_col;

				{ Save the tunnel location }
				if (TunnelCount < High(Tunnel)) then
				begin
					Tunnel[TunnelCount].X := col1;
					Tunnel[TunnelCount].Y := row1;
					inc(TunnelCount);

					{  Allow Doors }
					door_flag := true;
				end;
			end
			{ and handle corridor crossing points }
			else
			begin
				{ Accept we're going to use this location }
				row1 := temp_row;
				col1 := temp_col;

				{ If we have allowed doors }
				if (door_flag) then
				begin
					{ Save door locations }
					if (DoorCount < High(Door)) then
					begin
						Door[DoorCount].X := col1;
						Door[DoorCount].Y := row1;
						inc(DoorCount);
					end;

					{ No door in next grid }
					door_flag := false;
				end;

				{ Allow termination of tunnels }
				if random(PERCENTAGE) > DUN_TUN_CON then
				begin
					{ Find the distance between row1 and the starting row }
					temp_row := row1 - start_row;
					if temp_row < 0 then 
						temp_row := 0 - temp_row;

					{ Find the distance between col1 and the starting col }
					temp_col := col1 - start_col;
					if temp_col < 0 then 
						temp_col := 0 - temp_col;

					{ Terminate the tunnel if necessary }
					if (temp_row > 10) or (temp_col > 10) then 
						break;
				end;
			end;
		 end;

		{ Turn the stored tunnel locations into corridors }
		for loop := 0 to TunnelCount do
		begin
			{ Get the coordinate }
			x := Tunnel[loop].X;
			y := Tunnel[loop].Y;

			{ Turn that point in the dungeon into corridor }
			Terrain[x, y] := T_FLOOR_CORRIDOR;

			{ Stick in an occasional doorway }
			if random(PERCENTAGE) < DUN_TUN_PEN then
				TryPlaceDoor(x, y);
		end;

		{ Apply the piercings that we found }
		for loop := 0 to WallCount do
		begin
			{ Get the coordinate }
			x := Wall[loop].X;
			y := Wall[loop].Y;

			{ Turn that point in the dungeon into corridor }
			Terrain[x, y] := T_FLOOR_CORRIDOR;

			{ Stick in an occasional doorway }
			if random(PERCENTAGE) < DUN_TUN_PEN then 
				TryPlaceDoor(x, y);
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Check if we're not trying to go off the edges of the map }
function TDungeonLevel.InDungeonBounds(x, y: Integer): Boolean;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TDungeonLevel.InDungeonBounds()');

	{ Default Result }
	Result := False;
	
	try
		{ Check if the cell isn't off the map }
		Result := (x > IN_BOUNDS_BORDER) and (x < (DUNGEON_WID - IN_BOUNDS_BORDER))
			and (y > IN_BOUNDS_BORDER) and (y < (DUNGEON_HGT - IN_BOUNDS_BORDER));
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Build a room at a specified location }
function TDungeonLevel.BuildRoom(roomx, roomy, roomtype, segment: Integer; 
	var roomcentre: TPoint): Boolean;
var
  x, x1: Integer;
  y, y1: Integer;
  y1a, x1a, y2a, x2a: Integer;
	y1b, x1b, y2b, x2b: Integer;  
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.BuildRoom()');

	{ Default Result }
	Result := False;
	
	try
		{ Get size of room(s) }
		y1a := random(4);
		x1a := random(13);
		y2a := random(3);
		x2a := random(9);
		y1b := random(3);
		x1b := random(9);
		y2b := random(4);
		x2b := random(13);

		{ Two checks to see if we can draw the room or not }
		for x := roomx - x1a - 1 to roomx + x2a + 1 do
    begin
			for y := roomy - y1a - 1 to roomy + y2a + 1 do
      begin
				if not(InDungeonBounds(x, y)) then
				begin
					Result := False;
					Exit;
				end;
      end;
    end;
		for x := roomx - x1a - 1 to roomx + x2a + 1 do
			for y := roomy - y1a - 1 to roomy + y2a + 1 do
      begin
				if Terrain[x, y] <> T_SOFTWALL then
				begin
					Result := False;
					Exit;
				end;
      end;
				
		{ If the room is overlapping, check more }
		if roomtype = ROOM_OVERLAPPING then
		begin
			for x := roomx - x1b - 1 to roomx + x2b + 1 do
      begin
				for y := roomy - y1b - 1 to roomy + y2b + 1 do
        begin
					if not(InDungeonBounds(x, y)) then
					begin
						Result := False;
						Exit;
					end;
        end;
      end;
			for x := roomx - x1b - 1 to roomx + x2b + 1 do
      begin
				for y := roomy - y1b - 1 to roomy + y2b + 1 do
        begin
					if Terrain[x, y] <> T_SOFTWALL then
					begin
						Result := False;
						Exit;
					end;
        end;
      end;
		end;

		{ Draw a wall round the first room }
		for x := roomx - x1a - 1 to roomx + x2a + 1 do
		begin
			y1 := roomy - y1a - 1;
			if Terrain[x, y1] <> T_FLOOR_ROOM then 
				Terrain[x, y1] := T_ROOMWALL;
			y1 := roomy + y2a + 1;
			if Terrain[x, y1] <> T_FLOOR_ROOM then 
				Terrain[x, y1] := T_ROOMWALL;
		end;
		for y := roomy - y1a - 1 to roomy + y2a + 1 do
		begin
			x1 := roomx - x1a - 1;
			if Terrain[x1, y] <> T_FLOOR_ROOM then 
				Terrain[x1, y] := T_ROOMWALL;
			x1 := roomx + x2a + 1;
			if Terrain[x1, y] <> T_FLOOR_ROOM then 
				Terrain[x1, y] := T_ROOMWALL;
		end;

		{ Draw a wall around the second room if we have one }
		if roomtype = ROOM_OVERLAPPING then
		begin
			for x := roomx - x1b - 1 to roomx + x2b + 1 do
			begin
				Terrain[x, roomy - y1b - 1] := T_ROOMWALL;
				Terrain[x, roomy + y2b + 1] := T_ROOMWALL;
			end;
			for y := roomy - y1b - 1 to roomy + y2b + 1 do
			begin
				Terrain[roomx - x1b - 1, y] := T_ROOMWALL;
				Terrain[roomx + x2b + 1, y] := T_ROOMWALL;
			end;
		end;

		{ Draw the room }
		for x := roomx - x1a to roomx + x2a do
    begin
			for y := roomy - y1a to roomy + y2a do
      begin
				Terrain[x, y] := T_FLOOR_ROOM;
      end;
    end;
		if roomtype = ROOM_OVERLAPPING then
			for x := roomx - x1b to roomx + x2b do
      begin
				for y := roomy - y1b to roomy + y2b do
        begin
					Terrain[x, y] := T_FLOOR_ROOM;
        end;
      end;

		{ We have a very small chance of adding pillars }
		if (roomtype <> ROOM_OVERLAPPING) and (random(20) = 0) then
		begin
			for x := roomx - x1b to roomx + x2b do
      begin
				for y := roomy - y1b to roomy + y2b do
        begin
					if (x mod 2 = 0) and (y mod 2 = 0) and 
						(Terrain[x, y] = T_FLOOR_ROOM) then 
						Terrain[x, y] := T_HARDWALL;
        end;
      end;
		end;

		{ Store the room }
		roomcentre.X := roomx;
		roomcentre.Y := roomY;

		{ Indicate success }
		Result := True;  
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Try and place a door at a location }
function TDungeonLevel.TryPlaceDoor(x, y: Integer): Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.TryPlaceDoor()');

	{ Default Result }
	Result := False;

	try
		{ Paranoia }
		if not(InDungeonBounds(x, y)) then
			exit;

		{ Don't place doors inside rooms }
		if Terrain[x,y] = T_FLOOR_ROOM then 
			exit;

		{ Have a chance of placing a door }
		if Random(PERCENTAGE) < DUN_TUN_JCT then
			if CountAdjacentCorridors(x, y) <= 2 then
				Terrain[x, y] := T_DOOR_CLOSED;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Count the number of adjacent corridor tiles to a square }
function TDungeonLevel.CountAdjacentCorridors(x1, y1: Integer): Integer;
var
  Loop: Integer;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.CountAdjacentCorridors()');

	{ Default Result }
	Result := 0;

	try
		{ Scan adjacent grids }
		for Loop := 0 to 3 do
		begin
			{ Get the offsets }
			X := X1 + ddx_ddd[Loop];
			Y := Y1 + ddy_ddd[Loop];

			{ Skip rooms }
			if Terrain[x, Y] = T_FLOOR_CORRIDOR then 
				inc(Result);
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Count the number of adjacent floor tiles to a square }
function TDungeonLevel.CountAdjacentFloors(x1, y1: Integer): Integer;
var
  Loop: Integer;
  X: Integer;
  Y: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TDungeonLevel.CountAdjacentFloors()');

	{ Default Result }
	Result := 0;

	try
		{ Scan adjacent grids }
		for Loop := 0 to 3 do
		begin
			{ Get the offsets }
			X := X1 + ddx_ddd[Loop];
			Y := Y1 + ddy_ddd[Loop];

			{ Skip rooms }
			if Terrain[x, Y] = T_FLOOR_ROOM then
				inc(Result);
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ TDigger constructor }
constructor TDigger.Create(X1, Y1: Integer);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TDigger.Create()');
  
	try
  	X := X1;
  	Y := Y1;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Zone handling }
procedure TDungeonLevel.DrawZone(x, y, zoneid, zonesize: integer; maxtilesused: integer);
var
  Startx, Starty: Integer;
  Offsetx, Offsety: Integer;
  Tilesused: Integer;
  Z: Integer;
  Maxtries: Integer;
  TilesX: Array of Integer;
  TilesY: Array of Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.DrawZone()');
  
	try
		{ Set initial variables }
		Tilesused := 0;
		Maxtries := 30;
		SetLength(TilesX, MaxTilesUsed * 2);
		SetLength(TilesY, MaxTilesUsed * 2);

		{ Set the starting terrain }
		startx := x;
		starty := y;
		
		{ Set the tile }
		Zone[startx, starty] := zoneid;
		
		{ Repeat until tilesused = maxtilesused }
		repeat
			{ Pick a random direction, i.e. either N,S,E,W }
			z := 0;
			repeat
				offsetx := random(3) - 1;
				offsety := random(3) - 1;
				inc(z);
			until (((offsetx <> offsety)) and ((abs(x - (startx + offsetx)) 
				< zonesize) and (abs(y - (starty + offsety)) < zonesize)) or
				(z >= maxtries));
				
			{ Only go ahead if we have not timed out, i.e. suitable terrain has 
				been found }
			if z < maxtries then
			begin
				inc(startx, offsetx);
				inc(starty, offsety);
				
				{ Check for the presence of a zone }
				if Zone[startx, starty] <> zoneid then
				begin
					{ Hollow out this as a room as well }
					Zone[startx, starty] := zoneid;
					Terrain[startx, starty] := T_FLOOR_ROOM;
					Walkable[startx, starty] := true;
					TilesX[TilesUsed] := StartX;
					TilesY[TilesUsed] := StartY;
					inc(tilesused);
				end;
			end
			else
			{ We have timed out - we obviously cannot find any more tiles }
				tilesused := MaxTilesUsed + 1;
		until tilesused >= MaxTilesUsed;

		{ Now draw a border round the zone if necessary }
		for Z:= 0 to High(TilesX) - 1 do
		begin
			if TilesX[Z] > 0 then
			begin
				for OffSetX := TilesX[Z] - 1 to TilesX[Z] + 1 do
				begin
					for OffSetY := TilesY[Z] - 1 to TilesY[Z] + 1 do
					begin
						if (Terrain[OffSetX, OffSetY] = T_SOFTWALL) or 
							(Terrain[OffSetX, OffSetY] = T_ROOMWALL) then
							Zone[OffSetX, OffSetY] := zoneid;
					end;
				end;
			end;
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Put Vaults on Level }
procedure TDungeonLevel.PlaceVaultsonLevel(
  PercentageChanceOfGreaterVault: Integer);
var
  Cursed: Boolean;
  Counter: Integer;
  Ecology: String;
  PlaceGreaterVault: Boolean;
  ItemType: crItemType;
  ItemQuality: crItemQuality;
  ItemQualityRatio: Integer;
  LevelOffset: Integer;
  LocalMonster: TMonster;
  LocalItem: TItem;
  LocalVault: TVault;
  MonsterLevel: Integer;
  MonsterType: TMonsterArchetype;
  OffsetItemLevel: Integer;
  OffsetX: Integer;
  OffsetY: Integer;
  SuitableLocationFound: Boolean;
  TempX: Integer;
  TempY: Integer;
  VaultChar: Char;
  VaultLine: String;
  VaultMap: TStringList;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceVaultsonLevel()');
  
	try
		{ Create a buffer to hold the vault }
		VaultMap := TStringList.Create;

		{ Figure out the chance of a greater vault being placed }
		PlaceGreaterVault := Random(PERCENTAGE) < PercentageChanceOfGreaterVault;
		
		{ Get the vault settings }
		if PlaceGreaterVault then
		begin
		 LevelOffset := GetLevelOffset(LevelTheme);
		 LocalVault := GetRandomVault(LevelOffset, True);
		 Ecology := GetEcology(LevelOffset);
		end
		else
		begin
			LevelOffset := GetLevelOffset(LevelTheme);
			LocalVault := GetRandomVault(LevelOffset, False);
			Ecology := GetEcology(LevelOffset);
		end;

		{ If we have a vault }
		if Assigned(LocalVault) then
		begin
			{ If we have a suitable vault }
			if (LocalVault.VaultEcology = 'Common') or 
				(Ecology = LocalVault.VaultEcology) then
			begin
				{ Get the dimensions of the vault and find an appropriate place on the
					level to put it - one without a set of stairs, one without a zone and
					linked in }
				SuitableLocationFound := False;

				{ Paranoia Counter }
				Counter := 0;

				{ Iterate through until we've found a good location }
				repeat
					{ Set a default state }	
					SuitableLocationFound := True;
					
					{ Find a random floor cell }
					repeat
						TempX := Random(DUNGEONSIZEX) + 1;
						TempY := Random(DUNGEONSIZEY) + 1;
					until (Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
						(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR);
						
					{ Increase paranoia counter }
					inc(Counter);
					
					{ Check around this for suitable terrain }
					for OffsetX := TempX to TempX + LocalVault.VaultX do
          begin
						for OffsetY := TempY to TempY + LocalVault.VaultY do
						begin
							{ If we touch stairs we don't want a vault here }
							if ((Terrain[OffsetX, OffsetY] = T_STAIRS_UP) or
									(Terrain[OffsetX, OffsetY] = T_STAIRS_DOWN)) then 
								SuitableLocationFound := False;
								
							{ If touch a zone we don't want a vault here }	
							if Zone[OffsetX, OffsetY] > 0 then 
								SuitableLocationFound := False;
						end;
          end;
				until (SuitableLocationFound = True) or (Counter > 100);

				{ If we've got a suitable location }
				if (Counter < 100) and (SuitableLocationFound) then
				begin
					{ Place the vault }
					VaultMap.Text := LocalVault.VaultMap;

					{ For each line of the vault defintion }
					for OffsetY := 0 to VaultMap.Count - 1 do
					begin						
						{ For every cell of each line }
						VaultLine := VaultMap[OffsetY];
						for OffsetX:= 0 to (LocalVault.VaultX) do
						begin
							{ Blank the cell to begin with }
							Objects[TempX + OffSetX, TempY + OffSetY] := 0;
							Effects[TempX + OffSetX, TempY + OffSetY] := 0;
							RemoveMonsterFromMap(Point(TempX + OffSetX, TempY + OffSetY));
							Zone[TempX + OffSetX, TempY + OffSetY] := Z_VAULT;
							
							{ Get the vault character from the template }
							VaultChar := VaultLine[OffsetX + 3];
							
							{ ' ' represents floors in the definition }
							if VaultChar = ' ' then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_CORRIDOR;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ '%' represents walls in the definition }
							else if VaultChar = '%' then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
								Walkable[TempX + OffSetX, TempY + OffSetY] := False;
							end
							{ '.' represents floors in the definition }
							else if VaultChar = '.' then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ ',' represents monster or treasure in the definition }
							else if (VaultChar = ',') then
							begin
								if OneChanceIn(2) then
								begin
									{ Generate a random monster up to 1 levels OOD }
									MonsterLevel :=  LevelOffset + LevelDepth div 2 + Random(2);
									
									{ Don't exceed the dungeon depth }
									if MonsterLevel > MAXDUNGEONLEVEL then 
										MonsterLevel := MAXDUNGEONLEVEL;
										
									{ Get the random monster type }
									MonsterType := GetRandomMonsterArchetype(MonsterLevel, True);
									
									{ Create the monster }
									LocalMonster := TMonster.Create(MonsterType.ID);

									{ Add the Monster }
									LocalMonster.X := TempX + OffSetX;
									LocalMonster.Y := TempY + OffSetY;
									Monsters[TempX + OffSetX, TempY + OffSetY] := 
										GMonsterList.Count;
									GMonsterList.Add(LocalMonster);
								end
								else
								begin
									{ Treasure up to 3 levels OOD }
									OffsetItemLevel := Random(4) + LevelOffset + 
										LevelDepth div 2;
										
									{ See if the item is cursed }
									Cursed := (Random(PERCENTAGE) + 1) < CHANCECURSED;
									
									{ Get the item quality }
									ItemQualityRatio := Random(PERCENTAGE) + 1;
									if ItemQualityRatio < CHANCE_ARTIFACT then 
										ItemQuality := iArtifact
									else if ItemQualityRatio < CHANCE_EPIC then 
										ItemQuality := iEpic
									else if ItemQualityRatio < CHANCE_LEGENDARY then 
										ItemQuality := iLegendary
									else ItemQuality := iSuperb;
									
									{ Get the item type }
									ItemQualityRatio := Random(PERCENTAGE) + 1;
									if ItemQualityRatio < CHANCE_WEAPON then 
										ItemType := iWeapon
									else if ItemQualityRatio < CHANCE_ARMOUR then 
										ItemType := iArmour
									else if ItemQualityRatio < CHANCE_RING then 
										ItemType := iRing
									else 
										ItemType := iAmulet;

									{ Create the item }
									LocalItem := TItem.Create(ItemQuality, OffsetItemLevel, 
										ItemType, Cursed);
										
									{ Adjust the level rating }
									case LocalItem.ItemQuality of
										iSuperb: Inc(LevelRating, 1);
										iLegendary: Inc(LevelRating, 50);
										iEpic: Inc(LevelRating, 250);
										iArtifact: Inc(LevelRating, 1000);
									end;
									
									{ Add the item }
									LocalItem.Location := iFloor;
									GItemList.Add(LocalItem);
									Objects[TempX + OffSetX, TempY + OffSetY] := 
										GItemList.Count - 1;
								end;
								{ Items appear on empty squares }
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ '#' represents walls in the definition }
							else if (VaultChar = '#') then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
								Walkable[TempX + OffSetX, TempY + OffSetY] := False;
							end
							{ '$' represents walls in the definition }
							else if (VaultChar = '$') then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
								Walkable[TempX + OffSetX, TempY + OffSetY] := False;
							end
							{ 'X' represents hard walls in the definition }
							else if (VaultChar = 'X') then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_HARDWALL;
								Walkable[TempX + OffSetX, TempY + OffSetY] := False;
							end
							{ '* or K' represents standard items in the definition - we use
							  two characters so that we can use one of them ('K') to 
							  represent stairs up when definitions are used in entry vaulrs }
							else if (VaultChar in ['*', 'K']) then
							begin
								{ Standard Treasure }
								OffsetItemLevel := LevelOffset + LevelDepth div 2;

								{ See if the item is cursed }
								Cursed := (Random(PERCENTAGE) + 1) < CHANCECURSED;

								{ Get the item quality }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								if ItemQualityRatio < CHANCE_ARTIFACT then 
									ItemQuality := iArtifact
								else if ItemQualityRatio < CHANCE_EPIC then 
									ItemQuality := iEpic
								else if ItemQualityRatio < CHANCE_LEGENDARY then 
									ItemQuality := iLegendary
								else ItemQuality := iSuperb;

								{ Get the item type }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								if ItemQualityRatio < CHANCE_WEAPON then 
									ItemType := iWeapon
								else if ItemQualityRatio < CHANCE_ARMOUR then 
									ItemType := iArmour
								else if ItemQualityRatio < CHANCE_RING then 
									ItemType := iRing
								else 
									ItemType := iAmulet;

								{ Create the item }
								LocalItem := TItem.Create(ItemQuality, OffsetItemLevel, 
									ItemType, Cursed);

								{ Adjust the level rating }
								case LocalItem.ItemQuality of
									iSuperb: Inc(LevelRating, 1);
									iLegendary: Inc(LevelRating, 50);
									iEpic: Inc(LevelRating, 250);
									iArtifact: Inc(LevelRating, 1000);
								end;

								{ Add the item }
								LocalItem.Location := iFloor;
								GItemList.Add(LocalItem);
								Objects[TempX + OffSetX, TempY + OffSetY] := 
									GItemList.Count - 1;
						
								{ Items appear on empty squares }
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end							
							{ '+' represents closed doors in the definition }
							else if (VaultChar = '+') then
							begin
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_DOOR_CLOSED;
								Walkable[TempX + OffSetX, TempY + OffSetY] := False;
							end
							{ '9' represents OOD monsters in the definition }
							else if (VaultChar = '9') then
							begin
								{ Generate a random monster up to 4 levels OOD }
								MonsterLevel :=  LevelOffset + LevelDepth div 2 + Random(5);

								{ Don't exceed the dungeon depth }
								if MonsterLevel > MAXDUNGEONLEVEL then 
									MonsterLevel := MAXDUNGEONLEVEL;

								{ Get the random monster type }
								MonsterType := GetRandomMonsterArchetype(MonsterLevel, True);

								{ Create the monster }
								LocalMonster := TMonster.Create(MonsterType.ID);

								{ Add the Monster }
								LocalMonster.X := TempX + OffSetX;
								LocalMonster.Y := TempY + OffSetY;
								Monsters[TempX + OffSetX, TempY + OffSetY] := 
									GMonsterList.Count;
								GMonsterList.Add(LocalMonster);

                { Always give OOD monsters OOD treasure as well }
								OffsetItemLevel := LevelOffset + LevelDepth;

								{ See if the item is cursed }
								Cursed := (Random(PERCENTAGE) + 1) < CHANCECURSED;

								{ Get the item quality }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								if ItemQualityRatio < CHANCE_ARTIFACT then 
									ItemQuality := iArtifact
								else if ItemQualityRatio < CHANCE_EPIC then 
									ItemQuality := iEpic
								else if ItemQualityRatio < CHANCE_LEGENDARY then 
									ItemQuality := iLegendary
								else ItemQuality := iSuperb;

								{ Get the item type }
								ItemQualityRatio := Random(PERCENTAGE) + 1;
								if ItemQualityRatio < CHANCE_WEAPON then 
									ItemType := iWeapon
								else if ItemQualityRatio < CHANCE_ARMOUR then 
									ItemType := iArmour
								else if ItemQualityRatio < CHANCE_RING then 
									ItemType := iRing
								else 
									ItemType := iAmulet;

								{ Create the item }
								LocalItem := TItem.Create(ItemQuality, OffsetItemLevel, 
									ItemType, Cursed);

								{ Adjust the level rating }
								case LocalItem.ItemQuality of
									iSuperb: Inc(LevelRating, 1);
									iLegendary: Inc(LevelRating, 50);
									iEpic: Inc(LevelRating, 250);
									iArtifact: Inc(LevelRating, 1000);
								end;

								{ Add the item }
								LocalItem.Location := iFloor;
								GItemList.Add(LocalItem);
								Objects[TempX + OffSetX, TempY + OffSetY] := 
									GItemList.Count - 1;
						
								{ Items appear on empty squares }
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ '&' represents monsters in the definition }
							else if (VaultChar = '&') then
							begin
								{ Generate a random monster up to 2 levels OOD }
								MonsterLevel :=  LevelOffset + LevelDepth div 2 + Random(3);

								{ Don't exceed the dungeon depth }
								if MonsterLevel > MAXDUNGEONLEVEL then 
									MonsterLevel := MAXDUNGEONLEVEL;

								{ Get the random monster type }
								MonsterType := GetRandomMonsterArchetype(MonsterLevel, True);

								{ Create the monster }
								LocalMonster := TMonster.Create(MonsterType.ID);

								{ Add the Monster }
								LocalMonster.X := TempX + OffSetX;
								LocalMonster.Y := TempY + OffSetY;
								Monsters[TempX + OffSetX, TempY + OffSetY] := 
									GMonsterList.Count;
								GMonsterList.Add(LocalMonster);

                { Monsters appear on empty squares }
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ '@' represents monsters in the definition }
							else if (VaultChar = '@') then
							begin
								{ Generate a random monster up to 5 levels OOD }
								MonsterLevel :=  LevelOffset + LevelDepth div 2 + Random(6);

								{ Don't exceed the dungeon depth }
								if MonsterLevel > MAXDUNGEONLEVEL then 
									MonsterLevel := MAXDUNGEONLEVEL;

								{ Get the random monster type }
								MonsterType := GetRandomMonsterArchetype(MonsterLevel, True);

								{ Create the monster }
								LocalMonster := TMonster.Create(MonsterType.ID);

								{ Add the Monster }
								LocalMonster.X := TempX + OffSetX;
								LocalMonster.Y := TempY + OffSetY;
								Monsters[TempX + OffSetX, TempY + OffSetY] := 
									GMonsterList.Count;
								GMonsterList.Add(LocalMonster);

                { Monsters appear on empty squares }
                Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end
							{ '^' represents floors in definition }
							else if (VaultChar = '^') then
							begin
								{ TODO: they will represent traps at some point }
								Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
								Walkable[TempX + OffSetX, TempY + OffSetY] := True;
							end;
						end;
					end;

					{ Increase the level feeling appropriately for a greater vault }
					if (LevelRating < 1000) and (PlaceGreaterVault) then 
						inc(LevelRating, 1000);

					{ Add the vault info if we have the wizard screen displayed }
					WizardLog(Format('Generated Vault: %s at %d/%d', 
						[LocalVault.VaultName, TempX, TempY]));
				end;
			end;
		end;

		{ Free the vault buffer }
		VaultMap.Free;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Create and place and entry vault on the Level }
procedure TDungeonLevel.PlaceEntryVault(var StairLoc: TPoint);
var
  LocalVault: TVault;
  OffsetX: Integer;
  OffsetY: Integer;
  TempX: Integer;
  TempY: Integer;
  VaultChar: Char;
  VaultLine: String;
  X: Integer;
  Y: Integer;
  HalfVaultSize: TPoint;
  VaultMap: TStringList;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceEntryVault()');
  
	try
		{ Create a buffer to hold the vault }
		VaultMap := TStringList.Create;

		{ Get a suitable entry vault }
		LocalVault := GetEntryVault;
		
		{ If we have found a suitable vault }
		if Assigned(LocalVault) then
		begin
			{ Get the bounds of the vaukt }
			HalfVaultSize.X := (LocalVault.VaultX div 2 + 1);
			HalfVaultSize.Y := (LocalVault.VaultY div 2 + 1);

			{ Get the location of the stairs }
			TempX := StairLoc.X - HalfVaultSize.X;
			TempY := StairLoc.Y - HalfVaultSize.Y;

			{ Carve out the proposed location of the vault }
			for X := TempX - 1 to TempX + LocalVault.VaultX + 1 do
      begin
				for Y := TempY - 1 to TempY + LocalVault.VaultY + 1 do
				begin
					{ Remove everything from the map }
					Objects[X, Y] := 0;
					Effects[X, Y] := 0;
					RemoveMonsterFromMap(Point(X, Y));
					
					{ Make it walkable floor }
					Terrain[X, Y] := T_FLOOR_ROOM;
					Walkable[X, Y] := True;
					
					{ Flag this as a zone }
					Zone[X, Y] := Z_VAULT;
				end;
      end;

			{ Place the vault }
			VaultMap.Text := LocalVault.VaultMap;

			{ For each line of the vault defintion }
			for OffsetY := 0 to VaultMap.Count - 1 do
			begin
				{ For every cell of each line }
				VaultLine := VaultMap[OffsetY];
				for OffsetX := 0 to (LocalVault.VaultX) do
				begin
					{ Blank the cell again to begin with }
					Objects[TempX + OffSetX, TempY + OffSetY] := 0;
					Effects[TempX + OffSetX, TempY + OffSetY] := 0;
					Monsters[TempX + OffSetX, TempY + OffSetY] := 0;
					
					{ Get the vault character from the template }
					VaultChar := VaultLine[OffsetX + 3];
					
					{ ' ' represents floors in the entry vault }
					if (VaultChar = ' ') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_CORRIDOR;
						Walkable[TempX + OffSetX, TempY + OffSetY] := True;
					end
					{ 'K' represents stairs up in the entry vault definition }
					else if (Vaultchar = 'K') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_STAIRS_UP;
						Walkable[TempX + OffSetX, TempY + OffSetY] := True;
						StairLoc := Point(TempX + OffSetX, TempY + OffSetY);
					end
					{ '%' represents walls in the entry vault definition }
					else if (VaultChar = '%') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
						Walkable[TempX + OffSetX, TempY + OffSetY] := False;
					end
					{ '.' represents floors in the entry vault definition }
					else if (VaultChar = '.') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
						Walkable[TempX + OffSetX, TempY + OffSetY] := True;
					end
					{ '#' represents walls in the entry vault definition }
					else if (VaultChar = '#') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
						Walkable[TempX + OffSetX, TempY + OffSetY] := False;
					end
					{ 'X' represents walls in the entry vault definition }
					else if (VaultChar = 'X') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_SOFTWALL;
						Walkable[TempX + OffSetX, TempY + OffSetY] := False;
					end
					{ '+' represents closed doors in the entry vault definition }
					else if (VaultChar = '+') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_DOOR_CLOSED;
						Walkable[TempX + OffSetX, TempY + OffSetY] := False;
					end
					{ '+' represents closed doors in the entry vault definition }
					else if (VaultChar = '$') then
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_DOOR_CLOSED;
						Walkable[TempX + OffSetX, TempY + OffSetY] := False;
					end
					{ else in doubt make it a floor }
					else
					begin
						Terrain[TempX + OffSetX, TempY + OffSetY] := T_FLOOR_ROOM;
						Walkable[TempX + OffSetX, TempY + OffSetY] := True;
					end;
				end;
			end;
		end;

		{ Free the vault buffer }
		VaultMap.Free;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Effect Zone creation }
procedure TDungeonLevel.DrawEffectZone(x, y, zoneid, zonesize,
  maxtilesused: integer);
var
  Startx, Starty: Integer;
  Offsetx, Offsety: Integer;
  Tilesused: Integer;
  Z: Integer;
  Maxtries: Integer;
  TilesX: Array of Integer;
  TilesY: Array of Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.DrawEffectZone()');
  
	try
		{ Set initial variables }
		Tilesused := 0;
		Maxtries := 30;
		SetLength(TilesX, MaxTilesUsed * 2);
		SetLength(TilesY, MaxTilesUsed * 2);

		{ Set the starting terrain }
		startx := x;
		starty := y;
		
		{ Set the tile }
		Effects[startx, starty] := zoneid;
		
		{ Repeat until tilesused = maxtilesused }
		repeat
			{ Pick a random direction, i.e. either N,S,E,W }
			z := 0;
			repeat
				offsetx := random(3) - 1;
				offsety := random(3) - 1;
				inc(z);
			until (((offsetx <> offsety)) and ((abs(x - (startx + offsetx)) 
				< zonesize) and (abs(y - (starty + offsety)) < zonesize))) or
				(z >= maxtries);
						 ;
			{ Only go ahead if we have not timed out, i.e. suitable terrain has 
				been found }
			if z < maxtries then
			begin
				inc(startx, offsetx);
				inc(starty, offsety);
				
				{ Check for the presence of preexisting effects or stairs }
				if (Effects[startx, starty] <> zoneid) and 
					(Walkable[startx, starty] = true) and
					((Terrain[startX, starty] <> T_STAIRS_UP) and 
					(Terrain[startX, starty] <> T_STAIRS_DOWN)) then
				begin
					{ Mark an effect on this tile }
					Effects[startx, starty] := zoneid;
					TilesX[TilesUsed] := StartX;
					TilesY[TilesUsed] := StartY;
					inc(tilesused);
				end;
			end
			else
			{ We have timed out - we obviously cannot find any more tiles }
				tilesused := MaxTilesUsed + 1;
		until tilesused >= MaxTilesUsed;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Remove a monster from the map after it has been killed }
procedure TDungeonLevel.RemoveMonsterFromMap(Loc: TPoint);
var
  LocalMonster: TMonster;
  MonsterID: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.RemoveMonsterFromMap()');
 
	try
		{ Check we have a monster to remove }
		if Monsters[Loc.X, Loc.Y] > 0 then
		begin
			{ Get the monster }
			MonsterID := Monsters[Loc.X, Loc.Y];
			LocalMonster := GMonsterList[MonsterID] as TMonster;

			{ Remove the monster }
			Monsters[Loc.X, Loc.Y] := 0;
			LocalMonster.X := 0;
			LocalMonster.Y := 0;
			LocalMonster.Alive := False;
			LocalMonster.Awake := False;
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Reset the level }
procedure TDungeonLevel.Initialise;
var
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.Initialise()');
 
	try
		{ Clear level properties }
		Name := '';
		LevelDepth := 0;
		LevelTheme := 0;
		LevelRating := 0;

		{ Clear the terrain }
		for x := 1 to DUNGEONSIZEX  do
    begin
			for y := 1 to DUNGEONSIZEY do
      begin
        { Set the inner parts of the level to soft diggable terrain }
        if InDungeonBounds(x, y) then
          Terrain[x,y] := T_SOFTWALL
        else
          Terrain[x,y] := T_HARDWALL;

        { Set the default properties }
        Visible[x,y] := 0;
				Monsters[x,y] := 0;
				Effects[x,y] := 0;
				Objects[x,y] := 0;
				Walkable[x,y] := False;
				People[x,y] := 0;
				Zone[x,y] := 0;
        TerrainCost[x,y] := 0;
        Projectiles[x,y] := ' ';
        ProjectilesColour[x,y] := clBlack;
      end;
    end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Place Fountains }
procedure TDungeonLevel.PlaceFountainsOnlevel(FountainsWanted: Integer);
var
  Loop: Integer;
  TempX: Integer;
  TempY: Integer;
  Counter: Integer;
  OffSetX: Integer;
  OffSetY: Integer;
  SuitableLocationFound: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.PlaceFountainsOnlevel()');
 
	try
		{ Initialise a Paranoia counter }
		Counter := 0;

		{ For every fountain wanted }
		for Loop := 1 to FountainsWanted do
		begin
			{ Find a suitable clear area }
			repeat
				{ Get a floor square }
				SuitableLocationFound := True;
				repeat
					TempX := Random(DUNGEONSIZEX) + 1;
					TempY := Random(DUNGEONSIZEY) + 1;
				until ((Terrain[TempX, TempY] = T_FLOOR_ROOM) or 
					(Terrain[TempX, TempY] = T_FLOOR_CORRIDOR));
					
				{ Increase the paranoia counter }
				inc(Counter);
				
				{ Check the space around the square for empty floor }
				for OffsetX := TempX to TempX + 2 do
        begin
					for OffsetY := TempY to TempY + 2 do
					begin
						if ((Terrain[OffsetX, OffsetY] = T_STAIRS_UP) or
							(Terrain[OffsetX, OffsetY] = T_STAIRS_DOWN) or
							(Walkable[OffSetX, OffsetY] = false)  or
							(Objects[OffSetX, OffSetY] > 0)) then 
							SuitableLocationFound := False;
						if (Zone[OffsetX, OffsetY] > 0) then 
							SuitableLocationFound := False;
					end;
        end;
			until (SuitableLocationFound = True) or (Counter > 1000);

			{ If we've found a suitable space }
			if (Counter < 1000) and (SuitableLocationFound) then
			begin    
				{ Draw in the accopanying special terrain }
				if OneChanceIn(2) then
        begin
					Effects[TempX, TempY] := E_SPECIALEFFECT;
          Walkable[TempX, TempY] := False;
        end;
				if OneChanceIn(2) then
        begin
					Effects[TempX + 2, TempY] := E_SPECIALEFFECT;
          Walkable[TempX + 2, TempY] := False;
        end;
				if OneChanceIn(2) then        
        begin
					Effects[TempX + 2, TempY + 2] := E_SPECIALEFFECT;
          Walkable[TempX + 2, TempY + 2] := False;
        end;
				if OneChanceIn(2) then
        begin
					Effects[TempX, TempY + 2] := E_SPECIALEFFECT;
          Walkable[TempX, TempY + 2] := False;
        end;
					
				{ Place the fountain }
				Terrain[TempX + 1, TempY + 1] := T_FOUNTAIN;

				{ Add the fountain info if we have the wizard screen displayed }
				WizardLog(Format('Generated Fountain at %d/%d', [TempX + 1, 
					TempY + 1]));
			end
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Find what stair was used previously to reach this level }
function TDungeonLevel.FindStairsTaken(X, Y, Direction: Integer): Integer;
var
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.FindStairsTaken()');
  
  { Default Result}
  Result := -1;

  try
		{ Given the current location, find out which stair it is }
		for Loop := Low(StairsDownX) to High(StairsDownX) do
    begin
			{ Handle upstairs }
			if Direction = D_UP then
      begin
				if (StairsUpX[Loop] = X) and (StairsUpY[Loop] = Y) then
        begin
					Result := Loop;
          Break;
        end;
      end
			{ Handle downstairs }
			else if Direction = D_DOWN then
      begin
				if (StairsDownX[Loop] = X) and (StairsDownY[Loop] = Y) then
        begin
					Result := Loop;
          Break;
        end;
      end;

      { and exit  if we've found the stair }
      if Result <> -1 then
        break;
    end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the last item (i.e. bottommost) lying on a tile }
function TDungeonLevel.GetLastItemOnTile(Location: TPoint): TItem;
var
  LocalItem: TItem;
  ItemID: Integer;
  NextItem: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.GetLastItemOnTile()');
  
  { Default Result}
  Result := nil;

	try
		{ Make sure we have an item on this tile }
		if (Objects[Location.X, Location.Y] > 0)  and 
			(Objects[Location.X, Location.Y] < ITEM_GOLD) then
		begin
			{ Get the item }
			ItemID := Objects[Location.X, Location.Y];

			{ Iterate through the items looking for the last item }
			repeat
				{ Get the item }
				LocalItem := GItemList[ItemID] as TItem;
				
				{ Get the next item }
				NextItem := LocalItem.NextItem;
				
				{ If we have another item linked to this item, get that item }
				if NextItem <> NO_NEXT_ITEM then
					ItemID := NextItem;
			until NextItem = NO_NEXT_ITEM;

			{ Return the item } 
			Result := LocalItem;
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the number of items lying on a tile }
function TDungeonLevel.GetNumberofItemsOnTile(Location: TPoint): Integer;
var
  LocalItem: TItem;
  ItemID: Integer;
  NextItem: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.GetNumberofItemsOnTile()');
  
  { Default Result}
  Result := 0;

	try
		{ Make sure we have an item on this tile }
		if (Objects[Location.X, Location.Y] > 0)  and 
			(Objects[Location.X, Location.Y] < ITEM_GOLD) then
		begin
			{ Get the item }
			ItemID := Objects[Location.X, Location.Y];
			
			{ Set a running total }
			Result := 1;
			repeat
				{ Get the item }
				LocalItem := GItemList[ItemID] as TItem;

				{ Get the next item }
				NextItem := LocalItem.NextItem;
				if NextItem <> NO_NEXT_ITEM then
				begin
					{ Increment the running total }
					inc(Result);
					
					{ If we have another item, get it }
					ItemID := NextItem;
				end;
			until NextItem = NO_NEXT_ITEM;
		end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the description of all items lying on the tile }
function TDungeonLevel.GetDecriptionOfItemsOnTile(Location: TPoint): String;
var
  LocalItem: TItem;
  ItemID: Integer;
  NextItem: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.GetDecriptionOfItemsOnTile()');
  
  { Default Result}
  Result := '';

	try
		{ Make sure we have an item on this tile }
		if (Objects[Location.X, Location.Y] > 0)  and 
			(Objects[Location.X, Location.Y] < ITEM_GOLD) then
		begin
			{ Get the item }
			ItemID := Objects[Location.X, Location.Y];
			repeat
				{ Add the item name as a comma-seperated list }
				LocalItem := GItemList[ItemID] as TItem;
				Result := Result + Lower(LocalItem.Name) + ', ';

				{ Get the next item }
				NextItem := LocalItem.NextItem;
				if NextItem <> NO_NEXT_ITEM then
					{ If we have another item, get it }
					ItemID := NextItem;
			until NextItem = NO_NEXT_ITEM;
		end;

		{ Get rid of any extraneous spaces }
  	Result := Trim(Result);
  
  	{ Get rid the final comma }
  	Delete(Result, Length(Result), 1);
  
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

procedure TDungeonLevel.RegenerateTownsPeople;
var
  GuardType: Integer;
  LocalMonster: TMonster;
  X: Integer;
  Y: Integer;
  index: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.RegenerateTownsPeople()');

	try
    { First, remove any existing townspeople from the map }
    for x:=1 to (DUNGEONSIZEX) do
			for y:=1 to (DUNGEONSIZEY) do
        if Monsters[X, Y] <> 0 then
          Monsters[X, Y] := 0;

    { Then empty the townspeople list }
    GTownsPeopleList.Clear;

    { Add a placeholder monster to avoid 0-indexing }
    LocalMonster := TMonster.Create;
    GTownsPeopleList.Add(LocalMonster);

    { Now start adding people to the list, starting with the specials first }

    { Add character's housekeeper }
    LocalMonster := TMonster.Create;
    LocalMonster.Name := NPCTypes[HOUSEKEEPER];
    LocalMonster.ID := HOUSEKEEPER;
    LocalMonster.Colour := clGreen;
    LocalMonster.Char := '@';

    { Find random location in house }
    repeat
      X := Random(DUNGEONSIZEX) + 1;
      Y := Random(DUNGEONSIZEY) + 1;
    until ((Terrain[X, Y] = 24 ) and (Monsters[X, Y] = 0));
    LocalMonster.X := X;
    LocalMonster.Y := Y;
    Monsters[X, Y] := GTownsPeopleList.Count;
    GTownsPeopleList.Add(LocalMonster);

    { Add character's cats }
    LocalMonster := TMonster.Create;
    LocalMonster.Name := NPCTypes[PETCAT];
    LocalMonster.ID := PETCAT;
    LocalMonster.Colour := clYellow;
    LocalMonster.Char := 'c';

    { Find random location in house }
    repeat
      X := Random(DUNGEONSIZEX) + 1;
      Y := Random(DUNGEONSIZEY) + 1;
    until ((Terrain[X, Y] = 24 ) and (Monsters[X, Y] = 0));
    LocalMonster.X := X;
    LocalMonster.Y := Y;
    Monsters[X, Y] := GTownsPeopleList.Count;
    GTownsPeopleList.Add(LocalMonster);

    { Add character's cats }
    LocalMonster := TMonster.Create;
    LocalMonster.Name := NPCTypes[PETCAT];
    LocalMonster.ID := PETCAT;
    LocalMonster.Colour := clAqua;
    LocalMonster.Char := 'c';

    { Find random location in house }
    repeat
      X := Random(DUNGEONSIZEX) + 1;
      Y := Random(DUNGEONSIZEY) + 1;
    until ((Terrain[X, Y] = 24 ) and (Monsters[X, Y] = 0));
    LocalMonster.X := X;
    LocalMonster.Y := Y;
    Monsters[X, Y] := GTownsPeopleList.Count;
    GTownsPeopleList.Add(LocalMonster);

    { Now add a certain number of guards to the outside squares }
    for Index := 0 to NUMBER_OF_GUARDS do
    begin
      LocalMonster := TMonster.Create;
      GuardType := Random(High(NPCAdjective)) + 1;
      LocalMonster.Name := Format('%s %s',
        [NPCAdjective[GuardType], NPCTypes[GUARDS]]);
      LocalMonster.Colour := Darker(clSilver, GuardType * 2);
      LocalMonster.ID := GuardType;
      LocalMonster.Char := 'g';

      { Find a random location to place the townsperson }
      repeat
        X := Random(DUNGEONSIZEX) + 1;
        Y := Random(DUNGEONSIZEY) + 1;
      until ((Terrain[X, Y] = 9 ) and (Monsters[X, Y] = 0));
      LocalMonster.X := X;
      LocalMonster.Y := Y;
      Monsters[X, Y] := GTownsPeopleList.Count;

      { Add the townsperson }
      GTownsPeopleList.Add(LocalMonster);
    end;


    { Now set all the townspeople's standard flags }
    for Index := 0 to GTownsPeopleList.Count - 1 do
    begin
      LocalMonster := GTownsPeopleList.Items[Index] as TMonster;
      LocalMonster.Hostile := False;
      LocalMonster.Awake := True;
      LocalMonster.Alive := True;
    end;

 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Restock the town level shops }
procedure TDungeonLevel.RestockShops(LevelToUse: Integer);
var
  X: Integer;
  Y: Integer;
  Chance: Integer;
  ShopStart: TPoint;
  Item: TItem;
  ItemType: crItemType;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonBranch.RestockShops()');

	try
		{ TODO: this routine could do with additional refactoring - it is
		  hardcoded to the positions on the town level }
    if LevelToUse < 1
			then LevelToUse := 1;

    { Regenerate the townspeople as well}
    RegenerateTownsPeople;

		{ Weapon Shop }
		ShopStart := Point(183, 195);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				ItemType := iWeapon;
				
				{ Define the quality of the item and create it }
				Chance := Random(PERCENTAGE);
				if Chance > 95 then
					Item := TItem.Create(iEpic, LevelToUse, ItemType, False)
				else if Chance > 80 then
					Item := TItem.Create(iLegendary, LevelToUse, ItemType, False)
				else if Chance > 40 then
					Item := TItem.Create(iSuperb, LevelToUse, ItemType, False)
				else
					Item := nil;

				{ If we have an item add it to the shop }
				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := False;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;

		{ Jewellery Shop }
		ShopStart := Point(176, 197);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				if OneChanceIn(2) then 
					ItemType := iRing 
				else 
					ItemType := iAmulet;
					
				{ Define the quality of the item and create it }	
				Chance := Random(PERCENTAGE);
				if Chance > 95 then
					Item := TItem.Create(iEpic, LevelToUse, ItemType, False)
				else if Chance > 80 then
					Item := TItem.Create(iLegendary, LevelToUse, ItemType, False)
				else if Chance > 40 then
					Item := TItem.Create(iSuperb, LevelToUse, ItemType, False)
				else
					Item := nil;

				{ If we have an item add it to the shop }
				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := False;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;

		{ Potion Shop }
		ShopStart := Point(180, 188);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				ItemType := iPotion;
				
				{ Create the item }
				Item := TItem.Create(iSuperb, LevelToUse, ItemType, False);

				{ If we have an item add it to the shop }
				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := False;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;

		{ Scroll Shop }
		ShopStart := Point(217, 188);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				ItemType := iScroll;
				
				{ Create the item }
				Item := TItem.Create(iSuperb, LevelToUse, ItemType, False);

				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := False;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;

		{ Food Shop }
		ShopStart := Point(214, 195);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				ItemType := iConsumable;
				
				{ Create the item }
				Item := TItem.Create(iSuperb, LevelToUse, ItemType, False);
				
				{ If we have an item add it to the shop }
				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := True;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;

		{ Armour Shop }
		ShopStart := Point(221, 197);
		
		{ For each square inside the shop }
		for X := 0 to 2 do
    begin
			for Y := 0 to 3 do
			begin
				{ Empty the square }
				Objects[ShopStart.X + X, ShopStart.Y + Y] := 0;
				
				{ Select an item type }
				ItemType := iArmour;
				
				{ Define the quality of the item and create it }	
				Chance := Random(PERCENTAGE);
				if Chance > 95 then
					Item := TItem.Create(iEpic, LevelToUse, ItemType, False)
				else if Chance > 80 then
					Item := TItem.Create(iLegendary, LevelToUse, ItemType, False)
				else if Chance > 40 then
					Item := TItem.Create(iSuperb, LevelToUse, ItemType, False)
				else
					Item := nil;

				{ If we have an item add it to the shop }
				if Assigned(Item) then
				begin
					Item.Location := iFloor;
					Item.Known := False;
					GItemList.Add(Item);
					Objects[ShopStart.X + X, ShopStart.Y + Y] := GItemList.Count - 1;
				end;
			end;
    end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Load any previous adjacent monsters from the previous level }
procedure TDungeonLevel.LoadAdjacentMonsters(PreviousLevel: TDungeonLevel;
  MonstersToPutDown: Integer);
var
  FreeSpaces: Array[1..3, 1..3] of TPoint;
  SpaceCount: Integer;
  X: Integer;
  Y: Integer;
  TopLeft: Integer;
  TopRight: Integer;
  EmptyPoint: TPoint;
  Monster: TMonster;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.LoadAdjacentMonsters()');

	try
    { Check if we have any monsters to put down }
    if MonstersToPutDown > 0 then
    begin
      { Initialise the Freespace array }
      EmptyPoint := Point(-1, -1);

      for X := 1 to 3 do
        for Y := 1 to 3 do
          FreeSpaces[X, Y] := EmptyPoint;

      { Set the free space count }
      SpaceCount := 0;

      { First find any adjacent free spaces, without monsters where we can put down the
        monsters from previous levels }
      { Count and store any adjacent monsters }
      TopLeft := FormDisplay.Game.PlayerX - 2;
      TopRight := FormDisplay.Game.PlayerY - 2;
      for X := FormDisplay.Game.PlayerX - 1 to FormDisplay.Game.PlayerX + 1 do
      begin
        for Y := FormDisplay.Game.PlayerY - 1 to FormDisplay.Game.PlayerY + 1 do
        begin
          { don't check the player square }
          if (X <> FormDisplay.Game.PlayerX) and
            (Y <> FormDisplay.Game.PlayerY) then
          begin
            { Look for floors and non-monster squares }
            if (Walkable[X, Y]) and (Monsters[X, Y] < 1) then
            begin
              { Increase the count of suitable spaces found }

              { Store the free space }
              FreeSpaces[X - TopLeft, Y - TopRight] := Point(X, Y);
            end;
          end;
        end;
      end;

      { Then put down monsters if we have any }
      for X := 1 to 3 do
        for Y := 1 to 3 do
        begin
          { Find a nearby square empty of monsters and suitable for placement }
          if (FreeSpaces[X, Y].X <> 0) and (FreeSpaces[X, Y].Y <> 0) and
            (NearbyMonsters[X, Y] <> -1) then
          begin
            { Set the monster }
            Monsters[TopLeft + X, TopRight + Y] := NearbyMonsters[X, Y];

            { Alert the player that a monster has travelled up the stairs }
            Monster := GMonsterList[Monsters[TopLeft + X, TopRight + Y]] as
              TMonster;
            UnitEngine.UpDateLog(Format('%s follows you', [Monster.Name]),
              mesDefault);
          end;

        end;

      { Remove the putdown monsters from the previous level }
    end;
  except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ Save any adjacent monsters }
function TDungeonLevel.SaveAdjacentMonsters: Integer;
var
  X: Integer;
  Y: Integer;
  TopLeft: Integer;
  TopRight: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TDungeonLevel.SaveAdjacentMonsters()');

  { Default Result }
  Result := 0;

	try
    { Initialise array to hold any adjacent monsters }
    for X := 1 to 3 do
      for Y := 1 to 3 do
        NearbyMonsters[X, Y] := -1;

    { Count and store any adjacent monsters }
    TopLeft := FormDisplay.Game.PlayerX - 2;
    TopRight := FormDisplay.Game.PlayerY - 2;
    for X := FormDisplay.Game.PlayerX - 1 to FormDisplay.Game.PlayerX + 1 do
    begin
      for Y := FormDisplay.Game.PlayerY - 1 to FormDisplay.Game.PlayerY + 1 do
      begin
        { don't check the player square }
        if (X <> FormDisplay.Game.PlayerX) and
          (Y <> FormDisplay.Game.PlayerY) then
        begin
          { If we have a monster on the square... }
          if Monsters[X, Y] > 0 then
          begin
            { Increase the number of monsters found }
            Inc(Result);

            { Store the monster }
            NearbyMonsters[X - TopLeft, Y - TopRight] := Monsters[X, Y];
          end;
        end;
      end;
    end;
 	except
 	 	{ in case of error, log the Exception }
 	 	on E: Exception do hLog.AddException(E);
  end;
end;

{ The following FOV Functions were originally by Henri Haki
  http://roguebasin.roguelikedevelopment.org/index.php?title=
  FOV_using_recursive_shadowcasting_-_improved
  
  They have been modified slightly to fit into TDungeonLevel - the main
  change is that I've flipped the y-axis ahandling as the origin of maps
  is at the top left. I've left their formatting as I found it. }

{ FOV Routines }
function TDungeonLevel.GetSlopeStd(x1, y1, x2, y2 : single) : single;
begin
  Result := (x1 - x2) / (y1 - y2);
end;

{ FOV Routines }
function TDungeonLevel.GetSlopeInv(x1, y1, x2, y2 : single) : single;
begin
  Result := (y1 - y2) / (x1 - x2);
end;

{ FOV Routines }
function TDungeonLevel.GetVisDistance(x1, y1, x2, y2 : integer) : integer;
begin
  Result := (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
end;

{ FOV Routines }
procedure TDungeonLevel.RecursiveVisibility(xpos1,ypos1: Integer; oct, depth : integer; slopeA, slopeB : single);
var
  x, y : integer;
begin
  case oct of
    1 : begin
      y := ypos1 - depth;                                                { initialize y }
      x := round(xpos1 - slopeA * depth);                                { initialize z }
      while GetSlopeStd(x, y, xpos1, ypos1) >= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if (Walkable[x, y] = False) then begin             { if obstruction }
            if (Walkable[x - 1, y] = True) then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 1, depth + 1,
               slopeA, GetSlopeStd(x - 0.5, y + 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if (Walkable[x - 1, y] = False) then begin       { if prior obstruction }
              slopeA := GetSlopeStd(x - 0.5, y - 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                           { set block visible }
        end;
        inc(x);
      end;
      dec(x)
    end;
    2 : begin
      y := ypos1 - depth;                                                { initialize y }
      x := round(xpos1 - slopeA * depth);                                { initialize z }
      while GetSlopeStd(x, y, xpos1, ypos1) <= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x + 1, y] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 2, depth + 1, slopeA, GetSlopeStd(x + 0.5, y + 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x + 1, y] = False then begin       { if prior obstruction }
              slopeA := GetSlopeStd(x + 0.5, y - 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                         { set block visible }
        end;
        dec(x);
      end;
      inc(x)
    end;
    3 : begin
      x := xpos1 + depth;                                                { initialize y }
      y := round(ypos1 + slopeA * depth);                                { initialize z }
      while GetSlopeInv(x, y, xpos1, ypos1) <= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x, y - 1] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 3, depth + 1, slopeA, GetSlopeInv(x - 0.5, y - 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x, y - 1] = False then begin       { if prior obstruction }
              slopeA := GetSlopeInv(x + 0.5, y - 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                             { set block visible }
        end;
        inc(y);
      end;
      dec(y)
    end;
    4 : begin
      x := xpos1 + depth;                                                { initialize y }
      y := round(ypos1 + slopeA * depth);                                { initialize z }
      while GetSlopeInv(x, y, xpos1, ypos1) >= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x, y + 1] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 4, depth + 1, slopeA, GetSlopeInv(x - 0.5, y + 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x, y + 1] = False then begin       { if prior obstruction }
              slopeA := GetSlopeInv(x + 0.5, y + 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                            { set block visible }
        end;
        dec(y);
      end;
      inc(y)
    end;
    5 : begin
      y := ypos1 + depth;                                                { initialize y }
      x := round(xpos1 + slopeA * depth);                                { initialize z }
      while GetSlopeStd(x, y, xpos1, ypos1) >= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x + 1, y] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 5, depth + 1, slopeA, GetSlopeStd(x + 0.5, y - 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x + 1, y] = False then begin       { if prior obstruction }
              slopeA := GetSlopeStd(x + 0.5, y + 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                            { set block visible }
        end;
        dec(x);
      end;
      inc(x)
    end;
    6 : begin
      y := ypos1 + depth;                                                { initialize y }
      x := round(xpos1 + slopeA * depth);                                { initialize z }
      while GetSlopeStd(x, y, xpos1, ypos1) <= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x - 1, y] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 6, depth + 1, slopeA, GetSlopeStd(x - 0.5, y - 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x - 1, y] = False then begin       { if prior obstruction }
              slopeA := GetSlopeStd(x - 0.5, y + 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                              { set block visible }
        end;
        inc(x);
      end;
      dec(x)
    end;
    7 : begin
      x := xpos1 - depth;                                                { initialize y }
      y := round(ypos1 - slopeA * depth);                                { initialize z }
      while GetSlopeInv(x, y, xpos1, ypos1) <= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x, y + 1] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 7, depth + 1, slopeA, GetSlopeInv(x + 0.5, y + 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x, y + 1] = False then begin       { if prior obstruction }
              slopeA := GetSlopeInv(x - 0.5, y + 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                              { set block visible }
        end;
        dec(y);
      end;
      inc(y)
    end;
    8 : begin
      x := xpos1 - depth;                                                { initialize y }
      y := round(ypos1 - slopeA * depth);                                { initialize z }
      while GetSlopeInv(x, y, xpos1, ypos1) >= slopeB do begin            { while in octant }
        if GetVisDistance(x, y, xpos1, ypos1) <= FormDisplay.Game.mw then begin            { if within max visual range }
          if Walkable[x, y] = False then begin             { if obstruction }
            if not Walkable[x, y - 1] = False then begin   { if no prior obstruction }
              RecursiveVisibility(xpos1, ypos1, 8, depth + 1, slopeA, GetSlopeInv(x + 0.5, y - 0.5, xpos1, ypos1));
            end;                                                        { ^create recursive scan }
          end else begin                                                { no obstruction }
            if Walkable[x, y - 1] = False then begin       { if prior obstruction }
              slopeA := GetSlopeInv(x - 0.5, y - 0.5, xpos1, ypos1);      { adjust slope for later recursion }
            end;
          end;
          Visible[x, y] := 1;                              { set block visible }
        end;
        inc(y);
      end;
      dec(y)
    end;
  end;

  if (depth < FormDisplay.Game.mv) and Walkable[x, y] = True then begin   { break/continue }
    RecursiveVisibility(xpos1, ypos1, oct, depth + 1, slopeA, slopeB);
  end;
end;


end.
