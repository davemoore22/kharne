{ UnitHiScores

  Copyright (c) 2007-2009 Dave Moore 

  Implements Hiscore Functionality

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

unit UnitHiScores;

interface

uses ComCtrls, Graphics, UnitDataModule, Math, SysUtils, SQLiteTable3, HotLog,
  UnitVersion, UnitDisplay, UnitFunctions, UnitDungeon, UnitVars,  UnitCreature,
  UnitConst, UnitItem, UnitOtherFuncs;

{ Calculate the score }
function CalculateScore(Player: TCreature): Integer;

{ Write Hiscores to the Database }
function WriteHiScores: String;

{ Display the Hiscores to a TRichEdit }
procedure DisplayHiScores(RichEdit: TRichEdit; CurrentGame: String;
  UsePlayer: Boolean);

implementation

{ Calculate the score }
function CalculateScore(Player: TCreature): Integer;
var
  Loop: Integer;
  LocalItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitHiScores.CalculateScore()');

  { Default result }
  Result := 0;

  try
    { Basic Score Calculations }
    Inc(Result, FormDisplay.Game.Turns div 10);
    Inc(Result, Player.Gold);
    Inc(Result, Player.XP);

    { Increase Score by a factor of the value of the items carried }
    for Loop := S_INVENTORY_BEGIN to S_INVENTORY_END do
    begin
      if Player.Inventory[Loop] > NO_ITEM_IN_SLOT then
      begin
        LocalItem := GItemList[Player.Inventory[Loop]] as TItem;
        Inc(Result, LocalItem.Value div INVENTORY_SCORE_ITEM_DIVISOR);
      end;
    end;

    { Increase Score by a factor of the value of the items worn }
    for Loop := S_FEET to S_BACK do
    begin
      if Player.Inventory[Loop] > NO_ITEM_IN_SLOT then
      begin
        LocalItem := GItemList[Player.Inventory[Loop]] as TItem;
        Inc(Result, LocalItem.Value div WORN_SCORE_ITEM_DIVISOR);
      end;
    end;

    { Increase Score by a factor of how much exploring has been done }
    for Loop := Low(FormDisplay.Game.GDepthDelved) to
      High(FormDisplay.Game.GDepthDelved) do
      if FormDisplay.Game.GDepthDelved[Loop] > 0 then
        Inc(Result, Trunc(Power((FormDisplay.Game.GDepthDelved[Loop] - 1) * 10,
        2)));

    { Increase Score by the skills learned }
    for Loop := SKILL_START to SKILL_END do
      if Player.Skills[Loop] > UNSKILLED then
        Inc(Result, Trunc(Power(Player.Skills[Loop], 1.25)));

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Write a HiScore to the Database }
function WriteHiScores: String;
var
  GameGUID: TGUID;
  InsertScoreSQL: String;
  SQL: String;
  HiScoreID: Integer;
  LocalScore: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitHiScores.WriteHiScores()');

  { Default result }
  Result := '';

  try
    { Create the datamodule }
    DataModuleMain := TDataModuleMain.Create(nil);
    try
      with DataModuleMain do
      begin
        { Get the next HiScoreID }
        HiScoreID := 0;

        { Transaction is already open via the constructor of the Data Module }
        SLTable :=
          SlDatabase.GetTable('SELECT MAX(HISCOREID) AS MAXHISCOREID FROM HISCORES');
        while (not(SLTable.EOF)) do
        begin
          HiScoreID :=
            StrToIntDef(SLTable.FieldAsString(SLTable.FieldIndex['MAXHISCOREID']), 0);
          SLTable.Next;
        end;
        Inc(HiScoreID);

        { Now calculate the Score for the current game }
        LocalScore := CalculateScore(FormDisplay.Game.Player);

        { Generate a new ID }
        CreateGUID(GameGUID);

        { Format the SQL for Insertion }
        InsertScoreSQL := 'INSERT INTO HISCORES (HISCOREID, GAMEID, ' +
          'SCOREDATETIME, VERSION, CHARACTERNAME, LEVEL, KILLEDBY, CLASS, '+
          'RACE, KILLEDLOCATION, TURNS, SCORE) VALUES (''%d'', ''%s'', ' +
          '''%s'', ''%s'', ''%s'', ''%d'', ''%s'', ''%s'', ''%s'', "%s", ' +
          '''%d'', ''%d'')';
        SQL := Format(InsertScoreSQL, [HiScoreID, GUIDToString(GameGUID),
          DateTimeToStr(Now), GetVersion, FormDisplay.Game.Player.Name,
          FormDisplay.Game.Player.Levels, FormDisplay.Game.Player.KilledBy,
          GetClass(FormDisplay.Game.Player.CClass),
          GetRace(FormDisplay.Game.Player.Subrace),
          FormDisplay.Game.Dungeon.Name + ' (' +
          IntToStr(FormDisplay.Game.Dungeon.LevelDepth) + ')',
          FormDisplay.Game.Turns, LocalScore]);

        { Insert the Hi Score }
        SLDatabase.ExecSQL(SQL);
      end;
    finally
      { Get the ID of the HIScore we've just written }
      Result := GUIDToString(GameGUID);

      { Free the datamodule in all circumstances }
      DataModuleMain.Free;
      DataModuleMain := nil;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Write out the hiscores }
procedure DisplayHiScores(RichEdit: TRichEdit; CurrentGame: String;
  UsePlayer: Boolean);
var
  GameGUID: String;
  ScoreColour: TColor;
  Position: Integer;
  Score: Integer;
  CharName: String;
  Level: Integer;
  CClass: String;
  Race: String;
  KilledBy: String;
  KilledLoc: String;
  LocalScore: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TItemEnchantment.DisplayHiScores()');
  
  { Build a list of the current top scores }
  try
    { Connect to the Database to read the hiscorfes in }
    DataModuleMain := TDataModuleMain.Create(nil);
    try
      with DataModuleMain do
      begin
        { Check if we want a player header at the top of the high scores - if
          we are not in a game and simply want to view the hiscores (for
          example, from the title screen) then we don't }
        if UsePlayer then
        begin
          { Build up the header }
          RichEdit.Lines.Clear;
          RichEdit.SelAttributes.Size := 14;
          RichEdit.SelAttributes.Color := clWhite;
          RichEdit.SelText := 'Goodbye ';
          RichEdit.SelAttributes.Color := clYellow;
          RichEdit.SelText := FormDisplay.Game.Player.Name;
          RichEdit.SelAttributes.Color := clWhite;
          RichEdit.SelText := '...' + #10 + #10;

          { Output Game Info }
          RichEdit.SelAttributes.Color := clAqua;
          RichEdit.SelText := #9 + Format('%s the Level %d %s %s',
            [FormDisplay.Game.Player.Name, FormDisplay.Game.Player.Levels,
            GetRace(FormDisplay.Game.Player.Subrace),
            GetClass(FormDisplay.Game.Player.CClass)]) + #10;
          RichEdit.SelAttributes.Color := clSilver;
          RichEdit.SelText := #9 + 'Began the game on ' +
            DateTimeToStr(FormDisplay.Game.GameStartTime) + #10;
          RichEdit.SelAttributes.Color := clSilver;
          RichEdit.SelText := #9 + Format('Died %d turns later on %s after %s',
            [FormDisplay.Game.Turns, DateTimeToStr(Now), GetElapsedGameTime])
            + #10;
          RichEdit.SelAttributes.Color := clSilver;
          RichEdit.SelText := #9 + Format('%s on Level %d of the %s',
            [FormDisplay.Game.Player.KilledBy,
            FormDisplay.Game.Dungeon.LevelDepth,
            FormDisplay.Game.Dungeon.Name]) + #10;
          RichEdit.SelAttributes.Color := clSilver;
          RichEdit.SelText := #9 + 'With a score of ';
          RichEdit.SelAttributes.Color := clRed;
          LocalScore := CalculateScore(FormDisplay.Game.Player);
          RichEdit.SelText := IntToStr(LocalScore);
          RichEdit.SelAttributes.Color := clSilver;
          RichEdit.SelText := ' points.' + #10#10#10;
        end;

        { Now display the table header }
        RichEdit.SelAttributes.Color := clWhite;
        RichEdit.SelText := 'Best Scores:' + #10 + #10;
        RichEdit.SelAttributes.Size := 9;
        RichEdit.SelAttributes.Color := clLime;
        RichEdit.SelText := '#' + #9 + 'Score  ' + #9 + 'Name/Class' + #9#9 +
          'Fate' + #10;
        ScoreColour := clYellow;

        { Start with the first hiscore }
        Position := 1;

        { Read the hiscores in, highest score first }
        SLTable :=
          SlDatabase.GetTable('SELECT * FROM HISCORES ORDER BY SCORE DESC');

        { Whilst we have room to display the hiscores }
        while (not(SLTable.EOF) and (Position < MAXHISCORESDISPLAYED)) do
        begin
          { Get the Hiscore info from the row in the database }
          GameGUID := SLTable.FieldAsString(SLTable.FieldIndex['GAMEID']);
          Score := SLTable.FieldAsInteger(SLTable.FieldIndex['SCORE']);
          CharName :=
            SLTable.FieldAsString(SLTable.FieldIndex['CHARACTERNAME']);
          Level := SLTable.FieldAsInteger(SLTable.FieldIndex['LEVEL']);
          CClass := SLTable.FieldAsString(SLTable.FieldIndex['CLASS']);
          Race := SLTable.FieldAsString(SLTable.FieldIndex['RACE']);
          KilledBy := SLTable.FieldAsString(SLTable.FieldIndex['KILLEDBY']);
          KilledLoc :=
            SLTable.FieldAsString(SLTable.FieldIndex['KILLEDLOCATION']);

          { Flag the current game as a different colour }
          if GameGUID = CurrentGame then
            RichEdit.SelAttributes.Color := clAqua
          else
          begin
            { Do the colour gradient }
            ScoreColour := Darker(ScoreColour, 7);
            RichEdit.SelAttributes.Color := ScoreColour;
          end;

          { Write out each hiscore }
          RichEdit.SelAttributes.Size := 9;
          RichEdit.SelText := IntToStr(Position) + '.' + #9;
          RichEdit.SelText := Format('%-7.7d', [Score]) + #9;
          RichEdit.SelText := Format('%-20.20s', [CharName + ' (L' +
            IntToStr(Level) + ' ' + Copy(CClass,1,3) + ')']) + #9;
          RichEdit.SelText := KilledBy + ' in ' + KilledLoc + #9;
          RichEdit.SelText := #10;

          { Continue on to the next hiscore }
          SLTable.Next;
          Inc(Position);
        end;

        { Add instructions to the bottom }
        if UsePlayer then
          FormDisplay.LabelKeysHiScores.Caption := 'Press [S] to save Character'
            + ' Dump, [V] to view Character Dump, or [ESC] to return'
        else
          FormDisplay.LabelKeysHiScores.Caption := 'Press [ESC] to return';
      end;
    finally
      { In all circumstances, free the datamodule }
      DataModuleMain.Free;
      DataModuleMain := nil;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
