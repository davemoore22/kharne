{ UnitWizard

  Copyright (c) 2007-2009 Dave Moore 

  Wizard Form

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

unit UnitWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, HotLog, UnitMonster, UnitVars, UnitFunctions,
  UnitDefines, UnitConst;

{ Wizard Form }
type
  TFormWizard = class(TForm)
    ListViewMonsters: TListView;
    ButtonGainXP: TButton;
    ButtonGainGold: TButton;
    ButtonHeal: TButton;
    ButtonGetItem: TButton;
    MemoLog: TMemo;
    ButtonMagicMap: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ButtonGainXPClick(Sender: TObject);
    procedure ButtonGainGoldClick(Sender: TObject);
    procedure ButtonHealClick(Sender: TObject);
    procedure ButtonGetItemClick(Sender: TObject);
    procedure ButtonMagicMapClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentLeft: Integer;
    CurrentTop: Integer;
    procedure RefreshDisplay;
  end;

var
  FormWizard: TFormWizard;

implementation

uses UnitDisplay, UnitDungeon, UnitEngine, UnitItem;

{$R *.dfm}

{ On Showing the Form move it to the previous location }
procedure TFormWizard.FormShow(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.FormShow()');

  try
    CurrentLeft := Self.Left;
    CurrentTop := Self.Top;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Refresh the Display }
procedure TFormWizard.RefreshDisplay;
var
  Loop: Integer;
  ListItem: TListItem;
  LocalMonster: TMonster;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormWizard.RefreshDisplay()');

  try
    { Switch off Screen Updates to avoid flicker }
    ListViewMonsters.Selected := nil;
    ListViewMonsters.Items.BeginUpdate;

    { Clear the list of Visible Monsters }
    ListViewMonsters.Items.Clear;

    { Populate the list of Visible Monsters }
    for Loop := 0 to ActiveMonsterList.Count - 1 do
    begin
      LocalMonster := ActiveMonsterList[Loop] as TMonster;
      if LocalMonster.Alive then
      begin
        { Temporarily set the Report Style to vsSmallIcon to stop the annoying
          '...' appearing and trimming the captions }
        ListViewMonsters.ViewStyle := vsSmallIcon;

        { Add the Item to the ListView }
        ListItem := ListViewMonsters.Items.Add;

        { Display various properties of the Monster }
        ListItem.Caption := IntToStr(LocalMonster.ID);
        ListItem.SubItems.Add(LocalMonster.Name);
        ListItem.SubItems.Add(LocalMonster.Char);
        ListItem.SubItems.Add(PointToStr(Point(LocalMonster.X,
          LocalMonster.Y)));
        ListItem.SubItems.Add(IntToStr(FormDisplay.Game.Dungeon.Monsters
          [LocalMonster.X, LocalMonster.Y]));
        ListItem.SubItems.Add(IntToStr(LocalMonster.Energy));
        ListItem.SubItems.Add(IntToStr(LocalMonster.CurrentHP));
        ListItem.SubItems.Add(BoolToStr(LocalMonster.Alive, True));
        ListItem.SubItems.Add(BoolToStr(LocalMonster.Awake, True));

        { Reset the style back to the correct style }
        ListViewMonsters.ViewStyle := vsReport;
      end;
    end;

    { Switch Screen Updates back on }
    ListViewMonsters.Items.EndUpdate;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Hide the Form }
procedure TFormWizard.FormHide(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.FormHide()');

  try
    { Store the Current Screen Location }
    CurrentLeft := Self.Left;
    CurrentTop := Self.Top;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Gain some XP }
procedure TFormWizard.ButtonGainXPClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.ButtonGainXPClick()');

  try
    { Gain XP }
    FormDisplay.Game.Player.GainXP(FormDisplay.Game.Player.Levels *
      WIZARD_MODE_XP_GAIN_STEP);

    { Pass a Turn }
    UnitEngine.PassATurn();
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Gain some Gold }
procedure TFormWizard.ButtonGainGoldClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.ButtonGainGoldClick()');

  try
    { Gain Gold }
    FormDisplay.Game.Player.Gold := FormDisplay.Game.Player.Gold +
      WIZARD_MODE_GOLD_GAIN;

    { Pass a Turn }
    UnitEngine.PassATurn();
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Heal }
procedure TFormWizard.ButtonHealClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.ButtonHealClick()');

  try
    { Heal up the Player }
    FormDisplay.Game.Player.HP := FormDisplay.Game.Player.MaxHP;

    { Pass a Turn }
    UnitEngine.PassATurn();
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get a random magical item }
procedure TFormWizard.ButtonGetItemClick(Sender: TObject);
var
  ItemQuality: crItemQuality;
  ItemQualityRatio: Integer;
  ItemType: crItemType;
  LocalItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.ButtonGetItemClick()');

  try
    { To save time and effort, only allow this to work whenever there is no
      item currently on the player's space }
    if (FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
      FormDisplay.Game.PlayerY] = 0) then
    begin
      { Generate a random quality item }
      ItemQualityRatio := Random(PERCENTAGE) + 1;

      { All items generated this way are magical, and they have double the
        standard chance of being legendary of better items }
      if (ItemQualityRatio < CHANCE_ARTIFACT * 2) then
        ItemQuality := iArtifact
      else if (ItemQualityRatio < CHANCE_EPIC * 2) then
        ItemQuality := iEpic
      else if (ItemQualityRatio < CHANCE_LEGENDARY * 2) then
        ItemQuality := iLegendary
      else                         
        ItemQuality := iSuperb;

      { Now randomly choose an item type }
      ItemQualityRatio := Random(PERCENTAGE) + 1;

      { Choose one of four item types }
      if (ItemQualityRatio < CHANCE_WEAPON) then
        ItemType := iWeapon
      else if (ItemQualityRatio < CHANCE_ARMOUR) then
        ItemType := iArmour
      else if (ItemQualityRatio < CHANCE_RING) then
        ItemType := iRing
      else
        ItemType := iAmulet;

      { Create the item }
      LocalItem := TItem.Create(ItemQuality,
        FormDisplay.Game.Player.Levels + 1, ItemType, False);

      { Wizard-gifted items are always identified }
      LocalItem.Known := True;

      { Add the item to the ItemList }
      GItemList.Add(LocalItem);

      { Place the item on the Dungeon floor on the Player's Square}
      LocalItem.Location := iFloor;
      FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX,
        FormDisplay.Game.PlayerY] := GItemList.Count - 1;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Map the whole level }
procedure TFormWizard.ButtonMagicMapClick(Sender: TObject);
var
  X: Integer;
  Y: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormWizard.ButtonMagicMapClick()');

  { Iterate through the entire map, making all squares that the Player can
    either walk on or see visible }
  try
    for X := 1 to DUNGEONSIZEX do
      for Y := 1 to DUNGEONSIZEY do
        if (FormDisplay.Game.Dungeon.Visible[X, Y] = NOT_YET_VISIBLE) and
          (FormDisplay.Game.Dungeon.CountWalls(X, Y) <> TOTALLY_SURROUNDED) then
          FormDisplay.Game.Dungeon.Visible[X, Y] := PREVIOUSLY_VISIBLE;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
