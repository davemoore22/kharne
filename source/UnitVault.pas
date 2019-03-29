{ UnitVault

  Copyright (c) 2007-2009 Dave Moore 

  Vault Definitions and Miscellaneous Vault Functions

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

unit UnitVault;

interface

uses Classes, Contnrs, SysUtils, HotLog, UnitConst, UnitVars;

{ TVault represents individual vault designs. These are used during level
  creation whenever a vault is built. These are populated from the VAULTS table
  in the database }
type
  TVault = class

private
  { Private Members }
  FVaultID: Integer;      // Unique Identifier
  FVaultName: String;     // Short Description of this Vault's theme
  FVaultLevel: Integer;   // Level at which the Vault first appears
  FVaultEcology: String;  // Preferred Home of the Vault
  FVaultX: Integer;       // Vault Size Across (in map cells)
  FVaultY: Integer;       // Vault Size Down (in map cells)
  FVaultMap: TStringList; // Vault Matrix (should be FVaultX * FVaultY size)
  FVaultScore: Integer;   // How "difficult" the Vault is

  { Returns the VaultMap as a String }
  function GetVaultMap: String;
public
  { Constructors }
  constructor Create; overload;
  constructor Create(VaultID: Integer;
                     VaultName: String;
                     VaultLevel: Integer;
                     VaultEcology: String;
                     VaultX: Integer;
                     VaultY: Integer;
                     VaultMap: String;
                     VaultScore: Integer); overload;

  { Destructors }
  destructor Destroy; override;

  { Accessors }
  property VaultID: Integer read FVaultID;
  property VaultName: String read FVaultName;
  property VaultLevel: Integer read FVaultLevel;
  property VaultEcology: String read FVaultEcology;
  property VaultX: Integer read FVaultX;
  property VaultY: Integer read FVaultY;
  property VaultMap: String read GetVaultMap;
  property VaultScore: Integer read FVaultScore;
end;

{ Return a random vault, optionally returning a greater vault }
function GetRandomVault(MinimumVaultLevel: Integer = 0;
  GreaterVault: Boolean = False): TVault;

{ Return a random vault suitable for entry into a dungeon branch }
function GetEntryVault: TVault;

implementation

{ TVault }

{ Default Constructor }
constructor TVault.Create;
begin
  { Logging }
  hLog.Add('{now} {lNum} TVault.Create()');

  try
    { Create a completely empty vault }
    FVaultMap := TStringList.Create
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Preferred Constructor }
constructor TVault.Create(VaultID: Integer; VaultName: String;
  VaultLevel: Integer; VaultEcology: String; VaultX, VaultY: Integer;
  VaultMap: String; VaultScore: Integer);
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TVault.Create(%d, ''%s'')',
    [VaultID, VaultName]));

  try
    { Load the private member data }
    FVaultID := VaultID;
    FVaultName := VaultName;
    FVaultLevel := VaultLevel;
    FVaultEcology := VaultEcology;
    FVaultX := VaultX;
    FVaultY := VaultY;
    FVaultMap := TStringList.Create;
    FVaultMap.Text := VaultMap;
    FVaultScore := VaultScore;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Destructor }
destructor TVault.Destroy;
begin
  { Logging }
  hLog.Add('{now} {lNum} TVault.Destroy()');

  try
    FVaultMap.Free;
    FVaultMap := nil;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return the Vault Matrix as a String }
function TVault.GetVaultMap: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TVault.GetVaultMap()');

  { Default result }
  Result := '';

  try
    Result := FVaultMap.Text;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return a random Vault, optionally returning a Greater Vault of a certain
  level }
function GetRandomVault(MinimumVaultLevel: Integer;
  GreaterVault: Boolean): TVault;
var
  Counter: Integer;
  LocalVault: TVault;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} UnitVault.GetRandomVault(%d, %d)',
    [MinimumVaultLevel, Ord(GreaterVault)]));

  { Default result }
  Result := nil;

  try
    { Vault Definitions are stored in the GVaultList }
    ObjectList := GVaultList;

    { Check to see if we want a Greater Vault }
    if GreaterVault then
    begin
      { Try and find a Vault that meets our criteria }
      Counter := 0;
      repeat
        LocalVault := ObjectList.Items[Random(ObjectList.Count)] as TVault;
        Inc(Counter);
      until ((MinimumVaultLevel >= LocalVault.VaultLevel) and
        (LocalVault.VaultScore > SMALLEST_GREATER_VAULT)) or
        (Counter >= VAULT_PARANOIA);

      { If we have found a Suitable Vault then return it }
      if (Counter < VAULT_PARANOIA) then
      begin
        Result := LocalVault;
      end;
    end
    else
    begin
      { Find a standard Lesser Vault }
      Counter := 0;
      repeat
        LocalVault := ObjectList.Items[Random(ObjectList.Count)] as TVault;
        Inc(Counter);
      until (Counter >= VAULT_PARANOIA) or
        (LocalVault.VaultScore <= SMALLEST_GREATER_VAULT);

      { If we have found a Suitable Vault then return it }
      if (Counter < VAULT_PARANOIA) then
      begin
        Result := LocalVault;
      end;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return a vault suitable for use as an entry into a lowe level dungeon branch.
  These vaults will be denuded of items and monsters and are used, pace Crawl
  Stone Soup as a gentle introduction into the game }
function GetEntryVault: TVault;
var
  Counter: Integer;
  LocalVault: TVault;
  ObjectList: TObjectList;
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitVault.GetEntryVault()');

  { Default result }
  Result := nil;

  try
    { Vault Definitions are stored in the GVaultList }
    ObjectList := GVaultList;

    Counter := 0;
    repeat
      LocalVault := ObjectList.Items[Random(ObjectList.Count)] as TVault;
      Inc(Counter);
    until (LocalVault.VaultScore <= LARGEST_ENTRY_VAULT) or
      (Counter >= VAULT_PARANOIA);

    { If we have found a Suitable Vault then return it }
    if (Counter < VAULT_PARANOIA) then
    begin
      Result := LocalVault;
    end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
