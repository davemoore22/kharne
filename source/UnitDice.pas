{ UnitDice

  Copyright (c) 2007-2009 Dave Moore 

  Dice Class

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

unit UnitDice;

interface

uses SysUtils, HotLog;

{ TDice Class }
type TDice = Class(TObject)

private
	{ Private Members }
  FValue: String;

public
	{ Constructors }
  constructor Create(Value: String);
  
  { Destructors }
  destructor Destroy; override;
  
  { Roll the dice }
  function GetValue: Integer;
  
	{ Accessors }
  property Value: String read FValue write FValue;
end;

implementation

{ Constructor }
constructor TDice.Create(Value: String);
const
  AllowedChars = ['0'..'9', '+', '-', 'd'];
var
  Buffer: String;
  Loop: Integer;
begin
  { Logging }
	hLog.Add(Format('{now} {lNum} TDice.Create(%s)', [Value]));
	
  try 
  	{ Default constructor }
		inherited Create;
		
		{ Assign the dice value to a temporary buffer }
		Buffer := Value;
		
		{ Set a default value }
		if Length(Trim(Value)) = 0 then
			Buffer := '1d1'
		else
		begin
			{ Check for allowed characters }
			for Loop :=  1 to Length(Buffer) do
			begin
				{ If we have an invalid character, set a default value }
				if not(Buffer[Loop] in AllowedChars) then
				begin
					Buffer := '1d1';
					Break;
				end;
			end;
		end;
		
		{ If we have reached this point, then set the value }
		FValue := LowerCase(Buffer);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Destructor }
destructor TDice.Destroy;
begin
  { Logging }
	hLog.Add('{now} {lNum} TDice.Destroy()');
	
  try 
  	{ Default destructor }
  	inherited Destroy;
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Roll the dice }
function TDice.GetValue: Integer;
var
  Buffer: String;
  DFound: Boolean;
  DieType: Integer;
  Loop: Integer;
  Modifier: Integer;
  NFound: Boolean;
  NumberOfDice: Integer;
  Temp: String;
  Total: Integer;
begin
  { Logging }
	hLog.Add('{now} {lNum} TDice.GetValue()');
	
  try 
  	{ Blank the temporary variables }
		Temp := '';
		Buffer := FValue;
		NFound := False;

		{ Parse out number of dice }
		while (NFound = False) do
		begin
			if (Buffer[1] <> 'd') then
			begin
				Temp := Temp + Buffer[1];
				Delete(Buffer, 1, 1);
			end
			else
				NFound := True;
		end;
		NumberOfDice := StrToInt(Temp);

		{ Parse out the die type }
		Temp := '';
		DFound := False;
		Delete(Buffer, 1, 1);
		while (DFound = False) do
		begin
			if Length(Buffer) <> 0 then
			begin
				if (Buffer[1] <> '-') and (Buffer[1] <> '+') then
				begin
					Temp := Temp + Buffer[1];
					Delete(Buffer, 1, 1);
				end
				else
					DFound := True;
			end
			else
				DFound := True;
		end;
		DieType := StrToInt(Temp);

		{ Deal with the modifier }
		if Length(Buffer) > 0 then
			Modifier := StrToInt(Buffer)
		else
			Modifier := 0;

		{ Calculate Die Roll }
		Total := 0;
		for Loop := 1 to NumberOfDice do			
			Inc(Total, Random(DieType) + 1);	;
		Inc(Total, Modifier);

		{ Return the result }
		Result := Total;
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;   
end;

{ At the beginning, randomise entirely }
initialization
  Randomize;

end.
