{ UnitTimer

  Copyright (c) 2007-2009 Dave Moore 

  Timer Handling

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

unit UnitTimer;

interface

uses SysUtils, HotLog, UnitInterfaces;

{ Define the types of timers as an enum for simplicity }
type TGameTimerType = (timSpeed, timConfusion, timBlindness, timSeeInvisible,
  timParalysis, timFreeAction, timCombatMastery, timReflexes, timMight);

{ Procedure Pointer for Event Hook }      
type TGameTimerEvent = procedure(const GameEvent: TObject);

{ Class Definition - it inherits the ICommonObject interface to gain access
  to the StringValue method to allow easy persistance }
type TGameTimer = class(TInterfacedObject, ICommonObject)
private
  FTimerType: TGameTimerType;        // Timer Type, from the enum previously defined
  FTimerDuration: Integer;           // Starting Duration, in turns
  FTimerDurationLeft: Integer;       // Duration Left, in turns
  FTimerTickEvent: TGameTimerEvent;  // Optional Event to call on each decrement
  FTimerStartEvent: TGameTimerEvent; // Optional Event to call on starting the timer
  FTimerEndEvent: TGameTimerEvent;   // Optional Event to call on ending the timer (i.e. turns left = 0)
 
  { Private functions to support class properties defined below }
  function GetStatus: Boolean;

public
  { Standard Constructor }
  constructor Create(TimerType: TGameTimerType;
                     TimerDuration: Integer;
                     TimerTickEvent: TGameTimerEvent = nil;
                     TimerStartEvent: TGameTimerEvent = nil;
                     TimerEndEvent: TGameTimerEvent = nil);
                   
  { Decrement the Timer by one turn. Will return true if the timer has not expired }
  function Tick: Boolean;
 
  { Interface Method for Persistance }
  function GetStringValue: String;
 
  { Properties }
  property TimerType: TGameTimerType read FTimerType;      // Return the enum
  property TimerDuration: Integer read FTimerDurationLeft; // Number of Turns left
  property Active: Boolean read GetStatus;                 // True if Number of Turns left is > 0
end;

implementation

{ Standard Constructor - this is deliberately the only way to set up the duration etc }
constructor TGameTimer.Create(TimerType: TGameTimerType; TimerDuration: Integer;
  TimerTickEvent: TGameTimerEvent; TimerStartEvent: TGameTimerEvent;
  TimerEndEvent: TGameTimerEvent);
begin
  { Logging }
  hLog.Add(Format('{now} {lNum} TGameTimer.Create(%d, %d)', [Ord(TimerType),
    TimerDuration]));

  try
    { Load the private member data }
    FTimerType := TimerType;
    FTimerDuration := TimerDuration;
    FTimerDurationLeft := TimerDuration;
    FTimerTickEvent := TimerTickEvent;
    FTimerStartEvent := TimerStartEvent;
    FTimerEndEvent := TimerEndEvent;

    { If we have a start event defined then execute it now }
    if (Assigned(FTimerStartEvent)) then
      FTimerStartEvent(Self);

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Return true if the timer hasn't expired }
function TGameTimer.GetStatus: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGameTimer.GetStatus()');

  { Default result }
  Result := False;

  try
    Result := FTimerDurationLeft > 0;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Decrease the duration of the timer by a turn }
function TGameTimer.Tick: Boolean;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGameTimer.Tick()');

  { Default result }
  Result := False;

  try
    { Trigger events if they are defined }
    if (FTimerDurationLeft > 0) then
    begin
      { Interval Event }
      if (Assigned(FTimerTickEvent)) then
        FTimerTickEvent(Self);
    end
    else if (FTimerDurationLeft = 0) then
    begin
      { End Event }
      if (Assigned(FTimerEndEvent)) then
        FTimerEndEvent(Self);
    end;

    { Decremen the Counter }
    Dec(FTimerDurationLeft);

    { Return true if the Timer is still active }
    Result := GetStatus;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Get the Timer as a String }
function TGameTimer.GetStringValue: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TGameTimer.GetStringValue()');
end;

end.
