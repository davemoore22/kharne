{ This unit contains all functions and code that I've used that originate from
  the internet. As such, I don't think they can be placed under the MPL and thus
  this file isn't MPLed. However, I give express permission to link my own code
  as released under the MPL to this file }

unit UnitOtherFuncs;

interface

uses Graphics, Windows, SysUtils, Messages, HotLog;

{ Empty the Key Buffer of all events }
procedure EmptyKeyQueue;

{ Empty the Mouse Buffer of all events }
procedure EmptyMouseQueue;

{ Darken a colour by a certain percentage }
function Darker(Color:TColor; Percent:Byte):TColor;

{ Lighten a colour by a certain percentage }
function Lighter(Color:TColor; Percent:Byte):TColor;

{ Slightly darken a colour }
function SlightlyDark(Color:TColor):TColor;

{ Darken a colour }
function Dark(Color:TColor):TColor;

{ Significantly darken a colour }
function VeryDark(Color:TColor):TColor;

{ Slightly lighten a colour }
function SlightlyLight(Color:TColor):TColor;

{ Lighten a colour }
function Light(Color:TColor):TColor;

{ Significantly lighten a colour }
function VeryLight(Color:TColor):TColor;

implementation

uses UnitConst;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Darken a colour by a certain percentage }
function Darker(Color:TColor; Percent:Byte):TColor;
var
  r,g,b:Byte;
  Colour: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.Darker()');

  { Default result }
  Result := Color;
  
  try
  	{ Get the RGB Values }
		Colour:= ColorToRGB(Color);
		r:= GetRValue(Colour);
		g:= GetGValue(Colour);
		b:= GetBValue(Colour);
		
		{ Adjust them for the % closer to black given }
		r:= r - muldiv(r, Percent, 100);
		g:= g - muldiv(g, Percent, 100);
		b:= b - muldiv(b, Percent, 100);
		
		{ Return the result }
		Result := RGB(r,g,b);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Lighten a colour by a certain percentage }
function Lighter(Color:TColor; Percent:Byte):TColor;
var
  r,g,b:Byte;
  Colour: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.Lighter()');

  { Default result }
  Result := Color;
  
  try 
    { Get the RGB Values }
		Colour:= ColorToRGB(Color);
		r:= GetRValue(Colour);
		g:= GetGValue(Colour);
		b:= GetBValue(Colour);
		
		{ Adjust them for the % closer to white given }
		r := r + muldiv(255 - r, Percent, 100);
		g := g + muldiv(255 - g, Percent, 100);
		b := b + muldiv(255 - b, Percent, 100);
		
		{ Return the result }
		Result := RGB(r,g,b);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
 
{ Slightly darken a colour }
function SlightlyDark(Color:TColor):TColor; 
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.SlightlyDark()');

  { Default result }
  Result := Color;
  try
  	{ Slightly darken a colour }
  	Result := Darker(Color, SMALL_COLOUR_CHANGE);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Darken a colour }
function Dark(Color:TColor):TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.Dark()');

  { Default result }
  Result := Color;
  try
  	{ Darken a colour }
  	Result := Darker(Color, LARGE_COLOUR_CHANGE);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Significantly darken a colour }
function VeryDark(Color:TColor):TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.VeryDark()');

  { Default result }
  Result := Color;
  
  try
  	{ Significantly darken a colour }
  	Result := Darker(Color, LARGE_COLOUR_CHANGE);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Slightly lighten a colour }
function SlightlyLight(Color:TColor):TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.SlightlyLight()');

  { Default result }
  Result := Color;
  
  try
  	{ Slightly lighten a colour }
  	Result := Lighter(Color, SMALL_COLOUR_CHANGE);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Lighten a colour }
function Light(Color:TColor):TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.Light()');

  { Default result }
  Result := Color;
  
  try
  	{ Lighten a colour }
  	Result := Lighter(Color, 50);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Colour Adjustment Function originally by Den Bedard
  http://www.delphi3000.com/articles/article_2310.asp }
  
{ Significantly lighten a colour }
function VeryLight(Color:TColor):TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.VeryLight()');

  { Default result }
  Result := Color;
  
  try
  	{ Lighten the colour }
  	Result := Lighter(Color, LARGE_COLOUR_CHANGE);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Function originally by Peter Below
  http://www.swissdelphicenter.ch/en/showcode.php?id=1066 }

{ Empty the Key Buffer of all events }
procedure EmptyKeyQueue;
var
  Msg: TMsg;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.EmptyKeyQueue()');
  
  try
  	{ Remove all pending events from the system key queue }
  	while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST, 
  		PM_REMOVE or PM_NOYIELD) do;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Function originally by Peter Below
  http://www.swissdelphicenter.ch/en/showcode.php?id=1066 }

{ Empty the Mouse Buffer of all events }
procedure EmptyMouseQueue;
var
  Msg: TMsg;
begin
  { Logging }
  // hLog.Add('{now} {lNum} UnitFunctions.EmptyMouseQueue()');

  try
  	{ Remove all pending events from the system mouse queue }
  	while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST,
    	PM_REMOVE or PM_NOYIELD) do;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
