unit KeyboardHandler;

(*********************************************
TKeyboardHandler->TComponent

Provides on demand state of any key on the
keyboard.

Properties:

KeyDown-
  A boolean array property.  Return true if the
  specified key is pressed.
VirtualKeyDown-
  Same as KeyDown, but uses virtual keycodes.
  KeyDown doesn't handle ALL keys.
*********************************************)
interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TKeyboardHandler = class( TComponent )
  private
  protected
     function GetKeyDown( c: char ): boolean;
     function GetVirtKey( n: integer ): boolean;
  public
     property KeyDown[c: char]: boolean read GetKeyDown; default;
     property VirtualKeyDown[n: integer]: boolean read GetVirtKey;
  published
  end;

procedure Register;

implementation

function TKeyboardHandler.GetKeyDown( c: char ): boolean;
var
  vk: integer;
  nRC: SHORT;
begin
  vk := 0;
  case c of
     'a'..'z':
        vk := Ord( c ) - Ord( 'a' ) + $41;
     'A'..'Z':
        vk := Ord( c ) - Ord( 'A' ) + $41;
     ' ':
        vk := $20;
  end;
try
  nRC := GetAsyncKeyState( vk );
  Result := nRC < 0;
except
  Result := FALSE;
end;
end;

function TKeyboardHandler.GetVirtKey( n: integer ): boolean;
var
  nRC: SHORT;
begin
try
  nRC := GetAsyncKeyState( n );
  Result := nRC < 0;
except
  Result := FALSE;
end;
end;

procedure Register;
begin
  RegisterComponents( 'Silicon Commander', [TKeyboardHandler] );
end;

end.
