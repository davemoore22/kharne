{   Copyright © 1998 by Mik                                          }

unit Gradlabl;

interface

uses WinTypes, WinProcs, Messages, SysUtils, Classes, Controls,
     Forms, Graphics, Stdctrls;

type
  TextureType = (TyBitmap,TyGradient,TyOutline);
  TGradLabel = class(TGraphicControl)
    private
        colors : array[0..255] of tcolor;
        FTexture : TBitmap;
        FBeginColor : TColor;
        FEndColor : TColor;
        FTxType : TextureType;
        Fshadow : Boolean;
        FShadowColor : Tcolor;
        FBorderColor: Tcolor;
        FNumberOfColors : integer;
        Fdeep : Integer;

        procedure AutoInitialize;
        procedure AutoDestroy;
        procedure SetTexture(Value : TBitmap);
        procedure WMSize(var Message: TWMSize); message WM_SIZE;
        procedure SetStartColor(value : Tcolor);
        procedure SetEndColor(value : Tcolor);
        procedure Setshadowcolor(value:Tcolor);
        Procedure SetShadow(value:Boolean);
        Procedure SetTxType(value :TextureType);
        Procedure SetBorderColor(value:Tcolor);
        Procedure SetDeep(value: Integer);
    protected
        procedure Paint; override;
        procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure GradientFill(canvas:Tcanvas;width,height:integer);
        procedure calcolor;
    published
        property OnClick;
        property OnDblClick;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        Property Font;
        Property Caption;
        Property BorderColor: Tcolor read FBorderColor write SetBorderColor;
        Property Shadow: Boolean read Fshadow write SetShadow;
        Property ShadowColor : Tcolor read Fshadowcolor write Setshadowcolor;
        property Texture : TBitmap read FTexture write SetTexture;
        property StartColor : TColor read FBeginColor write SetStartColor;
        property EndColor : TColor read FEndColor write SetEndColor;
        property TxType : TextureType read FTxType write SetTxType;
        property ShadowDeep : Integer read Fdeep write SetDeep;
  end;

procedure Register;

implementation

Const Mik = 'Mik';

procedure Register;
begin
     RegisterComponents(Mik, [TGradLabel]);
end;

procedure TGradLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TGradLabel.SetStartColor(value : Tcolor);
Begin
  if FBeginColor <> value then
   begin
    FBeginColor := value;
    if FTxType = TyGradient then
     begin
       calcolor;
       invalidate;
     end;
   end;
End;

procedure TGradLabel.SetEndColor(value : Tcolor);
Begin
  if fEndcolor <> value then
   begin
    FEndcolor := value;
    if FTxType = TyGradient then
     begin
       calcolor;
       invalidate;
     end;
   end;
End;

procedure TGradLabel.SetBorderColor(value:Tcolor);
Begin
  if FBorderColor <> value then
   Begin
    FBorderColor := value;
    If not (FTxType = TyGradient) then
     Invalidate;
   end;
End;

procedure TGradLabel.Setshadowcolor(value:Tcolor);
begin
 if FshadowColor <> value then
  begin
    FshadowColor := value;
    If Fshadow then
      invalidate;
  end;
end;

procedure TGradLabel.SetDeep(value: Integer);
Begin
if (value in [0..9]) then
 if Fdeep <> value then
   begin
     Fdeep := value;
     if Fshadow then
       invalidate;
   end;
end;

procedure TGradLabel.SetShadow(value:Boolean);
begin
  If Fshadow <> value then
   begin
     FShadow := value;
     Invalidate;
   end;
end;

procedure TGradLabel.SetTxType(value :TextureType);
begin
 if FTxType <> value then
   Begin
    FTxType := value;
    Invalidate;
   End;
end;

procedure TGradLabel.calcolor;
var
  BeginRGB  : array[0..2] of Byte;
  RGBDifference  : array[0..2] of integer;
  R         : Byte;
  G         : Byte;
  B         : Byte;
  I: Byte;

begin
  BeginRGB[0] := GetRValue (ColorToRGB (FBeginColor));
  BeginRGB[1] := GetGValue (ColorToRGB (FBeginColor));
  BeginRGB[2] := GetBValue (ColorToRGB (FBeginColor));

  RGBDifference[0] := GetRValue (ColorToRGB (FEndColor)) - BeginRGB[0];
  RGBDifference[1] := GetGValue (ColorToRGB (FEndColor)) - BeginRGB[1];
  RGBDifference[2] := GetBValue (ColorToRGB (FEndColor)) - BeginRGB[2];
 for i := 0 to 255 do
 begin
    R := BeginRGB[0] + MulDiv (I, RGBDifference[0], FNumberOfColors - 1);
    G := BeginRGB[1] + MulDiv (I, RGBDifference[1], FNumberOfColors - 1);
    B := BeginRGB[2] + MulDiv (I, RGBDifference[2], FNumberOfColors - 1);
    colors[i] := RGB (R, G, B);
 end;
end;

procedure TGradLabel.GradientFill(canvas:Tcanvas;width,height:integer);
var
  Rc : TRect;
  I         : Integer;
  TmpBitmap   : TBitmap;

begin
TmpBitmap := TBitmap.Create;
TmpBitmap.Width := Width;
TmpBitmap.Height := Height;

Rc.Top := 0;
Rc.Bottom := Height;

with TmpBitmap do
  for I := 0 to FNumberOfColors do
    begin
     Rc.Left  := MulDiv (I   , Width, FNumberOfColors);
     Rc.Right := MulDiv (I + 1, Width, FNumberOfColors);
     Canvas.Brush.Color := colors[i];
     Canvas.FillRect (Rc);
    end;
   Canvas.Draw(0, 0, TmpBitmap);
   TmpBitmap.Free;
end;

procedure TGradLabel.AutoInitialize;
begin
     Width := 120;
     Height := 60;
     FTexture := TBitmap.Create;
     FborderColor := Clnavy;
     FBeginColor := clMaroon;
     FEndColor := clGreen;
     FTxType := TyGradient;
     Fshadow := false;
     Font.Name := 'Arial';
     Font.size := 26;
     Font.Style := [FsBold];
     FShadowColor := clbtnshadow;
     FNumberOfColors := 255;
     Fdeep := 1;
     Calcolor;
end;


procedure TGradLabel.AutoDestroy;
begin
     FTexture.Free;
end; 

procedure TGradLabel.SetTexture(Value : TBitmap);
begin
     FTexture.Assign(Value);
     Invalidate;
end;

constructor TGradLabel.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     AutoInitialize;
end;

destructor TGradLabel.Destroy;
begin
     AutoDestroy;
     inherited Destroy;
end;


procedure TGradLabel.WMSize(var Message: TWMSize);
var
     W, H: Integer;
begin
     inherited;
     W := Width;
     H := Height;
     if (W <> Width) or (H <> Height) then
        inherited SetBounds(Left, Top, W, H);

     Message.Result := 0;
end;


function CorrectColor(C : Real) : Integer;
begin
  Result := Round(C);
  if Result > 255 then Result := 255;
  if Result < 0 then Result := 0;
end;

function ERGB(R,G,B : Real) : TColor;
begin
	Result := RGB(CorrectColor(R), CorrectColor(G), CorrectColor(B));
end;

procedure DrawOutlinedText(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);
var
	Backup: TColor;
begin
  with Dest do
  begin
  	Backup := Font.Color;
    Font.Color := SC;
    Brush.Style := bsClear;
  	TextOut(X + 1, Y + 1, Text);
  	TextOut(X - 1, Y + 1, Text);
  	TextOut(X - 1, Y - 1, Text);
  	TextOut(X + 1, Y - 1, Text);
    Font.Color := Backup;
  	TextOut(X, Y, Text);
  end;
end;

procedure DrawTextOutline(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);
begin
  with Dest do
  begin
    Font.Color := SC;
    Brush.Style := bsClear;
  	TextOut(X + 1, Y + 1, Text);
  	TextOut(X - 1, Y + 1, Text);
  	TextOut(X - 1, Y - 1, Text);
  	TextOut(X + 1, Y - 1, Text);
  end;
end;

procedure DrawShadowTextExt(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor;
	SX, SY: Integer);
var
	Backup: TColor;
begin
  with Dest do
  begin
  	Backup := Font.Color;
    Brush.Style := bsClear;
    Font.Color := SC;
  	TextOut(X + SX, Y + SY, Text);
    Font.Color := Backup;
  	TextOut(X, Y, Text);
  end;
end;

procedure PaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);
var
	fMask: TBitmap;
  fStore: TBitmap;
begin
	fMask := TBitmap.Create;
  with fMask, fMask.Canvas do
  begin
  	Monochrome := True;
    Font.Assign(Dest.Font);
    Font.Color := clBlack;
  	Width := TextWidth(Text);
  	Height := TextHeight(Text);
  	Textout(0, 0, Text);
  end;
	fStore := TBitmap.Create;
  with fStore, fStore.Canvas do
  begin
  	Width := fMask.Width;
  	Height := fMask.Height;
  	CopyRect(Rect(0,0,Width,Height), Dest, Bounds(X,Y,Width,Height));

    CopyMode := cmSrcInvert;
    Draw(0, 0, Bmp);

    CopyMode := cmSrcAnd;
    CopyRect(Rect(0,0,Width,Height), fMask.Canvas, Rect(0,0,Width,Height));

    CopyMode := cmSrcInvert;
    Draw(0, 0, Bmp);
  end;
  Dest.Draw(X, Y, fStore);
  fStore.Free;
	fMask.Free;
end;

procedure StretchPaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);
var
	fMask: TBitmap;
  fStore: TBitmap;
  R: TRect;
begin
	fMask := TBitmap.Create;
  with fMask, fMask.Canvas do
  begin
  	Monochrome := True;
    Font.Assign(Dest.Font);
    Font.Color := clBlack;
  	Width := TextWidth(Text);
  	Height := TextHeight(Text);
  	Textout(0, 0, Text);
  end;
	fStore := TBitmap.Create;
  with fStore do
  begin
  	Width := fMask.Width;
  	Height := fMask.Height;
  	R := Rect(0,0,Width,Height);
  	with Canvas do 	begin
    	CopyRect(R, Dest, Bounds(X,Y,Width,Height));

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);

    	CopyMode := cmSrcAnd;
    	Draw(0, 0, fMask);

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);
    end;
  end;
  Dest.Draw(x, y, fStore);
  fStore.Free;
	fMask.Free;
end;


procedure TGradLabel.Paint;
var t : Tbitmap;
begin
Canvas.font := self.font;
If Fshadow then
  DrawShadowTextExt(canvas,1,1,caption, Fshadowcolor, fdeep, fdeep);
Case FTxType of
 TyOutline : DrawOutlinedText(Canvas, 1,1,caption,FborderColor);
 TyBitmap  : begin
              DrawOutlinedText(Canvas, 1,1,caption,FborderColor);
              If assigned(FTexture) then
                StretchPaintOnText(Canvas, 1,1,caption,FTexture);
             end;
 TyGradient:Begin
             t:= Tbitmap.create;
             t.width := width;
             t.height := height;
             Gradientfill(t.canvas,width,height);
             StretchPaintOnText(Canvas, 1,1,caption,T);
             t.free;
            end;
End; {case} 
end;

end.
