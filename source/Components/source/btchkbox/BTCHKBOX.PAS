{*******************************************************************************
    TBitCheckBox
    Copyright © 1996 Bill Menees
    bmenees@usit.net
    http://www.public.usit.net/bmenees

    This is a VCL component that behaves almost exactly like the TCheckBox
    component except it allows you to use images for each state.

    To install it you need three files:
               BtChkBox.pas, BtChkBox.res, and BtImages.res.

    The only difference in behavior between TBitCheckBox and TCheckBox is that
    OnClick DOESN'T fire when State is changed programmatically.  (Of course, it
    does fire if you change the state by clicking on the component.)  I always
    hated the fact that TCheckBox fires the OnClick event when State changes, so
    I took this opportunity to "fix" this in my component.

    Some other minor differences: There are now properties for setting the
    glyphs for each state.  Each glyph can contain an enabled image and a
    disabled image.  There's also a new public method: Toggle.

    You're free to use and redistribute this component as long as this header
    stays attached.  This component is provided AS IS with no warrenties or
    guarantees implied.

*******************************************************************************}

unit BtChkBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TBitCheckBox = class(TCustomControl)
  private
    fCheckedBitmap: TBitmap;
    fUncheckedBitmap: TBitmap;
    fGrayedBitmap: TBitmap;
    fAlignment: TLeftRight;
    fState: TCheckBoxState;
    fAllowGrayed: Boolean;
    fCheckedNumGlyphs: Integer;
    fGrayedNumGlyphs: Integer;
    fUncheckedNumGlyphs: Integer;

    function GetChecked: Boolean;

    procedure SetCheckedNumGlyphs(Value: Integer);
    procedure SetGrayedNumGlyphs(Value: Integer);
    procedure SetUncheckedNumGlyphs(Value: Integer);
    procedure SetCheckedBitmap(Value: TBitmap);
    procedure SetGrayedBitmap(Value: TBitmap);
    procedure SetUncheckedBitmap(Value: TBitmap);
    procedure SetState(Value: TCheckBoxState);
    procedure SetAlignment(Value: TLeftRight);
    procedure SetChecked(Value: Boolean);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Toggle; virtual;

  published
    property Alignment: TLeftRight read fAlignment write SetAlignment default taRightJustify;
    property AllowGrayed: Boolean read fAllowGrayed write fAllowGrayed default False;
    property Caption;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckedGlyph: TBitmap read fCheckedBitmap write SetCheckedBitmap;
    property CheckedNumGlyphs: Integer read fCheckedNumGlyphs write SetCheckedNumGlyphs default 1;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GrayedGlyph: TBitmap read fGrayedBitmap write SetGrayedBitmap;
    property GrayedNumGlyphs: Integer read fGrayedNumGlyphs write SetGrayedNumGlyphs default 1;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State: TCheckBoxState read fState write SetState default cbUnchecked;
    property TabOrder;
    property TabStop;
    property UncheckedGlyph: TBitmap read fUncheckedBitmap write SetUncheckedBitmap;
    property UncheckedNumGlyphs: Integer read fUncheckedNumGlyphs write SetUncheckedNumGlyphs default 1;
    property Visible;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
end;

procedure Register;

implementation

{$R BtChkBox.res}
{$R BtImages.res}

{******************************************************************************}
{** Non-Member Functions ******************************************************}
{******************************************************************************}

procedure Register;
begin
  RegisterComponents('Additional', [TBitCheckBox]);
end;

{******************************************************************************}
{** TBitCheckBox Public Methods ***********************************************}
{******************************************************************************}

constructor TBitCheckBox.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     fCheckedBitmap:=TBitmap.Create;
     fUncheckedBitmap:=TBitmap.Create;
     fGrayedBitmap:=TBitmap.Create;

     ControlStyle := [csSetCaption, csClickEvents];
     Alignment:=taRightJustify;
     Checked:=False;
     Width:=97;
     Height:=17;
     TabStop:=True;
     ParentCtl3D:=True;
     AllowGrayed:=False;

     CheckedGlyph:=nil;
     GrayedGlyph:=nil;
     UncheckedGlyph:=nil;
     CheckedNumGlyphs:=1;
     GrayedNumGlyphs:=1;
     UncheckedNumGlyphs:=1;
end;

destructor TBitCheckBox.Destroy;
begin
     fGrayedBitmap.Free;
     fUncheckedBitmap.Free;
     fCheckedBitmap.Free;
     inherited Destroy;
end;

procedure TBitCheckBox.Toggle;
begin
     case State of
          cbUnchecked:
             if AllowGrayed then
                State := cbGrayed
             else
                 State := cbChecked;
          cbChecked: State := cbUnchecked;
          cbGrayed: State := cbChecked;
     end;
end;

{******************************************************************************}
{** TBitCheckBox Protected Methods ********************************************}
{******************************************************************************}

procedure TBitCheckBox.Paint;
const
     DrawOptions = DT_LEFT or DT_VCENTER or DT_SINGLELINE;
var
   Rc, BitRect, FocusRect: TRect;
   Bitmap: TBitmap;
   HOffset, VOffset, NumGlyphs, GlyphWidth: Integer;
   OldColor: TColor;
begin
     with Canvas do
     begin
          {First, blank our client area.}
          Rc := ClientRect;
          Brush.Color := Self.Color;
          Brush.Style := bsSolid;
          FillRect(Rc);

          {Now make the brush clear so our fonts get
          drawn correctly (like 3D disabled text).}
          Brush.Style := bsClear;

          {Determine which bitmap will be used.}
          case State of
               cbChecked:
               begin
                    Bitmap:=CheckedGlyph;
                    NumGlyphs:=CheckedNumGlyphs;
               end;
               cbUnChecked:
               begin
                    Bitmap:=UncheckedGlyph;
                    NumGlyphs:=UncheckedNumGlyphs;
               end;
               else
               begin
                    Bitmap:=GrayedGlyph;
                    NumGlyphs:=GrayedNumGlyphs;
               end;
          end;
          GlyphWidth:=Bitmap.Width div NumGlyphs;

          {Draw the text (after determining the FocusRect).}
          Rc:=ClientRect;
          if Alignment = taLeftJustify then
             HOffset:=1
          else
              HOffset:=GlyphWidth+4;
          Rc.Left:=HOffset;
          Rc.Right:=Rc.Left+TextWidth(Caption);
          {Subtract out the width of the underscoring character.}
          if Pos('&', Caption) > 0 then Rc.Right:=Rc.Right-TextWidth('&');
          VOffset:=(ClientHeight-TextHeight(Caption)) div 2 - 2;
          Rc.Top:=VOffset;
          Rc.Bottom:=Rc.Top+TextHeight(Caption)+3;
          IntersectRect(FocusRect, Rc, ClientRect);

          OldColor:=Font.Color;
          {DrawText is used because it handles
          the underscored accelerator key.}
          if Enabled then
          begin
               DrawText(Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions);
          end
          else
          begin
               if Ctl3D then
               begin
                    {This draws disabled text in 3D.}
                    OffsetRect(FocusRect, 1, 1);
                    Font.Color := clBtnHighlight;
                    DrawText(Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions);
                    OffsetRect(FocusRect, -1, -1);
               end;
               {This draws disabled text like SQL6.}
               Font.Color:=clGrayText;
               DrawText(Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions);
          end;
          Font.Color:=OldColor;

          {Now we need make the brush solid again for the BrushCopy to work.}
          Brush.Color := Self.Color;
          Brush.Style := bsSolid;

          {Draw the focused rectangle.}
          if Focused then DrawFocusRect(FocusRect);

          {Now, draw the bitmap.}
          if Alignment = taLeftJustify then
             HOffset:=ClientWidth-GlyphWidth
          else
              HOffset:=0;
          VOffset:=(ClientHeight - Bitmap.Height) div 2;
          {Figure out where to draw the image.}
          Rc.Top:=VOffset;
          Rc.Bottom:=Bitmap.Height+VOffset;
          Rc.Left:=HOffset;
          Rc.Right:=GlyphWidth+HOffset;
          {Choose the correct bitmap.}
          {If we're disabled choose the second bitmap.}
          if not Enabled and (NumGlyphs = 2) then
             BitRect.Left:=GlyphWidth
          else
              BitRect.Left:=0;
          BitRect.Right:=BitRect.Left+GlyphWidth;
          BitRect.Top:=0;
          BitRect.Bottom:=Bitmap.Height;

          BrushCopy(Rc, Bitmap, BitRect, Bitmap.TransparentColor);
     end;
end;

procedure TBitCheckBox.Click;
begin
     Toggle;
     inherited Click;
     if Showing and CanFocus then SetFocus;
end;

procedure TBitCheckBox.KeyPress(var Key: Char);
begin
     inherited KeyPress(Key);
     if Key = ' ' then Click;
end;

{******************************************************************************}
{** TBitCheckBox Private Methods **********************************************}
{******************************************************************************}

procedure TBitCheckBox.SetAlignment(Value: TLeftRight);
begin
     if Value <> fAlignment then
     begin
          fAlignment:=Value;
          Invalidate;
     end;
end;

procedure TBitCheckBox.CMEnabledChanged(var Msg: TMessage);
begin
     inherited;
     Invalidate;
end;

procedure TBitCheckBox.CMDialogChar(var Msg: TCMDialogChar);
begin
     with Msg do
          if IsAccel(CharCode, Caption) and Enabled and CanFocus then
          begin
               SetFocus;
               if Focused then Toggle;
               Result := 1;
          end
          else
              inherited;
end;

procedure TBitCheckBox.CMFontChanged(var Msg: TMessage);
begin
     inherited;
     Canvas.Font:=Font;
     Invalidate;
end;

procedure TBitCheckBox.CMTextChanged(var Msg: TMessage);
begin
     inherited;
     Invalidate;
end;

procedure TBitCheckBox.CMSysColorChange(var Msg: TMessage);
begin
     inherited;
     Invalidate;
end;

procedure TBitCheckBox.CMCtl3DChanged(var Msg: TMessage);
begin
     inherited;
     Invalidate;
     Realign;
end;

procedure TBitCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
     inherited;
     Invalidate;
end;

procedure TBitCheckBox.WMKillFocus(var Message: TWMKillFocus);
begin
     inherited;
     Invalidate;
end;

procedure TBitCheckBox.SetState(Value: TCheckBoxState);
begin
     if fState <> Value then
     begin
          fState := Value;
          Invalidate;
     end;
end;

function TBitCheckBox.GetChecked: Boolean;
begin
     Result:=(State = cbChecked);
end;

procedure TBitCheckBox.SetChecked(Value: Boolean);
begin
     if Value then
        State:=cbChecked
     else
         State:=cbUnchecked;
end;

procedure TBitCheckBox.SetCheckedBitmap(Value: TBitmap);
begin
     if Value = nil then
        fCheckedBitmap.LoadFromResourceName(HInstance, 'CHECKED')
     else
         fCheckedBitmap.Assign(Value);
     if (fCheckedBitmap.Width mod fCheckedBitmap.Height) = 0 then
        CheckedNumGlyphs:=fCheckedBitmap.Width div fCheckedBitmap.Height;
     Invalidate;
end;

procedure TBitCheckBox.SetGrayedBitmap(Value: TBitmap);
begin
     if Value = nil then
        fGrayedBitmap.LoadFromResourceName(HInstance, 'GRAYED')
     else
         fGrayedBitmap.Assign(Value);
     if (fGrayedBitmap.Width mod fGrayedBitmap.Height) = 0 then
        GrayedNumGlyphs:=fGrayedBitmap.Width div fGrayedBitmap.Height;
     Invalidate;
end;

procedure TBitCheckBox.SetUncheckedBitmap(Value: TBitmap);
begin
     if Value = nil then
        fUncheckedBitmap.LoadFromResourceName(HInstance, 'UNCHECKED')
     else
         fUncheckedBitmap.Assign(Value);
     if (fUncheckedBitmap.Width mod fUncheckedBitmap.Height) = 0 then
        UncheckedNumGlyphs:=fUncheckedBitmap.Width div fUncheckedBitmap.Height;
     Invalidate;
end;

procedure TBitCheckBox.SetCheckedNumGlyphs(Value: Integer);
begin
     if Value < 1 then fCheckedNumGlyphs:=1
     else if Value > 2 then fCheckedNumGlyphs:=2
     else fCheckedNumGlyphs:=Value;
     Invalidate;
end;

procedure TBitCheckBox.SetGrayedNumGlyphs(Value: Integer);
begin
     if Value < 1 then fGrayedNumGlyphs:=1
     else if Value > 2 then fGrayedNumGlyphs:=2
     else fGrayedNumGlyphs:=Value;
     Invalidate;
end;

procedure TBitCheckBox.SetUncheckedNumGlyphs(Value: Integer);
begin
     if Value < 1 then fUncheckedNumGlyphs:=1
     else if Value > 2 then fUncheckedNumGlyphs:=2
     else fUncheckedNumGlyphs:=Value;
     Invalidate;
end;

end.
