unit UQPBar;

  // Demonstrates QProgBar.pas / TQProgressBar component use.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, QProgBar, ComCtrls, ExtCtrls, Grids, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    QProgressBar1: TQProgressBar;
    QProgressBar2: TQProgressBar;
    TrackBar1: TTrackBar;
    QProgressBar3: TQProgressBar;
    QProgressBar4: TQProgressBar;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    QProgressBar5: TQProgressBar;
    TrackBar2: TTrackBar;
    Panel1: TPanel;
    TrackBar3: TTrackBar;
    QProgressBar6: TQProgressBar;
    QProgressBar7: TQProgressBar;
    QProgressBar8: TQProgressBar;
    QProgressBar9: TQProgressBar;
    QProgressBar10: TQProgressBar;
    Label5: TLabel;
    Bevel2: TBevel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Label10: TLabel;
    Label11: TLabel;
    Bevel5: TBevel;
    QProgressBar11: TQProgressBar;
    Label12: TLabel;
    Label13: TLabel;
    TrackBar4: TTrackBar;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label14: TLabel;
    CheckBox4: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label15: TLabel;
    SpinEdit2: TSpinEdit;
    Label16: TLabel;
    CheckBox5: TCheckBox;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Panel2: TPanel;
    ColorDialog1: TColorDialog;
    Panel3: TPanel;
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  QProgressBar2.position := TrackBar1.Position;
  QProgressBar3.position := TrackBar1.Position;
  QProgressBar4.position := TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  application.ProcessMessages;
  QProgressBar5 .position := TrackBar2.Position;
  QProgressBar10.position := TrackBar2.Position;
  QProgressBar11.position := TrackBar2.Position;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  With QProgressBar6 Do
  Begin
  If orientation = boHorizontal
     Then orientation := boVertical
     Else orientation := boHorizontal;
  End;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  QProgressBar6.position := TrackBar3.Position;
  application.ProcessMessages;
  QProgressBar7.position := TrackBar3.Position;
  QProgressBar8.position := TrackBar3.Position;
  QProgressBar9.position := TrackBar3.Position;
end;



procedure TForm1.TrackBar4Change(Sender: TObject);
begin
  QProgressBar1.position := TrackBar4.Position;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  QProgressBar1.roundCorner := CheckBox1.Checked;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  QProgressBar1.barKind := TQBarKind(RadioGroup1.ItemIndex);
  RadioGroup2.Enabled   := RadioGroup1.ItemIndex = 1;
  CheckBox3.Enabled     := RadioGroup1.ItemIndex = 1;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  QProgressBar1.barLook := TQBarLook(RadioGroup2.ItemIndex);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  QProgressBar1.showInactivePos := CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  QProgressBar1.invertInactPos := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  QProgressBar1.shaped := CheckBox4.Checked;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  TRY
    QProgressBar1.blockSize := SpinEdit1.Value;
  EXCEPT;  // if you change spinEdit's values manually,
  END;     // an EConvertError will occur...
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  TRY
    QProgressBar1.spaceSize := SpinEdit2.Value;
  EXCEPT;  // if you change spinEdit's values manually,
  END;     // an EConvertError will occur...
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  QProgressBar1.showFullBlock := CheckBox5.Checked;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel2.Color;
  If ColorDialog1.Execute Then
  Begin
    QProgressBar1.startColor := ColorDialog1.Color;
    panel2.Color             := ColorDialog1.Color;
  End;
end;

procedure TForm1.Panel3Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel3.Color;
  If ColorDialog1.Execute Then
  Begin
    QProgressBar1.finalColor := ColorDialog1.Color;
    panel3.Color             := ColorDialog1.Color;
  End;
end;

end.


