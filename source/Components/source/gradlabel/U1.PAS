unit U1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, Gradlabl, Buttons;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpeedButton1: TSpeedButton;
    ColorDialog1: TColorDialog;
    GradLabel1: TGradLabel;
    SpeedButton2: TSpeedButton;
    GradLabel2: TGradLabel;
    GradLabel3: TGradLabel;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure CheckBox1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
SpinEdit1.enabled := CheckBox1.checked;
GradLabel1.shadow := CheckBox1.checked;
GradLabel2.shadow := CheckBox1.checked;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
GradLabel1.shadowdeep := SpinEdit1.value;
GradLabel2.shadowdeep := SpinEdit1.value;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
ColorDialog1.execute;
GradLabel1.shadowcolor := ColorDialog1.color;
GradLabel2.shadowcolor := ColorDialog1.color;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
ColorDialog1.execute;
GradLabel2.Bordercolor := ColorDialog1.color;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
GradLabel1.shadowdeep := SpinEdit1.value;
GradLabel2.shadowdeep := SpinEdit1.value;
end;

procedure TForm1.Button1Click(Sender: TObject);
var t: Tbitmap;
begin
t := tbitmap.create;
opendialog1.execute;
if opendialog1.filename <> '' then
 t.loadfromfile(opendialog1.filename);
 GradLabel3.txtype := tyoutline;
 GradLabel3.texture.assign(t);
 GradLabel3.txtype := tybitmap;
 t.free;
end;

end.
