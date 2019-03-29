{ UnitSplash

  Copyright (c) 2007-2009 Dave Moore 

  Splash Screen

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

unit UnitSplash;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, HotLog;

{ Splash Screen Form }
type
  TFormSplash = class(TForm)
    ImageSplash: TImage;
    Timer: TTimer;
    LabelProgress: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ImageSplashClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSplash: TFormSplash;

implementation

uses UnitDisplay;

{$R *.dfm}

{ Form Create }
procedure TFormSplash.FormCreate(Sender: TObject);
begin
  { Set up Logging }
  hLog.hlWriter.hlFileDef.append := False;

  { Start Logging if desired }
  if ParamCount > 0 then
    if SameText(ParamStr(1), 'log') then
      hLog.StartLogging;

  { log System Information}
  hLog.add('{OSVI}');
  hLog.add('{CPUI}');
  hLog.add('{mem}');

  { log Program Information }

  hLog.add('Application Name: {app_name}');
  hLog.add('Application Path: {app_path}');
  hLog.add('Application Version: {app_ver}');
  hLog.add('Application Parameters: {app_prm-}');
  hLog.add('');
  hLog.add('{/}{LNumOff}{*80*}');

  { Start the Timer to move onto the Main Window }
  Timer.Enabled := True;

  { Clear the Status Bar }
  LabelProgress.Caption := '';

  { Logging }
  hLog.Add('{now} {lNum} TFormSplash.FormCreate()');
end;

{ Display the main form and hide the splash screen }
procedure TFormSplash.TimerTimer(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormSplash.TimerTimer()');

  try
    { Disable the Timer since we've triggered it }
    Timer.Enabled := False;

    { Create the main form }
    FormDisplay := TFormDisplay.Create(Application);

    { Hide the Splash Screen }
    FormSplash.Hide;
    try
      { Display the main form }
      FormDisplay.ShowModal;
    finally
      { Exit and shut down }
      FormDisplay.Timer.Enabled := False;
      FormDisplay.TimerCycleColours.Enabled := False;
      FormDisplay.TimerAnimate.Enabled := False;
      FormDisplay.TimerColorReset.Enabled := False;
      FormDisplay.TimerAnimateIntro.Enabled := False;
      hLog.Add('{now} {lNum} *** Exiting ***');
      FormDisplay.Free;
      hLog.Destroy;
      Application.Terminate;
    end;

  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Clicking on the Splash Screen accelerates the timer }
procedure TFormSplash.ImageSplashClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormSplash.ImageSplashClick()');

  try
    { Execute the Timer }
    Timer.OnTimer(Sender);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
