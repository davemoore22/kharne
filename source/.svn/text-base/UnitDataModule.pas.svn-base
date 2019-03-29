{ UnitDataModule

  Copyright (c) 2007-2009 Dave Moore 

  DataModule

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

unit UnitDataModule;

interface

uses
  SysUtils, Classes, ImgList, Controls, DB, Forms, Dialogs, SQLiteTable3;

{ The datamodule holds the SQLLite connection objects }
type
  TDataModuleMain = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SLDatabase: TSQLiteDatabase;
    SLTable: TSQLIteTable;
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{$R *.dfm}

{ The datamodule is created early on, almost at the start of the application } 
procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
var
  DBFileName: String;
begin
  { find the SQLLite database }
  DBFileName := ExtractFilePath(Application.ExeName) + '\kharne.db3';

  { Connect to it }
  SLDatabase := TSQLiteDatabase.Create(DBFileName);

  { Start a transaction }
  SLDatabase.BeginTransaction;
end;

{ And at the end, destroy the datamodule }
procedure TDataModuleMain.DataModuleDestroy(Sender: TObject);
begin
  { Commit any active transaction }
  SLDatabase.Commit;

  { Disconnect from the SQLLite database }
  SLDatabase.Free;
  SLDatabase := nil;
end;

end.
