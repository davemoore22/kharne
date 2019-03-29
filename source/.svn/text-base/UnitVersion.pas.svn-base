{ UnitVersion

  Copyright (c) 2007-2009 Dave Moore  

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

  Contributor(s): Lance Leonard

}

unit UnitVersion;

interface

uses Windows,SysUtils, Messages, HotLog;

function GetVersion: String;

implementation

{ Version Derivation Code by Lance Leonard.
  Used from http://www.techtricks.com/delphi/getversion.php }

{ ---------------------------------------------------------
   Extracts the FileVersion element of the VERSIONINFO
   structure that Delphi maintains as part of a project's
   options.

   Results are returned as a standard string.  Failure
   is reported as "".

   Note that this implementation was derived from similar
   code used by Delphi to validate ComCtl32.dll.  For
   details, see COMCTRLS.PAS, line 3541.
  -------------------------------------------------------- }
function getVersion : string;
const
  NOVIDATA = '';
var
  dwInfoSize,           // Size of VERSIONINFO structure
  dwVerSize,            // Size of Version Info Data
  dwWnd: DWORD;         // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer;   // pointer to a version buffer
  strFileName,          // Name of the file to check
  strVersion: string;   // Holds parsed version number
begin
  { Logging }
  hLog.Add('{now} {lNum} UnitVersion.getVersion()');

  { Default result }
  Result := '';

  try
    strFileName := paramStr(0);
    dwInfoSize := getFileVersionInfoSize(pChar(strFileName), dwWnd);

    if (dwInfoSize = 0) then
    begin
      result := NOVIDATA;
    end
    else
    begin
      getMem(ptrVerBuf, dwInfoSize);
      try
        if getFileVersionInfo(pChar(strFileName), dwWnd, dwInfoSize,
          ptrVerBuf) then
        begin
          if verQueryValue(ptrVerBuf, '\', pointer(FI), dwVerSize ) then
          begin
            strVersion := format('%d.%d.%d.%d',
                                 [hiWord(FI.dwFileVersionMS),
                                  loWord(FI.dwFileVersionMS),
                                  hiWord(FI.dwFileVersionLS),
                                  loWord(FI.dwFileVersionLS)]);
          end;
        end;
      finally
        freeMem( ptrVerBuf );
      end;
    end;
  
    Result := strVersion;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
