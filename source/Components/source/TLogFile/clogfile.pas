unit clogfile;

interface

uses
  Classes, SysUtils, Forms;

type
  TMsgCategory = (mtInfo, mtWarning, mtError, mtExcept);

  TStdMessage = class(TCollectionItem)
  private
    FText: String;
    FCategory: TMsgCategory;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: String read FText write FText;
    property Category: TMsgCategory read FCategory write FCategory;
  end;

  TStdMessages = class(TCollection)
  private
    function GetStdMessage(Index: Cardinal): TStdMessage;
    procedure SetStdMessage(Index: Cardinal; Value: TStdMessage);
  public
    property StdMessage[Index: Cardinal]: TStdMessage read GetStdMessage write
      SetStdMessage; default;
    function Add: TStdMessage;
  end;

  TLogFile = class(TComponent)
  private
    FFileName: TFileName;
    FFormat: String;
    FMaxLines: Cardinal;
    FLogExcepts: Boolean;
    FStringList: TStringList;
    FStream: TFileStream;
    FReady: Boolean;
    FStdMessages: TStdmessages;
    procedure SetStdMessage(Value: TStdMessages);
    procedure SetLogExcepts(Value: Boolean);
    procedure ProcessExcept(Sender: TObject; E: Exception);
    procedure SetFileName(Value: TFileName);
    procedure Flush;
    function GetActualSize: Integer;
    function GetLines: Integer;
    procedure SetMaxLines(Value: Cardinal);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure LogMessage(AMessage: String; MsgCategory: TMsgCategory = mtInfo);
    procedure LogStdMessage(MessageID: Integer; Strings: Array of String);
  published
    property FileName: TFileName read FFileName write SetFileName;
    property FileSize: Integer read GetActualSize;
    property MaxLines: Cardinal read FMaxLines write SetMaxLines;
    property Lines: Integer read GetLines;
    property LogExceptions: Boolean read FLogExcepts write SetLogExcepts;
    property Ready: Boolean read FReady;
    property DateTimeFormat: String read FFormat write FFormat;
    property StdMessages: TStdMessages read FStdMessages write SetStdMessage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [TLogFile]);
end;

{ TStdMessage }

procedure TStdMessage.Assign(Source: TPersistent);
begin
  if Source is TStdMessage
    then begin
      FText := TStdMessage(Source).FText;
      FCategory := TStdMessage(Source).FCategory;
    end
    else inherited Assign(Source);
end;

constructor TStdMessage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FText := '';
  FCategory := mtInfo;
end;

{ TStdMessages }

function TStdMessages.Add: TStdMessage;
begin
  Result := TStdMessage.Create(Self);
end;

function TStdMessages.GetStdMessage(Index: Cardinal): TStdMessage;
begin
  Result := TStdMessage(inherited GetItem(Index));
end;

procedure TStdMessages.SetStdMessage(Index: Cardinal;
  Value: TStdMessage);
begin
  inherited SetItem(Index, Value);
end;

{ TLogFile }

constructor TLogFile.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FStdMessages := TStdMessages.Create(TStdMessage);
  FStringList := TStringList.Create;
  with FStringList
    do begin
      Duplicates := dupIgnore;
      Sorted := false;
      CaseSensitive := true;
    end;
  FFormat := 'dd. mmm, yyyy hh:mm:ss';
  LogExceptions := true;
  FMaxLines := 100;
  FReady := false;
  FileName := '';
end;

destructor TLogFile.Destroy;
begin
  LogMessage('Logging ended.' + #13#10, mtInfo);
  FStream.Free;
  FStringList.Destroy;
  FStdMessages.Destroy;
  inherited Destroy;
end;

procedure TLogFile.LogMessage(AMessage: String; MsgCategory: TMsgCategory);
var
  CategoryStr: String;
begin
  if not Ready
    then Exit;
  if MsgCategory = mtInfo
    then CategoryStr := '[Info]'
    else if MsgCategory = mtWarning
      then CategoryStr := '[Warning]'
      else if MsgCategory = mtError
        then CategoryStr := '[Error]'
        else CategoryStr := '[Except]';
  FStringList.Add('[' + FormatDateTime(FFormat, Now) + ']' +
    CategoryStr + ' ' + AMessage);
  while FStringList.Count > Integer(FMaxLines)
    do FstringList.Delete(0);
  Flush;
end;

function TLogFile.GetActualSize: Integer;
var
  SearchRec: TSearchRec;
begin
  Result := -1;
  try
    if FindFirst(FFileName, faAnyFile, SearchRec) = 0
      then Result := SearchRec.Size
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TLogFile.SetStdMessage(Value: TStdMessages);
begin
  FStdMessages.Assign(Value);
end;

procedure TLogFile.LogStdMessage(MessageID: Integer;
  Strings: Array of String);
var
  Temp, Trimmed: String;
  Last: Boolean;
  CurrPos, i: Integer;
begin
  if MessageID > FStdMessages.Count - 1
    then Exit;
  Trimmed := '';
  Temp := FStdMessages[MessageID].Text;
  Last := false;
  i := 0;
  while not Last
    do begin
      CurrPos := Pos('$', Temp);
      if CurrPos > 0
        then begin
          Trimmed := Trimmed + Copy(Temp, 0, CurrPos - 1) + Strings[i];
          i := i + 1;
          Temp := Copy(Temp, CurrPos + 1, Length(Temp) - CurrPos);
        end
      else begin
         Trimmed := Trimmed + Temp;
         Last := true;
      end;
    end;
  LogMessage(Trimmed, FStdMessages[MessageID].Category);
end;

procedure TLogFile.SetLogExcepts(Value: Boolean);
begin
  FLogExcepts := Value;
  if not (csDesigning in Self.ComponentState)
    then if FLogExcepts
      then Application.OnException := Self.ProcessExcept
      else Application.OnException := nil;
end;

procedure TLogFile.ProcessExcept(Sender: TObject; E: Exception);
begin
  LogMessage(E.Message, mtExcept);
end;

procedure TLogFile.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  if (Value = '') or (csDesigning in Self.ComponentState)
    then Exit;
  FReady := true;
  try
    if FileExists(FFileName)
      then begin
        FStream := TFileStream.Create(FFileName, fmOpenReadWrite or
          fmShareDenyWrite);
        FStringList.LoadFromStream(FStream);
      end
      else FStream := TFileStream.Create(FFileName, fmCreate or
        fmShareDenyWrite);
    LogMessage('Logging started.', mtInfo);
  except
    FReady := false;
  end;
end;

procedure TLogFile.Flush;
begin
  FStream.Seek(0, soFromBeginning);
  FStringList.SaveToStream(FStream);
end;

function TLogFile.GetLines: Integer;
begin
  if Ready
    then Result := FStringList.Count
    else Result := -1;
end;

procedure TLogFile.SetMaxLines(Value: Cardinal);
begin
  FMaxLines := Value;
  while FStringList.Count > Integer(FMaxLines)
    do FStringList.Delete(0);
end;

end.
