unit StreamManager;

interface

uses
  SysUtils, Classes;

type
  TPerson = record
    FullName: AnsiString;
    Gender: AnsiString;
    BirthDate: AnsiString;
    IDNumber: AnsiString;
    ChildrenIDs: array of AnsiString;
  end;

  TPeopleArray = array of TPerson;

  TArrayStream = class(TStream)
  private
    FData: TBytes;
    FPosition: Integer;
    FSize: Integer;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure Clear;

    property Data: TBytes read FData;
  end;

  TStreamManager = class
  public
    procedure SaveToStream(const People: TPeopleArray; Stream: TStream);
    function LoadFromStream(Stream: TStream): TPeopleArray;
  end;

implementation

{ TArrayStream }

constructor TArrayStream.Create;
begin
  inherited Create;
  FPosition := 0;
  FSize := 0;
  SetLength(FData, 0);
end;

destructor TArrayStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TArrayStream.SetSize(NewSize: Longint);
begin
  SetLength(FData, NewSize);
  FSize := NewSize;
  if FPosition > FSize then
    FPosition := FSize;
end;

function TArrayStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesToRead: Longint;
begin
  if FPosition >= FSize then
    BytesToRead := 0
  else
  begin
    BytesToRead := FSize - FPosition;
    if BytesToRead > Count then
      BytesToRead := Count;

    Move(FData[FPosition], Buffer, BytesToRead);
    Inc(FPosition, BytesToRead);
  end;
  Result := BytesToRead;
end;

function TArrayStream.Write(const Buffer; Count: Longint): Longint;
var
  NewPos: Longint;
begin
  NewPos := FPosition + Count;
  if NewPos > FSize then
    SetSize(NewPos);

  Move(Buffer, FData[FPosition], Count);
  FPosition := NewPos;
  Result := Count;
end;

function TArrayStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FSize + Offset;
  end;

  if FPosition < 0 then FPosition := 0;
  if FPosition > FSize then FPosition := FSize;

  Result := FPosition;
end;

procedure TArrayStream.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    Position := 0;
    FileStream.CopyFrom(Self, Size);
  finally
    FileStream.Free;
  end;
end;

procedure TArrayStream.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Clear;
    Position := 0;
    CopyFrom(FileStream, 0);
  finally
    FileStream.Free;
  end;
end;

procedure TArrayStream.Clear;
begin
  SetLength(FData, 0);
  FPosition := 0;
  FSize := 0;
end;

{ TStreamManager }

procedure TStreamManager.SaveToStream(const People: TPeopleArray; Stream: TStream);
var
  Count, i, j: Integer;
  Len: Integer;
  StrBytes: TBytes;
begin
  // Записываем количество записей
  Count := Length(People);
  Stream.WriteBuffer(Count, SizeOf(Count));

  for i := 0 to High(People) do
  begin
    // Сохраняем строковые поля
    StrBytes := TEncoding.UTF8.GetBytes(People[i].FullName);
    Len := Length(StrBytes);
    Stream.WriteBuffer(Len, SizeOf(Len));
    Stream.WriteBuffer(StrBytes[0], Len);

    StrBytes := TEncoding.UTF8.GetBytes(People[i].Gender);
    Len := Length(StrBytes);
    Stream.WriteBuffer(Len, SizeOf(Len));
    Stream.WriteBuffer(StrBytes[0], Len);

    StrBytes := TEncoding.UTF8.GetBytes(People[i].BirthDate);
    Len := Length(StrBytes);
    Stream.WriteBuffer(Len, SizeOf(Len));
    Stream.WriteBuffer(StrBytes[0], Len);

    StrBytes := TEncoding.UTF8.GetBytes(People[i].IDNumber);
    Len := Length(StrBytes);
    Stream.WriteBuffer(Len, SizeOf(Len));
    Stream.WriteBuffer(StrBytes[0], Len);

    // Сохраняем массив детей
    Count := Length(People[i].ChildrenIDs);
    Stream.WriteBuffer(Count, SizeOf(Count));

    for j := 0 to High(People[i].ChildrenIDs) do
    begin
      StrBytes := TEncoding.UTF8.GetBytes(People[i].ChildrenIDs[j]);
      Len := Length(StrBytes);
      Stream.WriteBuffer(Len, SizeOf(Len));
      Stream.WriteBuffer(StrBytes[0], Len);
    end;
  end;
end;

function TStreamManager.LoadFromStream(Stream: TStream): TPeopleArray;
var
  Count, i, j: Integer;
  Len: Integer;
  StrBytes: TBytes;
begin
  // Читаем количество записей
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(Result, Count);

  for i := 0 to Count - 1 do
  begin
    // Читаем строковые поля
    Stream.ReadBuffer(Len, SizeOf(Len));
    SetLength(StrBytes, Len);
    Stream.ReadBuffer(StrBytes[0], Len);
    Result[i].FullName := TEncoding.UTF8.GetString(StrBytes);

    Stream.ReadBuffer(Len, SizeOf(Len));
    SetLength(StrBytes, Len);
    Stream.ReadBuffer(StrBytes[0], Len);
    Result[i].Gender := TEncoding.UTF8.GetString(StrBytes);

    Stream.ReadBuffer(Len, SizeOf(Len));
    SetLength(StrBytes, Len);
    Stream.ReadBuffer(StrBytes[0], Len);
    Result[i].BirthDate := TEncoding.UTF8.GetString(StrBytes);

    Stream.ReadBuffer(Len, SizeOf(Len));
    SetLength(StrBytes, Len);
    Stream.ReadBuffer(StrBytes[0], Len);
    Result[i].IDNumber := TEncoding.UTF8.GetString(StrBytes);

    // Читаем массив детей
    Stream.ReadBuffer(Count, SizeOf(Count));
    SetLength(Result[i].ChildrenIDs, Count);

    for j := 0 to Count - 1 do
    begin
      Stream.ReadBuffer(Len, SizeOf(Len));
      SetLength(StrBytes, Len);
      Stream.ReadBuffer(StrBytes[0], Len);
      Result[i].ChildrenIDs[j] := TEncoding.UTF8.GetString(StrBytes);
    end;
  end;
end;

end.

