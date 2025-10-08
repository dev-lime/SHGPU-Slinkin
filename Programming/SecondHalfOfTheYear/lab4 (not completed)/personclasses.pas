unit PersonClasses;

interface

uses
  SysUtils, Classes, DateUtils;

type
  // Исключения
  EFileNotFound = class(Exception);
  EInvalidPropertyType = class(Exception);
  EUnexpectedEndOfFile = class(Exception);
  EFileCreationError = class(Exception);
  EIndexOutOfBounds = class(Exception);
  ESameGenderParents = class(Exception);
  EDublicateID = class(Exception);
  EInvalidParentAge = class(Exception);
  EParentCycle = class(Exception);

  TPerson = class;

  TPersonList = class;

  TPerson = class(TObject)
  private
    FFullName: AnsiString;
    FGender: Boolean; // True - male, False - female
    FBirthDate: TDateTime;
    FID: AnsiString;
    FFather: TPerson;
    FMother: TPerson;
    procedure SetFather(const Value: TPerson);
    procedure SetMother(const Value: TPerson);
  public
    constructor Create(const AFullName: AnsiString; AGender: Boolean;
      ABirthDate: TDateTime; const AID: AnsiString);

    property FullName: AnsiString read FFullName write FFullName;
    property Gender: Boolean read FGender write FGender;
    property BirthDate: TDateTime read FBirthDate write FBirthDate;
    property ID: AnsiString read FID write FID;
    property Father: TPerson read FFather write SetFather;
    property Mother: TPerson read FMother write SetMother;

    function Age: Integer;
    function ToString: AnsiString; override;
  end;

  TPersonList = class(TObject)
  private
    FItems: array of TPerson;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPerson;
    procedure SetItem(Index: Integer; const Value: TPerson);
    function FindByID(const ID: AnsiString): TPerson;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPerson read GetItem write SetItem; default;

    procedure Add(Person: TPerson);
    procedure Delete(Index: Integer);
    procedure Clear;

    procedure LoadFromFile(const FileName: AnsiString);
    procedure SaveToFile(const FileName: AnsiString);

    procedure Validate;
  end;

implementation

{ TPerson }

constructor TPerson.Create(const AFullName: AnsiString; AGender: Boolean;
  ABirthDate: TDateTime; const AID: AnsiString);
begin
  inherited Create;
  FFullName := AFullName;
  FGender := AGender;
  FBirthDate := ABirthDate;
  FID := AID;
  FFather := nil;
  FMother := nil;
end;

procedure TPerson.SetFather(const Value: TPerson);
begin
  if Value <> nil then
  begin
    if not Value.Gender then
      raise Exception.Create('Father must be male');
  end;
  FFather := Value;
end;

procedure TPerson.SetMother(const Value: TPerson);
begin
  if Value <> nil then
  begin
    if Value.Gender then
      raise Exception.Create('Mother must be female');
  end;
  FMother := Value;
end;

function TPerson.Age: Integer;
begin
  Result := YearsBetween(Now, FBirthDate);
end;

function TPerson.ToString: AnsiString;
var
  GenderStr: AnsiString;
begin
  if FGender then
    GenderStr := 'Male'
  else
    GenderStr := 'Female';

  Result := Format('%s (%s), ID: %s, Born: %s',
    [FFullName,
     GenderStr,
     FID,
     DateToStr(FBirthDate)]);
end;

{ TPersonList }

constructor TPersonList.Create;
begin
  inherited;
  SetLength(FItems, 0);
end;

destructor TPersonList.Destroy;
begin
  Clear;
  inherited;
end;

function TPersonList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TPersonList.GetItem(Index: Integer): TPerson;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    raise EIndexOutOfBounds.CreateFmt('Index %d is out of bounds', [Index]);
  Result := FItems[Index];
end;

procedure TPersonList.SetItem(Index: Integer; const Value: TPerson);
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    raise EIndexOutOfBounds.CreateFmt('Index %d is out of bounds', [Index]);
  FItems[Index] := Value;
end;

procedure TPersonList.Add(Person: TPerson);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := Person;
end;

procedure TPersonList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    raise EIndexOutOfBounds.CreateFmt('Index %d is out of bounds', [Index]);

  FItems[Index].Free;
  for I := Index to High(FItems) - 1 do
    FItems[I] := FItems[I + 1];
  SetLength(FItems, Length(FItems) - 1);
end;

procedure TPersonList.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Free;
  SetLength(FItems, 0);
end;

function TPersonList.FindByID(const ID: AnsiString): TPerson;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FItems) do
    if FItems[I].ID = ID then
    begin
      Result := FItems[I];
      Break;
    end;
end;

procedure TPersonList.LoadFromFile(const FileName: AnsiString);
var
  F: TextFile;
  S, FullName, ID, FatherID, MotherID: AnsiString;
  Gender: Boolean;
  BirthDate: TDateTime;
  Person: TPerson;
begin
  if not FileExists(FileName) then
    raise EFileNotFound.CreateFmt('File %s not found', [FileName]);

  AssignFile(F, FileName);
  try
    Reset(F);
    Clear;

    while not EOF(F) do
    begin
      // Чтение FullName
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading FullName');
      ReadLn(F, FullName);

      // Чтение Gender
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading Gender');
      ReadLn(F, S);
      if not (S = 'Male') and not (S = 'Female') then
        raise EInvalidPropertyType.Create('Invalid gender value');
      Gender := S = 'Male';

      // Чтение BirthDate
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading BirthDate');
      ReadLn(F, S);
      try
        BirthDate := StrToDate(S);
      except
        raise EInvalidPropertyType.Create('Invalid date format');
      end;

      // Чтение ID
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading ID');
      ReadLn(F, ID);

      // Чтение FatherID
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading FatherID');
      ReadLn(F, FatherID);

      // Чтение MotherID
      if EOF(F) then raise EUnexpectedEndOfFile.Create('Unexpected end of file while reading MotherID');
      ReadLn(F, MotherID);

      // Пустая строка между записями
      if not EOF(F) then ReadLn(F);

      // Создание личности
      Person := TPerson.Create(FullName, Gender, BirthDate, ID);
      Add(Person);
    end;

    Reset(F);

    for Person in FItems do
    begin
      ReadLn(F); // FullName
      ReadLn(F); // Gender
      ReadLn(F); // BirthDate
      ReadLn(F); // ID
      ReadLn(F, FatherID);
      ReadLn(F, MotherID);
      if not EOF(F) then ReadLn(F);

      if FatherID <> '' then
        Person.Father := FindByID(FatherID);
      if MotherID <> '' then
        Person.Mother := FindByID(MotherID);
    end;

  finally
    CloseFile(F);
  end;
end;

procedure TPersonList.SaveToFile(const FileName: AnsiString);
var
  F: TextFile;
  I: Integer;
  GenderStr: AnsiString;
begin
  AssignFile(F, FileName);
  try
    try
      Rewrite(F);
    except
      raise EFileCreationError.CreateFmt('Cannot create file %s', [FileName]);
    end;

    for I := 0 to High(FItems) do
    begin
      WriteLn(F, FItems[I].FullName);

      if FItems[I].Gender then
        GenderStr := 'Male'
      else
        GenderStr := 'Female';
      WriteLn(F, GenderStr);

      WriteLn(F, DateToStr(FItems[I].BirthDate));
      WriteLn(F, FItems[I].ID);

      if FItems[I].Father <> nil then
        WriteLn(F, FItems[I].Father.ID)
      else
        WriteLn(F, '');

      if FItems[I].Mother <> nil then
        WriteLn(F, FItems[I].Mother.ID)
      else
        WriteLn(F, '');

      if I < High(FItems) then
        WriteLn(F);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPersonList.Validate;
var
  I, J: Integer;
  Parent: TPerson;
  Visited: array of TPerson;

  function HasCycle(Start, Current: TPerson): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    if Current = nil then Exit;

    if Start = Current then
      Exit(True);

    // не посещали ли уже этого родителя
    for K := 0 to High(Visited) do
      if Visited[K] = Current then
        Exit;

    // Добавляет в посещенные
    SetLength(Visited, Length(Visited) + 1);
    Visited[High(Visited)] := Current;

    // Рекурсивно проверяет родителей
    Result := HasCycle(Start, Current.Father) or HasCycle(Start, Current.Mother);
  end;

begin
  // Проверка на дубликаты ID
  for I := 0 to High(FItems) do
    for J := I + 1 to High(FItems) do
      if FItems[I].ID = FItems[J].ID then
        raise EDublicateID.CreateFmt('Duplicate ID found: %s', [FItems[I].ID]);

  // Проверка родителей
  for I := 0 to High(FItems) do
  begin
    // Проверка пола родителей
    if (FItems[I].Father <> nil) and (FItems[I].Mother <> nil) then
      if FItems[I].Father.Gender = FItems[I].Mother.Gender then
        raise ESameGenderParents.CreateFmt('Parents have same gender for person: %s', [FItems[I].ID]);

    // Проверка возраста родителей
    if FItems[I].Father <> nil then
    begin
      Parent := FItems[I].Father;
      if (Parent.Age < 10) or (Parent.Age > 70) then
        raise EInvalidParentAge.CreateFmt('Father %s has invalid age: %d', [Parent.ID, Parent.Age]);
    end;

    if FItems[I].Mother <> nil then
    begin
      Parent := FItems[I].Mother;
      if (Parent.Age < 12) or (Parent.Age > 60) then
        raise EInvalidParentAge.CreateFmt('Mother %s has invalid age: %d', [Parent.ID, Parent.Age]);
    end;

    // Проверка на циклы
    if FItems[I].Father <> nil then
    begin
      SetLength(Visited, 0);
      if HasCycle(FItems[I], FItems[I].Father) then
        raise EParentCycle.CreateFmt('Cycle detected in father lineage for person: %s', [FItems[I].ID]);
    end;

    if FItems[I].Mother <> nil then
    begin
      SetLength(Visited, 0);
      if HasCycle(FItems[I], FItems[I].Mother) then
        raise EParentCycle.CreateFmt('Cycle detected in mother lineage for person: %s', [FItems[I].ID]);
    end;
  end;
end;

end.

