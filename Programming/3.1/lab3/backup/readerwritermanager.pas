unit ReaderWriterManager;

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

  TReaderWriterManager = class
  public
    procedure SaveToWriter(const People: TPeopleArray; Writer: TWriter);
    function LoadFromReader(Reader: TReader): TPeopleArray;
  end;

implementation

{ TReaderWriterManager }

procedure TReaderWriterManager.SaveToWriter(const People: TPeopleArray; Writer: TWriter);
var
  i, j: Integer;
begin
  // Записываем количество записей
  Writer.WriteInteger(Length(People));

  for i := 0 to High(People) do
  begin
    // Сохраняем строковые поля
    Writer.WriteString(People[i].FullName);
    Writer.WriteString(People[i].Gender);
    Writer.WriteString(People[i].BirthDate);
    Writer.WriteString(People[i].IDNumber);

    // Сохраняем массив детей
    Writer.WriteInteger(Length(People[i].ChildrenIDs));
    for j := 0 to High(People[i].ChildrenIDs) do
      Writer.WriteString(People[i].ChildrenIDs[j]);
  end;
end;

function TReaderWriterManager.LoadFromReader(Reader: TReader): TPeopleArray;
var
  Count, i, j: Integer;
begin
  // Читаем количество записей
  Count := Reader.ReadInteger;
  SetLength(Result, Count);

  for i := 0 to Count - 1 do
  begin
    // Читаем строковые поля
    Result[i].FullName := Reader.ReadString;
    Result[i].Gender := Reader.ReadString;
    Result[i].BirthDate := Reader.ReadString;
    Result[i].IDNumber := Reader.ReadString;

    // Читаем массив детей
    Count := Reader.ReadInteger;
    SetLength(Result[i].ChildrenIDs, Count);
    for j := 0 to Count - 1 do
      Result[i].ChildrenIDs[j] := Reader.ReadString;
  end;
end;

end.

