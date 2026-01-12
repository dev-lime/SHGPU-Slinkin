program DynamicDatabase;

uses
  SysUtils, Classes, StreamManager, ReaderWriterManager, unit1;

type
  TPerson = record
    FullName: AnsiString;
    Gender: AnsiString;
    BirthDate: AnsiString;
    IDNumber: AnsiString;
    ChildrenIDs: array of AnsiString;
  end;

var
  People: array of TPerson;

procedure AddPerson;
var
  Person: TPerson;
  ChildrenInput, ChildID: AnsiString;
  CommaPos: Integer;
begin
  while True do
  begin
    Write('ФИО: ');
    ReadLn(Person.FullName);
    if Person.FullName = '' then exit;

    Write('Пол: ');
    ReadLn(Person.Gender);

    Write('Дата рождения: ');
    ReadLn(Person.BirthDate);

    Write('Уникальный ID: ');
    ReadLn(Person.IDNumber);

    Write('ID детей (через запятую, оставьте пустым, если детей нет): ');
    ReadLn(ChildrenInput);
    SetLength(Person.ChildrenIDs, 0);

    while Length(ChildrenInput) > 0 do
    begin
      CommaPos := Pos(',', ChildrenInput);
      if CommaPos > 0 then
      begin
        ChildID := Trim(Copy(ChildrenInput, 1, CommaPos - 1));
        Delete(ChildrenInput, 1, CommaPos);
      end
      else
      begin
        ChildID := Trim(ChildrenInput);
        ChildrenInput := '';
      end;

      if ChildID <> '' then
      begin
        SetLength(Person.ChildrenIDs, Length(Person.ChildrenIDs) + 1);
        Person.ChildrenIDs[High(Person.ChildrenIDs)] := ChildID;
      end;
    end;

    SetLength(People, Length(People) + 1);
    People[High(People)] := Person;
  end;
end;

procedure PrintDatabase;
var
  name: AnsiString;
  i, j, k: Integer;
begin
  for i := 0 to High(People) do
  begin
    WriteLn('ФИО: ', People[i].FullName);
    WriteLn('Пол: ', People[i].Gender);
    WriteLn('Дата рождения: ', People[i].BirthDate);
    WriteLn('Уникальный ID: ', People[i].IDNumber);
    Write('Дети: ');

    if Length(People[i].ChildrenIDs) = 0 then
      WriteLn('Нет детей')
    else
    begin
      for j := 0 to High(People[i].ChildrenIDs) do
      begin
        name := '---';
        for k := 0 to High(People) do
        begin
          if People[k].IDNumber = People[i].ChildrenIDs[j] then
          begin
            name := People[k].FullName;
            break;
          end;
        end;
        Write(People[i].ChildrenIDs[j], ' (', name, ')');
        if j < High(People[i].ChildrenIDs) then
          Write(', ');
      end;
      WriteLn;
    end;
    WriteLn;
  end;
end;

procedure SaveToArrayStream(const FileName: string);
var
  Stream: TArrayStream;
  Manager: TStreamManager;
begin
  Stream := TArrayStream.Create;
  try
    Manager := TStreamManager.Create;
    try
      Manager.SaveToStream(People, Stream);
      Stream.SaveToFile(FileName);
      WriteLn('Данные сохранены в файл через TArrayStream: ', FileName);
    finally
      Manager.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure LoadFromArrayStream(const FileName: string);
var
  Stream: TArrayStream;
  Manager: TStreamManager;
begin
  Stream := TArrayStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Manager := TStreamManager.Create;
    try
      People := Manager.LoadFromStream(Stream);
      WriteLn('Данные загружены из файла через TArrayStream: ', FileName);
    finally
      Manager.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure SaveToReaderWriter(const FileName: string);
var
  Stream: TFileStream;
  Writer: TWriter;
  Manager: TReaderWriterManager;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Writer := TWriter.Create(Stream, 4096);
    try
      Manager := TReaderWriterManager.Create;
      try
        Manager.SaveToWriter(People, Writer);
        WriteLn('Данные сохранены через TWriter: ', FileName);
      finally
        Manager.Free;
      end;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure LoadFromReaderWriter(const FileName: string);
var
  Stream: TFileStream;
  Reader: TReader;
  Manager: TReaderWriterManager;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Reader := TReader.Create(Stream, 4096);
    try
      Manager := TReaderWriterManager.Create;
      try
        People := Manager.LoadFromReader(Reader);
        WriteLn('Данные загружены через TReader: ', FileName);
      finally
        Manager.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure ShowMenu;
var
  Choice: Integer;
  FileName: AnsiString;
begin
  repeat
    WriteLn;
    WriteLn('=== МЕНЮ ===');
    WriteLn('1. Ввод данных вручную');
    WriteLn('2. Сохранить данные в TArrayStream');
    WriteLn('3. Загрузить данные из TArrayStream');
    WriteLn('4. Сохранить данные через TWriter');
    WriteLn('5. Загрузить данные через TReader');
    WriteLn('6. Вывести базу данных');
    WriteLn('0. Выход');
    Write('Выберите действие: ');
    ReadLn(Choice);

    case Choice of
      1: begin
           WriteLn('Начинайте ввод данных. Чтобы завершить ввод, оставьте ФИО пустым.');
           AddPerson;
         end;
      2: begin
           Write('Введите имя файла для сохранения (TArrayStream): ');
           ReadLn(FileName);
           SaveToArrayStream(FileName);
         end;
      3: begin
           Write('Введите имя файла для загрузки (TArrayStream): ');
           ReadLn(FileName);
           LoadFromArrayStream(FileName);
         end;
      4: begin
           Write('Введите имя файла для сохранения (TWriter): ');
           ReadLn(FileName);
           SaveToReaderWriter(FileName);
         end;
      5: begin
           Write('Введите имя файла для загрузки (TReader): ');
           ReadLn(FileName);
           LoadFromReaderWriter(FileName);
         end;
      6: begin
           WriteLn;
           WriteLn('Все данные:');
           PrintDatabase;
         end;
    end;
  until Choice = 0;
end;

begin
  ShowMenu;
end.

