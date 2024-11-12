program DatabaseFromFile;

uses SysUtils;

type
  TPerson = record
    FullName: string;
    Gender: string;
    BirthDate: string;
    IDNumber: string;
    ChildrenIDs: array of string;
  end;

var
  People: array of TPerson;

procedure LoadDataFromFile(const FileName: string);
var
  F: Text;
  Line: string;
  CurrentPerson: TPerson;
  ChildID: string;
begin
  Assign(F, FileName);  // Исправлено AssignFile на Assign
  Reset(F);
  SetLength(People, 0);

  while not Eof(F) do
  begin
    Readln(F, Line);
    
    if Line <> '' then
    begin
      // Чтение ФИО
      CurrentPerson.FullName := Line;
      
      // Чтение пола
      Readln(F, Line);
      CurrentPerson.Gender := Line;
      
      // Чтение даты рождения
      Readln(F, Line);
      CurrentPerson.BirthDate := Line;
      
      // Чтение номера удостоверения личности
      Readln(F, Line);
      CurrentPerson.IDNumber := Line;

      // Чтение номеров детей
      SetLength(CurrentPerson.ChildrenIDs, 0);
      while not Eof(F) do
      begin
        Readln(F, Line);
        if Line = '---' then Break;

        SetLength(CurrentPerson.ChildrenIDs, Length(CurrentPerson.ChildrenIDs) + 1);
        CurrentPerson.ChildrenIDs[High(CurrentPerson.ChildrenIDs)] := Line;
      end;

      SetLength(People, Length(People) + 1);
      People[High(People)] := CurrentPerson;
    end;
  end;
  
  Close(F);  // Исправлено CloseFile на Close
end;

function FindPersonByID(const ID: string): string;
var
  i: Integer;
begin
  FindPersonByID := '- - -';  // Задаем начальное значение Result
  for i := 0 to High(People) do
    if People[i].IDNumber = ID then
    begin
      FindPersonByID := People[i].FullName;
      Exit;
    end;
end;

procedure PrintDatabase;
var
  i, j: Integer;
  ChildName: string;
begin
  for i := 0 to High(People) do
  begin
    WriteLn('ФИО: ', People[i].FullName);
    WriteLn('Пол: ', People[i].Gender);
    WriteLn('Дата рождения: ', People[i].BirthDate);
    WriteLn('Номер удостоверения личности: ', People[i].IDNumber);
    WriteLn('Дети:');
    
    if Length(People[i].ChildrenIDs) = 0 then
      WriteLn('  Нет детей')
    else
      for j := 0 to High(People[i].ChildrenIDs) do
      begin
        ChildName := FindPersonByID(People[i].ChildrenIDs[j]);
        WriteLn('  Номер удостоверения: ', People[i].ChildrenIDs[j], ' ФИО: ', ChildName);
      end;
    WriteLn;
  end;
end;

begin
  LoadDataFromFile('data.txt');
  PrintDatabase;
end.