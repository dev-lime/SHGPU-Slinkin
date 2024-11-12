program DynamicDatabase;

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

procedure AddPerson;
var
  Person: TPerson;
  ChildID: string;
begin
  WriteLn('Введите данные для нового человека (двойной Enter для завершения ввода):');

  // Ввод ФИО
  Write('ФИО: ');
  ReadLn(Person.FullName);
  if Person.FullName = '' then Exit;

  // Ввод пола
  Write('Пол: ');
  ReadLn(Person.Gender);

  // Ввод даты рождения
  Write('Дата рождения: ');
  ReadLn(Person.BirthDate);

  // Ввод уникального ID
  Write('Уникальный ID: ');
  ReadLn(Person.IDNumber);

  // Ввод ID детей
  WriteLn('ID детей (через пробел, пустой ввод завершает список детей):');
  SetLength(Person.ChildrenIDs, 0);
  while True do
  begin
    ReadLn(ChildID);
    if ChildID = '' then Break;
    SetLength(Person.ChildrenIDs, Length(Person.ChildrenIDs) + 1);
    Person.ChildrenIDs[High(Person.ChildrenIDs)] := ChildID;
  end;

  // Добавляем человека в базу данных
  SetLength(People, Length(People) + 1);
  People[High(People)] := Person;
end;

procedure PrintDatabase;
var
  i, j: Integer;
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
      for j := 0 to High(People[i].ChildrenIDs) do
        Write(People[i].ChildrenIDs[j], ' ');
        
    WriteLn;
    WriteLn;
  end;
end;

var
  Input: string;

begin
  WriteLn('Начинайте ввод данных. Для завершения ввода нажмите два раза Enter.');
  repeat
    AddPerson;
    WriteLn;
    
    // Проверка на двойной Enter
    Write('Нажмите Enter для продолжения или снова Enter для завершения: ');
    ReadLn(Input);
  until Input = '';

  WriteLn;
  WriteLn('Все данные:');
  PrintDatabase;
end.