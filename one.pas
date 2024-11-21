{
begin
 setLength(humanBasicMass,0);
 while True do
 begin
  writeln('Enter last name, frist name, patronymic');
  readln(name1);
  if (name1 = '') then 
  begin
   break;
  end;
  readln(humanPlus.name2);
  readln(humanPlus.name3);
  humanPlus.name1:=name1;
  writeln('gender: ');
  readln(humanPlus.gender);
  writeln('day, month, year of birth');
  readln(humanPlus.day);
  readln(humanPlus.month);
  readln(humanPlus.year);
  writeln('ID number: ');
  readln(humanPlus.personNumber);
  i:=1;
  SetLength(humanPlus.child,i);
  while True do
  begin
   writeln('ID number ',i,' child: ');
   readln(personNumberChild);
   if (personNumberChild = '') then
   begin
    if (i=1) then
    begin
     humanPlus.child[i-1]:='---';
    end;
    break;
   end;
   humanPlus.child[i - 1]:=personNumberChild;
   i:=i+1;
   SetLength(humanPlus.child,i);
  end;
  setLength(humanBasicMass,Length(humanBasicMass)+1);
  humanBasicMass[Length(humanBasicMass)-1]:=humanPlus;
 end;
}
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
  ChildrenInput, ChildID: string;
  CommaPos: Integer;
begin
  // Ввод данных для одного человека
  Write('ФИО: ');
  ReadLn(Person.FullName);
  if Person.FullName = '' then Exit;  // Если пусто, прекращаем ввод

  Write('Пол: ');
  ReadLn(Person.Gender);

  Write('Дата рождения: ');
  ReadLn(Person.BirthDate);

  Write('Уникальный ID: ');
  ReadLn(Person.IDNumber);

  // Ввод ID детей через запятую
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

  // Добавляем текущего человека в массив People
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
    begin
      for j := 0 to High(People[i].ChildrenIDs) do
      begin
        Write(People[i].ChildrenIDs[j]);
        if j < High(People[i].ChildrenIDs) then
          Write(', ');
      end;
      WriteLn;
    end;
    WriteLn;
  end;
end;

var
  Input: string;

begin
  WriteLn('Начинайте ввод данных. Чтобы завершить ввод и вывести базу данных, нажмите дважды Enter.');

  repeat
    AddPerson;
    Write('Нажмите Enter для продолжения или снова Enter для завершения: ');
    ReadLn(Input);
  until Input = '';  // Выход из цикла при двойном Enter

  WriteLn;
  WriteLn('Все данные:');
  PrintDatabase;
end.