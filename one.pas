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

procedure FindDateFemale;
var
  Input: string;
  i: Integer;
begin
  ReadLn(Input);
  for i := 0 to High(People) do
  begin
	if (People[i].Gender = 'Женский') then
	begin
      if (Input = People[i].BirthDate) then
      begin
        WriteLn;
        WriteLn('НАЙДЕНО СОВПАДЕНИЕ ПО ДАТЕ');
	    WriteLn('ФИО: ', People[i].FullName);
		WriteLn('Пол: ', People[i].Gender);
		WriteLn('Дата рождения: ', People[i].BirthDate);
		WriteLn('Уникальный ID: ', People[i].IDNumber);
	  end;
	end;
  end;
end;

procedure FindParents;
var
  Input: string;
  i, j: Integer;
begin
  ReadLn(Input);
  for i := 0 to High(People) do
  begin
	if Length(People[i].ChildrenIDs) <> 0 then
	begin
      for j := 0 to High(People[i].ChildrenIDs) do
      begin
        if (People[i].ChildrenIDs[j] = Input) then
        begin
          WriteLn;
          WriteLn('НАЙДЕНО СОВПАДЕНИЕ ПО ДЕТЯМ');
	      WriteLn('ФИО: ', People[i].FullName);
		  WriteLn('Пол: ', People[i].Gender);
		  WriteLn('Дата рождения: ', People[i].BirthDate);
		  WriteLn('Уникальный ID: ', People[i].IDNumber);
	      Write('Дети: ');
		  Write(People[i].ChildrenIDs[j]);
          if j < High(People[i].ChildrenIDs) then
            Write(', ');
          WriteLn;
        end;
      end;
    end;
  end;
end;

procedure FindGrandfathers;
var
  i, j, k: Integer;
begin
  for i := 0 to High(People) do
  begin
	if (People[i].Gender = 'Мужской') and (Length(People[i].ChildrenIDs) <> 0) then
	begin
      for j := 0 to High(People[i].ChildrenIDs) do
      begin
	    for k := 0 to High(People) do
		begin
	      if (People[k].IDNumber = People[i].ChildrenIDs[j]) and (Length(People[k].ChildrenIDs) <> 0) then
	      begin
			WriteLn;
			WriteLn('ВЫ НАПУГАЛИ ДЕДА');
			WriteLn('ФИО: ', People[i].FullName);
			WriteLn('Пол: ', People[i].Gender);
			WriteLn('Дата рождения: ', People[i].BirthDate);
			WriteLn('Уникальный ID: ', People[i].IDNumber);
		  end;
		end;
      end;
    end;
  end;
end;

begin
  WriteLn('Начинайте ввод данных. Чтобы завершить ввод и вывести базу данных, нажмите дважды Enter.');
  AddPerson;

  WriteLn;
  WriteLn('Все данные:');
  PrintDatabase;
  
  WriteLn('------------------------------');
  FindDateFemale;
  WriteLn;
  WriteLn('------------------------------');
  FindParents;
  WriteLn;
  WriteLn('------------------------------');
  FindGrandfathers;
  WriteLn;
  WriteLn('------------------------------');
end.
