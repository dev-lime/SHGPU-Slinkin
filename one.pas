program DynamicDatabase;
uses sysutils;

type
    TStringArray = array of string;
    TPerson = record
        FIO: string;
		Gender: char; // 'M' - мужчина, 'F' - женщина
		BirthDate: TDateTime;
		ID: string;
		ChildrenIDs: TStringArray;
	end;

var
	People: array of TPerson;
	line: string;
	numPeople: integer;

procedure AddPerson(person: TPerson);
begin
	SetLength(People, numPeople + 1);
	People[numPeople] := person;
	Inc(numPeople);
end;

function FindPersonByID(ID: string): integer;
var
	i: integer;
begin
	for i := 0 to numPeople - 1 do
	begin
		if People[i].ID = ID then
		begin
			FindPersonByID := i;
			Exit;
		end;
	end;
	FindPersonByID := -1;
end;

procedure ReadInput();
var
	person: TPerson;
	childrenCount, i: integer;
	childID: string;
begin
	numPeople := 0;
    while True do
	begin
		ReadLn(line);
		if line = '' then
			Break;
    
		person.FIO := line;
		ReadLn(person.Gender);
		ReadLn(line);
		person.BirthDate := StrToDate(line);
		ReadLn(person.ID);

		// Вводим информацию о детях
		ReadLn(childrenCount);
		SetLength(person.ChildrenIDs, childrenCount);
		for i := 0 to childrenCount - 1 do
     	begin
			ReadLn(childID);
		person.ChildrenIDs[i] := childID;
		end;

		AddPerson(person);
    end;
end;

procedure PrintPersonInfo(index: integer);
var
    i, childIndex: integer;
    childID: string;
begin
    with People[index] do
    begin
		WriteLn('ФИО: ', FIO);
		WriteLn('Пол: ', Gender);
		WriteLn('Дата рождения: ', DateToStr(BirthDate));
		WriteLn('Номер удостоверения личности: ', ID);
		WriteLn('Дети:');
		for i := 0 to Length(ChildrenIDs) - 1 do
		begin
			childID := ChildrenIDs[i];
			childIndex := FindPersonByID(childID);
			if childIndex <> -1 then
				WriteLn(' ', childID, ' - ', People[childIndex].FIO)
			else
				WriteLn(' ', childID, ' - - -');
		end;
    end;
end;

procedure PrintAllPeople();
var
    i: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
		PrintPersonInfo(i);
		WriteLn;
    end;
end;

procedure FindWomenByBirthDate(date: TDateTime);
var
    i: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
		if (People[i].Gender = 'F') and (People[i].BirthDate = date) then
		begin
		    WriteLn(People[i].FIO);
		end;
    end;
end;

procedure FindParentsByChildID(childID: string);
var
    i, j: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
        for j := 0 to Length(People[i].ChildrenIDs) - 1 do
        begin
		    if People[i].ChildrenIDs[j] = childID then
		    begin
			    WriteLn(People[i].FIO);
            end;
	    end;
    end;
end;

procedure FindGrandfathers();
var
    i, j: integer;
    parentIndex, grandparentIndex: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
	    if People[i].Gender = 'M' then
	    begin
		    for j := 0 to Length(People[i].ChildrenIDs) - 1 do
		    begin
			    parentIndex := FindPersonByID(People[i].ChildrenIDs[j]);
			    if parentIndex <> -1 then
			    begin
				    if Length(People[parentIndex].ChildrenIDs) > 0 then
				    begin
					    WriteLn(People[i].FIO);
					    Break;
				    end;
			    end;
		    end;
		end;
    end;
end;

procedure FindOrphans();
var
    i, j, found: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
		found := 0;
       	for j := 0 to numPeople - 1 do
begin
		    if i <> j then
		    begin
			    if FindPersonByID(People[i].ID) <> -1 then
			    begin
				    Inc(found);
				    Break;
			    end;
        	end;
	    end;
	    if found = 0 then
		    WriteLn(People[i].FIO);
    end;
end;

procedure ValidateDatabase();
var
    i, j, k, parentIndex, childIndex: integer;
begin
    for i := 0 to numPeople - 1 do
    begin
	    // Проверка детей
		for j := 0 to Length(People[i].ChildrenIDs) - 1 do
       	begin
		    childIndex := FindPersonByID(People[i].ChildrenIDs[j]);
		    if childIndex = -1 then
			    WriteLn('Ошибка: ребенок с ID ', People[i].ChildrenIDs[j], ' не найден.');
           	// Проверка возраста детей
           	if (YearsBetween(People[i].BirthDate, People[childIndex].BirthDate) < 10) or
			   (YearsBetween(People[i].BirthDate, People[childIndex].BirthDate) > 70) then
			    WriteLn('Ошибка возраста у ', People[i].FIO, ' и ребенка ', People[childIndex].FIO);
        end;
    end;
end;

begin
    ReadInput();
    PrintAllPeople();
    // Пример задач
    WriteLn('Женщины, родившиеся 01.01.1990:');
    FindWomenByBirthDate(StrToDate('01.01.1990'));
    
    WriteLn('Родители ребенка с ID 12345:');
    FindParentsByChildID('12345');
    
    WriteLn('Дедушки:');
    FindGrandfathers();
    
    WriteLn('Сироты:');
    FindOrphans();
    
    WriteLn('Проверка БД:');
    ValidateDatabase();
end.
