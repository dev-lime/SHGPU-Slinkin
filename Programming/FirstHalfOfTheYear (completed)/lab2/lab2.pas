{$mode objfpc}
{$R+}
uses math;

type
	TSingleSet = set of byte;
	TLongSet = array of TSingleSet;

{ Функция создания множества }
function createSet(count:integer):TLongSet;
var
	rSet: TLongSet;
	len: integer;
begin
	len := (count + 256) div 256;
	setLength(rSet, len);
	result := rSet;
end;

{ Функция изменения размера множества }
procedure setSize(var dstSet:TLongSet; newCount:integer);
var
	newLen: integer;
begin 
	newLen := ((newCount + 256) div 256);
	setLength(dstSet, newLen);
end;

{ Функция получения размера множества }
function getSize(bSet:TLongSet): integer;
begin
	result := length(bSet) * 256;
end;

{ Функция уничтожения множества }
procedure destroySet(var dstSet:TLongSet);
begin
	setLength(dstSet, 0);
end;

{ Аналог операции in }
function inSet(bSet:TLongSet; e:integer):boolean;
var
	i: integer;
begin
	i := ((e + 256) div 256)-1;
	e := (e mod 256);
	if i > length(bSet) then
		result := false
	else
	begin
		if e in bSet[i] then 
			result := true
		else
			result := false;
	end;
end;

{ Аналог функции include (добавление элемента) }
procedure includeSet(var dstSet:TLongSet; e:integer);
var
	i, minLen: integer;
begin
	minLen := (e + 256) div 256;
	if (Length(dstSet) < minLen) then setLength(dstSet, minLen);
	i := minLen - 1;
	e := (e + 256) mod 256;
	Include(dstSet[i], e);
end;

{ Аналог операции + (объединение), возвращает новое множество }
function sumSet(set1,set2:TLongSet):TLongSet; 
var
	rSet: TLongSet;
	i, len: integer;
begin
	if length(set1) >= length(set2) then
		len := length(set1)
	else
		len := length(set2);
	setLength(rSet, len);
	for i := 0 to length(set1)-1 do
	begin
		rSet[i] += set1[i];
	end;
	for i := 0 to length(set2)-1 do
	begin
		rSet[i] += set2[i];
	end;
	result := rSet;
end;

{ Аналог операции - (разность), возвращает новое множество }
function subSet(set1,set2:TLongSet):TLongSet;
var
	rSet: TLongSet;
	i, len, count: integer;
begin
	if length(set1) >= length(set2) then
	begin
		len := length(set1);
		setLength(set2, len);
	end
	else
	begin
		len := length(set2);
		setLength(set1, len);
	end;
	setLength(rSet, len);
	count := 0;
	for i := 0 to len-1 do
	begin
		rSet[i] := set1[i] - set2[i];

		if rSet[i] <> [] then count := i + 1;
	end;
	if count = 0 then
		setLength(rSet, count)
	else
		setLength(rSet, count);
	result := rSet;
end;

{ Аналог операции * (пересечение), возвращает новое множество }
function mulSet(set1,set2:TLongSet):TLongSet;
var
	rSet: TLongSet;
	i, len, count: integer;
begin
	if length(set1) >= length(set2) then
		len := length(set2)
	else
		len := length(set1);
	setLength(rSet, len);
	count := 0;
	for i := 0 to len-1 do
	begin
		rSet[i] := set1[i] * set2[i];
		if rSet[i] <> [] then count := i + 1;
	end;
	if count = 0 then
		setLength(rSet, count)
	else
		setLength(rSet, count);
	result := rSet;
end;

{ Симметричная разность (объединение минус пересечение) }
function symDiffSet(set1,set2:TLongSet):TLongSet;
var
	rSet: TLongSet;
	i, len, count: integer;
begin
	if length(set1) >= length(set2) then
	begin
		len := length(set1);
		setLength(set2, len);
	end
	else
	begin
		len := length(set2);
		setLength(set1, len);
	end;
	setLength(rSet, len);
	count := 0;
	for i := 0 to len-1 do
	begin
		rSet[i] := set1[i] >< set2[i];
		if rSet[i] <> [] then count := i + 1;
	end;
	if count = 0 then
			setLength(rSet, count)
		else
			setLength(rSet, count);
	setLength(rSet, count+1);
	result := rSet;
end;

{ Аналог функции exclude (удаление элемента) }
procedure excludeSet(var dstSet:TLongSet; e:integer);
var
	i: integer;
begin
	i := ((e + 256) div 256) - 1;
	e := (e + 256) mod 256;
	exclude(dstSet[i], e);
end;

{ Вывод множества }
procedure printSet(printSet: TLongSet);
var
  	i, j: integer;
begin
	for i := 0 to High(printSet) do
		for j := 0 to 255 do
		if byte(j) in printSet[i] then
			write(i * 256 + j, ' ');
	writeln;
end;

var
  	set1, set2, resultSet: TLongSet;
begin
	set1 := createSet(300);
	includeSet(set1, 5);
	includeSet(set1, 150);
	includeSet(set1, 290);
	includeSet(set1, 2900);

	set2 := createSet(0);
	includeSet(set2, 5);
	includeSet(set2, 100);
	includeSet(set2, 150);
	includeSet(set2, 255);
	//includeSet(set2, 256);
	//includeSet(set2, 289);
	includeSet(set2, 290);
	includeSet(set2, 2900);

	write('set1: ');
	printSet(set1);
	write('set2: ');
	printSet(set2);
	writeln();

	resultSet := subSet(set1, set2);
	write('set1 - set2: ');
	printSet(resultSet);

	write('size: ');
	writeln(getSize(resultSet));
	write('length: ');
	writeln(Length(resultSet));
	writeln();

	resultSet := subSet(set2, set1);
	write('set2 - set1: ');
	printSet(resultSet);

	write('size: ');
	writeln(getSize(resultSet));
	write('length: ');
	writeln(Length(resultSet));
end.
