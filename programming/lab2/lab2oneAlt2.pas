{$mode objfpc}
program LongSetManagement;
Uses heaptrc,math;

type
    TSingleSet = set of byte;
    TLongSet = array of TSingleSet;


function createSet(count: integer): TLongSet;
var
  size: integer;
begin
  result := nil;
  size := count div 256 + 1;
  SetLength(result,size);
  
end;

procedure setSize(var dstSet:TLongSet; newCount:integer);
var
   oldSize, newSize,i : integer;   
   tempSet : TLongSet;
   
begin
	oldSize := Length(dstSet);
	newSize := newCount div 256 + 1;
	if newSize < oldSize then
	begin
		SetLength(tempSet,newSize);
		for i:=0 to newSize -1 do
		tempSet[i] := dstSet[i];
		dstSet := tempSet;
	end
	else
	begin
		oldSize := 0;
		SetLength(dstSet,newSize);
end;
end;
 
function getSize(bSet: TLongSet): integer;
begin
  Result := Length(bSet) * 256;
end;

procedure destroySet(var dstSet:TLongSet);
begin
  SetLength(dstSet, 0);
end;

function sumSet(set1, set2: TLongSet): TLongSet;
var
  maxSize, i: integer;
begin
  Result := nil;
  maxSize := Max(Length(set1), Length(set2)); 
  SetLength(Result, maxSize);

  for i := 0 to maxSize - 1 do
  begin
    Result[i] := []; 
    if i < Length(set1) then
      Result[i] := Result[i] + set1[i];
    if i < Length(set2) then
      Result[i] := Result[i] + set2[i];
  end;
end;

function subSet(set1, set2: TLongSet): TLongSet;
var
  maxSize, i: integer;
begin
  Result := nil;
  maxSize := Max(Length(set1), Length(set2)); 
  SetLength(Result, maxSize);
  for i := 0 to maxSize - 1 do
  begin
    Result[i] := []; 
    if i < Length(set1) then
      Result[i] := set1[i];
    if i < Length(set2) then
      Result[i] := Result[i] - set2[i];
  end;
end;


function mulSet(set1, set2: TLongSet): TLongSet;
var
  maxSize, i: integer;
begin
  Result := nil;
  maxSize := Max(Length(set1), Length(set2));
  SetLength(Result, maxSize);

  for i := 0 to maxSize - 1 do
  begin
    Result[i] := [];
    if (i < Length(set1)) and (i < Length(set2)) then
      Result[i] := set1[i] * set2[i]
    else
      Result[i] := [];
  end;
end;



function inSet(bSet: TLongSet; e: integer): boolean;
var
  index, elementIndex: integer;
begin
  Result := False; 
  index := e div 256;   
  elementIndex := e mod 256; 
  if (index < Length(bSet)) then
    Result := elementIndex in bSet[index]; 
end;


procedure includeSet(var dstSet: TLongSet; e: integer);
var
  index, elementIndex: integer;
begin
  index := e div 256;      
  elementIndex := e mod 256; 
  if (index < Length(dstSet)) then
    Include(dstSet[index], elementIndex);
end;

procedure excludeSet(var dstSet: TLongSet; e: integer);
var
  index, elementIndex: integer;
begin
  index := e div 256;     
  elementIndex := e mod 256; 
  if (index < Length(dstSet)) then
    Exclude(dstSet[index], elementIndex); 
end;

function symDiffSet(set1, set2: TLongSet): TLongSet;
var
  maxSize, i: integer;
begin
  Result := nil;
  maxSize := Max(Length(set1), Length(set2)); 
  SetLength(Result, maxSize);          

  for i := 0 to maxSize - 1 do
  begin
    Result[i] := [];
    if i < Length(set1) then
      Result[i] := Result[i] + set1[i]; 

    if i < Length(set2) then
      Result[i] := Result[i] + set2[i]; 
  end;
  for i := 0 to maxSize - 1 do
  begin
    if (i < Length(set1)) and (i < Length(set2)) then
      Result[i] := Result[i] - (set1[i] * set2[i]);
  end;
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
  { Создание множества }
  set1 := createSet(300); { Создаёт множество, способное содержать элементы от 0 до 299 }

  { Добавление элементов }
  includeSet(set1, 5);    { Добавляем элемент 5 }
  includeSet(set1, 290);  { Добавляем элемент 290 }
  setSize(set1, 300);
  set2 := createSet(0);
  includeSet(set2, 290);
  includeSet(set2, 100);
  includeSet(set2, 255);
  includeSet(set2, 256);

  { print }
  includeSet(set1, 289);
  writeln('set1: ');
  printSet(set1);
  writeln('set2: ');
  printSet(set2);

  { Разность множеств }
  resultSet := subSet(set1, set2); { Разность set1 - set2 }
  writeln('Элементы множества после разности set1 - set2:');
  printSet(resultSet);
  resultSet := subSet(set2, set1); { Разность set2 - set1 }
  writeln('Элементы множества после разности set2 - set1:');
  printSet(resultSet);
  write('size: ');
  writeln(getSize(resultSet));
  write('length: ');
  writeln(Length(resultSet));
end.
