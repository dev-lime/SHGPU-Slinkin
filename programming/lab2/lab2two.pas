{$R+}
uses sysutils;

type
  TLongSet = bitpacked array[0..High(LongInt)] of boolean;
  PLongSet = ^TLongSet;

{ Создание множества на count элементов }
function createSet(count: integer): PLongSet;
var
  size: integer;
begin
  size := (count + 7) div 8; { Размер в байтах }
  GetMem(Result, size);
  FillChar(Result^, size, 0);
end;

{ Изменение размера множества }
procedure setSize(var dstSet: PLongSet; newCount: integer);
var
  newSize: integer;
begin
  newSize := (newCount + 7) div 8; { Новый размер в байтах }
  ReallocMem(dstSet, newSize);
end;

{ Получение размера множества }
function getSize(bSet: PLongSet): integer;
begin
  Result := MemSize(bSet) * 8; { Размер в битах }
end;

{ Уничтожение множества }
procedure destroySet(var dstSet: PLongSet);
begin
  if dstSet <> nil then
  begin
    FreeMem(dstSet);
    dstSet := nil;
  end;
end;

{ Проверка элемента в множестве }
function inSet(bSet: PLongSet; e: integer): boolean;
var
  byteIndex, bitPos: integer;
begin
  byteIndex := e div 8;
  bitPos := e mod 8;
  if byteIndex < MemSize(bSet) then
    Result := (PByte(bSet)[byteIndex] and (1 shl bitPos)) <> 0
  else
    Result := False;
end;

{ Добавление элемента в множество }
procedure includeSet(var dstSet: PLongSet; e: integer);
var
  byteIndex, bitPos, currentSize: integer;
begin
  byteIndex := e div 8;
  bitPos := e mod 8;
  currentSize := MemSize(dstSet);
  if byteIndex >= currentSize then
    setSize(dstSet, (byteIndex + 1) * 8);
  PByte(dstSet)[byteIndex] := PByte(dstSet)[byteIndex] or (1 shl bitPos);
end;

{ Удаление элемента из множества }
procedure excludeSet(var dstSet: PLongSet; e: integer);
var
  byteIndex, bitPos: integer;
begin
  byteIndex := e div 8;
  bitPos := e mod 8;
  if byteIndex < MemSize(dstSet) then
    PByte(dstSet)[byteIndex] := PByte(dstSet)[byteIndex] and not (1 shl bitPos);
end;

{ Объединение двух множеств }
function sumSet(set1, set2: PLongSet): PLongSet;
var
  maxSize, i: integer;
begin
  maxSize := Max(MemSize(set1), MemSize(set2));
  Result := createSet(maxSize * 8);
  for i := 0 to maxSize - 1 do
    PByte(Result)[i] := PByte(set1)[i] or PByte(set2)[i];
end;

{ Разность двух множеств }
function subSet(set1, set2: PLongSet): PLongSet;
var
  maxSize, i: integer;
begin
  maxSize := MemSize(set1);
  Result := createSet(maxSize * 8);
  for i := 0 to maxSize - 1 do
    PByte(Result)[i] := PByte(set1)[i] and not PByte(set2)[i];
end;

{ Пересечение двух множеств }
function mulSet(set1, set2: PLongSet): PLongSet;
var
  minSize, i: integer;
begin
  minSize := Min(MemSize(set1), MemSize(set2));
  Result := createSet(minSize * 8);
  for i := 0 to minSize - 1 do
    PByte(Result)[i] := PByte(set1)[i] and PByte(set2)[i];
end;

{ Симметрическая разность }
function symDiffSet(set1, set2: PLongSet): PLongSet;
var
  maxSize, i: integer;
begin
  maxSize := Max(MemSize(set1), MemSize(set2));
  Result := createSet(maxSize * 8);
  for i := 0 to maxSize - 1 do
    PByte(Result)[i] := PByte(set1)[i] xor PByte(set2)[i];
end;

{ Проверка работы функций }
var
  set1, set2, result: PLongSet;
begin
  set1 := createSet(64);
  set2 := createSet(64);

  { Добавляем элементы }
  includeSet(set1, 5);
  includeSet(set1, 20);
  includeSet(set2, 20);
  includeSet(set2, 40);

  writeln('Элемент 20 в set1: ', inSet(set1, 20));
  writeln('Элемент 40 в set1: ', inSet(set1, 40));

  { Объединение }
  result := sumSet(set1, set2);
  writeln('Объединение содержит элемент 40: ', inSet(result, 40));
  destroySet(result);

  { Разность }
  result := subSet(set1, set2);
  writeln('Разность содержит элемент 5: ', inSet(result, 5));
  destroySet(result);

  { Пересечение }
  result := mulSet(set1, set2);
  writeln('Пересечение содержит элемент 20: ', inSet(result, 20));
  destroySet(result);

  { Симметрическая разность }
  result := symDiffSet(set1, set2);
  writeln('Симметрическая разность содержит элемент 40: ', inSet(result, 40));
  writeln('Симметрическая разность содержит элемент 5: ', inSet(result, 5));
  destroySet(result);

  { Уничтожение множеств }
  destroySet(set1);
  destroySet(set2);
end.
