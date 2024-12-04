type
  TSingleSet = set of byte;
  TLongSet = array of TSingleSet;

{ Функция создания множества }
function createSet(count: integer): TLongSet;
var
  setSize: integer;
begin
  { Размер множества всегда кратен 256 }
  setSize := ((count + 255) div 256) * 256;
  SetLength(createSet, setSize div 256);
end;

{ Функция изменения размера множества }
procedure setSize(var dstSet: TLongSet; newCount: integer);
var
  setSize: integer;
begin
  { Размер множества всегда кратен 256 }
  setSize := ((newCount + 255) div 256) * 256;
  SetLength(setSize, setSize div 256);
end;

{ Функция получения размера множества }
function getSize(bSet: TLongSet): integer;
begin
  getSize := Length(bSet) * 256;
end;

{ Функция уничтожения множества }
procedure destroySet(var dstSet: TLongSet);
begin
  SetLength(dstSet, 0);
end;

{ Аналог операции in }
function inSet(bSet: TLongSet; e: integer): boolean;
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  inSet := (bSet[index] and (1 shl bitPos)) <> 0;
end;

{ Аналог операции + (объединение), возвращает новое множество }
function sumSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  len := Max(Length(set1), Length(set2));
  SetLength(Result, len);
  for i := 0 to len - 1 do
    sumSet[i] := set1[i] or set2[i];
end;

{ Аналог операции - (разность), возвращает новое множество }
function subSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  len := Max(Length(set1), Length(set2));
  SetLength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := set1[i] and not set2[i];
end;

{ Аналог операции * (пересечение), возвращает новое множество }
function mulSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  len := Max(Length(set1), Length(set2));
  SetLength(Result, len);
  for i := 0 to len - 1 do
    mulSet[i] := set1[i] and set2[i];
end;

{ Аналог функции include (добавление элемента) }
procedure includeSet(var dstSet: TLongSet; e: integer);
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  dstSet[index] := dstSet[index] or (1 shl bitPos);
end;

{ Аналог функции exclude (удаление элемента) }
procedure excludeSet(var dstSet: TLongSet; e: integer);
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  dstSet[index] := dstSet[index] and not (1 shl bitPos);
end;

{ Симметричная разность (объединение минус пересечение) }
function symDiffSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  len := Max(Length(set1), Length(set2));
  SetLength(Result, len);
  for i := 0 to len - 1 do
    symDiffSet[i] := set1[i] xor set2[i];
end;
