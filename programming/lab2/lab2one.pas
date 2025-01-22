{$R+}
uses math;

type
  TSingleSet = set of byte;
  TLongSet = array of TSingleSet;

{ Функция создания множества }
function createSet(count: integer): TLongSet;
var
  setSize: integer;
begin
  createSet := nil;
  { Размер множества всегда кратен 256 }
  setSize := ((count + 255) div 256);
  SetLength(createSet, setSize);
end;

{ Функция изменения размера множества }
procedure setSize(var dstSet: TLongSet; newCount: integer);
var
  setSize, i: integer;
begin
  setSize := (newCount + 255) div 256;
  if setSize > Length(dstSet) then
  begin
    SetLength(dstSet, setSize);
    for i := Length(dstSet) to setSize - 1 do
      dstSet[i] := [];
  end
  else
    SetLength(dstSet, setSize);
end;

{ Функция получения размера множества }
function getSize(bSet: TLongSet): integer;
var
  i, count: integer;
  element: byte;
begin
  count := 0;
  for i := 0 to High(bSet) do
    for element in bSet[i] do
      Inc(count);
  getSize := count * 256;
end;

{ Функция очистки множества }
procedure setClean(var mySet: TLongSet; newLen: integer);
var
  i: integer;
begin
  SetLength(mySet, newLen);
  for i := 0 to newLen - 1 do
    mySet[i] := [];
end;

{ Функция уничтожения множества }
procedure destroySet(var dstSet: TLongSet);
begin
  if Length(dstSet) > 0 then
    SetLength(dstSet, 0);
end;

{ Аналог операции in }
function inSet(bSet: TLongSet; e: integer): boolean;
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  inSet := (index < Length(bSet)) and (byte(bitPos) in bSet[index]);
end;

{ Аналог функции include (добавление элемента) }
procedure includeSet(var dstSet: TLongSet; e: integer);
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  if index >= Length(dstSet) then
    SetLength(dstSet, index + 1);
  Include(dstSet[index], byte(bitPos));
end;

{ Аналог операции + (объединение), возвращает новое множество }
function sumSet(set1, set2: TLongSet): TLongSet;
var
  i, maxLen: integer;
  resSet: TLongSet;
begin
  maxLen := Max(Length(set1), Length(set2));
  SetLength(resSet, maxLen);

  for i := 0 to maxLen - 1 do
  begin
    if i < Length(set1) then
      resSet[i] := set1[i];
    if i < Length(set2) then
      resSet[i] := resSet[i] + set2[i];
  end;

  sumSet := resSet;
end;

{ Аналог операции - (разность), возвращает новое множество }
function subSet(set1, set2: TLongSet): TLongSet;
var
  i: integer;
  resSet: TLongSet;
begin
  SetLength(resSet, Length(set1));

  for i := 0 to High(set1) do
  begin
    resSet[i] := set1[i];
    if i < Length(set2) then
      resSet[i] := resSet[i] - set2[i];
  end;

  subSet := resSet;
end;

{ Аналог операции * (пересечение), возвращает новое множество }
function mulSet(set1, set2: TLongSet): TLongSet;
var
  i, minLen: integer;
  resSet: TLongSet;
begin
  minLen := Min(Length(set1), Length(set2));
  SetLength(resSet, minLen);

  for i := 0 to minLen - 1 do
    resSet[i] := set1[i] * set2[i];

  mulSet := resSet;
end;

{ Аналог функции exclude (удаление элемента) }
procedure excludeSet(var dstSet: TLongSet; e: integer);
var
  index, bitPos: integer;
begin
  index := e div 256;
  bitPos := e mod 256;
  if index < Length(dstSet) then
    Exclude(dstSet[index], byte(bitPos));
end;

{ Симметричная разность (объединение минус пересечение) }
function symDiffSet(set1, set2: TLongSet): TLongSet;
var
  i, maxLen: integer;
  resSet: TLongSet;
begin
  maxLen := Max(Length(set1), Length(set2));
  SetLength(resSet, maxLen);

  for i := 0 to maxLen - 1 do
  begin
    if i < Length(set1) then
      resSet[i] := set1[i];
    if i < Length(set2) then
      resSet[i] := resSet[i] >< set2[i];
  end;

  symDiffSet := resSet;
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
  i: integer;

begin
  { Создание множества }
  set1 := createSet(300); { Создаёт множество, способное содержать элементы от 0 до 299 }

  { Добавление элементов }
  includeSet(set1, 5);    { Добавляем элемент 5 }
  includeSet(set1, 150);  { Добавляем элемент 150 }
  includeSet(set1, 290);  { Добавляем элемент 290 }

  { Проверка наличия элемента }
  if inSet(set1, 150) then
    writeln('Элемент 150 есть в множестве set1.')
  else
    writeln('Элемента 150 нет в множестве set1.');

  if inSet(set1, 10) then
    writeln('Элемент 10 есть в множестве set1.')
  else
    writeln('Элемента 10 нет в множестве set1.');

  { Удаление элемента }
  excludeSet(set1, 150); { Удаляем элемент 150 }
  if not inSet(set1, 150) then
    writeln('Элемент 150 успешно удалён из множества set1.');

  { Изменение размера множества }
  setSize(set1, 300); { Изменяем размер множества на 500 элементов }

  { Создание второго множества и объединение }
  set2 := createSet(0);
  includeSet(set2, 290);
  //includeSet(set2, 100);
  //includeSet(set2, 255);
  //includeSet(set2, 256);
  resultSet := sumSet(set1, set2); { Объединение set1 и set2 }
  writeln('Элементы множества после объединения:');
  printSet(resultSet);

  { Пересечение множеств }
  resultSet := mulSet(set1, set2); { Пересечение set1 и set2 }
  printSet(resultSet);

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
  writeln(getSize(resultSet));

  { Симметрическая разность }
  resultSet := symDiffSet(set1, set2); { Симметрическая разность set1 и set2 }
  writeln('Элементы множества после симметрической разности:');
  printSet(resultSet);

  { Уничтожение множества }
  destroySet(set1);
  destroySet(set2);
  destroySet(resultSet);

  writeln('Множества уничтожены.');
end.
