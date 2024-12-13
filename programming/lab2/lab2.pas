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
  setSize: integer;
begin
  { Размер множества всегда кратен 256 }
  setSize := ((newCount + 255) div 256);
  SetLength(dstSet, setSize);
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
  if index < Length(bSet) then
    inSet := byte(bitPos) in bSet[index]
  else
    inSet := False;
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
  i, len: integer;
begin
  sumSet := nil;
  len := Max(Length(set1), Length(set2));
  SetLength(sumSet, len);
  for i := 0 to len - 1 do
  begin
    if i < Length(set1) then
      sumSet[i] := set1[i];
    if i < Length(set2) then
      sumSet[i] := sumSet[i] + set2[i];
  end;
end;

{ Аналог операции - (разность), возвращает новое множество }
function subSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  subSet := nil;
  len := Max(Length(set1), Length(set2));
  SetLength(subSet, len);
  for i := 0 to Length(subSet)-1 do // очистка
    subSet[i] := [];
  for i := 0 to len - 1 do
  begin
    if i < Length(set1) then
    begin
      subSet[i] := set1[i];
    end;
    if i < Length(set2) then
      subSet[i] := subSet[i] - set2[i];
  end;
end;

{ Аналог операции * (пересечение), возвращает новое множество }
function mulSet(set1, set2: TLongSet): TLongSet;
var
  i, len: integer;
begin
  mulSet := nil;
  len := Max(Length(set1), Length(set2));
  SetLength(mulSet, len);
  for i := 0 to len - 1 do
  begin
    if (i < Length(set1)) and (i < Length(set2)) then
      mulSet[i] := set1[i] * set2[i];
  end;
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
  i, len: integer;
begin
  symDiffSet := nil;
  len := Max(Length(set1), Length(set2));
  SetLength(symDiffSet, len);
  for i := 0 to len - 1 do
  begin
    if (i < Length(set1)) and (i < Length(set2)) then
      symDiffSet[i] := (set1[i] + set2[i]) - (set1[i] * set2[i])
    else if i < Length(set1) then
      symDiffSet[i] := set1[i]
    else
      symDiffSet[i] := set2[i];
  end;
end;

{ Вывод множества }
procedure printSet(printSet: TLongSet);
var
  i: integer;
begin
  for i := 0 to getSize(printSet) do
    if inSet(printSet, i) then
      write(i, ' ');
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
  set2 := createSet(1024);
  includeSet(set2, 290);
  includeSet(set2, 100);
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
