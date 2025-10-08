{

Лабораторная 1. Простые типы данных в оперативной памяти
Разработать консольную программу на языке FreePascal, получающую на вводе строку,
значение которой может быть равным одному из следующих вариантов:
Shortint SmallInt Longint Longword Int64 Byte Word Cardinal QWord Char Boolean.

Если введенная строка не соответствует ни одному из приведенных вариантов,
вывести на экран слово Error и завершить работу программы.

Для введенного типа данных вывести на экран (1) нижную границу типа, (2) верхнюю границу типа,
(3) размер типа в байтах. Заполнить переменную введенного типа случайным значением
из диапазона от нижней до верхней границы типа включительно. Вывести на экран (4) полученное значение,
(5) предыдущее и (6) последующее значение. Вывести на экран (7) значения байтов, хранящих переменную введенного типа,
в порядке их расположения в оперативной памяти. Если полученное случайное значение является максимальным для данного типа,
то вместо последующего (6) значения вывести слово Overflow.
Если полученное случайное значение является минимальным для данного типа,
то вместо предыдущего (5) значения также вывести слово Overflow.

Запрещается использовать константные значения характеристик типа. Значения всех характеристик типа следует вычислять,
используя специальные функции для работы с порядковым типом данных (low, high, pred, succ и т.д.).

Пример вывода для типа Longint:

Исследуемый тип: Longint
Нижняя граница: -2147483648
Верхняя граница: 2147483647
Байт на переменную: 4
Случайное значение: 818295054
Предыдущее значение: 818295053
Последующее значение: 818295055
Содержимое оперативной памяти: 14 49 198 48
Получение случайных значений с использованием функции Random ограничено неотрицательным диапазоном типа Longint.
Поэтому для получения случайных значений из всего диапазона целочисленных типов данных, а также типов,
диапазон которых в положительной области превышает диапазон для типа Longint,
рекомендуется получать случайное значение для каждого байта целочисленного типа и
сохранять полученный результатв соответсвующей области памяти.

Значения типа Char могут не иметь корректного визуального представления,
поэтому их рекомендуется выводить в виде ASCII-кода. Например, вместо латинской буквы 'A' следует выводить значение #65:

Исследуемый тип: Char
Нижняя граница: #0
Верхняя граница: #255
Байт на переменную: 1
Случайное значение: #65
Предыдущее значение: #64
Последующее значение: #66
Содержимое оперативной памяти: 65

Для уменьшения копипастинга рекомендуется разработать шаблон (дженерик) класса, решающий поставленную задачу и специализировать его для всех используемых типов данных.

}

program Lab1;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TypInfo;

type
  generic TTypeInfo<T> = class
  private
    FValue: T;
  public
    procedure PrintTypeInfo;
    procedure GenerateRandomValue;
    procedure PrintValueInfo;
    procedure PrintMemoryContent;
  end;

  // Специализированный generic для Char
  generic TCharTypeInfo = class(specialize TTypeInfo<Char>)
  public
    procedure PrintTypeInfo;
    procedure PrintValueInfo;
  end;

  // Специализированный generic для Boolean
  generic TBooleanTypeInfo = class(specialize TTypeInfo<Boolean>)
  public
    procedure PrintTypeInfo;
    procedure PrintValueInfo;
  end;

  TShortIntInfo = specialize TTypeInfo<Shortint>;
  TSmallIntInfo = specialize TTypeInfo<SmallInt>;
  TLongintInfo = specialize TTypeInfo<Longint>;
  TLongwordInfo = specialize TTypeInfo<Longword>;
  TInt64Info = specialize TTypeInfo<Int64>;
  TByteInfo = specialize TTypeInfo<Byte>;
  TWordInfo = specialize TTypeInfo<Word>;
  TCardinalInfo = specialize TTypeInfo<Cardinal>;
  TQWordInfo = specialize TTypeInfo<QWord>;
  TCharInfo = specialize TCharTypeInfo;
  TBooleanInfo = specialize TBooleanTypeInfo;

procedure TTypeInfo.PrintTypeInfo;
begin
  Writeln('Исследуемый тип: ', PTypeInfo(TypeInfo(T))^.Name);
  Writeln('Нижняя граница: ', Low(T));
  Writeln('Верхняя граница: ', High(T));
  Writeln('Байт на переменную: ', SizeOf(T));
end;

procedure TTypeInfo.GenerateRandomValue;
var
  Bytes: array of Byte;
  I: Integer;
begin
  SetLength(Bytes, SizeOf(T));
  for I := 0 to High(Bytes) do
    Bytes[I] := Random(256);

  Move(Bytes[0], FValue, SizeOf(T));
end;

procedure TTypeInfo.PrintValueInfo;
var
  IsMin, IsMax: Boolean;
begin
  Write('Случайное значение: ');
  Writeln(FValue);

  IsMin := FValue = Low(T);
  IsMax := FValue = High(T);

  Write('Предыдущее значение: ');
  if IsMin then
    Writeln('Overflow')
  else
    Writeln(Pred(FValue));

  Write('Последующее значение: ');
  if IsMax then
    Writeln('Overflow')
  else
    Writeln(Succ(FValue));
end;

procedure TTypeInfo.PrintMemoryContent;
var
  Bytes: array of Byte;
  I: Integer;
begin
  Write('Содержимое оперативной памяти: ');
  SetLength(Bytes, SizeOf(T));
  Move(FValue, Bytes[0], SizeOf(T));

  for I := 0 to High(Bytes) do
  begin
    Write(Bytes[I]);
    if I < High(Bytes) then
      Write(' ');
  end;
  Writeln;
end;

// Специализированные методы для Char
procedure TCharTypeInfo.PrintTypeInfo;
begin
  Writeln('Исследуемый тип: Char');
  Writeln('Нижняя граница: #', Ord(Low(Char)));
  Writeln('Верхняя граница: #', Ord(High(Char)));
  Writeln('Байт на переменную: ', SizeOf(Char));
end;

procedure TCharTypeInfo.PrintValueInfo;
var
  IsMin, IsMax: Boolean;
begin
  Write('Случайное значение: #', Ord(FValue));
  Writeln;

  IsMin := FValue = Low(Char);
  IsMax := FValue = High(Char);

  Write('Предыдущее значение: ');
  if IsMin then
    Writeln('Overflow')
  else
    Writeln('#', Ord(Pred(FValue)));

  Write('Последующее значение: ');
  if IsMax then
    Writeln('Overflow')
  else
    Writeln('#', Ord(Succ(FValue)));
end;

// Специализированные методы для Boolean
procedure TBooleanTypeInfo.PrintTypeInfo;
begin
  Writeln('Исследуемый тип: Boolean');
  Writeln('Нижняя граница: ', Low(Boolean));
  Writeln('Верхняя граница: ', High(Boolean));
  Writeln('Байт на переменную: ', SizeOf(Boolean));
end;

procedure TBooleanTypeInfo.PrintValueInfo;
var
  IsMin, IsMax: Boolean;
begin
  Write('Случайное значение: ', FValue);
  Writeln;

  IsMin := FValue = Low(Boolean);
  IsMax := FValue = High(Boolean);

  Write('Предыдущее значение: ');
  if IsMin then
    Writeln('Overflow')
  else
    Writeln(Pred(FValue));

  Write('Последующее значение: ');
  if IsMax then
    Writeln('Overflow')
  else
    Writeln(Succ(FValue));
end;

var
  InputType: string;
  RandomSeed: Longword;

begin
  Randomize;
  RandomSeed := Random(MaxInt);
  RandSeed := RandomSeed;

  Write('Введите тип данных: ');
  Readln(InputType);

  if InputType = 'Shortint' then
  begin
    with TShortIntInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'SmallInt' then
  begin
    with TSmallIntInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Longint' then
  begin
    with TLongintInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Longword' then
  begin
    with TLongwordInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Int64' then
  begin
    with TInt64Info.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Byte' then
  begin
    with TByteInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Word' then
  begin
    with TWordInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Cardinal' then
  begin
    with TCardinalInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'QWord' then
  begin
    with TQWordInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Char' then
  begin
    with TCharInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else if InputType = 'Boolean' then
  begin
    with TBooleanInfo.Create do
    try
      PrintTypeInfo;
      GenerateRandomValue;
      PrintValueInfo;
      PrintMemoryContent;
    finally
      Free;
    end;
  end
  else
  begin
    Writeln('Error');
  end;
end.

