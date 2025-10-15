program Main;

uses
  funcs, SysUtils;

procedure processGetMax;
var
  input: string;
  values: array[1..5] of Integer;
  i, count: Integer;
begin
  writeln('Введите от 2 до 5 целых чисел через пробел:');
  readln(input);
  count := 0;
  
  // Разделение строки на числа
  for i := 1 to 5 do
    if input <> '' then
    begin
      if Pos(' ', input) > 0 then
      begin
        values[i] := StrToInt(Copy(input, 1, Pos(' ', input) - 1));
        Delete(input, 1, Pos(' ', input));
      end
      else
      begin
        values[i] := StrToInt(input);
        input := '';
      end;
      Inc(count);
    end;

  // Вызов функции getMax
  case count of
    2: writeln('Максимум: ', getMax(values[1], values[2]));
    3: writeln('Максимум: ', getMax(values[1], values[2], values[3]));
    4: writeln('Максимум: ', getMax(values[1], values[2], values[3], values[4]));
    5: writeln('Максимум: ', getMax(values[1], values[2], values[3], values[4], values[5]));
  else
    writeln('Ошибка: необходимо ввести от 2 до 5 чисел.');
  end;
end;

procedure processGetType;
var
  input: string;
  intValue: LongInt = 0;
  realValue: Real = 0.0;
  boolValue: Boolean = False;
begin
  writeln('Введите значение (целое число, вещественное число, строка или логическое 0/1):');
  readln(input);

  // Определение типа введённого значения
  if TryStrToInt(input, intValue) then
  begin
    writeln('Тип данных: ', getType(intValue));
  end
  else if TryStrToFloat(input, realValue) then
  begin
    writeln('Тип данных: ', getType(realValue));
  end
  else if (input = 'True') or (input = 'False') then
  begin
    boolValue := input = '1';
    writeln('Тип данных: ', getType(boolValue));
  end
  else
  begin
    writeln('Тип данных: ', getType(input));
  end;
end;

var
  intPart: Integer;
  fracPart: Real;
  digits, spaces, others: Integer;
  inputStr: string;
begin
  if validatedAll then
  begin
    writeln('Модуль funcs успешно прошел тестирование.');
    writeln;

    processGetMax;
    writeln;

    processGetType;
    Writeln;

    Writeln('Введите вещественное число для проверки getIntFrac:');
    Readln(fracPart);
    getIntFrac(fracPart, intPart, fracPart);
    Writeln('Целая часть: ', intPart);
    Writeln('Дробная часть: ', fracPart:0:6);
    Writeln;
    
    Writeln('Введите строку для проверки getStrChr:');
    Readln(inputStr);
    getStrChr(inputStr, digits, spaces, others);
    Writeln('Цифры: ', digits);
    Writeln('Пробелы: ', spaces);
    Writeln('Остальные символы: ', others);
    Writeln;
  end
  else
  begin
    writeln('Ошибка: модуль funcs не прошел тестирование.');
    Writeln;
  end;
end.
