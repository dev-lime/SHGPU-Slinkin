#Основная программа

program Main;

uses funcs;

var
  intPart: Integer;
  fracPart: Real;
  digits, spaces, others: Integer;
  inputStr: string;

begin
  if not validatedAll then
  begin
    Writeln('Модуль funcs содержит ошибки.');
    Halt(1);
  end;

  Writeln('Модуль funcs успешно прошел тестирование.');
  Writeln;

  // Демонстрация работы функций
  Writeln('Демонстрация getMax:');
  Writeln('Max(1, 2) = ', getMax(1, 2));
  Writeln('Max(1, 2, 3) = ', getMax(1, 2, 3));
  Writeln('Max(1, 2, 3, 4) = ', getMax(1, 2, 3, 4));
  Writeln('Max(1, 2, 3, 4, 5) = ', getMax(1, 2, 3, 4, 5));
  Writeln;

  Writeln('Демонстрация getType:');
  Writeln('getType(42): ', getType(42));
  Writeln('getType(3.14): ', getType(3.14));
  Writeln('getType("text"): ', getType('text'));
  Writeln('getType(True): ', getType(True));
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
end.
