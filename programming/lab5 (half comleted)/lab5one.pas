program OperatorOverloadingExample;

{$mode objfpc}
{$WARN 6058 OFF}

uses sysutils, variants;

// Перегрузка операторов для приведения типов и арифметических операций.

// Преобразование string в double
operator := (strValue: string): double;
begin
  result := StrToFloatDef(strValue, 0.0); // Если строка содержит число, преобразуем, иначе возвращаем 0.0
end;

// Преобразование double в string
operator := (numValue: double): string;
begin
  result := FloatToStrF(numValue, ffFixed, 6, 6); // Преобразуем число в строку с 6 знаками после запятой
end;

// Явное преобразование string в double
operator explicit (strValue: string): double;
begin
  result := StrToFloatDef(strValue, 0.0);
end;

// Явное преобразование double в string
operator explicit (numValue: double): string;
begin
  result := FloatToStrF(numValue, ffFixed, 0, 6); // Без экспоненциальной нотации
end;

// Перегрузка оператора сложения для разных типов
operator + (numValue: double; strValue: string): double;
begin
  result := numValue + StrToFloatDef(strValue, 0.0);
end;

operator + (strValue: string; numValue: double): double;
begin
  result := StrToFloatDef(strValue, 0.0) + numValue;
end;

operator + (varValue: variant; strValue: string): double;
begin
  result := StrToFloatDef(varValue, 0.0) + StrToFloatDef(strValue, 0.0);
end;

operator + (strValue: string; varValue: variant): double;
begin
  result := StrToFloatDef(strValue, 0.0) + StrToFloatDef(varValue, 0.0);
end;

operator + (varValue: variant; numValue: double): double;
begin
  result := StrToFloatDef(varValue, 0.0) + numValue;
end;

operator + (numValue: double; varValue: variant): double;
begin
  result := numValue + StrToFloatDef(varValue, 0.0);
end;

operator + (varValue1, varValue2: variant): double;
begin
  result := StrToFloatDef(varValue1, 0.0) + StrToFloatDef(varValue2, 0.0);
end;

// Перегрузка оператора вычитания
operator - (numValue: double; strValue: string): double;
begin
  result := numValue - StrToFloatDef(strValue, 0.0);
end;

operator - (strValue: string; numValue: double): double;
begin
  result := StrToFloatDef(strValue, 0.0) - numValue;
end;

operator - (varValue: variant; strValue: string): double;
begin
  result := StrToFloatDef(varValue, 0.0) - StrToFloatDef(strValue, 0.0);
end;

operator - (strValue: string; varValue: variant): double;
begin
  result := StrToFloatDef(strValue, 0.0) - StrToFloatDef(varValue, 0.0);
end;

operator - (varValue: variant; numValue: double): double;
begin
  result := StrToFloatDef(varValue, 0.0) - numValue;
end;

operator - (numValue: double; varValue: variant): double;
begin
  result := numValue - StrToFloatDef(varValue, 0.0);
end;

operator - (varValue1, varValue2: variant): double;
begin
  result := StrToFloatDef(varValue1, 0.0) - StrToFloatDef(varValue2, 0.0);
end;

// Перегрузка оператора умножения
operator * (numValue: double; strValue: string): double;
begin
  result := numValue * StrToFloatDef(strValue, 0.0);
end;

operator * (strValue: string; numValue: double): double;
begin
  result := StrToFloatDef(strValue, 0.0) * numValue;
end;

operator * (varValue: variant; strValue: string): double;
begin
  result := StrToFloatDef(varValue, 0.0) * StrToFloatDef(strValue, 0.0);
end;

operator * (strValue: string; varValue: variant): double;
begin
  result := StrToFloatDef(strValue, 0.0) * StrToFloatDef(varValue, 0.0);
end;

operator * (varValue: variant; numValue: double): double;
begin
  result := StrToFloatDef(varValue, 0.0) * numValue;
end;

operator * (numValue: double; varValue: variant): double;
begin
  result := numValue * StrToFloatDef(varValue, 0.0);
end;

operator * (varValue1, varValue2: variant): double;
begin
  result := StrToFloatDef(varValue1, 0.0) * StrToFloatDef(varValue2, 0.0);
end;

// Перегрузка оператора деления
operator / (numValue: double; strValue: string): double;
begin
  result := numValue / StrToFloatDef(strValue, 0.0);
end;

operator / (strValue: string; numValue: double): double;
begin
  result := StrToFloatDef(strValue, 0.0) / numValue;
end;

operator / (varValue: variant; strValue: string): double;
begin
  result := StrToFloatDef(varValue, 0.0) / StrToFloatDef(strValue, 0.0);
end;

operator / (strValue: string; varValue: variant): double;
begin
  result := StrToFloatDef(strValue, 0.0) / StrToFloatDef(varValue, 0.0);
end;

operator / (varValue: variant; numValue: double): double;
begin
  result := StrToFloatDef(varValue, 0.0) / numValue;
end;

operator / (numValue: double; varValue: variant): double;
begin
  result := numValue / StrToFloatDef(varValue, 0.0);
end;

operator / (varValue1, varValue2: variant): double;
begin
  result := StrToFloatDef(varValue1, 0.0) / StrToFloatDef(varValue2, 0.0);
end;

// Пример использования операторов
var
  st: string;
  d, d_else: double;
  vr: variant;
begin
  d := '3.0'; // Присваивание строкового значения числу
  d_else := 'wr'; // Некорректная строка, преобразуется в 0.0
  st := 8.0; // Преобразование числа в строку
  vr := '21.0'; // Присваивание строкового значения переменной variant

  // Тестирование перегрузок
  writeln('d - из строки в число: ', d:0:6);
  writeln('иначе в 0.0: ', d_else:0:6);
  writeln('st - из числа в строку: ', st);
  writeln;
  writeln('string(d) + string(d) = ', string(d) + string(d));
  writeln('double(st) + double(st) = ', double(st) + double(st):0:6);
  writeln('variants = ', double(vr) + double(vr):0:6);
  writeln;
end.
