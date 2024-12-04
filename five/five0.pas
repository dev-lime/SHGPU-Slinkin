program OverloadedOperators;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants;

function StrToDbl(const S: string): Double;
begin
  if not TryStrToFloat(S, Result) then
    Result := 0.0;
end;

// Перегрузка оператора "+" для комбинаций string и double
operator + (A: string; B: Double) Res: Double;
begin
  Res := StrToDbl(A) + B;
end;

operator + (A: Double; B: string) Res: Double;
begin
  Res := A + StrToDbl(B);
end;

operator + (A: Variant; B: Variant) Res: Double;
begin
  Res := Double(A) + Double(B);
end;

// Перегрузка оператора "-" для комбинаций string и double
operator - (A: string; B: Double) Res: Double;
begin
  Res := StrToDbl(A) - B;
end;

operator - (A: Double; B: string) Res: Double;
begin
  Res := A - StrToDbl(B);
end;

operator - (A: Variant; B: string) Res: Double;
begin
  Res := StrToDbl(A) - StrToDbl(B);
end;

operator - (A: string; B: Variant) Res: Double;
begin
  Res := Double(A) - Double(B);
end;

// Перегрузка оператора "*" для комбинаций string и double
operator * (A: string; B: Double) Res: Double;
begin
  Res := StrToDbl(A) * B;
end;

operator * (A: Double; B: string) Res: Double;
begin
  Res := A * StrToDbl(B);
end;

operator * (A: Variant; B: string) Res: Double;
begin
  Res := StrToDbl(A) * StrToDbl(B);
end;

operator * (A: string; B: Variant) Res: Double;
begin
  Res := Double(A) * Double(B);
end;

// Перегрузка оператора "/" для комбинаций string и double
operator / (A: string; B: Double) Res: Double;
begin
  if B = 0.0 then
    raise Exception.Create('Division by zero');
  Res := StrToDbl(A) / B;
end;

operator / (A: Double; B: string) Res: Double;
begin
  if StrToDbl(B) = 0.0 then
    raise Exception.Create('Division by zero');
  Res := A / StrToDbl(B);
end;

operator / (A: string; B: string) Res: Double;
begin
  if StrToDbl(B) = 0.0 then
    raise Exception.Create('Division by zero');
  Res := StrToDbl(A) / StrToDbl(B);
end;

operator / (A: Variant; B: Variant) Res: Double;
begin
  if Double(B) = 0.0 then
    raise Exception.Create('Division by zero');
  Res := Double(A) / Double(B);
end;

var
  V1, V2: Variant;
  S1, S2: string;
  D1, D2: Double;
  Res: Double;
begin
  WriteLn('Сложение');
  ReadLn(S1);
  ReadLn(S2);
  WriteLn((S1+StrToDbl(S2)));
  WriteLn('Вычитание');
  V1 := 6.0;
  ReadLn(S2);
  WriteLn(V1-S2);
  WriteLn('Умножение');
  ReadLn(S1);
  V2 := 8.0;
  WriteLn(S1*V2);
  WriteLn('Деление');
  ReadLn(S1);
  D2 := 10.0;
  WriteLn(StrToDbl(S1)/D2);
end.
