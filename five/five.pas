program VariantOperators;

uses
  SysUtils, Variants;

type
  TCustomVariant = record
    Value: Variant;
  end;

{ Прямое преобразование типов }
function StringToDouble(const S: string): Double;
begin
  if TryStrToFloat(S, Result) then
    Exit
  else
    Result := 0.0;
end;

function DoubleToString(const D: Double): string;
begin
  Result := FormatFloat('0.000000', D);
end;

{ Перегрузка операторов }
operator :=(const S: string): Double;
begin
  Result := StringToDouble(S);
end;

operator :=(const D: Double): string;
begin
  Result := DoubleToString(D);
end;

operator :=(const V: TCustomVariant): Double;
begin
  if VarIsFloat(V.Value) then
    Result := V.Value
  else if VarIsStr(V.Value) then
    Result := StringToDouble(V.Value)
  else
    Result := 0.0;
end;

operator :=(const V: TCustomVariant): string;
begin
  if VarIsStr(V.Value) then
    Result := V.Value
  else if VarIsFloat(V.Value) then
    Result := DoubleToString(V.Value)
  else
    Result := '';
end;

operator +(const A, B: TCustomVariant): Double;
begin
  Result := Double(A) + Double(B);
end;

operator -(const A, B: TCustomVariant): Double;
begin
  Result := Double(A) - Double(B);
end;

operator *(const A, B: TCustomVariant): Double;
begin
  Result := Double(A) * Double(B);
end;

operator /(const A, B: TCustomVariant): Double;
begin
  if Double(B) = 0.0 then
    raise EZeroDivide.Create('Division by zero');
  Result := Double(A) / Double(B);
end;

{ Перечислитель для целого числа }
type
  TIntEnumerator = class
  private
    FNumber: Integer;
    FCurrentDigit: Integer;
  public
    constructor Create(Num: Integer);
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

constructor TIntEnumerator.Create(Num: Integer);
begin
  FNumber := Abs(Num); // Работаем с абсолютным значением
  FCurrentDigit := -1;
end;

function TIntEnumerator.GetCurrent: Integer;
begin
  Result := FCurrentDigit;
end;

function TIntEnumerator.MoveNext: Boolean;
begin
  if FNumber > 0 then
  begin
    FCurrentDigit := FNumber mod 10;
    FNumber := FNumber div 10;
    Result := True;
  end
  else
    Result := False;
end;

function GetEnumerator(Num: Integer): TIntEnumerator;
begin
  Result := TIntEnumerator.Create(Num);
end;

{ Перечислитель для variant }
type
  TVariantEnumerator = class
  private
    FInnerEnumerator: TIntEnumerator;
  public
    constructor Create(V: Variant);
    destructor Destroy; override;
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

constructor TVariantEnumerator.Create(V: Variant);
begin
  if VarIsOrdinal(V) then
    FInnerEnumerator := TIntEnumerator.Create(V)
  else
    raise Exception.Create('Variant does not contain an integer');
end;

destructor TVariantEnumerator.Destroy;
begin
  FInnerEnumerator.Free;
  inherited;
end;

function TVariantEnumerator.GetCurrent: Integer;
begin
  Result := FInnerEnumerator.Current;
end;

function TVariantEnumerator.MoveNext: Boolean;
begin
  Result := FInnerEnumerator.MoveNext;
end;

function GetEnumerator(V: Variant): TVariantEnumerator;
begin
  Result := TVariantEnumerator.Create(V);
end;

{ Пример использования }
var
  c: Integer;
  v: TCustomVariant;
begin
  { Демонстрация перечислителей }
  for c in 123456 do
    Write(c:2);
  Writeln;

  v.Value := 123456;
  for c in v do
    Write(c:2);
  Writeln;

  { Демонстрация арифметики }
  v.Value := '12.34';
  Writeln(Double(v) + 10:0:6);
end.
