program NumberDigits;

type
  TIntegerEnumerator = record
  private
    FNumber: Integer;
    FCurrentDigit: Integer;
  public
    constructor Create(AValue: Integer);
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

  TVariantEnumerator = record
  private
    FVariantValue: Variant;
    FEnumerator: TIntegerEnumerator;
  public
    constructor Create(AValue: Variant);
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

{ TIntegerEnumerator }

constructor TIntegerEnumerator.Create(AValue: Integer);
begin
  FNumber := AValue;
  FCurrentDigit := AValue mod 10;
end;

function TIntegerEnumerator.GetCurrent: Integer;
begin
  Result := FCurrentDigit;
end;

function TIntegerEnumerator.MoveNext: Boolean;
begin
  FNumber := FNumber div 10;
  if FNumber = 0 then
    Exit(False);
  FCurrentDigit := FNumber mod 10;
  Result := True;
end;

{ TVariantEnumerator }

constructor TVariantEnumerator.Create(AValue: Variant);
begin
  FVariantValue := AValue;
  FEnumerator := TIntegerEnumerator.Create(FVariantValue);
end;

function TVariantEnumerator.GetCurrent: Integer;
begin
  Result := FEnumerator.Current;
end;

function TVariantEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

{ Main program }

var
  c: Integer;
  v: Variant;
  enumerator: TVariantEnumerator;
begin
  // Пример с целым числом
  for c in TIntegerEnumerator.Create(123456) do
    Write(c:2);
  Writeln;

  // Пример с variant
  v := 123456;
  enumerator := TVariantEnumerator.Create(v);
  while enumerator.MoveNext do
    Write(enumerator.Current:2);
  Writeln;
end.
