unit funcs;

interface

function getMax(a, b: Integer): Integer; overload;
function getMax(a, b, c: Integer): Integer; overload;
function getMax(a, b, c, d: Integer): Integer; overload;
function getMax(a, b, c, d, e: Integer): Integer; overload;

function getType(a: Integer): string; overload;
function getType(a: Real): string; overload;
function getType(a: string): string; overload;
function getType(a: Boolean): string; overload;

procedure getIntFrac(value: Real; var intPart: Integer; var fracPart: Real);
procedure getStrChr(const s: string; var digits, spaces, others: Integer);

function validatedAll: Boolean;

implementation

uses testFuncs;

function getMax(a, b: Integer): Integer;
begin
  if a > b then
    getMax := a
  else
    getMax := b;
end;

function getMax(a, b, c: Integer): Integer;
begin
  getMax := getMax(getMax(a, b), c);
end;

function getMax(a, b, c, d: Integer): Integer;
begin
  getMax := getMax(getMax(a, b, c), d);
end;

function getMax(a, b, c, d, e: Integer): Integer;
begin
  getMax := getMax(getMax(a, b, c, d), e);
end;

function getType(a: Integer): string;
begin
  getType := 'integer';
end;

function getType(a: Real): string;
begin
  getType := 'real';
end;

function getType(a: string): string;
begin
  getType := 'string';
end;

function getType(a: Boolean): string;
begin
  getType := 'boolean';
end;

procedure getIntFrac(value: Real; var intPart: Integer; var fracPart: Real);
begin
  intPart := Trunc(value);
  fracPart := Abs(Frac(value));
end;

procedure getStrChr(const s: string; var digits, spaces, others: Integer);
var
  i: Integer;
begin
  digits := 0;
  spaces := 0;
  others := 0;
  for i := 1 to Length(s) do
  begin
    if s[i] in ['0'..'9'] then
      Inc(digits)
    else if s[i] = ' ' then
      Inc(spaces)
    else
      Inc(others);
  end;
end;

function validatedAll: Boolean;
begin
  validatedAll := testFuncs.successFuncs;
end;

end.
