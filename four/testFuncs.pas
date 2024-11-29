unit testFuncs;

interface

var
  successFuncs: Boolean;

function test_getMax: Boolean;
function test_getType: Boolean;
function test_getIntFrac: Boolean;
function test_getStrChr: Boolean;

implementation

uses funcs;

function test_getMax: Boolean;
begin
  test_getMax := (getMax(1, 2) = 2) and
                 (getMax(1, 2, 3) = 3) and
                 (getMax(1, 2, 3, 4) = 4) and
                 (getMax(1, 2, 3, 4, 5) = 5);
end;

function test_getType: Boolean;
begin
  test_getType := (getType(42) = 'integer') and
                  (getType(3.14) = 'real') and
                  (getType('text') = 'string') and
                  (getType(True) = 'boolean');
end;

function test_getIntFrac: Boolean;
var
  intPart: Integer;
  fracPart: Real;
begin
  getIntFrac(3.14, intPart, fracPart);
  test_getIntFrac := (intPart = 3) and (Abs(fracPart - 0.14) < 1E-6);
end;

function test_getStrChr: Boolean;
var
  digits, spaces, others: Integer;
begin
  getStrChr('123 abc!', digits, spaces, others);
  test_getStrChr := (digits = 3) and (spaces = 1) and (others = 4);
end;

initialization
  successFuncs := test_getMax and test_getType and test_getIntFrac and test_getStrChr;

end.
