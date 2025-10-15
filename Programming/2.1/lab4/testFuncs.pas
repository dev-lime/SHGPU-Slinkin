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
                 (getMax(1, 2, 3, 4, 5) = 5) and
                 (getMax(1, 2) = 2) and
                 (getMax(3, 2, 1) = 3) and
                 (getMax(4, 3, 2, 1) = 4) and
                 (getMax(5, 4, 3, 2, 1) = 5) and
                 (getMax(2, 3, 1) = 3) and
                 (getMax(3, 1, 4, 2) = 4) and
                 (getMax(3, 5, 2, 1, 4) = 5);
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
  t1, t2, t3, t4: boolean;
begin
  getStrChr('', digits, spaces, others);
  t1 := (digits = 0) and (spaces = 0) and (others = 0);
  getStrChr('123 abc!', digits, spaces, others);
  t2 := (digits = 3) and (spaces = 1) and (others = 4);
  getStrChr('12345', digits, spaces, others);
  t3 := (digits = 5) and (spaces = 0) and (others = 0);
  getStrChr('abc', digits, spaces, others);
  t4 := (digits = 0) and (spaces = 0) and (others = 3);
  test_getStrChr := t1 and t2 and t3 and t4;
end;

initialization
  successFuncs := test_getMax and test_getType and test_getIntFrac and test_getStrChr;

end.
