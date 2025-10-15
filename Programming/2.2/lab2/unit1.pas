unit unit1;

{$mode ObjFPC}

interface
uses Classes, SysUtils;

type
    fcomp = function(a: integer): integer;
    vararr = array of integer;
    TAbstractFinder = class
      protected
      values: array of integer;
      comp: fcomp;
      public
        constructor Create(arr: array of integer);
        procedure ReplaceArray(arr: array of integer);
        destructor Destroy();override;
        procedure setCompare(c: fcomp);
        function isCorrect(): boolean; virtual; abstract;
        function findOne(): integer; virtual; abstract;
        function findOneS(e: integer): integer;
        function findOneCustomS(f: fcomp): integer;
        function findAll(): vararr; virtual; abstract;
        function findAllS(e: integer): vararr;
        function findAllCustomS(f: fcomp): vararr;
      end;
implementation

constructor TAbstractFinder.Create(arr: array of integer);
var len, i: integer;
begin
  inherited Create();
  len := length(arr);
  setlength(values, len);
  for i := 0 to len-1 do
  begin
    values[i] := arr[i];
    end;
  end;

procedure TAbstractFinder.ReplaceArray(arr: array of integer);
var len, i: integer;
begin
  len := length(arr);
  setlength(values, len);
  for i := 0 to len-1 do
  begin
    values[i] := arr[i];
  end;
end;

destructor TAbstractFinder.Destroy();
begin
  setlength(values, 0);
  inherited Destroy();
end;

procedure TAbstractFinder.setCompare(c: fcomp);
begin
  comp := c;
end;

var a1: integer;
function compare(a: integer): integer;
begin
  Result := a1 - a;
end;

function TAbstractFinder.findOneS(e: integer): integer;
var f: fcomp;
begin
  a1 := e;
  f := comp;
  setCompare(@compare);
  Result := findOne();
  comp := f;
end;

function TAbstractFinder.findOneCustomS(f: fcomp): integer;
var fold: fcomp;
begin
  fold := comp;
  setCompare(f);
  Result := findOne();
  comp := fold;
end;

function TAbstractFinder.findAllS(e: integer): vararr;
var f: fcomp;
begin
  a1 := e;
  f := comp;
  setCompare(@compare);
  Result := findAll();
  comp := f;
end;

function TAbstractFinder.findAllCustomS(f: fcomp): vararr;
var fold: fcomp;
begin
  fold := comp;
  setCompare(f);
  Result := findAll();
  comp := fold;
end;

end.

