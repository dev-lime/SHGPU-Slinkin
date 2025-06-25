unit Unit2;

{$mode ObjFPC}

interface
uses Classes, SysUtils, unit1;

type
  TLineFinder = class(TAbstractFinder)
    public
    function isCorrect(): boolean; override;
    function findOne(): integer; override;
    function findAll(): vararr; override;
  end;
  TBinFinder = class(TAbstractFinder)
    public
    function isCorrect(): boolean; override;
    function findOne(): integer; override;
    function findAll(): vararr; override;
  end;

implementation

function TLineFinder.isCorrect(): boolean;
begin
  Result := true;
end;

function TLineFinder.findOne(): integer;
var len, i: integer;
begin
  len := length(values);
  Result := -1;
  for i := 0 to len-1 do
  begin
    if (comp(values[i]) = 0) then
    begin
      Result := i;
      break;
      end;
  end;
end;

function TLineFinder.findAll(): vararr;
var len, i, lenarr: integer;
  arr: vararr;
begin
  lenarr := 0;
  setlength(arr, lenarr);
  len := length(values);
  for i:=0 to len-1 do
  begin
    if (comp(values[i]) = 0) then
    begin
      lenarr := lenarr + 1;
      setlength(arr, lenarr);
      arr[lenarr-1] := i;
      end;
  end;
  Result := arr;
end;

function TBinFinder.isCorrect(): boolean;
var len, i, last: integer;
begin
  len := length(values);
  Result := true;
  if (len = 0) then
    exit(true);
  last := comp(values[0]);
  for i := 1 to len-1 do
  begin
    if (comp(values[i]) < last) then
    begin
      Result := false;
      break;
    end;
    last := comp(values[i]);
  end;
end;

function TBinFinder.findOne(): integer;
var l, r, m, res: integer;
begin
  l := -1;
  r := length(values);
  result := -1;
  while (r - l > 1) do
  begin
    m := ((r - l) shr 1) + l;
    res := comp(values[m]);
    if (res = 0) then
    begin
      result := m;
      break;
    end;
    if (res > 0) then
      r := m;
    if (res < 0) then
      l := m;
  end;
end;

function TBinFinder.findAll(): vararr;
var l, r, m, res, pos, len: integer;
begin
  setlength(result, 0);
  l := -1;
  r := length(values);
  if (r = 0) then
    exit;
  pos := -1;
  len := 0;
  while (r - l > 1) do
  begin
    m := ((r - l) shr 1) + l;
    res := comp(values[m]);
    if (res = 0) then
    begin
      pos := m;
      r := m;
    end;
    if (res > 0) then
      r := m;
    if (res < 0) then
      l := m;
  end;
  while(comp(values[pos]) = 0) do
  begin
    setlength(result, len + 1);
    result[len] := pos;
    len := len + 1;
    pos := pos + 1;
  end;
end;

end.

