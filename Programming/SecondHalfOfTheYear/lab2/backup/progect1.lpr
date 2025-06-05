{$mode objfpc}
{$R+,S+}
program progect1;
uses unit1;

function compare(a:integer; b:integer):integer;
var a1, b1:integer;
begin
  a1:=a;
  b1:=b;
  Result:=a1-b1;
end;

function compare2(a:integer; b:integer):integer;
var a1, b1:integer;
begin
  a1:=a;
  b1:=b;
  Result:=b1-a1;
end;

function compare3(a:integer; b:integer):integer;
var a1, b1:integer;
begin
  a1:=a;
  b1:=b;
  Result:=(b1-a1) div 2;
end;

var i:TAbstractFinder;f:fcomp;arr:vararr;j:integer;
begin
  f:=@compare;
  i:= TAbstractFinder.Create([1,2,3,4,5,6]);
  i.ReplaceArray([0,1,2,3,4,5,6,0,0,0,0,1,0]);
  writeln(i.findOneS(30));
  writeln(i.findOneS(3));
  writeln(i.findMaxS(f));
  f:=@compare2;
  writeln(i.findMinS(f));
  arr:=i.findAllS(0);
  writeln();
  for j:=0 to length(arr)-1 do
    writeln(arr[j]);
  f:=@compare3;
  arr:=i.findAllCustomS(0, f);
  writeln();
  for j:=0 to length(arr)-1 do
    writeln(arr[j]);
  i.Decreate();
end.

