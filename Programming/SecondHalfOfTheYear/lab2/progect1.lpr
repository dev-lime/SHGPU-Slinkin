{$mode objfpc}
{$R+,S+}
program progect1;
uses heaptrc, unit1, unit2;
(*
function compare(a:integer):integer;
begin
  Result:=(a-3) div 2;
end;
*)
function compare2(a:integer):integer;
begin
  Result:=a;
end;

var i,i2:TAbstractFinder;f:fcomp;arr:vararr;j:integer;
begin
  // Line
  f:=@compare2;
  i:= TLineFinder.Create([1,2,3,4,5,6]);
  i.ReplaceArray([0,0,0,0,1,2,2,3,4,5,6]);
  i.setCompare(f);
  writeln(i.findOne());
  arr:=i.findAll();
  writeln();
  for j:=0 to length(arr)-1 do
    writeln(arr[j]);
  i.Destroy();
  writeln();
  // Bin
  //i2:= TBinFinder.Create([0,0,0,0,1,2,2,3,4,5,6]);
  i2:= TBinFinder.Create([1,8,9,7,6,5]);
  //i2.ReplaceArray([0,0,0,0,1,2,2,3,4,5,6]);
  i2.setCompare(f);
  writeln(i2.isCorrect());
  writeln(i2.findOne());
  arr:=i2.findAll();
  writeln();
  for j:=0 to length(arr)-1 do
    writeln(arr[j]);
  i2.Destroy();
end.

