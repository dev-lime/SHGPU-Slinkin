unit unit1;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils;

type
    fcomp=function(a:integer; b:integer):integer;
    vararr=array of integer;
    TAbstractFinder=class
      private
      values:array of integer;
      comp:fcomp;
      public
        constructor Create(arr: array of integer);
        procedure ReplaceArray(arr: array of integer);
        destructor Decreate();
        procedure setCompare(c: fcomp);
        function isCorrect():boolean;virtual;abstract;
        function findOne():integer;virtual;abstract;
        function findOneS(e:integer):integer;
        function findOneCustomS(e:integer; f:fcomp):integer;
        function findAll():integer;virtual;abstract;
        function findAllS(e:integer):integer;
        function findAllCustomS(e:integer; f:fcomp):vararr;
      end;
implementation

constructor TAbstractFinder.Create(arr: array of integer);
var len,i:integer;
begin
  len := length(arr);
  setlength(values, len);
  for i:=0 to len-1 do
  begin
    values[i]:= arr[i];
    end;
  end;

procedure TAbstractFinder.ReplaceArray(arr: array of integer);
var len,i:integer;
begin
  len := length(arr);
  setlength(values, len);
  for i:=0 to len-1 do
  begin
    values[i]:= arr[i];
  end;
end;

destructor TAbstractFinder.Decreate();
begin
  setlength(values, 0);
end;

procedure TAbstractFinder.setCompare(c: fcomp);
begin
  comp:=c;
end;

function TAbstractFinder.findOneS(e:integer):integer;
var len,i:integer;
begin
  len := length(values);
  Result:=-1;
  for i:=0 to len-1 do
  begin
    if (values[i] = e) then
    begin
      Result:=i;
      break;
      end;
  end;
end;

function TAbstractFinder.findOneCustomS(e:integer; f:fcomp):integer;
var len,i:integer;
begin
  len := length(values);
  Result:=-1;
  for i:=0 to len-1 do
  begin
    if (values[i] = e) then
    begin
      Result:=i;
      break;
      end;
  end;
end;

function TAbstractFinder.findAllS(e:integer):vararr;
var len,i,lenarr:integer;arr:vararr;
begin
  lenarr :=0;
  setlength(arr, lenarr);
  len := length(values);
  for i:=0 to len-1 do
  begin
    if (values[i] = e) then
    begin
      lenarr := lenarr + 1;
      setlength(arr, lenarr);
      arr[lenarr - 1]:=values[i];
      end;
  end;
  Result:=arr;
end;

function TAbstractFinder.findAllCustomS(e:integer; f:fcomp):vararr;
var len,i,lenarr:integer;arr:vararr;
begin
  lenarr :=0;
  setlength(arr, lenarr);
  len := length(values);
  for i:=0 to len-1 do
  begin
    if (f(values[i], e) = 0) then
    begin
      lenarr := lenarr + 1;
      setlength(arr, lenarr);
      arr[lenarr - 1]:=values[i];
      end;
  end;
  Result:=arr;
end;

end.

