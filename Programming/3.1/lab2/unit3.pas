unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Unit1;

type
    TArrayList = class(TInterfacedObject, IList)
        private
            arrObj: array of tobject;
            arrNext: array of integer;
            obj, posObj: integer;

            function nilArrObj: integer;
        public
            function addFirst(note: tobject): tobject;
            function addLast(node: tobject): tobject;
            function deleteFirst: tobject;
            function deleteAfter(prevNode: tobject): tobject;
            function insertAfter(prevNode, node: tobject): tobject;
            function compare(node1, node2: tobject): integer;
            function first: tobject;
            function next: tobject;
            function last: tobject;
            procedure showNode(node: tobject);
            function copyNode(node: tobject): tobject;
            procedure destroyList;

            procedure writeList(str: string = '');

            constructor Create;
            destructor Destroy; override;
    end;

implementation

constructor TArrayList.Create;
begin
    inherited Create;
    setLength(arrObj,0);
    setLength(arrNext,0);
    posObj := 0; obj := -1;
end;
destructor TArrayList.Destroy;
begin
    destroyList;
end;

procedure TArrayList.writeList(str: string);
var o: tobject;
begin
    if (str <> '') then writeln(str);
    o := first;
    while (o <> nil) do
    begin showNode(o); o := next; end;
    if (str <> '') then writeln(str);
end;

function TArrayList.nilArrObj: integer;
var i: integer;
begin
   for i := 0 to length(arrObj) - 1 do
       if (arrObj[i] = nil) then exit(i);
   setLength(arrObj, length(arrObj) + 1);
   setLength(arrNext, length(arrNext) + 1);
   arrObj[length(arrObj) - 1] := nil;
   arrNext[length(arrnext) - 1] := -2;
   result := length(arrObj) - 1;
end;

function TArrayList.addFirst(note: tobject): tobject;
var i: integer;
begin
    if (note = nil) then exit(note);
    i := nilArrObj;
    arrObj[i] := note;
    arrNext[i] := obj;
    obj := i;
    result := note;
end;
function TArrayList.addLast(node: tobject): tobject;
var i, i1: integer;
begin
    if (node = nil) then exit(node);
    i := nilArrObj;
    arrObj[i] := node;
    arrNext[i] := -1;
    if (obj = -1) then obj := i
    else
    begin
        i1 := obj;
        while (arrNext[i1] <> -1) do i1 := arrNext[i1];
        arrNext[i1]:= i;
    end;
    result := node;
end;
function TArrayList.insertAfter(prevNode, node: tobject): tobject;
var i, i1: integer;
begin
    if (obj = -1) or (prevNode = nil) then exit(nil);
    if (node = nil) then exit(node);
    i := nilArrObj;
    i1 := obj;
    arrObj[i] := node;
    while (arrObj[i1] <> prevNode) and (arrNext[i1] <> -1) do i1 := arrNext[i1];
    if (arrObj[i1] <> prevNode) and (arrNext[i1] = -1) then exit(nil);
    arrNext[i] := arrNext[i1];
    arrNext[i1] := i;
    result := node;
end;

function TArrayList.deleteFirst: tobject;
var i: integer;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit(nil);
    i := obj;
    obj := arrNext[obj];
    result := arrObj[i];
    arrObj[i] := nil;
    arrNext[i] := -2;
end;
function TArrayList.deleteAfter(prevNode: tobject): tobject;
var i, i1: integer;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit(nil);
    if (prevNode = nil) then exit(nil);
    i := obj;
    while (arrObj[i] <> prevNode) and (arrNext[i] <> -1) do i := arrNext[i];
    if (arrNext[i] = -1) then exit(nil);
    i1 := arrNext[i];
    result := arrObj[i1];
    arrNext[i] := arrNext[i1];
    arrNext[i1] := -2;
    arrObj[i1] := nil;
end;
procedure TArrayList.destroyList;
var i, i1: integer;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit;
    i := obj;
    posObj := 0;
    while (arrNext[i] > -1) do
    begin
        i1 := arrNext[i];
        arrObj[i].free;
        arrObj[i] := nil;
        arrNext[i] := -2;
        i := i1;
    end;
    arrObj[i].free;
    arrNext[i] := -2;
    obj := -1;
    setLength(arrObj, 0);
    setLength(arrNext, 0);
end;

function TArrayList.first: tobject;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit(nil);
    posObj := obj;
    result := arrObj[posObj];
end;
function TArrayList.next: tobject;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit(nil);
    if (posObj <= -1) then posObj := obj;
    if (arrNext[posObj] = -1) then exit(nil);
    posObj := arrNext[posObj];
    result := arrObj[posObj];
end;
function TArrayList.last: tobject;
var i: integer;
begin
    if (length(arrObj) = 0) or (obj = -1) then exit(nil);
    if (posObj <= -1) then posObj := obj;
    i := posObj;
    while (arrNext[i] <> -1) do i := arrNext[i];
    posObj := i;
    result := arrObj[posObj];
end;

procedure TArrayList.showNode(node: tobject);
begin
    if node is TIntObj then writeln(TIntObj(node).data)
    else if node is TRealObj then writeln(TRealObj(node).data:0:6)
    //else if node is TStrObj then writeln(TStrObj(node).data)
    else writeln('none type');
end;

function TArrayList.compare(node1, node2: tobject): integer;
var r1, r2: real;
    i1, i2: integer;
begin
    i1 := 0; i2 := 0;
    if node1 is TIntObj then r1 := TIntObj(node1).data
    else if node1 is TRealObj then r1 := TRealObj(node1).data
    //else if node1 is TStrObj then Val(TStrObj(node1).data, r1, i1)
    else i1 := -1;
    if node2 is TIntObj then r2 := TIntObj(node2).data
    else if node2 is TRealObj then r2 := TRealObj(node2).data
    //else if node2 is TStrObj then Val(TStrObj(node2).data, r2, i2)
    else i2 := -1;

    if (i1 <> 0) or (i2 <> 0) then exit(-2)
    else if (r1 - r2 > 0.0000001) then exit(1)
    else if (r1 - r2 < -0.0000001) then exit(-1);

    result := 0;
end;
function TArrayList.copyNode(node: tobject): tobject;
begin
    if node is TIntObj then exit(TIntObj.Create(TIntObj(node).data))
    else if node is TRealObj then exit(TRealObj.Create(TRealObj(node).data));
    //else if node is TStrObj then exit(TStrObj.Create(TStrObj(node).data));
    result := nil;
end;

end.

