unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Unit1;

type
    PNode = ^TNode;
    TNode = record
        data: TObject;
        next: PNode;
    end;

    TClassicList = class(TInterfacedObject, IList)
        private
            obj, posObj: PNode;
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

constructor TClassicList.Create;
begin
    inherited Create;
    obj := nil; posObj := nil;
end;
destructor TClassicList.Destroy;
begin
    destroyList;
end;

procedure TClassicList.writeList(str: string);
var o: tobject;
begin
    if (str <> '') then writeln(str);
    o := first;
    while (o <> nil) do
    begin showNode(o); o := next; end;
    if (str <> '') then writeln(str);
end;

function TClassicList.addFirst(note: tobject): tobject;
var pn: PNode;
begin
    if (note = nil) then exit(note);
    new(pn);
    pn^.data := note;
    pn^.next := obj;
    obj := pn;
    result := note;
end;
function TClassicList.addLast(node: tobject): tobject;
var pn, pn1: PNode;
begin
    if (node = nil) then exit(node);
    new(pn1);
    pn1^.data := node;
    pn1^.next := nil;
    if (obj = nil) then obj := pn1
    else
    begin
        pn := obj;
        while (pn^.next <> nil) do pn := pn^.next;
        pn^.next := pn1;
    end;
    result := node;
end;
function TClassicList.insertAfter(prevNode, node: tobject): tobject;
var pn, pn1: PNode;
begin
    if (prevNode = nil) then exit(nil);
    if (node = nil) then exit(nil);
    pn := obj;
    new(pn1);
    pn1^.data := node;
    while (pn^.data <> prevNode) and (pn <> nil) do
        pn := pn^.next;
    if (pn = nil) then exit(nil);
    pn1^.next := pn^.next;
    pn^.next := pn1;
    result := node;
end;

function TClassicList.deleteFirst: tobject;
var pn: PNode;
begin
    if (obj = nil) then exit(nil);
    pn := obj;
    obj := obj^.next;
    result := pn^.data;
    dispose(pn);
end;
function TClassicList.deleteAfter(prevNode: tobject): tobject;
var pn, pn1: PNode;
begin
    if (obj = nil) then exit(nil);
    if (prevNode = nil) then exit(nil);
    pn := obj;
    while (pn^.data <> prevNode) and (pn^.next <> nil) do
        pn := pn^.next;
    if (pn^.next = nil) then exit(nil);
    pn1 := pn^.next;
    result := pn1^.data;
    pn^.next := pn1^.next;
    dispose(pn1);
end;
procedure TClassicList.destroyList;
var pn, pn1: PNode;
begin
    pn := obj;
    posObj := nil;
    while (pn <> nil) do
    begin
        pn1 := pn^.next;
        pn^.data.free;
        dispose(pn);
        pn := pn1;
    end;
    obj := nil;
end;

function TClassicList.first: tobject;
begin
    if (obj = nil) then exit(nil);
    posObj := obj;
    result := posObj^.data;
end;
function TClassicList.next: tobject;
begin
    if (obj = nil) then exit(nil);
    if (posObj = nil) then posObj := obj;
    if (posObj^.next = nil) then exit(nil);
    posObj := posObj^.next;
    result := posObj^.data;
end;
function TClassicList.last: tobject;
var pn: PNode;
begin
    if (obj = nil) then exit(nil);
    if (posObj = nil) then posObj := obj;
    pn := posObj;
    while (pn^.next <> nil) do
        pn := pn^.next;
    posObj := pn;
    result := posObj^.data;
end;

procedure TClassicList.showNode(node: tobject);
begin
    if node is TIntObj then writeln(TIntObj(node).data)
    else if node is TRealObj then writeln(TRealObj(node).data:0:6)
    //else if node is TStrObj then writeln(TStrObj(node).data)
    else writeln('none type');
end;

function TClassicList.compare(node1, node2: tobject): integer;
var r1, r2: real;
    i1, i2: integer;
begin
    i1 := 0; i2 := 0;
    if node1 is TIntObj then r1 := TIntObj(node1).data
    else if node1 is TRealObj then r1 := TRealObj(node1).data
    //else if node1 is TStrObj then Val(TStrObj(node1).data, r1, i1)
    else i1 := -2;
    if node2 is TIntObj then r2 := TIntObj(node2).data
    else if node2 is TRealObj then r2 := TRealObj(node2).data
    //else if node2 is TStrObj then Val(TStrObj(node2).data, r2, i2)
    else i2 := -2;

    if (i1 <> 0) or (i2 <> 0) then exit(-2)
    else if (r1 - r2 > 0.0000001) then exit(1)
    else if (r1 - r2 < -0.0000001) then exit(-1);

    result := 0;
end;
function TClassicList.copyNode(node: tobject): tobject;
begin
    if node is TIntObj then exit(TIntObj.Create(TIntObj(node).data))
    else if node is TRealObj then exit(TRealObj.Create(TRealObj(node).data));
    //else if node is TStrObj then exit(TStrObj.Create(TStrObj(node).data));
    result := nil;
end;

end.
