unit Unit1;

{$mode ObjFPC}{$H+}
{$INTERFACES CORBA}

interface

uses Classes, SysUtils;

type
    IList = interface
        //['{2190A571-EDCA-4017-8D77-B2C693F84560}']
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
    end;

    generic TDataObj<T> = class
        public
            data: T;
            constructor Create(i: T);
    end;

    TIntObj = specialize TDataObj<integer>;
    TRealObj = specialize TDataObj<real>;
    //TStrObj = specialize TDataObj<string>;

implementation

constructor TDataObj.Create(i: T);
begin
    inherited Create; data := i;
end;

end.

