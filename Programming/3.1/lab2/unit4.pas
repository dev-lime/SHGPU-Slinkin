unit Unit4;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Unit1;

procedure ListMerge(LResult: IList; L: IList);
procedure ListChessMerge(LResult: IList; L: IList);
procedure ListSort(LResult: IList; L: IList; ASC: boolean);

implementation

procedure ListMerge(LResult: IList; L: IList);
begin
    LResult.addLast(L.copyNode(L.first));
    while (LResult.addLast(L.copyNode(L.next)) <> nil) do;
end;

procedure ListChessMerge(LResult: IList; L: IList);
var o, o1, o2: tobject;
begin
    o := LResult.first; o1 := L.first;
    while (o1 <> nil) do
    begin
        LResult.insertAfter(o, L.copyNode(o1));
        o2 := LResult.next; if (o2 <> nil) then o := o2;
        o2 := LResult.next; if (o2 <> nil) then o := o2;
        o1 := L.next;
    end;
end;

procedure ListSort(LResult: IList; L: IList; ASC:boolean);
var b0: boolean;
    k: integer;
    o1, o2, o3: tobject;
begin
    if (LResult = L) then exit;
    LResult.addLast(L.deleteFirst);
    while (LResult.addLast(L.deleteFirst) <> nil) do;
    b0 := true;
    while b0 do
    begin
        b0 := false; o3 := LResult.first; o1 := nil;
        while (o3 <> nil) do
        begin
            o2 := o3; o3 := LResult.next;
            if (o3 = nil) then break;
            k := LResult.compare(o2, o3);
            if (k = -2) then exit;
            if ((k = 1) and asc) or ((k = -1) and (not asc)) then
            begin
                b0 := true;
                if (o1 = nil) then LResult.deleteFirst
                else LResult.deleteAfter(o1);
                LResult.insertAfter(o3, o2);
            end;
            o1 := o2;
        end;
    end;
end;

end.

