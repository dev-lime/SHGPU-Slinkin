program project1;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Lists;

procedure WriteList(L: IList; const Separator: string);
var
  Node: TObject;
begin
  Node := L.first;
  while Node <> nil do
  begin
    L.showNode(Node);
    Write(Separator);
    Node := L.next;
  end;
  Writeln;
end;

var
  data, data2: IList;
  dataA, dataA2: IList;
begin
  data := TClassicList.Create;
  data2 := TClassicList.Create;
  dataA := TArrayList.Create;
  dataA2 := TArrayList.Create;

  data.addLast(TIntegerNode.Create(1));
  data.addLast(TFloatNode.Create(2));
  data.addLast(TIntegerNode.Create(3));
  data.addLast(TFloatNode.Create(3.5));
  data.addLast(TIntegerNode.Create(2));

  dataA.addLast(TIntegerNode.Create(12));
  dataA.addLast(TFloatNode.Create(11));
  dataA.addLast(TIntegerNode.Create(50));
  dataA.addLast(TFloatNode.Create(50.5));
  dataA.addLast(TIntegerNode.Create(11));

  data2.addLast(TIntegerNode.Create(1));
  data2.addLast(TFloatNode.Create(6.0001));
  data2.addLast(TIntegerNode.Create(6));
  data2.addLast(TFloatNode.Create(7));
  data2.addLast(TIntegerNode.Create(2));

  dataA2.addLast(TIntegerNode.Create(1));
  dataA2.addLast(TFloatNode.Create(3.5001));
  dataA2.addLast(TIntegerNode.Create(1));
  dataA2.addLast(TFloatNode.Create(3));
  dataA2.addLast(TIntegerNode.Create(1));

  Write('---'); WriteList(data, ' ');
  Write('+++'); WriteList(data2, ' ');
  Write('///'); WriteList(dataA, ' ');
  Write('==='); WriteList(dataA2, ' ');
  Writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'); Writeln;

  // ListMerge(data, dataA);
  // ListChessMerge(data, data2);
  ListSort(data, data2, true);

  Write('---'); WriteList(data, ' ');
  Write('+++'); WriteList(data2, ' ');
  Write('///'); WriteList(dataA, ' ');
  Write('==='); WriteList(dataA2, ' ');

  data.destroyList;
  data2.destroyList;
  dataA.destroyList;
  dataA2.destroyList;
end.

