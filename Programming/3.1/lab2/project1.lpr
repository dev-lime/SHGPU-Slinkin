program project1;

{$INTERFACES CORBA}
uses HeapTRC ,Unit1, Unit2, Unit3, Unit4, SysUtils;

var data, data2: TClassicList;
    dataA, dataA2: TArrayList;
begin
    data := TClassicList.Create;
    data2 := TClassicList.Create;
    dataA := TArrayList.Create;
    dataA2 := TArrayList.Create;

    data.addLast(TIntObj.Create(1));
    data.addLast(TRealObj.Create(2));
    data.addLast(TIntObj.Create(3));
    data.addLast(TRealObj.Create(3.5));
    data.addLast(TIntObj.Create(2));

    dataA.addLast(TIntObj.Create(12));
    dataA.addLast(TRealObj.Create(11));
    dataA.addLast(TIntObj.Create(50));
    dataA.addLast(TRealObj.Create(50.5));
    dataA.addLast(TIntObj.Create(11));

    data2.addLast(TIntObj.Create(1));
    data2.addLast(TRealObj.Create(6.0001));
    data2.addLast(TIntObj.Create(6));
    data2.addLast(TRealObj.Create(7));
    data2.addLast(TIntObj.Create(2));

    dataA2.addLast(TIntObj.Create(1));
    dataA2.addLast(TRealObj.Create(3.5001));
    dataA2.addLast(TIntObj.Create(1));
    dataA2.addLast(TRealObj.Create(3));
    dataA2.addLast(TIntObj.Create(1));

    data.writeList('---'); writeln;
    data2.writeList('+++'); writeln;
    dataA.writeList('///'); writeln;
    dataA2.writeList('==='); writeln;
    writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'); writeln;

    //ListMerge(data, dataA);
    //ListChessMerge(data, data2);
    ListSort(data, data2, true);

    data.writeList('---'); writeln;
    data2.writeList('+++'); writeln;
    dataA.writeList('///'); writeln;
    dataA2.writeList('==='); writeln;

    data.free;
    data2.free;
    dataA.free;
    dataA2.free;
    //readln;
end.
