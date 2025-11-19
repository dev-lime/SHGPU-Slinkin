program project1;

uses heaptrc ,Unit1, Unit2, Classes;

var human, human1: TArrayHuman;
    fl: TFileStream;
    fl1: TMemoryStream;
    fl2: TArrayStream;
begin
    human := TArrayHuman.Create;

    while (human.manualInputHuman) do;
    writeln; human.writeArrHuman('=====');

    fl := TFileStream.Create('1.txt', fmCreate);
    human.saveTStream(fl); fl.free;
    fl := TFileStream.Create('1.txt', fmOpenRead);
    human1 := TArrayHuman.Create;
    fl.seek(0, soBeginning);
    human1.loadTStream(fl); fl.free;
    writeln; human1.writeArrHuman('-----');
    human1.free;

    fl := TFileStream.Create('2.txt', fmCreate);
    human.saveTWriter(fl); fl.free;
    fl := TFileStream.Create('2.txt', fmOpenRead);
    human1 := TArrayHuman.Create;
    fl.seek(0, soBeginning);
    human1.loadTReader(fl); fl.free;
    writeln; human1.writeArrHuman('+++++');
    human1.free;

    fl1 := TMemoryStream.Create;
    fl1.seek(10000, soBeginning);
    writeln(fl1.position, ' <<<<');
    human.saveTStream(fl1);
    human1 := TArrayHuman.Create;
    fl1.seek(10000, soBeginning);
    human1.loadTStream(fl1);
    fl1.seek(0, soBeginning);
    fl := TFileStream.Create('3.txt', fmCreate);
    fl.CopyFrom(fl1, fl1.Size); fl.free;
    writeln; human1.writeArrHuman('/////');
    fl1.free; human1.free;

    fl2 := TArrayStream.Create;
    fl2.seek(10000, soBeginning);
    writeln(fl2.position, ' <<<<');
    human.saveTStream(fl2);
    human1 := TArrayHuman.Create;
    fl2.seek(10000, soBeginning);
    human1.loadTStream(fl2);
    fl := TFileStream.Create('4.txt', fmCreate);
    fl2.seek(0, soBeginning);
    fl.CopyFrom(fl2, fl2.Size); fl.free;
    writeln; human1.writeArrHuman('!!!!!');
    fl2.free; human1.free;

    fl := TFileStream.Create('4.txt', fmOpenRead);
    human1 := TArrayHuman.Create;
    fl.seek(10000, soBeginning);
    human1.loadTStream(fl); fl.free;
    writeln; human1.writeArrHuman('-----');
    human1.free;

    human.free;
    //readln;
end.

