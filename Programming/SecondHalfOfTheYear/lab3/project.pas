{$mode objfpc}
uses heaptrc, MemControl;

var
  x: TIntMemStorage;
  y: TFloatMemStorage;
  z: TIntFileStorage;
begin
  x := TIntMemStorage.Create();
  y := TFloatMemStorage.Create();
  z := TIntFileStorage.Create('storage.dat');

  writeln('> TIntMemStorage');
  x[2] := 2;
  writeln(x[1]);
  writeln(x[2]);
  writeln(x[3]);

  writeln();
  writeln('> TFloatMemStorage');
  y[2] := 2.134;
  y[1] := 0.123;
  writeln(y[1]:0:3);
  writeln(y[2]:0:3);
  writeln(y[3]:0:3);

  writeln();
  writeln('> TIntFileStorage');
  z[2] := 1;
  writeln(z[3]);
  writeln(z[2]);
  z[0] := -11;
  writeln(z[1]);
  z[1000000] := 123;
  writeln(z[1000000]);

  writeln();
  writeln('> Counts');
  writeln(x.count);
  writeln(y.count);
  writeln(z.count);
  x.free;
  y.free;
  z.free;
end.

