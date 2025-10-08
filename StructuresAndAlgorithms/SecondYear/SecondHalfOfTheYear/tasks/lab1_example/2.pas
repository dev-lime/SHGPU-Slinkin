{$mode objfpc}
procedure reverse();
 var x:integer;
begin
	writeln(ptruint(@x));
	readln(x);
	if x=0 then exit;
	reverse();
	writeln(x);
end;
BEGIN
 reverse();
END.

