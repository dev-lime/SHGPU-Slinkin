{$mode objfpc}
//Если n=1, то n!=1
//иначе n!=n*(n-1)!
function f(n:integer):integer;
begin
	//writeln(ptruint(@n));
	if n=1 then exit(1)
	else exit(n*f(n-1));
end;
var x:int64=-2147483648*34;
BEGIN
 writeln(f(33));	
{ while x<>0 do begin
	write(abs(x mod 2));
	x:=x div 2;
 end;
}	
END.

