{$mode objfpc}
const MAX=10;
	  MIN=1;	
var a:array[1..10]of string=
(
'0001001000',
'0010010000',
'0001010111',
'0000001100',
'0001100000',
'0000101111',
'0000101000',
'0000101000',
'0001101100',
'0010000100'
);

procedure fill(x,y:integer);
begin
	if (x<MIN)or(x>MAX)or(y<MIN)or(y>MAX)or(a[y][x]<>'0')
		then exit;
	a[y][x]:='*';
	fill(x+1,y);
	fill(x-1,y);
	fill(x,y+1);
	fill(x,y-1);	
end;

procedure outa();
	var s:string;
begin
	for s in a do writeln(s);
end;

BEGIN
 fill(5,1);
 outa();
END.
