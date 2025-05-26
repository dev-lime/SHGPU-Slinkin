{$mode objfpc}
uses dateutils,sysutils;
type TArray=array of integer;
	 TCompare=function (a,b:integer):integer;

procedure sort_v_base(A:TArray; comp:TCompare);
var	c,i,j:integer;
begin
	for i:=1 to length(a)-1 do begin
		j:=i; c:=a[j];
		while (j>0)and(comp(c,a[j-1])>0) do begin
			a[j]:=a[j-1];
			dec(j);
		end;	
		a[j]:=c;
	end;
end;


procedure sort_v(A:TArray; comp:TCompare);
var	c,i,j:integer;
begin
	for i:=2 to length(a)-1 do begin
		j:=i; c:=a[j];
		while (j>0)and(comp(c,a[j-1])>0) do dec(j);
		move(a[j],a[j+1],(i-j)*sizeof(integer));
		a[j]:=c;
	end;
end;
 
function cmp(a,b:integer):integer;
 begin result:=b-a; end;

var a:TArray=(10,1,50,5,5,60,8,2);
	//a:TArray=(1,1,50,5,5,6,8,20);
	c:integer;
BEGIN
	sort_v(a,@cmp);
	for c in a do write(c:3);
END.
