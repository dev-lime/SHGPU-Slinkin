{$mode objfpc}
uses dateutils,sysutils;
type TArray=array of integer;
	 TCompare=function (a,b:integer):integer;

procedure sort_q(A:TArray; comp:TCompare; left:integer=0; right:integer=-100);
var	center,c,i,j:integer;
begin
	if right=-100 then right:=length(a)-1;
	if left>=right then exit;
	center:=a[(left+right) div 2];
	i:=left; j:=right;
	repeat
		while (a[i]<center) do inc(i);
		while (a[j]>center) do dec(j);
		if (i<=j) then begin
			c:=a[i]; a[i]:=a[j]; a[j]:=c;
			inc(i);dec(j);
		end;
	until i>j;
	sort_q(a,comp,left,j);
	sort_q(a,comp,i,right);
end;	
 
function cmp(a,b:integer):integer;
 begin result:=b-a; end;

var a:TArray=(10,1,50,5,5,60,8,2);
	//a:TArray=(1,1,50,5,5,6,8,20);
	c:integer;
BEGIN
	sort_q(a,@cmp);
	for c in a do write(c:3);
END.
