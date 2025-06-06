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

procedure sort_bubble(A:TArray; comp:TCompare);
var	c,i,j:integer;
	flag:boolean;
begin
	for i:=1 to length(a)-1 do begin
		flag:=false;
		for j:=1 to length(a)-i do begin
			if comp(a[j-1],a[j])<0 then begin
				flag:=true;
				c:=a[j]; a[j]:=a[j-1]; a[j-1]:=c;
			end;
		end;
		if not flag then exit;	
	end;
end;


procedure sort_q(A:TArray; comp:TCompare; left:integer=0; right:integer=-100);
var	center,c,i,j:integer;
begin
	if right=-100 then right:=length(a)-1;
	if left>=right then exit;
	center:=a[(left+right) div 2];
	i:=left; j:=right;
	repeat
		while (comp(a[i],center)>0) do inc(i);
		while (comp(a[j],center)<0) do dec(j);
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
	b:TArray;
	c:integer;
	dt:TDateTime;
const max=10000000;	
BEGIN
	setlength(a,max);
	for c:=0 to max-1 do a[c]:=random(10);
//	for c in a do write(c:3);
//	writeln;
{
	b:=copy(a,0);
	dt:=now();
	sort_bubble(b,@cmp);
//	for c in b do write(c:3);
	writeln(millisecondsbetween(dt,now()));

	b:=copy(a,0);
	dt:=now();
	sort_v_base(b,@cmp);
//	for c in b do write(c:3);
	writeln(millisecondsbetween(dt,now()));

	b:=copy(a,0);
	dt:=now();
	sort_v(b,@cmp);
//	for c in b do write(c:3);
	writeln(millisecondsbetween(dt,now()));
}	
	b:=copy(a,0);
	dt:=now();
	sort_q(b,@cmp);
//	for c in b do write(c:3);
	writeln(millisecondsbetween(dt,now()));


	
END.
