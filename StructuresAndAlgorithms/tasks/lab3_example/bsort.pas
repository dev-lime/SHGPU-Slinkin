{$mode objfpc}
uses dateutils,sysutils;
type TArray=array of integer;
	 TCompare=function (a,b:integer):integer;

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
 
function cmp(a,b:integer):integer;
 begin result:=b-a; end;

var a:TArray=(10,1,50,5,5,60,8,2);
	//a:TArray=(1,1,50,5,5,6,8,20);
	c:integer;
BEGIN
	sort_bubble(a,@cmp);
	for c in a do write(c:3);
END.
