{$mode objfpc}
{$R+,S+}
unit TXCanvas;

interface
uses sysutils;
type
    TPixel=array[0..2]of integer;
    T24030124Canvas=class
    private
        Pixels:array of array of TPixel;
        width:integer;
        height:integer;
        max:integer;
        xpos:integer;
        ypos:integer;
        color:TPixel;
     //end;
     
    procedure bucketRec(x, y: integer; pixel: TPixel);
    
    public
    constructor Create();

    procedure read(s:ansistring);

    procedure save(s:ansistring);

    function getWidth():integer;

    function getHeight():integer;

    procedure setSize(w:integer; h:integer);

    function getColor():TPixel;

    procedure setColor(p:TPixel);

    procedure setColor(r:integer; g:integer; b:integer);

    procedure setPos(x:integer; y:integer);

    function getPosX():integer;

    function getPosY():integer;
    
    function getPixel():TPixel;

    procedure setPixel();
    
    procedure drawRect(x1, y1:integer);
    
    procedure drawRect(x0, y0, x1, y1:integer);
    
    procedure fillRect(x1, y1:integer);

    procedure fillRect(x0, y0, x1, y1:integer);
    
    procedure drawLine(x1, y1:integer);
    
    procedure bucket();
    
    procedure paste(p: T24030124Canvas);
    
    function comparePixels(a, b:TPixel):boolean;
    
    end;


implementation

procedure getinput(var buf: string; inp:ansistring;var pos:integer; symbol:char);
  begin
  buf:='';
  while ((pos <= length(inp)) and (inp[pos] <> symbol)) do
    begin 
    buf += inp[pos];
    pos += 1;
    end;
  pos += 1;
  end;

constructor T24030124Canvas.Create();
    begin
        height:=0;
        width:=0;
        max:=255;
        setSize(0, 0);
        setPos(0, 0);
        setColor(0,0,0);
    end;

procedure T24030124Canvas.read(s:ansistring);
var f:text;s1:ansistring;buf:string;w,h,d,i,j,k,pos:integer;
begin
    
    assignfile(f, s);
    reset(f);
    readln(f, s1);
    while (s1[1] = '#') do readln(f, s1);
    if (s1 <> 'P3') then 
    begin
    writeln('ERROR: Wrong format!');
    exit;
    end;
    readln(f, s1);
    while (s1[1] = '#') do readln(f, s1);
    
    pos:=1; 
    getinput(buf, s1, pos, ' ');
    w := StrtoInt(buf);
    getinput(buf, s1, pos, ' ');
    h := StrtoInt(buf);
   
    readln(f, s1);
    while (s1[1] = '#') do readln(f, s1);
    
    d := StrtoInt(s1);
    height:=0;
    width:=0;
    max:=d;
    setSize(w, h);
    setPos(0, 0);
    setColor(0,0,0);
    for i:=0 to getHeight()-1 do
    begin;
        for j:=0 to getWidth()-1 do
        begin
            for k:=0 to 2 do
            begin
                if pos >= length(s1) then
                begin
                    pos := 1;
                    readln(f, s1);
                    while (s1[1] = '#') do readln(f, s1);
                end;
                getinput(buf, s1, pos, ' ');
                Pixels[i][j][k] := StrtoInt(buf);
            end;
        end;
    end;
    writeln('read ', s, ' ', height, 'x', width, '@', max);
    close(f);
end;

procedure T24030124Canvas.save(s:ansistring);
var f:text;i,j:integer;
begin
    assignfile(f, s);
    rewrite(f);
    writeln(f, 'P3');
    writeln(f, width, ' ', height);
    writeln(f, max);
    for i:=0 to getHeight()-1 do
    begin;
        for j:=0 to getWidth()-1 do
        begin
            write(f, Pixels[i][j][0], ' ', Pixels[i][j][1], ' ', Pixels[i][j][2], ' ');
        end;
        writeln(f);
    end;
    writeln('save ', s, ' ', height, 'x', width, '@', max);
    close(f);
end;

function T24030124Canvas.getWidth():integer;
begin
    result:=width;
end;

function T24030124Canvas.getHeight():integer;
begin
    result:=height;
end;

procedure T24030124Canvas.setSize(w:integer; h:integer);
var i,j:integer;
begin
    if ((w < 0) or (h < 0)) then
    writeln('WARN: Wrong Canvas size');
    for i:=h to height-1 do
    begin
        setlength(Pixels[i], 0);
    end;
    setlength(Pixels, h);
    for i:=0 to h-1 do
    begin
        setlength(Pixels[i], w);
    end;
    for i:=width to w-1 do
    begin
        for j:=0 to height-1 do
        begin
            Pixels[j][i] := color;
        end;
    end;
    for i:=0 to w-1 do
    begin
        for j:=height to h-1 do
        begin
            Pixels[j][i] := color;
        end;
    end;
    width:=w;
    height:=h;
    xpos := 0;
    ypos := 0;
    writeln('setSize ', height, 'x', width, '@', max);
end;

function T24030124Canvas.getColor():TPixel;
begin
    result:=color;
end;

procedure T24030124Canvas.setColor(p:TPixel);
begin
    if ((p[0] < 0) or (p[0] > max) or (p[1] < 0) or (p[1] > max) or (p[2] < 0) or (p[2] > max)) then
    begin
        writeln('ERROR: Wrong color');
        exit;
    end;
    color:=p;
end;

procedure T24030124Canvas.setColor(r:integer; g:integer; b:integer);
begin
    if ((r < 0) or (r > max) or (g < 0) or (g > max) or (b < 0) or (b > max)) then
    begin
        writeln('ERROR: Wrong color');
        exit;
    end;
    color[0]:=r;
    color[1]:=g;
    color[2]:=b;
end;

procedure T24030124Canvas.setPos(x:integer; y:integer);
begin
    if ((x<0) or (y<0) or (x>=getWidth) or (y>=getHeight)) then
    begin
        writeln('ERROR: Out of bounds set');
        exit;
    end;
    xpos := x;
    ypos := y;
end;

function T24030124Canvas.getPosX():integer;
begin
    result:=xpos;
end;

function T24030124Canvas.getPosY():integer;
begin
    result:=ypos;
end;

function T24030124Canvas.getPixel():TPixel;
begin
    result:=Pixels[ypos][xpos];
end;

procedure T24030124Canvas.setPixel();
begin
    Pixels[ypos][xpos]:= color;
end;

procedure T24030124Canvas.drawRect(x1, y1:integer);
var x, y, x0, y0:integer;
begin
    x0:= getPosX();
    y0:= getPosY();
    x1:= x0 + x1;
    y1:= y0 + y1;
    writeln('drawRect ', x0, ',', y0, ';', x1, ',', y1);
    if ((x0 < 0) or (y0 < 0) or (x1 >= getWidth()) or (y1 >= getHeight())) then
    begin
        writeln('WARN: Out of bounds rect');
    end;
    for x:=x0 to x1 do
    begin
        setPos(x, y0);
        setPixel;
        setPos(x, y1);
        setPixel;
    end;
    for y:=y0 to y1 do
    begin
        setPos(x0, y);
        setPixel;
        setPos(x1, y);
        setPixel;
    end;
    
end;

procedure T24030124Canvas.drawRect(x0, y0, x1, y1:integer);
var x, y:integer;
begin
    writeln('drawRect ', x0, ',', y0, ';', x1, ',', y1);
    if ((x0 < 0) or (y0 < 0) or (x1 >= getWidth()) or (y1 >= getHeight())) then
    begin
        writeln('WARN: Out of bounds rect');
    end;
    for x:=x0 to x1 do
    begin
        setPos(x, y0);
        setPixel;
        setPos(x, y1);
        setPixel;
    end;
    for y:=y0 to y1 do
    begin
        setPos(x0, y);
        setPixel;
        setPos(x1, y);
        setPixel;
    end;
    
end;

procedure T24030124Canvas.fillRect(x1, y1:integer);
var x, y, x0, y0:integer;
begin
    x0:=getPosX();
    y0:=getPosY();
    x1:= x0 + x1;
    y1:= y0 + y1;
    writeln('fillRect ', x0, ',', y0, ';', x1, ',', y1);
    if ((x0 < 0) or (y0 < 0) or (x1 >= getWidth()) or (y1 >= getHeight())) then
    begin
        writeln('WARN: Out of bounds rect');
    end;
    if (x0 < 0) then x0:=0;
    if (y0 < 0) then y0:=0;
    if (x1 >= getWidth) then x1 := getWidth;
    if (y1 >= getHeight) then y1 := getHeight;
    for x:=x0 to x1 do
        for y:=y0 to y1 do
        begin
            Pixels[y][x]:=color;
        end;
end;

procedure T24030124Canvas.fillRect(x0, y0, x1, y1:integer);
var x, y:integer;
begin
    writeln('fillRect ', x0, ',', y0, ';', x1, ',', y1);
    if ((x0 < 0) or (y0 < 0) or (x1 >= getWidth()) or (y1 >= getHeight())) then
    begin
        writeln('WARN: Out of bounds rect');
    end;
    if (x0 < 0) then x0:=0;
    if (y0 < 0) then y0:=0;
    if (x1 >= getWidth) then x1 := getWidth;
    if (y1 >= getHeight) then y1 := getHeight;
    for x:=x0 to x1 do
        for y:=y0 to y1 do
        begin
            Pixels[y][x]:=color;
        end;
end;

procedure T24030124Canvas.drawLine(x1, y1:integer);
var x, y, dx, dy, dirx, diry, error, isswap, x0r, x1r, x0, y0:integer;
begin
    x0:= getPosX();
    y0:= getPosY();
    writeln('drawLine ', x0, ',', y0, ';', x1, ',', y1);
    if ((x0 < 0) or (y0 < 0) or (x1 >= getWidth()) or (y1 >= getHeight())) then
    begin
        writeln('WARN: Out of bounds draw');
    end;
    dx:= x1-x0;
    dy:= y1-y0;
    isswap:=0;
    if (abs(dx) < abs(dy)) then 
    begin
        isswap:=1;
        y:=dy;
        dy:=dx;
        dx:=y;
    end;
    error:=0;
    diry := -1;
    if (dy > 0) then
        diry := 1;
    dirx := -1;
    if (dx > 0) then
        dirx := 1;
    dx:=abs(dx);
    dy:=abs(dy);
    if (isswap = 0) then
    begin
        x0r:=x0;
        x1r:=x1;
        y:=y0;
    end
    else
    begin
        x0r:=y0;
        x1r:=y1;
        y:=x0;
    end;
    while (x0r <> x1r) do
    begin
        x:= x0r;
        if (isswap = 1) then
            setPos(y, x)
        else
            setPos(x, y);
        setPixel();
        error:=error+dy+1;
        if (error>= dx+1) then
        begin
            y:= y + diry;
            error:= error - dx - 1;
        end;
        x0r := x0r + dirx;
    end; 
    setPos(x1, y1);
end;

procedure T24030124Canvas.bucket();
var pixel:TPixel;
begin
    writeln('bucket ', xpos, ';', ypos);
    if ((xpos <= -1)or(ypos <= -1)or(xpos>=getWidth())or(ypos>=getHeight()))then
    begin
        writeln('ERROR: Out of bounds bucket');
    end;
    pixel:=Pixels[ypos][xpos];
    if ((color[0] = pixel[0]) and (color[1] = pixel[1]) and (color[2] = pixel[2])) then
        exit;
    bucketRec(xpos, ypos, pixel);
end;

procedure T24030124Canvas.bucketRec(x, y:integer; pixel:TPixel);
    var ymin:integer;
    begin
    setPos(x, y);
    if ((x <= -1)or(y <= -1)or(x>=getWidth())or(y>=getHeight())or(not comparePixels(getPixel(), pixel)))then exit;
    while (not((x <= -1)or(y <= -1)or(y>=getHeight())or(x>=getWidth())or(not comparePixels(getPixel(), pixel)))) do
    begin
		y:=y - 1;
		setPos(x, y);
    end;
	y:= y + 1;
	ymin := y;
	setPos(x, y);
	while ((y<height) and comparePixels(getPixel(), pixel)) do
	begin
		Pixels[y][x] := color;
		y := y + 1;
		setPos(x, y);
	end;
	y := y - 1;
	while (y >= ymin) do
	begin
		bucketrec(x+1, y, pixel);
		bucketrec(x-1, y, pixel);
		y:= y - 1;
	end;
end;

procedure T24030124Canvas.paste(p: T24030124Canvas);
var bx, by, sx, sy, x, y:integer; p1, p2:TPixel;
begin
    writeln('Paste canvas at ', getPosX(), ',', getPosY());
    bx := xpos;
    by := ypos;
    p1 := getColor();
    sx := p.getWidth();
    if (sx > getWidth() - getPosX()) then
        sx := getWidth() - getPosX();
    sy := p.getHeight();
    if (sy > getHeight() - getPosY()) then
        sy := getHeight() - getPosY();
    for x:=0 to sx-1 do
    begin
        for y:=0 to sy-1 do
        begin
            setPos(bx+x, by+y);
            p.setPos(x, y);
            p2 := p.getPixel();
            if not comparePixels(p2, p1) then
            begin
                setColor(p2);
                setPixel();
            end;
        end;
    end;
end;

function T24030124Canvas.comparePixels(a, b:TPixel):boolean;
begin
    result := ((a[0] = b[0]) and (a[1] = b[1]) and (a[2] = b[2]));
end;
end.
