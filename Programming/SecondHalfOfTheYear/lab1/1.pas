{$mode objfpc}
{$R+,S+}
uses TXCanvas;

procedure updateColor(p:T24030124Canvas; a:TPixel; c:integer);
var c1:integer;
begin
c1 := 255 - abs((c mod 768) - 384);
        if (c1 < 0) then c1 := 0;
        a[0] := c1;
        c1 := 255 - abs(((c + 256) mod 768) - 384);
        if (c1 < 0) then c1 := 0;
        a[1] := c1;
        c1 := 255 - abs(((c + 512) mod 768) - 384);
        if (c1 < 0) then c1 := 0;
        a[2] := c1;
        p.setColor(a);
end;

var p,q:T24030124Canvas;x,y,h,w,c:integer;a:TPixel;
begin
    // Loading and resizign
    p := T24030124Canvas.Create;
    q := T24030124Canvas.Create;
    p.read('Pic2.ppm');
    q.read('Pic4.ppm');
    p.save('Pic1.ppm');
    p.setSize(50, 50);
    p.setColor(128, 128, 128);
    p.setSize(400, 400);
    // Colors
    
    p.setColor(100, 300, 100);
    a := p.getColor();
    writeln(a[0],a[1], a[2]);
    p.setColor(100, 100, 100);
    a := p.getColor();
    writeln(a[0],a[1], a[2]);
    a[1] := 200;
    p.setColor(a);
    a := p.getColor();
    writeln(a[0], a[1], a[2]);
    // Postion
    
    p.setPos(99999, 99999);
    writeln(p.getPosX, p.getPosY);
    p.setPos(123, 123);
    writeln(p.getPosX, p.getPosY);
    
    // drawing 
    p.setColor(0, 0, 0);
    p.setPos(-2, -2);
    p.setPixel;
    p.setColor(255, 0, 0);
    p.setPos(39999, 299999);
    p.setPixel;
    p.setColor(0, 255, 0);
    p.setPos(2, 3);
    p.setPixel;
    p.setColor(0, 0, 255);
    p.setPos(3, 3);
    p.setPixel;
    
    h := p.getHeight div 2;
    w := p.getWidth div 2;
    c := 0;

    for x:=w - 100 to w + 100 do
    begin
        updateColor(p, a, c);
        p.setPos(x, h - 100);
        p.setPixel;
        c += 5;
    end;
    
    for y:=h - 99 to h + 100 do
    begin
        updateColor(p, a, c);
        p.setPos(w + 100, y);
        p.setPixel;
        c += 5;
    end;
    
    for x:=w + 99 downto w - 100 do
    begin
        updateColor(p, a, c);
        p.setPos(x, h + 100);
        p.setPixel;
        c += 5;
    end;
    
        for y:=h + 99 downto h - 100 do
    begin
        updateColor(p, a, c);
        p.setPos(w - 100, y);
        p.setPixel;
        c += 5;
    end;
    
    p.setColor(255, 0, 0);
    p.setPos(10, 10);
    p.drawRect(40, 40);
    p.setColor(24, 128, 24);
    p.setPos(11, 11);
    p.fillRect(38, 38);
    
    p.setColor(255, 0, 0);
    p.setpos(111, 199);
    p.setPixel();
    p.setpos(134, 178);
    p.setPixel();
    p.setpos(188, 154);
    p.setPixel();
    p.setpos(156, 178);
    p.setPixel();
    
    p.setColor(0, 0, 0);
    p.setPos(190, 140);
    p.bucket();
    p.setPos(1, 2);
    p.bucket();
    
    p.setColor(196, 128, 128);
    {
    p.setPos(-10, -10);
    p.drawline(10, 10);
    p.setPos(-10, -10);
    p.drawline(50, 10);
    p.setPos(-10, -10);
    p.drawline(10, 50);
    }
    p.setPos(150, 250);
    p.drawline(150, 200);
    p.drawline(200, 170);
    p.drawline(250, 200);
    p.drawline(150, 200);
    p.drawline(250, 250);
    p.drawline(250, 200);
    p.drawline(150, 250);
    p.drawline(250, 250);
    
    p.setColor(255, 255, 255);
    p.setPos(75, 75);
    p.paste(q);
    p.setColor(0, 0, 0);
    p.setPos(385, 385);
    p.paste(q);

    // Saving
    p.save('Pic3.ppm');
    p.setSize(0, 0);
    q.setSize(0, 0);
    
end.
