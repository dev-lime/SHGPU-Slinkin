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

var p:T24030124Canvas;x,y,h,w,c:integer;a:TPixel;
begin
    // Loading and resizign
    p := T24030124Canvas.Create;
    p.read('Pic5.ppm');
    // Colors
    
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
    
    p.setColor(0, 255, 0);
    p.setPos(400, 200);
    p.bucket();
    
    // Saving
    
    p.save('Pic3.ppm');
    p.setSize(0, 0);
    
end.
