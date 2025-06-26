unit canvas23030428;

{$mode objfpc}{$H+}

interface

type
  TRGB = record
    R: byte;
    G: byte;
    B: byte;
  end;

  TPencil = record
    X: integer;
    Y: integer;
    color: TRGB;
  end;

  { T23030428Canvas }

  T23030428Canvas = class
  private
    width, height: integer;
    pixels: array of array of TRGB;
    pencil: TPencil;
  public
    //холст
    procedure add(w, h: integer);
    procedure SetPixelColor(x, y: integer; r, g, b: byte);
    procedure SaveToPPM(const FName: string);
    procedure LoadFromPPM(const FName: string);

    //карандаш
    procedure SetPencilColor(r, g, b: byte);
    function GetPencilColor: TRGB;
    procedure KoordPencil(x, y: integer);
    function GetPencilX: integer;
    function GetPencilY: integer;
    procedure DrawPixel;

    //размер холста
    function GetWidth: integer;
    function GetHeight: integer;

    //6
    procedure rectangle6(sh, d: integer);

    //7
    procedure rectangle7(sh, d: integer);

    //8
    procedure segment(x2, y2: integer);

    //9
    procedure lim_area;

    //10
    procedure OverlayCanvas(canv: T23030428Canvas);
  end;

implementation

//создание холста
procedure T23030428Canvas.add(w, h: integer);
var
  i, j: integer;
begin
     width := w;
     height := h;
     setlength(pixels, height, width);
     for i := 0 to height - 1 do
         for j := 0 to width - 1 do
         begin
              pixels[i][j].R := 255;
              pixels[i][j].G := 255;
              pixels[i][j].B := 255;
    end;
end;

//для цвета карандаша
procedure T23030428Canvas.SetPencilColor(r, g, b: byte);
begin
     pencil.color.R := r;
     pencil.color.G := g;
     pencil.color.B := b;
end;

//получение цвета
function T23030428Canvas.GetPencilColor: TRGB;
begin
     result := pencil.color;
end;

//уст коорд
procedure T23030428Canvas.KoordPencil(x, y: integer);
begin
  //if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
  //begin
       pencil.X := x;
       pencil.Y := y;
  //end;
end;

//получение позиции
function T23030428Canvas.GetPencilX: integer;
begin
     result := pencil.X;
end;

function T23030428Canvas.GetPencilY: integer;
begin
     result := pencil.Y;
end;

procedure T23030428Canvas.SetPixelColor(x, y: integer; r, g, b: byte);
begin
  if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
  begin
       pixels[y][x].R := r;
       pixels[y][x].G := g;
       pixels[y][x].B := b;
  end;
end;

//рисование точки
procedure T23030428Canvas.DrawPixel;
begin
     SetPixelColor(pencil.X, pencil.Y, pencil.color.R, pencil.color.G, pencil.color.B);
end;


//сохр
procedure T23030428Canvas.SaveToPPM(const FName: string);
var
  f: text;
  i, j: integer;
begin
  assign(f, FName);
  rewrite(f);
  writeln(f, 'P3');
  writeln(f, width, ' ', height);
  writeln(f, '255');
  for i := 0 to height - 1 do
  begin
       for j := 0 to width - 1 do
       begin
            write(f, pixels[i][j].R, ' ', pixels[i][j].G, ' ', pixels[i][j].B, ' ');
       end;
       writeln(f);
  end;
  close(f);
end;

//загрузка
procedure T23030428Canvas.LoadFromPPM(const FName: string);
var
  f: text;
  header: string;
  maxColor: integer;
  i, j: integer;
begin
  assign(f, FName);
  reset(f);
  readln(f, header);
  try
     readln(f, width, height);
  except
     readln(f, header);
     readln(f, width, height);
  end;

  setlength(pixels, height, width);

  for i := 0 to height - 1 do
  begin
    for j := 0 to width - 1 do
        begin
             read(f, pixels[i][j].R, pixels[i][j].G, pixels[i][j].B);
        end;
  end;

  close(f);
end;

//вывод позиции
function T23030428Canvas.GetWidth: integer;
begin
     result := width;
end;

function T23030428Canvas.GetHeight: integer;
begin
     result := height;
end;

//2 block
//6
procedure T23030428Canvas.rectangle6(sh, d: integer);
var
   i, j, x, y: integer;
   color: TRGB;
begin
  x:= pencil.X;
  y:= pencil.Y;
  color := self.GetPencilColor;

   for i := 0 to d - 1 do
       SetPixelColor(x, y + i, color.R, color.G, color.B);

   for j := 0 to sh - 1 do
       SetPixelColor(x + j, y, color.R, color.G, color.B);
   for i := 0 to d - 1 do
       SetPixelColor(x + j, y + i, color.R, color.G, color.B);

   for j := 0 to sh - 1 do
       SetPixelColor(x + j, y + i, color.R, color.G, color.B);
end;

//7
procedure T23030428Canvas.rectangle7(sh, d: integer);
var
  i, j, x, y: integer;
  color: TRGB;
begin
  x:= pencil.X;
  y:= pencil.Y;
  color := self.GetPencilColor;

  for i := 0 to d - 1 do
    for j := 0 to sh - 1 do
      SetPixelColor(x + j, y + i, color.R, color.G, color.B);
end;

//8
procedure T23030428Canvas.segment(x2, y2: integer);
var
  x1, y1: integer;
  steps, i: integer;
  dx, dy: real;
  x, y: real;
  color: TRGB;
begin
  x1 := pencil.X;
  y1 := pencil.Y;
  color := GetPencilColor;

  dx := x2 - x1;
  dy := y2 - y1;

  if abs(dx) > abs(dy) then
    steps := abs(round(dx))
  else
    steps := abs(round(dy));

  if steps = 0 then
  begin
    SetPixelColor(x1, y1, color.R, color.G, color.B);
    exit;
  end;


  dx := dx / steps;
  dy := dy / steps;
  writeln(dx);
  halt(1);

  x := x1;
  y := y1;

  for i := 0 to steps do
  begin
    SetPixelColor(round(x), round(y), color.R, color.G, color.B);
    x := x + dx;
    y := y + dy;
  end;

  pencil.X := round(x - dx);
  pencil.Y := round(y - dy);
end;

//9
procedure T23030428Canvas.lim_area;
var
  color: TRGB;
  procedure rec_draw(x, y: integer);
  var
    color_tek: TRGB;
  begin
    if (x < 0) or (x >= width) or (y < 0) or (y>= height) then exit;

    color_tek := pixels[y][x];

    if (color_tek.R <> color.R) or (color_tek.G <> color.G) or (color_tek.B <> color.B) then exit;

    SetPixelColor(x, y, pencil.color.R, pencil.color.G, pencil.color.B);

    rec_draw(x + 1, y);
    rec_draw(x - 1, y);
    rec_draw(x, y + 1);
    rec_draw(x, y - 1);
  end;
begin
  color := pixels[pencil.Y][pencil.X];
  if (color.R = pencil.color.R) and (color.G = pencil.color.G) and (color.B = pencil.color.B) then exit;

  rec_draw(pencil.X, pencil.Y);
end;

//10
procedure T23030428Canvas.OverlayCanvas(canv: T23030428Canvas);
var
  i, j: integer;
  destX, destY: integer;
  srcColor: TRGB;
  prozr: TRGB;
begin
  prozr := self.pencil.color;

  for i := 0 to canv.GetHeight - 1 do
    for j := 0 to canv.GetWidth - 1 do
    begin
      srcColor := canv.pixels[i][j];
      if (srcColor.R = prozr.R) and
         (srcColor.G = prozr.G) and
         (srcColor.B = prozr.B) then
         continue;

      destX := pencil.X + j;
      destY := pencil.Y + i;

      if (destX >= 0) and (destX < self.width) and
         (destY >= 0) and (destY < self.height) then
      begin
        self.pixels[destY][destX] := srcColor;
      end;
    end;
end;

end.
