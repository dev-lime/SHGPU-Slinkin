unit XCanvas;

{$mode objfpc}{$H+}

interface

uses
  SysUtils; // для работы с исключениями

type
  TRGB = record
    R: byte;
    G: byte;
    B: byte;
  end;

  TPencil = record
    X: integer;
    Y: integer;
    Color: TRGB;
  end;

  { TXCanvas }

  TXCanvas = class
  private
    FWidth, FHeight: integer;
    FPixels: array of array of TRGB;
    FPencil: TPencil;
    procedure ValidateCoordinates(X, Y: integer);
  public
    procedure Initialize(Width, Height: integer);
    procedure SetPixel(X, Y: integer; Color: TRGB);
    function GetPixel(X, Y: integer): TRGB;
    procedure SaveToPPM(const FileName: string);
    procedure LoadFromPPM(const FileName: string);

    procedure SetPencilColor(Color: TRGB); overload;
    procedure SetPencilColor(R, G, B: byte); overload;
    function GetPencilColor: TRGB;
    procedure MovePencilTo(X, Y: integer);
    procedure DrawDot;

    function GetWidth: integer;
    function GetHeight: integer;
    procedure Resize(NewWidth, NewHeight: integer);

    procedure DrawRectangle(Width, Height: integer);
    procedure DrawFilledRectangle(Width, Height: integer);
    procedure DrawLineTo(TargetX, TargetY: integer);
    procedure FloodFill;
    procedure DrawCanvas(Source: TXCanvas);
  end;

implementation

procedure TXCanvas.ValidateCoordinates(X, Y: integer);
begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
    raise EInOutError.Create('Coordinates are out of canvas bounds');
end;

procedure TXCanvas.Initialize(Width, Height: integer);
var
  I, J: integer;
begin
  FWidth := Width;
  FHeight := Height;
  SetLength(FPixels, FHeight, FWidth);

  // Заполнение белым цветом
  for I := 0 to FHeight - 1 do
    for J := 0 to FWidth - 1 do
    begin
      FPixels[I][J].R := 255;
      FPixels[I][J].G := 255;
      FPixels[I][J].B := 255;
    end;
end;

procedure TXCanvas.SetPixel(X, Y: integer; Color: TRGB);
begin
  ValidateCoordinates(X, Y);
  FPixels[Y][X] := Color;
end;

function TXCanvas.GetPixel(X, Y: integer): TRGB;
begin
  ValidateCoordinates(X, Y);
  Result := FPixels[Y][X];
end;

procedure TXCanvas.SaveToPPM(const FileName: string);
var
  F: TextFile;
  I, J: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'P3');
    WriteLn(F, FWidth, ' ', FHeight);
    WriteLn(F, '255');

    for I := 0 to FHeight - 1 do
    begin
      for J := 0 to FWidth - 1 do
      begin
        Write(F, FPixels[I][J].R, ' ', FPixels[I][J].G, ' ', FPixels[I][J].B, ' ');
      end;
      WriteLn(F);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TXCanvas.LoadFromPPM(const FileName: string);
var
  F: TextFile;
  Header: string;
  I, J: integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  try
    ReadLn(F, Header);
    if Header <> 'P3' then
      raise Exception.Create('Неверный формат PPM файла');

    ReadLn(F, FWidth, FHeight);
    Initialize(FWidth, FHeight);

    // Пропускает строку с максимальным значением цвета
    ReadLn(F, Header);

    for I := 0 to FHeight - 1 do
      for J := 0 to FWidth - 1 do
        Read(F, FPixels[I][J].R, FPixels[I][J].G, FPixels[I][J].B);
  finally
    CloseFile(F);
  end;
end;

procedure TXCanvas.SetPencilColor(Color: TRGB);
begin
  FPencil.Color := Color;
end;

procedure TXCanvas.SetPencilColor(R, G, B: byte);
begin
  FPencil.Color.R := R;
  FPencil.Color.G := G;
  FPencil.Color.B := B;
end;

function TXCanvas.GetPencilColor: TRGB;
begin
  Result := FPencil.Color;
end;

procedure TXCanvas.MovePencilTo(X, Y: integer);
begin
  ValidateCoordinates(X, Y);
  FPencil.X := X;
  FPencil.Y := Y;
end;

procedure TXCanvas.DrawDot;
begin
  SetPixel(FPencil.X, FPencil.Y, FPencil.Color);
end;

function TXCanvas.GetWidth: integer;
begin
  Result := FWidth;
end;

function TXCanvas.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TXCanvas.Resize(NewWidth, NewHeight: integer);
begin
  Initialize(NewWidth, NewHeight);
end;

procedure TXCanvas.DrawRectangle(Width, Height: integer);
var
  I: integer;
begin
  // Верхняя и нижняя линии
  for I := 0 to Width - 1 do
  begin
    SetPixel(FPencil.X + I, FPencil.Y, FPencil.Color);
    SetPixel(FPencil.X + I, FPencil.Y + Height - 1, FPencil.Color);
  end;

  // Боковые линии
  for I := 1 to Height - 2 do
  begin
    SetPixel(FPencil.X, FPencil.Y + I, FPencil.Color);
    SetPixel(FPencil.X + Width - 1, FPencil.Y + I, FPencil.Color);
  end;
end;

procedure TXCanvas.DrawFilledRectangle(Width, Height: integer);
var
  I, J: integer;
begin
  for I := 0 to Height - 1 do
    for J := 0 to Width - 1 do
      SetPixel(FPencil.X + J, FPencil.Y + I, FPencil.Color);
end;

procedure TXCanvas.DrawLineTo(TargetX, TargetY: integer);
var
  X1, Y1, X2, Y2: integer;
  Dx, Dy, Step: integer;
  XInc, YInc: double;
  X, Y: double;
  I: integer;
begin
  X1 := FPencil.X;
  Y1 := FPencil.Y;
  X2 := TargetX;
  Y2 := TargetY;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Abs(Dx) > Abs(Dy) then
    Step := Abs(Dx)
  else
    Step := Abs(Dy);

  XInc := Dx / Step;
  YInc := Dy / Step;

  X := X1;
  Y := Y1;

  for I := 0 to Step do
  begin
    SetPixel(Round(X), Round(Y), FPencil.Color);
    X := X + XInc;
    Y := Y + YInc;
  end;

  MovePencilTo(TargetX, TargetY);
end;

procedure TXCanvas.FloodFill;
var
  TargetColor: TRGB;

  procedure Fill(X, Y: integer);
  begin
    if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
      Exit;

    if (FPixels[Y][X].R = TargetColor.R) and
       (FPixels[Y][X].G = TargetColor.G) and
       (FPixels[Y][X].B = TargetColor.B) then
    begin
      SetPixel(X, Y, FPencil.Color);
      Fill(X + 1, Y);
      Fill(X - 1, Y);
      Fill(X, Y + 1);
      Fill(X, Y - 1);
    end;
  end;

begin
  TargetColor := GetPixel(FPencil.X, FPencil.Y);

  if (TargetColor.R = FPencil.Color.R) and
     (TargetColor.G = FPencil.Color.G) and
     (TargetColor.B = FPencil.Color.B) then
    Exit;

  Fill(FPencil.X, FPencil.Y);
end;

procedure TXCanvas.DrawCanvas(Source: TXCanvas);
var
  I, J: integer;
  SourceColor: TRGB;
begin
  for I := 0 to Source.GetHeight - 1 do
    for J := 0 to Source.GetWidth - 1 do
    begin
      SourceColor := Source.GetPixel(J, I);

      // Пропускаем пиксели совпадающие с текущим цветом карандаша
      if (SourceColor.R = FPencil.Color.R) and
         (SourceColor.G = FPencil.Color.G) and
         (SourceColor.B = FPencil.Color.B) then
        Continue;

      if (FPencil.X + J < FWidth) and (FPencil.Y + I < FHeight) then
        SetPixel(FPencil.X + J, FPencil.Y + I, SourceColor);
    end;
end;

end.

