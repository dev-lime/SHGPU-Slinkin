unit XCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPWritePPM, FPReadPPM, FPImgCanv, Graphics;

type
  // Тип для хранения цвета пикселя
  TRGB = record
    R, G, B: Byte;
  end;
  PRGB = ^TRGB;

  // Тип для хранения позиции
  TPoint = record
    X, Y: Integer;
  end;

  { TXCanvas }

  TXCanvas = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FCanvas: array of array of TRGB;
    FCurrentColor: TRGB;
    FCurrentPos: TPoint;
    procedure SetSize(AWidth, AHeight: Integer);
    function GetPixel(X, Y: Integer): TRGB;
    procedure SetPixel(X, Y: Integer; const Value: TRGB);
    procedure FloodFillRecursive(X, Y: Integer; const OldColor, NewColor: TRGB);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    // 1) Методы загрузки и сохранения PPM P3
    procedure SaveToPPM(const FileName: string);
    procedure LoadFromPPM(const FileName: string);

    // 2) Методы для изменения и получения размеров холста
    procedure Resize(NewWidth, NewHeight: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;

    // 3) Методы для изменения и получения текущего цвета
    procedure SetColor(R, G, B: Byte);
    function GetColor: TRGB;

    // 4) Методы для изменения и получения текущей позиции
    procedure MoveTo(X, Y: Integer);
    function GetPosition: TPoint;

    // 5) Метод рисования точки в текущей позиции текущим цветом
    procedure DrawPoint;

    // 6) Метод рисования прямоугольника
    procedure DrawRect(Width, Height: Integer);

    // 7) Метод рисования закрашенного прямоугольника
    procedure DrawFilledRect(Width, Height: Integer);

    // 8) Метод рисования отрезка
    procedure DrawLineTo(X, Y: Integer);

    // 9) Метод закраски ограниченной области
    procedure FloodFill;

    // 10) Метод накладывания стороннего холста
    procedure BlendCanvas(OtherCanvas: TXCanvas);

    // 11) Методы загрузки и сохранения в других форматах
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    // Вспомогательные методы
    procedure Clear;
    procedure ClearWithColor(R, G, B: Byte);
  end;

implementation

{ TXCanvas }

constructor TXCanvas.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  SetSize(AWidth, AHeight);
  FCurrentColor.R := 0;
  FCurrentColor.G := 0;
  FCurrentColor.B := 0;
  FCurrentPos.X := 0;
  FCurrentPos.Y := 0;
end;

destructor TXCanvas.Destroy;
begin
  SetLength(FCanvas, 0, 0);
  inherited Destroy;
end;

procedure TXCanvas.SetSize(AWidth, AHeight: Integer);
var
  i, j: Integer;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    raise Exception.Create('Invalid canvas size');

  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FCanvas, FHeight, FWidth);

  // Инициализация холста белым цветом
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
    begin
      FCanvas[j, i].R := 255;
      FCanvas[j, i].G := 255;
      FCanvas[j, i].B := 255;
    end;
end;

function TXCanvas.GetPixel(X, Y: Integer): TRGB;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := FCanvas[Y, X]
  else
    raise Exception.Create('Pixel coordinates out of bounds');
end;

procedure TXCanvas.SetPixel(X, Y: Integer; const Value: TRGB);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    FCanvas[Y, X] := Value
  else
    raise Exception.Create('Pixel coordinates out of bounds');
end;

procedure TXCanvas.SaveToPPM(const FileName: string);
var
  PPMFile: TextFile;
  i, j: Integer;
begin
  AssignFile(PPMFile, FileName);
  try
    Rewrite(PPMFile);
    // Записываем заголовок PPM P3
    WriteLn(PPMFile, 'P3');
    WriteLn(PPMFile, FWidth, ' ', FHeight);
    WriteLn(PPMFile, '255');

    // Записываем данные пикселей
    for j := 0 to FHeight - 1 do
    begin
      for i := 0 to FWidth - 1 do
      begin
        Write(PPMFile, FCanvas[j, i].R, ' ', FCanvas[j, i].G, ' ', FCanvas[j, i].B, ' ');
      end;
      WriteLn(PPMFile);
    end;
  finally
    CloseFile(PPMFile);
  end;
end;

procedure TXCanvas.LoadFromPPM(const FileName: string);
var
  PPMFile: TextFile;
  MagicNumber: string;
  MaxVal: Integer;
  i, j: Integer;
  R, G, B: Integer;
begin
  AssignFile(PPMFile, FileName);
  try
    Reset(PPMFile);
    // Читаем заголовок
    ReadLn(PPMFile, MagicNumber);
    if MagicNumber <> 'P3' then
      raise Exception.Create('Only PPM P3 format is supported');

    ReadLn(PPMFile, FWidth, FHeight);
    ReadLn(PPMFile, MaxVal);

    SetSize(FWidth, FHeight);

    // Читаем данные пикселей
    for j := 0 to FHeight - 1 do
    begin
      for i := 0 to FWidth - 1 do
      begin
        Read(PPMFile, R, G, B);
        FCanvas[j, i].R := Byte(R);
        FCanvas[j, i].G := Byte(G);
        FCanvas[j, i].B := Byte(B);
      end;
    end;
  finally
    CloseFile(PPMFile);
  end;
end;

procedure TXCanvas.Resize(NewWidth, NewHeight: Integer);
var
  OldCanvas: array of array of TRGB;
  i, j: Integer;
begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) then Exit;

  // Сохраняем старый холст
  SetLength(OldCanvas, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
      OldCanvas[j, i] := FCanvas[j, i];

  // Устанавливаем новый размер
  SetSize(NewWidth, FHeight);

  // Копируем данные из старого холста
  for j := 0 to Min(FHeight, NewHeight) - 1 do
    for i := 0 to Min(FWidth, NewWidth) - 1 do
      FCanvas[j, i] := OldCanvas[j, i];

  SetLength(OldCanvas, 0, 0);
end;

function TXCanvas.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TXCanvas.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TXCanvas.SetColor(R, G, B: Byte);
begin
  FCurrentColor.R := R;
  FCurrentColor.G := G;
  FCurrentColor.B := B;
end;

function TXCanvas.GetColor: TRGB;
begin
  Result := FCurrentColor;
end;

procedure TXCanvas.MoveTo(X, Y: Integer);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    FCurrentPos.X := X;
    FCurrentPos.Y := Y;
  end
  else
    raise Exception.Create('Position out of canvas bounds');
end;

function TXCanvas.GetPosition: TPoint;
begin
  Result := FCurrentPos;
end;

procedure TXCanvas.DrawPoint;
begin
  SetPixel(FCurrentPos.X, FCurrentPos.Y, FCurrentColor);
end;

procedure TXCanvas.DrawRect(Width, Height: Integer);
var
  i: Integer;
begin
  // Верхняя и нижняя границы
  for i := 0 to Width - 1 do
  begin
    SetPixel(FCurrentPos.X + i, FCurrentPos.Y, FCurrentColor);
    SetPixel(FCurrentPos.X + i, FCurrentPos.Y + Height - 1, FCurrentColor);
  end;

  // Левая и правая границы
  for i := 0 to Height - 1 do
  begin
    SetPixel(FCurrentPos.X, FCurrentPos.Y + i, FCurrentColor);
    SetPixel(FCurrentPos.X + Width - 1, FCurrentPos.Y + i, FCurrentColor);
  end;
end;

procedure TXCanvas.DrawFilledRect(Width, Height: Integer);
var
  i, j: Integer;
begin
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      SetPixel(FCurrentPos.X + i, FCurrentPos.Y + j, FCurrentColor);
end;

procedure TXCanvas.DrawLineTo(X, Y: Integer);
var
  dx, dy, step, i: Integer;
  xInc, yInc, xCur, yCur: Double;
begin
  dx := X - FCurrentPos.X;
  dy := Y - FCurrentPos.Y;

  if Abs(dx) > Abs(dy) then
    step := Abs(dx)
  else
    step := Abs(dy);

  if step = 0 then
  begin
    SetPixel(FCurrentPos.X, FCurrentPos.Y, FCurrentColor);
    Exit;
  end;

  xInc := dx / step;
  yInc := dy / step;
  xCur := FCurrentPos.X;
  yCur := FCurrentPos.Y;

  for i := 0 to step do
  begin
    SetPixel(Round(xCur), Round(yCur), FCurrentColor);
    xCur := xCur + xInc;
    yCur := yCur + yInc;
  end;

  FCurrentPos.X := X;
  FCurrentPos.Y := Y;
end;

procedure TXCanvas.FloodFill;
var
  OldColor: TRGB;
begin
  OldColor := GetPixel(FCurrentPos.X, FCurrentPos.Y);
  if (OldColor.R = FCurrentColor.R) and
     (OldColor.G = FCurrentColor.G) and
     (OldColor.B = FCurrentColor.B) then
    Exit;

  FloodFillRecursive(FCurrentPos.X, FCurrentPos.Y, OldColor, FCurrentColor);
end;

procedure TXCanvas.FloodFillRecursive(X, Y: Integer; const OldColor, NewColor: TRGB);
begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
    Exit;

  if (FCanvas[Y, X].R = OldColor.R) and
     (FCanvas[Y, X].G = OldColor.G) and
     (FCanvas[Y, X].B = OldColor.B) then
  begin
    FCanvas[Y, X] := NewColor;
    FloodFillRecursive(X + 1, Y, OldColor, NewColor);
    FloodFillRecursive(X - 1, Y, OldColor, NewColor);
    FloodFillRecursive(X, Y + 1, OldColor, NewColor);
    FloodFillRecursive(X, Y - 1, OldColor, NewColor);
  end;
end;

procedure TXCanvas.BlendCanvas(OtherCanvas: TXCanvas);
var
  i, j: Integer;
  OtherColor: TRGB;
begin
  for j := 0 to Min(FHeight, OtherCanvas.FHeight) - 1 do
  begin
    for i := 0 to Min(FWidth, OtherCanvas.FWidth) - 1 do
    begin
      OtherColor := OtherCanvas.GetPixel(i, j);
      // Если цвет не равен текущему (прозрачному), то накладываем его
      if not ((OtherColor.R = FCurrentColor.R) and
              (OtherColor.G = FCurrentColor.G) and
              (OtherColor.B = FCurrentColor.B)) then
      begin
        SetPixel(FCurrentPos.X + i, FCurrentPos.Y + j, OtherColor);
      end;
    end;
  end;
end;

procedure TXCanvas.SaveToFile(const FileName: string);
var
  Image: TFPMemoryImage;
  Writer: TFPCustomImageWriter;
  Ext: string;
  i, j: Integer;
  Col: TFPColor;
begin
  Image := TFPMemoryImage.Create(FWidth, FHeight);
  try
    // Конвертируем наше изображение в формат TFPMemoryImage
    for j := 0 to FHeight - 1 do
    begin
      for i := 0 to FWidth - 1 do
      begin
        Col.Red := FCanvas[j, i].R * 256;
        Col.Green := FCanvas[j, i].G * 256;
        Col.Blue := FCanvas[j, i].B * 256;
        Col.Alpha := $FFFF;
        Image.Colors[i, j] := Col;
      end;
    end;

    // Определяем формат по расширению файла
    Ext := LowerCase(ExtractFileExt(FileName));
    if Ext = '.jpg' then
      Writer := TFPWriterJPEG.Create
    else if Ext = '.png' then
      Writer := TFPWriterPNG.Create
    else if Ext = '.pcx' then
      Writer := TFPWriterPCX.Create
    else if Ext = '.ppm' then
      Writer := TFPWriterPPM.Create
    else
      raise Exception.Create('Unsupported file format');

    try
      Image.SaveToFile(FileName, Writer);
    finally
      Writer.Free;
    end;
  finally
    Image.Free;
  end;
end;

procedure TXCanvas.LoadFromFile(const FileName: string);
var
  Image: TFPMemoryImage;
  Reader: TFPCustomImageReader;
  Ext: string;
  i, j: Integer;
  Col: TFPColor;
begin
  Image := TFPMemoryImage.Create(0, 0);
  try
    // Определяем формат по расширению файла
    Ext := LowerCase(ExtractFileExt(FileName));
    if Ext = '.jpg' then
      Reader := TFPReaderJPEG.Create
    else if Ext = '.png' then
      Reader := TFPReaderPNG.Create
    else if Ext = '.pcx' then
      Reader := TFPReaderPCX.Create
    else if Ext = '.ppm' then
      Reader := TFPReaderPPM.Create
    else
      raise Exception.Create('Unsupported file format');

    try
      Image.LoadFromFile(FileName, Reader);
    finally
      Reader.Free;
    end;

    // Устанавливаем размер холста
    SetSize(Image.Width, Image.Height);

    // Конвертируем изображение в наш формат
    for j := 0 to FHeight - 1 do
    begin
      for i := 0 to FWidth - 1 do
      begin
        Col := Image.Colors[i, j];
        FCanvas[j, i].R := Col.Red div 256;
        FCanvas[j, i].G := Col.Green div 256;
        FCanvas[j, i].B := Col.Blue div 256;
      end;
    end;
  finally
    Image.Free;
  end;
end;

procedure TXCanvas.Clear;
begin
  ClearWithColor(255, 255, 255);
end;

procedure TXCanvas.ClearWithColor(R, G, B: Byte);
var
  i, j: Integer;
begin
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
    begin
      FCanvas[j, i].R := R;
      FCanvas[j, i].G := G;
      FCanvas[j, i].B := B;
    end;
end;

end.

