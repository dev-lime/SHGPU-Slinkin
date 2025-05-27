unit XCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
    FTransparentColor: TRGB;
    procedure SetSize(AWidth, AHeight: Integer);
    function GetPixel(X, Y: Integer): TRGB;
    procedure SetPixel(X, Y: Integer; const Value: TRGB);
    procedure FloodFillRecursive(X, Y: Integer; const OldColor, NewColor: TRGB);
    function SameColor(const C1, C2: TRGB): Boolean;
    procedure Swap(var A, B: Integer);
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
    procedure SetTransparentColor(R, G, B: Byte);

    // 4) Методы для изменения и получения текущей позиции
    procedure MoveTo(X, Y: Integer);
    function GetPosition: TPoint;

    // 5) Метод рисования точки в текущей позиции текущим цветом
    procedure DrawPoint;

    // 6) Метод рисования прямоугольника
    procedure DrawRect(Width, Height: Integer);

    // 7) Метод рисования закрашенного прямоугольника
    procedure DrawFilledRect(Width, Height: Integer);

    // 8) Метод рисования отрезка (алгоритм Брезенхема)
    procedure DrawLineTo(X, Y: Integer);

    // 9) Метод закраски ограниченной области
    procedure FloodFill;

    // 10) Метод накладывания стороннего холста
    procedure BlendCanvas(OtherCanvas: TXCanvas);

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
  FTransparentColor.R := 0;
  FTransparentColor.G := 0;
  FTransparentColor.B := 0;
end;

destructor TXCanvas.Destroy;
begin
  SetLength(FCanvas, 0, 0);
  inherited Destroy;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
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

function TXCanvas.SameColor(const C1, C2: TRGB): Boolean;
begin
  Result := (C1.R = C2.R) and (C1.G = C2.G) and (C1.B = C2.B);
end;

procedure TXCanvas.Swap(var A, B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

procedure TXCanvas.SaveToPPM(const FileName: string);
var
  F: TextFile;
  i, j: Integer;
  Line: string;
  BytesWritten: Integer;
const
  MaxLineLength = 70; // Максимальная длина строки в PPM P3
begin
  AssignFile(F, FileName);
  try
    Rewrite(F);

    // Записываем заголовок PPM
    WriteLn(F, 'P3');
    WriteLn(F, FWidth, ' ', FHeight);
    WriteLn(F, '255');

    // Записываем данные пикселей с ограничением длины строки
    for j := 0 to FHeight - 1 do
    begin
      Line := '';
      BytesWritten := 0;

      for i := 0 to FWidth - 1 do
      begin
        // Добавляем компоненты цвета
        if Line <> '' then
        begin
          Line := Line + ' ';
          Inc(BytesWritten);
        end;

        // Добавляем красную компоненту
        if BytesWritten + Length(IntToStr(FCanvas[j, i].R)) > MaxLineLength then
        begin
          WriteLn(F, Line);
          Line := '';
          BytesWritten := 0;
        end;
        Line := Line + IntToStr(FCanvas[j, i].R);
        Inc(BytesWritten, Length(IntToStr(FCanvas[j, i].R)));

        // Добавляем зеленую компоненту
        if BytesWritten + 1 + Length(IntToStr(FCanvas[j, i].G)) > MaxLineLength then
        begin
          WriteLn(F, Line);
          Line := '';
          BytesWritten := 0;
        end
        else if Line <> '' then
        begin
          Line := Line + ' ';
          Inc(BytesWritten);
        end;
        Line := Line + IntToStr(FCanvas[j, i].G);
        Inc(BytesWritten, Length(IntToStr(FCanvas[j, i].G)));

        // Добавляем синюю компоненту
        if BytesWritten + 1 + Length(IntToStr(FCanvas[j, i].B)) > MaxLineLength then
        begin
          WriteLn(F, Line);
          Line := '';
          BytesWritten := 0;
        end
        else if Line <> '' then
        begin
          Line := Line + ' ';
          Inc(BytesWritten);
        end;
        Line := Line + IntToStr(FCanvas[j, i].B);
        Inc(BytesWritten, Length(IntToStr(FCanvas[j, i].B)));
      end;

      // Записываем оставшиеся данные строки
      if Line <> '' then
        WriteLn(F, Line);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TXCanvas.LoadFromPPM(const FileName: string);
var
  F: TextFile;
  MagicNumber: string;
  Width, Height, MaxVal: Integer;
  i, j: Integer;
  R, G, B: Integer;
  Line: string;
  Tokens: TStringList;
  TokenIndex: Integer;
begin
  AssignFile(F, FileName);
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    Tokens.StrictDelimiter := True;

    Reset(F);

    // Читаем заголовок
    ReadLn(F, MagicNumber);
    if MagicNumber <> 'P3' then
      raise Exception.Create('Only PPM P3 format is supported');

    // Читаем размеры изображения
    ReadLn(F, Line);
    Tokens.DelimitedText := Line;
    if Tokens.Count <> 2 then
      raise Exception.Create('Invalid image dimensions');
    Width := StrToInt(Tokens[0]);
    Height := StrToInt(Tokens[1]);

    // Читаем максимальное значение цвета
    ReadLn(F, MaxVal);
    if MaxVal <> 255 then
      raise Exception.Create('Only 8-bit color depth is supported');

    // Устанавливаем размер холста
    SetSize(Width, Height);

    TokenIndex := 0;
    Tokens.Clear;

    // Читаем данные пикселей
    for j := 0 to Height - 1 do
    begin
      for i := 0 to Width - 1 do
      begin
        // Если токены закончились, читаем следующую строку
        if TokenIndex >= Tokens.Count then
        begin
          if Eof(F) then
            raise Exception.Create('Unexpected end of file');
          ReadLn(F, Line);
          Tokens.DelimitedText := Line;
          TokenIndex := 0;
        end;

        // Читаем компоненты цвета
        if TokenIndex + 2 >= Tokens.Count then
          raise Exception.Create('Invalid color data');

        R := StrToInt(Tokens[TokenIndex]);
        G := StrToInt(Tokens[TokenIndex+1]);
        B := StrToInt(Tokens[TokenIndex+2]);
        TokenIndex := TokenIndex + 3;

        // Проверяем диапазон значений
        if (R < 0) or (R > 255) or (G < 0) or (G > 255) or (B < 0) or (B > 255) then
          raise Exception.Create('Invalid color value');

        FCanvas[j, i].R := Byte(R);
        FCanvas[j, i].G := Byte(G);
        FCanvas[j, i].B := Byte(B);
      end;
    end;
  finally
    CloseFile(F);
    Tokens.Free;
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
  SetSize(NewWidth, NewHeight);

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

procedure TXCanvas.SetTransparentColor(R, G, B: Byte);
begin
  FTransparentColor.R := R;
  FTransparentColor.G := G;
  FTransparentColor.B := B;
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
  dx, dy, sx, sy, err, e2: Integer;
  x0, y0, x1, y1: Integer;
begin
  // Алгоритм Брезенхема для рисования линии
  x0 := FCurrentPos.X;
  y0 := FCurrentPos.Y;
  x1 := X;
  y1 := Y;

  dx := Abs(x1 - x0);
  dy := Abs(y1 - y0);

  if x0 < x1 then sx := 1 else sx := -1;
  if y0 < y1 then sy := 1 else sy := -1;

  err := dx - dy;

  while True do
  begin
    SetPixel(x0, y0, FCurrentColor);

    if (x0 = x1) and (y0 = y1) then Break;

    e2 := 2 * err;
    if e2 > -dy then
    begin
      err := err - dy;
      x0 := x0 + sx;
    end;

    if e2 < dx then
    begin
      err := err + dx;
      y0 := y0 + sy;
    end;
  end;

  FCurrentPos.X := X;
  FCurrentPos.Y := Y;
end;

procedure TXCanvas.FloodFill;
var
  OldColor: TRGB;
begin
  OldColor := GetPixel(FCurrentPos.X, FCurrentPos.Y);
  if SameColor(OldColor, FCurrentColor) then Exit;

  FloodFillRecursive(FCurrentPos.X, FCurrentPos.Y, OldColor, FCurrentColor);
end;

procedure TXCanvas.FloodFillRecursive(X, Y: Integer; const OldColor, NewColor: TRGB);
begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then Exit;

  if SameColor(FCanvas[Y, X], OldColor) then
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
      // Если цвет не равен прозрачному, то накладываем его
      if not SameColor(OtherColor, FTransparentColor) then
      begin
        SetPixel(FCurrentPos.X + i, FCurrentPos.Y + j, OtherColor);
      end;
    end;
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
