unit shapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, Math;

type

  TShapeClass = class of TVectorShape;

  TVectorShape = class
  private
    FX1, FY1, FX2, FY2: Integer;
    FPenColor: TColor;
    FPenWidth: Integer;
    FPenStyle: TPenStyle;
    FBrushColor: TColor;
    FBrushStyle: TBrushStyle;
    FAlpha: Byte;
    FZOrder: Integer;
    FSelected: Boolean;
  public
    constructor Create; virtual;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    function HitTest(X, Y: Integer): Boolean; virtual;
    procedure MoveTo(DX, DY: Integer); virtual;
    procedure SaveToStream(S: TStream); virtual;
    procedure LoadFromStream(S: TStream); virtual;
    function GetClassName: string; virtual; abstract;
    property X1: Integer read FX1 write FX1;
    property Y1: Integer read FY1 write FY1;
    property X2: Integer read FX2 write FX2;
    property Y2: Integer read FY2 write FY2;
    property PenColor: TColor read FPenColor write FPenColor;
    property PenWidth: Integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property Alpha: Byte read FAlpha write FAlpha;
    property ZOrder: Integer read FZOrder write FZOrder;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TLineShape = class(TVectorShape)
    procedure Draw(ACanvas: TCanvas); override;
    function GetClassName: string; override;
  end;

  TRectShape = class(TVectorShape)
    procedure Draw(ACanvas: TCanvas); override;
    function GetClassName: string; override;
  end;

  TEllipseShape = class(TVectorShape)
    procedure Draw(ACanvas: TCanvas); override;
    function GetClassName: string; override;
  end;

  TTriangleShape = class(TVectorShape)
    procedure Draw(ACanvas: TCanvas); override;
    function GetClassName: string; override;
  end;

implementation

{ TVectorShape }

constructor TVectorShape.Create;
begin
  FPenColor := clBlack;
  FPenWidth := 1;
  FPenStyle := psSolid;
  FBrushColor := clWhite;
  FBrushStyle := bsClear;
  FAlpha := 255;
  FZOrder := 0;
  FSelected := False;
end;

function TVectorShape.HitTest(X, Y: Integer): Boolean;
var
  MinX, MinY, MaxX, MaxY: Integer;
begin
  MinX := Min(FX1, FX2);
  MaxX := Max(FX1, FX2);
  MinY := Min(FY1, FY2);
  MaxY := Max(FY1, FY2);
  Result := (X >= MinX) and (X <= MaxX) and (Y >= MinY) and (Y <= MaxY);
end;

procedure TVectorShape.MoveTo(DX, DY: Integer);
begin
  Inc(FX1, DX);
  Inc(FY1, DY);
  Inc(FX2, DX);
  Inc(FY2, DY);
end;

procedure TVectorShape.SaveToStream(S: TStream);
var
  ShapeClassName: string;
  Len: Byte;
begin
  ShapeClassName := GetClassName;
  Len := Length(ShapeClassName);
  S.Write(Len, SizeOf(Len));
  S.Write(ShapeClassName[1], Len);
  S.Write(FX1, SizeOf(FX1));
  S.Write(FY1, SizeOf(FY1));
  S.Write(FX2, SizeOf(FX2));
  S.Write(FY2, SizeOf(FY2));
  S.Write(FPenColor, SizeOf(FPenColor));
  S.Write(FPenWidth, SizeOf(FPenWidth));
  S.Write(FPenStyle, SizeOf(FPenStyle));
  S.Write(FBrushColor, SizeOf(FBrushColor));
  S.Write(FBrushStyle, SizeOf(FBrushStyle));
  S.Write(FAlpha, SizeOf(FAlpha));
  S.Write(FZOrder, SizeOf(FZOrder));
end;

procedure TVectorShape.LoadFromStream(S: TStream);
var
  Len: Byte;
  ShapeClassName: string;
begin
  Len := 0;
  ShapeClassName := '';
  S.Read(Len, SizeOf(Len));
  SetLength(ShapeClassName, Len);
  S.Read(ShapeClassName[1], Len);
  S.Read(FX1, SizeOf(FX1));
  S.Read(FY1, SizeOf(FY1));
  S.Read(FX2, SizeOf(FX2));
  S.Read(FY2, SizeOf(FY2));
  S.Read(FPenColor, SizeOf(FPenColor));
  S.Read(FPenWidth, SizeOf(FPenWidth));
  S.Read(FPenStyle, SizeOf(FPenStyle));
  S.Read(FBrushColor, SizeOf(FBrushColor));
  S.Read(FBrushStyle, SizeOf(FBrushStyle));
  S.Read(FAlpha, SizeOf(FAlpha));
  S.Read(FZOrder, SizeOf(FZOrder));
end;

{ TLineShape }

function TLineShape.GetClassName: string;
begin
  Result := 'TLineShape';
end;

procedure TLineShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Width := FPenWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Brush.Style := bsClear;
  ACanvas.MoveTo(FX1, FY1);
  ACanvas.LineTo(FX2, FY2);
  if FSelected then
  begin
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(FX1 - 3, FY1 - 3, FX1 + 3, FY1 + 3);
    ACanvas.Rectangle(FX2 - 3, FY2 - 3, FX2 + 3, FY2 + 3);
  end;
end;

{ TRectShape }

function TRectShape.GetClassName: string;
begin
  Result := 'TRectShape';
end;

procedure TRectShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Width := FPenWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Rectangle(FX1, FY1, FX2, FY2);
  if FSelected then
  begin
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(FX1 - 3, FY1 - 3, FX1 + 3, FY1 + 3);
    ACanvas.Rectangle(FX2 - 3, FY2 - 3, FX2 + 3, FY2 + 3);
    ACanvas.Rectangle(FX1 - 3, FY2 - 3, FX1 + 3, FY2 + 3);
    ACanvas.Rectangle(FX2 - 3, FY1 - 3, FX2 + 3, FY1 + 3);
  end;
end;

{ TEllipseShape }

function TEllipseShape.GetClassName: string;
begin
  Result := 'TEllipseShape';
end;

procedure TEllipseShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Width := FPenWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Ellipse(FX1, FY1, FX2, FY2);
  if FSelected then
  begin
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(FX1 - 3, FY1 - 3, FX1 + 3, FY1 + 3);
    ACanvas.Rectangle(FX2 - 3, FY2 - 3, FX2 + 3, FY2 + 3);
  end;
end;

{ TTriangleShape }

function TTriangleShape.GetClassName: string;
begin
  Result := 'TTriangleShape';
end;

procedure TTriangleShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Width := FPenWidth;
  ACanvas.Pen.Style := FPenStyle;
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  ACanvas.Polygon([Point((FX1 + FX2) div 2, Min(FY1, FY2)),
                   Point(Max(FX1, FX2), Max(FY1, FY2)),
                   Point(Min(FX1, FX2), Max(FY1, FY2))]);
  if FSelected then
  begin
    ACanvas.Pen.Color := clBlue;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(((FX1 + FX2) div 2) - 3, Min(FY1, FY2) - 3,
                      ((FX1 + FX2) div 2) + 3, Min(FY1, FY2) + 3);
    ACanvas.Rectangle(Max(FX1, FX2) - 3, Max(FY1, FY2) - 3,
                      Max(FX1, FX2) + 3, Max(FY1, FY2) + 3);
    ACanvas.Rectangle(Min(FX1, FX2) - 3, Max(FY1, FY2) - 3,
                      Min(FX1, FX2) + 3, Max(FY1, FY2) + 3);
  end;
end;

end.
