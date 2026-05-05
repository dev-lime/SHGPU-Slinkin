unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, Menus, ColorBox, Spin, Math;

type
  TShapeType = (stNone, stPoint, stLine, stRectangle, stCircle, stEllipse, stSelect);

  TVectorShape = class
  private
    FShapeType: TShapeType;
    FX1, FY1, FX2, FY2: Integer;
    FColor: TColor;
    FLineWidth: Integer;
  public
    constructor Create(AShapeType: TShapeType; X1, Y1, X2, Y2: Integer; AColor: TColor; AWidth: Integer);
    procedure Draw(Canvas: TCanvas);
    function ContainsPoint(X, Y: Integer): Boolean;
    property ShapeType: TShapeType read FShapeType;
    property X1: Integer read FX1 write FX1;
    property Y1: Integer read FY1 write FY1;
    property X2: Integer read FX2 write FX2;
    property Y2: Integer read FY2 write FY2;
    property Color: TColor read FColor write FColor;
    property LineWidth: Integer read FLineWidth write FLineWidth;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miExit: TMenuItem;
    EditMenu: TMenuItem;
    miUndo: TMenuItem;
    miDelete: TMenuItem;
    miSelectAll: TMenuItem;
    PaintBox: TPaintBox;
    PanelTools: TPanel;
    StatusBar: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ColorBox1: TColorBox;
    lblColor: TLabel;
    lblWidth: TLabel;
    spinWidth: TSpinEdit;
    btnPoint: TSpeedButton;
    btnLine: TSpeedButton;
    btnRect: TSpeedButton;
    btnCircle: TSpeedButton;
    btnEllipse: TSpeedButton;
    btnSelect: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure btnPointClick(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
    procedure btnRectClick(Sender: TObject);
    procedure btnCircleClick(Sender: TObject);
    procedure btnEllipseClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  private
    FShapes: TList;
    FSelected: TList;
    FCurrentTool: TShapeType;
    FIsDrawing: Boolean;
    FStartX, FStartY: Integer;
    FEndX, FEndY: Integer;
    FDragShape: TVectorShape;
    FDragOffsetX, FDragOffsetY: Integer;
    FIsDragging: Boolean;
    FSelectionRect: Boolean;
    FSelStartX, FSelStartY, FSelEndX, FSelEndY: Integer;
    procedure SetTool(ShapeType: TShapeType);
    procedure DrawAll;
    procedure DrawPreview(Canvas: TCanvas);
    procedure DrawSelection;
    function FindShapeAt(X, Y: Integer): TVectorShape;
    procedure AddShape(Shape: TVectorShape);
    procedure UpdateStatus(const X, Y: Integer);
    procedure ClearSelection;
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TVectorShape }

constructor TVectorShape.Create(AShapeType: TShapeType; X1, Y1, X2, Y2: Integer; AColor: TColor; AWidth: Integer);
begin
  inherited Create;
  FShapeType := AShapeType;
  FX1 := X1; FY1 := Y1; FX2 := X2; FY2 := Y2;
  FColor := AColor;
  FLineWidth := AWidth;
end;

procedure TVectorShape.Draw(Canvas: TCanvas);
var
  CX, CY, R: Integer;
begin
  Canvas.Pen.Color := FColor;
  Canvas.Pen.Width := FLineWidth;
  Canvas.Brush.Style := bsClear;

  case FShapeType of
    stPoint:
      begin
        Canvas.Pixels[FX1, FY1] := FColor;
      end;
    stLine:
      begin
        Canvas.MoveTo(FX1, FY1);
        Canvas.LineTo(FX2, FY2);
      end;
    stRectangle:
      begin
        Canvas.Rectangle(Min(FX1, FX2), Min(FY1, FY2), Max(FX1, FX2), Max(FY1, FY2));
      end;
    stCircle:
      begin
        CX := (FX1 + FX2) div 2;
        CY := (FY1 + FY2) div 2;
        R := Trunc(Sqrt(Sqr(FX2 - FX1) + Sqr(FY2 - FY1)) / 2);
        Canvas.Ellipse(CX - R, CY - R, CX + R, CY + R);
      end;
    stEllipse:
      begin
        Canvas.Ellipse(Min(FX1, FX2), Min(FY1, FY2), Max(FX1, FX2), Max(FY1, FY2));
      end;
  end;
end;

function TVectorShape.ContainsPoint(X, Y: Integer): Boolean;
const
  THRESHOLD = 5;
var
  CX, CY, R, Dist: Integer;
  Len, Dist1, Dist2, Diff: Double;
begin
  Result := False;
  case FShapeType of
    stPoint:
      Result := (Abs(X - FX1) <= THRESHOLD) and (Abs(Y - FY1) <= THRESHOLD);
    stLine:
      begin
        Len := Sqrt(Sqr(FX2 - FX1) + Sqr(FY2 - FY1));
        if Len < 1 then Exit;
        Dist1 := Sqrt(Sqr(X - FX1) + Sqr(Y - FY1));
        Dist2 := Sqrt(Sqr(X - FX2) + Sqr(Y - FY2));
        Diff := Abs(Dist1 + Dist2 - Len);
        Result := Diff <= THRESHOLD;
      end;
    stRectangle, stEllipse:
      begin
        Result := (X >= Min(FX1, FX2)) and (X <= Max(FX1, FX2)) and
                  (Y >= Min(FY1, FY2)) and (Y <= Max(FY1, FY2));
      end;
    stCircle:
      begin
        CX := (FX1 + FX2) div 2;
        CY := (FY1 + FY2) div 2;
        R := Trunc(Sqrt(Sqr(FX2 - FX1) + Sqr(FY2 - FY1)) / 2);
        Dist := Trunc(Sqrt(Sqr(X - CX) + Sqr(Y - CY)));
        Result := Abs(Dist - R) <= THRESHOLD + FLineWidth;
      end;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FShapes := TList.Create;
  FSelected := TList.Create;
  FCurrentTool := stLine;
  FIsDrawing := False;
  FDragShape := nil;
  FIsDragging := False;
  FSelectionRect := False;

  btnLine.Down := True;
  ColorBox1.Selected := clBlack;
  spinWidth.Value := 2;

  UpdateStatus(0, 0);
end;

destructor TForm1.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    TVectorShape(FShapes[i]).Free;
  FShapes.Free;
  FSelected.Free;
  inherited Destroy;
end;

procedure TForm1.SetTool(ShapeType: TShapeType);
begin
  FCurrentTool := ShapeType;
  ClearSelection;
  btnPoint.Down := ShapeType = stPoint;
  btnLine.Down := ShapeType = stLine;
  btnRect.Down := ShapeType = stRectangle;
  btnCircle.Down := ShapeType = stCircle;
  btnEllipse.Down := ShapeType = stEllipse;
  btnSelect.Down := ShapeType = stSelect;
  case ShapeType of
    stPoint: StatusBar.Panels[1].Text := 'Точка';
    stLine: StatusBar.Panels[1].Text := 'Линия';
    stRectangle: StatusBar.Panels[1].Text := 'Прямоугольник';
    stCircle: StatusBar.Panels[1].Text := 'Окружность';
    stEllipse: StatusBar.Panels[1].Text := 'Эллипс';
    stSelect: StatusBar.Panels[1].Text := 'Выделение';
  end;
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FStartX := X;
    FStartY := Y;
    FEndX := X;
    FEndY := Y;

    if FCurrentTool = stSelect then
    begin
      FDragShape := FindShapeAt(X, Y);
      if FDragShape <> nil then
      begin
        FIsDragging := True;
        FDragOffsetX := X - FDragShape.X1;
        FDragOffsetY := Y - FDragShape.Y1;
      end
      else
      begin
        FSelectionRect := True;
        FSelStartX := X;
        FSelStartY := Y;
      end;
    end
    else
    begin
      FIsDrawing := True;
    end;
  end;
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  DX, DY, DX2, DY2: Integer;
begin
  UpdateStatus(X, Y);

  if FIsDrawing then
  begin
    FEndX := X;
    FEndY := Y;
    PaintBox.Invalidate;
  end;

  if FIsDragging and (FDragShape <> nil) then
  begin
    DX := X - FDragOffsetX - FDragShape.X1;
    DY := Y - FDragOffsetY - FDragShape.Y1;
    DX2 := X - FDragOffsetX - FDragShape.X2;
    DY2 := Y - FDragOffsetY - FDragShape.Y2;

    FDragShape.X1 := FDragShape.X1 + DX;
    FDragShape.Y1 := FDragShape.Y1 + DY;
    FDragShape.X2 := FDragShape.X2 + DX2;
    FDragShape.Y2 := FDragShape.Y2 + DY2;

    FDragOffsetX := X - FDragShape.X1;
    FDragOffsetY := Y - FDragShape.Y1;

    PaintBox.Invalidate;
  end;

  if FSelectionRect then
  begin
    FSelEndX := X;
    FSelEndY := Y;
    PaintBox.Invalidate;
  end;
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewShape: TVectorShape;
  i: Integer;
  Shape: TVectorShape;
  MinX, MaxX, MinY, MaxY: Integer;
begin
  if FIsDrawing then
  begin
    FIsDrawing := False;

    if (Abs(FEndX - FStartX) > 2) or (Abs(FEndY - FStartY) > 2) then
    begin
      NewShape := TVectorShape.Create(FCurrentTool, FStartX, FStartY, FEndX, FEndY,
                                       ColorBox1.Selected, spinWidth.Value);
      AddShape(NewShape);
    end;

    PaintBox.Invalidate;
  end;

  if FIsDragging and (FDragShape <> nil) then
  begin
    FIsDragging := False;
    FDragShape := nil;
  end;

  if FSelectionRect then
  begin
    FSelectionRect := False;

    ClearSelection;

    MinX := Min(FSelStartX, FSelEndX);
    MaxX := Max(FSelStartX, FSelEndX);
    MinY := Min(FSelStartY, FSelEndY);
    MaxY := Max(FSelStartY, FSelEndY);

    if (Abs(MaxX - MinX) < 5) and (Abs(MaxY - MinY) < 5) then
    begin
      Shape := FindShapeAt(X, Y);
      if Shape <> nil then
        FSelected.Add(Shape);
    end
    else
    begin
      for i := 0 to FShapes.Count - 1 do
      begin
        Shape := TVectorShape(FShapes[i]);
        if (Shape.X1 >= MinX) and (Shape.X1 <= MaxX) and
           (Shape.Y1 >= MinY) and (Shape.Y1 <= MaxY) and
           (Shape.X2 >= MinX) and (Shape.X2 <= MaxX) and
           (Shape.Y2 >= MinY) and (Shape.Y2 <= MaxY) then
          FSelected.Add(Shape);
      end;
    end;

    PaintBox.Invalidate;
  end;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
begin
  DrawAll;
end;

procedure TForm1.btnPointClick(Sender: TObject);
begin
  SetTool(stPoint);
end;

procedure TForm1.btnLineClick(Sender: TObject);
begin
  SetTool(stLine);
end;

procedure TForm1.btnRectClick(Sender: TObject);
begin
  SetTool(stRectangle);
end;

procedure TForm1.btnCircleClick(Sender: TObject);
begin
  SetTool(stCircle);
end;

procedure TForm1.btnEllipseClick(Sender: TObject);
begin
  SetTool(stEllipse);
end;

procedure TForm1.btnSelectClick(Sender: TObject);
begin
  SetTool(stSelect);
end;

procedure TForm1.miNewClick(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg('Создать новый документ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    for i := 0 to FShapes.Count - 1 do
      TVectorShape(FShapes[i]).Free;
    FShapes.Clear;
    ClearSelection;
    PaintBox.Invalidate;
    StatusBar.Panels[0].Text := 'Новый документ';
  end;
end;

procedure TForm1.miSaveClick(Sender: TObject);
var
  F: TextFile;
  i: Integer;
  Shape: TVectorShape;
begin
  if SaveDialog1.Execute then
  begin
    AssignFile(F, SaveDialog1.FileName);
    Rewrite(F);
    Writeln(F, FShapes.Count);
    for i := 0 to FShapes.Count - 1 do
    begin
      Shape := TVectorShape(FShapes[i]);
      Writeln(F, Integer(Shape.ShapeType), ' ', Shape.X1, ' ', Shape.Y1, ' ',
              Shape.X2, ' ', Shape.Y2, ' ', Integer(Shape.Color), ' ', Shape.LineWidth);
    end;
    CloseFile(F);
    StatusBar.Panels[0].Text := 'Сохранено: ' + SaveDialog1.FileName;
  end;
end;

procedure TForm1.miOpenClick(Sender: TObject);
var
  F: TextFile;
  i, Count: Integer;
  ShapeTypeInt, X1, Y1, X2, Y2, ColorInt, Width: Integer;
  Shape: TVectorShape;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to FShapes.Count - 1 do
      TVectorShape(FShapes[i]).Free;
    FShapes.Clear;
    ClearSelection;

    AssignFile(F, OpenDialog1.FileName);
    Reset(F);
    Readln(F, Count);
    for i := 0 to Count - 1 do
    begin
      Readln(F, ShapeTypeInt, X1, Y1, X2, Y2, ColorInt, Width);
      Shape := TVectorShape.Create(TShapeType(ShapeTypeInt), X1, Y1, X2, Y2, TColor(ColorInt), Width);
      FShapes.Add(Shape);
    end;
    CloseFile(F);
    PaintBox.Invalidate;
    StatusBar.Panels[0].Text := 'Открыто: ' + OpenDialog1.FileName;
  end;
end;

procedure TForm1.miUndoClick(Sender: TObject);
begin
  if FShapes.Count > 0 then
  begin
    TVectorShape(FShapes[FShapes.Count - 1]).Free;
    FShapes.Delete(FShapes.Count - 1);
    ClearSelection;
    PaintBox.Invalidate;
    StatusBar.Panels[0].Text := 'Отмена действия';
  end;
end;

procedure TForm1.miDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if FSelected.Count > 0 then
  begin
    for i := FShapes.Count - 1 downto 0 do
    begin
      if FSelected.IndexOf(FShapes[i]) >= 0 then
      begin
        TVectorShape(FShapes[i]).Free;
        FShapes.Delete(i);
      end;
    end;
    ClearSelection;
    PaintBox.Invalidate;
    StatusBar.Panels[0].Text := 'Удалено выделенных';
  end;
end;

procedure TForm1.miSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  ClearSelection;
  for i := 0 to FShapes.Count - 1 do
    FSelected.Add(FShapes[i]);
  PaintBox.Invalidate;
  StatusBar.Panels[0].Text := 'Выделено: ' + IntToStr(FShapes.Count);
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.DrawAll;
var
  i: Integer;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(PaintBox.ClientRect);

  for i := 0 to FShapes.Count - 1 do
    TVectorShape(FShapes[i]).Draw(PaintBox.Canvas);

  DrawSelection;

  if FIsDrawing then
    DrawPreview(PaintBox.Canvas);

  if FSelectionRect then
  begin
    PaintBox.Canvas.Pen.Color := clBlue;
    PaintBox.Canvas.Pen.Style := psDash;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Rectangle(Min(FSelStartX, FSelEndX), Min(FSelStartY, FSelEndY),
                               Max(FSelStartX, FSelEndX), Max(FSelStartY, FSelEndY));
  end;
end;

procedure TForm1.DrawPreview(Canvas: TCanvas);
var
  PreviewShape: TVectorShape;
begin
  PreviewShape := TVectorShape.Create(FCurrentTool, FStartX, FStartY, FEndX, FEndY,
                                       ColorBox1.Selected, spinWidth.Value);
  Canvas.Pen.Style := psDash;
  PreviewShape.Draw(Canvas);
  Canvas.Pen.Style := psSolid;
  PreviewShape.Free;
end;

procedure TForm1.DrawSelection;
var
  i: Integer;
  Shape: TVectorShape;
begin
  for i := 0 to FSelected.Count - 1 do
  begin
    Shape := TVectorShape(FSelected[i]);
    PaintBox.Canvas.Pen.Color := clBlue;
    PaintBox.Canvas.Pen.Width := 1;
    PaintBox.Canvas.Pen.Style := psDot;
    PaintBox.Canvas.Brush.Style := bsClear;

    case Shape.ShapeType of
      stPoint:
        PaintBox.Canvas.Ellipse(Shape.X1 - 4, Shape.Y1 - 4, Shape.X1 + 4, Shape.Y1 + 4);
      stLine:
        begin
          PaintBox.Canvas.MoveTo(Shape.X1, Shape.Y1);
          PaintBox.Canvas.LineTo(Shape.X2, Shape.Y2);
        end;
      stRectangle:
        PaintBox.Canvas.Rectangle(Min(Shape.X1, Shape.X2) - 3, Min(Shape.Y1, Shape.Y2) - 3,
                                   Max(Shape.X1, Shape.X2) + 3, Max(Shape.Y1, Shape.Y2) + 3);
      stCircle, stEllipse:
        PaintBox.Canvas.Ellipse(Min(Shape.X1, Shape.X2) - 3, Min(Shape.Y1, Shape.Y2) - 3,
                                 Max(Shape.X1, Shape.X2) + 3, Max(Shape.Y1, Shape.Y2) + 3);
    end;
    PaintBox.Canvas.Pen.Style := psSolid;
  end;
end;

function TForm1.FindShapeAt(X, Y: Integer): TVectorShape;
var
  i: Integer;
begin
  Result := nil;
  for i := FShapes.Count - 1 downto 0 do
  begin
    if TVectorShape(FShapes[i]).ContainsPoint(X, Y) then
    begin
      Result := TVectorShape(FShapes[i]);
      Exit;
    end;
  end;
end;

procedure TForm1.AddShape(Shape: TVectorShape);
begin
  FShapes.Add(Shape);
  StatusBar.Panels[0].Text := 'Фигур: ' + IntToStr(FShapes.Count);
end;

procedure TForm1.UpdateStatus(const X, Y: Integer);
begin
  StatusBar.Panels[2].Text := Format('X: %d  Y: %d', [X, Y]);
end;

procedure TForm1.ClearSelection;
begin
  FSelected.Clear;
end;

end.
