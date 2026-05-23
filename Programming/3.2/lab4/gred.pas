unit gred;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, Buttons, ColorBox, Spin, Contnrs, Math, shapes;

type
  TToolType = (ttSelect, ttLine, ttRect, ttEllipse, ttTriangle);

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    PaintBox1: TPaintBox;
    StatusBar1: TStatusBar;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuSep1: TMenuItem;
    MenuExport: TMenuItem;
    MenuSep2: TMenuItem;
    MenuExit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    ToolPanel: TPanel;
    SpeedBtnSelect: TSpeedButton;
    SpeedBtnLine: TSpeedButton;
    SpeedBtnRect: TSpeedButton;
    SpeedBtnEllipse: TSpeedButton;
    SpeedBtnTriangle: TSpeedButton;
    PropPanel: TPanel;
    LabelPenColor: TLabel;
    ColorBoxPen: TColorBox;
    LabelPenWidth: TLabel;
    SpinEditPenWidth: TSpinEdit;
    LabelFillColor: TLabel;
    ColorBoxFill: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedBtnClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure PropertyChange(Sender: TObject);
  private
    FShapes: TObjectList;
    FCurrentTool: TToolType;
    FDrawing: Boolean;
    FStartPoint: TPoint;
    FCurrentShape: TVectorShape;
    FSelectedShape: TVectorShape;
    FDragging: Boolean;
    FDragStart: TPoint;
    FFileName: string;
    FModified: Boolean;
    procedure RenderScene(ACanvas: TCanvas; AWidth, AHeight: Integer);
    procedure UpdateStatusBar;
    procedure DeselectAll;
    function SelectShapeAt(X, Y: Integer): Boolean;
    function CreateShapeOfTool(Tool: TToolType): TVectorShape;
    procedure ApplyPropsTo(Shape: TVectorShape);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure UpdatePropPanel;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  ToolShapeMap: array[TToolType] of TShapeClass = (
    nil, TLineShape, TRectShape, TEllipseShape, TTriangleShape
  );

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FShapes := TObjectList.Create(True);
  FCurrentTool := ttLine;
  FDrawing := False;
  FDragging := False;
  FSelectedShape := nil;
  FCurrentShape := nil;
  FModified := False;
  FFileName := '';
  SpeedBtnLine.Down := True;
  ColorBoxPen.Selected := clRed;
  ColorBoxFill.Selected := clWhite;
  SpinEditPenWidth.Value := 2;
  UpdateStatusBar;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FShapes.Free;
end;

procedure TForm1.UpdateStatusBar;
var
  ToolName: string;
begin
  case FCurrentTool of
    ttSelect:   ToolName := 'Выделение';
    ttLine:     ToolName := 'Линия';
    ttRect:     ToolName := 'Прямоугольник';
    ttEllipse:  ToolName := 'Эллипс';
    ttTriangle: ToolName := 'Треугольник';
  end;
  StatusBar1.SimpleText := Format('Инструмент: %s | Фигур: %d', [ToolName, FShapes.Count]);
end;

procedure TForm1.SpeedBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  if not (Sender is TSpeedButton) then Exit;
  Btn := Sender as TSpeedButton;
  DeselectAll;
  UpdatePropPanel;
  if Btn = SpeedBtnSelect then FCurrentTool := ttSelect
  else if Btn = SpeedBtnLine then FCurrentTool := ttLine
  else if Btn = SpeedBtnRect then FCurrentTool := ttRect
  else if Btn = SpeedBtnEllipse then FCurrentTool := ttEllipse
  else if Btn = SpeedBtnTriangle then FCurrentTool := ttTriangle;
  PaintBox1.Repaint;
  UpdateStatusBar;
end;

function TForm1.CreateShapeOfTool(Tool: TToolType): TVectorShape;
var
  Cls: TShapeClass;
begin
  Result := nil;
  Cls := ToolShapeMap[Tool];
  if Cls <> nil then
    Result := Cls.Create;
end;

procedure TForm1.DeselectAll;
var
  I: Integer;
begin
  for I := 0 to FShapes.Count - 1 do
    TVectorShape(FShapes[I]).Selected := False;
  FSelectedShape := nil;
end;

function TForm1.SelectShapeAt(X, Y: Integer): Boolean;
var
  I: Integer;
  Shape: TVectorShape;
begin
  Result := False;
  for I := FShapes.Count - 1 downto 0 do
  begin
    Shape := TVectorShape(FShapes[I]);
    if Shape.HitTest(X, Y) then
    begin
      DeselectAll;
      Shape.Selected := True;
      FSelectedShape := Shape;
      UpdatePropPanel;
      Result := True;
      Exit;
    end;
  end;
end;

procedure TForm1.ApplyPropsTo(Shape: TVectorShape);
begin
  if Shape = nil then Exit;
  Shape.PenColor := ColorBoxPen.Selected;
  Shape.PenWidth := SpinEditPenWidth.Value;
  Shape.PenStyle := psSolid;
  Shape.BrushColor := ColorBoxFill.Selected;
  Shape.BrushStyle := bsSolid;
end;

procedure TForm1.UpdatePropPanel;
begin
  if FSelectedShape <> nil then
  begin
    ColorBoxPen.Selected := FSelectedShape.PenColor;
    SpinEditPenWidth.Value := FSelectedShape.PenWidth;
    ColorBoxFill.Selected := FSelectedShape.BrushColor;
  end;
end;

procedure TForm1.PropertyChange(Sender: TObject);
begin
  if FSelectedShape <> nil then
  begin
    ApplyPropsTo(FSelectedShape);
    PaintBox1.Repaint;
    FModified := True;
  end;
end;

procedure TForm1.RenderScene(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  I: Integer;
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(Rect(0, 0, AWidth, AHeight));
  for I := 0 to FShapes.Count - 1 do
    TVectorShape(FShapes[I]).Draw(ACanvas);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  RenderScene(PaintBox1.Canvas, PaintBox1.Width, PaintBox1.Height);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Shape: TVectorShape;
begin
  if FCurrentTool = ttSelect then
  begin
    if SelectShapeAt(X, Y) then
    begin
      FDragging := True;
      FDragStart := Point(X, Y);
    end
    else
    begin
      DeselectAll;
      UpdatePropPanel;
    end;
    PaintBox1.Repaint;
    Exit;
  end;

  Shape := CreateShapeOfTool(FCurrentTool);
  if Shape = nil then Exit;

  Shape.X1 := X;
  Shape.Y1 := Y;
  Shape.X2 := X;
  Shape.Y2 := Y;
  ApplyPropsTo(Shape);

  DeselectAll;
  FShapes.Add(Shape);
  FCurrentShape := Shape;
  FDrawing := True;
  FStartPoint := Point(X, Y);
  FModified := True;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  DX, DY: Integer;
begin
  StatusBar1.SimpleText := Format('X: %d  Y: %d', [X, Y]);

  if FDragging and (FSelectedShape <> nil) then
  begin
    DX := X - FDragStart.X;
    DY := Y - FDragStart.Y;
    if (DX <> 0) or (DY <> 0) then
    begin
      FSelectedShape.MoveTo(DX, DY);
      FDragStart := Point(X, Y);
      PaintBox1.Repaint;
    end;
  end;

  if FDrawing and (FCurrentShape <> nil) then
  begin
    FCurrentShape.X2 := X;
    FCurrentShape.Y2 := Y;
    PaintBox1.Repaint;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDrawing then
  begin
    FDrawing := False;
    FCurrentShape := nil;
    UpdateStatusBar;
  end;

  if FDragging then
  begin
    FDragging := False;
    FModified := True;
  end;
end;

{ File menu }

procedure TForm1.MenuNewClick(Sender: TObject);
begin
  if FModified then
    if MessageDlg('Изменения не сохранены. Создать новый файл?', mtConfirmation,
       [mbYes, mbNo], 0) <> mrYes then Exit;
  FShapes.Clear;
  DeselectAll;
  UpdatePropPanel;
  FFileName := '';
  FModified := False;
  PaintBox1.Repaint;
  UpdateStatusBar;
end;

procedure TForm1.MenuOpenClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.Filter := 'ГРЭД файлы (*.grd)|*.grd|Все файлы (*.*)|*.*';
  Dlg.DefaultExt := 'grd';
  if Dlg.Execute then
  begin
    LoadFromFile(Dlg.FileName);
    FFileName := Dlg.FileName;
    FModified := False;
  end;
  Dlg.Free;
end;

procedure TForm1.MenuSaveClick(Sender: TObject);
begin
  if FFileName = '' then
    MenuSaveAsClick(Sender)
  else
    SaveToFile(FFileName);
  FModified := False;
end;

procedure TForm1.MenuSaveAsClick(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(Self);
  Dlg.Filter := 'ГРЭД файлы (*.grd)|*.grd|Все файлы (*.*)|*.*';
  Dlg.DefaultExt := 'grd';
  if Dlg.Execute then
  begin
    SaveToFile(Dlg.FileName);
    FFileName := Dlg.FileName;
    FModified := False;
  end;
  Dlg.Free;
end;

procedure TForm1.MenuExportClick(Sender: TObject);
var
  Dlg: TSaveDialog;
  Bmp: TBitmap;
  Png: TPortableNetworkGraphic;
begin
  Dlg := TSaveDialog.Create(Self);
  Dlg.Filter := 'PNG изображение (*.png)|*.png|BMP изображение (*.bmp)|*.bmp';
  Dlg.DefaultExt := 'png';
  if Dlg.Execute then
  begin
    if LowerCase(ExtractFileExt(Dlg.FileName)) = '.bmp' then
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.SetSize(PaintBox1.Width, PaintBox1.Height);
        RenderScene(Bmp.Canvas, Bmp.Width, Bmp.Height);
        Bmp.SaveToFile(Dlg.FileName);
      finally
        Bmp.Free;
      end;
    end
    else
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.SetSize(PaintBox1.Width, PaintBox1.Height);
        RenderScene(Bmp.Canvas, Bmp.Width, Bmp.Height);
        Png := TPortableNetworkGraphic.Create;
        try
          Png.Assign(Bmp);
          Png.SaveToFile(Dlg.FileName);
        finally
          Png.Free;
        end;
      finally
        Bmp.Free;
      end;
    end;
  end;
  Dlg.Free;
end;

procedure TForm1.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuAboutClick(Sender: TObject);
begin
  ShowMessage(
    'ГРЭД 1.0 - векторный графический редактор.'#13#10 +
    'Разработан в среде Lazarus.'#13#10 +
    #13#10 +
    'Инструменты:'#13#10 +
    '- Линия, Прямоугольник, Эллипс, Треугольник'#13#10 +
    '- Выделение и перемещение фигур'#13#10 +
    '- Настройка цвета и толщины линий'#13#10 +
    '- Сохранение/загрузка в формате GRD'#13#10 +
    '- Экспорт в PNG/BMP'
  );
end;

{ File I/O }

procedure TForm1.SaveToFile(const AFileName: string);
var
  S: TFileStream;
  I, Count: Integer;
  Shape: TVectorShape;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    Count := FShapes.Count;
    S.Write(Count, SizeOf(Count));
    for I := 0 to Count - 1 do
    begin
      Shape := TVectorShape(FShapes[I]);
      Shape.SaveToStream(S);
    end;
  finally
    S.Free;
  end;
end;

procedure TForm1.LoadFromFile(const AFileName: string);
var
  S: TFileStream;
  I, Count: Integer;
  ShapeClassName: string;
  Len: Byte;
  Shape: TVectorShape;
begin
  Count := 0;
  S := TFileStream.Create(AFileName, fmOpenRead);
  try
    FShapes.Clear;
    DeselectAll;
    UpdatePropPanel;
    S.Read(Count, SizeOf(Count));
    Len := 0;
    ShapeClassName := '';
    for I := 0 to Count - 1 do
    begin
      S.Read(Len, SizeOf(Len));
      SetLength(ShapeClassName, Len);
      S.Read(ShapeClassName[1], Len);

      Shape := nil;
      if ShapeClassName = 'TLineShape' then Shape := TLineShape.Create
      else if ShapeClassName = 'TRectShape' then Shape := TRectShape.Create
      else if ShapeClassName = 'TEllipseShape' then Shape := TEllipseShape.Create
      else if ShapeClassName = 'TTriangleShape' then Shape := TTriangleShape.Create;

      if Shape <> nil then
      begin
        Shape.LoadFromStream(S);
        FShapes.Add(Shape);
      end;
    end;
  finally
    S.Free;
  end;
  PaintBox1.Repaint;
  UpdateStatusBar;
end;

end.
