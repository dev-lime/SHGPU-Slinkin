unit graf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, Spin, uPSComponent, uPSRuntime;

type

  { TForm1 }

  TForm1 = class(TForm)
    Btn_Draw: TButton;
    ColorBox1: TColorBox;
    Edit_Formula: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PaintBox1: TPaintBox;
    PSScript1: TPSScript;
    procedure Btn_DrawClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure LabelScaleClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; ShiftState: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PSScript1Compile(Sender: TPSScript);
    procedure PSScript1Execute(Sender: TPSScript);
    procedure Edit_FormulaEditingDone(Sender: TObject);
  private
    FScaleX: Double;
    FScaleY: Double;
    FGraphColor: TColor;
    FGraphDrawn: Boolean;
    FCompiled: Boolean;
    FScriptCode: TStringList;
    function EvaluateFormula(x: Double): Double;
    procedure DrawGraph;
    procedure DrawAxes;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math;

function WrpTan(x: Double): Double;
begin
  try Result := tan(x); except Result := NaN; end;
end;

function WrpLn(x: Double): Double;
begin
  try Result := ln(x); except Result := NaN; end;
end;

function WrpExp(x: Double): Double;
begin
  try Result := exp(x); except Result := NaN; end;
end;

function WrpPow(x, y: Double): Double;
begin
  try Result := power(x, y); except Result := NaN; end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FGraphDrawn := False;
  FCompiled := False;
  FScriptCode := TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FScriptCode.Free;
end;

procedure TForm1.PSScript1Compile(Sender: TPSScript);
begin
  Sender.AddFunction(@WrpTan, 'function tan(x: Double): Double;');
  Sender.AddFunction(@WrpLn, 'function ln(x: Double): Double;');
  Sender.AddFunction(@WrpExp, 'function exp(x: Double): Double;');
  Sender.AddFunction(@WrpPow, 'function pow(x, y: Double): Double;');
end;

procedure TForm1.PSScript1Execute(Sender: TPSScript);
begin
  Sender.Exec.RegisterDelphiFunction(@WrpTan, 'tan', cdRegister);
  Sender.Exec.RegisterDelphiFunction(@WrpLn, 'ln', cdRegister);
  Sender.Exec.RegisterDelphiFunction(@WrpExp, 'exp', cdRegister);
  Sender.Exec.RegisterDelphiFunction(@WrpPow, 'pow', cdRegister);
end;

function TForm1.EvaluateFormula(x: Double): Double;
begin
  Result := 0;
  if FCompiled then
  begin
    try
      Result := PSScript1.ExecuteFunction([x], 'FTPSS');
    except
      Result := 0;
    end;
  end;
end;

procedure TForm1.DrawAxes;
var
  w, h, cx, cy: Integer;
  xStart, xEnd, yStart, yEnd: Double;
  labelText: string;

  function CoordToStr(val: Double): string;
  begin
    if Abs(val) < 1e6 then
      Result := Format('%.2f', [val])
    else
      Result := Format('%.0e', [val]);
  end;

begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(Rect(0, 0, w, h));

  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Pen.Width := 1;

  xStart := -FScaleX;
  xEnd := FScaleX;
  yStart := FScaleY;
  yEnd := -FScaleY;

  PaintBox1.Canvas.MoveTo(0, cy);
  PaintBox1.Canvas.LineTo(w, cy);

  PaintBox1.Canvas.MoveTo(cx, 0);
  PaintBox1.Canvas.LineTo(cx, h);

  PaintBox1.Canvas.Font.Color := clBlack;
  PaintBox1.Canvas.Font.Size := 10;

  labelText := CoordToStr(xStart);
  PaintBox1.Canvas.TextOut(2, cy + 5, labelText);

  labelText := CoordToStr(xEnd);
  PaintBox1.Canvas.TextOut(w - PaintBox1.Canvas.TextWidth(labelText) - 2, cy + 5, labelText);

  PaintBox1.Canvas.TextOut(cx + 5, cy + 5, '(0,0)');

  labelText := CoordToStr(yStart);
  PaintBox1.Canvas.TextOut(cx + 5, 2, labelText);

  labelText := CoordToStr(yEnd);
  PaintBox1.Canvas.TextOut(cx + 5, h - PaintBox1.Canvas.TextHeight('X') - 2, labelText);
end;

procedure TForm1.DrawGraph;
var
  w, h, cx, cy: Integer;
  x, xStart, xEnd, dx: Double;
  px, py, prevPy: Integer;
  firstPoint: Boolean;
  val: Double;
  sx, sy: Double;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  xStart := -FScaleX;
  xEnd := FScaleX;

  dx := (xEnd - xStart) / w;

  sx := w / (2 * FScaleX);
  sy := h / (2 * FScaleY);

  PaintBox1.Canvas.Pen.Color := FGraphColor;
  PaintBox1.Canvas.Pen.Width := 2;

  firstPoint := True;
  prevPy := 0;

  x := xStart;
  while x <= xEnd do
  begin
    val := EvaluateFormula(x);

    if IsInfinite(val) or IsNan(val) then
    begin
      firstPoint := True;
      x := x + dx;
      Continue;
    end;

    px := cx + Round(x * sx);
    py := cy - Round(val * sy);

    if firstPoint then
    begin
      PaintBox1.Canvas.MoveTo(px, py);
      firstPoint := False;
    end
    else
    begin
      if Abs(py - prevPy) > h * 2 then
        PaintBox1.Canvas.MoveTo(px, py)
      else
        PaintBox1.Canvas.LineTo(px, py);
    end;

    prevPy := py;
    x := x + dx;
  end;
end;

procedure TForm1.Btn_DrawClick(Sender: TObject);
begin
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;

  FScriptCode.Clear;
  FScriptCode.Add('function FTPSS(x: Double): Double;');
  FScriptCode.Add('begin');
  FScriptCode.Add('  Result := ' + Edit_Formula.Text + ';');
  FScriptCode.Add('end;');
  FScriptCode.Add('begin end.');
  PSScript1.Script.Assign(FScriptCode);
  FCompiled := PSScript1.Compile;

  if not FCompiled then
  begin
    ShowMessage('Ошибка в формуле. Проверьте правильность введённого выражения.');
    FGraphDrawn := False;
    PaintBox1.Repaint;
    Exit;
  end;

  FGraphDrawn := True;
  PaintBox1.Repaint;
end;

procedure TForm1.Edit_FormulaEditingDone(Sender: TObject);
begin
  Btn_DrawClick(Sender);
end;

procedure TForm1.ScaleChanged(Sender: TObject);
begin
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  PaintBox1.Repaint;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PaintBox1.Repaint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawAxes;
  if FGraphDrawn then
    DrawGraph;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; ShiftState: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  edit: TFloatSpinEdit;
  pt: TPoint;
begin
  pt := FloatSpinEdit1.ScreenToClient(MousePos);
  if (pt.X >= 0) and (pt.X < FloatSpinEdit1.Width) and
     (pt.Y >= 0) and (pt.Y < FloatSpinEdit1.Height) then
    edit := FloatSpinEdit1
  else
  begin
    pt := FloatSpinEdit2.ScreenToClient(MousePos);
    if (pt.X >= 0) and (pt.X < FloatSpinEdit2.Width) and
       (pt.Y >= 0) and (pt.Y < FloatSpinEdit2.Height) then
      edit := FloatSpinEdit2
    else
      Exit;
  end;

  if WheelDelta > 0 then
    edit.Value := edit.Value + edit.Increment
  else
    edit.Value := edit.Value - edit.Increment;

  if edit.Value < edit.MinValue then edit.Value := edit.MinValue;
  if edit.Value > edit.MaxValue then edit.Value := edit.MaxValue;

  Handled := True;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShowMessage(
    'Гэомэтр 1.0: Формулы'#13#10 +
    #13#10 +
    'Формула, предназначенная для построения графика, представляет собой ' +
    'алгебраическое однострочное выражение, сформированное в рамках ' +
    'синтаксических правил языка программирования Pascal.'#13#10 +
    #13#10 +
    'В формуле допускается использовать арифметические операции +,-,*,/, ' +
    'функции sin, cos, tan, pow, ln, exp, sqrt. Разрешаются группировки ' +
    'с использованием скобок.'#13#10 +
    #13#10 +
    'Разрешается использовать целочисленные и вещественные константы, ' +
    'а также переменную-координату x.'#13#10 +
    #13#10 +
    'С помощью подстановки в формулу значений х производится расчёт ' +
    'координат y и построение графика функции на основании введенной формулы.'#13#10 +
    #13#10 +
    'Построение графика начинается с нажатия на клавишу Enter или кнопку "Рисовать"'#13#10 +
    #13#10 +
    'Примеры:'#13#10 +
    'sin(x)'#13#10 +
    'cos(sin(x)+1)'#13#10 +
    'sin(x*x)+cos(pow(x,3))'#13#10 +
    '1/(sin(x)+cos(x))'
  );
end;

procedure TForm1.LabelScaleClick(Sender: TObject);
begin
  ShowMessage(
    'Гэомэтр 1.0: Масштабирование'#13#10 +
    #13#10 +
    'Масштабирование используется для изменения масштаба графика ' +
    'в диапазоне [0.1, 10].'#13#10 +
    #13#10 +
    'Масштабирование производится отдельно для осей абсцисс и ординат.'#13#10 +
    'Изменение масштаба автоматически приводит к перерисовке графика.'
  );
end;

end.
