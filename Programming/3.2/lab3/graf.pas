unit graf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, Spin;

type

  { TFormulaParser }

  TFormulaParser = class
  private
    FExpr: string;
    FPos: Integer;
    FX: Double;
    function ParseExpression: Double;
    function ParseTerm: Double;
    function ParseFactor: Double;
    function ParsePower: Double;
    function ParsePrimary: Double;
    function ParseFunctionOrVar: Double;
    function ParseNumber: Double;
    procedure SkipSpaces;
    function CurrentChar: Char;
    function IsDigit(c: Char): Boolean;
    function IsLetter(c: Char): Boolean;
    function IsValid: Boolean;
  public
    function Evaluate(const expr: string; x: Double; out Valid: Boolean): Double;
  end;

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
    procedure Btn_DrawClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; ShiftState: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FFormula: string;
    FScaleX: Double;
    FScaleY: Double;
    FGraphColor: TColor;
    FGraphDrawn: Boolean;
    FParser: TFormulaParser;
    function EvaluateFormula(x: Double; out Valid: Boolean): Double;
    procedure DrawGraph;
    procedure DrawAxes;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math;

{ TFormulaParser }

function TFormulaParser.CurrentChar: Char;
begin
  if FPos <= Length(FExpr) then
    Result := FExpr[FPos]
  else
    Result := #0;
end;

function TFormulaParser.IsDigit(c: Char): Boolean;
begin
  Result := (c >= '0') and (c <= '9');
end;

function TFormulaParser.IsLetter(c: Char): Boolean;
begin
  Result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

function TFormulaParser.IsValid: Boolean;
begin
  Result := (FPos <= Length(FExpr));
end;

procedure TFormulaParser.SkipSpaces;
begin
  while CurrentChar = ' ' do
    Inc(FPos);
end;

function TFormulaParser.ParseNumber: Double;
var
  start: Integer;
  numStr: string;
  hasDot: Boolean;
begin
  SkipSpaces;
  start := FPos;
  hasDot := False;
  while IsDigit(CurrentChar) or (CurrentChar = '.') do
  begin
    if CurrentChar = '.' then
    begin
      if hasDot then Break;
      hasDot := True;
    end;
    Inc(FPos);
  end;
  numStr := Copy(FExpr, start, FPos - start);
  Result := StrToFloatDef(numStr, 0);
end;

function TFormulaParser.ParseFunctionOrVar: Double;
var
  funcName: string;
  start: Integer;
  arg1, arg2: Double;
begin
  SkipSpaces;
  start := FPos;
  while IsLetter(CurrentChar) do
    Inc(FPos);
  funcName := LowerCase(Copy(FExpr, start, FPos - start));

  if funcName = 'x' then
  begin
    Result := FX;
    Exit;
  end;

  if funcName = 'pi' then
  begin
    Result := Pi;
    Exit;
  end;

  SkipSpaces;
  if CurrentChar = '(' then
  begin
    Inc(FPos);
    arg1 := ParseExpression;
    SkipSpaces;
    if CurrentChar = ',' then
    begin
      Inc(FPos);
      arg2 := ParseExpression;
      SkipSpaces;
    end;
    if CurrentChar = ')' then
      Inc(FPos);

    if funcName = 'pow' then
      Result := Power(arg1, arg2)
    else if funcName = 'sin' then Result := Sin(arg1)
    else if funcName = 'cos' then Result := Cos(arg1)
    else if funcName = 'tan' then Result := Tan(arg1)
    else if funcName = 'exp' then Result := Exp(arg1)
    else if funcName = 'ln' then Result := Ln(arg1)
    else if funcName = 'sqrt' then Result := Sqrt(arg1)
    else if funcName = 'abs' then Result := Abs(arg1)
    else if funcName = 'arctan' then Result := ArcTan(arg1)
    else if funcName = 'sqr' then Result := Sqr(arg1)
    else Result := 0;
  end
  else
    Result := 0;
end;

function TFormulaParser.ParsePrimary: Double;
begin
  SkipSpaces;
  if CurrentChar = '(' then
  begin
    Inc(FPos);
    Result := ParseExpression;
    SkipSpaces;
    if CurrentChar = ')' then
      Inc(FPos);
  end
  else if IsLetter(CurrentChar) then
    Result := ParseFunctionOrVar
  else if IsDigit(CurrentChar) or (CurrentChar = '.') then
    Result := ParseNumber
  else
    Result := 0;
end;

function TFormulaParser.ParsePower: Double;
var
  exponent: Double;
begin
  Result := ParsePrimary;

  SkipSpaces;
  if CurrentChar = '^' then
  begin
    Inc(FPos);
    exponent := ParsePower;
    Result := Power(Result, exponent);
  end;
end;

function TFormulaParser.ParseFactor: Double;
begin
  SkipSpaces;
  if CurrentChar = '-' then
  begin
    Inc(FPos);
    Result := -ParseFactor;
  end
  else if CurrentChar = '+' then
  begin
    Inc(FPos);
    Result := ParseFactor;
  end
  else
    Result := ParsePower;
end;

function TFormulaParser.ParseTerm: Double;
var
  right: Double;
begin
  Result := ParseFactor;
  while IsValid do
  begin
    SkipSpaces;
    if CurrentChar = '*' then
    begin
      Inc(FPos);
      right := ParseFactor;
      Result := Result * right;
    end
    else if CurrentChar = '/' then
    begin
      Inc(FPos);
      right := ParseFactor;
      if right <> 0 then
        Result := Result / right
      else
        Result := 0;
    end
    else
      Break;
  end;
end;

function TFormulaParser.ParseExpression: Double;
var
  right: Double;
begin
  Result := ParseTerm;
  while IsValid do
  begin
    SkipSpaces;
    if CurrentChar = '+' then
    begin
      Inc(FPos);
      right := ParseTerm;
      Result := Result + right;
    end
    else if CurrentChar = '-' then
    begin
      Inc(FPos);
      right := ParseTerm;
      Result := Result - right;
    end
    else
      Break;
  end;
end;

function TFormulaParser.Evaluate(const expr: string; x: Double; out Valid: Boolean): Double;
begin
  FExpr := expr;
  FPos := 1;
  FX := x;
  try
    Result := ParseExpression;
    Valid := True;
  except
    Result := 0;
    Valid := False;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FGraphDrawn := False;
  FParser := TFormulaParser.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FParser.Free;
end;

function TForm1.EvaluateFormula(x: Double; out Valid: Boolean): Double;
begin
  Result := FParser.Evaluate(FFormula, x, Valid);
end;

procedure TForm1.DrawAxes;
var
  w, h, cx, cy: Integer;
  xStart, xEnd, yStart, yEnd: Double;
  labelX: string;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(Rect(0, 0, w, h));

  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Pen.Width := 2;

  xStart := -cx / (FScaleX * 50);
  xEnd := cx / (FScaleX * 50);
  yStart := cy / (FScaleY * 50);
  yEnd := -cy / (FScaleY * 50);

  PaintBox1.Canvas.MoveTo(0, cy);
  PaintBox1.Canvas.LineTo(w, cy);

  PaintBox1.Canvas.MoveTo(cx, 0);
  PaintBox1.Canvas.LineTo(cx, h);

  PaintBox1.Canvas.Font.Color := clBlack;
  PaintBox1.Canvas.Font.Size := 8;

  if Abs(xStart) < 1e6 then
    labelX := Format('%.2f', [xStart])
  else
    labelX := Format('%.0e', [xStart]);
  PaintBox1.Canvas.TextOut(2, cy + 5, labelX);

  if Abs(xEnd) < 1e6 then
    labelX := Format('%.2f', [xEnd])
  else
    labelX := Format('%.0e', [xEnd]);
  PaintBox1.Canvas.TextOut(w - PaintBox1.Canvas.TextWidth(labelX) - 2, cy + 5, labelX);

  PaintBox1.Canvas.TextOut(cx + 5, cy + 5, '0');

  if Abs(yStart) < 1e6 then
    labelX := Format('%.2f', [yStart])
  else
    labelX := Format('%.0e', [yStart]);
  PaintBox1.Canvas.TextOut(cx + 5, 2, labelX);

  if Abs(yEnd) < 1e6 then
    labelX := Format('%.2f', [yEnd])
  else
    labelX := Format('%.0e', [yEnd]);
  PaintBox1.Canvas.TextOut(cx + 5, h - PaintBox1.Canvas.TextHeight('X') - 2, labelX);
end;

procedure TForm1.DrawGraph;
var
  w, h, cx, cy: Integer;
  x, xStart, xEnd, dx: Double;
  px, py, prevPx, prevPy: Integer;
  firstPoint: Boolean;
  val: Double;
  valid: Boolean;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  xStart := -cx / (FScaleX * 50);
  xEnd := cx / (FScaleX * 50);
  dx := (xEnd - xStart) / w;

  PaintBox1.Canvas.Pen.Color := FGraphColor;
  PaintBox1.Canvas.Pen.Width := 2;

  firstPoint := True;
  prevPx := 0;
  prevPy := 0;

  x := xStart;
  while x <= xEnd do
  begin
    val := EvaluateFormula(x, valid);

    if not valid or IsInfinite(val) or IsNan(val) then
    begin
      firstPoint := True;
      x := x + dx;
      Continue;
    end;

    px := cx + Round(x * FScaleX * 50);
    py := cy - Round(val * FScaleY * 50);

    if py < -10000 then py := -10000;
    if py > h + 10000 then py := h + 10000;

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

    prevPx := px;
    prevPy := py;
    x := x + dx;
  end;
end;

procedure TForm1.Btn_DrawClick(Sender: TObject);
var
  valid: Boolean;
  testVal: Double;
begin
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;

  testVal := FParser.Evaluate(FFormula, 0, valid);
  if not valid then
  begin
    ShowMessage('Ошибка в формуле. Проверьте правильность введённого выражения.');
    FGraphDrawn := False;
    PaintBox1.Repaint;
    Exit;
  end;

  FGraphDrawn := True;
  PaintBox1.Repaint;
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

procedure TForm1.Label2Click(Sender: TObject);
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

procedure TForm1.Label3Click(Sender: TObject);
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
