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
  public
    function Evaluate(const expr: string; x: Double): Double;
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
    procedure UpdateFromControls;
  private
    FFormula: string;
    FScaleX: Double;
    FScaleY: Double;
    FGraphColor: TColor;
    FNeedDraw: Boolean;
    FParser: TFormulaParser;
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
  arg: Double;
  start: Integer;
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

  SkipSpaces;
  if CurrentChar = '(' then
  begin
    Inc(FPos);
    arg := ParseExpression;
    SkipSpaces;
    if CurrentChar = ')' then
      Inc(FPos);
  end
  else
    arg := 0;

  if funcName = 'sin' then Result := Sin(arg)
  else if funcName = 'cos' then Result := Cos(arg)
  else if funcName = 'tan' then Result := Tan(arg)
  else if funcName = 'exp' then Result := Exp(arg)
  else if funcName = 'ln' then Result := Ln(arg)
  else if funcName = 'sqrt' then Result := Sqrt(arg)
  else if funcName = 'abs' then Result := Abs(arg)
  else if funcName = 'arctan' then Result := ArcTan(arg)
  else if funcName = 'sqr' then Result := Sqr(arg)
  else if funcName = 'pi' then Result := Pi
  else Result := 0;
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
  baseVal, exponent: Double;
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
  while True do
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
  while True do
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

function TFormulaParser.Evaluate(const expr: string; x: Double): Double;
begin
  FExpr := expr;
  FPos := 1;
  FX := x;
  try
    Result := ParseExpression;
  except
    Result := 0;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FNeedDraw := False;
  FParser := TFormulaParser.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FParser.Free;
end;

procedure TForm1.UpdateFromControls;
begin
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FNeedDraw := True;
end;

procedure TForm1.ScaleChanged(Sender: TObject);
begin
  UpdateFromControls;
  PaintBox1.Repaint;
end;

function TForm1.EvaluateFormula(x: Double): Double;
begin
  try
    Result := FParser.Evaluate(FFormula, x);
  except
    Result := 0;
  end;
end;

procedure TForm1.DrawAxes;
var
  cx, cy, i: Integer;
  w, h: Integer;
  gridStep: Integer;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  PaintBox1.Canvas.Pen.Color := clSilver;
  PaintBox1.Canvas.Pen.Width := 1;

  gridStep := Round(FScaleX);
  if gridStep < 20 then gridStep := 20;

  i := cx mod gridStep;
  while i <= w do
  begin
    PaintBox1.Canvas.MoveTo(i, 0);
    PaintBox1.Canvas.LineTo(i, h);
    i := i + gridStep;
  end;

  i := cy mod gridStep;
  while i <= h do
  begin
    PaintBox1.Canvas.MoveTo(0, i);
    PaintBox1.Canvas.LineTo(w, i);
    i := i + gridStep;
  end;

  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Pen.Width := 2;

  PaintBox1.Canvas.MoveTo(0, cy);
  PaintBox1.Canvas.LineTo(w, cy);

  PaintBox1.Canvas.MoveTo(cx, 0);
  PaintBox1.Canvas.LineTo(cx, h);

  PaintBox1.Canvas.Font.Color := clBlack;
  PaintBox1.Canvas.Font.Size := 8;
  PaintBox1.Canvas.TextOut(cx + 5, cy + 5, '0');
  PaintBox1.Canvas.TextOut(w - 15, cy + 5, 'X');
  PaintBox1.Canvas.TextOut(cx + 5, 5, 'Y');
end;

procedure TForm1.DrawGraph;
var
  w, h, cx, cy: Integer;
  x, xStart, xEnd, dx: Double;
  px, py, prevPx, prevPy: Integer;
  firstPoint: Boolean;
  val: Double;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;
  cx := w div 2;
  cy := h div 2;

  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(Rect(0, 0, w, h));

  DrawAxes;

  if FFormula = '' then Exit;

  xStart := -cx / FScaleX;
  xEnd := cx / FScaleX;
  dx := (xEnd - xStart) / w;

  PaintBox1.Canvas.Pen.Color := FGraphColor;
  PaintBox1.Canvas.Pen.Width := 2;

  firstPoint := True;
  prevPx := 0;
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

    px := cx + Round(x * FScaleX);
    py := cy - Round(val * FScaleY);

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
begin
  UpdateFromControls;
  PaintBox1.Repaint;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if FNeedDraw then
    PaintBox1.Repaint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  DrawGraph;
end;

end.
