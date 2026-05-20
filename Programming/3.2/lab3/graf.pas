unit graf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, Spin, uPSComponent, uPSRuntime, uPSCompiler;

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
    procedure PSScript1Compile(Sender: TPSScript);
    procedure PSScript1Execute(Sender: TPSScript);
  private
    FFormula: string;
    FScaleX: Double;
    FScaleY: Double;
    FGraphColor: TColor;
    FNeedDraw: Boolean;
    FHasResult: Boolean;
    FResultValue: Double;
    FXValue: Double;
    function EvaluateFormula(x: Double): Double;
    procedure DrawGraph;
    procedure DrawAxes;
    procedure PSScript1ReadVar(Sender: TPSScript; const VarName: string; var Value: Variant; var Success: Boolean);
    procedure PSScript1WriteVar(Sender: TPSScript; const VarName: string; var Value: Variant; var Success: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FNeedDraw := False;
  FHasResult := False;
  FResultValue := 0;
  FXValue := 0;
end;

procedure TForm1.PSScript1Compile(Sender: TPSScript);
begin
  Sender.AddFunction(@Sin, 'function sin(x: Double): Double;');
  Sender.AddFunction(@Cos, 'function cos(x: Double): Double;');
  Sender.AddFunction(@Tan, 'function tan(x: Double): Double;');
  Sender.AddFunction(@Exp, 'function exp(x: Double): Double;');
  Sender.AddFunction(@Ln, 'function ln(x: Double): Double;');
  Sender.AddFunction(@Sqrt, 'function sqrt(x: Double): Double;');
  Sender.AddFunction(@Abs, 'function abs(x: Double): Double;');
  Sender.AddFunction(@ArcTan, 'function arctan(x: Double): Double;');
  Sender.AddFunction(@Sqr, 'function sqr(x: Double): Double;');
  Sender.AddFunction(@Pi, 'function pi: Double;');
  Sender.AddFunction(@Power, 'function power(base: Double; exponent: Double): Double;');
end;

procedure TForm1.PSScript1Execute(Sender: TPSScript);
begin
end;

procedure TForm1.PSScript1ReadVar(Sender: TPSScript; const VarName: string; var Value: Variant; var Success: Boolean);
begin
  if VarName = 'x' then
  begin
    Value := FXValue;
    Success := True;
  end;
end;

procedure TForm1.PSScript1WriteVar(Sender: TPSScript; const VarName: string; var Value: Variant; var Success: Boolean);
begin
  if VarName = 'Result' then
  begin
    FResultValue := Value;
    FHasResult := True;
    Success := True;
  end;
end;

function TForm1.EvaluateFormula(x: Double): Double;
var
  scriptCode: string;
begin
  Result := 0;
  FHasResult := False;
  FResultValue := 0;
  try
    FXValue := x;
    scriptCode := 'var x, Result: Double; begin Result := ' + FFormula + '; end;';
    PSScript1.Script.Text := scriptCode;
    if PSScript1.Compile then
    begin
      PSScript1.Execute;
      if FHasResult then
        Result := FResultValue;
    end;
  except
    Result := 0;
    FHasResult := False;
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

  gridStep := Round(50 * FScaleX);
  if gridStep < 20 then gridStep := 50;

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

  xStart := -cx / (50 * FScaleX);
  xEnd := cx / (50 * FScaleX);
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

    px := cx + Round(x * 50 * FScaleX);
    py := cy - Round(val * 50 * FScaleY);

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
  FFormula := Edit_Formula.Text;
  FScaleX := FloatSpinEdit1.Value;
  FScaleY := FloatSpinEdit2.Value;
  FGraphColor := ColorBox1.Selected;
  FNeedDraw := True;
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
