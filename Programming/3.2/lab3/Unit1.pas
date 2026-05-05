unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ColorBox, Math;

type
  { TFormulaEvaluator }

  TFormulaEvaluator = class
  private
    FFormula: string;
    FPos: Integer;
    function GetChar: char;
    function SkipSpaces: char;
    function ParseExpression: Extended;
    function ParseTerm: Extended;
    function ParseFactor: Extended;
    function ParseFunction: Extended;
    function ParseNumber: Extended;
    function Identifier: string;
  public
    property Formula: string read FFormula write FFormula;
    function Evaluate(X: Extended): Extended;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox: TPaintBox;
    PanelLeft: TPanel;
    lblFormula: TLabel;
    edtFormula: TEdit;
    btnDraw: TButton;
    lblScaleX: TLabel;
    lblScaleY: TLabel;
    spinScaleX: TFloatSpinEdit;
    spinScaleY: TFloatSpinEdit;
    lblColor: TLabel;
    clrGraph: TColorBox;
    lblInfo: TLabel;
    lblHelp: TLabel;
    PanelHelp: TPanel;
    memHelp: TMemo;
    btnHelp: TButton;
    lblOriginX: TLabel;
    lblOriginY: TLabel;
    spinOriginX: TFloatSpinEdit;
    spinOriginY: TFloatSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FEvaluator: TFormulaEvaluator;
    FXMin, FXMax, FYMin, FYMax: Extended;
    FScaleX, FScaleY: Extended;
    FOriginX, FOriginY: Extended;
    FDragging: Boolean;
    FDragStartX, FDragStartY: Integer;
    FDragOriginX, FDragOriginY: Extended;
    procedure DrawGraph;
    procedure DrawAxes;
    procedure DrawGrid;
    function WorldToScreenX(X: Extended): Integer;
    function WorldToScreenY(Y: Extended): Integer;
    function ScreenToWorldX(ScreenX: Integer): Extended;
    function ScreenToWorldY(ScreenY: Integer): Extended;
    procedure UpdateInfo;
    procedure UpdateScalesFromSpin;
    procedure RecalculateWorldBounds;
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TFormulaEvaluator }

function TFormulaEvaluator.GetChar: char;
begin
  if FPos <= Length(FFormula) then
    Result := FFormula[FPos]
  else
    Result := #0;
end;

function TFormulaEvaluator.SkipSpaces: char;
begin
  while (GetChar = ' ') or (GetChar = #9) do
    Inc(FPos);
  Result := GetChar;
end;

function TFormulaEvaluator.Identifier: string;
var
  Start: Integer;
begin
  Start := FPos;
  while ((GetChar >= 'a') and (GetChar <= 'z')) or
        ((GetChar >= 'A') and (GetChar <= 'Z')) do
    Inc(FPos);
  Result := Copy(FFormula, Start, FPos - Start);
end;

function TFormulaEvaluator.ParseNumber: Extended;
var
  Start: Integer;
  S: string;
begin
  Start := FPos;
  while ((GetChar >= '0') and (GetChar <= '9')) or (GetChar = '.') do
    Inc(FPos);
  S := Copy(FFormula, Start, FPos - Start);
  Result := StrToFloatDef(S, 0);
end;

function TFormulaEvaluator.ParseFunction: Extended;
var
  FuncName: string;
  Arg: Extended;
begin
  FuncName := LowerCase(Identifier);
  SkipSpaces;
  if GetChar <> '(' then
    raise Exception.Create('Expected "(" after function name');
  Inc(FPos);
  SkipSpaces;
  Arg := ParseExpression;
  SkipSpaces;
  if GetChar <> ')' then
    raise Exception.Create('Expected ")"');
  Inc(FPos);

  if FuncName = 'sin' then
    Result := Sin(Arg)
  else if FuncName = 'cos' then
    Result := Cos(Arg)
  else if FuncName = 'tan' then
    Result := Tan(Arg)
  else if FuncName = 'ctg' then
    Result := 1 / Tan(Arg)
  else if FuncName = 'arcsin' then
    Result := ArcSin(Arg)
  else if FuncName = 'arccos' then
    Result := ArcCos(Arg)
  else if FuncName = 'arctan' then
    Result := ArcTan(Arg)
  else if FuncName = 'sqrt' then
    Result := Sqrt(Arg)
  else if FuncName = 'ln' then
    Result := Ln(Arg)
  else if FuncName = 'log' then
    Result := Ln(Arg) / Ln(10)
  else if FuncName = 'exp' then
    Result := Exp(Arg)
  else if FuncName = 'abs' then
    Result := Abs(Arg)
  else if FuncName = 'sqr' then
    Result := Arg * Arg
  else
    raise Exception.Create('Unknown function: ' + FuncName);
end;

function TFormulaEvaluator.ParseFactor: Extended;
begin
  SkipSpaces;
  case GetChar of
    'x', 'X':
      begin
        Inc(FPos);
        Result := 0;
      end;
    '0'..'9', '.':
      Result := ParseNumber;
    '(':
      begin
        Inc(FPos);
        Result := ParseExpression;
        SkipSpaces;
        if GetChar = ')' then
          Inc(FPos);
      end;
    'a'..'z', 'A'..'Z':
      Result := ParseFunction;
    '-':
      begin
        Inc(FPos);
        Result := -ParseFactor;
      end;
    '+':
      begin
        Inc(FPos);
        Result := ParseFactor;
      end;
  else
    Result := 0;
  end;
end;

function TFormulaEvaluator.ParseTerm: Extended;
var
  Op: char;
begin
  Result := ParseFactor;
  while True do
  begin
    SkipSpaces;
    Op := GetChar;
    if (Op = '*') or (Op = '/') then
    begin
      Inc(FPos);
      if Op = '*' then
        Result := Result * ParseFactor
      else
        Result := Result / ParseFactor;
    end
    else
      Break;
  end;
end;

function TFormulaEvaluator.ParseExpression: Extended;
var
  Op: char;
begin
  Result := ParseTerm;
  while True do
  begin
    SkipSpaces;
    Op := GetChar;
    if (Op = '+') or (Op = '-') then
    begin
      Inc(FPos);
      if Op = '+' then
        Result := Result + ParseTerm
      else
        Result := Result - ParseTerm;
    end
    else
      Break;
  end;
end;

function TFormulaEvaluator.Evaluate(X: Extended): Extended;
begin
  FPos := 1;
  try
    Result := ParseExpression;
  except
    Result := 0;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEvaluator := TFormulaEvaluator.Create;
  FScaleX := 50;
  FScaleY := 50;
  FOriginX := 0;
  FOriginY := 0;

  spinScaleX.Value := FScaleX;
  spinScaleY.Value := FScaleY;
  spinOriginX.Value := FOriginX;
  spinOriginY.Value := FOriginY;

  edtFormula.Text := 'sin(x)';
  clrGraph.Selected := clRed;

  memHelp.Lines.Add('Справка по построению графиков:');
  memHelp.Lines.Add('');
  memHelp.Lines.Add('Поддерживаемые функции:');
  memHelp.Lines.Add('  sin(x), cos(x), tan(x), ctg(x)');
  memHelp.Lines.Add('  arcsin(x), arccos(x), arctan(x)');
  memHelp.Lines.Add('  sqrt(x), ln(x), log(x), exp(x)');
  memHelp.Lines.Add('  abs(x), sqr(x)');
  memHelp.Lines.Add('');
  memHelp.Lines.Add('Операции: +, -, *, /');
  memHelp.Lines.Add('Пример: sin(x) + cos(2*x)');
  memHelp.Lines.Add('');
  memHelp.Lines.Add('Управление:');
  memHelp.Lines.Add('  Колесико мыши - масштабирование');
  memHelp.Lines.Add('  Перетаскивание - перемещение');
  memHelp.Lines.Add('  SpinEdit - точное масштабирование');
  memHelp.Lines.Add('  spinOrigin - смещение начала координат');
  memHelp.Lines.Add('');
  memHelp.Lines.Add('Цвет графика выбирается в ColorBox.');

  RecalculateWorldBounds;
  DrawGraph;
end;

destructor TForm1.Destroy;
begin
  FEvaluator.Free;
  inherited Destroy;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
begin
  DrawGraph;
end;

procedure TForm1.btnDrawClick(Sender: TObject);
begin
  FEvaluator.Formula := edtFormula.Text;
  RecalculateWorldBounds;
  DrawGraph;
  UpdateInfo;
end;

procedure TForm1.PaintBoxResize(Sender: TObject);
begin
  RecalculateWorldBounds;
  DrawGraph;
end;

procedure TForm1.btnHelpClick(Sender: TObject);
begin
  PanelHelp.Visible := not PanelHelp.Visible;
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDragStartX := X;
    FDragStartY := Y;
    FDragOriginX := FOriginX;
    FDragOriginY := FOriginY;
  end;
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  WX, WY: Extended;
begin
  if FDragging then
  begin
    FOriginX := FDragOriginX - (X - FDragStartX) / FScaleX;
    FOriginY := FDragOriginY + (Y - FDragStartY) / FScaleY;
    spinOriginX.Value := FOriginX;
    spinOriginY.Value := FOriginY;
    RecalculateWorldBounds;
    DrawGraph;
  end
  else
  begin
    WX := ScreenToWorldX(X);
    WY := ScreenToWorldY(Y);
    lblInfo.Caption := Format('X: %.3f, Y: %.3f', [WX, WY]);
  end;
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDragging := False;
end;

procedure TForm1.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  WX, WY: Extended;
  Factor: Extended;
begin
  WX := ScreenToWorldX(MousePos.X);
  WY := ScreenToWorldY(MousePos.Y);

  if WheelDelta > 0 then
    Factor := 1.2
  else
    Factor := 1 / 1.2;

  FScaleX := FScaleX * Factor;
  FScaleY := FScaleY * Factor;

  if FScaleX < 1 then
    FScaleX := 1;
  if FScaleY < 1 then
    FScaleY := 1;
  if FScaleX > 10000 then
    FScaleX := 10000;
  if FScaleY > 10000 then
    FScaleY := 10000;

  FOriginX := WX - MousePos.X / FScaleX;
  FOriginY := WY + (PaintBox.Height - MousePos.Y) / FScaleY;

  spinScaleX.Value := FScaleX;
  spinScaleY.Value := FScaleY;
  spinOriginX.Value := FOriginX;
  spinOriginY.Value := FOriginY;

  RecalculateWorldBounds;
  DrawGraph;
  Handled := True;
end;

procedure TForm1.DrawGraph;
var
  ScreenX: Integer;
  WX, WY: Extended;
  ScreenY: Integer;
  PrevScreenY: Integer;
  FirstPoint: Boolean;
begin
  with PaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(PaintBox.ClientRect);

    DrawGrid;
    DrawAxes;

    FEvaluator.Formula := edtFormula.Text;

    Pen.Color := clrGraph.Selected;
    Pen.Width := 2;

    FirstPoint := True;
    PrevScreenY := 0;

    for ScreenX := 0 to PaintBox.Width - 1 do
    begin
      WX := ScreenToWorldX(ScreenX);
      try
        WY := FEvaluator.Evaluate(WX);
        ScreenY := WorldToScreenY(WY);

        if (ScreenY >= -1000) and (ScreenY <= PaintBox.Height + 1000) then
        begin
          if FirstPoint then
          begin
            MoveTo(ScreenX, ScreenY);
            FirstPoint := False;
          end
          else
          begin
            if Abs(ScreenY - PrevScreenY) > PaintBox.Height then
              FirstPoint := True
            else
              LineTo(ScreenX, ScreenY);
          end;
          PrevScreenY := ScreenY;
        end
        else
          FirstPoint := True;
      except
        FirstPoint := True;
      end;
    end;
  end;

  UpdateInfo;
end;

procedure TForm1.DrawAxes;
var
  OriginScreenX, OriginScreenY: Integer;
  StepX, StepY: Extended;
  StartX, EndX, ValX, SX: Extended;
  StartY, EndY, ValY, SY: Extended;
  iSX, iSY: Integer;
begin
  with PaintBox.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;

    OriginScreenX := WorldToScreenX(0);
    OriginScreenY := WorldToScreenY(0);

    if (OriginScreenX >= 0) and (OriginScreenX <= PaintBox.Width) then
    begin
      MoveTo(OriginScreenX, 0);
      LineTo(OriginScreenX, PaintBox.Height);
    end;

    if (OriginScreenY >= 0) and (OriginScreenY <= PaintBox.Height) then
    begin
      MoveTo(0, OriginScreenY);
      LineTo(PaintBox.Width, OriginScreenY);
    end;

    Brush.Style := bsClear;
    Font.Color := clBlack;
    Font.Size := 8;

    if FScaleX > 200 then
      StepX := 0.5
    else if FScaleX > 50 then
      StepX := 1
    else if FScaleX > 20 then
      StepX := 2
    else if FScaleX > 10 then
      StepX := 5
    else if FScaleX > 5 then
      StepX := 10
    else
      StepX := 20;

    StartX := (ScreenToWorldX(0) div StepX) * StepX - StepX;
    EndX := (ScreenToWorldX(PaintBox.Width) div StepX) * StepX + StepX;
    ValX := StartX;
    while ValX <= EndX do
    begin
      iSX := WorldToScreenX(ValX);
      SX := ValX;
      if (iSX >= 0) and (iSX <= PaintBox.Width) and (Abs(SX) > 0.001) then
        TextOut(iSX + 2, OriginScreenY + 2, FormatFloat('0.##', SX));
      ValX := ValX + StepX;
    end;

    if FScaleY > 200 then
      StepY := 0.5
    else if FScaleY > 50 then
      StepY := 1
    else if FScaleY > 20 then
      StepY := 2
    else if FScaleY > 10 then
      StepY := 5
    else if FScaleY > 5 then
      StepY := 10
    else
      StepY := 20;

    StartY := (ScreenToWorldY(PaintBox.Height) div StepY) * StepY - StepY;
    EndY := (ScreenToWorldY(0) div StepY) * StepY + StepY;
    ValY := StartY;
    while ValY <= EndY do
    begin
      iSY := WorldToScreenY(ValY);
      SY := ValY;
      if (iSY >= 0) and (iSY <= PaintBox.Height) and (Abs(SY) > 0.001) then
        TextOut(OriginScreenX + 2, iSY - 12, FormatFloat('0.##', SY));
      ValY := ValY + StepY;
    end;
  end;
end;

procedure TForm1.DrawGrid;
var
  StepX, StepY: Extended;
  StartX, EndX, ValX: Extended;
  StartY, EndY, ValY: Extended;
  SX, SY: Integer;
begin
  with PaintBox.Canvas do
  begin
    Pen.Color := clSilver;
    Pen.Width := 1;

    if FScaleX > 200 then
      StepX := 0.5
    else if FScaleX > 50 then
      StepX := 1
    else if FScaleX > 20 then
      StepX := 2
    else if FScaleX > 10 then
      StepX := 5
    else if FScaleX > 5 then
      StepX := 10
    else
      StepX := 20;

    StartX := (ScreenToWorldX(0) div StepX) * StepX - StepX;
    EndX := (ScreenToWorldX(PaintBox.Width) div StepX) * StepX + StepX;
    ValX := StartX;
    while ValX <= EndX do
    begin
      SX := WorldToScreenX(ValX);
      if (SX >= 0) and (SX <= PaintBox.Width) then
      begin
        MoveTo(SX, 0);
        LineTo(SX, PaintBox.Height);
      end;
      ValX := ValX + StepX;
    end;

    if FScaleY > 200 then
      StepY := 0.5
    else if FScaleY > 50 then
      StepY := 1
    else if FScaleY > 20 then
      StepY := 2
    else if FScaleY > 10 then
      StepY := 5
    else if FScaleY > 5 then
      StepY := 10
    else
      StepY := 20;

    StartY := (ScreenToWorldY(PaintBox.Height) div StepY) * StepY - StepY;
    EndY := (ScreenToWorldY(0) div StepY) * StepY + StepY;
    ValY := StartY;
    while ValY <= EndY do
    begin
      SY := WorldToScreenY(ValY);
      if (SY >= 0) and (SY <= PaintBox.Height) then
      begin
        MoveTo(0, SY);
        LineTo(PaintBox.Width, SY);
      end;
      ValY := ValY + StepY;
    end;
  end;
end;

function TForm1.WorldToScreenX(X: Extended): Integer;
begin
  Result := Round((X - FOriginX) * FScaleX);
end;

function TForm1.WorldToScreenY(Y: Extended): Integer;
begin
  Result := PaintBox.Height - Round((Y - FOriginY) * FScaleY);
end;

function TForm1.ScreenToWorldX(ScreenX: Integer): Extended;
begin
  Result := ScreenX / FScaleX + FOriginX;
end;

function TForm1.ScreenToWorldY(ScreenY: Integer): Extended;
begin
  Result := (PaintBox.Height - ScreenY) / FScaleY + FOriginY;
end;

procedure TForm1.UpdateInfo;
begin
  lblInfo.Caption := Format('ScaleX: %.0f, ScaleY: %.0f', [FScaleX, FScaleY]);
end;

procedure TForm1.UpdateScalesFromSpin;
begin
  FScaleX := spinScaleX.Value;
  FScaleY := spinScaleY.Value;
  FOriginX := spinOriginX.Value;
  FOriginY := spinOriginY.Value;
end;

procedure TForm1.RecalculateWorldBounds;
begin
  UpdateScalesFromSpin;
  FXMin := ScreenToWorldX(0);
  FXMax := ScreenToWorldX(PaintBox.Width);
  FYMin := ScreenToWorldY(PaintBox.Height);
  FYMax := ScreenToWorldY(0);
end;

end.
