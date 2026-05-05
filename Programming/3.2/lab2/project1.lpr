(*
ЛР2. Динамическое создание и управление виджетами.
Приложение "Неваляшка" - все виджеты создаются динамически, один файл исходного кода.
*)
program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, SysUtils, Math;

const
  MIN_WIDTH = 500;
  MAX_WIDTH = 1000;
  MIN_HEIGHT = 100;
  MAX_HEIGHT = 500;
  DEMO_STEP = 10;
  DEMO_INTERVAL = 50;

type
  { TRolyPolyForm }

  TRolyPolyForm = class(TForm)
  private
    FPrevWidth: Integer;
    FPrevHeight: Integer;
    FDemoMode: Integer;
    FUpdatingSize: Boolean;
    FCentering: Boolean;
    FEdtWidth: TEdit;
    FEdtHeight: TEdit;
    FBtnDemoPlus: TButton;
    FBtnDemoMinus: TButton;
    FLblWidth: TLabel;
    FLblHeight: TLabel;
    FTmrDemo: TTimer;
    procedure FormResizeHandler(Sender: TObject);
    procedure FormMoveHandler(Sender: TObject);
    procedure EdtWidthExit(Sender: TObject);
    procedure EdtHeightExit(Sender: TObject);
    procedure EdtWidthKeyPress(Sender: TObject; var Key: char);
    procedure EdtHeightKeyPress(Sender: TObject; var Key: char);
    procedure BtnDemoPlusClick(Sender: TObject);
    procedure BtnDemoMinusClick(Sender: TObject);
    procedure TmrDemoTimer(Sender: TObject);
    procedure CenterForm;
    procedure UpdateSizeFields;
    procedure SetControlsEnabled(Enabled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  RolyPolyForm: TRolyPolyForm;

{$R *.res}

constructor TRolyPolyForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'Неваляшка (динамические виджеты)';
  Width := 700;
  Height := 250;
  Position := poScreenCenter;

  Constraints.MinWidth := MIN_WIDTH;
  Constraints.MaxWidth := MAX_WIDTH;
  Constraints.MinHeight := MIN_HEIGHT;
  Constraints.MaxHeight := MAX_HEIGHT;

  OnResize := FormResizeHandler;
  OnMove := FormMoveHandler;

  FLblWidth := TLabel.Create(Self);
  FLblWidth.Parent := Self;
  FLblWidth.Left := 50;
  FLblWidth.Top := 50;
  FLblWidth.Width := 80;
  FLblWidth.Caption := 'Width:';

  FLblHeight := TLabel.Create(Self);
  FLblHeight.Parent := Self;
  FLblHeight.Left := 50;
  FLblHeight.Top := 90;
  FLblHeight.Width := 80;
  FLblHeight.Caption := 'Height:';

  FEdtWidth := TEdit.Create(Self);
  FEdtWidth.Parent := Self;
  FEdtWidth.Left := 150;
  FEdtWidth.Top := 47;
  FEdtWidth.Width := 121;
  FEdtWidth.Height := 21;
  FEdtWidth.TabOrder := 0;
  FEdtWidth.Text := IntToStr(Width);
  FEdtWidth.OnExit := EdtWidthExit;
  FEdtWidth.OnKeyPress := EdtWidthKeyPress;

  FEdtHeight := TEdit.Create(Self);
  FEdtHeight.Parent := Self;
  FEdtHeight.Left := 150;
  FEdtHeight.Top := 87;
  FEdtHeight.Width := 121;
  FEdtHeight.Height := 21;
  FEdtHeight.TabOrder := 1;
  FEdtHeight.Text := IntToStr(Height);
  FEdtHeight.OnExit := EdtHeightExit;
  FEdtHeight.OnKeyPress := EdtHeightKeyPress;

  FBtnDemoPlus := TButton.Create(Self);
  FBtnDemoPlus.Parent := Self;
  FBtnDemoPlus.Left := 50;
  FBtnDemoPlus.Top := 150;
  FBtnDemoPlus.Width := 150;
  FBtnDemoPlus.Height := 30;
  FBtnDemoPlus.Caption := 'Demo +';
  FBtnDemoPlus.TabOrder := 2;
  FBtnDemoPlus.OnClick := BtnDemoPlusClick;

  FBtnDemoMinus := TButton.Create(Self);
  FBtnDemoMinus.Parent := Self;
  FBtnDemoMinus.Left := 220;
  FBtnDemoMinus.Top := 150;
  FBtnDemoMinus.Width := 150;
  FBtnDemoMinus.Height := 30;
  FBtnDemoMinus.Caption := 'Demo -';
  FBtnDemoMinus.TabOrder := 3;
  FBtnDemoMinus.OnClick := BtnDemoMinusClick;

  FTmrDemo := TTimer.Create(Self);
  FTmrDemo.Enabled := False;
  FTmrDemo.Interval := DEMO_INTERVAL;
  FTmrDemo.OnTimer := TmrDemoTimer;

  FPrevWidth := Width;
  FPrevHeight := Height;
  FDemoMode := 0;
  FUpdatingSize := False;
  FCentering := False;
end;

procedure TRolyPolyForm.FormResizeHandler(Sender: TObject);
begin
  if FUpdatingSize then
    Exit;

  UpdateSizeFields;

  if (Width <> FPrevWidth) or (Height <> FPrevHeight) then
  begin
    CenterForm;
    FPrevWidth := Width;
    FPrevHeight := Height;
  end;
end;

procedure TRolyPolyForm.FormMoveHandler(Sender: TObject);
begin
  CenterForm;
end;

procedure TRolyPolyForm.EdtWidthExit(Sender: TObject);
var
  NewWidth: Integer;
begin
  if Trim(FEdtWidth.Text) = '' then
  begin
    FEdtWidth.Text := IntToStr(FPrevWidth);
    Exit;
  end;

  try
    NewWidth := StrToInt(Trim(FEdtWidth.Text));
    if (NewWidth >= MIN_WIDTH) and (NewWidth <= MAX_WIDTH) then
    begin
      FUpdatingSize := True;
      Width := NewWidth;
      FPrevWidth := NewWidth;
      FUpdatingSize := False;
    end
    else
    begin
      FEdtWidth.Text := IntToStr(FPrevWidth);
    end;
  except
    FEdtWidth.Text := IntToStr(FPrevWidth);
  end;
end;

procedure TRolyPolyForm.EdtHeightExit(Sender: TObject);
var
  NewHeight: Integer;
begin
  if Trim(FEdtHeight.Text) = '' then
  begin
    FEdtHeight.Text := IntToStr(FPrevHeight);
    Exit;
  end;

  try
    NewHeight := StrToInt(Trim(FEdtHeight.Text));
    if (NewHeight >= MIN_HEIGHT) and (NewHeight <= MAX_HEIGHT) then
    begin
      FUpdatingSize := True;
      Height := NewHeight;
      FPrevHeight := NewHeight;
      FUpdatingSize := False;
    end
    else
    begin
      FEdtHeight.Text := IntToStr(FPrevHeight);
    end;
  except
    FEdtHeight.Text := IntToStr(FPrevHeight);
  end;
end;

procedure TRolyPolyForm.EdtWidthKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    EdtWidthExit(Sender);
    Key := #0;
  end;
end;

procedure TRolyPolyForm.EdtHeightKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    EdtHeightExit(Sender);
    Key := #0;
  end;
end;

procedure TRolyPolyForm.BtnDemoPlusClick(Sender: TObject);
begin
  FDemoMode := 1;
  SetControlsEnabled(False);
  FTmrDemo.Enabled := True;
end;

procedure TRolyPolyForm.BtnDemoMinusClick(Sender: TObject);
begin
  FDemoMode := -1;
  SetControlsEnabled(False);
  FTmrDemo.Enabled := True;
end;

procedure TRolyPolyForm.TmrDemoTimer(Sender: TObject);
begin
  if FDemoMode = 1 then
  begin
    if (Width >= MAX_WIDTH) and (Height >= MAX_HEIGHT) then
    begin
      FTmrDemo.Enabled := False;
      FDemoMode := 0;
      SetControlsEnabled(True);
      Exit;
    end;
    if Width < MAX_WIDTH then
      Width := Min(Width + DEMO_STEP, MAX_WIDTH)
    else
      Height := Min(Height + DEMO_STEP, MAX_HEIGHT);
    FPrevWidth := Width;
    FPrevHeight := Height;
    UpdateSizeFields;
  end
  else if FDemoMode = -1 then
  begin
    if (Width <= MIN_WIDTH) and (Height <= MIN_HEIGHT) then
    begin
      FTmrDemo.Enabled := False;
      FDemoMode := 0;
      SetControlsEnabled(True);
      Exit;
    end;
    if Width > MIN_WIDTH then
      Width := Max(Width - DEMO_STEP, MIN_WIDTH)
    else
      Height := Max(Height - DEMO_STEP, MIN_HEIGHT);
    FPrevWidth := Width;
    FPrevHeight := Height;
    UpdateSizeFields;
  end;
end;

procedure TRolyPolyForm.CenterForm;
var
  ScreenW, ScreenH: Integer;
  NewLeft, NewTop: Integer;
begin
  if FCentering then
    Exit;
  FCentering := True;
  try
    ScreenW := Screen.Width;
    ScreenH := Screen.Height;
    NewLeft := (ScreenW - Width) div 2;
    NewTop := (ScreenH - Height) div 2;
    if (Left <> NewLeft) or (Top <> NewTop) then
    begin
      Left := NewLeft;
      Top := NewTop;
    end;
  finally
    FCentering := False;
  end;
end;

procedure TRolyPolyForm.UpdateSizeFields;
begin
  FEdtWidth.Text := IntToStr(Width);
  FEdtHeight.Text := IntToStr(Height);
end;

procedure TRolyPolyForm.SetControlsEnabled(Enabled: Boolean);
begin
  FBtnDemoPlus.Enabled := Enabled;
  FBtnDemoMinus.Enabled := Enabled;
  FEdtWidth.Enabled := Enabled;
  FEdtHeight.Enabled := Enabled;
end;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TRolyPolyForm, RolyPolyForm);
  Application.Run;
end.
