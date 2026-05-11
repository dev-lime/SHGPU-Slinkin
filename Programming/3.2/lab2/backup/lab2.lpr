program lab2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

const
  MIN_WIDTH = 500;
  MAX_WIDTH = 1000;
  MIN_HEIGHT = 100;
  MAX_HEIGHT = 500;
  STEP = 10;
  TIMER_INTERVAL = 50;

var
  MainForm: TForm;
  Btn_DemoPlus, Btn_DemoMinus: TButton;
  Edit_Width, Edit_Height: TEdit;
  Lbl_Width, Lbl_Height: TLabel;
  Timer1: TTimer;
  FUpdating: Boolean;
  FLastWidth, FLastHeight: Integer;
  FInitializing: Boolean;

procedure UpdateEditFields;
begin
  Edit_Width.Text := IntToStr(MainForm.Width);
  Edit_Height.Text := IntToStr(MainForm.Height);
end;

procedure UpdateLayout;
var
  TotalWidth, StartX, StartY: Integer;
begin
  TotalWidth := Lbl_Width.Width + 10 + Edit_Width.Width + 10 + Btn_DemoPlus.Width;
  StartX := (MainForm.ClientWidth - TotalWidth) div 2;
  StartY := (MainForm.ClientHeight - 76) div 2;

  Lbl_Width.Left := StartX;
  Edit_Width.Left := StartX + Lbl_Width.Width + 10;
  Btn_DemoPlus.Left := StartX + Lbl_Width.Width + 10 + Edit_Width.Width + 10;

  Lbl_Width.Top := StartY;
  Edit_Width.Top := StartY;
  Btn_DemoPlus.Top := StartY;

  Lbl_Height.Left := StartX;
  Edit_Height.Left := StartX + Lbl_Width.Width + 10;
  Btn_DemoMinus.Left := StartX + Lbl_Width.Width + 10 + Edit_Width.Width + 10;

  Lbl_Height.Top := StartY + 38;
  Edit_Height.Top := StartY + 38;
  Btn_DemoMinus.Top := StartY + 38;
end;

procedure SetControlsEnabled(AEnabled: Boolean);
begin
  Edit_Width.Enabled := AEnabled;
  Edit_Height.Enabled := AEnabled;
  Btn_DemoPlus.Enabled := AEnabled;
  Btn_DemoMinus.Enabled := AEnabled;
end;

procedure Edit_EditingDone(Sender: TObject);
var
  NewWidth, NewHeight: Integer;
begin
  if FUpdating then Exit;

  if not TryStrToInt(Edit_Width.Text, NewWidth) then
  begin
    Edit_Width.Text := IntToStr(FLastWidth);
    Exit;
  end;

  if not TryStrToInt(Edit_Height.Text, NewHeight) then
  begin
    Edit_Height.Text := IntToStr(FLastHeight);
    Exit;
  end;

  if NewWidth < MIN_WIDTH then
  begin
    NewWidth := MIN_WIDTH;
    Edit_Width.Text := IntToStr(NewWidth);
  end
  else if NewWidth > MAX_WIDTH then
  begin
    NewWidth := MAX_WIDTH;
    Edit_Width.Text := IntToStr(NewWidth);
  end;

  if NewHeight < MIN_HEIGHT then
  begin
    NewHeight := MIN_HEIGHT;
    Edit_Height.Text := IntToStr(NewHeight);
  end
  else if NewHeight > MAX_HEIGHT then
  begin
    NewHeight := MAX_HEIGHT;
    Edit_Height.Text := IntToStr(NewHeight);
  end;

  MainForm.Width := NewWidth;
  MainForm.Height := NewHeight;
  UpdateLayout;
end;

procedure Btn_DemoPlusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := 1;
  Timer1.Enabled := True;
end;

procedure Btn_DemoMinusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := -1;
  Timer1.Enabled := True;
end;

procedure Timer1Timer(Sender: TObject);
var
  Direction: Integer;
  NewW, NewH: Integer;
  Finished: Boolean;
begin
  Direction := Timer1.Tag;
  Finished := False;

  if Direction = 1 then
  begin
    NewW := MainForm.Width + STEP;
    NewH := MainForm.Height + STEP;

    if NewW > MAX_WIDTH then
      NewW := MAX_WIDTH;
    if NewH > MAX_HEIGHT then
      NewH := MAX_HEIGHT;

    MainForm.Width := NewW;
    MainForm.Height := NewH;

    if (NewW >= MAX_WIDTH) and (NewH >= MAX_HEIGHT) then
      Finished := True;
  end
  else if Direction = -1 then
  begin
    NewW := MainForm.Width - STEP;
    NewH := MainForm.Height - STEP;

    if NewW < MIN_WIDTH then
      NewW := MIN_WIDTH;
    if NewH < MIN_HEIGHT then
      NewH := MIN_HEIGHT;

    MainForm.Width := NewW;
    MainForm.Height := NewH;

    if (NewW <= MIN_WIDTH) and (NewH <= MIN_HEIGHT) then
      Finished := True;
  end;

  MainForm.Left := (Screen.Width - MainForm.Width) div 2;
  MainForm.Top := (Screen.Height - MainForm.Height) div 2;

  if Finished then
  begin
    Timer1.Enabled := False;
    SetControlsEnabled(True);
  end;
end;

procedure FormChangeBounds(Sender: TObject);
begin
  MainForm.Left := (Screen.Width - MainForm.Width) div 2;
  MainForm.Top := (Screen.Height - MainForm.Height) div 2;
end;

procedure FormResize(Sender: TObject);
begin
  if not FUpdating and not FInitializing then
  begin
    FUpdating := True;
    FLastWidth := MainForm.Width;
    FLastHeight := MainForm.Height;
    UpdateEditFields;
    UpdateLayout;
    FUpdating := False;
  end;
end;

begin
  RequireDerivedFormResource := False;
  Application.Scaled := True;
  Application.Initialize;

  FInitializing := True;
  FUpdating := False;

  MainForm := TForm.Create(nil);
  MainForm.Caption := 'Неваляшка';
  MainForm.Width := 500;
  MainForm.Height := 100;
  MainForm.Left := (Screen.Width - MainForm.Width) div 2;
  MainForm.Top := (Screen.Height - MainForm.Height) div 2;
  MainForm.Constraints.MinWidth := MIN_WIDTH;
  MainForm.Constraints.MaxWidth := MAX_WIDTH;
  MainForm.Constraints.MinHeight := MIN_HEIGHT;
  MainForm.Constraints.MaxHeight := MAX_HEIGHT;
  MainForm.OnResize := @FormResize;
  MainForm.OnChangeBounds := @FormChangeBounds;

  Lbl_Width := TLabel.Create(MainForm);
  Lbl_Width.Parent := MainForm;
  Lbl_Width.Caption := 'Размер по X';
  Lbl_Width.Font.Height := -26;

  Edit_Width := TEdit.Create(MainForm);
  Edit_Width.Parent := MainForm;
  Edit_Width.Text := '500';
  Edit_Width.Font.Height := -14;
  Edit_Width.TabOrder := 0;
  Edit_Width.OnEditingDone := @Edit_EditingDone;

  Lbl_Height := TLabel.Create(MainForm);
  Lbl_Height.Parent := MainForm;
  Lbl_Height.Caption := 'Размер по Y';
  Lbl_Height.Font.Height := -26;

  Edit_Height := TEdit.Create(MainForm);
  Edit_Height.Parent := MainForm;
  Edit_Height.Text := '100';
  Edit_Height.Font.Height := -14;
  Edit_Height.TabOrder := 1;
  Edit_Height.OnEditingDone := @Edit_EditingDone;

  Btn_DemoPlus := TButton.Create(MainForm);
  Btn_DemoPlus.Parent := MainForm;
  Btn_DemoPlus.Caption := 'Демо +';
  Btn_DemoPlus.Font.Height := -14;
  Btn_DemoPlus.TabOrder := 2;
  Btn_DemoPlus.OnClick := @Btn_DemoPlusClick;

  Btn_DemoMinus := TButton.Create(MainForm);
  Btn_DemoMinus.Parent := MainForm;
  Btn_DemoMinus.Caption := 'Демо -';
  Btn_DemoMinus.Font.Height := -14;
  Btn_DemoMinus.TabOrder := 3;
  Btn_DemoMinus.OnClick := @Btn_DemoMinusClick;

  Timer1 := TTimer.Create(MainForm);
  Timer1.Interval := TIMER_INTERVAL;
  Timer1.OnTimer := @Timer1Timer;

  FLastWidth := MainForm.Width;
  FLastHeight := MainForm.Height;

  UpdateLayout;
  UpdateEditFields;

  FInitializing := False;

  MainForm.Show;
  Application.Run;
end.