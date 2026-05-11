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

type
  TFormClass = class of TForm;

  TMainForm = class(TForm)
    Btn_DemoPlus: TButton;
    Btn_DemoMinus: TButton;
    Edit_Width: TEdit;
    Edit_Height: TEdit;
    Lbl_Width: TLabel;
    Lbl_Height: TLabel;
    Timer1: TTimer;
  private
    FUpdating: Boolean;
    FLastWidth: Integer;
    FLastHeight: Integer;
    FInitializing: Boolean;
    procedure Edit_EditingDone(Sender: TObject);
    procedure Btn_DemoPlusClick(Sender: TObject);
    procedure Btn_DemoMinusClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure UpdateEditFields;
    procedure UpdateLayout;
  end;

var
  MainForm: TMainForm;

procedure TMainForm.UpdateEditFields;
begin
  Edit_Width.Text := IntToStr(Width);
  Edit_Height.Text := IntToStr(Height);
end;

procedure TMainForm.UpdateLayout;
var
  TotalWidth, StartX, StartY: Integer;
begin
  TotalWidth := Lbl_Width.Width + 10 + Edit_Width.Width + 10 + Btn_DemoPlus.Width;
  StartX := (ClientWidth - TotalWidth) div 2;
  StartY := (ClientHeight - 76) div 2;

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

procedure TMainForm.SetControlsEnabled(AEnabled: Boolean);
begin
  Edit_Width.Enabled := AEnabled;
  Edit_Height.Enabled := AEnabled;
  Btn_DemoPlus.Enabled := AEnabled;
  Btn_DemoMinus.Enabled := AEnabled;
end;

procedure TMainForm.Edit_EditingDone(Sender: TObject);
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

  Width := NewWidth;
  Height := NewHeight;
  UpdateLayout;
end;

procedure TMainForm.Btn_DemoPlusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := 1;
  Timer1.Enabled := True;
end;

procedure TMainForm.Btn_DemoMinusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := -1;
  Timer1.Enabled := True;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  Direction: Integer;
  NewW, NewH: Integer;
  Finished: Boolean;
begin
  Direction := Timer1.Tag;
  Finished := False;

  if Direction = 1 then
  begin
    NewW := Width + STEP;
    NewH := Height + STEP;
    if NewW > MAX_WIDTH then NewW := MAX_WIDTH;
    if NewH > MAX_HEIGHT then NewH := MAX_HEIGHT;
    Width := NewW;
    Height := NewH;
    if (NewW >= MAX_WIDTH) and (NewH >= MAX_HEIGHT) then Finished := True;
  end
  else if Direction = -1 then
  begin
    NewW := Width - STEP;
    NewH := Height - STEP;
    if NewW < MIN_WIDTH then NewW := MIN_WIDTH;
    if NewH < MIN_HEIGHT then NewH := MIN_HEIGHT;
    Width := NewW;
    Height := NewH;
    if (NewW <= MIN_WIDTH) and (NewH <= MIN_HEIGHT) then Finished := True;
  end;

  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;

  if Finished then
  begin
    Timer1.Enabled := False;
    SetControlsEnabled(True);
  end;
end;

procedure TMainForm.FormChangeBounds(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if not FUpdating and not FInitializing then
  begin
    FUpdating := True;
    FLastWidth := Width;
    FLastHeight := Height;
    UpdateEditFields;
    UpdateLayout;
    FUpdating := False;
  end;
end;

begin
  RequireDerivedFormResource := False;
  Application.Scaled := True;
  Application.Initialize;

  MainForm := TMainForm.CreateNew(Application);
  MainForm.Caption := 'Неваляшка';
  MainForm.Width := 500;
  MainForm.Height := 100;
  MainForm.Left := (Screen.Width - MainForm.Width) div 2;
  MainForm.Top := (Screen.Height - MainForm.Height) div 2;
  MainForm.Constraints.MinWidth := MIN_WIDTH;
  MainForm.Constraints.MaxWidth := MAX_WIDTH;
  MainForm.Constraints.MinHeight := MIN_HEIGHT;
  MainForm.Constraints.MaxHeight := MAX_HEIGHT;
  MainForm.FInitializing := True;
  MainForm.FUpdating := False;

  MainForm.Lbl_Width := TLabel.Create(MainForm);
  MainForm.Lbl_Width.Parent := MainForm;
  MainForm.Lbl_Width.Caption := 'Размер по X';
  MainForm.Lbl_Width.Font.Height := -26;

  MainForm.Edit_Width := TEdit.Create(MainForm);
  MainForm.Edit_Width.Parent := MainForm;
  MainForm.Edit_Width.Text := '500';
  MainForm.Edit_Width.Font.Height := -14;
  MainForm.Edit_Width.TabOrder := 0;
  MainForm.Edit_Width.OnEditingDone := @MainForm.Edit_EditingDone;

  MainForm.Lbl_Height := TLabel.Create(MainForm);
  MainForm.Lbl_Height.Parent := MainForm;
  MainForm.Lbl_Height.Caption := 'Размер по Y';
  MainForm.Lbl_Height.Font.Height := -26;

  MainForm.Edit_Height := TEdit.Create(MainForm);
  MainForm.Edit_Height.Parent := MainForm;
  MainForm.Edit_Height.Text := '100';
  MainForm.Edit_Height.Font.Height := -14;
  MainForm.Edit_Height.TabOrder := 1;
  MainForm.Edit_Height.OnEditingDone := @MainForm.Edit_EditingDone;

  MainForm.Btn_DemoPlus := TButton.Create(MainForm);
  MainForm.Btn_DemoPlus.Parent := MainForm;
  MainForm.Btn_DemoPlus.Caption := 'Демо +';
  MainForm.Btn_DemoPlus.Font.Height := -14;
  MainForm.Btn_DemoPlus.TabOrder := 2;
  MainForm.Btn_DemoPlus.OnClick := @MainForm.Btn_DemoPlusClick;

  MainForm.Btn_DemoMinus := TButton.Create(MainForm);
  MainForm.Btn_DemoMinus.Parent := MainForm;
  MainForm.Btn_DemoMinus.Caption := 'Демо -';
  MainForm.Btn_DemoMinus.Font.Height := -14;
  MainForm.Btn_DemoMinus.TabOrder := 3;
  MainForm.Btn_DemoMinus.OnClick := @MainForm.Btn_DemoMinusClick;

  MainForm.Timer1 := TTimer.Create(MainForm);
  MainForm.Timer1.Interval := TIMER_INTERVAL;
  MainForm.Timer1.OnTimer := @MainForm.Timer1Timer;

  MainForm.FLastWidth := MainForm.Width;
  MainForm.FLastHeight := MainForm.Height;

  MainForm.OnResize := @MainForm.FormResize;
  MainForm.OnChangeBounds := @MainForm.FormChangeBounds;

  MainForm.UpdateLayout;
  MainForm.UpdateEditFields;
  MainForm.FInitializing := False;

  MainForm.Show;
  Application.Run;
end.