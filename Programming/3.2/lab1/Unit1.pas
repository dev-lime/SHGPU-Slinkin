{
 Разработать графическое приложение "Неваляшка", обладающее следующими функциональными возможностями:
 1. При изменении размеров или местоположения окна приложения, оно возвращается в центр экрана
 2. При изменении размеров окна приложения, его ширина и высота отображается в полях ввода
 3. Максимальные и минимальные размеры окна фиксированы (ширина: 500-1000, высота: 100-500).
 4. При изменении значения в поле ввода с дальнейшим нажатием клавиши Enter или потерей полем фокуса ввода,
 размеры формы изменяются в соответствии со введенным значением. Если введенное значение не является корректным числом,
то размеры формы не меняются, а поле восстанавливает свое предыдущее значение.
 5. При нажатии кнопки "Демо +" начинается увеличение размеров окна приложения,
 с визуализацией процесса, пока размер окна не достигнет максимальных границ.
 Во время увеличения размеров кнопки и поля формы заблокированы.
 6. При нажатии кнопки "Демо -" начинается уменьшение размеров окна приложения,
 с визуализацией процесса, пока размер окна не достигнет минимальных границ.
 Во время уменьшения размеров кнопки и поля формы заблокированы.
 При решении задания запрещено создавать графические виджеты во время исполнения программы.
}

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

const
  MIN_WIDTH = 500;
  MAX_WIDTH = 1000;
  MIN_HEIGHT = 100;
  MAX_HEIGHT = 500;
  STEP = 10;
  TIMER_INTERVAL = 50;

type

  { TForm1 }

  TForm1 = class(TForm)
    Btn_DemoPlus: TButton;
    Btn_DemoMinus: TButton;
    Edit_Width: TEdit;
    Edit_Height: TEdit;
    Lbl_Width: TLabel;
    Lbl_Height: TLabel;
    Timer1: TTimer;
    procedure Btn_DemoMinusClick(Sender: TObject);
    procedure Btn_DemoPlusClick(Sender: TObject);
    procedure Edit_EditingDone(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FUpdating: Boolean;
    FLastWidth: Integer;
    FLastHeight: Integer;
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure UpdateEditFields;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  Constraints.MinWidth := MIN_WIDTH;
  Constraints.MaxWidth := MAX_WIDTH;
  Constraints.MinHeight := MIN_HEIGHT;
  Constraints.MaxHeight := MAX_HEIGHT;
  FLastWidth := Width;
  FLastHeight := Height;
  UpdateEditFields;
  Timer1.Interval := TIMER_INTERVAL;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if not FUpdating then
  begin
    FUpdating := True;
    FLastWidth := Width;
    FLastHeight := Height;
    UpdateEditFields;
    FUpdating := False;
  end;
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TForm1.Edit_EditingDone(Sender: TObject);
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
end;

procedure TForm1.Btn_DemoPlusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := 1;
  Timer1.Enabled := True;
end;

procedure TForm1.Btn_DemoMinusClick(Sender: TObject);
begin
  SetControlsEnabled(False);
  Timer1.Tag := -1;
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Direction: Integer;
begin
  Direction := Timer1.Tag;

  if Direction = 1 then
  begin
    if (Width >= MAX_WIDTH) and (Height >= MAX_HEIGHT) then
    begin
      Timer1.Enabled := False;
      SetControlsEnabled(True);
      Exit;
    end;
    if Width < MAX_WIDTH then
      Width := Width + STEP;
    if Height < MAX_HEIGHT then
      Height := Height + STEP;
  end
  else if Direction = -1 then
  begin
    if (Width <= MIN_WIDTH) and (Height <= MIN_HEIGHT) then
    begin
      Timer1.Enabled := False;
      SetControlsEnabled(True);
      Exit;
    end;
    if Width > MIN_WIDTH then
      Width := Width - STEP;
    if Height > MIN_HEIGHT then
      Height := Height - STEP;
  end;
end;

procedure TForm1.SetControlsEnabled(AEnabled: Boolean);
begin
  Edit_Width.Enabled := AEnabled;
  Edit_Height.Enabled := AEnabled;
  Btn_DemoPlus.Enabled := AEnabled;
  Btn_DemoMinus.Enabled := AEnabled;
end;

procedure TForm1.UpdateEditFields;
begin
  Edit_Width.Text := IntToStr(Width);
  Edit_Height.Text := IntToStr(Height);
end;

end.