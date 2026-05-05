unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnDemoPlus: TButton;
    btnDemoMinus: TButton;
    edtWidth: TEdit;
    edtHeight: TEdit;
    lblWidth: TLabel;
    lblHeight: TLabel;
    tmrDemo: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMove(Sender: TObject);
    procedure edtWidthExit(Sender: TObject);
    procedure edtHeightExit(Sender: TObject);
    procedure edtWidthKeyPress(Sender: TObject; var Key: char);
    procedure edtHeightKeyPress(Sender: TObject; var Key: char);
    procedure btnDemoPlusClick(Sender: TObject);
    procedure btnDemoMinusClick(Sender: TObject);
    procedure tmrDemoTimer(Sender: TObject);
  private
    FPrevWidth: Integer;
    FPrevHeight: Integer;
    FDemoMode: Integer;
    FUpdatingSize: Boolean;
    FCentering: Boolean;
    procedure CenterForm;
    procedure UpdateSizeFields;
    procedure SetControlsEnabled(Enabled: Boolean);
  public
  end;

var
  Form1: TForm1;

const
  MIN_WIDTH = 500;
  MAX_WIDTH = 1000;
  MIN_HEIGHT = 100;
  MAX_HEIGHT = 500;
  DEMO_STEP = 10;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := MIN_WIDTH;
  Constraints.MaxWidth := MAX_WIDTH;
  Constraints.MinHeight := MIN_HEIGHT;
  Constraints.MaxHeight := MAX_HEIGHT;

  FPrevWidth := Width;
  FPrevHeight := Height;
  FDemoMode := 0;
  FUpdatingSize := False;
  FCentering := False;

  UpdateSizeFields;
  CenterForm;
end;

procedure TForm1.FormResize(Sender: TObject);
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

procedure TForm1.FormMove(Sender: TObject);
begin
  CenterForm;
end;

procedure TForm1.edtWidthExit(Sender: TObject);
var
  NewWidth: Integer;
begin
  if Trim(edtWidth.Text) = '' then
  begin
    edtWidth.Text := IntToStr(FPrevWidth);
    Exit;
  end;

  try
    NewWidth := StrToInt(Trim(edtWidth.Text));
    if (NewWidth >= MIN_WIDTH) and (NewWidth <= MAX_WIDTH) then
    begin
      FUpdatingSize := True;
      Width := NewWidth;
      FPrevWidth := NewWidth;
      FUpdatingSize := False;
    end
    else
    begin
      edtWidth.Text := IntToStr(FPrevWidth);
    end;
  except
    edtWidth.Text := IntToStr(FPrevWidth);
  end;
end;

procedure TForm1.edtHeightExit(Sender: TObject);
var
  NewHeight: Integer;
begin
  if Trim(edtHeight.Text) = '' then
  begin
    edtHeight.Text := IntToStr(FPrevHeight);
    Exit;
  end;

  try
    NewHeight := StrToInt(Trim(edtHeight.Text));
    if (NewHeight >= MIN_HEIGHT) and (NewHeight <= MAX_HEIGHT) then
    begin
      FUpdatingSize := True;
      Height := NewHeight;
      FPrevHeight := NewHeight;
      FUpdatingSize := False;
    end
    else
    begin
      edtHeight.Text := IntToStr(FPrevHeight);
    end;
  except
    edtHeight.Text := IntToStr(FPrevHeight);
  end;
end;

procedure TForm1.edtWidthKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    edtWidthExit(Sender);
    Key := #0;
  end;
end;

procedure TForm1.edtHeightKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    edtHeightExit(Sender);
    Key := #0;
  end;
end;

procedure TForm1.btnDemoPlusClick(Sender: TObject);
begin
  FDemoMode := 1;
  SetControlsEnabled(False);
  tmrDemo.Enabled := True;
end;

procedure TForm1.btnDemoMinusClick(Sender: TObject);
begin
  FDemoMode := -1;
  SetControlsEnabled(False);
  tmrDemo.Enabled := True;
end;

procedure TForm1.tmrDemoTimer(Sender: TObject);
begin
  if FDemoMode = 1 then
  begin
    if (Width >= MAX_WIDTH) and (Height >= MAX_HEIGHT) then
    begin
      tmrDemo.Enabled := False;
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
      tmrDemo.Enabled := False;
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

procedure TForm1.CenterForm;
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

procedure TForm1.UpdateSizeFields;
begin
  edtWidth.Text := IntToStr(Width);
  edtHeight.Text := IntToStr(Height);
end;

procedure TForm1.SetControlsEnabled(Enabled: Boolean);
begin
  btnDemoPlus.Enabled := Enabled;
  btnDemoMinus.Enabled := Enabled;
  edtWidth.Enabled := Enabled;
  edtHeight.Enabled := Enabled;
end;

end.
