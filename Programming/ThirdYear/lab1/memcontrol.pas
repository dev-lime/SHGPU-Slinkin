unit MemControl;

{$mode ObjFPC}

interface

uses sysUtils;

type
  TData = pointer;

  TAbstractStorage = class
  protected
    function getData(position: qword): TData; virtual; abstract;
    procedure setData(position: qword; value: TData); virtual; abstract;
    function getCount: qword; virtual; abstract;
    procedure setCount(value: qword); virtual; abstract;
  protected
    property data[position: qword]: TData read getData write setData; default;
  public
    property count: qword read getCount write setCount;
  end;

  TMemStorage = class(TAbstractStorage)
  private
    fdata: array of TData;
  protected
    function getData(position: qword): TData; override;
    procedure setData(position: qword; value: TData); override;
    function getCount: qword; override;
    procedure setCount(value: qword); override;
  end;

  TIntMemStorage = class(TMemStorage)
  protected
    function getIData(position: qword): integer;
    procedure setIData(position: qword; value: integer);
  public
    property dataI[position: qword]: integer read getIData write setIData; default;
  end;

  TFloatMemStorage = class(TMemStorage)
  protected
    function getfData(position: qword): extended;
    procedure setfData(position: qword; value: extended);
  public
    property dataF[position: qword]: extended read getfData write setfData; default;
    destructor Destroy; override;
  end;

  TFileStorage = class(TAbstractStorage)
  private
    fd: file;
  protected
    function getData(position: qword): TData; override;
    procedure setData(position: qword; value: TData); override;
    function getCount: qword; override;
    procedure setCount(value: qword); override;
  public
    constructor Create(filename: string);
    destructor Destroy(); override;
  end;

  TIntFileStorage = class(TFileStorage)
  protected
    function getIData(position: qword): integer;
    procedure setIData(position: qword; value: integer);
  public
    property dataI[position: qword]: integer read getIData write setIData; default;
  end;

implementation

function TMemStorage.getData(position: qword): TData;
begin
  if (position >= getCount) then
    exit(nil);
  Result := fdata[position];
end;

procedure TMemStorage.setData(position: qword; value: TData);
begin
  if (position >= getCount) then
    setCount(position + 1);
  fdata[position] := value;
end;

function TMemStorage.getCount: qword;
begin
  Result := length(fdata);
end;

procedure TMemStorage.setCount(value: qword);
var
  old: qword;
begin
  old := getCount;
  setlength(fdata, value);
  for old := old to value-1 do
    fdata[old] := nil;
end;

function TIntMemStorage.getIData(position: qword): integer;
begin
  Result := integer(getData(position));
end;

procedure TIntMemStorage.setIData(position: qword; value: integer);
begin
  setData(position, Pointer(value));
end;

function TFloatMemStorage.getfData(position: qword): extended;
var
  p: pointer;
begin
  p := data[position];
  if (p = nil) then
    exit(0);
  Result := extended(p^);
end;

procedure TFloatMemStorage.setfData(position: qword; value: extended);
var
  p: ^extended;
begin
  if (getData(position) = nil) then
  begin
    p := getmem(sizeof(extended));
    data[position] := p;
  end;
  p := data[position];
  p^ := value;
end;

destructor TFloatMemStorage.Destroy;
var
  i: integer;
begin
  for i := 0 to getCount-1 do
  begin
    if (data[i] <> nil) then
    begin
      freemem(data[i]);
    end;
  end;
end;

constructor TFileStorage.Create(filename: string);
begin
  inherited Create;
  if (not fileExists(filename)) then
  begin
    assign(fd, filename);
    rewrite(fd);
    close(fd);
  end;
  assign(fd, filename);
  reset(fd, sizeof(TData));
end;

destructor TFileStorage.Destroy();
begin
  close(fd);
end;

function TFileStorage.getData(position: qword): TData;
begin
  if (position < filesize(fd)) then
  begin
    seek(fd, position);
    BlockRead(fd, Result, 1);
  end
  else
    exit(nil);
end;

procedure TFileStorage.setData(position: qword; value: TData);
begin
  seek(fd, position);
  BlockWrite(fd, value, 1);
end;

function TFileStorage.getCount: qword;
begin
  Result := filesize(fd);
end;

procedure TFileStorage.setCount(value: qword);
var
  i: qword;
  p: TData;
begin
  p := nil;
  for i := getCount to value - 1 do
  begin
    seek(fd, i);
    BlockWrite(fd, p, 1);
  end;
end;

function TIntFileStorage.getIData(position: qword): integer;
begin
  Result := integer(getData(position));
end;

procedure TIntFileStorage.setIData(position: qword; value: integer);
begin
  setData(position, Pointer(value));
end;

end.

