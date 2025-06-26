unit MemControl;
{$mode ObjFPC}
interface uses sysUtils;

type
  TData = Pointer;

  // абстрактный класс с описанием механизма доступа к данным
  TAbstractStorage = class
  protected
    function getData(position: qword): TData; virtual; abstract;
    procedure setData(position: qword; value: TData); virtual; abstract;
    function getCount: qword; virtual; abstract;
  public
    property data[position: qword]: TData read getData write setData; default;
    property count: qword read getCount;
  end;

  // класс с реализацией хранилища в оперативной памяти
  TMemStorage = class(TAbstractStorage)
  private
    fdata: array of TData;
  protected
    function getData(position: qword): TData; override;
    procedure setData(position: qword; value: TData); override;
    function getCount: qword; override;
  end;

  // класс с реализацией хранилища целочисленных данных в оперативной памяти
  TIntMemStorage = class(TMemStorage)
  protected
    function getIData(position: qword): integer;
    procedure setIData(position: qword; value: integer);
  public
    property dataI[position: qword]: integer read getIData write setIData; default;
  end;

  // класс с реализацией хранилища вещественных данных в оперативной памяти
  TFloatMemStorage = class(TMemStorage)
  private
    fExtendedData: array of Extended;
  protected
    function getfData(position: qword): Extended;
    procedure setfData(position: qword; value: Extended);
    function getCount: qword; override;
  public
    property dataF[position: qword]: Extended read getfData write setfData; default;
    constructor Create;
  end;

  // Базовый класс для файлового хранилища
  TFileStorage = class(TAbstractStorage)
  private
    fFileName: string;
    fFile: File;
    fDataSize: Integer;
  protected
    function getData(position: qword): TData; override;
    procedure setData(position: qword; value: TData); override;
    function getCount: qword; override;
  public
    constructor Create(const filename: string; dataSize: Integer);
    destructor Destroy; override;
  end;

  // Класс для хранения целых чисел в файле
  TIntFileStorage = class(TFileStorage)
  protected
    function getIData(position: qword): integer;
    procedure setIData(position: qword; value: integer);
  public
    constructor Create(const filename: string);
    property dataI[position: qword]: integer read getIData write setIData; default;
  end;

implementation

{ TMemStorage }

function TMemStorage.getData(position: qword): TData;
begin
  if position >= Length(fdata) then
    Result := nil
  else
    Result := fdata[position];
end;

procedure TMemStorage.setData(position: qword; value: TData);
begin
  if position >= Length(fdata) then
    SetLength(fdata, position + 1);
  fdata[position] := value;
end;

function TMemStorage.getCount: qword;
begin
  Result := Length(fdata);
end;

{ TIntMemStorage }

function TIntMemStorage.getIData(position: qword): integer;
var
  p: TData;
begin
  p := getData(position);
  if p = nil then
    Result := 0
  else
    Result := Integer(p);
end;

procedure TIntMemStorage.setIData(position: qword; value: integer);
begin
  setData(position, TData(Pointer(value)));
end;

{ TFloatMemStorage }

constructor TFloatMemStorage.Create;
begin
  inherited;
  SetLength(fExtendedData, 0);
end;

function TFloatMemStorage.getfData(position: qword): Extended;
begin
  if position >= Length(fExtendedData) then
    Result := 0.0
  else
    Result := fExtendedData[position];
end;

procedure TFloatMemStorage.setfData(position: qword; value: Extended);
begin
  if position >= Length(fExtendedData) then
    SetLength(fExtendedData, position + 1);
  fExtendedData[position] := value;
end;

function TFloatMemStorage.getCount: qword;
begin
  Result := Length(fExtendedData);
end;

{ TFileStorage }

constructor TFileStorage.Create(const filename: string; dataSize: Integer);
begin
  inherited Create;
  fFileName := filename;
  fDataSize := dataSize;
  AssignFile(fFile, fFileName);
  if FileExists(fFileName) then
    Reset(fFile, fDataSize)
  else
    Rewrite(fFile, fDataSize);
end;

destructor TFileStorage.Destroy;
begin
  CloseFile(fFile);
  inherited;
end;

function TFileStorage.getData(position: qword): TData;
var
  buf: Pointer;
begin
  if position >= FileSize(fFile) then
    Exit(nil);

  Seek(fFile, position);
  GetMem(buf, fDataSize);
  BlockRead(fFile, buf^, 1);
  Result := buf;
end;

procedure TFileStorage.setData(position: qword; value: TData);
begin
  if position >= FileSize(fFile) then
    Seek(fFile, FileSize(fFile));

  Seek(fFile, position);
  if value <> nil then
    BlockWrite(fFile, value^, 1)
  else
  begin
    // записывает нулевые данные
    GetMem(value, fDataSize);
    FillChar(value^, fDataSize, 0);
    BlockWrite(fFile, value^, 1);
    FreeMem(value);
  end;
end;

function TFileStorage.getCount: qword;
begin
  Result := FileSize(fFile);
end;

{ TIntFileStorage }

constructor TIntFileStorage.Create(const filename: string);
begin
  inherited Create(filename, SizeOf(Integer));
end;

function TIntFileStorage.getIData(position: qword): integer;
var
  p: TData;
begin
  p := getData(position);
  if p = nil then
    Result := 0
  else
  begin
    Result := Integer(p^);
    FreeMem(p);
  end;
end;

procedure TIntFileStorage.setIData(position: qword; value: integer);
var
  p: ^Integer;
begin
  GetMem(p, SizeOf(Integer));
  p^ := value;
  setData(position, p);
  FreeMem(p);
end;

end.

