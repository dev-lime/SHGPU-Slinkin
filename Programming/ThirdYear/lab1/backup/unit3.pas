unit Unit3;

{$mode ObjFPC}

interface

uses Classes, SysUtils, unit1;

type
    generic TFileStorage<TData> = class(specialize TAbstractStorage<TData>)
        public
            constructor Create(filename: string);
            destructor Destroy(); override;
        protected
            function getData(position: qword): TData; override;
            function getCount: qword; override;

            procedure setCount(value: qword); override;
            procedure setData(position: qword; value: TData); override;
        private
            fd: file;
    end;

    TByteFileStorage = class(specialize TFileStorage<pointer>)
        protected
            function getBData(position: qword): Byte;

            procedure setBData(position: qword; value: Byte);
        public
            property dataB[position: qword]: Byte read getBData write setBData; default;
    end;

    TStringFileStorage = class(specialize TFileStorage<Pointer>)
        protected
            function getSData(position: qword): Char;
            function getS1Data: Shortstring;

            procedure setSData(position: qword; value: Char);
            procedure setS1Data(value: Shortstring);
        public
            property dataS[position: qword]: Char read getSData write setSData; default;
            property dataS1: Shortstring read getS1Data write setS1Data;
    end;

implementation

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
    if (position >= getCount) then
        setCount(position + 1);
    seek(fd, position);
    BlockWrite(fd, value, 1);
end;

function TFileStorage.getCount: qword;
begin
    Result := filesize(fd);
end;
procedure TFileStorage.setCount(value: qword);
var i: integer; p: TData;
begin
    p := nil;
    for i := getCount to value - 1 do
    begin
        seek(fd, i);
        BlockWrite(fd, p, 1);
    end;
end;


function TByteFileStorage.getBData(position: qword): Byte;
begin
    Result := Byte(getData(position));
end;
procedure TByteFileStorage.setBData(position: qword; value: Byte);
begin
    setData(position, Pointer(value));
end;

function TStringFileStorage.getSData(position: qword): Char;
begin
    Result := Char(getData(position));
end;
procedure TStringFileStorage.setSData(position: qword; value: Char);
var b: byte;
begin
    b := ord(value);
    setData(position, Pointer(b));
end;

function TStringFileStorage.getS1Data: Shortstring;
var str: Shortstring;
    i: integer;
begin
    str[0] := Chr(getCount);
    for i := 1 to getCount do
        str[i] := getSData(i-1);
    Result := str;
end;
procedure TStringFileStorage.setS1Data(value: Shortstring);
var i: integer;
begin
    for i:=0 to Length(value)-1 do
        setSData(i, value[i+1]);
end;


end.

