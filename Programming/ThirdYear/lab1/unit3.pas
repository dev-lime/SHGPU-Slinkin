unit Unit3;

{$mode ObjFPC}

interface

uses Classes, SysUtils, unit1;

type
    generic TFileStorage<TData> = class(specialize TAbstractStorage<TData>)
        public
            constructor Create(filename: string);
            destructor Destroy(); override;

            property dataT[position: qword]: TData read getData write setData; default;
        protected
            function getData(position: qword): TData; override;
            function getCount: qword; override;

            procedure setCount(value: qword); override;
            procedure setData(position: qword; value: TData); override;
        private
            fd: file;
    end;

    TByteFileStorage = specialize TFileStorage<Byte>;
    TStringFileStorage = specialize TFileStorage<Shortstring>;

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
        exit(TData(''));
end;
procedure TFileStorage.setData(position: qword; value: TData);
begin
    if (value <> TData('')) or (position < filesize(fd)) then
    begin
        seek(fd, position);
        BlockWrite(fd, value, 1);
    end;
end;

function TFileStorage.getCount: qword;
begin
    Result := filesize(fd);
end;
procedure TFileStorage.setCount(value: qword);
var p: TData;
begin
    p := TData('');
    seek(fd, value);
    if (value >= getCount) then
        BlockWrite(fd, p, 1)
    else
        Truncate(fd);
end;

end.

