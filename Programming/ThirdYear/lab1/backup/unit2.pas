unit Unit2;

{$mode ObjFPC}

interface

uses Classes, SysUtils, Unit1;

type
    TInt64MemStorage = class(specialize TMemStorage<pointer>)
        protected
	    function getIData(position: qword): Int64;

            procedure setIData(position: qword; value: Int64);

        public
	    property dataI[position:qword]: Int64 read getIData write setIData; default;
    end;

    TExtendedMemStorage = class(specialize TMemStorage<pointer>)
        protected
	    function getEData(position: qword): extended;

            procedure setEData(position: qword; value: extended);
        public
            destructor Destroy; override;

        public
	    property dataE[position: qword]: extended read getEData write setEData; default;
    end;

implementation

function TInt64MemStorage.getIData(position: qword): Int64;
begin
    Result := Int64(getData(position));
end;
procedure TInt64MemStorage.setIData(position: qword; value: Int64);
begin
    setData(position, Pointer(value));
end;


function TExtendedMemStorage.getEData(position: qword): extended;
var p: pointer;
begin
    p := data[position];
    if (p = nil) then exit(0);
    Result := extended(p^);
end;
procedure TExtendedMemStorage.setEData(position: qword; value: extended);
var p: ^extended;
begin
    if (getData(position) = nil) then
    begin
        p := getmem(sizeof(extended));
        data[position] := p;
    end;
    p := data[position];
    p^ := value;
end;

destructor TExtendedMemStorage.Destroy;
var i: integer;
begin
    for i := 0 to getCount-1 do
    begin
        if (data[i] <> nil) then
        begin
            freemem(data[i]);
        end;
    end;
end;

end.

