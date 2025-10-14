unit Unit2;

{$mode ObjFPC}

interface

uses Classes, SysUtils, Unit1;

type
    generic TMemStorage<TData> = class(specialize TAbstractStorage<TData>)
        protected
	    function getData(position: qword): TData; override;
	    function getCount: qword; override;

            procedure setData(position: qword; value: TData); override;
	    procedure setCount(value: qword); override;
        public
    	    property dataT[position:qword]: TData read getData write setData; default;
        private
	    fdata: array of TData;
    end;

    TInt64MemStorage = specialize TMemStorage<Int64>;
    TExtendedMemStorage = specialize TMemStorage<Extended>;

implementation

function TMemStorage.getData(position: qword): TData;
begin
    if (position >= getCount) then exit(0);
    Result := fdata[position];
end;
procedure TMemStorage.setData(position: qword; value: TData);
begin
    if (value <> TData(0)) or (position < getCount) then
    begin
        if (position >= getCount) then
            setCount(position + 1);
        fdata[position] := value;
    end;
end;

function TMemStorage.getCount: qword;
begin
    Result := Length(fdata);
end;
procedure TMemStorage.setCount(value: qword);
var old: integer;
begin
    old := getCount;
    setlength(fdata, value);
    for old := old to value-1 do
        fdata[old] := 0;
end;

end.

