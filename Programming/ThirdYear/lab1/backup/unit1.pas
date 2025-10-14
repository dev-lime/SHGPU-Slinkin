unit Unit1;

{$mode ObjFPC}

interface

uses Classes, SysUtils;

type
    generic TAbstractStorage<TData> = class
        protected
	    function getData(position: qword): TData; virtual; abstract;
	    function getCount: qword; virtual; abstract;

            procedure setData(position: qword; value: TData); virtual; abstract;
	    procedure setCount(value: qword); virtual; abstract;

        protected
	    property data[position: qword]: TData read getData write setData; default;
        public
            property count: qword read getCount write setCount;
    end;

    generic TMemStorage<TData> = class(specialize TAbstractStorage<TData>)
        protected
	    function getData(position: qword): TData; override;
	    function getCount: qword; override;

            procedure setData(position: qword; value: TData); override;
	    procedure setCount(value: qword); override;
        private
	    fdata: array of TData;
    end;

implementation

function TMemStorage.getData(position: qword): TData;
begin
    if (position >= getCount) then exit(nil);
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
    Result := Length(fdata);
end;
procedure TMemStorage.setCount(value: qword);
var old: integer;
begin
    old := getCount;
    setlength(fdata, value);
    for old := old to value-1 do
        fdata[old] := nil;
end;

end.

