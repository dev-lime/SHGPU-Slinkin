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

implementation

end.

