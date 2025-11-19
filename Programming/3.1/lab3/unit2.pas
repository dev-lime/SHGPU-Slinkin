unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils;

type
    TArrayStream = class(TStream)
        private
            FData: array of Byte;
            FPos: Int64;
        public
            constructor Create;
            destructor Destroy; override;

            function Read(var Buffer; Count: LongInt): LongInt; override;
            function Write(const Buffer; Count: LongInt): LongInt; override;
            function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

            function GetData: TBytes;
    end;

implementation

constructor TArrayStream.Create;
begin
    inherited Create;
    FPos := 0; SetLength(FData, 0);
end;
destructor TArrayStream.Destroy;
begin
    SetLength(FData, 0);
end;

function TArrayStream.Read(var Buffer; Count: LongInt): LongInt;
var BytesAvailable: LongInt;
begin
    BytesAvailable := Length(FData) - FPos;
    if BytesAvailable <= 0 then Exit(0);
    if Count > BytesAvailable then Count := BytesAvailable;

    Move(FData[FPos], Buffer, Count);
    Inc(FPos, Count);
    Result := Count;
end;
function TArrayStream.Write(const Buffer; Count: LongInt): LongInt;
begin
    if FPos + Count > Length(FData) then
        SetLength(FData, FPos + Count);

    Move(Buffer, FData[FPos], Count);
    Inc(FPos, Count);
    Result := Count;
end;

function TArrayStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    case Origin of
        soBeginning: FPos := Offset;
        soCurrent: FPos := FPos + Offset;
        soEnd: FPos := Length(FData) + Offset;
    end;
    if FPos < 0 then FPos := 0;
    //if FPos > Length(FData) then FPos := Length(FData);
    Result := FPos;
end;

function TArrayStream.GetData: TBytes;
begin
    Result := FData;
end;


end.

