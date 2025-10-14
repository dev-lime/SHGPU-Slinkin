(*
Для хранения данных с прямым доступом используются различные структуры - основанные на указателях, статические и динамические массивы, файлы, разделяемая память и т.п. В ООП, для универсализации подхода к хранению данных, обычно разрабатывается иерархия классов, объекты которых позволяют обеспечить единообразный доступ к данным, вне зависимости от способа хранения. Задача заключается в разработке такой иерархии.

1. Разработать и реализовать иерахию классов для хранения данных любых типов в оперативной памяти с прямым доступом по индексу. Доступ к данным производится через свойство-массив по умолчанию, с беззнаковым индексом и специфичным типом данных. Предусмотреть обработку целочисленных и вещественных типов данных. Структура иерархии:
 TAbstractStorage ─➤ TMemStorage ─➤ TIntMemStorage
                          ╰───────➤ TFloatMemStorage
Описание классов и вспомогательных типов данных:
type
 // базовый тип элемента данных в хранилище
 TData=pointer;

 // абстрактный класс с описанием механизма доступа к данным
 { TAbstractStorage }

 TAbstractStorage=class
    protected
	function getData(position:qword):TData;virtual;abstract;
	procedure setData(position:qword; value:TData);virtual;abstract;
	function getCount:qword;virtual;abstract;
    protected
	property data[position:qword]:TData read getData write setData; default;
	property count:qword read getcount;
 end;

 // класс с реализацией хранилища в оперативной памяти
 // с автоматическим расширением хранилища при попытке сохранения
 // данных за его пределами и возвратом нулевого значения
 // при попытке чтения данных за его пределами.
 { TMemStorage }

 TMemStorage=class(TAbstractStorage)
    private
	fdata:array of TData;
    protected
	function getData(position:qword):TData;override;
	procedure setData(position:qword; value:TData);override;
	function getCount:qword;override;
 end;


 // класс с реализацией хранилища целочисленных данных в оперативной памяти
 // с преобразованием типа данных integer в TData и обратно.
 { TIntMemStorage }

 TIntMemStorage=class(TMemStorage)
    protected
	function getIData(position:qword):integer;
	procedure setIData(position:qword; value:integer);
    public
	property dataI[position:qword]:integer read getIData write setIData; default;
    end;

 // класс с реализацией хранилища вещественных данных максимальной емкости в оперативной памяти
 // с выделением из оперативной памяти требуемого объема при первом сохранении элемента
 // по каждому индексу и освобождением выделенной памяти деструктором.
 // Преобразование TData <-> extended по аналогии с классом TIntMemStorage невозможно,
 // так как размер типа extended превышает размер типа TData.
 { TFloatMemStorage }

 TFloatMemStorage=class(TMemStorage)
    protected
	function getfData(position:qword):extended;
	procedure setfData(position:qword; value:extended);
    public
	property dataF[position:qword]:extended read getfData write setfData; default;
	destructor Destroy;override;
    end;
Пример тестовой программы	Результат
var x:TFloatMemStorage;
    y:TIntMemStorage;
begin
    x:=TFloatMemStorage.Create;
    x[2]:=1.2;
    writeln(x[0]:0:2);
    writeln(x[2]:0:2);
    writeln(x.count);
    x.free;
    y:=TIntMemStorage.Create;
    y[5]:=7;
    y[3]:=8;
    writeln(y[1]);
    writeln(y[3]);
    writeln(y[5]);
    writeln(y.count);
    y.free;
end.

2. Расширить полученную иерархию классом хранилища в нетипированном файле данных любых типов с прямым доступом по индексу. Разработать класс для хранения целочисленных данных в файле. Общая структура иерархии:
 TAbstractStorage ─➤ TMemStorage ─➤ TIntMemStorage
        │                 ╰───────➤ TFloatMemStorage
        ╰──────────➤ TFileStorage ─➤ TIntFileStorage
Проверить работоспособность созданных классов.
*)

(*
Модифицировать решение ЛР3. Свойства класса прошлого полугодия.
Удалить определение типа TData, его заменит шаблон в дженериках.
Класс TAbstractStorage преобразовать в дженерик, указав TData в качестве шаблона.
Пример:

generic TAbstractStorage<TData>=class
...
Классы TMemStorage и TFileStorage преобразовать в дженерики, с наследованием от специализации TAbstractStorage.
Пример:
generic TMemStorage<TData>=class(specialize TAbstractStorage<TData>)
...
Специализировать:
TMemStorage в классы TInt64MemStorage (хранение значений Int64) и TExtendedMemStorage (хранение значений Extended)
TFileStorage в классы TBуteFileStorage (хранение значений Byte) и TStringFileStorage (хранение значений ShortString)
Протестировать работу специализированных классов.
*)

program project1;

uses unit1, Unit2, Unit3;

var x: TExtendedMemStorage;
    y: TInt64MemStorage;
    z: TByteFileStorage;
    s: TStringFileStorage;
begin
    writeln('TExtendedMemStorage');
    x := TExtendedMemStorage.Create;
    x[2] := 1.6;
    writeln(x[0]:0:2,' ',x[2]:0:2);
    writeln(x.count);
    x.free;

    writeln;
    writeln('TInt64MemStorage');
    y := TInt64MemStorage.Create;
    y[2] := 7;
    y[6] := 8;
    writeln(y[1],' ',y[2],' ',y[3],' ',y[4],' ',y[5],' ',y[6],' ',y[100000]);
    y[100000] := 0;
    y[50] := 1;
    writeln(y[1],' ',y[2],' ',y[6],' ',y[100000],' ',y[50]);
    writeln(y.count);
    y.free;

    writeln;
    writeln('TByteFileStorage');
    z := TByteFileStorage.Create('storage.dat');
    z[0] := 1;
    z[2] := 11;
    writeln(z[3],' ',z[2],' ',z[1],' ',z[0]);
    z[50] := 123;
    writeln(z[50],' ',z[100]);
    writeln(z.count);
    z.free;

    writeln;
    writeln('TStringFileStorage');
    s := TStringFileStorage.Create('storage1.dat');
    s[1] := 'meta';
    writeln(s[1],' ',s[2]);
    s[2] := 'beretta';
    s[0] := 'kotleta';
    s[50000] := 'dr';
    writeln(s[1],' ',s[2],' ',s[0],' ',s[50000]);
    writeln(s[3]);
    writeln(s.count);
    s.count := 100;
    writeln(s.count);
    s.free;

    readln;
end.
