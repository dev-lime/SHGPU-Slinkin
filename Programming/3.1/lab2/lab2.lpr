{
Разработать два несвязанных иерархией класса односвязных списков TClassicList и TArrayList, релизующих интерфейс IList. Максимально возможное количество одних методов интерфейса реализовывать через другие методы интерфейса.
TClassicList реализует классический список, со связкой узлов друг с другом с помощью указатетелей.

TArrayList реализует список c храниением узлов в динамическом массиве.

IList=interface
    // добавляет узел в начало списка
    function addFirst(node:tobject):tobject;
    // добавляет узел в конец списка
    function addLast(node:tobject):tobject;
    // удаляет узел из начала списка
    function deleteFirst:tobject;
    // удаляет узел после переданного узла
    function deleteAfter(prevNode:tobject):tobject;
    // вставляет узел после переданного узла
    function insertAfter(prevNode:tobject; insnode:tobject):tobject;
    // Возращает 0 при равенстве содержимого узлов
    // Возращает 1, если содержимое node1 больше содержимого node2
    // Возращает -1, если содержимое node1 меньше содержимого node2
    function compare(node1,node2:tobject):integer;
    // Перемещает внутренний указатель в начало списка, возращает первый узел списка
    // Если список пуст, возращает nil
    function first:tobject;
    // Перемещает внутренний указатель на следующий узел, возвращает данный узел
    // Если внутренний указатель вышел за пределы списка, возращает nil
    function next:tobject;
    // Перемещает внутренний указатель на конец списка, возвращает последний узел списка
    // Если список пуст, возращает nil
    function last:tobject;
    // Визуализирует узел
    procedure showNode(node:tobject);
    // Создает и возвращает копию узла
    function copyNode(node:tobject):tobject;
    // Уничтожает содержимое списка
    procedure destroyList;
end;
Оба класса манипулируют узлами, порожденными от TObject, содержащими одно значение встроенного типа данных (integer, real или string). Узлы должны поддерживать отношения трихотомии и транзитивности (т.е. быть сортируемыми). Допустимо разработать и использовать различные классы узлов. Рекомендуется использовать узлы с подержкой разных встроенных типов. +5 баллов при использовании двух классов узлов со значениями различных встроенных типов. +10 баллов при использовании трех классов узлов со значениями различных встроенных типов.
Студент самостоятельно формирует классы TClassicList, TArrayList и класс(ы) узлов.

Разработать и проверить работоспособность следующих процедур:

Простое слияние списков. LResult - целевой список, в конец которого копируется содержимое списка L.
procedure ListMerge(LResult:IList; L:IList);
Список L по окончании работы процедуры остается неизменным.

Шахматное слияние списков. LResult - целевой список, в который через один узел копируются элементы списка L. Если списки различаются по длине, до последние узлы списка будут копией окончания более длинного списка.
procedure ListChessMerge(LResult:IList; L:IList);
Список L по окончании работы процедуры остается неизменным.

Сортировка списка. LResult - целевой список, в который будет перемещено содержимое списка L, с упорядочиванием. Если список LResult изначально не пустой, перенести его содержимое в список L. Использовать сортировку вставками. Если ASC=true, использовать сортировку по возрастанию, иначе - по невозрастанию.
procedure ListSort(LResult:IList; L:IList; ASC:boolean);
По окончании сортировки список L должен быть пустым.
}

program project1;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Lists;

procedure WriteList(L: IList; const Separator: string);
var
  Node: TObject;
begin
  Node := L.first;
  while Node <> nil do
  begin
    L.showNode(Node);
    Write(Separator);
    Node := L.next;
  end;
  Writeln;
end;

var
  data, data2: IList;
  dataA, dataA2: IList;
begin
  data := TClassicList.Create;
  data2 := TClassicList.Create;
  dataA := TArrayList.Create;
  dataA2 := TArrayList.Create;

  data.addLast(TIntegerNode.Create(1));
  data.addLast(TFloatNode.Create(2));
  data.addLast(TIntegerNode.Create(3));
  data.addLast(TFloatNode.Create(3.5));
  data.addLast(TIntegerNode.Create(2));

  dataA.addLast(TIntegerNode.Create(12));
  dataA.addLast(TFloatNode.Create(11));
  dataA.addLast(TIntegerNode.Create(50));
  dataA.addLast(TFloatNode.Create(50.5));
  dataA.addLast(TIntegerNode.Create(11));

  data2.addLast(TIntegerNode.Create(1));
  data2.addLast(TFloatNode.Create(6.0001));
  data2.addLast(TIntegerNode.Create(6));
  data2.addLast(TFloatNode.Create(7));
  data2.addLast(TIntegerNode.Create(2));

  dataA2.addLast(TIntegerNode.Create(1));
  dataA2.addLast(TFloatNode.Create(3.5001));
  dataA2.addLast(TIntegerNode.Create(1));
  dataA2.addLast(TFloatNode.Create(3));
  dataA2.addLast(TIntegerNode.Create(1));

  Write('---'); WriteList(data, ' ');
  Write('+++'); WriteList(data2, ' ');
  Write('///'); WriteList(dataA, ' ');
  Write('==='); WriteList(dataA2, ' ');
  Writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'); Writeln;

  // ListMerge(data, dataA);
  // ListChessMerge(data, data2);
  ListSort(data, data2, true);

  Write('---'); WriteList(data, ' ');
  Write('+++'); WriteList(data2, ' ');
  Write('///'); WriteList(dataA, ' ');
  Write('==='); WriteList(dataA2, ' ');

  data.destroyList;
  data2.destroyList;
  dataA.destroyList;
  dataA2.destroyList;
end.

