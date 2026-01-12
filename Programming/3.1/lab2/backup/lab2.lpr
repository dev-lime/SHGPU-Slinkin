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

program lab2;

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
  intList1, intList2: IList;
  floatList1, floatList2: IList;
begin
  try
    intList1 := TClassicList.Create;
    intList2 := TClassicList.Create;

    floatList1 := TArrayList.Create;
    floatList2 := TArrayList.Create;

    Writeln('Тест для целых чисел');
    intList1.addLast(TIntegerNode.Create(5));
    intList1.addLast(TIntegerNode.Create(1));
    intList1.addLast(TIntegerNode.Create(8));
    intList1.addLast(TIntegerNode.Create(3));
    intList1.addLast(TIntegerNode.Create(6));

    intList2.addLast(TIntegerNode.Create(2));
    intList2.addLast(TIntegerNode.Create(7));
    intList2.addLast(TIntegerNode.Create(4));
    intList2.addLast(TIntegerNode.Create(9));
    intList2.addLast(TIntegerNode.Create(0));

    Write('Список 1: '); WriteList(intList1, ' ');
    Write('Список 2: '); WriteList(intList2, ' ');

    // Слияние
    ListMerge(intList1, intList2);
    Write('После простого слияния: '); WriteList(intList1, ' ');

    intList1.destroyList;
    intList2.destroyList;

    intList1.addLast(TIntegerNode.Create(1));
    intList1.addLast(TIntegerNode.Create(3));
    intList1.addLast(TIntegerNode.Create(5));

    intList2.addLast(TIntegerNode.Create(2));
    intList2.addLast(TIntegerNode.Create(4));
    intList2.addLast(TIntegerNode.Create(6));

    Write('Новый список 1: '); WriteList(intList1, ' ');
    Write('Новый список 2: '); WriteList(intList2, ' ');

    ListChessMerge(intList1, intList2);
    Write('После шахматного слияния: '); WriteList(intList1, ' ');

    // Сортировка
    intList1.destroyList;
    intList2.destroyList;

    intList2.addLast(TIntegerNode.Create(9));
    intList2.addLast(TIntegerNode.Create(2));
    intList2.addLast(TIntegerNode.Create(7));
    intList2.addLast(TIntegerNode.Create(1));
    intList2.addLast(TIntegerNode.Create(5));

    Write('Список для сортировки: '); WriteList(intList2, ' ');

    ListSort(intList1, intList2, True);
    Write('После сортировки по возрастанию: '); WriteList(intList1, ' ');

    Writeln;
    Writeln('Тест для вещественных чисел');

    floatList1.addLast(TFloatNode.Create(3.14));
    floatList1.addLast(TFloatNode.Create(2.71));
    floatList1.addLast(TFloatNode.Create(1.41));

    floatList2.addLast(TFloatNode.Create(1.61));
    floatList2.addLast(TFloatNode.Create(0.57));
    floatList2.addLast(TFloatNode.Create(2.35));

    Write('Вещественный список 1: '); WriteList(floatList1, ' ');
    Write('Вещественный список 2: '); WriteList(floatList2, ' ');

    ListSort(floatList1, floatList2, False);
    Write('После сортировки по невозрастанию: '); WriteList(floatList1, ' ');

    Writeln;
    Writeln('Тест для строк');

    try
      intList1.destroyList;
      intList1.addLast(TStringNode.Create('banana'));
      intList1.addLast(TStringNode.Create('apple'));
      intList1.addLast(TStringNode.Create('cherry'));

      Write('Строковый список: '); WriteList(intList1, ' ');
    except
      on E: Exception do
        Writeln('Ожидаемая ошибка при смешивании типов: ', E.Message);
    end;

    // Очистка
    intList1.destroyList;
    intList2.destroyList;
    floatList1.destroyList;
    floatList2.destroyList;

    Writeln('Все тесты завершены успешно!');

  except
    on E: Exception do
    begin
      Writeln('Ошибка: ', E.Message);
      Readln;
    end;
  end;
end.

