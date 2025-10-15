program lab2;

uses
  ListsUnit;

procedure TestBasicOperations;
var
  classicList: IList;
  arrayList: IList;
  node: TObject;
begin
  Writeln('=== Тестирование базовых операций ===');

  // Тестирование TClassicList
  classicList := TClassicList.Create;
  Writeln('TClassicList:');

  classicList.addLast(TIntNode.Create(10));
  classicList.addLast(TIntNode.Create(20));
  classicList.addFirst(TIntNode.Create(5));

  node := classicList.first;
  while node <> nil do
  begin
    Write('['); classicList.showNode(node); Write('] ');
    node := classicList.next;
  end;
  Writeln;

  // Тестирование TArrayList
  arrayList := TArrayList.Create;
  Writeln('TArrayList:');

  arrayList.addLast(TStringNode.Create('Hello'));
  arrayList.addLast(TStringNode.Create('World'));
  arrayList.addFirst(TStringNode.Create('Start'));

  node := arrayList.first;
  while node <> nil do
  begin
    Write('['); arrayList.showNode(node); Write('] ');
    node := arrayList.next;
  end;
  Writeln;
end;

procedure TestMergeOperations;
var
  list1, list2, list3: IList;
  node: TObject;
begin
  Writeln;
  Writeln('=== Тестирование операций слияния ===');

  // Простое слияние
  list1 := TClassicList.Create;
  list2 := TClassicList.Create;

  list1.addLast(TIntNode.Create(1));
  list1.addLast(TIntNode.Create(2));
  list1.addLast(TIntNode.Create(3));

  list2.addLast(TIntNode.Create(4));
  list2.addLast(TIntNode.Create(5));

  Writeln('List1 до слияния:');
  node := list1.first;
  while node <> nil do
  begin
    Write('['); list1.showNode(node); Write('] ');
    node := list1.next;
  end;
  Writeln;

  Writeln('List2 для слияния:');
  node := list2.first;
  while node <> nil do
  begin
    Write('['); list2.showNode(node); Write('] ');
    node := list2.next;
  end;
  Writeln;

  ListMerge(list1, list2);

  Writeln('List1 после простого слияния:');
  node := list1.first;
  while node <> nil do
  begin
    Write('['); list1.showNode(node); Write('] ');
    node := list1.next;
  end;
  Writeln;

  // Шахматное слияние
  list1 := TArrayList.Create;
  list3 := TArrayList.Create;

  list1.addLast(TRealNode.Create(1.1));
  list1.addLast(TRealNode.Create(2.2));
  list1.addLast(TRealNode.Create(3.3));

  list3.addLast(TRealNode.Create(4.4));
  list3.addLast(TRealNode.Create(5.5));
  list3.addLast(TRealNode.Create(6.6));

  Writeln('List1 до шахматного слияния:');
  node := list1.first;
  while node <> nil do
  begin
    Write('['); list1.showNode(node); Write('] ');
    node := list1.next;
  end;
  Writeln;

  Writeln('List3 для шахматного слияния:');
  node := list3.first;
  while node <> nil do
  begin
    Write('['); list3.showNode(node); Write('] ');
    node := list3.next;
  end;
  Writeln;

  ListChessMerge(list1, list3);

  Writeln('List1 после шахматного слияния:');
  node := list1.first;
  while node <> nil do
  begin
    Write('['); list1.showNode(node); Write('] ');
    node := list1.next;
  end;
  Writeln;
end;

procedure TestSortOperations;
var
  listToSort, sortedList: IList;
  node: TObject;
begin
  Writeln;
  Writeln('=== Тестирование сортировки ===');

  listToSort := TClassicList.Create;
  sortedList := TClassicList.Create;

  // Элементы в случайном порядке
  listToSort.addLast(TIntNode.Create(5));
  listToSort.addLast(TIntNode.Create(1));
  listToSort.addLast(TIntNode.Create(8));
  listToSort.addLast(TIntNode.Create(3));
  listToSort.addLast(TIntNode.Create(9));
  listToSort.addLast(TIntNode.Create(2));

  Writeln('Список до сортировки:');
  node := listToSort.first;
  while node <> nil do
  begin
    Write('['); listToSort.showNode(node); Write('] ');
    node := listToSort.next;
  end;
  Writeln;

  // Сортировка по возрастанию
  ListSort(sortedList, listToSort, True);

  Writeln('Список после сортировки по возрастанию:');
  node := sortedList.first;
  while node <> nil do
  begin
    Write('['); sortedList.showNode(node); Write('] ');
    node := sortedList.next;
  end;
  Writeln;

  // Проверяет, что исходный список пуст
  Writeln('Исходный список после сортировки (должен быть пуст):');
  node := listToSort.first;
  if node = nil then
    Writeln('Пустой')
  else
  begin
    while node <> nil do
    begin
      Write('['); listToSort.showNode(node); Write('] ');
      node := listToSort.next;
    end;
  end;
  Writeln;
end;

begin
  Writeln('Тестирование реализации списков на Pascal');
  Writeln('=========================================');

  TestBasicOperations;
  TestMergeOperations;
  TestSortOperations;

  Writeln;
  Writeln('Все тесты завершены.');
  Readln;
end.

