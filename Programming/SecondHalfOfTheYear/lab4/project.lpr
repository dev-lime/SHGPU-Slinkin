(*
Разработать класс TPerson, описывающий личность. Класс должен содержать следующие свойства: ФИО (AnsiString), пол(Boolean), дату рождения (TDateTime), номер удостоверения личности (AnsiString), объект отца (TPerson), объект матери (TPerson).

Разработать класс TPersonList, описывающий набор личностей. Класс должен содержать:
1. Свойство-массив, хранящий экземпляры TPerson
2. Средства для получения количества, добавления (в конец), удаления (по индексу) личностей.
3. Дополнительный к основному конструктор для загрузки набора личностей из текстового файла и процедуру, для сохранения набора личностей в текстовом файле. При загрузке-сохранении отца и матери используются их удостоверения личности.
4. Метод тестирования корректности набора личности.

Разработать набор классов исключительных ситуаций, каждая из которых будет генерироваться в строго определенных случаях:
1.1 При загрузке файл отсутствует.
1.2 При загрузке из файла очередное свойство некорректного типа
1.3 При загрузке файл неожиданно завершается.
2.1 При сохранении невозможно создать целевой файл.
3.1 При доступе к свойству-массиву индекс выходит за его пределы.
4.1 При тестировании корректности обнаружены родители одного пола.
4.2 При тестировании корректности обнаружены несколько личностей с одним удостоверением личности.
4.3 При тестировании корректности обнаружен некорректный возраст для одного из родителей (считать детородным возрастом для женщин 12-60 лет, для мужчин 10-70 лет)
4.4 При тестировании корректности обнаружен прямой или косвенный цикл для одного из родителей

Разработать программ / набор программ для проверки разработанных классов. Обеспечить корректную обработку разработанных исключительных ситуаций.
*)

program project;

uses
  SysUtils, PersonClasses;

procedure TestBasicOperations;
var
  List: TPersonList;
  Person1, Person2, Person3: TPerson;
  I: Integer; // Объявляем переменную цикла заранее
begin
  WriteLn('Testing basic operations...');

  List := TPersonList.Create;
  try
    // Создаем несколько личностей
    Person1 := TPerson.Create('John Doe', True, EncodeDate(1980, 1, 15), 'ID001');
    Person2 := TPerson.Create('Jane Smith', False, EncodeDate(1985, 5, 20), 'ID002');
    Person3 := TPerson.Create('Mike Johnson', True, EncodeDate(2010, 10, 10), 'ID003');

    try
      // Устанавливаем родителей
      Person3.Father := Person1;
      Person3.Mother := Person2;

      // Добавляем в список
      List.Add(Person1);
      List.Add(Person2);
      List.Add(Person3);

      // Выводим информацию
      WriteLn('List contains ', List.Count, ' persons:');
      for I := 0 to List.Count - 1 do // Классический синтаксис цикла
        WriteLn('  ', List[I].ToString);

      // Проверяем валидацию
      try
        List.Validate;
        WriteLn('Validation passed successfully');
      except
        on E: Exception do
          WriteLn('Validation error: ', E.Message);
      end;

      // Сохраняем в файл
      List.SaveToFile('persons.txt');
      WriteLn('Saved to file');

      // Загружаем из файла
      List.Clear;
      List.LoadFromFile('persons.txt');
      WriteLn('Loaded from file: ', List.Count, ' persons');

    finally
      // Объекты удаляются списком, не нужно освобождать вручную
    end;

  finally
    List.Free;
  end;
end;

procedure TestExceptions;
var
  List: TPersonList;
  Person1, Person2: TPerson;
begin
  WriteLn('Testing exceptions...');

  // Тестирование исключений при работе с файлами
  List := TPersonList.Create;
  try
    try
      List.LoadFromFile('nonexistent.txt');
    except
      on E: EFileNotFound do
        WriteLn('Caught expected exception: ', E.Message);
    end;

    try
      List.SaveToFile('/invalid/path/persons.txt');
    except
      on E: EFileCreationError do
        WriteLn('Caught expected exception: ', E.Message);
    end;
  finally
    List.Free;
  end;

  // Тестирование исключений валидации
  List := TPersonList.Create;
  try
    Person1 := TPerson.Create('John Doe', True, EncodeDate(1980, 1, 15), 'ID001');
    Person2 := TPerson.Create('Jane Smith', True, EncodeDate(1985, 5, 20), 'ID002');

    try
      List.Add(Person1);
      List.Add(Person2);

      // Попытка установить отца и мать одного пола
      try
        Person1.Father := Person2;
        Person1.Mother := Person2;
        List.Validate;
      except
        on E: ESameGenderParents do
          WriteLn('Caught expected exception: ', E.Message);
      end;

      // Попытка создать дубликат ID
      try
        Person2.ID := 'ID001';
        List.Validate;
      except
        on E: EDublicateID do
          WriteLn('Caught expected exception: ', E.Message);
      end;

      // Некорректный возраст родителя
      try
        Person2.ID := 'ID002';
        Person2.Gender := False;
        Person1.BirthDate := EncodeDate(2020, 1, 1); // Слишком молодой отец
        List.Validate;
      except
        on E: EInvalidParentAge do
          WriteLn('Caught expected exception: ', E.Message);
      end;

    finally
      // Объекты удаляются списком
    end;
  finally
    List.Free;
  end;
end;

begin
  try
    TestBasicOperations;
    WriteLn;
    //TestExceptions;
  except
    on E: Exception do
      WriteLn('Unhandled exception: ', E.Message);
  end;
  ReadLn;
end.

