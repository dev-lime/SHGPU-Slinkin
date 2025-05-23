(*
Разработать класс TXCanvas для рисования различных графических объектов,
где X - номер студенческого билета студента. Каждая точка холста описывается цветом в виде триплета RGB.
Рисование обеспечивается сущностью "карандаш", которая описывается цветом RGB и текущим местоположением (позицией).
Запрещается использовать любые существующие средства поддержки графики, за исключением последнего задания.
Проверить работоспособность всех методов класса.
Декларируемые возможности TXCanvas:
1) Методы загрузки и сохранения холста в формате PPM P3 (см. смежный предмет)
2) Методы для изменения и получения размеров холста
3) Методы для изменения и получения текущего цвета
4) Методы для изменения и получения текущей позиции
5) Метод рисования точки в текущей позиции текущим цветом
6) Метод рисования прямоугольника заданного размера в текущей позиции текущим цветом
7) Метод рисования закрашенного прямоугольника заданного размера в текущей позиции текущим цветом
8) Метод рисования отрезка текущим цветом в заданную позицию из текущей с перемещением карандаша в конечную позицию
9) Метод закраски ограниченной области в текущей позиции текущим цветом (см. смежный предмет)
10) Метод накладывания стороннего холста на текущий холст в текущую позицию, считая прозрачным текущим цвет
11) Методы загрузки и сохранения холста в форматах JPG,PNG,PCX и других, поддерживаемых пакетом fcl-image.
*)

program TestXCanvas;

uses
  XCanvas, SysUtils;

var
  Canvas: TXCanvas;
  Pos: TPoint;
  Color: TRGB;
  i: Integer;
begin
  try
    // Создаем холст
    Canvas := TXCanvas.Create(800, 600);
    try
      // Тест 1: Очистка холста и установка цвета
      Canvas.ClearWithColor(200, 200, 255);
      Canvas.SetColor(255, 0, 0);

      // Тест 2: Рисование точек
      Canvas.MoveTo(100, 100);
      Canvas.DrawPoint;

      // Тест 3: Рисование линий
      Canvas.MoveTo(50, 50);
      Canvas.DrawLineTo(150, 150);

      // Тест 4: Рисование прямоугольников
      Canvas.SetColor(0, 255, 0);
      Canvas.MoveTo(200, 200);
      Canvas.DrawRect(100, 80);

      Canvas.SetColor(0, 0, 255);
      Canvas.MoveTo(300, 300);
      Canvas.DrawFilledRect(120, 90);

      // Тест 5: Заливка области
      Canvas.SetColor(255, 255, 0);
      Canvas.MoveTo(400, 400);
      Canvas.FloodFill;

      // Тест 6: Сохранение в PPM
      Canvas.SaveToPPM('test.ppm');

      // Тест 7: Загрузка из PPM
      Canvas.LoadFromPPM('test.ppm');

      // Тест 8: Сохранение в других форматах
      Canvas.SaveToFile('test.png');
      Canvas.SaveToFile('test.jpg');

      // Тест 9: Получение информации
      Writeln('Canvas size: ', Canvas.GetWidth, 'x', Canvas.GetHeight);
      Pos := Canvas.GetPosition;
      Writeln('Current position: ', Pos.X, ',', Pos.Y);
      Color := Canvas.GetColor;
      Writeln('Current color: R=', Color.R, ' G=', Color.G, ' B=', Color.B);

      // Тест 10: Создание второго холста и наложение
      Canvas.ClearWithColor(0, 0, 0); // Черный фон
      Canvas.SetColor(0, 0, 0); // Прозрачный цвет - черный

      // Создаем второй холст с красным кругом
      with TXCanvas.Create(100, 100) do
      try
        ClearWithColor(0, 0, 0); // Черный фон
        SetColor(255, 0, 0); // Красный цвет

        // Рисуем круг (аппроксимация)
        for i := 0 to 359 do
        begin
          MoveTo(50 + Round(40 * Cos(i * Pi / 180)),
                50 + Round(40 * Sin(i * Pi / 180)));
          DrawPoint;
        end;

        // Наложение на основной холст
        Canvas.MoveTo(350, 250);
        Canvas.BlendCanvas(TXCanvas(self));
      finally
        Free;
      end;

      // Сохраняем результат
      Canvas.SaveToFile('blended.png');

      Writeln('All tests completed successfully.');
    finally
      Canvas.Free;
    end;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.

