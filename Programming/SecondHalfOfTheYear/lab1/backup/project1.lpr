(*
Разработать класс TXCanvas для рисования различных графических объектов, где X - номер студенческого билета студента. Каждая точка холста описывается цветом в виде триплета RGB. Рисование обеспечивается сущностью "карандаш", которая описывается цветом RGB и текущим местоположением (позицией). Запрещается использовать любые существующие средства поддержки графики, за исключением последнего задания. Проверить работоспособность всех методов класса.

Декларируемые возможности TXCanvas:
1. Методы загрузки и сохранения холста в формате PPM P3 (см. смежный предмет)
2. Методы для изменения и получения размеров холста
3. Методы для изменения и получения текущего цвета
4. Методы для изменения и получения текущей позиции
5. Метод рисования точки в текущей позиции текущим цветом
6. Метод рисования прямоугольника заданного размера в текущей позиции текущим цветом
7. Метод рисования закрашенного прямоугольника заданного размера в текущей позиции текущим цветом
8. Метод рисования отрезка текущим цветом в заданную позицию из текущей с перемещением карандаша в конечную позицию
9. Метод закраски ограниченной области в текущей позиции текущим цветом (см. смежный предмет)
10. Метод накладывания стороннего холста на текущий холст в текущую позицию, считая прозрачным текущим цвет
11. Методы загрузки и сохранения холста в форматах JPG,PNG,PCX и других, поддерживаемых пакетом fcl-image.
*)

program project1;

uses
  XCanvas;

var
  Canvas, Overlay: TXCanvas;
  Color: TRGB;
begin
  Canvas := TXCanvas.Create;
  Canvas.Initialize(800, 600);

  // Установка цвета карандаша
  // 1 - через отдельные компоненты
  Canvas.SetPencilColor(255, 0, 0); // красный

  // 2 - через запись TRGB
  Color.R := 0;
  Color.G := 255;
  Color.B := 0;
  Canvas.SetPencilColor(Color); // Зеленый

  // Перемещение карандаша и рисование точки
  Canvas.MovePencilTo(50, 50);
  Canvas.DrawDot; // зеленая точка в (50,50)

  // Рисование прямоугольника (контур)
  Canvas.SetPencilColor(0, 0, 255); // Синий
  Canvas.MovePencilTo(100, 100);
  Canvas.DrawRectangle(150, 100);

  // Рисование закрашенного прямоугольника
  Canvas.SetPencilColor(255, 255, 0); // Желтый
  Canvas.MovePencilTo(300, 200);
  Canvas.DrawFilledRectangle(120, 80);

  // Рисование линии
  Canvas.SetPencilColor(128, 0, 128); // Фиолетовый
  Canvas.MovePencilTo(400, 400);
  Canvas.DrawLineTo(600, 500); // Линия от 400 400 до 600 500

  // Заливка области
  Canvas.SetPencilColor(0, 255, 255); // Голубой
  Canvas.MovePencilTo(150, 150);
  Canvas.FloodFill;

  // Наложение холстов
  Overlay := TXCanvas.Create;
  try
    Overlay.Initialize(100, 100);
    Overlay.SetPencilColor(255, 0, 255); // Розовый
    Overlay.MovePencilTo(0, 0);
    Overlay.DrawFilledRectangle(100, 100); // Полностью розовый холст

    // Делает часть прозрачной (совпадает с текущим цветом карандаша)
    Overlay.SetPencilColor(0, 255, 255);
    Overlay.MovePencilTo(20, 20);
    Overlay.DrawFilledRectangle(60, 60);

    // Наложение на основной холст
    Canvas.MovePencilTo(200, 300);
    Canvas.DrawCanvas(Overlay);
  finally
    Overlay.Free;
  end;

  // Сохранение в PPM
  Canvas.SaveToPPM('demo.ppm');

  // Загрузка из PPM и изменение
  Canvas.LoadFromPPM('demo.ppm');
  Canvas.SetPencilColor(255, 165, 0); // Оранжевый
  Canvas.MovePencilTo(700, 50);
  Canvas.DrawDot;
  Canvas.SaveToPPM('modified.ppm');

  // Получение информации о холсте
  Writeln('Ширина холста: ', Canvas.GetWidth);
  Writeln('Высота холста: ', Canvas.GetHeight);

  // Получение цвета пикселя
  Color := Canvas.GetPixel(50, 50);
  Writeln('Цвет в точке (50,50): R=', Color.R, ' G=', Color.G, ' B=', Color.B);

  // Изменение размера холста
  Canvas.Resize(400, 400);
  Canvas.SetPencilColor(255, 255, 255); // Белый
  Canvas.MovePencilTo(0, 0);
  Canvas.DrawFilledRectangle(400, 400); // Очищаем холст
  Canvas.SetPencilColor(0, 0, 0); // Черный
  Canvas.MovePencilTo(200, 200);
  Canvas.DrawLineTo(300, 300);
  Canvas.SaveToPPM('resized.ppm');

  Canvas.Free;
end.

