program Project1;

uses
  canvas23030428;

var
  canvas: T23030428Canvas;
  color: TRGB;

  canvas1, canvas2: T23030428Canvas;
begin
  canvas := T23030428Canvas.Create;
  ////6 - ширина, высота
  canvas.add(100, 100);
  //canvas.SetPencilColor(0, 0, 255);
  //canvas.KoordPencil(14, 15);
  //canvas.rectangle6(70, 50);
  //canvas.SaveToPPM('test6.ppm');
  //
  ////7
  //canvas.add(100, 100);
  //canvas.SetPencilColor(0, 255, 0);
  //canvas.KoordPencil(14, 15);
  //canvas.rectangle7(20, 40);
  //canvas.SaveToPPM('test7.ppm');
  //
  ////8
  //canvas.add(100, 100);
  //canvas.SetPencilColor(0, 0, 0);
  //canvas.KoordPencil(13, 0);
  //canvas.segment(13, 10);
  //canvas.SaveToPPM('test8.ppm');
  //
  ////9
  //canvas.add(100, 100);
  //canvas.SetPencilColor(0, 255, 0);
  //canvas.KoordPencil(10, 10);
  //canvas.rectangle7(20, 20);
  //canvas.SetPencilColor(255, 0, 0);
  //canvas.KoordPencil(14, 15);
  //canvas.lim_area;
  //canvas.SaveToPPM('test9_1.ppm');
  //
  //canvas.add(100, 100);
  //canvas.SetPencilColor(0, 255, 0);
  //canvas.KoordPencil(10, 10);
  //canvas.rectangle6(20, 20);
  //canvas.SetPencilColor(255, 0, 0);
  //canvas.KoordPencil(14, 15);
  //canvas.lim_area;
  //canvas.SaveToPPM('test9_2.ppm');
  //
  ////10
  //canvas1 := T23030428Canvas.Create;
  //canvas1.add(100, 100);
  //canvas1.SetPencilColor(200, 200, 200);
  //canvas1.KoordPencil(0, 0);
  //canvas1.rectangle7(100, 100);
  //
  //canvas2 := T23030428Canvas.Create;
  //canvas2.add(10, 10);
  //canvas2.SetPencilColor(255, 255, 255);
  //canvas2.KoordPencil(0, 0);
  //canvas2.rectangle7(10, 10);
  //
  //canvas2.SetPencilColor(0, 0, 0);
  //canvas2.KoordPencil(0, 0);
  //canvas2.segment(0, 9);
  //
  //canvas1.SetPencilColor(255, 255, 255); //прозрач
  //canvas1.KoordPencil(20, 20);
  //canvas1.OverlayCanvas(canvas2);
  //canvas1.SaveToPPM('10t.ppm');
  //
  ////дом
  //canvas.add(1000, 1000);
  //canvas.SetPencilColor(0,0,0);
  //canvas.KoordPencil(300,500);
  //canvas.segment(300, 200);
  //canvas.SetPencilColor(200, 200, 0);
  //canvas.segment(450, 50);
  //canvas.SetPencilColor(0, 255, 0);
  //canvas.segment(590, 200);
  //canvas.segment(590, 500);
  //canvas.SetPencilColor(0, 0, 255);
  //canvas.segment(300, 200);
  //canvas.segment(590, 200);
  //canvas.SetPencilColor(200, 0, 200);
  //canvas.segment(300, 500);
  //canvas.SetPencilColor(255, 0, 0);
  //canvas.segment(590, 500);
  //
  //canvas.SetPencilColor(0, 200, 200);
  //canvas.KoordPencil(250, 500);
  //canvas.rectangle6(400, 100);
  //canvas.SetPencilColor(0, 255, 0);
  //canvas.KoordPencil(260, 510);
  //canvas.rectangle7(380, 80);
  //
  //canvas.KoordPencil(420, 240);
  //canvas.SetPencilColor(255, 0, 0);
  //canvas.lim_area;
  //canvas.SaveToPPM('home.ppm');

   canvas.SetPencilColor(0,0,0);
   canvas.KoordPencil(1,12); //чтоюы была тройка
   canvas.segment(12,1);
end.
