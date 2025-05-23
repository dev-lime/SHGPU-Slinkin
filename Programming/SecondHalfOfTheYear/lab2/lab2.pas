(*

Поиск данных может быть реализован различными, зачастую несовместимыми методами. В таких случаях, при использовании ООП, предлагается разработка базового класса-предка (суперкласса), в который, кроме набора статических методов, будут включены и основные абстрактные методы, необходимые для организации поиска. Разработанные на основе суперкласса классы-потомки (подклассы) реализуют указанные методы, каждый - своим способом. Задача заключается в разработке трех классов-поисковиков:
TAbstractFinder - базовый абстрактный суперкласс, предполагающий организацию поиска данных. Экземпляры этого класса не создаются.
TLineFinder - подкласс TAbstractFinder, реализующий функциональность линейного поиска.
TBinFinder - подкласс TAbstractFinder, реализующий функциональность бинарного поиска.

Декларируемые возможности TAbstractFinder:

1) Конструктор (с передачей набора данных в виде динамического массива) и деструктор (при необходимости).
2) Процедура замены текущего набора данных на новый (статическая)
3) Процедура установки текущего метода сравнения двух элементов (статическая)
4) Функция проверки корректности набора данных (abstract)
5) Функция без параметров для поиска одного элемента с использованием текущего установленного метода сравнения двух элементов (abstract).
6) Функция для поиска одного переданного элемента (статическая).
7) Функция для поиска одного элемента с передачей функции сравнения (статическая).
8) Функция без параметров для поиска всех элементов с использованием текущего установленного метода сравнения двух элементов (abstract).
9) Функция для поиска всех элементов, равных переданному элемента (статическая).
10) Функция для поиска всех элементов, с передачей функции сравнения (статическая).

Классы TLineFinder и TBinFinder должны реализовать все абстрактные методы, заявленные в суперклассе, перекрыть при необходимости конструктор/деструктор, создать требуемые дополнительные поля и методы в приватной области.

Шаблон программы проверки:

var F:TAbstractFinder;
begin
    F:=TLineFinder.Create([1,4,2,3,2,1]);
    // F:=TBinFinder.Create([1,4,12,13,21,101]);
// Далее - проверка работоспособности поисковика. 
// Смена типа поисковика в предыдущих строках 
// не должна влиять на результат работы программы.
    F.free;
end.

*)
