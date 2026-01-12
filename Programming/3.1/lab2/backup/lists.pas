unit Lists;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Variants;

type
  IList = interface
    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode: TObject; insnode: TObject): TObject;
    function compare(node1, node2: TObject): integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
  end;

  // Базовый класс узла
  TBaseNode = class(TObject)
  protected
    function GetValue: Variant; virtual; abstract;
  public
    function CompareTo(Other: TObject): Integer; virtual; abstract;
    function Clone: TBaseNode; virtual; abstract;
    procedure Show; virtual; abstract;
    function GetTypeName: string; virtual; abstract;
    property Value: Variant read GetValue;
  end;

  // Узел с целочисленным значением
  TIntegerNode = class(TBaseNode)
  private
    FValue: Integer;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(AValue: Integer);
    function CompareTo(Other: TObject): Integer; override;
    function Clone: TBaseNode; override;
    procedure Show; override;
    function GetTypeName: string; override;
    property ValueInt: Integer read FValue;
  end;

  // Узел со строковым значением
  TStringNode = class(TBaseNode)
  private
    FValue: string;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(AValue: string);
    function CompareTo(Other: TObject): Integer; override;
    function Clone: TBaseNode; override;
    procedure Show; override;
    function GetTypeName: string; override;
    property ValueStr: string read FValue;
  end;

  // Узел с вещественным значением
  TFloatNode = class(TBaseNode)
  private
    FValue: Double;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(AValue: Double);
    function CompareTo(Other: TObject): Integer; override;
    function Clone: TBaseNode; override;
    procedure Show; override;
    function GetTypeName: string; override;
    property ValueFloat: Double read FValue;
  end;

  // Классический связный список
  TClassicList = class(TInterfacedObject, IList)
  private
    FNodeType: string; // Тип узлов, хранящихся в списке
    function GetNodeType(node: TObject): string;
    procedure CheckNodeType(node: TObject);
  protected
    type
      PNode = ^TNode;
      TNode = record
        Data: TObject;
        Next: PNode;
      end;
    var
      FHeadPtr: PNode;
      FTailPtr: PNode;
      FCurrentPtr: PNode;
  public
    constructor Create;
    destructor Destroy; override;
    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode: TObject; insnode: TObject): TObject;
    function compare(node1, node2: TObject): integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
    property NodeType: string read FNodeType;
  end;

  // Список на основе динамического массива
  TArrayList = class(TInterfacedObject, IList)
  private
    FNodeType: string;
    FItems: array of TObject;
    FCurrentIndex: Integer;
    FCount: Integer;
    FCapacity: Integer;
    function GetNodeType(node: TObject): string;
    procedure CheckNodeType(node: TObject);
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode: TObject; insnode: TObject): TObject;
    function compare(node1, node2: TObject): integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
    property NodeType: string read FNodeType;
  end;

// Процедуры работы со списками
procedure ListMerge(LResult: IList; L: IList);
procedure ListChessMerge(LResult: IList; L: IList);
procedure ListSort(LResult: IList; L: IList; ASC: boolean);

implementation

{ TIntegerNode }

constructor TIntegerNode.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

function TIntegerNode.CompareTo(Other: TObject): Integer;
begin
  if not (Other is TIntegerNode) then
    raise Exception.Create('Type mismatch in comparison: expected TIntegerNode');

  if FValue < TIntegerNode(Other).FValue then
    Result := -1
  else if FValue > TIntegerNode(Other).FValue then
    Result := 1
  else
    Result := 0;
end;

function TIntegerNode.Clone: TBaseNode;
begin
  Result := TIntegerNode.Create(FValue);
end;

procedure TIntegerNode.Show;
begin
  Write(FValue:8);
end;

function TIntegerNode.GetValue: Variant;
begin
  Result := FValue;
end;

function TIntegerNode.GetTypeName: string;
begin
  Result := 'Integer';
end;

{ TStringNode }

constructor TStringNode.Create(AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TStringNode.CompareTo(Other: TObject): Integer;
begin
  if not (Other is TStringNode) then
    raise Exception.Create('Type mismatch in comparison: expected TStringNode');

  if FValue < TStringNode(Other).FValue then
    Result := -1
  else if FValue > TStringNode(Other).FValue then
    Result := 1
  else
    Result := 0;
end;

function TStringNode.Clone: TBaseNode;
begin
  Result := TStringNode.Create(FValue);
end;

procedure TStringNode.Show;
begin
  Write(FValue:20);
end;

function TStringNode.GetValue: Variant;
begin
  Result := FValue;
end;

function TStringNode.GetTypeName: string;
begin
  Result := 'String';
end;

{ TFloatNode }

constructor TFloatNode.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
end;

function TFloatNode.CompareTo(Other: TObject): Integer;
begin
  if not (Other is TFloatNode) then
    raise Exception.Create('Type mismatch in comparison: expected TFloatNode');

  if FValue < TFloatNode(Other).FValue then
    Result := -1
  else if FValue > TFloatNode(Other).FValue then
    Result := 1
  else
    Result := 0;
end;

function TFloatNode.Clone: TBaseNode;
begin
  Result := TFloatNode.Create(FValue);
end;

procedure TFloatNode.Show;
begin
  Write(FValue:12:4);
end;

function TFloatNode.GetValue: Variant;
begin
  Result := FValue;
end;

function TFloatNode.GetTypeName: string;
begin
  Result := 'Float';
end;

{ TClassicList }

constructor TClassicList.Create;
begin
  inherited;
  FHeadPtr := nil;
  FTailPtr := nil;
  FCurrentPtr := nil;
  FNodeType := '';
end;

destructor TClassicList.Destroy;
begin
  destroyList;
  inherited;
end;

function TClassicList.GetNodeType(node: TObject): string;
begin
  if node is TBaseNode then
    Result := TBaseNode(node).GetTypeName
  else
    Result := '';
end;

procedure TClassicList.CheckNodeType(node: TObject);
var
  NodeTypeStr: string;
begin
  NodeTypeStr := GetNodeType(node);
  if NodeTypeStr = '' then
    raise Exception.Create('Invalid node type');

  if FNodeType = '' then
    FNodeType := NodeTypeStr
  else if FNodeType <> NodeTypeStr then
    raise Exception.CreateFmt('Cannot mix node types. Expected %s, got %s',
      [FNodeType, NodeTypeStr]);
end;

function TClassicList.addFirst(node: TObject): TObject;
begin
  CheckNodeType(node);

  // Используем insertAfter с nil в качестве предыдущего узла
  Result := insertAfter(nil, node);
end;

function TClassicList.addLast(node: TObject): TObject;
begin
  CheckNodeType(node);

  // Используем insertAfter с последним узлом в качестве предыдущего
  Result := insertAfter(last, node);
end;

function TClassicList.deleteFirst: TObject;
begin
  if FHeadPtr = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := FHeadPtr^.Data;

  // Используем deleteAfter с nil в качестве предыдущего узла
  // deleteAfter(nil) будет удалять первый элемент
  deleteAfter(nil);
end;

function TClassicList.deleteAfter(prevNode: TObject): TObject;
var
  PrevPtr, CurrPtr: PNode;
begin
  if FHeadPtr = nil then
  begin
    Result := nil;
    Exit;
  end;

  if prevNode = nil then
  begin
    // Удаляем первый элемент
    Result := FHeadPtr^.Data;
    CurrPtr := FHeadPtr;
    FHeadPtr := FHeadPtr^.Next;

    if FHeadPtr = nil then
      FTailPtr := nil;

    if FCurrentPtr = CurrPtr then
      FCurrentPtr := FHeadPtr;

    Dispose(CurrPtr);
    Exit;
  end;

  // Находим узел prevNode
  PrevPtr := FHeadPtr;
  while (PrevPtr <> nil) and (PrevPtr^.Data <> prevNode) do
    PrevPtr := PrevPtr^.Next;

  if (PrevPtr = nil) or (PrevPtr^.Next = nil) then
  begin
    Result := nil;
    Exit;
  end;

  CurrPtr := PrevPtr^.Next;
  Result := CurrPtr^.Data;
  PrevPtr^.Next := CurrPtr^.Next;

  if FTailPtr = CurrPtr then
    FTailPtr := PrevPtr;

  if FCurrentPtr = CurrPtr then
    FCurrentPtr := CurrPtr^.Next;

  Dispose(CurrPtr);
end;

function TClassicList.insertAfter(prevNode: TObject; insnode: TObject): TObject;
var
  NewNode, PrevPtr: PNode;
begin
  CheckNodeType(insnode);

  if FHeadPtr = nil then
  begin
    // Создаем первый узел
    New(NewNode);
    NewNode^.Data := insnode;
    NewNode^.Next := nil;
    FHeadPtr := NewNode;
    FTailPtr := NewNode;
    Result := insnode;
    Exit;
  end;

  if prevNode = nil then
  begin
    // Вставка в начало
    New(NewNode);
    NewNode^.Data := insnode;
    NewNode^.Next := FHeadPtr;
    FHeadPtr := NewNode;
    Result := insnode;
    Exit;
  end;

  // Находим узел prevNode
  PrevPtr := FHeadPtr;
  while (PrevPtr <> nil) and (PrevPtr^.Data <> prevNode) do
    PrevPtr := PrevPtr^.Next;

  if PrevPtr = nil then
  begin
    // Узел не найден, вставляем в конец
    Result := addLast(insnode);
    Exit;
  end;

  // Вставляем после найденного узла
  New(NewNode);
  NewNode^.Data := insnode;
  NewNode^.Next := PrevPtr^.Next;
  PrevPtr^.Next := NewNode;

  if FTailPtr = PrevPtr then
    FTailPtr := NewNode;

  Result := insnode;
end;

function TClassicList.compare(node1, node2: TObject): integer;
begin
  if not (node1 is TBaseNode) or not (node2 is TBaseNode) then
  begin
    Result := 0;
    Exit;
  end;

  Result := TBaseNode(node1).CompareTo(node2);
end;

function TClassicList.first: TObject;
begin
  FCurrentPtr := FHeadPtr;
  if FCurrentPtr <> nil then
    Result := FCurrentPtr^.Data
  else
    Result := nil;
end;

function TClassicList.next: TObject;
begin
  if FCurrentPtr = nil then
    Result := nil
  else
  begin
    FCurrentPtr := FCurrentPtr^.Next;
    if FCurrentPtr <> nil then
      Result := FCurrentPtr^.Data
    else
      Result := nil;
  end;
end;

function TClassicList.last: TObject;
begin
  FCurrentPtr := FTailPtr;
  if FCurrentPtr <> nil then
    Result := FCurrentPtr^.Data
  else
    Result := nil;
end;

procedure TClassicList.showNode(node: TObject);
begin
  if node is TBaseNode then
    TBaseNode(node).Show
  else
    Write('Not a node');
end;

function TClassicList.copyNode(node: TObject): TObject;
begin
  if node is TBaseNode then
    Result := TBaseNode(node).Clone
  else
    Result := nil;
end;

procedure TClassicList.destroyList;
var
  Current, Temp: PNode;
begin
  Current := FHeadPtr;
  while Current <> nil do
  begin
    Temp := Current;
    Current := Current^.Next;
    if Temp^.Data <> nil then
      Temp^.Data.Free;
    Dispose(Temp);
  end;
  FHeadPtr := nil;
  FTailPtr := nil;
  FCurrentPtr := nil;
  FNodeType := '';
end;

function TClassicList.isEmpty: Boolean;
begin
  Result := FHeadPtr = nil;
end;

{ TArrayList }

constructor TArrayList.Create;
begin
  inherited;
  FCapacity := 10;
  SetLength(FItems, FCapacity);
  FCount := 0;
  FCurrentIndex := -1;
  FNodeType := '';
end;

destructor TArrayList.Destroy;
begin
  destroyList;
  inherited;
end;

function TArrayList.GetNodeType(node: TObject): string;
begin
  if node is TBaseNode then
    Result := TBaseNode(node).GetTypeName
  else
    Result := '';
end;

procedure TArrayList.CheckNodeType(node: TObject);
var
  NodeTypeStr: string;
begin
  NodeTypeStr := GetNodeType(node);
  if NodeTypeStr = '' then
    raise Exception.Create('Invalid node type');

  if FNodeType = '' then
    FNodeType := NodeTypeStr
  else if FNodeType <> NodeTypeStr then
    raise Exception.CreateFmt('Cannot mix node types. Expected %s, got %s',
      [FNodeType, NodeTypeStr]);
end;

procedure TArrayList.Grow;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems, FCapacity);
end;

function TArrayList.addFirst(node: TObject): TObject;
begin
  CheckNodeType(node);
  // Реализуем через insertAfter с nil
  Result := insertAfter(nil, node);
end;

function TArrayList.addLast(node: TObject): TObject;
begin
  CheckNodeType(node);
  // Реализуем через insertAfter с последним узлом
  Result := insertAfter(last, node);
end;

function TArrayList.deleteFirst: TObject;
begin
  if FCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  Result := FItems[0];
  // Используем deleteAfter с nil
  deleteAfter(nil);
end;

function TArrayList.deleteAfter(prevNode: TObject): TObject;
var
  i, idx: Integer;
begin
  if FCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  if prevNode = nil then
  begin
    // Удаляем первый элемент
    Result := FItems[0];
    if FItems[0] <> nil then
      FItems[0].Free;

    for i := 0 to FCount - 2 do
      FItems[i] := FItems[i + 1];

    Dec(FCount);
    FItems[FCount] := nil;

    if FCurrentIndex >= FCount then
      FCurrentIndex := FCount - 1;

    Exit;
  end;

  // Находим индекс prevNode
  idx := -1;
  for i := 0 to FCount - 1 do
    if FItems[i] = prevNode then
    begin
      idx := i;
      Break;
    end;

  if (idx = -1) or (idx >= FCount - 1) then
  begin
    Result := nil;
    Exit;
  end;

  Result := FItems[idx + 1];
  if FItems[idx + 1] <> nil then
    FItems[idx + 1].Free;

  for i := idx + 1 to FCount - 2 do
    FItems[i] := FItems[i + 1];

  Dec(FCount);
  FItems[FCount] := nil;

  if FCurrentIndex >= FCount then
    FCurrentIndex := FCount - 1;
end;

function TArrayList.insertAfter(prevNode: TObject; insnode: TObject): TObject;
var
  i, idx: Integer;
begin
  CheckNodeType(insnode);

  if FCount = 0 then
  begin
    // Первый элемент
    if FCount = FCapacity then
      Grow;

    FItems[0] := insnode;
    Inc(FCount);
    Result := insnode;
    Exit;
  end;

  if prevNode = nil then
  begin
    // Вставка в начало
    if FCount = FCapacity then
      Grow;

    for i := FCount downto 1 do
      FItems[i] := FItems[i - 1];

    FItems[0] := insnode;
    Inc(FCount);
    Result := insnode;
    Exit;
  end;

  // Находим индекс prevNode
  idx := -1;
  for i := 0 to FCount - 1 do
    if FItems[i] = prevNode then
    begin
      idx := i;
      Break;
    end;

  if idx = -1 then
  begin
    // Узел не найден, вставляем в конец
    if FCount = FCapacity then
      Grow;

    FItems[FCount] := insnode;
    Inc(FCount);
    Result := insnode;
    Exit;
  end;

  // Вставляем после найденного узла
  if FCount = FCapacity then
    Grow;

  for i := FCount downto idx + 2 do
    FItems[i] := FItems[i - 1];

  FItems[idx + 1] := insnode;
  Inc(FCount);
  Result := insnode;
end;

function TArrayList.compare(node1, node2: TObject): integer;
begin
  if not (node1 is TBaseNode) or not (node2 is TBaseNode) then
  begin
    Result := 0;
    Exit;
  end;

  Result := TBaseNode(node1).CompareTo(node2);
end;

function TArrayList.first: TObject;
begin
  if FCount > 0 then
  begin
    FCurrentIndex := 0;
    Result := FItems[0];
  end
  else
  begin
    FCurrentIndex := -1;
    Result := nil;
  end;
end;

function TArrayList.next: TObject;
begin
  if (FCurrentIndex < 0) or (FCurrentIndex >= FCount - 1) then
  begin
    FCurrentIndex := -1;
    Result := nil;
  end
  else
  begin
    Inc(FCurrentIndex);
    Result := FItems[FCurrentIndex];
  end;
end;

function TArrayList.last: TObject;
begin
  if FCount > 0 then
  begin
    FCurrentIndex := FCount - 1;
    Result := FItems[FCount - 1];
  end
  else
  begin
    FCurrentIndex := -1;
    Result := nil;
  end;
end;

procedure TArrayList.showNode(node: TObject);
begin
  if node is TBaseNode then
    TBaseNode(node).Show
  else
    Write('Not a node');
end;

function TArrayList.copyNode(node: TObject): TObject;
begin
  if node is TBaseNode then
    Result := TBaseNode(node).Clone
  else
    Result := nil;
end;

procedure TArrayList.destroyList;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if FItems[i] <> nil then
      FItems[i].Free;

  FCount := 0;
  FCurrentIndex := -1;
  FNodeType := '';
end;

function TArrayList.isEmpty: Boolean;
begin
  Result := FCount = 0;
end;

{ Процедуры работы со списками }

procedure ListMerge(LResult: IList; L: IList);
var
  Node: TObject;
begin
  Node := L.first;
  while Node <> nil do
  begin
    LResult.addLast(LResult.copyNode(Node));
    Node := L.next;
  end;
end;

procedure ListChessMerge(LResult: IList; L: IList);
var
  NodeL, NodeResult, PrevNode: TObject;
  Skip: Boolean;
begin
  NodeL := L.first;
  PrevNode := nil;
  Skip := False; // Флаг для пропуска узла в целевом списке

  while NodeL <> nil do
  begin
    if PrevNode = nil then
    begin
      // Первый элемент или список пустой
      LResult.addLast(LResult.copyNode(NodeL));
      PrevNode := LResult.last;
    end
    else if not Skip then
    begin
      // Вставляем после предыдущего узла
      LResult.insertAfter(PrevNode, LResult.copyNode(NodeL));
      PrevNode := LResult.next; // Переходим к вставленному узлу
    end;

    // Переключаем флаг пропуска
    Skip := not Skip;

    // Если нужно пропустить, двигаемся по целевому списку
    if Skip then
    begin
      if PrevNode <> nil then
      begin
        NodeResult := LResult.next;
        if NodeResult <> nil then
          PrevNode := NodeResult;
      end;
    end;

    NodeL := L.next;
  end;
end;

procedure ListSort(LResult: IList; L: IList; ASC: boolean);
var
  SourceNode, DestNode, PrevNode, TempNode, CopiedNode: TObject;
  Inserted: Boolean;
begin
  // Если LResult не пустой, переносим его содержимое в L (с копированием)
  if not LResult.isEmpty then
  begin
    // Копируем все узлы из LResult в L
    SourceNode := LResult.first;
    while SourceNode <> nil do
    begin
      CopiedNode := L.copyNode(SourceNode);
      if CopiedNode <> nil then
        L.addLast(CopiedNode);
      SourceNode := LResult.next;
    end;

    // Очищаем LResult через destroyList
    LResult.destroyList;
  end;

  // Сортировка вставками
  SourceNode := L.first;
  while SourceNode <> nil do
  begin
    // Создаем копию узла для вставки в отсортированный список
    TempNode := LResult.copyNode(SourceNode);
    if TempNode = nil then
    begin
      SourceNode := L.next;
      Continue;
    end;

    // Находим место для вставки в LResult
    DestNode := LResult.first;
    PrevNode := nil;
    Inserted := False;

    while DestNode <> nil do
    begin
      if ASC then
      begin
        // Сортировка по возрастанию
        if LResult.compare(TempNode, DestNode) < 0 then
        begin
          if PrevNode = nil then
            LResult.addFirst(TempNode)
          else
            LResult.insertAfter(PrevNode, TempNode);
          Inserted := True;
          Break;
        end;
      end
      else
      begin
        // Сортировка по убыванию
        if LResult.compare(TempNode, DestNode) > 0 then
        begin
          if PrevNode = nil then
            LResult.addFirst(TempNode)
          else
            LResult.insertAfter(PrevNode, TempNode);
          Inserted := True;
          Break;
        end;
      end;
      PrevNode := DestNode;
      DestNode := LResult.next;
    end;

    // Если не нашли подходящее место, вставляем в конец
    if not Inserted then
    begin
      if PrevNode = nil then
        LResult.addFirst(TempNode)
      else
        LResult.insertAfter(PrevNode, TempNode);
    end;

    SourceNode := L.next;
  end;

  // Очищаем исходный список L
  L.destroyList;
end;

end.

