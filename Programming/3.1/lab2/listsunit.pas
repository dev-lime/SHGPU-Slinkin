unit ListsUnit;

interface

uses
  SysUtils;

type
  // Базовый класс узла
  TBaseNode = class(TObject)
  private
    FValue: Variant;
  public
    constructor Create(AValue: Variant);
    function GetValue: Variant;
    procedure SetValue(AValue: Variant);
    property Value: Variant read GetValue write SetValue;
  end;

  // Узел для целых чисел
  TIntNode = class(TBaseNode)
  public
    constructor Create(AValue: Integer);
  end;

  // Узел для вещественных чисел
  TRealNode = class(TBaseNode)
  public
    constructor Create(AValue: Real);
  end;

  // Узел для строк
  TStringNode = class(TBaseNode)
  public
    constructor Create(AValue: string);
  end;

  // Интерфейс списка
  IList = interface
    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode, newNode: TObject): TObject;
    function compare(node1, node2: TObject): Integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
  end;

  // Классический связный список
  TClassicList = class(TInterfacedObject, IList)
  private
    FHead: TObject;
    FTail: TObject;
    FCurrent: TObject;
    FSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode, newNode: TObject): TObject;
    function compare(node1, node2: TObject): Integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
    function getSize: Integer;
  end;

  // Узел для классического списка
  TClassicNode = class(TObject)
  private
    FData: TObject;
    FNext: TClassicNode;
  public
    constructor Create(AData: TObject);
    destructor Destroy; override;
    property Data: TObject read FData write FData;
    property Next: TClassicNode read FNext write FNext;
  end;

  // Список на основе массива
  TArrayList = class(TInterfacedObject, IList)
  private
    FArray: array of TObject;
    FCurrentIndex: Integer;
    FSize: Integer;
    FCapacity: Integer;
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;

    function addFirst(node: TObject): TObject;
    function addLast(node: TObject): TObject;
    function deleteFirst: TObject;
    function deleteAfter(prevNode: TObject): TObject;
    function insertAfter(prevNode, newNode: TObject): TObject;
    function compare(node1, node2: TObject): Integer;
    function first: TObject;
    function next: TObject;
    function last: TObject;
    procedure showNode(node: TObject);
    function copyNode(node: TObject): TObject;
    procedure destroyList;
    function isEmpty: Boolean;
    function getSize: Integer;
    function findNodeIndex(node: TObject): Integer;
  end;

// Процедуры работы со списками
procedure ListMerge(LResult: IList; L: IList);
procedure ListChessMerge(LResult: IList; L: IList);
procedure ListSort(LResult: IList; L: IList; ASC: Boolean);

implementation

{ TBaseNode }

constructor TBaseNode.Create(AValue: Variant);
begin
  inherited Create;
  FValue := AValue;
end;

function TBaseNode.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TBaseNode.SetValue(AValue: Variant);
begin
  FValue := AValue;
end;

{ TIntNode }

constructor TIntNode.Create(AValue: Integer);
begin
  inherited Create(AValue);
end;

{ TRealNode }

constructor TRealNode.Create(AValue: Real);
begin
  inherited Create(AValue);
end;

{ TStringNode }

constructor TStringNode.Create(AValue: string);
begin
  inherited Create(AValue);
end;

{ TClassicNode }

constructor TClassicNode.Create(AData: TObject);
begin
  inherited Create;
  FData := AData;
  FNext := nil;
end;

destructor TClassicNode.Destroy;
begin
  if FData <> nil then
    FData.Free;
  inherited Destroy;
end;

{ TClassicList }

constructor TClassicList.Create;
begin
  inherited Create;
  FHead := nil;
  FTail := nil;
  FCurrent := nil;
  FSize := 0;
end;

destructor TClassicList.Destroy;
begin
  destroyList;
  inherited Destroy;
end;

function TClassicList.addFirst(node: TObject): TObject;
var
  newNode: TClassicNode;
begin
  newNode := TClassicNode.Create(node);
  if FHead = nil then
  begin
    FHead := newNode;
    FTail := newNode;
  end
  else
  begin
    TClassicNode(newNode).Next := TClassicNode(FHead);
    FHead := newNode;
  end;
  Inc(FSize);
  Result := newNode;
end;

function TClassicList.addLast(node: TObject): TObject;
var
  newNode: TClassicNode;
begin
  if FHead = nil then
    Result := addFirst(node)
  else
  begin
    newNode := TClassicNode.Create(node);
    TClassicNode(FTail).Next := newNode;
    FTail := newNode;
    Inc(FSize);
    Result := newNode;
  end;
end;

function TClassicList.deleteFirst: TObject;
var
  temp: TClassicNode;
  data: TObject;
begin
  if FHead = nil then
    Result := nil
  else
  begin
    temp := TClassicNode(FHead);
    data := temp.Data;
    FHead := temp.Next;
    if FHead = nil then
      FTail := nil;
    temp.Free;
    Dec(FSize);
    Result := data;
  end;
end;

function TClassicList.deleteAfter(prevNode: TObject): TObject;
var
  prev, temp: TClassicNode;
  data: TObject;
begin
  if (prevNode = nil) or (TClassicNode(prevNode).Next = nil) then
    Result := nil
  else
  begin
    prev := TClassicNode(prevNode);
    temp := prev.Next;
    data := temp.Data;
    prev.Next := temp.Next;

    if temp = FTail then
      FTail := prev;

    temp.Free;
    Dec(FSize);
    Result := data;
  end;
end;

function TClassicList.insertAfter(prevNode, newNode: TObject): TObject;
var
  prev, node: TClassicNode;
begin
  if prevNode = nil then
    Result := addFirst(newNode)
  else
  begin
    prev := TClassicNode(prevNode);
    node := TClassicNode.Create(newNode);
    node.Next := prev.Next;
    prev.Next := node;

    if prev = FTail then
      FTail := node;

    Inc(FSize);
    Result := node;
  end;
end;

function TClassicList.compare(node1, node2: TObject): Integer;
var
  val1, val2: Variant;
begin
  if (node1 is TBaseNode) and (node2 is TBaseNode) then
  begin
    val1 := TBaseNode(node1).Value;
    val2 := TBaseNode(node2).Value;

    if val1 < val2 then
      Result := -1
    else if val1 > val2 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function TClassicList.first: TObject;
begin
  FCurrent := FHead;
  if FCurrent <> nil then
    Result := TClassicNode(FCurrent).Data
  else
    Result := nil;
end;

function TClassicList.next: TObject;
begin
  if (FCurrent = nil) or (TClassicNode(FCurrent).Next = nil) then
  begin
    FCurrent := nil;
    Result := nil;
  end
  else
  begin
    FCurrent := TClassicNode(FCurrent).Next;
    Result := TClassicNode(FCurrent).Data;
  end;
end;

function TClassicList.last: TObject;
begin
  FCurrent := FTail;
  if FCurrent <> nil then
    Result := TClassicNode(FCurrent).Data
  else
    Result := nil;
end;

procedure TClassicList.showNode(node: TObject);
begin
  if (node <> nil) and (node is TBaseNode) then
    Write(TBaseNode(node).Value)
  else
    Write('nil');
end;

function TClassicList.copyNode(node: TObject): TObject;
begin
  if (node <> nil) and (node is TBaseNode) then
  begin
    if node is TIntNode then
      Result := TIntNode.Create(TBaseNode(node).Value)
    else if node is TRealNode then
      Result := TRealNode.Create(TBaseNode(node).Value)
    else if node is TStringNode then
      Result := TStringNode.Create(TBaseNode(node).Value)
    else
      Result := TBaseNode.Create(TBaseNode(node).Value);
  end
  else
    Result := nil;
end;

procedure TClassicList.destroyList;
var
  temp: TClassicNode;
begin
  while FHead <> nil do
  begin
    temp := TClassicNode(FHead);
    FHead := temp.Next;
    temp.Free;
  end;
  FTail := nil;
  FCurrent := nil;
  FSize := 0;
end;

function TClassicList.isEmpty: Boolean;
begin
  Result := FHead = nil;
end;

function TClassicList.getSize: Integer;
begin
  Result := FSize;
end;

{ TArrayList }

constructor TArrayList.Create;
begin
  inherited Create;
  FCapacity := 10;
  SetLength(FArray, FCapacity);
  FSize := 0;
  FCurrentIndex := -1;
end;

destructor TArrayList.Destroy;
begin
  destroyList;
  inherited Destroy;
end;

procedure TArrayList.Grow;
begin
  FCapacity := FCapacity * 2;
  SetLength(FArray, FCapacity);
end;

function TArrayList.addFirst(node: TObject): TObject;
var
  i: Integer;
begin
  if FSize = FCapacity then
    Grow;

  for i := FSize downto 1 do
    FArray[i] := FArray[i - 1];

  FArray[0] := node;
  Inc(FSize);
  Result := node;
end;

function TArrayList.addLast(node: TObject): TObject;
begin
  if FSize = FCapacity then
    Grow;

  FArray[FSize] := node;
  Inc(FSize);
  Result := node;
end;

function TArrayList.deleteFirst: TObject;
var
  i: Integer;
  data: TObject;
begin
  if FSize = 0 then
    Result := nil
  else
  begin
    data := FArray[0];
    for i := 0 to FSize - 2 do
      FArray[i] := FArray[i + 1];
    Dec(FSize);
    Result := data;
  end;
end;

function TArrayList.deleteAfter(prevNode: TObject): TObject;
var
  index, i: Integer;
  data: TObject;
begin
  index := findNodeIndex(prevNode);
  if (index = -1) or (index >= FSize - 1) then
    Result := nil
  else
  begin
    data := FArray[index + 1];
    for i := index + 1 to FSize - 2 do
      FArray[i] := FArray[i + 1];
    Dec(FSize);
    Result := data;
  end;
end;

function TArrayList.insertAfter(prevNode, newNode: TObject): TObject;
var
  index, i: Integer;
begin
  index := findNodeIndex(prevNode);
  if index = -1 then
    Result := addFirst(newNode)
  else if index = FSize - 1 then
    Result := addLast(newNode)
  else
  begin
    if FSize = FCapacity then
      Grow;

    for i := FSize downto index + 2 do
      FArray[i] := FArray[i - 1];

    FArray[index + 1] := newNode;
    Inc(FSize);
    Result := newNode;
  end;
end;

function TArrayList.compare(node1, node2: TObject): Integer;
var
  val1, val2: Variant;
begin
  if (node1 is TBaseNode) and (node2 is TBaseNode) then
  begin
    val1 := TBaseNode(node1).Value;
    val2 := TBaseNode(node2).Value;

    if val1 < val2 then
      Result := -1
    else if val1 > val2 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function TArrayList.first: TObject;
begin
  if FSize > 0 then
  begin
    FCurrentIndex := 0;
    Result := FArray[0];
  end
  else
  begin
    FCurrentIndex := -1;
    Result := nil;
  end;
end;

function TArrayList.next: TObject;
begin
  if (FCurrentIndex = -1) or (FCurrentIndex >= FSize - 1) then
  begin
    FCurrentIndex := -1;
    Result := nil;
  end
  else
  begin
    Inc(FCurrentIndex);
    Result := FArray[FCurrentIndex];
  end;
end;

function TArrayList.last: TObject;
begin
  if FSize > 0 then
  begin
    FCurrentIndex := FSize - 1;
    Result := FArray[FSize - 1];
  end
  else
  begin
    FCurrentIndex := -1;
    Result := nil;
  end;
end;

procedure TArrayList.showNode(node: TObject);
begin
  if (node <> nil) and (node is TBaseNode) then
    Write(TBaseNode(node).Value)
  else
    Write('nil');
end;

function TArrayList.copyNode(node: TObject): TObject;
begin
  if (node <> nil) and (node is TBaseNode) then
  begin
    if node is TIntNode then
      Result := TIntNode.Create(TBaseNode(node).Value)
    else if node is TRealNode then
      Result := TRealNode.Create(TBaseNode(node).Value)
    else if node is TStringNode then
      Result := TStringNode.Create(TBaseNode(node).Value)
    else
      Result := TBaseNode.Create(TBaseNode(node).Value);
  end
  else
    Result := nil;
end;

procedure TArrayList.destroyList;
var
  i: Integer;
begin
  for i := 0 to FSize - 1 do
    if FArray[i] <> nil then
      FArray[i].Free;
  FSize := 0;
  FCurrentIndex := -1;
end;

function TArrayList.isEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TArrayList.getSize: Integer;
begin
  Result := FSize;
end;

function TArrayList.findNodeIndex(node: TObject): Integer;
var
  i: Integer;
begin
  for i := 0 to FSize - 1 do
    if FArray[i] = node then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

{ Процедуры работы со списками }

procedure ListMerge(LResult: IList; L: IList);
var
  node: TObject;
begin
  node := L.first;
  while node <> nil do
  begin
    LResult.addLast(LResult.copyNode(node));
    node := L.next;
  end;
end;

procedure ListChessMerge(LResult: IList; L: IList);
var
  nodeL, nodeResult: TObject;
  insertAfterCurrent: Boolean;
begin
  nodeL := L.first;
  nodeResult := LResult.first;
  insertAfterCurrent := False;

  while nodeL <> nil do
  begin
    if nodeResult = nil then
    begin
      LResult.addLast(LResult.copyNode(nodeL));
    end
    else
    begin
      if insertAfterCurrent then
      begin
        LResult.insertAfter(nodeResult, LResult.copyNode(nodeL));
        nodeResult := LResult.next;
      end
      else
      begin
        LResult.addLast(LResult.copyNode(nodeL));
      end;
      insertAfterCurrent := not insertAfterCurrent;
    end;
    nodeL := L.next;
  end;
end;

procedure ListSort(LResult: IList; L: IList; ASC: Boolean);
var
  sourceNode, destNode, tempNode: TObject;
  inserted: Boolean;
begin
  // Если LResult не пустой, переносит содержимое в L
  if not LResult.isEmpty then
  begin
    sourceNode := LResult.first;
    while sourceNode <> nil do
    begin
      L.addLast(LResult.copyNode(sourceNode));
      sourceNode := LResult.next;
    end;
    LResult.destroyList;
  end;

  // Сортировка вставками
  sourceNode := L.first;
  while sourceNode <> nil do
  begin
    tempNode := LResult.copyNode(sourceNode);

    if LResult.isEmpty then
    begin
      LResult.addFirst(tempNode);
    end
    else
    begin
      inserted := False;
      destNode := LResult.first;

      while (destNode <> nil) and not inserted do
      begin
        if ASC then
        begin
          if LResult.compare(tempNode, destNode) <= 0 then
          begin
            if destNode = LResult.first then
              LResult.addFirst(tempNode)
            else
              LResult.insertAfter(LResult.first, tempNode); // Нужен предыдущий узел
            inserted := True;
          end;
        end
        else
        begin
          if LResult.compare(tempNode, destNode) >= 0 then
          begin
            if destNode = LResult.first then
              LResult.addFirst(tempNode)
            else
              LResult.insertAfter(LResult.first, tempNode); // Нужен предыдущий узел
            inserted := True;
          end;
        end;

        if not inserted then
          destNode := LResult.next;
      end;

      if not inserted then
        LResult.addLast(tempNode);
    end;

    sourceNode := L.next;
  end;

  L.destroyList;
end;

end.
