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
    property ValueFloat: Double read FValue;
  end;

  // Классический связный список
  TClassicList = class(TInterfacedObject, IList)
  private
    function FindNodePtr(node: TObject): Pointer;
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
  end;

  // Список на основе динамического массива
  TArrayList = class(TInterfacedObject, IList)
  private
    FItems: array of TObject;
    FCurrentIndex: Integer;
    FCount: Integer;
    FCapacity: Integer;
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
var
  OtherNode: TBaseNode;
begin
  if not (Other is TBaseNode) then
    raise Exception.Create('Type mismatch in comparison');

  OtherNode := TBaseNode(Other);

  // Сравниваем по значению Variant
  if FValue < OtherNode.Value then
    Result := -1
  else if FValue > OtherNode.Value then
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

{ TStringNode }

constructor TStringNode.Create(AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TStringNode.CompareTo(Other: TObject): Integer;
var
  OtherNode: TBaseNode;
begin
  if not (Other is TBaseNode) then
    raise Exception.Create('Type mismatch in comparison');

  OtherNode := TBaseNode(Other);

  // Для строк использует сравнение как Variant
  if FValue < OtherNode.Value then
    Result := -1
  else if FValue > OtherNode.Value then
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

{ TFloatNode }

constructor TFloatNode.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
end;

function TFloatNode.CompareTo(Other: TObject): Integer;
var
  OtherNode: TBaseNode;
begin
  if not (Other is TBaseNode) then
    raise Exception.Create('Type mismatch in comparison');

  OtherNode := TBaseNode(Other);

  if FValue < OtherNode.Value then
    Result := -1
  else if FValue > OtherNode.Value then
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

{ TClassicList }

constructor TClassicList.Create;
begin
  inherited;
  FHeadPtr := nil;
  FTailPtr := nil;
  FCurrentPtr := nil;
end;

destructor TClassicList.Destroy;
begin
  destroyList;
  inherited;
end;

function TClassicList.addFirst(node: TObject): TObject;
var
  NewNode: PNode;
begin
  New(NewNode);
  NewNode^.Data := node;
  NewNode^.Next := FHeadPtr;
  FHeadPtr := NewNode;

  if FTailPtr = nil then
    FTailPtr := FHeadPtr;

  Result := node;
end;

function TClassicList.addLast(node: TObject): TObject;
var
  NewNode: PNode;
begin
  if FHeadPtr = nil then
  begin
    Result := addFirst(node);
    Exit;
  end;

  New(NewNode);
  NewNode^.Data := node;
  NewNode^.Next := nil;

  if FTailPtr <> nil then
    FTailPtr^.Next := NewNode;

  FTailPtr := NewNode;
  Result := node;
end;

function TClassicList.deleteFirst: TObject;
var
  Temp: PNode;
begin
  if FHeadPtr = nil then
  begin
    Result := nil;
    Exit;
  end;

  Temp := FHeadPtr;
  Result := Temp^.Data;
  FHeadPtr := FHeadPtr^.Next;

  if FHeadPtr = nil then
    FTailPtr := nil;

  if FCurrentPtr = Temp then
    FCurrentPtr := FHeadPtr;

  Dispose(Temp);
end;

function TClassicList.deleteAfter(prevNode: TObject): TObject;
var
  PrevPtr, CurrPtr, NextPtr: PNode;
begin
  if FHeadPtr = nil then
  begin
    Result := nil;
    Exit;
  end;

  PrevPtr := FindNodePtr(prevNode);
  if (PrevPtr = nil) or (PrevPtr^.Next = nil) then
  begin
    Result := nil;
    Exit;
  end;

  CurrPtr := PrevPtr^.Next;
  NextPtr := CurrPtr^.Next;
  Result := CurrPtr^.Data;

  PrevPtr^.Next := NextPtr;

  if FTailPtr = CurrPtr then
    FTailPtr := PrevPtr;

  if FCurrentPtr = CurrPtr then
    FCurrentPtr := NextPtr;

  Dispose(CurrPtr);
end;

function TClassicList.FindNodePtr(node: TObject): Pointer;
var
  Current: PNode;
begin
  Current := FHeadPtr;
  while Current <> nil do
  begin
    if Current^.Data = node then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
  Result := nil;
end;

function TClassicList.insertAfter(prevNode: TObject; insnode: TObject): TObject;
var
  PrevPtr, NewNode: PNode;
begin
  if FHeadPtr = nil then
  begin
    Result := addFirst(insnode);
    Exit;
  end;

  if prevNode = nil then
  begin
    Result := addFirst(insnode);
    Exit;
  end;

  PrevPtr := FindNodePtr(prevNode);
  if PrevPtr = nil then
  begin
    Result := addLast(insnode);
    Exit;
  end;

  New(NewNode);
  NewNode^.Data := insnode;
  NewNode^.Next := PrevPtr^.Next;
  PrevPtr^.Next := NewNode;

  if FTailPtr = PrevPtr then
    FTailPtr := NewNode;

  Result := insnode;
end;

function TClassicList.compare(node1, node2: TObject): integer;
var
  BaseNode1, BaseNode2: TBaseNode;
begin
  if not (node1 is TBaseNode) or not (node2 is TBaseNode) then
  begin
    Result := 0;
    Exit;
  end;

  BaseNode1 := TBaseNode(node1);
  BaseNode2 := TBaseNode(node2);

  Result := BaseNode1.CompareTo(BaseNode2);
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
  if (FCurrentPtr = nil) or (FCurrentPtr^.Next = nil) then
  begin
    FCurrentPtr := nil;
    Result := nil;
  end
  else
  begin
    FCurrentPtr := FCurrentPtr^.Next;
    Result := FCurrentPtr^.Data;
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

    // Удаляет узел, но не данные
    Dispose(Temp);
  end;
  FHeadPtr := nil;
  FTailPtr := nil;
  FCurrentPtr := nil;
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
end;

destructor TArrayList.Destroy;
begin
  destroyList;
  inherited;
end;

procedure TArrayList.Grow;
begin
  FCapacity := FCapacity * 2;
  SetLength(FItems, FCapacity);
end;

function TArrayList.addFirst(node: TObject): TObject;
var
  i: Integer;
begin
  if FCount = FCapacity then
    Grow;

  for i := FCount downto 1 do
    FItems[i] := FItems[i - 1];

  FItems[0] := node;
  Inc(FCount);
  Result := node;
end;

function TArrayList.addLast(node: TObject): TObject;
begin
  if FCount = FCapacity then
    Grow;

  FItems[FCount] := node;
  Inc(FCount);
  Result := node;
end;

function TArrayList.deleteFirst: TObject;
var
  i: Integer;
begin
  if FCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  Result := FItems[0];
  for i := 0 to FCount - 2 do
    FItems[i] := FItems[i + 1];

  Dec(FCount);
  FItems[FCount] := nil;

  if FCurrentIndex >= FCount then
    FCurrentIndex := FCount - 1;
end;

function TArrayList.deleteAfter(prevNode: TObject): TObject;
var
  i, idx: Integer;
begin
  Result := nil;

  // Находит индекс prevNode
  idx := -1;
  for i := 0 to FCount - 1 do
    if FItems[i] = prevNode then
    begin
      idx := i;
      Break;
    end;

  if (idx = -1) or (idx >= FCount - 1) then
    Exit;

  Result := FItems[idx + 1];

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
  if FCount = 0 then
  begin
    Result := addFirst(insnode);
    Exit;
  end;

  if prevNode = nil then
  begin
    Result := addFirst(insnode);
    Exit;
  end;

  // Находит индекс prevNode
  idx := -1;
  for i := 0 to FCount - 1 do
    if FItems[i] = prevNode then
    begin
      idx := i;
      Break;
    end;

  if idx = -1 then
  begin
    Result := addLast(insnode);
    Exit;
  end;

  if FCount = FCapacity then
    Grow;

  // Сдвигает элементы
  for i := FCount downto idx + 2 do
    FItems[i] := FItems[i - 1];

  FItems[idx + 1] := insnode;
  Inc(FCount);
  Result := insnode;
end;

function TArrayList.compare(node1, node2: TObject): integer;
var
  BaseNode1, BaseNode2: TBaseNode;
begin
  if not (node1 is TBaseNode) or not (node2 is TBaseNode) then
  begin
    Result := 0;
    Exit;
  end;

  BaseNode1 := TBaseNode(node1);
  BaseNode2 := TBaseNode(node2);

  Result := BaseNode1.CompareTo(BaseNode2);
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
begin
  // Обнуляет счетчик
  FCount := 0;
  FCurrentIndex := -1;
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
  NodeFromL: TObject;
  NodeFromResult: TObject;
  InsertAfterNode: TObject;
begin
  NodeFromL := L.first;
  InsertAfterNode := LResult.first;

  while NodeFromL <> nil do
  begin
    if InsertAfterNode = nil then
      LResult.addLast(LResult.copyNode(NodeFromL))
    else
      LResult.insertAfter(InsertAfterNode, LResult.copyNode(NodeFromL));

    NodeFromL := L.next;

    // Пропускает один узел в целевом списке
    if InsertAfterNode <> nil then
    begin
      NodeFromResult := LResult.next;
      if NodeFromResult <> nil then
        InsertAfterNode := NodeFromResult
      else
        InsertAfterNode := LResult.last;
    end;
  end;
end;

procedure ListSort(LResult: IList; L: IList; ASC: boolean);
var
  SourceNode, DestNode, TempNode: TObject;
  Inserted: Boolean;
  PrevNode: TObject;
begin
  // Если LResult не пустой, переносит его содержимое в L
  if not LResult.isEmpty then
  begin
    TempNode := LResult.first;
    while TempNode <> nil do
    begin
      L.addLast(TempNode);
      TempNode := LResult.next;
    end;
    LResult.destroyList;
  end;

  // Сортировка вставками
  SourceNode := L.first;
  while SourceNode <> nil do
  begin
    // Копирует узел для вставки
    TempNode := LResult.copyNode(SourceNode);

    // Вставляет в отсортированный список
    DestNode := LResult.first;
    PrevNode := nil;
    Inserted := False;

    while DestNode <> nil do
    begin
      if ASC then
      begin
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

    // Если не вставил, добавляет в конец
    if not Inserted then
    begin
      if PrevNode = nil then
        LResult.addFirst(TempNode)
      else
        LResult.insertAfter(PrevNode, TempNode);
    end;

    SourceNode := L.next;
  end;

  L.destroyList;
end;

end.

