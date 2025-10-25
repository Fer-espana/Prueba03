unit Privado;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, sha1;

// Alias para el tipo de ítem de datos privados que se almacenará en el árbol
type
  { Clase Privado }
  TPrivado = class
  public
    Remitente: String;
    Asunto: String;
    Fecha: String;
    Mensaje: String;

    constructor Create(ARemitente, AAsunto, AFecha, AMensaje: String);
    function GetHash: String;
  end;

  { -------- Nodo del árbol Merkle -------- }
  TMerkleNode = class
  public
    Hash: String;
    Left, Right: TMerkleNode;
    Privado: TPrivado; // Referencia al ítem de datos privados

    constructor CreateLeaf(APrivado: TPrivado); overload;
    constructor CreateInternal(ALeft, ARight: TMerkleNode); overload;
    destructor Destroy; override;
  private
    function CalculateHash(const LeftHash, RightHash: String): String;
  end;

  { Árbol Merkle}
  TMerkleTree = class
  private
    Leaves: TList; // lista de nodos hoja (contiene TMerkleNode)
    Root: TMerkleNode;

    // Método auxiliar para liberar todos los nodos internos (no hojas)
    procedure ClearInternalNodes(Node: TMerkleNode);
    // Función getter para la propiedad LeavesCount
    function GetLeavesCount: Integer;

    procedure BuildTree;
    procedure GenerateDotRecursive(Node: TMerkleNode; var SL: TStringList; var IdCounter: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Métodos de gestión
    procedure Insert(ARemitente, AAsunto, AFecha, AMensaje: String);
    function Delete(APrivado: TPrivado): Boolean;

    // Método para verificar si existe un correo por asunto
    function ExisteAsunto(const AAsunto: String): Boolean;

    // Método para acceder a una hoja por índice (necesario para la UI)
    function GetLeaf(Index: Integer): TMerkleNode;

    // Métodos de visualización
    function GenerateDot: String;
    function GetRootHash: String;

    function IsEmpty: Boolean;

    // Usando la función getter para la lectura
    property LeavesCount: Integer read GetLeavesCount;
  end;

implementation

{ Implementación TPrivado }
constructor TPrivado.Create(ARemitente, AAsunto, AFecha, AMensaje: String);
begin
  Remitente := ARemitente;
  Asunto := AAsunto;
  Fecha := AFecha;
  Mensaje := AMensaje;
end;

function TPrivado.GetHash: String;
var
  JSON: TJSONObject;
  Data: String;
  Digest: TSHA1Digest;
  i: Integer;
begin
  // 1. Crear un JSON con los atributos del ítem privado
  JSON := TJSONObject.Create;
  try
    JSON.Add('Remitente', Remitente);
    JSON.Add('Asunto', Asunto);
    JSON.Add('Fecha', Fecha);
    JSON.Add('Mensaje', Mensaje);
    Data := JSON.AsJSON; // Convertir JSON a cadena
  finally
    JSON.Free;
  end;

  // 2. Calcular el hash SHA-1 de la cadena JSON
  Digest := SHA1String(Data);

  // 3. Convertir el hash binario a formato hexadecimal
  Result := '';
  for i := 0 to High(Digest) do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
end;


{ Implementación TMerkleNode }
constructor TMerkleNode.CreateLeaf(APrivado: TPrivado);
begin
  Privado := APrivado;
  Hash := Privado.GetHash;
  Left := nil;
  Right := nil;
end;

constructor TMerkleNode.CreateInternal(ALeft, ARight: TMerkleNode);
begin
  Privado := nil; // Nodo interno no tiene ítem privado asociado
  Left := ALeft;
  Right := ARight;

  // Calcular hash del nodo interno combinando los hashes de los hijos
  if Assigned(ARight) then
    Hash := CalculateHash(ALeft.Hash, ARight.Hash)
  else
    Hash := CalculateHash(ALeft.Hash, ALeft.Hash);
end;

destructor TMerkleNode.Destroy;
begin
  // Liberar el ítem TPrivado solo si es un nodo hoja (tiene Privado asignado)
  if Assigned(Privado) then
    Privado.Free;

  inherited Destroy;
end;


function TMerkleNode.CalculateHash(const LeftHash, RightHash: String): String;
var
  Combined: String;
  Digest: TSHA1Digest;
  i: Integer;
begin
  Combined := LeftHash + RightHash;
  Digest := SHA1String(Combined);

  Result := '';
  for i := 0 to High(Digest) do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
end;


{ Implementación TMerkleTree }
constructor TMerkleTree.Create;
begin
  Leaves := TList.Create;
  Root := nil;
end;

destructor TMerkleTree.Destroy;
var
  i: Integer;
begin
  // 1. Liberar los nodos hoja y sus contenidos (TPrivado)
  for i := 0 to Leaves.Count - 1 do
    TMerkleNode(Leaves[i]).Free;

  Leaves.Clear;
  Leaves.Free;

  inherited Destroy;
end;

// Implementación del método getter para obtener una hoja por índice
function TMerkleTree.GetLeaf(Index: Integer): TMerkleNode;
begin
  if (Index >= 0) and (Index < Leaves.Count) then
    Result := TMerkleNode(Leaves[Index])
  else
    Result := nil;
end;

// Implementación de la función pública IsEmpty
function TMerkleTree.IsEmpty: Boolean;
begin
  Result := (Root = nil) or (Leaves.Count = 0);
end;

// Implementación del método para verificar duplicados
function TMerkleTree.ExisteAsunto(const AAsunto: String): Boolean;
var
  i: Integer;
  Node: TMerkleNode;
begin
  Result := False;

  for i := 0 to Leaves.Count - 1 do
  begin
    Node := TMerkleNode(Leaves[i]);
    if Assigned(Node) and Assigned(Node.Privado) then
    begin
      // Comparación case-insensitive
      if AnsiCompareText(Node.Privado.Asunto, AAsunto) = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// Procedimiento para liberar los nodos INTERNOS del árbol anterior
procedure TMerkleTree.ClearInternalNodes(Node: TMerkleNode);
begin
  if not Assigned(Node) then Exit;

  // Si el nodo es una hoja, no lo liberamos (está en Leaves)
  if Assigned(Node.Privado) then Exit;

  // Liberar recursivamente los hijos internos
  ClearInternalNodes(Node.Left);
  ClearInternalNodes(Node.Right);

  // Liberar el nodo interno actual
  Node.Free;
end;

// Getter para la propiedad LeavesCount
function TMerkleTree.GetLeavesCount: Integer;
begin
  Result := Leaves.Count;
end;

procedure TMerkleTree.Insert(ARemitente, AAsunto, AFecha, AMensaje: String);
var
  Leaf: TMerkleNode;
  Priv: TPrivado;
begin
  // Validar duplicados antes de insertar
  if ExisteAsunto(AAsunto) then
  begin
    raise Exception.Create('Ya existe un correo privado con el asunto: "' + AAsunto + '"');
  end;

  // 1. Liberar los nodos internos generados en la reconstrucción anterior
  ClearInternalNodes(Root);
  Root := nil; // Resetear Root antes de la reconstrucción

  // 2. Crear un nuevo ítem privado y su nodo hoja
  Priv := TPrivado.Create(ARemitente, AAsunto, AFecha, AMensaje);
  Leaf := TMerkleNode.CreateLeaf(Priv);

  // 3. Agregar la hoja a la lista
  Leaves.Add(Leaf);

  // 4. Reconstruir todo el árbol Merkle
  BuildTree;
end;

function TMerkleTree.Delete(APrivado: TPrivado): Boolean;
var
  i: Integer;
  NodeToDelete: TMerkleNode;
begin
  Result := False;
  NodeToDelete := nil;

  // 1. Encontrar el nodo hoja (TMerkleNode) que contiene el TPrivado
  for i := 0 to Leaves.Count - 1 do
  begin
    NodeToDelete := TMerkleNode(Leaves[i]);
    if Assigned(NodeToDelete.Privado) and (NodeToDelete.Privado = APrivado) then
    begin
      // 2. Eliminar el nodo de la lista de hojas
      Leaves.Delete(i);
      Result := True;
      Break;
    end;
    NodeToDelete := nil;
  end;

  if Result then
  begin
    // 3. Liberar la memoria del nodo hoja y su contenido
    if Assigned(NodeToDelete) then
      NodeToDelete.Free;

    // 4. Liberar los nodos internos antiguos y reconstruir el árbol
    ClearInternalNodes(Root);
    Root := nil;
    BuildTree;
  end;
end;


procedure TMerkleTree.BuildTree;
var
  CurrentLevel, NextLevel: TList;
  i: Integer;
  Left, Right, Parent: TMerkleNode;
begin
  // Si no hay hojas, el árbol queda vacío
  if Leaves.Count = 0 then
  begin
    Root := nil;
    Exit;
  end;

  // Si solo hay una hoja, esa es la raíz
  if Leaves.Count = 1 then
  begin
    Root := TMerkleNode(Leaves[0]);
    Exit;
  end;

  CurrentLevel := TList.Create;
  try
    // Clonar las hojas (solo las referencias a los nodos hoja)
    CurrentLevel.Assign(Leaves);

    // Construir niveles superiores hasta la raíz
    while CurrentLevel.Count > 1 do
    begin
      NextLevel := TList.Create;
      i := 0;
      while i < CurrentLevel.Count do
      begin
        Left := TMerkleNode(CurrentLevel[i]);
        Inc(i);

        // Obtener hijo derecho
        if i < CurrentLevel.Count then
        begin
          Right := TMerkleNode(CurrentLevel[i]);
          Inc(i);
        end
        else
          Right := nil;

        // Crear nodo padre (interno)
        Parent := TMerkleNode.CreateInternal(Left, Right);
        NextLevel.Add(Parent);
      end;

      // Liberar el TList del nivel actual (NO los nodos que contiene)
      CurrentLevel.Free;
      CurrentLevel := NextLevel;
    end;

    // Asignar el nodo raíz final
    Root := TMerkleNode(CurrentLevel[0]);

  finally
    // Liberar el TList del último nivel (el que contiene la Raíz)
    CurrentLevel.Free;
  end;
end;

procedure TMerkleTree.GenerateDotRecursive(Node: TMerkleNode; var SL: TStringList; var IdCounter: Integer);
var
  NodeId, LeftId, RightId: Integer;
  LabelText: String;
  HashPreview: String;
begin
  if Node = nil then Exit;

  NodeId := IdCounter;
  Inc(IdCounter);
  HashPreview := Copy(Node.Hash, 1, 8);

  // Crear etiqueta para el nodo
  if Assigned(Node.Privado) then
  begin
    // Nodo hoja: muestra detalles del ítem privado
    LabelText := 'De: ' + Node.Privado.Remitente + '\n' +
                 'Asunto: ' + Node.Privado.Asunto + '\n' +
                 'Fecha: ' + Node.Privado.Fecha + '\n' +
                 'Hash: ' + HashPreview + '...';

    SL.Add('  node' + IntToStr(NodeId) + ' [shape=box, style="filled", fillcolor="#C8E6C9", label="' + LabelText + '"];');
  end
  else
  begin
    // Nodo interno: solo muestra el hash
    LabelText := 'Hash: ' + HashPreview + '...';
    SL.Add('  node' + IntToStr(NodeId) + ' [shape=ellipse, style="filled", fillcolor="#BBDEFB", label="' + LabelText + '"];');
  end;

  // Procesar hijos recursivamente y crear los enlaces
  if Assigned(Node.Left) then
  begin
    LeftId := IdCounter;
    GenerateDotRecursive(Node.Left, SL, IdCounter);
    SL.Add('  node' + IntToStr(NodeId) + ' -> node' + IntToStr(LeftId) + ' [color="#0D47A1"];');
  end;

  if Assigned(Node.Right) then
  begin
    RightId := IdCounter;
    GenerateDotRecursive(Node.Right, SL, IdCounter);
    SL.Add('  node' + IntToStr(NodeId) + ' -> node' + IntToStr(RightId) + ' [color="#0D47A1"];');
  end;
end;

function TMerkleTree.GenerateDot: String;
var
  SL: TStringList;
  Counter: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph MerkleTreePrivados {');
    SL.Add('  node [fontname="Arial", fontsize=10];');
    SL.Add('  graph [rankdir=TB, bgcolor="#F5F5F5"];');
    SL.Add('  subgraph cluster_0 {');
    SL.Add('    label="Árbol Merkle de Documentos Privados";');
    SL.Add('    style=rounded;');
    SL.Add('    color="#4CAF50";');

    // Si el árbol está vacío
    if Root = nil then
      SL.Add('    empty [label="Árbol vacío. Ingrese un ítem.", shape=box, style="filled", fillcolor="#FFCDD2"];')
    else
    begin
      Counter := 0;
      GenerateDotRecursive(Root, SL, Counter);

      // Resaltar la raíz
      SL.Add('  node0 [style="filled, bold", fillcolor="#FFEB3B", peripheries=2];');
    end;

    SL.Add('  }');
    SL.Add('}');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TMerkleTree.GetRootHash: String;
begin
  if Assigned(Root) then
    Result := Root.Hash
  else
    Result := '';
end;

end.
