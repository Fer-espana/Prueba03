unit UAVLTreeBorradores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaDobleEnlazadaCorreos;

type
  PNodeAVL = ^TNodeAVL;
  TNodeAVL = record
    Key: Integer;
    Correo: TCorreo;
    Height: Integer;
    Left: PNodeAVL;
    Right: PNodeAVL;
  end;

  TAVLTree = record
    Root: PNodeAVL;
  end;

  // *** NUEVAS DECLARACIONES PARA BORRADORES ***
  PBorrador = ^TBorrador;
  TBorrador = record
    id: Integer;
    remitente: String;
    destinatario: String;
    asunto: String;
    mensaje: String;
    fecha: TDateTime;
  end;

  TArbolAVL = record
    raiz: PNodeAVL;
  end;

// Procedimientos y Funciones Mínimas
procedure InicializarAVL(var Tree: TAVLTree);
procedure InsertarEnAVL(var Tree: TAVLTree; Key: Integer; Correo: TCorreo);
function BuscarEnAVL(Tree: TAVLTree; Key: Integer): PCorreo;
function EliminarDeAVL(var Tree: TAVLTree; Key: Integer): Boolean;
function ObtenerRecorridoAVL(Tree: TAVLTree; Tipo: string): string;
procedure GenerarReporteDOTAVL(Tree: TAVLTree; NombreArchivo: string);
procedure LiberarAVL(var Tree: TAVLTree);

// *** NUEVAS FUNCIONES PARA BORRADORES (Fase 3) ***
procedure InicializarArbolAVL(var arbol: TArbolAVL);
procedure InsertarBorrador(var arbol: TArbolAVL; id: Integer; remitente, destinatario, asunto, mensaje: String);
function BuscarBorrador(arbol: TArbolAVL; id: Integer): PBorrador;
function ExtraerBorrador(var arbol: TArbolAVL; id: Integer): PBorrador;
procedure RecorrerInOrden(nodo: PNodeAVL; var lista: TStringList);
procedure RecorrerPreOrden(nodo: PNodeAVL; var lista: TStringList);
procedure RecorrerPostOrden(nodo: PNodeAVL; var lista: TStringList);

implementation

// *** FUNCIONES AUXILIARES AVL ***
function Max(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

function Height(nodo: PNodeAVL): Integer;
begin
  if nodo = nil then Result := 0
  else Result := nodo^.Height;
end;

function BalanceFactor(nodo: PNodeAVL): Integer;
begin
  if nodo = nil then Result := 0
  else Result := Height(nodo^.Left) - Height(nodo^.Right);
end;

procedure UpdateHeight(nodo: PNodeAVL);
begin
  if nodo <> nil then
    nodo^.Height := Max(Height(nodo^.Left), Height(nodo^.Right)) + 1;
end;

function RotateRight(y: PNodeAVL): PNodeAVL;
var
  x, T2: PNodeAVL;
begin
  x := y^.Left;
  T2 := x^.Right;

  x^.Right := y;
  y^.Left := T2;

  UpdateHeight(y);
  UpdateHeight(x);

  Result := x;
end;

function RotateLeft(x: PNodeAVL): PNodeAVL;
var
  y, T2: PNodeAVL;
begin
  y := x^.Right;
  T2 := y^.Left;

  y^.Left := x;
  x^.Right := T2;

  UpdateHeight(x);
  UpdateHeight(y);

  Result := y;
end;

function BalanceNode(nodo: PNodeAVL): PNodeAVL;
var
  balance: Integer;
begin
  UpdateHeight(nodo);
  balance := BalanceFactor(nodo);

  // Left Left Case
  if (balance > 1) and (BalanceFactor(nodo^.Left) >= 0) then
    Result := RotateRight(nodo)
  // Right Right Case
  else if (balance < -1) and (BalanceFactor(nodo^.Right) <= 0) then
    Result := RotateLeft(nodo)
  // Left Right Case
  else if (balance > 1) and (BalanceFactor(nodo^.Left) < 0) then
  begin
    nodo^.Left := RotateLeft(nodo^.Left);
    Result := RotateRight(nodo);
  end
  // Right Left Case
  else if (balance < -1) and (BalanceFactor(nodo^.Right) > 0) then
  begin
    nodo^.Right := RotateRight(nodo^.Right);
    Result := RotateLeft(nodo);
  end
  else
    Result := nodo;
end;

// *** IMPLEMENTACIONES COMPLETAS ***
procedure InicializarAVL(var Tree: TAVLTree);
begin
  Tree.Root := nil;
end;

procedure InsertarEnAVL(var Tree: TAVLTree; Key: Integer; Correo: TCorreo);

  function InsertNode(nodo: PNodeAVL; Key: Integer; Correo: TCorreo): PNodeAVL;
  begin
    if nodo = nil then
    begin
      New(nodo);
      nodo^.Key := Key;
      nodo^.Correo := Correo;
      nodo^.Left := nil;
      nodo^.Right := nil;
      nodo^.Height := 1;
      Result := nodo;
    end
    else if Key < nodo^.Key then
      nodo^.Left := InsertNode(nodo^.Left, Key, Correo)
    else if Key > nodo^.Key then
      nodo^.Right := InsertNode(nodo^.Right, Key, Correo)
    else
      Result := nodo; // Claves duplicadas no permitidas

    Result := BalanceNode(Result);
  end;

begin
  Tree.Root := InsertNode(Tree.Root, Key, Correo);
end;

function BuscarEnAVL(Tree: TAVLTree; Key: Integer): PCorreo;

  function SearchNode(nodo: PNodeAVL; Key: Integer): PNodeAVL;
  begin
    if (nodo = nil) or (nodo^.Key = Key) then
      Result := nodo
    else if Key < nodo^.Key then
      Result := SearchNode(nodo^.Left, Key)
    else
      Result := SearchNode(nodo^.Right, Key);
  end;

var
  nodo: PNodeAVL;
begin
  Result := nil;
  nodo := SearchNode(Tree.Root, Key);
  if nodo <> nil then
    Result := @nodo^.Correo;
end;

function EliminarDeAVL(var Tree: TAVLTree; Key: Integer): Boolean;

  function FindMinNode(nodo: PNodeAVL): PNodeAVL;
  begin
    if nodo = nil then
      Result := nil
    else if nodo^.Left = nil then
      Result := nodo
    else
      Result := FindMinNode(nodo^.Left);
  end;

  function DeleteNode(nodo: PNodeAVL; Key: Integer): PNodeAVL;
  var
    temp: PNodeAVL;
  begin
    if nodo = nil then
      Result := nil
    else if Key < nodo^.Key then
      nodo^.Left := DeleteNode(nodo^.Left, Key)
    else if Key > nodo^.Key then
      nodo^.Right := DeleteNode(nodo^.Right, Key)
    else
    begin
      // Nodo encontrado
      if (nodo^.Left = nil) or (nodo^.Right = nil) then
      begin
        if nodo^.Left = nil then
          temp := nodo^.Right
        else
          temp := nodo^.Left;

        if temp = nil then
        begin
          temp := nodo;
          nodo := nil;
        end
        else
          nodo^ := temp^;

        Dispose(temp);
      end
      else
      begin
        temp := FindMinNode(nodo^.Right);
        nodo^.Key := temp^.Key;
        nodo^.Correo := temp^.Correo;
        nodo^.Right := DeleteNode(nodo^.Right, temp^.Key);
      end;
    end;

    if nodo <> nil then
      Result := BalanceNode(nodo)
    else
      Result := nil;
  end;

begin
  if BuscarEnAVL(Tree, Key) <> nil then
  begin
    Tree.Root := DeleteNode(Tree.Root, Key);
    Result := True;
  end
  else
    Result := False;
end;

// *** NUEVAS IMPLEMENTACIONES PARA BORRADORES ***
procedure InicializarArbolAVL(var arbol: TArbolAVL);
begin
  arbol.raiz := nil;
end;

procedure InsertarBorrador(var arbol: TArbolAVL; id: Integer; remitente, destinatario, asunto, mensaje: String);
var
  correo: TCorreo;
begin
  // Convertir borrador a correo para insertar en AVL
  correo.Id := id;
  correo.Remitente := remitente;
  correo.Destinatario := destinatario;
  correo.Asunto := asunto;
  correo.Mensaje := mensaje;
  correo.Fecha := Now;
  correo.Estado := 'B'; // Borrador
  correo.Programado := False;

  InsertarEnAVL(TAVLTree(arbol), id, correo);
end;

function BuscarBorrador(arbol: TArbolAVL; id: Integer): PBorrador;
var
  correo: PCorreo;
begin
  Result := nil;
  correo := BuscarEnAVL(TAVLTree(arbol), id);
  if correo <> nil then
  begin
    New(Result);
    Result^.id := correo^.Id;
    Result^.remitente := correo^.Remitente;
    Result^.destinatario := correo^.Destinatario;
    Result^.asunto := correo^.Asunto;
    Result^.mensaje := correo^.Mensaje;
    Result^.fecha := correo^.Fecha;
  end;
end;

function ExtraerBorrador(var arbol: TArbolAVL; id: Integer): PBorrador;
var
  borrador: PBorrador;
begin
  Result := nil;
  borrador := BuscarBorrador(arbol, id);
  if borrador <> nil then
  begin
    // Crear copia del resultado
    New(Result);
    Result^ := borrador^;

    // Eliminar del árbol
    EliminarDeAVL(TAVLTree(arbol), id);

    // Liberar borrador temporal
    Dispose(borrador);
  end;
end;

// *** PROCEDIMIENTOS DE RECORRIDO ***
procedure RecorrerInOrden(nodo: PNodeAVL; var lista: TStringList);
begin
  if nodo <> nil then
  begin
    RecorrerInOrden(nodo^.Left, lista);
    lista.Add(Format('%d|%s|%s', [nodo^.Key, nodo^.Correo.Asunto, nodo^.Correo.Destinatario]));
    RecorrerInOrden(nodo^.Right, lista);
  end;
end;

procedure RecorrerPreOrden(nodo: PNodeAVL; var lista: TStringList);
begin
  if nodo <> nil then
  begin
    lista.Add(Format('%d|%s|%s', [nodo^.Key, nodo^.Correo.Asunto, nodo^.Correo.Destinatario]));
    RecorrerPreOrden(nodo^.Left, lista);
    RecorrerPreOrden(nodo^.Right, lista);
  end;
end;

procedure RecorrerPostOrden(nodo: PNodeAVL; var lista: TStringList);
begin
  if nodo <> nil then
  begin
    RecorrerPostOrden(nodo^.Left, lista);
    RecorrerPostOrden(nodo^.Right, lista);
    lista.Add(Format('%d|%s|%s', [nodo^.Key, nodo^.Correo.Asunto, nodo^.Correo.Destinatario]));
  end;
end;

// *** RESTANTES IMPLEMENTACIONES ***
function RecorridoRecursivo(Nodo: PNodeAVL; Tipo: string): string;
var
  LeftStr, RightStr: string;
begin
  if Nodo = nil then Exit('');

  LeftStr := RecorridoRecursivo(Nodo^.Left, Tipo);
  RightStr := RecorridoRecursivo(Nodo^.Right, Tipo);

  case UpperCase(Tipo) of
    'PRE-ORDEN': Result := IntToStr(Nodo^.Key) + ' ' + LeftStr + RightStr;
    'IN-ORDEN': Result := LeftStr + IntToStr(Nodo^.Key) + ' ' + RightStr;
    'POST-ORDEN': Result := LeftStr + RightStr + IntToStr(Nodo^.Key) + ' ';
    else Result := '';
  end;
end;

function ObtenerRecorridoAVL(Tree: TAVLTree; Tipo: string): string;
begin
  Result := Trim(RecorridoRecursivo(Tree.Root, Tipo));
  if Result = '' then Result := 'Árbol Vacío';
end;

procedure GenerarNodoDOT(Nodo: PNodeAVL; var Archivo: TextFile; var Contador: Integer);
var
  CurrentID: Integer;
  LeftID, RightID: Integer;
begin
  if Nodo = nil then Exit;

  CurrentID := Contador;
  Inc(Contador);

  WriteLn(Archivo, '  node', CurrentID, ' [label="{',
          'ID: ', IntToStr(Nodo^.Key), ' | ',
          'Asunto: ', Nodo^.Correo.Asunto, ' | ',
          'Dest: ', Nodo^.Correo.Destinatario, ' | ',
          'Altura: ', IntToStr(Nodo^.Height),
          '}"];');

  if Nodo^.Left <> nil then
  begin
    LeftID := Contador;
    GenerarNodoDOT(Nodo^.Left, Archivo, Contador);
    WriteLn(Archivo, '  node', CurrentID, ' -> node', LeftID, ' [color=blue, label="L"];');
  end;

  if Nodo^.Right <> nil then
  begin
    RightID := Contador;
    GenerarNodoDOT(Nodo^.Right, Archivo, Contador);
    WriteLn(Archivo, '  node', CurrentID, ' -> node', RightID, ' [color=red, label="R"];');
  end;
end;

procedure GenerarReporteDOTAVL(Tree: TAVLTree; NombreArchivo: string);
var
  Archivo: TextFile;
  Contador: Integer;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph BorradoresAVL {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightcyan];');
    WriteLn(Archivo, '  edge [arrowhead=vee];');
    WriteLn(Archivo, '');

    Contador := 0;

    if Tree.Root <> nil then
      GenerarNodoDOT(Tree.Root, Archivo, Contador)
    else
      WriteLn(Archivo, '  vacio [label="ÁRBOL AVL VACÍO", shape=plaintext];');

    WriteLn(Archivo, '}');
  finally
    CloseFile(Archivo);
  end;
end;

procedure LiberarAVL(var Tree: TAVLTree);

  procedure FreeNode(nodo: PNodeAVL);
  begin
    if nodo <> nil then
    begin
      FreeNode(nodo^.Left);
      FreeNode(nodo^.Right);
      Dispose(nodo);
    end;
  end;

begin
  FreeNode(Tree.Root);
  Tree.Root := nil;
end;

end.
