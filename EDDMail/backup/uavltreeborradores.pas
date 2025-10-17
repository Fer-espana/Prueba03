unit UAVLTreeBorradores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaDobleEnlazadaCorreos; // Usamos TCorreo

// Definición de las estructuras para el Árbol AVL
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

// Procedimientos y Funciones Mínimas
procedure InicializarAVL(var Tree: TAVLTree);
procedure InsertarEnAVL(var Tree: TAVLTree; Key: Integer; Correo: TCorreo);
function BuscarEnAVL(Tree: TAVLTree; Key: Integer): PCorreo;
function EliminarDeAVL(var Tree: TAVLTree; Key: Integer): Boolean;
function ObtenerRecorridoAVL(Tree: TAVLTree; Tipo: string): string;
procedure GenerarReporteDOTAVL(Tree: TAVLTree; NombreArchivo: string);
procedure LiberarAVL(var Tree: TAVLTree);


implementation

// DECLARACIÓN FORWARD (CORRECCIÓN: MOVIDA AQUÍ)
procedure GenerarNodoDOT(Nodo: PNodeAVL; var Archivo: TextFile; var Contador: Integer); forward;

// STUBS (Lógica pendiente)
procedure InicializarAVL(var Tree: TAVLTree); begin Tree.Root := nil; end;
procedure InsertarEnAVL(var Tree: TAVLTree; Key: Integer; Correo: TCorreo); begin end;
function BuscarEnAVL(Tree: TAVLTree; Key: Integer): PCorreo; begin Result := nil; end;
function EliminarDeAVL(var Tree: TAVLTree; Key: Integer): Boolean; begin Result := False; end;

// IMPLEMENTACIÓN DE RECORRIDO
function RecorridoRecursivo(Nodo: PNodeAVL; Tipo: string): string;
var
  LeftStr, RightStr: string;
begin
  if Nodo = nil then Exit('');

  LeftStr := RecorridoRecursivo(Nodo^.Left, Tipo);
  // CORRECCIÓN: Se cambió "RecorrivoRecursivo" a "RecorridoRecursivo"
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

// PROCEDIMIENTO RECURSIVO PARA GENERAR NODO DOT
procedure GenerarNodoDOT(Nodo: PNodeAVL; var Archivo: TextFile; var Contador: Integer);
var
  CurrentID: Integer;
  LeftID, RightID: Integer;
begin
  if Nodo = nil then Exit;

  CurrentID := Contador;
  Inc(Contador);

  // Crear nodo actual
  WriteLn(Archivo, '  node', CurrentID, ' [label="{',
          'ID: ', IntToStr(Nodo^.Key), ' | ',
          'Asunto: ', Nodo^.Correo.Asunto, ' | ',
          'Dest: ', Nodo^.Correo.Destinatario, ' | ',
          'Altura: ', IntToStr(Nodo^.Height),
          '}"];');

  // Conectar con hijos (recursivamente)
  if Nodo^.Left <> nil then
  begin
    LeftID := Contador;
    GenerarNodoDOT(Nodo^.Left, Archivo, Contador);
    WriteLn(Archivo, '  node', CurrentID, ' -> node', LeftID, ' [color=blue, label="L"];'); // L para Left
  end;

  if Nodo^.Right <> nil then
  begin
    RightID := Contador;
    GenerarNodoDOT(Nodo^.Right, Archivo, Contador);
    WriteLn(Archivo, '  node', CurrentID, ' -> node', RightID, ' [color=red, label="R"];'); // R para Right
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

    // Encabezado DOT
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
begin
  Tree.Root := nil;
end;

end.
