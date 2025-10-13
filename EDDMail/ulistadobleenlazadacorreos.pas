unit UListaDobleEnlazadaCorreos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PCorreo = ^TCorreo;
  TCorreo = record
    Id: Integer;
    Remitente: string;
    Destinatario: string;  // NUEVO CAMPO
    Estado: Char;  // 'L'=Leído, 'N'=No leído, 'E'=Eliminado, 'P'=Programado
    Programado: Boolean;
    Asunto: string;
    Fecha: string;
    Mensaje: string;
    Anterior: PCorreo;
    Siguiente: PCorreo;
  end;

  TListaCorreos = record
    Cabeza: PCorreo;
    Cola: PCorreo;
    Count: Integer;
  end;

// Procedimientos básicos
procedure InicializarListaCorreos(var Lista: TListaCorreos);
// CORREGIR: Agregar el parámetro Destinatario en la declaración
procedure InsertarCorreo(var Lista: TListaCorreos; Id: Integer;
  Remitente, Destinatario: string; Estado: Char; Programado: Boolean;
  Asunto, Fecha, Mensaje: string);
function BuscarCorreoPorId(Lista: TListaCorreos; Id: Integer): PCorreo;
procedure EliminarCorreo(var Lista: TListaCorreos; Id: Integer);
procedure MostrarCorreos(Lista: TListaCorreos);
procedure LiberarListaCorreos(var Lista: TListaCorreos);
procedure GenerarReporteDOTCorreosRecibidos(Lista: TListaCorreos; NombreArchivo: string);
function ObtenerCorreoPorPosicion(Lista: TListaCorreos; Posicion: Integer): PCorreo;

implementation

procedure InicializarListaCorreos(var Lista: TListaCorreos);
begin
  Lista.Cabeza := nil;
  Lista.Cola := nil;
  Lista.Count := 0;
end;

procedure InsertarCorreo(var Lista: TListaCorreos; Id: Integer;
  Remitente, Destinatario: string; Estado: Char; Programado: Boolean;
  Asunto, Fecha, Mensaje: string);
var
  Nuevo: PCorreo;
begin
  New(Nuevo);
  Nuevo^.Id := Id;
  Nuevo^.Remitente := Remitente;
  Nuevo^.Destinatario := Destinatario;  // NUEVO CAMPO
  Nuevo^.Estado := Estado;
  Nuevo^.Programado := Programado;
  Nuevo^.Asunto := Asunto;
  Nuevo^.Fecha := Fecha;
  Nuevo^.Mensaje := Mensaje;
  Nuevo^.Anterior := nil;
  Nuevo^.Siguiente := nil;

  if Lista.Cabeza = nil then
  begin
    Lista.Cabeza := Nuevo;
    Lista.Cola := Nuevo;
  end
  else
  begin
    Nuevo^.Anterior := Lista.Cola;
    Lista.Cola^.Siguiente := Nuevo;
    Lista.Cola := Nuevo;
  end;

  Inc(Lista.Count);
end;

function BuscarCorreoPorId(Lista: TListaCorreos; Id: Integer): PCorreo;
var
  Actual: PCorreo;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    if Actual^.Id = Id then
      Exit(Actual);
    Actual := Actual^.Siguiente;
  end;
  Result := nil;
end;

procedure EliminarCorreo(var Lista: TListaCorreos; Id: Integer);
var
  Actual: PCorreo;
begin
  Actual := BuscarCorreoPorId(Lista, Id);
  if Actual = nil then Exit;

  if Actual^.Anterior <> nil then
    Actual^.Anterior^.Siguiente := Actual^.Siguiente
  else
    Lista.Cabeza := Actual^.Siguiente;

  if Actual^.Siguiente <> nil then
    Actual^.Siguiente^.Anterior := Actual^.Anterior
  else
    Lista.Cola := Actual^.Anterior;

  Dispose(Actual);
  Dec(Lista.Count);
end;

procedure MostrarCorreos(Lista: TListaCorreos);
var
  Actual: PCorreo;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    WriteLn('ID: ', Actual^.Id);
    WriteLn('Remitente: ', Actual^.Remitente);
    WriteLn('Destinatario: ', Actual^.Destinatario);
    WriteLn('Estado: ', Actual^.Estado);
    WriteLn('Asunto: ', Actual^.Asunto);
    WriteLn('-------------------');
    Actual := Actual^.Siguiente;
  end;
end;

function ObtenerCorreoPorPosicion(Lista: TListaCorreos; Posicion: Integer): PCorreo;
var
  Actual: PCorreo;
  i: Integer;
begin
  if (Posicion < 0) or (Posicion >= Lista.Count) then
    Exit(nil);

  Actual := Lista.Cabeza;
  for i := 0 to Posicion - 1 do
  begin
    if Actual = nil then Break;
    Actual := Actual^.Siguiente;
  end;
  Result := Actual;
end;

procedure LiberarListaCorreos(var Lista: TListaCorreos);
var
  Actual, Temp: PCorreo;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    Temp := Actual;
    Actual := Actual^.Siguiente;
    Dispose(Temp);
  end;
  Lista.Cabeza := nil;
  Lista.Cola := nil;
  Lista.Count := 0;
end;

procedure GenerarReporteDOTCorreosRecibidos(Lista: TListaCorreos; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PCorreo;
  Contador: Integer;
  EstadoStr: string;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph CorreosRecibidos {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '  edge [color=darkblue];');
    WriteLn(Archivo, '');

    Contador := 0;
    Actual := Lista.Cabeza;

    while Actual <> nil do
    begin
      // Convertir estado a texto legible
      case Actual^.Estado of
        'N': EstadoStr := 'NO LEIDO';
        'L': EstadoStr := 'LEIDO';
        'E': EstadoStr := 'ELIMINADO';
        'P': EstadoStr := 'PROGRAMADO';
        else EstadoStr := 'DESCONOCIDO';
      end;

      // Crear nodo para cada correo
      WriteLn(Archivo, '  correo', Contador, ' [label="');
      WriteLn(Archivo, 'ID: ', Actual^.Id, '\\n');
      WriteLn(Archivo, 'Remitente: ', Actual^.Remitente, '\\n');
      WriteLn(Archivo, 'Asunto: ', Actual^.Asunto, '\\n');
      WriteLn(Archivo, 'Fecha: ', Actual^.Fecha, '\\n');
      WriteLn(Archivo, 'Estado: ', EstadoStr, '"];');

      // Conectar con el siguiente correo
      if (Actual^.Siguiente <> nil) then
      begin
        WriteLn(Archivo, '  correo', Contador, ' -> correo', Contador + 1, ';');
      end;

      // Conectar con el correo anterior (para mostrar doble enlace)
      if (Actual^.Anterior <> nil) then
      begin
        WriteLn(Archivo, '  correo', Contador, ' -> correo', Contador - 1, ' [color=red, style=dashed];');
      end;

      Inc(Contador);
      Actual := Actual^.Siguiente;
    end;

    // Pie del archivo DOT
    WriteLn(Archivo, '');
    WriteLn(Archivo, '  // Leyenda');
    WriteLn(Archivo, '  subgraph cluster_leyenda {');
    WriteLn(Archivo, '    label="Leyenda";');
    WriteLn(Archivo, '    leyenda1 [label="Flecha azul: Siguiente\\nFlecha roja: Anterior", shape=note, fillcolor=lightyellow];');
    WriteLn(Archivo, '  }');
    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

end.
