unit UColaCorreosProgramados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaDobleEnlazadaCorreos;

type
  PNodoCola = ^TNodoCola;
  TNodoCola = record
    Correo: PCorreo;
    Siguiente: PNodoCola;
  end;

  TCola = record
    Frente: PNodoCola;
    Final: PNodoCola;
    Count: Integer;
  end;

// Procedimientos básicos
procedure InicializarCola(var Cola: TCola);
procedure Encolar(var Cola: TCola; Correo: PCorreo);
function Desencolar(var Cola: TCola): PCorreo;
function FrenteCola(Cola: TCola): PCorreo;
function ColaVacia(Cola: TCola): Boolean;
procedure MostrarCola(Cola: TCola);
procedure LiberarCola(var Cola: TCola);
procedure GenerarReporteDOTCorreosProgramados(Cola: TCola; NombreArchivo: string);

implementation

procedure InicializarCola(var Cola: TCola);
begin
  Cola.Frente := nil;
  Cola.Final := nil;
  Cola.Count := 0;
end;

procedure Encolar(var Cola: TCola; Correo: PCorreo);
var
  Nuevo: PNodoCola;
begin
  New(Nuevo);
  Nuevo^.Correo := Correo;
  Nuevo^.Siguiente := nil;

  if Cola.Final = nil then
  begin
    Cola.Frente := Nuevo;
    Cola.Final := Nuevo;
  end
  else
  begin
    Cola.Final^.Siguiente := Nuevo;
    Cola.Final := Nuevo;
  end;

  Inc(Cola.Count);
end;

function Desencolar(var Cola: TCola): PCorreo;
var
  Temp: PNodoCola;
begin
  if Cola.Frente = nil then
    Exit(nil);

  Temp := Cola.Frente;
  Result := Temp^.Correo;
  Cola.Frente := Cola.Frente^.Siguiente;

  if Cola.Frente = nil then
    Cola.Final := nil;

  Dispose(Temp);
  Dec(Cola.Count);
end;

function FrenteCola(Cola: TCola): PCorreo;
begin
  if Cola.Frente = nil then
    Result := nil
  else
    Result := Cola.Frente^.Correo;
end;

function ColaVacia(Cola: TCola): Boolean;
begin
  Result := Cola.Frente = nil;
end;

procedure MostrarCola(Cola: TCola);
var
  Actual: PNodoCola;
begin
  Actual := Cola.Frente;
  while Actual <> nil do
  begin
    WriteLn('Correo Programado - ID: ', Actual^.Correo^.Id);
    WriteLn('Asunto: ', Actual^.Correo^.Asunto);
    WriteLn('Fecha: ', Actual^.Correo^.Fecha);
    WriteLn('-------------------');
    Actual := Actual^.Siguiente;
  end;
end;

procedure LiberarCola(var Cola: TCola);
var
  Temp: PNodoCola;
begin
  while Cola.Frente <> nil do
  begin
    Temp := Cola.Frente;
    Cola.Frente := Cola.Frente^.Siguiente;
    Dispose(Temp);
  end;
  Cola.Final := nil;
  Cola.Count := 0;
end;

procedure GenerarReporteDOTCorreosProgramados(Cola: TCola; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PNodoCola;
  Contador: Integer;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph CorreosProgramados {');
    WriteLn(Archivo, '  rankdir=LR;');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightyellow];');
    WriteLn(Archivo, '  edge [color=darkorange];');
    WriteLn(Archivo, '');

    Contador := 0;
    Actual := Cola.Frente;

    // Mostrar nodos de frente y final
    WriteLn(Archivo, '  frente [label="FRENTE", shape=ellipse, fillcolor=orange];');
    WriteLn(Archivo, '  final [label="FINAL", shape=ellipse, fillcolor=lightgreen];');

    while Actual <> nil do
    begin
      if Actual^.Correo <> nil then
      begin
        // Crear nodo para cada correo programado
        WriteLn(Archivo, '  programado', Contador, ' [label="');
        WriteLn(Archivo, 'ID: ', Actual^.Correo^.Id, '\\n');
        WriteLn(Archivo, 'Remitente: ', Actual^.Correo^.Remitente, '\\n');
        WriteLn(Archivo, 'Destinatario: ', Actual^.Correo^.Destinatario, '\\n');
        WriteLn(Archivo, 'Asunto: ', Actual^.Correo^.Asunto, '\\n');
        WriteLn(Archivo, 'Fecha Prog.: ', Actual^.Correo^.Fecha, '"];');

        // Conectar con el siguiente elemento de la cola
        if Contador = 0 then
        begin
          WriteLn(Archivo, '  frente -> programado', Contador, ';');
        end
        else
        begin
          WriteLn(Archivo, '  programado', Contador - 1, ' -> programado', Contador, ';');
        end;

        // Si es el último elemento, conectar con "final"
        if Actual^.Siguiente = nil then
        begin
          WriteLn(Archivo, '  programado', Contador, ' -> final;');
        end;

        Inc(Contador);
      end;
      Actual := Actual^.Siguiente;
    end;

    // Si la cola está vacía
    if Contador = 0 then
    begin
      WriteLn(Archivo, '  vacio [label="COLA VACÍA", shape=plaintext];');
      WriteLn(Archivo, '  frente -> vacio;');
      WriteLn(Archivo, '  vacio -> final;');
    end;

    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

end.
