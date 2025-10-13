unit UPilaPapelera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaDobleEnlazadaCorreos;

type
  PNodoPila = ^TNodoPila;
  TNodoPila = record
    Correo: PCorreo;
    Siguiente: PNodoPila;
  end;

  TPila = record
    Cima: PNodoPila;
    Count: Integer;
  end;

// Procedimientos básicos
procedure InicializarPila(var Pila: TPila);
procedure Apilar(var Pila: TPila; Correo: PCorreo);
function Desapilar(var Pila: TPila): PCorreo;
function CimaPila(Pila: TPila): PCorreo;
function PilaVacia(Pila: TPila): Boolean;
procedure MostrarPila(Pila: TPila);
procedure LiberarPila(var Pila: TPila);
procedure GenerarReporteDOTPapelera(Pila: TPila; NombreArchivo: string);

implementation

procedure InicializarPila(var Pila: TPila);
begin
  Pila.Cima := nil;
  Pila.Count := 0;
end;

procedure Apilar(var Pila: TPila; Correo: PCorreo);
var
  Nuevo: PNodoPila;
begin
  New(Nuevo);
  Nuevo^.Correo := Correo;
  Nuevo^.Siguiente := Pila.Cima;
  Pila.Cima := Nuevo;
  Inc(Pila.Count);
end;

function Desapilar(var Pila: TPila): PCorreo;
var
  Temp: PNodoPila;
begin
  if Pila.Cima = nil then
    Exit(nil);

  Temp := Pila.Cima;
  Result := Temp^.Correo;
  Pila.Cima := Pila.Cima^.Siguiente;
  Dispose(Temp);
  Dec(Pila.Count);
end;

function CimaPila(Pila: TPila): PCorreo;
begin
  if Pila.Cima = nil then
    Result := nil
  else
    Result := Pila.Cima^.Correo;
end;

function PilaVacia(Pila: TPila): Boolean;
begin
  Result := Pila.Cima = nil;
end;

procedure MostrarPila(Pila: TPila);
var
  Actual: PNodoPila;
begin
  Actual := Pila.Cima;
  while Actual <> nil do
  begin
    WriteLn('Correo Eliminado - ID: ', Actual^.Correo^.Id);
    WriteLn('Asunto: ', Actual^.Correo^.Asunto);
    WriteLn('Remitente: ', Actual^.Correo^.Remitente);
    WriteLn('-------------------');
    Actual := Actual^.Siguiente;
  end;
end;

procedure LiberarPila(var Pila: TPila);
var
  Temp: PNodoPila;
begin
  while Pila.Cima <> nil do
  begin
    Temp := Pila.Cima;

    // Liberar el correo si existe
    if Temp^.Correo <> nil then
      Dispose(Temp^.Correo);

    Pila.Cima := Pila.Cima^.Siguiente;
    Dispose(Temp);
  end;
  Pila.Count := 0;
end;

procedure GenerarReporteDOTPapelera(Pila: TPila; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PNodoPila;
  Contador: Integer;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph Papelera {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightcoral];');
    WriteLn(Archivo, '  edge [color=darkred, dir=back];'); // dir=back para mostrar pila correctamente
    WriteLn(Archivo, '');

    Contador := 0;
    Actual := Pila.Cima;

    // Mostrar nodo de cima de pila
    WriteLn(Archivo, '  cima [label="CIMA DE PILA", shape=ellipse, fillcolor=orange];');

    while Actual <> nil do
    begin
      if Actual^.Correo <> nil then
      begin
        // Crear nodo para cada correo en la papelera
        WriteLn(Archivo, '  papelera', Contador, ' [label="');
        WriteLn(Archivo, 'ID: ', Actual^.Correo^.Id, '\\n');
        WriteLn(Archivo, 'Remitente: ', Actual^.Correo^.Remitente, '\\n');
        WriteLn(Archivo, 'Asunto: ', Actual^.Correo^.Asunto, '\\n');
        WriteLn(Archivo, 'Fecha: ', Actual^.Correo^.Fecha, '"];');

        // Conectar con el siguiente elemento de la pila
        if Contador = 0 then
        begin
          WriteLn(Archivo, '  cima -> papelera', Contador, ';');
        end
        else
        begin
          WriteLn(Archivo, '  papelera', Contador - 1, ' -> papelera', Contador, ';');
        end;

        Inc(Contador);
      end;
      Actual := Actual^.Siguiente;
    end;

    // Mostrar nodo de base de pila si hay elementos
    if Contador > 0 then
    begin
      WriteLn(Archivo, '  base [label="BASE DE PILA", shape=ellipse, fillcolor=lightgreen];');
      WriteLn(Archivo, '  papelera', Contador - 1, ' -> base;');
    end;

    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

end.
