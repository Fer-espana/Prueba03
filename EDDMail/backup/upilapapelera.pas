unit UPilaPapelera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCorreos;

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
    Pila.Cima := Pila.Cima^.Siguiente;
    Dispose(Temp);
  end;
  Pila.Count := 0;
end;

end.
