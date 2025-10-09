unit UColaCorreosProgramados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCorreos;

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

end.
