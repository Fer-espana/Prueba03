unit UCorreos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PCorreo = ^TCorreo;
  TCorreo = record
    Id: Integer;
    Remitente: string;
    Estado: Char;  // 'L'=Leído, 'N'=No leído, 'E'=Eliminado
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
procedure InsertarCorreo(var Lista: TListaCorreos; Id: Integer; Remitente: string;
  Estado: Char; Programado: Boolean; Asunto, Fecha, Mensaje: string);
function BuscarCorreoPorId(Lista: TListaCorreos; Id: Integer): PCorreo;
procedure EliminarCorreo(var Lista: TListaCorreos; Id: Integer);
procedure MostrarCorreos(Lista: TListaCorreos);
procedure LiberarListaCorreos(var Lista: TListaCorreos);
function ObtenerCorreoPorPosicion(Lista: TListaCorreos; Posicion: Integer): PCorreo;

implementation

procedure InicializarListaCorreos(var Lista: TListaCorreos);
begin
  Lista.Cabeza := nil;
  Lista.Cola := nil;
  Lista.Count := 0;
end;

procedure InsertarCorreo(var Lista: TListaCorreos; Id: Integer; Remitente: string;
  Estado: Char; Programado: Boolean; Asunto, Fecha, Mensaje: string);
var
  Nuevo: PCorreo;
begin
  New(Nuevo);
  Nuevo^.Id := Id;
  Nuevo^.Remitente := Remitente;
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

end.
