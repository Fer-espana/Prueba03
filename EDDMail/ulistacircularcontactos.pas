unit UListaCircularContactos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PContacto = ^TContacto;
  TContacto = record
    Id: Integer;
    Nombre: string;
    Email: string;
    Telefono: string;
    Siguiente: PContacto;
  end;

  TListaContactos = record
    Cabeza: PContacto;
    Count: Integer;
  end;

// Procedimientos básicos
procedure InicializarListaContactos(var Lista: TListaContactos);
procedure InsertarContacto(var Lista: TListaContactos; Id: Integer;
  Nombre, Email, Telefono: string);
function BuscarContactoPorEmail(Lista: TListaContactos; Email: string): PContacto;
procedure EliminarContacto(var Lista: TListaContactos; Email: string);
procedure MostrarContactos(Lista: TListaContactos);
procedure LiberarListaContactos(var Lista: TListaContactos);
function ObtenerSiguienteContacto(Lista: TListaContactos; Actual: PContacto): PContacto;

implementation

procedure InicializarListaContactos(var Lista: TListaContactos);
begin
  Lista.Cabeza := nil;
  Lista.Count := 0;
end;

procedure InsertarContacto(var Lista: TListaContactos; Id: Integer;
  Nombre, Email, Telefono: string);
var
  Nuevo, Actual: PContacto;
begin
  New(Nuevo);
  Nuevo^.Id := Id;
  Nuevo^.Nombre := Nombre;
  Nuevo^.Email := Email;
  Nuevo^.Telefono := Telefono;

  if Lista.Cabeza = nil then
  begin
    Lista.Cabeza := Nuevo;
    Nuevo^.Siguiente := Nuevo;  // Circular: apunta a sí mismo
  end
  else
  begin
    Actual := Lista.Cabeza;
    while Actual^.Siguiente <> Lista.Cabeza do
      Actual := Actual^.Siguiente;

    Actual^.Siguiente := Nuevo;
    Nuevo^.Siguiente := Lista.Cabeza;
  end;

  Inc(Lista.Count);
end;

function BuscarContactoPorEmail(Lista: TListaContactos; Email: string): PContacto;
var
  Actual: PContacto;
begin
  if Lista.Cabeza = nil then Exit(nil);

  Actual := Lista.Cabeza;
  repeat
    if Actual^.Email = Email then
      Exit(Actual);
    Actual := Actual^.Siguiente;
  until Actual = Lista.Cabeza;

  Result := nil;
end;

procedure EliminarContacto(var Lista: TListaContactos; Email: string);
var
  Actual, Anterior: PContacto;
begin
  if Lista.Cabeza = nil then Exit;

  Actual := Lista.Cabeza;
  Anterior := nil;

  // Buscar el contacto a eliminar
  repeat
    if Actual^.Email = Email then
      Break;
    Anterior := Actual;
    Actual := Actual^.Siguiente;
  until Actual = Lista.Cabeza;

  if Actual^.Email <> Email then Exit; // No encontrado

  if Lista.Count = 1 then
  begin
    // Único elemento
    Dispose(Lista.Cabeza);
    Lista.Cabeza := nil;
  end
  else
  begin
    if Actual = Lista.Cabeza then
      Lista.Cabeza := Actual^.Siguiente;

    if Anterior <> nil then
      Anterior^.Siguiente := Actual^.Siguiente;

    // Ajustar el último nodo para mantener circularidad
    if Anterior <> nil then
    begin
      // Encontrar el último nodo y ajustar
      while Anterior^.Siguiente <> Actual do
        Anterior := Anterior^.Siguiente;
    end;

    Dispose(Actual);
  end;

  Dec(Lista.Count);
end;

procedure MostrarContactos(Lista: TListaContactos);
var
  Actual: PContacto;
  i: Integer;
begin
  if Lista.Cabeza = nil then Exit;

  Actual := Lista.Cabeza;
  i := 0;
  repeat
    WriteLn('Contacto ', i + 1);
    WriteLn('ID: ', Actual^.Id);
    WriteLn('Nombre: ', Actual^.Nombre);
    WriteLn('Email: ', Actual^.Email);
    WriteLn('Teléfono: ', Actual^.Telefono);
    WriteLn('-------------------');
    Actual := Actual^.Siguiente;
    Inc(i);
  until (Actual = Lista.Cabeza) or (i >= Lista.Count);
end;

function ObtenerSiguienteContacto(Lista: TListaContactos; Actual: PContacto): PContacto;
begin
  if Lista.Cabeza = nil then
    Result := nil
  else if Actual = nil then
    Result := Lista.Cabeza
  else
    Result := Actual^.Siguiente;
end;

procedure LiberarListaContactos(var Lista: TListaContactos);
var
  Actual, Temp: PContacto;
  i: Integer;
begin
  if Lista.Cabeza = nil then Exit;

  Actual := Lista.Cabeza;
  for i := 0 to Lista.Count - 1 do
  begin
    Temp := Actual;
    Actual := Actual^.Siguiente;
    Dispose(Temp);
  end;
  Lista.Cabeza := nil;
  Lista.Count := 0;
end;

end.
