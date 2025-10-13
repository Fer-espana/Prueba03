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
function BuscarContactoPorEmail(Lista: TListaContactos; Email: string): PContacto; // MOVIDO DESPUÉS DE LOS TIPOS
procedure EliminarContacto(var Lista: TListaContactos; Email: string);
procedure MostrarContactos(Lista: TListaContactos);
procedure LiberarListaContactos(var Lista: TListaContactos);
function ObtenerSiguienteContacto(Lista: TListaContactos; Actual: PContacto): PContacto;
procedure GenerarReporteDOTContactos(Lista: TListaContactos; NombreArchivo: string);

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
  i: Integer;
begin
  Result := nil;
  if Lista.Cabeza = nil then Exit;

  Actual := Lista.Cabeza;
  for i := 0 to Lista.Count - 1 do
  begin
    if Actual^.Email = Email then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
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

procedure GenerarReporteDOTContactos(Lista: TListaContactos; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PContacto;
  Contador, TotalContactos: Integer;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph Contactos {');
    WriteLn(Archivo, '  layout=circo; // Diseño circular');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightgreen];');
    WriteLn(Archivo, '  edge [color=darkgreen];');
    WriteLn(Archivo, '');

    if Lista.Cabeza = nil then
    begin
      WriteLn(Archivo, '  vacio [label="No hay contactos", shape=plaintext];');
    end
    else
    begin
      Contador := 0;
      Actual := Lista.Cabeza;
      TotalContactos := Lista.Count;

      repeat
        // Crear nodo para cada contacto
        WriteLn(Archivo, '  contacto', Contador, ' [label="');
        WriteLn(Archivo, 'ID: ', Actual^.Id, '\\n');
        WriteLn(Archivo, 'Nombre: ', Actual^.Nombre, '\\n');
        WriteLn(Archivo, 'Email: ', Actual^.Email, '\\n');
        WriteLn(Archivo, 'Teléfono: ', Actual^.Telefono, '"];');

        // Conectar con el siguiente contacto (comportamiento circular)
        if Contador < TotalContactos - 1 then
        begin
          WriteLn(Archivo, '  contacto', Contador, ' -> contacto', Contador + 1, ';');
        end;

        Inc(Contador);
        Actual := Actual^.Siguiente;

      until (Actual = Lista.Cabeza) or (Contador >= TotalContactos);

      // Conectar el último con el primero para completar el círculo
      if TotalContactos > 1 then
      begin
        WriteLn(Archivo, '  contacto', TotalContactos - 1, ' -> contacto0;');
      end;

      // Mostrar el nodo cabeza
      WriteLn(Archivo, '  cabeza [label="CABEZA", shape=ellipse, fillcolor=orange];');
      WriteLn(Archivo, '  cabeza -> contacto0;');
    end;

    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

end.
