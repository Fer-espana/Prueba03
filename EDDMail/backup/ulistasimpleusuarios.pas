unit UListaSimpleUsuarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PUsuario = ^TUsuario;
  TUsuario = record
    Id: Integer;
    Nombre: string;
    Usuario: string;
    Email: string;
    Telefono: string;
    Password: string; // NUEVO CAMPO
    Siguiente: PUsuario;
  end;

  TListaUsuarios = record
    Cabeza: PUsuario;
    Count: Integer;
  end;

var
  ListaUsuariosGlobal: TListaUsuarios;

// Procedimientos básicos
procedure InicializarListaUsuarios(var Lista: TListaUsuarios);
procedure InsertarUsuario(var Lista: TListaUsuarios; Id: Integer;
  Nombre, Usuario, Email, Telefono: string);
function BuscarUsuarioPorEmail(Lista: TListaUsuarios; Email: string): PUsuario;
procedure MostrarUsuarios(Lista: TListaUsuarios);
procedure LiberarListaUsuarios(var Lista: TListaUsuarios);
// ELIMINAR: procedure CargarUsuariosDesdeJSON(var Lista: TListaUsuarios; NombreArchivo: string);

implementation

procedure InicializarListaUsuarios(var Lista: TListaUsuarios);
begin
  Lista.Cabeza := nil;
  Lista.Count := 0;
end;

// En UListaSimpleUsuarios.pas - AGREGAR EN LA SECCIÓN implementation
function BuscarIndiceUsuario(Email: string): Integer;
var
  Actual: PUsuario;
  Index: Integer;
begin
  Actual := ListaUsuariosGlobal.Cabeza;
  Index := 0;

  while Actual <> nil do
  begin
    if Actual^.Email = Email then
      Exit(Index);
    Actual := Actual^.Siguiente;
    Inc(Index);
  end;

  Result := -1; // No encontrado
end;

procedure InsertarUsuario(var Lista: TListaUsuarios; Id: Integer;
  Nombre, Usuario, Email, Telefono, Password: string);
var
  Nuevo, Actual: PUsuario;
begin
  New(Nuevo);
  Nuevo^.Id := Id;
  Nuevo^.Nombre := Nombre;
  Nuevo^.Usuario := Usuario;
  Nuevo^.Email := Email;
  Nuevo^.Telefono := Telefono;
  Nuevo^.Password := Password; // GUARDAR CONTRASEÑA
  Nuevo^.Siguiente := nil;

  if Lista.Cabeza = nil then
    Lista.Cabeza := Nuevo
  else
  begin
    Actual := Lista.Cabeza;
    while Actual^.Siguiente <> nil do
      Actual := Actual^.Siguiente;
    Actual^.Siguiente := Nuevo;
  end;

  Inc(Lista.Count);
end;

function BuscarUsuarioPorEmail(Lista: TListaUsuarios; Email: string): PUsuario;
var
  Actual: PUsuario;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    if SameText(Actual^.Email, Email) then
      Exit(Actual);
    Actual := Actual^.Siguiente;
  end;
  Result := nil;
end;

procedure MostrarUsuarios(Lista: TListaUsuarios);
var
  Actual: PUsuario;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    WriteLn('ID: ', Actual^.Id);
    WriteLn('Nombre: ', Actual^.Nombre);
    WriteLn('Usuario: ', Actual^.Usuario);
    WriteLn('Email: ', Actual^.Email);
    WriteLn('Teléfono: ', Actual^.Telefono);
    WriteLn('-------------------');
    Actual := Actual^.Siguiente;
  end;
end;

procedure LiberarListaUsuarios(var Lista: TListaUsuarios);
var
  Actual, Temp: PUsuario;
begin
  Actual := Lista.Cabeza;
  while Actual <> nil do
  begin
    Temp := Actual;
    Actual := Actual^.Siguiente;
    Dispose(Temp);
  end;
  Lista.Cabeza := nil;
  Lista.Count := 0;
end;

initialization
begin
  InicializarListaUsuarios(ListaUsuariosGlobal);
end;

end.
