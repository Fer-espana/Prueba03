unit UListaSimpleUsuarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UAVLTreeBorradores, UArbolB;

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
  Nombre, Usuario, Email, Telefono, Password: string);
function BuscarUsuarioPorEmail(Lista: TListaUsuarios; Email: string): PUsuario;
procedure MostrarUsuarios(Lista: TListaUsuarios);
procedure LiberarListaUsuarios(var Lista: TListaUsuarios);

// NUEVAS DECLARACIONES
procedure GenerarReporteDOTUsuarios(Lista: TListaUsuarios; NombreArchivo: string);
function ObtenerIDyEmailPorID(Lista: TListaUsuarios; IDUsuario: Integer): string;

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

// =======================================================
// IMPLEMENTACIONES DE FUNCIONES DE REPORTE/BUSQUEDA (MOVIDAS ANTES DE initialization)
// =======================================================

function ObtenerIDyEmailPorID(Lista: TListaUsuarios; IDUsuario: Integer): string;
var
  Actual: PUsuario;
begin
  // Valor por defecto si no se encuentra
  Result := 'ID_Invalido_' + IntToStr(IDUsuario);

  Actual := Lista.Cabeza;

  while Actual <> nil do
  begin
    if Actual^.Id = IDUsuario then
    begin
      // Retorna el Email y el ID formateados para el reporte
      Result := Actual^.Email + ' (ID: ' + IntToStr(Actual^.Id) + ')';
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
end;

procedure GenerarReporteDOTUsuarios(Lista: TListaUsuarios; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PUsuario;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph Usuarios {');
    WriteLn(Archivo, '  rankdir=LR; // De izquierda a derecha');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '  edge [color=darkblue, arrowhead=vee];');
    WriteLn(Archivo, '');

    Actual := Lista.Cabeza;

    // 1. Crear nodos y enlaces
    while Actual <> nil do
    begin
      // Crear nodo para cada usuario
      WriteLn(Archivo, '  user_', Actual^.Id, ' [label="{<data> ID: ', Actual^.Id, ' | Nombre: ',
        Actual^.Nombre, ' | Email: ', Actual^.Email, '|<next> Siguiente }"];');

      // Conectar con el siguiente nodo
      if Actual^.Siguiente <> nil then
      begin
        WriteLn(Archivo, '  user_', Actual^.Id, ':next -> user_', Actual^.Siguiente^.Id, ':data;');
      end;

      Actual := Actual^.Siguiente;
    end;

    // 2. Mostrar puntero Cabeza
    if Lista.Cabeza <> nil then
    begin
      WriteLn(Archivo, '  cabeza [label="CABEZA", shape=ellipse, fillcolor=orange];');
      WriteLn(Archivo, '  cabeza -> user_', Lista.Cabeza^.Id, ':data;');
    end
    else
    begin
      WriteLn(Archivo, '  vacio [label="Lista de Usuarios Vacia", shape=plaintext];');
    end;

    // Pie del archivo DOT
    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;


initialization
begin
  InicializarListaUsuarios(ListaUsuariosGlobal);
end;

end.
