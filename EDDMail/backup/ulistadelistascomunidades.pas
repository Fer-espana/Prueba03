unit UListadeListasComunidades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UUsuarios;

type
  PComunidad = ^TComunidad;
  TComunidad = record
    Nombre: string;
    Usuarios: PUsuario; // Lista de usuarios en esta comunidad
    Siguiente: PComunidad; // Siguiente comunidad
  end;

// Lista global de comunidades
var
  ListaComunidades: PComunidad = nil;

// Funciones para controlar las acciones de las comunidades
function CrearComunidad(Nombre: string): Boolean;
function AgregarUsuarioAComunidad(NombreComunidad: string; Correo: string): Boolean;
function ExisteComunidad(Nombre: string): Boolean;
function TraerUsuario(Correo: string): PUsuario;
procedure MostrarComunidades;
procedure LiberarComunidades;

implementation

// Función auxiliar para buscar usuario por correo (similar a la de UUsuarios pero devuelve PUsuario)
function TraerUsuario(Correo: string): PUsuario;
var
  Actual: PUsuario;
begin
  Actual := ListaUsuariosGlobal.Cabeza;
  while Actual <> nil do
  begin
    if SameText(Actual^.Email, Correo) then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
  Result := nil;
end;

// Verificar si existe la comunidad
function ExisteComunidad(Nombre: string): Boolean;
var
  Actual: PComunidad;
begin
  Actual := ListaComunidades;
  while Actual <> nil do
  begin
    if SameText(Actual^.Nombre, Nombre) then
    begin
      Result := True;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
  Result := False;
end;

// Crear una nueva comunidad
function CrearComunidad(Nombre: string): Boolean;
var
  Nueva: PComunidad;
begin
  if ExisteComunidad(Nombre) then
  begin
    Result := False; // Ya existe
    Exit;
  end;

  New(Nueva);
  Nueva^.Nombre := Nombre;
  Nueva^.Usuarios := nil;
  Nueva^.Siguiente := ListaComunidades; // Insertar al inicio
  ListaComunidades := Nueva;
  Result := True;
end;

// Agregar usuario a una comunidad existente
function AgregarUsuarioAComunidad(NombreComunidad: string; Correo: string): Boolean;
var
  Comunidad: PComunidad;
  UsuarioGlobal: PUsuario;
  Actual, Nuevo: PUsuario;
begin
  Result := False;

  // Buscar la comunidad
  Comunidad := ListaComunidades;
  while (Comunidad <> nil) and (not SameText(Comunidad^.Nombre, NombreComunidad)) do
    Comunidad := Comunidad^.Siguiente;

  if Comunidad = nil then Exit; // No existe la comunidad

  // Buscar el usuario en la lista global
  UsuarioGlobal := TraerUsuario(Correo);
  if UsuarioGlobal = nil then Exit; // No existe el usuario

  // Verificar si ya está en la comunidad
  Actual := Comunidad^.Usuarios;
  while Actual <> nil do
  begin
    if SameText(Actual^.Email, UsuarioGlobal^.Email) then
      Exit; // Ya existe en la comunidad
    Actual := Actual^.Siguiente;
  end;

  // Crear un nuevo nodo usuario para la comunidad (copia del global)
  New(Nuevo);
  // Copiar datos del usuario global
  Nuevo^.Id := UsuarioGlobal^.Id;
  Nuevo^.Nombre := UsuarioGlobal^.Nombre;
  Nuevo^.Usuario := UsuarioGlobal^.Usuario;
  Nuevo^.Email := UsuarioGlobal^.Email;
  Nuevo^.Telefono := UsuarioGlobal^.Telefono;

  // No copiamos las listas internas para evitar duplicación compleja
  // En su lugar, podrías usar punteros a las mismas listas o manejarlo diferente
  Nuevo^.Siguiente := Comunidad^.Usuarios;

  // Insertar en la comunidad
  Comunidad^.Usuarios := Nuevo;
  Result := True;
end;

// Mostrar todas las comunidades y sus usuarios
procedure MostrarComunidades;
var
  Comunidad: PComunidad;
  Usuario: PUsuario;
  Count: Integer;
begin
  Writeln('=== LISTA DE COMUNIDADES ===');

  Comunidad := ListaComunidades;
  while Comunidad <> nil do
  begin
    Writeln('Comunidad: ', Comunidad^.Nombre);
    Writeln('Usuarios:');

    Usuario := Comunidad^.Usuarios;
    Count := 0;
    while Usuario <> nil do
    begin
      Writeln('  [', Count + 1, '] ', Usuario^.Nombre, ' (', Usuario^.Email, ')');
      Usuario := Usuario^.Siguiente;
      Inc(Count);
    end;

    if Count = 0 then
      Writeln('  (No hay usuarios en esta comunidad)');

    Writeln('---');
    Comunidad := Comunidad^.Siguiente;
  end;

  if ListaComunidades = nil then
    Writeln('No hay comunidades creadas.');
end;

// Liberar toda la memoria de las comunidades
procedure LiberarComunidades;
var
  Comunidad, TempComunidad: PComunidad;
  Usuario, TempUsuario: PUsuario;
begin
  Comunidad := ListaComunidades;
  while Comunidad <> nil do
  begin
    // Liberar lista de usuarios de la comunidad
    Usuario := Comunidad^.Usuarios;
    while Usuario <> nil do
    begin
      TempUsuario := Usuario;
      Usuario := Usuario^.Siguiente;
      Dispose(TempUsuario);
    end;

    TempComunidad := Comunidad;
    Comunidad := Comunidad^.Siguiente;
    Dispose(TempComunidad);
  end;

  ListaComunidades := nil;
end;

// Función adicional: Remover usuario de comunidad
function RemoverUsuarioDeComunidad(NombreComunidad: string; Correo: string): Boolean;
var
  Comunidad: PComunidad;
  Actual, Anterior: PUsuario;
begin
  Result := False;

  // Buscar la comunidad
  Comunidad := ListaComunidades;
  while (Comunidad <> nil) and (not SameText(Comunidad^.Nombre, NombreComunidad)) do
    Comunidad := Comunidad^.Siguiente;

  if Comunidad = nil then Exit; // No existe la comunidad

  // Buscar el usuario en la comunidad
  Actual := Comunidad^.Usuarios;
  Anterior := nil;

  while Actual <> nil do
  begin
    if SameText(Actual^.Email, Correo) then
    begin
      // Encontrado, eliminar de la lista
      if Anterior = nil then
        Comunidad^.Usuarios := Actual^.Siguiente
      else
        Anterior^.Siguiente := Actual^.Siguiente;

      Dispose(Actual);
      Result := True;
      Exit;
    end;

    Anterior := Actual;
    Actual := Actual^.Siguiente;
  end;
end;

// Función adicional: Obtener comunidad por nombre
function ObtenerComunidad(Nombre: string): PComunidad;
var
  Actual: PComunidad;
begin
  Actual := ListaComunidades;
  while Actual <> nil do
  begin
    if SameText(Actual^.Nombre, Nombre) then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
  Result := nil;
end;

end.
