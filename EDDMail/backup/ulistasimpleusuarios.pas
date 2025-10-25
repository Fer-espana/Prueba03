unit ulistasimpleusuarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Process, LCLIntf, LCLType, Math, FileUtil,
  // Dependencias existentes
  ulistadobleenlazadacorreos, ulistacircularcontactos, upilapapelera,
  ucolacorreosprogramados, uarbolb, uavltreeborradores,
  // *** NUEVA DEPENDENCIA (Fase 3) ***
  privado; // <-- Unidad que define TMerkleTree

type
  { TUsuario }
  PUsuario = ^TUsuario;
  TUsuario = record
    id: Integer;
    nombre: String;
    usuario: String;
    password: String;
    email: String; // Clave principal para búsquedas
    telefono: String;
    // Estructuras existentes
    inbox: TListaCorreos;
    contacts: TListaContactos;
    papelera: TPila;
    programados: TCola;
    favoritos: TArbolB;
    borradores: TArbolAVL;
    // *** NUEVA ESTRUCTURA (Fase 3) ***
    privados: TMerkleTree; // <-- Campo para el Árbol Merkle
    // Puntero lista simple
    siguiente: PUsuario;
  end;

  { TListaUsuarios }
  TListaUsuarios = record
    primero: PUsuario;
    count: Integer;
  end;

// Variables globales (si las mueves a uglobal, quítalas de aquí)
var
  ListaUsuarios: TListaUsuarios;
  UsuarioLogueado: PUsuario = nil; // Usuario actualmente logueado

// Procedimientos y Funciones existentes...
procedure InicializarLista(var lista: TListaUsuarios);
procedure AgregarUsuario(var lista: TListaUsuarios; id: Integer; nombre, usuario, password, email, telefono: String);
function BuscarUsuario(lista: TListaUsuarios; email: String): PUsuario;
function BuscarUsuarioPorID(lista: TListaUsuarios; id: Integer): PUsuario;
procedure GenerarGraphvizUsuarios(lista: TListaUsuarios; nombreArchivo: string);
// *** NUEVA FUNCIÓN (Fase 3 - La copiaremos en Paso 7) ***
// procedure GenerarGrafoContactos(var lista: TListaUsuarios; nombreArchivoBase: String);
procedure LiberarListaUsuarios(var lista: TListaUsuarios); // <-- IMPORTANTE: Modificarla para liberar MerkleTree

implementation

procedure InicializarLista(var lista: TListaUsuarios);
begin
  lista.primero := nil;
  lista.count := 0;
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
  nuevo^.privados := TMerkleTree.Create;

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

procedure LiberarListaUsuarios(var lista: TListaUsuarios);
var
  actual, siguiente: PUsuario;
begin
  actual := lista.primero;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;

    // Liberar estructuras internas
    LiberarListaCorreos(actual^.inbox);
    LiberarListaContactos(actual^.contacts); // Asegúrate de tener esta función
    LiberarPila(actual^.papelera); // Asegúrate de tener esta función
    LiberarCola(actual^.programados); // Asegúrate de tener esta función
    LiberarArbolB(actual^.favoritos); // Asegúrate de tener esta función
    LiberarArbolAVL(actual^.borradores); // Asegúrate de tener esta función

    // *** NUEVA LIBERACIÓN (Fase 3) ***
    actual^.privados.Free; // <-- Liberar el objeto MerkleTree

    Dispose(actual); // Liberar el nodo de usuario
    actual := siguiente;
  end;
  lista.primero := nil;
  lista.count := 0;
end;

procedure AgregarUsuario(var lista: TListaUsuarios; id: Integer; nombre, usuario, password, email, telefono: String);
var
  nuevo, actual: PUsuario;
begin
  // (Validaciones existentes de ID y Email...)

  New(nuevo);
  nuevo^.id := id;
  nuevo^.nombre := nombre;
  nuevo^.usuario := usuario;
  nuevo^.password := password;
  nuevo^.email := email;
  nuevo^.telefono := telefono;

  // Inicializar estructuras existentes
  InicializarListaCorreos(nuevo^.inbox);
  InicializarListaContactos(nuevo^.contacts);
  InitPila(nuevo^.papelera);
  InitCola(nuevo^.programados);
  InicializarArbolB(nuevo^.favoritos);
  InicializarArbolAVL(nuevo^.borradores);

  // *** NUEVA INICIALIZACIÓN (Fase 3) ***
  nuevo^.privados := TMerkleTree.Create; // <-- Inicializar el Árbol Merkle

  nuevo^.siguiente := nil;

  // Insertar en la lista simple (al final o al principio, como lo tengas)
  if lista.primero = nil then
  begin
    lista.primero := nuevo;
  end
  else
  begin
    actual := lista.primero;
    while actual^.siguiente <> nil do
      actual := actual^.siguiente;
    actual^.siguiente := nuevo;
  end;
  Inc(lista.count);
end;



// *** PEGAR AQUÍ LA FUNCIÓN GenerarGrafoContactos (Paso 7) ***

// =======================================================
// IMPLEMENTACIONES DE FUNCIONES DE REPORTE/BUSQUEDA
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
