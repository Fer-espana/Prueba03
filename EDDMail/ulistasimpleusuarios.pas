unit uListaSimpleUsuarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Process, LCLIntf, LCLType, Math, FileUtil,
  // Dependencias existentes
  ulistadobleenlazadacorreos, ulistacircularcontactos, upilapapelera,
  ucolacorreosprogramados, uarbolb, uavltreeborradores,
  // *** NUEVA DEPENDENCIA (Fase 3) ***
  privado, // <-- Unidad que define TMerkleTree
  uglobal; // <-- Para DirectorioReportesRoot

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
// *** NUEVA DECLARACIÓN (Fase 3) ***
procedure GenerarReporteGrafo(nombreArchivoBase: String); // Adaptado de referencia
procedure LiberarListaUsuarios(var lista: TListaUsuarios);

implementation

// *** AÑADIR uses SI ES NECESARIO ***
uses StrUtils; // <-- Para AnsiCompareText

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
  Actual := ListaUsuarios.primero;
  Index := 0;

  while Actual <> nil do
  begin
    if Actual^.Email = Email then
      Exit(Index);
    Actual := Actual^.siguiente;
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
  Nuevo^.siguiente := nil;
  Nuevo^.privados := TMerkleTree.Create;

  // Inicializar estructuras existentes
  InicializarListaCorreos(Nuevo^.inbox);
  InicializarListaContactos(Nuevo^.contacts);
  InitPila(Nuevo^.papelera);
  InitCola(Nuevo^.programados);
  InicializarArbolB(Nuevo^.favoritos);
  InicializarArbolAVL(Nuevo^.borradores);

  if Lista.primero = nil then
    Lista.primero := Nuevo
  else
  begin
    Actual := Lista.primero;
    while Actual^.siguiente <> nil do
      Actual := Actual^.siguiente;
    Actual^.siguiente := Nuevo;
  end;

  Inc(Lista.count);
end;

function BuscarUsuarioPorEmail(Lista: TListaUsuarios; Email: string): PUsuario;
var
  Actual: PUsuario;
begin
  Actual := Lista.primero;
  while Actual <> nil do
  begin
    if SameText(Actual^.Email, Email) then
      Exit(Actual);
    Actual := Actual^.siguiente;
  end;
  Result := nil;
end;

procedure MostrarUsuarios(Lista: TListaUsuarios);
var
  Actual: PUsuario;
begin
  Actual := Lista.primero;
  while Actual <> nil do
  begin
    WriteLn('ID: ', Actual^.Id);
    WriteLn('Nombre: ', Actual^.Nombre);
    WriteLn('Usuario: ', Actual^.Usuario);
    WriteLn('Email: ', Actual^.Email);
    WriteLn('Teléfono: ', Actual^.Telefono);
    WriteLn('-------------------');
    Actual := Actual^.siguiente;
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

// *** NUEVA IMPLEMENTACIÓN (Fase 3 - Adaptada de la referencia) ***
procedure GenerarReporteGrafo(nombreArchivoBase: String);
var
  dotFile: TextFile;
  dotFilePath, rutaCarpeta: String;
  currentUser: PUsuario;
  currentContact: PContacto;
  processedEdges: TStringList;
  u1, u2, edgeKey: String;
  nodeIdCounter: Integer;
begin
  // 1. Validar si hay usuarios
  if ListaUsuarios.primero = nil then
  begin
    ShowMessage('No hay usuarios registrados para generar el grafo de contactos.');
    Exit;
  end;

  // 2. Crear directorio y definir ruta del archivo .dot
  rutaCarpeta := DirectorioReportesRoot; // Usa la variable global de uglobal.pas
  if not DirectoryExists(rutaCarpeta) then
    CreateDir(rutaCarpeta);
  dotFilePath := IncludeTrailingPathDelimiter(rutaCarpeta) + nombreArchivoBase + '.dot';

  // 3. Inicializar TStringList para control de aristas duplicadas
  processedEdges := TStringList.Create;
  processedEdges.Sorted := True;

  // 4. Abrir archivo y escribir encabezado DOT
  AssignFile(dotFile, dotFilePath);
  try
    try
      Rewrite(dotFile);
      WriteLn(dotFile, 'graph ContactosGrafo {');
      WriteLn(dotFile, '  rankdir=LR;');
      WriteLn(dotFile, '  node [shape=circle, style=filled];');
      WriteLn(dotFile, '  graph [label="Relaciones de Contactos", labelloc=t, fontname="Arial"];');

      // --- Generar Nodos y Aristas ---
      currentUser := ListaUsuarios.primero;
      nodeIdCounter := 0;

      while currentUser <> nil do
      begin
        Inc(nodeIdCounter);
        // --- A. Generar Nodo para el Usuario Actual ---
        WriteLn(dotFile, Format('  \"%s\" [label=\"ID: %d\nUsuario: %s\", fillcolor=\"lightblue\"];', [
          currentUser^.email,
          currentUser^.id,
          currentUser^.usuario
        ]));

        // --- B. Generar Nodos y Aristas para sus Contactos ---
        if (currentUser^.contacts.primero <> nil) then // Verifica si la lista de contactos no está vacía
        begin
          currentContact := currentUser^.contacts.primero;
          repeat
            // u1: Dueño de la lista (usuario actual)
            u1 := currentUser^.email;
            // u2: El contacto (email del contacto)
            u2 := currentContact^.email;

            // Generar Nodo para el contacto
            WriteLn(dotFile, Format('  \"%s\" [label=\"Contacto: %s\", fillcolor=\"lightgreen\"];', [
               u2, u2
            ]));

            // --- Control de Aristas Duplicadas ---
            // Normalizar clave (menor_mayor) para evitar A--B y B--A
            if AnsiCompareText(u1, u2) < 0 then
              edgeKey := u1 + '_' + u2
            else
              edgeKey := u2 + '_' + u1;

            // Agregar Arista si no existe en processedEdges
            if processedEdges.IndexOf(edgeKey) = -1 then
            begin
              WriteLn(dotFile, Format('  \"%s\" -- \"%s\";', [u1, u2]));
              processedEdges.Add(edgeKey);
            end;

            currentContact := currentContact^.siguiente;
          until currentContact = currentUser^.contacts.primero; // Condición de parada para lista circular
        end;

        currentUser := currentUser^.siguiente;
      end;

      // 5. Cerrar archivo DOT
      WriteLn(dotFile, '}');
    except
      on E: Exception do
      begin
        ShowMessage('Error al generar el archivo .dot del grafo: ' + E.Message);
      end;
    end;
  finally
    CloseFile(dotFile);
    processedEdges.Free;
  end;
end;

// =======================================================
// IMPLEMENTACIONES DE FUNCIONES DE REPORTE/BUSQUEDA
// =======================================================

function ObtenerIDyEmailPorID(Lista: TListaUsuarios; IDUsuario: Integer): string;
var
  Actual: PUsuario;
begin
  Result := 'ID_Invalido_' + IntToStr(IDUsuario);
  Actual := Lista.primero;

  while Actual <> nil do
  begin
    if Actual^.Id = IDUsuario then
    begin
      Result := Actual^.Email + ' (ID: ' + IntToStr(Actual^.Id) + ')';
      Exit;
    end;
    Actual := Actual^.siguiente;
  end;
end;

procedure GenerarGraphvizUsuarios(lista: TListaUsuarios; nombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PUsuario;
begin
  AssignFile(Archivo, nombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph Usuarios {');
    WriteLn(Archivo, '  rankdir=LR; // De izquierda a derecha');
    WriteLn(Archivo, '  node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '  edge [color=darkblue, arrowhead=vee];');
    WriteLn(Archivo, '');

    Actual := lista.primero;

    // 1. Crear nodos y enlaces
    while Actual <> nil do
    begin
      // Crear nodo para cada usuario
      WriteLn(Archivo, '  user_', Actual^.Id, ' [label="{<data> ID: ', Actual^.Id, ' | Nombre: ',
        Actual^.Nombre, ' | Email: ', Actual^.Email, '|<next> Siguiente }"];');

      // Conectar con el siguiente nodo
      if Actual^.siguiente <> nil then
      begin
        WriteLn(Archivo, '  user_', Actual^.Id, ':next -> user_', Actual^.siguiente^.Id, ':data;');
      end;

      Actual := Actual^.siguiente;
    end;

    // 2. Mostrar puntero Cabeza
    if lista.primero <> nil then
    begin
      WriteLn(Archivo, '  cabeza [label="CABEZA", shape=ellipse, fillcolor=orange];');
      WriteLn(Archivo, '  cabeza -> user_', lista.primero^.Id, ':data;');
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

function BuscarUsuario(lista: TListaUsuarios; email: String): PUsuario;
begin
  Result := BuscarUsuarioPorEmail(lista, email);
end;

function BuscarUsuarioPorID(lista: TListaUsuarios; id: Integer): PUsuario;
var
  Actual: PUsuario;
begin
  Actual := lista.primero;
  while Actual <> nil do
  begin
    if Actual^.Id = id then
      Exit(Actual);
    Actual := Actual^.siguiente;
  end;
  Result := nil;
end;

initialization
begin
  InicializarLista(ListaUsuarios);
end;

end.
