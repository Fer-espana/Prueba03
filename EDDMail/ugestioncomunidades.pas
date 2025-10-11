unit UGestionComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UGLOBAL, UListadeListasComunidades, UListaSimpleUsuarios;

type

  { TForm14 }

  TForm14 = class(TForm)
    btnCrearComunidad: TButton;
    btnAgregarUsuario: TButton;
    btnRemoverUsuario: TButton;
    btnGenerarReporte: TButton;
    cmbComunidades: TComboBox;
    cmbUsuariosSistema: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    gridUsuariosComunidad: TStringGrid;
    editNuevaComunidad: TEdit;
    procedure btnAgregarUsuarioClick(Sender: TObject);
    procedure btnCrearComunidadClick(Sender: TObject);
    procedure btnGenerarReporteClick(Sender: TObject);
    procedure btnRemoverUsuarioClick(Sender: TObject);
    procedure cmbComunidadesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CargarComunidades;
    procedure CargarUsuariosSistema;
    procedure MostrarUsuariosComunidad(NombreComunidad: string);
    // AGREGAR FUNCIONES FALTANTES
    function ObtenerComunidad(Nombre: string): PComunidad;
    function RemoverUsuarioDeComunidad(NombreComunidad: string; Correo: string): Boolean;
    procedure GenerarReporteGraphvizComunidades(NombreArchivo: string);
  public
  end;

var
  Form14: TForm14;

implementation

{$R *.lfm}

{ TForm14 }

procedure TForm14.FormCreate(Sender: TObject);
begin
  Caption := 'Gestión de Comunidades - Root';

  // Configurar grid
  gridUsuariosComunidad.ColCount := 3;
  gridUsuariosComunidad.RowCount := 1;

  // INICIALIZAR CABECERAS DEL GRID EN CÓDIGO
  gridUsuariosComunidad.Cells[0, 0] := 'ID';
  gridUsuariosComunidad.Cells[1, 0] := 'Nombre';
  gridUsuariosComunidad.Cells[2, 0] := 'Email';

  // Configurar anchos de columnas
  gridUsuariosComunidad.ColWidths[0] := 50;
  gridUsuariosComunidad.ColWidths[1] := 200;
  gridUsuariosComunidad.ColWidths[2] := 250;
end;

procedure TForm14.FormShow(Sender: TObject);
begin
  CargarComunidades;
  CargarUsuariosSistema;
end;

procedure TForm14.CargarComunidades;
var
  Actual: PComunidad;
begin
  cmbComunidades.Clear;
  Actual := ListaComunidades;

  while Actual <> nil do
  begin
    cmbComunidades.Items.Add(Actual^.Nombre);
    Actual := Actual^.Siguiente;
  end;

  if cmbComunidades.Items.Count > 0 then
    cmbComunidades.ItemIndex := 0;
end;

procedure TForm14.CargarUsuariosSistema;
var
  Actual: PUsuario;
begin
  cmbUsuariosSistema.Clear;
  Actual := ListaUsuariosGlobal.Cabeza;

  while Actual <> nil do
  begin
    // No incluir al usuario root
    if Actual^.Email <> 'root@edd.com' then
      cmbUsuariosSistema.Items.Add(Actual^.Email);
    Actual := Actual^.Siguiente;
  end;

  if cmbUsuariosSistema.Items.Count > 0 then
    cmbUsuariosSistema.ItemIndex := 0;
end;

// IMPLEMENTAR FUNCIÓN ObtenerComunidad
function TForm14.ObtenerComunidad(Nombre: string): PComunidad;
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

procedure TForm14.MostrarUsuariosComunidad(NombreComunidad: string);
var
  Comunidad: PComunidad;
  Usuario: PUsuario;
  Fila: Integer;
begin
  gridUsuariosComunidad.RowCount := 1; // Limpiar grid

  Comunidad := ObtenerComunidad(NombreComunidad);
  if Comunidad = nil then Exit;

  Usuario := Comunidad^.Usuarios;
  Fila := 1;

  while Usuario <> nil do
  begin
    gridUsuariosComunidad.RowCount := gridUsuariosComunidad.RowCount + 1;
    gridUsuariosComunidad.Cells[0, Fila] := IntToStr(Usuario^.Id);
    gridUsuariosComunidad.Cells[1, Fila] := Usuario^.Nombre;
    gridUsuariosComunidad.Cells[2, Fila] := Usuario^.Email;

    Usuario := Usuario^.Siguiente;
    Inc(Fila);
  end;
end;

procedure TForm14.btnCrearComunidadClick(Sender: TObject);
var
  NombreComunidad: string;
begin
  NombreComunidad := Trim(editNuevaComunidad.Text);

  if NombreComunidad = '' then
  begin
    ShowMessage('Ingrese un nombre para la comunidad');
    Exit;
  end;

  if CrearComunidad(NombreComunidad) then
  begin
    ShowMessage('Comunidad "' + NombreComunidad + '" creada exitosamente');
    editNuevaComunidad.Text := '';
    CargarComunidades;
  end
  else
  begin
    ShowMessage('Error: La comunidad "' + NombreComunidad + '" ya existe');
  end;
end;

procedure TForm14.btnAgregarUsuarioClick(Sender: TObject);
var
  Comunidad, Usuario: string;
begin
  if cmbComunidades.ItemIndex = -1 then
  begin
    ShowMessage('Seleccione una comunidad');
    Exit;
  end;

  if cmbUsuariosSistema.ItemIndex = -1 then
  begin
    ShowMessage('Seleccione un usuario');
    Exit;
  end;

  Comunidad := cmbComunidades.Text;
  Usuario := cmbUsuariosSistema.Text;

  if AgregarUsuarioAComunidad(Comunidad, Usuario) then
  begin
    ShowMessage('Usuario agregado a la comunidad exitosamente');
    MostrarUsuariosComunidad(Comunidad);
  end
  else
  begin
    ShowMessage('Error: No se pudo agregar el usuario a la comunidad');
  end;
end;

procedure TForm14.cmbComunidadesChange(Sender: TObject);
begin
  if cmbComunidades.ItemIndex >= 0 then
    MostrarUsuariosComunidad(cmbComunidades.Text);
end;

// IMPLEMENTAR FUNCIÓN RemoverUsuarioDeComunidad
function TForm14.RemoverUsuarioDeComunidad(NombreComunidad: string; Correo: string): Boolean;
var
  Comunidad: PComunidad;
  Actual, Anterior: PUsuario;
begin
  Result := False;

  // Buscar la comunidad
  Comunidad := ObtenerComunidad(NombreComunidad);
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

procedure TForm14.btnRemoverUsuarioClick(Sender: TObject);
var
  Comunidad, Usuario: string;
  FilaSeleccionada: Integer;
begin
  if cmbComunidades.ItemIndex = -1 then
  begin
    ShowMessage('Seleccione una comunidad');
    Exit;
  end;

  FilaSeleccionada := gridUsuariosComunidad.Row;
  if FilaSeleccionada <= 0 then
  begin
    ShowMessage('Seleccione un usuario de la lista');
    Exit;
  end;

  Comunidad := cmbComunidades.Text;
  Usuario := gridUsuariosComunidad.Cells[2, FilaSeleccionada]; // Email en columna 2

  if RemoverUsuarioDeComunidad(Comunidad, Usuario) then
  begin
    ShowMessage('Usuario removido de la comunidad exitosamente');
    MostrarUsuariosComunidad(Comunidad);
  end
  else
  begin
    ShowMessage('Error: No se pudo remover el usuario de la comunidad');
  end;
end;

// IMPLEMENTAR FUNCIÓN GenerarReporteGraphvizComunidades
procedure TForm14.GenerarReporteGraphvizComunidades(NombreArchivo: string);
var
  Archivo: TextFile;
  Comunidad: PComunidad;
  Usuario: PUsuario;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph Comunidades {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=box, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '  edge [color=darkgreen];');
    WriteLn(Archivo, '');

    // Nodos de comunidades
    Comunidad := ListaComunidades;
    while Comunidad <> nil do
    begin
      WriteLn(Archivo, '  "', Comunidad^.Nombre, '" [shape=ellipse, fillcolor=orange];');

      // Nodos de usuarios y conexiones
      Usuario := Comunidad^.Usuarios;
      while Usuario <> nil do
      begin
        WriteLn(Archivo, '  "', Usuario^.Email, '" [label="', Usuario^.Nombre, '\n', Usuario^.Email, '"];');
        WriteLn(Archivo, '  "', Comunidad^.Nombre, '" -> "', Usuario^.Email, '";');
        Usuario := Usuario^.Siguiente;
      end;

      Comunidad := Comunidad^.Siguiente;
      WriteLn(Archivo, '');
    end;

    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

procedure TForm14.btnGenerarReporteClick(Sender: TObject);
var
  RutaArchivo: string;
begin
  RutaArchivo := ExtractFilePath(Application.ExeName) + 'Root-Reportes' + PathDelim + 'comunidades.dot';

  // Crear directorio si no existe
  ForceDirectories(ExtractFilePath(RutaArchivo));

  GenerarReporteGraphvizComunidades(RutaArchivo);
  ShowMessage('Reporte Graphviz generado: ' + RutaArchivo + sLineBreak +
              'Ejecute: dot -Tpng ' + RutaArchivo + ' -o comunidades.png');
end;

end.
