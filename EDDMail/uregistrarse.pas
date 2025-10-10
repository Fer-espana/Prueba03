unit URegistrarse;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaSimpleUsuarios;

type

  { TForm8 }

  TForm8 = class(TForm)
    btnRegistrar: TButton;
    editPassword: TEdit;
    editTelefono: TEdit;
    editUsuario: TEdit;
    editEmail: TEdit;
    editNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnRegistrarClick(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure editNombreChange(Sender: TObject);
    procedure editPasswordChange(Sender: TObject);
    procedure editTelefonoChange(Sender: TObject);
    procedure editUsuarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function ValidarCampos: Boolean;
    function GenerarNuevoId: Integer;
    procedure GuardarListaUsuariosEnJSON;
  public

  end;

var
  Form8: TForm8;

implementation

{$R *.lfm}

{ TForm8 }

procedure TForm8.FormCreate(Sender: TObject);
begin
  Caption := 'Registro de Usuario';
  // Configurar placeholders
  editNombre.TextHint := 'Ingrese nombre completo';
  editUsuario.TextHint := 'Ingrese nombre de usuario';
  editEmail.TextHint := 'ejemplo@edd.com';
  editTelefono.TextHint := '12345678';
  editPassword.TextHint := 'Contraseña segura';
end;

// =============================================================================
// AGREGAR ESTE PROCEDIMIENTO QUE FALTABA
// =============================================================================
procedure TForm8.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario para evitar errores
end;

// =============================================================================
// EVENTOS DE CAMBIO EN EDITs - SOLO PARA EVITAR ERRORES
// =============================================================================

procedure TForm8.editNombreChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm8.editUsuarioChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm8.editEmailChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm8.editTelefonoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm8.editPasswordChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

// =============================================================================
// LÓGICA PRINCIPAL
// =============================================================================

function TForm8.ValidarCampos: Boolean;
begin
  Result := False;

  if Trim(editNombre.Text) = '' then
  begin
    ShowMessage('El nombre es obligatorio');
    editNombre.SetFocus;
    Exit;
  end;

  if Trim(editUsuario.Text) = '' then
  begin
    ShowMessage('El usuario es obligatorio');
    editUsuario.SetFocus;
    Exit;
  end;

  if Trim(editEmail.Text) = '' then
  begin
    ShowMessage('El email es obligatorio');
    editEmail.SetFocus;
    Exit;
  end;

  if Pos('@', editEmail.Text) = 0 then
  begin
    ShowMessage('El email debe contener @');
    editEmail.SetFocus;
    Exit;
  end;

  if Trim(editPassword.Text) = '' then
  begin
    ShowMessage('La contraseña es obligatoria');
    editPassword.SetFocus;
    Exit;
  end;

  if Length(editPassword.Text) < 6 then
  begin
    ShowMessage('La contraseña debe tener al menos 6 caracteres');
    editPassword.SetFocus;
    Exit;
  end;

  Result := True;
end;

function TForm8.GenerarNuevoId: Integer;
var
  Actual: PUsuario;
  MaxId: Integer;
begin
  MaxId := 0;
  Actual := ListaUsuariosGlobal.Cabeza;

  while Actual <> nil do
  begin
    if Actual^.Id > MaxId then
      MaxId := Actual^.Id;
    Actual := Actual^.Siguiente;
  end;

  Result := MaxId + 1;
end;

procedure TForm8.GuardarListaUsuariosEnJSON;
var
  Archivo: TextFile;
  Actual: PUsuario;
  RutaArchivo, RutaCarpeta: string;
  EsPrimerUsuario: Boolean;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Data';
  RutaArchivo := RutaCarpeta + PathDelim + 'usuarios.json';

  // Crear carpeta Data si no existe
  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  // Crear o sobrescribir archivo
  AssignFile(Archivo, RutaArchivo);
  try
    Rewrite(Archivo);

    // Escribir encabezado JSON
    WriteLn(Archivo, '{');
    WriteLn(Archivo, '  "usuarios": [');

    // Escribir usuarios
    Actual := ListaUsuariosGlobal.Cabeza;
    EsPrimerUsuario := True;

    while Actual <> nil do
    begin
      if not EsPrimerUsuario then
        WriteLn(Archivo, ',');

      WriteLn(Archivo, '    {');
      WriteLn(Archivo, '      "id": ', Actual^.Id, ',');
      WriteLn(Archivo, '      "nombre": "', Actual^.Nombre, '",');
      WriteLn(Archivo, '      "usuario": "', Actual^.Usuario, '",');
      WriteLn(Archivo, '      "email": "', Actual^.Email, '",');
      WriteLn(Archivo, '      "telefono": "', Actual^.Telefono, '"');
      Write(Archivo, '    }');

      EsPrimerUsuario := False;
      Actual := Actual^.Siguiente;
    end;

    // Escribir cierre JSON
    WriteLn(Archivo);
    WriteLn(Archivo, '  ]');
    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;

  ShowMessage('Usuarios guardados exitosamente en: ' + RutaArchivo);
end;

// En URegistrarse.pas, en el procedimiento btnRegistrarClick, agregar:
procedure TForm8.btnRegistrarClick(Sender: TObject);
var
  NuevoId: Integer;
  UsuarioExistente: PUsuario;
begin
  // Validar campos
  if not ValidarCampos then
    Exit;

  // Verificar si el email ya existe
  UsuarioExistente := BuscarUsuarioPorEmail(ListaUsuariosGlobal, Trim(editEmail.Text));
  if UsuarioExistente <> nil then
  begin
    ShowMessage('Error: El email ' + editEmail.Text + ' ya está registrado');
    editEmail.SetFocus;
    Exit;
  end;

  // Generar nuevo ID
  NuevoId := GenerarNuevoId;

  // Insertar nuevo usuario en la lista global
  InsertarUsuario(ListaUsuariosGlobal, NuevoId,
    Trim(editNombre.Text),
    Trim(editUsuario.Text),
    Trim(editEmail.Text),
    Trim(editTelefono.Text));

  // Guardar en archivo JSON
  GuardarListaUsuariosEnJSON;

  ShowMessage('¡Usuario registrado exitosamente!' + sLineBreak +
              'ID: ' + IntToStr(NuevoId) + sLineBreak +
              'Nombre: ' + editNombre.Text + sLineBreak +
              'Email: ' + editEmail.Text);

  // Limpiar campos para nuevo registro
  editNombre.Text := '';
  editUsuario.Text := '';
  editEmail.Text := '';
  editTelefono.Text := '';
  editPassword.Text := '';

  // Cerrar formulario
  Close;
end;

end.
