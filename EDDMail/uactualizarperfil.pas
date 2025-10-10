unit UActualizarPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UGLOBAL, UListaSimpleUsuarios;

type

  { TForm7 }

  TForm7 = class(TForm)
    btnActualizar: TButton;
    editTelefono: TEdit;
    editEmail: TEdit;
    editUsuario: TEdit;
    editNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnActualizarClick(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure editNombreChange(Sender: TObject);
    procedure editTelefonoChange(Sender: TObject);
    procedure editUsuarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    function ValidarCampos: Boolean;
    procedure CargarDatosActuales;
  public
    { public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.lfm}

{ TForm7 }

procedure TForm7.FormCreate(Sender: TObject);
begin
  Caption := 'Actualizar Perfil';
  // Los labels y botones ya están configurados en el .lfm
end;

procedure TForm7.FormShow(Sender: TObject);
begin
  CargarDatosActuales;
  editNombre.SetFocus;
end;

procedure TForm7.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm7.CargarDatosActuales;
begin
  if UsuarioActual <> nil then
  begin
    editNombre.Text := UsuarioActual^.Nombre;
    editUsuario.Text := UsuarioActual^.Usuario;
    editEmail.Text := UsuarioActual^.Email;
    editTelefono.Text := UsuarioActual^.Telefono;

    // Hacer el campo email de solo lectura ya que es el identificador único
    editEmail.ReadOnly := True;
    editEmail.Color := clBtnFace; // Color de fondo gris para indicar que es solo lectura
  end;
end;

function TForm7.ValidarCampos: Boolean;
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
    ShowMessage('El correo es obligatorio');
    editEmail.SetFocus;
    Exit;
  end;

  if Trim(editTelefono.Text) = '' then
  begin
    ShowMessage('El teléfono es obligatorio');
    editTelefono.SetFocus;
    Exit;
  end;

  // Validar formato de email básico
  if Pos('@', editEmail.Text) = 0 then
  begin
    ShowMessage('El correo debe contener @');
    editEmail.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TForm7.btnActualizarClick(Sender: TObject);
begin
  if not ValidarCampos then
    Exit;

  if UsuarioActual <> nil then
  begin
    // Actualizar los datos del usuario (excepto el email que es el identificador)
    UsuarioActual^.Nombre := Trim(editNombre.Text);
    UsuarioActual^.Usuario := Trim(editUsuario.Text);
    UsuarioActual^.Telefono := Trim(editTelefono.Text);
    // El email no se actualiza porque es el identificador único

    ShowMessage('Perfil actualizado exitosamente:' + sLineBreak +
                'Nombre: ' + UsuarioActual^.Nombre + sLineBreak +
                'Usuario: ' + UsuarioActual^.Usuario + sLineBreak +
                'Correo: ' + UsuarioActual^.Email + sLineBreak +
                'Teléfono: ' + UsuarioActual^.Telefono);

    // Cerrar formulario
    Close;
  end
  else
  begin
    ShowMessage('Error: No hay usuario actual');
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS
// =============================================================================

procedure TForm7.editNombreChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm7.editUsuarioChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm7.editEmailChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm7.editTelefonoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
