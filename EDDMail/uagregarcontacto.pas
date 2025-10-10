unit UAgregarContacto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UGLOBAL, UListaSimpleUsuarios, UListaCircularContactos;

type

  { TForm6 }

  TForm6 = class(TForm)
    btnAgregarContacto: TButton;
    editEmail: TEdit;
    Label1: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    function ValidarCampos: Boolean;
  public
    { public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.lfm}

{ TForm6 }

procedure TForm6.FormCreate(Sender: TObject);
begin
  Caption := 'Agregar Contacto';
  // Los labels y botones ya están configurados en el .lfm
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm6.editEmailChange(Sender: TObject);
begin
  // Evento vacío pero necesario para el OnChange del editEmail
end;

function TForm6.ValidarCampos: Boolean;
var
  Email: string;
  UsuarioContacto: PUsuario;
  BandejaUsuario: PBandejaUsuario;
  ContactoExistente: PContacto;
begin
  Result := False;
  Email := Trim(editEmail.Text);

  // Validar campo vacío
  if Email = '' then
  begin
    ShowMessage('Debe ingresar un correo electrónico');
    editEmail.SetFocus;
    Exit;
  end;

  // Validar formato de email básico
  if Pos('@', Email) = 0 then
  begin
    ShowMessage('El correo debe contener @');
    editEmail.SetFocus;
    Exit;
  end;

  // Validar que el usuario exista en el sistema
  UsuarioContacto := BuscarUsuarioPorEmail(ListaUsuariosGlobal, Email);
  if UsuarioContacto = nil then
  begin
    ShowMessage('El usuario no existe en el sistema');
    editEmail.SetFocus;
    Exit;
  end;

  // Validar que no sea el mismo usuario
  if (UsuarioActual <> nil) and (UsuarioActual^.Email = Email) then
  begin
    ShowMessage('No puede agregarse a sí mismo como contacto');
    editEmail.SetFocus;
    Exit;
  end;

  // Validar que el contacto no esté ya en la lista
  if UsuarioActual <> nil then
  begin
    BandejaUsuario := ObtenerBandejaUsuario(UsuarioActual^.Email);
    if BandejaUsuario <> nil then
    begin
      ContactoExistente := BuscarContactoPorEmail(BandejaUsuario^.Contactos, Email);
      if ContactoExistente <> nil then
      begin
        ShowMessage('El contacto ya existe en su lista');
        editEmail.SetFocus;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

procedure TForm6.btnAgregarContactoClick(Sender: TObject);
var
  Email: string;
  BandejaUsuario: PBandejaUsuario;
  UsuarioContacto: PUsuario;
  NuevoId: Integer;
begin
  if not ValidarCampos then
    Exit;

  Email := Trim(editEmail.Text);

  // Obtener o crear bandeja del usuario actual
  if UsuarioActual <> nil then
  begin
    BandejaUsuario := ObtenerBandejaUsuario(UsuarioActual^.Email);
    if BandejaUsuario = nil then
      BandejaUsuario := CrearBandejaUsuario(UsuarioActual^.Email);

    // Buscar información del usuario contacto
    UsuarioContacto := BuscarUsuarioPorEmail(ListaUsuariosGlobal, Email);

    if UsuarioContacto <> nil then
    begin
      // Generar nuevo ID para el contacto
      NuevoId := BandejaUsuario^.Contactos.Count + 1;

      // Insertar contacto en la lista circular
      InsertarContacto(BandejaUsuario^.Contactos, NuevoId,
        UsuarioContacto^.Nombre, Email, UsuarioContacto^.Telefono);

      ShowMessage('Contacto agregado exitosamente:' + sLineBreak +
                  'Nombre: ' + UsuarioContacto^.Nombre + sLineBreak +
                  'Email: ' + Email);

      // Limpiar campo
      editEmail.Text := '';
      editEmail.SetFocus;
    end;
  end
  else
  begin
    ShowMessage('Error: No hay usuario actual');
  end;
end;

end.
