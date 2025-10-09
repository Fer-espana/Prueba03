unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UListaSimpleUsuarios;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLogin: TButton;
    btnRegistrar: TButton;
    Button1: TButton;
    Button2: TButton;
    edtEmail: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    procedure btnLoginClick(Sender: TObject);
    procedure btnRegistrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ListaUsuarios: TListaUsuarios;
  public

  end;

var
  Form1: TForm1;

const
  ROOT_EMAIL = 'root@edd.com';
  ROOT_PASSWORD = 'root123';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Login';  // Título de la ventana
  // Inicializar la lista de usuarios
  InicializarListaUsuarios(ListaUsuarios);

  // Insertar usuario root por defecto
  InsertarUsuario(ListaUsuarios, 1, 'Administrador Root', 'root',
    ROOT_EMAIL, '0000-0000');

  StatusBar1.SimpleText := 'Sistema inicializado. Usuario root creado.';
end;

procedure TForm1.btnLoginClick(Sender: TObject);
var
  Usuario: PUsuario;
begin
  // Verificar credenciales root
  if (edtEmail.Text = ROOT_EMAIL) and (edtPassword.Text = ROOT_PASSWORD) then
  begin
    ShowMessage('¡Bienvenido Root!');
    StatusBar1.SimpleText := 'Sesión root iniciada';
    Exit;
  end;

  // Buscar usuario normal
  Usuario := BuscarUsuarioPorEmail(ListaUsuarios, edtEmail.Text);
  if Usuario = nil then
  begin
    ShowMessage('Error: Usuario no encontrado');
    Exit;
  end;

  // Por ahora, cualquier contraseña funciona para usuarios normales
  ShowMessage('¡Bienvenido ' + Usuario^.Nombre + '!');
  StatusBar1.SimpleText := 'Sesión de usuario iniciada: ' + Usuario^.Nombre;
end;

procedure TForm1.btnRegistrarClick(Sender: TObject);
var
  NuevoId: Integer;
begin
  // Validar campos
  if (edtEmail.Text = '') or (edtPassword.Text = '') then
  begin
    ShowMessage('Error: Email y contraseña son obligatorios');
    Exit;
  end;

  // Verificar si el email ya existe
  if BuscarUsuarioPorEmail(ListaUsuarios, edtEmail.Text) <> nil then
  begin
    ShowMessage('Error: El email ya está registrado');
    Exit;
  end;

  // Generar nuevo ID
  if ListaUsuarios.Cabeza = nil then
    NuevoId := 1
  else
    NuevoId := ListaUsuarios.Count + 1;

  // Insertar nuevo usuario
  InsertarUsuario(ListaUsuarios, NuevoId,
    'Usuario ' + IntToStr(NuevoId),  // Nombre por defecto
    'user' + IntToStr(NuevoId),      // Usuario por defecto
    edtEmail.Text,
    'Sin teléfono');                 // Teléfono por defecto

  ShowMessage('Usuario registrado exitosamente!');
  StatusBar1.SimpleText := 'Nuevo usuario registrado: ' + edtEmail.Text;

  // Limpiar campos
  edtEmail.Text := '';
  edtPassword.Text := '';
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Liberar memoria al cerrar
  LiberarListaUsuarios(ListaUsuarios);
end;

end.
