unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UListaSimpleUsuarios, URegistrarseNuevo;

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
    // Ya no necesitamos ListaUsuarios local, usamos la global
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
  Caption := 'EDDMail - Login';

  // La lista global ya se inicializó en UListaSimpleUsuarios
  // Solo insertamos el usuario root si no existe
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, ROOT_EMAIL) = nil then
  begin
    InsertarUsuario(ListaUsuariosGlobal, 1, 'Administrador Root', 'root',
      ROOT_EMAIL, '0000-0000');
    StatusBar1.SimpleText := 'Sistema inicializado. Usuario root creado.';
  end
  else
    StatusBar1.SimpleText := 'Sistema inicializado.';
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

  // Buscar usuario normal en la lista GLOBAL
  Usuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, edtEmail.Text);
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
begin
  // Abrir formulario de registro
  Form8 := TForm8.Create(Application);
  try
    Form8.ShowModal;
  finally
    Form8.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Liberar memoria de la lista GLOBAL al cerrar
  LiberarListaUsuarios(ListaUsuariosGlobal);
end;

end.
