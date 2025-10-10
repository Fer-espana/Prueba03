unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UListaSimpleUsuarios, URegistrarse, UUsuarioEstandar, UROOT, UGLOBAL;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLogin: TButton;
    btnRegistrar: TButton;
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

  // Insertar usuario root si no existe
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, ROOT_EMAIL) = nil then
  begin
    InsertarUsuario(ListaUsuariosGlobal, 1, 'Administrador Root', 'root',
      ROOT_EMAIL, '0000-0000');
    StatusBar1.SimpleText := 'Sistema inicializado. Usuario root creado.';
  end
  else
    StatusBar1.SimpleText := 'Sistema inicializado. Listo para usar.';
end;

procedure TForm1.btnLoginClick(Sender: TObject);
var
  Usuario: PUsuario;
  FormUsuario: TForm3;
  FormRoot: TForm2;
begin
  // Verificar credenciales root
  if (edtEmail.Text = ROOT_EMAIL) and (edtPassword.Text = ROOT_PASSWORD) then
  begin
    EsUsuarioRoot := True;
    UsuarioActual := nil;

    ShowMessage('¡Bienvenido Root!');
    StatusBar1.SimpleText := 'Sesión root iniciada';

    // Navegar a formulario Root
    FormRoot := TForm2.Create(Application);
    FormRoot.Show;
    Self.Hide; // Ocultar login
    Exit;
  end;

  // Buscar usuario normal en la lista GLOBAL
  Usuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, edtEmail.Text);
  if Usuario = nil then
  begin
    ShowMessage('Error: Usuario no encontrado');
    Exit;
  end;

  // Configurar usuario actual
  EsUsuarioRoot := False;
  UsuarioActual := Usuario;

  ShowMessage('¡Bienvenido ' + Usuario^.Nombre + '!');
  StatusBar1.SimpleText := 'Sesión de usuario iniciada: ' + Usuario^.Nombre;

  // Navegar a formulario de usuario estándar
  FormUsuario := TForm3.Create(Application);
  FormUsuario.SetUsuarioActual(Usuario);
  FormUsuario.Show;
  Self.Hide; // Ocultar login
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
  LiberarListaUsuarios(ListaUsuariosGlobal);
end;

end.
