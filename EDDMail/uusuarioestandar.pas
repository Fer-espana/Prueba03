unit UUsuarioEstandar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UListaSimpleUsuarios,
  UBandejaEntrada, UEnviarCorreo, UPapelera, UProgramarCorreo, UCorreosProgramados,
  UAgregarContacto, UVentanaContactos, UActualizarPerfil, UGLOBAL;
  // Removemos UPrincipal para evitar referencia circular

type

  { TForm3 }

  TForm3 = class(TForm)
    btnBandejaEntrada: TButton;
    bntEnviarCorreo: TButton;
    btnPapelera: TButton;
    btnProgramarCorreo: TButton;
    btnCorreosProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnActualizarPerfil: TButton;
    btnGenerarReportes: TButton;
    btnRegresarLogin: TButton;
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure bntEnviarCorreoClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnCorreosProgramadosClick(Sender: TObject);
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnGenerarReportesClick(Sender: TObject);
    procedure btnRegresarLoginClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    UsuarioActual: PUsuario;
  public
    procedure SetUsuarioActual(Usuario: PUsuario);
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Configurar botón de cerrar sesión
  btnRegresarLogin.Caption := 'Cerrar Sesión';
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  // Limpiar recursos si es necesario
end;

procedure TForm3.btnBandejaEntradaClick(Sender: TObject);
var
  FormBandeja: TForm9;
begin
  FormBandeja := TForm9.Create(Application);
  FormBandeja.SetBandejaActual(UsuarioActual^.Email);
  FormBandeja.Show;
end;

procedure TForm3.bntEnviarCorreoClick(Sender: TObject);
var
  FormEnviar: TForm4;
begin
  FormEnviar := TForm4.Create(Application);
  FormEnviar.SetBandejaActual(UsuarioActual^.Email);
  FormEnviar.Show;
end;

procedure TForm3.btnPapeleraClick(Sender: TObject);
var
  FormPapelera: TForm11;
begin
  FormPapelera := TForm11.Create(Application);
  // FormPapelera NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormPapelera.Show;
end;

procedure TForm3.btnProgramarCorreoClick(Sender: TObject);
var
  FormProgramarCorreo: TForm5;
begin
  FormProgramarCorreo := TForm5.Create(Application);
  // FormProgramarCorreo NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormProgramarCorreo.Show;
end;

procedure TForm3.btnCorreosProgramadosClick(Sender: TObject);
var
  FormCorreosProgramados: TForm12;
begin
  FormCorreosProgramados := TForm12.Create(Application);
  // FormCorreosProgramados NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormCorreosProgramados.Show;
end;

procedure TForm3.btnAgregarContactoClick(Sender: TObject);
var
  FormAgregarContacto: TForm6;
begin
  FormAgregarContacto := TForm6.Create(Application);
  // FormAgregarContacto NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormAgregarContacto.Show;
end;

procedure TForm3.btnContactosClick(Sender: TObject);
var
  FormContactos: TForm13;
begin
  FormContactos := TForm13.Create(Application);
  // FormContactos NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormContactos.Show;
end;

procedure TForm3.btnActualizarPerfilClick(Sender: TObject);
var
   FormActualizarPerfil: TForm7;
begin
  FormActualizarPerfil := TForm7.Create(Application);
  // FormActualizarPerfil NO tiene SetBandejaActual - ELIMINAR ESTA LÍNEA
  FormActualizarPerfil.Show;
end;

procedure TForm3.btnGenerarReportesClick(Sender: TObject);
begin
  ShowMessage('Generando reportes...');
  // Aquí irá la lógica para generar reportes
end;

procedure TForm3.btnRegresarLoginClick(Sender: TObject);
var
  i: Integer;
begin
  // Cerrar sesión del usuario actual
  UsuarioActual := nil;
  EsUsuarioRoot := False;

  // Mostrar mensaje de confirmación
  ShowMessage('Sesión cerrada exitosamente');

  // Buscar y mostrar el formulario de login principal
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i].Name = 'Form1' then
    begin
      Screen.Forms[i].Show;
      Break;
    end;
  end;

  // Cerrar este formulario
  Close;
end;


procedure TForm3.SetUsuarioActual(Usuario: PUsuario);
begin
  UsuarioActual := Usuario;
  if UsuarioActual <> nil then
    Caption := 'EDDMail - ' + UsuarioActual^.Nombre;
end;

procedure TForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  // Mostrar el login cuando se cierra este formulario
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;

end.
