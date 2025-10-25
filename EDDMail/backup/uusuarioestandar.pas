unit UUsuarioEstandar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UListaSimpleUsuarios,
  UBandejaEntrada, UEnviarCorreo, UPapelera, UProgramarCorreo, UCorreosProgramados,
  UAgregarContacto, UVentanaContactos, UActualizarPerfil, UGLOBAL, process, UVerBorradores, UFavoritos,
  // *** NUEVAS DEPENDENCIAS (Fase 3) ***
  bitacora, privados, ugestioncomunidades; // <-- Añadir Bitacora y el formulario Privados

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
    btnVerBorradoresDeMensajes: TButton;
    btnFavoritos: TButton;
    // *** NUEVO BOTÓN (Fase 3) ***
    BtnVerPrivados: TButton;
    LabelUsuario: TLabel;
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
    procedure btnVerBorradoresDeMensajesClick(Sender: TObject);
    procedure btnFavoritosClick(Sender: TObject);
    // *** NUEVOS EVENTOS (Fase 3) ***
    procedure BtnVerPrivadosClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    UsuarioActual: PUsuario;
  public
    procedure SetUsuarioActual(Usuario: PUsuario);
    procedure RefrescarDatos;
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

// *** NUEVO PROCEDIMIENTO (Fase 3) ***
procedure TForm3.FormDestroy(Sender: TObject);
begin
  // Registrar salida del usuario estándar
  if UsuarioActual <> nil then
    RegistrarSalida(LogAccesos, UsuarioActual^.Email);

  // Limpiar recursos si es necesario
  UsuarioActual := nil;
end;

// LÓGICA DE REFRESCADO GLOBAL
procedure TForm3.RefrescarDatos;
begin
  // Buscamos las instancias globales de los formularios y llamamos a su método RefrescarDatos,
  // si están asignados y visibles.

  // 1. Refrescar Bandeja de Entrada (TForm9)
  if Assigned(Form9) and (Form9.Visible) then
    Form9.RefrescarDatos;

  // 2. Refrescar Borradores (TForm16)
  if Assigned(Form16) and (Form16.Visible) then
    Form16.RefrescarDatos;

  // 3. Refrescar Favoritos (TForm17)
  if Assigned(Form17) and (Form17.Visible) then
    Form17.RefrescarDatos;
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
  FormPapelera.Show;
end;

procedure TForm3.btnProgramarCorreoClick(Sender: TObject);
var
  FormProgramarCorreo: TForm5;
begin
  FormProgramarCorreo := TForm5.Create(Application);
  FormProgramarCorreo.Show;
end;

procedure TForm3.btnCorreosProgramadosClick(Sender: TObject);
var
  FormCorreosProgramados: TForm12;
begin
  FormCorreosProgramados := TForm12.Create(Application);
  FormCorreosProgramados.Show;
end;

procedure TForm3.btnAgregarContactoClick(Sender: TObject);
var
  FormAgregarContacto: TForm6;
begin
  FormAgregarContacto := TForm6.Create(Application);
  FormAgregarContacto.Show;
end;

procedure TForm3.btnContactosClick(Sender: TObject);
var
  FormContactos: TForm13;
begin
  FormContactos := TForm13.Create(Application);
  FormContactos.Show;
end;

procedure TForm3.btnActualizarPerfilClick(Sender: TObject);
var
    FormActualizarPerfil: TForm7;
begin
  FormActualizarPerfil := TForm7.Create(Application);
  FormActualizarPerfil.Show;
end;

procedure TForm3.btnGenerarReportesClick(Sender: TObject);
begin
  if UsuarioActual <> nil then
    GenerarReportesUsuario(UsuarioActual^.Email)
  else
    ShowMessage('Error: No hay usuario actual para generar reportes');
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

procedure TForm3.btnVerBorradoresDeMensajesClick(Sender: TObject);
var
  FormBorradores: TForm16;
begin
  if UsuarioActual = nil then Exit;

  FormBorradores := TForm16.Create(Application);
  FormBorradores.RefrescarDatos;
  FormBorradores.Show;
end;

procedure TForm3.SetUsuarioActual(Usuario: PUsuario);
begin
  UsuarioActual := Usuario;
  if UsuarioActual <> nil then
  begin
    Caption := 'EDDMail - ' + UsuarioActual^.Nombre;
    // *** ACTUALIZAR LABEL (Fase 3) ***
    LabelUsuario.Caption := 'Hola: ' + UsuarioActual^.Nombre;
  end
  else
    LabelUsuario.Caption := 'Hola: Usuario Desconocido';
end;

procedure TForm3.btnFavoritosClick(Sender: TObject);
var
  FormFavoritos: TForm17;
begin
  if UsuarioActual = nil then Exit;

  FormFavoritos := TForm17.Create(Application);
  FormFavoritos.RefrescarDatos;
  FormFavoritos.Show;
end;

// *** NUEVO PROCEDIMIENTO (Fase 3) ***
procedure TForm3.BtnVerPrivadosClick(Sender: TObject);
var
  FormPriv: TFormPrivados;
begin
  if UsuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado.');
    Exit;
  end;

  FormPriv := TFormPrivados.Create(Application);
  try
    FormPriv.ShowModal;
  finally
    FormPriv.Free;
  end;
end;

procedure TForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  // Mostrar el login cuando se cierra este formulario
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;

end.
