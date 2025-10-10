unit UUsuarioEstandar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UListaSimpleUsuarios,
  UBandejaEntrada, UEnviarCorreo, UPapelera, UProgramarCorreo, UCorreosProgramados,
  UAgregarContacto, UVentanaContactos, UActualizarPerfil, UGLOBAL;
  // Removemos UNAVEGACION ya que no existe

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
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure bntEnviarCorreoClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnProgramarCorreoClick(Sender: TObject);
    procedure btnCorreosProgramadosClick(Sender: TObject);
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnGenerarReportesClick(Sender: TObject);
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

end;

procedure TForm3.FormDestroy(Sender: TObject);
begin

end;

procedure TForm3.btnBandejaEntradaClick(Sender: TObject);
begin
  Form9 := TForm9.Create(Application);
  Form9.SetBandejaActual(UsuarioActual^.Email);
  Form9.Show;
end;

procedure TForm3.bntEnviarCorreoClick(Sender: TObject);
begin
  Form4 := TForm4.Create(Application);
  Form4.SetBandejaActual(UsuarioActual^.Email);
  Form4.Show;
end;

procedure TForm3.btnPapeleraClick(Sender: TObject);
begin
  Form11 := TForm11.Create(Application);
  Form11.Show;
end;

procedure TForm3.btnProgramarCorreoClick(Sender: TObject);
begin
  Form5 := TForm5.Create(Application);
  Form5.Show;
end;

procedure TForm3.btnCorreosProgramadosClick(Sender: TObject);
begin
  Form12 := TForm12.Create(Application);
  Form12.Show;
end;

procedure TForm3.btnAgregarContactoClick(Sender: TObject);
begin
  Form6 := TForm6.Create(Application);
  Form6.Show;
end;

procedure TForm3.btnContactosClick(Sender: TObject);
begin
  Form13 := TForm13.Create(Application);
  Form13.Show;
end;

procedure TForm3.btnActualizarPerfilClick(Sender: TObject);
begin
  Form7 := TForm7.Create(Application);
  Form7.Show;
end;

procedure TForm3.btnGenerarReportesClick(Sender: TObject);
begin
  ShowMessage('Generando reportes...');
  // Aquí irá la lógica para generar reportes
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
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;

end.
