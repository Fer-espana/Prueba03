unit UROOT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGLOBAL;
  // QUITAMOS UPrincipal de aquí

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Panel de Administración Root';
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Al cerrar el formulario Root, volver al login
  CloseAction := caFree;
  Form1.Show;
end;

procedure TForm2.btnCargaMasivaClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de carga masiva - Próximamente');
end;

procedure TForm2.btnReporteUsuariosClick(Sender: TObject);
begin
  ShowMessage('Generando reporte de usuarios...');
  // Aquí irá la lógica para generar reporte
end;

procedure TForm2.btnReporteRelacionesClick(Sender: TObject);
begin
  ShowMessage('Generando reporte de relaciones...');
  // Aquí irá la lógica para generar reporte
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  // Limpiar estado global
  EsUsuarioRoot := False;
  UsuarioActual := nil;
end;

end.
