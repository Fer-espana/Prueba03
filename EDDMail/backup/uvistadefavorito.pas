unit UVistadeFavorito;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaDobleEnlazadaCorreos, UGLOBAL, UPilaPapelera, UArbolB;

type

  { TForm18 } // <--- TForm18

  TForm18 = class(TForm)
    btnEliminar: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblRemitente: TLabel;
    lblAsunto: TLabel;
    lblFecha: TLabel;
    lblEstado: TLabel;
    MemoMensaje: TMemo;
    procedure btnEliminarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CorreoActual: PCorreo;
    BandejaActual: PBandejaUsuario; // Necesario para la eliminación
    procedure NotificarFormularioPrincipal;
  public
    procedure SetCorreoActual(Correo: PCorreo; Bandeja: PBandejaUsuario);
  end;

var
  Form18: TForm18;

implementation

{$R *.lfm}

{ TForm18 }

procedure TForm18.FormCreate(Sender: TObject);
begin
  Caption := 'Vista de Correo Favorito';

  // Configurar etiquetas
  Label1.Caption := 'Remitente:';
  Label2.Caption := 'Asunto:';
  Label3.Caption := 'Fecha:';
  Label4.Caption := 'Estado:';
  Label5.Caption := 'Mensaje:';

  MemoMensaje.ReadOnly := True;

  btnEliminar.Caption := 'Eliminar de Favoritos';
end;

procedure TForm18.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm18.SetCorreoActual(Correo: PCorreo; Bandeja: PBandejaUsuario);
var
  EstadoStr: string;
begin
  CorreoActual := Correo;
  BandejaActual := Bandeja;

  if CorreoActual <> nil then
  begin
    lblRemitente.Caption := CorreoActual^.Remitente;
    lblAsunto.Caption := CorreoActual^.Asunto;
    lblFecha.Caption := CorreoActual^.Fecha;

    case CorreoActual^.Estado of
      'N': EstadoStr := 'NO LEIDO';
      'L': EstadoStr := 'LEIDO';
      'E': EstadoStr := 'ELIMINADO';
      'P': EstadoStr := 'PROGRAMADO';
      else EstadoStr := 'FAVORITO';
    end;
    lblEstado.Caption := EstadoStr;

    MemoMensaje.Text := CorreoActual^.Mensaje;
    Caption := 'Favorito: ' + CorreoActual^.Asunto;
  end;
end;

procedure TForm18.NotificarFormularioPrincipal;
var
  i: Integer;
begin
  // Busca TForm17 y lo refresca
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TForm17 then
    begin
      (Screen.Forms[i] as TForm17).RefrescarDatos;
    end;
  end;
end;

procedure TForm18.btnEliminarClick(Sender: TObject);
begin
  if (CorreoActual = nil) or (BandejaActual = nil) then Exit;

  if MessageDlg('Confirmar',
                '¿Está seguro de que desea eliminar este correo de Favoritos?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Llamar a la función de eliminación del Árbol B
    if EliminarDeArbolB(BandejaActual^.Favoritos, CorreoActual^.Id) then
    begin
      ShowMessage('Correo eliminado de Favoritos.');
      NotificarFormularioPrincipal;
      Close;
    end
    else
    begin
      ShowMessage('Error: No se pudo eliminar el correo del Árbol B.');
    end;
  end;
end;

end.
