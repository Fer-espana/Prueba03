unit UVistadeCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaDobleEnlazadaCorreos, UGLOBAL, UPilaPapelera;

type

  { TForm10 }

  TForm10 = class(TForm)
    btnEliminarCorreo: TButton;
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
    procedure btnEliminarCorreoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CorreoActual: PCorreo;
    BandejaActual: PBandejaUsuario;
  public
    procedure SetCorreoActual(Correo: PCorreo; Bandeja: PBandejaUsuario);
  end;

var
  Form10: TForm10;

implementation

{$R *.lfm}

{ TForm10 }

procedure TForm10.FormCreate(Sender: TObject);
begin
  Caption := 'Vista de Correo';

  // Configurar etiquetas
  Label1.Caption := 'Remitente:';
  Label2.Caption := 'Asunto:';
  Label3.Caption := 'Fecha:';
  Label4.Caption := 'Estado:';
  Label5.Caption := 'Mensaje:';

  // Configurar memo como solo lectura
  MemoMensaje.ReadOnly := True;

  btnEliminarCorreo.Caption := 'Eliminar Correo';
end;

procedure TForm10.SetCorreoActual(Correo: PCorreo; Bandeja: PBandejaUsuario);
var
  EstadoStr: string;
begin
  CorreoActual := Correo;
  BandejaActual := Bandeja;

  if CorreoActual <> nil then
  begin
    // Mostrar información del correo
    lblRemitente.Caption := CorreoActual^.Remitente;
    lblAsunto.Caption := CorreoActual^.Asunto;
    lblFecha.Caption := CorreoActual^.Fecha;

    // Convertir estado a texto legible
    case CorreoActual^.Estado of
      'N': EstadoStr := 'NO LEIDO';
      'L': EstadoStr := 'LEIDO';
      'E': EstadoStr := 'ELIMINADO';
      else EstadoStr := 'DESCONOCIDO';
    end;
    lblEstado.Caption := EstadoStr;

    MemoMensaje.Text := CorreoActual^.Mensaje;

    // Actualizar título de la ventana
    Caption := 'Correo: ' + CorreoActual^.Asunto;
  end;
end;

procedure TForm10.btnEliminarCorreoClick(Sender: TObject);
begin
  if (CorreoActual = nil) or (BandejaActual = nil) then
  begin
    ShowMessage('Error: No hay correo seleccionado');
    Exit;
  end;

  // Confirmar eliminación
  if MessageDlg('Confirmar',
                '¿Está seguro de que desea eliminar este correo?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      // Cambiar estado a eliminado
      CorreoActual^.Estado := 'E';

      // Opcional: Mover a la papelera global
      Apilar(PilaPapeleraGlobal, CorreoActual);

      ShowMessage('Correo eliminado correctamente');
      Close; // Cerrar el formulario de vista
    except
      on E: Exception do
        ShowMessage('Error al eliminar el correo: ' + E.Message);
    end;
  end;
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
  // No liberamos CorreoActual porque es parte de la lista global
end;

end.
