unit UEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaCircularContactos, UListaDobleEnlazadaCorreos;

type

  { TForm4 }

  TForm4 = class(TForm)
    btnEnviar: TButton;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoMensaje: TMemo;
    procedure btnEnviarClick(Sender: TObject);
    procedure editAsuntoChange(Sender: TObject);
    procedure editDestinatarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoMensajeChange(Sender: TObject);
  private
    ListaContactos: TListaContactos;
  public
  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin
  Caption := 'Enviar Correo';
  InicializarListaContactos(ListaContactos);
end;

procedure TForm4.btnEnviarClick(Sender: TObject);
var
  Destinatario, Asunto, Mensaje: string;
begin
  Destinatario := editDestinatario.Text;
  Asunto := editAsunto.Text;
  Mensaje := MemoMensaje.Text;

  // Validaciones básicas
  if (Destinatario = '') or (Asunto = '') or (Mensaje = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  // Verificar si el destinatario está en contactos
  if BuscarContactoPorEmail(ListaContactos, Destinatario) = nil then
  begin
    ShowMessage('Error: El destinatario no está en sus contactos');
    Exit;
  end;

  // Aquí iría la lógica para enviar el correo
  ShowMessage('Correo enviado exitosamente a: ' + Destinatario);

  // Limpiar campos
  editDestinatario.Text := '';
  editAsunto.Text := '';
  MemoMensaje.Text := '';
end;

procedure TForm4.editAsuntoChange(Sender: TObject);
begin

end;

procedure TForm4.editDestinatarioChange(Sender: TObject);
begin

end;

procedure TForm4.FormDestroy(Sender: TObject);
begin

end;

procedure TForm4.MemoMensajeChange(Sender: TObject);
begin

end;

end.

