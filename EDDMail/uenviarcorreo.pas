unit UEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaCircularContactos, UListaDobleEnlazadaCorreos, UGLOBAL,
  UListaSimpleUsuarios, UMatrizDispersaRelaciones;

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
    BandejaActual: PBandejaUsuario;
    function GenerarIdCorreo: Integer;
    function ValidarDestinatario(Destinatario: string): Boolean;
    function BuscarIndiceUsuario(Email: string): Integer;
  public
    procedure SetBandejaActual(Email: string);
  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin
  Caption := 'Enviar Correo';
end;

procedure TForm4.SetBandejaActual(Email: string);
begin
  BandejaActual := ObtenerBandejaUsuario(Email);
  if BandejaActual = nil then
    BandejaActual := CrearBandejaUsuario(Email);
end;

function TForm4.GenerarIdCorreo: Integer;
var
  CorreoId: Integer;
begin
  Randomize;
  CorreoId := Random(100000) + 1;
  Result := CorreoId;
end;

function TForm4.BuscarIndiceUsuario(Email: string): Integer;
var
  Actual: PUsuario;
  Index: Integer;
begin
  Actual := ListaUsuariosGlobal.Cabeza;
  Index := 0;

  while Actual <> nil do
  begin
    if Actual^.Email = Email then
      Exit(Index);
    Actual := Actual^.Siguiente;
    Inc(Index);
  end;

  Result := -1; // No encontrado
end;

function TForm4.ValidarDestinatario(Destinatario: string): Boolean;
var
  Contacto: PContacto;
begin
  // Verificar si el destinatario existe en el sistema
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, Destinatario) = nil then
  begin
    ShowMessage('Error: El destinatario no existe en el sistema');
    Exit(False);
  end;

  // Verificar si está en los contactos del usuario actual
  if BandejaActual <> nil then
  begin
    Contacto := BuscarContactoPorEmail(BandejaActual^.Contactos, Destinatario);
    if Contacto = nil then
    begin
      ShowMessage('Error: El destinatario no está en sus contactos');
      Exit(False);
    end;
  end;

  Result := True;
end;

procedure TForm4.btnEnviarClick(Sender: TObject);
var
  Destinatario, Asunto, Mensaje: string;
  BandejaDestino: PBandejaUsuario;
  NuevoId: Integer;
  FechaActual: string;
  IndiceRemitente, IndiceDestinatario: Integer;
begin
  Destinatario := Trim(editDestinatario.Text);
  Asunto := Trim(editAsunto.Text);
  Mensaje := Trim(MemoMensaje.Text);

  // Validaciones básicas
  if (Destinatario = '') or (Asunto = '') or (Mensaje = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  // Validar destinatario
  if not ValidarDestinatario(Destinatario) then
    Exit;

  // Obtener o crear bandeja del destinatario
  BandejaDestino := ObtenerBandejaUsuario(Destinatario);
  if BandejaDestino = nil then
    BandejaDestino := CrearBandejaUsuario(Destinatario);

  // Generar nuevo ID y fecha
  NuevoId := GenerarIdCorreo;
  FechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Insertar correo en la bandeja del destinatario
  InsertarCorreo(BandejaDestino^.BandejaEntrada, NuevoId,
    UsuarioActual^.Email, 'N', False, Asunto, FechaActual, Mensaje);

  // Actualizar matriz de relaciones
  IndiceRemitente := BuscarIndiceUsuario(UsuarioActual^.Email);
  IndiceDestinatario := BuscarIndiceUsuario(Destinatario);

  if (IndiceRemitente <> -1) and (IndiceDestinatario <> -1) then
  begin
    InsertarValor(MatrizRelaciones, IndiceRemitente, IndiceDestinatario, 1);
  end;

  ShowMessage('Correo enviado exitosamente a: ' + Destinatario +
              sLineBreak + 'ID: ' + IntToStr(NuevoId));

  // Limpiar campos
  editDestinatario.Text := '';
  editAsunto.Text := '';
  MemoMensaje.Text := '';
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS PARA EVITAR ERRORES
// =============================================================================

procedure TForm4.editAsuntoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm4.editDestinatarioChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm4.MemoMensajeChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  // No liberamos BandejaActual porque es global
end;

end.
