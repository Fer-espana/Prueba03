unit UProgramarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  UGLOBAL, UListaSimpleUsuarios, UListaCircularContactos, UColaCorreosProgramados,
  UListaDobleEnlazadaCorreos, DateUtils, UCorreosProgramados; // AGREGAR UCorreosProgramados

type

  { TForm5 }

  TForm5 = class(TForm)
    btnProgramar: TButton;
    Fecha: TDateEdit;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoMensaje: TMemo;
    procedure btnProgramarClick(Sender: TObject);
    procedure editAsuntoChange(Sender: TObject);
    procedure editDestinatarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoMensajeChange(Sender: TObject);
  private
    { private declarations }
    function ValidarCampos: Boolean;
    function GenerarIdCorreo: Integer;
    function ObtenerFechaHoraProgramada: TDateTime;
  public
    { public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.lfm}

{ TForm5 }

procedure TForm5.FormCreate(Sender: TObject);
begin
  Caption := 'Programar Correo';

  // Configurar Fecha (solo fecha, la hora será la actual + 1 hora)
  Fecha.Date := Date;

  // Configurar labels (mantener los existentes)
  Label1.Caption := 'Destinatario:';
  Label2.Caption := 'Asunto:';
  Label3.Caption := 'Mensaje:';
  Label4.Caption := 'Fecha:';

  btnProgramar.Caption := 'Programar';

  // Limpiar el Memo
  MemoMensaje.Lines.Clear;
end;

procedure TForm5.FormShow(Sender: TObject);
begin
  editDestinatario.SetFocus;
end;

function TForm5.ValidarCampos: Boolean;
var
  Destinatario: string;
  FechaProgramada: TDateTime;
  UsuarioDestino: PUsuario;
  BandejaUsuario: PBandejaUsuario;
  Contacto: PContacto;
begin
  Result := False;
  Destinatario := Trim(editDestinatario.Text);

  // Validar campos vacíos
  if Destinatario = '' then
  begin
    ShowMessage('Debe ingresar un destinatario');
    editDestinatario.SetFocus;
    Exit;
  end;

  if Trim(editAsunto.Text) = '' then
  begin
    ShowMessage('Debe ingresar un asunto');
    editAsunto.SetFocus;
    Exit;
  end;

  if Trim(MemoMensaje.Text) = '' then
  begin
    ShowMessage('Debe ingresar un mensaje');
    MemoMensaje.SetFocus;
    Exit;
  end;

  // Validar que el destinatario exista en el sistema
  UsuarioDestino := BuscarUsuarioPorEmail(ListaUsuariosGlobal, Destinatario);
  if UsuarioDestino = nil then
  begin
    ShowMessage('El usuario destinatario no existe en el sistema');
    editDestinatario.SetFocus;
    Exit;
  end;

  // Validar que el destinatario esté en los contactos
  if UsuarioActual <> nil then
  begin
    BandejaUsuario := ObtenerBandejaUsuario(UsuarioActual^.Email);
    if BandejaUsuario <> nil then
    begin
      Contacto := BuscarContactoPorEmail(BandejaUsuario^.Contactos, Destinatario);
      if Contacto = nil then
      begin
        ShowMessage('El destinatario no está en su lista de contactos');
        editDestinatario.SetFocus;
        Exit;
      end;
    end;
  end;

  // Validar fecha programada (usaremos fecha actual + 1 hora por defecto)
  FechaProgramada := ObtenerFechaHoraProgramada;
  if FechaProgramada <= Now then
  begin
    ShowMessage('La fecha programada debe ser mayor a la fecha actual');
    Fecha.SetFocus;
    Exit;
  end;

  Result := True;
end;

function TForm5.GenerarIdCorreo: Integer;
var
  MaxId: Integer;
  BandejaUsuario: PBandejaUsuario;
  CorreoActual: PCorreo;
begin
  MaxId := 0;

  // Buscar el ID máximo en todas las bandejas
  BandejaUsuario := ListaBandejas;
  while BandejaUsuario <> nil do
  begin
    CorreoActual := BandejaUsuario^.BandejaEntrada.Cabeza;
    while CorreoActual <> nil do
    begin
      if CorreoActual^.Id > MaxId then
        MaxId := CorreoActual^.Id;
      CorreoActual := CorreoActual^.Siguiente;
    end;
    BandejaUsuario := BandejaUsuario^.Siguiente;
  end;

  Result := MaxId + 1;
end;

function TForm5.ObtenerFechaHoraProgramada: TDateTime;
begin
  // Como solo tenemos fecha, programaremos para la fecha seleccionada a las 09:00 AM
  Result := Trunc(Fecha.Date) + EncodeTime(9, 0, 0, 0); // 09:00 AM
end;

procedure TForm5.btnProgramarClick(Sender: TObject);
var
  NuevoCorreo: PCorreo;
  FechaProgramada: TDateTime;
  Destinatario: string;
  NuevoId: Integer;
  FechaProgramadaStr: string;
  i: Integer;
begin
  if not ValidarCampos then
    Exit;

  Destinatario := Trim(editDestinatario.Text);
  FechaProgramada := ObtenerFechaHoraProgramada;
  FechaProgramadaStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', FechaProgramada);

  // Generar nuevo ID
  NuevoId := GenerarIdCorreo;

  // Crear nuevo correo programado
  New(NuevoCorreo);
  try
    NuevoCorreo^.Id := NuevoId;
    NuevoCorreo^.Remitente := UsuarioActual^.Email;
    NuevoCorreo^.Destinatario := Destinatario;
    NuevoCorreo^.Asunto := Trim(editAsunto.Text);
    NuevoCorreo^.Mensaje := Trim(MemoMensaje.Text);
    NuevoCorreo^.Fecha := FechaProgramadaStr;
    NuevoCorreo^.Estado := 'P'; // 'P' para Programado
    NuevoCorreo^.Programado := True;
    NuevoCorreo^.Anterior := nil;
    NuevoCorreo^.Siguiente := nil;

    // ENCOLAR EN LA COLA GLOBAL
    Encolar(ColaCorreosProgramados, NuevoCorreo);
    ShowMessage('Correo encolado. Total en cola: ' + IntToStr(ColaCorreosProgramados.Count));

    // NOTIFICAR A TODAS LAS INSTANCIAS ABIERTAS DE CORREOS PROGRAMADOS
    for i := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[i] is TForm12 then
      begin
        (Screen.Forms[i] as TForm12).RefrescarTabla;
      end;
    end;

    ShowMessage('Correo programado exitosamente para: ' +
                DateTimeToStr(FechaProgramada) + sLineBreak +
                'ID: ' + IntToStr(NuevoId) + sLineBreak +
                'Ahora puedes verlo en "Correos Programados"');

    // Limpiar campos
    editDestinatario.Text := '';
    editAsunto.Text := '';
    MemoMensaje.Text := '';

    // Cerrar formulario
    Close;

  except
    on E: Exception do
    begin
      ShowMessage('Error al programar correo: ' + E.Message);
      Dispose(NuevoCorreo);
    end;
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS PARA EVITAR ERRORES
// =============================================================================

procedure TForm5.editDestinatarioChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm5.editAsuntoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm5.MemoMensajeChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
