unit UEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // Dependencias existentes
  uglobal, ulistasimpleusuarios, ulistacircularcontactos, ulistadobleenlazadacorreos,
  uavltreeborradores, // Para guardar borrador
  // *** NUEVAS DEPENDENCIAS (Fase 3) ***
  blockchain, // <-- Para registrar envío
  privado,    // <-- Para guardar como privado
  UVerBorradores; // Para notificar actualización de borradores

type

  { TForm4 }

  TForm4 = class(TForm)
    btnEnviar: TButton;
    // *** NUEVO BOTÓN (Fase 3) ***
    btnGuardarPrivado: TButton;
    btnGuardarBorrador: TButton;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoMensaje: TMemo;
    procedure btnEnviarClick(Sender: TObject);
    procedure btnGuardarBorradorClick(Sender: TObject);
    procedure btnGuardarPrivadoClick(Sender: TObject); // Evento del botón Guardar Privado
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
    procedure GuardarCorreoEnJSON(Id: Integer; Remitente, Destinatario, Asunto, Mensaje, Fecha: string);
    procedure NotificarFormularioBorradores;
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

procedure TForm4.NotificarFormularioBorradores;
var
  i: Integer;
  FormBorradores: TForm16;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TForm16 then
    begin
      FormBorradores := (Screen.Forms[i] as TForm16);
      FormBorradores.RefrescarDatos;
      Break;
    end;
  end;
end;

procedure TForm4.btnEnviarClick(Sender: TObject);
var
  Destinatario, Asunto, Mensaje: string;
  BandejaDestino: PBandejaUsuario;
  NuevoId: Integer;
  FechaActual: string;
  IndiceRemitente, IndiceDestinatario: Integer;
  destinatarioUsuario: PUsuario;
begin
  Destinatario := Trim(editDestinatario.Text);
  Asunto := Trim(editAsunto.Text);
  Mensaje := MemoMensaje.Text; // No quitar espacios intermedios

  // Validaciones básicas
  if (Destinatario = '') or (Asunto = '') or (Mensaje = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  // Validar usuario logueado
  if UsuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado.');
    Exit;
  end;

  // Validar destinatario usando contactos
  if not ValidarDestinatario(Destinatario) then
    Exit;

  // Validar si el destinatario existe en el sistema
  destinatarioUsuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, Destinatario);
  if destinatarioUsuario = nil then
  begin
    ShowMessage('Error: El contacto "' + Destinatario + '" no es un usuario registrado en el sistema.');
    Exit;
  end;

  // --- Lógica de Envío ---
  BandejaDestino := ObtenerBandejaUsuario(Destinatario);
  if BandejaDestino = nil then
    BandejaDestino := CrearBandejaUsuario(Destinatario);

  // Generar nuevo ID y fecha
  NuevoId := GenerarIdCorreo;
  FechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Insertar correo en la bandeja del destinatario
  InsertarCorreo(BandejaDestino^.BandejaEntrada, NuevoId,
    UsuarioActual^.Email, Destinatario, 'N', False, Asunto, FechaActual, Mensaje);

  // *** INTEGRACIÓN BLOCKCHAIN (Fase 3) ***
  AgregarBloque(SistemaBlockchain, IntToStr(NuevoId), UsuarioActual^.Email, Asunto, Mensaje);

  // Guardar en log JSON
  GuardarCorreoEnJSON(NuevoId, UsuarioActual^.Email, Destinatario, Asunto, Mensaje, FechaActual);

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

procedure TForm4.btnGuardarBorradorClick(Sender: TObject);
var
  NuevoCorreo: TCorreo;
  NuevoID: Integer;
begin
  if (BandejaActual = nil) or (editAsunto.Text = '') or (MemoMensaje.Text = '') then
  begin
    ShowMessage('Asunto y mensaje son obligatorios.');
    Exit;
  end;

  // Generar nuevo ID
  NuevoID := GenerarIdCorreo;

  // Crear registro TCorreo con estado 'Borrador'
  NuevoCorreo.Id := NuevoID;
  NuevoCorreo.Remitente := UsuarioActual^.Email;
  NuevoCorreo.Destinatario := editDestinatario.Text;
  NuevoCorreo.Estado := 'B';
  NuevoCorreo.Programado := False;
  NuevoCorreo.Asunto := editAsunto.Text;
  NuevoCorreo.Fecha := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  NuevoCorreo.Mensaje := MemoMensaje.Text;

  InsertarEnAVL(BandejaActual^.Borradores, NuevoID, NuevoCorreo);

  ShowMessage('Correo guardado como borrador (ID: ' + IntToStr(NuevoID) + ').');
  NotificarFormularioBorradores;
  Self.Close;
end;

//
// *** NUEVO PROCEDIMIENTO COMPLETO (Fase 3 con MerkleTree) ***
//
procedure TForm4.btnGuardarPrivadoClick(Sender: TObject);
var
  destinatario: String;
  asunto: String;
  mensajeTexto: String;
  fechaStr: String;
  merklePrivados: TMerkleTree;
begin
  if UsuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado.');
    Exit;
  end;

  // Obtener instancia del árbol Merkle del usuario logueado
  merklePrivados := UsuarioActual^.privados;
  if not Assigned(merklePrivados) then
  begin
    ShowMessage('Error interno: El árbol de privados no está inicializado.');
    Exit;
  end;

  // 1. Obtener datos
  destinatario := Trim(editDestinatario.Text);
  asunto := Trim(editAsunto.Text);
  mensajeTexto := MemoMensaje.Text; // No trimear el mensaje
  fechaStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // 2. Validaciones
  if (asunto = '') then
  begin
    ShowMessage('El asunto es obligatorio para guardar como privado.');
    Exit;
  end;

  if (mensajeTexto = '') then
  begin
    ShowMessage('El mensaje no puede estar vacío para guardar como privado.');
    Exit;
  end;

  // Validar duplicado de asunto
  if merklePrivados.ExisteAsunto(asunto) then
  begin
    ShowMessage('Ya existe un correo privado con el asunto: "' + asunto + '". No se permiten duplicados.');
    Exit;
  end;

  // 3. Insertar en el Árbol Merkle
  try
    if destinatario = '' then
       destinatario := UsuarioActual^.Email;

    merklePrivados.Insert(destinatario, asunto, fechaStr, mensajeTexto);

    ShowMessage('Contenido guardado exitosamente como correo privado.' + #13#10 +
                'El Árbol Merkle ha sido reconstruido.');

    editDestinatario.Clear;
    editAsunto.Clear;
    MemoMensaje.Clear;
    Close;
  except
    on E: Exception do
      ShowMessage('Error al guardar como privado: ' + E.Message);
  end;
end;

procedure TForm4.GuardarCorreoEnJSON(Id: Integer; Remitente, Destinatario, Asunto, Mensaje, Fecha: string);
var
  Archivo: TextFile;
  RutaArchivo, RutaCarpeta: string;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Data';
  RutaArchivo := RutaCarpeta + PathDelim + 'correos_enviados_log.json';

  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  AssignFile(Archivo, RutaArchivo);
  try
    if FileExists(RutaArchivo) then
      Append(Archivo)
    else
      Rewrite(Archivo);

    WriteLn(Archivo, '{');
    WriteLn(Archivo, '  "id": ', Id, ',');
    WriteLn(Archivo, '  "remitente": "', Remitente, '",');
    WriteLn(Archivo, '  "destinatario": "', Destinatario, '",');
    WriteLn(Archivo, '  "asunto": "', Asunto, '",');
    WriteLn(Archivo, '  "mensaje": "', StringReplace(Mensaje, sLineBreak, '\n', [rfReplaceAll]), '",');
    WriteLn(Archivo, '  "fecha": "', Fecha, '"');
    WriteLn(Archivo, '}');
  finally
    CloseFile(Archivo);
  end;
end;

//
// EVENTOS VACÍOS NECESARIOS
//
procedure TForm4.editAsuntoChange(Sender: TObject);
begin
end;

procedure TForm4.editDestinatarioChange(Sender: TObject);
begin
end;

procedure TForm4.MemoMensajeChange(Sender: TObject);
begin
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  // No liberar BandejaActual (es global)
end;

end.

