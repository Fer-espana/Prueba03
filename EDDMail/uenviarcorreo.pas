unit UEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaCircularContactos, UListaDobleEnlazadaCorreos, UGLOBAL,
  UListaSimpleUsuarios, UMatrizDispersaRelaciones, UAVLTreeBorradores;

type

  { TForm4 } // <--- La clase correcta es TForm4

  TForm4 = class(TForm)
    btnEnviar: TButton;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoMensaje: TMemo;
    btnGuardarBorrador: TButton;
    procedure btnEnviarClick(Sender: TObject);
    procedure editAsuntoChange(Sender: TObject);
    procedure editDestinatarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoMensajeChange(Sender: TObject);
    procedure btnGuardarBorradorClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    function GenerarIdCorreo: Integer;
    function ValidarDestinatario(Destinatario: string): Boolean;
    function BuscarIndiceUsuario(Email: string): Integer;
    procedure GuardarCorreoEnJSON(Id: Integer; Remitente, Destinatario, Asunto, Mensaje, Fecha: string); // <--- NUEVA DECLARACIÓN
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
    UsuarioActual^.Email, Destinatario, 'N', False, Asunto, FechaActual, Mensaje);

  // NUEVA LÍNEA: GUARDAR EN LOG JSON
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

// CORRECCIÓN CLAVE: Se corrige TForm7 a TForm4
procedure TForm4.btnGuardarBorradorClick(Sender: TObject);
var
  NuevoCorreo: TCorreo; // Usaremos la estructura TCorreo
  NuevoID: Integer;
begin
  // CORRECCIÓN: Se corrigen los nombres de los componentes (editAsunto, editDestinatario)
  if (BandejaActual = nil) or (editAsunto.Text = '') or (MemoMensaje.Text = '') then
  begin
    ShowMessage('Asunto y mensaje son obligatorios.');
    Exit;
  end;

  // 1. Generar nuevo ID (CORRECCIÓN: Se llama al método local GenerarIdCorreo)
  NuevoID := GenerarIdCorreo;

  // 2. Crear un nuevo registro TCorreo con el estado 'Borrador' (o 'B')
  NuevoCorreo.Id := NuevoID;
  NuevoCorreo.Remitente := UsuarioActual^.Email;
  NuevoCorreo.Destinatario := editDestinatario.Text;
  NuevoCorreo.Estado := 'B'; // 'B' para Borrador (Asumiendo que usas 'B')
  NuevoCorreo.Programado := False;
  NuevoCorreo.Asunto := editAsunto.Text;
  NuevoCorreo.Fecha := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  NuevoCorreo.Mensaje := MemoMensaje.Text;

  // 3. Insertar en el Árbol AVL de borradores
  InsertarEnAVL(BandejaActual^.Borradores, NuevoID, NuevoCorreo);

  ShowMessage('Correo guardado como borrador (ID: ' + IntToStr(NuevoID) + ').');
  Self.Close; // Self.Close funciona correctamente dentro de TForm4
end;

procedure TForm4.GuardarCorreoEnJSON(Id: Integer; Remitente, Destinatario, Asunto, Mensaje, Fecha: string);
var
  Archivo: TextFile;
  RutaArchivo, RutaCarpeta: string;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Data';
  RutaArchivo := RutaCarpeta + PathDelim + 'correos_enviados_log.json'; // Usar un nombre de log distinto

  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  AssignFile(Archivo, RutaArchivo);
  try
    // La forma más simple de evitar errores de formato en JSON es añadir cada objeto en una nueva línea
    // y envolver el contenido completo en un array al leer.
    if FileExists(RutaArchivo) then
      Append(Archivo) // Abrir para añadir al final
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
