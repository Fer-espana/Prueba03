unit UCorregirBorrador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaSimpleUsuarios, UListaDobleEnlazadaCorreos,
  UListaCircularContactos, UMatrizDispersaRelaciones, UPilaPapelera;

type

  { TForm15 } // <--- TForm15

  TForm15 = class(TForm)
    editDestinatario: TEdit;
    editAsunto: TEdit;
    MemoMensaje: TMemo;
    btnEnviar: TButton;
    btnEliminar: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblID: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    BorradorID: Integer;
    CorreoBorradorActual: PCorreo;
    procedure CargarBorrador;
    function ValidarDestinatario(Destinatario: string): Boolean;
    function GenerarNuevoId: Integer;
    function BuscarIndiceUsuario(Email: string): Integer;
  public
    procedure SetBorrador(AID: Integer; ABandeja: PBandejaUsuario);
  end;

var
  Form15: TForm15;

implementation

{$R *.lfm}

{ TForm15 }

procedure TForm15.FormCreate(Sender: TObject);
begin
  Caption := 'Modificar y Enviar Borrador';
  btnEnviar.Caption := 'Enviar';
  btnEliminar.Caption := 'Eliminar Borrador';
  Label4.Caption := 'ID Borrador:';
end;

procedure TForm15.SetBorrador(AID: Integer; ABandeja: PBandejaUsuario);
begin
  BorradorID := AID;
  BandejaActual := ABandeja;
  CargarBorrador;
end;

procedure TForm15.CargarBorrador;
begin
  // Buscar el correo en el Árbol AVL y guardar la referencia
  if (BandejaActual <> nil) and (BorradorID > 0) then
  begin
    CorreoBorradorActual := BuscarEnAVL(BandejaActual^.Borradores, BorradorID);

    if CorreoBorradorActual <> nil then
    begin
      // Cargar campos:
      lblID.Caption := IntToStr(BorradorID);
      editDestinatario.Text := CorreoBorradorActual^.Destinatario;
      editAsunto.Text := CorreoBorradorActual^.Asunto;
      MemoMensaje.Text := CorreoBorradorActual^.Mensaje;
    end
    else
    begin
      ShowMessage('Error: Borrador no encontrado en el AVL.');
      Close;
    end;
  end;
end;

procedure TForm15.btnEnviarClick(Sender: TObject);
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

  if not ValidarDestinatario(Destinatario) then
    Exit;

  BandejaDestino := ObtenerBandejaUsuario(Destinatario);
  if BandejaDestino = nil then
    BandejaDestino := CrearBandejaUsuario(Destinatario);

  // 1. Generar nuevo ID para el correo ENVIADO
  NuevoId := GenerarIdCorreo;
  FechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // 2. Insertar correo en la bandeja del destinatario con los datos MODIFICADOS
  InsertarCorreo(BandejaDestino^.BandejaEntrada, NuevoId,
    UsuarioActual^.Email, Destinatario, 'N', False, Asunto, FechaActual, Mensaje);

  // 3. Eliminar de la lista de borradores (Árbol AVL)
  if EliminarDeAVL(BandejaActual^.Borradores, BorradorID) then
  begin
    ShowMessage('Borrador enviado exitosamente y eliminado de la lista. Nuevo ID: ' + IntToStr(NuevoId));

    // 4. Actualizar matriz de relaciones
    IndiceRemitente := BuscarIndiceUsuario(UsuarioActual^.Email);
    IndiceDestinatario := BuscarIndiceUsuario(Destinatario);

    // Asumimos que BuscarIndiceUsuario devuelve el ID, por lo que usamos los IDs.
    if (IndiceRemitente <> -1) and (IndiceDestinatario <> -1) then
      InsertarValor(MatrizRelaciones, IndiceRemitente, IndiceDestinatario, 1);

    Close;
  end
  else
  begin
    ShowMessage('Error: El correo fue enviado (ID: ' + IntToStr(NuevoId) + '), pero no se pudo eliminar el borrador del AVL.');
    // Nota: El correo enviado ya tiene un nuevo ID, y no está vinculado al ID del borrador original.
  end;
end;

function TForm15.GenerarNuevoId: Integer;
begin
  Result := GenerarIdCorreo;
end;

function TForm15.BuscarIndiceUsuario(Email: string): Integer;
var
  Actual: PUsuario;
begin
  Actual := ListaUsuariosGlobal.Cabeza;

  while Actual <> nil do
  begin
    if Actual^.Email = Email then
      Exit(Actual^.Id); // Devolvemos el ID
    Actual := Actual^.Siguiente;
  end;

  Result := -1; // No encontrado
end;

function TForm15.ValidarDestinatario(Destinatario: string): Boolean;
var
  Contacto: PContacto;
begin
  Result := False;
  // 1. Verificar si el destinatario existe en el sistema
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, Destinatario) = nil then
  begin
    ShowMessage('Error: El destinatario no existe en el sistema');
    Exit;
  end;

  // 2. Verificar si está en los contactos del usuario actual
  if BandejaActual <> nil then
  begin
    Contacto := BuscarContactoPorEmail(BandejaActual^.Contactos, Destinatario);
    if Contacto = nil then
    begin
      ShowMessage('Error: El destinatario no está en sus contactos');
      Exit;
    end;
  end;

  Result := True;
end;

procedure TForm15.btnEnviarClick(Sender: TObject);
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

  if not ValidarDestinatario(Destinatario) then
    Exit;

  BandejaDestino := ObtenerBandejaUsuario(Destinatario);
  if BandejaDestino = nil then
    BandejaDestino := CrearBandejaUsuario(Destinatario);

  NuevoId := GenerarNuevoId;
  FechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // 1. Insertar correo en la bandeja del destinatario
  InsertarCorreo(BandejaDestino^.BandejaEntrada, NuevoId,
    UsuarioActual^.Email, Destinatario, 'N', False, Asunto, FechaActual, Mensaje);

  // 2. Eliminar de la lista de borradores (Árbol AVL)
  if EliminarDeAVL(BandejaActual^.Borradores, BorradorID) then
  begin
    ShowMessage('Borrador enviado exitosamente y eliminado de la lista.');

    // 3. Actualizar matriz de relaciones
    IndiceRemitente := BuscarIndiceUsuario(UsuarioActual^.Email);
    IndiceDestinatario := BuscarIndiceUsuario(Destinatario);

    if (IndiceRemitente <> -1) and (IndiceDestinatario <> -1) then
      InsertarValor(MatrizRelaciones, IndiceRemitente, IndiceDestinatario, 1);

    Close;
  end
  else
  begin
    ShowMessage('Error: El borrador fue enviado, pero no se pudo eliminar del AVL.');
  end;
end;

procedure TForm15.btnEliminarClick(Sender: TObject);
begin
  if (BandejaActual <> nil) and (BorradorID > 0) then
  begin
    if MessageDlg('Confirmar Eliminación',
                '¿Está seguro de que desea eliminar permanentemente este borrador?',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      if EliminarDeAVL(BandejaActual^.Borradores, BorradorID) then
      begin
        ShowMessage('Borrador eliminado permanentemente.');
        Close;
      end
      else
      begin
        ShowMessage('Error: No se pudo encontrar y eliminar el borrador.');
      end;
    end;
  end;
end;

end.
