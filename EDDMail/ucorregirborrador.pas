unit UCorregirBorrador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaSimpleUsuarios, UListaDobleEnlazadaCorreos,
  UListaCircularContactos, UMatrizDispersaRelaciones, UPilaPapelera; // Importa UPilaPapelera para eliminar

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
    // Referencia al correo en el nodo AVL para facilitar la copia
    CorreoBorradorActual: PCorreo;
    procedure CargarBorrador;
    function ValidarDestinatario(Destinatario: string): Boolean;
    function GenerarNuevoId: Integer;
    function BuscarIndiceUsuario(Email: string): Integer;
  public
    // Establece el ID del borrador y la Bandeja del usuario actual
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
    // BuscarEnAVL devuelve el PCorreo dentro del nodo AVL
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

function TForm15.GenerarNuevoId: Integer;
begin
  // Llama a la función global para asegurar un ID único
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
      Exit(Actual^.Id);
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

  // 1. Generar nuevo ID para el correo ENVIADO
  NuevoId := GenerarNuevoId;
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

    if (IndiceRemitente <> -1) and (IndiceDestinatario <> -1) then
      InsertarValor(MatrizRelaciones, IndiceRemitente, IndiceDestinatario, 1);

    Close;
  end
  else
  begin
    ShowMessage('Error: El correo fue enviado (ID: ' + IntToStr(NuevoId) + '), pero no se pudo eliminar el borrador del AVL.');
  end;
end;

procedure TForm15.btnEliminarClick(Sender: TObject);
var
  CorreoCopia: PCorreo;
begin
  if (BandejaActual <> nil) and (BorradorID > 0) then
  begin
    if MessageDlg('Confirmar Eliminación',
                '¿Está seguro de que desea eliminar este borrador y enviarlo a la Papelera?',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      // 1. Crear una copia del correo antes de la eliminación del AVL
      if CorreoBorradorActual <> nil then
      begin
        New(CorreoCopia);
        CorreoCopia^ := CorreoBorradorActual^; // Copiar contenido del borrador
        CorreoCopia^.Estado := 'E'; // Marcar como Eliminado

        // 2. Eliminar de la lista de borradores (Árbol AVL)
        if EliminarDeAVL(BandejaActual^.Borradores, BorradorID) then
        begin
          // 3. Mover la copia a la papelera global (Pila)
          Apilar(PilaPapeleraGlobal, CorreoCopia);

          ShowMessage('Borrador eliminado y movido a la Papelera.');
          Close;
        end
        else
        begin
          ShowMessage('Error: No se pudo encontrar y eliminar el borrador.');
          Dispose(CorreoCopia); // Liberar la copia si la eliminación falló
        end;
      end
      else
      begin
        ShowMessage('Error: No se pudo obtener la referencia del borrador.');
      end;
    end;
  end;
end;

end.
