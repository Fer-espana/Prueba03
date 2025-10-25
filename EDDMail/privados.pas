unit Privados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  privado, Usuarios;

type

  { TFormPrivados }

  TFormPrivados = class(TForm)
    Bbuscar: TButton;
    BEliminar: TButton;
    Lasunto: TLabel;
    Lfecha: TLabel;
    LRemitente: TLabel;
    Mmensaje: TMemo;
    PanelCorreo: TPanel;
    Label1: TLabel;
    Lcantidad: TLabel;
    Mcorreos: TMemo;
    TEbuscar: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BbuscarClick(Sender: TObject);
    procedure BEliminarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Declaraciones privadas }
    UltimoPrivadoEncontrado: TPrivado;

    procedure MostrarDetallesPrivado(APrivado: TPrivado);
    procedure LimpiarDetalles;

    // Retorna el árbol del usuario logueado
    function GetMerkleTreeInstance: TMerkleTree;
  public
    { Declaraciones públicas }
    property MerkleTreeInstance: TMerkleTree read GetMerkleTreeInstance;
    procedure RefrescarListaYContador;
    procedure AgregarPrivadoExterno(ARemitente, AAsunto, AFecha, AMensaje: String);
    function ExisteCorreoPrivado(const AAsunto: String): Boolean;
  end;

var
  FormPrivados: TFormPrivados;

implementation

{$R *.lfm}

{ TFormPrivados }

// Implementación del Getter - Retorna el árbol del usuario logueado
function TFormPrivados.GetMerkleTreeInstance: TMerkleTree;
begin
  Result := nil;

  // Verificar que hay un usuario logueado
  if UsuarioLogueado = nil then
  begin
    ShowMessage('Error: No hay un usuario logueado.');
    Exit;
  end;

  // Verificar que el árbol Merkle del usuario está inicializado
  if UsuarioLogueado^.privados = nil then
  begin
    ShowMessage('Error: El árbol Merkle de privados no está inicializado.');
    Exit;
  end;

  Result := UsuarioLogueado^.privados;
end;

// Evento que se ejecuta al mostrar el formulario
procedure TFormPrivados.FormShow(Sender: TObject);
begin
  // Verificar que hay usuario logueado
  if UsuarioLogueado = nil then
  begin
    ShowMessage('Error: Debe iniciar sesión primero.');
    Close;
    Exit;
  end;

  // Verificar que el árbol Merkle está inicializado
  if UsuarioLogueado^.privados = nil then
  begin
    ShowMessage('Error: El árbol de correos privados no está inicializado.');
    Close;
    Exit;
  end;

  // Refrescar la lista al mostrar el formulario
  RefrescarListaYContador;
end;

procedure TFormPrivados.RefrescarListaYContador;
var
  i: Integer;
  Node: TMerkleNode;
  PrivadoItem: TPrivado;
  MerkleTree: TMerkleTree;
begin
  // Obtener el árbol Merkle del usuario logueado
  MerkleTree := GetMerkleTreeInstance;

  // Limpiar el Memo de correos y el contador
  Mcorreos.Clear;
  Lcantidad.Caption := '0';

  // Verificar que el árbol esté disponible
  if not Assigned(MerkleTree) then
  begin
    LimpiarDetalles;
    Exit;
  end;

  // Recorrer el árbol usando LeavesCount y GetLeaf
  if MerkleTree.LeavesCount > 0 then
  begin
    for i := 0 to MerkleTree.LeavesCount - 1 do
    begin
      Node := MerkleTree.GetLeaf(i);

      if Assigned(Node) and Assigned(Node.Privado) then
      begin
        PrivadoItem := Node.Privado;
        // Mostrar en el Memo el índice, remitente y asunto
        Mcorreos.Lines.Add('[' + IntToStr(i + 1) + '] De: ' + PrivadoItem.Remitente + ' | Asunto: ' + PrivadoItem.Asunto);
      end;
    end;

    // Actualizar el contador
    Lcantidad.Caption := IntToStr(MerkleTree.LeavesCount);
  end
  else
  begin
    // Si no hay correos privados
    Mcorreos.Lines.Add('No hay correos privados.');
    Lcantidad.Caption := '0';
  end;

  // Limpiar los detalles al refrescar la lista
  LimpiarDetalles;
end;

procedure TFormPrivados.MostrarDetallesPrivado(APrivado: TPrivado);
begin
  if Assigned(APrivado) then
  begin
    LRemitente.Caption := 'De: ' + APrivado.Remitente;
    Lasunto.Caption := 'Asunto: ' + APrivado.Asunto;
    Lfecha.Caption := 'Fecha: ' + APrivado.Fecha;
    Mmensaje.Text := APrivado.Mensaje;
  end;
end;

procedure TFormPrivados.LimpiarDetalles;
begin
  LRemitente.Caption := 'De: ---';
  Lasunto.Caption := 'Asunto: ---';
  Lfecha.Caption := 'Fecha: ---';
  Mmensaje.Clear;
  UltimoPrivadoEncontrado := nil;
end;

procedure TFormPrivados.FormCreate(Sender: TObject);
begin
  // Solo limpiar la interfaz
  LimpiarDetalles;
  Mcorreos.Clear;
  Lcantidad.Caption := '0';
end;

procedure TFormPrivados.BbuscarClick(Sender: TObject);
var
  AsuntoBuscado: string;
  i: Integer;
  Node: TMerkleNode;
  FoundPrivado: TPrivado;
  MerkleTree: TMerkleTree;
begin
  // Obtener el árbol del usuario logueado
  MerkleTree := GetMerkleTreeInstance;
  if not Assigned(MerkleTree) then
    Exit;

  AsuntoBuscado := Trim(TEbuscar.Text);
  LimpiarDetalles;
  FoundPrivado := nil;

  if AsuntoBuscado = '' then
  begin
    ShowMessage('Por favor, ingrese un asunto para buscar.');
    Exit;
  end;

  // Recorrer las hojas para encontrar el privado por Asunto
  for i := 0 to MerkleTree.LeavesCount - 1 do
  begin
    Node := MerkleTree.GetLeaf(i);
    if Assigned(Node) and Assigned(Node.Privado) and (LowerCase(Node.Privado.Asunto) = LowerCase(AsuntoBuscado)) then
    begin
      FoundPrivado := Node.Privado;
      Break;
    end;
  end;

  if Assigned(FoundPrivado) then
  begin
    MostrarDetallesPrivado(FoundPrivado);
    UltimoPrivadoEncontrado := FoundPrivado;
    ShowMessage('Privado encontrado por Asunto: ' + AsuntoBuscado);
  end
  else
  begin
    ShowMessage('No se encontró ningún privado con el asunto: ' + AsuntoBuscado);
  end;
end;

procedure TFormPrivados.BEliminarClick(Sender: TObject);
var
  MerkleTree: TMerkleTree;
begin
  // Obtener el árbol del usuario logueado
  MerkleTree := GetMerkleTreeInstance;
  if not Assigned(MerkleTree) then
    Exit;

  if not Assigned(UltimoPrivadoEncontrado) then
  begin
    ShowMessage('Primero debe buscar y seleccionar un privado para poder eliminarlo.');
    Exit;
  end;

  if MessageDlg('¿Está seguro de que desea eliminar el privado con Asunto: ' + UltimoPrivadoEncontrado.Asunto + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if MerkleTree.Delete(UltimoPrivadoEncontrado) then
    begin
      ShowMessage('Privado eliminado exitosamente. El Árbol Merkle ha sido reconstruido.');
    end
    else
    begin
      ShowMessage('Error: No se pudo encontrar y eliminar el privado.');
    end;

    LimpiarDetalles;
    TEbuscar.Clear;
    RefrescarListaYContador;
  end;
end;

procedure TFormPrivados.AgregarPrivadoExterno(ARemitente, AAsunto, AFecha, AMensaje: String);
var
  MerkleTree: TMerkleTree;
begin
  MerkleTree := GetMerkleTreeInstance;
  if not Assigned(MerkleTree) then
  begin
    ShowMessage('Error: No se puede agregar el privado. Árbol no inicializado.');
    Exit;
  end;

  try
    MerkleTree.Insert(ARemitente, AAsunto, AFecha, AMensaje);
    ShowMessage('Correo privado agregado exitosamente.');
    RefrescarListaYContador;
  except
    on E: Exception do
      ShowMessage('Error al agregar el correo privado: ' + E.Message);
  end;
end;

function TFormPrivados.ExisteCorreoPrivado(const AAsunto: String): Boolean;
var
  MerkleTree: TMerkleTree;
begin
  Result := False;
  MerkleTree := GetMerkleTreeInstance;

  if Assigned(MerkleTree) then
    Result := MerkleTree.ExisteAsunto(AAsunto);
end;

end.
