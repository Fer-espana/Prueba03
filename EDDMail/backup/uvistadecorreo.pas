unit UVistadeCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UListaDobleEnlazadaCorreos, UGLOBAL, UPilaPapelera, UArbolB;

type

  { TForm10 }

  TForm10 = class(TForm)
    btnEliminarCorreo: TButton;
    btnFavorito: TButton;
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
    procedure btnFavoritoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CorreoActual: PCorreo;
    BandejaActual: PBandejaUsuario;
    procedure NotificarFormularioPrincipal;
  public
    procedure SetCorreoActual(Correo: PCorreo; Bandeja: PBandejaUsuario);
  end;

var
  Form10: TForm10;

implementation

// Unidades movidas a la implementación para evitar referencias circulares
uses UUsuarioEstandar, UFavoritos;

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

    // Opcional: Deshabilitar el botón si ya es favorito
    if BuscarEnArbolB(BandejaActual^.Favoritos, CorreoActual^.Id) <> nil then
      btnFavorito.Enabled := False
    else
      btnFavorito.Enabled := True;
  end;
end;

procedure TForm10.NotificarFormularioPrincipal;
var
  i: Integer;
  FormUsuario: TForm3;
begin
  // Busca TForm3 (UUsuarioEstandar)
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TForm3 then
    begin
      FormUsuario := (Screen.Forms[i] as TForm3);
      // Llama al método RefrescarDatos, que ahora es public en TForm3
      FormUsuario.RefrescarDatos;
      Break; // Salir después de encontrar el formulario
    end;
  end;
end;


procedure TForm10.btnEliminarCorreoClick(Sender: TObject);
var
  CorreoCopia: PCorreo;
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
      // Crear una COPIA del correo para la papelera
      New(CorreoCopia);
      CorreoCopia^.Id := CorreoActual^.Id;
      CorreoCopia^.Remitente := CorreoActual^.Remitente;
      CorreoCopia^.Destinatario := CorreoActual^.Destinatario;
      CorreoCopia^.Estado := 'E'; // Eliminado
      CorreoCopia^.Programado := CorreoActual^.Programado;
      CorreoCopia^.Asunto := CorreoActual^.Asunto;
      CorreoCopia^.Fecha := CorreoActual^.Fecha;
      CorreoCopia^.Mensaje := CorreoActual^.Mensaje;
      CorreoCopia^.Anterior := nil;
      CorreoCopia^.Siguiente := nil;

      // Mover a la papelera global
      Apilar(PilaPapeleraGlobal, CorreoCopia);

      // Eliminar el correo original de la bandeja de entrada
      EliminarCorreo(BandejaActual^.BandejaEntrada, CorreoActual^.Id);

      ShowMessage('Correo eliminado correctamente y movido a la papelera');

      // Notificar al formulario principal para refrescar la lista de correos
      NotificarFormularioPrincipal;

      Close; // Cerrar el formulario de vista
    except
      on E: Exception do
      begin
        ShowMessage('Error al eliminar el correo: ' + E.Message);
        if CorreoCopia <> nil then
          Dispose(CorreoCopia);
      end;
    end;
  end;
end;

procedure TForm10.btnFavoritoClick(Sender: TObject);
var
  BandejaPropia: PBandejaUsuario;
  CorreoCopia: TCorreo;
begin
  if (CorreoActual = nil) then Exit;

  BandejaPropia := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if BandejaPropia = nil then
  begin
    BandejaPropia := CrearBandejaUsuario(UsuarioActual^.Email);
  end;

  // 1. Verificar si ya está en favoritos
  if BuscarEnArbolB(BandejaPropia^.Favoritos, CorreoActual^.Id) <> nil then
  begin
    ShowMessage('Este correo ya está marcado como Favorito.');
    (Sender as TButton).Enabled := False;
    Exit;
  end;

  // 2. Crear una copia por valor del correo para el nodo del Árbol B
  CorreoCopia := CorreoActual^;

  // 3. Insertar en el Árbol B de Favoritos
  if UArbolB.InsertarEnArbolB(BandejaPropia^.Favoritos, CorreoCopia.Id, CorreoCopia) then
  begin
    ShowMessage('Correo marcado como favorito (Árbol B) con ID: ' + IntToStr(CorreoCopia.Id));

    // Notificar al formulario principal para refrescar (para que se actualice el contador de Favoritos)
    NotificarFormularioPrincipal;

    (Sender as TButton).Enabled := False;
  end
  else
    ShowMessage('Error al insertar en Favoritos.');
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
  // No liberamos CorreoActual porque es parte de la lista global
end;

end.
