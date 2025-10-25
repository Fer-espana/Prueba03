unit UFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UListaDobleEnlazadaCorreos, UGLOBAL, UPapelera, UArbolB,
  UVistadeFavorito, Process; // UVistadeFavorito es TForm18

type

  { TForm17 } // <--- TForm17

  TForm17 = class(TForm)
    btnGenerarReporte: TButton;
    editNumeroFavoritos: TEdit;
    listViewFavoritos: TListView; // <--- AÑADIDO
    Label1: TLabel; // Para 'Total Favoritos:'
    procedure btnGenerarReporteClick(Sender: TObject);
    procedure editNumeroFavoritosChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaInformacionClick(Sender: TObject);
    procedure tablaInformacionDblClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure ActualizarTabla;
    procedure ConfigurarListView;
    procedure GenerarReporteFavoritos;
    function ObtenerIDSeleccionado: Integer;
    procedure RecorrerArbolBParaTabla(Nodo: PNodoB; var Fila: Integer);
    procedure NotificarPapeleraSiEstaAbierta;
  public
    procedure RefrescarDatos; // <-- Necesario para la notificación externa
  end;

var
  Form17: TForm17;

implementation

{$R *.lfm}

{ TForm17 }

procedure TForm17.ConfigurarListView;
// ... (código existente)
begin
  with listViewFavoritos do
  begin
    ViewStyle := vsReport;
    ReadOnly := True;
    RowSelect := True;
    Columns.Clear;

    // Configurar columnas
    Columns.Add.Caption := 'ID';
    Columns.Add.Caption := 'Asunto';
    Columns.Add.Caption := 'Remitente';

    Columns[0].Width := 50;
    Columns[1].Width := 250;
    Columns[2].Width := 150;
  end;
end;


procedure TForm17.FormCreate(Sender: TObject);
begin
  Caption := 'Correos Favoritos (Árbol B)';

  // Configurar TListView (llamando al nuevo procedimiento)
  ConfigurarListView;

  btnGenerarReporte.Caption := 'Generar Reporte (Árbol B)';
  Label1.Caption := 'Total Favoritos:';
end;

procedure TForm17.FormShow(Sender: TObject);
begin
  RefrescarDatos;
end;

procedure TForm17.RefrescarDatos;
begin
  BandejaActual := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if BandejaActual = nil then
    BandejaActual := CrearBandejaUsuario(UsuarioActual^.Email);

  ActualizarTabla; // Llama a la lógica de recarga
end;

procedure TForm17.RecorrerArbolBParaTabla(Nodo: PNodoB; var Fila: Integer);
// ... (código existente)
var
  i: Integer;
  Item: TListItem; // Objeto para TListView
begin
  if Nodo = nil then Exit;

  // Recorrido In-Orden:
  for i := 0 to Nodo^.ContadorClaves - 1 do
  begin
    // 1. Recorrer el hijo (solo si no es hoja)
    if not Nodo^.EsHoja then
      RecorrerArbolBParaTabla(Nodo^.Hijos[i], Fila);

    // 2. Procesar la clave (TCorreo)
    Item := listViewFavoritos.Items.Add;
    Item.Caption := IntToStr(Nodo^.Claves[i].ID); // Columna 0
    Item.SubItems.Add(Nodo^.Claves[i].Correo.Asunto); // Columna 1
    Item.SubItems.Add(Nodo^.Claves[i].Correo.Remitente); // Columna 2

    // Guardar el ID en TListItem.Data usando PtrInt
    Item.Data := Pointer(PtrInt(Nodo^.Claves[i].ID));
    Inc(Fila);
  end;

  // 3. Recorrer el último hijo (solo si no es hoja)
  if not Nodo^.EsHoja then
    RecorrerArbolBParaTabla(Nodo^.Hijos[Nodo^.ContadorClaves], Fila);
end;


procedure TForm17.ActualizarTabla;
// ... (código existente)
var
  Fila: Integer;
begin
  listViewFavoritos.Items.Clear; // Limpiar TListView
  Fila := 0; // Fila comienza en 0 para TListView.Items.Count

  if (BandejaActual = nil) or (BandejaActual^.Favoritos.Raiz = nil) then
  begin
    editNumeroFavoritos.Text := '0';
    Exit;
  end;

  RecorrerArbolBParaTabla(BandejaActual^.Favoritos.Raiz, Fila);

  // Actualizar contador
  editNumeroFavoritos.Text := IntToStr(listViewFavoritos.Items.Count);
end;

function TForm17.ObtenerIDSeleccionado: Integer;
var
  Item: TListItem;
begin
  Result := -1;
  Item := listViewFavoritos.Selected; // Obtener el ítem seleccionado

  if Assigned(Item) then
    // Recuperar el ID guardado en TListItem.Data
    Result := PtrInt(Item.Data);
end;

procedure TForm17.tablaInformacionDblClick(Sender: TObject);
var
  IDSeleccionado: Integer;
  Correo: PCorreo; // PCorreo es el tipo correcto para recibir el puntero
  FormVista: TForm18; // <--- TForm18
begin
  IDSeleccionado := ObtenerIDSeleccionado;

  if IDSeleccionado <> -1 then
  begin
    // CORRECCIÓN: Se espera que BuscarEnArbolB devuelva un PCorreo
    Correo := BuscarEnArbolB(BandejaActual^.Favoritos, IDSeleccionado);

    if Correo <> nil then
    begin
      FormVista := TForm18.Create(Application);
      // Pasamos la referencia del TCorreo dentro del Árbol B.
      FormVista.SetCorreoActual(Correo, BandejaActual);
      FormVista.ShowModal;
      FormVista.Free;

      ActualizarTabla;
      NotificarPapeleraSiEstaAbierta;
    end;
  end;
end;

procedure TForm17.GenerarReporteFavoritos;
var
  RutaCarpeta: string;
  RutaArchivoDOT, RutaArchivoPNG: string;
  Proc: TProcess;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) +
                 StringReplace(UsuarioActual^.Email, '@', '-', [rfReplaceAll]) +
                 '-Reportes';

  ForceDirectories(RutaCarpeta);
  RutaArchivoDOT := RutaCarpeta + PathDelim + 'favoritos.dot';
  RutaArchivoPNG := RutaCarpeta + PathDelim + 'favoritos.png';

  GenerarReporteDOTArbolB(BandejaActual^.Favoritos, RutaArchivoDOT);

  if FileExists(RutaArchivoDOT) then
  begin
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := 'dot';
      Proc.Parameters.Add('-Tpng');
      Proc.Parameters.Add(RutaArchivoDOT);
      Proc.Parameters.Add('-o');
      Proc.Parameters.Add(RutaArchivoPNG);
      Proc.Options := [poWaitOnExit];
      Proc.Execute;
    finally
      Proc.Free;
    end;
  end;

  ShowMessage('Reporte DOT y PNG de Favoritos generado.');
end;

procedure TForm17.btnGenerarReporteClick(Sender: TObject);
begin
  GenerarReporteFavoritos;
end;

procedure TForm17.NotificarPapeleraSiEstaAbierta;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TForm11 then
    begin
      // Asume que TForm11 tiene el método RefrescarPapelera
      (Screen.Forms[i] as TForm11).RefrescarPapelera;
    end;
  end;
end;

// Eventos vacíos
procedure TForm17.editNumeroFavoritosChange(Sender: TObject); begin end;
procedure TForm17.tablaInformacionClick(Sender: TObject); begin end;
procedure TForm17.FormClose(Sender: TObject; var CloseAction: TCloseAction); begin CloseAction := caFree; end;
procedure TForm17.FormDestroy(Sender: TObject); begin end;

end.
