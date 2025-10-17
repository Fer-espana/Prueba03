unit UFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UListaDobleEnlazadaCorreos, UGLOBAL, UPapelera, UArbolB,
  UVistadeFavorito, Process; // UVistadeFavorito es TForm18

type

  { TForm17 } // <--- TForm17

  TForm17 = class(TForm)
    btnGenerarReporte: TButton;
    editNumeroFavoritos: TEdit;
    tablaInformacion: TStringGrid;
    Label1: TLabel; // Para 'Total Favoritos:'
    procedure btnGenerarReporteClick(Sender: TObject);
    procedure editNumeroFavoritosChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionClick(Sender: TObject);
    procedure tablaInformacionDblClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure ActualizarTabla;
    procedure GenerarReporteFavoritos;
    function ObtenerIDSeleccionado: Integer;
    procedure RecorrerArbolBParaTabla(Nodo: PNodoB; var Fila: Integer);
    procedure NotificarPapeleraSiEstaAbierta; // Reutilizado de BandejaEntrada
  public
    procedure RefrescarDatos;
  end;

var
  Form17: TForm17;

implementation

{$R *.lfm}

{ TForm17 }

procedure TForm17.FormCreate(Sender: TObject);
begin
  Caption := 'Correos Favoritos (Árbol B)';

  // Configurar tabla
  tablaInformacion.ColCount := 3;
  tablaInformacion.RowCount := 1;
  tablaInformacion.Cells[0, 0] := 'ID';
  tablaInformacion.Cells[1, 0] := 'Asunto';
  tablaInformacion.Cells[2, 0] := 'Remitente';
  tablaInformacion.ColWidths[0] := 50;
  tablaInformacion.ColWidths[1] := 250;
  tablaInformacion.ColWidths[2] := 150;

  tablaInformacion.Options := tablaInformacion.Options - [goEditing];

  btnGenerarReporte.Caption := 'Generar Reporte (Árbol B)';
  Label1.Caption := 'Total Favoritos:';
end;

procedure TForm17.RefrescarDatos;
begin
  BandejaActual := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if BandejaActual = nil then
    BandejaActual := CrearBandejaUsuario(UsuarioActual^.Email);

  ActualizarTabla;
end;

// Recorrido In-Orden para listar los correos del Árbol B por ID
procedure TForm17.RecorrerArbolBParaTabla(Nodo: PNodoB; var Fila: Integer);
var
  i: Integer;
begin
  if Nodo = nil then Exit;

  // Recorrido In-Orden
  for i := 0 to Nodo^.ContadorClaves - 1 do
  begin
    if not Nodo^.EsHoja then
      RecorrerArbolBParaTabla(Nodo^.Hijos[i], Fila);

    // Procesar la clave (TCorreo)
    tablaInformacion.RowCount := tablaInformacion.RowCount + 1;
    tablaInformacion.Cells[0, Fila] := IntToStr(Nodo^.Claves[i].ID);
    tablaInformacion.Cells[1, Fila] := Nodo^.Claves[i].Correo.Asunto;
    tablaInformacion.Cells[2, Fila] := Nodo^.Claves[i].Correo.Remitente;
    tablaInformacion.Objects[0, Fila] := TObject(Nodo^.Claves[i].ID); // Guardar ID
    Inc(Fila);
  end;

  // Recorrer el último hijo
  if not Nodo^.EsHoja then
    RecorrerArbolBParaTabla(Nodo^.Hijos[Nodo^.ContadorClaves], Fila);
end;


procedure TForm17.ActualizarTabla;
var
  Fila: Integer;
begin
  tablaInformacion.RowCount := 1;
  Fila := 1;

  if (BandejaActual = nil) or (BandejaActual^.Favoritos.Raiz = nil) then
  begin
    editNumeroFavoritos.Text := '0';
    Exit;
  end;

  RecorrerArbolBParaTabla(BandejaActual^.Favoritos.Raiz, Fila);

  // Actualizar contador
  editNumeroFavoritos.Text := IntToStr(Fila - 1);
end;

function TForm17.ObtenerIDSeleccionado: Integer;
var
  FilaSeleccionada: Integer;
begin
  Result := -1;
  FilaSeleccionada := tablaInformacion.Row;

  if (FilaSeleccionada > 0) and (FilaSeleccionada < tablaInformacion.RowCount) then
    Result := Integer(tablaInformacion.Objects[0, FilaSeleccionada]);
end;

procedure TForm17.tablaInformacionDblClick(Sender: TObject);
var
  IDSeleccionado: Integer;
  Correo: PCorreo;
  FormVista: TForm18; // <--- TForm18
begin
  IDSeleccionado := ObtenerIDSeleccionado;

  if IDSeleccionado <> -1 then
  begin
    // Buscar la referencia al correo en el Árbol B
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
