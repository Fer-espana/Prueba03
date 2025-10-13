unit UBandejaEntrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UListaDobleEnlazadaCorreos, UGLOBAL, UVistadeCorreo;

type

  { TForm9 }

  TForm9 = class(TForm)
    btnOrdenAlfabetico: TButton;
    editNumeroNoLeidos: TEdit;
    tablaInformacion: TStringGrid;
    procedure btnOrdenAlfabeticoClick(Sender: TObject);
    procedure editNumeroNoLeidosChange(Sender: TObject);
    procedure editNumeroNoLeidosClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionClick(Sender: TObject);
    procedure tablaInformacionDblClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure ActualizarTabla;
    procedure OrdenarPorAsunto;
    function ContarCorreosNoLeidos: Integer;
  public
    procedure SetBandejaActual(Email: string);
  end;

var
  Form9: TForm9;

implementation

{$R *.lfm}

{ TForm9 }

procedure TForm9.FormCreate(Sender: TObject);
begin
  Caption := 'Bandeja de Entrada';

  // Configurar tabla
  tablaInformacion.ColCount := 3;
  tablaInformacion.RowCount := 1;
  tablaInformacion.Cells[0, 0] := 'Estado';
  tablaInformacion.Cells[1, 0] := 'Asunto';
  tablaInformacion.Cells[2, 0] := 'Remitente';
  tablaInformacion.ColWidths[0] := 80;
  tablaInformacion.ColWidths[1] := 250;
  tablaInformacion.ColWidths[2] := 150;

  // Hacer la tabla de solo lectura
  tablaInformacion.Options := tablaInformacion.Options - [goEditing];
end;

procedure TForm9.SetBandejaActual(Email: string);
begin
  BandejaActual := ObtenerBandejaUsuario(Email);
  if BandejaActual = nil then
    BandejaActual := CrearBandejaUsuario(Email);

  ActualizarTabla;
end;

procedure TForm9.ActualizarTabla;
var
  i: Integer;
  Correo: PCorreo;
  EstadoStr: string;
begin
  // Limpiar tabla (excepto encabezados)
  tablaInformacion.RowCount := 1;

  if (BandejaActual = nil) or (BandejaActual^.BandejaEntrada.Count = 0) then
  begin
    editNumeroNoLeidos.Text := '0';
    Exit;
  end;

  // Llenar tabla con correos
  for i := 0 to BandejaActual^.BandejaEntrada.Count - 1 do
  begin
    Correo := ObtenerCorreoPorPosicion(BandejaActual^.BandejaEntrada, i);
    if Correo <> nil then
    begin
      tablaInformacion.RowCount := tablaInformacion.RowCount + 1;

      // Convertir estado a texto legible
      case Correo^.Estado of
        'N': EstadoStr := 'NO LEIDO';
        'L': EstadoStr := 'LEIDO';
        'E': EstadoStr := 'ELIMINADO';
        else EstadoStr := 'DESCONOCIDO';
      end;

      tablaInformacion.Cells[0, i + 1] := EstadoStr;
      tablaInformacion.Cells[1, i + 1] := Correo^.Asunto;
      tablaInformacion.Cells[2, i + 1] := Correo^.Remitente;
    end;
  end;

  // Actualizar contador de no leídos
  editNumeroNoLeidos.Text := IntToStr(ContarCorreosNoLeidos);
end;

function TForm9.ContarCorreosNoLeidos: Integer;
var
  i: Integer;
  Correo: PCorreo;
begin
  Result := 0;
  if (BandejaActual = nil) then Exit;

  for i := 0 to BandejaActual^.BandejaEntrada.Count - 1 do
  begin
    Correo := ObtenerCorreoPorPosicion(BandejaActual^.BandejaEntrada, i);
    if (Correo <> nil) and (Correo^.Estado = 'N') then
      Inc(Result);
  end;
end;

procedure TForm9.OrdenarPorAsunto;
var
  i, j: Integer;
  TempCorreo: TCorreo;
  Lista: TListaCorreos;
begin
  if (BandejaActual = nil) or (BandejaActual^.BandejaEntrada.Count < 2) then
    Exit;

  Lista := BandejaActual^.BandejaEntrada;

  // Ordenamiento burbuja simple por asunto (A-Z)
  for i := 0 to Lista.Count - 2 do
  begin
    for j := 0 to Lista.Count - 2 - i do
    begin
      if CompareText(
        ObtenerCorreoPorPosicion(Lista, j)^.Asunto,
        ObtenerCorreoPorPosicion(Lista, j + 1)^.Asunto) > 0 then
      begin
        // Intercambiar correos
        TempCorreo := ObtenerCorreoPorPosicion(Lista, j)^;
        ObtenerCorreoPorPosicion(Lista, j)^ := ObtenerCorreoPorPosicion(Lista, j + 1)^;
        ObtenerCorreoPorPosicion(Lista, j + 1)^ := TempCorreo;
      end;
    end;
  end;

  ActualizarTabla;
  ShowMessage('Correos ordenados por asunto (A-Z)');
end;

procedure TForm9.btnOrdenAlfabeticoClick(Sender: TObject);
begin
  OrdenarPorAsunto;
end;

procedure TForm9.tablaInformacionDblClick(Sender: TObject);
var
  FilaSeleccionada: Integer;
  Correo: PCorreo;
  FormVista: TForm10;
begin
  FilaSeleccionada := tablaInformacion.Row;

  // Validar que se seleccionó una fila válida (no el encabezado)
  if (FilaSeleccionada > 0) and (BandejaActual <> nil) then
  begin
    // Obtener el correo seleccionado (restar 1 porque la fila 0 es el encabezado)
    Correo := ObtenerCorreoPorPosicion(BandejaActual^.BandejaEntrada, FilaSeleccionada - 1);

    if Correo <> nil then
    begin
      // Marcar como leído si no lo está
      if Correo^.Estado = 'N' then
      begin
        Correo^.Estado := 'L';
        ActualizarTabla; // Actualizar la tabla para reflejar el cambio de estado
      end;

      // Abrir formulario de vista de correo
      FormVista := TForm10.Create(Application);
      FormVista.SetCorreoActual(Correo, BandejaActual);
      FormVista.ShowModal;
      FormVista.Free;

      // Actualizar la tabla después de cerrar la vista (por si se eliminó el correo)
      ActualizarTabla;

      // Notificar a la papelera para que se actualice si está abierta
      NotificarPapeleraSiEstaAbierta;
    end;
  end;
end;

// Agregar este procedimiento en la sección private de UBandejaEntrada.pas
procedure TForm9.NotificarPapeleraSiEstaAbierta;
var
  i: Integer;
begin
  // Buscar si el formulario de papelera está abierto y actualizarlo
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TForm11 then
    begin
      (Screen.Forms[i] as TForm11).RefrescarPapelera;
    end;
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS PARA EVITAR ERRORES
// =============================================================================

procedure TForm9.editNumeroNoLeidosChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm9.editNumeroNoLeidosClick(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm9.tablaInformacionClick(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm9.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  // No liberamos BandejaActual porque es global
end;

end.
