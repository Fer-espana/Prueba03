unit UPapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UGLOBAL, UPilaPapelera, UListaDobleEnlazadaCorreos;

type

  { TForm11 }

  TForm11 = class(TForm)
    btnBuscarPorAsunto: TButton;
    btnEliminarCorreo: TButton;
    editBuscarCorreoPorAsunto: TEdit;
    tablaInformacionCorreo: TStringGrid;
    procedure btnBuscarPorAsuntoClick(Sender: TObject);
    procedure btnEliminarCorreoClick(Sender: TObject);
    procedure editBuscarCorreoPorAsuntoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionCorreoClick(Sender: TObject);
    procedure tablaInformacionCorreoEditingDone(Sender: TObject);
  private
    { private declarations }
    procedure ActualizarTablaPapelera;
    procedure BuscarPorAsunto(Asunto: string);
    function ObtenerCorreoSeleccionado: PCorreo;
  public
    { public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.lfm}

{ TForm11 }

procedure TForm11.FormCreate(Sender: TObject);
begin
  Caption := 'Papelera';

  // Configurar la tabla según el .lfm (3 columnas: Asunto, Remitente, Mensaje)
  // Las columnas ya están definidas en el .lfm, solo configuramos comportamiento
  tablaInformacionCorreo.Options := tablaInformacionCorreo.Options - [goEditing];

  // Configurar botones según el .lfm
  btnBuscarPorAsunto.Caption := 'Buscar';
  btnEliminarCorreo.Caption := 'Eliminar';
  editBuscarCorreoPorAsunto.TextHint := 'Buscar por asunto...';
end;

procedure TForm11.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm11.ActualizarTablaPapelera;
var
  NodoActual: PNodoPila;
  Fila: Integer;
begin
  // Limpiar tabla (mantener encabezados que ya están en el .lfm)
  tablaInformacionCorreo.RowCount := 1; // Fila 0 son los encabezados

  if PilaVacia(PilaPapeleraGlobal) then
    Exit;

  // Recorrer la pila y mostrar los correos
  NodoActual := PilaPapeleraGlobal.Cima;
  Fila := 1;

  while NodoActual <> nil do
  begin
    if NodoActual^.Correo <> nil then
    begin
      tablaInformacionCorreo.RowCount := tablaInformacionCorreo.RowCount + 1;

      // Usar las 3 columnas definidas en el .lfm: Asunto, Remitente, Mensaje
      tablaInformacionCorreo.Cells[0, Fila] := NodoActual^.Correo^.Asunto;
      tablaInformacionCorreo.Cells[1, Fila] := NodoActual^.Correo^.Remitente;

      // Mostrar solo un preview del mensaje (primeros 50 caracteres)
      if Length(NodoActual^.Correo^.Mensaje) > 50 then
        tablaInformacionCorreo.Cells[2, Fila] := Copy(NodoActual^.Correo^.Mensaje, 1, 50) + '...'
      else
        tablaInformacionCorreo.Cells[2, Fila] := NodoActual^.Correo^.Mensaje;

      Inc(Fila);
    end;
    NodoActual := NodoActual^.Siguiente;
  end;
end;

procedure TForm11.BuscarPorAsunto(Asunto: string);
var
  NodoActual: PNodoPila;
  Fila, Coincidencias: Integer;
  AsuntoBuscado: string;
begin
  if Asunto = '' then
  begin
    ActualizarTablaPapelera;
    Exit;
  end;

  // Limpiar tabla (mantener encabezados)
  tablaInformacionCorreo.RowCount := 1;

  if PilaVacia(PilaPapeleraGlobal) then
    Exit;

  AsuntoBuscado := UpperCase(Trim(Asunto));
  NodoActual := PilaPapeleraGlobal.Cima;
  Fila := 1;
  Coincidencias := 0;

  while NodoActual <> nil do
  begin
    if (NodoActual^.Correo <> nil) and
       (Pos(AsuntoBuscado, UpperCase(NodoActual^.Correo^.Asunto)) > 0) then
    begin
      tablaInformacionCorreo.RowCount := tablaInformacionCorreo.RowCount + 1;

      tablaInformacionCorreo.Cells[0, Fila] := NodoActual^.Correo^.Asunto;
      tablaInformacionCorreo.Cells[1, Fila] := NodoActual^.Correo^.Remitente;

      // Mostrar solo un preview del mensaje
      if Length(NodoActual^.Correo^.Mensaje) > 50 then
        tablaInformacionCorreo.Cells[2, Fila] := Copy(NodoActual^.Correo^.Mensaje, 1, 50) + '...'
      else
        tablaInformacionCorreo.Cells[2, Fila] := NodoActual^.Correo^.Mensaje;

      Inc(Fila);
      Inc(Coincidencias);
    end;
    NodoActual := NodoActual^.Siguiente;
  end;

  if Coincidencias = 0 then
    ShowMessage('No se encontraron correos con el asunto: ' + Asunto)
  else
    ShowMessage('Se encontraron ' + IntToStr(Coincidencias) + ' correo(s)');
end;

function TForm11.ObtenerCorreoSeleccionado: PCorreo;
var
  FilaSeleccionada: Integer;
  NodoActual: PNodoPila;
  Fila: Integer;
  AsuntoSeleccionado: string;
begin
  Result := nil;
  FilaSeleccionada := tablaInformacionCorreo.Row;

  if (FilaSeleccionada <= 0) or PilaVacia(PilaPapeleraGlobal) then
    Exit;

  // Obtener el asunto del correo seleccionado (columna 0)
  AsuntoSeleccionado := tablaInformacionCorreo.Cells[0, FilaSeleccionada];

  // Buscar el correo en la pila por asunto (puede haber duplicados, tomamos el primero)
  NodoActual := PilaPapeleraGlobal.Cima;
  Fila := 1;

  while NodoActual <> nil do
  begin
    if (NodoActual^.Correo <> nil) and (NodoActual^.Correo^.Asunto = AsuntoSeleccionado) then
    begin
      Result := NodoActual^.Correo;
      Exit;
    end;
    NodoActual := NodoActual^.Siguiente;
    Inc(Fila);
  end;
end;

procedure TForm11.btnBuscarPorAsuntoClick(Sender: TObject);
begin
  BuscarPorAsunto(Trim(editBuscarCorreoPorAsunto.Text));
end;

procedure TForm11.btnEliminarCorreoClick(Sender: TObject);
var
  CorreoSeleccionado: PCorreo;
begin
  CorreoSeleccionado := ObtenerCorreoSeleccionado;

  if CorreoSeleccionado = nil then
  begin
    ShowMessage('Seleccione un correo de la lista para eliminar permanentemente');
    Exit;
  end;

  if MessageDlg('Confirmar Eliminación',
                '¿Está seguro de que desea eliminar permanentemente este correo?' + sLineBreak +
                'Asunto: ' + CorreoSeleccionado^.Asunto + sLineBreak +
                'Esta acción no se puede deshacer.',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Aquí iría la lógica para eliminar permanentemente el correo
    // Por ahora, simplemente mostramos un mensaje
    ShowMessage('Funcionalidad de eliminación permanente en desarrollo');
    // En una implementación real, aquí se liberaría la memoria del correo
    // y se removería de la pila
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS
// =============================================================================

procedure TForm11.editBuscarCorreoPorAsuntoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm11.tablaInformacionCorreoClick(Sender: TObject);
begin
  // Evento vacío pero necesario
  // Podría usarse para habilitar/deshabilitar botones según selección
end;

procedure TForm11.tablaInformacionCorreoEditingDone(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
