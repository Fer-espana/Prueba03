unit UPapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UGLOBAL, UPilaPapelera, UListaDobleEnlazadaCorreos;

type

  { TForm11 }

  TForm11 = class(TForm)
    btnBuscarPorAsunto: TButton;
    btnEliminarCorreo: TButton;
    editBuscarCorreoPorAsunto: TEdit;
    ListView1: TListView;
    procedure btnBuscarPorAsuntoClick(Sender: TObject);
    procedure btnEliminarCorreoClick(Sender: TObject);
    procedure editBuscarCorreoPorAsuntoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
  private
    { private declarations }
    procedure ActualizarListView;
    procedure BuscarPorAsunto(Asunto: string);
    function ObtenerCorreoSeleccionado: PCorreo;
  public
    { public declarations }
    procedure RefrescarPapelera;
  end;

var
  Form11: TForm11;

implementation

{$R *.lfm}

{ TForm11 }

procedure TForm11.FormCreate(Sender: TObject);
begin
  Caption := 'Papelera';

  // Configurar ListView
  with ListView1 do
  begin
    ViewStyle := vsReport;
    GridLines := True;
    ReadOnly := True;
    // Agregar columnas
    Columns.Add.Caption := 'Asunto';
    Columns.Add.Caption := 'Remitente';
    Columns.Add.Caption := 'Fecha';
    Columns.Add.Caption := 'Preview Mensaje';
    // Ajustar anchos de columnas
    Columns[0].Width := 150;
    Columns[1].Width := 120;
    Columns[2].Width := 100;
    Columns[3].Width := 200;
  end;

  // Configurar botones
  btnBuscarPorAsunto.Caption := 'Buscar';
  btnEliminarCorreo.Caption := 'Eliminar Permanentemente';
  editBuscarCorreoPorAsunto.TextHint := 'Buscar por asunto...';
end;

procedure TForm11.FormShow(Sender: TObject);
begin
  ActualizarListView;
end;

procedure TForm11.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm11.RefrescarPapelera;
begin
  ActualizarListView;
end;

procedure TForm11.ActualizarListView;
var
  NodoActual: PNodoPila;
  ListItem: TListItem;
begin
  ListView1.Items.Clear;

  if PilaVacia(PilaPapeleraGlobal) then
    Exit;

  NodoActual := PilaPapeleraGlobal.Cima;

  while NodoActual <> nil do
  begin
    if NodoActual^.Correo <> nil then
    begin
      ListItem := ListView1.Items.Add;
      ListItem.Caption := NodoActual^.Correo^.Asunto;
      ListItem.SubItems.Add(NodoActual^.Correo^.Remitente);
      ListItem.SubItems.Add(NodoActual^.Correo^.Fecha);

      // Mostrar solo un preview del mensaje (primeros 30 caracteres)
      if Length(NodoActual^.Correo^.Mensaje) > 30 then
        ListItem.SubItems.Add(Copy(NodoActual^.Correo^.Mensaje, 1, 30) + '...')
      else
        ListItem.SubItems.Add(NodoActual^.Correo^.Mensaje);

      // Guardar referencia al correo en el Data del ListItem
      ListItem.Data := NodoActual^.Correo;
    end;
    NodoActual := NodoActual^.Siguiente;
  end;
end;

procedure TForm11.BuscarPorAsunto(Asunto: string);
var
  NodoActual: PNodoPila;
  ListItem: TListItem;
  AsuntoBuscado: string;
  Coincidencias: Integer;
begin
  ListView1.Items.Clear;
  Coincidencias := 0;

  if PilaVacia(PilaPapeleraGlobal) then
    Exit;

  AsuntoBuscado := UpperCase(Trim(Asunto));
  NodoActual := PilaPapeleraGlobal.Cima;

  while NodoActual <> nil do
  begin
    if (NodoActual^.Correo <> nil) and
       (Pos(AsuntoBuscado, UpperCase(NodoActual^.Correo^.Asunto)) > 0) then
    begin
      ListItem := ListView1.Items.Add;
      ListItem.Caption := NodoActual^.Correo^.Asunto;
      ListItem.SubItems.Add(NodoActual^.Correo^.Remitente);
      ListItem.SubItems.Add(NodoActual^.Correo^.Fecha);

      if Length(NodoActual^.Correo^.Mensaje) > 30 then
        ListItem.SubItems.Add(Copy(NodoActual^.Correo^.Mensaje, 1, 30) + '...')
      else
        ListItem.SubItems.Add(NodoActual^.Correo^.Mensaje);

      ListItem.Data := NodoActual^.Correo;
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
begin
  Result := nil;
  if (ListView1.Selected <> nil) then
    Result := PCorreo(ListView1.Selected.Data);
end;

procedure TForm11.btnBuscarPorAsuntoClick(Sender: TObject);
begin
  BuscarPorAsunto(Trim(editBuscarCorreoPorAsunto.Text));
end;

procedure TForm11.btnEliminarCorreoClick(Sender: TObject);
var
  CorreoSeleccionado: PCorreo;
  TempPila: TPila;
  NodoActual: PNodoPila;
  CorreoEncontrado: Boolean;
begin
  CorreoSeleccionado := ObtenerCorreoSeleccionado;

  if CorreoSeleccionado = nil then
  begin
    ShowMessage('Seleccione un correo de la lista para eliminar permanentemente');
    Exit;
  end;

  if MessageDlg('Confirmar Eliminación Permanente',
                '¿Está seguro de que desea eliminar PERMANENTEMENTE este correo?' + sLineBreak +
                'Asunto: ' + CorreoSeleccionado^.Asunto + sLineBreak +
                'Remitente: ' + CorreoSeleccionado^.Remitente + sLineBreak +
                'Esta acción NO se puede deshacer.',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    // INICIALIZAR TempPila explícitamente
    InicializarPila(TempPila);
    CorreoEncontrado := False;

    NodoActual := PilaPapeleraGlobal.Cima;

    while NodoActual <> nil do
    begin
      // Solo apilar los correos que NO son el seleccionado
      if NodoActual^.Correo <> CorreoSeleccionado then
      begin
        Apilar(TempPila, NodoActual^.Correo);
      end
      else
      begin
        CorreoEncontrado := True;
        // Liberar la memoria del correo eliminado
        Dispose(NodoActual^.Correo);
      end;
      NodoActual := NodoActual^.Siguiente;
    end;

    if CorreoEncontrado then
    begin
      // Liberar la pila original y asignar la temporal
      LiberarPila(PilaPapeleraGlobal); // Solo libera nodos, no correos
      PilaPapeleraGlobal := TempPila;

      ShowMessage('Correo eliminado permanentemente');
      ActualizarListView;
    end
    else
    begin
      ShowMessage('Error: No se pudo encontrar el correo para eliminar');
      LiberarPila(TempPila); // Liberar la pila temporal si no se usó
    end;
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS
// =============================================================================

procedure TForm11.editBuscarCorreoPorAsuntoChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm11.ListView1Click(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
