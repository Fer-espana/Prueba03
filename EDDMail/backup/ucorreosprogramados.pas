unit UCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UGLOBAL, UColaCorreosProgramados, UListaDobleEnlazadaCorreos, UListaSimpleUsuarios;

type

  { TForm12 }

  TForm12 = class(TForm)
    btnEnviarCorreosProgramados: TButton;
    ListView1: TListView;
    procedure btnEnviarCorreosProgramadosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
  private
    { private declarations }
    procedure ActualizarTablaCorreosProgramados;
    procedure EnviarCorreoSeleccionado;
    function ObtenerCorreoSeleccionado: PCorreo;
    function GenerarNuevoId: Integer;
    function DesencolarCorreoPorId(Id: Integer): Boolean; // NUEVO MÉTODO
  public
    { public declarations }
    procedure RefrescarTabla;
  end;

var
  Form12: TForm12;

implementation

{$R *.lfm}

{ TForm12 }

procedure TForm12.FormCreate(Sender: TObject);
begin
  Caption := 'Correos Programados';

  // Configurar el ListView
  with ListView1 do
  begin
    ViewStyle := vsReport;
    GridLines := True;
    ReadOnly := True;
    // Agregar columnas
    Columns.Add.Caption := 'Asunto';
    Columns.Add.Caption := 'Remitente';
    Columns.Add.Caption := 'Destinatario'; // NUEVA COLUMNA
    Columns.Add.Caption := 'Fecha Programada';
    // Ajustar anchos de columnas
    Columns[0].Width := 150;
    Columns[1].Width := 120;
    Columns[2].Width := 120;
    Columns[3].Width := 120;
  end;

  btnEnviarCorreosProgramados.Caption := 'Enviar Seleccionado';
end;

procedure TForm12.RefrescarTabla;
begin
  ActualizarTablaCorreosProgramados;
end;

procedure TForm12.FormShow(Sender: TObject);
begin
  ActualizarTablaCorreosProgramados;
end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

procedure TForm12.ActualizarTablaCorreosProgramados;
var
  NodoActual: PNodoCola;
  ListItem: TListItem;
begin
  ListView1.Items.Clear;

  if ColaVacia(ColaCorreosProgramados) then
    Exit;

  NodoActual := ColaCorreosProgramados.Frente;

  while NodoActual <> nil do
  begin
    if NodoActual^.Correo <> nil then
    begin
      ListItem := ListView1.Items.Add;
      ListItem.Caption := NodoActual^.Correo^.Asunto;
      ListItem.SubItems.Add(NodoActual^.Correo^.Remitente);
      ListItem.SubItems.Add(NodoActual^.Correo^.Destinatario); // NUEVA COLUMNA
      ListItem.SubItems.Add(NodoActual^.Correo^.Fecha);
      ListItem.Data := NodoActual^.Correo; // Guardar referencia al correo
    end;
    NodoActual := NodoActual^.Siguiente;
  end;
end;

function TForm12.ObtenerCorreoSeleccionado: PCorreo;
begin
  Result := nil;
  if (ListView1.Selected <> nil) then
    Result := PCorreo(ListView1.Selected.Data);
end;

function TForm12.GenerarNuevoId: Integer;
var
  MaxId: Integer;
  BandejaUsuario: PBandejaUsuario;
  CorreoActual: PCorreo;
  NodoActual: PNodoCola;
begin
  MaxId := 0;

  // Buscar el ID máximo en todas las bandejas
  BandejaUsuario := ListaBandejas;
  while BandejaUsuario <> nil do
  begin
    CorreoActual := BandejaUsuario^.BandejaEntrada.Cabeza;
    while CorreoActual <> nil do
    begin
      if CorreoActual^.Id > MaxId then
        MaxId := CorreoActual^.Id;
      CorreoActual := CorreoActual^.Siguiente;
    end;
    BandejaUsuario := BandejaUsuario^.Siguiente;
  end;

  // Buscar también en los correos programados
  if not ColaVacia(ColaCorreosProgramados) then
  begin
    NodoActual := ColaCorreosProgramados.Frente;
    while NodoActual <> nil do
    begin
      if (NodoActual^.Correo <> nil) and (NodoActual^.Correo^.Id > MaxId) then
        MaxId := NodoActual^.Correo^.Id;
      NodoActual := NodoActual^.Siguiente;
    end;
  end;

  Result := MaxId + 1;
end;

// NUEVO MÉTODO: Desencolar un correo específico por ID
function TForm12.DesencolarCorreoPorId(Id: Integer): Boolean;
var
  TempPila: TPila;
  NodoActual: PNodoCola;
  CorreoEncontrado: Boolean;
begin
  Result := False;
  if ColaVacia(ColaCorreosProgramados) then Exit;

  // Inicializar pila temporal
  InicializarCola(TempPila);
  CorreoEncontrado := False;

  NodoActual := ColaCorreosProgramados.Frente;

  while NodoActual <> nil do
  begin
    if (NodoActual^.Correo <> nil) and (NodoActual^.Correo^.Id = Id) then
    begin
      // Encontrado, no lo encolamos en la temporal (lo eliminamos)
      CorreoEncontrado := True;
      // Liberar memoria del correo
      Dispose(NodoActual^.Correo);
    end
    else
    begin
      // Encolar en la temporal
      Encolar(TempPila, NodoActual^.Correo);
    end;
    NodoActual := NodoActual^.Siguiente;
  end;

  if CorreoEncontrado then
  begin
    // Reemplazar la cola original con la temporal
    LiberarCola(ColaCorreosProgramados); // Solo libera nodos, no correos
    ColaCorreosProgramados := TempPila;
    Result := True;
  end
  else
  begin
    // Liberar la cola temporal si no se usó
    LiberarCola(TempPila);
  end;
end;

procedure TForm12.EnviarCorreoSeleccionado;
var
  CorreoSeleccionado: PCorreo;
  BandejaDestino: PBandejaUsuario;
  NuevoId: Integer;
  FechaActual: string;
  CorreoEnviado: PCorreo;
begin
  // Obtener el correo seleccionado
  CorreoSeleccionado := ObtenerCorreoSeleccionado;

  if CorreoSeleccionado = nil then
  begin
    ShowMessage('Seleccione un correo de la lista para enviar');
    Exit;
  end;

  // Obtener o crear bandeja del destinatario
  BandejaDestino := ObtenerBandejaUsuario(CorreoSeleccionado^.Destinatario);
  if BandejaDestino = nil then
    BandejaDestino := CrearBandejaUsuario(CorreoSeleccionado^.Destinatario);

  // Generar nuevo ID y fecha actual
  NuevoId := GenerarNuevoId;
  FechaActual := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Crear una copia del correo para enviar (no usar el mismo objeto)
  New(CorreoEnviado);
  try
    CorreoEnviado^.Id := NuevoId;
    CorreoEnviado^.Remitente := CorreoSeleccionado^.Remitente;
    CorreoEnviado^.Destinatario := CorreoSeleccionado^.Destinatario;
    CorreoEnviado^.Asunto := CorreoSeleccionado^.Asunto;
    CorreoEnviado^.Mensaje := CorreoSeleccionado^.Mensaje;
    CorreoEnviado^.Fecha := FechaActual;
    CorreoEnviado^.Estado := 'N'; // No leído
    CorreoEnviado^.Programado := False;
    CorreoEnviado^.Anterior := nil;
    CorreoEnviado^.Siguiente := nil;

    // Insertar correo en la bandeja del destinatario
    InsertarCorreo(BandejaDestino^.BandejaEntrada,
      CorreoEnviado^.Id,
      CorreoEnviado^.Remitente,
      CorreoEnviado^.Destinatario,
      CorreoEnviado^.Estado,
      CorreoEnviado^.Programado,
      CorreoEnviado^.Asunto,
      CorreoEnviado^.Fecha,
      CorreoEnviado^.Mensaje);

    // DESENCOLAR el correo programado después de enviarlo
    if DesencolarCorreoPorId(CorreoSeleccionado^.Id) then
    begin
      ShowMessage('Correo enviado exitosamente a: ' + CorreoSeleccionado^.Destinatario);

      // Actualizar la tabla
      ActualizarTablaCorreosProgramados;
    end
    else
    begin
      ShowMessage('Error: No se pudo remover el correo de la cola de programados');
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Error al enviar correo: ' + E.Message);
      Dispose(CorreoEnviado);
    end;
  end;
end;

procedure TForm12.btnEnviarCorreosProgramadosClick(Sender: TObject);
var
  CorreoSeleccionado: PCorreo;
begin
  if ColaVacia(ColaCorreosProgramados) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  CorreoSeleccionado := ObtenerCorreoSeleccionado;
  if CorreoSeleccionado = nil then
  begin
    ShowMessage('Seleccione un correo de la lista para enviar');
    Exit;
  end;

  if MessageDlg('Confirmar Envío',
                '¿Está seguro de que desea enviar el correo seleccionado?' + sLineBreak +
                'Asunto: ' + CorreoSeleccionado^.Asunto + sLineBreak +
                'Destinatario: ' + CorreoSeleccionado^.Destinatario,
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    EnviarCorreoSeleccionado;
  end;
end;

procedure TForm12.ListView1Click(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
