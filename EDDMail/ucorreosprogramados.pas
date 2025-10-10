unit UCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UGLOBAL, UColaCorreosProgramados, UListaDobleEnlazadaCorreos, UListaSimpleUsuarios;

type

  { TForm12 }

  TForm12 = class(TForm)
    btnEnviarCorreosProgramados: TButton;
    tablaCorreosProgramados: TStringGrid;
    procedure btnEnviarCorreosProgramadosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaCorreosProgramadosClick(Sender: TObject);
  private
    { private declarations }
    procedure ActualizarTablaCorreosProgramados;
    procedure EnviarCorreosProgramados;
    function ObtenerCorreoSeleccionado: PCorreo;
  public
    { public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.lfm}

{ TForm12 }

procedure TForm12.FormCreate(Sender: TObject);
begin
  Caption := 'Correos Programados';

  // Configurar la tabla según el .lfm (3 columnas: Asunto, Remitente, Fecha de Envio)
  tablaCorreosProgramados.Options := tablaCorreosProgramados.Options - [goEditing];

  btnEnviarCorreosProgramados.Caption := 'Enviar';
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
  Fila: Integer;
begin
  // Limpiar tabla (mantener encabezados que ya están en el .lfm)
  tablaCorreosProgramados.RowCount := 1;

  if ColaVacia(ColaCorreosProgramados) then
  begin
    Exit;
  end;

  // Recorrer la cola y mostrar los correos programados
  NodoActual := ColaCorreosProgramados.Frente;
  Fila := 1;

  while NodoActual <> nil do
  begin
    if NodoActual^.Correo <> nil then
    begin
      tablaCorreosProgramados.RowCount := tablaCorreosProgramados.RowCount + 1;

      // Usar las 3 columnas definidas en el .lfm: Asunto, Remitente, Fecha de Envio
      tablaCorreosProgramados.Cells[0, Fila] := NodoActual^.Correo^.Asunto;
      tablaCorreosProgramados.Cells[1, Fila] := NodoActual^.Correo^.Remitente;
      tablaCorreosProgramados.Cells[2, Fila] := NodoActual^.Correo^.Fecha;

      Inc(Fila);
    end;
    NodoActual := NodoActual^.Siguiente;
  end;
end;

function TForm12.ObtenerCorreoSeleccionado: PCorreo;
var
  FilaSeleccionada: Integer;
  NodoActual: PNodoCola;
  AsuntoSeleccionado: string;
begin
  Result := nil;
  FilaSeleccionada := tablaCorreosProgramados.Row;

  if (FilaSeleccionada <= 0) or ColaVacia(ColaCorreosProgramados) then
    Exit;

  // Obtener el asunto del correo seleccionado
  AsuntoSeleccionado := tablaCorreosProgramados.Cells[0, FilaSeleccionada];

  // Buscar el correo en la cola por asunto
  NodoActual := ColaCorreosProgramados.Frente;

  while NodoActual <> nil do
  begin
    if (NodoActual^.Correo <> nil) and (NodoActual^.Correo^.Asunto = AsuntoSeleccionado) then
    begin
      Result := NodoActual^.Correo;
      Exit;
    end;
    NodoActual := NodoActual^.Siguiente;
  end;
end;

procedure TForm12.EnviarCorreosProgramados;
var
  CorreoProgramado: PCorreo;
  BandejaDestino: PBandejaUsuario;
  CorreoSeleccionado: PCorreo;
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

  // Crear una copia del correo para enviar
  New(CorreoProgramado);
  try
    CorreoProgramado^.Id := CorreoSeleccionado^.Id;
    CorreoProgramado^.Remitente := CorreoSeleccionado^.Remitente;
    CorreoProgramado^.Destinatario := CorreoSeleccionado^.Destinatario;
    CorreoProgramado^.Asunto := CorreoSeleccionado^.Asunto;
    CorreoProgramado^.Mensaje := CorreoSeleccionado^.Mensaje;
    CorreoProgramado^.Fecha := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now); // Fecha actual
    CorreoProgramado^.Estado := 'N'; // 'N' para No leído
    CorreoProgramado^.Programado := False;
    CorreoProgramado^.Anterior := nil;
    CorreoProgramado^.Siguiente := nil;

    // Insertar en la bandeja de entrada del destinatario
    InsertarCorreo(BandejaDestino^.BandejaEntrada,
      CorreoProgramado^.Id,
      CorreoProgramado^.Remitente,
      CorreoProgramado^.Destinatario,
      CorreoProgramado^.Estado,
      CorreoProgramado^.Programado,
      CorreoProgramado^.Asunto,
      CorreoProgramado^.Fecha,
      CorreoProgramado^.Mensaje);

    ShowMessage('Correo enviado exitosamente a: ' + CorreoSeleccionado^.Destinatario);

    // Actualizar la tabla
    ActualizarTablaCorreosProgramados;

  except
    on E: Exception do
    begin
      ShowMessage('Error al enviar correo: ' + E.Message);
      Dispose(CorreoProgramado);
    end;
  end;
end;

procedure TForm12.btnEnviarCorreosProgramadosClick(Sender: TObject);
begin
  if ColaVacia(ColaCorreosProgramados) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  if MessageDlg('Confirmar Envío',
                '¿Está seguro de que desea enviar el correo seleccionado?' + sLineBreak +
                'Asunto: ' + ObtenerCorreoSeleccionado^.Asunto,
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    EnviarCorreosProgramados;
  end;
end;

procedure TForm12.tablaCorreosProgramadosClick(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
