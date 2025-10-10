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

  // La tabla ya está configurada en el .lfm con 3 columnas: Asunto, Remitente, Fecha de Envio
  // Solo configuramos comportamiento
  tablaCorreosProgramados.Options := tablaCorreosProgramados.Options - [goEditing];

  // Configurar botón según el .lfm
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
  tablaCorreosProgramados.RowCount := 1; // Fila 0 son los encabezados

  if ColaVacia(ColaCorreosProgramados) then
  begin
    // No mostrar mensaje aquí para no ser intrusivo
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

procedure TForm12.EnviarCorreosProgramados;
var
  CorreoProgramado: PCorreo;
  BandejaDestino: PBandejaUsuario;
  CorreosEnviados: Integer;
begin
  if ColaVacia(ColaCorreosProgramados) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  CorreosEnviados := 0;

  // Procesar todos los correos programados en la cola
  while not ColaVacia(ColaCorreosProgramados) do
  begin
    CorreoProgramado := Desencolar(ColaCorreosProgramados);

    if CorreoProgramado <> nil then
    begin
      // Obtener o crear bandeja del destinatario
      BandejaDestino := ObtenerBandejaUsuario(CorreoProgramado^.Destinatario);
      if BandejaDestino = nil then
        BandejaDestino := CrearBandejaUsuario(CorreoProgramado^.Destinatario);

      // Cambiar estado a "No leído" y quitar programación
      CorreoProgramado^.Estado := 'N';
      CorreoProgramado^.Programado := False;

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

      Inc(CorreosEnviados);

      // Liberar el correo original de la cola
      Dispose(CorreoProgramado);
    end;
  end;

  ShowMessage('Se enviaron ' + IntToStr(CorreosEnviados) + ' correo(s) programados');
  ActualizarTablaCorreosProgramados;
end;

procedure TForm12.btnEnviarCorreosProgramadosClick(Sender: TObject);
begin
  if ColaVacia(ColaCorreosProgramados) then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  if MessageDlg('Confirmar Envío',
                '¿Está seguro de que desea enviar todos los correos programados?' + sLineBreak +
                'Esta acción enviará ' + IntToStr(ColaCorreosProgramados.Count) + ' correo(s).',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    EnviarCorreosProgramados;
  end;
end;

// =============================================================================
// EVENTOS VACÍOS PERO NECESARIOS
// =============================================================================

procedure TForm12.tablaCorreosProgramadosClick(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
