unit FBitacora;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  DateUtils,
  Bitacora;

type

  { TFFbitacora }

  TFFbitacora = class(TForm)
    Button1: TButton;
    BVisualizar: TButton;
    Label1: TLabel;
    MemoBitacora: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure BVisualizarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure CargarBitacoraEnMemo;
  public

  end;

var
  FFbitacora: TFFbitacora;

implementation

{$R *.lfm}

procedure TFFbitacora.CargarBitacoraEnMemo;
var
  actual: PAcceso;
  linea: string;
begin
  // 1. Limpiar el Memo
  MemoBitacora.Lines.Clear;

  // 2. Añadir Encabezados
  linea := Format('%-20s | %-22s | %s', ['USUARIO', 'ENTRADA', 'SALIDA']);
  MemoBitacora.Lines.Add(linea);
  MemoBitacora.Lines.Add(StringOfChar('-', Length(linea))); // Separador

  // 3. Recorrer la Lista Simple (LogAccesos)
  actual := LogAccesos.primero;

  while actual <> nil do
  begin
    // Formato de fecha para la Entrada y Salida
    linea := actual^.usuario;

    // Obtener la hora de salida
    if actual^.salida = 0 then
      // Sesión abierta
      linea := Format('%-20s | %-22s | %s',
                      [actual^.usuario,
                       FormatDateTime('yyyy-mm-dd hh:nn:ss', actual^.entrada),
                       'ACTIVA'])
    else
      // Sesión cerrada
      linea := Format('%-20s | %-22s | %s',
                      [actual^.usuario,
                       FormatDateTime('yyyy-mm-dd hh:nn:ss', actual^.entrada),
                       FormatDateTime('yyyy-mm-dd hh:nn:ss', actual^.salida)]);

    MemoBitacora.Lines.Add(linea);
    actual := actual^.siguiente;
  end;
end;


procedure TFFbitacora.FormCreate(Sender: TObject);
begin
  CargarBitacoraEnMemo;
end;

procedure TFFbitacora.BVisualizarClick(Sender: TObject);
begin
  CargarBitacoraEnMemo;
end;

procedure TFFbitacora.Button1Click(Sender: TObject);
begin

  ExportarBitacoraJson(LogAccesos, 'Log_Accesos.json');
end;

end.
