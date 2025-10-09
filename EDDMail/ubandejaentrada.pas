unit UBandejaEntrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  UListaDobleEnlazadaCorreos;

type

  { TForm9 }

  TForm9 = class(TForm)
    btnOrdenAlfabetico: TButton;
    editNumeroNoLeidos: TEdit;
    tablaInformacion: TStringGrid;
    procedure btnOrdenAlfabeticoClick(Sender: TObject);
    procedure editNumeroNoLeidosChange(Sender: TObject);
    procedure editNumeroNoLeidosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionClick(Sender: TObject);
    procedure tablaInformacionDblClick(Sender: TObject);
  private
    ListaCorreos: TListaCorreos;
    procedure ActualizarTabla;
  public
  end;

var
  Form9: TForm9;

implementation

{$R *.lfm}

{ TForm9 }

procedure TForm9.FormCreate(Sender: TObject);
begin
  Caption := 'Bandeja de Entrada';

  // Inicializar lista de correos
  InicializarListaCorreos(ListaCorreos);

  // Configurar tabla
  tablaInformacion.ColCount := 3;
  tablaInformacion.RowCount := 1;
  tablaInformacion.Cells[0, 0] := 'Estado';
  tablaInformacion.Cells[1, 0] := 'Asunto';
  tablaInformacion.Cells[2, 0] := 'Remitente';

  // Agregar algunos correos de ejemplo
  InsertarCorreo(ListaCorreos, 1, 'remitente@ejemplo.com', 'N', False,
    'Bienvenida', '2024-01-01', 'Mensaje de bienvenida');

  ActualizarTabla;
end;

procedure TForm9.ActualizarTabla;
var
  i: Integer;
  Correo: PCorreo;
begin
  // Limpiar tabla (excepto encabezados)
  tablaInformacion.RowCount := 1;

  // Contador de no leídos
  editNumeroNoLeidos.Text := '0';

  // Llenar tabla con correos
  for i := 0 to ListaCorreos.Count - 1 do
  begin
    Correo := ObtenerCorreoPorPosicion(ListaCorreos, i);
    if Correo <> nil then
    begin
      tablaInformacion.RowCount := tablaInformacion.RowCount + 1;
      tablaInformacion.Cells[0, i + 1] := Correo^.Estado;
      tablaInformacion.Cells[1, i + 1] := Correo^.Asunto;
      tablaInformacion.Cells[2, i + 1] := Correo^.Remitente;

      // Contar no leídos
      if Correo^.Estado = 'N' then
        editNumeroNoLeidos.Text := IntToStr(StrToInt(editNumeroNoLeidos.Text) + 1);
    end;
  end;
end;

procedure TForm9.btnOrdenAlfabeticoClick(Sender: TObject);
begin
  ShowMessage('Ordenando por asunto...');
  // Aquí irá la lógica de ordenamiento
end;

procedure TForm9.editNumeroNoLeidosChange(Sender: TObject);
begin

end;

procedure TForm9.editNumeroNoLeidosClick(Sender: TObject);
begin

end;

procedure TForm9.FormDestroy(Sender: TObject);
begin

end;

procedure TForm9.tablaInformacionClick(Sender: TObject);
begin

end;

procedure TForm9.tablaInformacionDblClick(Sender: TObject);
begin

end;

end.

