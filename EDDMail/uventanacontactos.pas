unit UVentanaContactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  UGLOBAL, UListaCircularContactos;

type

  { TForm13 }

  TForm13 = class(TForm)
    btnContactoAnterior: TButton;
    btnContactoSiguiente: TButton;
    tablaContacto: TStringGrid;
    procedure btnContactoAnteriorClick(Sender: TObject);
    procedure btnContactoSiguienteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ContactoActual: PContacto;
    BandejaUsuario: PBandejaUsuario;
    procedure MostrarContactoActual;
    procedure ActualizarBotonesNavegacion;
  public
  end;

var
  Form13: TForm13;

implementation

{$R *.lfm}

{ TForm13 }

procedure TForm13.FormCreate(Sender: TObject);
begin
  Caption := 'Mis Contactos';
  // Configurar tabla
  tablaContacto.ColCount := 4;
  tablaContacto.RowCount := 2; // Encabezado + 1 fila de datos

  // INICIALIZAR CABECERAS EN CÓDIGO
  tablaContacto.Cells[0, 0] := 'Nombre';
  tablaContacto.Cells[1, 0] := 'Usuario';
  tablaContacto.Cells[2, 0] := 'Correo';
  tablaContacto.Cells[3, 0] := 'Teléfono';

  // Configurar anchos de columnas
  tablaContacto.ColWidths[0] := 100;
  tablaContacto.ColWidths[1] := 100;
  tablaContacto.ColWidths[2] := 150;
  tablaContacto.ColWidths[3] := 100;

  // Hacer tabla de solo lectura
  tablaContacto.Options := tablaContacto.Options - [goEditing];
end;

procedure TForm13.FormShow(Sender: TObject);
begin
  if UsuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario actual');
    Close;
    Exit;
  end;

  // Obtener bandeja del usuario
  BandejaUsuario := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if (BandejaUsuario = nil) or (BandejaUsuario^.Contactos.Cabeza = nil) then
  begin
    ShowMessage('No tiene contactos agregados');
    // Limpiar tabla
    tablaContacto.RowCount := 1;
    Exit;
  end;

  // Iniciar con el primer contacto
  ContactoActual := BandejaUsuario^.Contactos.Cabeza;
  MostrarContactoActual;
  ActualizarBotonesNavegacion;
end;

procedure TForm13.MostrarContactoActual;
begin
  if ContactoActual = nil then
  begin
    // Limpiar tabla si no hay contactos
    tablaContacto.RowCount := 1;
    Exit;
  end;

  // Mostrar datos del contacto actual
  tablaContacto.Cells[0, 1] := ContactoActual^.Nombre;
  tablaContacto.Cells[1, 1] := ContactoActual^.Email; // Usar email como "usuario"
  tablaContacto.Cells[2, 1] := ContactoActual^.Email;
  tablaContacto.Cells[3, 1] := ContactoActual^.Telefono;
end;

procedure TForm13.ActualizarBotonesNavegacion;
var
  TotalContactos: Integer;
begin
  TotalContactos := BandejaUsuario^.Contactos.Count;

  // Habilitar/deshabilitar botones según posición
  btnContactoAnterior.Enabled := (TotalContactos > 1);
  btnContactoSiguiente.Enabled := (TotalContactos > 1);

  // Actualizar título con posición actual
  if TotalContactos > 0 then
    Caption := Format('Mis Contactos (%d de %d)', [1, TotalContactos])
  else
    Caption := 'Mis Contactos (0 contactos)';
end;

procedure TForm13.btnContactoAnteriorClick(Sender: TObject);
var
  Temp: PContacto; // DECLARACIÓN CORRECTA EN SECCIÓN VAR
begin
  if (ContactoActual = nil) or (BandejaUsuario^.Contactos.Count <= 1) then
    Exit;

  // Navegar al contacto anterior en la lista circular
  // En una lista circular, el anterior es el último nodo antes de completar el círculo
  if ContactoActual^.Siguiente = BandejaUsuario^.Contactos.Cabeza then
  begin
    // Estamos en el primer contacto, ir al último
    ContactoActual := BandejaUsuario^.Contactos.Cabeza;
    while ContactoActual^.Siguiente <> BandejaUsuario^.Contactos.Cabeza do
      ContactoActual := ContactoActual^.Siguiente;
  end
  else
  begin
    // Buscar el nodo anterior (esto es ineficiente en lista circular simple)
    // Para mejor rendimiento, considerar hacer la lista doblemente enlazada circular
    Temp := BandejaUsuario^.Contactos.Cabeza; // SIN 'var' AQUÍ
    while (Temp <> nil) and (Temp^.Siguiente <> ContactoActual) do
      Temp := Temp^.Siguiente;

    if Temp <> nil then
      ContactoActual := Temp;
  end;

  MostrarContactoActual;
end;

procedure TForm13.btnContactoSiguienteClick(Sender: TObject);
begin
  if (ContactoActual = nil) or (BandejaUsuario^.Contactos.Count <= 1) then
    Exit;

  // Navegar al siguiente contacto (comportamiento circular)
  ContactoActual := ContactoActual^.Siguiente;
  MostrarContactoActual;
end;

procedure TForm13.FormDestroy(Sender: TObject);
begin
  // No liberar ContactoActual ya que pertenece a la lista global
end;

end.
