unit UVerBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, ComCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaDobleEnlazadaCorreos, UCorregirBorrador; // <--- UCorregirBorrador para TForm15

type

  { TForm16 } // <--- NUEVO NÚMERO DE FORMULARIO: 16

  TForm16 = class(TForm)
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    MemoRecorrido: TMemo;
    tablaBorradores: TStringGrid;
    Label1: TLabel;
    btnModificarEnviar: TButton;
    procedure btnPreOrdenClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaBorradoresDblClick(Sender: TObject);
    procedure btnModificarEnviarClick(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure CargarTablaDesdeAVL;
    procedure CargarTablaRecursivo(Nodo: PNodeAVL; var Fila: Integer);
    procedure MostrarRecorrido(TipoRecorrido: string);
    function ObtenerIDSeleccionado: Integer;
    procedure RecorrerInOrdenParaTabla(Nodo: PNodeAVL; var Fila: Integer); // Solo In-Orden para la tabla
  public
    procedure RefrescarDatos;
  end;

var
  Form16: TForm16;

implementation

{$R *.lfm}

{ TForm16 }

procedure TForm16.FormCreate(Sender: TObject);
begin
  Caption := 'Borradores de Mensajes (Árbol AVL)';

  tablaBorradores.ColCount := 4;
  tablaBorradores.RowCount := 1;
  tablaBorradores.Cells[0, 0] := 'ID';
  tablaBorradores.Cells[1, 0] := 'Asunto';
  tablaBorradores.Cells[2, 0] := 'Destinatario';
  tablaBorradores.Cells[3, 0] := 'Fecha';
  tablaBorradores.ColWidths[0] := 50;
  tablaBorradores.ColWidths[1] := 150;
  tablaBorradores.ColWidths[2] := 150;
  tablaBorradores.ColWidths[3] := 120;
  tablaBorradores.Options := tablaBorradores.Options - [goEditing];

  Label1.Caption := 'Recorrido del Árbol AVL:';
  btnModificarEnviar.Caption := 'Modificar/Enviar Borrador';
end;

procedure TForm16.FormShow(Sender: TObject);
begin
  RefrescarDatos;
end;

procedure TForm16.RefrescarDatos;
begin
  BandejaActual := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if BandejaActual = nil then
  begin
    BandejaActual := CrearBandejaUsuario(UsuarioActual^.Email); // Crear si no existe
  end;

  CargarTablaDesdeAVL;
  MemoRecorrido.Lines.Clear;

  // Mostrar el recorrido In-Orden por defecto
  MostrarRecorrido('In-Orden');
end;

// Recorrido In-Orden para la tabla de visualización
procedure TForm16.RecorrerInOrdenParaTabla(Nodo: PNodeAVL; var Fila: Integer);
begin
  if Nodo = nil then Exit;

  RecorrerInOrdenParaTabla(Nodo^.Left, Fila);

  tablaBorradores.RowCount := tablaBorradores.RowCount + 1;
  tablaBorradores.Cells[0, Fila] := IntToStr(Nodo^.Key);
  tablaBorradores.Cells[1, Fila] := Nodo^.Correo.Asunto;
  tablaBorradores.Cells[2, Fila] := Nodo^.Correo.Destinatario;
  tablaBorradores.Cells[3, Fila] := Nodo^.Correo.Fecha;

  // Guardar la clave (ID) en la columna 0, fila Fila para el doble clic
  tablaBorradores.Objects[0, Fila] := TObject(Nodo^.Key);

  Inc(Fila);

  RecorrerInOrdenParaTabla(Nodo^.Right, Fila);
end;


procedure TForm16.CargarTablaDesdeAVL;
var
  Fila: Integer;
begin
  tablaBorradores.RowCount := 1;
  Fila := 1;
  RecorrerInOrdenParaTabla(BandejaActual^.Borradores.Root, Fila);

  if tablaBorradores.RowCount = 1 then
    tablaBorradores.RowCount := 2; // Asegurar al menos una fila visible si está vacía
end;

function TForm16.ObtenerIDSeleccionado: Integer;
var
  FilaSeleccionada: Integer;
begin
  Result := -1;
  FilaSeleccionada := tablaBorradores.Row;

  if (FilaSeleccionada > 0) and (FilaSeleccionada < tablaBorradores.RowCount) then
  begin
    Result := Integer(tablaBorradores.Objects[0, FilaSeleccionada]);
  end;
end;

// Implementación de recorrido (usa la función en UAVLTreeBorradores.pas)
procedure TForm16.MostrarRecorrido(TipoRecorrido: string);
var
  RecorridoStr: string;
begin
  RecorridoStr := ObtenerRecorridoAVL(BandejaActual^.Borradores, TipoRecorrido);
  MemoRecorrido.Lines.Clear;
  MemoRecorrido.Lines.Add('Recorrido ' + TipoRecorrido + ':');
  MemoRecorrido.Lines.Add(RecorridoStr);
end;

procedure TForm16.btnPreOrdenClick(Sender: TObject);
begin
  MostrarRecorrido('Pre-Orden');
end;

procedure TForm16.btnInOrdenClick(Sender: TObject);
begin
  MostrarRecorrido('In-Orden');
end;

procedure TForm16.btnPostOrdenClick(Sender: TObject);
begin
  MostrarRecorrido('Post-Orden');
end;

procedure TForm16.tablaBorradoresDblClick(Sender: TObject);
begin
  btnModificarEnviarClick(Sender);
end;

procedure TForm16.btnModificarEnviarClick(Sender: TObject);
var
  IDSeleccionado: Integer;
  FormCorregir: TForm15; // Usamos el TForm15
begin
  IDSeleccionado := ObtenerIDSeleccionado;

  if IDSeleccionado <> -1 then
  begin
    FormCorregir := TForm15.Create(Application);
    FormCorregir.SetBorrador(IDSeleccionado, BandejaActual);
    FormCorregir.ShowModal;
    FormCorregir.Free;

    // Refrescar la tabla después de modificar/enviar/eliminar
    RefrescarDatos;
  end
  else
  begin
    ShowMessage('Seleccione un borrador válido.');
  end;
end;

end.
