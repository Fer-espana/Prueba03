unit UVerBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, ComCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaDobleEnlazadaCorreos, UCorregirBorrador, Process;

type

  { TForm16 } // <--- TForm16

  TForm16 = class(TForm)
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    MemoRecorrido: TMemo;
    tablaBorradores: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    editNumeroBorradores: TEdit;
    btnModificarEnviar: TButton;
    procedure btnPreOrdenClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaBorradoresDblClick(Sender: TObject);
    procedure btnModificarEnviarClick(Sender: TObject);
    procedure editNumeroBorradoresChange(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure CargarTablaDesdeAVL;
    function ObtenerIDSeleccionado: Integer;
    procedure RecorrerInOrdenParaTabla(Nodo: PNodeAVL; var Fila: Integer);
    procedure MostrarRecorrido(TipoRecorrido: string);
    procedure GenerarReporteArbol(TipoOrden: string);
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

  // Configuración de la tabla similar a Bandeja de Entrada
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
  Label2.Caption := 'Borradores:';
end;

procedure TForm16.FormShow(Sender: TObject);
begin
  RefrescarDatos;
end;

procedure TForm16.RefrescarDatos;
begin
  BandejaActual := ObtenerBandejaUsuario(UsuarioActual^.Email);
  if BandejaActual = nil then
    BandejaActual := CrearBandejaUsuario(UsuarioActual^.Email);

  CargarTablaDesdeAVL;
  MemoRecorrido.Lines.Clear;
  MostrarRecorrido('In-Orden'); // Mostrar In-Orden por defecto
end;

procedure TForm16.RecorrerInOrdenParaTabla(Nodo: PNodeAVL; var Fila: Integer);
begin
  if Nodo = nil then Exit;

  RecorrerInOrdenParaTabla(Nodo^.Left, Fila);

  tablaBorradores.RowCount := tablaBorradores.RowCount + 1;
  tablaBorradores.Cells[0, Fila] := IntToStr(Nodo^.Key);
  tablaBorradores.Cells[1, Fila] := Nodo^.Correo.Asunto;
  tablaBorradores.Cells[2, Fila] := Nodo^.Correo.Destinatario;
  tablaBorradores.Cells[3, Fila] := Nodo^.Correo.Fecha;

  // CORRECCIÓN: Usar PtrInt para almacenar el ID
  tablaBorradores.Objects[0, Fila] := TObject(PtrInt(Nodo^.Key));
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

  // Actualizar contador
  editNumeroBorradores.Text := IntToStr(Fila - 1);
end;

function TForm16.ObtenerIDSeleccionado: Integer;
var
  FilaSeleccionada: Integer;
begin
  Result := -1;
  FilaSeleccionada := tablaBorradores.Row;

  if (FilaSeleccionada > 0) and (FilaSeleccionada < tablaBorradores.RowCount) then
    // CORRECCIÓN: Usar PtrInt para recuperar el ID
    Result := PtrInt(tablaBorradores.Objects[0, FilaSeleccionada]);
end;

procedure TForm16.MostrarRecorrido(TipoRecorrido: string);
var
  RecorridoStr: string;
begin
  RecorridoStr := ObtenerRecorridoAVL(BandejaActual^.Borradores, TipoRecorrido);
  MemoRecorrido.Lines.Clear;
  MemoRecorrido.Lines.Add('Recorrido ' + TipoRecorrido + ' (ID):');
  MemoRecorrido.Lines.Add(RecorridoStr);
end;

procedure TForm16.GenerarReporteArbol(TipoOrden: string);
var
  RutaCarpeta: string;
  RutaDOT, RutaPNG: string;
  Proc: TProcess;
begin
  if (BandejaActual = nil) or (BandejaActual^.Borradores.Root = nil) then
  begin
    ShowMessage('No hay borradores para generar el árbol.');
    Exit;
  end;

  RutaCarpeta := ExtractFilePath(Application.ExeName) +
                 StringReplace(UsuarioActual^.Email, '@', '-', [rfReplaceAll]) +
                 '-Reportes';

  ForceDirectories(RutaCarpeta);
  RutaDOT := RutaCarpeta + PathDelim + 'borradores_avl_' + LowerCase(TipoOrden) + '.dot';
  RutaPNG := RutaCarpeta + PathDelim + 'borradores_avl_' + LowerCase(TipoOrden) + '.png';

  // 2. Generar archivo DOT
  GenerarReporteDOTAVL(BandejaActual^.Borradores, RutaDOT);

  // 3. Generar el recorrido en el Memo y en la imagen PNG
  MostrarRecorrido(TipoOrden);

  if FileExists(RutaDOT) then
  begin
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := 'dot';
      Proc.Parameters.Add('-Tpng');
      Proc.Parameters.Add(RutaDOT);
      Proc.Parameters.Add('-o');
      Proc.Parameters.Add(RutaPNG);
      Proc.Options := [poWaitOnExit];
      Proc.Execute;
    finally
      Proc.Free;
    end;
  end;

  ShowMessage('Reporte DOT y PNG (' + TipoOrden + ') generado. Abra ' + RutaPNG + ' para ver el Árbol AVL.');
end;


procedure TForm16.btnPreOrdenClick(Sender: TObject); begin GenerarReporteArbol('Pre-Orden'); end;
procedure TForm16.btnInOrdenClick(Sender: TObject); begin GenerarReporteArbol('In-Orden'); end;
procedure TForm16.btnPostOrdenClick(Sender: TObject); begin GenerarReporteArbol('Post-Orden'); end;

procedure TForm16.tablaBorradoresDblClick(Sender: TObject);
begin
  btnModificarEnviarClick(Sender);
end;

procedure TForm16.btnModificarEnviarClick(Sender: TObject);
var
  IDSeleccionado: Integer;
  FormCorregir: TForm15;
begin
  IDSeleccionado := ObtenerIDSeleccionado;

  if IDSeleccionado <> -1 then
  begin
    FormCorregir := TForm15.Create(Application);
    FormCorregir.SetBorrador(IDSeleccionado, BandejaActual);
    FormCorregir.ShowModal;
    FormCorregir.Free;

    RefrescarDatos;
  end
  else
  begin
    ShowMessage('Seleccione un borrador válido.');
  end;
end;

procedure TForm16.editNumeroBorradoresChange(Sender: TObject); begin end;

end.
