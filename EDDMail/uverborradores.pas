unit UVerBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaDobleEnlazadaCorreos, UCorregirBorrador, Process; // Grids ELIMINADO

type

  { TForm16 } // <--- TForm16

  TForm16 = class(TForm)
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    MemoRecorrido: TMemo;
    listViewBorradores: TListView; // <--- AÑADIDO
    Label1: TLabel;
    Label2: TLabel;
    editNumeroBorradores: TEdit;
    btnModificarEnviar: TButton;
    procedure btnPreOrdenClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaBorradoresDblClick(Sender: TObject); // Se mantiene por compatibilidad con LFM
    procedure btnModificarEnviarClick(Sender: TObject);
    procedure editNumeroBorradoresChange(Sender: TObject);
  private
    BandejaActual: PBandejaUsuario;
    procedure CargarTablaDesdeAVL;
    procedure ConfigurarListView;
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

procedure TForm16.ConfigurarListView;
begin
  with listViewBorradores do
  begin
    ViewStyle := vsReport;
    ReadOnly := True;
    RowSelect := True;
    Columns.Clear;

    // Configurar columnas
    Columns.Add.Caption := 'ID';
    Columns.Add.Caption := 'Asunto';
    Columns.Add.Caption := 'Destinatario';
    Columns.Add.Caption := 'Fecha';

    Columns[0].Width := 50;
    Columns[1].Width := 150;
    Columns[2].Width := 150;
    Columns[3].Width := 120;
  end;
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  Caption := 'Borradores de Mensajes (Árbol AVL)';

  ConfigurarListView;

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
var
  Item: TListItem;
begin
  if Nodo = nil then Exit;

  RecorrerInOrdenParaTabla(Nodo^.Left, Fila);

  // Procesar el nodo
  Item := listViewBorradores.Items.Add;
  Item.Caption := IntToStr(Nodo^.Key); // Columna 0 (ID)
  Item.SubItems.Add(Nodo^.Correo.Asunto); // Columna 1
  Item.SubItems.Add(Nodo^.Correo.Destinatario); // Columna 2
  Item.SubItems.Add(Nodo^.Correo.Fecha); // Columna 3

  // Guardar el ID en el puntero de datos del TListView
  Item.Data := Pointer(PtrInt(Nodo^.Key)); // Uso de PtrInt (solución para la portabilidad)
  Inc(Fila);

  RecorrerInOrdenParaTabla(Nodo^.Right, Fila);
end;


procedure TForm16.CargarTablaDesdeAVL;
var
  Fila: Integer;
begin
  listViewBorradores.Items.Clear; // Limpiar TListView
  Fila := 0;

  RecorrerInOrdenParaTabla(BandejaActual^.Borradores.Root, Fila);

  // Actualizar contador
  editNumeroBorradores.Text := IntToStr(listViewBorradores.Items.Count);
end;

function TForm16.ObtenerIDSeleccionado: Integer;
var
  Item: TListItem;
begin
  Result := -1;
  Item := listViewBorradores.Selected; // Obtener el ítem seleccionado

  if Assigned(Item) then
    // Recuperar el ID guardado en TListItem.Data
    Result := PtrInt(Item.Data); // Uso de PtrInt (solución para la portabilidad)
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
