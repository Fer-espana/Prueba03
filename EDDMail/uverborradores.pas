unit UVerBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UGLOBAL, UAVLTreeBorradores, UListaDobleEnlazadaCorreos, UCorregirBorrador, Process,
  // *** NUEVAS DEPENDENCIAS ***
  UPilaPapelera; // <-- Para Push a la papelera

type

  { TForm16 }

  TForm16 = class(TForm)
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    MemoRecorrido: TMemo;
    listViewBorradores: TListView;
    Label1: TLabel;
    Label2: TLabel;
    editNumeroBorradores: TEdit;
    btnModificarEnviar: TButton;
    // *** NUEVO BOTÓN ***
    btnEliminar: TButton;
    procedure btnPreOrdenClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tablaBorradoresDblClick(Sender: TObject);
    procedure btnModificarEnviarClick(Sender: TObject);
    procedure editNumeroBorradoresChange(Sender: TObject);
    // *** NUEVO EVENTO ***
    procedure btnEliminarClick(Sender: TObject);
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
  // *** NUEVO BOTÓN CONFIGURADO ***
  btnEliminar.Caption := 'Eliminar Borrador';
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
  MostrarRecorrido('In-Orden');
end;

procedure TForm16.RecorrerInOrdenParaTabla(Nodo: PNodeAVL; var Fila: Integer);
var
  Item: TListItem;
begin
  if Nodo = nil then Exit;

  RecorrerInOrdenParaTabla(Nodo^.Left, Fila);

  Item := listViewBorradores.Items.Add;
  Item.Caption := IntToStr(Nodo^.Key);
  Item.SubItems.Add(Nodo^.Correo.Asunto);
  Item.SubItems.Add(Nodo^.Correo.Destinatario);
  Item.SubItems.Add(Nodo^.Correo.Fecha);

  Item.Data := Pointer(PtrInt(Nodo^.Key));
  Inc(Fila);

  RecorrerInOrdenParaTabla(Nodo^.Right, Fila);
end;

procedure TForm16.CargarTablaDesdeAVL;
var
  Fila: Integer;
begin
  listViewBorradores.Items.Clear;
  Fila := 0;

  RecorrerInOrdenParaTabla(BandejaActual^.Borradores.Root, Fila);

  editNumeroBorradores.Text := IntToStr(listViewBorradores.Items.Count);
end;

function TForm16.ObtenerIDSeleccionado: Integer;
var
  Item: TListItem;
begin
  Result := -1;
  Item := listViewBorradores.Selected;

  if Assigned(Item) then
    Result := PtrInt(Item.Data);
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

  GenerarReporteDOTAVL(BandejaActual^.Borradores, RutaDOT);

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

procedure TForm16.btnPreOrdenClick(Sender: TObject);
begin
  GenerarReporteArbol('Pre-Orden');
end;

procedure TForm16.btnInOrdenClick(Sender: TObject);
begin
  GenerarReporteArbol('In-Orden');
end;

procedure TForm16.btnPostOrdenClick(Sender: TObject);
begin
  GenerarReporteArbol('Post-Orden');
end;

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

// *** NUEVA IMPLEMENTACIÓN PARA ELIMINAR BORRADOR ***
procedure TForm16.btnEliminarClick(Sender: TObject);
var
  IDSeleccionado: Integer;
  borradorExtraido: PBorrador;
  correoParaPila: PCorreo;
begin
  IDSeleccionado := ObtenerIDSeleccionado;

  if IDSeleccionado = -1 then
  begin
    ShowMessage('Por favor, seleccione un borrador para eliminar.');
    Exit;
  end;

  if UsuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado.');
    Exit;
  end;

  if MessageDlg('Confirmar',
                Format('¿Está seguro de que desea mover el borrador ID %d a la papelera?', [IDSeleccionado]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // 1. Extraer el borrador del Árbol AVL
    borradorExtraido := ExtraerBorrador(BandejaActual^.Borradores, IDSeleccionado);

    if borradorExtraido <> nil then
    begin
      try
        // 2. Convertir el PBorrador a PCorreo para la Pila
        New(correoParaPila);
        correoParaPila^.Id := borradorExtraido^.id;
        correoParaPila^.Remitente := borradorExtraido^.remitente;
        correoParaPila^.Destinatario := borradorExtraido^.destinatario;
        correoParaPila^.Asunto := borradorExtraido^.asunto;
        correoParaPila^.Mensaje := borradorExtraido^.mensaje;
        correoParaPila^.Estado := 'B'; // Estado Borrador
        correoParaPila^.Fecha := borradorExtraido^.fecha;
        correoParaPila^.Programado := False;
        correoParaPila^.Siguiente := nil;
        correoParaPila^.Anterior := nil;

        // 3. Empujar (Push) el PCorreo a la Pila
        if Apilar(BandejaActual^.Papelera, correoParaPila) then
        begin
          ShowMessage('Borrador movido a la papelera exitosamente.');
        end
        else
        begin
          ShowMessage('Error al mover el borrador a la papelera.');
          Dispose(correoParaPila); // Liberar memoria si falla
        end;

      finally
        // 4. Liberar la memoria del borrador extraído
        Dispose(borradorExtraido);
      end;

      // 5. Recargar la lista para reflejar la eliminación
      RefrescarDatos;
    end
    else
    begin
      ShowMessage('Error: No se pudo extraer el borrador del árbol AVL.');
      RefrescarDatos; // Recargar por si hubo un cambio
    end;
  end;
end;

procedure TForm16.editNumeroBorradoresChange(Sender: TObject);
begin
  // Evento vacío pero necesario
end;

end.
