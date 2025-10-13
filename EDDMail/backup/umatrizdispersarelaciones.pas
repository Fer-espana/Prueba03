unit UMatrizDispersaRelaciones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UListaSimpleUsuarios;

type
  PCelda = ^TCelda;
  TCelda = record
    Fila, Columna: Integer;
    Valor: Integer;
    SiguienteFila, SiguienteColumna: PCelda;
  end;

  TMatrizDispersa = record
    Filas, Columnas: Integer;
    CabezaFila, CabezaColumna: PCelda;
  end;

// Procedimientos básicos
procedure InicializarMatriz(var Matriz: TMatrizDispersa);
procedure InsertarValor(var Matriz: TMatrizDispersa; Fila, Columna, Valor: Integer);
function ObtenerValor(Matriz: TMatrizDispersa; Fila, Columna: Integer): Integer;
procedure GenerarReporteRelaciones(Matriz: TMatrizDispersa; NombreArchivo: string);
procedure LiberarMatriz(var Matriz: TMatrizDispersa);
procedure GenerarReporteRelaciones(Matriz: TMatrizDispersa; NombreArchivo: string);

implementation

procedure InicializarMatriz(var Matriz: TMatrizDispersa);
begin
  Matriz.Filas := 0;
  Matriz.Columnas := 0;
  Matriz.CabezaFila := nil;
  Matriz.CabezaColumna := nil;
end;

procedure InsertarValor(var Matriz: TMatrizDispersa; Fila, Columna, Valor: Integer);
var
  NuevaCelda, ActualFila, ActualColumna, AnteriorFila, AnteriorColumna: PCelda;
  Encontrado: Boolean;
begin
  // Buscar si ya existe la celda
  ActualFila := Matriz.CabezaFila;
  Encontrado := False;

  while (ActualFila <> nil) and not Encontrado do
  begin
    if (ActualFila^.Fila = Fila) and (ActualFila^.Columna = Columna) then
    begin
      // Actualizar valor existente
      ActualFila^.Valor := Valor;
      Encontrado := True;
    end;
    ActualFila := ActualFila^.SiguienteFila;
  end;

  if Encontrado then Exit;

  // Crear nueva celda
  New(NuevaCelda);
  NuevaCelda^.Fila := Fila;
  NuevaCelda^.Columna := Columna;
  NuevaCelda^.Valor := Valor;
  NuevaCelda^.SiguienteFila := nil;
  NuevaCelda^.SiguienteColumna := nil;

  // Insertar en lista de filas
  if (Matriz.CabezaFila = nil) or
     (Matriz.CabezaFila^.Fila > Fila) or
     ((Matriz.CabezaFila^.Fila = Fila) and (Matriz.CabezaFila^.Columna > Columna)) then
  begin
    NuevaCelda^.SiguienteFila := Matriz.CabezaFila;
    Matriz.CabezaFila := NuevaCelda;
  end
  else
  begin
    ActualFila := Matriz.CabezaFila;
    AnteriorFila := nil;

    while (ActualFila <> nil) and
          ((ActualFila^.Fila < Fila) or
           ((ActualFila^.Fila = Fila) and (ActualFila^.Columna < Columna))) do
    begin
      AnteriorFila := ActualFila;
      ActualFila := ActualFila^.SiguienteFila;
    end;

    NuevaCelda^.SiguienteFila := ActualFila;
    if AnteriorFila <> nil then
      AnteriorFila^.SiguienteFila := NuevaCelda;
  end;

    if Fila > Matriz.Filas then Matriz.Filas := Fila;
  if Columna > Matriz.Columnas then Matriz.Columnas := Columna;

  Inc(Matriz.Filas);
  Inc(Matriz.Columnas);

  // Insertar en lista de columnas (similar lógica)
  // Por simplicidad, aquí solo implementamos una dirección
end;

function ObtenerValor(Matriz: TMatrizDispersa; Fila, Columna: Integer): Integer;
var
  Actual: PCelda;
begin
  Actual := Matriz.CabezaFila;
  while Actual <> nil do
  begin
    if (Actual^.Fila = Fila) and (Actual^.Columna = Columna) then
      Exit(Actual^.Valor);
    Actual := Actual^.SiguienteFila;
  end;
  Result := 0; // Valor por defecto si no existe
end;

procedure GenerarReporteRelaciones(Matriz: TMatrizDispersa; NombreArchivo: string);
var
  Archivo: TextFile;
  Actual: PCelda;
  InfoRemitente, InfoDestinatario: string;
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);

    // Encabezado DOT
    WriteLn(Archivo, 'digraph RelacionesUsuario {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=box, style=filled, fillcolor=lightcoral];');
    WriteLn(Archivo, '  edge [color=red, arrowhead=normal];');
    WriteLn(Archivo, '  label="Relaciones de Interacción Remitente vs. Destinatario";');
    WriteLn(Archivo, '');

    Actual := Matriz.CabezaFila;

    while Actual <> nil do
    begin
      // Una relación (Fila, Columna) con un Valor > 0 representa una interacción.
      if Actual^.Valor > 0 then
      begin
        // *** 1. Usar la nueva función de mapeo a la lista global de usuarios ***
        // ListaUsuariosGlobal debe estar declarada en UGLOBAL.pas
        InfoRemitente := ObtenerIDyEmailPorID(ListaUsuariosGlobal, Actual^.Fila);
        InfoDestinatario := ObtenerIDyEmailPorID(ListaUsuariosGlobal, Actual^.Columna);

        // 2. Crear nodos con la información completa
        // Usamos InfoRemitente/InfoDestinatario como ID de nodo y etiqueta para Graphviz
        WriteLn(Archivo, '  "', InfoRemitente, '" [label="', InfoRemitente, '"];');
        WriteLn(Archivo, '  "', InfoDestinatario, '" [label="', InfoDestinatario, '"];');

        // 3. Conexión con la cantidad de correos
        WriteLn(Archivo, '  "', InfoRemitente, '" -> "', InfoDestinatario, '" [label="',
          Actual^.Valor, ' correos", color="darkgreen"];');
      end;

      Actual := Actual^.SiguienteFila;
    end;

    // Pie del archivo DOT
    WriteLn(Archivo, '}');

  finally
    CloseFile(Archivo);
  end;
end;

procedure LiberarMatriz(var Matriz: TMatrizDispersa);
var
  Actual, Temp: PCelda;
begin
  Actual := Matriz.CabezaFila;
  while Actual <> nil do
  begin
    Temp := Actual;
    Actual := Actual^.SiguienteFila;
    Dispose(Temp);
  end;
  Matriz.CabezaFila := nil;
  Matriz.CabezaColumna := nil;
  Matriz.Filas := 0;
  Matriz.Columnas := 0;
end;

end.
