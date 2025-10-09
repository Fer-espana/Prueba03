unit UMatrizDispersaRelaciones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
  NuevaCelda, Actual, Anterior: PCelda;
begin
  // Crear nueva celda
  New(NuevaCelda);
  NuevaCelda^.Fila := Fila;
  NuevaCelda^.Columna := Columna;
  NuevaCelda^.Valor := Valor;
  NuevaCelda^.SiguienteFila := nil;
  NuevaCelda^.SiguienteColumna := nil;

  // Actualizar dimensiones
  if Fila > Matriz.Filas then Matriz.Filas := Fila;
  if Columna > Matriz.Columnas then Matriz.Columnas := Columna;

  // Insertar en lista de filas
  if (Matriz.CabezaFila = nil) or (Matriz.CabezaFila^.Fila > Fila) then
  begin
    NuevaCelda^.SiguienteFila := Matriz.CabezaFila;
    Matriz.CabezaFila := NuevaCelda;
  end
  else
  begin
    Actual := Matriz.CabezaFila;
    Anterior := nil;
    while (Actual <> nil) and (Actual^.Fila < Fila) do
    begin
      Anterior := Actual;
      Actual := Actual^.SiguienteFila;
    end;

    if (Actual <> nil) and (Actual^.Fila = Fila) then
    begin
      // Buscar en la misma fila por columna
      while (Actual <> nil) and (Actual^.Fila = Fila) and (Actual^.Columna < Columna) do
      begin
        Anterior := Actual;
        Actual := Actual^.SiguienteFila;
      end;

      if (Actual <> nil) and (Actual^.Fila = Fila) and (Actual^.Columna = Columna) then
      begin
        // Actualizar valor existente
        Actual^.Valor := Valor;
        Dispose(NuevaCelda);
        Exit;
      end;
    end;

    NuevaCelda^.SiguienteFila := Actual;
    if Anterior <> nil then
      Anterior^.SiguienteFila := NuevaCelda;
  end;

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
begin
  AssignFile(Archivo, NombreArchivo);
  try
    Rewrite(Archivo);
    WriteLn(Archivo, 'Reporte de Relaciones - Matriz Dispersa');
    WriteLn(Archivo, '=====================================');

    Actual := Matriz.CabezaFila;
    while Actual <> nil do
    begin
      WriteLn(Archivo, 'Fila ', Actual^.Fila, ' | Columna ', Actual^.Columna, ' | Valor: ', Actual^.Valor);
      Actual := Actual^.SiguienteFila;
    end;

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
