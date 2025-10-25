unit Bitacora;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Dialogs,
  fpjson, jsonparser;

type
  PAcceso = ^TAcceso;
  TAcceso = record
    usuario: String;
    entrada: TDateTime;
    salida: TDateTime;
    siguiente: PAcceso;
  end;


  TListaAccesos = record
    primero: PAcceso;
    ultimo: PAcceso;
    count: Integer;
  end;

var
  LogAccesos: TListaAccesos; // Variable global para la bitácora

// PROCEDIMIENTOS PÚBLICOS
procedure InicializarBitacora(var lista: TListaAccesos);
procedure RegistrarEntrada(var lista: TListaAccesos; usuarioEmail: String);
procedure RegistrarSalida(var lista: TListaAccesos; usuarioEmail: String);
procedure ExportarBitacoraJson(lista: TListaAccesos; nombreArchivo: String);

implementation

procedure InicializarBitacora(var lista: TListaAccesos);
begin
  lista.primero := nil;
  lista.ultimo := nil;
  lista.count := 0;
end;

procedure RegistrarEntrada(var lista: TListaAccesos; usuarioEmail: String);
var
  nuevo: PAcceso;
begin
  New(nuevo);

  nuevo^.usuario := usuarioEmail;
  nuevo^.entrada := Now;
  nuevo^.salida := 0;
  nuevo^.siguiente := nil;

  if lista.primero = nil then
  begin
    lista.primero := nuevo;
    lista.ultimo := nuevo;
  end
  else
  begin
    lista.ultimo^.siguiente := nuevo;
    lista.ultimo := nuevo;
  end;

  Inc(lista.count);
end;

procedure RegistrarSalida(var lista: TListaAccesos; usuarioEmail: String);
var
  actual: PAcceso;
  ultimoRegistro: PAcceso;
begin
  ultimoRegistro := nil;
  actual := lista.primero;

  while actual <> nil do
  begin
    if (actual^.usuario = usuarioEmail) and (actual^.salida = 0) then
      ultimoRegistro := actual;

    actual := actual^.siguiente;
  end;

  if ultimoRegistro <> nil then
  begin
    ultimoRegistro^.salida := Now;
  end
  else
  begin
    RegistrarEntrada(lista, usuarioEmail);
    RegistrarSalida(lista, usuarioEmail);
  end;
end;

// Genera el archivo JSON sin usar IOUtils
procedure ExportarBitacoraJson(lista: TListaAccesos; nombreArchivo: String);
var
  JSONRootArray: TJSONArray;
  JSONRecord: TJSONObject;
  Actual: PAcceso;
  rutaCompleta: String;
  rutaCarpeta: String;
  SL: TStringList; // Usamos TStringList de Classes
begin
  if lista.primero = nil then
  begin
    ShowMessage('La bitácora está vacía. No se puede exportar.');
    Exit;
  end;

  // 1. Crear la ruta de salida
  rutaCarpeta := ExtractFilePath(ParamStr(0)) + 'Root-Reportes';
  if not DirectoryExists(rutaCarpeta) then
    CreateDir(rutaCarpeta);

  if LowerCase(ExtractFileExt(nombreArchivo)) <> '.json' then
    nombreArchivo := nombreArchivo + '.json';

  rutaCompleta := rutaCarpeta + DirectorySeparator + nombreArchivo;

  // 2. Crear la estructura JSON
  JSONRootArray := TJSONArray.Create;
  Actual := lista.primero;
  SL := TStringList.Create; // Crear TStringList

  try
    while Actual <> nil do
    begin
      JSONRecord := TJSONObject.Create;

      JSONRecord.Add('usuario', TJSONString.Create(Actual^.usuario));
      JSONRecord.Add('entrada', TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', Actual^.entrada)));

      if Actual^.salida = 0 then
        JSONRecord.Add('salida', TJSONString.Create('')) // Sesión abierta
      else
        JSONRecord.Add('salida', TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', Actual^.salida)));

      JSONRootArray.Add(JSONRecord);
      Actual := Actual^.siguiente;
    end;

    // 3. Guardar en archivo usando TStringList (de Classes)
    SL.Text := JSONRootArray.AsJSON;
    SL.SaveToFile(rutaCompleta); // SaveToFile es un método de TStringList

    ShowMessage('Bitácora exportada exitosamente a ' + rutaCompleta);

  except
    on E: Exception do
      ShowMessage('Error al exportar la bitácora: ' + E.Message);
  end;

  // 4. Liberar memoria
  JSONRootArray.Free;
  SL.Free;
end;

end.
