unit LZW;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs; // Necesitamos FileUtil y Dialogs para guardar

type
  // Tipo de dato para códigos de compresión (16 bits)
  TCode = Word;
  // Arreglo de códigos comprimidos
  TCodeArray = array of TCode;
  // Diccionario que almacena cadenas
  TDictionary = array of string;


  // Clase principal del compresor LZW
  TCompresorLZW = class
  private
    // Inicializa el diccionario con los 256 caracteres ASCII
    procedure Inicializar(var dict: TDictionary);

    // Busca una cadena en el diccionario, devuelve su posición o -1 si no existe
    // (Método auxiliar, lo mantenemos privado)
    function Buscar(const dict: TDictionary; const cadena: string): Integer;

    // Agrega una nueva cadena al diccionario
    // (Método auxiliar, lo mantenemos privado)
    procedure Agregar(var dict: TDictionary; const cadena: string);

  public
    // Comprime un texto en una secuencia de códigos (Público)
    function Comprimir(const texto: string): TCodeArray;

    // Descomprime una secuencia de códigos en texto original (Público, para probar)
    function Descomprimir(const codigos: TCodeArray): string;
  end;

// FUNCIÓN PÚBLICA REQUERIDA PARA GUARDAR EL ARCHIVO COMPRIMIDO
procedure GuardarCodigosLZWComoTxt(const codigos: TCodeArray; const nombreArchivo: string);

// Guardar como binario (opcional, pero se mantiene para eficiencia)
procedure GuardarCodigosLZWComoBin(const codigos: TCodeArray; const nombreArchivo: string);


implementation

// Implementación de los métodos de TCompresorLZW (Copiados de tu código)

procedure TCompresorLZW.Inicializar(var dict: TDictionary);
var
  i: Integer;
begin
  // Crea el diccionario con 256 entradas iniciales (caracteres ASCII)
  SetLength(dict, 256);
  for i := 0 to 255 do
    dict[i] := Chr(i);  // Asigna cada carácter ASCII
end;

function TCompresorLZW.Buscar(const dict: TDictionary; const cadena: string): Integer;
var
  i: Integer;
begin
  // El método Buscar con un simple bucle es ineficiente,
  // pero lo mantenemos ya que es la implementación de tu auxiliar.
  for i := 0 to High(dict) do
    if dict[i] = cadena then
      Exit(i);
  Result := -1;
end;

procedure TCompresorLZW.Agregar(var dict: TDictionary; const cadena: string);
begin
  // Aumenta el tamaño del diccionario y agrega una nueva cadena
  SetLength(dict, Length(dict) + 1);
  dict[High(dict)] := cadena;
end;

function TCompresorLZW.Comprimir(const texto: string): TCodeArray;
var
  dict: TDictionary;
  entrada: string;
  caracter: Char;
  codigo: Integer;
  siguienteCodigo: Integer;
  i: Integer;
  resultado: array of TCode;
begin
  Inicializar(dict);
  siguienteCodigo := 256;
  SetLength(resultado, 0);
  entrada := '';

  for i := 1 to Length(texto) do
  begin
    caracter := texto[i];
    codigo := Buscar(dict, entrada + caracter);

    if codigo <> -1 then
      entrada := entrada + caracter
    else
    begin
      codigo := Buscar(dict, entrada);
      if codigo <> -1 then
      begin
        SetLength(resultado, Length(resultado) + 1);
        resultado[High(resultado)] := codigo;

        if siguienteCodigo < 65536 then
        begin
          Agregar(dict, entrada + caracter);
          Inc(siguienteCodigo);
        end;
      end;
      entrada := caracter;
    end;
  end;

  if entrada <> '' then
  begin
    codigo := Buscar(dict, entrada);
    SetLength(resultado, Length(resultado) + 1);
    resultado[High(resultado)] := codigo;
  end;

  Result := resultado;
end;

function TCompresorLZW.Descomprimir(const codigos: TCodeArray): string;
var
  dict: TDictionary;
  anterior, actual: string;
  codigoAnterior, codigoActual: TCode;
  i: Integer;
  siguienteCodigo: Integer;
begin
  Inicializar(dict);
  siguienteCodigo := 256;

  if Length(codigos) = 0 then
    Exit;

  codigoAnterior := codigos[0];
  anterior := dict[codigoAnterior];
  Result := anterior;

  for i := 1 to High(codigos) do
  begin
    codigoActual := codigos[i];

    if codigoActual < Length(dict) then
      actual := dict[codigoActual]
    else if codigoActual = siguienteCodigo then
      actual := anterior + anterior[1]
    else
      raise Exception.Create('Error: Código inválido en la descompresión');

    Result := Result + actual;

    if siguienteCodigo < 65536 then
    begin
      Agregar(dict, anterior + actual[1]);
      Inc(siguienteCodigo);
    end;

    anterior := actual;
    codigoAnterior := codigoActual;
  end;
end;

// Implementación de las funciones auxiliares de guardado

procedure GuardarCodigosLZWComoTxt(const codigos: TCodeArray; const nombreArchivo: string);
var
  f: TextFile;
  i: Integer;
begin
  // Manejo de errores de I/O básico
  {$I-}
  AssignFile(f, nombreArchivo);
  Rewrite(f);
  for i := 0 to High(codigos) do
  begin
    Write(f, codigos[i]);
    if i < High(codigos) then
      Write(f, ','); // Códigos separados por coma
  end;
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
    ShowMessage('Error de I/O al guardar el archivo de texto.');
end;

procedure GuardarCodigosLZWComoBin(const codigos: TCodeArray; const nombreArchivo: string);
var
  f: File of Word;
  i: Integer;
begin
  {$I-}
  AssignFile(f, nombreArchivo);
  Rewrite(f);
  for i := 0 to High(codigos) do
    Write(f, codigos[i]);
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
    ShowMessage('Error de I/O al guardar el archivo binario.');
end;

end.
