unit Blockchain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Process, Dialogs, sha1;


const

  DIFFICULTY = 4;

// --- ESTRUCTURAS ---

type
  // Puntero al bloque
  PBloque = ^TBloque;

  // Estructura del Bloque de la Cadena
  TBloque = record
    index: Integer;
    timestamp: String;
    data: String;
    nonce: Integer;
    previousHash: String;
    hash: String;
    siguiente: PBloque;
  end;

  // Estructura de la Blockchain (Lista Enlazada con inserción a la cabeza)
  TBlockchain = record
    head: PBloque;
    blockCount: Integer; // Contador total de bloques
  end;


var
  SistemaBlockchain: TBlockchain;

procedure InicializarBlockchain(var Chain: TBlockchain);
procedure AgregarBloque(var Chain: TBlockchain; ID_Correo, Remitente, Asunto, Mensaje: String);
procedure GenerarReporteBlockchain(Chain: TBlockchain; nombreArchivo: string);
procedure ConvertirDotAPngBlockchain(nombreArchivo: string);

implementation

// --- UTILERÍA ---

function GetFormattedTimestamp: String;
begin
  Result := FormatDateTime('dd-mm-yy::hh:nn:ss', Now);
end;

function GenerarDataString(ID_Correo, Remitente, Asunto, Mensaje: String): String;
begin
  Result := Format('ID:%s|REM:%s|ASUNTO:%s|MSG:%s', [ID_Correo, Remitente, Asunto, Mensaje]);
end;

function GenerarInputString(index: Integer; timestamp, data: String; nonce: Integer; previousHash: String): String;
begin
  Result := Format('%d%s%s%d%s', [index, timestamp, data, nonce, previousHash]);
end;

// Genera el Hash con SHA-1
function CalculateHash(index: Integer; timestamp, data: String; nonce: Integer; previousHash: String): String;
var
  InputString: String;
  Digest: TSHA1Digest;
  i: Integer;
  hexString: string;
begin
  InputString := GenerarInputString(index, timestamp, data, nonce, previousHash);

  // 1. Calcular el Digest SHA-1
  Digest := SHA1String(RawByteString(InputString));

  // 2. Convertir el Digest a cadena hexadecimal
  hexString := '';
  for i := 0 to High(Digest) do
    hexString := hexString + LowerCase(IntToHex(Digest[i], 2));

  // 3. Retornar en mayúsculas
  Result := UpperCase(hexString);
end;


function MineBlock(var NuevoBloque: TBloque; difficulty: Integer): String;
var
  targetPrefix: String;
  currentHash: String;
begin
  targetPrefix := StringOfChar('0', difficulty);
  NuevoBloque.nonce := 0;
  repeat
    NuevoBloque.nonce := NuevoBloque.nonce + 1;
    NuevoBloque.timestamp := GetFormattedTimestamp;

    currentHash := CalculateHash(
      NuevoBloque.index,
      NuevoBloque.timestamp,
      NuevoBloque.data,
      NuevoBloque.nonce,
      NuevoBloque.previousHash
    );

  until Pos(targetPrefix, currentHash) = 1;

  NuevoBloque.hash := currentHash;
  Result := currentHash;
end;


procedure CrearBloqueGenesis(var Chain: TBlockchain);
var
  genesis: PBloque;
begin
  New(genesis);

  genesis^.index := 0;
  genesis^.previousHash := '0000';
  genesis^.data := GenerarDataString('0', 'Sistema', 'Bloque Génesis', 'Inicio de la cadena de correos.');
  genesis^.siguiente := nil;

  MineBlock(genesis^, DIFFICULTY);

  Chain.head := genesis;
  Chain.blockCount := 1;
end;


procedure InicializarBlockchain(var Chain: TBlockchain);
begin
  Chain.head := nil;
  Chain.blockCount := 0;
  CrearBloqueGenesis(Chain);
  ShowMessage(Format('Blockchain inicializada con Bloque Génesis. Hash (SHA-1): %s', [Copy(Chain.head^.hash, 1, 15) + '...']));
end;

procedure AgregarBloque(var Chain: TBlockchain; ID_Correo, Remitente, Asunto, Mensaje: String);
var
  nuevo: PBloque;
  previousBlock: PBloque;
begin
  New(nuevo);

  // Aseguramos que previousBlock sea el HEAD actual (Génesis o el último bloque)
  previousBlock := Chain.head;

  // Verificación de seguridad (para evitar SIGSEGV)
  if previousBlock = nil then
  begin
      ShowMessage('Error de Blockchain: La cadena no tiene Bloque Génesis.');
      Dispose(nuevo);
      Exit;
  end;

  Inc(Chain.blockCount);

  nuevo^.index := Chain.blockCount - 1;
  nuevo^.data := GenerarDataString(ID_Correo, Remitente, Asunto, Mensaje);
  nuevo^.previousHash := previousBlock^.hash; // Hash del bloque anterior
  nuevo^.siguiente := previousBlock;

  MineBlock(nuevo^, DIFFICULTY);
  Chain.head := nuevo;
end;

procedure ConvertirDotAPngBlockchain(nombreArchivo: string);
var
  rutaCarpeta: string;
  rutaCompletaDot: string;
  rutaCompletaPng: string;
  p: TProcess;
begin
  rutaCarpeta := ExtractFilePath(ParamStr(0)) + 'Root-Reportes';
  rutaCompletaDot := rutaCarpeta + DirectorySeparator + nombreArchivo + '.dot';
  rutaCompletaPng := rutaCarpeta + DirectorySeparator + nombreArchivo + '.png';

  if not FileExists(rutaCompletaDot) then
  begin
    ShowMessage('Error: El archivo DOT de la Blockchain no se encontró en ' + rutaCompletaDot);
    Exit;
  end;

  p := TProcess.Create(nil);
  try
    p.Executable := 'dot'; // Comando de Graphviz
    p.Parameters.Add('-Tpng');
    p.Parameters.Add(rutaCompletaDot);
    p.Parameters.Add('-o');
    p.Parameters.Add(rutaCompletaPng);

    p.Options := [poWaitOnExit, poUsePipes];
    p.Execute;

    if p.ExitStatus <> 0 then
      ShowMessage('Error al generar el PNG de la Blockchain (código de salida: ' + IntToStr(p.ExitStatus) + '). Asegúrese de que Graphviz esté instalado y en el PATH.');
  finally
    p.Free;
  end;
end;


procedure GenerarReporteBlockchain(Chain: TBlockchain; nombreArchivo: string);
var
  f: TextFile;
  rutaCompleta: string;
  actual: PBloque;
  rutaCarpeta: string;
  // Variables para la conversión a String y evitar errores de tipo
  indexStr, nonceStr, dataStr, prevHashStr, hashStr, prevIndexStr: String;
begin
  if Chain.head = nil then
    Exit;

  rutaCarpeta := ExtractFilePath(ParamStr(0)) + 'Root-Reportes';
  if not DirectoryExists(rutaCarpeta) then
    CreateDir(rutaCarpeta);

  rutaCompleta := rutaCarpeta + DirectorySeparator + nombreArchivo + '.dot';

  AssignFile(f, rutaCompleta);
  Rewrite(f);

  try
    Writeln(f, 'digraph Blockchain {');
    Writeln(f, '  rankdir=TB;'); // De arriba (nuevo) a abajo (antiguo)
    Writeln(f, '  node [shape=record, style=filled, fillcolor=yellowgreen];');
    Writeln(f, Format('  labelloc="t"; label="Blockchain de Correos Enviados (Algoritmo: SHA-1, Dificultad: %d)";', [DIFFICULTY]));

    actual := Chain.head;
    while actual <> nil do
    begin
      // CONVERSIÓN EXPLÍCITA DE ENTEROS A CADENA
      indexStr := IntToStr(actual^.index);
      nonceStr := IntToStr(actual^.nonce);
      dataStr := StringReplace(Copy(actual^.data, 1, 40) + '...', '|', '\|', [rfReplaceAll]);
      prevHashStr := Copy(actual^.previousHash, 1, 15) + '...';
      hashStr := Copy(actual^.hash, 1, 15) + '...';

      // Definición del nodo con puertos <prev> y <hash>
      Writeln(f, Format('  node%s [label="{Index: %s | Time: %s | Nonce: %s | {<data> Data: %s} | {<prev> Prev Hash: %s} | {<hash> Hash: %s}}"];',
        [
          indexStr, // Arg 1 (node name)
          indexStr, // Arg 2 (Index label)
          actual^.timestamp,
          nonceStr,
          dataStr,
          prevHashStr,
          hashStr
        ]));
      actual := actual^.siguiente;
    end;

    actual := Chain.head;
    while (actual <> nil) and (actual^.siguiente <> nil) do
    begin
      // CONEXIÓN CRIPTOGRÁFICA
      indexStr := IntToStr(actual^.index); // Índice del bloque ACTUAL (el destino)
      prevIndexStr := IntToStr(actual^.siguiente^.index); // Índice del bloque ANTERIOR (la fuente)

      Writeln(f, Format('  node%s:hash -> node%s:prev [label="Chain Link", color=red, arrowhead=normal];',
        [
          prevIndexStr, // Bloque de donde sale el Hash (El Anterior)
          indexStr // Bloque a donde llega el Prev Hash (El Actual)
        ]));
      actual := actual^.siguiente;
    end;

    Writeln(f, '}');
  finally
    CloseFile(f);
  end;

  // Llamada para generar el PNG
  ConvertirDotAPngBlockchain(nombreArchivo);
end;

end.
