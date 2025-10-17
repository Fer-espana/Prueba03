unit UROOT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGLOBAL,
  UListaSimpleUsuarios, fpjson, jsonparser, UMatrizDispersaRelaciones,
  UGestionComunidades, Process, UListaDobleEnlazadaCorreos;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCargaMasiva: TButton;
    btnGestionComunidades: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure btnGestionComunidadesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON;
    procedure GuardarUsuariosEnJSON;
    // CORRECCIÓN 1: Se declara el método en la interface (section private)
    procedure CargarCorreosDesdeJSON;
  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Panel de Administración Root';
  // Cargar usuarios existentes al iniciar
  CargarUsuariosDesdeJSON;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;


procedure TForm2.CargarUsuariosDesdeJSON;
var
  Archivo: TextFile;
  Linea, JSONContent: string;
  JSONData: TJSONData; // CAMBIAR: eliminar Parser
  UsuariosArray: TJSONArray;
  i: Integer;
  UsuarioObj: TJSONObject;
begin
  if not FileExists('Data/usuarios.json') then
  begin
    ShowMessage('Archivo Data/usuarios.json no encontrado. Se creará uno nuevo al registrar usuarios.');
    Exit;
  end;

  try
    // Leer archivo JSON
    AssignFile(Archivo, 'Data/usuarios.json');
    Reset(Archivo);
    JSONContent := '';
    while not EOF(Archivo) do
    begin
      ReadLn(Archivo, Linea);
      JSONContent := JSONContent + Linea;
    end;
    CloseFile(Archivo);

    // Parsear JSON - USAR GetJSON EN LUGAR DE TJSONParser
    JSONData := GetJSON(JSONContent);
    try
      if JSONData.FindPath('usuarios') <> nil then
      begin
        UsuariosArray := TJSONArray(JSONData.FindPath('usuarios'));
        for i := 0 to UsuariosArray.Count - 1 do
        begin
          UsuarioObj := TJSONObject(UsuariosArray.Items[i]);

          // Insertar usuario en la lista global
          InsertarUsuario(ListaUsuariosGlobal,
            UsuarioObj.Get('id', 0),
            UsuarioObj.Get('nombre', ''),
            UsuarioObj.Get('usuario', ''),
            UsuarioObj.Get('email', ''),
            UsuarioObj.Get('telefono', ''),
            UsuarioObj.Get('password', '')); // Contraseña vacía por defecto
        end;
        ShowMessage('Usuarios cargados exitosamente desde Data/usuarios.json');
      end;
    finally
      JSONData.Free; // LIBERAR JSONData en lugar de Parser
    end;
  except
    on E: Exception do
      ShowMessage('Error al cargar usuarios: ' + E.Message);
  end;
end;

procedure TForm2.GuardarUsuariosEnJSON;
var
  Archivo: TextFile;
  Actual: PUsuario;
  RutaArchivo, RutaCarpeta: string;
  EsPrimerUsuario: Boolean;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Data';
  RutaArchivo := RutaCarpeta + PathDelim + 'usuarios.json';

  // Crear carpeta Data si no existe
  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  // Crear o sobrescribir archivo
  AssignFile(Archivo, RutaArchivo);
  try
    Rewrite(Archivo);

    // Escribir encabezado JSON
    WriteLn(Archivo, '{');
    WriteLn(Archivo, '  "usuarios": [');

    // Escribir usuarios
    Actual := ListaUsuariosGlobal.Cabeza;
    EsPrimerUsuario := True;

    while Actual <> nil do
    begin
      if not EsPrimerUsuario then
        WriteLn(Archivo, ',');

      WriteLn(Archivo, '    {');
      WriteLn(Archivo, '      "id": ', Actual^.Id, ',');
      WriteLn(Archivo, '      "nombre": "', Actual^.Nombre, '",');
      WriteLn(Archivo, '      "usuario": "', Actual^.Usuario, '",');
      WriteLn(Archivo, '      "email": "', Actual^.Email, '",');
      WriteLn(Archivo, '      "telefono": "', Actual^.Telefono, '",'); // Agregar coma
      WriteLn(Archivo, '      "password": "', Actual^.Password, '"');  // NUEVA LÍNEA
      Write(Archivo, '    }');

      EsPrimerUsuario := False;
      Actual := Actual^.Siguiente;
    end;

    // Escribir cierre JSON
    WriteLn(Archivo);
    WriteLn(Archivo, '  ]');
    WriteLn(Archivo, '}');

    ShowMessage('Usuarios guardados en: ' + RutaArchivo);
  finally
    CloseFile(Archivo);
  end;
end;

procedure TForm2.btnCargaMasivaClick(Sender: TObject);
begin
  // Recargar solo usuarios (función existente)
  CargarUsuariosDesdeJSON;

  // NUEVA FUNCIÓN DE CARGA DE CORREOS
  CargarCorreosDesdeJSON; // <--- LLAMAR A LA FUNCIÓN DE CARGA DE CORREOS

  ShowMessage('Carga Masiva de Usuarios y Correos completada.');
end;

// =======================================================
// REPORTE DE USUARIOS (LISTA SIMPLE ENLAZADA) - IMPLEMENTACIÓN DOT
// =======================================================
procedure TForm2.btnReporteUsuariosClick(Sender: TObject);
var
  RutaCarpeta, NombreDOT, NombrePNG, RutaDOT, RutaPNG: string;
  Proc: TProcess; // VARIABLE PARA TProcess
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Root-Reportes';
  NombreDOT := 'reporte_usuarios.dot';
  NombrePNG := 'reporte_usuarios.png';
  RutaDOT := RutaCarpeta + PathDelim + NombreDOT;
  RutaPNG := RutaCarpeta + PathDelim + NombrePNG;

  // 1. Crear la carpeta si no existe
  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  // 2. Generar el archivo DOT
  GenerarReporteDOTUsuarios(ListaUsuariosGlobal, RutaDOT);

  // 3. EJECUTAR GRAPHVIZ (Generación automática de PNG) - CORREGIDO
  if FileExists(RutaDOT) then
  begin
    // Comando: dot -Tpng <archivo.dot> -o <archivo.png>
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

  ShowMessage('Reporte de Usuarios (Lista Simple) generado en: ' + RutaDOT + sLineBreak +
              'Imagen PNG generada automáticamente en: ' + RutaPNG);
end;

// =======================================================
// REPORTE DE RELACIONES (MATRIZ DISPERSA) - IMPLEMENTACIÓN DOT
// =======================================================
procedure TForm2.btnReporteRelacionesClick(Sender: TObject);
var
  RutaCarpeta, NombreDOT, NombrePNG, RutaDOT, RutaPNG: string;
  Proc: TProcess; // VARIABLE PARA TProcess
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Root-Reportes';
  NombreDOT := 'reporte_relaciones.dot';
  NombrePNG := 'reporte_relaciones.png';
  RutaDOT := RutaCarpeta + PathDelim + NombreDOT;
  RutaPNG := RutaCarpeta + PathDelim + NombrePNG;

  // 1. Crear carpeta si no existe
  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  // 2. Generar reporte de relaciones (Matriz Dispersa a DOT)
  GenerarReporteRelaciones(MatrizRelaciones, RutaDOT);

  // 3. EJECUTAR GRAPHVIZ (Generación automática de PNG) - CORREGIDO
  if FileExists(RutaDOT) then
  begin
    // Comando: dot -Tpng <archivo.dot> -o <archivo.png>
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

  ShowMessage('Reporte de Relaciones (Matriz Dispersa) generado en: ' + RutaDOT + sLineBreak +
              'Imagen PNG generada automáticamente en: ' + RutaPNG);
end;

procedure TForm2.btnGestionComunidadesClick(Sender: TObject);
var
  FormGestion: TForm14;
begin
  FormGestion := TForm14.Create(Application);
  FormGestion.Show;
end;

// CORRECCIÓN 2: Se añade el identificador de la clase (TForm2.) para vincular el método.
procedure TForm2.CargarCorreosDesdeJSON;
var
  Archivo: TextFile;
  Linea, JSONContent: string;
  JSONData: TJSONData;
  CorreosArray: TJSONArray;
  i: Integer;
  CorreoObj: TJSONObject;
  RutaArchivo: string;
  BandejaDestino: PBandejaUsuario;
  EstadoCorreo: Char;
begin
  RutaArchivo := ExtractFilePath(Application.ExeName) + 'Data' + PathDelim + 'correos.json';

  if not FileExists(RutaArchivo) then
  begin
    ShowMessage('Advertencia: Archivo Data/correos.json no encontrado.');
    Exit;
  end;

  try
    AssignFile(Archivo, RutaArchivo);
    Reset(Archivo);
    JSONContent := '';
    while not EOF(Archivo) do
    begin
      ReadLn(Archivo, Linea);
      JSONContent := JSONContent + Linea;
    end;
    CloseFile(Archivo);

    JSONData := GetJSON(JSONContent);
    try
      if (JSONData <> nil) and (JSONData.FindPath('correos') <> nil) then
      begin
        CorreosArray := TJSONArray(JSONData.FindPath('correos'));
        for i := 0 to CorreosArray.Count - 1 do
        begin
          CorreoObj := TJSONObject(CorreosArray.Items[i]);

          // Mapear estado
          if SameText(CorreoObj.Get('estado', ''), 'NL') then
            EstadoCorreo := 'N'
          else if SameText(CorreoObj.Get('estado', ''), 'LEÍDO') then
            EstadoCorreo := 'L'
          else if SameText(CorreoObj.Get('estado', ''), 'ELIMINADO') then
            EstadoCorreo := 'E'
          else
            EstadoCorreo := 'N'; // Valor por defecto

          // Obtener o crear bandeja del destinatario
          BandejaDestino := ObtenerBandejaUsuario(CorreoObj.Get('destinatario', ''));
          if BandejaDestino = nil then
            BandejaDestino := CrearBandejaUsuario(CorreoObj.Get('destinatario', ''));

          // Insertar correo
          InsertarCorreo(BandejaDestino^.BandejaEntrada,
            CorreoObj.Get('id', 0),
            CorreoObj.Get('remitente', ''),
            CorreoObj.Get('destinatario', ''),
            EstadoCorreo,
            False, // No son programados en esta carga
            CorreoObj.Get('asunto', ''),
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), // Usar fecha actual para la carga
            CorreoObj.Get('mensaje', ''));

          // Actualizar Matriz de Relaciones (solo si remitente/destinatario son usuarios válidos)
          if (BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('remitente', '')) <> nil) and
             (BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('destinatario', '')) <> nil) then
          begin
            // Asumiendo que BuscarIndiceUsuario devuelve el ID
            InsertarValor(MatrizRelaciones,
                          BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('remitente', ''))^.Id,
                          BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('destinatario', ''))^.Id,
                          1);
          end;
        end;
        ShowMessage(IntToStr(CorreosArray.Count) + ' correos cargados exitosamente.');
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error al cargar correos: ' + E.Message);
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  // Limpiar estado global
  EsUsuarioRoot := False;
  UsuarioActual := nil;
end;

end.
