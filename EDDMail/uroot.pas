unit UROOT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGLOBAL,
  UListaSimpleUsuarios, fpjson, jsonparser, UMatrizDispersaRelaciones,
  UGestionComunidades, Process, UListaDobleEnlazadaCorreos,
  // *** NUEVAS DEPENDENCIAS (Fase 3) ***
  bitacora, fbitacora, // <-- Añadir Bitacora y el formulario fbitacora
  FileUtil, LCLIntf, LCLType, Math, StrUtils,
  ulistacircularcontactos, blockchain,
  // *** NUEVA DEPENDENCIA PARA REPORTE DE COMUNIDADES ***
  ulistadelistascomunidades, // <-- Añadir para GenerarReporteComunidades
  // *** NUEVA DEPENDENCIA PARA REPORTE MERKLE ***
  privado; // <-- Añadir unidad Merkle

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCargaMasiva: TButton;
    btnCerrarSesion: TButton;
    btnGestionComunidades: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    // *** NUEVOS BOTONES (Fase 3) ***
    BtnCargaCorreos: TButton;
    BtnCargaContactos: TButton;
    BtnVerMensajesComunidad: TButton;
    BtnReporteComunidades: TButton;
    BtnReporteGrafoContactos: TButton;
    BtnReporteBlockchain: TButton;
    BtnVerBitacora: TButton;
    // *** NUEVO BOTÓN PARA REPORTE MERKLE ***
    BtnReporteMerkle: TButton;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure btnGestionComunidadesClick(Sender: TObject);
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    // *** NUEVOS EVENTOS (Fase 3) ***
    procedure BtnCargaCorreosClick(Sender: TObject);
    procedure BtnCargaContactosClick(Sender: TObject);
    procedure BtnVerMensajesComunidadClick(Sender: TObject);
    procedure BtnReporteComunidadesClick(Sender: TObject);
    procedure BtnReporteGrafoContactosClick(Sender: TObject);
    procedure BtnReporteBlockchainClick(Sender: TObject);
    procedure BtnVerBitacoraClick(Sender: TObject);
    // *** NUEVO EVENTO PARA REPORTE MERKLE ***
    procedure BtnReporteMerkleClick(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON;
    procedure GuardarUsuariosEnJSON;
    procedure CargarCorreosDesdeJSON;
    // *** NUEVOS PROCEDIMIENTOS (Fase 3) ***
    procedure CargarContactosDesdeJSON(const archivo: string);
    function ConvertirDotAPng(nombreArchivoDot: string): Boolean;
    // *** NUEVA FUNCIÓN AUXILIAR PARA GUARDAR STRING ***
    procedure GuardarStringAArchivo(const contenido, rutaArchivo: string);
  public

  end;

var
  Form2: TForm2;

implementation

uses UPrincipal;

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

// *** NUEVO PROCEDIMIENTO (Fase 3) ***
procedure TForm2.FormDestroy(Sender: TObject);
begin
  // Registrar salida del Root
  RegistrarSalida(LogAccesos, 'root@edd.com');

  // Limpiar estado global
  EsUsuarioRoot := False;
  UsuarioActual := nil;
end;

procedure TForm2.CargarUsuariosDesdeJSON;
var
  Archivo: TextFile;
  Linea, JSONContent: string;
  JSONData: TJSONData;
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

    // Parsear JSON
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
            UsuarioObj.Get('password', ''));
        end;
        ShowMessage('Usuarios cargados exitosamente desde Data/usuarios.json');
      end;
    finally
      JSONData.Free;
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
      WriteLn(Archivo, '      "telefono": "', Actual^.Telefono, '",');
      WriteLn(Archivo, '      "password": "', Actual^.Password, '"');
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
  CargarCorreosDesdeJSON;

  ShowMessage('Carga Masiva de Usuarios y Correos completada.');
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) ***
procedure TForm2.BtnCargaCorreosClick(Sender: TObject);
begin
  CargarCorreosDesdeJSON;
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) ***
procedure TForm2.BtnCargaContactosClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'JSON files (*.json)|*.json|All files (*.*)|*.*';
    OpenDialog.Title := 'Seleccionar archivo JSON de Contactos';
    if OpenDialog.Execute then
    begin
      CargarContactosDesdeJSON(OpenDialog.FileName);
    end;
  finally
    OpenDialog.Free;
  end;
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) ***
procedure TForm2.BtnVerMensajesComunidadClick(Sender: TObject);
begin
  // Asumiendo que tienes un formulario para ver mensajes de comunidad
  // Si no existe, puedes implementarlo o mostrar un mensaje
  ShowMessage('Funcionalidad de mensajes de comunidad - Por implementar');
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) - IMPLEMENTADO ***
procedure TForm2.BtnReporteComunidadesClick(Sender: TObject);
var
  nombreBase: String;
  dotFilePath, pngFilePath: String;
begin
  nombreBase := 'comunidades_bst';

  // Asegúrate que la función exista en la unidad correspondiente y reciba ArbolComunidades
  if Assigned(@GenerarReporteComunidades) then
  begin
    GenerarReporteComunidades(ArbolComunidades, nombreBase); // Llama a la función del BST
  end
  else
  begin
    ShowMessage('Error: Función GenerarReporteComunidades no disponible');
    Exit;
  end;

  dotFilePath := IncludeTrailingPathDelimiter(DirectorioReportesRoot) + nombreBase + '.dot';
  pngFilePath := ChangeFileExt(dotFilePath, '.png');

  if ConvertirDotAPng(dotFilePath) then
    ShowMessage('Reporte de Comunidades (BST) generado en: ' + pngFilePath)
  else
    ShowMessage('Error al generar la imagen del reporte de Comunidades.');
end;

// *** MODIFICADO/CONFIRMADO (Fase 3) ***
procedure TForm2.BtnReporteGrafoContactosClick(Sender: TObject);
var
   nombreBase: String;
   dotFilePath: String;
   pngFilePath: String;
begin
   nombreBase := 'grafo_contactos'; // Nombre base sin extensión ni ruta

   // Llama a la función que AHORA está en ulistasimpleusuarios
   GenerarReporteGrafo(nombreBase); // <-- Llamada correcta

   // Construir rutas completas (usando DirectorioReportesRoot de uglobal)
   dotFilePath := IncludeTrailingPathDelimiter(DirectorioReportesRoot) + nombreBase + '.dot';
   pngFilePath := ChangeFileExt(dotFilePath, '.png');

   // Convertir a PNG (asegúrate que ConvertirDotAPng esté disponible aquí)
   if ConvertirDotAPng(dotFilePath) then
      ShowMessage('Reporte del Grafo de Contactos generado en: ' + pngFilePath)
   else
      ShowMessage('Error al generar la imagen del Grafo de Contactos.');
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) ***
procedure TForm2.BtnReporteBlockchainClick(Sender: TObject);
var
   nombreBase: String;
   dotFilePath: String;
   pngFilePath: String;
begin
   nombreBase := 'Blockchain_Reporte';

   // Llama a la función en blockchain.pas
   if Assigned(@GenerarReporteBlockchain) then
   begin
     GenerarReporteBlockchain(SistemaBlockchain, nombreBase);
   end
   else
   begin
     ShowMessage('Función GenerarReporteBlockchain no disponible');
     Exit;
   end;

   // Construir rutas completas
   dotFilePath := ExtractFilePath(Application.ExeName) + 'Root-Reportes' + PathDelim + nombreBase + '.dot';
   pngFilePath := ChangeFileExt(dotFilePath, '.png');

   // Convertir a PNG
   if ConvertirDotAPng(dotFilePath) then
      ShowMessage('Reporte de Blockchain generado en: ' + pngFilePath)
   else
      ShowMessage('Error al generar la imagen de Blockchain.');
end;

// *** NUEVO EVENTO ONCLICK (Fase 3) ***
procedure TForm2.BtnVerBitacoraClick(Sender: TObject);
var
  FormBitacora: TFFbitacora;
begin
  FormBitacora := TFFbitacora.Create(Application);
  try
    FormBitacora.ShowModal;
  finally
    FormBitacora.Free;
  end;
end;

// *** NUEVO EVENTO ONCLICK PARA REPORTE MERKLE ***
procedure TForm2.BtnReporteMerkleClick(Sender: TObject);
var
  usuarioActual: PUsuario;
  nombreBase, dotFilePath, pngFilePath: String;
  archivosGenerados: TStringList;
begin
  archivosGenerados := TStringList.Create;
  try
    usuarioActual := ListaUsuariosGlobal.primero;
    if usuarioActual = nil then
    begin
       ShowMessage('No hay usuarios registrados para generar reportes de Árbol Merkle.');
       Exit;
    end;

    while usuarioActual <> nil do
    begin
      // Verificar si el árbol Merkle del usuario tiene nodos
      if Assigned(usuarioActual^.privados) and (usuarioActual^.privados.Root <> nil) then
      begin
        nombreBase := 'MerkleTree_' + usuarioActual^.email; // Nombre único por usuario
        dotFilePath := IncludeTrailingPathDelimiter(DirectorioReportesRoot) + nombreBase + '.dot';
        pngFilePath := ChangeFileExt(dotFilePath, '.png');

        // Llama a la función GenerateDot del objeto MerkleTree
        GuardarStringAArchivo(usuarioActual^.privados.GenerateDot, dotFilePath);

        if ConvertirDotAPng(dotFilePath) then
           archivosGenerados.Add(pngFilePath)
        else
           archivosGenerados.Add('Error generando: ' + nombreBase);
      end;
      usuarioActual := usuarioActual^.siguiente;
    end;

    if archivosGenerados.Count > 0 then
       ShowMessage('Reportes de Árbol Merkle generados en ' + DirectorioReportesRoot + ':' + #13#10 + archivosGenerados.Text)
    else
       ShowMessage('Ningún usuario tiene correos privados para generar reportes Merkle.');

  finally
    archivosGenerados.Free;
  end;
end;

// =======================================================
// REPORTE DE USUARIOS (LISTA SIMPLE ENLAZADA) - IMPLEMENTACIÓN DOT
// =======================================================
procedure TForm2.btnReporteUsuariosClick(Sender: TObject);
var
  RutaCarpeta, NombreDOT, NombrePNG, RutaDOT, RutaPNG: string;
  Proc: TProcess;
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

  // 3. EJECUTAR GRAPHVIZ (Generación automática de PNG)
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
  Proc: TProcess;
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

  // 3. EJECUTAR GRAPHVIZ (Generación automática de PNG)
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

procedure TForm2.btnCerrarSesionClick(Sender: TObject);
begin
  // 1. Limpiar el estado de la sesión Root
  EsUsuarioRoot := False;
  UsuarioActual := nil;

  // 2. Mostrar el formulario de Login (TForm1 de UPrincipal)
  Form1.Show;

  // 3. Cerrar el formulario actual (Root)
  Self.Close;
end;

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

          // Insertar correo: Seguridad contra punteros nulos
          if BandejaDestino <> nil then
          begin
            InsertarCorreo(BandejaDestino^.BandejaEntrada,
              CorreoObj.Get('id', 0),
              CorreoObj.Get('remitente', ''),
              CorreoObj.Get('destinatario', ''),
              EstadoCorreo,
              False, // No son programados en esta carga
              CorreoObj.Get('asunto', ''),
              FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
              CorreoObj.Get('mensaje', ''));

            // Actualizar Matriz de Relaciones
            if (BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('remitente', '')) <> nil) and
               (BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('destinatario', '')) <> nil) then
            begin
              InsertarValor(MatrizRelaciones,
                            BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('remitente', ''))^.Id,
                            BuscarUsuarioPorEmail(ListaUsuariosGlobal, CorreoObj.Get('destinatario', ''))^.Id,
                            1);
            end;
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

// *** NUEVO PROCEDIMIENTO (Fase 3) ***
procedure TForm2.CargarContactosDesdeJSON(const archivo: string);
var
  jsonText: string;
  jsonData, jsonUsuariosArray, jsonUsuarioItem, jsonContactosArray: TJSONData;
  jsonContactoItem: TJSONStringType;
  i, j: Integer;
  usuarioPrincipalMail, contactoMail: string;
  usuarioPrincipalPtr: PUsuario;
  contactosCargados, contactosOmitidos: Integer;
  SL: TStringList;
begin
   contactosCargados := 0;
   contactosOmitidos := 0;
   SL := TStringList.Create;
   try
      SL.LoadFromFile(archivo);
      jsonText := SL.Text;
   except
      on E: Exception do begin
         ShowMessage('Error al leer el archivo JSON: ' + E.Message);
         SL.Free;
         Exit;
      end;
   end;
   SL.Free;

   try
      jsonData := GetJSON(jsonText);
      if not (jsonData is TJSONObject) then raise Exception.Create('El JSON raíz no es un objeto.');

      if not TJSONObject(jsonData).FindPath('Usuarios', jsonUsuariosArray) or not (jsonUsuariosArray is TJSONArray) then
         raise Exception.Create('No se encontró el array "Usuarios" o no es válido.');

      for i := 0 to TJSONArray(jsonUsuariosArray).Count - 1 do
      begin
         if not (TJSONArray(jsonUsuariosArray).Items[i] is TJSONObject) then Continue;
         jsonUsuarioItem := TJSONObject(TJSONArray(jsonUsuariosArray).Items[i]);

         // Obtener el email del usuario principal
         if not jsonUsuarioItem.FindPathAsString('Usuario', usuarioPrincipalMail) then Continue;

         // Buscar este usuario en nuestra lista
         usuarioPrincipalPtr := BuscarUsuarioPorEmail(ListaUsuariosGlobal, usuarioPrincipalMail);
         if usuarioPrincipalPtr = nil then
         begin
            // Si el usuario principal no existe, omitimos sus contactos
            if jsonUsuarioItem.FindPath('Contactos', jsonContactosArray) and (jsonContactosArray is TJSONArray) then
              Inc(contactosOmitidos, TJSONArray(jsonContactosArray).Count);
            Continue;
         end;

         // Procesar la lista de contactos para este usuario
         if jsonUsuarioItem.FindPath('Contactos', jsonContactosArray) and (jsonContactosArray is TJSONArray) then
         begin
            for j := 0 to TJSONArray(jsonContactosArray).Count - 1 do
            begin
               if not (TJSONArray(jsonContactosArray).Items[j] is TJSONStringType) then
               begin
                  Inc(contactosOmitidos);
                  Continue;
               end;
               jsonContactoItem := TJSONStringType(TJSONArray(jsonContactosArray).Items[j]);
               contactoMail := jsonContactoItem.AsString;

               // Agregar contacto a la lista del usuario
               AgregarContactoALista(usuarioPrincipalPtr^.contacts, contactoMail);
               Inc(contactosCargados);
            end;
         end;
      end;

      ShowMessage(Format('Carga de contactos completada.' + #13#10 +
        'Contactos agregados: %d' + #13#10 +
        'Contactos omitidos (Usuario principal no encontrado o dato inválido): %d',
        [contactosCargados, contactosOmitidos]));

   except
      on E: Exception do
         ShowMessage('Error procesando el JSON de contactos: ' + E.Message);
   end;
end;

// *** ASEGÚRATE DE TENER ESTA FUNCIÓN (copiada de referencia si es necesario) ***
function TForm2.ConvertirDotAPng(nombreArchivoDot: string): Boolean;
var
  rutaCompletaPng: string;
  dotProcess: TProcess;
  DirectorioSalida: String;
begin
  Result := False; // Por defecto
  if not FileExists(nombreArchivoDot) then
  begin
    ShowMessage('Error: El archivo .dot no existe: ' + nombreArchivoDot);
    Exit;
  end;

  rutaCompletaPng := ChangeFileExt(nombreArchivoDot, '.png');
  DirectorioSalida := ExtractFilePath(nombreArchivoDot); // Obtener directorio del .dot

  dotProcess := TProcess.Create(nil);
  try
    dotProcess.Executable := 'dot'; // Asegúrate que 'dot' esté en el PATH del sistema
    dotProcess.Parameters.Add('-Tpng');
    dotProcess.Parameters.Add(nombreArchivoDot);
    dotProcess.Parameters.Add('-o');
    dotProcess.Parameters.Add(rutaCompletaPng);
    // Especificar directorio de trabajo puede ayudar si hay problemas de ruta
    dotProcess.CurrentDirectory := DirectorioSalida;
    dotProcess.Options := [poWaitOnExit, poUsePipes]; // poUsePipes puede ayudar a capturar errores
    dotProcess.Execute;

    if dotProcess.ExitCode <> 0 then
      ShowMessage('Error al ejecutar Graphviz (dot). Código: ' + IntToStr(dotProcess.ExitCode) + #13#10 +
                  'Asegúrese que Graphviz esté instalado y en el PATH.')
    else
      Result := True; // Éxito
  finally
    dotProcess.Free;
  end;
end;

// *** FUNCIÓN AUXILIAR PARA GUARDAR STRING ***
procedure TForm2.GuardarStringAArchivo(const contenido, rutaArchivo: string);
var
  F: TextFile;
begin
  AssignFile(F, rutaArchivo);
  try
    Rewrite(F);
    Write(F, contenido);
  finally
    CloseFile(F);
  end;
end;

end.
