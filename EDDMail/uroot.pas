unit UROOT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UGLOBAL,
  UListaSimpleUsuarios, fpjson, jsonparser, UMatrizDispersaRelaciones;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON;
    procedure GuardarUsuariosEnJSON;
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
  Parser: TJSONParser;
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
    Parser := TJSONParser.Create(JSONContent);
    try
      JSONData := Parser.Parse;
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
            UsuarioObj.Get('telefono', ''));
        end;
        ShowMessage('Usuarios cargados exitosamente desde Data/usuarios.json');
      end;
    finally
      Parser.Free;
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
      WriteLn(Archivo, '      "telefono": "', Actual^.Telefono, '"');
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
  // Recargar usuarios desde JSON
  CargarUsuariosDesdeJSON;
  ShowMessage('Usuarios cargados exitosamente desde Data/usuarios.json' + sLineBreak +
              'Total: ' + IntToStr(ListaUsuariosGlobal.Count) + ' usuarios');
end;

procedure TForm2.btnReporteUsuariosClick(Sender: TObject);
begin
  // Guardar usuarios actuales en JSON
  GuardarUsuariosEnJSON;
  ShowMessage('Reporte de usuarios generado en: Data/usuarios.json');
end;

procedure TForm2.btnReporteRelacionesClick(Sender: TObject);
var
  RutaCarpeta, RutaArchivo: string;
begin
  RutaCarpeta := ExtractFilePath(Application.ExeName) + 'Root-Reportes';
  RutaArchivo := RutaCarpeta + PathDelim + 'reporte_relaciones.txt';

  // Crear carpeta si no existe
  if not DirectoryExists(RutaCarpeta) then
    ForceDirectories(RutaCarpeta);

  // Generar reporte de relaciones
  GenerarReporteRelaciones(MatrizRelaciones, RutaArchivo);
  ShowMessage('Reporte de relaciones generado en: ' + RutaArchivo);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  // Limpiar estado global
  EsUsuarioRoot := False;
  UsuarioActual := nil;
end;

end.
