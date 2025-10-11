unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UListaSimpleUsuarios, URegistrarse, UUsuarioEstandar, UROOT, UGLOBAL, fpjson, jsonparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLogin: TButton;
    btnRegistrar: TButton;
    edtEmail: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    procedure btnLoginClick(Sender: TObject);
    procedure btnRegistrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CargarUsuariosDesdeJSON;
    procedure GuardarUsuariosEnJSON; // AGREGAR ESTA DECLARACIÓN
  public
  end;

var
  Form1: TForm1;

const
  ROOT_EMAIL = 'root@edd.com';
  ROOT_PASSWORD = 'root123';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Login';

  // Cargar usuarios desde JSON al iniciar
  CargarUsuariosDesdeJSON;

  // Insertar usuario root si no existe
  if BuscarUsuarioPorEmail(ListaUsuariosGlobal, ROOT_EMAIL) = nil then
  begin
    InsertarUsuario(ListaUsuariosGlobal, 1, 'Administrador Root', 'root',
      ROOT_EMAIL, '0000-0000');
    StatusBar1.SimpleText := 'Sistema inicializado. Usuario root creado.';

    // Guardar el usuario root en el JSON
    GuardarUsuariosEnJSON;
  end
  else
    StatusBar1.SimpleText := 'Sistema inicializado. ' + IntToStr(ListaUsuariosGlobal.Count) + ' usuario(s) cargados.';
end;

// MANTENER LA IMPLEMENTACIÓN DE GuardarUsuariosEnJSON DONDE ESTÁ
procedure TForm1.GuardarUsuariosEnJSON;
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

  finally
    CloseFile(Archivo);
  end;
end;

procedure TForm1.CargarUsuariosDesdeJSON;
var
  Archivo: TextFile;
  Linea, JSONContent: string;
  Parser: TJSONParser;
  JSONData: TJSONData;
  UsuariosArray: TJSONArray;
  i: Integer;
  UsuarioObj: TJSONObject;
  RutaArchivo: string;
begin
  RutaArchivo := ExtractFilePath(Application.ExeName) + 'Data' + PathDelim + 'usuarios.json';

  if not FileExists(RutaArchivo) then
  begin
    // Crear directorio Data si no existe
    ForceDirectories(ExtractFilePath(RutaArchivo));
    Exit;
  end;

  try
    // Leer archivo JSON
    AssignFile(Archivo, RutaArchivo);
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
      if (JSONData <> nil) and (JSONData.FindPath('usuarios') <> nil) then
      begin
        UsuariosArray := TJSONArray(JSONData.FindPath('usuarios'));
        for i := 0 to UsuariosArray.Count - 1 do
        begin
          UsuarioObj := TJSONObject(UsuariosArray.Items[i]);

          // Insertar usuario en la lista global (evitar duplicados)
          if BuscarUsuarioPorEmail(ListaUsuariosGlobal, UsuarioObj.Get('email', '')) = nil then
          begin
            InsertarUsuario(ListaUsuariosGlobal,
              UsuarioObj.Get('id', 0),
              UsuarioObj.Get('nombre', ''),
              UsuarioObj.Get('usuario', ''),
              UsuarioObj.Get('email', ''),
              UsuarioObj.Get('telefono', ''));
          end;
        end;
      end;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
      // Silenciosamente ignorar errores de carga
  end;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
var
  Usuario: PUsuario;
  FormUsuario: TForm3;
  FormRoot: TForm2;
begin
  // Verificar credenciales root
  if (edtEmail.Text = ROOT_EMAIL) and (edtPassword.Text = ROOT_PASSWORD) then
  begin
    EsUsuarioRoot := True;
    UsuarioActual := nil;

    ShowMessage('¡Bienvenido Root!');
    StatusBar1.SimpleText := 'Sesión root iniciada';

    // Navegar a formulario Root - CORREGIDO
    FormRoot := TForm2.Create(nil);
    FormRoot.Show;
    Self.Hide; // Ocultar login
    Exit;
  end;

  // Buscar usuario normal en la lista GLOBAL
  Usuario := BuscarUsuarioPorEmail(ListaUsuariosGlobal, edtEmail.Text);
  if Usuario = nil then
  begin
    ShowMessage('Error: Usuario no encontrado');
    Exit;
  end;

  // Configurar usuario actual
  EsUsuarioRoot := False;
  UsuarioActual := Usuario;

  ShowMessage('¡Bienvenido ' + Usuario^.Nombre + '!');
  StatusBar1.SimpleText := 'Sesión de usuario iniciada: ' + Usuario^.Nombre;

  // Navegar a formulario de usuario estándar - CORREGIDO
  FormUsuario := TForm3.Create(nil);
  FormUsuario.SetUsuarioActual(Usuario);
  FormUsuario.Show;
  Self.Hide; // Ocultar login
end;

procedure TForm1.btnRegistrarClick(Sender: TObject);
begin
  // Abrir formulario de registro - CORREGIDO
  Form8 := TForm8.Create(nil);
  try
    Form8.ShowModal;
  finally
    Form8.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LiberarListaUsuarios(ListaUsuariosGlobal);
end;

end.
