unit UGLOBAL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, Process, // AGREGADA LA UNIDAD "Process" PARA poWaitOnExit
  UListaSimpleUsuarios, UListaDobleEnlazadaCorreos,
  UListaCircularContactos, UColaCorreosProgramados,
  UPilaPapelera, UMatrizDispersaRelaciones;

// EL RESTO DEL CÓDIGO PERMANECE EXACTAMENTE IGUAL
type
  PBandejaUsuario = ^TBandejaUsuario;
  TBandejaUsuario = record
    Email: string;
    BandejaEntrada: TListaCorreos;
    Contactos: TListaContactos;
    Papelera: TPila;
    Siguiente: PBandejaUsuario;
  end;

var
  UsuarioActual: PUsuario;
  EsUsuarioRoot: Boolean;
  ColaCorreosProgramados: TCola;
  PilaPapeleraGlobal: TPila;
  MatrizRelaciones: TMatrizDispersa;
  ListaBandejas: PBandejaUsuario;
  UltimoIdCorreo: Integer;

function ObtenerBandejaUsuario(Email: string): PBandejaUsuario;
function CrearBandejaUsuario(Email: string): PBandejaUsuario;
procedure InicializarBandejasGlobales;
procedure GenerarReportesUsuario(EmailUsuario: string);
function GenerarIdCorreo: Integer;

implementation

// LAS IMPLEMENTACIONES DE FUNCIONES PERMANECEN EXACTAMENTE IGUALES
function ObtenerBandejaUsuario(Email: string): PBandejaUsuario;
var
  Actual: PBandejaUsuario;
begin
  Actual := ListaBandejas;
  while Actual <> nil do
  begin
    if Actual^.Email = Email then
      Exit(Actual);
    Actual := Actual^.Siguiente;
  end;
  Result := nil;
end;

function CrearBandejaUsuario(Email: string): PBandejaUsuario;
var
  Nueva: PBandejaUsuario;
begin
  New(Nueva);
  Nueva^.Email := Email;
  InicializarListaCorreos(Nueva^.BandejaEntrada);
  InicializarListaContactos(Nueva^.Contactos);
  InicializarPila(Nueva^.Papelera);
  Nueva^.Siguiente := ListaBandejas;
  ListaBandejas := Nueva;
  Result := Nueva;
end;

procedure InicializarBandejasGlobales;
begin
  ListaBandejas := nil;
end;

procedure GenerarReportesUsuario(EmailUsuario: string);
var
  CarpetaReportes: string;
  BandejaUsuario: PBandejaUsuario;
  NombreArchivoDOT, NombreArchivoPNG, RutaDOT, RutaPNG: string;
begin
  // 1. Definir la carpeta de reportes (ej: fes-edd.com-Reportes)
  CarpetaReportes := ExtractFilePath(Application.ExeName) +
                     StringReplace(EmailUsuario, '@', '-', [rfReplaceAll]) +
                     '-Reportes';

  if not DirectoryExists(CarpetaReportes) then
    ForceDirectories(CarpetaReportes);

  BandejaUsuario := ObtenerBandejaUsuario(EmailUsuario);

  if BandejaUsuario <> nil then
  begin
    // --- 1. REPORTE DE CORREOS RECIBIDOS (Lista Doble Enlazada) ---
    NombreArchivoDOT := 'correos_recibidos.dot';
    NombreArchivoPNG := 'correos_recibidos.png';
    RutaDOT := CarpetaReportes + PathDelim + NombreArchivoDOT;
    RutaPNG := CarpetaReportes + PathDelim + NombreArchivoPNG;

    GenerarReporteDOTCorreosRecibidos(BandejaUsuario^.BandejaEntrada, RutaDOT);
    // Ejecutar Graphviz - CORREGIDO: parámetros correctos
    if FileExists(RutaDOT) then
      ExecuteProcess('dot', ['-Tpng', RutaDOT, '-o', RutaPNG], [poWaitOnExit]);


    // --- 2. REPORTE DE PAPELERA (Pila) ---
    NombreArchivoDOT := 'papelera.dot';
    NombreArchivoPNG := 'papelera.png';
    RutaDOT := CarpetaReportes + PathDelim + NombreArchivoDOT;
    RutaPNG := CarpetaReportes + PathDelim + NombreArchivoPNG;

    GenerarReporteDOTPapelera(PilaPapeleraGlobal, RutaDOT);
    // Ejecutar Graphviz - CORREGIDO: parámetros correctos
    if FileExists(RutaDOT) then
      ExecuteProcess('dot', ['-Tpng', RutaDOT, '-o', RutaPNG], [poWaitOnExit]);


    // --- 3. REPORTE DE CORREOS PROGRAMADOS (Cola) ---
    NombreArchivoDOT := 'correos_programados.dot';
    NombreArchivoPNG := 'correos_programados.png';
    RutaDOT := CarpetaReportes + PathDelim + NombreArchivoDOT;
    RutaPNG := CarpetaReportes + PathDelim + NombreArchivoPNG;

    GenerarReporteDOTCorreosProgramados(ColaCorreosProgramados, RutaDOT);
    // Ejecutar Graphviz - CORREGIDO: parámetros correctos
    if FileExists(RutaDOT) then
      ExecuteProcess('dot', ['-Tpng', RutaDOT, '-o', RutaPNG], [poWaitOnExit]);


    // --- 4. REPORTE DE CONTACTOS (Lista Circular) ---
    NombreArchivoDOT := 'contactos.dot';
    NombreArchivoPNG := 'contactos.png';
    RutaDOT := CarpetaReportes + PathDelim + NombreArchivoDOT;
    RutaPNG := CarpetaReportes + PathDelim + NombreArchivoPNG;

    GenerarReporteDOTContactos(BandejaUsuario^.Contactos, RutaDOT);
    // Ejecutar Graphviz - CORREGIDO: parámetros correctos
    if FileExists(RutaDOT) then
      ExecuteProcess('dot', ['-Tpng', RutaDOT, '-o', RutaPNG], [poWaitOnExit]);
  end;

  ShowMessage('Reportes DOT y sus imágenes PNG generadas exitosamente en: ' + CarpetaReportes);
end;

function GenerarIdCorreo: Integer;
begin
  Inc(UltimoIdCorreo);
  Result := UltimoIdCorreo;
end;

initialization
begin
  UsuarioActual := nil;
  EsUsuarioRoot := False;
  InicializarCola(ColaCorreosProgramados);
  InicializarPila(PilaPapeleraGlobal);
  InicializarMatriz(MatrizRelaciones);
  InicializarBandejasGlobales;
  UltimoIdCorreo := 0;
end;

end.
