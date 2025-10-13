// UGLOBAL.pas - SOLO CAMBIOS NECESARIOS
unit UGLOBAL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, // UNIDADES AGREGADAS PARA CORREGIR ERRORES
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
begin
  CarpetaReportes := ExtractFilePath(Application.ExeName) +
                     StringReplace(EmailUsuario, '@', '-', [rfReplaceAll]) +
                     '-Reportes';

  if not DirectoryExists(CarpetaReportes) then
    ForceDirectories(CarpetaReportes);

  BandejaUsuario := ObtenerBandejaUsuario(EmailUsuario);

  if BandejaUsuario <> nil then
  begin
    GenerarReporteDOTCorreosRecibidos(BandejaUsuario^.BandejaEntrada,
      CarpetaReportes + PathDelim + 'correos_recibidos.dot');

    GenerarReporteDOTPapelera(PilaPapeleraGlobal,
      CarpetaReportes + PathDelim + 'papelera.dot');

    GenerarReporteDOTCorreosProgramados(ColaCorreosProgramados,
      CarpetaReportes + PathDelim + 'correos_programados.dot');

    GenerarReporteDOTContactos(BandejaUsuario^.Contactos,
      CarpetaReportes + PathDelim + 'contactos.dot');
  end;

  ShowMessage('Reportes generados en: ' + CarpetaReportes + sLineBreak +
              'Para generar imágenes ejecute:' + sLineBreak +
              'dot -Tpng archivo.dot -o archivo.png');
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
