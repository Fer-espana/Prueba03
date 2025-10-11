// UGLOBAL.pas - CORREGIDO
unit UGLOBAL;

{$mode objfpc}{$H+}

interface

uses
  UListaSimpleUsuarios, UListaDobleEnlazadaCorreos,
  UListaCircularContactos, UColaCorreosProgramados,
  UPilaPapelera, UMatrizDispersaRelaciones;
  // Quitamos UListadeListasComunidades ya que no se usa aún

type
  // Estructura para manejar bandejas por usuario
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
  UltimoIdCorreo: Integer;
  UltimoIdUsuario: Integer;
  UltimoIdContacto: Integer;

  // Estructuras globales
  ColaCorreosProgramados: TCola;
  PilaPapeleraGlobal: TPila;
  MatrizRelaciones: TMatrizDispersa;
  ListaBandejas: PBandejaUsuario;  // Lista de todas las bandejas

// Funciones para manejar bandejas
function ObtenerBandejaUsuario(Email: string): PBandejaUsuario;
function CrearBandejaUsuario(Email: string): PBandejaUsuario;
procedure InicializarBandejasGlobales;

implementation

function GenerarIdCorreo: Integer;
begin
  Inc(UltimoIdCorreo);
  Result := UltimoIdCorreo;
end;

function GenerarIdUsuario: Integer;
begin
  Inc(UltimoIdUsuario);
  Result := UltimoIdUsuario;
end;

function GenerarIdContacto: Integer;
begin
  Inc(UltimoIdContacto);
  Result := UltimoIdContacto;
end;


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

// En UGLOBAL.pas - verificar que tenga esto en initialization
initialization
begin
  UltimoIdCorreo := 0;
  UltimoIdUsuario := 0;
  UltimoIdContacto := 0;
  UsuarioActual := nil;
  EsUsuarioRoot := False;
  InicializarCola(ColaCorreosProgramados);  // ESTA LÍNEA ES CRÍTICA
  InicializarPila(PilaPapeleraGlobal);
  InicializarMatriz(MatrizRelaciones);
  InicializarBandejasGlobales;
end;

end.
