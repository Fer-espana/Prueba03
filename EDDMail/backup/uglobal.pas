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

initialization
begin
  UsuarioActual := nil;
  EsUsuarioRoot := False;
  InicializarCola(ColaCorreosProgramados);
  InicializarPila(PilaPapeleraGlobal);
  InicializarMatriz(MatrizRelaciones);
  InicializarBandejasGlobales;
end;

end.
