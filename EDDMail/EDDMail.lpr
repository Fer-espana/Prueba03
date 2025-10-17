program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UPrincipal, UROOT, UUsuarioEstandar, UEnviarCorreo, UProgramarCorreo,
  datetimectrls, UAgregarContacto, UActualizarPerfil, URegistrarse,
  UBandejaEntrada, UVistadeCorreo, UPapelera, UCorreosProgramados,
  UVentanaContactos, UListaSimpleUsuarios, UListaDobleEnlazadaCorreos,
  UListaCircularContactos, UColaCorreosProgramados, UPilaPapelera,
  UMatrizDispersaRelaciones, UListadeListasComunidades, UGLOBAL, UNAVEGACION,
  UGestionComunidades, UCorregirBorrador, UVerBorradores;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  // SOLO creamos Form1 (login), los demás se crean dinámicamente
  Application.Run;
end.
