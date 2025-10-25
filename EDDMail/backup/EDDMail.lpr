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
  UGestionComunidades, UAVLTreeBorradores, UArbolB, UCorregirBorrador,
  UVerBorradores, Blockchain, Bitacora, Privado, LZW, FBitacora, Privados;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm14, Form14);
  Application.CreateForm(TForm16, Form16);
  // SOLO creamos Form1 (login), los demás se crean dinámicamente
  Application.Run;
end.
