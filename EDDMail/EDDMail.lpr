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
  Forms,
  // Unidades existentes (asegúrate que estén todas)
  uprincipal, uregistrarse, uglobal, ulistasimpleusuarios,
  ulistadobleenlazadacorreos, umatrizdispersarelaciones, uroot,
  uusuarioestandar, uagregarcontacto, ulistacircularcontactos,
  uventanacontactos, uactualizarperfil, uenviarcorreo, upilapapelera,
  ucolacorreosprogramados, upapelera, uprogramarcorreo, ucorreosprogramados,
  uarbolb, ufavoritos, uvistadefavorito, uavltreeborradores, uverborradores,
  ucorregirborrador, ugestioncomunidades, ulistadelistascomunidades,
  uvistadecorreo, unavegacion, // <-- Añade las que falten de tu proyecto
  // Unidades Nuevas (Fase 3)
  blockchain, bitacora, privado, lzw,
  fbitacora, privados // <-- Formularios nuevos
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  // Inicializaciones existentes
  InicializarLista(ListaUsuarios);
  InicializarMatriz(MatrizRelaciones);
  InicializarArbolComunidades(ArbolComunidades); // Asumiendo que tienes esta variable global

  // *** NUEVAS INICIALIZACIONES (Fase 3) ***
  InicializarBlockchain(SistemaBlockchain); // Declara SistemaBlockchain: TBlockchain; en uglobal.pas o aquí
  InicializarBitacora(LogAccesos);        // Declara LogAccesos: TListaAccesos; en uglobal.pas o aquí

  // Crear y correr el formulario principal
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
