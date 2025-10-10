unit UPapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type

  { TForm11 }

  TForm11 = class(TForm)
    btnBuscarPorAsunto: TButton;
    btnEliminarCorreo: TButton;
    editBuscarCorreoPorAsunto: TEdit;
    tablaInformacionCorreo: TStringGrid;
    procedure btnBuscarPorAsuntoClick(Sender: TObject);
    procedure btnEliminarCorreoClick(Sender: TObject);
    procedure editBuscarCorreoPorAsuntoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionCorreoClick(Sender: TObject);
    procedure tablaInformacionCorreoEditingDone(Sender: TObject);
  private

  public

  end;

var
  Form11: TForm11;

implementation

{$R *.lfm}

{ TForm11 }

procedure TForm11.FormCreate(Sender: TObject);
begin

end;

procedure TForm11.btnBuscarPorAsuntoClick(Sender: TObject);
begin

end;

procedure TForm11.btnEliminarCorreoClick(Sender: TObject);
begin

end;

procedure TForm11.editBuscarCorreoPorAsuntoChange(Sender: TObject);
begin

end;

procedure TForm11.FormDestroy(Sender: TObject);
begin

end;

procedure TForm11.tablaInformacionCorreoClick(Sender: TObject);
begin

end;

procedure TForm11.tablaInformacionCorreoEditingDone(Sender: TObject);
begin

end;

end.

