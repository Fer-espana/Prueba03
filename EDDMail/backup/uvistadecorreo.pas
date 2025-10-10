unit UVistadeCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TForm10 }

  TForm10 = class(TForm)
    btnEliminarCorreo: TButton;
    tablaInformacionCorreo: TStringGrid;
    procedure btnEliminarCorreoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionCorreoClick(Sender: TObject);
    procedure tablaInformacionCorreoEditingDone(Sender: TObject);
  private

  public

  end;

var
  Form10: TForm10;

implementation

{$R *.lfm}

{ TForm10 }

procedure TForm10.btnEliminarCorreoClick(Sender: TObject);
begin

end;

procedure TForm10.FormCreate(Sender: TObject);
begin

end;

procedure TForm10.FormDestroy(Sender: TObject);
begin

end;

procedure TForm10.tablaInformacionCorreoClick(Sender: TObject);
begin

end;

procedure TForm10.tablaInformacionCorreoEditingDone(Sender: TObject);
begin

end;

end.

