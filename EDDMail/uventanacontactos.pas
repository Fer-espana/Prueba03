unit UVentanaContactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TForm13 }

  TForm13 = class(TForm)
    btnContactoAnterior: TButton;
    btnContactoSiguiente: TButton;
    tablaContacto: TStringGrid;
    procedure btnContactoAnteriorClick(Sender: TObject);
    procedure btnContactoSiguienteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaContactoClick(Sender: TObject);
  private

  public

  end;

var
  Form13: TForm13;

implementation

{$R *.lfm}

{ TForm13 }

procedure TForm13.FormCreate(Sender: TObject);
begin

end;

procedure TForm13.btnContactoAnteriorClick(Sender: TObject);
begin

end;

procedure TForm13.btnContactoSiguienteClick(Sender: TObject);
begin

end;

procedure TForm13.FormDestroy(Sender: TObject);
begin

end;

procedure TForm13.tablaContactoClick(Sender: TObject);
begin

end;

end.

