unit UCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type

  { TForm12 }

  TForm12 = class(TForm)
    btnEnviarCorreosProgramados: TButton;
    tablaCorreosProgramados: TStringGrid;
    procedure btnEnviarCorreosProgramadosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaCorreosProgramadosClick(Sender: TObject);
  private

  public

  end;

var
  Form12: TForm12;

implementation

{$R *.lfm}

{ TForm12 }

procedure TForm12.btnEnviarCorreosProgramadosClick(Sender: TObject);
begin

end;

procedure TForm12.FormCreate(Sender: TObject);
begin

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin

end;

procedure TForm12.tablaCorreosProgramadosClick(Sender: TObject);
begin

end;

end.

