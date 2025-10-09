unit UROOT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

procedure TForm2.btnCargaMasivaClick(Sender: TObject);
begin

end;

procedure TForm2.btnReporteUsuariosClick(Sender: TObject);
begin

end;

procedure TForm2.btnReporteRelacionesClick(Sender: TObject);
begin

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin

end;

end.

