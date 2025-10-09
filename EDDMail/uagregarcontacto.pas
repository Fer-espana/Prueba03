unit UAgregarContacto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, Grids;

type

  { TForm6 }

  TForm6 = class(TForm)
    btnAgregarContacto: TButton;
    editEmail: TEdit;
    Label1: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form6: TForm6;

implementation

{$R *.lfm}

{ TForm6 }

procedure TForm6.FormCreate(Sender: TObject);
begin

end;

procedure TForm6.editEmailChange(Sender: TObject);
begin

end;

procedure TForm6.btnAgregarContactoClick(Sender: TObject);
begin

end;


procedure TForm6.FormDestroy(Sender: TObject);
begin

end;

end.

