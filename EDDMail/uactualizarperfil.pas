unit UActualizarPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm7 }

  TForm7 = class(TForm)
    btnActualizar: TButton;
    editTelefono: TEdit;
    editEmail: TEdit;
    editUsuario: TEdit;
    editNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnActualizarClick(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure editNombreChange(Sender: TObject);
    procedure editTelefonoChange(Sender: TObject);
    procedure editUsuarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form7: TForm7;

implementation

{$R *.lfm}

{ TForm7 }

procedure TForm7.FormCreate(Sender: TObject);
begin

end;

procedure TForm7.editNombreChange(Sender: TObject);
begin

end;

procedure TForm7.editTelefonoChange(Sender: TObject);
begin

end;

procedure TForm7.editEmailChange(Sender: TObject);
begin

end;

procedure TForm7.btnActualizarClick(Sender: TObject);
begin

end;

procedure TForm7.editUsuarioChange(Sender: TObject);
begin

end;

procedure TForm7.FormDestroy(Sender: TObject);
begin

end;

end.

