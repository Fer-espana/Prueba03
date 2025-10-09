unit URegistrarse;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm8 }

  TForm8 = class(TForm)
    btnRegistrar: TButton;
    editPassword: TEdit;
    editTelefono: TEdit;
    editUsuario: TEdit;
    editEmail: TEdit;
    editNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnRegistrarClick(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure editNombreChange(Sender: TObject);
    procedure editPasswordChange(Sender: TObject);
    procedure editTelefonoChange(Sender: TObject);
    procedure editUsuarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form8: TForm8;

implementation

{$R *.lfm}

{ TForm8 }

procedure TForm8.FormCreate(Sender: TObject);
begin

end;

procedure TForm8.editNombreChange(Sender: TObject);
begin

end;

procedure TForm8.editPasswordChange(Sender: TObject);
begin

end;

procedure TForm8.editTelefonoChange(Sender: TObject);
begin

end;

procedure TForm8.editUsuarioChange(Sender: TObject);
begin

end;

procedure TForm8.editEmailChange(Sender: TObject);
begin

end;

procedure TForm8.btnRegistrarClick(Sender: TObject);
begin

end;

procedure TForm8.FormDestroy(Sender: TObject);
begin

end;

end.

