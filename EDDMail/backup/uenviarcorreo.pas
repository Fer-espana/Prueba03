unit UEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm4 }

  TForm4 = class(TForm)
    btnEnviar: TButton;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoMensaje: TMemo;
    procedure btnEnviarClick(Sender: TObject);
    procedure editAsuntoChange(Sender: TObject);
    procedure editDestinatarioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoMensajeChange(Sender: TObject);
  private

  public

  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin

end;

procedure TForm4.btnEnviarClick(Sender: TObject);
begin

end;

procedure TForm4.editAsuntoChange(Sender: TObject);
begin

end;

procedure TForm4.editDestinatarioChange(Sender: TObject);
begin

end;

procedure TForm4.FormDestroy(Sender: TObject);
begin

end;

procedure TForm4.MemoMensajeChange(Sender: TObject);
begin

end;

end.

