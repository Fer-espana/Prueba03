unit UProgramarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DateTimePicker;

type

  { TForm5 }

  TForm5 = class(TForm)
    btnProgramar: TButton;
    Fecha: TDateTimePicker;
    editAsunto: TEdit;
    editDestinatario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoMensaje: TMemo;
    procedure FechaClick(Sender: TObject);
    procedure editAsuntoChange(Sender: TObject);
    procedure editDestinatarioChange(Sender: TObject);
    procedure MemoMensajeChange(Sender: TObject);
  private

  public

  end;

var
  Form5: TForm5;

implementation

{$R *.lfm}

{ TForm5 }

procedure TForm5.editDestinatarioChange(Sender: TObject);
begin

end;

procedure TForm5.MemoMensajeChange(Sender: TObject);
begin

end;

procedure TForm5.editAsuntoChange(Sender: TObject);
begin

end;

procedure TForm5.FechaClick(Sender: TObject);
begin

end;

end.

