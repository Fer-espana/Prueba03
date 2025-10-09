unit UBandejaEntrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type

  { TForm9 }

  TForm9 = class(TForm)
    btnOrdenAlfabetico: TButton;
    editNumeroNoLeidos: TEdit;
    tablaInformacion: TStringGrid;
    procedure btnOrdenAlfabeticoClick(Sender: TObject);
    procedure editNumeroNoLeidosChange(Sender: TObject);
    procedure editNumeroNoLeidosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tablaInformacionClick(Sender: TObject);
    procedure tablaInformacionDblClick(Sender: TObject);
  private

  public

  end;

var
  Form9: TForm9;

implementation

{$R *.lfm}

{ TForm9 }

procedure TForm9.FormCreate(Sender: TObject);
begin

end;

procedure TForm9.btnOrdenAlfabeticoClick(Sender: TObject);
begin

end;

procedure TForm9.editNumeroNoLeidosChange(Sender: TObject);
begin

end;

procedure TForm9.editNumeroNoLeidosClick(Sender: TObject);
begin

end;

procedure TForm9.FormDestroy(Sender: TObject);
begin

end;

procedure TForm9.tablaInformacionClick(Sender: TObject);
begin

end;

procedure TForm9.tablaInformacionDblClick(Sender: TObject);
begin

end;

end.

