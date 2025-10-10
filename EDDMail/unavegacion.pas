unit UNAVEGACION;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

procedure MostrarLogin;

implementation

uses UPrincipal;

procedure MostrarLogin;
begin
  if Assigned(Form1) then
    Form1.Show;
end;

end.
