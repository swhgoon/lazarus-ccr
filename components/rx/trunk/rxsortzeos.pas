unit RxSortZeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRxSortZeos = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation
uses exsortzeos;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxSortZeos]);
end;

end.
