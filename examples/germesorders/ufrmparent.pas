unit ufrmParent; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs; 

type

  { TfrmParent }

  TfrmParent = class(TForm)
  private
    { private declarations }
  protected
    (*
    {$IFDEF LCLwince}
    procedure DoShow; override;
    {$ENDIF}
    *)
  public
    { public declarations }
    {$IFDEF LCLwince}
    procedure WindowResize;
    {$ENDIF}
  end; 

{$IFDEF LCLwince}
procedure TaskBarHide;
procedure TaskBarUnHide;
{$ENDIF}

implementation
uses uDebug
  {$IFDEF LCLwince},Windows{$ENDIF};

{$IFDEF LCLwince}
function TaskBarHwnd:HWND;
begin
  Result:=FindWindow('HHTaskBar', nil);
  //Result:=FindWindow('SipWndClass', nil);
  //Result:=FindWindow('MS_SIPBUTTON', nil);
end;

procedure TaskBarHide;
var H:HWND;
    S:String;

begin
  GlobalLogger.Log('Попытка скрыть TaskBar');

  H:=TaskBarHwnd;
  if (H <> 0) then
    begin
      S:='...закончилась удачно';
      ShowWindow(H, SW_HIDE);
    end
    else
    begin
      S:='...закончилась неудачно';
    end;
  GlobalLogger.Log(S);
end;

procedure TaskBarUnHide;
var H:HWND;
  S:String;
begin
  GlobalLogger.Log('Попытка показать TaskBar');
  H:=TaskBarHwnd;
  if (H <> 0) then
    begin
      S:='...закончилась удачно';
      ShowWindow(H, SW_SHOW);
    end
    else
    begin
      S:='...закончилась неудачно';
    end;
  GlobalLogger.Log(S);
end;
{$ENDIF}

{$IFDEF LCLwince}
procedure TfrmParent.WindowResize;
var WR:Windows.Rect;
begin
  if SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0) then
    begin
      {SetWindowPos(Handle,HWND_TOPMOST,0,0,WR.right -
        WR.left,WR.bottom - WR.top, SWP_SHOWWINDOW);}
      SetWindowPos(Handle,HWND_TOP,0,0,WR.right -
        WR.left,WR.bottom - WR.top, SWP_SHOWWINDOW);
    end;
end;

{
procedure TfrmParent.DoShow;
begin
  inherited;
  WindowResize;
end;
}
{$ENDIF}

initialization
  {$I ufrmparent.lrs}

end.

