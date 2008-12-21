program germesorders;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this },
  uDebug, SysUtils, rx,
  uOrders, uTestForm, ufrmParent;

begin
  GlobalLogger.Log('Старт приложения GermesOrders. %s', [FormatDateTime('dd-mm-yyyy hh-mm-ss', Now)]);

  try
    try
      {$IFDEF LCLwince}
      TaskBarHide;
      
      GlobalLogger.Log('Запуск под WinCE');
      Application.ApplicationType:=atPDA;
      {$ENDIF}
      
      Application.Initialize;
      Application.OnException:=@GlobalLogger.ExceptionHandler;
      Application.StopOnException:=False;

      Application.CreateForm(TfrmOrders, frmOrders);
      {$IFDEF LCLwince}
      frmOrders.WindowResize;
      {$ENDIF}
      Application.Run;
    except
      on E:Exception do
        begin
          GlobalLogger.LogException(E);
        end;
    end;
  finally
    {$IFDEF LCLwince}
    TaskBarUnHide;
    {$ENDIF}
      
    GlobalLogger.Log('Завершение приложения GermesOrders');
  end;
end.

