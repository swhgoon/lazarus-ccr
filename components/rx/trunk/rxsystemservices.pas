unit RxSystemServices;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRxServiceType = (sstAll, sstService, sstDrivers);
  TRxServiceStatus = (sssAll, sssActive, sssInactive);
  TRxServiceState = (srsStoped,          //SERVICE_STOPPED          : S := 'Сервис не запущен'
                     srsStartPending,    //SERVICE_START_PENDING    : S := 'Сервис в процессе запуска';
                     srsStopPending,     //SERVICE_STOP_PENDING     : S := 'Сервис в процессе завершения';
                     srsRunning,         //SERVICE_RUNNING          : S := 'Сервис запущен';
                     srsContinuePending, //SERVICE_CONTINUE_PENDING : S := 'Сервис в процессе запуска после временной оснановки';
                     srsPausePending,    //SERVICE_PAUSE_PENDING    : S := 'Сервис в процессе временной оснановки';
                     srsPaused           //SERVICE_PAUSED           : S := 'Сервис временно оснановлен';
                     );

  TRxServiceItem = record
    Name:string;
    Description:string;
    Status:TRxServiceState;
  end;

type

  { TRxSystemServices }

  TRxSystemServices = class(TComponent)
  private
    FItemCount: integer;
    FServerName: string;
    FServiceStatus: TRxServiceStatus;
    FServiceType: TRxServiceType;
    function GetItems(Index: integer): TRxServiceItem;
    procedure SetItemCount(const AValue: integer);
    procedure SetItems(Index: integer; const AValue: TRxServiceItem);
    procedure SetServerName(const AValue: string);
    procedure SetServiceStatus(const AValue: TRxServiceStatus);
    procedure SetServiceType(const AValue: TRxServiceType);
  protected
    procedure ClearItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index:integer]:TRxServiceItem read GetItems write SetItems;
    property ItemCount:integer read FItemCount write SetItemCount;
  published
    property ServerName:string read FServerName write SetServerName;
    property ServiceType:TRxServiceType read FServiceType write SetServiceType;     //(sstAll, sstService, sstDrivers);
    property ServiceStatus:TRxServiceStatus read FServiceStatus write SetServiceStatus; //(sssAll, sssActive, sssInactive);
  end;

implementation

{ TRxSystemServices }

procedure TRxSystemServices.SetServerName(const AValue: string);
begin
  if FServerName=AValue then exit;
  FServerName:=AValue;
end;

function TRxSystemServices.GetItems(Index: integer): TRxServiceItem;
begin

end;

procedure TRxSystemServices.SetItemCount(const AValue: integer);
begin
  if FItemCount=AValue then exit;
  FItemCount:=AValue;
end;

procedure TRxSystemServices.SetItems(Index: integer;
  const AValue: TRxServiceItem);
begin

end;

procedure TRxSystemServices.SetServiceStatus(const AValue: TRxServiceStatus);
begin
  if FServiceStatus=AValue then exit;
  FServiceStatus:=AValue;
end;

procedure TRxSystemServices.SetServiceType(const AValue: TRxServiceType);
begin
  if FServiceType=AValue then exit;
  FServiceType:=AValue;
end;

procedure TRxSystemServices.ClearItems;
begin
  FItemCount:=0;
end;

constructor TRxSystemServices.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRxSystemServices.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

end.
