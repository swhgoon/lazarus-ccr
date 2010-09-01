{
    This file is part of the Web Service Toolkit
    Copyright (c) 2010 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit client_utils;

interface
uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, wst_types, filter_intf;

Type

{$M+}
  
  { TBaseTransport }

  TBaseTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FFilter : IDataFilter;
  protected
    function HasFilter() : Boolean;
    function GetFilter() : IDataFilter;
    function GetFilterString: string;
    procedure SetFilterString(const Value: string);
    procedure FilterInput(ASource, ADest : TStream);
    procedure FilterOutput(ASource, ADest : TStream);
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream); virtual; 
    function GetCookieManager() : ICookieManager; virtual; 
  published
    property FilterString : string read GetFilterString write SetFilterString;
  End;
{$M+}


implementation
uses
  wst_consts;

{ TBaseTransport }

constructor TBaseTransport.Create();
begin
  inherited;
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TBaseTransport.Destroy();
begin
  FPropMngr := Nil;
  inherited;
end;

procedure TBaseTransport.SendAndReceive(ARequest, AResponse : TStream); 
begin
  raise ETransportExecption.CreateFmt(SERR_UnsupportedOperation,['SendAndReceive']);
end;

function TBaseTransport.GetCookieManager() : ICookieManager; 
begin
  raise ETransportExecption.CreateFmt(SERR_UnsupportedOperation,['GetCookieManager']);
end;

procedure TBaseTransport.FilterInput(ASource, ADest: TStream);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteInput(TMemoryStream(ASource).Memory^,ASource.Size);
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteInput(locInBuffer[0],Length(locInBuffer));
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

procedure TBaseTransport.FilterOutput(ASource, ADest: TStream);
var
  locInBuffer, locBuffer : TByteDynArray;
  locOldPos : Int64;
begin
  if ASource.InheritsFrom(TMemoryStream) then begin
    locBuffer := FFilter.ExecuteOutput(TMemoryStream(ASource).Memory^,ASource.Size);
  end else begin
    SetLength(locInBuffer,ASource.Size);
    locOldPos := ASource.Position;
    ASource.Position := 0;
    try
      ASource.Read(locInBuffer[0],Length(locInBuffer));
    finally
      ASource.Position := locOldPos;
    end;
    locBuffer := FFilter.ExecuteOutput(locInBuffer[0],Length(locInBuffer));
  end;
  ADest.Size := Length(locBuffer);
  ADest.Position := 0;
  ADest.Write(locBuffer[0],Length(locBuffer));
  ADest.Position := 0;
end;

function TBaseTransport.GetFilter() : IDataFilter;
begin
  Result := FFilter;
end;

function TBaseTransport.GetFilterString: string;
var
  locPM : IPropertyManager;
  ls : TStringList;
  locRes, s : string;
  i : Integer;
begin
  locRes := '';
  if ( FFilter <> nil ) then begin
    locRes := FFilter.GetName();
    locPM := FFilter.GetPropertyManager();
    ls := TStringList.Create();
    try
      if ( locPM.GetPropertyNames(ls) > 0 ) then begin
        for i := 0 to Pred(ls.Count) do begin
          s := ls[i];
          locRes := Format('%s,%s>%s',[locRes,s,locPM.GetProperty(s)]);
        end;
      end;
    finally
      ls.Free();
    end;
  end;
  Result := locRes;
end;

function TBaseTransport.GetPropertyManager() : IPropertyManager;
begin
  Result := FPropMngr;
end;

function TBaseTransport.HasFilter() : Boolean;
begin
  Result := (FFilter <> nil);
end;

procedure TBaseTransport.SetFilterString(const Value: string);
var
  locBuffer, locName, locValue : string;
  locPM : IPropertyManager;
  locFilterManager : IDataFilterRegistry;
  locFilter : IDataFilter;
begin
  locBuffer := Value;
  if IsStrEmpty(locBuffer) then begin
    FFilter := nil;
    Exit;
  end;

  //The filter name
  locName := Trim(GetToken(locBuffer,','));
  locFilterManager := GetDataFilterRegistry();
  if not locFilterManager.Find(locName,locFilter) then
    raise ETransportExecption.CreateFmt(SERR_DataFilterNotFound,[locName]);
  locPM := locFilter.GetPropertyManager();
  while True do begin
    locName := GetToken(locBuffer,'>');
    if IsStrEmpty(locName) then
      Break;
    locValue := GetToken(locBuffer,',');
    locPM.SetProperty(locName,locValue);
  end;
  FFilter := locFilter;
end;

end.
