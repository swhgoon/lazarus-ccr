{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_imp".
  Date            : "01/07/2006 22:14".
}
Unit metadata_service_imp;
{$INCLUDE wst.inc}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, metadata_service;

Type


  TWSTMetadataService_ServiceImp=class(TSimpleFactoryItem,IWSTMetadataService)
  Protected
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(
      Const AName : string
    ):TWSTMtdRepository;
  End;


  procedure RegisterWSTMetadataServiceImplementationFactory();

Implementation
uses metadata_repository;

{ TWSTMetadataService_ServiceImp implementation }
function TWSTMetadataService_ServiceImp.GetRepositoryList():TArrayOfStringRemotable;
var
  i, c : Integer;
  mn : IModuleMetadataMngr;
Begin
  Result := TArrayOfStringRemotable.Create();
  try
    mn := GetModuleMetadataMngr();
    c := mn.GetCount();
    Result.SetLength(c);
    for i := 0 to Pred(c) do
      Result[i] := mn.GetRepositoryName(i);
  except
    FreeAndNil(Result);
    raise;
  end;
End;

function TWSTMetadataService_ServiceImp.GetRepositoryInfo(Const AName : string):TWSTMtdRepository;

  procedure LoadService(ARawServ : PService; AObjServ : TWSTMtdService);

    procedure LoadOperation(ARawOper : PServiceOperation; AObjOper : TWSTMtdServiceOperation);

      procedure LoadParam(ARawParam : POperationParam; AObjPrm : TWSTMtdOperationParam);
      begin
        if Assigned(ARawParam) and Assigned(AObjPrm) then begin
          AObjPrm.Name :=ARawParam^.Name;
          AObjPrm.TypeName := ARawParam^.TypeName;
          AObjPrm.Modifier := ARawParam^.Modifier;
        end;
      end;

    var
      ii, cc : Integer;
    begin
      if Assigned(ARawOper) and Assigned(AObjOper) then begin
        AObjOper.Name :=ARawOper^.Name;
        cc := ARawOper^.ParamsCount;
        AObjOper.Params.SetLength(cc);
        for ii := 0 to Pred(cc) do
          LoadParam(@(ARawOper^.Params[ii]),AObjOper.Params[ii]);
      end;
    end;

  var
    k, d : Integer;
  begin
    if Assigned(ARawServ) and Assigned(AObjServ) then begin
      AObjServ.Name :=ARawServ^.Name;
      d := ARawServ^.OperationsCount;
      AObjServ.Operations.SetLength(d);
      for k := 0 to Pred(d) do
        LoadOperation(@(ARawServ^.Operations[k]),AObjServ.Operations[k]);
    end;
  end;

var
  repData : PServiceRepository;
  mn : IModuleMetadataMngr;
  i, c : Integer;
Begin
  Result := nil;
  mn := GetModuleMetadataMngr();
  mn.LoadRepositoryName(AName,'/',repData);
  if Assigned(repData) then begin
    try
      try
        Result := TWSTMtdRepository.Create();
        Result.Name := repData^.Name;
        Result.NameSpace := repData^.NameSpace;
        c := repData^.ServicesCount;
        Result.Services.SetLength(c);
        if ( c > 0 ) then begin
          for i := 0 to Pred(c) do begin
            LoadService(@(repData^.Services[i]),Result.Services[i]);
          end;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      mn.ClearRepository(repData);
    end;
  end;
End;


procedure RegisterWSTMetadataServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('IWSTMetadataService',TImplementationFactory.Create(TWSTMetadataService_ServiceImp) as IServiceImplementationFactory);
End;

End.
