{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf".
  Date            : "05/05/2007 19:07".
}
unit user_service_intf;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:UserService';
  sUNIT_NAME = 'user_service_intf';

type

  TUser = class;
  TUserArray = class;

  TUserCategory = ( 
    Normal
    ,Admin
  );

  TUser = class(TBaseComplexRemotable)
  private
    FCategory : TUserCategory;
    FUserName : string;
    FeMail : string;
    FPreferences : string;
  published
    property Category : TUserCategory read FCategory write FCategory;
    property UserName : string read FUserName write FUserName;
    property eMail : string read FeMail write FeMail;
    property Preferences : string read FPreferences write FPreferences;
  end;

  TUserArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TUser;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TUser Read GetItem;Default;
  end;

  UserService = interface(IInvokable)
    ['{842D8408-E142-470F-9CDD-FAD0D8AEEB12}']
    function GetList():TUserArray;
    procedure Add(
      Const AUser : TUser
    );
    procedure Update(
      Const AUser : TUser
    );
    function Find(
      Const AName : string
    ):TUser;
    function Delete(
      Const AName : string
    ):boolean;
  end;

  procedure Register_user_service_intf_ServiceMetadata();

Implementation
uses metadata_repository;

{ TUserArray }

function TUserArray.GetItem(AIndex: Integer): TUser;
begin
  Result := Inherited GetItem(AIndex) As TUser;
end;

class function TUserArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TUser;
end;


procedure Register_user_service_intf_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'UserService',
    'TRANSPORT_Address',
    'http://127.0.0.1:8000/services/UserService'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'UserService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'GetList',
    'TRANSPORT_soapAction',
    'urn:UserService/UserServiceGetList'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'GetList',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'GetList',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Add',
    'TRANSPORT_soapAction',
    'urn:UserService/UserServiceAdd'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Add',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Add',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Update',
    'TRANSPORT_soapAction',
    'urn:UserService/UserServiceUpdate'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Update',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Update',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Find',
    'TRANSPORT_soapAction',
    'urn:UserService/UserServiceFind'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Find',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Find',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Delete',
    'TRANSPORT_soapAction',
    'urn:UserService/UserServiceDelete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Delete',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'UserService',
    'Delete',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUserCategory),'TUserCategory');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUser),'TUser');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUserArray),'TUserArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TUserArray)].RegisterExternalPropertyName(sARRAY_ITEM,'item');


End.
