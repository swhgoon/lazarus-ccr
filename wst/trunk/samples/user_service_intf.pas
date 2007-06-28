{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf".
  Date            : "26/06/2007 23:46:28".
}
unit user_service_intf;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:UserService';
  sUNIT_NAME = 'user_service_intf';

type

  TUserArray = class;
  TUser_Type = class;

  TUserCategory_Type = ( 
    Normal
    ,Admin
  );

  TUser_Type = class(TBaseComplexRemotable)
  private
    FCategory : TUserCategory_Type;
    FUserName : string;
    FeMail : string;
    FPreferences : string;
  published
    property Category : TUserCategory_Type read FCategory write FCategory;
    property UserName : string read FUserName write FUserName;
    property eMail : string read FeMail write FeMail;
    property Preferences : string read FPreferences write FPreferences;
  end;

  TUserArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TUser_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TUser_Type Read GetItem;Default;
  end;

  UserService = interface(IInvokable)
    ['{757DE451-CE83-454C-8757-6D72428EB1AA}']
    function GetList():TUserArray;
    procedure Add(
      const  AUser : TUser_Type
    );
    procedure Update(
      const  AUser : TUser_Type
    );
    function Find(
      const  AName : string
    ):TUser_Type;
    function Delete(
      const  AName : string
    ):boolean;
  end;

  procedure Register_user_service_intf_ServiceMetadata();

Implementation
uses metadata_repository;

{ TUserArray }

function TUserArray.GetItem(AIndex: Integer): TUser_Type;
begin
  Result := Inherited GetItem(AIndex) As TUser_Type;
end;

class function TUserArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TUser_Type;
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUserCategory_Type),'TUserCategory');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUser_Type),'TUser');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TUserArray),'TUserArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TUserArray)].RegisterExternalPropertyName(sARRAY_ITEM,'item');

  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TUser_Type)].RegisterExternalPropertyName('Category','item');

End.
