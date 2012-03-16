unit nsEnumerators;

{$MACRO on}

{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ELSE Windows}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}

interface

uses
  nsXPCOM, nsTypes, Classes;

function NewSimpleEnumeratorFromTInterfaceList(AList: TInterfaceList;
                AOwn: Boolean = True): nsISimpleEnumerator;

implementation

uses
  nsXPCOM_std19, nsError;

type
  TSimpleEnumeratorWithInterfaceList = class(TInterfacedObject,
                                             nsISimpleEnumerator_std19)
    FList: TInterfaceList;
    FOwn: Boolean;
    FIndex: Integer;
    destructor Destroy; override;
    function HasMoreElements(out _retval: PRBool): nsresult; extdecl;
    function GetNext(out _retval: nsISupports_std19): nsresult; extdecl;
  end;

function NewSimpleEnumeratorFromTInterfaceList(AList: TInterfaceList;
                AOwn: Boolean = True): nsISimpleEnumerator;
var
  enum: TSimpleEnumeratorWithInterfaceList;
begin
  enum := TSimpleEnumeratorWithInterfaceList.Create;
  enum.FList := AList;
  enum.FOwn := AOwn;
  Result := enum as nsISimpleEnumerator;
end;

destructor TSimpleEnumeratorWithInterfaceList.Destroy;
begin
  if FOwn then
    FList.Free;

  inherited;
end;

function TSimpleEnumeratorWithInterfaceList.HasMoreElements(out _retval
                : PRBool): nsresult;
begin
  _retval :=  (FIndex < FList.Count);
  Result := NS_OK;
end;

function TSimpleEnumeratorWithInterfaceList.GetNext(out _retval
                : nsISupports_std19): nsresult;
begin
  if (FIndex < FList.Count) then
  begin
    Result := FList.Items[FIndex].QueryInterface(nsISupports_std19, _retval);
    Inc(FIndex);
  end else
    Result := NS_ERROR_FAILURE;
end;

end.
