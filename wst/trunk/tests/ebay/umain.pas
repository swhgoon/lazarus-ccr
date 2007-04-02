unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, eBayWSDL;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button3: TButton;
    edteBayAuthToken: TEdit;
    edtAppId: TEdit;
    edtDevId: TEdit;
    edtAuthCert: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    trvOut: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses  TypInfo, StrUtils,
      httpsend,
      ssl_openssl,
      service_intf, soap_formatter, base_service_intf, base_soap_formatter,
      ebay, ebay_proxy,
      synapse_http_protocol;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  locService : IeBayAPIInterfaceService;
  locHdr : TCustomSecurityHeaderType;
  r : TGetCategoriesRequestType;
  rsp : TGetCategoriesResponseType;
begin
  try
    r := nil;
    rsp := nil;
    locHdr := TCustomSecurityHeaderType.Create();
    try
      locHdr.eBayAuthToken := edteBayAuthToken.Text;

      locHdr.Credentials.AppId := edtAppId.Text;
      locHdr.Credentials.DevId := edtDevId.Text;
      locHdr.Credentials.AuthCert := edtAuthCert.Text;
      locService := TeBayAPIInterfaceService_Proxy.Create(
                      'eBayAPIInterfaceService',
                      'SOAP:Style=Document;EncodingStyle=Litteral;UniqueAddress=false',
                      'http:Address=https://api.sandbox.ebay.com/wsapi'
                    );
      (locService as ICallContext).AddHeader(locHdr,True);
      r := TGetCategoriesRequestType.Create();
      r.Version := sEBAY_VERSION;
      locService.GetCategories(r,rsp);
      if Assigned(rsp) then
        ShowMessageFmt('CategoryCount=%d; Message=%s; Version = %s',[rsp.CategoryCount,rsp.Message,rsp.Version])
      else
        ShowMessage('rsp = nil');
    finally
      r.Free();
      rsp.Free();
    end;
  except
    on e : ESOAPException do begin
      ShowMessageFmt('SOAP EXCEPTION Code : "%s"; String = "%s"; Msg : '#13'%s',[e.FaultCode,e.FaultString,e.Message]);
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);

  procedure ShowResponse(ARsp : TGetPopularKeywordsResponseType);
  var
    nd, an, nn, pn : TTreeNode;
    k : Integer;
    ci : TCategoryType;
  begin
    trvOut.BeginUpdate();
    try
      trvOut.Items.Clear();
      if not Assigned(ARsp) then
        Exit;
      nd := trvOut.Items.AddChild(nil,'Response');
      trvOut.Items.AddChild(nd,'Ack = ' + GetEnumName(TypeInfo(TAckCodeType),Ord(ARsp.Ack)));
      trvOut.Items.AddChild(nd,'Version = ' + ARsp.Version);
      trvOut.Items.AddChild(nd,'HasMore = ' + IfThen(ARsp.HasMore,'True','False'));

      pn := trvOut.Items.AddChild(nd,'PaginationResult');
      trvOut.Items.AddChild(pn,'TotalNumberOfEntries = ' + IntToStr(ARsp.PaginationResult.TotalNumberOfEntries));
      trvOut.Items.AddChild(pn,'TotalNumberOfPages = ' + IntToStr(ARsp.PaginationResult.TotalNumberOfPages));

      an := trvOut.Items.AddChild(nd,'CategoryArray ( ' + IntToStr(ARsp.CategoryArray.Length) + ')');
      for k := 0 to Pred(ARsp.CategoryArray.Length) do begin
        ci := ARsp.CategoryArray[k];
        nn := trvOut.Items.AddChild(an,'Category ( ' + IntToStr(k) + ' )');
        trvOut.Items.AddChild(nn,'CategoryID = ' + ci.CategoryID);
        trvOut.Items.AddChild(nn,'CategoryParentID = ' + ci.CategoryParentID);
        trvOut.Items.AddChild(nn,'Keywords = ' + ci.Keywords);
      end;
    finally
      trvOut.EndUpdate();
    end;
  end;
  
var
  locService : IeBayAPIInterfaceService;
  locHdr : TCustomSecurityHeaderType;
  r : TGetPopularKeywordsRequestType;
  rsp : TGetPopularKeywordsResponseType;
  kpCrs : TCursor;
begin
  try
    r := nil;
    rsp := nil;
    kpCrs := Screen.Cursor;
    locHdr := TCustomSecurityHeaderType.Create();
    try
      Screen.Cursor := crHourGlass;
      locHdr.eBayAuthToken := edteBayAuthToken.Text;

      locHdr.Credentials.AppId := edtAppId.Text;
      locHdr.Credentials.DevId := edtDevId.Text;
      locHdr.Credentials.AuthCert := edtAuthCert.Text;
      locService := TeBayAPIInterfaceService_Proxy.Create(
                      'eBayAPIInterfaceService',
                      'SOAP:Style=Document;EncodingStyle=Litteral;UniqueAddress=false',
                      'http:Address=https://api.sandbox.ebay.com/wsapi'
                    );
      (locService as ICallContext).AddHeader(locHdr,True);
      r := TGetPopularKeywordsRequestType.Create();
      r.Version := sEBAY_VERSION;
      r.IncludeChildCategories := True;
      locService.GetPopularKeywords(r,rsp);
      if Assigned(rsp) then begin
        ShowResponse(rsp);
      end else begin
        ShowMessage('rsp = nil');
      end;
    finally
      Screen.Cursor := kpCrs;
      r.Free();
      rsp.Free();
    end;
  except
    on e : ESOAPException do begin
      ShowMessageFmt('SOAP EXCEPTION Code : "%s"; String = "%s"; Msg : '#13'%s',[e.FaultCode,e.FaultString,e.Message]);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SYNAPSE_RegisterHTTP_Transport();
end;

initialization
  {$I umain.lrs}

end.

