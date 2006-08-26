program test_ebay;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  service_intf, soap_formatter, base_service_intf, base_soap_formatter,
  //ics_http_protocol,
  //indylaz,
  //indy_http_protocol,
  ebay, ebay_proxy,
  ssl_openssl, synapse_http_protocol;

var
  locService : IeBayAPIInterfaceService;
  locHdr : TCustomSecurityHeaderType;
  r : TGetCategoriesRequestType;
  rsp : TGetCategoriesResponseType;
begin
  //Indy_RegisterHTTP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  r := nil;
  rsp := nil;
  locHdr := TCustomSecurityHeaderType.Create();
  try
    locHdr.eBayAuthToken :=  {
    'AgAAAA**AQAAAA**aAAAAA**OeGvRA**nY+sHZ2PrBmdj6wVnY+sEZ2PrA2dj6wJnY+lAZOE'+
    'pgqdj6x9nY+seQ**uoUAAA**AAMAAA**z5djiOw1a7Tk12KGGPqSpvnxxNYOVUtaSbmQ7hYd4p'+
    'X4XfafLKBtImKsW9SUsbmBS9fXOyBnXA3k0jLelpiMptvlZ8N52UQA/ePc6+JE7LJFrARMoBaW5l'+
    'HEQOMESJLAdFJiGmLwrnagdeo6WRI89guRtDkydPyHwHUJ7aCFQvwzeD/b+1pnXelHQvQBRFtD3drU'+
    'BV9FbAf1/d4w/C+x5EHrBHyA+/T9uBelb3wkI8Rk/jnwF+L1qZlSW90pcyi3uxoSuBGVolgihrL/IKE'+
    '2mPcK3GAtqROu6Tsasjzz/tqkSIuFLeJ9HphAzdB+LNhyR1NGbe+l+goY74saRbEb2iqYo5wCTTLELC2k3'+
    '9p0V1Fp7CWn3Fet+y6fz8PXMb1BfYKg6fLzHXaqCRaffHJCSkvhrWwIVEuxbot4o5T8/v'+
    'TcmmAm3T78S4B6NBdLPv7f4WxbzYYRS8Y8k7Y9GZ1/8Jomfv+LlGNrs0/sN+PkCJATAJZ3W'+
    'tIWqyg9GHnHVA+oKCdmItd2j6nEiNq7whNdJegMOWp3jI2BvJoauJc06lw6ZMHhuj4zDiDnEwP'+
    'DCBmY6sHWMUx1xacahKYrRsvKYvE9/eOlEaQP7OCDmJm6VVwJIkSejOnmnMmUxLGMu6to17jruAj'+
    'Wb4s0oXSKPg9J/M2rvgE0l0tWj3O6kt9jPH533K5Wj2I/i6s0blc9z9eY/WY4+HDHe+VFX9AqMmHuD'+
    'yog//CUNDaG5HUSw10GM26gvswNpYWGih5Ju5ylvf9B';}
    
    'AgAAAA**AQAAAA**aAAAAA**5Ca0RA**nY+sHZ2PrBmdj6wVnY+sEZ2PrA2dj6wJnY+lAZSFpg'+
    'Wdj6x9nY+seQ**uoUAAA**AAMAAA**CVYGMzI5zQ2Wh9dcHROrT0o6/BWlHNSzb+sPVl+W7UK8o0'+
    'zpmispZNrnzXjlqd5m5nZjWfXzEGFTZVw7B+2k14tcQyiCQQn0nD6ft5KUWsxZ4Ugx/EgilEFNhT7l'+
    'iQXBxblWq1K3CJJtyCRu1Q/eyW0c4cttutktG3c5wFGR20QUm8pFBaXVNEB11jAyzz2dB+Ij3efuSTZR'+
    'umGNaVHeNXkLXTfaVuOzREjU5zye4bh1cHtw72pS+oTbmKB+Svflhtq7asqnfrsllRENP6fEpCzJSVqbMW'+
    'Om+rulRa0qKOOpEGk2Mme8HDdccwtqHIq1MwT9WbcF2pV6aGKpllU4H+ii7SYwDTr8mwb45t7l26loyszoZo'+
    'NelhXq3TS85KwmDqwgZzVlHoY+4yZVe8FRvOY7rYbtCJtZnwv7fx8+tdoogeE2eW5hNkXPvuS+Wh9yj+T1yexp'+
    '5szSfOVmn1Obik6Cz/qOxF+AIHpdO1N8qC6D/x85nlkxUbvVWBHkAVYsAxbQ1uZzpRIednc8wKLZ47cTUGPinP1B'+
    'hgC9+l14Isquhsx5gx9t3vc79lzfRPMOaQ5k42vZaUFYTpQ2tYn7kQ9y850NPBdNVmUxLi5hCActWCHFplNrYVnnnm'+
    'WOcuZT+DTUmh2OHiL59Av33CPhGNCGktEX0/I3FNTbM2OHCqet/eSRXNHM4JuuLhP2p7IyDfbowkXpwDZtanew64itUr'+
    'iSInDbHpO9xlVK32t/+na6yNuCGqFEEtnl5gJ2OI1P';

    //locHdr.Credentials.Username := 'inousa12';
    //locHdr.Credentials.Password := 'atou121076';
    locHdr.Credentials.AppId := 'INOUSSAOUEU258CIC9Z5E83UXC1BE5';
    locHdr.Credentials.DevId := 'L11ZDC63VDJ1FPLJL5EA161OQ2MS95';
    locHdr.Credentials.AuthCert := 'A266GKZC9F5$HI2HIH58A-D3JH2YA4';//'L11ZDC63VDJ1FPLJL5EA161OQ2MS95;INOUSSAOUEU258CIC9Z5E83UXC1BE5;A266GKZC9F5$HI2HIH58A-D3JH2YA4';
    locService := TeBayAPIInterfaceService_Proxy.Create(
                    'eBayAPIInterfaceService',
                    'SOAP:Style=Document;EncodingStyle=Litteral',
                    'http:Address=https://api.sandbox.ebay.com/wsapi?callname=GetCategories&siteid=0&appid=INOUSSAOUEU258CIC9Z5E83UXC1BE5&version=467'
                  );   //https://api.sandbox.ebay.com/wsapi
                       //https://api.sandbox.ebay.com/wsapi
                       //https://api.sandbox.ebay.com/ws/api.dll
    (locService as ICallContext).AddHeader(locHdr,True);
    r := TGetCategoriesRequestType.Create();
    r.Version := '467';
    try
      locService.GetCategories(r,rsp);
    except
      on e : Exception do begin
        WriteLn('Exception : ',e.Message);
        raise;
      end;
    end;
  finally
    r.Free();
    rsp.Free();
  end;
end.

