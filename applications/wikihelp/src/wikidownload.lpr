program wikidownload;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }, synapse, httpsend,xmlread,dom;

type

  { TWikiDownload }

  TWikiDownload = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    pages : TStringList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure ExportPage(pagename : string);
  end;

{ TWikiDownload }

procedure TWikiDownload.DoRun;
var
  ErrorMsg: String;
  ss: TStringStream;
  http: THTTPSend;
  s: String;
  tmp: String;
  i: Integer;
begin
  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;

  { add your program here }

  ss := TStringStream.Create('');
  http := THttpSend.Create;
  http.UserAgent := 'Mozilla/4.0 (compatible; WikiHelp)';
  http.HTTPMethod('GET',GetOptionValue('a','allpages'));
  http.Document.SaveToStream(ss);
  http.Free;
  s := ss.DataString;
  s := copy(s,pos('<table style="background: inherit;" border="0" width="100%"><tr><td>',s),length(s));
  s := copy(s,0,pos('</td></tr></table><div class="printfooter">',s));
  if s = '' then
    begin
      writeln('Special Page not found !');
      exit;
    end;
  ss.Free;
  pages := TStringList.Create;
  while pos('<a href="',s) > 0 do
    begin
      s := copy(s,pos('<a href="',s)+10,length(s));
      tmp := copy(s,0,pos('"',s)-1);
      if pos('.php/',tmp) > 0 then
        tmp := copy(tmp,pos('.php/',tmp)+5,length(tmp));
      if copy(tmp,0,length(GetOptionValue('o','pageoffset'))) = GetOptionValue('o','pageoffset') then
        pages.Add(tmp);
      s := copy(s,pos('"',s)+1,length(s));
    end;
  if HasOption('r','recurse') then
    begin
    end
  else
    for i := 0 to pages.Count-1 do
      ExportPage(pages[i]);
  pages.free;
  // stop program loop
  Terminate;
end;

constructor TWikiDownload.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TWikiDownload.Destroy;
begin
  inherited Destroy;
end;

procedure TWikiDownload.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

function ValidateFileName(old : string) : string;
begin
  Result := StringReplace(old,'\','',[rfReplaceAll]);
  Result := StringReplace(Result,'/','',[rfReplaceAll]);
  Result := StringReplace(Result,'@','',[rfReplaceAll]);
  Result := StringReplace(Result,';','',[rfReplaceAll]);
  Result := StringReplace(Result,'#','_',[rfReplaceAll]);
  Result := StringReplace(Result,'>','_',[rfReplaceAll]);
  Result := StringReplace(Result,'<','_',[rfReplaceAll]);
  Result := StringReplace(Result,'|','_',[rfReplaceAll]);
  Result := StringReplace(Result,'"','_',[rfReplaceAll]);
  Result := StringReplace(Result,':','_',[rfReplaceAll]);
  Result := StringReplace(Result,'*','_',[rfReplaceAll]);
  Result := StringReplace(Result,'?','_',[rfReplaceAll]);
end;

procedure TWikiDownload.ExportPage(pagename: string);
var
  http: THTTPSend;
  xml: TXMLDocument;
  iNode: TDOMNode;
  f : TextFile;
  tmp: WideString;
  istr: WideString;

  procedure ReplaceImages(ImageTagName : string);
  begin
      while pos('[['+ImageTagName+':',istr) > 0 do
        begin
          istr := copy(istr,pos('[['+ImageTagName+':',istr)+length(ImageTagname)+3,length(istr));
          if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
            begin
              http := THttpSend.Create;
              http.HTTPMethod('GET',GetOptionValue('i','imagedir')+copy(istr,0,pos('|',istr)-1));
              http.Document.SaveToFile(GetOptionValue('o','output')+DirectorySeparator+ValidateFileName(lowercase(copy(istr,0,pos('|',istr)-1))));
              http.Free;
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end
          else
            begin
{              aWikiStart := eWikiPage.Text;
              if pos('index.php',aWikiStart) > 0 then
                aWikiStart := copy(aWikiStart,0,pos('index.php',aWikiStart)-1);}
              http := THttpSend.Create;
              http.HTTPMethod('GET',GetOptionValue('i','imagedir')+copy(istr,0,pos(']]',istr)-1));
              http.Document.SaveToFile(StringReplace(trim(GetOptionValue('o','output')),#13,'',[rfReplaceAll])+DirectorySeparator+ValidateFileName(lowercase(copy(istr,0,pos(']]',istr)-1))));
              http.Free;
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end;
        end;
  end;

  procedure AddLinksFromPage;
  var
    linkcontent: String;
  begin
    while pos('[[',istr) > 0 do
      begin
        istr := copy(istr,pos('[[',istr)+2,length(istr));
        if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
          begin
            linkcontent := copy(istr,0,pos('|',istr)-1);
            if pages.IndexOf(linkcontent) = -1 then
              pages.Add(linkcontent);
          end
        else
          begin
            linkcontent := copy(istr,0,pos(']]',istr)-1);
            if pages.IndexOf(linkcontent) = -1 then
              pages.Add(linkcontent);
          end;
      end;
  end;
begin
  http := THttpSend.Create;
  http.UserAgent := 'Mozilla/4.0 (compatible; WikiHelp)';
  http.HTTPMethod('POST',GetOptionValue('e','exportpage')+'?action=submit&pages='+pagename+'&curonly=true');
  if http.ResultCode = 200 then
    begin
      xml := TXMLDocument.Create;
      try
        ReadXMLFile(xml,http.Document);
        http.Free;
        iNode := xml.DocumentElement.FindNode('page');
        if Assigned(iNode) then
          iNode := iNode.FindNode('revision');
        if Assigned(iNode) then
          iNode := iNode.FindNode('text');
        if Assigned(iNode) then
          begin
            AssignFile(f,StringReplace(trim(GetOptionValue('o','output')),#13,'',[rfReplaceAll])+DirectorySeparator+ValidateFilename(pagename)+'.txt');
            rewrite(f);
            if Assigned(iNode.FirstChild) then
              tmp := iNode.FirstChild.NodeValue
            else
              tmp := iNode.NodeValue;
            writeln(f,tmp);
            Closefile(f);
            istr := tmp;
            ReplaceImages('Bild');
            istr := tmp;
            ReplaceImages('Image');
            istr := tmp;
            if hasoption('r','recursive') then
              AddLinksFromPage;
          end
        else
          writeln('Page Node for: '+pagename+' not found !');
        xml.Free;
      except
        on e : Exception do
          writeln('Error Processing :'+pagename+':'+e.Message+' ('+StringReplace(trim(GetOptionValue('o','output')),#13,'',[rfReplaceAll])+DirectorySeparator+ValidateFilename(pagename)+'.txt)');
      end;
    end
  else
    writeln('Page: '+pagename+' not found !');
end;

var
  Application: TWikiDownload;
begin
  Application:=TWikiDownload.Create(nil);
  Application.Title:='Wiki Download';
  Application.Run;
  Application.Free;
end.

