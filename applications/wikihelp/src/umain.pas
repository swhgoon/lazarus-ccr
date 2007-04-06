unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,httpsend, ComCtrls, Utils, xmlread,dom, EditBtn, FileUtil,
  XMLPropStorage,Clipbrd;

type

  { TfWikiHelp }

  TfWikiHelp = class(TForm)
    bSearch: TButton;
    bCreate: TButton;
    eOutputDir: TDirectoryEdit;
    ePageOffset: TEdit;
    eWikiPage: TEdit;
    eFoundPages: TLabel;
    lOutputDir: TLabel;
    lbFoundPages: TListBox;
    lPageOffset: TLabel;
    lWikiPage: TLabel;
    pbProgress: TProgressBar;
    Properties: TXMLPropStorage;
    cbLanguage: TComboBox;
    lLanguage: TLabel;
    cbAddLinkedPages: TCheckBox;
    procedure bCreateClick(Sender: TObject);
    procedure bSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure cbLanguageSelect(Sender: TObject);
  private
    { private declarations }
    SpecialPageURL : string;
    ExportPageURL : string;
    ImageTagName : string;
  public
    { public declarations }
    procedure Wiki2Html(wname : string;OutputDir : string;content : string);
  end; 

var
  fWikiHelp: TfWikiHelp;

implementation

uses uAppconsts;

{ TfWikiHelp }

procedure TfWikiHelp.bSearchClick(Sender: TObject);
var
  http : THttpSend;
  ss : TStringStream;
  s : string;
  tmp: String;
begin
  lbFoundPages.Items.Clear;
  ss := TStringStream.Create('');
  http := THttpSend.Create;
  http.UserAgent := 'Mozilla/4.0 (compatible; WikiHelp)';
  http.HTTPMethod('GET',eWikiPage.Text+SpecialPageURL);
  http.Document.SaveToStream(ss);
  http.Free;
  s := ss.DataString;
  s := copy(s,pos('<table style="background: inherit;" border="0" width="100%"><tr><td>',s),length(s));
  s := copy(s,0,pos('</td></tr></table><div class="printfooter">',s));
  if s = '' then
    begin
      Showmessage('Special Page not found !');
      exit;
    end;
  ss.Free;
  while pos('<a href="',s) > 0 do
    begin
      s := copy(s,pos('<a href="',s)+10,length(s));
      tmp := copy(s,0,pos('"',s)-1);
      while pos(copy(tmp,0,pos('/',tmp)-1),eWikiPage.Text) > 0 do
        tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
      if copy(tmp,0,length(ePageOffset.Text)) = ePageOffset.Text then
        begin
          lbFoundPages.Items.Add(tmp);
        end;
      s := copy(s,pos('"',s)+1,length(s));
    end;
end;

procedure TfWikiHelp.FormCreate(Sender: TObject);
var
  FindRec: TSearchRec;
begin
  if not DirectoryExists(GetConfigDir(vAppname)) then
    ForceDirectories(GetConfigDir(vAppname));
  Properties.FileName := GetConfigDir(vAppname)+'config.xml';
  Properties.Restore;
  if Properties.StoredValue['WIKIPAGE'] <> '' then
    eWikiPage.Text := Properties.StoredValue['WIKIPAGE'];
  ePageOffset.Text := Properties.StoredValue['PAGEOFFSET'];
  eOutputDir.Text := Properties.StoredValue['OUTPUTDIR'];
  cbLanguage.Items.Clear;
  IF FindFirst(ExtractFileDir(Application.Exename) + DirectorySeparator + '*.xml', faAnyFile, FindRec) = 0 THEN
    REPEAT
      IF (FindRec.Name <> '.') AND (FindRec.Name <> '..') THEN
        cbLanguage.Items.Add(copy(FindRec.Name,0,rpos('.',FindRec.Name)-1));
    UNTIL FindNext(FindRec) <> 0;
  FindClose(FindRec);
end;

procedure TfWikiHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Properties.StoredValue['WIKIPAGE'] := eWikiPage.Text;
  Properties.StoredValue['PAGEOFFSET'] := ePageOffset.Text;
  Properties.StoredValue['OUTPUTDIR'] := eOutputDir.Text;
end;

procedure TfWikiHelp.cbLanguageSelect(Sender: TObject);
var
  xml: TXMLDocument;
  iNode: TDOMNode;
begin
  if FileExists(ExtractFilePath(Application.Exename)+DirectorySeparator+cbLanguage.Text+'.xml') then
    begin
      xml := TXMLDocument.Create;
      ReadXMLFile(xml,ExtractFilePath(Application.Exename)+DirectorySeparator+cbLanguage.Text+'.xml');
      iNode := xml.DocumentElement.FindNode('SpecialPageURL');
        SpecialPageURL := iNode.FirstChild.NodeValue;
      iNode := xml.DocumentElement.FindNode('ExportPageURL');
      if Assigned(iNode) then
        ExportPageURL := StringReplace(iNode.FirstChild.NodeValue,#10,'',[rfReplaceAll]);
      iNode := xml.DocumentElement.FindNode('ImageTagName');
      if Assigned(iNode) then
        ImageTagName := iNode.FirstChild.NodeValue;
      xml.Free;
    end;
end;

procedure DoReplace(var InStr,OutStr : string;ReplaceTag,NewTag : string;MustbeInOneLine : Boolean = False);
var
  NewLine: String;
begin
  while pos(ReplaceTag,instr) > 0 do
    begin
      NewLine := copy(instr,pos(ReplaceTag,instr)+length(ReplaceTag),length(instr));
      if MustBeInOneLine
      and ((pos(#10,NewLine) < pos(ReplaceTag,NewLine))) and (not (length(NewLine) = pos(ReplaceTag,NewLine)+length(ReplaceTag)-1)) then
        break;
      outstr := outstr+copy(instr,0,pos(ReplaceTag,instr)-1);
      instr := copy(instr,pos(replaceTag,instr)+length(ReplaceTag),length(instr));
      outstr := outstr+'<'+NewTag+'>'+copy(instr,0,pos(ReplaceTag,instr)-1)+'</'+NewTag+'>';
      instr := copy(instr,pos(ReplaceTag,instr)+length(ReplaceTag),length(instr));
    end;
  outstr := outstr+instr;
  instr := outstr;
  outstr := '';
end;

procedure TfWikiHelp.Wiki2Html(wname: string;OutputDir : string;content: string);
var
  f : TextFile;
  istr : string;
  ostr : string;
  http : THttpSend;
  open_uls,act_uls : integer;
  i: LongInt;
  intd: Boolean;
  tstr: String;
  aWikiStart: String;
  linkcontent: String;
begin
  istr := content;
  ForceDirectories(OutputDir);
  AssignFile(f,AppendPathDelim(OutputDir)+ValidateFileName(lowercase(wname))+'.html');
  Rewrite(f);
  ostr := '';
  open_uls := 0;
  act_uls := 0;
  //Remove NOTOC
  istr := StringReplace(istr,'__NOTOC__','',[rfReplaceAll]);
  //Remove TOC
  istr := StringReplace(istr,'__TOC__','',[rfReplaceAll]);
  //Remove Templates
  while pos('{{',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('{{',istr)-1);
      istr := copy(istr,pos('{{',istr)+2,length(istr));

      istr := copy(istr,pos('}}',istr)+2,length(istr));
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Lists
  while pos('*',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('*',istr)-1);
      istr := copy(istr,pos('*',istr)+1,length(istr));
      inc(act_uls);
      while istr[1] = '*' do
        begin
          inc(act_uls);
          istr := copy(istr,2,length(istr));
        end;
      if open_uls < act_uls then
        begin
          for i := open_uls to act_uls-1 do
            ostr := ostr+'<ul>';
        end
      else
        begin
          for i := act_uls to open_uls-1 do
            ostr := ostr+'</ul>';
        end;
      open_uls := act_uls;
      act_uls := 0;
      ostr := ostr+'<li>';
      if pos(#10,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr)-1);
          istr := copy(istr,pos(#10,istr)+1,length(istr));
        end
      else
        begin
          ostr := ostr+istr;
          istr := '';
        end;
      ostr := ostr+'</li>';
      if (length(istr) > 0) and (istr[1] <> '*') then
        begin
          for i := 0 to open_uls-1 do
            ostr := ostr+'</ul>';
          open_uls := 0;
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  open_uls := 0;
  act_uls := 0;
  //Replace Numerated Lists
  while pos('#',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('#',istr)-1);
      istr := copy(istr,pos('#',istr)+1,length(istr));
      inc(act_uls);
      while istr[1] = '#' do
        begin
          inc(act_uls);
          istr := copy(istr,2,length(istr));
        end;
      if open_uls < act_uls then
        begin
          for i := open_uls to act_uls-1 do
            ostr := ostr+'<ol>';
        end
      else
        begin
          for i := act_uls to open_uls-1 do
            ostr := ostr+'</ol>';
        end;
      open_uls := act_uls;
      act_uls := 0;
      ostr := ostr+'<li>';
      if pos(#10,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr)-1);
          istr := copy(istr,pos(#10,istr)+1,length(istr));
        end
      else
        begin
          ostr := ostr+istr;
          istr := '';
        end;
      ostr := ostr+'</li>';
      if (length(istr) > 0) and (istr[1] <> '#') then
        begin
          for i := 0 to open_uls-1 do
            ostr := ostr+'</ol>';
          open_uls := 0;
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Tables
  while pos('{|',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('{|',istr)-1);
      istr := copy(istr,pos('{|',istr)+2,length(istr));
      //remove also content behing {|
      istr := copy(istr,pos(#10,istr)-1,length(istr));
      tstr := copy(istr,0,pos(#10+'|}',istr)-1);
      istr := copy(istr,pos(#10+'|}',istr)+3,length(istr));
      ostr := ostr+'<table><tr>';
      tstr := StringReplace(tstr,'|-','</tr><tr>',[rfReplaceAll]);
      intd := False;
      while length(tstr) > 2 do
        begin
          if ((tstr[1] = #10) and (tstr[2] = '|'))
          or ((tstr[1] = #10) and (tstr[2] = '!')) then
            begin
              if inTD then
                ostr := ostr+'</td>'
              else
                ostr := ostr+'<td>';
              inTD := not inTD;
              tstr := copy(tstr,3,length(tstr));
            end
          else if ((tstr[1] = '!') and (tstr[2] = '!'))
               or ((tstr[1] = '|') and (tstr[2] = '|')) then
            begin
              if inTD then
                begin
                  ostr := ostr+'</td><td>'
                end
              else //Schould never happen
                begin
                  ostr := ostr+'<td>';
                  inTD := True;
                end;
              tstr := copy(tstr,3,length(tstr));
            end
          else
            begin
              if (tstr[1] = #10) and InTD then
                begin
                  ostr := ostr+'</td>';
                  InTD := False;
                end
              else
                ostr := ostr+tstr[1];
              tstr := copy(tstr,2,length(tstr));
            end;
        end;
      ostr := ostr+tstr+'</tr></table>';
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Images
  while pos('[['+ImageTagName+':',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('[['+ImageTagName+':',istr)-1);
      istr := copy(istr,pos('[['+ImageTagName+':',istr)+length(ImageTagname)+3,length(istr));
      if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
        begin
          http := THttpSend.Create;
          http.HTTPMethod('GET','/images/'+copy(istr,0,pos('|',istr)-1));
          http.Document.SaveToFile(AppendPathDelim(OutputDir)+ValidateFileName(lowercase(copy(istr,0,pos('|',istr)-1))));
          http.Free;
          ostr := ostr+'<img src="'+ValidateFileName(lowercase(copy(istr,0,pos('|',istr)-1)))+'" alt="';
          istr := copy(istr,0,pos('|',istr)+1);
          ostr := ostr+copy(istr,0,pos(']]',istr)-1)+'"></img>';
          istr := copy(istr,pos(']]',istr)+2,length(istr));
        end
      else
        begin
          aWikiStart := eWikiPage.Text;
          if pos('index.php',aWikiStart) > 0 then
            aWikiStart := copy(aWikiStart,0,pos('index.php',aWikiStart)-1);
          http := THttpSend.Create;
          http.HTTPMethod('GET',aWikiStart+'/images/'+copy(istr,0,pos(']]',istr)-1));
          http.Document.SaveToFile(AppendPathDelim(OutputDir)+ValidateFileName(lowercase(copy(istr,0,pos(']]',istr)-1))));
          http.Free;
          ostr := ostr+'<img src="'+ValidateFileName(lowercase(copy(istr,0,pos(']]',istr)-1)))+'" alt="'+copy(istr,0,pos(']]',istr)-1)+'"></img>';
          istr := copy(istr,pos(']]',istr)+2,length(istr));
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Links
  while pos('[[',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('[[',istr)-1);
      istr := copy(istr,pos('[[',istr)+2,length(istr));
      if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
        begin
          linkcontent := copy(istr,0,pos('|',istr)-1);
          if (cbAddLinkedPages.Checked and (lbFoundPages.Items.IndexOf(linkcontent) = -1)) then
            begin
              lbFoundPages.Items.Add(linkcontent);
              pbProgress.Max := pbProgress.Max+1;
            end;
          if (lbFoundPages.Items.IndexOf(linkcontent) > -1) or (FileExists(AppendPathDelim(OutputDir)+ValidateFileName(lowercase(linkcontent)+'.html'))) then
            begin
              ostr := ostr+'<a href="'+ValidateFileName(linkcontent)+'.html">';
              istr := copy(istr,pos('|',istr)+1,length(istr));
              ostr := ostr+copy(istr,0,pos(']]',istr)-1)+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end
          else
            begin
              istr := copy(istr,pos('|',istr)+1,length(istr));
              ostr := ostr+copy(istr,0,pos(']]',istr)-1);
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end;
        end
      else
        begin
          linkcontent := copy(istr,0,pos(']]',istr)-1);
          if (cbAddLinkedPages.Checked and (lbFoundPages.Items.IndexOf(linkcontent) = -1)) then
            begin
              lbFoundPages.Items.Add(linkcontent);
              pbProgress.Max := pbProgress.Max+1;
            end;
          if lbFoundPages.Items.IndexOf(linkcontent) > -1 then
            begin
              ostr := ostr+'<a href="'+ValidateFileName(linkcontent)+'.html">'+copy(istr,0,pos(']]',istr)-1)+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end
          else
            begin
              ostr := ostr+copy(istr,0,pos(']]',istr)-1);
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end;
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace extern Links
  while pos('[http://',lowercase(istr)) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('[http://',lowercase(istr))-1);
      istr := copy(istr,pos('[http://',lowercase(istr)),length(istr));
      if (pos(' ',istr) > 0) and (pos(' ',istr) < pos(']',lowercase(istr))) then
        begin
          ostr := ostr+'<a href="'+StringReplace(copy(istr,2,pos(' ',istr)-2),'http://./','',[rfReplaceAll])+'" target="_BLANK">';
          istr := copy(istr,pos(' ',istr)+1,length(istr));
          ostr := ostr+copy(istr,0,pos(']',lowercase(istr))-1)+'</a>';
          istr := copy(istr,pos(']',lowercase(istr))+1,length(istr));
        end
      else
        begin
          ostr := ostr+'<a href="'+StringReplace(copy(istr,2,pos(']',lowercase(istr))-2),'http://./','',[rfReplaceAll])+'" target="_BLANK">'+StringReplace(copy(istr,2,pos(']',lowercase(istr))-2),'http://./','',[rfReplaceAll])+'</a>';
          istr := copy(istr,pos(']',lowercase(istr))+1,length(istr));
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Bold Text
  while pos('''''''',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('''''''',istr)-1);
      istr := copy(istr,pos('''''''',istr)+3,length(istr));
      ostr := ostr+'<b>'+copy(istr,0,pos('''''''',istr)-1)+'</b>';
      istr := copy(istr,pos('''''''',istr)+3,length(istr));
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Italic Text
  while pos('''''',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('''''',istr)-1);
      istr := copy(istr,pos('''''',istr)+2,length(istr));
      ostr := ostr+'<i>'+copy(istr,0,pos('''''',istr)-1)+'</i>';
      istr := copy(istr,pos('''''',istr)+2,length(istr));
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Header Level 5
  DoReplace(istr,ostr,'=====','h6',True);
  //Replace Header Level 4
  DoReplace(istr,ostr,'====','h5',True);
  //Replace Header Level 3
  DoReplace(istr,ostr,'===','h4',True);
  //Replace Header Level 2
  DoReplace(istr,ostr,'==','h3',True);
  //Replace Header Level 1
//  DoReplace(istr,ostr,'=','h2',True); //Too many problems at time TODO: check if we are in an html tag bevore replace
  //Process unformated stuff
  while pos(#10+' ',istr) > 0 do
    begin
      //Replace Line breaks in text bevore pre
      ostr := ostr+StringReplace(StringReplace(copy(istr,0,pos(#10+' ',istr)-1),#10#10,'<br><br>',[rfReplaceAll]),#10,'',[rfReplaceAll]);
      istr := copy(istr,pos(#10+' ',istr)+2,length(istr));
      ostr := ostr+'<pre>';
      while (pos(#10+' ',istr) > 0) do
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr));
          istr := copy(istr,pos(#10,istr)+1,length(istr));
          if (length(istr) > 0) and (istr[1] <> ' ') then
            break
          else
            istr := copy(istr,2,length(istr));
        end;
      ostr := ostr+'</pre>';
    end;
  ostr := ostr+StringReplace(StringReplace(istr,#10#10,'<br><br>',[rfReplaceAll]),#10,'',[rfReplaceAll]);
  //Remove <br> after <h*>
  ostr := StringReplace(ostr,'</h2><br><br>','</h2>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h3><br><br>','</h3>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h4><br><br>','</h4>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h5><br><br>','</h5>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h6><br><br>','</h6>',[rfReplaceAll]);

  ostr := StringReplace(ostr,'</h2><br>','</h2>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h3><br>','</h3>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h4><br>','</h4>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h5><br>','</h5>',[rfReplaceAll]);
  ostr := StringReplace(ostr,'</h6><br>','</h6>',[rfReplaceAll]);

  write(f,'<html><head><title>'+wname+'</title></head><body><font face="arial,verdana">'+ostr+'</font></body></html>');
  CloseFile(f);
end;

procedure TfWikiHelp.bCreateClick(Sender: TObject);
var
  http : THttpSend;
  xml : TXMLDocument;
  i: Integer;
  iNode: TDOMNode;
  a: Integer;
  aWikiStart : string;
begin
  bCreate.Enabled := false;
  Screen.Cursor := crHourglass;
  pbProgress.Max := lbFoundPages.Items.Count;
  pbProgress.Position := 0;
  while lbFoundPages.Items.Count > 0 do
    begin
      aWikiStart := eWikiPage.Text;
      if pos('index.php',aWikiStart) > 0 then
        aWikiStart := copy(aWikiStart,0,pos('index.php',aWikiStart)-1);
      http := THttpSend.Create;
      http.HTTPMethod('POST',aWikiStart+Format(ExportPageURL,[HTTPEncode(lbFoundPages.Items[0])]));
      if http.ResultCode = 200 then
        begin
          xml := TXMLDocument.Create;
          try
            ReadXMLFile(xml,http.Document);
            iNode := xml.DocumentElement.FindNode('page');
            if Assigned(iNode) then
              iNode := iNode.FindNode('revision');
            if Assigned(iNode) then
              iNode := iNode.FindNode('text');
            if Assigned(iNode) then
              begin
                Wiki2Html(lbFoundPages.Items[0],eOutputDir.Text,iNode.FirstChild.NodeValue);
              end
            else
              Showmessage('Page: '+lbFoundPages.Items[0]+' not found !');
            xml.Free;
          except
            on e : Exception do
              Showmessage('Error Processing :'+lbFoundPages.Items[0]+':'+e.Message);
          end;
        end
      else
        Showmessage('Page: '+lbFoundPages.Items[0]+' not found !');
      http.Free;
      pbProgress.Position := i+1;
      lbFoundPages.Items.Delete(0);
      Application.Processmessages;
    end;
  Screen.Cursor := crDefault;
  bCreate.Enabled := True;
end;

initialization
  {$I umain.lrs}

end.

