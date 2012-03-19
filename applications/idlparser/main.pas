unit main;

{$mode objfpc}{$H+}

{ Main form for application to convert IDL to Pascal sourcefiles

  Copyright (C) 20120 Joost van der Sluis/CNOC joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, ValEdit, contnrs, idlparser, idlGenPascal;

type

  { TMainForm }

  TMainForm = class(TForm)
    bOpen: TButton;
    bConvert: TButton;
    bSave: TButton;
    bSettings: TButton;
    cbParamPrefix: TCheckBox;
    Label1: TLabel;
    memoPascalfile: TMemo;
    memoIDLFile: TMemo;
    OpenDialog: TOpenDialog;
    pBottom: TPanel;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    procedure bOpenClick(Sender: TObject);
    procedure bConvertClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bSettingsClick(Sender: TObject);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  PascaltypeSettings;

{ TMainForm }

procedure TMainForm.bOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
    memoIDLFile.Lines.LoadFromFile(OpenDialog.FileName);
    label1.Caption:=OpenDialog.FileName;
    end;
end;

procedure TMainForm.bConvertClick(Sender: TObject);
var
  IDLList: TIDLList;

begin
  memoPascalfile.lines.clear;

  IDLList := TIDLList.create;
  try
    IDLList.OwnsObjects:=true;
    ParseFile(IDLList, memoIDLFile.Lines);
    GeneratePascalSource(IDLList,memoPascalfile.Lines,TypeSettings.ValueListEditor1.Strings, cbParamPrefix.Checked);
  finally
    IDLList.Free;
  end;
end;

procedure TMainForm.bSaveClick(Sender: TObject);
begin
  SaveDialog.FileName:=ChangeFileExt(LowerCase(ExtractFileName(Label1.Caption)),'.inc');
  if SaveDialog.Execute then
    memoPascalfile.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.bSettingsClick(Sender: TObject);
begin
  TypeSettings.Show;
end;


end.

