unit uform2;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, Forms;

type

  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end; 

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses wstimportdlg;//, HeapTrc;

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var
  f : TformImport;
begin
  f := TformImport.Create(nil);
  try
    f.ShowModal();
  finally
    f.Release();
  end;
end;

//initialization
  //SetHeapTraceOutput('heapTrace.txt');

end.

