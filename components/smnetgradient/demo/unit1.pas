unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
   SMNetGradient;

type

  { TMainForm }

  TMainForm = class(TForm)
    NetGradient1: TNetGradient;
    NetGradient2: TNetGradient;
    NetGradient3: TNetGradient;
    NetGradient4: TNetGradient;
    NetGradient5: TNetGradient;
    NetGradient6: TNetGradient;
    NetGradient8: TNetGradient;
    NetGradient9: TNetGradient;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

initialization
  {$I unit1.lrs}

end.

