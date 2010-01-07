unit ViewCode;

interface

   uses
      delphicompat, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, StdCtrls, LResources, SynHighlighterPas, SynEdit;

   type

      { TfrmViewCode }

      TfrmViewCode =
      class(TForm)
        SynEdit1: TSynEdit;
        SynPasSyn1: TSynPasSyn;
         procedure FormActivate(Sender: TObject);
         
         private
         { Private declarations }
         public
         { Public declarations }
      end;

implementation

   procedure TfrmViewCode.FormActivate(Sender: TObject);
   var
      r  : TRect;
   begin
      {get size of desktop}
      {$ifdef Windows}
      SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
      Height := r.Bottom-Top;
      Width  := r.Right-Left;
      {$endif}
   end;

initialization
  {$I ViewCode.lrs}

end.
