//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 11-11-2004.                                 ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//
//  File        : MGSignals.pas   REV. 1.1   (22-06-2005)
//
//  Description : Implementazione di un dispatcher di messaggi (svincolato dalla VCL)
//                verso delle classi registrate.
//                Implements a message dispatcher (disengaged from VCL)
//                to registered classes.
//
//******************************************************************************
//   WARNING -TO TEST IN ExtFind  (compare of method is different under Lazarus?)


unit MGSignals;
{$mode delphi}{$H+}

interface
uses MGTree16, MGList,
     {$ifdef WINDOWS}
      Windows,
     {$endif}
     Messages, Forms, Classes;

Type
    TSignalMethod = function (var aMessage: TMessage):Boolean of object;
    //necessario xchè TSignalMethod è una coppia di puntatori...
    //necessary because TSignalMethod is a pair of pointers
    PSignalMethod =^TSignalMethod;

    { TMGSignalsManager }

    TMGSignalsManager = class
    protected
       rClients : TMGTree16;

       procedure FreeClassOnList(Tag :Integer; wMessageID :Integer; wMessageList :TObject);
       procedure FreeLists(Tag :Integer; wMessageID :Integer; wMessageList :TObject);
    public
       constructor Create;
       destructor Destroy; override;
       procedure Connect(ClassMethod :TSignalMethod; MessageID :Integer);
       procedure Disconnect(ClassMethod :TSignalMethod; MessageID :Integer); overload;
       procedure Disconnect(ClassPointer :TObject); overload;
       function Signal(MessageID :Cardinal; WParam, LParam :Integer; var Handled :Boolean) :Integer; overload;
       function Signal(var aMessage: TMessage) :Boolean; overload;
    end;

    { TMessagesList }

    PMessage =^TMessage;

    TMessagesList = class (TMGList)
    protected
        function allocData :Pointer; override;
        procedure deallocData(pData :Pointer); override;
    public
        function Add(aMessage :TMessage) :PMessage; overload;
    end;

    { TSignalsAsyncThread }

    TSignalsAsyncThread = class(TThread)
    private
       curMsg :PMessage;
    protected
       msgSignals : TMGSignalsManager;
       msgList    : TMessagesList;

       procedure _Signal;
       procedure Execute; override;
    public
       constructor Create;
       destructor Destroy; override;
       procedure Connect(ClassMethod :TSignalMethod; MessageID :Integer; Priority :Integer=0);  //Priority for future use
       procedure Disconnect(ClassMethod :TSignalMethod; MessageID :Integer); overload;
       procedure Disconnect(ClassPointer :TObject); overload;
       procedure Signal(MessageID :Cardinal; WParam, LParam :Integer); overload;
       procedure Signal(aMessage: TMessage); overload;
    end;

    { TMGSignals }

    TMGSignals = class
    protected
       signals_async :TSignalsAsyncThread;
       signals_sync  :TMGSignalsManager;

    public
       constructor Create;
       destructor Destroy; override;
       procedure Connect(ClassMethod :TSignalMethod; MessageID :Integer);
       procedure Disconnect(ClassMethod :TSignalMethod; MessageID :Integer); overload;
       procedure Disconnect(ClassPointer :TObject); overload;
       procedure ConnectAsync(ClassMethod :TSignalMethod; MessageID :Integer; Priority :Integer=0);  //Priority for future use
       procedure DisconnectAsync(ClassMethod :TSignalMethod; MessageID :Integer); overload;
       procedure DisconnectAsync(ClassPointer :TObject); overload;
       function Signal(MessageID :Cardinal; WParam, LParam :Integer; var Handled :Boolean) :Integer; overload;
       function Signal(var aMessage: TMessage) :Boolean; overload;
    end;

implementation

Type
    TSignalMethodsList = class (TMGList)
    protected
        function allocData :Pointer; override;
        procedure deallocData(pData :Pointer); override;
        function CompBySignalMethod(xTag :Pointer; ptData1, ptData2 :Pointer) :Boolean;
    public
        function Add(AMethod :TSignalMethod) :PSignalMethod; overload;
        function Find(AMethod :TSignalMethod) : Integer; overload;
        function ExtFind(AMethod :TSignalMethod) : Pointer; overload;
        function Delete(AMethod :TSignalMethod) :Boolean; overload;
        function DeleteByClassMethod(AMethod :TSignalMethod) :Boolean;
        procedure DeleteByClass(ClassPointer :TObject);
        function CallAllMethods(var aMessage: TMessage) :Boolean;
    end;



{ TMessagesList }

function TMessagesList.allocData: Pointer;
begin
     GetMem(Result, sizeOf(TSignalMethod));
end;

procedure TMessagesList.deallocData(pData: Pointer);
begin
     FreeMem(pData, sizeOf(TSignalMethod));
end;

function TMessagesList.Add(aMessage: TMessage): PMessage;
begin
     Result :=Add;
     Result^ :=aMessage;
end;


{ TSignalsAsyncThread }

procedure TSignalsAsyncThread._Signal;
begin
     Self.msgSignals.Signal(curMsg^);
end;

procedure TSignalsAsyncThread.Execute;
begin
     //Get the Message from the First Position (Head)
     curMsg :=msgList.GetFirst;
     while (curMsg<>Nil) do
     begin
          //Process the Message
          //maxm: For Future use may be 3 msgSignals owned by priority
          Synchronize(_Signal);

         msgList.DeleteFirst;
         curMsg :=msgList.GetFirst;
     end;
end;

constructor TSignalsAsyncThread.Create;
begin
     inherited Create(true);
     msgList :=TMessagesList.Create;
end;

destructor TSignalsAsyncThread.Destroy;
begin
     msgList.Free;
     inherited Destroy;
end;

procedure TSignalsAsyncThread.Connect(ClassMethod: TSignalMethod;  MessageID: Integer; Priority :Integer=0);
begin
     Self.msgSignals.Connect(ClassMethod, MessageID);
end;

procedure TSignalsAsyncThread.Disconnect(ClassMethod: TSignalMethod; MessageID: Integer);
begin
     Self.msgSignals.Disconnect(ClassMethod, MessageID);
end;

procedure TSignalsAsyncThread.Disconnect(ClassPointer: TObject);
begin
     Self.msgSignals.Disconnect(ClassPointer);
end;

procedure TSignalsAsyncThread.Signal(MessageID: Cardinal; WParam, LParam: Integer);
Var
   aMessage :TMessage;

begin
     aMessage.Msg :=MessageID;
     aMessage.WParam :=WParam;
     aMessage.LParam :=LParam;
     Signal(aMessage);
end;

procedure TSignalsAsyncThread.Signal(aMessage: TMessage);
begin
     //Add the Message to Last Position (Tail)
     msgList.Add(aMessage);
     //Wakeup
     Self.Resume;
end;


// =============================================================================

function TSignalMethodsList.allocData :Pointer;
begin
     GetMem(Result, sizeOf(TSignalMethod));
end;

procedure TSignalMethodsList.deallocData(pData :Pointer);
begin
     FreeMem(pData, sizeOf(TSignalMethod));
end;

function TSignalMethodsList.CompBySignalMethod(xTag :Pointer; ptData1, ptData2 :Pointer) :Boolean;
Var
   m1,
   m2 :TSignalMethod;
   Message1: TMessage;

begin
     m1 :=PSignalMethod(ptData1)^;
     m2 :=PSignalMethod(ptData2)^;

     Result := (TMethod(m1).Data = TMethod(m2).Data) and //Stessa Classe (Instanza) Same Instance
               (TMethod(m1).Code = TMethod(m2).Code);     //Stesso Metodo           Same Method


               //(@m1 = @m2); dovrebbe essere così, ma un metodo di due classi
               //dello stesso tipo viene sempre considerato uguale...
               //EN
               //(@m1 = @m2); should be so, but a method of the same class type
               //is always considered the same...
               //esempio (example):
               //     Classe1, Classe2 :TForm;
               //  Classe1.func = Classe2.func  because
               //    TForm.func = TForm.func    but Classe1 is not the same of Class2
end;

function TSignalMethodsList.Add(AMethod :TSignalMethod) :PSignalMethod;
begin
     Result :=ExtFind(AMethod);
     if (Result=Nil)
     then begin
               Result :=Add;
               Result^ :=AMethod;
          end;
end;

function TSignalMethodsList.Find(AMethod :TSignalMethod) : Integer;
Var
   auxPointer :PSignalMethod;

begin
     GetMem(auxPointer, sizeOf(TSignalMethod));
     auxPointer^ :=AMethod;
     Result :=Find(auxPointer, 0, CompBySignalMethod);
     FreeMem(auxPointer);
end;

function TSignalMethodsList.ExtFind(AMethod :TSignalMethod) : Pointer;
Var
   auxPointer :PSignalMethod;

begin
     GetMem(auxPointer, sizeOf(TSignalMethod));
     auxPointer^ :=AMethod;
     Result :=ExtFind(auxPointer, 0, CompBySignalMethod);
     FreeMem(auxPointer);
end;


function TSignalMethodsList.Delete(AMethod :TSignalMethod) :Boolean;
begin
     Result :=DeleteByClassMethod(AMethod);
end;

function TSignalMethodsList.DeleteByClassMethod(AMethod :TSignalMethod) :Boolean;
Var
   auxPointer :PSignalMethod;

begin
     GetMem(auxPointer, sizeOf(TSignalMethod));
     auxPointer^ :=AMethod;
     Result :=Delete(auxPointer, 0, CompBySignalMethod);
     FreeMem(auxPointer);
end;

procedure TSignalMethodsList.DeleteByClass(ClassPointer :TObject);
Var
   Pt :PSignalMethod;

begin
     Pt :=FindFirst;
     while (Pt<>Nil) do
     begin
          if (TMethod(Pt^).Data = ClassPointer)
          then DeleteCurrent;

          Pt :=FindNext;
     end;
     FindClose;
end;

function TSignalMethodsList.CallAllMethods(var aMessage: TMessage) :Boolean;
Var
   Pt  :PSignalMethod;

begin
     Result :=False;

     Pt :=FindFirst;
     while (Pt<>Nil) do
     begin
          if Assigned(Pt^)
          then Result :=Pt^(aMessage)
          else Result :=False;

          if Result
          then Pt :=FindNext
          else Pt :=Nil;
     end;
     FindClose;
end;


// =============================================================================

constructor TMGSignalsManager.Create;
begin
     inherited Create;
     rClients :=TMGTree16.Create;
end;


procedure TMGSignalsManager.FreeLists(Tag :Integer; wMessageID :Integer; wMessageList :TObject);
begin
     if (wMessageList<>Nil)
     then TSignalMethodsList(wMessageList).Free;
end;

destructor TMGSignalsManager.Destroy;
begin
     rClients.Clear(0, FreeLists);
     rClients.Free;
     inherited Destroy;
end;


procedure TMGSignalsManager.Connect(ClassMethod :TSignalMethod; MessageID :Integer);
Var
   TreeData :PMGTree16Data;
   theList  :TSignalMethodsList;

begin
     TreeData :=rClients.Add(MessageID);
     if (TreeData<>Nil) then
     begin
          theList :=TSignalMethodsList(TreeData^.UData);
          if (theList=nil) //La Lista non esiste...
          then begin
                    theList :=TSignalMethodsList.Create;
                    TreeData^.UData :=theList;
               end;
          theList.Add(ClassMethod);
     end;
end;

procedure TMGSignalsManager.Disconnect(ClassMethod :TSignalMethod; MessageID :Integer);
Var
   uMessageID   :Integer;
   uMessageList :TObject;

begin
     if Assigned(ClassMethod) then
     begin
          if rClients.Find(MessageID, uMessageID, uMessageList)
          then if (uMessageList<>Nil)
               then begin
                         TSignalMethodsList(uMessageList).DeleteByClassMethod(ClassMethod);
                         if (TSignalMethodsList(uMessageList).Count=0)
                         then rClients.Del(MessageID);
                    end;
      end;
end;

procedure TMGSignalsManager.FreeClassOnList(Tag :Integer; wMessageID :Integer; wMessageList :TObject);
Var
   ClassPointer :TObject;

begin
     ClassPointer :=TObject(Tag);
     if (ClassPointer<>Nil) and (wMessageList<>Nil) then
     begin
          TSignalMethodsList(wMessageList).DeleteByClass(ClassPointer);
     end;
end;

procedure TMGSignalsManager.Disconnect(ClassPointer :TObject);
begin
     rClients.WalkOnTree(Integer(ClassPointer), FreeClassOnList);
end;

function TMGSignalsManager.Signal(MessageID :Cardinal; WParam, LParam :Integer; var Handled :Boolean) :Integer;
Var
   aMessage :TMessage;

begin
     aMessage.Msg :=MessageID;
     aMessage.WParam :=WParam;
     aMessage.LParam :=LParam;
     Handled :=Signal(aMessage);
     Result :=aMessage.Result;
end;

function TMGSignalsManager.Signal(var aMessage: TMessage):Boolean;
Var
   uMessageID :Integer;
   uMessageList :TObject;

begin
     Result :=False;

     if rClients.Find(aMessage.Msg, uMessageID, uMessageList)
     then if (uMessageList<>Nil)
          then Result :=TSignalMethodsList(uMessageList).CallAllMethods(aMessage);
end;

{ TMGSignals }

constructor TMGSignals.Create;
begin
     signals_async :=TSignalsAsyncThread.Create;
     signals_sync  :=TMGSignalsManager.Create;
end;

destructor TMGSignals.Destroy;
begin
     signals_async.Free;
     signals_sync.Free;
     inherited Destroy;
end;

procedure TMGSignals.Connect(ClassMethod: TSignalMethod; MessageID: Integer);
begin
     signals_sync.Connect(ClassMethod, MessageID);
end;

procedure TMGSignals.Disconnect(ClassMethod: TSignalMethod; MessageID: Integer);
begin
     signals_sync.Disconnect(ClassMethod, MessageID);
end;

procedure TMGSignals.Disconnect(ClassPointer: TObject);
begin
     signals_sync.Disconnect(ClassPointer);
end;

procedure TMGSignals.ConnectAsync(ClassMethod: TSignalMethod; MessageID: Integer; Priority :Integer=0);
begin
     signals_async.Connect(ClassMethod, MessageID, Priority);
end;

procedure TMGSignals.DisconnectAsync(ClassMethod: TSignalMethod; MessageID: Integer);
begin
     signals_async.Disconnect(ClassMethod, MessageID);
end;

procedure TMGSignals.DisconnectAsync(ClassPointer: TObject);
begin
     signals_async.Disconnect(ClassPointer);
end;

function TMGSignals.Signal(MessageID: Cardinal; WParam, LParam: Integer;
  var Handled: Boolean): Integer;
begin
     Result :=signals_sync.Signal(MessageID, WParam, LParam, Handled);
     signals_async.Signal(MessageID, WParam, LParam);
end;

function TMGSignals.Signal(var aMessage: TMessage): Boolean;
begin
     Result :=signals_sync.Signal(aMessage);
     signals_async.Signal(aMessage);
end;

end.
