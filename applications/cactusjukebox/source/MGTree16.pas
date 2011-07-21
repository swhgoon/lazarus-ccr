(******************************************************************************

  Massimo Magnano 08-11-2004.

  File : MGTree16.pas      REV. 1.1


      Implementazione di un albero per la memorizzazione di 2^32 Dati a 32 Bit.
  Nel caso peggiore si hanno 16 passi tra i nodi per la ricerca di un dato

      Implementation of a tree to store 2^32 Data of 32 Bit.
  In the worst case you have 16 walk on the nodes for data research
******************************************************************************)


unit MGTree16;
{$mode delphi}{$H+}

interface

//{$O-}

Type
    PMGTree16Data =^TMGTree16Data;
    TMGTree16Data = packed record
       Data  :Integer;
       UData :TObject;
       Nodes :array[0..3] of PMGTree16Data;
    end;
    PPMGTree16Data =^PMGTree16Data;

    TWalkFunction = procedure (Tag:Integer; Data  :Integer; UData :TObject) of object;

    TMGTree16 = class
    protected
       pRoot     :PMGTree16Data;
       Allocated :Cardinal;

       function allocData :PMGTree16Data;
       procedure deallocData(pData :PMGTree16Data);
       function InternalFind(Value :Integer; var pParent :PMGTree16Data;
                             var ValNode :Byte) :PMGTree16Data;
    public
       constructor Create;
       destructor  Destroy; override;
       function Add(Data:Integer; UData :TObject=nil; pNode :PMGTree16Data=Nil) : PMGTree16Data;
       function Del(Data :Integer) : Boolean;
       function Find(Value :Integer; var Data :Integer; var UData :TObject) : Boolean;
       procedure WalkOnTree(Tag:Integer; WalkFunction :TWalkFunction);
       procedure Clear(Tag:Integer=0; WalkFunction :TWalkFunction=Nil);
    end;


implementation

constructor TMGTree16.Create;
begin
     Allocated :=0;
     pRoot :=Self.allocData;
end;

destructor  TMGTree16.Destroy;
begin
     Self.Clear;
     FreeMem(pRoot);

     inherited Destroy;
end;

function TMGTree16.allocData :PMGTree16Data;
begin
     GetMem(Result, sizeof(TMGTree16Data));
     FillChar(Result^, sizeof(TMGTree16Data), 0);
     Inc(Allocated, sizeof(TMGTree16Data));
end;

procedure TMGTree16.deallocData(pData :PMGTree16Data);
begin
     FreeMem(pData, sizeof(TMGTree16Data));
     Dec(Allocated, sizeof(TMGTree16Data));
end;

//Add a new node on tree, if pNode <> nil add pNode else create a new node and
//assign to it Data, UData
// Return : the node just created or nil if error
function TMGTree16.Add(Data :Integer; UData :TObject=nil; pNode :PMGTree16Data=Nil) : PMGTree16Data;
Var
   ValNode :Byte;  //0..3
   i       :Integer;
   pParent,
   pData   :PMGTree16Data;
   iValue  :Integer;

begin
     pData :=pRoot;
     Result :=Nil;
     if (pNode<>Nil)
     then Data :=pNode^.Data;
     iValue :=Data;

     for i:=0 to 15 do
     begin
          ValNode := (Data and $00000003);

          pParent :=pData;
          pData :=pData^.Nodes[ValNode];
          if (pData=Nil)
          then begin
                    if pNode=Nil
                    then begin
                              pData :=Self.allocData;
                              pData^.Data :=iValue;
                              pData^.UData :=UData;
                         end
                    else pData :=pNode;

                    pParent^.Nodes[ValNode] :=pData;
                    Result :=pData;
                    Exit;
               end;

          if (pData^.Data=iValue)
          then begin
                    Result :=pData;
                    Exit;
               end;

          Data :=Data shr 2;
     end;
end;

//Del the node that have Data as ID, if node have subnode attached to it
// reinsert in correct position.
function TMGTree16.Del(Data :Integer) : Boolean;
Var
   pData,
   pParent    :PMGTree16Data;
   ValNode, i :Byte;

begin
     pData :=InternalFind(Data, pParent, ValNode);

     Result := (pData<>Nil);
     if Result
     then begin
              //Reinserisco le (foglie <> Nil) del nodo che si sta per eliminare

               pParent^.Nodes[ValNode] :=pData^.Nodes[ValNode]; //bypass
               for i:=0 to 3 do
               begin
                    if (i<>ValNode) and (pData^.Nodes[i]<>Nil)
                    then Add(0, nil, pData^.Nodes[i]);
                end;
               Self.deallocData(pData);
          end;     
end;

//Find the node that have Value as ID
//Return :
//          pParent = Parent of the Node
//          ValNode = sub Node on were i'm attached in Parent
//          Result  = the Node
function TMGTree16.InternalFind(Value :Integer; var pParent :PMGTree16Data;
                              var ValNode :Byte) :PMGTree16Data;
Var
   i       :Integer;
   pData   :PMGTree16Data;
   iValue  :Integer;

begin
     pData :=pRoot;
     iValue :=Value;
     Result :=Nil;

     for i:=0 to 15 do
     begin
          ValNode := (Value and $00000003);

          pParent :=pData;
          pData :=pData^.Nodes[ValNode];
          if (pData=Nil)
          then begin
                    Result :=Nil;
                    Exit;
               end;

          if (pData^.Data=iValue)
          then begin
                    Result :=pData;
                    Exit;
               end;

          Value :=Value shr 2;
     end;
end;

//User Visible Find, search for node that have Value as ID
//Return : Data, UData = Node data
//         Result = True if find
function TMGTree16.Find(Value :Integer; var Data :Integer; var UData :TObject) : Boolean;
Var
   pData,
   pParent :PMGTree16Data;
   ValNode :Byte;

begin
     pData :=InternalFind(Value, pParent, ValNode);

     Result := (pData<>Nil);
     if Result
     then begin
               Data :=pData^.Data;
               UData :=pData^.UData;
          end;     
end;

//Recursivly Walk on tree and call user defined function on every node
procedure TMGTree16.WalkOnTree(Tag:Integer; WalkFunction :TWalkFunction);

  procedure __Walk(pData :PMGTree16Data);
  Var
     i :Byte;

  begin
       for i :=0 to 3 do
       begin
            if pData^.Nodes[i]<>Nil
            then begin
                      __Walk(pData^.Nodes[i]);
                      if Assigned(WalkFunction)
                      then WalkFunction(Tag, pData^.Nodes[i]^.Data, pData^.Nodes[i]^.UData);
                 end;
        end;
  end;

begin
     __Walk(pRoot);
end;

//Recursivly delete node on tree, first call user defined function on every node.
procedure TMGTree16.Clear(Tag:Integer=0; WalkFunction :TWalkFunction=Nil);


  procedure __Clear(pData :PMGTree16Data);
  Var
     i :Byte;

  begin
       for i :=0 to 3 do
       begin
            if pData^.Nodes[i]<>Nil
            then begin
                      __Clear(pData^.Nodes[i]);
                      if Assigned(WalkFunction)
                      then WalkFunction(Tag, pData^.Nodes[i]^.Data, pData^.Nodes[i]^.UData);
                      Self.deallocData(pData^.Nodes[i]);
                      pData^.Nodes[i] :=Nil;
                 end;
        end;
  end;

begin
     __Clear(pRoot);
end;


end.

