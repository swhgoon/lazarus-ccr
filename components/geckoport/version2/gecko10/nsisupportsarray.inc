type
  nsISupportsArrayEnumFunc = function(aElement: nsISupports; aData: pointer) : longbool;

type

  nsISupportsArray = interface(nsICollection)
  ['{791eafa0-b9e6-11d1-8031-006008159b5a}']
    function Equals(other: nsISupportsArray) : longbool; safecall;
    function ElementAt(aIndex: idlulong) : nsISupports; safecall;
    function IndexOf(aPossibleElement: nsISupports) : idllong; safecall;
    function IndexOfStartingAt(aPossibleElement: nsISupports; aStartIndex: idlulong) : idllong; safecall;
    function LastIndexOf(aPossibleElement: nsISupports) : idllong; safecall;
    function GetIndexOf(aPossibleElement: nsISupports) : idllong; safecall;
    function GetIndexOfStartingAt(aPossibleElement: nsISupports; aStartIndex: idlulong) : idllong; safecall;
    function GetLastIndexOf(aPossibleElement: nsISupports) : idllong; safecall;
    function InsertElementAt(aElement: nsISupports; aIndex: idlulong) : longbool; safecall;
    function ReplaceElementAt(aElement: nsISupports; aIndex: idlulong) : longbool; safecall;
    function RemoveElementAt(aIndex: idlulong) : longbool; safecall;
    function RemoveLastElement(aElement: nsISupports) : longbool; safecall;
    procedure DeleteLastElement(aElement: nsISupports); safecall;
    procedure DeleteElementAt(aIndex: idlulong); safecall;
    function AppendElements(aElements: nsISupportsArray) : longbool; safecall;
    procedure Compact(); safecall;
    function EnumerateForwards(aFunc: nsISupportsArrayEnumFunc; aData: voidPtr) : longbool; safecall;
    function EnumerateBackwards(aFunc: nsISupportsArrayEnumFunc; aData: voidPtr) : longbool; safecall;
    function clone() : nsISupportsArray; safecall;
    function MoveElement(aFrom: idllong; aTo: idllong) : longbool; safecall;
    function InsertElementsAt(aOther: nsISupportsArray; aIndex: idlulong) : longbool; safecall;
    function RemoveElementsAt(aIndex: idlulong; aCount: idlulong) : longbool; safecall;
    function SizeTo(aSize: idllong) : longbool; safecall;

  end;

