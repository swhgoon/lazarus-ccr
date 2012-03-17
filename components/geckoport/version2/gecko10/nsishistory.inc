type

  nsISHistory = interface(nsISupports)
  ['{ef2c9bcb-96b8-4095-933a-cb1c506f2c58}']
    function Getcount(): idllong; safecall;
    property count : idllong read Getcount;
    function Getindex(): idllong; safecall;
    property index : idllong read Getindex;
    function GetrequestedIndex(): idllong; safecall;
    property requestedIndex : idllong read GetrequestedIndex;
    function GetmaxLength(): idllong; safecall;
    procedure SetmaxLength(amaxLength: idllong); safecall;
    property maxLength : idllong read GetmaxLength write SetmaxLength;
    function getEntryAtIndex(anindex: idllong; modifyIndex: longbool) : nsIHistoryEntry; safecall;
    procedure PurgeHistory(numEntries: idllong); safecall;
    procedure addSHistoryListener(aListener: nsISHistoryListener); safecall;
    procedure removeSHistoryListener(aListener: nsISHistoryListener); safecall;
    function GetSHistoryEnumerator(): nsISimpleEnumerator; safecall;
    property SHistoryEnumerator : nsISimpleEnumerator read GetSHistoryEnumerator;
    procedure reloadCurrentEntry(); safecall;

  end;

