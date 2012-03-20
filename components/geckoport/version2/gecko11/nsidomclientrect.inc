type

  nsIDOMClientRect = interface(nsISupports)
  ['{B2F824C4-D9D3-499B-8D3B-45C8245497C6}']
    function Getleft(): idlfloat; safecall;
    property left : idlfloat read Getleft;
    function Gettop(): idlfloat; safecall;
    property top : idlfloat read Gettop;
    function Getright(): idlfloat; safecall;
    property right : idlfloat read Getright;
    function Getbottom(): idlfloat; safecall;
    property bottom : idlfloat read Getbottom;
    function Getwidth(): idlfloat; safecall;
    property width : idlfloat read Getwidth;
    function Getheight(): idlfloat; safecall;
    property height : idlfloat read Getheight;

  end;

