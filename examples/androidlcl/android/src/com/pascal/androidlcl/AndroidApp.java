package com.pascal.androidlcl;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.*;
import android.util.*;
import android.content.*;
import android.view.*;
import android.widget.*;
import java.util.*;

public class AndroidApp
{
  // info from other classes
  Activity activity;
  AndroidPipesComm MyAndroidPipesComm;
  JavaLang MyJavaLang;
  AndroidAll MyAndroidAll;

  // lists of variables
  ArrayList ViewElements;

  //
  // android.app.*
  static final int amkUI_Activity_setContentView = 0x0000;
  static final int amkUI_Activity_getWindowManager = 0x0080;

  public AndroidApp(AndroidPipesComm AAndroidPipesComm, Activity AActivity, JavaLang AJavaLang, AndroidAll AAndroidAll)
  {
    activity = AActivity;
    MyAndroidPipesComm = AAndroidPipesComm;
    MyJavaLang = AJavaLang;
    MyAndroidAll = AAndroidAll;
    ViewElements = AAndroidAll.ViewElements;
  }

  public void DebugOut(String Str)
  {
    MyAndroidPipesComm.DebugOut(Str);
  }

  public boolean ProcessCommand(int Buffer)
  {
    //DebugOut("AndroidUI.ProcessCommand");
    //DebugOut("AndroidUI.ProcessCommand Command=" + java.lang.Integer.toHexString(Buffer));

    int lInt, lIndex, lPascalPointer;
    Integer lTag;
    TextView lTextView;
    CharSequence lChars;
    //
    WindowManager param_self_WindowManager;
    //

    switch (Buffer)
    {
    // android.app.Activity 
    // public void setContentView (View view)
    case amkUI_Activity_setContentView:
      DebugOut("amkUI_Activity_setContentView");
      lInt = MyAndroidPipesComm.GetInt();
      View lView = (View) ViewElements.get(lInt);
      activity.setContentView(lView);
      MyAndroidPipesComm.SendResult();
      break;
/*    case amkUI_Activity_getWindowManager:
      DebugOut("amkUI_Activity_getWindowManager");
      WindowManagerElements.add(activity.getWindowManager());
      MyAndroidPipesComm.SendIntResult(WindowManagerElements.size() - 1);
      break;*/
    default:
      return false;
    }
    return true;
  }
}

