package com.pascal.androidlcl;

import java.lang.*;
import java.util.*;

public class JavaLang
{
  // info from other classes
  AndroidPipesComm MyAndroidPipesComm;

  // lists of variables
  ArrayList LangElements = new ArrayList();

  // 
  static final int amkJavaLang_New_String = 0x0000;

  public JavaLang(AndroidPipesComm AAndroidPipesComm)
  {
    MyAndroidPipesComm = AAndroidPipesComm;
  }

  public void DebugOut(String Str)
  {
    MyAndroidPipesComm.DebugOut(Str);
  }

  public void ProcessCommand(int Buffer)
  {
    //  DebugOut("<before readByte>");
    int lInt;

    switch (Buffer)
    {
    // java.lang.String
    // String(char[] data)
    case amkJavaLang_New_String:
      DebugOut("amkJavaLang_New_String");
      lInt = MyAndroidPipesComm.GetInt(); // Length
      char[] lChars = new char[lInt];
      for (int i = 0; i < lInt; i++)
      {
        lChars[i] = (char) MyAndroidPipesComm.GetByte();
      }
      LangElements.add(new String(lChars));
      MyAndroidPipesComm.SendIntResult(LangElements.size() - 1);
      break;
    }
  }
}

