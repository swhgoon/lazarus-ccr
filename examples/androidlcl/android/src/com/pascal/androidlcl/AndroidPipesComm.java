package com.pascal.androidlcl;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.view.*;
import android.widget.*;
import java.io.*;

public class AndroidPipesComm
{
  // info from other classes
  Activity activity;
  
  // Data pool accessed by many methods
  java.lang.Process PascalProcess;
  DataOutputStream writer;
  DataInputStream reader;
  // Data pool to solve the lack of passing parameter by reference
  byte lType = 0;
  int lSubtype = 0;

  // Android Message Kind
  static byte amkFloatResult = 103;
  static byte amkIntResult = 102;
  static byte amkResult = 101;
  static byte amkActivityCallback = 0;
  static byte amkLog = 1;
  static byte amkUICommand = 2;
  static byte amkJavaLangCall = 3;
  static byte amkTimer = 4;

  // Add / remove modules here
  public JavaLang MyJavaLang;
  public AndroidAll MyAndroidAll;
  public AndroidApp MyAndroidApp;

  public AndroidPipesComm(Activity AActivity)
  {
    activity = AActivity;
    MyJavaLang = new JavaLang(this);
    MyAndroidAll = new AndroidAll(this, activity, MyJavaLang);
    MyAndroidApp = new AndroidApp(this, activity, MyJavaLang, MyAndroidAll);
  }

  public void DebugOut(String Str)
  {
    Log.v("AndroidPipesComm:", Str);
//    tv.setText(tv.getText().toString() + Str);
  }
  
  public void ErrorOut(String Str)
  {
    Log.v("AndroidPipesComm:", Str);
//    tv.setText(tv.getText().toString() + Str);
  }

  public void TerminateApplication()
  {
    System.exit(0);
  }

  public void PrintPascalException(byte Buffer)
  {
    String PascalMessage = "" + (char) Buffer;
    try
    {
      while (true)
      {
        PascalMessage = PascalMessage + (char) reader.readByte();
      }
    }
    catch (IOException e)
    {
      ErrorOut(PascalMessage);
    }
  }
  
  // Waits for a particular Pascal message.
  // In the mean time processes any other incoming messages
  public void WaitForPascalMessage(byte AExpectedMessageType, int AExpectedMessageSubtype)
  {
    DebugOut("START WaitForPascalMessage AExpectedMessageType=" + AExpectedMessageType
     + " AExpectedMessageSubtype=" + java.lang.Integer.toHexString(AExpectedMessageSubtype));

    while (true)
    {
      WaitAndProcessPascalMessage();
      DebugOut("MID_END WaitForPascalMessage lType=" + lType + " lSubtype=" + java.lang.Integer.toHexString(lSubtype));
      if ((lType == AExpectedMessageType) && (lSubtype == AExpectedMessageSubtype)) return;
      if ((lType == amkActivityCallback) && (AExpectedMessageType == amkActivityCallback)) return;
    }
  }

  // Waits for and processes a Pascal message
  public void WaitAndProcessPascalMessage()
  {
    try
    {
      DebugOut("WaitAndProcessPascalMessage");
      byte Buffer = reader.readByte(); // blocking read
      lType = Buffer;
      lSubtype = 0;

      if (Buffer == amkActivityCallback)
      {
        DebugOut("amkActivityCallback");
//        lSubtype = reader.readInt(); // blocking read
      }
      else if (Buffer == amkLog)
      {
        DebugOut("amkLog");
      }
      else if (Buffer == amkUICommand)
      {
        DebugOut("amkUICommand");
        lSubtype = reader.readInt(); // blocking read
        if (MyAndroidAll.ProcessCommand(lSubtype) == true) ;
        else if (MyAndroidApp.ProcessCommand(lSubtype) == true) ;
        else
        {
          ErrorOut("Unknown UI Command!!!" + java.lang.Integer.toHexString(lSubtype));
        };
      }
      else if (Buffer == amkJavaLangCall)
      {
        DebugOut("amkJavaLangCall");
        lSubtype = reader.readInt(); // blocking read
        MyJavaLang.ProcessCommand(lSubtype);
      }
//      else if (Buffer == amkTimer)
//      {
//        DebugOut("amkTimer");
//        lSubtype = reader.readInt(); // blocking read
//        MyAndroidTimer.ProcessCommand(lSubtype);
//      }
      else
      {
        ErrorOut("Unknown Pascal message!!! " + java.lang.Integer.toHexString(Buffer));

        // If we get an Unknown Pascal message, it might be an error printed to the console, so lets print it and quit
        PrintPascalException(Buffer);
        TerminateApplication();
      }
    }
    catch (EOFException e)
    {
      ErrorOut("[WaitAndProcessPascalMessage] EOFException=" + e.getMessage());
      TerminateApplication();
    }
    catch (IOException e)
    {
      ErrorOut("[WaitAndProcessPascalMessage] IOException=" + e.getMessage());
    }
  }

  // Receiving data
 
  public byte GetByte()
  {
    try
    {
      return reader.readByte();
    }
    catch (IOException e)
    {
      ErrorOut("[GetByte] IOException=" + e.getMessage());
    }
    return -1;
  }

  public int GetInt()
  {
    try
    {
      return reader.readInt();
    }
    catch (IOException e)
    {
      ErrorOut("[GetInt] IOException=" + e.getMessage());
    }
    return -1;
  }

  public boolean GetBool()
  {
    try
    {
      int Tmp = reader.readInt();
      if (Tmp == 0) return false;
      else return true;
    }
    catch (IOException e)
    {
      ErrorOut("[GetBool] IOException=" + e.getMessage());
    }
    return false;
  }

  public float GetFloat()
  {
    try
    {
      return reader.readFloat();
    }
    catch (IOException e)
    {
      ErrorOut("[GetFloat] IOException=" + e.getMessage());
    }
    return -1;
  }

  // Sending results

  public void SendResult()
  {
    try
    {
      writer.writeByte(amkResult);
    }
    catch (IOException e)
    {
      ErrorOut("[SendResult] IOException=" + e.getMessage());
    } 
  }

  public void SendIntResult(int Result)
  {
    try
    {
      writer.writeByte(amkIntResult);
      writer.writeInt(Result);
    }
    catch (IOException e)
    {
      ErrorOut("[SendIntResult] IOException=" + e.getMessage());
    } 
  }

  public void SendFloatResult(float Result)
  {
    try
    {
      writer.writeByte(amkFloatResult);
      writer.writeFloat(Result);
//      writer.writeInt(Float.floatToIntBits(Result));
    }
    catch (IOException e)
    {
      ErrorOut("[SendFloatResult] IOException=" + e.getMessage());
    } 
  }

  // Sending data

  // Convenience routine, uses SendIntResult
  public void SendBoolResult(boolean Result)
  {
    if (Result == false) SendIntResult(0);
    else SendIntResult(-1);
  }

  public void SendInt(int AData)
  {
    try
    {
      writer.writeInt(AData);
    }
    catch (IOException e)
    {
      ErrorOut("[SendBoolResult] IOException=" + e.getMessage());
    }
  }

  public void SendMessage(byte AMessageKind, int AMessageSubtype)
  {
    try
    {
      writer.writeByte(AMessageKind);
      writer.writeInt(AMessageSubtype);
    }
    catch (IOException e)
    {
      ErrorOut("[SendMessage] IOException=" + e.getMessage());
      TerminateApplication();
    }
  }

//      String separator = System.getProperty("line.separator");
/*    // Displays a dialog with the result of the operation
    AlertDialog alertDialog;
    alertDialog = new AlertDialog.Builder(this).create();
    alertDialog.setTitle("Result of cp");
    alertDialog.setMessage(Str + "||" + Str2 + "||" + Str3);
    alertDialog.show();*/
      // Waits until it finishes
//      process.waitFor();
   // Thread.currentThread().sleep(1000);
}
