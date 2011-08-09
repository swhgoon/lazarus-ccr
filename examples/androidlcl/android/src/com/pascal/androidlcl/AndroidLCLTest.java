package com.pascal.androidlcl;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.view.*;
import android.widget.*;
import java.io.*;

public class AndroidLCLTest extends Activity
{
  AndroidPipesComm MyAndroidPipesComm;
  String ExecutablePath;
  // Debug
  boolean DEBUG_PASCAL4ANDROID = true;

  // Android Message subtype
  static int ams_ActivityCallback_onCreateStarted = 0x0001;
  static int ams_ActivityCallback_onStartStarted = 0x0002;
  static int ams_ActivityCallback_onRestartStarted = 0x0003;
  static int ams_ActivityCallback_onResumeStarted = 0x0004;
  static int ams_ActivityCallback_onPauseStarted = 0x0005;
  static int ams_ActivityCallback_onStopStarted = 0x0006;
  static int ams_ActivityCallback_onDestroyStarted = 0x0007;
  static int ams_ActivityCallback_onCreateOptionsMenuStarted = 0x0008;
  static int ams_ActivityCallback_onKeyUpStarted = 0x0010;

  static int ams_ActivityCallback_onCreateFinished = 0x1001;
  static int ams_ActivityCallback_onStartFinished = 0x1002;
  static int ams_ActivityCallback_onRestartFinished = 0x1003;
  static int ams_ActivityCallback_onResumeFinished = 0x1004;
  static int ams_ActivityCallback_onPauseFinished = 0x1005;
  static int ams_ActivityCallback_onStopFinished = 0x1006;
  static int ams_ActivityCallback_onDestroyFinished = 0x1007;
  static int ams_ActivityCallback_onCreateOptionsMenuFinished = 0x1008;
  static int ams_ActivityCallback_onKeyUpFinished = 0x1010;

  Button.OnClickListener buttonClickCallback = new Button.OnClickListener()
    {
      public void onClick(View v)
      {
        MyAndroidPipesComm.DebugOut("Click!");
      }
    };

  /** Called when the activity is first created. */
  @Override
  public void onCreate(Bundle savedInstanceState)
  {
    super.onCreate(savedInstanceState);

    MyAndroidPipesComm = new AndroidPipesComm(this);

    // Prepare the application
    ExecutablePath = getApplicationContext().getFilesDir() + "/androidlcltest";
 
    String LibraryPathPath = "/data/data/com.pascal.androidlcl/lib/libandroidlcltest.so";

    MyAndroidPipesComm.DebugOut("Executing application " + LibraryPathPath + " copied to " + ExecutablePath);

    String Str = FileCopy(LibraryPathPath, ExecutablePath);

    String Str2 = RunProgram("/system/bin/chmod", "755", ExecutablePath);

    // Run the application
    RunPascalProgram(ExecutablePath);

    // Waits until the Pascal side finishes its initialization
    MyAndroidPipesComm.DebugOut("WaitForPascalMessage(amkActivityCallback, ams_ActivityCallback_onCreateFinished)");
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onCreateFinished);
    MyAndroidPipesComm.DebugOut("END onCreate");
  }

  /**  */
  @Override
  protected void onStart()
  {
    MyAndroidPipesComm.DebugOut("START onStart");
    super.onStart();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStartStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStartFinished);
    MyAndroidPipesComm.DebugOut("END onStart");
  }

  /**  */
  @Override
  protected void onRestart()
  {
    MyAndroidPipesComm.DebugOut("START onRestart");
    super.onRestart();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onRestartStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onRestartFinished);
    MyAndroidPipesComm.DebugOut("END onRestart");
  }

  /**  */
  @Override
  protected void onResume()
  {
    MyAndroidPipesComm.DebugOut("START onResume");
    super.onResume();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onResumeStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onResumeFinished);
    MyAndroidPipesComm.DebugOut("END onResume");
  }

  /**  */
  @Override
  protected void onPause()
  {
    MyAndroidPipesComm.DebugOut("START onPause");
    super.onPause();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onPauseStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onPauseFinished);
    MyAndroidPipesComm.DebugOut("END onPause");
  }

  /**  */
  @Override
  protected void onStop()
  {
    MyAndroidPipesComm.DebugOut("START onStop");
    super.onStop();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStopStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStopFinished);
    MyAndroidPipesComm.DebugOut("END onStop");
  }

  /**  */
  @Override
  protected void onDestroy()
  {
    MyAndroidPipesComm.DebugOut("START onDestroy");
    super.onDestroy();
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStartStarted);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onStartFinished);
    MyAndroidPipesComm.DebugOut("END onDestroy");
  }

  // public boolean onCreateOptionsMenu (Menu menu)
  /*@Override
  public boolean onCreateOptionsMenu(Menu menu)
  {
    if (DEBUG_PASCAL4ANDROID == true) MyAndroidPipesComm.DebugOut("START onCreateOptionsMenu");
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onCreateOptionsMenuStarted);
    MyAndroidPipesComm.MyAndroidMenu.MenuElements.add(menu);
    int lMenuIndex = MyAndroidPipesComm.MyAndroidMenu.MenuElements.size() - 1;
    MyAndroidPipesComm.SendInt(lMenuIndex);
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onCreateOptionsMenuFinished);
    int lInt = MyAndroidPipesComm.GetInt();
    // ToDo: If we start supporting object deletion then we need to use indexOf to get the new index
    MyAndroidPipesComm.MyAndroidMenu.MenuElements.remove(lMenuIndex);
    if (DEBUG_PASCAL4ANDROID == true) MyAndroidPipesComm.DebugOut("END onCreateOptionsMenu");
    return (lInt != 0);
  }*/

  // generic Key Listener
  @Override
  public boolean onKeyUp(int keyCode, KeyEvent event)
  {
    if (DEBUG_PASCAL4ANDROID == true) MyAndroidPipesComm.DebugOut("START onKeyUp");
    MyAndroidPipesComm.SendMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onKeyUpStarted);
    MyAndroidPipesComm.SendInt(keyCode);
    // ToDo: Also send the KeyEvent
    //MyAndroidPipesComm.MyAndroidMenu.MenuElements.add(menu);
    //int lMenuIndex = MyAndroidPipesComm.MyAndroidMenu.MenuElements.size() - 1;
    MyAndroidPipesComm.WaitForPascalMessage(MyAndroidPipesComm.amkActivityCallback, ams_ActivityCallback_onKeyUpFinished);
    int lInt = MyAndroidPipesComm.GetInt();
    if (DEBUG_PASCAL4ANDROID == true) MyAndroidPipesComm.DebugOut("END onCreateOptionsMenu");
    if (lInt != 0) return true;
    return super.onKeyUp(keyCode, event);
  }

  //
  private static String FileCopy(String srFile, String dtFile)
  {
    try
    {
      File f1 = new File(srFile);
      File f2 = new File(dtFile);
      InputStream in = new FileInputStream(f1);
      
      //For Overwrite the file.
      OutputStream out = new FileOutputStream(f2, true);

      byte[] buf = new byte[1024];
      int len;
      while ((len = in.read(buf)) > 0){
        out.write(buf, 0, len);
      }
      in.close();
      out.close();

      return "Success dest=" + dtFile;
    }
    catch(FileNotFoundException e)
    {
      return "Error=FileNotFoundException=" + e.getMessage();
    }
    catch(IOException e)
    {
      return "Error=IOException=" + e.getMessage();
    }
  }

  /*
    Simple method for running external applications.
    It waits until the program finishes running.
    it uses Runtime.getRuntime()
  */
  public String RunProgram(String cmd, String param1, String param2)
  {
    String output = "";
    try
    {
      if (param1 == null) param1 = "";
      if (param2 == null) param2 = "";

      // Runs the application
      java.lang.Process process = Runtime.getRuntime().exec(cmd + " " + param1 + " " + param2);

      // Now connects pipes to it
      DataInputStream reader = new DataInputStream(process.getInputStream());

      // Waits until it finishes
      process.waitFor();

      // And stores the result of the pipes in the result of the function
      String line;
      String separator = System.getProperty("line.separator");
      while ((line = reader.readLine()) != null)
      {   
        output += line + separator;
      }
    }
    catch (IOException e)
    {
      output += "error=IOException=" + e.getMessage();
    } 
    catch (InterruptedException e)
    {
      output += "error=InterruptedException=" + e.getMessage();
    } 

    return output;
  }

  /*
    This function will run a Pascal program and interact with it via pipes
    Uses Runtime.getRuntime()
  */
  public void RunPascalProgram(String cmd)
  {
    try
    {
      // Runs the application
      MyAndroidPipesComm.PascalProcess = Runtime.getRuntime().exec(cmd);

      // Now connects pipes to it
      MyAndroidPipesComm.writer = new DataOutputStream(
        MyAndroidPipesComm.PascalProcess.getOutputStream());
      MyAndroidPipesComm.reader = new DataInputStream(
        MyAndroidPipesComm.PascalProcess.getInputStream());
    }
    catch (IOException e)
    {
      MyAndroidPipesComm.DebugOut("error=IOException=" + e.getMessage());
    } 
  }
}

