package com.pascal.lcltest;

import android.app.*;
import android.content.*;
import android.os.*;
import android.widget.*;
import android.util.*;
import android.graphics.*;
import android.view.*;

public class LCLActivity extends Activity 
{
  // Our drawing surface
  private class LCLSurface extends SurfaceView
  {
    public LCLSurface(Context context)
    {
      super(context);
    }
   
    @Override protected void onDraw(Canvas canvas)
    {
      Bitmap lclbitmap = Bitmap.createBitmap(getWidth(), getHeight(), Bitmap.Config.ARGB_8888);
      LCLDrawToBitmap(getWidth(), getHeight(), lclbitmap);
      canvas.drawBitmap(lclbitmap, 0, 0, null);
    }
  }    

  /** Called when the activity is first created. */
  @Override
  public void onCreate(Bundle savedInstanceState) 
  {
    super.onCreate(savedInstanceState);
          
//        TextView  tv = new TextView(this);
//        tv.setText( Integer.toString(intFromJNI()) );
//        setContentView(tv);
    LCLSurface lclsurface = new LCLSurface(this);
    setContentView(lclsurface);
  }
  
  // JNI table of functions  
  public native String stringFromJNI();
  public native int intFromJNI();
  public native int LCLDrawToBitmap(int width, int height, Bitmap bitmap);
    
  public long nativeCodeLoaded=0;
     
  static 
  {
    try 
    {
      Log.i("JNI", "Trying to load libnativetest.so");  
      System.loadLibrary("nativetest");
    } 
    catch(UnsatisfiedLinkError ule) 
    {
      Log.e("JNI", "WARNING: Could not load libnativetest.so");
      ule.printStackTrace();
    }
  }
}
