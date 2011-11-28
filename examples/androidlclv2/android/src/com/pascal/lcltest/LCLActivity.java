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
      // Allows View.postInvalidate() to work
      setWillNotDraw(false);
    }

    @Override protected void onDraw(Canvas canvas)
    {
      int lWidth = getWidth();
      int lHeight = getHeight();

      Log.v("?", "LCLSurface.onDraw width=" + Integer.toString(lWidth)
        + " height=" + Integer.toString(lHeight));
 
      Bitmap lclbitmap = Bitmap.createBitmap(lWidth, lHeight, Bitmap.Config.ARGB_8888);
      LCLDrawToBitmap(lWidth, lHeight, lclbitmap);
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
    lclsurface.postInvalidate();
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
      Log.i("JNI", "Trying to load liblclapp.so");  
      System.loadLibrary("lclapp");
    } 
    catch(UnsatisfiedLinkError ule) 
    {
      Log.e("JNI", "WARNING: Could not load liblclapp.so");
      ule.printStackTrace();
    }
  }
}
