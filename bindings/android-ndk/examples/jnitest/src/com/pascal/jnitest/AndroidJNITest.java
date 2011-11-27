package com.pascal.jnitest;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;
import android.util.Log;

public class AndroidJNITest extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
          
        TextView  tv = new TextView(this);
        tv.setText( Integer.toString(intFromJNI()) );
        setContentView(tv);
    }
    
    public native String stringFromJNI();
    public native int intFromJNI();
    
    public long nativeCodeLoaded=0;
     
    static {
    	try {
     		Log.i("JNI", "Trying to load libnativetest.so");  
    		System.loadLibrary("nativetest");
    	} catch(UnsatisfiedLinkError ule) {
            Log.e("JNI", "WARNING: Could not load libnativetest.so");
            ule.printStackTrace();
        }

    }    
}
