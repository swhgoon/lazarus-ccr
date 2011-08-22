package com.pascal.androidlcl;

import android.app.*;
import android.view.*;
import android.os.*;
import android.util.*;
import android.content.*;
import android.view.*;
import android.widget.*;
import android.R.*;
import java.util.*;
import java.lang.*;

public class AndroidAll
{
  // info from other classes
  Activity activity;
  AndroidPipesComm MyAndroidPipesComm;
  JavaLang MyJavaLang;
  // lists of variables
  ArrayList ViewElements;

  public AndroidAll(AndroidPipesComm AAndroidPipesComm, Activity AActivity, JavaLang AJavaLang)
  {
    activity = AActivity;
    MyAndroidPipesComm = AAndroidPipesComm;
    MyJavaLang = AJavaLang;
    ViewElements = new ArrayList();
  }

  public void DebugOut(String Str)
  {
    MyAndroidPipesComm.DebugOut(Str);
  }

  // DisplayMetrics
  static final int amkUI_TDisplayMetrics_Create_0 = 0x00101000;
  static final int amkUI_TDisplayMetrics_density_1 = 0x00101001;
  static final int amkUI_TDisplayMetrics_densityDpi_2 = 0x00101002;
  static final int amkUI_TDisplayMetrics_heightPixels_3 = 0x00101003;
  static final int amkUI_TDisplayMetrics_scaledDensity_4 = 0x00101004;
  static final int amkUI_TDisplayMetrics_widthPixels_5 = 0x00101005;
  static final int amkUI_TDisplayMetrics_xdpi_6 = 0x00101006;
  static final int amkUI_TDisplayMetrics_ydpi_7 = 0x00101007;
  // Activity
  static final int amkUI_TActivity_setTitle_0 = 0x00102000;
  static final int amkUI_TActivity_getTitle_1 = 0x00102001;
  // Display
  static final int amkUI_TDisplay_getMetrics_0 = 0x00103000;
  // WindowManager
  static final int amkUI_TWindowManager_getDefaultDisplay_0 = 0x00104000;
  // ViewGroup.LayoutParams
  static final int amkUI_TViewGroup_LayoutParams_Create_0 = 0x00105000;
  // View
  static final int amkUI_TView_setLayoutParams_0 = 0x00106000;
  static final int amkUI_TView_setVisibility_1 = 0x00106001;
  // ViewGroup
  static final int amkUI_TViewGroup_addView_0 = 0x00107000;
  static final int amkUI_TViewGroup_addView_1 = 0x00107001;
  static final int amkUI_TViewGroup_addView_2 = 0x00107002;
  static final int amkUI_TViewGroup_addView_3 = 0x00107003;
  static final int amkUI_TViewGroup_addView_4 = 0x00107004;
  // LinearLayout
  static final int amkUI_TLinearLayout_Create_0 = 0x00108000;
  static final int amkUI_TLinearLayout_setOrientation_1 = 0x00108001;
  // AbsoluteLayout
  static final int amkUI_TAbsoluteLayout_Create_0 = 0x00109000;
  // AbsoluteLayout.LayoutParams
  static final int amkUI_TAbsoluteLayout_LayoutParams_Create_0 = 0x0010A000;
  // TextView
  static final int amkUI_TTextView_Create_0 = 0x0010B000;
  static final int amkUI_TTextView_setText_1 = 0x0010B001;
  static final int amkUI_TTextView_setOnClickListener_2 = 0x0010B002;
  static final int amkUI_TTextView_OnClickListener_Start_3 = 0x0010B003;
  static final int amkUI_TTextView_OnClickListener_Finished_4 = 0x0010B004;
  static final int amkUI_TTextView_setTextSize_5 = 0x0010B005;
  static final int amkUI_TTextView_getText_6 = 0x0010B006;
  // EditText
  static final int amkUI_TEditText_Create_0 = 0x0010C000;
  static final int amkUI_TEditText_setText_1 = 0x0010C001;
  // Button
  static final int amkUI_TButton_Create_0 = 0x0010D000;
  static final int amkUI_TButton_setText_1 = 0x0010D001;
  // FrameLayout
  // TimePicker
  static final int amkUI_TTimePicker_Create_0 = 0x0010F000;
  static final int amkUI_TTimePicker_getCurrentHour_1 = 0x0010F001;
  static final int amkUI_TTimePicker_setCurrentHour_2 = 0x0010F002;
  static final int amkUI_TTimePicker_getCurrentMinute_3 = 0x0010F003;
  static final int amkUI_TTimePicker_setCurrentMinute_4 = 0x0010F004;
  static final int amkUI_TTimePicker_is24HourView_5 = 0x0010F005;
  static final int amkUI_TTimePicker_setIs24HourView_6 = 0x0010F006;
  // ScrollView
  static final int amkUI_TScrollView_Create_0 = 0x00110000;
  // CompoundButton
  static final int amkUI_TCompoundButton_isChecked_0 = 0x00111000;
  static final int amkUI_TCompoundButton_performClick_1 = 0x00111001;
  static final int amkUI_TCompoundButton_setChecked_2 = 0x00111002;
  static final int amkUI_TCompoundButton_toggle_3 = 0x00111003;
  // CheckBox
  static final int amkUI_TCheckBox_Create_0 = 0x00112000;
  // AdapterView
  // AbsSpinner
  static final int amkUI_TAbsSpinner_getCount_0 = 0x00114000;
  static final int amkUI_TAbsSpinner_setAdapter_1 = 0x00114001;
  // Spinner
  static final int amkUI_TSpinner_Create_0 = 0x00115000;
  // Filterable
  // Adapter
  // ListAdapter
  // SpinnerAdapter
  // BaseAdapter
  // ArrayAdapter<String>
  static final int amkUI_TArrayAdapter_String__Create_0 = 0x0011B000;
  static final int amkUI_TArrayAdapter_String__add_1 = 0x0011B001;
  static final int amkUI_TArrayAdapter_String__clear_2 = 0x0011B002;
  static final int amkUI_TArrayAdapter_String__insert_3 = 0x0011B003;
  static final int amkUI_TArrayAdapter_String__remove_4 = 0x0011B004;
  // layout

  public boolean ProcessCommand(int Buffer)
  {
    //DebugOut("AndroidUI.ProcessCommand Command=" + java.lang.Integer.toHexString(Buffer));
    // basic types
    int lInt, lIndex, lPascalPointer;
    boolean lBool;
    float lFloat;
    // Self params
    View param_self_View;
    ViewGroup param_self_ViewGroup;
    TextView param_self_TextView;
    Button param_self_Button;
    EditText param_self_EditText;
    LinearLayout param_self_LinearLayout;
    TimePicker param_self_TimePicker;
    Display param_self_Display;
    DisplayMetrics param_self_DisplayMetrics;
    CompoundButton param_self_CompoundButton;
    WindowManager param_self_WindowManager;
    AbsSpinner param_self_AbsSpinner;
    ArrayAdapter<String> param_self_ArrayAdapter_String_;
    // Params
    ViewGroup.LayoutParams lViewGroup_LayoutParams_1, lViewGroup_LayoutParams_2, lViewGroup_LayoutParams_3;
    SpinnerAdapter lSpinnerAdapter_1;
    DisplayMetrics lDisplayMetrics_1;
    CharSequence lCharSequence_1;
    String lString_1;
    View lView_1;
    int lint_1, lint_2, lint_3, lint_4;
    float lfloat_1, lfloat_2;
    boolean lboolean_1;
    // Results
    float lResult_float;
    int lResult_int;
    boolean lResult_boolean;
    CharSequence lResult_CharSequence;
    Display lResult_Display;

    switch (Buffer)
    {

    case amkUI_TDisplayMetrics_Create_0:
      DebugOut("amkUI_TDisplayMetrics_Create_0");
      ViewElements.add(new DisplayMetrics());
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // field float density
    case amkUI_TDisplayMetrics_density_1:
      DebugOut("amkUI_TDisplayMetrics_density_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.density;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field int densityDpi
    case amkUI_TDisplayMetrics_densityDpi_2:
      DebugOut("amkUI_TDisplayMetrics_densityDpi_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.densityDpi;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field int heightPixels
    case amkUI_TDisplayMetrics_heightPixels_3:
      DebugOut("amkUI_TDisplayMetrics_heightPixels_3");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.heightPixels;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field float scaledDensity
    case amkUI_TDisplayMetrics_scaledDensity_4:
      DebugOut("amkUI_TDisplayMetrics_scaledDensity_4");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.scaledDensity;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field int widthPixels
    case amkUI_TDisplayMetrics_widthPixels_5:
      DebugOut("amkUI_TDisplayMetrics_widthPixels_5");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.widthPixels;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field float xdpi
    case amkUI_TDisplayMetrics_xdpi_6:
      DebugOut("amkUI_TDisplayMetrics_xdpi_6");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.xdpi;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field float ydpi
    case amkUI_TDisplayMetrics_ydpi_7:
      DebugOut("amkUI_TDisplayMetrics_ydpi_7");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.ydpi;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // method void setTitle(CharSequence title)
    case amkUI_TActivity_setTitle_0:
      DebugOut("amkUI_TActivity_setTitle_0");
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lCharSequence_1 = (CharSequence) MyJavaLang.LangElements.get(lInt);
      //
      activity.setTitle(lCharSequence_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method CharSequence getTitle()
    case amkUI_TActivity_getTitle_1:
      DebugOut("amkUI_TActivity_getTitle_1");
      // params
      //
      lResult_CharSequence = activity.getTitle();
      MyAndroidPipesComm.SendStringResult(lResult_CharSequence);
      break;
    // method void getMetrics(DisplayMetrics outMetrics)
    case amkUI_TDisplay_getMetrics_0:
      DebugOut("amkUI_TDisplay_getMetrics_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_Display = (Display) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lDisplayMetrics_1 = (DisplayMetrics) ViewElements.get(lInt);
      //
      param_self_Display.getMetrics(lDisplayMetrics_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method Display getDefaultDisplay()
    case amkUI_TWindowManager_getDefaultDisplay_0:
      DebugOut("amkUI_TWindowManager_getDefaultDisplay_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_WindowManager = (WindowManager) ViewElements.get(lInt);
      // params
      //
      lResult_Display = param_self_WindowManager.getDefaultDisplay();
      ViewElements.add(lResult_Display);
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TViewGroup_LayoutParams_Create_0:
      DebugOut("amkUI_TViewGroup_LayoutParams_Create_0");
      lint_1 = MyAndroidPipesComm.GetInt();
      lint_2 = MyAndroidPipesComm.GetInt();
      ViewElements.add(new ViewGroup.LayoutParams(lint_1, lint_2));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setLayoutParams(ViewGroup.LayoutParams params);
    case amkUI_TView_setLayoutParams_0:
      DebugOut("amkUI_TView_setLayoutParams_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_View = (View) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lViewGroup_LayoutParams_1 = (ViewGroup.LayoutParams) ViewElements.get(lInt);
      //
      param_self_View.setLayoutParams(lViewGroup_LayoutParams_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method void setVisibility(int visibility);
    case amkUI_TView_setVisibility_1:
      DebugOut("amkUI_TView_setVisibility_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_View = (View) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lint_1 = lInt;
      //
      param_self_View.setVisibility(lint_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method void addView(View child, int aindex, ViewGroup.LayoutParams params); overload;
    case amkUI_TViewGroup_addView_0:
      DebugOut("amkUI_TViewGroup_addView_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ViewGroup = (ViewGroup) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lView_1 = (View) ViewElements.get(lInt);
      lInt = MyAndroidPipesComm.GetInt();
      lint_2 = lInt;
      lInt = MyAndroidPipesComm.GetInt();
      lViewGroup_LayoutParams_3 = (ViewGroup.LayoutParams) ViewElements.get(lInt);
      //
      param_self_ViewGroup.addView(lView_1, lint_2, lViewGroup_LayoutParams_3);
      MyAndroidPipesComm.SendResult();
      break;
    // method void addView(View child, ViewGroup.LayoutParams params); overload;
    case amkUI_TViewGroup_addView_1:
      DebugOut("amkUI_TViewGroup_addView_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ViewGroup = (ViewGroup) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lView_1 = (View) ViewElements.get(lInt);
      lInt = MyAndroidPipesComm.GetInt();
      lViewGroup_LayoutParams_2 = (ViewGroup.LayoutParams) ViewElements.get(lInt);
      //
      param_self_ViewGroup.addView(lView_1, lViewGroup_LayoutParams_2);
      MyAndroidPipesComm.SendResult();
      break;
    // method void addView(View child, int aindex); overload;
    case amkUI_TViewGroup_addView_2:
      DebugOut("amkUI_TViewGroup_addView_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ViewGroup = (ViewGroup) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lView_1 = (View) ViewElements.get(lInt);
      lInt = MyAndroidPipesComm.GetInt();
      lint_2 = lInt;
      //
      param_self_ViewGroup.addView(lView_1, lint_2);
      MyAndroidPipesComm.SendResult();
      break;
    // method void addView(View child); overload;
    case amkUI_TViewGroup_addView_3:
      DebugOut("amkUI_TViewGroup_addView_3");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ViewGroup = (ViewGroup) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lView_1 = (View) ViewElements.get(lInt);
      //
      param_self_ViewGroup.addView(lView_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method void addView(View child, int width, int height); overload;
    case amkUI_TViewGroup_addView_4:
      DebugOut("amkUI_TViewGroup_addView_4");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ViewGroup = (ViewGroup) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lView_1 = (View) ViewElements.get(lInt);
      lInt = MyAndroidPipesComm.GetInt();
      lint_2 = lInt;
      lInt = MyAndroidPipesComm.GetInt();
      lint_3 = lInt;
      //
      param_self_ViewGroup.addView(lView_1, lint_2, lint_3);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TLinearLayout_Create_0:
      DebugOut("amkUI_TLinearLayout_Create_0");
      ViewElements.add(new LinearLayout(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setOrientation(int orientation);
    case amkUI_TLinearLayout_setOrientation_1:
      DebugOut("amkUI_TLinearLayout_setOrientation_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_LinearLayout = (LinearLayout) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lint_1 = lInt;
      //
      param_self_LinearLayout.setOrientation(lint_1);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TAbsoluteLayout_Create_0:
      DebugOut("amkUI_TAbsoluteLayout_Create_0");
      ViewElements.add(new AbsoluteLayout(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TAbsoluteLayout_LayoutParams_Create_0:
      DebugOut("amkUI_TAbsoluteLayout_LayoutParams_Create_0");
      lint_1 = MyAndroidPipesComm.GetInt();
      lint_2 = MyAndroidPipesComm.GetInt();
      lint_3 = MyAndroidPipesComm.GetInt();
      lint_4 = MyAndroidPipesComm.GetInt();
      ViewElements.add(new AbsoluteLayout.LayoutParams(lint_1, lint_2, lint_3, lint_4));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TTextView_Create_0:
      DebugOut("amkUI_TTextView_Create_0");
      ViewElements.add(new TextView(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); virtual;
    case amkUI_TTextView_setText_1:
      DebugOut("amkUI_TTextView_setText_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TextView = (TextView) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lCharSequence_1 = (CharSequence) MyJavaLang.LangElements.get(lInt);
      //
      param_self_TextView.setText(lCharSequence_1);
      MyAndroidPipesComm.SendResult();
      break;
    // callbacksettercaller setOnClickListener callOnClickListener OnClickListener = procedure (v: TView) of object;
    case amkUI_TTextView_setOnClickListener_2:
      DebugOut("amkUI_TTextView_setOnClickListener_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TextView = (TextView) ViewElements.get(lInt);
      lPascalPointer = MyAndroidPipesComm.GetInt();
      param_self_TextView.setTag(Integer.valueOf(lPascalPointer));

      // Run the code
      param_self_TextView.setOnClickListener(
      new View.OnClickListener()
      {
        public void onClick(View v)
        {
          // Perform action
          DebugOut("START TextView OnClickListener");
          MyAndroidPipesComm.SendMessage(AndroidPipesComm.amkUICommand, amkUI_TTextView_OnClickListener_Start_3);
          Integer lTag = (Integer) v.getTag();
          MyAndroidPipesComm.SendInt(lTag.intValue());
          MyAndroidPipesComm.WaitForPascalMessage(AndroidPipesComm.amkUICommand, amkUI_TTextView_OnClickListener_Finished_4);
          DebugOut("END TextView OnClickListener");
        }
      });
      MyAndroidPipesComm.SendResult();
      break;
    // method void setTextSize(int unit_; float size);
    case amkUI_TTextView_setTextSize_5:
      DebugOut("amkUI_TTextView_setTextSize_5");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TextView = (TextView) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lint_1 = lInt;
      lFloat = MyAndroidPipesComm.GetFloat();
      lfloat_2 = lFloat;
      //
      param_self_TextView.setTextSize(lint_1, lfloat_2);
      MyAndroidPipesComm.SendResult();
      break;
    // method CharSequence getText()
    case amkUI_TTextView_getText_6:
      DebugOut("amkUI_TTextView_getText_6");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TextView = (TextView) ViewElements.get(lInt);
      // params
      //
      lResult_CharSequence = param_self_TextView.getText();
      MyAndroidPipesComm.SendStringResult(lResult_CharSequence);
      break;
    case amkUI_TEditText_Create_0:
      DebugOut("amkUI_TEditText_Create_0");
      ViewElements.add(new EditText(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); override;
    case amkUI_TEditText_setText_1:
      DebugOut("amkUI_TEditText_setText_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_EditText = (EditText) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lCharSequence_1 = (CharSequence) MyJavaLang.LangElements.get(lInt);
      //
      param_self_EditText.setText(lCharSequence_1);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TButton_Create_0:
      DebugOut("amkUI_TButton_Create_0");
      ViewElements.add(new Button(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); override;
    case amkUI_TButton_setText_1:
      DebugOut("amkUI_TButton_setText_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_Button = (Button) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lCharSequence_1 = (CharSequence) MyJavaLang.LangElements.get(lInt);
      //
      param_self_Button.setText(lCharSequence_1);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TTimePicker_Create_0:
      DebugOut("amkUI_TTimePicker_Create_0");
      ViewElements.add(new TimePicker(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method int getCurrentHour();
    case amkUI_TTimePicker_getCurrentHour_1:
      DebugOut("amkUI_TTimePicker_getCurrentHour_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_TimePicker.getCurrentHour();
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // method void setCurrentHour(int currentHour);
    case amkUI_TTimePicker_setCurrentHour_2:
      DebugOut("amkUI_TTimePicker_setCurrentHour_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lint_1 = lInt;
      //
      param_self_TimePicker.setCurrentHour(lint_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method int getCurrentMinute;
    case amkUI_TTimePicker_getCurrentMinute_3:
      DebugOut("amkUI_TTimePicker_getCurrentMinute_3");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_TimePicker.getCurrentMinute();
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // method void setCurrentMinute(int currentMinute);
    case amkUI_TTimePicker_setCurrentMinute_4:
      DebugOut("amkUI_TTimePicker_setCurrentMinute_4");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lint_1 = lInt;
      //
      param_self_TimePicker.setCurrentMinute(lint_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method boolean is24HourView;
    case amkUI_TTimePicker_is24HourView_5:
      DebugOut("amkUI_TTimePicker_is24HourView_5");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_TimePicker.is24HourView();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method void setIs24HourView(boolean AIs24HourView);
    case amkUI_TTimePicker_setIs24HourView_6:
      DebugOut("amkUI_TTimePicker_setIs24HourView_6");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      lBool = MyAndroidPipesComm.GetBool();
      lboolean_1 = lBool;
      //
      param_self_TimePicker.setIs24HourView(lboolean_1);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TScrollView_Create_0:
      DebugOut("amkUI_TScrollView_Create_0");
      ViewElements.add(new ScrollView(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method boolean 	isChecked() 
    case amkUI_TCompoundButton_isChecked_0:
      DebugOut("amkUI_TCompoundButton_isChecked_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_CompoundButton.isChecked();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method boolean 	performClick() 
    case amkUI_TCompoundButton_performClick_1:
      DebugOut("amkUI_TCompoundButton_performClick_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_CompoundButton.performClick();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method void 	setChecked(boolean checked) 
    case amkUI_TCompoundButton_setChecked_2:
      DebugOut("amkUI_TCompoundButton_setChecked_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      lBool = MyAndroidPipesComm.GetBool();
      lboolean_1 = lBool;
      //
      param_self_CompoundButton.setChecked(lboolean_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method void 	toggle() 
    case amkUI_TCompoundButton_toggle_3:
      DebugOut("amkUI_TCompoundButton_toggle_3");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      param_self_CompoundButton.toggle();
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TCheckBox_Create_0:
      DebugOut("amkUI_TCheckBox_Create_0");
      ViewElements.add(new CheckBox(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method int getCount()
    case amkUI_TAbsSpinner_getCount_0:
      DebugOut("amkUI_TAbsSpinner_getCount_0");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_AbsSpinner = (AbsSpinner) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_AbsSpinner.getCount();
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // method void setAdapter(SpinnerAdapter adapter)
    case amkUI_TAbsSpinner_setAdapter_1:
      DebugOut("amkUI_TAbsSpinner_setAdapter_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_AbsSpinner = (AbsSpinner) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lSpinnerAdapter_1 = (SpinnerAdapter) ViewElements.get(lInt);
      //
      param_self_AbsSpinner.setAdapter(lSpinnerAdapter_1);
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TSpinner_Create_0:
      DebugOut("amkUI_TSpinner_Create_0");
      ViewElements.add(new Spinner(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TArrayAdapter_String__Create_0:
      DebugOut("amkUI_TArrayAdapter_String__Create_0");
      lint_1 = MyAndroidPipesComm.GetInt();
      ViewElements.add(new ArrayAdapter<String>(activity, lint_1));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void add(String aobject)
    case amkUI_TArrayAdapter_String__add_1:
      DebugOut("amkUI_TArrayAdapter_String__add_1");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ArrayAdapter_String_ = (ArrayAdapter<String>) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lString_1 = (String) MyJavaLang.LangElements.get(lInt);
      //
      param_self_ArrayAdapter_String_.add(lString_1);
      MyAndroidPipesComm.SendResult();
      break;
    // method void clear()
    case amkUI_TArrayAdapter_String__clear_2:
      DebugOut("amkUI_TArrayAdapter_String__clear_2");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ArrayAdapter_String_ = (ArrayAdapter<String>) ViewElements.get(lInt);
      // params
      //
      param_self_ArrayAdapter_String_.clear();
      MyAndroidPipesComm.SendResult();
      break;
    // method void insert(String aobject, int aindex)
    case amkUI_TArrayAdapter_String__insert_3:
      DebugOut("amkUI_TArrayAdapter_String__insert_3");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ArrayAdapter_String_ = (ArrayAdapter<String>) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lString_1 = (String) MyJavaLang.LangElements.get(lInt);
      lInt = MyAndroidPipesComm.GetInt();
      lint_2 = lInt;
      //
      param_self_ArrayAdapter_String_.insert(lString_1, lint_2);
      MyAndroidPipesComm.SendResult();
      break;
    // method void remove(String aobject)
    case amkUI_TArrayAdapter_String__remove_4:
      DebugOut("amkUI_TArrayAdapter_String__remove_4");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_ArrayAdapter_String_ = (ArrayAdapter<String>) ViewElements.get(lInt);
      // params
      lInt = MyAndroidPipesComm.GetInt();
      lString_1 = (String) MyJavaLang.LangElements.get(lInt);
      //
      param_self_ArrayAdapter_String_.remove(lString_1);
      MyAndroidPipesComm.SendResult();
      break;

    default:
      return false;
    }
    return true;
  }
}
