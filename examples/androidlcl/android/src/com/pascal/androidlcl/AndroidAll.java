package com.pascal.androidlcl;

import android.app.*;
import android.view.*;
import android.os.*;
import android.util.*;
import android.content.*;
import android.view.*;
import android.widget.*;
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
  static final int amkUI_TDisplayMetrics_Create = 0x00101000;
  static final int amkUI_TDisplayMetrics_density = 0x00101001;
  static final int amkUI_TDisplayMetrics_densityDpi = 0x00101002;
  static final int amkUI_TDisplayMetrics_heightPixels = 0x00101003;
  static final int amkUI_TDisplayMetrics_scaledDensity = 0x00101004;
  static final int amkUI_TDisplayMetrics_widthPixels = 0x00101005;
  static final int amkUI_TDisplayMetrics_xdpi = 0x00101006;
  static final int amkUI_TDisplayMetrics_ydpi = 0x00101007;
  // Display
  static final int amkUI_TDisplay_getMetrics = 0x00102000;
  // WindowManager
  static final int amkUI_TWindowManager_getDefaultDisplay = 0x00103000;
  // ViewGroup.LayoutParams
  static final int amkUI_TViewGroup_LayoutParams_Create = 0x00104000;
  // View
  static final int amkUI_TView_setLayoutParams = 0x00105000;
  static final int amkUI_TView_setVisibility = 0x00105001;
  // ViewGroup
  static final int amkUI_TViewGroup_addView = 0x00106000;
  // LinearLayout
  static final int amkUI_TLinearLayout_Create = 0x00107000;
  static final int amkUI_TLinearLayout_setOrientation = 0x00107001;
  // AbsoluteLayout
  static final int amkUI_TAbsoluteLayout_Create = 0x00108000;
  // AbsoluteLayout.LayoutParams
  static final int amkUI_TAbsoluteLayout_LayoutParams_Create = 0x00109000;
  // TextView
  static final int amkUI_TTextView_Create = 0x0010A000;
  static final int amkUI_TTextView_setText = 0x0010A001;
  static final int amkUI_TTextView_setOnClickListener = 0x0010A002;
  static final int amkUI_TTextView_OnClickListener_Start = 0x0010A002;
  static final int amkUI_TTextView_OnClickListener_Finished = 0x0010A002;
  static final int amkUI_TTextView_setTextSize = 0x0010A002;
  // EditText
  static final int amkUI_TEditText_Create = 0x0010B000;
  static final int amkUI_TEditText_setText = 0x0010B001;
  // Button
  static final int amkUI_TButton_Create = 0x0010C000;
  static final int amkUI_TButton_setText = 0x0010C001;
  // FrameLayout
  // TimePicker
  static final int amkUI_TTimePicker_Create = 0x0010E000;
  static final int amkUI_TTimePicker_getCurrentHour = 0x0010E001;
  static final int amkUI_TTimePicker_setCurrentHour = 0x0010E002;
  static final int amkUI_TTimePicker_getCurrentMinute = 0x0010E003;
  static final int amkUI_TTimePicker_setCurrentMinute = 0x0010E004;
  static final int amkUI_TTimePicker_is24HourView = 0x0010E005;
  static final int amkUI_TTimePicker_setIs24HourView = 0x0010E006;
  // ScrollView
  static final int amkUI_TScrollView_Create = 0x0010F000;
  // CompoundButton
  static final int amkUI_TCompoundButton_isChecked = 0x00110000;
  static final int amkUI_TCompoundButton_performClick = 0x00110001;
  static final int amkUI_TCompoundButton_setChecked = 0x00110002;
  static final int amkUI_TCompoundButton_toggle = 0x00110003;
  // CheckBox
  static final int amkUI_TCheckBox_Create = 0x00111000;

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
    // Params
    ViewGroup.LayoutParams lViewGroup_LayoutParams_1, lViewGroup_LayoutParams_2;
    DisplayMetrics lDisplayMetrics_1;
    CharSequence lCharSequence_1;
    View lView_1;
    int lint_1, lint_2, lint_3, lint_4;
    float lfloat_1, lfloat_2;
    boolean lboolean_1;
    // Results
    float lResult_float;
    int lResult_int;
    boolean lResult_boolean;
    Display lResult_Display;

    switch (Buffer)
    {

    case amkUI_TDisplayMetrics_Create:
      DebugOut("amkUI_TDisplayMetrics_Create");
      ViewElements.add(new DisplayMetrics());
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // field float density
    case amkUI_TDisplayMetrics_density:
      DebugOut("amkUI_TDisplayMetrics_density");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.density;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field int densityDpi
    case amkUI_TDisplayMetrics_densityDpi:
      DebugOut("amkUI_TDisplayMetrics_densityDpi");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.densityDpi;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field int heightPixels
    case amkUI_TDisplayMetrics_heightPixels:
      DebugOut("amkUI_TDisplayMetrics_heightPixels");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.heightPixels;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field float scaledDensity
    case amkUI_TDisplayMetrics_scaledDensity:
      DebugOut("amkUI_TDisplayMetrics_scaledDensity");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.scaledDensity;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field int widthPixels
    case amkUI_TDisplayMetrics_widthPixels:
      DebugOut("amkUI_TDisplayMetrics_widthPixels");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_DisplayMetrics.widthPixels;
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // field float xdpi
    case amkUI_TDisplayMetrics_xdpi:
      DebugOut("amkUI_TDisplayMetrics_xdpi");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.xdpi;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // field float ydpi
    case amkUI_TDisplayMetrics_ydpi:
      DebugOut("amkUI_TDisplayMetrics_ydpi");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_DisplayMetrics = (DisplayMetrics) ViewElements.get(lInt);
      // params
      //
      lResult_float = param_self_DisplayMetrics.ydpi;
      MyAndroidPipesComm.SendFloatResult(lResult_float);
      break;
    // method void getMetrics(DisplayMetrics outMetrics)
    case amkUI_TDisplay_getMetrics:
      DebugOut("amkUI_TDisplay_getMetrics");
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
    case amkUI_TWindowManager_getDefaultDisplay:
      DebugOut("amkUI_TWindowManager_getDefaultDisplay");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_WindowManager = (WindowManager) ViewElements.get(lInt);
      // params
      //
      lResult_Display = param_self_WindowManager.getDefaultDisplay();
      ViewElements.add(lResult_Display);
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TViewGroup_LayoutParams_Create:
      DebugOut("amkUI_TViewGroup_LayoutParams_Create");
      lint_1 = MyAndroidPipesComm.GetInt();
      lint_2 = MyAndroidPipesComm.GetInt();
      ViewElements.add(new ViewGroup.LayoutParams(lint_1, lint_2));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setLayoutParams(ViewGroup.LayoutParams params);
    case amkUI_TView_setLayoutParams:
      DebugOut("amkUI_TView_setLayoutParams");
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
    case amkUI_TView_setVisibility:
      DebugOut("amkUI_TView_setVisibility");
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
    // method void addView(View child, ViewGroup.LayoutParams params);
    case amkUI_TViewGroup_addView:
      DebugOut("amkUI_TViewGroup_addView");
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
    case amkUI_TLinearLayout_Create:
      DebugOut("amkUI_TLinearLayout_Create");
      ViewElements.add(new LinearLayout(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setOrientation(int orientation);
    case amkUI_TLinearLayout_setOrientation:
      DebugOut("amkUI_TLinearLayout_setOrientation");
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
    case amkUI_TAbsoluteLayout_Create:
      DebugOut("amkUI_TAbsoluteLayout_Create");
      ViewElements.add(new AbsoluteLayout(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TAbsoluteLayout_LayoutParams_Create:
      DebugOut("amkUI_TAbsoluteLayout_LayoutParams_Create");
      lint_1 = MyAndroidPipesComm.GetInt();
      lint_2 = MyAndroidPipesComm.GetInt();
      lint_3 = MyAndroidPipesComm.GetInt();
      lint_4 = MyAndroidPipesComm.GetInt();
      ViewElements.add(new AbsoluteLayout.LayoutParams(lint_1, lint_2, lint_3, lint_4));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    case amkUI_TTextView_Create:
      DebugOut("amkUI_TTextView_Create");
      ViewElements.add(new TextView(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); virtual;
    case amkUI_TTextView_setText:
      DebugOut("amkUI_TTextView_setText");
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
    // method void setTextSize(int unit_; float size);
    case amkUI_TTextView_setTextSize:
      DebugOut("amkUI_TTextView_setTextSize");
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
    case amkUI_TEditText_Create:
      DebugOut("amkUI_TEditText_Create");
      ViewElements.add(new EditText(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); override;
    case amkUI_TEditText_setText:
      DebugOut("amkUI_TEditText_setText");
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
    case amkUI_TButton_Create:
      DebugOut("amkUI_TButton_Create");
      ViewElements.add(new Button(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method void setText(CharSequence AText); override;
    case amkUI_TButton_setText:
      DebugOut("amkUI_TButton_setText");
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
    case amkUI_TTimePicker_Create:
      DebugOut("amkUI_TTimePicker_Create");
      ViewElements.add(new TimePicker(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method int getCurrentHour();
    case amkUI_TTimePicker_getCurrentHour:
      DebugOut("amkUI_TTimePicker_getCurrentHour");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_TimePicker.getCurrentHour();
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // method void setCurrentHour(int currentHour);
    case amkUI_TTimePicker_setCurrentHour:
      DebugOut("amkUI_TTimePicker_setCurrentHour");
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
    case amkUI_TTimePicker_getCurrentMinute:
      DebugOut("amkUI_TTimePicker_getCurrentMinute");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_int = param_self_TimePicker.getCurrentMinute();
      MyAndroidPipesComm.SendIntResult(lResult_int);
      break;
    // method void setCurrentMinute(int currentMinute);
    case amkUI_TTimePicker_setCurrentMinute:
      DebugOut("amkUI_TTimePicker_setCurrentMinute");
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
    case amkUI_TTimePicker_is24HourView:
      DebugOut("amkUI_TTimePicker_is24HourView");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_TimePicker = (TimePicker) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_TimePicker.is24HourView();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method void setIs24HourView(boolean AIs24HourView);
    case amkUI_TTimePicker_setIs24HourView:
      DebugOut("amkUI_TTimePicker_setIs24HourView");
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
    case amkUI_TScrollView_Create:
      DebugOut("amkUI_TScrollView_Create");
      ViewElements.add(new ScrollView(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;
    // method boolean 	isChecked() 
    case amkUI_TCompoundButton_isChecked:
      DebugOut("amkUI_TCompoundButton_isChecked");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_CompoundButton.isChecked();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method boolean 	performClick() 
    case amkUI_TCompoundButton_performClick:
      DebugOut("amkUI_TCompoundButton_performClick");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      lResult_boolean = param_self_CompoundButton.performClick();
      MyAndroidPipesComm.SendBoolResult(lResult_boolean);
      break;
    // method void 	setChecked(boolean checked) 
    case amkUI_TCompoundButton_setChecked:
      DebugOut("amkUI_TCompoundButton_setChecked");
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
    case amkUI_TCompoundButton_toggle:
      DebugOut("amkUI_TCompoundButton_toggle");
      // Self
      lInt = MyAndroidPipesComm.GetInt();
      param_self_CompoundButton = (CompoundButton) ViewElements.get(lInt);
      // params
      //
      param_self_CompoundButton.toggle();
      MyAndroidPipesComm.SendResult();
      break;
    case amkUI_TCheckBox_Create:
      DebugOut("amkUI_TCheckBox_Create");
      ViewElements.add(new CheckBox(activity));
      MyAndroidPipesComm.SendIntResult(ViewElements.size() - 1);
      break;

    default:
      return false;
    }
    return true;
  }
}
