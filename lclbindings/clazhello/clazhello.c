/*
clazhello.c

Simple demonstration program that uses the LCL Exports library to build a GUI

LICENSE: Public domain
         
AUTHOR: Felipe Monteiro de Carvalho
*/

#include "vcl.h"

TFormH MyForm;
TButtonH MyButton;
TLabelH MyLabel;

/* Callback for the close button */
FASTCALL_TNOTIFYEVENT_START(MyButton_OnClick) /* void* Self, TObjectH Sender */

    TCustomForm_Close(MyForm);
}

/* Application entry-point */
APPBEGIN()
{
    Application_Initialize();

    Application_CreateForm(&MyForm);

    TControl_SetTop(MyForm, 50);
    TControl_SetLeft(MyForm, 50);
    TControl_SetHeight(MyForm, 200);
    TControl_SetWidth(MyForm, 200);
    TControl_SetCaption(MyForm, "C Lazarus app");

    MyButton = TButton_Create(MyForm);
    TWinControl_SetParent(MyButton, MyForm);

    TControl_SetTop(MyButton, 150);
    TControl_SetLeft(MyButton, 50);
    TControl_SetWidth(MyButton, 100);

    TControl_SetOnClick(MyButton, &MyButton_OnClick);
    TControl_SetCaption(MyButton, "Close Button");

    MyLabel = TLabel_Create(MyForm);
    TWinControl_SetParent(MyLabel, MyForm);

    TControl_SetTop(MyLabel, 50);
    TControl_SetLeft(MyLabel, 60);
    TControl_SetCaption(MyLabel, "Toolbox is cool!!!");

    /* Enter main loop */
    Application_Run();

    return 0;
}
