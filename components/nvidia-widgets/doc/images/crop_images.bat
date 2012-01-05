::this script takes a screenshot from the widget_test example
::and cut's the individual widget examples and saves them to 
::a separate file
::the crop application is located in ./utils

crop widget_test.png 7 32 152 79 label.png
crop widget_test.png 7 79 182 109 button.png
crop widget_test.png 7 121 406 180 checkbutton.bmp
crop widget_test.png 7 190 394 251 radiobutton.bmp
crop widget_test.png 7 249 117 279 horizontal_slider.png
crop widget_test.png 7 279 118 348 listbox.png
crop widget_test.png 116 279 255 348 combobox.png
crop widget_test.png 7 348 235 383 line_edit.png
crop widget_test.png 7 383 262 458 panel.png
crop widget_test.png 7 458 262 537 frame.png
crop widget_test.png 4 537 75 618 listitem.png
crop widget_test.png 4 615 114 724 textureview.bmp

del checkbutton.bmp
del radiobutton.bmp
del textureview.bmp

ren *.bmp *.png
