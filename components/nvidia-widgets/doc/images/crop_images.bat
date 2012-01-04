::this script takes a screenshot from the widget_test example
::and cut's the individual widget examples and saves them to 
::a separate file
::the crop application is located in ./utils

crop widget_test.png 7 30 150 70 label.png
crop widget_test.png 7 70 170 100 button.png
crop widget_test.png 7 100 385 162 checkbutton.bmp
crop widget_test.png 7 162 375 222 radiobutton.bmp
crop widget_test.png 7 222 120 252 horizontal_slider.png
crop widget_test.png 7 252 108 309 listbox.png
crop widget_test.png 104 252 233 309 combobox.png
crop widget_test.png 7 309 235 340 line_edit.png
crop widget_test.png 7 340 242 407 panel.png
crop widget_test.png 7 414 242 480 frame.png
crop widget_test.png 4 478 74 542 listitem.png
crop widget_test.png 4 540 112 649 textureview.bmp