::this script takes a screenshot from the widget_test example
::and cut's the individual widget examples and saves them to 
::a separate file
::the crop application is located in ./utils

..\..\utils\crop widget_test.png 4 29 152 81 label.png
..\..\utils\crop widget_test.png 7 79 182 111 button.png
..\..\utils\crop widget_test.png 7 118 406 180 checkbutton.bmp
..\..\utils\crop widget_test.png 7 190 394 251 radiobutton.bmp
..\..\utils\crop widget_test.png 7 253 117 279 horizontal_slider.png
..\..\utils\crop widget_test.png 7 279 120 350 listbox.png
..\..\utils\crop widget_test.png 116 279 255 348 combobox.png
..\..\utils\crop widget_test.png 7 353 237 385 line_edit.png
..\..\utils\crop widget_test.png 7 383 264 460 panel.png
..\..\utils\crop widget_test.png 7 468 262 539 frame.png
..\..\utils\crop widget_test.png 4 537 77 620 listitem.png
..\..\utils\crop widget_test.png 4 615 114 724 textureview.bmp

del checkbutton.png
del radiobutton.png
del textureview.png

ren checkbutton.bmp checkbutton.png
ren radiobutton.bmp radiobutton.png
ren textureview.bmp textureview.png
