==General==
This is the FPC port of the nvidia-widget set (http://code.google.com/p/nvidia-widgets/). This widgetset is lightwight and extremely usefull for projects that do not want to add a dependency to some large widget set e.g. GTK2 or QT.

Originally ported by Darius Blaszyk in June 2011. The code works out of the box with FPC and does not need any 3rd party utilities.


==License==
The original code was released under the MIT license. This code, as it's derived work, will therefore also be released under MIT license. As far as I understand GPL easily mixes with MIT, so if your project is GPL you can use it as you would with a GPL library.


==Widgets==
This widgetset comes with a layoutmanager. This means that you don't need to specify screen coordinates when you define the widgets, but you specify how you want to layout them and then just add the widgets as you like. You can embed multiple layout panels in each other to build complex layouts.

The most important widgets are implemented, but more are allways welcome;

- Label
- Button
- Checkbutton
- Radiobutton
- Horizontal slider
- Listbox
- Combobox
- Line edit
- Panel
- Textureview


==Context and backend==
This widgetset is very modular, which is expressed by the fact that both the context as the backend are separated into it's own classes. Currently the GLut context is implemented and for the backend OpenGL obviously. Adding a new context or backend is very easy. All you need to do is implement the UIContext or UIPainter classes.


==About IMGUI==
This widgetset is and immediate mode graphical user interface (IMGUI). This means that this widgetset does not retain widget information outside the paint loop. The advantage of this is that it is very lightweight and easily expandible. 


==More information==

IMGUI : https://mollyrocket.com/forums/viewforum.php?f=10


If you have a patch or want more information you can contact me via email: dhkblaszyk@zeelandnet.nl