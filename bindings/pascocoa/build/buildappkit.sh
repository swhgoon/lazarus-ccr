cd ..
cd parser
DEFAULT_INI="default.ini"
APPKIT_INI="../build/appkit.ini"
FRAMEWORK="/System/Library/Frameworks/AppKit.framework/Headers"
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSButton.h > ../appkit/NSButton.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSWindow.h > ../appkit/NSWindow.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSGraphics.h > ../appkit/NSGraphics.inc
