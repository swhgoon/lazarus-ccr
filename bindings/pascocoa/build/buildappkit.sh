cd ..
cd parser
DEFAULT_INI="../parser/default.ini"
FRAMEWORK="/System/Library/Frameworks/AppKit.framework/Headers"
./objcparser -ini=$DEFAULT_INI -ini=appkit.ini $FRAMEWORK/NSButton.h > ../appkit/NSButton.inc
./objcparser -ini=$DEFAULT_INI -ini=appkit.ini $FRAMEWORK/NSWindow.h > ../appkit/NSWindow.inc
./objcparser -ini=$DEFAULT_INI -ini=appkit.ini $FRAMEWORK/NSGraphics.h > ../appkit/NSGraphics.inc
