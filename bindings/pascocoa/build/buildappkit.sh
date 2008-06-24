cd ..
cd parser
DEFAULT_INI="default.ini"
APPKIT_INI="../build/appkit.ini"
FRAMEWORK="/System/Library/Frameworks/AppKit.framework/Headers"
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSApplication.h > ../appkit/NSApplication.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSBitmapImageRep.h > ../appkit/NSBitmapImageRep.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSButton.h > ../appkit/NSButton.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSControl.h > ../appkit/NSControl.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSGraphics.h > ../appkit/NSGraphics.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSGraphicsContext.h > ../appkit/NSGraphicsContext.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSImage.h > ../appkit/NSImage.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSImageRep.h > ../appkit/NSImageRep.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSMenu.h > ../appkit/NSMenu.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSMenuItem.h > ../appkit/NSMenuItem.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSSpellProtocol.h > ../appkit/NSSpellProtocol.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSStatusBar.h > ../appkit/NSStatusBar.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSStatusItem.h > ../appkit/NSStatusItem.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSText.h > ../appkit/NSText.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSUserInterfaceValidation.h > ../appkit/NSUserInterfaceValidation.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSWindow.h > ../appkit/NSWindow.inc
