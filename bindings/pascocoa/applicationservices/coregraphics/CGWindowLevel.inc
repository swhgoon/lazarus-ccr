{%MainUnit CoreGraphics.pas}
{
   CGWindowLevel.h
   CoreGraphics

   Copyright (c) 2000 Apple Computer, Inc. All rights reserved.

}

//#include <CoreGraphics/CGBase.h>
//#include <stdint.h>

//CG_EXTERN_C_BEGIN

{
 * Windows may be assigned to a particular level. When assigned to a level,
 * the window is ordered relative to all other windows in that level.
 * Windows with a higher level are sorted in front of windows with a lower
 * level.
 *
 * A common set of window levels is defined here for use within higher
 * level frameworks.  The levels are accessed via a key and function,
 * so that levels may be changed or adjusted in future releases without
 * breaking binary compatability.
 }

type
  CGWindowLevel = LongInt;
  CGWindowLevelKey = LongInt;

  CGCommonWindowLevelKey = (
    kCGBaseWindowLevelKey		= 0,
    kCGMinimumWindowLevelKey,
    kCGDesktopWindowLevelKey,
    kCGBackstopMenuLevelKey,
    kCGNormalWindowLevelKey,
    kCGFloatingWindowLevelKey,
    kCGTornOffMenuWindowLevelKey,
    kCGDockWindowLevelKey,
    kCGMainMenuWindowLevelKey,
    kCGStatusWindowLevelKey,
    kCGModalPanelWindowLevelKey,
    kCGPopUpMenuWindowLevelKey,
    kCGDraggingWindowLevelKey,
    kCGScreenSaverWindowLevelKey,
    kCGMaximumWindowLevelKey,
    kCGOverlayWindowLevelKey,
    kCGHelpWindowLevelKey,
    kCGUtilityWindowLevelKey,
    kCGDesktopIconWindowLevelKey,
    kCGCursorWindowLevelKey,
    kCGAssistiveTechHighWindowLevelKey,
    kCGNumberOfWindowLevelKeys	{ Internal bookkeeping; must be last }
  );

//CGWindowLevel CGWindowLevelForKey( CGWindowLevelKey key );

{ number of levels above kCGMaximumWindowLevel reserved for internal use }
const
  kCGNumReservedWindowLevels =	(16);

{ Definitions of older constant values as calls }
  kCGBaseWindowLevel		= Low(LongInt);	{ LONG_MIN }
  kCGMinimumWindowLevel 	= kCGBaseWindowLevel + 1; //CGWindowLevelForKey(kCGMinimumWindowLevelKey)	/* (kCGBaseWindowLevel + 1) */
  kCGDesktopWindowLevel		= kCGBaseWindowLevel; //CGWindowLevelForKey(kCGDesktopWindowLevelKey)	/* kCGMinimumWindowLevel */
  kCGDesktopIconWindowLevel	= kCGBaseWindowLevel + 20; //CGWindowLevelForKey(kCGDesktopIconWindowLevelKey)	/* kCGMinimumWindowLevel + 20 */
  kCGBackstopMenuLevel		= -20; //CGWindowLevelForKey(kCGBackstopMenuLevelKey)	/* -20 */
  kCGNormalWindowLevel		= 0; //CGWindowLevelForKey(kCGNormalWindowLevelKey)	/* 0 */
  kCGFloatingWindowLevel	= 3; //CGWindowLevelForKey(kCGFloatingWindowLevelKey)	/* 3 */
  kCGTornOffMenuWindowLevel	= 3; //CGWindowLevelForKey(kCGTornOffMenuWindowLevelKey)	/* 3 */
  kCGDockWindowLevel		= 20; //CGWindowLevelForKey(kCGDockWindowLevelKey)	/* 20 */
  kCGMainMenuWindowLevel	= 24; //CGWindowLevelForKey(kCGMainMenuWindowLevelKey)	/* 24 */
  kCGStatusWindowLevel		= 25; //CGWindowLevelForKey(kCGStatusWindowLevelKey)	/* 25 */
  kCGModalPanelWindowLevel	= 8; //CGWindowLevelForKey(kCGModalPanelWindowLevelKey)	/* 8 */
  kCGPopUpMenuWindowLevel	= 101; //CGWindowLevelForKey(kCGPopUpMenuWindowLevelKey)	{ 101 }
  kCGDraggingWindowLevel	= 500; //CGWindowLevelForKey(kCGDraggingWindowLevelKey)	{ 500 }
  kCGScreenSaverWindowLevel	= 1000; //CGWindowLevelForKey(kCGScreenSaverWindowLevelKey)	{ 1000 }
  kCGCursorWindowLevel		= 2000; //CGWindowLevelForKey(kCGCursorWindowLevelKey)	{ 2000 }
  kCGOverlayWindowLevel		= 102; //CGWindowLevelForKey(kCGOverlayWindowLevelKey)	{ 102 }
  kCGHelpWindowLevel		= 102; //CGWindowLevelForKey(kCGHelpWindowLevelKey)	{ 102 }
  kCGUtilityWindowLevel		= 19; //CGWindowLevelForKey(kCGUtilityWindowLevelKey)	{ 19 }

  kCGAssistiveTechHighWindowLevel	= 1500; //CGWindowLevelForKey(kCGAssistiveTechHighWindowLevelKey)	{ 1500 }

  kCGMaximumWindowLevel 		= High(LongInt) -kCGNumReservedWindowLevels;	{ LONG_MAX - kCGNumReservedWindowLevels }


//CG_EXTERN_C_END

