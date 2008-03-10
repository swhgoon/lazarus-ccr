{%mainunit appkit.pas}
{
	NSImage.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSIMAGE_PAS_H}
{$define NSIMAGE_PAS_H}

{#import <AppKit/NSGraphics.h>
#import <AppKit/NSBitmapImageRep.h>}

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2

type
  NSImageLoadStatus = (
    NSImageLoadStatusCompleted,
    NSImageLoadStatusCancelled,
    NSImageLoadStatusInvalidData,
    NSImageLoadStatusUnexpectedEOF,
    NSImageLoadStatusReadError
  );

  NSImageCacheMode = (
    NSImageCacheDefault,    // unspecified. use image rep's default
    NSImageCacheAlways,     // always generate a cache when drawing
    NSImageCacheBySize,     // cache if cache size is smaller than original data
    NSImageCacheNever       // never cache, always draw direct
  );

//#endif

{ Class and method name strings }
const
  Str_NSImage = 'NSImage';
  
  Str_initWithContentsOfFile = 'initWithContentsOfFile:';

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSIMAGE_PAS_C}
{$define NSIMAGE_PAS_C}

{#import <AppKit/NSGraphics.h>
#import <AppKit/NSBitmapImageRep.h>}

//@class NSArray, NSColor, NSImageRep, NSPasteboard, NSURL;

  NSImage = class(NSObject) // <NSCopying, NSCoding>
  public
    { Extra binding functions }
    function getClass: objc.id; override;
  public
{
+ (id)imageNamed:(NSString *)name;	/* If this finds & creates the image, only name is saved when archived */

- (id)initWithSize:(NSSize)aSize;
- (id)initWithData:(NSData *)data;			/* When archived, saves contents */}
    constructor initWithContentsOfFile(fileName: CFStringRef);	{ When archived, saves contents }
{- (id)initWithContentsOfURL:(NSURL *)url;               /* When archived, saves contents */
- (id)initByReferencingFile:(NSString *)fileName;	/* When archived, saves fileName */
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
- (id)initByReferencingURL:(NSURL *)url;		/* When archived, saves url, supports progressive loading */
#endif
- (id)initWithPasteboard:(NSPasteboard *)pasteboard;

- (void)setSize:(NSSize)aSize;
- (NSSize)size;
- (BOOL)setName:(NSString *)string;
- (NSString *)name;
- (void)setScalesWhenResized:(BOOL)flag;
- (BOOL)scalesWhenResized;
- (void)setDataRetained:(BOOL)flag;
- (BOOL)isDataRetained;
- (void)setCachedSeparately:(BOOL)flag;
- (BOOL)isCachedSeparately;
- (void)setCacheDepthMatchesImageDepth:(BOOL)flag;
- (BOOL)cacheDepthMatchesImageDepth;
- (void)setBackgroundColor:(NSColor *)aColor;
- (NSColor *)backgroundColor;
- (void)setUsesEPSOnResolutionMismatch:(BOOL)flag;
- (BOOL)usesEPSOnResolutionMismatch;
- (void)setPrefersColorMatch:(BOOL)flag;
- (BOOL)prefersColorMatch;
- (void)setMatchesOnMultipleResolution:(BOOL)flag;
- (BOOL)matchesOnMultipleResolution;
- (void)dissolveToPoint:(NSPoint)point fraction:(float)aFloat;
- (void)dissolveToPoint:(NSPoint)point fromRect:(NSRect)rect fraction:(float)aFloat;
- (void)compositeToPoint:(NSPoint)point operation:(NSCompositingOperation)op;
- (void)compositeToPoint:(NSPoint)point fromRect:(NSRect)rect operation:(NSCompositingOperation)op;
- (void)compositeToPoint:(NSPoint)point operation:(NSCompositingOperation)op fraction:(float)delta;
- (void)compositeToPoint:(NSPoint)point fromRect:(NSRect)rect operation:(NSCompositingOperation)op fraction:(float)delta;
- (void)drawAtPoint:(NSPoint)point fromRect:(NSRect)fromRect operation:(NSCompositingOperation)op fraction:(float)delta;
- (void)drawInRect:(NSRect)rect fromRect:(NSRect)fromRect operation:(NSCompositingOperation)op fraction:(float)delta;
- (BOOL)drawRepresentation:(NSImageRep *)imageRep inRect:(NSRect)rect;
- (void)recache;
- (NSData *)TIFFRepresentation;
- (NSData *)TIFFRepresentationUsingCompression:(NSTIFFCompression)comp factor:(float)aFloat;

- (NSArray *)representations;
- (void)addRepresentations:(NSArray *)imageReps;
- (void)addRepresentation:(NSImageRep *)imageRep;
- (void)removeRepresentation:(NSImageRep *)imageRep;

- (BOOL)isValid;
- (void)lockFocus;
- (void)lockFocusOnRepresentation:(NSImageRep *)imageRepresentation;
- (void)unlockFocus;

- (NSImageRep *)bestRepresentationForDevice:(NSDictionary *)deviceDescription;

- (void)setDelegate:(id)anObject;
- (id)delegate;

/* These return union of all the types registered with NSImageRep.
*/
+ (NSArray *)imageUnfilteredFileTypes;
+ (NSArray *)imageUnfilteredPasteboardTypes;
+ (NSArray *)imageFileTypes;
+ (NSArray *)imagePasteboardTypes;

+ (BOOL)canInitWithPasteboard:(NSPasteboard *)pasteboard;

- (void)setFlipped:(BOOL)flag;
- (BOOL)isFlipped;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
- (void)cancelIncrementalLoad;

-(void)setCacheMode:(NSImageCacheMode)mode;
-(NSImageCacheMode)cacheMode;
#endif

@end

#ifdef WIN32

@interface NSImage (NSWindowsExtensions)
- (id)initWithIconHandle:(void * /* HICON */)icon;
- (id)initWithBitmapHandle:(void * /* HBITMAP */)bitmap;
@end

#endif

@interface NSObject(NSImageDelegate)
- (NSImage *)imageDidNotDraw:(id)sender inRect:(NSRect)aRect;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
- (void)image:(NSImage*)image willLoadRepresentation:(NSImageRep*)rep;
- (void)image:(NSImage*)image didLoadRepresentationHeader:(NSImageRep*)rep;
- (void)image:(NSImage*)image didLoadPartOfRepresentation:(NSImageRep*)rep withValidRows:(int)rows;
- (void)image:(NSImage*)image didLoadRepresentation:(NSImageRep*)rep withStatus:(NSImageLoadStatus)status;
#endif
@end

@interface NSBundle(NSBundleImageExtension)
- (NSString *)pathForImageResource:(NSString *)name;	/* May return nil if no file found */
@end}

  end;
  
{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

function NSImage.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSImage);
end;

constructor NSImage.initWithContentsOfFile(fileName: CFStringRef);
type
  initWithContentsOfFile_t = function (param1: objc.id; param2: SEL;
   param3: CFStringRef): objc.id; cdecl;
var
  vinit: initWithContentsOfFile_t;
begin
  ClassId := getClass();
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  vinit := initWithContentsOfFile_t(@objc_msgSend);
  Handle := vinit(allocbuf, sel_registerName(PChar(Str_initWithContentsOfFile)), fileName);
end;

{$endif}

