(*
  A quick and simple opengl font library that uses GNU freetype2, written
  and distributed as part of a tutorial for nehe.gamedev.net.
  Sven Olsen, 2003
*)
unit GLFreeType;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, freetypeh, GL, GLu;

//This holds all of the information related to any
//freetype font that we want to create.
type

  { TGLFreeType }

  TGLFreeType = object
    textures: pGLuint; //< Holds the texture id's
    list_base: GLuint; //< Holds the first display list id
    Height: cardinal; //< Holds the height of the font.

    //The init function will create a font of
    //of the height h from the file fname.
    procedure Init(const fname: string; AHeight: cardinal);

    //Free all the resources assosiated with the font.
    procedure Clean;

    //The flagship function of the library - this thing will print
    //out text at window coordinates x, y, using the font ft_font.
    //The current modelview matrix will also be applied to the text.
    procedure Print(x, y: double; Text: string);

    function TextWidth(const Text: string): integer;
  end;

implementation

const
  CHAR_NUM = 255;

var
  //this is the cached advance of each glyph in pixels
  glyphadvance: array[0..CHAR_NUM - 1] of LongInt;

//This function gets the first power of 2 >= the
//int that we pass it.
function next_p2(a: integer): integer; inline;
var
  rval: integer;
begin
  rval := 2;
  while rval < a do
    rval := rval shl 1;
  Result := rval;
end;

//Create a display list coresponding to the give character.
procedure make_dlist(face: PFT_Face; ch: char; list_base: GLuint; tex_base: pGLuint);
var
  glyph: PFT_Glyph;
  bitmap_glyph: PFT_BitmapGlyph;
  bitmap: FT_Bitmap;
  Width: integer;
  Height: integer;
  expanded_data: pGLubyte;
  x: double;
  y: double;
  i, j: integer;
begin
  //The first thing we do is get FreeType to render our character
  //into a bitmap.  This actually requires a couple of FreeType commands:

  //Load the Glyph for our character.
  if FT_Load_Glyph(face, FT_Get_Char_Index(face, Ord(ch)), FT_LOAD_DEFAULT) = 1 then
    raise Exception.Create('FT_Load_Glyph failed');

  //Move the face's glyph into a Glyph object.
  if FT_Get_Glyph(face^.glyph, glyph) = 1 then
    raise Exception.Create('FT_Get_Glyph failed');

  FT_Glyph_To_Bitmap(glyph, FT_RENDER_MODE_NORMAL, nil, True);

  //Convert the glyph to a bitmap.
  bitmap_glyph := PFT_BitmapGlyph(glyph);

  //This reference will make accessing the bitmap easier
  bitmap := bitmap_glyph^.bitmap;

  //Use our helper function to get the widths of
  //the bitmap data that we will need in order to create
  //our texture.
  Width := next_p2(bitmap.Width);
  Height := next_p2(bitmap.rows);

  //Allocate memory for the texture data.
  GetMem(expanded_data, 2 * Width * Height);
  //writeln(2 * Width * Height, 'bytes for character #', Ord(ch), ' - ', ch);

  //Here we fill in the data for the expanded bitmap.
  //Notice that we are using two channel bitmap (one for
  //luminocity and one for alpha), but we assign
  //both luminocity and alpha to the value that we
  //find in the FreeType bitmap.
  //We use the ?: operator so that value which we use
  //will be 0 if we are in the padding zone, and whatever
  //is the the Freetype bitmap otherwise.
  for j:=0 to height -1 do
    for i:=0 to width - 1 do
    begin
          expanded_data[2*(i+j*width)] := 255;

          if (i>=bitmap.width) or (j>=bitmap.rows) then
          expanded_data[2*(i+j*width)+1] := 0
          else
            expanded_data[2*(i+j*width)+1] :=  byte((bitmap.buffer + (i + bitmap.Width * j))^);
    end;

  glBindTexture(GL_TEXTURE_2D, tex_base[Ord(ch)]);

  //Now we just setup some texture paramaters.
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, expanded_data);

  //Here we actually create the texture itself, notice
  //that we are using GL_LUMINANCE_ALPHA to indicate that
  //we are using 2 channel data.

  //With the texture created, we don't need to expanded data anymore
  FreeMem(expanded_data, 2 * Width * Height);
  glNewList(list_base + Ord(ch), GL_COMPILE);

  //So now we can create the display list
  glBindTexture(GL_TEXTURE_2D, tex_base[Ord(ch)]);
  glPushMatrix;
  glTranslatef(bitmap_glyph^.left, 0, 0);

  //first we need to move over a little so that
  //the character has the right amount of space
  //between it and the one before it.
  glTranslatef(0, bitmap_glyph^.top - bitmap.rows, 0);

  //Now we move down a little in the case that the
  //bitmap extends past the bottom of the line
  //(this is only true for characters like 'g' or 'y'.

  //Now we need to account for the fact that many of
  //our textures are filled with empty padding space.
  //We figure what portion of the texture is used by
  //the actual character and store that information in
  //the x and y variables, then when we draw the
  //quad, we will only reference the parts of the texture
  //that we contain the character itself.
  x := bitmap.Width / Width;
  y := bitmap.rows / Height;

  //Here we draw the texturemaped quads.
  //The bitmap that we got from FreeType was not
  //oriented quite like we would like it to be,
  //so we need to link the texture to the quad
  //so that the result will be properly aligned.
  glBegin(GL_QUADS);
    glTexCoord2d(0, 0);
    glVertex2f(0, bitmap.rows);
    glTexCoord2d(0, y);
    glVertex2f(0, 0);
    glTexCoord2d(x, y);
    glVertex2f(bitmap.Width, 0);
    glTexCoord2d(x, 0);
    glVertex2f(bitmap.Width, bitmap.rows);
  glEnd;
  glPopMatrix;
  glTranslatef(face^.glyph^.advance.x shr 6, 0, 0);

  //increment the raster position as if we were a bitmap font.
  //(needed if you want to calculate text length)
  glyphadvance[Ord(ch)] := face^.glyph^.advance.x shr 6;

  //Finish the display list
  glEndList;

  FT_Done_Glyph(glyph);
end;

procedure TGLFreeType.Init(const fname: string; AHeight: cardinal);
var
  library_: PFT_Library = nil;
  face: PFT_Face = nil; //The object in which Freetype holds information on a given font is called a "face".
  i: byte;
begin
  //Allocate some memory to store the texture ids.
  GetMem(textures, CHAR_NUM * SizeOf(GLuint));

  Height := AHeight;

  //Create and initilize a freetype font library.
  if FT_Init_FreeType(library_) = 1 then
    raise Exception.Create('FT_Init_FreeType failed');

  //This is where we load in the font information from the file.
  //Of all the places where the code might die, this is the most likely,
  //as FT_New_Face will die if the font file does not exist or is somehow broken.
  if FT_New_Face(library_, PChar(fname), 0, face) = 1 then
    raise Exception.CreateFmt('FT_New_Face failed (there is probably a problem with your font file "%s")', [fname]);

  //For some twisted reason, Freetype measures font size
  //in terms of 1/64ths of pixels.  Thus, to make a font
  //h pixels high, we need to request a size of h*64.
  //(h << 6 is just a prettier way of writting h*64)
  FT_Set_Char_Size(face, Height shl 6, Height shl 6, 96, 96);

  //Here we ask opengl to allocate resources for
  //all the textures and displays lists which we
  //are about to create.
  list_base := glGenLists(CHAR_NUM);
  glGenTextures(CHAR_NUM, textures);

  //This is where we actually create each of the fonts display lists.
  for i := 0 to CHAR_NUM - 1 do
    make_dlist(face, Chr(i), list_base, textures);

  //We don't need the face information now that the display
  //lists have been created, so we free the assosiated resources.
  FT_Done_Face(face);

  //Ditto for the library.
  FT_Done_FreeType(library_);
end;

procedure TGLFreeType.Clean;
begin
  glDeleteLists(list_base, CHAR_NUM);
  glDeleteTextures(CHAR_NUM, textures);
  FreeMem(textures, CHAR_NUM * SizeOf(GLuint));
end;

//A fairly straight forward function that pushes
//a projection matrix that will make object world
//coordinates identical to window coordinates.
procedure pushScreenCoordinateMatrix;
var
  viewport: array [0..3] of GLint;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glGetIntegerv(GL_VIEWPORT, viewport);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  gluOrtho2D(viewport[0], viewport[2], viewport[1], viewport[3]);
  glPopAttrib;
end;

//Pops the projection matrix without changing the current
//MatrixMode.
procedure pop_projection_matrix;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glPopAttrib;
end;

//Much like Nehe's glPrint function, but modified to work
//with freetype fonts.
procedure TGLFreeType.Print(x, y: double; Text: string);
var
  font: GLuint;
  modelview_matrix: array [0..15] of double;
begin
  //We want a coordinate system where things coresponding to window pixels.
  pushScreenCoordinateMatrix;

  font := list_base;

  //Results Are Stored In Text
  glPushAttrib(GL_LIST_BIT or GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT);

  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_DEPTH_TEST);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glListBase(font);

  glGetFloatv(GL_MODELVIEW_MATRIX, @modelview_matrix[0]);

  glPushMatrix;
  glLoadIdentity;
  glTranslatef(x, y, 0);
  glMultMatrixf(@modelview_matrix[0]);
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));

  //The commented out raster position stuff can be useful if you need to
  //know the length of the text that you are creating.
  //If you decide to use it make sure to also uncomment the glBitmap command
  //in make_dlist.
  //glRasterPos2f(0,0);
  glPopMatrix;
  //float rpos[4];
  //glGetFloatv(GL_CURRENT_RASTER_POSITION ,rpos);
  //float len=x-rpos[0];

  glPopAttrib;
  pop_projection_matrix;
end;

function TGLFreeType.TextWidth(const Text: string): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Text) do
    Result += glyphadvance[Ord(Text[i])];
end;

end.

