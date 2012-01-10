type
  //add more here
  NV_CONTEXT = (GLUT);
  NV_PAINTER = (GL);
  NV_FONT = (GLFREETYPE, GLUTBITMAP);

const
  NV_DEBUG = True;
  NV_DEBUG_FLAGS: array [0..2] of string = ('-g', '-gl', '-gh');
  
  NV_PROFILE = False;
  NV_PROFILE_FLAGS: array [0..0] of string = ('-pg');  

var  
  //choose the context 
  NV_ACTIVE_CONTEXT: NV_CONTEXT = GLUT;
  
  //choose the painter
  NV_ACTIVE_PAINTER: NV_PAINTER = GL;
  
  //choose the font
  NV_ACTIVE_FONT: NV_FONT = GLFREETYPE;  