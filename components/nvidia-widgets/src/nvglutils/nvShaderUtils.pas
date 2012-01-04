//
// Utility functions for compiling shaders and programs
//
// Author: Evan Hart
// Copyright (c) NVIDIA Corporation. All rights reserved.
////////////////////////////////////////////////////////////////////////////////
unit nvShaderUtils;

{$mode objfpc}{$H+}

interface

{ $define NV_REPORT_COMPILE_ERRORS}

uses
  Classes, SysUtils, GL, GLext;

function CompileGLSLShader(target: GLenum; shader: string): GLuint;
function CompileGLSLShaderFromFile(target: GLenum; filename: string): GLuint;
function LinkGLSLProgram(vertexShader: GLuint; fragmentShader: GLuint): GLuint;
function LinkGLSLProgram(vertexShader: GLuint; geometryShader: GLuint; inputType: GLint; vertexOut: GLint; outputType: GLint; fragmentShader: GLuint): GLuint;
function CompileASMShader(program_type: GLenum; code: string): GLuint;
function CompileASMShaderFromFile(target: GLenum; filename: string): GLuint;

implementation

function CompileGLSLShader(target: GLenum; shader: string): GLuint;
var
  _object: GLuint;
  compiled: GLint;
  {$ifdef NV_REPORT_COMPILE_ERRORS}
  temp: string;
  {$endif}
begin
  _object := glCreateShader(target);
  if _object = 0 then
  begin
    Result := _object;
    exit;
  end;

  glShaderSource(_object, 1, @shader, nil);

  glCompileShader(_object);

  //check if shader compiled
  compiled := 0;
  glGetShaderiv(_object, GL_COMPILE_STATUS, @compiled);

  if compiled = 0 then
  begin
    {$ifdef NV_REPORT_COMPILE_ERRORS}
      temp := '';
      glGetShaderInfoLog(_object, 256, nil, @temp[1]);
      writeln(stderr, 'Compile failed:');
      writeln(stderr, temp);
    {$endif}
    glDeleteShader(_object);

    Result := 0;
    exit;
  end;

  Result := _object;
end;

//
//
////////////////////////////////////////////////////////////
function CompileGLSLShaderFromFile(target: GLenum; filename: string): GLuint;
var
  _object: GLuint;
  s: TStrings;
begin
  //must read files as binary to prevent problems from newline translation
  try
    try
      s := TStringList.Create;
      s.LoadFromFile(filename);

      _object := CompileGLSLShader(target, s.Text);
      Result := _object;
      exit;
    finally
      FreeAndNil(s);
    end;
  except
    Result := 0;
    exit;
  end;
end;

// Create a program composed of vertex and fragment shaders.
function LinkGLSLProgram(vertexShader: GLuint; fragmentShader: GLuint): GLuint;
var
  _program: GLuint;
  {$ifdef NV_REPORT_COMPILE_ERRORS}
  infoLogLength: GLint;
  infoLog: string;
  charsWritten: GLint;
  {$endif}
  linkSucceed: GLint;
begin
  _program := glCreateProgram();
  glAttachShader(_program, vertexShader);
  glAttachShader(_program, fragmentShader);
  glLinkProgram(_program);

  {$ifdef NV_REPORT_COMPILE_ERRORS}
    // Get error log.
    glGetProgramiv(_program, GL_INFO_LOG_LENGTH, @infoLogLength);

    infoLog := StringOfChar(' ',infoLogLength);
    glGetProgramInfoLog(_program, infoLogLength, @charsWritten, @infoLog[1]);
    writeln(stdout, infoLog);
  {$endif}

  // Test linker result.
  linkSucceed := GL_FALSE;
  glGetProgramiv(_program, GL_LINK_STATUS, @linkSucceed);
  if linkSucceed = GL_FALSE then
  begin
    glDeleteProgram(_program);
    Result := 0;
    exit;
  end;

  Result := _program;
end;

// Create a program composed of vertex, geometry and fragment shaders.
function LinkGLSLProgram(vertexShader: GLuint; geometryShader: GLuint; inputType: GLint; vertexOut: GLint; outputType: GLint; fragmentShader: GLuint): GLuint;
var
  _program: GLuint;
  {$ifdef NV_REPORT_COMPILE_ERRORS}
  charsWritten: GLint;
  infoLogLength: GLint;
  infoLog: string;
  {$endif}
  linkSucceed: GLint;
begin
  _program := glCreateProgram();
  glAttachShader(_program, vertexShader);
  glAttachShader(_program, geometryShader);

  {$note fix this}
  //glProgramParameteriEXT(_program,GL_GEOMETRY_INPUT_TYPE_EXT,inputType);
  //glProgramParameteriEXT(_program,GL_GEOMETRY_VERTICES_OUT_EXT,vertexOut);
  //glProgramParameteriEXT(_program,GL_GEOMETRY_OUTPUT_TYPE_EXT,outputType);
  glAttachShader(_program, fragmentShader);
  glLinkProgram(_program);

  {$ifdef NV_REPORT_COMPILE_ERRORS}
    //Get error log.
    glGetProgramiv(_program, GL_INFO_LOG_LENGTH, @infoLogLength);

    infoLog := StringOfChar(' ',infoLogLength);
    glGetProgramInfoLog(_program, infoLogLength, @charsWritten, @infoLog[1]);
    writeln(stdout, infoLog);
  {$endif}

  //Test linker result.
  linkSucceed := GL_FALSE;
  glGetProgramiv(_program, GL_LINK_STATUS, @linkSucceed);
  if linkSucceed = GL_FALSE then
  begin
    glDeleteProgram(_program);
    Result := 0;
    exit;
  end;

  Result := _program;
end;

//
//
////////////////////////////////////////////////////////////
function CompileASMShader(program_type: GLenum; code: string): GLuint;
var
  {$ifdef NV_REPORT_COMPILE_ERRORS}
  error_string: string;
  {$endif}
  program_id: GLuint;
  error_pos: GLint;
begin
  glGenProgramsARB(1, @program_id);
  glBindProgramARB(program_type, program_id);
  glProgramStringARB(program_type, GL_PROGRAM_FORMAT_ASCII_ARB, length(code), @code[1]);

  glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @error_pos);
  if error_pos <> -1 then
  begin
    {$ifdef NV_REPORT_COMPILE_ERRORS}
      error_string := glGetString(GL_PROGRAM_ERROR_STRING_ARB);
      writeln(stderr, 'Program error at position: ', error_pos);
      writeln(stderr, error_string);
    {$endif}
    Result := 0;
    exit;
  end;

  Result := program_id;
end;

//
//
////////////////////////////////////////////////////////////
function CompileASMShaderFromFile(target: GLenum; filename: string): GLuint;
var
  program_id: GLuint;
  s: TStrings;
begin
  //must read files as binary to prevent problems from newline translation
  try
    try
      s := TStringList.Create;
      s.LoadFromFile(filename);

      program_id := CompileASMShader(target, s.Text);
      Result := program_id;
      exit;
    finally
      FreeAndNil(s);
    end;
  except
    Result := 0;
    exit;
  end;
end;

end.

