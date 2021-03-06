(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_lame.pas,v $
Revision 1.5  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.2  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.1  2005/12/19 18:36:38  z0m3ie
*** empty log message ***

Revision 1.6  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.5  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.4  2005/11/28 21:57:24  z0m3ie
mostly FileOut fixes
moved PBuffer to PBuffer8
set all to dynamically Buffering

Revision 1.3  2005/11/28 19:10:14  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_lame;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  ACS_File,Classes, SysUtils, ACS_Classes, lame,
{$IFDEF LINUX}
  baseunix;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

type

  TMP3Quality = (ql0, ql1, ql2, ql3, ql4, ql5,
   ql6, ql7, ql8, ql9);

  TMP3Mode = (STEREO = 0,
              JOINT_STEREO,
              DUAL_CHANNEL, // LAME doesn't supports this!
              MONO);

  TMP3SampleRate = (srDefault, sr32kHz, sr41kHz, sr48kHz);

  TMP3BitRate    = (br8, br16, br24, br32, br40, br48, br56, br64, br80, br96,
                    br112, br128, br144, br160, br192, br224, br256, br320);

  BOOL = Boolean;

  TMP3Out = class(TACSCustomFileOut)
  private
    FBitRate : TMP3BitRate;
    _plgf : PLame_global_flags;
    mp3buf : PByte;
    mp3buf_size : Integer;
    FTitle : String;
    FArtist : String;
    FAlbum : String;
    FYear : String;
    FTrack : String;
    FComment : String;
    FGenre : String;
    FCopyright: BOOL;
    FOriginal: BOOL;
    FEnableVBR: BOOL;
    FWriteVBRHeader: BOOL;
    FCRC: BOOL;
    FVBRQuality: TMP3Quality;
    FVBRMinBitrate: integer;
    FVBRMaxBitrate: integer;
    FQuality: TMP3Quality;
    FMode: TMP3Mode;
    FSampleRate: TMP3SampleRate;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BitRate         : TMP3BitRate read FBitRate write FBitRate stored True;
    property Id3TagTitle     : String read FTitle write FTitle;
    property Id3TagArtist    : String read FArtist write FArtist;
    property Id3TagAlbum     : String read FAlbum write FAlbum;
    property Id3TagYear      : String read FYear write FYear;
    property Id3TagTrack     : String read FTrack write FTrack;
    property Id3TagComment   : String read FComment write FComment;
    property Id3TagGenre     : String read FGenre write FGenre;
    property Quality         : TMP3Quality read FQuality write FQuality default ql5;

    property SampleRate      : TMP3SampleRate read FSampleRate write FSampleRate default srDefault;
    property Mode            : TMP3Mode read FMode write FMode default STEREO;

    //Extras
    property CRC	     : BOOL read FCRC write FCRC default false;
    property Copyright	     : BOOL read FCopyright write FCopyright default false;
    property Original        : BOOL read FOriginal write FOriginal default true;

    //VBR encoding
    property WriteVBRHeader  : BOOL read FWriteVBRHeader write FWriteVBRHeader default false;
    property EnableVBR	     : BOOL read FEnableVBR write FEnableVBR default false;
    property VBRQuality      : TMP3Quality read FVBRQuality write FVBRQuality default ql5;
    property VBRMinBitrate   : integer read FVBRMinBitrate write FVBRMinBitrate default 128;
    property VBRMaxBitrate   : integer read FVBRMaxBitrate write FVBRMaxBitrate default 192;
  end;

implementation

  constructor TMP3Out.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    LoadLAME;
    FBitRate := br128;

    FBufferSize := $4000;
    FQuality := ql5;
    FSampleRate := srDefault;
    FMode := STEREO;
    FCRC  := false;
    FCopyright := false;
    FOriginal := true;

    FWriteVBRHeader := false;
    FEnableVBR := false;
    FVBRQuality := ql5;
    FVBRMinBitrate := 128;
    FVBRMaxBitrate := 192;

    if not (csDesigning in ComponentState) then
    if not LameLoaded then
    raise EACSException.Create(LAME_PATH + ' library could not be loaded.');
  end;

  destructor TMP3Out.Destroy;
  begin
    UnloadLAME;
    inherited Destroy;
  end;

  procedure TMP3Out.Prepare;
  var
    samples, br, tbr, sr, ql : Integer;
  begin
    GetMem(FBuffer,FBufferSize);
    if not FStreamAssigned then
    begin
      if FFileName = '' then raise EACSException.Create('File name is not assigned.');
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;
    _plgf := lame_init;
    if FInput.Size > 0 then
    samples := FInput.Size div ((Finput.BitsPerSample shr 3)*Finput.Channels);
    lame_set_num_samples(_plgf, samples);
    lame_set_in_samplerate(_plgf, Finput.SampleRate);
    lame_set_num_channels(_plgf, 2); // not Finput.Channels see the note below
    case FBitRate of
      br8 : br := 8;
      br16 : br := 16;
      br24 : br := 24;
      br32 : br := 32;
      br40 : br := 40;
      br48 : br := 48;
      br56 : br := 56;
      br64 : br := 64;
      br80 : br := 80;
      br96 : br := 96;
      br112 : br := 112;
      br128 : br := 128;
      br144 : br := 144;
      br160 : br := 160;
      br192 : br := 192;
      br224 : br := 224;
      br256 : br := 256;
      br320 : br := 320;
    end;
    case FSampleRate of
      srDefault: sr := 0;
      sr32kHz: sr := 32000;
      sr41kHz: sr := 41000;
      sr48kHz: sr := 48000;
    end;
    lame_set_out_samplerate(_plgf,sr);
{    lame_set_mode(_plgf, Integer(FMode));
    if FCopyright then
    lame_set_copyright(_plgf,1)
    else
    lame_set_copyright(_plgf,0);
    if FOriginal then
    lame_set_original(_plgf,1)
    else
    lame_set_original(_plgf,0);
    if FCRC then
    lame_set_error_protection(_plgf,1)
    else
    lame_set_error_protection(_plgf,0);

    if not EnableVBR then
      begin
        lame_set_brate(_plgf, br);
        case FQuality of
          ql0: ql:=0;
          ql1: ql:=1;
          ql2: ql:=2;
          ql3: ql:=3;
          ql4: ql:=4;
          ql5: ql:=5;
          ql6: ql:=6;
          ql7: ql:=7;
          ql8: ql:=8;
          ql9: ql:=9;
        end;
        lame_set_quality(_plgf,ql);
      end
    else
      begin
        if FWriteVBRHeader then
        lame_set_bWriteVbrTag(_plgf,1);
        lame_set_VBR(_plgf,2);
        case FVBRQuality of
          ql0: ql:=0;
          ql1: ql:=1;
          ql2: ql:=2;
          ql3: ql:=3;
          ql4: ql:=4;
          ql5: ql:=5;
          ql6: ql:=6;
          ql7: ql:=7;
          ql8: ql:=8;
          ql9: ql:=9;
        end;
        lame_set_VBR_q(_plgf,ql);
        if FVBRMinBitrate > FVBRMaxBitrate then
          begin
            tbr := FVBRMinBitrate;
            FVBRMinBitrate := FVBRMaxBitrate;
            FVBRMinBitrate := tbr;
          end;
        lame_set_VBR_min_bitrate_kbps(_plgf,FVBRMinBitrate);
        lame_set_VBR_max_bitrate_kbps(_plgf,FVBRMaxBitrate);
      end;
}
    lame_init_params(_plgf);
    mp3buf_size := (FBufferSize shr 1) + (FBufferSize shr 3) + 7200;
    GetMem(mp3buf, mp3buf_size);
  end;

  procedure TMP3Out.Done;
  var
    res : Integer;
  begin
    id3tag_init(_plgf);
    id3tag_set_title(_plgf, PChar(FTitle));
    id3tag_set_artist(_plgf, PChar(FArtist));
    id3tag_set_album(_plgf, PChar(FAlbum));
    id3tag_set_year(_plgf, PChar(FYear));
    id3tag_set_track(_plgf, PChar(FTrack));
    id3tag_set_comment(_plgf, PChar(FComment));
    id3tag_set_genre(_plgf, PChar(FGenre));
    res := lame_encode_flush(_plgf, mp3buf, mp3buf_size);
    FStream.Write(mp3buf^, res);
    if not FStreamAssigned then FStream.Free;
    lame_close(_plgf);
    FreeMem(mp3buf);
    FInput.Flush;
    FreeMem(FBuffer);
  end;

  function TMP3Out.DoOutput(Abort : Boolean):Boolean;
  var
    Len, res, ns : Integer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Abort then
    begin
      Result := False;
      Exit;
    end;
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(@FBuffer, FBufferSize);
    InputLock := False;
    if Len <> 0 then
    begin
      if FInput.Channels = 2 then
      begin
        ns := Len shr 2;
        res := lame_encode_buffer_interleaved(_plgf, @FBuffer, ns, mp3buf, mp3buf_size);
      end else
      begin
        (* If the input stream is mono we turn it into stereo here.
           This is the way to bypass some pour performance on mono streams,
           maybe not the best way, but for a while ...*)
        ns := Len shr 1;
        res := lame_encode_buffer(_plgf, @FBuffer, @FBuffer, ns, mp3buf, mp3buf_size);
      end;
      if res < 0 then
      begin
        Result := False;
        Exit;
      end;
      FStream.Write(mp3buf^, res);
    end else
    begin
      Result := False;
      Exit;
    end;
  end;

initialization

  FileFormats.Add('mp3','Mpeg Layer III',TMP3Out);


end.
