
(*
 * LibAxl: C Header file to Free Pascal translation.
 * Copyright (C) 2009, Wimpie Nortje <wimpienortje@gmail.com>
 *)

(*
 *  LibAxl:  Another XML library
 *  Copyright (C) 2006 Advanced Software Production Line, S.L.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of 
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *  
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to
 *  develop proprietary applications using this library without any
 *  royalty or fee but returning back any change, improvement or
 *  addition in the form of source code, project image, documentation
 *  patches, etc. 
 *
 *  For commercial support on build XML enabled solutions contact us:
 *          
 *      Postal address:
 *         Advanced Software Production Line, S.L.
 *         Edificio Alius A, Oficina 102,
 *         C/ Antonio Suarez Nº 10,
 *         Alcalá de Henares 28802 Madrid
 *         Spain
 *
 *      Email address:
 *         info@aspl.es - http://www.aspl.es/xml
 *)

(**
 * \addtogroup axl_stream_module
 * @{
 *)

//axlStream * axl_stream_new             (const char  * stream_source,
//                                        int           stream_size,
//                                        const char  * file_path,
//                                        int           fd_handler,
//                                        axlError   ** error);
function      axl_stream_new             (const stream_source:Pchar;
                                          stream_size: longint;
                                          const file_path: Pchar;
                                          fd_handler: longint;
                                          error: PPaxlError):PaxlStream;
                                          cdecl;external External_library name 'axl_stream_new';

//int         axl_stream_inspect         (axlStream * stream, const char * chunk, int inspected_size);
function      axl_stream_inspect         (stream:PaxlStream; const chunk:Pchar; inspected_size:longint):longint;
                                          cdecl;external External_library name 'axl_stream_inspect';

//int         axl_stream_inspect_code    (axlStream * stream, char value, int index_desp);
function      axl_stream_inspect_code    (stream:PaxlStream; value:char; index_desp:longint):longint;
                                          cdecl;external External_library name 'axl_stream_inspect_code';

//int         axl_stream_peek            (axlStream * stream, const char * chunk, int inspected_size);
function      axl_stream_peek            (stream:PaxlStream; const chunk:Pchar; inspected_size:longint):longint;
                                          cdecl;external External_library name 'axl_stream_peek';

//int         axl_stream_inspect_several (axlStream * stream, int chunk_num, ...);
function      axl_stream_inspect_several (stream:PaxlStream; chunk_num:longint; args:array of const):longint;
                                          cdecl;external External_library name 'axl_stream_inspect_several';

function      axl_stream_inspect_several (stream:PaxlStream; chunk_num:longint):longint;
                                          cdecl;external External_library name 'axl_stream_inspect_several';

//void        axl_stream_accept          (axlStream * stream);
procedure     axl_stream_accept          (stream:PaxlStream);
                                          cdecl;external External_library name 'axl_stream_accept';

//void        axl_stream_move            (axlStream * stream, int index);
procedure     axl_stream_move            (stream:PaxlStream; index:longint);
                                          cdecl;external External_library name 'axl_stream_move';

//void        axl_stream_step            (axlStream * stream, int bytes);
procedure     axl_stream_step            (stream:PaxlStream; bytes:longint);
                                          cdecl;external External_library name 'axl_stream_step';

//void        axl_stream_push            (axlStream * stream, const char * content, int size);
procedure     axl_stream_push            (stream:PaxlStream; const content:Pchar; size:longint);
                                          cdecl;external External_library name 'axl_stream_push';

//axl_bool    axl_stream_fall_outside    (axlStream * stream, int inspected_size);
function      axl_stream_fall_outside    (stream:PaxlStream; inspected_size:longint):Taxl_bool;
                                          cdecl;external External_library name 'axl_stream_fall_outside';

//axl_bool    axl_stream_check           (axlStream * stream, char * chunk, int inspected_size);
function      axl_stream_check           (stream:PaxlStream; chunk:Pchar; inspected_size:longint):Taxl_bool;
                                          cdecl;external External_library name 'axl_stream_check';

//char      * axl_stream_get_until       (axlStream * stream,
//                                        char      * valid_chars,
//                                        int       * chunk_matched,
//                                        axl_bool    accept_terminator,
//                                        int         chunk_num, ...);
function      axl_stream_get_until       (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          chunk_num:longint;
                                          args:array of const):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until';

function      axl_stream_get_until       (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          chunk_num:longint):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until';

//char      * axl_stream_get_until_ref   (axlStream * stream,
//                                        char      * valid_chars,
//                                        int       * chunk_matched,
//                                        axl_bool    accept_terminator,
//                                        int       * result_size,
//                                        int         chunk_num, ...);
function      axl_stream_get_until_ref   (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          var result_size:longint;
                                          chunk_num:longint;
                                          args:array of const):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until_ref';

function      axl_stream_get_until_ref   (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          var result_size:longint;
                                          chunk_num:longint):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until_ref';

//char      * axl_stream_get_until_zero  (axlStream * stream,
//                                        char      * valid_chars,
//                                        int       * chunk_matched,
//                                        axl_bool    accept_terminator,
//                                        int         chunk_num, ...);
function      axl_stream_get_until_zero  (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          chunk_num:longint;
                                          args:array of const):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until_zero';

function      axl_stream_get_until_zero  (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          chunk_num:longint):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_until_zero';

//char      * axl_stream_get_until_ref_zero  (axlStream * stream,
//                                            char      * valid_chars,
//                                            int       * chunk_matched,
//                                            axl_bool    accept_terminator,
//                                            int       * result_size,
//                                            int         chunk_num, ...);
function      axl_stream_get_until_ref_zero  (stream:PaxlStream;
                                              valid_chars:Pchar;
                                              var chunk_matched:longint;
                                              accept_terminator:Taxl_bool;
                                              var result_size:longint;
                                              chunk_num:longint;
                                              args:array of const):Pchar;
                                              cdecl;external External_library name 'axl_stream_get_until_ref_zero';

function      axl_stream_get_until_ref_zero  (stream:PaxlStream;
                                              valid_chars:Pchar;
                                              var chunk_matched:longint;
                                              accept_terminator:Taxl_bool;
                                              var result_size:longint;
                                              chunk_num:longint):Pchar;
                                              cdecl;external External_library name 'axl_stream_get_until_ref_zero';

//void       axl_stream_set_buffer_alloc   (axlStream * stream,
//                                          axlStreamAlloc handler,
//                                          axlPointer     data);
procedure    axl_stream_set_buffer_alloc   (stream:PaxlStream;
                                            handler:TaxlStreamAlloc;
                                            data:TaxlPointer);
                                            cdecl;external External_library name 'axl_stream_set_buffer_alloc';


(**
 * @brief Enum value that allows to configure which item to nullify
 * inside the provided \ref axlStream reference.
 * 
 *)
type TNullifyItem = (
  	(**
  	 * @brief Nullify the last chunk returned due to call from \ref axl_stream_get_until.
  	 *)
  	LAST_CHUNK,
  	(**
  	 * @brief Nullify the last chunk returned by \ref axl_stream_get_near_to function.
  	 *)
  	LAST_NEAR_TO,
  	(**
  	 * @brief Nullify the last chunk returned by \ref axl_stream_get_following function.
  	 *)
  	LAST_GET_FOLLOWING
  );

//void        axl_stream_nullify         (axlStream * stream,
//                                        NullifyItem item);
procedure     axl_stream_nullify         (stream:PaxlStream;
                                          item:TNullifyItem);
                                          cdecl;external External_library name 'axl_stream_nullify';

//char      * axl_stream_get_untilv      (axlStream * stream,
//                                        char      * valid_chars,
//                                        int       * chunk_matched,
//                                        axl_bool    accept_terminator,
//                                        int       * result_size,
//                                        int         chunk_num,
//                                        va_list args);
function      axl_stream_get_untilv      (stream:PaxlStream;
                                          valid_chars:Pchar;
                                          var chunk_matched:longint;
                                          accept_terminator:Taxl_bool;
                                          var result_size:longint;
                                          chunk_num:longint;
                                          args:array of const):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_untilv';

//int         axl_stream_get_index       (axlStream * stream);
function      axl_stream_get_index       (stream:PaxlStream):longint;
                                          cdecl;external External_library name 'axl_stream_get_index';

//int         axl_stream_get_global_index (axlStream * stream);
function      axl_stream_get_global_index (stream:PaxlStream):longint;
                                            cdecl;external External_library name 'axl_stream_get_global_index';

//int         axl_stream_get_size        (axlStream * stream);
function      axl_stream_get_size        (stream:PaxlStream):longint;
                                          cdecl;external External_library name 'axl_stream_get_size';

//const char * axl_stream_get_near_to    (axlStream * stream, int count);
function       axl_stream_get_near_to    (stream:PaxlStream; count:longint):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_near_to';

//const char * axl_stream_get_following  (axlStream * stream, int count);
function       axl_stream_get_following  (stream:PaxlStream; count:longint):Pchar;
                                          cdecl;external External_library name 'axl_stream_get_following';

//void        axl_stream_link            (axlStream  *   stream,
//                                        axlPointer     element,
//                                        axlDestroyFunc func);
procedure     axl_stream_link            (stream:PaxlStream;
                                          element:TaxlPointer;
                                          func:TaxlDestroyFunc);
                                          cdecl;external External_library name 'axl_stream_link';

//void        axl_stream_link_full       (axlStream  *   stream,
//                                        axlPointer     element,
//                                        axlDestroyFunc func,
//                                        axl_bool       free_on_finish);
procedure     axl_stream_link_full       (stream:PaxlStream;
                                          element:TaxlPointer;
                                          func:TaxlDestroyFunc;
                                          free_on_finish:Taxl_bool);
                                          cdecl;external External_library name 'axl_stream_link_full';

//void        axl_stream_unlink          (axlStream * stream);
procedure     axl_stream_unlink          (stream:PaxlStream);
                                          cdecl;external External_library name 'axl_stream_unlink';

//void        axl_stream_free            (axlStream * stream);
procedure     axl_stream_free            (stream:PaxlStream);
                                          cdecl;external External_library name 'axl_stream_free';

//axl_bool    axl_stream_is_white_space  (char * chunk);
function      axl_stream_is_white_space  (chunk:Pchar):Taxl_bool;
                                          cdecl;external External_library name 'axl_stream_is_white_space';

//void        axl_stream_consume_white_spaces (axlStream * stream);
procedure     axl_stream_consume_white_spaces (stream:PaxlStream);
                                                cdecl;external External_library name 'axl_stream_consume_white_spaces';

//axl_bool    axl_stream_remains         (axlStream * stream);
function      axl_stream_remains         (stream:PaxlStream):Taxl_bool;
                                          cdecl;external External_library name 'axl_stream_remains';

(* @} *)

(* string handling interface *)

//void        axl_stream_trim              (char * chunk);
procedure     axl_stream_trim              (chunk:Pchar);
                                            cdecl;external External_library name 'axl_stream_trim';

//void        axl_stream_trim_with_size    (char * chunk, int * trimmed);
procedure     axl_stream_trim_with_size    (chunk:Pchar; var trimmed:longint);
                                            cdecl;external External_library name 'axl_stream_trim_with_size';

//void        axl_stream_remove            (char * chunk, const char * value, axl_bool first);
procedure     axl_stream_remove            (chunk:Pchar; const value:Pchar; first:Taxl_bool);
                                            cdecl;external External_library name 'axl_stream_remove';

//axl_bool    axl_stream_cmp               (const char * chunk1, const char * chunk2, int size);
function      axl_stream_cmp               (const chunk1:Pchar; const chunk2:Pchar; size:longint):Taxl_bool;
                                            cdecl;external External_library name 'axl_stream_cmp';

//axl_bool    axl_stream_casecmp           (const char * chunk1, const char * chunk2, int size);
function      axl_stream_casecmp           (const chunk1:Pchar; const chunk2:Pchar; size:longint):Taxl_bool;
                                            cdecl;external External_library name 'axl_stream_casecmp';

//char      * axl_stream_strdup            (const char * chunk);
function      axl_stream_strdup            (const chunk:Pchar):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup';

//char      * axl_stream_strdup_n          (const char * chunk, int n);
function      axl_stream_strdup_n          (const chunk:Pchar; n:longint):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_n';

//int         axl_stream_vprintf_len       (const char * format,
//                                          va_list args);
function      axl_stream_vprintf_len       (const format:Pchar;
                                            args:array of const):longint;
                                            cdecl;external External_library name 'axl_stream_vprintf_len';

//int         axl_stream_printf_len        (const char * format,
//                                          ...);
function      axl_stream_printf_len        (const format:Pchar;
                                            args:array of const):longint;
                                            cdecl;external External_library name 'axl_stream_printf_len';

function      axl_stream_printf_len        (const format:Pchar):longint;
                                            cdecl;external External_library name 'axl_stream_printf_len';

//char      * axl_stream_strdup_printf     (const char * chunk, ...);
function      axl_stream_strdup_printf     (chunk:Pchar; args:array of const):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_printf';

function      axl_stream_strdup_printf     (chunk:Pchar):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_printf';

//char      * axl_stream_strdup_printfv    (const char * chunk, va_list args);
function      axl_stream_strdup_printfv    (const chunk:Pchar; args:array of const):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_printfv';

//char      * axl_stream_strdup_printf_len (const char * chunk,
//                                          int  * chunk_size, ...);
function      axl_stream_strdup_printf_len (const chunk:Pchar;
                                            var chunk_size:longint; args:array of const):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_printf_len';

function      axl_stream_strdup_printf_len (const chunk:Pchar;
                                            var chunk_size:longint):Pchar;
                                            cdecl;external External_library name 'axl_stream_strdup_printf_len';

//int         axl_stream_printf_buffer     (char * buffer,
//                                          int    buffer_size,
//                                          int  * real_size,
//                                          const char * format, ...);
function      axl_stream_printf_buffer     (buffer:Pchar;
                                            buffer_size:longint;
                                            var real_size:longint;
                                            const format:Pchar; args:array of const):longint;
                                            cdecl;external External_library name 'axl_stream_printf_buffer';

function      axl_stream_printf_buffer     (buffer:Pchar;
                                            buffer_size:longint;
                                            var real_size:longint;
                                            format:Pchar):longint;
                                            cdecl;external External_library name 'axl_stream_printf_buffer';

//char     ** axl_stream_split           (const char * chunk, int separator_num, ...);
function      axl_stream_split           (const chunk:Pchar; separator_num:longint; args:array of const):PPchar;
                                          cdecl;external External_library name 'axl_stream_split';

function      axl_stream_split           (const chunk:Pchar; separator_num:longint):PPchar;
                                          cdecl;external External_library name 'axl_stream_split';

//void        axl_stream_clean_split     (char ** split);
procedure     axl_stream_clean_split     (split:PPchar);
                                          cdecl;external External_library name 'axl_stream_clean_split';

//char      * axl_stream_join            (char ** strings, const char * separator);
function      axl_stream_join            (strings:PPchar; const separator:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_join';

//char      * axl_stream_concat          (const char * chunk1, const char * chunk2);
function      axl_stream_concat          (const chunk1:Pchar; const chunk2:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_concat';

//int         axl_stream_strv_num        (char ** chunks);
function      axl_stream_strv_num        (chunks:PPchar):longint;
                                          cdecl;external External_library name 'axl_stream_strv_num';

//void        axl_stream_freev           (char ** chunks);
procedure     axl_stream_freev           (chunks:PPchar);
                                          cdecl;external External_library name 'axl_stream_freev';

//char      * axl_stream_to_upper        (char  * chunk);
function      axl_stream_to_upper        (chunk:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_to_upper';

//char      * axl_stream_to_lower        (char  * chunk);
function      axl_stream_to_lower        (chunk:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_to_lower';

//char      * axl_stream_to_upper_copy   (const char  * chunk);
function      axl_stream_to_upper_copy   (const chunk:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_to_upper_copy';

//char      * axl_stream_to_lower_copy   (const char  * chunk);
function      axl_stream_to_lower_copy   (const chunk:Pchar):Pchar;
                                          cdecl;external External_library name 'axl_stream_to_lower_copy';

//axl_bool    axl_cmp (const char * string, const char * string2);
function      axl_cmp (const string1:Pchar; const string2:Pchar):Taxl_bool;
                        cdecl;external External_library name 'axl_cmp';

//axl_bool    axl_casecmp (const char * string, const char * string2);
function      axl_casecmp (const string1:Pchar; string2:Pchar):Taxl_bool;
                            cdecl;external External_library name 'axl_casecmp';

//axl_bool    axl_memcmp (const char * string, const char * string2, int size);
function      axl_memcmp (const string1:Pchar; string2:Pchar; size:longint):Taxl_bool;
                          cdecl;external External_library name 'axl_memcmp';

//char      * axl_strdup (const char * string);
function      axl_strdup (const string1:Pchar):Pchar;
                          cdecl;external External_library name 'axl_strdup';

//axl_bool    axl_stream_setup_decode        (axlStream         * stream,
//                                            const char        * source_encoding,
//                                            axlStreamDecode     decode_f,
//                                            axlPointer          user_data,
//                                            axlError         ** error);
function      axl_stream_setup_decode        (stream:PaxlStream;
                                              const source_encoding:Pchar;
                                              decode_f:TaxlStreamDecode;
                                              user_data:TaxlPointer;
                                              error:PPaxlError):Taxl_bool;
                                              cdecl;external External_library name 'axl_stream_setup_decode';

//axl_bool    axl_stream_setup_check         (axlStream                * stream,
//                                            const char               * source_encoding,
//                                            axlStreamContentCheck      check,
//                                            axlPointer                 user_data,
//                                            axlError                ** error);
function      axl_stream_setup_check         (stream:PaxlStream;
                                              const source_encoding:Pchar;
                                              check:TaxlStreamContentCheck;
                                              user_data:TaxlPointer;
                                              error:PPaxlError):Taxl_bool;
                                              cdecl;external External_library name 'axl_stream_setup_check';

//axl_bool    axl_stream_decode              (axlStream  * stream,
//                                            char       * output,
//                                            int          output_max_size,
//                                            int        * output_decoded,
//                                            int        * op_result,
//                                            axlError  ** error);
function      axl_stream_decode              (stream:PaxlStream;
                                              output:Pchar;
                                              output_max_size:longint;
                                              var output_decoded:longint;
                                              var op_result:longint;
                                              error:PPaxlError):Taxl_bool;
                                              cdecl;external External_library name 'axl_stream_decode';

//axl_bool    axl_stream_content_check       (axlStream   * stream,
//                                            const char  * content,
//                                            int           content_length,
//                                            axlError   ** error);
function      axl_stream_content_check       (stream:PaxlStream;
                                              const content:Pchar;
                                              content_length:longint;
                                              error:PPaxlError):Taxl_bool;
                                              cdecl;external External_library name 'axl_stream_content_check';

(**
 * \addtogroup axl_string_module
 * @{
 *)

{$if 0}
//*** Find efficient way to define aliases as in C
(**
 * @brief Alias definition for \ref axl_stream_strdup_printf.
 *
 * See \ref axl_stream_strdup_printf information for more details.
 *)
#define axl_strdup_printf axl_stream_strdup_printf

(**
 * @brief Alias definition for \ref axl_stream_strdup_printfv.
 *)
#define axl_strdup_printfv axl_stream_strdup_printfv

(**
 * @brief Alias definition for the \ref axl_stream_concat.
 *)
#define     axl_concat axl_stream_concat

(**
 * @brief Alias definition for the \ref axl_stream_join.
 *)
#define     axl_join   axl_stream_join

(**
 * @brief Alias definition for the \ref axl_stream_split.
 *)
#define     axl_split  axl_stream_split

(**
 * @brief Alias definition for the \ref axl_stream_freev.
 *)
#define     axl_freev  axl_stream_freev
{$endif} //if 0

(**
 * @brief Convenient interface to check if an string reference is NULL
 * to return an static "undefined string". This is useful in the case
 * string references are used on functions that can't handle NULL
 * values.
 *
 * @param ref An string reference to check.
 *)
//#define     axl_check_undef(ref) (ref ? ref : "NULL")
function axl_check_undef(ref:Pchar):Pchar;

(* @} *)

