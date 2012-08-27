program GtkLauncher;

{$mode objfpc}{$H+}

{ This is a FPC port/modification of the original source from:
  http://trac.webkit.org/browser/trunk/WebKitTools/GtkLauncher/main.c

  Found @ http://freepascal-bits.blogspot.com/2009/10/webkit-experimenting.html

  - Tested on Linux only.
  - Depends on the libwebkit[-dev] package (which is not installed by default
    at least on Ubuntu).
  - You must include the protocol specifier when entering an uri in the address
    edit box, eg. 'http://google.com' and not just 'google.com' as you can
    in most real browsers.

/*
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE COMPUTER, INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE COMPUTER, INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
}

uses
  gtk3, glib2, gobject2, math, webkit3;

var
  load_progress: gint = 0;
  main_statusbar: PGtkStatusbar = nil;
  main_title: pgchar = nil;
  main_window: PGtkWidget = nil;
  status_context_id: guint = 0;
  uri_entry: PGtkWidget = nil;
  web_view: PWebKitWebView = nil;

procedure activate_uri_entry_cb(entry: PGtkWidget; data: gpointer); cdecl;
var uri: Pgchar;
begin
  uri := PGtkEntry(entry)^.get_text;
  assert(assigned(uri));
  web_view^.load_uri(uri);
  //web_view_open(web_view, uri);
end;

procedure update_title (window: PGtkWindow);
var
  string_: PGString;
  title: Pgchar;
begin
  string_ := g_string_new(main_title);
  g_string_append(string_, ' - FPC WebKit/Gtk Launcher');
  if load_progress < 100 then
    g_string_append_printf(string_, ' (%d%%)', [load_progress]);
  title := g_string_free(string_, false);
  gtk_window_set_title(window, title);
  g_free(title);
end;

procedure link_hover_cb(page: PWebKitWebView; const title: pgchar; const link: pgchar; data: gpointer); cdecl;
begin
  gtk_statusbar_pop(main_statusbar, status_context_id);
  if assigned(link) then
    gtk_statusbar_push(main_statusbar, status_context_id, link);
end;

procedure title_change_cb(web_view: PWebKitWebView; web_frame: PWebKitWebFrame; const title: pgchar; data: gpointer); cdecl;
begin
  if assigned(main_title) then
    g_free(main_title);
  main_title := g_strdup(title);
  update_title(PGtkWindow(main_window));
end;

procedure progress_change_cb(page: PWebKitWebView; progress: gint; data: gpointer); cdecl;
begin
  load_progress := progress;
  update_title(PGtkWindow(main_window));
end;

procedure destroy_cb(widget: pGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

procedure go_back_cb(widget: PGtkWidget; data: gpointer); cdecl;
begin
  web_view^.go_back;
end;

procedure go_forward_cb(widget: PGtkWidget; data: gpointer); cdecl;
begin
  web_view^.go_forward;
end;

function create_browser: PGtkWidget;
begin
  result := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(result), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  web_view := TWebKitWebView.new;
  PGtkContainer(result)^.add(web_view);
  g_signal_connect_data(web_view, 'title-changed', TGCallback(@title_change_cb), web_view, nil, 0);
  g_signal_connect_data(web_view, 'load-progress-changed', TGCallback(@progress_change_cb), web_view, nil, 0);
  g_signal_connect_data(web_view, 'hovering-over-link', TGCallback(@link_hover_cb), web_view, nil, 0);
end;

function create_statusbar: PGtkWidget;
begin
  main_statusbar := TGtkStatusbar.new;
  status_context_id := gtk_statusbar_get_context_id(main_statusbar, 'Link Hover');
  result := PGtkWidget(main_statusbar);
end;

function create_toolbar: PGtkWidget;
var item: PGtkToolItem;
begin
  result := gtk_toolbar_new;
  gtk_orientable_set_orientation(PGtkOrientable(result), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style(PGtkToolbar(result), GTK_TOOLBAR_BOTH_HORIZ);
  item := gtk_tool_button_new_from_stock(STOCK_GO_BACK);
  g_signal_connect_data(PGObject(item), 'clicked', TGCallback(@go_back_cb), nil, nil, 0);
  gtk_toolbar_insert(PGtkToolbar(result), item, -1);
  item := gtk_tool_button_new_from_stock(STOCK_GO_FORWARD);
  g_signal_connect_data(PGObject(item), 'clicked', TGCallback(@go_forward_cb), nil, nil, 0);
  gtk_toolbar_insert(PGtkToolbar(result), item, -1);
  item := gtk_tool_item_new;
  gtk_tool_item_set_expand(item, true);
  uri_entry := gtk_entry_new;
  gtk_container_add(PGtkContainer(item), uri_entry);
  g_signal_connect_data(PGObject(uri_entry), 'activate', TGCallback(@activate_uri_entry_cb), nil, nil, 0);
  gtk_toolbar_insert(PGtkToolbar(result), item, -1);
  item := gtk_tool_button_new_from_stock(STOCK_OK);
  g_signal_connect_object(PGObject(item), 'clicked', TGCallback(@activate_uri_entry_cb), gpointer(uri_entry), G_CONNECT_SWAPPED);
  gtk_toolbar_insert(PGtkToolbar(result), item, -1);
end;

function create_window: PGtkWidget;
begin
  result := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(PGtkWindow(result), 800, 600);
  gtk_widget_set_name(result, 'FPC GtkLauncher');
  g_signal_connect_data(result, 'destroy', TGCallback(@destroy_cb), nil, nil, 0);
end;

var vbox: PGtkWidget;

begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  gtk_init(@argc,@argv);
  vbox := TGtkBox.new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_pack_start(PGtkBox(vbox), create_toolbar, false, false, 0);
  gtk_box_pack_start(PGtkBox(vbox), create_browser, true, true, 0);
  gtk_box_pack_start(PGtkBox(vbox), create_statusbar, false, false, 0);
  main_window := create_window;
  gtk_container_add(PGtkContainer(main_window), vbox);
  web_view^.load_uri('http://www.lazarus.freepascal.org');
  gtk_widget_grab_focus(PGtkWidget(web_view));
  gtk_widget_show_all(main_window);
  gtk_main;
end.
