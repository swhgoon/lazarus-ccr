{* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *}

{*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *}
unit gtk3;

{$mode objfpc}{$H+}

interface 

uses gdk3;

const gtk3lib = 'libgtk+3.0';

{#include <gtk/gtkaboutdialog.h>
#include <gtk/gtkaccelgroup.h>
#include <gtk/gtkaccellabel.h>
#include <gtk/gtkaccelmap.h>
#include <gtk/gtkaccessible.h>
#include <gtk/gtkaction.h>
#include <gtk/gtkactiongroup.h>
#include <gtk/gtkactivatable.h>
#include <gtk/gtkadjustment.h>
#include <gtk/gtkalignment.h>
#include <gtk/gtkappchooser.h>
#include <gtk/gtkappchooserdialog.h>
#include <gtk/gtkappchooserwidget.h>
#include <gtk/gtkappchooserbutton.h>
#include <gtk/gtkapplication.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkaspectframe.h>
#include <gtk/gtkassistant.h>
#include <gtk/gtkbbox.h>
#include <gtk/gtkbin.h>
#include <gtk/gtkbindings.h>
#include <gtk/gtkborder.h>
#include <gtk/gtkbox.h>
#include <gtk/gtkbuildable.h>
#include <gtk/gtkbuilder.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkcalendar.h>
#include <gtk/gtkcellarea.h>
#include <gtk/gtkcellareabox.h>
#include <gtk/gtkcellareacontext.h>
#include <gtk/gtkcelleditable.h>
#include <gtk/gtkcelllayout.h>
#include <gtk/gtkcellrenderer.h>
#include <gtk/gtkcellrendereraccel.h>
#include <gtk/gtkcellrenderercombo.h>
#include <gtk/gtkcellrendererpixbuf.h>
#include <gtk/gtkcellrendererprogress.h>
#include <gtk/gtkcellrendererspin.h>
#include <gtk/gtkcellrendererspinner.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkcellrenderertoggle.h>
#include <gtk/gtkcellview.h>
#include <gtk/gtkcheckbutton.h>
#include <gtk/gtkcheckmenuitem.h>
#include <gtk/gtkclipboard.h>
#include <gtk/gtkcolorbutton.h>
#include <gtk/gtkcolorsel.h>
#include <gtk/gtkcolorseldialog.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtkcomboboxtext.h>
#include <gtk/gtkcontainer.h>
#include <gtk/gtkcssprovider.h>
#include <gtk/gtkdebug.h>
#include <gtk/gtkdialog.h>
#include <gtk/gtkdnd.h>
#include <gtk/gtkdrawingarea.h>
#include <gtk/gtkeditable.h>
#include <gtk/gtkentry.h>
#include <gtk/gtkentrybuffer.h>
#include <gtk/gtkentrycompletion.h>
#include <gtk/gtkenums.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkexpander.h>
#include <gtk/gtkfixed.h>
#include <gtk/gtkfilechooser.h>
#include <gtk/gtkfilechooserbutton.h>
#include <gtk/gtkfilechooserdialog.h>
#include <gtk/gtkfilechooserwidget.h>
#include <gtk/gtkfilefilter.h>
#include <gtk/gtkfontbutton.h>
#include <gtk/gtkfontsel.h>
#include <gtk/gtkframe.h>
#include <gtk/gtkgradient.h>
#include <gtk/gtkgrid.h>
#include <gtk/gtkhandlebox.h>
#include <gtk/gtkhbbox.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkhpaned.h>
#include <gtk/gtkhscale.h>
#include <gtk/gtkhscrollbar.h>
#include <gtk/gtkhseparator.h>
#include <gtk/gtkhsv.h>
#include <gtk/gtkiconfactory.h>
#include <gtk/gtkicontheme.h>
#include <gtk/gtkiconview.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkimcontext.h>
#include <gtk/gtkimcontextsimple.h>
#include <gtk/gtkimmulticontext.h>
#include <gtk/gtkinfobar.h>
#include <gtk/gtkinvisible.h>
#include <gtk/gtklabel.h>
#include <gtk/gtklayout.h>
#include <gtk/gtklinkbutton.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkmenu.h>
#include <gtk/gtkmenubar.h>
#include <gtk/gtkmenuitem.h>
#include <gtk/gtkmenushell.h>
#include <gtk/gtkmenutoolbutton.h>
#include <gtk/gtkmessagedialog.h>
#include <gtk/gtkmisc.h>
#include <gtk/gtkmodules.h>
#include <gtk/gtkmountoperation.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtknumerableicon.h>
#include <gtk/gtkoffscreenwindow.h>
#include <gtk/gtkorientable.h>
#include <gtk/gtkpagesetup.h>
#include <gtk/gtkpapersize.h>
#include <gtk/gtkpaned.h>
#include <gtk/gtkprintcontext.h>
#include <gtk/gtkprintoperation.h>
#include <gtk/gtkprintoperationpreview.h>
#include <gtk/gtkprintsettings.h>
#include <gtk/gtkprogressbar.h>
#include <gtk/gtkradioaction.h>
#include <gtk/gtkradiobutton.h>
#include <gtk/gtkradiomenuitem.h>
#include <gtk/gtkradiotoolbutton.h>
#include <gtk/gtkrange.h>
#include <gtk/gtkrc.h>
#include <gtk/gtkrecentaction.h>
#include <gtk/gtkrecentchooser.h>
#include <gtk/gtkrecentchooserdialog.h>
#include <gtk/gtkrecentchoosermenu.h>
#include <gtk/gtkrecentchooserwidget.h>
#include <gtk/gtkrecentfilter.h>
#include <gtk/gtkrecentmanager.h>
#include <gtk/gtkscale.h>
#include <gtk/gtkscalebutton.h>
#include <gtk/gtkscrollable.h>
#include <gtk/gtkscrollbar.h>
#include <gtk/gtkscrolledwindow.h>
#include <gtk/gtkselection.h>
#include <gtk/gtkseparator.h>
#include <gtk/gtkseparatormenuitem.h>
#include <gtk/gtkseparatortoolitem.h>
#include <gtk/gtksettings.h>
#include <gtk/gtkshow.h>
#include <gtk/gtksizegroup.h>
#include <gtk/gtksizerequest.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtkspinner.h>
#include <gtk/gtkstatusbar.h>
#include <gtk/gtkstatusicon.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkstylecontext.h>
#include <gtk/gtkstyleproperties.h>
#include <gtk/gtkstyleprovider.h>
#include <gtk/gtkstyle.h>
#include <gtk/gtkswitch.h>
#include <gtk/gtksymboliccolor.h>
#include <gtk/gtktable.h>
#include <gtk/gtktearoffmenuitem.h>
#include <gtk/gtktextattributes.h>
#include <gtk/gtktextbuffer.h>
#include <gtk/gtktextbufferrichtext.h>
#include <gtk/gtktextchild.h>
#include <gtk/gtktextiter.h>
#include <gtk/gtktextmark.h>
#include <gtk/gtktexttag.h>
#include <gtk/gtktexttagtable.h>
#include <gtk/gtktextview.h>
#include <gtk/gtkthemingengine.h>
#include <gtk/gtktoggleaction.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtktoggletoolbutton.h>
#include <gtk/gtktoolbar.h>
#include <gtk/gtktoolbutton.h>
#include <gtk/gtktoolitem.h>
#include <gtk/gtktoolitemgroup.h>
#include <gtk/gtktoolpalette.h>
#include <gtk/gtktoolshell.h>
#include <gtk/gtktooltip.h>
#include <gtk/gtktestutils.h>
#include <gtk/gtktreednd.h>
#include <gtk/gtktreemodel.h>
#include <gtk/gtktreemodelfilter.h>
#include <gtk/gtktreemodelsort.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtktreesortable.h>
#include <gtk/gtktreestore.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreeviewcolumn.h>
#include <gtk/gtktypebuiltins.h>
#include <gtk/gtkuimanager.h>
#include <gtk/gtkvbbox.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkversion.h>
#include <gtk/gtkviewport.h>
#include <gtk/gtkvolumebutton.h>
#include <gtk/gtkvpaned.h>
#include <gtk/gtkvscale.h>
#include <gtk/gtkvscrollbar.h>
#include <gtk/gtkvseparator.h>
#include <gtk/gtkwidget.h>
#include <gtk/gtkwidgetpath.h>}
{$include gtk/gtkwindow.inc}

implementation

{ gtkwindow.inc }

function GTK_TYPE_WINDOW(): GType;
begin
  Result := gtk_window_get_type();
end;

{#define GTK_WINDOW(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_WINDOW, GtkWindow))
#define GTK_WINDOW_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_WINDOW, GtkWindowClass))
#define GTK_IS_WINDOW(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_WINDOW))
#define GTK_IS_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WINDOW))
#define GTK_WINDOW_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_WINDOW, GtkWindowClass))}

end.