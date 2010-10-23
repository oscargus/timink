# Timink - an Inkscape extension for digital timing diagrams.
# Copyright (C) 2010 Daniel Lutz
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import sys
import math
import re
try:
    import pygtk
    pygtk.require('2.0')
    import gtk
    import pango
except ImportError:
    raise RuntimeError("pygtk ist not available!")

from ti_info import EXTENSION_TITLE, VERSIONJOINT
from ti_version import VersionJoint
from ti_math import isfinite
from ti_signalspec import SignalClusterSpecParser
from ti_usrparams import UsrParams

class UserError(Exception):
    def __init__(self, msg, hint = None):
        self.msg = msg
        self.hint = hint
    def __str__(self):
        return repr(self.msg) + '\n' + repr(self.hint)

def escapeStringForUser(s):
    if s is None:
        s = '""'
    else:
        s = unicode(s).encode('unicode-escape').replace('"', '\\"')
        s = '"' + s + '"'
    return s

def printMessage(s, firstLinePrefix=''):
    lines = re.split('[\r\n]+', s)
    print >> sys.stderr, firstLinePrefix + lines[0]
    contLinePrefix = ' ' * len(firstLinePrefix)
    for line in lines[1:]:
        print >> sys.stderr, contLinePrefix + line

def printInfo(s):
    printMessage(s, 'Info: ')

def showErrorDlg(msg, hint = None, title = None):
    assert msg is not None and len(msg) > 0
    try:
        dlg = gtk.MessageDialog(type=gtk.MESSAGE_ERROR, buttons=gtk.BUTTONS_OK, message_format=msg)
        if title is None:
            title = EXTENSION_TITLE
        dlg.set_title(title)
        dlg.format_secondary_text(hint)
        dlg.run()
        dlg.destroy()
    except:
        printMessage(msg, 'Error: ')
        if hint is not None and len(hint) > 0:
            printMessage(hint, '       ')

# from http://people.gnome.org/~gianmt/busybox.py
# (found via http://stackoverflow.com/questions/1893748/pygtk-dynamic-label-wrapping):
class WrapLabel(gtk.Label):
    __gtype_name__ = 'WrapLabel'

    def __init__(self, str=None):
        gtk.Label.__init__(self)

        self.__wrap_width = 0
        self.layout = self.get_layout()
        self.layout.set_wrap(pango.WRAP_WORD_CHAR)

        if str != None:
            self.set_text(str)

        self.set_alignment(0.0, 0.0)

    def do_size_request(self, requisition):
        layout = self.get_layout()
        width, height = layout.get_pixel_size()
        requisition.width = 0
        requisition.height = height

    def do_size_allocate(self, allocation):
        gtk.Label.do_size_allocate(self, allocation)
        self.__set_wrap_width(allocation.width)

    def set_text(self, str):
        gtk.Label.set_text(self, str)
        self.__set_wrap_width(self.__wrap_width)

    def set_markup(self, str):
        gtk.Label.set_markup(self, str)
        self.__set_wrap_width(self.__wrap_width)

    def __set_wrap_width(self, width):
        if width == 0:
            return
        layout = self.get_layout()
        layout.set_width(width * pango.SCALE)
        if self.__wrap_width != width:
            self.__wrap_width = width
            self.queue_resize()


class LengthEditor(object):

    UNITNAMES = [ 'pt', 'px', 'mm', 'in' ] # defines display order
    UNITTITLEDICT = { # key: unit name. value: unit title (for combobox)
        'px': u'px',
        'pt': u'pt',
        'mm': u'mm',
        'in': u'in'
    }

    def __init__(self, initialValueStr, minValue, maxValue, dispDigitNo, tooltipStr):
        assert sorted(LengthEditor.UNITNAMES) == sorted(UsrParams.LENGTH_UNIT_DICT.keys())
        assert sorted(LengthEditor.UNITTITLEDICT.keys()) == sorted(LengthEditor.UNITNAMES)
        assert isfinite(minValue) and isfinite(maxValue)
        assert minValue < maxValue

        r = UsrParams.parseLength(initialValueStr)
        assert r is not None
        initialValue, initialUnitName, initialValueInPx = r
        del r

        self.valueSpinner = None
        self.unitCombo = None

        adj = gtk.Adjustment(value=initialValue,
                             lower=minValue, upper=maxValue, step_incr=0.1, page_incr=1.0)
        self.valueSpinner = gtk.SpinButton(adj, 0, digits=dispDigitNo)
        self.valueSpinner.set_numeric(True)
        self.valueSpinner.set_tooltip_text(tooltipStr)
        self.valueSpinner.show()

        self.unitCombo = gtk.combo_box_new_text()
        self.unitCombo.set_tooltip_text(
            u'Note: "px" means "user unit" in SVG terminology.\n'
          + u'Its size is a pixel width at a resolution of 90 dpi.')
        for unitName in LengthEditor.UNITNAMES:
            self.unitCombo.append_text(LengthEditor.UNITTITLEDICT[unitName])
        self.unitCombo.set_active(LengthEditor.UNITNAMES.index(initialUnitName))
        self.unitCombo.show()

        self.hbox = gtk.HBox()
        self.hbox.pack_start(self.valueSpinner, expand=True, fill=True)
        self.hbox.pack_start(self.unitCombo, expand=False, fill=False)

    def getWidget(self):
        return self.hbox

    def getLengthValue(self):
        s = None
        unitIndex = self.unitCombo.get_active()
        if unitIndex >= 0 and unitIndex < len(LengthEditor.UNITNAMES):
            s = str(self.valueSpinner.get_value()) + LengthEditor.UNITNAMES[unitIndex]
        if math.isnan(UsrParams.getLengthValue(s)):
            s = None
        return s

    def set_sensitive(self, sensitive):
        self.valueSpinner.set_sensitive(sensitive)
        self.unitCombo.set_sensitive(sensitive)


class SignalClusterEditor(object):
    """Editor for signal cluster specification and user parameters."""

    def __init__(self, signalClusterSpecStr, usrParams, existingVersionJoint, isNew, hasPositions):
        assert usrParams is not None
        assert usrParams.isValid()

        if signalClusterSpecStr is None:
            signalClusterSpecStr = u''
        if not isinstance(signalClusterSpecStr, unicode):
            signalClusterSpecStr = unicode(signalClusterSpecStr, errors='replace')
        self.signalClusterSpecStr = signalClusterSpecStr
        self.usrParams = usrParams
        self.existingVersionJoint = existingVersionJoint
        self.isNew = isNew
        self.hasPositions = hasPositions

        self.signalClusterSpecTextView = None
        self.unitTimeWidthEditor = None
        self.signalHeightEditor = None
        self.edgeTimeWidthEditor = None
        self.placmHomogRadioButton = None
        self.originDistXEditor = None
        self.originDistYEditor = None

    def _createSignalClusterSpecPage(self):
        signalClusterSpecScroll = gtk.ScrolledWindow()
        signalClusterSpecScroll.set_border_width(6)
        signalClusterSpecScroll.set_shadow_type(gtk.SHADOW_IN)
        signalClusterSpecScroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        signalClusterSpecTextView = gtk.TextView()
        signalClusterSpecTextView.set_accepts_tab(False)
        signalClusterSpecTextView.modify_font(pango.FontDescription(u'monospace'))
        signalClusterSpecTextView.set_tooltip_text(
            u'Signal cluster specification.\n' +
             'Each line containing at least one state character ("0", "1", "-", "x", "X" etc.) ' +
             'represents one signal. Spaces are ignored.')
        signalClusterSpecTextView.get_buffer().set_text(self.signalClusterSpecStr)
        signalClusterSpecScroll.add(signalClusterSpecTextView)
        signalClusterSpecTextView.show()
        signalClusterSpecScroll.show()

        exampleLabel = gtk.Label(
            u'01010101 00110011 000111000111 00001111\n' +
            u'xXxXxXxX xxXXxxXX xxxXXXxxxXXX xxxxXXXX\n' +
            u'0[X]1    0[XXxx]- 0(1001)(1010)-')
        exampleLabel.modify_font(pango.FontDescription(u'monospace'))
        exampleLabel.set_alignment(0.0, 0.0)
        exampleLabel.set_selectable(True)
        exampleLabel.show()
        exampleFrame = gtk.Frame()
        exampleFrame.set_border_width(6)
        exampleFrame.set_shadow_type(gtk.SHADOW_IN)
        exampleFrame.show()
        exampleFrame.add(exampleLabel)

        exampleExpander = gtk.Expander(u'Examples and templates')
        exampleExpander.add(exampleFrame)
        exampleExpander.show()

        signalClusterSpecPageVBox = gtk.VBox()
        signalClusterSpecPageVBox.pack_start(signalClusterSpecScroll, expand=True, fill=True)
        signalClusterSpecPageVBox.pack_start(exampleExpander, expand=False, fill=True)
        signalClusterSpecPageVBox.show()

        self.signalClusterSpecTextView = signalClusterSpecTextView

        return signalClusterSpecPageVBox

    def _createLayoutPage(self):
        table = gtk.Table(rows=2, columns=3, homogeneous=False)
        table.set_border_width(6)
        table.set_col_spacing(column=0, spacing=12)
        table.show()

        def appendToTable(table, labelStr, editor, startRow):
            label = gtk.Label(labelStr)
            label.set_alignment(1.0, 0.0)
            table.attach(label, 0, 1, startRow, startRow + 1, yoptions=0)
            table.attach(editor.getWidget(), 1, 2, startRow, startRow + 1, yoptions=0)

        editor = LengthEditor(
            initialValueStr=self.usrParams.unitTimeWidth,
            minValue=0.001, maxValue=100.0, dispDigitNo=3,
            tooltipStr=u'Width of unit time (along the t axis, before transformations).\n' +
                       u'Unit time: the time difference between adjacent states in a signal specification.')
        appendToTable(table, u'Unit time width:', editor, 0)
        self.unitTimeWidthEditor = editor

        editor = LengthEditor(
            initialValueStr=self.usrParams.signalHeight,
            minValue=0.001, maxValue=100.0, dispDigitNo=3,
            tooltipStr=u'Height of signal graph along the s axis (before transformations)..')
        appendToTable(table, u'Signal height:', editor, 1)
        self.signalHeightEditor = editor

        editor = LengthEditor(
            initialValueStr=self.usrParams.edgeTimeWidth,
            minValue=0.0, maxValue=100.0, dispDigitNo=3,
            tooltipStr=u'Width of rising or falling edge (0% to 100%) along the s axis (before transformations).')
        appendToTable(table, u'Edge time width:', editor, 2)
        self.edgeTimeWidthEditor = editor

        signalGeomFrame = gtk.Frame(label=u'Signal Dimensions')
        signalGeomFrame.set_border_width(6)
        signalGeomFrame.add(table)
        signalGeomFrame.show_all()

        placmIndivRadioButton = gtk.RadioButton(None, u'Individual')
        placmHomogRadioButton = gtk.RadioButton(placmIndivRadioButton, u'homogeneous')
        self.placmHomogRadioButton = placmHomogRadioButton

        table = gtk.Table(rows=2, columns=3, homogeneous=False)
        table.set_col_spacing(column=0, spacing=12)

        editor = LengthEditor(
            initialValueStr=self.usrParams.originDistX,
            minValue=-100.0, maxValue=100.0, dispDigitNo=3,
            tooltipStr=u'Horizontal distance between the origins of adjacent signals (before transformations).')
        appendToTable(table, u'x distance:', editor, 0)
        self.originDistXEditor = editor

        editor = LengthEditor(
            initialValueStr=self.usrParams.originDistY,
            minValue=0.001, maxValue=100.0, dispDigitNo=3,
            tooltipStr=u'Vertical distance between the origins of adjacent signals (before transformations).')
        appendToTable(table, u'y distance:', editor, 1)
        self.originDistYEditor = editor

        tableAlignm = gtk.Alignment(xalign=0.0, yalign=0.0, xscale=1.0, yscale=1.0)
        tableAlignm.set_padding(padding_top=0, padding_bottom=6, padding_left=24, padding_right=6)
        tableAlignm.add(table)
        placmHomogVBox = gtk.VBox()
        placmHomogVBox.pack_start(placmHomogRadioButton, expand=False, fill=False)
        placmHomogVBox.pack_start(tableAlignm, expand=False, fill=False)
        radioGroupVBox = gtk.VBox()
        radioGroupVBox.pack_start(placmIndivRadioButton, expand=False, fill=False)
        radioGroupVBox.pack_start(placmHomogVBox, expand=False, fill=False)
        placmIndivRadioButton.set_tooltip_text(
            u'Keep placement of all existing signals ' +
            u'and determine origin of new signals by linear interpolation/extrapolation ' +
            u'from the origins of adjacent existing signals.\n' +
            u'Needs at least two existing signals.')
        placmHomogVBox.set_tooltip_text(
            u'Place all signals such that the distance between all origins of adjacent signals is the same.\n' +
            u'The origin of the first existing signal remains unchanged.')

        placmHomogRadioButton.connect("toggled", self._cb_radioButtonToggled, placmHomogRadioButton)

        if not self.hasPositions:
            placmHomogRadioButton.set_active(True)
            placmIndivRadioButton.set_sensitive(False)
            placmHomogRadioButton.set_sensitive(False)
        elif self.usrParams.placementMethod == u'homogeneous':
            placmHomogRadioButton.set_active(True)
        else:
            placmIndivRadioButton.set_active(True)

        signalLayoutFrame = gtk.Frame(label=u'Signal Placement')
        signalLayoutFrame.set_border_width(6)
        signalLayoutFrame.add(radioGroupVBox)
        signalLayoutFrame.show_all()

        layoutVBox = gtk.VBox()
        layoutVBox.pack_start(signalGeomFrame, expand=False, fill=False)
        layoutVBox.pack_start(signalLayoutFrame, expand=False, fill=False)
        layoutVBox.show()

        return layoutVBox

    def _createWarningArea(self, labelStr):
        warningHBox = gtk.HBox()
        warningHBox.set_border_width(6)
        warningHBox.set_spacing(12)
        icon = gtk.Image()
        icon.set_from_stock(gtk.STOCK_DIALOG_WARNING, gtk.ICON_SIZE_MENU)
        icon.set_alignment(0.0, 0.5)
        textLabel = WrapLabel(labelStr)
        textLabel.set_alignment(0.0, 0.5)
        textLabel.set_line_wrap(True)
        warningHBox.pack_start(icon, expand=False, fill=False)
        warningHBox.pack_start(textLabel, expand=True, fill=True)
        warningHBox.show_all()
        return warningHBox

    def _createOptionalVersionWarningArea(self, existingVersionJoint, runningVersionJoint):
        """
        Compares tho versions and creates a warning area with a description of the difference,
        if the differ.

        existingVersionJoint: version (VersionJoint object) used for creation of the existing Timink object
        runningVersionJoint:  version (VersionJoint object) of running Timink

        Returns: None or the widget of the warning area
        """
        warningArea = None
        if existingVersionJoint is not None and runningVersionJoint is not None and existingVersionJoint != runningVersionJoint:
            if existingVersionJoint.model != runningVersionJoint.model:
                assert len(existingVersionJoint.model.components) == 2
                assert len(runningVersionJoint.model.components) == 2
                if existingVersionJoint.model.components[0] != runningVersionJoint.model.components[0]:
                    labelStr = u'This object was created with Timink {rv}, ' \
                             + u'which uses a different (not fully compatible) SVG representation of the Timink object.\n\n' \
                             + u'This object will be changed to the SVG representation of the running version.\n' \
                             + u'When edited with the original version of Timink, some elements will probably not be updated.'
                elif existingVersionJoint.model < runningVersionJoint.model:
                    labelStr = u'This object was created with Timink {rv}.' \
                             + u'The running version uses additional elements / attribute in its SVG representation of the Timink object.\n\n' \
                             + u'This object will be upgraded to the SVG representation of the running version.\n' \
                             + u'When edited with the original version of Timink, the additional elements remain untouched.'
                else:
                    labelStr = u'This object was created with Timink {rv}, '\
                             + u'which uses additional elements / attributes in its SVG representation of the Timink object. ' \
                             + u'These are unsupported by the running Timink version.\n\n' \
                             + u'This object will be downgraded to the SVG representation of the running version.\n' \
                             + u'Unsupported elements remain untouched.'

            else:
                labelStr = u'This object was created with Timink {rv}, which is compatible with the running version.'

            labelStr = labelStr.format(rv=str(existingVersionJoint.extension))
            warningArea = self._createWarningArea(labelStr)
        return warningArea

    def _cb_radioButtonToggled(self, widget, data=None):
        radioButton = data
        if radioButton == self.placmHomogRadioButton:
            sensitive = radioButton.get_active()
            self.originDistXEditor.getWidget().get_parent().set_sensitive(sensitive)

    def run(self):
        """
        Returns a None -- if the user pressed 'Cancel' -- or
        (signalClusterSpecStr, usrParams)  -- if the user pressed 'Ok'.

        signalClusterSpecStr is a valid signal cluster specification
        and usrParams is a valid UsrParams object.
        """

        dlgTitle = u'{title} {version}'.format(title=EXTENSION_TITLE, version=VERSIONJOINT.extension)
        dlg = gtk.Dialog(dlgTitle, None, gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT) # ???
        dlg.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT)
        if self.isNew:
            newButton = dlg.add_button(gtk.STOCK_NEW, gtk.RESPONSE_ACCEPT)
            newButton.set_tooltip_text(u'Inserts a Timink object in the center of the current viewport.\n' +
                                       u'(It is usually more efficient to start with a copy of a template drawing.)')
        else:
            dlg.add_button(gtk.STOCK_APPLY, gtk.RESPONSE_ACCEPT)

        paramNotebook = gtk.Notebook()
        signalClusterSpecPage = self._createSignalClusterSpecPage()
        signalClusterSpecPageIndex = paramNotebook.append_page(signalClusterSpecPage, gtk.Label(u'Signal cluster'))
        del signalClusterSpecPage
        layoutPage = self._createLayoutPage()
        layoutPageIndex = paramNotebook.append_page(layoutPage, gtk.Label(u'Layout'))
        del layoutPage
        paramNotebook.show()

        warningArea = self._createOptionalVersionWarningArea(self.existingVersionJoint, VERSIONJOINT)
        if warningArea is not None:
            dlg.vbox.pack_start(warningArea, expand=False, fill=False)
        dlg.vbox.pack_start(paramNotebook, expand=True, fill=True, padding=6)

        signalClusterSpecStr = None
        usrParams = None

        ok = False

        self.signalClusterSpecTextView.grab_focus()
        response = dlg.run()
        while response == gtk.RESPONSE_ACCEPT:

            textBuffer = self.signalClusterSpecTextView.get_buffer()
            signalClusterSpecStr = textBuffer.get_text(textBuffer.get_start_iter(), textBuffer.get_end_iter())
            signalClusterSpecStr = signalClusterSpecStr.decode('utf-8')

            usrParams = UsrParams()
            usrParams.unitTimeWidth = self.unitTimeWidthEditor.getLengthValue()
            usrParams.signalHeight = self.signalHeightEditor.getLengthValue()
            usrParams.edgeTimeWidth = self.edgeTimeWidthEditor.getLengthValue()
            usrParams.originDistX = self.originDistXEditor.getLengthValue()
            usrParams.originDistY = self.originDistYEditor.getLengthValue()
            if self.placmHomogRadioButton.get_active():
                usrParams.placementMethod = u'homogeneous'
            else:
                usrParams.placementMethod = u'individual'

            ok = True
            if not SignalClusterSpecParser.isValid(signalClusterSpecStr):

                invCharPosList = SignalClusterSpecParser.getInvalidCharPos(signalClusterSpecStr)
                nonmatchingParenthesisIndex = SignalClusterSpecParser.getFirstNonmatchingParenthesis(signalClusterSpecStr)
                invMultiStateRange = SignalClusterSpecParser.getFirstInvalidMultiPathState(signalClusterSpecStr)
                invBreakRange = SignalClusterSpecParser.getFirstInvalidBreak(signalClusterSpecStr)
                if len(invCharPosList) > 0:
                    showErrorDlg(u'Invalid character in signal cluster specification.',
                                 u'The valid characters are:\n'
                               + u'white spaces, "0", "1", "-", "x", "X", "y", "Y", "_", "(",  ")", "[", "]')
                    assert invCharPosList[0] < len(signalClusterSpecStr)
                    i = invCharPosList[0]
                    j = i + 1
                    while j in invCharPosList:
                        j = j + 1
                    selStart = textBuffer.get_iter_at_offset(i)
                    selEnd = textBuffer.get_iter_at_offset(j)
                    textBuffer.select_range(selStart, selEnd)
                elif nonmatchingParenthesisIndex is not None:
                    showErrorDlg(u'Non-matching parenthesis (or bracket).')
                    selStart = textBuffer.get_iter_at_offset(nonmatchingParenthesisIndex)
                    selEnd = textBuffer.get_iter_at_offset(nonmatchingParenthesisIndex + 1)
                    textBuffer.select_range(selStart, selEnd)
                elif invMultiStateRange is not None:
                    showErrorDlg(u'Invalid multi state.',
                                 u'A valid multi state consists of any number (at least one) '
                               + u'of "0", "1", "-" in parentheses.\n\n'
                               + u'Example: "(0-)".')
                    selStart = textBuffer.get_iter_at_offset(invMultiStateRange[0])
                    selEnd = textBuffer.get_iter_at_offset(invMultiStateRange[1])
                    textBuffer.select_range(selStart, selEnd)
                elif invBreakRange is not None:
                    showErrorDlg(u'Invalid break.',
                                 u'A valid break is a sequence of "_" and white space between states.\n'
                               + u'Breaks at the beginning or at the end of a signal specification are invalid.')
                    selStart = textBuffer.get_iter_at_offset(invBreakRange[0])
                    selEnd = textBuffer.get_iter_at_offset(invBreakRange[1])
                    textBuffer.select_range(selStart, selEnd)
                else:
                    showErrorDlg(u'Please enter a signal cluster specification.',
                                 u'Don\'t know what is wrong (this is a bug; please report).')
                self.signalClusterSpecTextView.scroll_mark_onscreen(textBuffer.get_insert())
                paramNotebook.set_current_page(signalClusterSpecPageIndex)
                self.signalClusterSpecTextView.grab_focus()
                signalClusterSpecStr = None
                ok = False

            elif not usrParams.isValid():

                showErrorDlg(u'Invalid parameter.', 'Don\'t know which one (this is a bug; please report).')
                paramNotebook.set_current_page(layoutPageIndex)
                usrParams = None
                ok = False

            if ok:
                signalClusterSpecStr = SignalClusterSpecParser.cleanUp(signalClusterSpecStr)
                break
            else:
                response = dlg.run()

        dlg.destroy()

        # clipboard is destroyed, gtk.clipboard_get().store() doesn't help ???

        r = None
        if ok:
            assert SignalClusterSpecParser.isValid(signalClusterSpecStr)
            assert usrParams.isValid()
            r = (signalClusterSpecStr, usrParams)
        return r

assert escapeStringForUser(None) == '""'
assert escapeStringForUser('') == '""'
assert escapeStringForUser('a\x00"b\'') == '"a\\x00\\"b\'"'
assert escapeStringForUser(u'a\x00"b\'') == '"a\\x00\\"b\'"'
