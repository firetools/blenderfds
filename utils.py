# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.


def is_iterable(var):
    """!
    Check if var is iterable or not

    >>> is_iterable("hello"), is_iterable((1,2,3)), is_iterable({1,2,3})
    (False, True, True)
    """
    # A str is iterable in Py... not what I want
    if isinstance(var, str):
        return False
    # Let's try and fail nicely
    try:
        for _ in var:
            break
    except TypeError:
        return False
    return True

    # BF specific exception


class BFException(Exception):
    """!
    Exception raised by BlenderFDS methods in case of an error.
    """

    def __init__(self, sender=None, msg=None):
        """!
        Class constructor.
        @param sender: the object that generates the exception
        @param msg: exception message
        """
        ## The object that generates the exception
        self.sender = sender
        ## Exception message
        self.msg = msg or "Unknown error"

    def __str__(self):
        sender = self.sender
        if sender:
            try:  # FIXME add element type
                element = sender.element
                name = f"{element.name}: {sender.fds_label or sender.label or sender.__class__.__name__}"
            except:
                name = getattr(sender, "name", None) or sender.__class__.__name__
            return f"{name}: {self.msg}"
        else:
            return self.msg


class BFNotImported(BFException):
    """!
    Exception raised by BlenderFDS methods when some FDS importing fails.
    """

    pass

