# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, general utilities and exception types.
"""


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
            element = getattr(sender, "element", None)
            if element:
                name = f"{element.name}: {sender.fds_label or sender.label or sender.__class__.__name__}"
            else:
                name = getattr(sender, "name", None) or sender.__class__.__name__
            return f"ERROR: {name}: {self.msg}"
        else:
            return self.msg

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}(sender={self.sender}, msg={self.msg})"


class BFNotImported(BFException):
    """!
    Exception raised by BlenderFDS methods when some FDS importing fails.
    """

    pass
