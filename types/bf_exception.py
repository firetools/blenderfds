"""!
BlenderFDS, general utilities and exception types.
"""


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
            try:
                element = sender.element
                name = f"{element.name}: {sender.fds_label or sender.label or sender.__class__.__name__}"
            except:
                name = getattr(sender, "name", None) or sender.__class__.__name__
            return f"{name}: {self.msg}"
        else:
            return self.msg

    def to_fds(self):
        return f"! {self.__class__.__name__}:\n! {self.__str__()}\n\n"


class BFNotImported(BFException):
    """!
    Exception raised by BlenderFDS methods when some FDS importing fails.
    """

    pass


class BFWarning(BFException):
    """!
    Non blocking Exception raised by BlenderFDS methods.
    """

    pass
