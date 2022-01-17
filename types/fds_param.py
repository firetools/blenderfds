"""!
BlenderFDS, Blender representations of a FDS parameter.
"""

import re, logging
from .bf_exception import BFException, is_iterable

log = logging.getLogger(__name__)


class FDSParam:
    """!
    Datastructure representing an FDS parameter.
    """

    def __init__(
        self,
        fds_label=None,
        value=None,
        precision=3,
        exponential=False,
        msg=None,
        msgs=None,
    ) -> None:
        """!
        Class constructor.
        @param fds_label: namelist parameter label.
        @param value: parameter value of any type.
        @param precision: float precision, number of decimal digits.
        @param exponential: if True sets exponential representation of floats.
        @param msg: comment message string.
        @param msgs: list of comment message strings.
        """
        ## parameter label
        self.fds_label = fds_label
        ## parameter value of any type
        self.value = value
        ## float precision, number of decimal digits
        self.precision = precision
        ## if True sets exponential representation of floats
        self.exponential = exponential
        ## list of comment message strings
        self.msgs = msgs and list(msgs) or list()
        self.msg = msg

    def __str__(self) -> str:
        res = self.to_fds()
        if len(res) > 80:
            return res[:37] + " ... " + res[-37:]
        return res

    def _get_formatted_values(self) -> tuple:
        """!
        Return a tuple of FDS formatted values or an empty tuple, eg. "'Test1'","'Test2'".
        """
        values = self.values
        if not values:
            return tuple()
        elif isinstance(values[0], float):
            if self.exponential:
                return tuple(f"{v:.{self.precision}E}" for v in values)
            else:
                return tuple(
                    f"{round(v,self.precision):.{self.precision}f}" for v in values
                )
        elif isinstance(values[0], str):
            return tuple("'" in v and f'"{v}"' or f"'{v}'" for v in values)
        elif isinstance(values[0], bool):  # always before int
            return tuple(v and "T" or "F" for v in values)
        elif isinstance(values[0], int):
            return tuple(str(v) for v in values)
        else:
            raise ValueError(f"Unknown value type <{self.value}>")

    @property
    def values(self) -> tuple:
        """!
        Return an iterable self.value.
        """
        if self.value is None:
            return tuple()
        elif not is_iterable(self.value):
            return tuple((self.value,))
        else:
            return self.value

    @property
    def msg(self) -> str:
        """!
        Return all self.msgs in one line.
        """
        return " | ".join(m for m in self.msgs if m)  # protect from None

    @msg.setter
    def msg(self, value) -> None:
        """!
        Append msg to self.msgs.
        """
        if value:
            self.msgs.append(value)

    def to_fds(self, context=None) -> str:
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        v = ",".join(self._get_formatted_values())
        if self.fds_label:
            if v:  # "ABC=1,2,3"
                return f"{self.fds_label}={v}"
            else:  # "ABC"
                return self.fds_label

    _RE_DECIMAL_POS = r"\.([0-9]+)"  # decimal positions

    _RE_SCAN_DECIMAL = re.compile(
        _RE_DECIMAL_POS, re.VERBOSE | re.DOTALL | re.IGNORECASE
    )

    _RE_INTEGER = r"([0-9]*)\.?[0-9]*[eE]"  # integer postions of exp notation

    _RE_SCAN_INTEGER = re.compile(_RE_INTEGER, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _RE_SCAN_VALUES = re.compile(
        r"""'.*?'|".*?"|[^,\s\t]+""", re.VERBOSE | re.DOTALL | re.IGNORECASE
    )

    def from_fds(self, f90) -> None:
        """!
        Import from FDS formatted string, on error raise BFException.
        @param f90: FDS formatted string containing value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        # Remove trailing spaces and newlines, then scan values
        f90c = " ".join(f90.strip().splitlines())
        values = re.findall(self._RE_SCAN_VALUES, f90c)
        # Eval values
        for i, v in enumerate(values):
            if v in (".TRUE.", "T"):
                values[i] = True
            elif v in (".FALSE.", "F"):
                values[i] = False
            else:
                try:
                    values[i] = eval(v)
                except Exception as err:
                    msg = f"Error parsing value <{v}> in <{self.fds_label}={f90c}>:\n<{err}>"
                    raise BFException(self, msg)
        # Post treatment of float
        if isinstance(values[0], float):  # first value is a float
            # Get precision
            match = re.findall(self._RE_DECIMAL_POS, f90c)
            self.precision = match and max(len(m) for m in match) or 1
            # Get exponential
            match = re.findall(self._RE_INTEGER, f90c)
            if match:
                self.exponential = True
                self.precision += max(len(m) for m in match) - 1
        # Record in self.value
        if len(values) == 1:
            self.value = values[0]
        else:
            self.value = values
