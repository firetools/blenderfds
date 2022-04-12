"""!
BlenderFDS, Blender list of FDS namelists or parameters.
"""

import re, logging
from ..config import MAXLEN, INDENT
from .bf_exception import BFException

log = logging.getLogger(__name__)


class FDSList(list):
    """!
    List of FDSParam, FDSNamelist, FDSList instances.
    """

    def __init__(self, iterable=(), f90_namelists=None, msgs=(), msg=None, header=None) -> None:
        """!
        Class constructor.
        @param iterable: iterable content of any type.
        @param msgs: list of comment message strings.
        @param msg: comment message string.
        """
        super().__init__(iterable)
        ## list of comment message strings.
        self.msgs = list(msgs)
        if msg:
            self.msgs.append(msg)
        ## header string, that appears only if self is not empty
        self.header = header
        ## set iterable from f90_namelists
        if f90_namelists:
            self.from_fds(f90_namelists=f90_namelists)


    def __repr__(self) -> str:
        iterable = ",".join(str(item) for item in self)
        return f"{self.__class__.__name__}({iterable})"

    def __contains__(self, fds_label) -> bool:
        """!Check if fds_label is in iterable."""
        return bool(self.get_fds_label(fds_label=fds_label, remove=False))

    def _classify_items(self, inv_ps=None, multi_ps=None, add_ns=None):
        """!
        Classify self items in params msgs, invariant params,
        multi params, additional namelists.
        """
        if inv_ps is None:  # first run
            # init invariant params, multi params, additional namelists
            inv_ps, multi_ps, add_ns = FDSList(), None, FDSList()
        for item in self:
            match item:
                case None:
                    continue
                case FDSParam():  # invariant params
                    inv_ps.append(item)
                case FDSMulti():  # multi param (one only)
                    if multi_ps:
                        raise Exception(f"One only FDSMulti allowed in: {self!r}")
                    multi_ps = item
                case FDSNamelist():  # additional namelists
                    add_ns.append(item)
                case FDSList():  # recurse
                    inv_ps, multi_ps, add_ns = item._classify_items(
                        inv_ps=inv_ps, multi_ps=multi_ps, add_ns=add_ns)
                case _:
                    raise ValueError(f"Unrecognized type of <{item!r}> in <{self!r}>")
        return inv_ps, multi_ps, add_ns

    def get_fds_label(self, fds_label=None, remove=False):
        """!
        Get first occurence of fds_label.
        @param fds_label: FDS label.
        @param remove: remove found item.
        @return item or None.
        """
        for i, item in enumerate(self):
            if item is None:
                continue
            try:
                label = item.fds_label
            except AttributeError:
                # item is FDSList, recurse
                found = item.get_fds_label(fds_label=fds_label, remove=remove)
                if found:
                    return found
            else:
                # item is FDSParam or FDSNamelist, check
                if fds_label:
                    if label == fds_label:
                        # first occurrence w fds_label
                        if remove:
                            self.pop(i)
                        return item
                else:
                    # first occurrence
                    if remove:
                        self.pop(i)
                    return item

    def to_string(self) -> str:
        """!
        Return the FDS formatted string.
        """
        lines = self.msgs
        lines.extend(item.to_string() for item in self)
        string = "\n".join(l for l in lines if l)
        if string and self.header:
            string = f"{self.header}\n{string}"
        return string

    _RE_SCAN = re.compile(  # scan f90_namelists
        r"""
        ^&                    # & at the beginning of the line
        ([A-Z]+[A-Z0-9]*)     # namelist label (group 0)
        [,\s\t]*              # 0+ separator, greedy
        (                     # namelist params (group 1)
        (?:'.*?'|".*?"|.*?)*  # 0+ any char, protect quoted strings, greedy
        ) 
        [,\s\t]*              # 0+ separator, greedy
        /                     # / end char
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE | re.MULTILINE,
    )  # MULTILINE, so that ^ is the beginning of each line

    def from_fds(self, f90_namelists) -> None:  # TODO add comments to msgs
        """!
        Fill self from FDS file or text, on error raise BFException.
        @param f90_namelists: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        self.clear()
        for match in re.finditer(self._RE_SCAN, f90_namelists):
            label, f90_params = match.groups()
            fds_namelist = FDSNamelist(fds_label=label)
            fds_namelist.from_fds(f90_params=f90_params)
            self.append(fds_namelist)


class FDSMulti(FDSList):
    """!
    FDSList of FDSList instances.
    """

    pass

class FDSNamelist(FDSList):
    """!
    List representing an FDS namelist.
    """

    def __init__(self, fds_label, iterable=(), f90_params=None, msgs=(), msg=None) -> None:
        """!
        Class constructor.
        @param fds_label: fds label of group (eg. namelist or param).
        @param iterable: iterable content of any type.
        @param f90_param: init from f90 param string.
        @param msgs: list of comment message strings.
        @param msg: comment message string.
        """
        super().__init__(iterable=iterable, msgs=msgs, msg=msg)
        ## fds label of group (eg. namelist or param).
        self.fds_label = fds_label
        ## set iterable from f90_params
        if f90_params:
            self.from_fds(f90_params=f90_params)

    def __repr__(self) -> str:
        iterable = ",".join(str(item) for item in self)
        return f"{self.__class__.__name__}({self.fds_label}, {iterable})"

    def __bool__(self):
        return bool(self.fds_label or self.msgs)

    def _remove_duplicated_ps_from_inv_ps(self, inv_ps, multi_ps):
        """!
        Remove params from inv_ps that are duplicated in multi_ps
        (eg. ID, IJK, XB, ...)
        """
        if multi_ps:  # protect from empty
            for p in multi_ps[0]:
                inv_ps.get_fds_label(fds_label=p.fds_label, remove=True)
        return inv_ps

    def _generate_many_fds_namelists(self, inv_ps, multi_ps, add_ns):
        """!
        Generate many fds namelists, if needed.
        """
        fds_list = FDSList()
        if multi_ps:
            fds_list.msgs.extend(self.msgs)
            fds_list.msgs.extend(multi_ps.msgs)
            for mp in multi_ps:
                # Add its multi params (ID comes always first)
                fds_namelist = FDSNamelist(fds_label=self.fds_label, iterable=mp)
                # Add invariant params and no msg
                fds_namelist.extend(inv_ps)
                # Append one of multi (eg. an OBST voxel) to the list
                fds_list.append(fds_namelist)
        else:
            fds_namelist = FDSNamelist(
                fds_label=self.fds_label, iterable=inv_ps, msgs=self.msgs
            )
            fds_list.append(fds_namelist)  # append self
        fds_list.extend(add_ns)  # append additional namelists (eg. MOVE)
        return fds_list

    def _append_word(self, lines, word, separator=" ", force_break=False) -> list:
        """!
        Append word to the last of lines, generate newline and ident if needed.
        """
        if not force_break and len(lines[-1]) + len(separator) + len(word) <= MAXLEN:
            lines[-1] += separator + word  # current line
        else:
            lines.append(" " * INDENT + word)  # new line w indent, wo separator
        return lines

    def _to_one_string(self, inv_ps):
        """!
        Format self, a namelist that generates only one namelist.
        """
        lines = self.msgs
        lines.extend(m for p in self if p for m in p.msgs)  # self params msgs
        lines.append(f"&{self.fds_label}")  # start namelist, eg. "&OBST"
        for p in inv_ps:  # add params, use inv_ps because it is flattened
            if p is None:
                continue
            fds_label, fds_values = p.fds_label, p.to_strings()
            if not fds_values:  # fds_label only provided, probably preformatted
                lines = self._append_word(lines, word=fds_label)
            else:  # fds_label and its values provided
                word = f"{fds_label}={','.join(fds_values)}"
                if len(word) <= MAXLEN - INDENT or len(fds_values) == 1:
                    # short param, keep together
                    lines = self._append_word(lines, word=word)
                else:
                    # long param, split in lines
                    lines = self._append_word(
                        lines, word=f"{fds_label}={fds_values[0]},"
                    )  # first
                    for v in fds_values[1:-1]:
                        lines = self._append_word(lines, word=f"{v},", separator="")
                    lines = self._append_word(
                        lines, word=f"{fds_values[-1]}", separator=""
                    )  # last
        lines[-1] += " /"  # close
        return "\n".join(lines)

    def to_string(self) -> str:
        inv_ps, multi_ps, add_ns = self._classify_items()
        # Many namelists to be generated? recurse
        if multi_ps or add_ns:
            inv_ps = self._remove_duplicated_ps_from_inv_ps(
                inv_ps=inv_ps, multi_ps=multi_ps
            )
            fds_list = self._generate_many_fds_namelists(
                inv_ps=inv_ps, multi_ps=multi_ps, add_ns=add_ns
            )
            return fds_list.to_string()
        # Otherwise format self
        return self._to_one_string(inv_ps=inv_ps)

    _RE_SCAN = re.compile(  # scan f90_params
        r"""
        ([A-Z][A-Z0-9_\(\):,]*?)  # label (group 0)
        [,\s\t]*                  # 0+ separators
        =                         # = sign
        [,\s\t]*                  # 0+ separators
        (                         # value (group 1)
            (?:'.*?'|".*?"|.+?)*?     # 1+ any char, protect str, not greedy
                (?=                       # end previous match when:
                    (?:                       # there is another label:
                        [,\s\t]+                  # 1+ separators
                        [A-Z][A-Z0-9_\(\):,]*?    # label
                        [,\s\t]*                  # 0+ separators
                        =                         # = sign
                    )
                |                         # or
                    $                         # it is end of line
                )
        )
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE,
    )  # no MULTILINE, so that $ is the end of the file

    def from_fds(self, f90_params) -> None:
        """!
        Fill self from FDS formatted string of parameters, on error raise BFException.
        @param f90_params: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        self.clear()
        f90_params = " ".join(
            f90_params.strip().splitlines()
        )  # rm trailing spaces and newlines
        for match in re.finditer(self._RE_SCAN, f90_params):
            label, f90_value = match.groups()
            self.append(FDSParam(fds_label=label, f90_value=f90_value))


class FDSParam(FDSList):
    """!
    List representing an FDS parameter.
    """

    def __init__(self, fds_label, iterable=(), value=None, f90_value=None, precision=3, exponential=False, msgs=(), msg=None) -> None:
        """!
        Class constructor.
        @param fds_label: namelist parameter label.
        @param iterable: iterable of values of any type (inherited from List).
        @param value: parameter value of any type.
        @param f90_value: init from f90 value string.
        @param precision: float precision, number of decimal digits.
        @param exponential: if True sets exponential representation of floats.
        @param msgs: list of comment message strings.
        @param msg: comment message string.
        """
        super().__init__(iterable=iterable, msgs=msgs, msg=msg)
        ## fds label of group (eg. namelist or param).
        self.fds_label = fds_label
        ## float precision, number of decimal digits
        self.precision = precision
        ## if True sets exponential representation of floats
        self.exponential = exponential
        # Set parameter value from f90_value
        if f90_value:
            self.from_fds(f90_value=f90_value)
        else:
            self.set_value(value=value)

    def __repr__(self) -> str:
        iterable = ",".join(str(item) for item in self)
        return f"{self.__class__.__name__}({self.fds_label}, {iterable})"


    def get_value(self):
        """!
        Return value from self.values.
        """
        if not self:
            return None
        if len(self) == 1:
            return self[0]
        return tuple(self)

    def set_value(self, value=None) -> None:
        """!
        Set self.values from value.
        """
        self.clear()
        match value:
            case None:
                pass
            case int()|float()|str():
                self.append(value)
            case _:
                self.extend(value)

    def to_strings(self) -> tuple:
        """!
        Return a tuple of FDS formatted values or an empty tuple, eg. "'Test1'","'Test2'".
        """
        if not len(self):  # no content
            return tuple()
        match self[0]:
            case float():
                if self.exponential:
                    return tuple(f"{v:.{self.precision}E}" for v in self)
                else:
                    return tuple(
                        f"{round(v,self.precision):.{self.precision}f}" for v in self
                    )
            case str():
                return tuple("'" in v and f'"{v}"' or f"'{v}'" for v in self)
            case bool():  # always before int
                return tuple(v and "T" or "F" for v in self)
            case int():
                return tuple(str(v) for v in self)
            case _:
                raise ValueError(f"Unknown value type <{self[0]}> in {self}")

    def to_string(self) -> str:
        """!
        Return the FDS formatted string.
        """
        if self.fds_label:
            v = ",".join(self.to_strings())
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

    def from_fds(self, f90_value) -> None:
        """!
        Import from FDS formatted string, on error raise BFException.
        @param f90_value: FDS formatted string containing value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        self.clear()
        # Remove trailing spaces and newlines, then scan values
        f90_value = " ".join(f90_value.strip().splitlines())
        values = re.findall(self._RE_SCAN_VALUES, f90_value)
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
                    msg = f"Error parsing value <{v}> in <{self.fds_label}={f90_value}>:\n<{err}>"
                    raise BFException(self, msg)
        # Post treatment of float
        if isinstance(values[0], float):  # first value is a float
            # Get precision
            match = re.findall(self._RE_DECIMAL_POS, f90_value)
            self.precision = match and max(len(m) for m in match) or 1
            # Get exponential
            match = re.findall(self._RE_INTEGER, f90_value)
            if match:
                self.exponential = True
                self.precision += max(len(m) for m in match) - 1
        # Record
        self.extend(values)
        print(self.fds_label, f90_value, self.precision, self.exponential)  # FIXME
