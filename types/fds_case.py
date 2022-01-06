"""!
BlenderFDS, Blender representations of FDS entities.
"""

import re, logging

from .bf_exception import BFException, is_iterable
from .. import utils

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
        f90=None,
    ):
        """!
        Class constructor.
        @param fds_label: namelist parameter label.
        @param value: parameter value of any type.
        @param precision: float precision, number of decimal digits.
        @param exponential: if True sets exponential representation of floats.
        @param msg: comment message.
        @param f90: FDS formatted string of value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        ## parameter label
        self.fds_label = fds_label
        ## parameter value of any type
        self.value = value
        ## float precision, number of decimal digits
        self.precision = precision
        ## if True sets exponential representation of floats
        self.exponential = exponential
        ## comment message
        self.msg = msg
        # Fill self.value from f90 string
        if f90:
            self.from_fds(f90=f90)

    def __str__(self):
        res = self.to_fds()
        if len(res) > 80:
            return res[:37] + " ... " + res[-37:]
        return res

    def _get_formatted_values(self):
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
    def values(self):
        """!
        Return an iterable self.value.
        """
        if self.value is None:
            return tuple()
        elif not is_iterable(self.value):
            return tuple((self.value,))
        else:
            return self.value

    def to_fds(self, context=None):
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

    _re_decimal = r"\.([0-9]+)"  # decimal positions

    _scan_decimal = re.compile(_re_decimal, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _re_integer = r"([0-9]*)\.?[0-9]*[eE]"  # integer postions of exp notation

    _scan_integer = re.compile(_re_integer, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _scan_values = re.compile(
        r"""'.*?'|".*?"|[^,\s\t]+""", re.VERBOSE | re.DOTALL | re.IGNORECASE
    )

    def from_fds(self, f90):
        """!
        Import from FDS formatted string, on error raise BFException.
        @param f90: FDS formatted string containing value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        # Remove trailing spaces and newlines, then scan values
        f90c = " ".join(f90.strip().splitlines())
        values = re.findall(self._scan_values, f90c)
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
                    msg = f"Error while parsing value <{v}> in <{self.fds_label}={f90}>\n<{err}>"
                    raise BFException(self, msg)
        # Post treatment of float
        if isinstance(values[0], float):  # first value is a float
            # Get precision
            match = re.findall(self._re_decimal, f90c)
            self.precision = match and max(len(m) for m in match) or 1
            # Get exponential
            match = re.findall(self._re_integer, f90c)
            if match:
                self.exponential = True
                self.precision += max(len(m) for m in match) - 1
        # Record in self.value
        if len(values) == 1:
            self.value = values[0]
        else:
            self.value = values


class FDSNamelist:
    """!
    Datastructure representing an FDS namelist.
    """

    ## max number of columns of formatted output
    maxlen = 80  # TODO to config

    def __init__(self, fds_label=None, fds_params=None, msg=None, f90=None):
        """!
        Class constructor.
        @param fds_label: namelist group label.
        @param fds_params: list of FDSParam and additional FDSNamelist instances.
        @param msg: comment message.
        @param f90: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        ## namelist group label
        self.fds_label = fds_label
        ## list (single) of FDSParam and additional FDSNamelist,
        ## or list of list (multi) of FDSParam instances
        ## eg. (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...)
        self.fds_params = fds_params or list()
        ## comment message
        self.msg = msg
        # Fill self.fds_params from f90 string
        if f90:
            self.from_fds(f90=f90)

    def __str__(self):
        return self.to_fds()

    def get_by_label(self, fds_label, remove=False):
        """!
        Return the first FDSParam instance in list by its label.
        @param fds_label: namelist parameter label.
        @param remove: remove it from self
        @return None or FDSParam.
        """
        for res in self.fds_params:
            if res.fds_label == fds_label:
                if remove:
                    self.fds_params.remove(res)
                return res

    def to_fds(self, context=None):
        """!
        Return the FDS formatted string.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or a None.
        """
        msgs = self.msg and self.msg.splitlines() or list()
        # Classify parameters
        invps = list()  # invariant parameters
        multips = list()  # multi parameters
        addns = list()  # additional namelists
        # fds_params is always a tuple. Its elements can be:
        #   None, FDSParam, FDSNamelist,
        #   a tuple or FDSParam and FDSNamelist (many),
        #   or a tuple of tuple of FDSParam (multi)
        for p in self.fds_params:
            if not p:
                continue  # None or empty tuple
            elif isinstance(p, FDSParam):
                # Invariant parameter
                invps.append(p)
                msgs.append(p.msg)
            elif isinstance(p, FDSNamelist):
                # Additional namelist
                addns.append(p)
                msgs.append(p.msg)
            elif isinstance(p, tuple):
                if isinstance(p[0], tuple):
                    # Multi parameter
                    multips = p
                    msgs.extend(p0.msg for p0 in multips[0])
                else:
                    # Many parameters
                    for pp in p:
                        if isinstance(pp, FDSParam):
                            invps.append(pp)
                        if isinstance(pp, FDSNamelist):
                            addns.append(pp)
                        msgs.append(pp.msg)
            else:
                raise ValueError(f"Unrecognized type of <{p}>")
        # Treat invariant, many and multi parameters
        # nl = FDSParam, FDSParam, ...
        nls = list()  # list of nl
        if multips:
            # Remove parameters in invps that are duplicated in multips (eg. ID, IJK or XB)
            multips_fds_labels = [p.fds_label for p in multips[0]]
            invps = [p for p in invps if p.fds_label not in multips_fds_labels]
            # Add nl with one of multips + invps
            for multip in multips:
                nl = list(multip)
                nl.extend(invps)
                nls.append(nl)
        else:
            nls.append(invps)
        # Prepare message lines
        lines = list(f"! {m}" for m in msgs if m)  # all messages
        # Treat additional namelists first
        lines.extend(n.to_fds(context) for n in addns)
        # Prepare namelist lines
        if self.fds_label:
            for nl in nls:
                newline = False
                line = f"&{self.fds_label}"
                for p in nl:
                    if not p.fds_label:
                        continue
                    label = p.fds_label
                    vs = p._get_formatted_values()  # list of str
                    if not vs:  # no formatted values
                        if not newline and len(line) + 1 + len(label) <= self.maxlen:
                            # Parameter to the same line
                            newline = False
                            line += " " + label
                        else:
                            # Parameter to new line
                            lines.append(line)
                            line = "      " + label  # new line
                    else:  # available formatted values
                        v = ",".join(vs)  # values str
                        if (
                            not newline
                            and len(line) + 1 + len(label) + 1 + len(v) <= self.maxlen
                        ):
                            # Parameter to the same line
                            newline = False
                            line += " " + label + "=" + v
                        else:
                            # Parameter to new line
                            lines.append(line)
                            line = "      " + label + "="  # new line
                            if len(line) + len(v) <= self.maxlen:
                                # Formatted values do not need splitting
                                line += v
                            else:
                                # Formatted values need splitting
                                newline = True  # the following needs a new line
                                for v in vs:
                                    if len(line) + len(v) + 1 <= self.maxlen:
                                        line += v + ","
                                    else:
                                        lines.append(line)
                                        line = "        " + v + ","  # new line
                                line = line[:-1]  # remove last ","
                line += " /"
                lines.append(line)
        return "\n".join(lines)

    _scan_params = re.compile(
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

    def from_fds(self, f90):
        """!
        Import from FDS formatted string of parameters, on error raise BFException.
        @param f90: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        f90 = " ".join(f90.strip().splitlines())
        for match in re.finditer(self._scan_params, f90):
            label, f90_value = match.groups()
            self.fds_params.append(FDSParam(fds_label=label, f90=f90_value))


class FDSCase:
    """!
    Datastructure representing an FDS case.
    """

    def __init__(self, fds_namelists=None, msg=None, filepath=None, f90=None):
        """!
        Class constructor.
        @param fds_namelists: list of FDSNamelist instances.
        @param msg: comment message.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        ## list of FDSNamelist instances
        self.fds_namelists = fds_namelists or list()
        ## comment message
        self.msg = msg
        # Fill fds_namelists from filepath or f90
        if filepath or f90:
            self.from_fds(filepath=filepath, f90=f90)

    def __str__(self):
        return self.to_fds()

    def get_by_label(self, fds_label, remove=False):
        """!
        Return the first FDSNamelist instance in list by its label.
        @param fds_label: namelist label.
        @param remove: remove it from self
        @return None or FDSNamelist.
        """
        for res in self.fds_namelists:
            if res.fds_label == fds_label:
                if remove:
                    self.fds_namelists.remove(res)
                return res

    def to_fds(self, context=None):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        lines = list()
        if self.msg:
            lines.extend(tuple(f"! {m}" for m in self.msg.splitlines()))
        lines.extend(
            fds_namelist.to_fds() for fds_namelist in self.fds_namelists if fds_namelist
        )
        return "\n".join(l for l in lines if l)

    _scan_namelists = re.compile(
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

    def from_fds(self, filepath=None, f90=None):
        """!
        Import from FDS file, on error raise BFException.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        # Init f90
        if filepath and not f90:
            f90 = utils.read_txt_file(filepath)
        elif f90 and filepath:
            raise AssertionError("Cannot set both filepath and f90.")
        # Scan and fill self.fds_namelist
        for match in re.finditer(self._scan_namelists, f90):
            label, f90_params = match.groups()
            try:
                fds_namelist = FDSNamelist(fds_label=label, f90=f90_params)
            except BFException as err:
                err.msg += f"\nin namelist:\n&{label} {f90_params} /"
                raise err
            self.fds_namelists.append(fds_namelist)
