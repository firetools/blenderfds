"""!
BlenderFDS, Blender representations of a FDS case.
"""

import re, logging

from .. import utils
from .bf_exception import BFException
from .fds_namelist import FDSNamelist


log = logging.getLogger(__name__)


class FDSCase(list):
    """!
    List of BFNamelist instances representing an FDS case.
    """

    def __init__(
        self, fds_namelists=None, msg=None, msgs=None, filepath=None, f90=None
    ) -> None:
        """!
        Class constructor.
        @param fds_namelists: list of FDSNamelist instances.
        @param msg: comment message string.
        @param msgs: list of comment message strings.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        ## list of FDSNamelist instances
        super().__init__(fds_namelists or ())
        ## list of comment message strings
        self.msgs = msgs or list()
        self.msg = msg
        # Fill fds_namelists from filepath or f90
        if filepath or f90:
            self.from_fds(filepath=filepath, f90=f90)

    def __str__(self):
        return self.to_fds()

    def __contains__(self, fds_label) -> bool:
        # self can be a list of lists (multi), but only when exporting
        # in that case this fails
        return fds_label in (n.fds_label for n in self)

    def get(self, fds_label, remove=False) -> list:
        """!
        Return the list of FDSNamelist instances by their fds_label.
        @param fds_label: namelist label.
        @param remove: remove found from self
        @return list of FDSNamelist.
        """
        # self can be a list of lists (multi), but only when exporting
        # in that case this fails
        items = list()
        for p in self:
            if p.fds_label == fds_label:
                if remove:
                    self.remove(p)
                items.append(p)
        return items

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
        lines = list()
        if self.msgs:
            lines.extend(tuple(f"! {m}" for m in self.msgs))
        lines.extend(fds_namelist.to_fds() for fds_namelist in self if fds_namelist)
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

    def from_fds(self, filepath=None, f90=None) -> None:
        """!
        Import from FDS file, on error raise BFException.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        # Init f90
        if filepath and not f90:
            f90 = utils.io.read_txt_file(filepath)
        elif f90 and filepath:
            raise AssertionError("Cannot set both filepath and f90.")
        # Scan and fill self
        for match in re.finditer(self._scan_namelists, f90):
            label, f90_params = match.groups()
            try:
                self.append(FDSNamelist(fds_label=label, f90=f90_params))
            except BFException as err:
                err.msg += f"\nin namelist:\n&{label} {f90_params} /"
                raise err
