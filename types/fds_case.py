"""!
BlenderFDS, Blender representations of a FDS case.
"""

import re, logging
from .. import utils
from .bf_exception import BFException
from .fds_namelist import FDSNamelist

log = logging.getLogger(__name__)


class FDSCase:
    """!
    List of BFNamelist instances representing an FDS case.
    """

    def __init__(self, fds_namelists=None, msgs=None) -> None:
        """!
        Class constructor.
        @param msgs: list of comment message strings.
        """
        ## list of FDSNamelist instances
        self.fds_namelists = fds_namelists and list(fds_namelists) or list()
        ## list of comment message strings
        self.msgs = msgs and list(msgs) or list()

    def __str__(self):
        try:
            return self.to_fds()
        except:
            return self.__repr__()

    def __repr__(self) -> str:
        items = ", ".join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f"<{self.__class__.__name__}({items})>"

    def __contains__(self, fds_label) -> bool:
        # self can be a list of lists (multi), but only when exporting
        # in that case this fails
        return fds_label in (
            fds_namelist.fds_label for fds_namelist in self.fds_namelists
        )

    def copy(self):  # shallow copy
        return FDSCase(fds_namelists=self.fds_namelist[:], msgs=self.msgs[:])

    def get_fds_namelist(self, fds_label, remove) -> FDSNamelist or None:
        """!
        Return and remove the first FDSNamelist instance in self.fds_namelists by its fds_label.
        @param fds_label: namelist label.
        @param remove: remove found.
        @return FDSNamelist or None.
        """
        for fds_namelist in self.fds_namelists:
            if not fds_namelist:
                continue
            if fds_label and fds_namelist.fds_label != fds_label:
                continue
            if remove:
                self.fds_namelists.remove(fds_namelist)
            return fds_namelist

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

    _RE_SCAN_NAMELISTS = re.compile(
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
        elif not f90 and not filepath:
            raise AssertionError("Both filepath and f90 unset.")
        # Scan and fill self
        for match in re.finditer(self._RE_SCAN_NAMELISTS, f90):
            label, f90_params = match.groups()
            fds_namelist = FDSNamelist(fds_label=label)
            try:
                fds_namelist.from_fds(f90=f90_params)
            except BFException as err:
                err.msg += f"\nin namelist:\n&{label} {f90_params} /"
                raise err
            else:
                self.fds_namelists.append(fds_namelist)
