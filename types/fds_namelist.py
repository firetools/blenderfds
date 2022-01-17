"""!
BlenderFDS, Blender representations of a FDS namelist.
"""

import re, logging
from .fds_param import FDSParam

log = logging.getLogger(__name__)


class FDSNamelist:
    """!
    Datastructure representing an FDS namelist.
    """

    ## max number of columns of formatted output
    MAXLEN = 80  # TODO to config

    def __init__(self, fds_label=None, fds_params=None, msgs=None) -> None:
        """!
        Class constructor.
        @param fds_label: namelist group label.
        @param fds_params: list of FDSParam and additional FDSNamelist instances.
        @param msgs: list of comment message strings.
        """
        ## namelist group label
        self.fds_label = fds_label
        ## list (single) of FDSParam and additional FDSNamelist,
        ## or list of list (multi) of FDSParam instances
        ## eg. (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...)
        self.fds_params = fds_params or list()
        ## list of comment message strings
        self.msgs = msgs and list(msgs) or list()

    def __str__(self) -> str:
        return self.to_fds()

    def __contains__(self, fds_label) -> bool:
        # self can be a list of lists (multi), but only when exporting
        # in that case this fails
        return fds_label in (fds_params.fds_label for fds_params in self.fds_params)

    def get(self, fds_label=None, remove=False) -> FDSParam or None:
        """!
        Return the first FDSParam instance in list by its label.
        @param fds_label: namelist parameter label.
        @param remove: remove it from self
        @return None or FDSParam.
        """
        # self.fds_params can be a list of lists (multi), but only when exporting
        # in that case this fails
        for fds_param in self.fds_params:
            if not fds_label or fds_param.fds_label == fds_label:
                if remove:
                    self.fds_params.remove(fds_param)
                return fds_param

    def to_fds(self, context=None) -> str:
        """!
        Return the FDS formatted string.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or a None.
        """
        msgs = self.msgs
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
                msgs.extend(p.msgs)
            elif isinstance(p, FDSNamelist):
                # Additional namelist
                addns.append(p)
                msgs.extend(p.msgs)
            elif isinstance(p, tuple):
                if isinstance(p[0], tuple):
                    # Multi parameter
                    multips = p
                    for p0 in multips[0]:
                        msgs.extend(p0.msgs)
                else:
                    # Many parameters
                    for pp in p:
                        if isinstance(pp, FDSParam):
                            invps.append(pp)
                        if isinstance(pp, FDSNamelist):
                            addns.append(pp)
                        msgs.extend(pp.msgs)
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
                nl = list()
                nl.extend(invps)  # first invariant
                nl.extend(multip)  # then variant
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
                        if not newline and len(line) + 1 + len(label) <= self.MAXLEN:
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
                            and len(line) + 1 + len(label) + 1 + len(v) <= self.MAXLEN
                        ):
                            # Parameter to the same line
                            newline = False
                            line += " " + label + "=" + v
                        else:
                            # Parameter to new line
                            lines.append(line)
                            line = "      " + label + "="  # new line
                            if len(line) + len(v) <= self.MAXLEN:
                                # Formatted values do not need splitting
                                line += v
                            else:
                                # Formatted values need splitting
                                newline = True  # the following needs a new line
                                for v in vs:
                                    if len(line) + len(v) + 1 <= self.MAXLEN:
                                        line += v + ","
                                    else:
                                        lines.append(line)
                                        line = "        " + v + ","  # new line
                                line = line[:-1]  # remove last ","
                line += " /"
                lines.append(line)
        return "\n".join(lines)

    _RE_SCAN_PARAMS = re.compile(
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

    def from_fds(self, f90) -> None:
        """!
        Import from FDS formatted string of parameters, on error raise BFException.
        @param f90: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        # Remove trailing spaces and newlines, then scan it
        f90 = " ".join(f90.strip().splitlines())
        for match in re.finditer(self._RE_SCAN_PARAMS, f90):
            label, f90_value = match.groups()
            fds_param = FDSParam(fds_label=label)
            fds_param.from_fds(f90=f90_value)
            self.fds_params.append(fds_param)
