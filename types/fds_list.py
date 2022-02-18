from .fds_namelist import FDSNamelist
from .fds_param import FDSParam


class FDSList(list):  # TODO unused
    """!
    List of FDSParam, FDSNamelist, FDSList instances.
    """

    def __init__(self, *args, msgs=None, **kwargs):
        """!
        Class constructor.
        @param msgs: list of comment message strings.
        """
        super().__init__(*args, **kwargs)
        ## list of comment message strings
        self.msgs = msgs and list(msgs) or list()

    def __repr__(self) -> str:
        items = ", ".join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f"<{self.__class__.__name__}({items})>"

    def __contains__(self, fds_label) -> bool:
        for item in self:
            label = getattr(item, "fds_label")
            if label:
                if item.fds_label == fds_label:
                    return True
                else:
                    if item.__contains__(fds_label):
                        return True
        return False

    def copy(self):
        return FDSList(self, msgs=self.msgs.copy())

    def get_fds_label(self, fds_label=None, remove=False):
        """!
        Get first occurence of fds_label.
        @param fds_label: namelist label.
        @param remove: remove found.
        @return any contents or None.
        """
        for item in self:
            if item is None:
                continue
            if fds_label:
                label = getattr(item, "fds_label")
                if label:
                    if label == fds_label:
                        if remove:
                            self.remove(item)
                        return item
                else:
                    subitem = item.get_fds_label(fds_label=fds_label, remove=remove)
                    if subitem is not None:
                        return subitem
            else:
                if remove:
                    self.remove(item)
                return item
