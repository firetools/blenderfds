import logging
from ...types import BFNamelistOb
from .object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .DEVC import OP_DEVC_QUANTITY
from .XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_PROF(BFNamelistOb):
    label = "PROF"
    description = "Wall profile output"
    enum_id = 1013
    fds_label = "PROF"
    bf_params = OP_ID, OP_FYI, OP_DEVC_QUANTITY, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "DUMMY1"}
