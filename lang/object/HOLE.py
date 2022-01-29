import logging
from ...types import BFNamelistOb
from .object import OP_ID, OP_FYI, OP_other
from .XB import OP_XB

log = logging.getLogger(__name__)


class ON_HOLE(BFNamelistOb):
    label = "HOLE"
    description = "Obstruction cutout"
    enum_id = 1009
    fds_label = "HOLE"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_other
    bf_other = {"appearance": "WIRE"}
