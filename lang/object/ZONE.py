import logging
from ...types import BFNamelistOb
from .object import OP_ID, OP_FYI, OP_other
from .XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_ZONE(BFNamelistOb):
    label = "ZONE"
    description = "Pressure zone"
    enum_id = 1016
    fds_label = "ZONE"
    bf_params = OP_ID, OP_FYI, OP_XYZ, OP_other
    bf_other = {"appearance": "WIRE"}
