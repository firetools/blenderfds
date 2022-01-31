import logging
from ..types import BFNamelistOb
from .bf_object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .OP_XB import OP_XB
from .OP_XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_INIT(BFNamelistOb):
    label = "INIT"
    description = "Initial condition"
    enum_id = 1015
    fds_label = "INIT"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "WIRE"}
