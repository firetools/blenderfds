import logging
from ...types import BFNamelistOb
from .object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .XB import OP_XB
from .XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_INIT(BFNamelistOb):
    label = "INIT"
    description = "Initial condition"
    enum_id = 1015
    fds_label = "INIT"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "DUMMY2"}
