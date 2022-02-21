import logging
from ..types import BFNamelistOb
from .bf_object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .OP_XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_HVAC(BFNamelistOb):
    label = "HVAC"
    description = "HVAC system definition"
    collection = "HVAC"
    enum_id = 1017
    fds_label = "HVAC"
    bf_params = OP_ID, OP_FYI, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "WIRE"}
