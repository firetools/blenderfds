# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistOb
from .bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .ON_DEVC import OP_DEVC_QUANTITY
from .OP_XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_PROF(BFNamelistOb):
    label = "PROF"
    description = "Wall profile output"
    collection = "Output"
    enum_id = 1013
    fds_label = "PROF"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_XYZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "WIRE"}
