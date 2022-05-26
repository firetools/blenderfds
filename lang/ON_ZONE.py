# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistOb
from .bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_other
from .OP_XYZ import OP_XYZ

log = logging.getLogger(__name__)


class ON_ZONE(BFNamelistOb):
    label = "ZONE"
    description = "Pressure zone"
    collection = "Domain"
    enum_id = 1016
    fds_label = "ZONE"
    bf_params = OP_namelist_cls, OP_ID, OP_FYI, OP_XYZ, OP_other
    bf_other = {"appearance": "WIRE"}
