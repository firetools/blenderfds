# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistOb
from .bf_object import (
    OP_namelist_cls,
    OP_ID,
    OP_FYI,
    OP_other,
    OP_RGB_override,
    OP_COLOR_override,
    OP_TRANSPARENCY_override,
)
from .OP_XB import OP_XB, OP_XB_voxel_size, OP_XB_center_voxels
from .ON_MULT import OP_other_MULT_ID

log = logging.getLogger(__name__)


class ON_HOLE(BFNamelistOb):
    label = "HOLE"
    description = "Obstruction cutout"
    collection = "Obstacles"
    enum_id = 1009
    fds_label = "HOLE"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_XB,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_other_MULT_ID,
        OP_RGB_override,
        OP_COLOR_override,
        OP_TRANSPARENCY_override,
        OP_other,
    )
    bf_other = {"appearance": "WIRE"}
