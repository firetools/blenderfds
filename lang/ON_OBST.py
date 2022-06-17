# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistOb
from .bf_object import (
    OP_namelist_cls,
    OP_ID,
    OP_FYI,
    OP_ID_suffix,
    OP_other,
    OP_RGB_override,
    OP_COLOR_override,
    OP_TRANSPARENCY_override,
)
from .OP_SURF_ID import OP_SURF_ID
from .OP_XB import OP_XB, OP_XB_voxel_size, OP_XB_center_voxels
from .ON_MULT import OP_other_MULT_ID

log = logging.getLogger(__name__)


class ON_OBST(BFNamelistOb):
    label = "OBST"
    description = "Obstruction"
    collection = "Obstacles"
    enum_id = 1000
    fds_label = "OBST"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_XB,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_ID_suffix,
        OP_other_MULT_ID,
        OP_RGB_override,
        OP_COLOR_override,
        OP_TRANSPARENCY_override,
        OP_other,
    )
    bf_import_order = 100
