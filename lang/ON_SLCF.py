# SPDX-License-Identifier: GPL-3.0-or-later

import logging, re
from bpy.types import Object
from bpy.props import BoolProperty
from ..types import BFParam, BFNamelistOb, BFException
from .bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .ON_DEVC import OP_DEVC_QUANTITY
from .OP_XB import OP_XB, OP_XB_voxel_size, OP_XB_center_voxels
from .OP_PB import OP_PB, OP_PBX, OP_PBY, OP_PBZ

log = logging.getLogger(__name__)


class OP_SLCF_VECTOR(BFParam):
    label = "VECTOR"
    description = "Create animated vectors"
    fds_label = "VECTOR"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_vector"


class OP_SLCF_CELL_CENTERED(BFParam):
    label = "CELL_CENTERED"
    description = "Output the actual cell-centered data with no averaging"
    fds_label = "CELL_CENTERED"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_cell_centered"


class ON_SLCF(BFNamelistOb):
    label = "SLCF"
    description = "Slice file"
    collection = "Output"
    enum_id = 1012
    fds_label = "SLCF"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_SLCF_VECTOR,
        OP_SLCF_CELL_CENTERED,
        OP_XB,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "WIRE"}
