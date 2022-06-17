# SPDX-License-Identifier: GPL-3.0-or-later

import logging, bpy
from bpy.types import Object
from bpy.props import PointerProperty
from ..types import BFParam, BFNamelistOb, BFException
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
from .OP_XYZ import OP_XYZ
from .OP_PB import OP_PB, OP_PBX, OP_PBY, OP_PBZ
from .ON_MULT import OP_other_MULT_ID

log = logging.getLogger(__name__)


class OP_VENT_OBST_ID(BFParam):
    label = "OBST_ID"
    description = "Specify OBST on which projecting the condition"
    fds_label = "OBST_ID"
    bpy_type = Object
    bpy_prop = PointerProperty
    bpy_idname = "bf_vent_obst_id"
    bpy_other = {"type": Object}

    def get_value(self, context):
        if self.element.bf_vent_obst_id:
            return self.element.bf_vent_obst_id.name

    def set_value(self, context, value):
        if value:
            ob = bpy.data.objects.get(value)
            if ob:
                self.element.bf_vent_obst_id = ob
            else:
                raise BFException(self, f"Object <{value}> not found")
        else:
            self.element.bf_vent_obst_id = None


class ON_VENT(BFNamelistOb):
    label = "VENT"
    description = "Boundary condition patch"
    collection = "Obstacles"
    enum_id = 1010
    fds_label = "VENT"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_VENT_OBST_ID,
        OP_XB,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_XYZ,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other_MULT_ID,
        OP_RGB_override,
        OP_COLOR_override,
        OP_TRANSPARENCY_override,
        OP_other,
    )
    bf_import_order = 300
