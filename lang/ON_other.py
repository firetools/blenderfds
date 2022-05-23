# SPDX-License-Identifier: GPL-3.0-or-later

import logging, re
from bpy.types import Object
from bpy.props import StringProperty
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


class OP_other_namelist(BFParam):
    label = "Label"
    description = "Other namelist label, eg <ABCD>"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_other_namelist"
    bpy_default = "ABCD"
    bpy_other = {"maxlen": 4}

    def check(self, context):
        if not re.match("^[A-Z0-9_]{4}$", self.element.bf_other_namelist):
            raise BFException(
                self,
                f"Malformed other namelist label <{self.element.bf_other_namelist}>",
            )


class ON_other(BFNamelistOb):
    label = "Other"
    description = "Other namelist"
    enum_id = 1007
    bf_params = (
        OP_namelist_cls,
        OP_other_namelist,
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
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

    @property
    def fds_label(self):
        return self.element.bf_other_namelist
