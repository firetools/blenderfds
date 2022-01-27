import logging, bpy
from bpy.types import Object
from bpy.props import PointerProperty
from ...types import BFParam, BFNamelistOb, BFException
from .object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .SURF_ID import OP_SURF_ID
from .XB import OP_XB
from .XYZ import OP_XYZ
from .PB import OP_PB, OP_PBX, OP_PBY, OP_PBZ

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
    enum_id = 1010
    fds_label = "VENT"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_VENT_OBST_ID,
        OP_XB,
        OP_XYZ,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}
