import logging, re
from bpy.types import Object
from bpy.props import FloatProperty, BoolProperty, StringProperty
from ..types import BFParam, BFNamelistOb, BFException
from .bf_object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .OP_XB import OP_XB
from .OP_XYZ import OP_XYZ
from .OP_PB import OP_PB, OP_PBX, OP_PBY, OP_PBZ

log = logging.getLogger(__name__)


class OP_DEVC_QUANTITY(BFParam):
    label = "QUANTITY"
    description = "Output quantity"
    fds_label = "QUANTITY"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_quantity"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_quantity", icon="VIEWZOOM", text="")


class OP_DEVC_SETPOINT(BFParam):
    label = "SETPOINT [~]"
    description = "Value of the device at which its state changes"
    fds_label = "SETPOINT"
    bpy_type = Object
    bpy_idname = "bf_devc_setpoint"
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"step": 10.0, "precision": 3}
    bpy_export = "bf_devc_setpoint_export"
    bpy_export_default = False


class OP_DEVC_INITIAL_STATE(BFParam):
    label = "INITIAL_STATE"
    description = "Set device initial state"
    fds_label = "INITIAL_STATE"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_initial_state"


class OP_DEVC_LATCH(BFParam):
    label = "LATCH"
    description = "Device only changes state once"
    fds_label = "LATCH"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_latch"


class OP_DEVC_PROP_ID(BFParam):
    label = "PROP_ID"
    description = "Reference to a PROP (Property) line for self properties"
    fds_label = "PROP_ID"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_devc_prop_id"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_prop_id", icon="VIEWZOOM", text="")


class ON_DEVC(BFNamelistOb):
    label = "DEVC"
    description = "Device"
    enum_id = 1011
    fds_label = "DEVC"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_DEVC_SETPOINT,
        OP_DEVC_INITIAL_STATE,
        OP_DEVC_LATCH,
        OP_DEVC_PROP_ID,
        OP_XB,
        OP_XYZ,
        OP_PB,  # TODO used?
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "WIRE"}
