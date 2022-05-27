# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators for the MESH namelist.
"""


import logging, bpy
from bpy.types import Operator

log = logging.getLogger(__name__)


class OBJECT_OT_bf_clean_ma_slots(Operator):
    """!
    Clean Material slots of Object, keep only active Object.
    """

    bl_label = "Clean Material Slots"
    bl_idname = "object.bf_clean_ma_slots"
    bl_description = "Clean Material slots from unused or empty instances"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and len(ob.material_slots) > 1

    def execute(self, context):
        ob = context.object
        orig_active_ma_index = ob.active_material_index
        for i in reversed(range(len(ob.material_slots))):
            ms = ob.material_slots[i]
            ob.active_material_index = i
            if ms.material and i == orig_active_ma_index:
                continue
            bpy.ops.object.material_slot_remove()
        ob.active_material_index = 0
        self.report({"INFO"}, "Material slots cleaned")
        return {"FINISHED"}


bl_classes = [
    OBJECT_OT_bf_clean_ma_slots,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
