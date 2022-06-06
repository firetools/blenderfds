# SPDX-License-Identifier: GPL-3.0-or-later

import logging, bpy
from bpy.types import Object
from ..types import BFParam, BFException

log = logging.getLogger(__name__)


class OP_SURF_ID(BFParam):
    label = "SURF_ID"
    description = "Reference to SURF"
    fds_label = "SURF_ID"
    bpy_type = Object
    bpy_idname = "active_material"
    bpy_export = "bf_surf_id_export"
    bpy_export_default = False

    def get_value(self, context):
        if self.element.active_material:
            return self.element.active_material.name

    def set_value(self, context, value):
        ob = self.element
        if value is None:
            ob.active_material = None
        else:
            try:  # TODO use utils
                ma = bpy.data.materials.get(value)
            except IndexError:
                raise BFException(self, f"Blender Material <{value}> does not exists")
            else:
                ob.active_material = ma
                ob.bf_surf_id_export = True

    def get_exported(self, context):
        ob = self.element
        return ob.bf_surf_id_export and bool(ob.active_material)

    def check(self, context):
        ob = self.element
        if (
            ob.bf_surf_id_export
            and ob.active_material
            and not ob.active_material.bf_surf_export
        ):
            raise BFException(
                self, f"Referenced SURF <{ob.active_material.name}> not exported"
            )

    def draw_operators(self, context, layout):
        layout.operator("object.bf_clean_ma_slots", icon="BRUSH_DATA", text="")
