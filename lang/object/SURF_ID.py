import logging, bpy
from bpy.types import Object
from ...types import BFParam, BFException

log = logging.getLogger(__name__)


class OP_SURF_ID(BFParam):
    label = "SURF_ID"
    description = "Reference to SURF"
    fds_label = "SURF_ID"
    bpy_type = Object
    bpy_idname = "active_material"
    bpy_export = "bf_surf_id_export"
    bpy_export_default = True

    def get_value(self, context):
        if self.element.active_material:
            return self.element.active_material.name

    def set_value(self, context, value):
        if value is None:
            self.element.active_material = None
        else:
            try:  # FIXME use utils
                ma = bpy.data.materials.get(value)
            except IndexError:
                raise BFException(self, f"Blender Material <{value}> does not exists")
            else:
                self.element.active_material = ma

    def get_exported(self, context):
        ob = self.element
        return ob.bf_surf_id_export and ob.active_material

    def check(self, context):
        ob = self.element
        if (
            ob.bf_surf_id_export
            and ob.active_material
            and not ob.active_material.bf_surf_export
        ):
            raise BFException(
                self, f"Referred SURF <{ob.active_material.name}> is not exported"
            )
