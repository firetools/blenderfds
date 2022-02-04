"""!
BlenderFDS, operators to show generated FDS geometry.
"""


import logging
from bpy.types import Operator
from ...types import BFException
from ... import utils, lang

log = logging.getLogger(__name__)


class OBJECT_OT_bf_show_fds_geometry(Operator):
    """!
    Show geometry of Object as exported to FDS.
    """

    bl_label = "FDS Geometry"
    bl_idname = "object.bf_show_fds_geometry"
    bl_description = "Show/Hide geometry as exported to FDS"

    @classmethod
    def poll(cls, context):
        return context.active_object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object

        # Hide tmp
        if ob.bf_is_tmp or ob.bf_has_tmp:
            utils.geometry.rm_tmp_objects()
            try:
                context.view_layer.objects.active = ob
            except ReferenceError:
                try:
                    context.view_layer.objects.active = context.selected_objects[0]
                except IndexError:
                    pass
            w.cursor_modal_restore()
            self.report({"INFO"}, "Temporary geometry hidden")
            return {"FINISHED"}

        # Check
        bf_namelist = ob.bf_namelist
        if not bf_namelist:
            w.cursor_modal_restore()
            self.report({"WARNING"}, "Not exported, no FDS geometry shown")
            return {"CANCELLED"}

        # Make tmp Object and set its appearance
        ob_tmp = utils.geometry.get_tmp_object(context, ob, f"{ob.name}_tmp")
        ob_tmp.bf_namelist_cls = ob.bf_namelist_cls
        ob_tmp.show_name = False
        ob_tmp.show_wire = True

        # Copy materials
        for ms in ob.material_slots:
            ma = ms.material
            if not ma:
                raise BFException(self, f"Empty material slot")
            ob_tmp.data.materials.append(ma)

        # Set tmp geometry
        is_shown = list()
        try:
            is_shown.append(
                bf_namelist.show_fds_geometry(context=context, ob_tmp=ob_tmp)
            )
            for bf_param in bf_namelist.bf_params:
                is_shown.append(
                    bf_param.show_fds_geometry(context=context, ob_tmp=ob_tmp)
                )
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        except Exception as err:
            self.report({"ERROR"}, f"Unexpected error: {err}")
            return {"CANCELLED"}
        else:
            if any(is_shown):
                self.report({"INFO"}, "FDS geometry shown")
            else:
                self.report({"WARNING"}, "No FDS geometry to show")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


bl_classes = [
    OBJECT_OT_bf_show_fds_geometry,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
