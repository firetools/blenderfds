"""!
BlenderFDS, operators to check GEOM sanity and intersections.
"""

import logging
from bpy.types import Operator
from ...types import BFException
from ... import lang

log = logging.getLogger(__name__)


class OBJECT_OT_bf_check_intersections(Operator):
    """!
    Check self-intersections or intersections with other selected objects.
    """

    bl_label = "Check Intersections"
    bl_idname = "object.bf_geom_check_intersections"
    bl_description = (
        "Check self-intersections or intersections with other selected objects"
    )

    @classmethod
    def poll(cls, context):
        return context.active_object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        obs = context.selected_objects
        if obs:
            obs.remove(ob)
        try:
            lang.GEOM.check_intersections(context, ob, obs, protect=ob.data.bf_geom_protect)
        except BFException as err:
            self.report({"ERROR"}, f"Check intersections: {err}")
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "No intersection detected")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


class SCENE_OT_bf_check_sanity(Operator):
    """!
    Check if closed orientable manifold, with no degenerate geometry.
    """

    bl_label = "Check Sanity"
    bl_idname = "object.bf_geom_check_sanity"
    bl_description = "Check if closed orientable manifold, with no degenerate geometry"

    @classmethod
    def poll(cls, context):
        return context.active_object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        try:
            lang.GEOM.check_geom_sanity(
                context,
                ob,
                protect=ob.data.bf_geom_protect,
                check_open=not ob.bf_geom_is_terrain,
            )
        except BFException as err:
            self.report({"ERROR"}, f"Check sanity: {err}")
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "Geometry sanity ok")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


bl_classes = [OBJECT_OT_bf_check_intersections, SCENE_OT_bf_check_sanity]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
