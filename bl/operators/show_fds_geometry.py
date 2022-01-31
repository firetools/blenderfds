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
        return context.object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.object
        # Hide
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
        # Show
        bf_namelist = ob.bf_namelist
        if not bf_namelist.get_exported(context):
            w.cursor_modal_restore()
            self.report({"WARNING"}, "Not exported, nothing to show")
            return {"CANCELLED"}
        # Manage GEOM
        if ob.bf_namelist_cls == "ON_GEOM" and not ob.hide_render:  # was bf_export
            try:
                fds_verts, fds_faces, fds_surfs, _, msg = lang.ON_GEOM.ob_to_geom(
                    context=context,
                    ob=ob,
                    check=ob.data.bf_geom_check_sanity,
                    check_open=not ob.data.bf_geom_is_terrain,
                    world=True,
                )
            except BFException as err:
                self.report({"ERROR"}, f"Show: {err}")
                return {"CANCELLED"}
            else:
                ob_tmp = utils.geometry.get_tmp_object(
                    context, ob, f"{ob.name}_GEOM_tmp"
                )
                # copy materials
                for ms in ob.material_slots:
                    ma = ms.material
                    if not ma:
                        raise BFException(
                            self, f"Object <{ob.name}> has empty material slot"
                        )
                    ob_tmp.data.materials.append(ma)
                lang.ON_GEOM.geom_to_ob(
                    context=context,
                    ob=ob_tmp,
                    fds_verts=fds_verts,
                    fds_faces=fds_faces,
                    fds_surfs=fds_surfs,
                )
                ob_tmp.show_wire = True
                self.report({"INFO"}, msg)
                return {"FINISHED"}
            finally:
                w.cursor_modal_restore()
        # Manage XB, XYZ, PB*
        msgs, ob_tmp = list(), None
        # XB
        if ob.bf_xb_export and ob.bf_namelist.get_bf_param_xb():
            try:
                xbs, msg = lang.OP_XB.ob_to_xbs(context=context, ob=ob, bf_xb=ob.bf_xb)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Show: {err}")
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = utils.geometry.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_XB_tmp"
                )
                lang.OP_XB.xbs_to_ob(
                    context=context,
                    ob=ob_tmp,
                    xbs=xbs,
                    bf_xb=ob.bf_xb,
                )
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # XYZ
        if ob.bf_xyz_export and ob.bf_namelist.get_bf_param_xyz():
            try:
                xyzs, msg = lang.OP_XYZ.ob_to_xyzs(
                    context=context, ob=ob, bf_xyz=ob.bf_xyz
                )
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Show: {err}")
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = utils.geometry.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_XYZ_tmp"
                )
                lang.OP_XYZ.xyzs_to_ob(context=context, ob=ob_tmp, xyzs=xyzs)
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # PB
        if ob.bf_pb_export and ob.bf_namelist.get_bf_param_pb():
            try:
                pbs, msg = lang.OP_PB.ob_to_pbs(context=context, ob=ob, bf_pb=ob.bf_pb)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Show: {err}")
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = utils.geometry.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_PB*_tmp"
                )
                lang.OP_PB.pbs_to_ob(context=context, ob=ob_tmp, pbs=pbs)
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # Close
        w.cursor_modal_restore()
        msg = "; ".join(msg for msg in msgs if msg)
        if ob_tmp:
            self.report({"INFO"}, msg or "Geometry exported")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, msg or "No geometry exported")
            return {"CANCELLED"}


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
