"""!
BlenderFDS, operators for the MESH tools panel.
"""

import logging, bpy
from bpy.types import Operator
from bpy.props import BoolProperty, FloatVectorProperty
from ... import utils, lang

log = logging.getLogger(__name__)


class OBJECT_OT_bf_set_mesh_cell_size(Operator):
    """!
    Set current MESH cell size.
    """

    bl_label = "Set Cell Size"
    bl_idname = "object.bf_set_mesh_cell_size"
    bl_description = "Set current MESH cell size"
    bl_options = {"REGISTER", "UNDO"}

    bf_cell_sizes: FloatVectorProperty(
        name="Desired Cell Sizes [m]",
        description="Desired MESH cell sizes",
        default=(0.3, 0.3, 0.3),
        min=0.001,
        precision=3,
        size=3,
    )
    bf_poisson_restriction: BoolProperty(
        name="Poisson Restriction",
        description="Respect FDS Poisson solver restriction on IJK value while setting desired cell sizes.",
        default=True,
    )

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        ob = context.active_object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        layout = self.layout
        layout.prop(self, "bf_cell_sizes", text="")
        layout.prop(self, "bf_poisson_restriction")

    def invoke(self, context, event):
        ob = context.active_object
        # Set default
        self.bf_cell_sizes = lang.MESH.get_cell_sizes(context, ob)
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        ob = context.active_object
        ob.bf_xb, ob.bf_xb_export = "BBOX", True
        ob.bf_mesh_ijk = lang.MESH.get_ijk_from_desired_cs(
            context, ob, self.bf_cell_sizes, self.bf_poisson_restriction
        )
        self.report({"INFO"}, "MESH cell size set")
        return {"FINISHED"}


# FIXME FIXME FIXME  align meshes


class OBJECT_OT_bf_align_selected_meshes(Operator):
    bl_label = "Align Selected"
    bl_idname = "object.bf_align_selected_meshes"
    bl_description = "Align selected MESHes to the current Object MESH"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def invoke(self, context, event):  # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")
        # Get source and destination objects
        source_element = context.active_object
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH"
            and ob != source_element
            and ob.bf_namelist_cls == "ON_MESH"
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        # Align  # FIXME FIXME FIXME
        rijk = source_element.bf_mesh_ijk  # ref ijk
        rxb = utils.geometry.get_bbox_xb(context, ob=source_element, world=True)
        for de in destination_elements:
            mijk = de.bf_mesh_ijk
            mxb = utils.geometry.get_bbox_xb(context, ob=de, world=True)
            rijk, rxb, mijk, mxb, msgs = lang.MESH.align_meshes(
                rijk, rxb, mijk, mxb, poisson=False, protect_rl=False
            )
            source_element.bf_mesh_ijk = rijk
            matrix = source_element.matrix_world.invert()
            lang.XB.xbs_to_ob(
                context=context,
                ob=source_element,
                xbs=(rxb,),
                bf_xb="BBOX",
                matrix=matrix,
            )
            de.bf_mesh_ijk = mijk
            matrix = de.matrix_world.invert()
            lang.XB.xbs_to_ob(
                context=context,
                ob=de,
                xbs=(mxb,),
                bf_xb="BBOX",
                matrix=matrix,
            )
            log.debug("\n".join(msgs))
        # Update 3dview
        context.view_layer.update()
        self.report({"INFO"}, "MESH Objects aligned")
        return {"FINISHED"}


bl_classes = [
    OBJECT_OT_bf_set_mesh_cell_size,
    OBJECT_OT_bf_align_selected_meshes,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)