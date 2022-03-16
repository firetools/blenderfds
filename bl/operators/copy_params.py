"""!
BlenderFDS, operators to copy FDS parameter values between entities.
"""

import logging, bpy
from bpy.types import Operator, Scene
from bpy.props import StringProperty

log = logging.getLogger(__name__)


def _bf_props_copy(context, source_element, dest_elements):
    """!
    Copy all parameters from source_element to dest_elements.
    """
    # Get bf_namelists
    if isinstance(source_element, Scene):
        bf_namelists = source_element.bf_namelists
    else:
        bf_namelists = tuple((source_element.bf_namelist,))
        for dest_element in dest_elements:
            dest_element.bf_namelist_cls = source_element.bf_namelist_cls
    # Copy them
    for bf_namelist in bf_namelists:
        for dest_element in dest_elements:
            bf_namelist.copy_to(context=context, dest_element=dest_element)


class SCENE_OT_bf_copy_props_to_scene(Operator):
    """!
    Copy all current scene FDS parameters to another Scene.
    """

    bl_label = "Copy To"
    bl_idname = "scene.bf_props_to_scene"
    bl_description = "Copy all current scene FDS parameters to another Scene"
    bl_options = {"REGISTER", "UNDO"}

    bf_dest_element: StringProperty(name="Destination Scene")

    def draw(self, context):
        layout = self.layout
        row = layout.row()
        row.prop_search(self, "bf_dest_element", bpy.data, "scenes", text="Scene")

    def execute(self, context):
        # Get source and dest scenes
        source_element = context.scene
        dest_elements = (bpy.data.scenes.get(self.bf_dest_element, None),)  # a tuple!
        if source_element in dest_elements:
            self.report({"WARNING"}, "Destination same as source Scene")
            return {"CANCELLED"}
        if not dest_elements[0]:
            self.report({"ERROR"}, "No destination Scene")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source Scene")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, dest_elements)
        self.report({"INFO"}, f"Copied to destination Scene <{dest_elements[0].name}>")
        return {"FINISHED"}

    def invoke(self, context, event):
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


class OBJECT_OT_bf_copy_FDS_properties_to_sel_obs(Operator):
    """!
    Copy current object FDS parameters to selected Objects.
    """

    bl_label = "Copy To"
    bl_idname = "object.bf_props_to_sel_obs"
    bl_description = "Copy current object FDS parameters to selected Objects"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        return context.active_object

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")
        # Get source and destination objects
        source_element = context.active_object
        dest_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not dest_elements:
            self.report({"ERROR"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source Object")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, dest_elements)
        self.report(
            {"INFO"},
            f"Copied to {len(dest_elements)} selected Object(s)",
        )
        return {"FINISHED"}


class MATERIAL_OT_bf_assign_BC_to_sel_obs(Operator):
    """!
    Assign current boundary condition to selected Objects.
    """

    bl_label = "Assign To"
    bl_idname = "material.bf_surf_to_sel_obs"
    bl_description = "Assign current boundary condition to selected Objects"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        source_element = context.active_object
        active_material = source_element.active_material
        return source_element and active_material

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT", toggle=False)
        # Get source and destination materials
        source_element = context.active_object
        active_material = source_element.active_material
        dest_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not dest_elements:
            self.report({"ERROR"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source Object")
            return {"CANCELLED"}
        if not active_material:
            self.report({"WARNING"}, "No boundary condition to assign")
            return {"CANCELLED"}
        # Loop on objects
        for ob in dest_elements:
            ob.active_material = active_material
            log.debug(f"Assign Material <{active_material.name}> -> <{ob.name}>")
        # Set myself as exported
        active_material.bf_surf_export = True
        # Return
        self.report(
            {"INFO"},
            f"Assigned Material <{active_material.name}> to {len(dest_elements)} selected Object(s)",
        )
        return {"FINISHED"}


bl_classes = [
    SCENE_OT_bf_copy_props_to_scene,
    OBJECT_OT_bf_copy_FDS_properties_to_sel_obs,
    MATERIAL_OT_bf_assign_BC_to_sel_obs,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
