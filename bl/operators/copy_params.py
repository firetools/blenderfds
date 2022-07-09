# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to copy FDS parameter values between entities.
"""

import logging, bpy
from bpy.types import Operator, Scene
from bpy.props import StringProperty

log = logging.getLogger(__name__)


def _bf_props_copy(context, source_element, dest_elements):
    """!
    Copy all FDS parameters from source_element to dest_elements.
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


class SCENE_OT_bf_copy_props_to_sc(Operator):
    """!
    Copy current FDS case parameters to another Scene.
    """

    bl_label = "Copy To"
    bl_idname = "scene.bf_props_to_sc"
    bl_description = "Copy case parameters to another Scene"
    bl_options = {"REGISTER", "UNDO"}

    bf_dest_element: StringProperty(name="Destination")

    def draw(self, context):
        self.layout.prop_search(self, "bf_dest_element", bpy.data, "scenes")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        # Get source and dest element
        source_element = context.scene
        dest_element = bpy.data.scenes.get(self.bf_dest_element, None)
        if source_element == dest_element:
            self.report({"WARNING"}, "Destination same as source")
            return {"CANCELLED"}
        if not dest_element:
            self.report({"ERROR"}, "No destination")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source")
            return {"CANCELLED"}

        # Copy
        _bf_props_copy(context, source_element, (dest_element,))
        self.report({"INFO"}, f"Parameters copied")
        return {"FINISHED"}


class OBJECT_OT_bf_copy_FDS_properties_to_sel_obs(Operator):
    """!
    Copy current FDS parameters to selected Objects.
    """

    bl_label = "Copy To"
    bl_idname = "object.bf_props_to_sel_obs"
    bl_description = "Copy FDS parameters to selected Objects"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        return context.object

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")

        # Get source and destination objects
        source_element = context.object
        dest_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not dest_elements:
            self.report({"ERROR"}, "No destination, select Objects")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source")
            return {"CANCELLED"}

        # Copy
        _bf_props_copy(context, source_element, dest_elements)
        self.report(
            {"INFO"},
            f"Parameters copied to {len(dest_elements)} selected Object(s)",
        )
        return {"FINISHED"}


class MATERIAL_OT_bf_copy_props_to_ma(Operator):
    """!
    Copy current FDS parameters to another Material.
    """

    bl_label = "Copy To"
    bl_idname = "material.bf_props_to_ma"
    bl_description = "Copy parameters to another Material"
    bl_options = {"REGISTER", "UNDO"}

    bf_dest_element: StringProperty(name="Destination")

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.active_material

    def draw(self, context):
        self.layout.prop_search(self, "bf_dest_element", bpy.data, "materials")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        # Get source and dest element
        source_element = context.object.active_material
        dest_element = bpy.data.materials.get(self.bf_dest_element, None)
        if source_element == dest_element:
            self.report({"WARNING"}, "Destination same as source")
            return {"CANCELLED"}
        if not dest_element:
            self.report({"ERROR"}, "No destination")
            return {"CANCELLED"}
        if not source_element:
            self.report({"ERROR"}, "No source")
            return {"CANCELLED"}

        # Copy
        _bf_props_copy(context, source_element, (dest_element,))
        self.report({"INFO"}, "Parameters copied")
        return {"FINISHED"}


bl_classes = [
    SCENE_OT_bf_copy_props_to_sc,
    OBJECT_OT_bf_copy_FDS_properties_to_sel_obs,
    MATERIAL_OT_bf_copy_props_to_ma,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
