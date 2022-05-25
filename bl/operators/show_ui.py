# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to show parts of Blender UI.
"""

import bpy
from bpy.types import Operator
from ... import utils


class SCENE_OT_bf_show_material_panel(Operator):
    """!
    Show the Material Panel.
    """

    bl_label = "Show the Material Panel"
    bl_idname = "scene.bf_show_material_panel"
    bl_description = "Show the Material Panel of the property editor"

    def execute(self, context):
        utils.ui.show_property_panel(context, space_context="MATERIAL")
        self.report({"INFO"}, f"See Blender Material Panel")
        return {"FINISHED"}


class SCENE_OT_bf_show_text(Operator):
    """!
    Show free text in the editor.
    """

    bl_label = "Show Free Text"
    bl_idname = "scene.bf_show_text"
    bl_description = "Show free text in the editor"

    def execute(self, context):
        context.scene.bf_config_text = utils.ui.show_bl_text(
            context=context,
            bl_text=context.scene.bf_config_text,
            name="Text",
        )
        self.report(
            {"INFO"}, f"See Blender text editor: <{context.scene.bf_config_text.name}>"
        )
        return {"FINISHED"}


bl_classes = [
    SCENE_OT_bf_show_text,
    SCENE_OT_bf_show_material_panel,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
