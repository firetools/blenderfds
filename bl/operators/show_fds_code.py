"""!
BlenderFDS, operators to show generated FDS code.
"""

import logging
from bpy.types import Operator
from bpy.props import StringProperty, EnumProperty
from ...types import BFException
from ... import utils

log = logging.getLogger(__name__)


class WM_OT_bf_dialog(Operator):
    """!
    BlenderFDS Dialog.
    """

    bl_label = "BlenderFDS"
    bl_idname = "wm.bf_dialog"
    bl_description = "BlenderFDS Dialog"

    type: EnumProperty(
        name="Type",
        items=(("INFO", "Information", "Information"), ("ERROR", "Error", "Error")),
        description="Dialog type",
        default="INFO",
    )

    msg: StringProperty(
        name="Message", description="Dialog message", default="No message"
    )

    description: StringProperty(name="Description", description="Dialog description")

    def execute(self, context):
        return {"FINISHED"}

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def draw(self, context):
        layout = self.layout
        col = layout.column()
        col.label(text=self.msg, icon=self.type)
        if self.description:
            col.separator()
            descriptions = self.description.splitlines()
            for description in descriptions:
                row = col.row()
                row.label(text=description)


class _show_fds_code:
    """!
    Helper for showing fds code operators
    """

    def draw(self, context):
        if self.lines:
            lines = self.lines.split("\n")
        else:
            lines = ("No FDS code is exported",)
        if len(lines) > 20:
            lines = lines[:15] + [" ···"] + lines[-4:]
        layout = self.layout
        for line in lines:
            layout.label(text=line)

    def execute(self, context):
        self.report({"INFO"}, "FDS code shown")
        return {"FINISHED"}

    def _get_lines(self, context):
        """!
        Placeholder method to get text lines.
        """
        return str()

    def invoke(self, context, event):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        try:
            self.lines = self._get_lines(context)  # get FDS code
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            wm = context.window_manager
            return wm.invoke_props_dialog(self, width=600)
        finally:
            w.cursor_modal_restore()


class OBJECT_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Object.
    """

    bl_label = "Show FDS Code"
    bl_idname = "object.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Object"

    @classmethod
    def poll(cls, context):
        return context.object

    def _get_lines(self, context):
        return context.object.to_fds_list(context).to_string()


class COLLECTION_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Object.
    """

    bl_label = "Show FDS Code"
    bl_idname = "collection.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Collection"

    @classmethod
    def poll(cls, context):
        return context.collection

    def _get_lines(self, context):
        return context.collection.to_fds_list(context).to_string()


class MATERIAL_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Material.
    """

    bl_label = "Show FDS Code"
    bl_idname = "material.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Material"

    @classmethod
    def poll(cls, context):
        return context.object and context.object.active_material

    def _get_lines(self, context):
        return context.object.active_material.to_fds_list(context).to_string()


class SCENE_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from Scene.
    """

    bl_label = "Show FDS Code"
    bl_idname = "scene.bf_show_fds_code"
    bl_description = "Show FDS code exported from Scene"

    @classmethod
    def poll(cls, context):
        return context.scene

    def _get_lines(self, context):
        return context.scene.to_fds_list(context).to_string()


class SCENE_OT_bf_show_text(Operator):
    """!
    Show free text in the editor.
    """

    bl_label = "Show Free Text"
    bl_idname = "scene.bf_show_text"
    bl_description = "Show free text in the editor"

    def execute(self, context):
        context.scene.bf_config_text = utils.ui.get_text_in_editor(
            context=context,
            text=context.scene.bf_config_text,
            name="Text",
        )
        self.report(
            {"INFO"}, f"See Blender text editor: <{context.scene.bf_config_text.name}>"
        )
        return {"FINISHED"}


bl_classes = [
    WM_OT_bf_dialog,
    OBJECT_OT_bf_show_fds_code,
    COLLECTION_OT_bf_show_fds_code,
    MATERIAL_OT_bf_show_fds_code,
    SCENE_OT_bf_show_fds_code,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
