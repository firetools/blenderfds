"""!
BlenderFDS, operators to show generated FDS code.
"""

import logging, bpy
from bpy.types import Operator
from bpy.props import StringProperty, EnumProperty
from ...types import BFException

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
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
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
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        if self.lines:
            lines = self.lines.split("\n")
        else:
            lines = ("No FDS code is exported",)
        if len(lines) > 60:
            lines = lines[:55] + ["..."] + lines[-4:]
        layout = self.layout
        for line in lines:
            layout.label(text=line)

    def execute(self, context):
        self.report({"INFO"}, "FDS code shown")
        return {"FINISHED"}

    def _get_lines(self, context):
        """!
        Placeholder method to get text lines.
        @param context: the Blender context.
        @return text
        """
        return str()

    def invoke(self, context, event):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        try:
            self.lines = self._get_lines(context)  # get FDS code
        except BFException as err:
            self.report({"ERROR"}, f"Show: {err}")
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

    bl_label = "FDS Code"
    bl_idname = "object.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Object"

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object

    def _get_lines(self, context):
        """!
        Get Object related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.active_object.to_fds(context)


class MATERIAL_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Material.
    """

    bl_label = "FDS Code"
    bl_idname = "material.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Material"

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object and context.active_object.active_material

    def _get_lines(self, context):
        """!
        Get Material related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.active_object.active_material.to_fds(context)


class SCENE_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from Scene.
    """

    bl_label = "FDS Code"
    bl_idname = "scene.bf_show_fds_code"
    bl_description = "Show FDS code exported from Scene"

    @classmethod
    def poll(cls, context):
        return context.scene

    def _get_lines(self, context):
        """!
        Get Scene related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.scene.to_fds(context)


class SCENE_OT_bf_show_text(Operator):
    """!
    Show free text in the editor.
    """

    bl_label = "Show Free Text"
    bl_idname = "scene.bf_show_text"
    bl_description = "Show free text in the editor"

    def execute(self, context):
        te = context.scene.bf_config_text
        # If not existing, create one
        if not te:
            bpy.ops.text.new()
            te = bpy.data.texts[-1]
            context.scene.bf_config_text = te
        # Show text in text editor
        done = False
        for w in context.window_manager.windows:
            for area in w.screen.areas:
                if area.type == "TEXT_EDITOR":
                    space = area.spaces[0]
                    space.text = te
                    space.show_line_numbers = True
                    space.show_line_highlight = True
                    space.show_word_wrap = True
                    space.show_margin = True
                    space.margin_column = 130
                    space.show_syntax_highlight = True
                    done = True
                    break
        if done:
            self.report({"INFO"}, f"See Blender text editor: <{te.name}>")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, f"Open Blender text editor first")
            return {"CANCELLED"}


bl_classes = [
    WM_OT_bf_dialog,
    OBJECT_OT_bf_show_fds_code,
    MATERIAL_OT_bf_show_fds_code,
    SCENE_OT_bf_show_fds_code,
    SCENE_OT_bf_show_text,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
