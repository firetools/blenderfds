import bpy
from bpy.types import Operator
from bpy.props import StringProperty
from bpy_extras.io_utils import ExportHelper
from ... import utils
from ...types import BFException


class ExportSceneToFDS(Operator, ExportHelper):
    """!
    Export current Blender Scene to an FDS case file.
    """

    bl_idname = "export_current_scene.fds"
    bl_label = "Export to FDS"
    bl_description = "Export current Blender Scene to an FDS case"
    bl_options = {"PRESET", "UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene

    def invoke(self, context, event):
        # Check saved
        if not bpy.data.is_saved:
            self.report({"ERROR"}, "Save the Blender file first!")
            return {"CANCELLED"}

        # Set best filepath as default, empty path is /home/user
        sc = context.scene
        self.filepath = utils.io.append_filename(
            path=sc.bf_config_directory, name=sc.name, extension=".fds"
        )
        return super().invoke(context, event)

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        sc = context.scene

        # If filepath was relative, keep it relative
        if not utils.io.is_abs(sc.bf_config_directory):
            self.filepath = bpy.path.relpath(self.filepath)
        sc.bf_config_directory, sc.name = utils.io.extract_path_name(self.filepath)

        # Export
        try:
            sc.to_fds(
                context=context,
                full=True,
                save=True,
            )
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, f"Current Scene exported to <{sc.bf_config_directory}>")
        return {"FINISHED"}


class ExportAllSceneToFDS(Operator):
    """!
    Export all Blender Scene to its FDS case file.
    """

    bl_idname = "export_all_scene.fds"
    bl_label = "Export all Scene to FDS"
    bl_description = "Export all Blender Scene to its FDS case file"

    @classmethod
    def poll(cls, context):
        return context.scene

    def invoke(self, context, event):
        # Check saved
        if not bpy.data.is_saved:
            self.report({"ERROR"}, "Save the Blender file first!")
            return {"CANCELLED"}

        return context.window_manager.invoke_confirm(self, event)  # confirm

    def execute(self, context):
        # Check destination
        for sc in bpy.data.scenes:
            if not sc.bf_config_directory:
                msg = f"Missing FDS case directory in Scene <{sc.name}>"
                self.report({"ERROR"}, msg)
                return {"CANCELLED"}

        # Export
        for sc in bpy.data.scenes:
            w = context.window_manager.windows[0]
            w.cursor_modal_set("WAIT")
            try:
                sc.to_fds(
                    context=context,
                    full=True,
                    save=True,
                )
            except Exception as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, f"{len(bpy.data.scenes)} exported")
        return {"FINISHED"}


bl_classes = [ExportSceneToFDS, ExportAllSceneToFDS]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
