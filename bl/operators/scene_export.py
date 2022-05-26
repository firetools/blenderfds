# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to export an FDS case.
"""

import bpy
from bpy.types import Operator
from bpy.props import StringProperty
from bpy_extras.io_utils import ExportHelper
from ... import utils


class ExportSceneToFDS(Operator, ExportHelper):
    """!
    Export current Blender Scene to an FDS case file.
    """

    bl_idname = "export_scene.fds"
    bl_label = "Export to FDS"
    bl_description = "Export current Blender Scene to an FDS case file"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene

    def invoke(self, context, event):
        # Check if saved
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

        # If filepath was relative, keep it relative
        sc = context.scene
        if not utils.io.is_abs(sc.bf_config_directory):
            self.filepath = bpy.path.relpath(self.filepath)
        sc.bf_config_directory, sc.name = utils.io.extract_path_name(self.filepath)

        # Export
        try:
            sc.to_fds(context=context, full=True, save=True)
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, "FDS case exported")
        return {"FINISHED"}


# Register/unregister

bl_classes = (ExportSceneToFDS,)


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
