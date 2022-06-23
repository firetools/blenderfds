# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operator to import an FDS case.
"""

import bpy
from bpy.types import Operator
from bpy.props import StringProperty
from bpy_extras.io_utils import ImportHelper
from ... import utils
from ...types import BFException


def _import_fds_case_to_scene(context, filepath, to_new_scene=True):
    if to_new_scene:
        sc = bpy.data.scenes.new("New case")
    else:
        sc = context.scene
    fds_namelist_qty = sc.from_fds(
        context,
        filepath=filepath,
    )
    return fds_namelist_qty  # Number of imported namelists


class ImportFDSToScene(Operator, ImportHelper):
    """!
    Import an FDS case file to a new Blender Scene.
    """

    bl_idname = "import_to_scene.fds"
    bl_label = "Import from FDS"
    bl_description = "Import an FDS case file to a new Blender Scene"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")

        # Import
        try:
            _import_fds_case_to_scene(context, self.filepath, to_new_scene=True)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Close
        utils.ui.view_all(context=context)
        w.cursor_modal_restore()
        self.report({"INFO"}, f"FDS case imported")
        return {"FINISHED"}


class ImportFDSToCurrentScene(Operator, ImportHelper):
    """!
    Import FDS case file to current Blender Scene.
    """

    bl_idname = "import_to_current_scene.fds"
    bl_label = "Import Snippet"
    bl_description = "Import FDS code snippet to current Blender Scene"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")

        # Import
        try:
            fds_namelist_qty = _import_fds_case_to_scene(
                context, self.filepath, to_new_scene=False
            )
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Close
        utils.ui.view_all(context=context)
        w.cursor_modal_restore()
        self.report(
            {"INFO"},
            fds_namelist_qty == 1
            and f"1 namelist imported"
            or f"{fds_namelist_qty} namelists imported",
        )
        return {"FINISHED"}


bl_classes = [
    ImportFDSToScene,
    ImportFDSToCurrentScene,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
