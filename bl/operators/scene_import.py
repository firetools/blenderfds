# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to import an FDS case.
"""

import os, bpy
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty
from bpy_extras.io_utils import ImportHelper
from ... import utils
from ...types import BFException


def _get_all_filepaths(filepath, extension=".fds"):
    filepaths = list()
    for f in os.scandir(os.path.dirname(filepath)):
        if os.path.isfile(f) and f.name.endswith(extension):
            filepaths.append(f.path)
    return filepaths


def _import_fds_to_scene(context, filepath, to_new_scene=True):
    if to_new_scene:
        sc = bpy.data.scenes.new("New case")
    else:
        sc = context.scene
    fds_namelist_qty = sc.from_fds(
        context,
        filepath=filepath,
    )
    return fds_namelist_qty


class ImportFDSToScene(Operator, ImportHelper):
    """!
    Import FDS case file to a Blender Scene.
    """

    bl_idname = "import_to_scene.fds"
    bl_label = "Import from FDS"
    bl_description = "Import FDS case file to a Blender Scene"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    all_cases: BoolProperty(
        name="Import All",
        default=False,
        description="Import all available cases in selected path.",
    )

    @classmethod
    def poll(cls, context):
        return context.scene

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")

        # Set filepaths
        if self.all_cases:
            filepaths = _get_all_filepaths(filepath, extension=".fds")
        else:
            filepaths = (self.filepath,)

        # Import
        for filepath in filepaths:
            try:
                _import_fds_to_scene(context, filepath, to_new_scene=True)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Import <{filepath}>: {err}")
                return {"CANCELLED"}

        # Close
        utils.ui.view_all(context=context)
        w.cursor_modal_restore()
        self.report({"INFO"}, f"{len(filepaths)} imported")
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
        filepath = self.filepath
        try:
            fds_namelist_qty = _import_fds_to_scene(
                context, filepath, to_new_scene=False
            )
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Import <{filepath}>: {err}")
            return {"CANCELLED"}

        # Close
        utils.ui.view_all(context=context)
        w.cursor_modal_restore()
        self.report({"INFO"}, f"{fds_namelist_qty} namelists imported")
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
