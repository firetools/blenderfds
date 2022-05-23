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

    new_scene: BoolProperty(
        name="New Scene",
        default=True,
        description="Import selected case into a new Scene.",
    )

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

        # List filepaths
        filepaths = list()
        if self.all_cases:
            for f in os.scandir(os.path.dirname(self.filepath)):
                if os.path.isfile(f) and f.name.endswith(".fds"):
                    filepaths.append(f.path)
        else:
            filepaths.append(self.filepath)

        # Import
        for filepath in filepaths:
            if self.new_scene:
                sc = bpy.data.scenes.new("New case")
            else:
                sc = context.scene
            try:
                sc.from_fds(
                    context,
                    filepath=filepath,
                )
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
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
        return bpy.ops.import_to_scene.fds(
            filepath=self.filepath, new_scene=False, all_cases=False
        )


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
