"""!
BlenderFDS, import/export menu.
"""

import os, bpy, logging
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper
from .. import utils
from ..types import BFException

log = logging.getLogger(__name__)


class ImportFDSToScene(Operator, ImportHelper):
    """!
    Import FDS case file to a Blender Scene.
    """

    bl_idname = "import_scene.fds"
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
        return context.scene is not None

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
                sc = bpy.data.scenes.new("tmp_name")
            else:
                sc = context.scene
            try:
                sc.from_fds(context, filepath=filepath)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Import: {str(err)}")
                return {"CANCELLED"}

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, f"{len(filepaths)} imported")
        return {"FINISHED"}


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

    def invoke(self, context, event):
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
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Export: {str(err)}")
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

    def invoke(self, context, event):
        return context.window_manager.invoke_confirm(self, event)  # confirm

    def execute(self, context):
        # Check destination
        for sc in bpy.data.scenes:
            if not sc.bf_config_directory:
                self.report(
                    {"ERROR"}, f"Missing FDS case directory in Scene <{sc.name}>"
                )
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
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Export: {str(err)}")
                return {"CANCELLED"}
        w.cursor_modal_restore()
        self.report({"INFO"}, f"{len(bpy.data.scenes)} exported")
        return {"FINISHED"}


# Menu functions


def menu_func_import_FDS(self, context):
    self.layout.operator(ImportFDSToScene.bl_idname, text="NIST FDS as Scene")


def menu_func_export_to_fds(self, context):
    self.layout.operator(ExportAllSceneToFDS.bl_idname, text="All Scenes as NIST FDS")
    self.layout.operator(ExportSceneToFDS.bl_idname, text="Current Scene as NIST FDS")


bl_classes = [ImportFDSToScene, ExportSceneToFDS, ExportAllSceneToFDS]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)

    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    from bpy.utils import unregister_class

    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)

    for c in reversed(bl_classes):
        unregister_class(c)
