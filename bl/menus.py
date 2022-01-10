"""!
BlenderFDS, import/export menu.
"""

import os

import bpy, logging
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper
from .. import utils
from ..types import BFException

log = logging.getLogger(__name__)


class ImportFDS(Operator, ImportHelper):
    """!
    Import FDS case file to a Blender Scene.
    """

    bl_idname = "import_scene.fds"
    bl_label = "Import FDS"
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

    def _get_new_scene_from_filepath(self, filepath):
        filepath = bpy.path.abspath(filepath)
        name, path = utils.os_filepath_to_bl(filepath)
        # Create new scene
        sc = bpy.data.scenes.new(name)
        sc.bf_config_directory = path
        return sc

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
                sc = self._get_new_scene_from_filepath(filepath)
            else:
                sc = context.scene
            try:
                sc.from_fds(context, filepath=filepath)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, f"Import error: {str(err)}")
                return {"CANCELLED"}

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, f"<{sc.name}> imported")
        return {"FINISHED"}


class ExportFDS(Operator, ExportHelper):
    """!
    Export Blender Scene to an FDS case file.
    """

    bl_idname = "export_scene.fds"
    bl_label = "Export FDS"
    bl_description = "Export Blender Scenes as FDS case files"

    # Inherited from ExportHelper
    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    # Custom
    all_scenes: BoolProperty(
        name="All Scenes",
        default=False,
        description="Export all available Scenes to the selected path.",
    )
    all_surfs: BoolProperty(
        name="All SURFs",
        default=False,
        description="Export all available SURFs, even unrelated to exported Objects.",
    )

    def _export_scene(self, context, sc):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        try:
            sc.to_fds(
                context=context,
                full=True,
                all_surfs=self.all_surfs,
                filepath=self.filepath,
            )
        except BFException as err:
            self.report({"ERROR"}, f"Export error: {str(err)}")
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, f"Scene <{sc.name}> exported")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()

    def invoke(self, context, event):
        sc = context.scene
        try:  # get best filepath for dialog
            self.filepath = utils.bl_path_to_os(
                bl_path=sc.bf_config_directory or "//",
                name=sc.name,
                extension=".fds",
            )
        except BFException:
            self.filepath = bpy.path.ensure_ext(sc.name, ".fds")
        return super().invoke(context, event)  # open dialog

    def execute(self, context):
        if self.all_scenes:
            for sc in bpy.data.scenes:
                res = self._export_scene(context=context, sc=sc)
                if res != {"FINISHED"}:  # on any CANCEL break
                    break
        else:
            res = self._export_scene(context=context, sc=context.scene)
        return res


# Menu functions


def menu_func_import_FDS(self, context):
    self.layout.operator("import_scene.fds", text="NIST FDS (.fds)")


def menu_func_export_to_fds(self, context):
    self.layout.operator(ExportFDS.bl_idname, text="NIST FDS (.fds)")


# Register


def register():
    from bpy.utils import register_class

    register_class(ImportFDS)
    register_class(ExportFDS)

    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    from bpy.utils import unregister_class

    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)

    unregister_class(ImportFDS)
    unregister_class(ExportFDS)
