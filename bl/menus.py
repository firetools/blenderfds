"""!
BlenderFDS, import/export menu panel
"""

import os

import bpy, logging
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty, FloatProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper

from ..types import FDSCase
from .. import io
from ..utils import BFException, BFNotImported, is_iterable


log = logging.getLogger(__name__)


# Collections

bl_classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collection.
    @param cls: the class to subscribe.
    @return the class subscribed.
    """
    bl_classes.append(cls)
    return cls


# Import menus


@subscribe
class ImportFDS(Operator, ImportHelper):
    """!
    Import FDS case file to a Scene.
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
        """!
        Test if the operator can be called or not
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.scene is not None

    def _get_new_scene_from_filepath(self, filepath):
        filepath = bpy.path.abspath(filepath)
        name, path = io.calc_bl_name_and_dir(filepath)
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


def menu_func_import_FDS(self, context):
    """!
    Function to import FDS into a new scene.
    @param context: the Blender context.
    """
    self.layout.operator("import_scene.fds", text="NIST FDS (.fds)")


# Export menu


@subscribe
class ExportFDS(Operator, ExportHelper):
    """!
    Export current Scene to FDS case file.
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

    def _get_fds_filepath(self, sc):
        return io.calc_path(
            bl_path=sc.bf_config_directory or "//", name=sc.name, extension=".fds",
        )

    def _get_best_fds_filepath_for_dialog(self, sc):
        try:
            return self._get_fds_filepath(sc)
        except BFException:
            return bpy.path.ensure_ext(sc.name, ".fds")

    def _get_name_and_path_from_filepath(self, filepath):
        filepath = bpy.path.abspath(filepath)
        return io.calc_bl_name_and_dir(filepath)

    def _export_scene(self, context, sc):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        filepath = self._get_fds_filepath(sc)
        try:
            sc.to_fds(
                context=context, full=True, all_surfs=self.all_surfs, filepath=filepath
            )
        except BFException as err:
            self.report({"ERROR"}, f"Error assembling fds file:\n<{err}>")
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, f"Scene <{sc.name}> exported")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()

    def invoke(self, context, event):
        sc = context.scene
        self.filepath = self._get_best_fds_filepath_for_dialog(sc)
        return super().invoke(context, event)  # open dialog

    def execute(self, context):
        if self.all_scenes:
            for sc in bpy.data.scenes:
                res = self._export_scene(context=context, sc=sc)
                if res != {"FINISHED"}:  # on CANCEL break
                    break
        else:
            name, path = self._get_name_and_path_from_filepath(self.filepath)
            sc = context.scene
            sc.name, sc.bf_config_directory = name, path
            res = self._export_scene(context=context, sc=sc)
        return res


def menu_func_export_to_fds(self, context):
    """!
    Export function for current Scene to FDS case file.
    """
    self.layout.operator(ExportFDS.bl_idname, text="NIST FDS (.fds)")


# Register


def register():
    """!
    Load the Python classes and functions to Blender.
    """
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    """!
    Unload the Python classes and functions from Blender.
    """
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)
