"""!
BlenderFDS, import/export menus.
"""

import bpy


def menu_func_import_FDS(self, context):
    self.layout.operator("import_to_scene.fds", text="NIST FDS as New Scene (.fds)")


def menu_func_export_to_fds(self, context):
    self.layout.operator("export_all_scene.fds", text="All Scenes as NIST FDS (.fds)")
    self.layout.operator(
        "export_current_scene.fds", text="Current Scene as NIST FDS (.fds)"
    )


def register():
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
