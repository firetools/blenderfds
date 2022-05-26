# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, import/export menus.
"""

import bpy


def menu_func_import_FDS(self, context):
    self.layout.operator("import_to_scene.fds", text="NIST FDS Case (.fds)")


def menu_func_export_to_fds(self, context):
    self.layout.operator("export_scene.fds", text="NIST FDS Case (.fds)")


def register():
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
