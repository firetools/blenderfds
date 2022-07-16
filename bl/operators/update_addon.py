# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to update the addon.
"""

import logging
from bpy.types import Operator
from bpy.props import EnumProperty
from ...utils import updater

log = logging.getLogger(__name__)


class WM_OT_bf_update_tags(Operator):
    bl_label = "Update Tags"
    bl_idname = "wm.bf_update_tags"
    bl_description = "Update tags"

    def execute(self, context):
        try:
            context.window_manager["tags"] = updater.get_tags()
        except Exception as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "Tags updated")
            return {"FINISHED"}


def _get_target_items(self, context):
    tags = context.window_manager.get("tags", ())
    return ((t["name"], t["name"], f"Install {t['name']} version") for t in tags)


class WM_OT_bf_update_addon(Operator):
    bl_label = "Upgrade Addon"
    bl_idname = "wm.bf_update_addon"
    bl_description = "Upgrade, downgrade, or restore the addon from its repository"

    bf_target: EnumProperty(
        name="Version",
        description="Select the addon version to install",
        items=_get_target_items,
    )

    def draw(self, context):
        row = self.layout.row(align=True)
        row.prop(self, "bf_target")
        row.operator("wm.bf_update_tags", text="", icon="FILE_REFRESH")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):

        # Get zipball_url
        tags = context.window_manager.get("tags", list())
        url_file = None
        for t in tags:
            if t["name"] == self.bf_target:
                url_file = t["zipball_url"]
                break
        if not url_file:
            self.report({"ERROR"}, "Update cancelled, no zip file selected.")
            return {"CANCELLED"}

        # Install
        try:
            updater.install_addon(url_file=url_file)
        except Exception as err:
            self.report({"ERROR"}, f"Install failed: {err}")
            return {"CANCELLED"}
        self.report({"WARNING"}, f"Install completed, restart Blender!")
        context.window_manager["bf_restart_required"] = True
        return {"FINISHED"}


bl_classes = [
    WM_OT_bf_update_tags,
    WM_OT_bf_update_addon,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
