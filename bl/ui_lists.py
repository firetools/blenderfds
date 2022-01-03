from bpy.types import PropertyGroup, UIList
from bpy.props import BoolProperty, StringProperty
from bpy.utils import register_class, unregister_class
import logging

log = logging.getLogger(__name__)

# Prepare for "Other" and "List of filepaths" parameters
# PropertyGroup and UIList
# The PG properties should always be: bf_export, name


class WM_PG_bf_other(PropertyGroup):
    """!
    Blender PropertyGroup for items of 'other' FDS parameters.
    """

    bf_export: BoolProperty(name="Export", default=True)
    name: StringProperty(name="Name")


class WM_UL_bf_other_items(UIList):
    """!
    Blender UIList for items of 'other' FDS parameters.
    """

    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


class WM_PG_bf_filepaths(PropertyGroup):
    """!
    Blender PropertyGroup for items of 'filepaths' FDS parameters.
    """

    bf_export: BoolProperty(name="Export", default=False)
    name: StringProperty(name="Name", subtype="FILE_PATH")


class WM_UL_bf_filepaths_items(UIList):
    """!
    Blender UIList for items of 'filepaths' FDS parameters.
    """

    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


# Register

bl_classes = (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
    WM_PG_bf_filepaths,
    WM_UL_bf_filepaths_items,
)


def register():
    log.debug("Register ui_lists...")
    for c in bl_classes:
        register_class(c)


def unregister():
    log.debug("Unregister ui_lists...")
    for c in bl_classes:
        unregister_class(c)
