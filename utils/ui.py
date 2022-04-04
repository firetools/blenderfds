"""!
BlenderFDS, Blender user interface utilities.
"""

import bpy


def get_screen_area(context, area_type="PROPERTIES"):
    """!
    Get existing ui area or create one
    """
    selected_area = None
    for w in context.window_manager.windows:
        for area in w.screen.areas:
            if area.type == area_type:
                selected_area = area
                break
    if not selected_area:
        # Call user prefs window
        bpy.ops.screen.userpref_show("INVOKE_DEFAULT")
        # Change area type
        area = context.window_manager.windows[-1].screen.areas[0]
        area.type = area_type
        selected_area = area
    return selected_area


def get_text_in_editor(context, text=None, name=None):
    """!
    Show text in Blender Text Editor.
    """
    # If not given, create text
    if not text:
        text = bpy.data.texts.new(name or str())
    # Rewind to first line
    text.current_line_index = 0
    # Search existing ui area or create one
    selected_area = get_screen_area(context, area_type="TEXT_EDITOR")
    # Set highlighting
    space = selected_area.spaces[0]
    space.text = text
    space.show_line_numbers = True
    space.show_line_highlight = True
    space.show_word_wrap = True
    space.show_margin = True
    space.margin_column = 130
    space.show_syntax_highlight = True
    return


def show_property_panel(context, space_context="MATERIAL"):
    """!
    Show Material Panel.
    """
    selected_area = get_screen_area(context, area_type="PROPERTIES")
    selected_area.spaces[0].context = space_context


def view_all(context):
    """!
    Run view3d.view_all operator.
    """
    for area in context.screen.areas:
        if area.type == "VIEW_3D":
            for region in area.regions:
                if region.type == "WINDOW":
                    override = {
                        "area": area,
                        "region": region,
                        "edit_object": context.edit_object,
                    }
                    bpy.ops.view3d.view_all(override)
            for space in area.spaces:
                if space.type == "VIEW_3D":
                    space.clip_end = 1e6
