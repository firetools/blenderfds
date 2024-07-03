# SPDX-License-Identifier: GPL-3.0-or-later

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


def show_bl_text(context, bl_text=None, name=None):
    """!
    Show bl_text in Blender Text Editor.
    """
    # If not given, create text
    if not bl_text:
        bl_text = bpy.data.texts.new(name or str())
    # Rewind to the first line
    bl_text.current_line_index = 0
    # Search existing ui area or create one
    selected_area = get_screen_area(context, area_type="TEXT_EDITOR")
    # Set highlighting
    space = selected_area.spaces[0]
    space.text = bl_text
    space.show_line_numbers = True
    space.show_line_highlight = True
    space.show_word_wrap = True
    space.show_margin = True
    space.margin_column = 130
    space.show_syntax_highlight = True
    return bl_text


def write_bl_text(context, bl_text, header=None, texts=()):
    """!
    Write text in bl_text from Blender Text Editor.
    """
    if not texts:
        return
    # Prepare body
    body = bl_text.as_string().strip()
    if body:
        body += "\n"
        if header:
            body += "\n"
    if header:
        body += f"{header}\n"
    body += "\n".join(texts)
    bl_text.from_string(body)


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
                    with bpy.context.temp_override(**override):
                        bpy.ops.view3d.view_all()
            for space in area.spaces:
                if space.type == "VIEW_3D":
                    space.clip_end = 1e6
