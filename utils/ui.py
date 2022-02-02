"""!
BlenderFDS, Blender user interface utilities.
"""

import bpy


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
    selected_area = None
    for w in context.window_manager.windows:
        for area in w.screen.areas:
            if area.type == "TEXT_EDITOR":
                selected_area = area
                break
    if not selected_area:
        # Call user prefs window
        bpy.ops.screen.userpref_show("INVOKE_DEFAULT")
        # Change area type
        area = context.window_manager.windows[-1].screen.areas[0]
        area.type = "TEXT_EDITOR"
        selected_area = area
    # Set highlighting
    space = selected_area.spaces[0]
    space.text = text
    space.show_line_numbers = True
    space.show_line_highlight = True
    space.show_word_wrap = True
    space.show_margin = True
    space.margin_column = 130
    space.show_syntax_highlight = True
    return text
