# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, text management utilities.
"""

from ..config import MAXLEN, INDENT


def append_word(lines, word, separator=" ", force_break=False) -> list:
    """!
    Append word to the last of lines, generate newline and ident if needed.
    """
    if not force_break and len(lines[-1]) + len(separator) + len(word) <= MAXLEN:
        # append to current line
        lines[-1] += separator + word
    else:
        # append to new line w indent, wo separator
        lines.append(" " * INDENT + word)
    return lines