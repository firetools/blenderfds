# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Object
from bpy.props import FloatProperty, IntProperty, FloatVectorProperty, BoolProperty
from ...config import LP
from ...types import BFNamelist, BFParam, BFNotImported, FDSList
from ... import utils


log = logging.getLogger(__name__)


def update_bf_mult(ob, context):
    utils.geometry.rm_geometric_cache(ob=ob)
    if ob.bf_has_tmp:
        utils.geometry.rm_tmp_objects()


class OP_MULT_ID(BFParam):
    label = "ID"
    description = "Multiplier transformation name"
    fds_label = "ID"
    bpy_type = Object

    def get_value(self, context):
        return f"{self.element.name}_mult"

    def set_value(self, context, value=None):
        pass


class OP_MULT_DX(BFParam):
    label = "DX"
    description = "Displacement along axis x"
    fds_label = "DX"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dx"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_DY(BFParam):
    label = "DY"
    description = "Displacement along axis y"
    fds_label = "DY"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dy"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_DZ(BFParam):
    label = "DZ"
    description = "Displacement along axis z"
    fds_label = "DZ"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dz"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_DX0(BFParam):
    label = "DX0"
    description = "Initial displacement along axis x"
    fds_label = "DX0"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dx0"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_DY0(BFParam):
    label = "DY0"
    description = "Initial displacement along axis y"
    fds_label = "DY0"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dy0"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_DZ0(BFParam):
    label = "DZ0"
    description = "Initial displacement along axis z"
    fds_label = "DZ0"
    fds_default = 0.0
    bpy_type = Object
    bpy_idname = "bf_mult_dz0"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "update": update_bf_mult,
    }


class OP_MULT_I_LOWER(BFParam):
    label = "I_LOWER"
    description = "Lower I index"
    fds_label = "I_LOWER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_i_lower"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_I_LOWER_SKIP(BFParam):
    label = "I_LOWER_SKIP"
    description = "Lower skip I index"
    fds_label = "I_LOWER_SKIP"
    fds_default = -999
    bpy_type = Object
    bpy_idname = "bf_mult_i_lower_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_i_lower_skip >= ob.bf_mult_i_lower


class OP_MULT_I_UPPER_SKIP(BFParam):
    label = "I_UPPER_SKIP"
    description = "Upper skip I index"
    fds_label = "I_UPPER_SKIP"
    fds_default = +999
    bpy_type = Object
    bpy_idname = "bf_mult_i_upper_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_i_upper_skip <= ob.bf_mult_i_upper


class OP_MULT_I_UPPER(BFParam):
    label = "I_UPPER"
    description = "Upper I index"
    fds_label = "I_UPPER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_i_upper"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_J_LOWER(BFParam):
    label = "J_LOWER"
    description = "Lower J index"
    fds_label = "J_LOWER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_j_lower"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_J_LOWER_SKIP(BFParam):
    label = "J_LOWER_SKIP"
    description = "Lower skip J index"
    fds_label = "J_LOWER_SKIP"
    fds_default = -999
    bpy_type = Object
    bpy_idname = "bf_mult_j_lower_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_j_lower_skip >= ob.bf_mult_j_lower


class OP_MULT_J_UPPER_SKIP(BFParam):
    label = "J_UPPER_SKIP"
    description = "Upper skip J index"
    fds_label = "J_UPPER_SKIP"
    fds_default = +999
    bpy_type = Object
    bpy_idname = "bf_mult_j_upper_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_j_upper_skip <= ob.bf_mult_j_upper


class OP_MULT_J_UPPER(BFParam):
    label = "J_UPPER"
    description = "Upper J index"
    fds_label = "J_UPPER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_j_upper"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_K_LOWER(BFParam):
    label = "K_LOWER"
    description = "Lower K index"
    fds_label = "K_LOWER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_k_lower"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_K_LOWER_SKIP(BFParam):
    label = "K_LOWER_SKIP"
    description = "Lower skip K index"
    fds_label = "K_LOWER_SKIP"
    fds_default = -999
    bpy_type = Object
    bpy_idname = "bf_mult_k_lower_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_k_lower_skip >= ob.bf_mult_k_lower


class OP_MULT_K_UPPER_SKIP(BFParam):
    label = "K_UPPER_SKIP"
    description = "Upper skip K index"
    fds_label = "K_UPPER_SKIP"
    fds_default = +999
    bpy_type = Object
    bpy_idname = "bf_mult_k_upper_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_k_upper_skip <= ob.bf_mult_k_upper


class OP_MULT_K_UPPER(BFParam):
    label = "K_UPPER"
    description = "Upper K index"
    fds_label = "K_UPPER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_k_upper"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


# MULT 2


class OP_MULT_DXB(BFParam):
    label = "DXB"
    description = "Displacements along XB"
    fds_label = "DXB"
    fds_default = tuple((0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    bpy_type = Object
    bpy_idname = "bf_mult_dxb"
    bpy_prop = FloatVectorProperty
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "unit": "LENGTH",
        "size": 6,
        "update": update_bf_mult,
    }


class OP_MULT_N_LOWER(BFParam):
    label = "N_LOWER"
    description = "Lower N index"
    fds_label = "N_LOWER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_n_lower"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class OP_MULT_N_LOWER_SKIP(BFParam):
    label = "N_LOWER_SKIP"
    description = "Lower skip N index"
    fds_label = "N_LOWER_SKIP"
    fds_default = -999
    bpy_type = Object
    bpy_idname = "bf_mult_n_lower_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_n_lower_skip >= ob.bf_mult_n_lower


class OP_MULT_N_UPPER_SKIP(BFParam):
    label = "N_UPPER_SKIP"
    description = "Upper skip N index"
    fds_label = "N_UPPER_SKIP"
    fds_default = +999
    bpy_type = Object
    bpy_idname = "bf_mult_n_upper_skip"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}

    def get_exported(self, context):
        ob = self.element
        return ob.bf_mult_n_upper_skip <= ob.bf_mult_n_upper


class OP_MULT_N_UPPER(BFParam):
    label = "N_UPPER"
    description = "Upper N index"
    fds_label = "N_UPPER"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mult_n_upper"
    bpy_prop = IntProperty
    bpy_other = {"update": update_bf_mult}


class ON_MULT(BFNamelist):  # not in namelist menu
    label = "MULT"
    description = "Multiplier"
    bpy_type = Object
    enum_id = None
    fds_label = "MULT"
    bpy_export = "bf_mult_export"
    bpy_export_default = False
    bpy_other = {"update": update_bf_mult}
    bf_params = (
        OP_MULT_ID,  # used when importing
        OP_MULT_DX,
        OP_MULT_DY,
        OP_MULT_DZ,
        OP_MULT_DX0,
        OP_MULT_DY0,
        OP_MULT_DZ0,
        OP_MULT_DXB,
        OP_MULT_I_LOWER,
        OP_MULT_I_LOWER_SKIP,
        OP_MULT_I_UPPER_SKIP,
        OP_MULT_I_UPPER,
        OP_MULT_J_LOWER,
        OP_MULT_J_LOWER_SKIP,
        OP_MULT_J_UPPER_SKIP,
        OP_MULT_J_UPPER,
        OP_MULT_K_LOWER,
        OP_MULT_K_LOWER_SKIP,
        OP_MULT_K_UPPER_SKIP,
        OP_MULT_K_UPPER,
        OP_MULT_N_LOWER,
        OP_MULT_N_LOWER_SKIP,
        OP_MULT_N_UPPER_SKIP,
        OP_MULT_N_UPPER,
    )

    def draw(self, context, layout):  # TODO feedback if DXB used
        ob = self.element

        row = layout.row(align=True)  # X,Y,Z

        col = row.column(align=True)
        col.alignment = "RIGHT"
        col.label(text=" ")
        col.label(text="X")
        col.label(text="Y")
        col.label(text="Z")

        col = row.column(align=True)
        col.label(text="DX, DY, DZ")
        col.prop(ob, "bf_mult_dx", text="")
        col.prop(ob, "bf_mult_dy", text="")
        col.prop(ob, "bf_mult_dz", text="")

        col = row.column(align=True)
        col.label(text="DX0, DY0, DZ0")
        col.prop(ob, "bf_mult_dx0", text="")
        col.prop(ob, "bf_mult_dy0", text="")
        col.prop(ob, "bf_mult_dz0", text="")

        row = layout.row(align=True)  # DXB
        row.label(text="DXB")
        row.prop(ob, "bf_mult_dxb", text="")

        row.separator()

        row = layout.row(align=True)  # I,J,K,N

        col = row.column(align=True)
        col.alignment = "RIGHT"
        col.label(text=" ")
        col.label(text="I")
        col.label(text="J")
        col.label(text="K")
        col.separator()
        col.label(text="N")

        col = row.column(align=True)
        col.label(text="LOWER")
        col.prop(ob, "bf_mult_i_lower", text="")
        col.prop(ob, "bf_mult_j_lower", text="")
        col.prop(ob, "bf_mult_k_lower", text="")
        col.separator()
        col.prop(ob, "bf_mult_n_lower", text="")

        col = row.column(align=True)
        col.label(text="LOWER_SKIP")
        col.prop(ob, "bf_mult_i_lower_skip", text="")
        col.prop(ob, "bf_mult_j_lower_skip", text="")
        col.prop(ob, "bf_mult_k_lower_skip", text="")
        col.separator()
        col.prop(ob, "bf_mult_n_lower_skip", text="")

        col = row.column(align=True)
        col.label(text="UPPER_SKIP")
        col.prop(ob, "bf_mult_i_upper_skip", text="")
        col.prop(ob, "bf_mult_j_upper_skip", text="")
        col.prop(ob, "bf_mult_k_upper_skip", text="")
        col.separator()
        col.prop(ob, "bf_mult_n_upper_skip", text="")

        col = row.column(align=True)
        col.label(text="UPPER")
        col.prop(ob, "bf_mult_i_upper", text="")
        col.prop(ob, "bf_mult_j_upper", text="")
        col.prop(ob, "bf_mult_k_upper", text="")
        col.separator()
        col.prop(ob, "bf_mult_n_upper", text="")


# Called by other namelists
# that support a MULT_ID
# and the relative MULT namelist
# (See also: ON_MOVE)
# It is not exported: multiples are always generated


class OP_other_MULT_ID(BFParam):
    label = "MULT_ID"
    description = "Reference to multiplier transformation"
    fds_label = "MULT_ID"  # for importing only
    bpy_type = Object

    def get_exported(self, context):
        return False

    def set_value(self, context, value=None):
        # Get required MULT parameters from dict created by SN_MULT
        try:
            f90_params = context.scene["bf_mult_coll"][value]
        except KeyError as err:
            raise BFNotImported(self, f"Missing MULT ID='{value}'")
        ON_MULT(element=self.element).from_fds_list(
            context=context,
            fds_list=FDSList(f90_params=f90_params),
        )
