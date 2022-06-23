# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty, StringProperty
from ..config import HOC_P, RADIATIVE_FRACTION_P, YIELD_P
from ..types import BFParam, BFParamOther, BFParamFYI, BFNamelistSc
from ..bl.ui_lists import (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
)

log = logging.getLogger(__name__)


class SP_REAC_FYI(BFParamFYI):
    bpy_type = Scene
    bpy_idname = "bf_reac_fyi"


class SP_REAC_FUEL(BFParam):
    label = "FUEL"
    description = "Identificator of fuel species"
    fds_label = "FUEL"
    bpy_type = Scene
    bpy_prop = StringProperty
    bpy_idname = "bf_reac_fuel"


class SP_REAC_FORMULA(BFParam):
    label = "FORMULA"
    description = "Chemical formula of fuel species, it can only contain C, H, O, or N"
    fds_label = "FORMULA"
    bpy_type = Scene
    bpy_prop = StringProperty
    bpy_idname = "bf_reac_formula"
    bpy_export = "bf_reac_formula_export"
    bpy_export_default = False


class SP_REAC_CO_YIELD(BFParam):
    label = "CO_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into carbon monoxide"
    fds_label = "CO_YIELD"
    fds_default = 0.0
    bpy_type = Scene
    bpy_prop = FloatProperty
    bpy_idname = "bf_reac_co_yield"
    bpy_other = {"step": 1.0, "precision": YIELD_P, "min": 0.0, "max": 1.0}
    bpy_export = "bf_reac_co_yield_export"
    bpy_export_default = False


class SP_REAC_SOOT_YIELD(SP_REAC_CO_YIELD):
    label = "SOOT_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into smoke particulate"
    fds_label = "SOOT_YIELD"
    bpy_type = Scene
    bpy_idname = "bf_reac_soot_yield"
    bpy_export = "bf_reac_soot_yield_export"
    bpy_export_default = False


class SP_REAC_HEAT_OF_COMBUSTION(BFParam):
    label = "HEAT_OF_COMBUSTION [kJ/kg]"
    description = "Fuel heat of combustion"
    fds_label = "HEAT_OF_COMBUSTION"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_reac_heat_of_combustion"
    bpy_prop = FloatProperty
    bpy_other = {"precision": HOC_P, "min": 0.0}
    bpy_export = "bf_reac_heat_of_combustion_export"
    bpy_export_default = False


class SP_REAC_IDEAL(BFParam):
    label = "IDEAL"
    description = "Set ideal heat of combustion"
    fds_label = "IDEAL"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_reac_ideal"


class SP_REAC_RADIATIVE_FRACTION(BFParam):
    label = "RADIATIVE_FRACTION"
    description = (
        "Fraction of the total combustion energy that is released "
        "in the form of thermal radiation"
    )
    fds_label = "RADIATIVE_FRACTION"
    fds_default = 0.35
    bpy_type = Scene
    bpy_idname = "bf_reac_radiative_fraction"
    bpy_prop = FloatProperty
    bpy_other = {"precision": RADIATIVE_FRACTION_P, "min": 0.0, "max": 1.0}
    bpy_export = "bf_reac_radiative_fraction_export"
    bpy_export_default = False


class SP_REAC_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_reac_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_REAC(BFNamelistSc):
    label = "REAC"
    description = "Reaction"
    enum_id = 3004
    fds_label = "REAC"
    bpy_export = "bf_reac_export"
    bpy_export_default = False
    bf_params = (
        SP_REAC_FYI,
        SP_REAC_FUEL,
        SP_REAC_FORMULA,
        SP_REAC_CO_YIELD,
        SP_REAC_SOOT_YIELD,
        SP_REAC_HEAT_OF_COMBUSTION,
        SP_REAC_IDEAL,
        SP_REAC_RADIATIVE_FRACTION,  # moved from RADI
        SP_REAC_other,
    )
