# SPDX-License-Identifier: GPL-3.0-or-later

from fileinput import filename
import logging, bpy

from bpy.types import Scene, Object, Material
from ...types import BFNamelist, BFNotImported
from ... import utils

log = logging.getLogger(__name__)


def sc_from_fds_list(context, sc, fds_list, set_tmp=False, texts=(), filename=None):

    # Special treatment for the REAC namelists
    fds_label = "REAC"
    fds_namelists = fds_list.get_fds_namelists(fds_label=fds_label, remove=True)
    if len(fds_namelists) == 1:
        bf_namelist = BFNamelist.get_subclass(fds_label=fds_label)
        _import_sc(context, sc, bf_namelist, fds_namelists[0], texts)
    elif len(fds_namelists) > 1:
        texts.append(f"REAC: {len(fds_namelists)} reactions")
        texts.extend(n.to_string() for n in fds_namelists)

    # Import by fds_label
    fds_labels = (
        "HEAD",
        "MOVE",  # pre-load moves
        "MULT",  # pre-load multiplicity
        "MESH",  # create domain collection
        "SURF",  # load SURFs
        "CATF",  # load additional SURFs
        "OBST",
        "GEOM",
        None,
    )
    for fds_label in fds_labels:
        import_by_fds_label(
            context=context,
            sc=sc,
            fds_list=fds_list,
            fds_label=fds_label,
            set_tmp=set_tmp,  # TODO or Scene var?
            texts=texts,
            filename=filename,
        )

    # Empty temporary Scene collections and states
    # they get created in SN_MOVE, SN_MULT, SN_REAC
    if "bf_move_coll" in sc:
        del sc["bf_move_coll"]
    if "bf_mult_coll" in sc:
        del sc["bf_mult_coll"]


def _import_sc(context, sc, bf_namelist, fds_namelist, texts):
    """!
    Import Scene related namelist.
    """
    is_imported = False
    try:
        bf_namelist(element=sc).from_fds(context=context, fds_namelist=fds_namelist)
        is_imported = True
    except BFNotImported as err:
        texts.append(str(err))
    return is_imported


def _import_ob(
    context,
    sc,
    bf_namelist,
    fds_namelist,
    hid,
    texts,
    set_tmp=False,
    co_description=None,
):
    """!
    Import Object related namelist.
    """
    # Get the right collection
    if set_tmp:
        co = sc.collection
    else:
        co_name = f"New {bf_namelist.collection}"
        if co_description:
            co_name += f" from <{co_description}>"
        co = bpy.data.collections.get(co_name)
        if co and not sc.user_of_id(co):  # not in current scene?
            co.name = f"{co_name}.001"  # rename existing
            co = None  # not the right one
        if not co:
            co = bpy.data.collections.new(name=co_name)
            sc.collection.children.link(co)

    # Create the new Object
    ob = utils.geometry.get_new_object(context=context, name=hid, co=co)

    # Set it tmp
    if set_tmp:
        utils.geometry.set_is_tmp(context=context, ob=ob)

    # Fill it
    try:
        ob.from_fds(context, fds_namelist=fds_namelist)
    except BFNotImported as err:  # FIXME what's the exact role of BFNotImported and BFException?
        # Show error in Free Text
        texts.append(str(err))
        return False
    return True


def _import_ma(context, sc, fds_namelist, hid, texts):
    """!
    Import Material related namelist.
    """
    is_imported = False
    ma = bpy.data.materials.new(hid)  # new Material
    try:
        ma.from_fds(context, fds_namelist=fds_namelist)
        is_imported = True
    except BFNotImported as err:
        # Show error in Free Text
        texts.append(str(err))
    return is_imported


def import_by_fds_label(
    context, sc, fds_list, fds_label=None, set_tmp=False, texts=(), filename=None
) -> str:
    """!
    Import all namelists with fds_label from fds_list into Scene.
    """
    # Scan fds_list for fds_label
    for fds_namelist in fds_list.get_fds_namelists(fds_label=fds_label, remove=True):

        # Manage found
        bf_namelist = BFNamelist.get_subclass(fds_label=fds_namelist.fds_label)
        is_imported = False
        if bf_namelist:
            hid = f"New {fds_namelist.fds_label}"

            if bf_namelist.bpy_type == Scene:
                is_imported = _import_sc(
                    context=context,
                    sc=sc,
                    bf_namelist=bf_namelist,
                    fds_namelist=fds_namelist,
                    texts=texts,
                )

            elif bf_namelist.bpy_type == Object:
                is_imported = _import_ob(
                    context=context,
                    sc=sc,
                    bf_namelist=bf_namelist,
                    fds_namelist=fds_namelist,
                    hid=hid,
                    texts=texts,
                    set_tmp=set_tmp,
                    co_description=filename,
                )

            elif bf_namelist.bpy_type == Material:
                is_imported = _import_ma(
                    context=context,
                    sc=sc,
                    fds_namelist=fds_namelist,
                    hid=hid,
                    texts=texts,
                )

            else:
                raise AssertionError(f"Unhandled bf_namelist for <{fds_namelist}>")

        # Last resort, import to Free Text
        if not is_imported:
            texts.append(fds_namelist.to_string())
