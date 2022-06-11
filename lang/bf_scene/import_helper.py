# SPDX-License-Identifier: GPL-3.0-or-later

import logging, bpy
from bpy.types import Scene, Object, Material
from ...types import BFNamelist, BFNotImported
from ... import utils

log = logging.getLogger(__name__)


def _import_sc(context, sc, bf_namelist, fds_namelist, texts):
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
    co_description="",
    set_tmp=False,
):
    # Get the right collection
    if set_tmp:
        co = sc.collection
    else:
        co_name = f"New {bf_namelist.collection}"
        if co_description:
            co_name += f" | From: <{co_description}>"
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
    except BFNotImported as err:
        # Show error in Free Text
        texts.append(str(err))
        return False
    return True


def _import_ma(context, sc, fds_namelist, hid, texts):
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
    context,
    sc,
    fds_list,
    fds_label=None,
    co_description="",
    set_tmp=False,
) -> str:
    """!
    Import all namelists with fds_label from fds_list into Scene.
    """
    # Scan fds_list for fds_label
    texts = list()
    while True:
        fds_namelist = fds_list.get_by_fds_label(fds_label=fds_label, remove=True)
        if not fds_namelist:
            break

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
                    co_description=co_description,
                    set_tmp=set_tmp,
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

    # Finally, write free text
    if co_description:
        header = f"-- From: <{co_description}>"
    else:
        header = f"-- From: <{sc.name}.fds>"
    utils.ui.write_bl_text(
        context, bl_text=sc.bf_config_text, header=header, texts=texts
    )
