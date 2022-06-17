# SPDX-License-Identifier: GPL-3.0-or-later

import logging, bpy

from bpy.types import Scene, Object, Material
from ...types import BFNamelist, BFNotImported
from ... import utils

log = logging.getLogger(__name__)


def sc_from_fds_list(context, sc, fds_list, set_tmp=False, texts=(), filename=None):
    """Import in Scene from fds_list."""

    # Special treatment for REACs
    _treat_REACs(context, sc, fds_list, texts)

    # Get properly ordered list of managed bf_namelists
    managed_bf_namelists = sorted(
        (n for n in BFNamelist.subclasses if n.fds_label),
        key=lambda k: k.bf_import_order,
    )
    managed_fds_labels = tuple(n.fds_label for n in managed_bf_namelists)

    # Get fds_namelists
    fds_namelists = fds_list.get_fds_namelists()

    # Select managed and unmanaged fds_namelists
    fds_namelists.reverse()  # for pop from the beginning
    managed_fds_namelists_by_fds_label = dict()
    unmanaged_fds_namelists = list()
    while fds_namelists:
        fds_namelist = fds_namelists.pop()
        fds_label = fds_namelist.fds_label
        if fds_label in managed_fds_labels:
            try:
                managed_fds_namelists_by_fds_label[fds_label].append(fds_namelist)
            except KeyError:
                managed_fds_namelists_by_fds_label[fds_label] = list((fds_namelist,))
        else:
            unmanaged_fds_namelists.append(fds_namelist)

    # Import managed namelists
    for bf_namelist in managed_bf_namelists:
        managed_fds_label = bf_namelist.fds_label
        fds_namelists = managed_fds_namelists_by_fds_label.pop(managed_fds_label, ())
        hid = f"New {managed_fds_label}"

        # Import to calling Blender Scene
        if bf_namelist.bpy_type == Scene:
            _import_to_sc(
                context=context,
                sc=sc,
                bf_namelist=bf_namelist,
                fds_namelists=fds_namelists,
                texts=texts,
            )

        # Create new Blender Object
        elif bf_namelist.bpy_type == Object:
            _import_to_ob(
                context=context,
                sc=sc,
                bf_namelist=bf_namelist,
                fds_namelists=fds_namelists,
                hid=hid,
                texts=texts,
                set_tmp=set_tmp,
                filename=filename,
            )

        # Create new Blender Material instances
        elif bf_namelist.bpy_type == Material:
            _import_to_ma(
                context=context,
                sc=sc,
                bf_namelist=bf_namelist,
                fds_namelists=fds_namelists,
                hid=hid,
                texts=texts,
            )

        else:
            raise Exception(f"Wrong bpy_type <{bf_namelist.bpy_type}> in {bf_namelist}")

    # Import unmanaged namelists to free text
    # using the import order
    for fds_namelist in unmanaged_fds_namelists:
        texts.append(fds_namelist.to_string())

    # Empty temporary Scene collections and states
    # they get created in SN_MOVE, SN_MULT, SN_REAC
    if "bf_move_coll" in sc:
        del sc["bf_move_coll"]
    if "bf_mult_coll" in sc:
        del sc["bf_mult_coll"]


def _treat_REACs(context, sc, fds_list, texts):
    """Special treatment for REAC namelists."""
    fds_namelists = fds_list.get_fds_namelists(fds_label="REAC", remove=True)

    # Only one REAC to Scene SN_REAC
    if len(fds_namelists) == 1:
        bf_namelist = BFNamelist.get_subclass(fds_label="REAC")
        _import_to_sc(context, sc, bf_namelist, fds_namelists, texts)

    # Many REACs to free text
    elif len(fds_namelists) > 1:
        texts.append(f"REAC: {len(fds_namelists)} reactions")
        texts.extend(n.to_string() for n in fds_namelists)


def _import_to_sc(context, sc, bf_namelist, fds_namelists, texts):
    """Import Scene related fds_namelists."""
    for fds_namelist in fds_namelists:
        try:
            bf_namelist(element=sc).from_fds(context=context, fds_namelist=fds_namelist)
        except BFNotImported as err:
            texts.append(fds_namelist.to_string())
        except Exception as err:
            texts.append(f"ERROR: {err}")
            texts.append(fds_namelist.to_string())


def _import_to_ob(
    context, sc, bf_namelist, fds_namelists, hid, texts, set_tmp, filename=None
):
    """Import Object related fds_namelists."""
    for fds_namelist in fds_namelists:

        # Get the right Collection
        if set_tmp:  # tmp geometry
            co = sc.collection
        else:
            if filename:
                co_name = f"New {bf_namelist.collection} from <{filename}>"
            else:
                co_name = f"New {bf_namelist.collection}"
            co = bpy.data.collections.get(co_name[:56])  # max len
            if co and not sc.user_of_id(co):  # not in current scene?
                co.name = f"{co_name}.001"  # rename existing
                co = None  # not the right one
            if not co:
                co = bpy.data.collections.new(name=co_name)
                sc.collection.children.link(co)

        # Create the new Object
        ob = utils.geometry.get_new_object(context=context, name=hid, co=co)

        # Set it tmp
        if set_tmp:  # tmp geometry
            utils.geometry.set_is_tmp(context=context, ob=ob)

        # Fill it
        ob.bf_namelist_cls = bf_namelist.__name__
        try:
            bf_namelist(element=ob).from_fds(context=context, fds_namelist=fds_namelist)
        except BFNotImported as err:
            texts.append(fds_namelist.to_string())
        except Exception as err:
            texts.append(f"ERROR: {err}")
            texts.append(fds_namelist.to_string())


def _import_to_ma(context, sc, bf_namelist, fds_namelists, hid, texts):
    """Import Material related namelist."""
    for fds_namelist in fds_namelists:
        ma = bpy.data.materials.new(hid)  # new Material
        ma.bf_namelist_cls = bf_namelist.__name__
        ma.use_fake_user = True  # eg. used by CTRL
        try:
            bf_namelist(element=ma).from_fds(context=context, fds_namelist=fds_namelist)
        except BFNotImported as err:
            texts.append(fds_namelist.to_string())
        except Exception as err:
            texts.append(f"ERROR: {err}")
            texts.append(fds_namelist.to_string())
