# SPDX-License-Identifier: GPL-3.0-or-later

import time, logging, bpy

from ...config import MAXLEN
from ...types import FDSList, FDSParam
from ... import utils, bl_info

log = logging.getLogger(__name__)


def _get_header(context):
    bfv = bl_info["version"]
    blv = bpy.app.version_string
    now = time.strftime("%a, %d %b %Y, %H:%M:%S", time.localtime())
    bl_filepath = utils.io.shorten(
        bpy.data.filepath or "not saved", max_len=MAXLEN - 8, start_part=0
    )
    msgs = (  # Always shown
        f"! Generated by BlenderFDS {bfv[0]}.{bfv[1]}.{bfv[2]} on Blender {blv}",
        f"! Date: {now}",
        f"! File: {bl_filepath}",
        f"\n--- Case from Blender Scene: <{context.scene.name}> | View Layer: <{context.view_layer.name}>\n",
    )
    return FDSList(msgs=msgs)


def _get_scene(context):
    iterable = (
        bf_namelist.to_fds_list(context)
        for bf_namelist in context.scene.bf_namelists
        if bf_namelist
    )
    return FDSList(iterable=iterable)


def _get_free_text(context):
    sc = context.scene
    if not sc.bf_config_text:
        return FDSList()
    header = f"\n--- Free text from Blender Text: <{sc.bf_config_text.name}>\n"
    msg = sc.bf_config_text.as_string().strip()
    return FDSList(header=header, msg=msg)


def _get_domain(context):
    sc = context.scene

    # Get all exported MESHes and sort them by name
    obs = utils.geometry.get_exported_obs(context, obs=context.scene.objects)
    mesh_obs = list((ob for ob in obs if ob.bf_namelist_cls == "ON_MESH"))
    mesh_obs.sort(key=lambda k: k.name)

    # Linearize MESH fds_list
    mesh_fds_list = FDSList()
    for ob in mesh_obs:
        mesh_fds_list.extend(ob.to_fds_list(context).get_flat_ns())

    # Short track
    if not sc.bf_config_mpi_processes_export:
        mesh_fds_list.header = f"\n--- Computational domain"
        return mesh_fds_list

    # Init item_weights: ((w0, item0), (w1, item1), ...)
    item_weigths = list()
    for nl in mesh_fds_list:
        ijk = nl.get_fds_param(fds_label="IJK")
        ncell = ijk[0] * ijk[1] * ijk[2]
        item_weigths.append((ncell, nl))  # weigth, item

    # Binpack
    nbin = sc.bf_config_mpi_processes
    bins = utils.binpacking.binpack(nbin=nbin, item_weigths=item_weigths)

    # Prepare output
    ncell_tot = sum(w for w, _ in bins)
    nmesh_tot = len(mesh_fds_list)
    header = f"\n--- Computational domain | MPI Processes: {nbin} | MESH Qty: {nmesh_tot} | Cell Qty: {ncell_tot}"
    domain_fds_list = FDSList(header=header)
    for mpi_process, bin in enumerate(bins):

        # Per MPI Process
        ncell, mesh_fds_namelists = bin
        nmesh_tot = len(mesh_fds_namelists)
        header = f"\n-- MPI Process: <{mpi_process}> | MESH Qty: {nmesh_tot} | Cell Qty: {ncell}"
        bin_fds_list = FDSList(header=header)
        domain_fds_list.append(bin_fds_list)
        for mesh_fds_namelist in mesh_fds_namelists:

            # Per MESH
            mesh_fds_namelist.append(
                FDSParam(
                    fds_label="MPI_PROCESS",
                    value=mpi_process,
                )
            )
            bin_fds_list.append(mesh_fds_namelist)

    return domain_fds_list


def _get_collections(context):
    header = "\n--- Geometric namelists from Blender Collections"
    iterable = context.scene.collection.to_fds_list(context, full=True)
    return FDSList(header=header, iterable=iterable)


def _fill_mas(fds_namelist, mas, fds_label):
    """!
    Fill mas with Materials referenced by fds_namelist.
    """
    for fds_param in fds_namelist.get_fds_params(fds_label=fds_label):
        for hid in fds_param:
            ma = bpy.data.materials.get(hid)
            if ma:
                mas.append(ma)
        return True


def _get_ref_mas(context, collections_fds_list):
    """!
    Get list of referenced Blender Materials.
    """
    # Prepare list of namelists from free_text and collections
    fds_list = FDSList()
    sc = context.scene
    if sc.bf_config_text:
        fds_list.from_fds(f90_namelists=sc.bf_config_text.as_string())
    fds_list.append(collections_fds_list)

    # Get references to mas
    mas = list()
    for fds_namelist in fds_list.get_fds_namelists():
        fds_label = fds_namelist.fds_label
        if fds_label == "OBST":
            if _fill_mas(fds_namelist=fds_namelist, mas=mas, fds_label="SURF_ID"):
                continue
            if _fill_mas(fds_namelist=fds_namelist, mas=mas, fds_label="SURF_IDS"):
                continue
            _fill_mas(fds_namelist=fds_namelist, mas=mas, fds_label="SURF_ID6")
        elif fds_label in ("GEOM", "VENT", "DEVC", "CTRL", "PART"):
            _fill_mas(fds_namelist=fds_namelist, mas=mas, fds_label="SURF_ID")

    # Add default SURF from Scene
    if sc.bf_default_surf:
        mas.append(sc.bf_default_surf)

    # Remove doubles, set alphabetic sorting by name
    mas = list(set(mas))
    mas.sort(key=lambda k: k.name)
    return mas


def _get_materials(context, collections_fds_list):
    header = "\n--- Boundary conditions from Blender Materials\n"
    mas = _get_ref_mas(context, collections_fds_list)
    iterable = (ma.to_fds_list(context=context) for ma in mas)
    return FDSList(header=header, iterable=iterable)


def sc_to_fds_list(context, sc, full=False) -> FDSList:
    """!
    Return the FDSList instance from sc, never None.
    """
    # Init components
    scene_fds_list = _get_scene(context)

    if not full:
        return scene_fds_list

    header_fds_list = _get_header(context)
    free_text_fds_list = _get_free_text(context)
    domain_fds_list = _get_domain(context)
    collections_fds_list = _get_collections(context)
    materials_fds_list = _get_materials(
        context,
        collections_fds_list=collections_fds_list,
    )

    # Join components
    fds_list = FDSList()
    fds_list.append(header_fds_list)
    fds_list.append(scene_fds_list)
    if sc.bf_config_text_position == "BEGIN":
        fds_list.append(free_text_fds_list)
    fds_list.append(materials_fds_list)
    fds_list.append(domain_fds_list)
    fds_list.append(collections_fds_list)
    if sc.bf_config_text_position == "END":
        fds_list.append(free_text_fds_list)

    # Close with TAIL
    if sc.bf_head_export:
        fds_list.append(FDSList(msg="\n&TAIL /\n"))

    return fds_list
