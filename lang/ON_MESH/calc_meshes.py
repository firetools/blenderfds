# SPDX-License-Identifier: GPL-3.0-or-later

"""!
Calc all MESH parameters.
"""

from math import ceil
from ... import utils
from .split_mesh import split_mesh
from ..ON_MULT import multiply_xbs


def get_factor(n):
    """!Generator for prime factors of n (from http://dhananjaynene.com/)."""
    yield 1
    i = 2
    limit = n ** 0.5
    while i <= limit:
        if n % i == 0:
            yield i
            n = n / i
            limit = n ** 0.5
        else:
            i += 1
    if n > 1:
        yield int(n)


def get_n_for_poisson(n):
    """!Get a good number for poisson solver at least bigger than n."""
    good = set((1, 2, 3, 5))
    while True:
        if [i for i in get_factor(n) if i not in good]:
            n += 1
        else:
            break
    return n


def get_poisson_ijk(ijk):
    """!Get an IJK respecting the Poisson constraint, close to the current one."""
    return ijk[0], get_n_for_poisson(ijk[1]), get_n_for_poisson(ijk[2])


def get_ijk_from_desired_cs(context, ob, desired_cs, poisson):  # Used by: mesh_tools.py
    """!Calc MESH IJK from cell sizes."""
    xb = utils.geometry.get_bbox_xb(context=context, ob=ob, world=True)
    ijk = (
        round((xb[1] - xb[0]) / desired_cs[0]) or 1,
        round((xb[3] - xb[2]) / desired_cs[1]) or 1,
        round((xb[5] - xb[4]) / desired_cs[2]) or 1,
    )
    if poisson:
        return get_poisson_ijk(ijk)
    else:
        return ijk


def get_cell_sizes(context, ob):  # used by: mesh_tools.py
    """!Get cell sizes."""
    xb = utils.geometry.get_bbox_xb(context=context, ob=ob, world=True)
    ijk = ob.bf_mesh_ijk
    return (
        (xb[1] - xb[0]) / ijk[0],
        (xb[3] - xb[2]) / ijk[1],
        (xb[5] - xb[4]) / ijk[2],
    )


def get_mesh_geometry(context, ob):
    """!Get geometry and info on generated MESH instances."""
    # Split
    hids, ijks, xbs, ncell, cs, nsplit = split_mesh(
        hid=ob.name,
        ijk=ob.bf_mesh_ijk,
        export=ob.bf_mesh_nsplits_export,
        nsplits=ob.bf_mesh_nsplits,
        xb=utils.geometry.get_bbox_xb(context=context, ob=ob, world=True),
    )

    # Multiply
    hids, xbs, _, nmult = multiply_xbs(
        context=context, ob=ob, hids=hids, xbs=xbs, msgs=list()
    )
    ijks *= nmult

    # Prepare header
    ijk = ob.bf_mesh_ijk
    ncell_tot = ijk[0] * ijk[1] * ijk[2] * nmult
    has_good_ijk = tuple(ijk) == get_poisson_ijk(ijk) and "Yes" or "No"
    aspect = get_cell_aspect(cs)
    nmesh = nmult * nsplit
    return (
        hids,
        ijks,
        xbs,
        nmesh,
        nsplit,
        nmult,
        ncell_tot,
        ncell,
        cs,
        aspect,
        has_good_ijk,
    )


def get_cell_aspect(cell_sizes):
    """!Get max cell aspect ratio."""
    scs = sorted(cell_sizes)
    try:
        return max(
            scs[2] / scs[0],
            scs[2] / scs[1],
            scs[1] / scs[0],
        )
    except ZeroDivisionError:
        return 999.0
