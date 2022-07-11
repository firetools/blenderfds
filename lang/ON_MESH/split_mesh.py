# SPDX-License-Identifier: GPL-3.0-or-later

"""!
Split MESH parameters according to nsplits.
"""


from ...types import BFException

# FIXME split when there is a coarser mesh?


def split_cells(ncell, nsplit):
    """!
    Split ncell cells in nsplit parts, conserving the total number ncell of cells
    """
    if ncell < nsplit * 3:
        return list((ncell,))
    base = ncell // nsplit
    remainder = ncell % nsplit
    ncells = list((base,)) * nsplit
    for i in range(0, remainder):
        ncells[i] += 1
    return ncells


def split_mesh(hid, ijk, export, nsplits, xb):
    """!
    Split ijk cells along the axis in nsplits, calc new hids, ijks and xbs
    """
    # Init
    ijks, xbs = list(), list()
    if not export:
        nsplits = 1, 1, 1
    # Split cells along axis
    icells = split_cells(ijk[0], nsplits[0])
    jcells = split_cells(ijk[1], nsplits[1])
    kcells = split_cells(ijk[2], nsplits[2])
    ncell = icells[0] * jcells[0] * kcells[0]
    expected_nsplits = nsplits[0] * nsplits[1] * nsplits[2]
    # Prepare new mesh ijks and origins (in cell number)
    corigins = list()
    corigin_i, corigin_j, corigin_k = 0, 0, 0
    for i in icells:
        for j in jcells:
            for k in kcells:
                ijks.append((i, j, k))
                corigins.append((corigin_i, corigin_j, corigin_k))
                corigin_k += k
            corigin_k = 0
            corigin_j += j
        corigin_j = 0
        corigin_i += i
    # Calc cell sizes along axis
    cs = (
        (xb[1] - xb[0]) / ijk[0],
        (xb[3] - xb[2]) / ijk[1],
        (xb[5] - xb[4]) / ijk[2],
    )
    # Prepare xbs
    # corigin (0,0,0) is at coordinates xb[0], xb[2], xb [4]
    for i, corigin in enumerate(corigins):
        xbs.append(
            (
                xb[0] + corigin[0] * cs[0],
                xb[0] + (corigin[0] + ijks[i][0]) * cs[0],
                xb[2] + corigin[1] * cs[1],
                xb[2] + (corigin[1] + ijks[i][1]) * cs[1],
                xb[4] + corigin[2] * cs[2],
                xb[4] + (corigin[2] + ijks[i][2]) * cs[2],
            )
        )
    # Prepare hids
    nsplit = len(xbs)
    if nsplit > 1:
        hids = tuple(f"{hid}_s{i}" for i in range(len(xbs)))
    else:
        hids = tuple((hid,))
    if nsplit != expected_nsplits:
        raise BFException(sender=None, msg="Too much splitting, not enough cells.")
    return hids, ijks, xbs, ncell, cs, nsplit
