#!/usr/bin/env python3

"""!
Split MESH parameters according to nsplits.
"""


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
    Split ijk cells along the axis in nsplits, calc new ids, ijks and xbs
    """
    # Init
    ijks, xbs = list(), list()
    ncell = ijk[0] * ijk[1] * ijk[2]
    if not export:
        nsplits = 1, 1, 1
    # Split cells along axis
    icells = split_cells(ijk[0], nsplits[0])
    jcells = split_cells(ijk[1], nsplits[1])
    kcells = split_cells(ijk[2], nsplits[2])
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
    # Prepare ids
    if len(xbs) > 1:
        hids = (f"{hid}_s{i}" for i in range(len(xbs)))
    else:
        hids = (hid,)
    return hids, ijks, xbs, ncell, cs


def test():
    hids, ijks, xbs, ncell, cs = split_mesh(
        hid="H",
        ijk=(10, 10, 10),
        nsplits=(2, 3, 1),
        xb=(0.0, 10.0, 0.0, 10.0, 0.0, 10.0),
    )
    assert tuple(hids) == (
        "H_0",
        "H_1",
        "H_2",
        "H_3",
        "H_4",
        "H_5",
    )
    assert ijks == [
        (5, 4, 10),
        (5, 3, 10),
        (5, 3, 10),
        (5, 4, 10),
        (5, 3, 10),
        (5, 3, 10),
    ]
    assert xbs == [
        (0.0, 5.0, 0.0, 4.0, 0.0, 10.0),
        (0.0, 5.0, 4.0, 7.0, 0.0, 10.0),
        (0.0, 5.0, 7.0, 10.0, 0.0, 10.0),
        (5.0, 10.0, 0.0, 4.0, 0.0, 10.0),
        (5.0, 10.0, 4.0, 7.0, 0.0, 10.0),
        (5.0, 10.0, 7.0, 10.0, 0.0, 10.0),
    ]


if __name__ == "__main__":
    test()
