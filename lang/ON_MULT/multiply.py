# SPDX-License-Identifier: GPL-3.0-or-later

from ... import utils
from ..OP_XB.xbs_to_ob import xbs_to_ob


def multiply_xbs(context, ob, hids, xbs, msgs):
    """!
    Return lists of xb and their hid as multiplied by FDS MULT from ob.
    """
    # Calc, fast track
    if not ob.bf_mult_export:
        nmult = 1
        return hids, xbs, msgs, nmult

    # Init the rest
    dxb = ob.bf_mult_dxb
    d = (ob.bf_mult_dx, ob.bf_mult_dy, ob.bf_mult_dz)
    d0 = (ob.bf_mult_dx0, ob.bf_mult_dy0, ob.bf_mult_dz0)
    lower = (
        ob.bf_mult_i_lower,
        ob.bf_mult_j_lower,
        ob.bf_mult_k_lower,
        ob.bf_mult_n_lower,
    )
    lower_skip = (
        ob.bf_mult_i_lower_skip,
        ob.bf_mult_j_lower_skip,
        ob.bf_mult_k_lower_skip,
        ob.bf_mult_n_lower_skip,
    )
    upper = (
        ob.bf_mult_i_upper,
        ob.bf_mult_j_upper,
        ob.bf_mult_k_upper,
        ob.bf_mult_n_upper,
    )
    upper_skip = (
        ob.bf_mult_i_upper_skip,
        ob.bf_mult_j_upper_skip,
        ob.bf_mult_k_upper_skip,
        ob.bf_mult_n_upper_skip,
    )

    # Calc, slow track
    multi_xbs, multi_hids = list(), list()
    for xb, hid in zip(xbs, hids):
        new_xbs, new_hids = multiply_xb(
            xb,
            hid,
            dxb,
            d,  # dx,dy,dz
            d0,  # dx0,dy0,dz0,
            lower,  # i,j,k,n
            lower_skip,  # i,j,k,n
            upper,  # i,j,k,n
            upper_skip,
        )
        multi_xbs.extend(new_xbs)
        multi_hids.extend(new_hids)
    nmult = len(new_xbs)

    # Msgs
    if nmult > 1:
        msgs.append(f"Multiples: {nmult}")

    # Generate multiples
    return multi_hids, multi_xbs, msgs, nmult


def multiply_xb(
    xb,
    hid,
    dxb,
    d,  # dx,dy,dz
    d0,  # dx0,dy0,dz0,
    lower,  # i,j,k,n
    lower_skip,  # i,j,k,n
    upper,  # i,j,k,n
    upper_skip,  # i,j,k,n
):
    """!
    Return lists of xb and their hid as multiplied by FDS MULT.
    """
    xbs, hids = list(), list()
    dx, dy, dz = d
    dx0, dy0, dz0 = d0
    i_lower, j_lower, k_lower, n_lower = lower
    i_lower_skip, j_lower_skip, k_lower_skip, n_lower_skip = lower_skip
    i_upper, j_upper, k_upper, n_upper = upper
    i_upper_skip, j_upper_skip, k_upper_skip, n_upper_skip = upper_skip
    has_skip = False
    if any(dxb):
        # N, DXB: OBST MULT example
        if n_lower_skip >= n_lower or n_upper_skip <= n_upper:
            has_skip = True
        for n in range(n_lower, n_upper + 1):
            if has_skip and n_lower_skip <= n <= n_upper_skip:
                continue
            xbs.append(
                (
                    xb[0] + dx0 + n * dxb[0],
                    xb[1] + dx0 + n * dxb[1],
                    xb[2] + dy0 + n * dxb[2],
                    xb[3] + dy0 + n * dxb[3],
                    xb[4] + dz0 + n * dxb[4],
                    xb[5] + dz0 + n * dxb[5],
                )
            )
            hids.append(f"{hid}_{n}")
    else:
        # I,J,K: MESH refining example
        if (
            i_lower_skip >= i_lower
            or j_lower_skip >= j_lower
            or k_lower_skip >= k_lower
            or i_upper_skip <= i_upper
            or j_upper_skip <= j_upper
            or k_upper_skip <= k_upper
        ):
            has_skip = True
        i_lower_skip = max(i_lower, i_lower_skip)
        for i in range(i_lower, i_upper + 1):
            for j in range(j_lower, j_upper + 1):
                for k in range(k_lower, k_upper + 1):
                    if has_skip and (
                        i_lower_skip <= i <= i_upper_skip
                        and j_lower_skip <= j <= j_upper_skip
                        and k_lower_skip <= k <= k_upper_skip
                    ):
                        continue
                    xbs.append(
                        (
                            xb[0] + dx0 + i * dx,
                            xb[1] + dx0 + i * dx,
                            xb[2] + dy0 + j * dy,
                            xb[3] + dy0 + j * dy,
                            xb[4] + dz0 + k * dz,
                            xb[5] + dz0 + k * dz,
                        )
                    )
                    hids.append(f"{hid}_i{i}_j{j}_k{k}")
    return xbs, hids
