# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, Geographic information system: original code from https://github.com/Turbo87/utm
- simplified version that only handle utm zones (and not latitude bands from MGRS grid)
- reverse coord order : latlon --> lonlat
- add support for UTM EPSG codes

More infos : http://geokov.com/education/utm.aspx

Formulas : https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
"""

# The crs is WGS84
# zn: UTM zone number
# ne: UTM north emisphere True/False
# easting: easting
# northing: northing
# elevation: elevation
# lon: longitude in decimal degrees
# lat: latitude in decimal degrees
# epsg: string "EPSG:1234"

import math


K0 = 0.9996

E = 0.00669438
E2 = E * E
E3 = E2 * E
E_P2 = E / (1.0 - E)

SQRT_E = math.sqrt(1 - E)
_E = (1 - SQRT_E) / (1 + SQRT_E)
_E2 = _E * _E
_E3 = _E2 * _E
_E4 = _E3 * _E
_E5 = _E4 * _E

M1 = 1 - E / 4 - 3 * E2 / 64 - 5 * E3 / 256
M2 = 3 * E / 8 + 3 * E2 / 32 + 45 * E3 / 1024
M3 = 15 * E2 / 256 + 45 * E3 / 1024
M4 = 35 * E3 / 3072

P2 = 3.0 / 2 * _E - 27.0 / 32 * _E3 + 269.0 / 512 * _E5
P3 = 21.0 / 16 * _E2 - 55.0 / 32 * _E4
P4 = 151.0 / 96 * _E3 - 417.0 / 128 * _E5
P5 = 1097.0 / 512 * _E4

R = 6378137

# Helper functions: WGS84 UTM <> LonLat


def lonlat_to_zn(lon, lat):
    """!
    Conversion from longitude/latitude to UTM zone number.
    @param lon: longitude in decimal degrees.
    @param lat: latitude in decimal degrees.
    @return the UTM zone number.
    """
    if 56 <= lat < 64 and 3 <= lon < 12:
        return 32
    if 72 <= lat <= 84 and lon >= 0:
        if lon < 9:
            return 31
        elif lon < 21:
            return 33
        elif lon < 33:
            return 35
        elif lon < 42:
            return 37
    return int((lon + 180) / 6) + 1


def lat_to_ne(lat):
    """!
    Detect if latitude is on the UTM north hemisphere.
    @param lat: latitude in decimal degrees.
    @return True if UTM north hemisphere. False otherwise.
    """
    if lat >= -1e-6:
        return True
    else:
        return False


def zn_to_central_lon(zn):
    """!
    UTM zone number to central longitude.
    @param zn: UTM zone number.
    @return central longitude.
    """
    return (zn - 1) * 6 - 180 + 3


def lonlat_to_utm(lon, lat, force_zn=None, force_ne=None):
    """!
    Conversion from longitude/latitude to UTM.
    @param lon: longitude in decimal degrees.
    @param lat: latitude in decimal degrees.
    @param force_zn: UTM zone number (if None, its value will be derived from the entered longitude and latitude).
    @param force_ne: UTM north emisphere (if None, its value will be derived from the entered latitude).
    @return UTM as zn (UTM zone number), ne (UTM north emisphere True/False), easting and northing
    """
    # Check range
    if not -80.0 <= lat <= 84.0:
        raise ValueError(f"Latitude {lat} out of UTM range")
    if not -180.0 <= lon <= 180.0:
        raise ValueError(f"Longitude {lon} out of UTM range")
    # Force zone and emisphere
    if force_zn is not None:
        zn = force_zn
    else:
        zn = lonlat_to_zn(lon=lon, lat=lat)
    if force_ne is not None:
        ne = force_ne
    else:
        ne = lat_to_ne(lat=lat)
    # Compute
    lat_rad = math.radians(lat)
    lat_sin = math.sin(lat_rad)
    lat_cos = math.cos(lat_rad)
    lat_tan = lat_sin / lat_cos
    lat_tan2 = lat_tan * lat_tan
    lat_tan4 = lat_tan2 * lat_tan2
    lon_rad = math.radians(lon)
    central_lon = zn_to_central_lon(zn=zn)
    central_lon_rad = math.radians(central_lon)
    n = R / math.sqrt(1 - E * lat_sin ** 2)
    c = E_P2 * lat_cos ** 2
    a = lat_cos * (lon_rad - central_lon_rad)
    a2 = a * a
    a3 = a2 * a
    a4 = a3 * a
    a5 = a4 * a
    a6 = a5 * a
    m = R * (
        M1 * lat_rad
        - M2 * math.sin(2 * lat_rad)
        + M3 * math.sin(4 * lat_rad)
        - M4 * math.sin(6 * lat_rad)
    )
    easting = (
        K0
        * n
        * (
            a
            + a3 / 6 * (1 - lat_tan2 + c)
            + a5 / 120 * (5 - 18 * lat_tan2 + lat_tan4 + 72 * c - 58 * E_P2)
        )
        + 500000
    )
    northing = K0 * (
        m
        + n
        * lat_tan
        * (
            a2 / 2
            + a4 / 24 * (5 - lat_tan2 + 9 * c + 4 * c ** 2)
            + a6 / 720 * (61 - 58 * lat_tan2 + lat_tan4 + 600 * c - 330 * E_P2)
        )
    )
    if not ne:
        northing += 10000000
    return zn, ne, easting, northing


def utm_to_lonlat(zn, ne, easting, northing):
    """!
    Conversion from UTM to longitude/latitude.
    @param zn: UTM zone number.
    @param ne: UTM north emisphere True/False.
    @param easting: easting.
    @param northing: northing.
    @return the longitude and latitude.
    """
    x = easting - 500000
    y = northing
    if not ne:
        y -= 10000000
    m = y / K0
    mu = m / (R * M1)
    p_rad = (
        mu
        + P2 * math.sin(2 * mu)
        + P3 * math.sin(4 * mu)
        + P4 * math.sin(6 * mu)
        + P5 * math.sin(8 * mu)
    )
    p_sin = math.sin(p_rad)
    p_sin2 = p_sin * p_sin
    p_cos = math.cos(p_rad)
    p_tan = p_sin / p_cos
    p_tan2 = p_tan * p_tan
    p_tan4 = p_tan2 * p_tan2
    ep_sin = 1 - E * p_sin2
    ep_sin_sqrt = math.sqrt(1 - E * p_sin2)
    n = R / ep_sin_sqrt
    r = (1 - E) / ep_sin
    c = _E * p_cos ** 2
    c2 = c * c
    d = x / (n * K0)
    d2 = d * d
    d3 = d2 * d
    d4 = d3 * d
    d5 = d4 * d
    d6 = d5 * d
    lat_rad = (
        p_rad
        - (p_tan / r)
        * (d2 / 2 - d4 / 24 * (5 + 3 * p_tan2 + 10 * c - 4 * c2 - 9 * E_P2))
        + d6 / 720 * (61 + 90 * p_tan2 + 298 * c + 45 * p_tan4 - 252 * E_P2 - 3 * c2)
    )
    lon_rad = (
        d
        - d3 / 6 * (1 + 2 * p_tan2 + c)
        + d5 / 120 * (5 - 2 * c + 28 * p_tan2 - 3 * c2 + 8 * E_P2 + 24 * p_tan4)
    ) / p_cos
    lon = math.degrees(lon_rad) + zn_to_central_lon(zn=zn)
    lat = math.degrees(lat_rad)
    return lon, lat


# Each UTM zone on WGS84 datum has a dedicated EPSG code:
# 326xx for north hemisphere and 327xx for south
# where xx is the zone number from 1 to 60


def zn_ne_to_epsg(zn, ne):
    """!
    Conversion from UTM to EPSG.
    @param zn: UTM zone number.
    @param ne: UTM north emisphere True/False.
    @return the EPSG.
    """
    if ne:
        return "EPSG:326" + str(zn).zfill(2)
    else:
        return "EPSG:327" + str(zn).zfill(2)


def epsg_to_code(epsg):
    """!
    Conversion from EPSG to code.
    @param epsg: EPSG.
    @return the code.
    """
    code = _, code = epsg.split(":")
    return code


def epsg_to_zn_ne(epsg):
    """!
    Conversion from EPSG to UTM.
    @param epsg: EPSG.
    @return UTM as zn (UTM zone number) and ne (UTM north emisphere True/False)
    """
    code = epsg_to_code(epsg)
    zn = int(code[-2:])
    if code[2] == "6":
        ne = True
    else:
        ne = False
    return zn, ne


def lonlat_to_epsg(lon, lat):
    """!
    Conversion from longitude/latitude to EPSG.
    @param lon: longitude in decimal degrees.
    @param lat: latitude in decimal degrees.
    @return the EPSG.
    """
    zn = lonlat_to_zn(lon=lon, lat=lat)
    if lat_to_ne(lat):
        return "EPSG:326" + str(zn).zfill(2)
    else:
        return "EPSG:327" + str(zn).zfill(2)


# Helper functions: WebMerc <> LonLat


class Ellipsoid:
    """!
    Ellipsoid coordinate system.
    """

    def __init__(self, a, b):
        """!
        Constructor of the class.
        @param a: equatorial radius in meters.
        @param b: polar radius in meters.
        """
        self.a = a  # equatorial radius in meters
        self.b = b  # polar radius in meters
        self.f = (self.a - self.b) / self.a  # inverse flat
        self.perimeter = 2 * math.pi * self.a  # perimeter at equator


GRS80 = Ellipsoid(6378137, 6356752.314245)


def webMercToLonLat(x, y):
    """!
    Conversion from UTM to Geographic coordinate system.
    @param x: UTM x coordinate.
    @param y: UTM y coordinate.
    @return the longitude/latitude.
    """
    k = GRS80.perimeter / 360
    lon = x / k
    lat = y / k
    lat = (
        180 / math.pi * (2 * math.atan(math.exp(lat * math.pi / 180.0)) - math.pi / 2.0)
    )
    return lon, lat


def lonLatToWebMerc(lon, lat):
    """!
    Conversion from Geographic coordinate system to UTM coordinate system.
    @param lon: longitude in decimal degrees.
    @param lat: latitude in decimal degrees.
    @return the x/y UTM coordinates.
    """
    k = GRS80.perimeter / 360
    x = lon * k
    lat = math.log(math.tan((90 + lat) * math.pi / 360.0)) / (math.pi / 180.0)
    y = lat * k
    return x, y


# Classes

# WGS84
class UTM:
    """!
    UTM coordinate system.
    """

    def __init__(self, zn=1, ne=True, easting=500000, northing=5000000, elevation=0.0):
        """!
        Constructor of the class.
        @param zn: UTM zone number.
        @param ne: UTM north emisphere True/False.
        @param easting: easting in meters.
        @param northing: northing in meters.
        @param elevation: elevation in meters.
        """
        # Check range
        if not 1 <= zn <= 60:
            raise ValueError(f"Zone number {zn} out of range")
        if not 100000 <= easting < 900000:
            raise ValueError(f"Easting {easting} out of range")
        if ne:
            if not -1e-6 <= northing <= 9300000:
                raise ValueError(
                    f"Northing {northing} out of range in northern emisphere"
                )
        else:
            if not 1100000 <= northing <= 10000000:
                raise ValueError(
                    f"Northing {northing} out of range in southern emisphere"
                )
        # Assign
        self.zn, self.ne, self.easting, self.northing, self.elevation = (
            zn,
            ne,
            easting,
            northing,
            elevation,
        )

    def __str__(self):
        """!
        String representation of the class.
        @return the string.
        """
        return f"{self.zn}{self.ne and 'N' or 'S'}  {self.easting:.1f}m E  {self.northing:.1f}m N (WGS84) h={self.elevation}m"

    def __repr__(self):
        """!
        Representation of the class.
        @return UTM string
        """
        return f"UTM({self.zn}, {self.ne}, {self.easting}, {self.northing}, {self.elevation})"

    @property
    def epsg(self):
        """!
        The EPSG conversion.
        @return the EPSG.
        """
        return zn_ne_to_epsg(zn=self.zn, ne=self.ne)

    @epsg.setter
    def epsg(self, epsg):
        """!
        Update the UTM coordinate using the EPSG code.
        @param epsg: EPSG code.
        """
        self.zn, self.ne = epsg_to_zn_ne(epsg=epsg)

    def to_LonLat(self):
        """!
        Retrive the Geographic coordinate.
        @return the LonLat equivalent object.
        """
        return LonLat(
            *utm_to_lonlat(
                zn=self.zn, ne=self.ne, easting=self.easting, northing=self.northing
            )
        )

    def to_url(self):
        """!
        Retrive the openstreetmap url.
        @return the openstreetmap url.
        """
        return self.to_LonLat().to_url()


# WGS84
class LonLat:
    """!
    Geographic coordinate system.
    """

    def __init__(self, lon=0.0, lat=0.0, elevation=0.0):
        """!
        Constructor of the class.
        @param lon: longitude in decimal degrees.
        @param lat: latitude in decimal degrees.
        @param elevation: elevation in meters.
        """
        if not -180.0 <= lon <= 180.0:
            raise ValueError(f"Longitude <{lon}> out of range")
        if not -90.0 <= lat <= 90.0:
            raise ValueError(f"Latitude <{lat}> out of range")
        self.lon, self.lat, self.elevation = lon, lat, elevation

    def __str__(self):
        """!
        String representation of the class.
        @return the string.
        """
        return f"{self.lon:.6f}° {self.lon<0. and 'W' or 'E'}  {self.lat:.6f}° {self.lat<0. and 'S' or 'N'} (WGS84) h={self.elevation}m"

    def __repr__(self):
        """!
        Representation of the class.
        @return LonLat string
        """
        return f"LonLat({self.lon}, {self.lat}, {self.elevation})"

    def to_UTM(self, force_zn=None, force_ne=None):
        """!
        Retrive the UTM coordinate.
        @return the UTM equivalent object.
        """
        return UTM(
            *lonlat_to_utm(
                lon=self.lon, lat=self.lat, force_zn=force_zn, force_ne=force_ne
            ),
            self.elevation,
        )

    def to_url(self):
        """!
        Retrive the openstreetmap url.
        @return the openstreetmap url.
        """
        return f"http://www.openstreetmap.org/?mlat={self.lat}&mlon={self.lon}&zoom=12"
