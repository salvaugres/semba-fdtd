The FDTD json format.

# general

# boundary

# mesh

# materials

# materialRegions

# probes

Require type and at least one elementIds or cellRegions entry.
Require a domain.

## domain

## type
If type is bulkCurrent:
    \[field\]: electric, magnetic (DEFAULTS TO FIRST)

# sources
All sources require type and at least one elementIds or cellRegions entry.

## type
If type is Planewave

If type is nodalSource
    <field>: electric, magnetic, current
    \[isInitialField\]: false, true (DEFAULTS TO FIRST)


# cables