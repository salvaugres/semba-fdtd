# The FDTD json format.

## general
## boundary
## mesh
Coordinates in mesh are considered to be relative to the cells, starting in (0,0,0).
Elements are geometrical entities which reference the coordinates or specify cells in the mesh.

If one or more directions increase, all of them must increase.
If one direction increases and other decreases, is undefined behavior.

## materials
Allowed types: pec, pmc, simple

### simple
simple type is an isotropic material with specified relative permittivity, relative permeability, electric conductivity and/or magnetic conductivity.

## materialRegions


## probes

Require type and at least one elementIds or cellRegions entry.
Require a domain.
### domain
### type
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