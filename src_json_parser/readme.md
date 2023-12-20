# The `smbjson` parser module
This module allows to read the FDTD JSON format and parse it into the semba-fdtd data structures.

# Compilation and testing
## Compilation with GNU Fortran
Assuming `gfortran` and `cmake` are accessible from path, this module can be compiled from the project main directory run

    cmake -S . -B <BUILD_DIR> -DCompileWithJSON=YES
    cmake --build <BUILD_DIR> -j

## Compilation with Intel Fortran Classic
Tested to work.

## Compilation with NVIDIA Fortran compiler
Tested to work with `-O0` optimizations. Higher optimizations produce SEGFAULTs.

## Testing
This project uses the ctest tool available within cmake. Once build, you can run

    ctest --test-dir <BUILD_DIR>/src_json_parser/test/ --output-on-failure


# The FDTD JSON format.
This format aims to provide a way to input data for a FDTD simulation. Being in JSON, it can be easily navigated with most text editors, such as Visual Studio Code or Notepad++. There are also multiple tools to read and write them.

The following are examples of different cases:
 1. An empty space illuminated by a plane wave: [planewave.fdtd.json](testData/cases/planewave.fdtd.json). The field at a point close to the center is recorded.
 2. A thin straight wire illuminated by a plane wave: [holland1981.fdtd.json](testData/cases/holland1981.fdtd.json) which aims to replicate the case described in https://doi.org/10.1109/TEMC.1981.303899. It contains a probe which records the wire at the middle of the wire.
 3. A current injection which mimics a lightning strike on a square metallic surface: [currentinjection.fdtd.json](testData/cases/currentInjection.fdtd.json). It contains two bulk current probes to measure the current at the entry and exit lines.
 4. A shielded pair of wires feeded by a voltage source in one of its ends: [shieldedPair.fdtd.json](testData/cases/shieldedPair.fdtd.json). The interior of the shield uses a multiconductor transmision line (MTLN) algortihm to evolve the common mode currents which are induced in the shield and propagated inside using a transfer impedance. 
 5. 
 
## Entries description
### `general`
This entry must contain the following items
### `boundary`
### `mesh`
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