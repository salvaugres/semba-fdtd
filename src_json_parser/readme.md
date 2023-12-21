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


# The FDTD-JSON format
This format aims to provide a way to input data for a full FDTD simulation. Being in JSON, it can be easily navigated with most text editors, such as Visual Studio Code or Notepad++. There are also multiple tools to read and write them.

The following are examples of different cases:
 1. An empty space illuminated by a plane wave: [planewave.fdtd.json](testData/cases/planewave.fdtd.json). The field at a point close to the center is recorded.
 2. A thin straight wire illuminated by a plane wave: [holland1981.fdtd.json](testData/cases/holland1981.fdtd.json) which aims to replicate the case described in https://doi.org/10.1109/TEMC.1981.303899. It contains a probe which records the wire at the middle of the wire.
 3. A current injection which mimics a lightning strike on a square metallic surface: [currentinjection.fdtd.json](testData/cases/currentInjection.fdtd.json). It contains two bulk current probes to measure the current at the entry and exit lines.
 4. A shielded pair of wires feeded by a voltage source in one of its ends: [shieldedPair.fdtd.json](testData/cases/shieldedPair.fdtd.json). The interior of the shield uses a multiconductor transmision line (MTL) algortihm to evolve the common mode currents which are induced in the shield and propagated inside using a transfer impedance. 
 5. A multiconductor transmission line network (MTLN) case which includes three cable bundles with a bifurcation: [mtln.fdtd.json](testData/cases/mtln.fdtd.json). 
 
## FDTD-JSON objects description
All units are assumed to be SI-MKS. 

Angle brackets `<entry>` indicate that that entry is mandatory.
Square brackets `[entry]` are optional entries.

### `<general>`
This object must always be present and contains general information regarding the solver. It must contain the following entries:

- `<timeStep>`: A real number indicating the time step used by the solver, in seconds. 
- `<numberOfSteps>`: An integer for the number of steps which the solver will iterate.

Example:

    "general": {
        "timeStep": 10e-12,
        "numberOfSteps": 2000
    }
    
### `[boundary]`
This specifies the boundaries which will be used to terminate the computational domain. 
If `boundary` is not present it defaults to a `mur` absorbing condition in all bounds.
The entries within `boundary` are objects labelled with the place where they will be applied: 
 - `all`, or
 - `xLower`, `xUpper`, `yLower`, `yUpper`, `zLower` `zUpper`. 

These objects must contain a `<type>` label which can be:
 * `pec` for perfectly electric conducting termination.
 * `pmc` for perfectly magnetic conducting termination.
 * `mur` for Mur's first order absorbing boundary condition.
 * `pml` for perfectly matched layer termination. If this `type` is selected, it must also contain:
    - `<layers>`: with an integer indicating the number of pml layers which will be used. TODO REVIEW
    - `<order>`: TODO REVIEW
    - `<reflection>`: TODO REVIEW

Example: 

    "boundary": {
        "all": {
            "type": "pml",
            "layers": 6, 
            "order": 2.0,
            "reflection": 0.001
        }
    }

### `<mesh>`
All the geometrical information of the case is exclusively stored by the `mesh` object. 

#### `<grid>`
The `grid` object represents a collection of rectangular cuboids or *cells* which tessellate the space to form a structured mesh. This object is defined with the following entries:
- `<numberOfCells>` is an array of three positive integers which indicate the number of cells in each Cartesian direction.
- `<steps>` is an object which contains three arrays, labeled with `<x>`, `<y>` and `<z>` which represent the cell sizes in that direction. Each array may contain a single real to define a [regular grid](https://en.wikipedia.org/wiki/Regular_grid); or, alternatively, a number of reals equal to the number of cells to define a [rectilinear grid](https://en.wikipedia.org/wiki/Regular_grid).

#### `[coordinates]`
This is an array of objects which represent Cartesian coordinates within the grid. Each object of the array must contain the following entries:
 * `<id>`: an integer number that must be unique within this array.
 * `<relativePosition>`: Containing an array of 3 numbers which can be integer or reals. The whole part of the number indicates the cell and the fractional part indicates the fractional position within that cell.

 TODO EXAMPLE IMAGE

#### `[elements]`
The `elements` entry is an array of objects, each of which represents a geometrical entity. Within this format context the concept of element is broader than usually because they can be relatively complex geometrical entities. An *element objects* must contain a 
 * `<id>` formed by an integer which uniquely identifies it within the `elements` array.
 * `<type>` which can be one of the following: 
   - `node`, representing a point in space. Elements with this type include a `<coordinateIds>` entry which is an array of a single integer with the id of a coordinate and which must exist in the within the `mesh` `coordinates` array.
   - `polyline`, representing an oriented collection of segments. It must contain a list `<coordinateIds>` with at least two coordinates.
   - `junction`, representing a topological connection between different entities. TODO REVIEW ?AND REMOVE?.
   - `cellRegion`, containing a list of  one or more `<intervals>` defined following the [interval convention](#####interval-convention). 

##### `interval` convention
An `interval` is formed by two triplets $\mathbf{a} = \{a_x, a_y, a_z\}$ and $\mathbf{b} = \{b_x, b_y, b_z\}$ of integer numbers which define three closed-open intervals $[a_x, b_x)$, $[a_y, b_y)$, and $[a_z, b_z)$. 

 hich allow to specify points within the grid (pixels), oriented lines (linels), oriented surfaces 
If one or more directions increase, all of them must increase.
If one direction increases and other decreases, is undefined behavior.

TODO IMAGE

## `[materials]`
This entry is an array formed by all the physical models contained in the simulation. Each object within the array must contain:
- `<id>`: An integer number that uniquely identifies the material.

// TODO

### simple
simple type is an isotropic material with specified relative permittivity, relative permeability, electric conductivity and/or magnetic conductivity.

## `[materialRegions]`
This entry stores associations between `materials` and `elements`. 

// TODO


## `[probes]`

Require type and at least one elementIds or cellRegions entry.
Require a domain.
### `<type>`
If type is bulkCurrent:
    \[field\]: electric, magnetic (DEFAULTS TO FIRST)
### `[domain]`

# `[sources]`
All sources require type and at least one elementIds or cellRegions entry.

## `<type>`
If type is Planewave

If type is nodalSource
    <field>: electric, magnetic, current
    \[isInitialField\]: false, true (DEFAULTS TO FIRST)


# `[cables]`