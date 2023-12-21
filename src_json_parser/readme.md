# The `smbjson` parser module
This module allows to read the [FDTD-JSON format](#the-fdtd-json-format) and parse it into the semba-fdtd data structures.

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
```json
    "general": {
        "timeStep": 10e-12,
        "numberOfSteps": 2000
    }
```
    
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

```json
    "boundary": {
        "all": {
            "type": "pml",
            "layers": 6, 
            "order": 2.0,
            "reflection": 0.001
        }
    }
```

### `<mesh>`
All the geometrical information of the simulation case is exclusively stored by the `mesh` object. 

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
The `elements` entry contains an array of JSON objects, each of which represents a geometrical entity. Within context of this format specification, an *element* can be a relatively simple entity such as `node` or a `polyline`, but it can also be a much more complex geometrical entity such as a `cellRegion`. An *element objects* must contain a 
 * `<id>` formed by an integer which uniquely identifies it within the `elements` array.
 * `<type>` which can be one of the following: 
   - `node`, representing a point in space. Elements with this type include a `<coordinateIds>` entry which is an array of a single integer with the id of a coordinate and which must exist in the within the `mesh` `coordinates` array.
   - `polyline`, representing an oriented collection of segments. It must contain a list `<coordinateIds>` with at least two coordinates.
   - `cellRegion`, containing a list of  one or more `<intervals>` defined following the [interval convention](#####interval-convention). 

##### The `interval` convention
An `interval` is formed by a pair of two arrays, each formed by triplets of integer numbers $\mathbf{a} = \{a_x, a_y, a_z\}$ and $\mathbf{b} = \{b_x, b_y, b_z\}$ which define a region formed by three closed-open intervals $[a_x, b_x) \times [a_y, b_y) \times [a_z, b_z)$. 
Each integer number indicates a Cartesian plane in the `grid` assuming that they are numbered from $0$ to $N$, with $N$ being the number of cells in that direction. 
The size of the interval is defined as $|a_x - b_x| \times |a_y - b_y| \times |a_z - b_z|$ therefore must be positive or zero.
An interval allows specifying regions within the grid which can be a point, an oriented line, an oriented surface, or a volume:

+ A *point* is defined when $\mathbf{a} = \mathbf{b}$. In this case, the interval specifies an intersection three grid planes. Points have no orientation.

+ An *oriented line* is defined when the interval has the same initial and ending values in all directions except one, for instance $a_x \neq b_x$. In this case there are two possibilities:

  - when $(b_x - a_x) > 0$, the line is oriented towards $+\hat{x}$.
  - when $(b_x - a_x) < 0$, the line is oriented towards $-\hat{x}$. 

  In both cases the line would have a size of $|b_x - a_x|$.  

+ An *oriented surface* is defined when one initial and ending value is the same and the other two are different, e.g. $a_x = b_x$, $a_y \neq b_y$, $a_z \neq b_z$. In this case there are four possibilities:
  
  - when the $(b_y - a_y) > 0$ and $(b_z - a_z) > 0$, the surface normal is assumed to be oriented towards $+\hat{x}$.
  - when the $(b_y - a_y) < 0$ and $(b_z - a_z) < 0$, the surface normal is assumed to be oriented towards $-\hat{x}$.
  - The other two cases in which there is a mix of positive and negative signs are undefined.

  In all cases the size of the surface is 

+ A *volume* is defined when each number in $\mathbf{a}$ is strictly smaller than the numbers in $\mathbf{b}$ for each direction, i.e. $a_x < b_x$, $a_y < b_y$, and $a_z < b_z$. The rest of the cases in which all numbers are different but not necessarily smaller are left as undefined.

TODO EXAMPLES IMAGE

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
This entry is an array which stores all the electromagnetic sources of the simulation case. Each source is a JSON object which must contain the following entries:
 + `<magnitudeFile>` contains a relative path to the plain text file which will be used as a magnitude for this source. This file must contain two columns, with the first stating the time and the second one the magnitude value; an example magnitude file can be found at [gauss.exc](testData/cases/gauss.exc).
 + `<type>` must be a label of the ones defined below. Some examples of source `type` are `planewave` or `nodalSource`.
 + `<elementIds>` is an array of integers which must exist within the `mesh` `elements` list. These indicate the geometrical place where this source is located. The `type` and number of the allowed elements depends on the source `type` and can be check in the descriptions of each source object, below.
 
## `planewave`
The `planewave` object represents an electromagnetic plane wave front which propagates towards a $\hat{k}$ direction with an electric field pointing towards $\hat{E}$. The `elementIds` in planewaves must define a single `cellRegion` element formed by a single cuboid region.
Besides the common entries in [sources](#sources), it must also contain the following ones:

 + `<direction>`, is an object containing `<theta>` and `<phi>`, which are the angles of the propagation vector $\hat{k} (\theta, \phi)$.
 + `<polarization>`, is an object containing `<theta>` and `<phi>` which indicates the direction of the electric field vector $\hat{E}(\theta, \phi)$.

An example of a planewave propagating towards $\hat{z}$ and polarized in the $+\hat{x}$ follows,
```json
    {
        "type": "planewave",
        "magnitudeFile": "gauss.exc",
        "elementIds": [2],
        "direction": {
            "theta": 0.0,
            "phi": 0.0
        },
        "polarization": {
            "theta": 1.5708,
            "phi": 0.0
        }
    }
```

## `nodalSource`
TODO
If `type` is nodalSource
    `[field]`: electric, magnetic, current


# `[cables]`