# semba-fdtd

In a nutshell, semba-fdtd capabilities are

- Cluster working capabilites through MPI.
- Multiple threads per processor through OpenMP.
- Closed/symmetric problems by means of PEC and PMC conditions.
- Open problems by means of PML boundary conditions (CPML formulation) or by Mur ABCs.
- Non-uniformly meshed domains by means of mesh-grading techniques.
- Bulk lossless and lossy dielectrics.
- Materials with frequency dependent relative permittivity and/or permeability, with an arbitrary number of complex-conjugate pole-residue pairs.
- Bulk anisotropic lossless and lossy dielectrics.
- Equivalent models of multilayered skin-depth materials.
- Branched multiwires: 
– Junctions of wires of different radii.
	1. Junctions of multiwires.
	2. Wire bundles.
	3. Loaded with p.u.l resistance and inductance wires.
	4. Grounding through lumped elements.
	- Plane-wave illumination with arbitrary time variation.
- Multiple planewaves illumination for reverberation chamber modeling.
- Hertzian dipole sources.
	- Equivalent Huygens surfaces.
	- Low frequency thin composites and lossy surfaces.
	- Thin slots.
	- Time, frequency and transfer function probes .
	- Near-to-far field transformation.

Most of these capabilities are explained in further detail in the references below

- Miguel Ruiz Cabello, Maksims Abalenkovs, Luis Diaz Angulo, Clemente Cobos Sanchez, Franco Moglie, Salvador Gonzalez Garcia, *Performance of parallel FDTD method for shared- and distributed-memory architectures: Application to bioelectromagnetics*. PLOS ONE. 2020. https://doi.org/10.1371/journal.pone.0238115

- Luis Diaz Angulo, Miguel Ruiz Cabello, Jesus Alvarez, Amelia Rubio Bretones, Salvador Gonzalez Garcia, *From Microscopic to Macroscopic Description of Composite Thin Panels: A Road Map for Their Simulation in Time Domain*. IEEE Transactions on Microwave Theory and Techniques. 2018. https://doi.org/10.1109/TMTT.2017.2786263.

- Miguel Ruiz Cabello, Luis Diaz Angulo, Jesus Alvarez, Ian Flintoft, Samuel Bourke, John Dawson, *A Hybrid Crank–Nicolson FDTD Subgridding Boundary Condition for Lossy Thin-Layer Modeling*. IEEE Transactions on Microwave Theory and Techniques. 2017. https://doi.org/10.1109/TMTT.2016.2637348.

- Miguel Ruiz Cabello, Luis Diaz Angulo, Amelia Rubio Bretones, Rafael Gomez Martin, Salvador Gonzalez Garcia and Jesus Alvarez, *A novel subgriding scheme for arbitrarily dispersive thin-layer modeling*, 2017 IEEE MTT-S International Conference on Numerical Electromagnetic and Multiphysics Modeling and Optimization for RF, Microwave, and Terahertz Applications (NEMO), Seville, Spain, 2017.
https://doi.org/10.1109/NEMO.2017.7964255.

- Guadalupe Gutierrez Gutierrez, Daniel Mateos Romero, Miguel Ruiz Cabello, Enrique Pascual-Gil, Luis Diaz Angulo, David Garcia Gomez, Salvador Gonzalez Garcia, 
*On the Design of Aircraft Electrical Structure Networks*, 
IEEE Transactions on Electromagnetic Compatibility. 2016. https://doi.org/10.1109/TEMC.2016.2514379.

# Usage
## Compilation

## Running cases
semba-fdtd uses the following [format](src_json_parser/readme.md) as input files.

# License
All rights reserved by the University of Granada (Spain)
_______________________________________________________________
Authors: 
- Salvador Gonzalez Garcia <salva@ugr.es>
- Miguel Ruiz Cabello <mcabello@ugr.es>
- Luis Diaz Angulo <lmdiazangulo@ugr.es>
- Amelia Rubio Bretones <arubio@ugr.es>
- Rafael Gomez Martin <rgomez@ugr.es>
_______________________________________________________________

