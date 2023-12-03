module labels_mod
   ! LABELS
   ! -- common labels
   character (len=*), parameter :: J_NAME = "name"
   character (len=*), parameter :: J_ID = "id"
   character (len=*), parameter :: J_TYPE = "type"

   character (len=*), parameter :: J_DIR_X = "x"
   character (len=*), parameter :: J_DIR_Y = "y"
   character (len=*), parameter :: J_DIR_Z = "z"

   ! -- materials
   character (len=*), parameter :: J_MATERIALS = "materials"
   character (len=*), parameter :: J_MAT_TYPE_WIRE = "wire"
   character (len=*), parameter :: J_MAT_WIRE_RADIUS = "radius"
   character (len=*), parameter :: J_MAT_WIRE_RESISTANCE = "resistancePerMeter"
   character (len=*), parameter :: J_MAT_WIRE_INDUCTANCE = "inductancerPermeter"
   character (len=*), parameter :: J_MAT_TYPE_CONNECTOR = "connector"
   character (len=*), parameter :: J_MAT_CONNECTOR_TYPE = "connectorType"

   ! -- cables -- thin wires
   character (len=*), parameter :: J_CABLES = "cables"
   character (len=*), parameter :: J_CAB_MAT_ID = "materialId"
   character (len=*), parameter :: J_CAB_INI_CONN_ID = "initialConnectorId"
   character (len=*), parameter :: J_CAB_END_CONN_ID = "endConnectorId"

   ! -- Mesh and geometry.
   character (len=*), parameter :: J_MESH = "mesh"

   character (len=*), parameter :: J_COORDINATES = "coordinates"
   character (len=*), parameter :: J_COORD_POS = "position"
   character (len=*), parameter :: J_ELEMENTS = "elements"
   character (len=*), parameter :: J_POLYLINES = "polylines"
   character (len=*), parameter :: J_NODES = "nodes"
   character (len=*), parameter :: J_ELEM_COORD_IDS = "coordinateIds"

   character (len=*), parameter :: J_ELEMENTIDS = "elementIds"

   character (len=*), parameter :: J_VOXEL_REGION = "voxelRegion"
   character (len=*), parameter :: J_VOXELS = "voxels"
   character (len=*), parameter :: J_SURFELS = "surfels"
   character (len=*), parameter :: J_LINELS = "linels"
   character (len=*), parameter :: J_PIXELS = "pixels"


   ! type(NFDEGeneral)
   character (len=*), parameter :: J_GENERAL = "general"
   character (len=*), parameter :: J_TIME_STEP = "timeStep"
   character (len=*), parameter :: J_NUMBER_OF_STEPS = "numberOfSteps"

   ! type(Desplazamiento)
   character (len=*), parameter :: J_GRID = "grid"
   character (len=*), parameter :: J_NUMBER_OF_CELLS = "numberOfCells"
   character (len=*), parameter :: J_STEPS = "steps"

   ! type(Frontera)
   character (len=*), parameter :: J_BOUNDARY = "boundary"
   character (len=*), parameter :: J_BOUNDARY_TYPE = "type"
   character (len=*), parameter :: J_BND_ALL = "all"
   character (len=*), parameter :: J_BND_PEC = "pec"
   character (len=*), parameter :: J_BND_PMC = "pmc"
   character (len=*), parameter :: J_BND_PERIODIC = "periodic"
   character (len=*), parameter :: J_BND_MUR = "mur"
   character (len=*), parameter :: J_BND_PML = "pml"
   character (len=*), parameter :: J_BND_PML_LAYERS = "layers"
   character (len=*), parameter :: J_BND_PML_ORDER = "order"
   character (len=*), parameter :: J_BND_PML_REFLECTION = "reflection"

   ! -- source types
   character (len=*), parameter :: J_SOURCES = "sources"
   character (len=*), parameter :: J_SRC_MAGNITUDE_FILE = "magnitudeFile"
   character (len=*), parameter :: J_SRC_TYPE = "type"
   ! type(Planewave)
   character (len=*), parameter :: J_PW_TYPE = "planewave"
   character (len=*), parameter :: J_PW_ATTRIBUTE = "attribute"
   character (len=*), parameter :: J_PW_DIRECTION = "direction"
   character (len=*), parameter :: J_PW_DIRECTION_THETA = "theta"
   character (len=*), parameter :: J_PW_DIRECTION_PHI = "phi"
   character (len=*), parameter :: J_PW_POLARIZATION = "polarization"
   character (len=*), parameter :: J_PW_POLARIZATION_ALPHA = "alpha"
   character (len=*), parameter :: J_PW_POLARIZATION_BETA  = "beta"

   ! --- probe types
   character (len=*), parameter :: J_PROBES = "probes"
   character (len=*), parameter :: J_PR_TYPE = "type"
   character (len=*), parameter :: J_PR_OUTPUT_NAME = "outputName"
   character (len=*), parameter :: J_PR_DIRECTIONS = "directions"

   ! domain stuff
   character (len=*), parameter :: J_PR_DOMAIN = "domain"
   character (len=*), parameter :: J_PR_DOMAIN_FILENAME = "filename"
   character (len=*), parameter :: J_PR_DOMAIN_TYPE = "type"
   character (len=*), parameter :: J_PR_DOMAIN_TIME = "time"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ = "frequency"
   character (len=*), parameter :: J_PR_DOMAIN_TRANSFER = "transfer"
   character (len=*), parameter :: J_PR_DOMAIN_TIMEFREQ = "timeFrequency"
   character (len=*), parameter :: J_PR_DOMAIN_TIMETRANSF = "timeTransfer"
   character (len=*), parameter :: J_PR_DOMAIN_FREQTRANSF = "frequencyTransfer"
   character (len=*), parameter :: J_PR_DOMAIN_TIMEFREQTRANSF = "all"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_START = "timeStart"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_STOP   = "timeEnd"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_STEP  = "timeStep"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_START = "frequencyStart"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_STOP   = "frequencyEnd"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_STEP  = "frequencyStep"

   ! type(Sonda)
   character (len=*), parameter :: J_PR_FARFIELD = "farField"
   ! type(MasSonda)
   character (len=*), parameter :: J_PR_ELECTRIC = "electric"
   character (len=*), parameter :: J_PR_MAGNETIC = "magnetic"
   character (len=*), parameter :: J_PR_CURRENT = "current"
   character (len=*), parameter :: J_PR_VOLTAGE = "voltage"

end module
