!=============================================================================80
!
!> Define global types.
!
!=============================================================================80

module global_type_defs

  use type_defs_equation
  use type_defs_dimension
  use type_defs_bc
  use type_defs_bcdata
  use type_defs_gas
  use type_defs_variable
  use type_defs_scheme
  use type_defs_file

  implicit none

!...Define scope
  private

  public :: TypeDefs

!...Declare class
  type :: ClassTypeDefs
    type(ClassDimensionType),  public :: Dimension
    type(ClassBCType),         public :: BC
    type(ClassBCDataType),     public :: BCData
    type(ClassEquationType),   public :: Equation
    type(ClassGasType),        public :: Gas
    type(ClassVariableType),   public :: Variable
    type(ClassFileType),       public :: File
    type(ClassSchemeType),     public :: Scheme
  contains
    procedure, public, nopass :: DimensionIndex => DimensionIndex_fun
    procedure, public, nopass :: DimensionName  => DimensionName_fun
    procedure, public, nopass :: BCTypeIndex => BCTypeIndex_fun
    procedure, public, nopass :: BCTypeName  => BCTypeName_fun
    procedure, public, nopass :: BCDataTypeIndex  => BCDataTypeIndex_fun
    procedure, public, nopass :: BCDataTypeName   => BCDataTypeName_fun
    procedure, public, nopass :: EquationIndex => EquationIndex_fun
    procedure, public, nopass :: EquationName  => EquationName_fun
    procedure, public, nopass :: EqnVarIndex   => EqnVarIndex_fun
    procedure, public, nopass :: EqnResIndex   => EqnResIndex_fun
    procedure, public, nopass :: EqnResName    => EqnResName_fun
    procedure, public, nopass :: GasIndex => GasIndex_fun
    procedure, public, nopass :: GasName  => GasName_fun
    procedure, public, nopass :: VariableIndex => VariableIndex_fun
    procedure, public, nopass :: VariableName  => VariableName_fun
    procedure, public, nopass :: FileIndex => FileIndex_fun
    procedure, public, nopass :: FileName  => FileName_fun
    procedure, public, nopass :: FileType  => FileType_fun
    procedure, public, nopass :: FileExt   => FileExt_fun
    procedure, public, nopass :: SchemeIndex => SchemeIndex_fun
    procedure, public, nopass :: SchemeName  => SchemeName_fun
  end type ClassTypeDefs

  type(ClassTypeDefs), protected :: TypeDefs

end module global_type_defs
