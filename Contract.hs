-- the data type contract defined on the constructors in the paper

type Date     = Double
type Obs      = Double
type Currency = String

data Contract = Zero Contract
  | One Date Currency
  | Give Contract
  | And Contract Contract
  | Or Contract Contract
  | Truncate Date Contract
  | Then Contract Contract
  | Scale Obs Contract
  | Get Contract
  | Anytime Contract
  
