module Navi.Config.Phase
  ( ConfigPhase (..),
  )
where

-- | Phase index for config data.
data ConfigPhase
  = -- | Toml phase.
    ConfigPhaseToml
    -- | Env phase.
  | ConfigPhaseEnv
