# force the evaluation of all objects so delayedAssign() completes
invisible(lapply(parent.env(registry), force))
register_conditions()
