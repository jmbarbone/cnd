# forces the evaluation of objects made with delayedAssign()
cnd_evaluate()

# update package functions and build documentation
cnd_exports()
op <- options(cnd.condition.silent = TRUE)
cnd_document("cnd")
options(op)
