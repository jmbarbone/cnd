# forces the evaluation of objects made with delayedAssign()
cnd_evaluate()

# update package functions and build documentation
cnd_exports()
class(condition) <- "cnd::condition_generator"
cnd_document()
