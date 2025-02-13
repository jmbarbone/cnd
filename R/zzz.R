# forces the evaluation of objects made with delayedAssign()
cnd_evaluate()

# update package functions and build documentation
cnd_exports()
# TODO replace with "cnd::condition_progenitor
# TODO replace "cnd::condition_function" with "cnd::condition_generator"
class(condition) <- "cnd::condition_generator"
cnd_document()
