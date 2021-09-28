print.modelPochard <-
function(x, ...)
{
    if (!inherits(x, "modelPochard")) {
        stop("object should inherit the class modelPochard")
    }
    summary(x,...)
}
