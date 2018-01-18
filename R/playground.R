

## Package dependencies
# require(CodeDepends)
# require(Rgraphviz)

## Analyze script
sc <- readScript("experiments.R")
tmp <- getInputs(sc)

# Remove input rows that cause problems (typically tidyverse -related)
sct <- tibble(idx = 1:length(sc), script = sc) %>%
        group_by(idx) %>%
        mutate(inputs = try(getInputs(script)),
               success = class(inputs[[1]]) == "ScriptNodeInfo")
good_idx <- filter(sct, success)$idx
info = getInputs(sc[good_idx])

# Make some graph
g <- makeVariableGraph( info = getInputs(sc[good_idx]) )

pdf('experiments-R.pdf', width = 20, height = 20)
plot(g)
dev.off()

g2 = makeTaskGraph(info = getInputs(sc[good_idx]))
plot(g2)


## Call graphs for packages
gg = makeCallGraph("package:ggplot2")
gg = layoutGraph(gg, layoutType = "circo")
graph.par(list(nodes = list(fontsize=55)))
renderGraph(gg) ## could also call plot directly



f = system.file("samples", "dual.R", package = "CodeDepends")
g = makeTaskGraph(f)
plot(g)

g = makeTaskGraph("/home/jkor/work_local/projects/core/npp-operator-stress/R/experiments.R")
plot(g)
