## devtools::install_github("felipepratalima/taxdumpr")
require(taxdumpr)
require(magrittr)
require(dplyr)

set.seed(1)

x <- Taxdumpr("~/Dados/taxdump/nodes.dmp", "~/Dados/taxdump/names.dmp", "~/Dados/taxdump/merged.dmp")

dados <- read.delim("CompostitionForLoadingFunctions.tsv", header = T)

n <- 900
classifications <- sample(unique(dados$TaxonomyId), n, replace = T)
classifications <- c(classifications, replicate(100, 1))

classificationsDf <- data.frame(
  sequenceId = 1:1000,
  taxonomyId = classifications
)
taxonomyDf <- getStandardLineageIdsAndScientificNamesByIdsAsDataFrame(x, unique(classificationsDf$taxonomyId))
classificationsDf <- classificationsDf %>% merge(taxonomyDf, by = "taxonomyId")
write.table(classificationsDf, "classifications.tsv", sep = "\t", quote = F, row.names = F, col.names = T)


classificationsSummariesDf <- rbind(
  classificationsDf %>% group_by(superkingdomId, superkingdomName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "superkingdom") %>%
    select(taxonomyId = superkingdomId, taxonomyName = superkingdomName, rank, CumulativePercentual),
  classificationsDf %>% group_by(phylumId, phylumName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "phylum") %>%
    select(taxonomyId = phylumId, taxonomyName = phylumName, rank, CumulativePercentual),
  classificationsDf %>% group_by(classId, className) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "class") %>%
    select(taxonomyId = classId, taxonomyName = className, rank, CumulativePercentual),
  classificationsDf %>% group_by(orderId, orderName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "order") %>%
    select(taxonomyId = orderId, taxonomyName = orderName, rank, CumulativePercentual),
  classificationsDf %>% group_by(familyId, familyName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "family") %>%
    select(taxonomyId = familyId, taxonomyName = familyName, rank, CumulativePercentual),
  classificationsDf %>% group_by(genusId, genusName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "genus") %>%
    select(taxonomyId = genusId, taxonomyName = genusName, rank, CumulativePercentual),
  classificationsDf %>% group_by(speciesId, speciesName) %>% summarise(CumulativePercentual = n() / 1000 * 100) %>%
    mutate(rank = "species") %>%
    select(taxonomyId = speciesId, taxonomyName = speciesName, rank, CumulativePercentual)
)

profileDf <- data.frame(table(classifications))
colnames(profileDf) <- c("taxonomyId", "count")
profileDf$classificationsPercentual <- profileDf$count / sum(profileDf$count) * 100
profileDf <- profileDf %>% merge(classificationsSummariesDf, by = "taxonomyId", all = T)
write.table(profileDf, "profile.tsv", sep = "\t", quote = F, row.names = F, col.names = T)

