## check asv length per primer per type of reads

library(seqinr)

# graph jusino

bind_rows(
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may_sep/primer_test/03-asvs/asvdb-coi_jusino-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "separated by sample"),
  
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asvdb-coi_jusino-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "not separated")
) %>%
  mutate(seq_len = nchar(seq)) %>%
  ggplot(aes(x = seq_len, fill = trim)) +
  geom_histogram(binwidth = 1, position = "dodge") + ggtitle(c("Jusino primer read length"))



# graph leray
bind_rows(
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may_sep/primer_test/03-asvs/asvdb-coi_leray-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "separated by sample"),
  
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asvdb-coi_leray-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "not separated")
) %>%
  mutate(seq_len = nchar(seq)) %>%
  ggplot(aes(x = seq_len, fill = trim)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  ggtitle(c("Leray primer read length with xlim (290,320)")) + xlim(290,320)


# graph verkuil
bind_rows(
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may_sep/primer_test/03-asvs/asvdb-coi_verkuil-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "separated by sample"),
  
  read.fasta("C:/Users/abelk/Documents/Conservation MSc/DNA metabarcoding RSP1/Sequencing data/habrok_download/primer_test_may/primer_test/03-asvs/asvdb-coi_verkuil-20260504.fasta", as.string = T) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Zotu", values_to = "seq") %>%
    mutate(trim = "not separated")
) %>%
  mutate(seq_len = nchar(seq)) %>%
  ggplot(aes(x = seq_len, fill = trim)) + 
  geom_histogram(binwidth = 1, position = "dodge") +ggtitle(c("Verkuil primer read length")) 
