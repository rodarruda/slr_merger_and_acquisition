## Análise TF-IDF

#- TF-IDF: Term Frequency-Inverse Document Frequency
#   + TF: aumenta o peso de palavras muito utilizadas. Vamos utilizar uma alternativa ao uso das *stop words*
#   + IDF: diminui o peso de palavras comumente utilizadas e aumenta o peso de palavras que não são muito utilizadas em uma coleção de documentos
#   + TF-IDF: Multiplica os dois fatores calculando a frequência de uso ponderada por quão raro o termo é utilizado.
#   + A estatística IDF é calculada como: IDF(termo)=LN(Ndocs/Ndocs que contém o termo)

#- Carga de bibliotecas no R
#install.packages("tidytext")
#install.packages("qdap")
#install.packages("forcats")
#install.packages("gridExtra")

library(dplyr)
library(tidytext)

require(data.table)
require(ggplot2)

require(gridExtra)

library(qdap)
library(forcats)

library(stringr)

basePath <- "C:\\Your root path here"

#Open txt files with transcription of interviews - room to improve by using array.

pathEnt1_1 <- paste(basePath, "Transcricao Entrevista E1_B1.txt",sep = "")
pathEnt1_2 <- paste(basePath, "Transcricao Entrevista E1_B2.txt",sep = "")

ent1_1 <- readLines(pathEnt1_1, encoding = "UTF-8")
ent1_2 <- readLines(pathEnt1_2, encoding = "UTF-8")

pathEnt2_1 <- paste(basePath, "Transcricao Entrevista E2_B1.txt",sep = "")
pathEnt2_2 <- paste(basePath, "Transcricao Entrevista E2_B2.txt",sep = "")

ent2_1 <- readLines(pathEnt2_1, encoding = "UTF-8")
ent2_2 <- readLines(pathEnt2_2, encoding = "UTF-8")


pathEnt3_1 <- paste(basePath, "Transcricao Entrevista E3_B1.txt",sep = "")
pathEnt3_2 <- paste(basePath, "Transcricao Entrevista E3_B2.txt",sep = "")

ent3_1 <- readLines(pathEnt3_1, encoding = "UTF-8")
ent3_2 <- readLines(pathEnt3_2, encoding = "UTF-8")

pathEnt4_1 <- paste(basePath, "Transcricao Entrevista E4_B1.txt",sep = "")
pathEnt4_2 <- paste(basePath, "Transcricao Entrevista E4_B2.txt",sep = "")

ent4_1 <- readLines(pathEnt4_1, encoding = "UTF-8")
ent4_2 <- readLines(pathEnt4_2, encoding = "UTF-8")

pathEnt5_1 <- paste(basePath, "Transcricao Entrevista E5_B1.txt",sep = "")
pathEnt5_2 <- paste(basePath, "Transcricao Entrevista E5_B2.txt",sep = "")

ent5_1 <- readLines(pathEnt5_1, encoding = "UTF-8")
ent5_2 <- readLines(pathEnt5_2, encoding = "UTF-8")


trataSimbolos <- function(n){
  n <- gsub(",","",n)
  n <- gsub("\\.","",n)
  n <- gsub("\\?","",n)
  return(n)
}

ent1_1 <- tolower(trataSimbolos(ent1_1))
ent1_2 <- tolower(trataSimbolos(ent1_2))

ent2_1 <- tolower(trataSimbolos(ent2_1))
ent2_2 <- tolower(trataSimbolos(ent2_2))

ent3_1 <- tolower(trataSimbolos(ent3_1))
ent3_2 <- tolower(trataSimbolos(ent3_2))

ent4_1 <- tolower(trataSimbolos(ent4_1))
ent4_2 <- tolower(trataSimbolos(ent4_2))

ent5_1 <- tolower(trataSimbolos(ent5_1))
ent5_2 <- tolower(trataSimbolos(ent5_2))


#REMOVE all questions, in transcription is started by AUTHOR: 
breakIntoWords_DF <- function(n, interviewee){
  removeQuestions <- {"AUTHOR:"}
  
  n <- n[!grepl(paste0(removeQuestions, collapse = "|"), n)]
  
  words <- paste(n,collapse=" ")
  
  words <- strsplit(words, split=' ', fixed=TRUE)
  
  dataFrame <- data.frame(words, interviewee)
  
  colnames(dataFrame)[1] <- "Words"
  
  return(dataFrame)
}

### Entrevistado 1
parcial1_1 <- breakIntoWords_DF(ent1_1, "E1 - Primeiro Bloco")
parcial1_2 <- breakIntoWords_DF(ent1_2, "E1 - Segundo Bloco")


### Entrevistado 2
parcial4_1 <- breakIntoWords_DF(ent4_1, "E2 - Primeiro Bloco")
parcial4_2 <- breakIntoWords_DF(ent4_2, "E2 - Segundo Bloco")


### Entrevistado 3
parcial5_1 <- breakIntoWords_DF(ent5_1, "E3 - Primeiro Bloco")
parcial5_2 <- breakIntoWords_DF(ent5_2, "E3 - Segundo Bloco")


### Entrevistado 4
parcial2_1 <- breakIntoWords_DF(ent2_1, "E4 - Primeiro Bloco")
parcial2_2 <- breakIntoWords_DF(ent2_2, "E4 - Segundo Bloco")


### Entrevistado 5
parcial3_1 <- breakIntoWords_DF(ent3_1, "E5 - Primeiro Bloco")
parcial3_2 <- breakIntoWords_DF(ent3_2, "E5 - Segundo Bloco")

head(parcial1_1)

# Removal of non-related terms (conjunctions, common words etc...)
estruturais<-c( "a",       "à",       "agora",   "ainda",   "algum",   "alguma",  "algumas", "alguns", 
                "antes",   "ao",      "aos",     "aqui",    "as",      "às",      "assim",  "até",
                "bem",     "CAPÍTULO","com",     "comigo",  "como",    "cousa",   "creio",   "da",      "dar",
                "das",     "dava",    "de",      "dela",     "dele",   "dentro",  "depois",  "depressa", "deu",
                "disse",   "dizer",   "dizia",   "do",      "dos",     "dous",    "duas",    "e",       "é",
                "ela",     "ele",     "eles",    "em",      "então",   "entrar",  "entre",   "era",     "eram",
                "esta",    "está",    "estar",   "estava",  "este",    "eu",      "falar",
                "fazer",   "fez",     "fim",     "foi",     "fora",    "fosse",   "fui",     "gente",   "grande",
                "há",      "havia",   "ia",      "ir",      "isso",    "isto",    "já",      "Já",      "la",      "lá",
                "lhe",     "lo",      "logo",    "mais",    "mal",     "mas",     "me",      "meio",    "melhor",
                "menos",   "mesma",   "mesmo",   "meu",     "meus",    "mim",     "minha",   "muito",   "na",      "nada",
                "não",     "nas",     "nem",     "no",      "nos",     "nós",     "nossa",   "o",       "os",      "Os",      "ou",
                "outra",   "outras",  "outro",   "outros",  "para",    "pela",
                "pelo",    "pode",    "podia",   "pois",    "por",     "porque",  "pouco",   "primeira","primeiro","quando",
                "quando",  "que",     "queria",  "quis",    "são",     "se",      "sei",     "sem",
                "sempre",  "senhor",  "senhora", "ser",     "seu",     "seus",    "si",      "só",      "sua",     "tal",
                "também",  "tanto",   "tão",     "tarde",   "tem",     "ter",     "tinha",   "toda",    "todas",
                "todo",    "todos",   "tudo",    "uma",     "veio",    "vez",     "vezes",
                "vi", "df:", "bom", "vamos", "você","vão", "vou", "aa:", "sa:", 
                "acho", "nesse","cada","foram","essa","2","esse","talvez","bastante", "cara", "vai", "tipo"
                , "época", "caso", "normalmente", "certamente", "vendo", "ano", "basicamente", "certo", "obviamente", "pesando", "além", "anterior", "aquela", "costumavam", "dia", "quer", "etapa", "varia"
                , "90", "120", "algo", "alguém", "ali", "anos" ,"dessa" ,"desse" ,"deste" ,"deve" ,"devia" ,"dias"
                , "dois", "elas", "enfim", "estando", "estavam", "estavam", "etc", "ali", "etc", "ler", "num", "par", "pós", "pra", "pro", "quê", "sim", "uns", "ver"
                , "nessa", "novo", "onde", "seja", "sendo", "será", "serem", "série"
                , "tenha", "tenho", "tentar", "teve", "tido", "tinham", "tive", "trazer", "vale", "vários"
                , "torno", "qual", "tipicamente", "aspecto", "tava", "elemento", "comprador", "comprar", "menor", "diria"
                , "apenas", "apresenta", "chegar", "existir", "poderia", "diferentes", "leva", "especialmente", "extremamente", "mapeando", "máximo", "tomar", "muitas"
                , "20%", "80%", "alí", "boa", "dão", "faz", "fiz", "for", "ham", "têm", "têm", "xyz"
                , "hoje", "hora", "indo", "iria", "lado", "mora", "numa", "olha", "pega", "putz", "quem", "sido", "terá", "trás", "traz", "vejo", "vira", "vivi", "ambos", "caramba", "desses", "essas", "esses"
                , "estão", "estes", "estilo", "estou", "existe", "existem", "existia"
                , "react", "python", "invés", "neste", "ninguém", "quais", "supor", "tendo", "teria", "termos", "venha"
                , "pergunta", "resposta", "tende", "aquele", "durante", "fala", "utilizada", "somado"
                , "acredito", "geral", "próprio", "eventuais", "gerar", "sobre", "usualmente", "tópicos"
                , "posterior", "questão", "depender", "aspectos", "pauta", "trabalhistas"
                , "evidente", "motivo", "conseguir", "workshop", "gj:", "pv:"
                , "consegue", "falando", "participei", "primeiros", "imagino"
                , "querer", "deles", "usa"
                , "alteração", "backup", "definido", "dificulta", "entregando", "envolvidas", "escolhendo", "fase", "integrado", "internas", "longa", "maioria", "nenhuma", "pessoal", "times"
                , "goldman", "passar", "perna", "bancário", "apareço", "continua", "fiscal", "conosco", "própria", "sofreu", "procuraram", "começou", "vasta", "nosso"
                , "virar", "saber", "durar", "ficar", "justamente", "forma", "monte", "juntar", "levar", "vir"
                , "entregar", "precisar"
                )

analiseAntes <- rbind(parcial1_1, parcial2_1, parcial3_1, parcial4_1, parcial5_1)

analiseAntes <- subset(analiseAntes, !(Words %in% estruturais) & !(nchar(Words) <= 2))

head(analiseAntes)


analiseDepois <- rbind(parcial1_2, parcial2_2, parcial3_2, parcial4_2, parcial5_2)

analiseDepois <- subset(analiseDepois, !(Words %in% estruturais) & !(nchar(Words) <= 2))

head(analiseDepois)


interview_words_Antes <- analiseAntes %>% 
unnest_tokens(word, Words, token = "regex", pattern = "\\s+") %>%
count(interviewee, word, sort = TRUE) %>% ungroup()

head(interview_words_Antes)

interview_words_Depois <- analiseDepois %>% 
unnest_tokens(word, Words, token = "regex", pattern = "\\s+") %>%
count(interviewee, word, sort = TRUE) %>% ungroup()

head(interview_words_Depois)



interview_words_Antes <- interview_words_Antes %>% bind_tf_idf(word, interviewee, n)

head(interview_words_Antes)


interview_words_Depois <- interview_words_Depois %>% bind_tf_idf(word, interviewee, n)

head(interview_words_Depois)



#OS log abaixo podem ser usados para comparar os índices TF-IDF dos termos.

log(5/1) #1.609438
log(5/2) #0.9162907
log(5/3) #0.5108256
log(5/4) #0.2231436
log(5/5) #0


interview_words_Antes %>% arrange(desc(tf_idf))



interview_words_Antes %>% arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(interviewee) %>% top_n(15) %>% ungroup %>%
  ggplot(aes(word, tf_idf, fill = interviewee)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = NULL) + facet_wrap(~interviewee, ncol = 2, scales = "free") + coord_flip()


interview_words_Depois %>% arrange(desc(tf_idf))


#testeXPTO <- interview_words_Depois %>% arrange(interviewee, desc(tf_idf)) %>%
#  mutate(word = factor(word, levels = rev(unique(word)))) %>%
#  group_by(interviewee) %>% top_n(15) %>% ungroup



interview_words_Depois %>% arrange(interviewee, desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(interviewee) %>% top_n(15) %>% ungroup %>%
  ggplot(aes(x = reorder(word, tf_idf), tf_idf, fill = interviewee)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = NULL) + facet_wrap(~interviewee, ncol = 2, scales = "free") + coord_flip()


interview_words_Antes %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% top_n(20) %>% 
  ggplot(aes(word, tf_idf)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + coord_flip()


interview_words_Depois %>% arrange(desc(tf_idf))


interview_words_Depois %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% top_n(20) %>% 
  ggplot(aes(word, tf_idf)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + coord_flip()


#Write to csv to facilitate word / results checking.
#write.csv(interview_words_Antes, file = paste(basePath, "PalavrasAntes.csv",sep = ""))
#write.csv(interview_words_Depois %>% arrange(interviewee, desc(tf_idf)) %>%
            mutate(word = factor(word, levels = rev(unique(word)))), file = paste(basePath, "PalavrasDepois.csv",sep = ""))
