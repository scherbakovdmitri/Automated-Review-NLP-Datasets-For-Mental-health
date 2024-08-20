require(tidyverse)

df=read.csv('raw.extracted.data.csv')

choose.multiple.del.2=function(x) 
{
  x=na.omit(x)
  if (all(is.na(x))) return("Not extracted")
  if (length(x)==1) return(paste0(x,"*"))
  else
  {
    res=x %>% str_split(',') %>% unlist %>% trimws %>% table %>% as.data.frame %>%
      filter(Freq>1) %>% #mutate(sig=paste0(`.`)) %>% 
      mutate(sig=case_when(Freq==1~paste0(`.`,"*"),
                           Freq==2~paste0(`.`,"**"),
                           Freq>=3~paste0(`.`,"***")
      )) %>%
      pull(sig) %>% paste0(collapse=',')
    if (str_length(res)<1) return(paste0(x[1],"*"))
    else return(res)
  }
}


choose=function(x) 
{
  #print(x)
  x=na.omit(x)
  if (all(is.na(x))) return("Not extracted")
  #return(paste0(names(table(x)[1])))
  t=sort(table(x),d=T)
  if (t[1]>=3) sig="***"
  if (t[1]==2) sig="**"
  if (t[1]==1) sig="*"
  if (nrow(table(x))>1&t[1]==1&names(t[1]) %in% c("not related to mental health","didn't use natural language processing"))
    return(paste0(names(t[2]),sig))   ## if it is not related extrat something else
  #else 
  return(paste0(names(t[1]),sig))
  
}

df.2=
  df %>% #filter(id=='#10379') %>% head()
  mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  mutate_all(~case_when(
    str_detect(.x,'not.*applicable|specified|mentioned|provided|reported|named')~NA,#"N/A",
    str_detect(.x,'none')~NA,#"N/A",
    str_length(.x)<1~NA,#"N/A",
    is.na(.x)~NA,#"N/A",
    .default=.x
  )) %>% 
  group_by(id) %>% 
  mutate(across(c(fld4,fld7,fld8,fld10,fld11,fld12,fld13,fld14),choose))  %>%  
  mutate(across(c(fld1,fld2,fld3,fld5,fld6,fld9),choose.multiple.del.2)) %>% 
  ungroup %>% 
  mutate_all(~tidyr::replace_na(.x,"N/A")) %>% 
  mutate_all(~ifelse(str_length(.x)==0,"N/A",.x)) %>% 
  mutate_all(toupper) 


df.3=
  df.2 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup  %>% 
  filter(!str_detect(tolower(fld2),"didn't use natural language processing")) %>% 
  filter(!str_detect(tolower(fld4),"not related to mental health")) 



df.2 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup  %>% nrow ### total extracted

df.2 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup  %>% 
  filter(str_detect(tolower(fld2),"didn't use natural language processing")) %>% nrow ## didn't use NLP

df.2 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup  %>% 
  filter(str_detect(tolower(fld4),"not related to mental health")) %>% 
  nrow ## not related to MH

df.3 %>% nrow


###### World map

world_map <- map_data("world")
df.3.2=
  df.3 %>% 
  #df.2 %>% 
  mutate(fld1.1=gsub("\\/.*","",fld1)) %>% 
  mutate(region=tolower(gsub("\\*","",fld1.1))) %>% pull(region) %>% 
  str_split(',') %>% 
  unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(region=case_when(col=="us"|col=='united states'|col=="ny"|col=="ca"|col=="washington dc"|col=="tn"~"usa",
                          col=="t<u+00fc>rkiye"~"turkey",
                          .default=col
  )) %>% 
  group_by(region) %>% add_tally(name="count") %>% 
  ungroup %>% distinct(region,count)
df.3.2 %>% 
  print.AsIs() %>% arrange(desc(count)) %>%
  mutate(percent=round(count*100/172,1)) %>% 
  print.AsIs()%>% clipr::write_clip()
# World map
world_map <- map_data("world") %>%  mutate(region=tolower(region))%>% left_join(df.3.2) %>% 
  mutate(count=replace_na(count,NA)) %>% 
  filter(!region %in% c('antarctica','arctica'))
m1=ggplot() +
  #geom_text(data = world_map, aes(x = long, y = lat, label = count), size = 4)#, vjust = 1.5)
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group, fill = count, alpha = count), color = "black") +
  scale_fill_gradient(low = "#E6FFE6", high = "darkgreen") +
  scale_alpha_continuous(range = c(0.9, 1)) +
  theme_minimal()+
  guides(alpha = "none")


state_lookup <- data.frame(state = tolower(state.abb), region = tolower(state.name))
df.4=
  df.3 %>% 
  mutate(fld1=gsub(".*\\/","",fld1)) %>% 
  mutate(state=tolower(gsub("\\*","",fld1))) %>% 
  left_join(state_lookup,by=c('state'='region'))  %>% 
  mutate(state=ifelse(!is.na(state.y),state.y,state)) %>% 
  left_join(state_lookup) %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% add_tally(name="count") %>% 
  ungroup %>% distinct(state,count,region) 

### Total count of states extracted
df.3 %>% 
  mutate(fld1=gsub(".*\\/","",fld1)) %>% 
  mutate(state=tolower(gsub("\\*","",fld1))) %>% 
  left_join(state_lookup,by=c('state'='region'))  %>% 
  mutate(state=ifelse(!is.na(state.y),state.y,state)) %>% 
  left_join(state_lookup) %>% 
  filter(!is.na(region)) %>% dim
df.4 %>% 
  print.AsIs() %>% arrange(desc(count)) #%>% print.AsIs() %>% clipr::write_clip()
# US map
us_map <- map_data("state") %>%  mutate(region=tolower(region))%>% left_join(df.4) %>% 
  mutate(count=replace_na(count,NA)) %>% mutate(count=as.integer(count))
m2=ggplot() +
  geom_polygon(data = us_map, aes(x=long, y = lat, group = group, fill = count), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  #scale_alpha_discrete()+
  scale_alpha_continuous(range = c(0.5, 1)) +
  theme_minimal()+
  guides(alpha = "none")


cowplot::plot_grid(m1,m2,labels=c("A","B"),nrow=2,label_size=30)

### NLP types
p2.1=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld2)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% pull(col)
  as.data.frame %>% rename(col=1) %>% #pull(col) %>% unique %>% clipr::write_clip()
  mutate(col=case_when(
    col %in% c("BERT", "ROBERTA", "RUBERT", "DISTILBERT", "BERTWEET", "BERT-CNN", 
               "GPT-3", "GPT-2", "GPT-1", "GPT-4", "CHATGPT", "GPT-3.5", "GPT-3.5-TURBO",
               "LLAMA", "LLAMA-2", "MISTRAL", "T5", "T5-SMALL", "BERT+LSTM", 
               "CAMEMBERT", "BERTOPIC", "BIOBERT", "SAPBERT", "XLM-ROBERTA", 
               "XLM", "ALBERT", "XLNET", "BERT-LSTM", "BERT-BILSTM", "SCIBERT", 
               "DISTILROBERTA", "BIOCLINICALBERT", "LONGFORMER", "DEBERTA", 
               "TRANSFORMER-BASED NEURAL NETWORKS", "TRANSFORMER-BASED MODEL", 
               "TRANSFORMER-BASED DEEP LEARNING", "TRANSFORMER-BASED ARCHITECTURE NEMO", 
               "M-BERT", "TRANSFORMERS", "BERT-RF", "ARABERT", "MARBERT", 
               "SAPBERT", "TRANSFORMER ENCODER", "TEXT-TO-TEXT TRANSFORMER", 
               "TRANSFORMER", "SWIN TRANSFORMER", "VISION TRANSFORMER", "LLM-DRIVEN CONVERSATIONAL AI") ~ "Transformer Models",
    
    col %in% c("TF-IDF", "TF-IDF WITH SVM", "BAG OF WORDS", "BOW", "N-GRAM REPRESENTATION", 
               "N-GRAM LANGUAGE MODELS", "WORD EMBEDDINGS", "WORD EMBEDDING", 
               "SWIVEL EMBEDDING", "DICT2VEC", "DOC2VEC", "WORD2VEC", "METAPATH2VEC", 
               "RDF2VEC", "TEXT2VEC", "WORD EMBEDDING ASSOCIATION TEST") ~ "Traditional Text Representation & Embedding",
    
    col %in% c("RANDOM FOREST", "DECISION TREE", "DECISION TREES", "DT", "J48", 
               "SUPPORT VECTOR MACHINE", "SVM", "SUPPORT VECTOR MACHINES", 
               "SUPPORT VECTOR CLASSIFIER", "XGBOOST", "XGBOOST CLASSIFIER", 
               "XG-BOOST", "XGB CLASSIFIER", "XGBOOSTREGRESSOR", "RANDOM FOREST", 
               "LOGISTIC REGRESSION", "LINEAR REGRESSION", "LASSO LOGISTIC REGRESSION", 
               "ELASTICNET REGULARIZED REGRESSION", "ELASTIC-NET LOGISTIC REGRESSION", 
               "ELASTIC NET REGRESSION", "BAYESIAN LOGISTIC REGRESSION", 
               "GAUSSIAN NAÏVE BAYES", "NAIVE BAYES", "NAÏVE BAYES", "MULTINOMIAL NAIVE BAYES", 
               "MULTINOMIAL NAÏVE BAYES", "COMPLEMENT NAIVE BAYES", "GRADIENT BOOSTING TREES", 
               "GRADIENT BOOSTING", "BOOSTED DECISION TREES", "GRADIENT TREE BOOSTING", 
               "ENSEMBLE MODELS", "ENSEMBLE TRANSFER LEARNING", "ENSEMBLE CLASSIFIERS", 
               "QUANTUM CONVOLUTIONAL NEURAL NETWORK", "ADABOOST", "ADAPTIVE BOOSTING", 
               "GRADIENT BOOST", "MIXED-EFFECTS MODELS", "GENERALIZED LINEAR MIXED-EFFECTS MODELS") ~ "Machine Learning Models",
    
    col %in% c("NLP TOOLS LIWC", "LIWC", "LIWC LINGUISTIC INQUIRY AND WORD COUNT", 
               "LINGUISTIC INQUIRY AND WORD COUNT LIWC", "LIWC DICTIONARIES", 
               "LIWC15 TEXT ANALYSIS", "LINGUISTIC INQUIRY AND WORD COUNT") ~ "Linguistic Inquiry & Word Count (LIWC)",
    
    col %in% c("CNN", "CNN-RNN", "CNN-LSTM", "CNN-BILSTM", "CNN & LSTM", "T-CNN", "KNN",
               "1D CNN", "1D-CNN", "TEXTCNN", "BI-LSTM", "BI-LSTM-CNN", "BILSTM-CRF", 
               "BILSTM", "LSTM-RNN", "LSTM", "RNN", "LSTM WITH ATTENTION", 
               "GRU", "BIATTENTION-GRU", "SEQ2SEQ", "ATTENTION-BASED BIDIRECTIONAL LSTM-CNN","DEEP LEARNING") ~ "Neural Network Models",
    
    col %in% c("NATURAL LANGUAGE PROCESSING", "NATURAL-LANGUAGE PROCESSING", 
               "NATURAL LANGUAGE PROCESSING NLP", "NATURAL LANGUAGE UNDERSTANDING", 
               "NATURAL LANGUAGE PROCESSING APPLICATIONS", "NATURAL LANGUAGE TOOLKIT NLTK", 
               "SPACY NLP LIBRARY", "NATURAL LANGUAGE PRE-PROCESSING", "CORPUS-BASED DISCOURSE ANALYSIS AND NATURAL LANGUAGE PROCESSING", 
               "NLP", "NLP TOOLS", "NLP ALGORITHM", "NLP-BASED METHODS", 
               "HIGH THROUGHPUT PHENOTYPING NLP", "OTHER NLP CHALLENGE", 
               "OTHER NATURAL LANGUAGE PROCESSING", "RULE-BASED NLP", 
               "LEXICON-BASED APPROACH", "LEXICON- AND RULE-BASED NLP", 
               "RULES-BASED NATURAL LANGUAGE PROCESSING", "RULES-BASED APPROACH", 
               "RULES-BASED NATURAL LANGUAGE PROCESSING", "EDRUGTRENDS NLP-BASED TEXT PROCESSING TOOLS", 
               "LEO NLP", "HPSG-BASED NLP", "GATE", "STANFORD CORENLP", 
               "MEDLEE", "MTERMS NLP SYSTEM AND MACHINE LEARNING CLASSIFICATION ALGORITHMS", 
               "CTAKES", "OTHER COMPUTERIZED TEXT ANALYSIS") ~ "Natural Language Processing Tools",
    
    col %in% c("TOPIC MODELING", "TOPIC MODELLING", "STRUCTURAL TOPIC MODELLING", 
               "STRUCTURAL TOPIC MODELING", "LDA", "LDA-VEM", "LLDA", "SLDA", 
               "BITERM TOPIC MODEL BTM", "BITERM TOPIC MODELING", "UNSUPERVISED TOPIC MODELING", "TEXT-MINING",
               "DYNAMIC TOPIC MODELING", "TEXT-MINING-ANALYSIS", "TEXT-MINING ANALYSIS", 
               "TEXT MINING", "TEXT MINING ANALYSIS", "TEXT MINING THEMATIC ANALYSIS", 
               "TEXT CLASSIFICATION", "TEXT ANALYTICS", "TEXT MINING AND MACHINE LEARNING", 
               "TEXT MINING/MACHINE LEARNING", "TEXT-BASED MEASURE", 
               "NATURAL LANGUAGE PROCESSING TOPIC MODELLING", "OTHER TEXT MINING", 
               "TEXT MINING THEMATIC ANALYSIS") ~ "Topic Modeling & Text Mining",
    
    col %in% c("RULE-BASED METHOD", "RULE-BASED APPROACH", "RULE-BASED PARSER", 
               "RULES-BASED METHODS", "RULE-BASED", "PATTERN-MATCHING", 
               "OTHER RULE-BASED ESTIMATOR", "RULE-BASED APPROACH AND HYBRID MACHINE LEARNING", 
               "OTHER REGULAR EXPRESSION MATCHING", "REGULAR EXPRESSIONS", 
               "RULE-BASED NLP", "RULES-BASED NATURAL LANGUAGE PROCESSING", 
               "LEXICON- AND RULE-BASED NLP", "RULES-BASED APPROACH") ~ "Rule-Based Methods",
    
    col %in% c("BAYESIAN NETWORKS", "BAYESIAN NETWORK", "BAYESIAN NETWORK ANALYSIS", 
               "BAYESIAN LOGISTIC REGRESSION") ~ "Bayesian Models",
    
    col %in% c("SUPERVISED MACHINE LEARNING", "SUPERVISED CLASSIFICATION", 
               "SUPERVISED CLASSIFIERS", "SEMI-SUPERVISED LEARNING", 
               "POSITIVE AND UNLABELED LEARNING", "UNSUPERVISED TOPIC MODELING", 
               "ZERO-SHOT LEARNING", "REINFORCEMENT LEARNING", "MACHINE LEARNING", 
               "OTHER MACHINE LEARNING", "TREE-BASED MACHINE LEARNING", 
               "OTHER MACHINE LEARNING APPROACHES", "MACHINE TRANSLATION", 
               "SUPERVISED LEARNING", "SUPERVISED CLASSIFIERS") ~ "Unspecified Machine Learning approaches",
    
    col %in% c("VADER", "VADER SENTIMENT ANALYSIS", "SENTIMENT ANALYSIS", 
               "SENTIMENT ANALYSIS AND COGNITION ENGINE", "SENTIMENT-TRANSFORMERS NLP MODEL", 
               "ASPECT-BASED SENTIMENT ANALYSIS", "STYLOMETRIC MEASURES AND LEXICON-BASED SENTIMENT ANALYSIS", "TEXT SENTIMENT ANALYSIS") ~ "Sentiment Analysis",
    TRUE ~ "Other"
  )) %>% 
  #  mutate(col=ifelse(!col %in% c("BERT","LDA","SVM","CNN","LSTM","ROBERTA","LIWC","RNN","KNN"),str_to_title(col),col)) %>%
  group_by(col) %>% tally %>% #clipr::write_clip()
  filter(n>=5) %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('')+
  theme(text=element_text(size=24))+
  coord_flip()

#### outcomes
p1.2=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld3)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  #mutate(col=str_to_title(col)) %>%
  mutate(col=case_when(
    str_detect(tolower(col),"ptsd|traumatic stress")~"PTSD",
    str_detect(tolower(col),"depress")~"DEPRESSION",
    str_detect(tolower(col),"suicid")~"SUICIDE",
    str_detect(tolower(col),"opioid use|substance abuse|drug abuse|addiction|smoking|opioid misuse|substance use|overdose|alcohol")~"SUBSTANCE USE DISORDER",
    str_detect(tolower(col),"partner violence")~"DOMESTIC VIOLENCE",
    str_detect(tolower(col),"anxiety")~"ANXIETY",
    str_detect(tolower(col),"alzheimer")~"DEMENTIA",
    str_detect(tolower(col),"adhd|attention deficit")~"ADHD",
    str_detect(tolower(col),"mental health|mental illness|mental disorders")~"MENTAL HEALTH (UNSPECIFIED)",
    str_detect(tolower(col),"anorexia")~"EATING DISORDERS",
    str_detect(tolower(col),"distress")~"STRESS",
    str_detect(tolower(col),"cancer")~"CANCER-RELATED",
    str_detect(tolower(col),"diabetes")~"DIABETES-RELATED",
    .default=col
  )) %>% 
  mutate(col=ifelse(!col %in% c("PTSD","ADHD"),str_to_title(col),col)) %>%
  group_by(col) %>% tally %>%  arrange(desc(n)) %>% #clipr::write_clip()
  filter(n>5) %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('')+
  theme(text=element_text(size=24))+
  coord_flip()


#### area
p1.1=df.3 %>% 
  mutate(fld2=gsub("[*]","",fld4)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(col=case_when(
    str_detect(tolower(col),"addiction")~"SUBSTANCE ABUSE",
    .default=col
  )) %>% 
  mutate(col=str_to_title(col)) %>%
  group_by(col) %>% tally %>%  #clipr::write_clip()
  filter(n>5) %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('')+
  theme(text=element_text(size=30))+
  coord_flip()

cowplot::plot_grid(p1.1,p1.2,labels=c("A","B"),label_size=30)

cowplot::plot_grid(p1.2,p2.1,p2.2,labels=c("A","B","C"),ncol=3,label_size=30)

### demographic

p3.1=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld5)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(col=str_to_title(col)) %>%
  mutate(col=case_when(
    str_detect(tolower(col),"insurance")~"Insurance",
    str_detect(tolower(col),"gender")~"Gender",
    str_detect(tolower(col),"race and ethnicity|hispanic")~"Race/Ethnicity",
    str_detect(tolower(col),"education")~"Education",
    str_detect(tolower(col),"employment")~"Employment",
    str_detect(tolower(col),"income")~"Income",
    str_detect(tolower(col),"ethnic group")~"Ethnicity",
    str_detect(tolower(col),"sex")~"Sex",
    str_detect(tolower(col),"profession|occupation")~"Occupation",
    .default=col
  )) %>% 
  group_by(col) %>% tally %>% arrange(desc(n)) %>% #clipr::write_clip()
  filter(n>=3,col!="Not Extracted") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Types of demographic variables')+
  theme(text=element_text(size=24))+
  coord_flip()


#### SDOH
p3.2=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld6)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(col=case_when(
    str_detect(tolower(col),"rural")~"URBAN/RURAL",
    str_detect(tolower(col),"deprivation")~"DEPRIVATION INDEX",
    str_detect(tolower(col),"insur")~"INSURANCE",
    str_detect(tolower(col),"opioid use|substance abuse|drug abuse|addiction|smoking|opioid misuse|substance use|overdose|alcohol|substance misuse")~"SUBSTANCE USE",
    str_detect(tolower(col),"partner violence")~"DOMESTIC VIOLENCE",
    str_detect(tolower(col),"poverty")~"POVERTY",
    str_detect(tolower(col),"service in iraq")~"MILITARY SERVICE",
    str_detect(tolower(col),"body mass|admission|charlseon|prior mental")~"PRIOR ILLNESS",
    #str_detect(tolower(col),"mental health|mental illness|mental disorders")~"MENTAL HEALTH (UNSPECIFIED)",
    #str_detect(tolower(col),"anorexia")~"EATING DISORDERS",
    #str_detect(tolower(col),"distress")~"STRESS",
    #str_detect(tolower(col),"cancer")~"CANCER-RELATED",
    .default=col
  )) %>% 
  group_by(col) %>% tally %>% arrange(desc(n)) %>% #clipr::write_clip()
  mutate(col=str_to_title(col)) %>%
  filter(n>=2,col!="Not Extracted") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('SDOH reported')+
  theme(text=element_text(size=24))+
  coord_flip()

cowplot::plot_grid(p3.1,p3.2,labels=c("A","B"),ncol=2,label_size=30)

#### name of dataset
p2.3=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld7)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(col=case_when(
    str_detect(tolower(col),"twitter dataset|twitter data|^twitter$")~"TWITTER DATA",
    str_detect(tolower(col),"reddit dataset|reddit posts|reddit data")~"REDDIT",
    str_detect(tolower(col),"medical records")~"ELECTRONIC HEALTH RECORDS",
    str_detect(tolower(col),"kaggle")~"KAGGLE DATASET",
    str_detect(tolower(col),"poverty")~"POVERTY",
    str_detect(tolower(col),"service in iraq")~"MILITARY SERVICE",
    str_detect(tolower(col),"body mass|admission|charlseon|prior mental")~"PRIOR ILLNESS",
    #str_detect(tolower(col),"mental health|mental illness|mental disorders")~"MENTAL HEALTH (UNSPECIFIED)",
    #str_detect(tolower(col),"anorexia")~"EATING DISORDERS",
    #str_detect(tolower(col),"distress")~"STRESS",
    #str_detect(tolower(col),"cancer")~"CANCER-RELATED",
    .default=col
  )) %>% 
  mutate(col=case_when(
    col == "ADRESS CHALLENGE DATASET"|col == "ADRESS DATASET" ~ "ADReSS Challenge Dataset",
    col == "CEGS N-GRID 2016 SHARED TASK DATASET" ~ "CEGS N-GRID 2016 Shared Task Dataset",
    col == "CHILDBIRTH NARRATIVES" ~ "Childbirth Narratives",
    col == "CLINICAL NOTES" ~ "Clinical Notes",
    col %in% c("CLINICAL RECORD INTERACTIVE SEARCH CRIS", 
               "CLINICAL RECORDS INTERACTIVE SEARCH CRIS", 
               "CRIS CLINICAL RECORD INTERACTIVE SEARCH") ~ "Clinical Record Interactive Search (CRIS)",
    col %in% c("COVID-19 TWEETS DATASET", 
               "COVID-19 TWITTER CHATTER DATASET") ~ "COVID-19 Twitter Dataset",
    col == "DAIC-WOZ" ~ "DAIC-WOZ",
    col == "DREADDIT" ~ "Dreaddit",
    col == "ELECTRONIC HEALTH RECORDS" ~ "Electronic Health Records (EHR)",
    col == "KAGGLE DATASET" ~ "Kaggle Dataset",
    col == "MIMIC-III" ~ "MIMIC-III",
    col == "NATIONAL VIOLENT DEATH REPORTING SYSTEM NVDRS" ~ "National Violent Death Reporting System (NVDRS)",
    col == "PARTNERS HEALTHCARE RESEARCH PATIENT DATA REGISTRY" ~ "Partners Healthcare Research Patient Data Registry",
    col == "PUBMED ABSTRACTS" ~ "PubMed Abstracts",
    col == "REDDIT" ~ "Reddit",
    col == "SINA WEIBO" ~ "Sina Weibo",
    col == "SLAM BRC CASE REGISTER" ~ "SLAM BRC Case Register",
    col == "TWITTER DATA" ~ "Twitter Data",
    TRUE ~ col
  )) %>% 
  group_by(col) %>% tally %>% # clipr::write_clip()
  filter(n>=3,col!="N/A") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Most common datasets')+
  theme(text=element_text(size=20))+coord_flip()


#### type of dataset
p2.2=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld8)) %>% 
  pull(fld2)  %>% #tolower %>% 
  gsub("OTHER","",.) %>% 
  gsub("\\[","",.) %>% 
  gsub("\\]","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  filter(col!="") %>% 
  mutate(col=case_when(
    col=="CLINICAL NOTES"~"Clinical data",
    # Counseling Data
    col %in% c("COUNSELING SESSION NOTES", "THERAPY SESSION NOTES", "INTERVIEW TRANSCRIPTS", "TEXT TRANSCRIPTS FROM INTERVIEWS", "TRANSCRIBED INTERVIEWS","INTERVIEW DATA") ~ "Counseling Data",
    
    # Social Media Data
    str_detect(col,"SOCIAL MEDIA DATA")|col %in% c("SOCIAL MEDIA","SOCIAL MEDIA PLATFORMS","SOCIAL MEDIA COMMENTS") ~ "Social Media",
    
    # Forum Data
    col %in% c("ONLINE FORUM", "REDDIT MESSAGES", "ANONYMOUS ONLINE MARKETPLACES AND FORUMS") ~ "Online forums",
    
    
    # YouTube Data
    col %in% c("YOUTUBE VIDEOS") ~ "YouTube Data",
    
    str_detect(col,"VIDEO|AUDIO") ~ "Audio and Video Data",
    
    # Blogs and Online Articles
    col %in% c("BLOGS", "BLOG DATASET", "ONLINE NEWS ARTICLES", "NEWS REPORTS", "NEWSPAPER ARTICLES") ~ "Blogs and Online Articles",
    
    # Websites and Online Platforms
    col %in% c("WEBSITES", "ONLINE COMMUNITY", "ONLINE HEALTH COMMUNITY", "ONLINE PLATFORM", "ONLINE CONSULTATION PLATFORM", "ONLINE SOURCES") ~ "Websites and Online Platforms",
    
    # Academic and Research Data
    col %in% c("MEDICAL ARTICLES", "ACADEMIC TEXTS","JOURNAL ARTICLES", "SCIENTIFIC LITERATURE", "ACADEMIC ARTICLES", "BIBLIOMETRIC ANALYSIS", "BIBLIOMETRIC ANALYSIS AND TOPIC MODELING", "BIBLIOGRAPHIC RECORDS AND FULL-TEXT ARTICLES", "SCIENTIFIC CORPUS", "NATIONAL RESEARCH DATABASE") ~ "Articles and academic texts",
    
    # Mobile and Digital Health Data
    col %in% c("SMARTPHONE DATA", "MOBILE APPLICATION", "MOBILE APP", "SMARTPHONE APP", "MOBILE APP USAGE LOGS", "MOBILE APP DATA", "SMARTPHONE APP DATA", "DIGITAL MEMORY NOTEBOOK APP", "MHEALTH APPLICATION", "MOBILE HEALTH INTERVENTION") ~ "Mobile and Digital Health Data",
    
    # Survey Data
    str_detect(col,"QUESTIONNAIRE")|col %in% c("SURVEY DATA","WEB SURVEY", "SURVEY RESPONSES", "ONLINE SURVEY", "FREE-RESPONSE SURVEY DATA", "OPEN-ENDED QUESTIONNAIRE RESPONSES", "OPEN-ENDED RESPONSES", "OPEN-ENDED SURVEY RESPONSES", "QUESTIONNAIRE", "NATIONAL SURVEY") ~ "Survey Data",
    
    # Chatbot and AI Interaction Data
    col %in% c("CHATBOT INTERACTION", "CHATBOT INTERACTIONS", "CHATBOT CONVERSATION DATA", "CHATBOT CONVERSATION DATASET", "AI CHATBOT", "AI CONVERSATIONAL AGENTS", "DATASET FOR CHATBOT") ~ "Chatbot and AI Interaction Data",
    
    # Diary and Personal Account Data
    col %in% c("DIARY ENTRIES", "DIARIES") ~ "Diary and Personal Account Data",
    
    # Synthetic Data
    col %in% c("SYNTHETIC NOTES", "SYNTHETIC PATIENT DATA", "SYNTHETIC DATASET") ~ "Synthetic Data",
    str_detect(col,"FOCUS GROUP")~"Focus groups",
    str_detect(col,"PHONE CALL|TEXT MESSAG|PRIVATE ELECTRONIC|APP")~"Mobile and Digital Health Data",
    TRUE ~ "Other",
  )) %>% #pull(col) %>% unique %>% clipr::write_clip()
  group_by(col) %>% tally %>% # clipr::write_clip()
  filter(n>=1) %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Type of dataset')+
  theme(text=element_text(size=20))+coord_flip()


#cowplot::plot_grid(p2.1,p2.2,p2.3,labels=c("A","B","C"),ncol=3,label_size=30)

#### Extracted variables 
p3=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld9)) %>% 
  pull(fld2)  %>% #tolower %>% 
  gsub("OTHER","",.) %>% 
  gsub("\\[","",.) %>% 
  gsub("\\]","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  filter(col!="",col!="NA") %>% #pull(col) %>% unique %>% clipr::write_clip()
  mutate(col=case_when(
    # Suicide and Mental Health
    col %in% c("SUICIDE", "SUICIDAL IDEATION", "SUICIDE PROPENSITY SCORES", "SUICIDALITY DOCUMENTATION", "SUICIDE RISK FACTORS", "SUICIDAL THOUGHTS", "SUICIDAL BEHAVIOR", "SUICIDALITY-RELATED MENTIONS", "SUICIDE ATTEMPTS", "RISK OF SUICIDE ATTEMPT", "FREQUENCY OF SUICIDE KEYWORDS", "TWEETS WITH SUICIDAL IDEATION", "COMMENTS AND POSTS ABOUT SUICIDAL IDEATION", "MENTIONS OF SUICIDALITY") ~ "Suicide and Mental Health",
    
    # Emotions and Sentiments
    col %in% c("EMOTIONS", "EMOTIONAL STATES", "EMOTIONAL DYNAMICS", "EMOTIONAL TONE", "EMOTIONAL EXPRESSIONS", "EMOTIONAL VALENCE", "EMOTIONAL CONTENT", "EMOTIONAL FEATURES", "EMOTIONS DETECTION", "SENTIMENT LABELS", "SENTIMENT ANALYSIS", "POSITIVE AND NEGATIVE SENTIMENT SCORES", "SENTIMENT SCORES", "SENTIMENT POLARITY", "EMOTIONAL POLARITY", "EMOTIONAL CATEGORIES", "SENTIMENT CHANGE", "EXPRESSED SENTIMENT INDICES", "EMOTION LABELS", "NEGATIVE EMOTION", "POSITIVE EMOTION") ~ "Emotions and Sentiments",
    
    # Personality and Psychological Traits
    col %in% c("PERSONALITY TRAITS", "PERSONALITY TYPES", "PERSONALITY DIMENSIONS", "BIG FIVE PERSONALITY TRAITS", "PSYCHOLOGICAL TRAITS", "NEUROTICISM", "EXTRAVERSION", "OPENNESS TO EXPERIENCE", "AGREEABLENESS", "CONSCIENTIOUSNESS", "PSYCHOLOGICAL STATES", "PERSONALITY SCORES") ~ "Personality and Psychological Traits",
    
    # Clinical and Diagnostic Assessments
    col %in% c("PHQ-8 SCORE", "PHQ-9 SCORES", "GAD-7 SCORES", "C-SSRS SCORES", "PSYCHOMETRIC SCORES", "DIAGNOSTIC REPORTS", "CLINICAL ASSESSMENTS", "DIAGNOSIS CODES", "CLINICAL OUTCOMES", "DIAGNOSTIC TOOLS", "DIAGNOSTIC STATEMENTS", "DIAGNOSTIC CRITERIA EXPRESSIONS", "PSYCHIATRIC SYMPTOMS", "SYMPTOM SEVERITY", "CLINICAL VARIABLES", "DEPRESSION SEVERITY", "SYMPTOMS", "DIAGNOSTIC OUTCOMES", "PSYCHIATRIC CONSULTATION") ~ "Clinical and Diagnostic Assessments",
    
    # Mental Health Conditions and Disorders
    col %in% c("DEPRESSION", "DEPRESSIVE MOODS", "ANXIETY", "PTSD", "DEMENTIA", "BIPOLAR DISORDER", "SCHIZOPHRENIA", "ADHD", "SUBSTANCE ABUSE", "ALCOHOLISM", "EATING DISORDERS", "MENTAL HEALTH CONDITIONS", "PSYCHIATRIC CONDITIONS", "MENTAL DISORDERS", "DISORDERS", "PSYCHOLOGICAL CONDITIONS", "SYMPTOMS OF DEPRESSION", "SYMPTOMS OF MANIA", "CO-OCCURRING DISORDERS") ~ "Mental Health Conditions and Disorders",
    
    # Violence, Abuse, and Trauma
    col %in% c("ABUSIVE RELATIONSHIPS", "BULLYING", "SEXUAL VIOLENCE", "EMOTIONAL VIOLENCE", "PHYSICAL VIOLENCE", "TYPES OF VIOLENCE", "GENDER VIOLENCE", "DOMESTIC VIOLENCE", "IPV", "TRAUMA", "ABUSE", "CYBERBULLYING", "HOSTILITY", "VIOLENCE") ~ "Violence, Abuse, and Trauma",
    
    # Social Media and Online Behavior
    col %in% c("TWEETS", "REDITS", "SOCIAL MEDIA POSTS", "USER POSTS", "OFFENSIVE AND NON-OFFENSIVE TWEETS", "TWEETS LABELED AS DEPRESSION POSITIVE OR NEGATIVE", "TWEETS INDICATING DEPRESSIVE BEHAVIOR", "SOCIAL MEDIA", "USER BEHAVIOR", "SOCIAL INFORMATION", "SOCIAL CONNECTIONS", "SOCIAL ENGAGEMENT", "ONLINE BEHAVIOR") ~ "Social Media and Online Behavior",
    
    # Textual and Linguistic Analysis
    col %in% c("LINGUISTIC FEATURES", "TEXTUAL FEATURES", "TEXT POSTS", "LINGUISTIC CHARACTERISTICS", "LINGUISTIC PATTERNS", "PSYCHOLINGUISTIC FEATURES", "TEXT INFORMATION", "TEXT DATA", "TEXTUAL ANALYSIS", "TEXTUAL SAMPLES", "N-GRAMS", "LIWC", "POS TAGS", "Q-GRAMS", "WORD EMBEDDINGS", "TOPIC MODELING", "LEXICAL FEATURES", "TEXT LENGTH", "LANGUAGE FEATURES", "TEXT SAMPLES", "SENTENCES") ~ "Textual and Linguistic Analysis",
    
    # Communication and Interaction Data
    col %in% c("CONVERSATION DATA", "DIALOGUE SAFETY LABELS", "MODEL RESPONSES", "COMMUNICATION PATTERNS", "INTERACTION PATTERNS", "DIALOGUE TURNS", "CONVERSATION OUTCOMES", "USER INTERACTIONS", "USER RESPONSES", "CONVERSATION HISTORY") ~ "Communication and Interaction Data",
    
    # Health and Physiological Data
    col %in% c("HEART RATE DATA", "PHYSIOLOGICAL DATA", "CARDIOVASCULAR INFORMATION", "MEDICAL DATA", "HEALTHCARE TEXT DATA", "HEALTH HISTORY", "HEALTH CONDITIONS", "LAB RESULTS", "MEDICAL RECORDS", "EHR DATA", "PHYSIOLOGICAL FEATURES") ~ "Health and Physiological Data",
    
    # Cognitive and Analytical Processes
    col %in% c("ANALYTICAL THINKING", "COGNITIVE PROCESSES", "COGNITIVE BEHAVIORAL PATTERNS", "PSYCHOLOGICAL PROCESSES", "COGNITIVE STATES", "COGNITIVE TEST RESULTS", "COGNITIVE FUNCTION", "COGNITIVE STATUS") ~ "Cognitive and Analytical Processes",
    
    # Demographics and User Information
    col %in% c("USER ID", "USER DEMOGRAPHICS", "DEMOGRAPHIC INFORMATION", "PATIENT DEMOGRAPHICS", "SOCIODEMOGRAPHIC DATA", "DEMOGRAPHIC VARIABLES", "AGE", "GENDER", "USER PROFILES", "USER INFORMATION", "DEMOGRAPHICS") ~ "Demographics and User Information",
    
    # Treatment and Intervention Data
    col %in% c("MEDICATIONS", "MEDICATION", "TREATMENTS", "TREATMENT HISTORY", "TREATMENT OUTCOMES", "THERAPY SESSION NOTES", "MEDICATION FEATURES", "CLINICAL INTERVENTIONS", "THERAPEUTIC OUTCOMES", "TREATMENT RESPONSE", "INTERVENTIONS", "THERAPY") ~ "Treatment and Intervention Data",
    
    # Violence and Abuse Indicators
    col %in% c("ABUSIVE TEXT MESSAGE", "BULLYING TRACES", "VIOLENCE INDICATORS", "CYBERBULLYING", "ABUSIVE VS NON-ABUSIVE CLASSIFICATION", "VIOLENCE CLASSIFICATION", "GENDER VIOLENCE CLASSIFICATION", "HOSTILITY") ~ "Violence and Abuse Indicators",
    
    # Sentiment Labels and Emotion Detection
    col %in% c("SENTIMENT LABELS POSITIVE", "NEGATIVE", "NEUTRAL", "SENTIMENT LABELS", "SENTIMENT SCORES", "SENTIMENT RATE", "EMOTION LABELS DEPRESSION", "INSOMNIA", "ANGER", "FEAR", "HATE", "HOPE", "JOY", "LOVE", "SADNESS", "SURPRISE", "ANXIETY LEVEL", "ANXIETY DETECTION", "NEGATIVE EMOTIONS", "POSITIVE EMOTIONS") ~ "Sentiment Labels and Emotion Detection",
    
    # Contextual and Content Features
    col %in% c("CONTENT FEATURES", "CONTEXT FEATURES", "MESSAGE CONTENT", "POST CONTENT", "CONTENT OF POSTS", "DESCRIPTION", "TITLE", "CONTENT CHARACTERISTICS", "POST TIME", "POST TITLE", "BODY CONTENT", "TEXT CONTENT", "EMOTIONAL CONTENT") ~ "Contextual and Content Features",
    
    # Linguistic Style and Structure
    col %in% c("LINGUISTIC STYLE", "LINGUISTIC CHARACTERISTICS", "SENTENCE STRUCTURES", "SYNTAX", "GRAMMAR", "TEXT STRUCTURE", "LANGUAGE USE", "SYNTAX ANALYSIS", "LINGUISTIC SYNTAX") ~ "Linguistic Style and Structure",
    
    # Safety and Risk Categories
    col %in% c("SAFETY CATEGORIES", "RISK FACTORS", "RISK ASSESSMENT", "RISK CASES", "RISK CLASSIFICATION", "RISK SCORES", "SAFETY INDICATORS", "RISK OF SUICIDE", "SAFETY LABELS") ~ "Safety and Risk Categories",
    col %in% c("EMOTIONAL RESPONSES", "EMOTIONAL STATES", "EMOTIONAL REACTIONS", "EMOTIONAL EXPRESSIONS", "EMOTIONAL BIAS", "EMOTIONAL INTENSITY", "EMOTIONAL RESPONSES", "EMOTIONAL WORDS") ~ "Emotional States and Responses",
    col %in% c("SOCIAL BEHAVIOR", "USER BEHAVIOR", "SOCIAL INTERACTIONS", "SOCIAL ENGAGEMENT", "BEHAVIORAL DATA", "USER BEHAVIOR FEATURES", "SOCIAL FUNCTION", "SOCIAL WORDS", "BEHAVIORAL PATTERNS") ~ "Social and Behavioral Data",
    #Psychological Symptoms and Indicators
    col %in% c("PSYCHOLOGICAL SYMPTOMS", "PSYCHIATRIC SYMPTOMS", "PSYCHOLOGICAL CONDITIONS", "MENTAL HEALTH SYMPTOMS", "SYMPTOMS SEVERITY", "SYMPTOM PROFILES", "DEPRESSION SYMPTOMS", "SYMPTOM INDICATORS") ~ "Psychological Symptoms and Indicators",
    #Survey and Response Data
    col %in% c("SURVEY RESPONSES", "SURVEY DATA", "OPEN-ENDED RESPONSES", "QUESTIONNAIRE", "INTERVIEW DATA", "QUESTIONNAIRE DATA", "QUESTION-ANSWER PAIRS", "SURVEY RESULTS", "SELF-REPORT SCALES") ~ "Survey and Response Data",
    #Support Systems and Resources
    
    col %in% c("SUPPORT SYSTEMS", "SOCIAL SUPPORT", "MENTAL HEALTH SUPPORT", "THERAPIST SUPPORT", "SUPPORTIVE TEXT MESSAGES", "EMOTIONAL SUPPORT", "INFORMATIONAL SUPPORT") ~ "Support Systems and Resources",
    
    #Sentiment and Emotion Classification
    
    col %in% c("SENTIMENT CLASSIFICATION", "EMOTION CLASSIFICATION", "SENTIMENT LABEL", "EMOTION DETECTION", "SENTIMENT ANALYSIS", "EMOTION DETECTION", "SENTIMENT RATINGS", "EMOTION LABELS") ~ "Sentiment and Emotion Classification",
    
    #Medical and Health Data
    
    col %in% c("MEDICAL HISTORY", "PATIENT HISTORY", "HEALTH DATA", "MEDICAL RECORDS", "MEDICATION RECORDS", "CLINICAL DOCUMENTATION", "HEALTH CONDITIONS", "MEDICAL CONDITIONS", "CLINICAL NOTES", "EHR DATA") ~ "Medical and Health Data",
    
    #Cognitive and Mental Functioning
    
    col %in% c("MENTAL FUNCTIONING", "COGNITIVE PROCESSES", "PSYCHOLOGICAL PROCESSES", "COGNITIVE TEST RESULTS", "COGNITIVE STATES", "COGNITIVE FUNCTION", "MENTAL STATES") ~ "Cognitive and Mental Functioning",
    
    #Behavioral and Psychological Analysis
    
    col %in% c("BEHAVIORAL ANALYSIS", "PSYCHOLOGICAL ANALYSIS", "PSYCHOLOGICAL PROCESSES", "BEHAVIORAL FEATURES", "BEHAVIORAL DATA", "PSYCHOLOGICAL STATES", "PSYCHOLOGICAL CONDITIONS") ~ "Behavioral and Psychological Analysis",
    
    #Demographic and Social Information
    
    col %in% c("DEMOGRAPHIC INFORMATION", "SOCIODEMOGRAPHIC DATA", "AGE", "GENDER", "USER INFORMATION", "USER DEMOGRAPHICS", "DEMOGRAPHIC VARIABLES", "PATIENT DEMOGRAPHICS") ~ "Demographic and Social Information",
    
    #Language and Linguistic Features
    
    col %in% c("LINGUISTIC FEATURES", "TEXTUAL FEATURES", "LEXICAL FEATURES", "LANGUAGE USE", "LINGUISTIC PATTERNS", "TEXTUAL ANALYSIS", "TEXT LENGTH", "SYNTACTIC FEATURES") ~ "Language and Linguistic Features",
    
    #Survey and Questionnaire Responses
    
    col %in% c("SURVEY RESPONSES", "QUESTIONNAIRE", "INTERVIEW DATA", "OPEN-ENDED RESPONSES", "QUESTION-ANSWER PAIRS", "SELF-REPORT SCALES", "QUESTIONNAIRE DATA") ~ "Survey and Questionnaire Responses",
    
    #User and Social Engagement
    
    col %in% c("USER ENGAGEMENT", "SOCIAL INTERACTIONS", "USER BEHAVIOR", "SOCIAL MEDIA POSTS", "SOCIAL BEHAVIOR", "USER POSTS", "SOCIAL CONNECTIONS") ~ "User and Social Engagement",
    
    #Sentiment and Emotion Detection
    
    col %in% c("SENTIMENT DETECTION", "EMOTION DETECTION", "SENTIMENT ANALYSIS", "SENTIMENT RATINGS", "EMOTION CLASSIFICATION", "SENTIMENT LABELS", "EMOTION LABELS") ~ "Sentiment and Emotion Detection",
    
    #Clinical Assessments and Scores
    
    col %in% c("CLINICAL ASSESSMENTS", "PSYCHOMETRIC SCORES", "DIAGNOSTIC TOOLS", "SYMPTOM SCORES", "SYMPTOM SEVERITY", "CLINICAL OUTCOMES", "DIAGNOSTIC REPORTS") ~ "Clinical Assessments and Scores",
    
    #Support Systems and Networks
    
    col %in% c("SUPPORT SYSTEMS", "SOCIAL SUPPORT", "MENTAL HEALTH SUPPORT", "THERAPIST SUPPORT", "SUPPORTIVE TEXT MESSAGES", "EMOTIONAL SUPPORT") ~ "Support Systems and Networks",
    
    #Contextual Information and Metadata
    
    col %in% c("CONTEXT INFORMATION", "METADATA", "USER ID", "POST TIME", "CONTEXT FEATURES", "MESSAGE CONTENT", "CONVERSATION DATA", "CONTEXTUAL FEATURES") ~ "Contextual Information and Metadata",
    
    #Audio, Visual, and Multimedia Data
    
    col %in% c("AUDIO FEATURES", "VISUAL FEATURES", "MULTIMEDIA DATA", "SPEECH FEATURES", "FACIAL EXPRESSIONS", "AUDIO-VISUAL DATA", "MULTIMEDIA RECORDS") ~ "Audio, Visual, and Multimedia Data",
    
    #Survey Responses and Feedback
    
    col %in% c("SURVEY RESPONSES", "FEEDBACK", "INTERVIEW DATA", "OPEN-ENDED RESPONSES", "QUESTIONNAIRE DATA", "SURVEY DATA", "SELF-REPORTS") ~ "Survey Responses and Feedback",
    
    #User Behavior and Interaction Data
    
    col %in% c("USER BEHAVIOR", "INTERACTION DATA", "USER INTERACTIONS", "BEHAVIORAL DATA", "SOCIAL BEHAVIOR", "CONVERSATION OUTCOMES") ~ "User Behavior and Interaction Data",
    
    #Medical and Health Records
    
    col %in% c("MEDICAL RECORDS", "HEALTH RECORDS", "CLINICAL DOCUMENTATION", "PATIENT HISTORY", "MEDICAL DATA", "HEALTH INFORMATION", "HEALTH HISTORY") ~ "Medical and Health Records",
    
    #Cognitive Processes and States
    
    col %in% c("COGNITIVE PROCESSES", "MENTAL STATES", "PSYCHOLOGICAL STATES", "COGNITIVE FUNCTION", "COGNITIVE BEHAVIORAL PATTERNS", "COGNITIVE TEST RESULTS") ~ "Cognitive Processes and States",
    
    #Behavioral Analysis and Features
    
    col %in% c("BEHAVIORAL FEATURES", "BEHAVIORAL ANALYSIS", "USER BEHAVIOR", "SOCIAL BEHAVIOR", "INTERACTION PATTERNS", "BEHAVIORAL DATA") ~ "Behavioral Analysis and Features",
    
    #Psychological Conditions and Symptoms
    
    col %in% c("PSYCHOLOGICAL CONDITIONS", "PSYCHOLOGICAL SYMPTOMS", "MENTAL HEALTH CONDITIONS", "PSYCHIATRIC SYMPTOMS", "SYMPTOMS", "PSYCHOLOGICAL STATES", "MENTAL DISORDERS") ~ "Psychological Conditions and Symptoms",
    
    #Demographic and Social Characteristics
    
    col %in% c("DEMOGRAPHIC CHARACTERISTICS", "SOCIAL CHARACTERISTICS", "USER DEMOGRAPHICS", "SOCIODEMOGRAPHIC DATA", "AGE", "GENDER", "PATIENT DEMOGRAPHICS") ~ "Demographic and Social Characteristics",
    
    #Contextual and Linguistic Features
    
    col %in% c("CONTEXTUAL FEATURES", "LINGUISTIC FEATURES", "TEXTUAL FEATURES", "LANGUAGE USE", "SENTENCE STRUCTURES", "TEXTUAL ANALYSIS") ~ "Contextual and Linguistic Features",
    
    #Sentiment and Emotion Labels
    
    col %in% c("SENTIMENT LABELS", "EMOTION LABELS", "SENTIMENT CLASSIFICATION", "SENTIMENT RATINGS", "EMOTION CLASSIFICATION", "EMOTIONAL LABELS") ~ "Sentiment and Emotion Labels",
    
    #Clinical Assessments and Reports
    
    col %in% c("CLINICAL ASSESSMENTS", "DIAGNOSTIC REPORTS", "SYMPTOM SCORES", "CLINICAL OUTCOMES", "DIAGNOSTIC TOOLS", "PSYCHOMETRIC SCORES") ~ "Clinical Assessments and Reports",
    
    #Multimedia and Visual Data
    
    col %in% c("MULTIMEDIA DATA", "VISUAL DATA", "AUDIO DATA", "FACIAL EXPRESSIONS", "SPEECH FEATURES", "AUDIO-VISUAL DATA") ~ "Multimedia and Visual Data",
    
    #Contextual Metadata and Information
    
    col %in% c("CONTEXTUAL INFORMATION", "METADATA", "USER ID", "POST TIME", "CONTEXT FEATURES", "CONVERSATION DATA") ~ "Contextual Metadata and Information",
    
    #User Behavior and Social Interaction
    
    col %in% c("USER BEHAVIOR", "SOCIAL INTERACTION", "USER INTERACTIONS", "SOCIAL ENGAGEMENT", "BEHAVIORAL DATA", "INTERACTION PATTERNS") ~ "User Behavior and Social Interaction",
    
    #Psychological and Cognitive Functioning
    
    col %in% c("PSYCHOLOGICAL FUNCTIONING", "COGNITIVE FUNCTIONING", "MENTAL STATES", "PSYCHOLOGICAL STATES", "COGNITIVE PROCESSES", "COGNITIVE TEST RESULTS") ~ "Psychological and Cognitive Functioning",
    
    TRUE ~ "Uncategorized"
  )) %>% 
  #mutate(col=str_to_title(col)) %>%
  group_by(col) %>% tally %>% #arrange(desc(n)) %>% clipr::write_clip()
  #arrange(desc(n)) %>% head # clipr::write_clip()
  filter(n>=1,col!="N/A") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Extracted information/variables')+
  theme(text=element_text(size=20))+
  coord_flip()

p1=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld10)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% #pull(col) %>% table
  mutate(col=case_when(
    str_detect(tolower(col),"yes")~"Yes",
    str_detect(tolower(col),"not extracted")~"No",
    str_detect(tolower(col),"no")~"No",
    .default="Unclear"
  )) %>% 
  
  #mutate(col=str_to_title(col)) %>%
  group_by(col) %>% tally %>% arrange(desc(n)) %>% # clipr::write_clip()
  filter(n>=1) %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Access terms to dataset provided')+
  theme(text=element_text(size=24))

p2=
  df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld11)) %>% 
  pull(fld2)  %>% #tolower %>%
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  snakecase::to_title_case() %>% 
  as.data.frame %>% rename(col=1) %>% 
  mutate(col=case_when(
    #  str_detect(tolower(col),"yes")~"Yes",
    #  str_detect(tolower(col),"no")~"No",
    str_detect(tolower(col),"not extracted")~"Unclear",
    .default=col,
  )) %>% 
  #mutate(col=str_to_title(col)) %>%
  group_by(col) %>% tally %>% arrange(desc(n)) %>% # clipr::write_clip()
  filter(n>=1,col!="N a") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Access level to dataset')+
  theme(text=element_text(size=24))


p3=df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld12)) %>% 
  pull(fld2)  %>% #tolower %>% 
  #gsub("other","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  #mutate(col=case_when(
  #  str_detect(tolower(col),"yes")~"Yes",
  #  str_detect(tolower(col),"no")~"No",
  #  .default="Unclear"
  #)) %>% 
  #mutate(col=str_to_title(col)) %>%
  mutate(col=case_when(
    col %in% c("N/A", "NOT GIVEN") ~ "No Information Provided",
    
    col %in% c("SIGNING USE AGREEMENT", 
               "SIGNING A DATA USAGE AGREEMENT WITH THE INSTITUTION", 
               "SIGNING A DATA USAGE AGREEMENT", 
               "DATA USE AGREEMENT AND IRB APPROVAL", 
               "DATA USAGE AGREEMENT WITH THE HEALTH SYSTEM") ~ "Signing Data Usage Agreement",
    
    col %in% c("JOINING THE DEMENTIABANK CONSORTIUM GROUP", 
               "BECOMING A MEMBER OF DEMENTIABANK", 
               "BECOME A MEMBER OF DEMENTIABANK") ~ "Membership Requirement",
    
    col %in% c("CONTACT BRIAN MACWHINNEY", 
               "EMAILING THE AUTHOR", 
               "REQUEST VIA EMAIL TO THE AUTHOR", 
               "CONTACT THE FORUM OWNER TO PRESENT THE PROJECT AND OBTAIN AGREEMENT") ~ "Contact Author or Owner",
    
    col %in% c("IRB APPROVAL", 
               "APPLICATION TO, AND APPROVAL BY, THE CHIEF MEDICAL OFFICER OF PSYCHIATRY", 
               "RESEARCH PROJECT PROPOSAL SUBMISSION", 
               "REQUEST SUBMISSION", 
               "APPLYING FROM THE I2B2 DATA PORTAL", 
               "DETERMINED BY THE SUITABILITY OF SHARING BASED ON INTENDED USE") ~ "Application or Proposal Submission",
    
    col %in% c("API ACCESS", 
               "RESEARCHER MUST USE THE PUBLIC STREAMING API OR PYTHON LIBRARY SNSCRAPE") ~ "API Access",
    
    col %in% c("IT IS FREE UNDER A CREATIVE COMMONS LICENSE") ~ "Creative Commons License",
    
    col %in% c("COMPLETION OF SPECIFIC TRAININGS AND OBTAINING A CERTIFICATE", 
               "TRAINING") ~ "Completion of Training",
    
    col %in% c("RESTRICTED TO HONORARY OR SUBSTANTIVE EMPLOYEES OF SLAM") ~ "Restricted Access",
    
    TRUE ~ "Other"
  )) %>% 
  group_by(col) %>% tally %>% # clipr::write_clip()
  filter(n>=1,col!="N/A") %>% #pull(col) %>% unique %>% clipr::write_clip()
  ggplot()+
  geom_col(aes(x=reorder(col,n),y=n))+
  xlab('Requirements to access dataset')+
  theme(text=element_text(size=24))+coord_flip()

cowplot::plot_grid(p1,p2,labels=c("A","B"),ncol=2,label_size=30)


cowplot::plot_grid(p1,p2,p3,labels=c("A","B","C"),label_size=30)




#### Extracted variables 

all.vars=df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld7)) %>% 
  pull(fld2)  %>% #tolower %>% 
  gsub("OTHER","",.) %>% 
  gsub("\\[","",.) %>% 
  gsub("\\]","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  filter(col!="",col!="NA") %>% pull(col) 

all=df.3 %>% 
  mutate(fld2=gsub("[*()]","",fld9)) %>% 
  pull(fld2)  %>% #tolower %>% 
  gsub("OTHER","",.) %>% 
  gsub("\\[","",.) %>% 
  gsub("\\]","",.) %>% 
  str_split(',') %>% unlist %>% trimws %>% #str_to_title %>% 
  as.data.frame %>% rename(col=1) %>% 
  filter(col!="",col!="NA") %>% pull(col) 

library(wordcloud)
library(tm)
# Create a vector of words
words <- all.vars
# Convert vector to a text corpus
corpus <- Corpus(VectorSource(words))
# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term document matrix
tdm <- TermDocumentMatrix(corpus)
# Convert to a matrix and data frame
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)
# Generate the word cloud
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 2, 
          scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

all %>% dim

tb<-table(all)
wordcloud(names(tb),as.numeric(tb), scale=c(8,.3),min.freq=1,max.words=100, random.order=T, rot.per=.15, colors="black", vfont=c("sans serif","plain"))
wordcloud(names(tb),as.numeric(tb), min.freq = 3, 
          scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Convert vector to a text corpus
corpus <- Corpus(VectorSource(all))
# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation) # Optional
corpus <- tm_map(corpus, removeNumbers) # Optional
corpus <- tm_map(corpus, removeWords, stopwords("en")) # Optional

# Create a term document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert to a matrix and data frame
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the word cloud
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 4, 
          scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))




#### Generate citation table

meta=read.csv('/Users/scherbak/Documents/MUSC/NLP datasets review/review_453175_included_csv_20240821041854.csv')
df.3 %>% left_join(meta %>% 
                     rename(id=Covidence..)) %>% 
  rowwise %>% 
  mutate(ref=paste0(gsub(";",",",Authors),' ',Title,'. ',Journal,' ',Published.Year,';',Volume,if (str_length(Issue)>0) paste0('(',Issue,')'),if(str_length(DOI)>5) paste0(' doi: ',DOI,'.'))) %>% 
  select(matches('fld|ref'),-Ref,-fld4,-fld14) %>% 
  select(ref,everything()) %>%
  rowid_to_column("N") %>% 
  rename(Citatation=2,Country=3,"NLP Method"=4,"Outcome"=5,"Demographic variables"=6,'SDOH'=7,"Dataset name"=8,'Dataset type'=9,'What was extracted'=10,'Access to dataset described'=11,'Access level'=12,'How to get access'=13,'URL to dataset'=14) %>% 
  write.csv('Final.extraction.table.csv')




