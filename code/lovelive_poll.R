library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(utf8)
library(httr)
library(XML)
library(doSNOW)

#Data scrape Love Live! Wikia for song information
##Obtain URLs of group discographies
groups <- c("M%27s", "A-RISE", "Aqours", "Saint_Snow", "PERFECT_Dream_Project")
urls <- paste0("https://love-live.fandom.com/wiki/Category:", groups, "_Songs")
##Function to extract song wikia link
get_songs <- function(url) {
  req <- GET(url)
  stop_for_status(req)
  doc <- htmlParse(content(req))
  return(xpathSApply(doc, '//li[@class="category-page__member"]/a', fun=xmlGetAttr, name="href"))
}
##Parallel execution to retrieve all song links
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)
song_links <- foreach(i=urls, .combine=c, .inorder=FALSE, .packages=c("httr", "XML")) %dopar% {
  return(get_songs(i))
}
stopCluster(cl)
song_links <- paste0("https://love-live.fandom.com", song_links)
##Function to extract song name and artist(s)
get_song_info <- function(url) {
  req <- GET(url)
  stop_for_status(req)
  doc <- htmlParse(content(req))
  songs <- xpathSApply(doc, '//aside/h2[1]', fun=xmlValue)
  artists <- xpathSApply(doc, '//aside/section[1]/h2', fun=xmlValue)
  return(data.frame(songs, artists))
  }
##Parallel execution to retrieve all song names and artists
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)
song_df <- foreach(i=song_links, .combine=rbind, .inorder=FALSE, .packages=c("httr", "XML")) %dopar% {
  return(get_song_info(i))
}
stopCluster(cl)
##Clean song information

#Data manipulation
##Fixing artist names
song_df <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\lovelive\\data\\scrape_results\\lovelive_songinfo.rds")
song_df <- song_df %>%
  mutate(artists=gsub("^.+ by ", "", artists), songs=gsub(" \\(song\\)$", "", songs)) %>%
  mutate_all(as.character) %>%
  mutate(artists=utf8_normalize(artists, map_quote=TRUE)) %>%
  mutate(artists=str_trim(artists))
mu <- filter(song_df, songs=="Snow halation")[,"artists"]
##Some song-specific edits
###Changing Omoi yo Hitotsu ni Nare to Aqours
song_df[grepl("Omoi yo Hitotsu ni Nare", song_df$songs),"artists"] <- "Aqours"
###Changing Aishiteru Banzai! to Muse
song_df[grepl("Aishiteru Banzai!", song_df$songs),"artists"] <- mu
##Changing artists of small group songs into coarser category
members <- rbind(
  data.frame(
    character=c("Chika Takami", "Riko Sakurauchi", "Kanan Matsuura", "Dia Kurosawa", "You Watanabe", "Yoshiko Tsushima", "Hanamaru Kunikida", "Mari Ohara", "Ruby Kurosawa"),
    desc="Aqours small group"
  ),
  data.frame(
    character=c("Honoka Kosaka", "Kotori Minami", "Umi Sonoda", "Rin Hoshizora", "Hanayo Koizumi", "Maki Nishikino", "Eli Ayase", "Nozomi Tojo", "Nico Yazawa"),
    desc=paste(mu, "small group")
  ),
  data.frame(
    character=c("Setsuna Yuki", "Rina Tennoji", "Emma Verde", "Kanata Konoe", "Ai Miyashita", "Karin Asaka", "Kasumi Nakasu", "Shizuku Osaka", "Ayumu Uehara"),
    desc="PDP small group"
  )
) %>%
  mutate_all(as.character)
song_df <- song_df %>%
  mutate(multi=grepl("and|&|,", artists)) %>%
  mutate(first=ifelse(multi, str_match(artists, "^([[:alpha:]]+ [[:alpha:]]+)")[,2], NA)) %>%
  left_join(members, by=c("first"="character")) %>%
  mutate(artists=ifelse(multi, desc, artists)) %>%
  select(-multi, -first, -desc) %>%
  unique()

#Poll data cleaning
##Read in data
raw <- data.table::fread("C:\\Users\\Bearkey\\Documents\\Anime\\lovelive\\data\\lovelive_songpopularity2019.csv", data.table=FALSE) %>%
  filter_all(any_vars(.!=""))
vote_limit <- data.frame(question_no=1:ncol(raw),
                         limit=c(2, 2, 2, 2, 3, 3, 2,
                                 3, 5, 1, 1, 6, 1, 2,
                                 2, 2, 5, 2, 2, 2, 2,
                                 2, 1, 1, 6, 1, 3, 1))
##Function to deal with songs that have commas in their names and to tabulate votes
comma_songs <- filter(song_df, grepl(", ", songs))[,"songs"]
get_df <- function(i, type="tabulate") {
  vec <- raw[,i]
  if (type=="info") l <- length(vec[vec!=""])
  comma_df <- data.frame(song=character(0), votes=numeric(0))
  for (j in 1:length(comma_songs)) {
    hits <- str_count(vec, comma_songs[j]) %>% sum()
    if (hits==0) {
      next
    } else {
      comma_df <- rbind(comma_df, data.frame(song=comma_songs[j], votes=hits))
    }
  }
  if (nrow(comma_df)>0) {
    vec <- gsub(paste(comma_df[,"song"], collapse="|"), "", vec)
    vec <- gsub("^, |, ?$", "", vec)
  }
  vec <- str_split(vec, ", ") %>% unlist()
  vec <- vec[vec!=""]
  if (type=="info") {
    df <- data.frame(stat=c("Number of ballots: ", "Number of votes: ", "Vote limit: "),
               value=c(l, length(vec)+sum(comma_df[,"votes"]), vote_limit[i,"limit"]))
    colnames(df) <- NULL
    return(df)
  } else {
    data.frame(song=vec) %>%
      group_by(song) %>%
      summarise(votes=n()) %>%
      as.data.frame() %>%
      rbind(comma_df) %>%
      return()
  }
}
##Tabulate votes for all questions
questions <- names(raw)
q_type <- c(rep("song", 12), "seiyuu", rep("song", 12), "seiyuu", "song", "seiyuu")
vote_totals <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\lovelive\\data\\cleaned_results\\lovelive_pollresults_plotready.rds")
vote_totals <- lapply(1:length(questions), function(i) data.frame(get_df(i), question_no=i, q_type=q_type[i], question=questions[i])) %>%
  plyr::rbind.fill() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(song=ifelse(song=="Susume?Tomorrow", "Susume→Tomorrow", song)) %>%
  mutate(song=ifelse(song=="??HEARTBEAT", "？←HEARTBEAT", song)) %>%
  mutate(song=ifelse(song=="Mogyutto \"\"love\"\" de Sekkin Chuu!", "Mogyutto \"love\" de Sekkin Chuu!", song)) %>%
  mutate(song=ifelse(song=="Baby maybe Koi no Button", "baby maybe Koi no Button", song)) %>%
  mutate(song=ifelse(song=="Sweet&sweet holiday", "sweet&sweet holiday", song)) %>%
  mutate(song=ifelse(song=="Soldier game", "soldier game", song)) %>%
  mutate(song=ifelse(song=="After school NAVIGATORS", "after school NAVIGATORS", song)) %>%
  mutate(song=ifelse(song=="Niko puri?Joshi dou", "Niko puri♥Joshi dou", song)) %>%
  mutate(song=ifelse(song=="Blueberry?Train", "Blueberry♥Train", song)) %>%
  mutate(song=ifelse(song=="Yume?ONCE AGAIN", "Yume☆ONCE AGAIN", song)) %>%
  mutate(song=ifelse(song=="MY Mai?TONIGHT", "MY Mai☆TONIGHT", song)) %>%
  mutate(song=ifelse(song=="Thrilling?One Way", filter(song_df, grepl("Thrilling", songs))[,1], song)) %>%
  mutate(song=ifelse(song=="\"\"MY LIST\"\" to you!", "\"MY LIST\" to you!", song)) %>%
  mutate(song=ifelse(song=="Aqours?HEROES", "Aqours☆HEROES", song)) %>%
  mutate(song=ifelse(song=="Hop?Step?Waai!", filter(song_df, grepl("Waai", songs))[,1], song)) %>%
  mutate(song=ifelse(song=="Jimo Ai ? Mantan ? Summer Life", "Jimo Ai ♡ Mantan ☆ Summer Life", song)) %>%
  mutate(song=ifelse(song=="In this unstable world", "in this unstable world", song)) %>%
  mutate(song=ifelse(song=="Doki Pipo ? Emotion", "Doki Pipo ☆ Emotion", song)) %>%
  left_join(select(song_df, songs, artists), by=c("song"="songs")) %>%
  rename(Artist=artists) %>%
  mutate(Artist=enc2native(Artist))
vote_sums <- vote_totals %>%
  group_by(question_no) %>%
  summarise(total=sum(votes)) %>%
  as.data.frame()
vote_totals <- vote_totals %>%
  left_join(vote_sums, by="question_no") %>%
  mutate(prop=paste0(signif(votes/total, 3)*100, "%")) %>%
  select(-total)
##Set color map for songs
hex <- fread("C:\\Users\\Bearkey\\Documents\\Anime\\lovelive\\data\\lovelive_hexcolors.csv", data.table=FALSE) %>%
  mutate(group=enc2native(group))
song_color_map <- hex[,"color"]
names(song_color_map) <- hex[,"group"]
song_outline <- hex[,"secondary"]
song_outline[which(song_outline=="")] <- NA
names(song_outline) <- hex[,"group"]
##Set color map for seiyuu
seiyuu <- vote_totals %>%
  filter(q_type=="seiyuu") %>%
  select(song) %>%
  mutate(character=gsub(" \\(.+\\)$", "", song)) %>%
  left_join(hex, by=c("character"="group")) %>%
  select(song, color)
seiyuu_color_map <- seiyuu[,"color"]
names(seiyuu_color_map) <- seiyuu[,"song"]
##Function to generate bar plot given the question number
get_ggplot <- function(qnum) {
  type <- filter(vote_totals, question_no==qnum)[1,"q_type"]
  if (type=="song") {
    color_map <- song_color_map
  } else {
    color_map <- seiyuu_color_map
  }
  df <- filter(vote_totals, question_no==qnum)
  base <- ggplot(df, aes(reorder(song, votes), votes))+
    coord_flip()+
    scale_fill_manual(values=color_map)+
    labs(y="Number of votes")+
    theme_bw()+
    theme(legend.position="bottom", axis.title.y=element_blank())+
    scale_y_continuous(expand=expand_scale(mult=c(0, 0.13)))
  if (type=="song") {
    gg <- base+geom_bar(stat="identity", aes(fill=Artist, col=Artist), size=1.2)+scale_color_manual(values=song_outline)
    if (length(unique(df$Artist))==1) gg <- gg+theme(legend.position="none")
  } else {
    gg <- base+geom_bar(stat="identity", aes(fill=song))+theme(legend.position="none")
  }
  return(gg+geom_text(aes(label=paste(votes, prop, sep=", ")), hjust=-0.05))
}
##Tabulating metadata for questions
metadata <- lapply(1:length(questions), get_df, "info")
