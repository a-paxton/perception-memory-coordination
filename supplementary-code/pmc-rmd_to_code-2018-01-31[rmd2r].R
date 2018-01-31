#' ---	
#' title: "Data preparation: Interpersonal coordination in perception and memory"	
#' author: "A. Paxton, T. J. H. Morgan, J. Suchow, & T. L. Griffiths"	
#' output:	
#'   html_document:	
#'     keep_md: yes	
#'     number_sections: yes	
#' ---	
#' 	
#' This R markdown provides the data preparation for our manuscript, "Interpersonal coordination in perception and memory" (Paxton, Morgan, Suchow, & Griffiths, *in preparation*).	
#' 	
#' To run this from scratch, you will need the following files:	
#' 	
#' * `./data/`: Contains experimental data. All data for included dyads are freely available in the OSF repository for the project: `https://osf.io/8fu7x/`.	
#' * `./supplementary-code/required_packages-pmc.r`: Installs required libraries, if they are not already installed. **NOTE**: This should be run *before* running this script.	
#' * `./supplementary-code/libraries_and_functions-pmc.r`: Loads in necessary libraries and creates new functions for our analyses.	
#' 	
#' Additional files will be created during the initial run that will help reduce processing time. Several of these files are available as CSVs from the OSF repository listed above.	
#' 	
#' As part of our manuscript for the proceedings of the 2018 annual meeting of the Cognitive Science Society (see `cogsci2018/` directory within this repository), this R markdown file will be converted into an .R script and embedded into the manuscript. Each time the CogSci proceeding is compiled, it will generate a new .R version of this file, dated to the last time this file was changed.	
#' 	
#' **Code written by**: A. Paxton (University of California, Berkeley)	
#' 	
#' **Date last modified**: 26 January 2018	
#' 	
#' ***	
#' 	
#' # Data import	
#' 	
#' ***	
#' 	
#' ## Preliminaries	
#' 	
#' 	
	
# clear our workspace	
rm(list=ls())	
	
# read in libraries and create functions	
source('./supplementary-code/required_packages-pmc.r')	
source('./supplementary-code/libraries_and_functions-pmc.r')	
	
#' 	
#' 	
#' ***	
#' 	
#' ## Concatenate experiment files	
#' 	
#' 	
	
# get list of individual experiments included in the data	
experiment_files = list.dirs('./data', recursive=FALSE)	
	
# concatenate the files	
vector_files = data.frame()	
info_files = data.frame()	
questionnaire_files = data.frame()	
node_files = data.frame()	
participant_files = data.frame()	
for (experiment in experiment_files){	
  	
  # read in the next experiment's files and add ID to each	
  exp_id = strsplit(as.character(experiment),"/|-")[[1]][3]	
  next_vector = read.table(paste(experiment,'/vector.csv',sep=''), sep=',',	
                           header=TRUE, stringsAsFactors = FALSE) %>%	
    mutate(experiment = exp_id)	
  next_info = read.table(paste(experiment,'/info.csv',sep=''), sep=',',	
                         header=TRUE, stringsAsFactors = FALSE) %>%	
    mutate(experiment = exp_id)	
  next_q = read.table(paste(experiment,'/question.csv',sep=''), sep=',',	
                           header=TRUE, stringsAsFactors = FALSE) %>%	
    mutate(experiment = exp_id)	
  next_node = read.table(paste(experiment,'/node.csv',sep=''), sep=',',	
                           header=TRUE, stringsAsFactors = FALSE) %>%	
    mutate(experiment = exp_id)	
  next_participant = read.table(paste(experiment,'/participant.csv',sep=''), sep=',',	
                           header=TRUE, stringsAsFactors = FALSE) %>%	
    mutate(experiment = exp_id)	
	
  # append to group files	
  vector_files = rbind.data.frame(vector_files, next_vector)	
  info_files = rbind.data.frame(info_files, next_info)	
  questionnaire_files = rbind.data.frame(questionnaire_files, next_q)	
  node_files = rbind.data.frame(node_files, next_node)	
  participant_files = rbind.data.frame(participant_files, next_participant)	
	
}	
	
#' 	
#' 	
#' ## Export duration and bonus data	
#' 	
#' 	
	
# clean up the questionnaire files	
question_trimmed = questionnaire_files %>%	
  select(experiment, participant_id, number, question, response)	
	
# clean up the participant files	
participant_trimmed = participant_files %>%	
  dplyr::filter(status=='approved') %>%	
  mutate(creation_time = ymd_hms(creation_time)) %>%	
  mutate(end_time = ymd_hms(end_time)) %>%	
  mutate(duration = (end_time - creation_time)) %>%	
  select(experiment, worker_id, status, base_pay, bonus, duration)	
	
# join the files and omit any folks that aren't completely included in both dataframes	
partipation_descriptives = full_join(question_trimmed, participant_trimmed,	
                                     by = c('experiment',	
                                            'participant_id' = 'worker_id')) %>%	
  na.omit() %>%	
  	
  # remove actual question data	
  select(-response, -question, -status, -number)	
	
#' 	
#' 	
#' 	
	
# export bonuses	
write.table(partipation_descriptives, './data/partipation_descriptives.csv', 	
            sep=',', append = FALSE, quote = FALSE, row.names = FALSE, col.names = TRUE)	
	
#' 	
#' 	
#' ## Identify dyads from vector data	
#' 	
#' In order to figure out which participants' nodes were connected to one another in dyads, we use the vectors created between nodes (excluding the stimulus-creating node). We then use that information to identify which stimuli were sent to which dyads.	
#' 	
#' 	
	
# use the vectors connecting the nodes to identify pairs	
vector_df = vector_files %>%	
  	
  # convert time to integer and winnow out unnecessary variables and nodes	
  mutate(t = round(as.numeric(ymd_hms(creation_time)), 0)) %>%	
  select(experiment, t, origin_id, destination_id, network_id) %>%	
  dplyr::filter(!origin_id == 1) %>%	
  	
  # find pairs from vector files	
  group_by(experiment, t) %>%	
  mutate(min_id = pmin(origin_id,destination_id)) %>%	
  mutate(max_id = pmax(origin_id,destination_id)) %>%	
  ungroup() %>%	
  	
  # get unique pairs and number them	
  select(-origin_id, -destination_id) %>%	
  distinct() %>%	
  mutate(dyad = seq_along(min_id)) %>%	
	
  # gather the participants into a single column	
  gather(key="id",value="participant", min_id, max_id) %>%	
  select(-id)	
	
# figure out which stimuli were sent to which dyads	
dyad_df = info_files %>%	
  mutate(t = round(as.numeric(ymd_hms(creation_time)), 0)) %>%	
  dplyr::filter(origin_id == 1) %>%	
  select(experiment, t, contents) %>%	
  full_join(., vector_df,	
            by = c('experiment', 't')) %>%	
  select(-t)	
	
#' 	
#' 	
#' ## Prepare dataframe	
#' 	
#' We now take the concatenated files and begin processing, including de-duplication of dataset.	
#' 	
#' The structure of the experiment sometimes led to near-duplicate rows to be sent to the server to manage partner communication. We must now identify these near-duplicates and strip them out. We can best identify these by using the `response_counter` variable: A properly de-duplicated dataset should have only 1 row per `response_counter` value in each trial for each participant.	
#' 	
#' 	
	
info_df = info_files %>% ungroup() %>%	
  	
  # filter out stimulus nodes	
  dplyr::filter(!origin_id == 1) %>%	
  	
  # convert time and get rid of unnecessary variables	
  mutate(t = round(as.numeric(ymd_hms(creation_time)), 0)) %>%	
  select(experiment, t, property3, origin_id, network_id, contents) %>%	
    	
  # read in `contents` as JSONs	
  cbind(., jsonlite::stream_in(textConnection(.$contents))) %>%	
	
  # rename a whole slew of variables	
  dplyr::rename(participant = origin_id,	
                trial_type = trialType,	
                trial_number = trialNumber,	
                guess_counter = guessCounter,	
                response_counter = responseCounter,	
                accept_type = acceptType,	
                #response_type = responseType,	
                stimulus_number = chosenStimulusNumber,	
                length = chosenStimulusLength) %>%	
  	
  # get rid of unnecessary variables and arrange rows	
  select(-property3, -finalAccuracy, -contents) %>%	
  dplyr::arrange(experiment, participant, trial_number, response_counter) %>%	
  	
  # remove the automatically generated infos that produced NAs in `guess`	
  dplyr::filter(!is.na(guess)) %>%	
  	
  # determine uniqueness without considering time or response_type	
  group_by(experiment, participant, network_id, trial_type,	
           trial_number, guess_counter, response_counter) %>%	
  summarise_all(first) %>%	
  ungroup() %>%	
  	
  # replace NAs from guesses and calculate error with each guess	
  mutate(guess = replace(guess, guess<0, NA)) %>%	
  mutate(guess_error = length - guess) %>%	
  	
  # merge info dataframe with dyad number information	
  full_join(., dyad_df,	
            by = c('experiment', 'participant','network_id')) %>%	
  dplyr::rename(stimulus_list = contents)	
	
#' 	
#' 	
#' 	
	
# sanity check	
sanity_df = info_df %>% ungroup() %>%	
  	
  # count number of times we see the same response counter	
  group_by(experiment, participant, trial_type, trial_number, guess_counter, response_counter) %>%	
  summarise(n = n()) %>%	
  ungroup() %>%	
  	
  # filter to include only test-condition duplicates	
  dplyr::filter(n!=1 & trial_type=="test") %>%	
  	
  # join with the main dataset to check it out	
  inner_join(., info_df, 	
             by = c("experiment", "participant", "trial_type", 	
                    "trial_number", "guess_counter", "response_counter")) %>%	
  	
  # identify whether any identified duplicate rows have diferent accept_type values	
  group_by(experiment, participant, trial_type, trial_number, guess_counter, response_counter) %>%	
  dplyr::filter(length(unique(accept_type))!=1) %>%	
  ungroup()	
	
# print sanity check	
cat('Problematic rows identified (i.e., duplicates with differing accept types): ',dim(sanity_df)[1],sep='')	
	
#' 	
#' 	
#' ***	
#' 	
#' # Data cleaning	
#' 	
#' ***	
#' 	
#' ## Identify pairs 	
#' 	
#' Next, we identify all dyads in which both participants completed all trials.	
#' 	
#' 	
	
# identify usable dyads	
paired_individuals = info_df %>%	
  	
  # count the number of infos and trials per participant	
  group_by(experiment,participant) %>%	
  summarise(trials = max(trial_number),	
            dyad = ifelse(length(unique(dyad)==1),	
                          unique(dyad),	
                          NA),	
            infos = n()) %>%	
  ungroup() %>%	
  na.omit() %>%	
  	
  # count the infos sent by each participant in each dyad	
  group_by(experiment, dyad) %>%	
  mutate(participant = paste('p',(participant - min(participant)),sep='')) %>%	
  spread(key = participant, value = infos) %>%	
  ungroup() %>%	
  mutate(difference_in_responses = abs(p1-p0)) %>%	
	
  # remove any participants who weren't paired with someone	
  na.omit() %>%	
  	
  # only include pairs in which both individuals completed 24 trials	
  dplyr::filter(trials==24)	
	
# export the data	
write.table(paired_individuals, './data/participant_pairs.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", row.names = FALSE, col.names = TRUE)	
	
#' 	
#' 	
#' 	
	
total_paired_individuals = paired_individuals %>% ungroup() %>%	
  select(experiment, dyad) %>%	
  distinct()	
	
cat('Total participants with partners who finished: ',dim(total_paired_individuals)[1], sep='')	
	
#' 	
#' 	
#' ## Remove problematic trials	
#' 	
#' Some dyads became mismatched in their progress throughout the game. Essentially, in some trials, one player would move on to the next trial, while their partner would "hang" in the previous one. The program would automatically move someone forward after this mismatched state persisted for a few seconds.	
#' 	
#' To deal with this issue, we strike the entire trial for that dyad.	
#' 	
#' 	
	
# exclude trials with mismatched data	
discarded_trials_df = info_df %>%	
  dplyr::filter(dyad %in% paired_individuals$dyad & trial_type=="test") %>%  	
  	
  # count the number of infos per trial per participant	
  group_by(experiment,participant,trial_number) %>%	
  summarise(dyad = ifelse(length(unique(dyad)==1),	
                          unique(dyad),	
                          NA),	
            infos = n()) %>%	
  ungroup() %>%	
  na.omit() %>%	
  	
  # count the infos sent by each participant in each dyad	
  group_by(experiment, dyad) %>%	
  mutate(participant = paste('p',(participant - min(participant)),sep='')) %>%	
  spread(key = participant, value = infos) %>%	
  ungroup() %>%	
  mutate(difference_in_responses = abs(p1-p0)) %>%	
  	
  # single out the trials with mismatching responses	
  dplyr::filter(!difference_in_responses==0)	
	
#' 	
#' 	
#' 	
	
# share information on how many trials will be removed	
total_removed_trials = discarded_trials_df %>% ungroup() %>%	
  group_by(experiment, dyad) %>%	
  summarise(total_discarded = n())	
	
cat('Total trials discarded: ',sum(total_removed_trials$total_discarded),' (across ', length(unique(total_removed_trials$dyad)),' dyads) ', sep='')	
	
#' 	
#' 	
#' ## Winnow the data	
#' 	
#' 	
	
# winnow and recorder columns	
winnowed_info_df = info_df %>% ungroup() %>%	
  dplyr::filter(dyad %in% paired_individuals$dyad) %>%	
  dplyr::left_join(., discarded_trials_df,	
                   by = c("experiment","dyad","trial_number")) %>%	
  dplyr::filter(is.na(difference_in_responses)) %>%	
  mutate(t = round(t,-1)) %>%	
  select(experiment, t, dyad, participant, 	
         trial_type, trial_number, response_counter, guess_counter, accept_type, 	
         length, guess, guess_error, network_id) %>%	
  na.omit()	
	
winnowed_info_df = unique(setDT(winnowed_info_df), by = c('experiment', 'dyad',	
        'participant', 'trial_type', 'trial_number', 'response_counter', 'guess_counter',	
        'accept_type', 'length', 'guess', 'guess_error', 'network_id'))	
	
#' 	
#' 	
#' 	
	
included_trial_info = winnowed_info_df %>% ungroup() %>%	
  dplyr::filter(trial_type == 'test') %>%	
  group_by(experiment, dyad) %>% 	
  summarize(included_trials = length(unique(trial_number)))	
	
cat('Mean included trials per dyad: ',mean(included_trial_info$included_trials), sep='')	
	
#' 	
#' 	
#' ## Quick sanity check	
#' 	
#' For sanity, let's also check that everyone included in our winnowed dataset completed both training and test trials.	
#' 	
#' 	
	
# ensure that everyone completed both training and test	
only_one_trial_type = winnowed_info_df %>% ungroup() %>%	
  select(experiment, participant, trial_type) %>%	
  distinct() %>%	
  group_by(experiment, participant) %>%	
  summarize(n=n()) %>%	
  dplyr::filter(n!=2)	
	
#' 	
#' 	
#' 	
	
cat('Included participants who did not submit guesses during any training trials: ',dim(only_one_trial_type)[1], sep='')	
	
#' 	
#' 	
#' It looked like some participants chose not to complete the training trials or struggled with getting their guesses submitted in time.  We'll need to handle this when we create training slopes.	
#' 	
#' ***	
#' 	
#' # Data processing	
#' 	
#' ***	
#' 	
#' ## Add questionnaire data	
#' 	
#' In the experiment's current form, different tables include different information, and some tables present the same information under different labels. This is true for questionnaire data. To accurately pair individuals' guess data with their questionnaire responses, we match the `participant_id` variables in `node_df` and `question_df`, and we join the `id` variable in `node_df` with the `participant` variable in `info_df`.	
#' 	
#' 	
	
# clean up questionnaire data by converting the stringified JSONs to a new variable	
question_df = questionnaire_files %>% ungroup() %>%	
  select(experiment, participant_id, response) %>%	
  cbind(., jsonlite::stream_in(textConnection(.$response))) %>%	
  select(-response)	
	
# clean up the node dataframe	
node_df = node_files %>% ungroup() %>%	
  select(experiment, participant_id, id) %>%	
  na.omit()	
	
# join questionnaire data wth infos and remove any participants whose survey data we don't have	
winnowed_info_df = left_join(question_df, node_df,	
                                by=c('experiment','participant_id')) %>%	
  left_join(winnowed_info_df, .,	
                              by=c('experiment','participant' = 'id')) %>%	
  drop_na(cooperative_partner, cooperative_self, trust_partner, trust_self, engagement, difficulty)	
	
#' 	
#' 	
#' 	
	
# identify how many dyads have matching infos and complete questionnaire data	
usable_question_dyads = winnowed_info_df %>% ungroup() %>%	
  select(experiment, dyad, participant) %>%	
  distinct() %>%	
  group_by(experiment, dyad) %>%	
  summarise(included_p = n()) %>%	
  ungroup() %>%	
  dplyr::filter(included_p==2)	
	
# if needed, remove dyads who didn't have questionnaire data	
winnowed_info_df = winnowed_info_df %>% ungroup() %>%	
  dplyr::filter(dyad %in% usable_question_dyads$dyad)	
	
#' 	
#' 	
#' 	
	
cat('Total dyads with all guess and questionnaire data: ',dim(usable_question_dyads)[1], sep='')	
	
#' 	
#' 	
#' ## Create unique dyad and participant IDs across all experiments	
#' 	
#' Dallinger provides numeric IDs for each participant that are unique only within each experiment. Therefore, we create participant and dyad identifiers that are unique across the entire dataset.	
#' 	
#' 	
	
# create unique dyad IDs	
unique_dyad_ids = winnowed_info_df %>% ungroup() %>%	
  select(experiment, dyad) %>%	
  distinct() %>%	
  mutate(unique_dyad = row_number())	
	
# create unique participant IDs	
unique_participant_ids = winnowed_info_df %>% ungroup() %>%	
  select(experiment, participant) %>%	
  distinct() %>%	
  mutate(unique_participant = row_number())	
	
# merge both into the main dataframe and rename	
winnowed_info_df = right_join(unique_participant_ids, winnowed_info_df,	
                             by=c('experiment', 'participant')) %>%	
  right_join(unique_dyad_ids, ., by=c('experiment','dyad')) %>%	
  dplyr::rename(original_participant = participant,	
                original_dyad = dyad,	
                participant = unique_participant,	
                dyad = unique_dyad) %>%	
  dplyr::arrange(experiment, participant, trial_number, response_counter)	
	
#' 	
#' 	
#' ## Increment all counters by 1	
#' 	
#' Data were collected using Pythonic counters (i.e., starting from 0). We'll here update the dataframe to reflect R conventions (i.e., starting from 1).	
#' 	
#' 	
	
winnowed_info_df = winnowed_info_df %>%	
  mutate(trial_number = trial_number + 1) %>%	
  mutate(response_counter = response_counter + 1) %>%	
  mutate(guess_counter = guess_counter + 1)	
	
#' 	
#' 	
#' ## Normalize error by maximum possible error	
#' 	
#' Because stimuli line lengths could range from 1-100, each trial provided a bound on the total possible guess error.  As a result, we need to normalize each guess error by the maximum *possible* error for that trial.	
#' 	
#' 	
	
winnowed_info_df = winnowed_info_df %>% ungroup() %>%	
  mutate(normalized_error = guess_error/max(abs(100-length),abs(length-100)))	
	
#' 	
#' 	
#' ## Create training accuracy metric	
#' 	
#' We next create a training metric that quantifies the *non-directional* improvement over the training rounds. Essentially, this captures the change in relative accuracy over training, regardless of whether participants began by over- or under-estimating line lengths.	
#' 	
#' 	
	
# create a slope to see how quickly they improved	
winnowed_info_df = winnowed_info_df %>% ungroup() %>%	
  	
  # if they didn't do training, give them a flat training performance	
  mutate(normalized_error = replace(normalized_error, 	
                                    which(normalized_error<0L), 	
                                    0)) %>%	
  	
  select(participant, trial_type, trial_number, normalized_error) %>%	
  na.omit() %>%	
  dplyr::filter(trial_type == 'train') %>%	
  group_by(participant) %>%	
  do(broom::tidy(lm(abs(.$normalized_error) ~ .$trial_number))) %>%	
  dplyr::filter(term=='.$trial_number') %>%	
  select(participant, estimate) %>%	
  dplyr::rename(training_improvement = estimate) %>%	
  left_join(winnowed_info_df, .,	
            by='participant') %>%	
  	
  # if they didn't complete training, give them a 0	
  mutate(training_improvement = replace(training_improvement, 	
                                    which(training_improvement<0L), 	
                                    0))	
	
#' 	
#' 	
#' 	
	
# create a plot to show training slopes	
training_slope_plot = ggplot(dplyr::filter(winnowed_info_df, 	
                                           trial_type=='train'), 	
                             aes(x = trial_number,	
                                 y = abs(normalized_error))) +	
  geom_line(aes(color=as.factor(participant))) +	
  scale_color_viridis(discrete=TRUE) +	
  stat_smooth() +	
  ylab('Absolute error of guess') +	
  scale_x_continuous(breaks=c(1,5,10)) +	
  xlab('Training trial') +	
  ggtitle('Accuracy of individual participants over training') +	
  theme(legend.position="none")	
	
# save a high-resolution version of the plot	
ggsave(plot = training_slope_plot,	
       height = 3,	
       width = 5,	
       filename = './figures/pmc-training_slopes.jpg')	
	
# save a smaller version of the plot for knitr	
ggsave(plot = training_slope_plot,	
       height = 3,	
       width = 5,	
       dpi=100,	
       filename = './figures/pmc-training_slopes-knitr.jpg')	
	
#' 	
#' 	
#' ![**Figure**. Included participants' absolute normalized error over all training trials and best-fit line (in blue).](./figures/pmc-training_slopes-knitr.jpg)	
#' 	
#' 	
	
# figure out what our equal x-axis limits will be	
x_limit = winnowed_info_df %>% ungroup() %>%	
  na.omit() %>%	
  summarise(lim = max(abs(normalized_error))) %>%	
  .$lim	
	
# create plot	
normalized_error_hist = ggplot(winnowed_info_df,	
                               aes(x = normalized_error)) + 	
  geom_histogram(aes(fill = factor(trial_number)),bins=30) +	
  scale_fill_viridis(discrete=TRUE,	
                     breaks=c('1',	
                              '5',	
                              '10',	
                              '15',	
                              '20',	
                              '25'),	
                     labels=c('First',	
                              '',	
                              '',	
                              '',	
                              '',	
                              'Last'),	
                     name = "Trial") +	
  xlab('Normalized error') +	
  ylab('Count') +	
  xlim(-x_limit, x_limit) +	
  ggtitle('Normalized error over all trials')	
  	
# save a high-resolution version of the plot	
ggsave(plot = normalized_error_hist,	
       height = 4,	
       width = 5,	
       filename = './figures/pmc-error_hist.jpg')	
	
# save a smaller version of the plot for knitr	
ggsave(plot = normalized_error_hist,	
       height = 4,	
       width = 5,	
       dpi=100,	
       filename = './figures/pmc-error_hist-knitr.jpg')	
	
#' 	
#' 	
#' ![**Figure**. Histogram of individual participants' normalized error for each guess over all trials. Histogram is further broken down by the trial number at which each guess was given.](./figures/pmc-error_hist-knitr.jpg)	
#' 	
#' ## Widen data to include partner's guess	
#' 	
#' 	
	
# create a column for the partner's guess at that time	
winnowed_info_df = winnowed_info_df %>% ungroup() %>%	
  	
  # create participant binary values	
  group_by(experiment, dyad) %>%	
  mutate(partner_id = (min(participant)+max(participant)) - participant) %>%	
  mutate(self_id = participant) %>%	
  ungroup() %>%	
	
  # gather into multiple values	
  select(self_id, partner_id, normalized_error, trial_number, response_counter) %>%	
  dplyr::rename(partner_error = normalized_error) %>%	
  distinct() %>%	
  	
  # merge	
  left_join(winnowed_info_df, .,	
            by=c('participant'='self_id',	
                 'trial_number',	
                 'response_counter'))	
	
#' 	
#' 	
#' ## Export raw data	
#' 	
#' 	
	
write.table(winnowed_info_df, './data/winnowed_data.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", row.names = FALSE, col.names = TRUE)	
	
#' 	
#' 	
#' ***	
#' 	
#' # Data exploration and descriptive statistics	
#' 	
#' ***	
#' 	
#' ## Preliminaries	
#' 	
#' 	
	
# clear our workspace	
rm(list=ls())	
	
# read in libraries and create functions	
source('./supplementary-code/libraries_and_functions-pmc.r')	
	
# read in dataset	
winnowed_info_df = read.table('./data/winnowed_data.csv', sep=',',header = TRUE)	
	
#' 	
#' 	
#' ## Bonuses and duration	
#' 	
#' 	
	
participation_descriptives = read.table('./data/partipation_descriptives.csv', 	
                                        sep=',', header = TRUE, )	
	
#' 	
#' 	
#' 	
#' 	
cat('Average participation duration: ',mean(participation_descriptives$duration),' minutes',sep='')	
#' 	
#' 	
#' 	
cat('Average particpant performance bonus (minus flat completion bonus):  $',	
          mean(participation_descriptives$bonus-.33),sep='')	
#' 	
#' 	
#' 	
#' ## Variable distributions	
#' 	
#' 	
	
# adapted from https://drsimonj.svbtle.com/quick-plot-of-all-variables	
all_variable_plot = winnowed_info_df %>%	
  mutate_all(funs(as.numeric)) %>%	
  select(one_of(questionnaire_variables),	
         normalized_error, experiment, dyad, training_improvement) %>%	
  gather() %>%                	
  ggplot(aes(value)) +	
    facet_wrap(~ key, scales = "free") +	
    geom_density() +	
    xlab('Value') +	
    ylab('Density') +	
    ggtitle('Density plots of questionnaire data and outcome variables')	
	
# save a high-resolution version of the plot	
ggsave(plot = all_variable_plot,	
       height = 4,	
       width = 8,	
       filename = './figures/pmc-all_variables.jpg')	
	
# save a smaller version of the plot for knitr	
ggsave(plot = all_variable_plot,	
       height = 4,	
       width = 8,	
       dpi=100,	
       filename = './figures/pmc-all_variables-knitr.jpg')	
	
#' 	
#' 	
#' ![**Figure**. Density plots of questionnaire responses, normalized error, and improvement over training trials.](./figures/pmc-all_variables-knitr.jpg)	
#' ***	
#' 	
#' # Data manipulation	
#' 	
#' ***	
#' 	
#' ## Preliminaries	
#' 	
#' 	
	
# clear our workspace	
rm(list=ls())	
	
# read in libraries and create functions	
source('./supplementary-code/libraries_and_functions-pmc.r')	
	
# read in dataset	
winnowed_info_df = read.table('./data/winnowed_data.csv', sep=',',header = TRUE)	
	
#' 	
#' 	
#' ## Calculate cross-correlation between partners' normalized error	
#' 	
#' First, we prepare the dataframe for cross-correlation by transitioning from long-form data for both participants within the dyad to using wide-form data for each dyad, with one column for each constituent participant's `normalized_error` at each `trial_number` and `response_counter`.	
#' 	
#' 	
	
# strip out unnecessary information	
infos = winnowed_info_df %>% ungroup() %>%	
  select(experiment,dyad,participant,t,normalized_error,trial_number,response_counter) %>%	
  	
  # create participant binary values	
  group_by(experiment, dyad) %>%	
  mutate(partner_id = (min(participant)+max(participant)) - participant) %>%	
  mutate(self_id = participant) %>%	
  ungroup() %>%	
  	
  # create binary ID	
  group_by(experiment, dyad) %>%	
  mutate(partner_binary = participant - min(participant)) %>%	
  ungroup() %>%	
  	
  # remove training trials	
  dplyr::filter(trial_number > 10)	
	
# create a dataframe with one dyad per row and separate columns for each participant's error data	
binary_dfs = split(infos, infos$partner_binary)	
p0_df = data.frame(binary_dfs[[1]]) %>%	
  dplyr::rename(error0 = normalized_error) %>%	
  select(experiment, dyad, trial_number, response_counter, error0)	
p1_df = data.frame(binary_dfs[[2]]) %>%	
  dplyr::rename(error1 = normalized_error) %>%	
  select(experiment, dyad, trial_number, response_counter, error1)	
	
#' 	
#' 	
#' Once the data are prepared, we calculate the cross-correlation coefficients between participants' `normalized_error` during all test rounds. The maximum lag is specified within the `libraries_and_functions-pmc.r` file.	
#' 	
#' 	
	
# calculate cross-correlation	
ccf_df = full_join(p0_df,p1_df,	
                   by= c("experiment", "dyad", "trial_number", "response_counter")) %>%	
  	
  # calculate cross-correlation for each dyad's error scores	
  group_by(experiment,dyad) %>%	
  do(ccf = ccf(.$error0, .$error1, lag.max = ccf_max_lag, type = 'correlation',	
               na.action = na.pass, plot=FALSE)) %>%	
  ungroup() %>%	
  	
  # extract cross-correlations from the embedded list	
  select(ccf) %>%	
  dplyr::pull(ccf) %>%	
  unlist() %>%	
  matrix(.,ncol=length(unique(winnowed_info_df$dyad))) %>%	
  	
  # convert it into a proper dataframe and select only the coefficients	
  as.data.frame() %>%	
  slice(1:(ccf_max_lag*2+1)) %>%	
  t() %>%	
  as.data.frame %>%	
  rowid_to_column(var='dyad') %>%	
	
  # rename variables and strip rownames	
  rename_(.dots=setNames(names(.), 	
                         gsub("V", "", names(.)))) %>%	
  remove_rownames() %>%	
  	
  # reshape the data to combine lag and r	
  gather(key = 'lag' , value='r', -dyad) %>%	
  mutate_all(as.numeric) %>%	
  mutate(lag = lag - ccf_max_lag - 1)	
	
#' 	
#' 	
#' Because we don't have any theoretical expectations about or experimental manipulations to change *who* might be leading and following, we ignore directionality for this first-pass analysis.	
#' 	
#' 	
	
# ignore lag directionality	
ccf_df = ccf_df %>% ungroup() %>%	
  mutate(lag = abs(lag)) %>%	
  group_by(dyad,lag) %>%	
  summarise(r = mean(r))	
	
#' 	
#' 	
#' Once we've calculated the cross-correlation coefficients for each dyad, we merge it into the questionnaire data.	
#' 	
#' 	
	
# grab what we need for the cross-correlation analyses	
questions_only = winnowed_info_df %>%	
  select(one_of(c('experiment','dyad','participant',	
                  questionnaire_variables, 'training_improvement'))) %>%	
  	
  # create a mean training improvement score for the dyad	
  group_by(experiment, dyad) %>%	
  mutate(training_improvement = mean(training_improvement)) %>%	
  ungroup() %>%	
  	
  # select only the unique rows	
  distinct() 	
	
# merge into the ccf dataframe	
ccf_df = full_join(questions_only, ccf_df,	
                   by='dyad','experiment')	
	
#' 	
#' 	
#' Let's clean up a bit before we move on.	
#' 	
#' 	
	
# clean up unneeded variables	
rm(p0_df,p1_df,binary_dfs, infos, questions_only)	
	
#' 	
#' 	
#' ## Create interaction terms	
#' 	
#' ### For `winnowed_info_df`	
#' 	
#' 	
	
# create interactions	
winnowed_info_df = winnowed_info_df %>% ungroup() %>%	
  	
  # create a turn variable across trials and responses	
  as.data.frame() %>%	
  group_by(experiment,dyad,participant) %>%	
  mutate(turn = row_number()) %>%	
  ungroup() %>%	
  	
  # exclude training data	
  dplyr::filter(trial_type=='test') %>%	
	
  # survey interactions	
  mutate(cooperative.both = cooperative_self * cooperative_partner) %>%	
  mutate(trust.both = trust_self * trust_partner) %>%	
  mutate(cooperative.trust.self = cooperative_self * trust_self) %>%	
  mutate(cooperative.trust.partner = cooperative_partner * trust_partner) %>%	
  	
  # error interactions	
  mutate(error.length = (normalized_error+.00001) * length) %>%	
  mutate(error.turn = (normalized_error+.00001) * turn) %>%	
  mutate(error.length.turn = (normalized_error+.00001) * turn * length) %>%	
  	
  # other interactions	
  mutate(turn.training = turn * training_improvement)	
	
#' 	
#' 	
#' 	
	
# spin off a dataset for only first answers	
first_guess_df = winnowed_info_df %>% ungroup() %>%	
  	
  # grab just the final guess on each trial guess	
  dplyr::filter(response_counter==1) %>%	
  	
  # filter out "turn" variables	
  select(-contains("turn")) %>%	
  	
  # recreate the interactions at the trial level	
  mutate(error.length = (normalized_error+.00001) * length) %>%	
  mutate(error.trial = (normalized_error+.00001) * trial_number) %>%	
  mutate(error.length.trial = (normalized_error+.00001) * trial_number * length) %>%	
  mutate(trial.training = trial_number * training_improvement)	
	
#' 	
#' 	
#' 	
	
# spin off a dataset for only final answers	
final_guess_df = winnowed_info_df %>% ungroup() %>%	
  	
  # grab just the final guess on each trial guess	
  group_by(experiment,dyad,participant,trial_number) %>%	
  slice(n()) %>%	
  ungroup() %>%	
  	
  # filter out "turn" variables	
  select(-contains("turn")) %>%	
  	
  # recreate the error interactions at the trial level	
  mutate(error.length = (normalized_error+.00001) * length) %>%	
  mutate(error.trial = (normalized_error+.00001) * trial_number) %>%	
  mutate(error.length.trial = (normalized_error+.00001) * trial_number * length) %>%	
  mutate(trial.training = trial_number * training_improvement)	
	
#' 	
#' 	
#' ### For `ccf_df`	
#' 	
#' 	
	
# create first- and second-order orthogonal polynomials for lag 	
raw_lag = min(ccf_df$lag):max(ccf_df$lag)	
lag_vals = data.frame(raw_lag)	
lag_offset = (0-min(raw_lag)) + 1	
t = stats::poly((raw_lag + lag_offset), 2)	
lag_vals[, paste("lag_ot", 1:2, sep="")] = t[lag_vals$raw_lag + lag_offset, 1:2]	
	
# join it to the original data table	
ccf_df = left_join(ccf_df,lag_vals, by = c("lag" = "raw_lag"))	
	
#' 	
#' 	
#' 	
	
ccf_df = ccf_df %>% ungroup() %>%	
  	
  # create interactions among static variables of interest	
  mutate(cooperative.both = cooperative_self * cooperative_partner) %>%	
  mutate(trust.both = trust_self * trust_partner) %>%	
  mutate(cooperative.trust.self = cooperative_self * trust_self) %>%	
  mutate(cooperative.trust.partner = cooperative_partner * trust_partner) %>%	
	
  # first-order polynomials with lag	
  mutate(cooperative_self.lag_ot1 = cooperative_self * lag_ot1) %>%	
  mutate(cooperative_partner.lag_ot1 = cooperative_partner * lag_ot1) %>%	
  mutate(trust_self.lag_ot1 = trust_self * lag_ot1) %>%	
  mutate(trust_partner.lag_ot1 = trust_partner * lag_ot1) %>%	
	
  # first-order polynomials with lag	
  mutate(cooperative_self.lag_ot2 = cooperative_self * lag_ot2) %>%	
  mutate(cooperative_partner.lag_ot2 = cooperative_partner * lag_ot2) %>%	
  mutate(trust_self.lag_ot2 = trust_self * lag_ot2) %>%	
  mutate(trust_partner.lag_ot2 = trust_partner * lag_ot2) %>%	
  	
  # polynomial interactions	
  mutate(lag_ot1.lag_ot2 = lag_ot1 * lag_ot2) %>%	
  mutate(cooperative.both.lag_ot1.lag_ot2 = cooperative.both * lag_ot1 * lag_ot2) %>%	
  mutate(trust.both.lag_ot1.lag_ot2 = trust_self * trust_partner * lag_ot1 * lag_ot2) %>%	
  mutate(cooperative.trust.self.lag_ot1.lag_ot2 = cooperative_self * trust_self * lag_ot1 * lag_ot2) %>%	
  mutate(cooperative.trust.partner.lag_ot1.lag_ot2 = cooperative_partner * trust_partner * lag_ot1 * lag_ot2)	
	
#' 	
#' 	
#' ## Create standardized datasets	
#' 	
#' ### For `winnowed_info_df`	
#' 	
#' 	
	
info_plot = winnowed_info_df %>% ungroup() %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
	
info_st = winnowed_info_df %>% ungroup() %>%	
  mutate_all(funs(as.numeric(scale(as.numeric(.))))) %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
	
first_guess_plot = first_guess_df %>% ungroup() %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
	
first_guess_st = first_guess_df %>% ungroup() %>%	
  mutate_all(funs(as.numeric(scale(as.numeric(.))))) %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
  	
final_guess_plot = final_guess_df %>% ungroup() %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
  	
final_guess_st = final_guess_df %>% ungroup() %>%	
  mutate_all(funs(as.numeric(scale(as.numeric(.))))) %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
	
#' 	
#' 	
#' ### For `ccf_df`	
#' 	
#' 	
	
# create unstandardized dataframe and convert relevant variables to factors	
ccf_plot = ccf_df %>% ungroup() %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
  	
# create standardized dataframe and convert relevant variables to factors	
ccf_st = ccf_df %>%	
  mutate_all(funs(as.numeric(scale(as.numeric(.))))) %>%	
  mutate_at(vars(participant,dyad),	
            funs(factor))	
	
#' 	
#' 	
#' ## Export analysis-ready datasets	
#' 	
#' 	
	
# export standardized and plotting info datasets	
write.table(info_plot, './data/info_plot.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
write.table(info_st, './data/info_st.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
	
# export standardized and plotting first-guess datasets	
write.table(first_guess_plot, './data/first_guess_plot.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
write.table(first_guess_st, './data/first_guess_st.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
	
# export standardized and plotting final-guess datasets	
write.table(final_guess_plot, './data/final_guess_plot.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
write.table(final_guess_st, './data/final_guess_st.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
	
# export standardized and plotting ccf datasets	
write.table(ccf_plot, './data/ccf_plot.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
write.table(ccf_st, './data/ccf_st.csv', sep=',',	
            append = FALSE, quote = FALSE, na = "NA", 	
            row.names = FALSE, col.names = TRUE)	
	
#' 	
#' 	
