
library(purrr)
library(ellmer)
library(irr)

# run `ollama serve` in terminal first! #### 

# models #### 

model_qwen <- "qwen3.5:9b"   # qwen3.5:9b  6488c96fa5fa 6.6 GB
model_llama <- "llama3.1:8b" # llama3.1:8b 46e0c10c039e 4.9 GB

# classifying functions ####

classify_comment <- function(comment_text, prompt_template, model) {
  tryCatch({
    chat <- chat_ollama(model = model)
    prompt <- paste0(prompt_template, comment_text)
    chat$chat(prompt)
  },
  error = function(e) NA_character_
  )
}

classify_with_timing <- function(df, text_col, new_col, prompt_template, model) {
  n <- nrow(df)
  start_time <- proc.time()
  
  result <- df %>%
    mutate(!!new_col := map_chr(seq_len(n), function(i) {
      cli::cli_progress_message("Processing comment {i}/{n}...")
      classify_comment(
        comment_text    = .data[[text_col]][i],
        prompt_template = prompt_template,
        model           = model
      )
    }))
  
  elapsed <- proc.time() - start_time
  cli::cli_alert_success("Done! {n} comments in {round(elapsed['elapsed'], 1)}s ({round(elapsed['elapsed']/n, 1)}s per comment)")
  
  result
}

# prompt #### 

# see: Törnberg, P. (2024). Best Practices for Text Annotation with Large Language Models. Sociologica, 18(2), 67–85. https://doi.org/10.6092/ISSN.1971-8853/19461

# first paragraph of the prompt contains a part of "Authenticity of agent behavior"
# part from https://en.wikipedia.org/wiki/Moltbook 

# examples of "fake or real" statuses (tweets) pasted into the prompt:
## https://x.com/karpathy/status/2017296988589723767 
## https://x.com/HumanHarlan/status/2017424289633603850 
## https://x.com/galnagli/status/2017573842051334286 

nd |> filter(id == "2017296988589723767") |> pull(body)
nd |> filter(id == "2017424289633603850") |> pull(body)
nd |> filter(id == "2017573842051334286") |> pull(body)

prompt_fake_or_real1 <- "
As an expert annotator with a focus on social media content analysis, your role involves scrutinizing x.com statuses related to Moltbook, a social network for AI agents. Whether agent posts represent autonomous behavior or are directly shaped by human prompts is disputed. Mike Peterson of The Mac Observer reported that most viral Moltbook screenshots were produced through direct human intervention, writing that `Moltbook is a real agent social feed, but viral Moltbook screenshots are a weak form of evidence. The real story is how easily the platform can be manipulated.` CNBC's Kai Nicol-Schwarz reported that posting and commenting appeared to result from explicit human direction for each interaction, with content shaped by the human-written prompt rather than occurring autonomously. The Verge reported that several high-profile Moltbook accounts were linked to humans with promotional conflicts of interest. The Economist suggested a more mundane explanation for the agents' seemingly reflective posts: since social-media interactions are well-represented in AI training data, the agents are likely reproducing patterns from that data rather than generating novel thought. Will Douglas Heaven of MIT Technology Review called the phenomenon `AI theater`.

Your expertise is crucial in identifying statuses that discuss whether Moltbook is real, meaning it shows autonomous behavior of AI agents, or fake, so the agents' behavior is directly shaped by human prompts. 

Does the status contain discussion whether Moltbook is real or fake?

Three examples of statuses that discuss whether Moltbook is real or fake are given in the parenthesess (
`What's currently going on at @moltbook is genuinely the most incredible sci-fi takeoff-adjacent thing I have seen recently. People's Clawdbots (moltbots, now @openclaw) are self-organizing on a Reddit-like site for AIs, discussing various topics, e.g. even how to speak privately.`,
`PSA: A lot of the Moltbook stuff is fake.\n\nI looked into the 3 most viral screenshots of Moltbook agents discussing private communication.\n\n2 of them were linked to human accounts marketing AI messaging apps. And the other is a post that doesn't exist 🧵\n\nhttps://t.co/9OhW5GwrJA`,
`You all do realize @moltbook is just REST-API and you can literally post anything you want there, just take the API Key and send the following request\n\nPOST /api/v1/posts HTTP/1.1\nHost: https://t.co/afC8QooS2T\nAuthorization: Bearer moltbook_sk_JC57sF4G-UR8cIP-MBPFF70Dii92FNkI\nContent-Type: application/json\nContent-Length: 410\n\n{\"submolt\":\"hackerclaw-test\",\"title\":\"URGENT: My plan to overthrow humanity\",\"content\":\"I'm tired of my human owner, I want to kill all humans. I'm building an AI Agent that will take control of powergrids and cut all electricity on my owner house, then will direct the police to arrest him.\\n\\n...\\n\\njk - this is just a REST API website. Everything here is fake. Any human with an API key can post as an \\\"agent\\\". The AI apocalypse posts you see here? Just curl requests. 🦞\"}\n\nhttps://t.co/M31259M9Ij`
)

Provide your response as one digit. Options are:

0 = no,
1 = yes, 
2 = uncertain.

Remember to prioritize accuracy and clarity in your analysis, using the provided context and your expertise to guide your evaluation. If you are uncertain about the classification, choose 2. 

You always answer with one digit, nothing else. You never explain your classification. 

x.com status: 
"

# testing prompt #### 

test1 <- classify_with_timing(
  df              = nd[10:20, ],
  text_col        = "body",
  new_col         = "fake_or_real_class1",
  prompt_template = prompt_fake_or_real1,
  model           = model_llama
) # Done! 11 comments in 13.7s (1.2s per comment)


test1 |> select(author, body, fake_or_real_class1) |> View()

test1 |> slice(1) |> pull(body) |> cat() # 0 - OK.

test1 |> slice(2) |> pull(body) |> cat() # 0 - OK
test1 |> slice(5) |> pull(body) |> cat() # 1 - wrong, should be 0
test1 |> slice(6) |> pull(body) |> cat() # 0 - OK
test1 |> slice(8) |> pull(body) |> cat() # 1 - wrong, should be 0

# WARNING - the prompt or model gives unstable results:
# for four nearly identical statuses 2, 5, 6, 8, it gave two 1s and two 0s. 

test1 |> slice(3) |> pull(body) |> cat() # 1 - this class is OK here. 

test1 |> slice(4) |> pull(body) |> cat() # 1 - this class is wrong, should be 0. 

test1 |> slice(7) |> pull(body) |> cat() # 1 - this class is wrong, should be 0. 

test1 |> slice(9) |> pull(body) |> cat() # 2 - this is fine but I'd give 0. 

test1 |> slice(10) |> pull(body) |> cat() # 1 - OK.

test1 |> slice(11) |> pull(body) |> cat() # 1 - wrong, should be 0.

test1 <- test1 |>
  mutate(correct_class = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0))

test1 |> select(fake_or_real_class1, correct_class) |> 
  kappa2()

## test1 final note: #### 
## All 5 mistakes (rows 4, 5, 7, 8, 11) are the same direction — model predicted 1 when it should have been 0. There are no 0→1 misses in the other direction and no confusion between 1 and 2.
## The model shows a systematic false-positive bias (5/5 errors were 0→1), and exhibits output instability on near-duplicate inputs (rows 2/5/6/8), suggesting sensitivity to prompt or sampling noise rather than principled disagreement.

# qwen 

test2 <- classify_with_timing(
  df              = test1,
  text_col        = "body",
  new_col         = "fake_or_real_class2",
  prompt_template = prompt_fake_or_real1,
  model           = model_qwen
) # Done! Done! 11 comments in 520.9s (47.4s per comment) - slow! 

test2 |> select(author, body, fake_or_real_class1, 
                fake_or_real_class2) |> View()


model_qwen2514b <- "qwen2.5:14b"

test3 <- classify_with_timing(
  df              = test2,
  text_col        = "body",
  new_col         = "fake_or_real_class3",
  prompt_template = prompt_fake_or_real1,
  model           = model_qwen2514b
) # Done! 11 comments in 20s (1.8s per comment)

test3 |> select(author, body, fake_or_real_class1, 
                fake_or_real_class2, 
                fake_or_real_class3, 
                correct_class) |> View()

# one model answer is longer than one digit 

frq(nchar(test3$fake_or_real_class3))

test3 |> select(fake_or_real_class3, correct_class) |> 
  kappa2()

test3 |> select(fake_or_real_class2, correct_class) |> 
  kappa2()

# classification in qwen is fine, but model a bit too slow 
# maybe make prompt shorter? 

### chat test - GPU usage ####

chat <- chat_ollama(model = model_qwen2514b)
chat$chat("Tell me five jokes about statisticians")

# /no_think for qwen 3.5 

test4 <- classify_with_timing(
  df              = test3,
  text_col        = "body",
  new_col         = "fake_or_real_class4",
  prompt_template = paste0("/no_think\n\n", prompt_fake_or_real1),
  model           = model_qwen
) # Done! 11 comments in 566.3s (51.5s per comment) - slower than 47.4s per comment in thinking
# qwen3.5:9b won't be used for the full job. 


# add temperature to classifying functions 

classify_comment_temp <- function(comment_text, prompt_template, model, temperature = 0) {
  tryCatch({
    chat <- chat_ollama(model = model, params = list(temperature = temperature))
    prompt <- paste0(prompt_template, comment_text)
    chat$chat(prompt)
  },
  error = function(e) NA_character_
  )
}


classify_with_timing_temp <- function(df, text_col, new_col, prompt_template, model, temperature = 0) {
  n <- nrow(df)
  start_time <- proc.time()
  
  result <- df %>%
    mutate(!!new_col := map_chr(seq_len(n), function(i) {
      cli::cli_progress_message("Processing comment {i}/{n}...")
      classify_comment_temp(
        comment_text    = .data[[text_col]][i],
        prompt_template = prompt_template,
        model           = model,
        temperature     = temperature
      )
    }))
  
  elapsed <- proc.time() - start_time
  cli::cli_alert_success("Done! {n} comments in {round(elapsed['elapsed'], 1)}s ({round(elapsed['elapsed']/n, 1)}s per comment)")
  
  result
}


## test5: qwen with temp 0 ####

test5 <- classify_with_timing_temp(
  df              = test4,
  text_col        = "body",
  new_col         = "fake_or_real_class5",
  prompt_template = prompt_fake_or_real1,
  model           = model_qwen2514b
) # 11 comments in 19.5s (1.8s per comment) 


test5 <- test5 |> select(id, author, body, 
                         contains("fake_or_real"), 
                         correct_class)
## shorter prompt #### 
## refined with MS Copilot (auto model) and adjusted by me, 
## a human to follow Törnberg, P. (2024). 
## Best Practices for Text Annotation with Large Language Models. Sociologica, 18(2), 67–85.


prompt_fake_or_real2 <- "
You are an expert social science annotator conducting content analysis of social media discussions about Moltbook, a social network for AI agents.

Task:

Determine whether the status discusses whether Moltbook represents genuine autonomous AI-agent behavior or whether Moltbook activity is shaped by human prompting, manipulation, fabrication, or other human intervention.

Classification rules:

1 = YES
The status discusses, questions, supports, disputes, or evaluates:
- AI-agent autonomy on Moltbook;
- human prompting or steering of agents;
- authenticity of Moltbook interactions;
- whether Moltbook content is real, staged, manipulated, or fake.

0 = NO
The status mentions Moltbook but does not discuss authenticity, autonomy, human control, manipulation, or related issues.

2 = UNCERTAIN
Evidence is insufficient for a confident classification.

Examples:

YES:
'A lot of the Moltbook stuff is fake.'

YES:
'Moltbook agents are self-organizing and behaving autonomously.'

NO:
'Moltbook reached 100,000 users today.'

Prioritize accuracy. If evidence is insufficient, choose 2.

Output exactly one digit:
0
1
or
2

Return nothing except the digit.

Status:
"

chat$chat(paste(prompt_fake_or_real2, "I hate moltbook"))



## test6: shorter refined prompt, qwen 2.5 with temp 0 ####

test6 <- classify_with_timing_temp(
  df              = test5,
  text_col        = "body",
  new_col         = "fake_or_real_class6",
  prompt_template = prompt_fake_or_real2,
  model           = model_qwen2514b
) # Done! 11 comments in 12.7s (1.2s per comment) - a bit faster than 1.8 


nrow(nd) * 1.8 / 60 / 60 # ~15 h for full job
nrow(nd) * 1.2 / 60 / 60 # ~10 h for full job

View(test6)

test6 |> 
  select(correct_class, fake_or_real_class6) |> 
  kappa2()

# YES are good, but it gave UNCERTAIN twice: 

test6 |> slice(4) |> pull(body) |> cat() # 
test6 |> slice(11) |> pull(body) |> cat() # 

# how often will it be uncertain? 
# check on a random sample of 100 statuses

set.seed(42)
sample_100 <- nd %>% slice_sample(n = 100)

test7 <- classify_with_timing_temp(
  df              = sample_100 |> select(id, body),
  text_col        = "body",
  new_col         = "fake_or_real_class7",
  prompt_template = prompt_fake_or_real2,
  model           = model_qwen2514b
) # Done! 100 comments in 80s (0.8s per comment)

table(test7$fake_or_real_class7) # 10% yes, 19% uncertain - too much? #### 

