
library(purrr)
library(ellmer)

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

## test1 final note: #### 
## All 5 mistakes (rows 4, 5, 7, 8, 11) are the same direction — model predicted 1 when it should have been 0. There are no 0→1 misses in the other direction and no confusion between 1 and 2.
## The model shows a systematic false-positive bias (5/5 errors were 0→1), and exhibits output instability on near-duplicate inputs (rows 2/5/6/8), suggesting sensitivity to prompt or sampling noise rather than principled disagreement.

## TODO do the same prompt on qwen 



