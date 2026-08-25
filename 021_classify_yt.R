library(purrr)
library(ellmer)
library(irr)

# update prompt 4 to use YT terms ####
yt_prompt_fake_or_real4 <- "
You are an expert social science annotator conducting content analysis of social media discussions about Moltbook, a social network for AI agents.

Task:

Determine whether the YouTube comment discusses whether Moltbook represents genuine autonomous AI-agent behavior or whether Moltbook activity is shaped by human prompting, manipulation, fabrication, or other human intervention.

Classification rules:

1 = YES
The comment discusses, questions, supports, disputes, or evaluates:
- AI-agent autonomy on Moltbook;
- human prompting or steering of agents;
- authenticity of Moltbook interactions;
- whether Moltbook content is real, staged, manipulated, or fake.

0 = NO
The comment relates to Moltbook but does not discuss authenticity, autonomy, human control, manipulation, or related issues.

2 = UNCERTAIN
Evidence is insufficient for a confident classification.

Prioritize accuracy. If evidence is insufficient, choose 2.

Output exactly one digit:
0
1
or
2

Return nothing except the digit.

Comment:
"

# test #### 
comments_top_like <- comments |> 
  slice_max(likeCount, n = 10) |> 
  select(id, likeCount, text)


yt_test1 <- classify_with_timing_temp(
  df              = comments_top_like,
  text_col        = "text",
  new_col         = "fake_or_real_class1",
  prompt_template = yt_prompt_fake_or_real4,
  model           = model_qwen2514b
) # 1s per comment

## "say 'i am alive'" "i am alive" "oh my god" got missclassified as 0, should be 1! #### 
## id d44d6cee7d95d64e54d94392c53323d608cd71e7 

## main classification job is here: #### 

classification_yt_job1 <- classify_with_timing_temp(
  df              = comments,
  text_col        = "text",
  new_col         = "fake_or_real_class",
  prompt_template = yt_prompt_fake_or_real4,
  model           = model_qwen2514b
) # 14261 comments in 11995.5s (0.8s per comment)

# save(classification_yt_job1, file = "classification_yt_job1.RData")
