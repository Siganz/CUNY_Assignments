## Generative AI Usage Disclosure

I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
OpenAI. (2026). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed February 11, 2026.


## Q&A Appendix — API, Windows, and Data Pipeline

---

### Q: What is a window function?

**A:**  
A window function computes over a related set of rows **without collapsing them**. Each row keeps its identity while borrowing context from nearby rows (e.g., moving averages, cumulative sums, ranks).

---

### Q: Does a 6-day window include the current day?

**A:**  
Yes.  
`ROWS BETWEEN 5 PRECEDING AND CURRENT ROW` defines a 6-row trailing window: the current row plus the five before it.

---

### Q: What does YTD actually mean?

**A:**  
Year-to-date is a **running average** from January 1 to the current date, not a simple per-day mean.

---

### Q: Why does YTD use `cumsum()`?

**A:**  
Because a running average is literally:

```
YTD average = cumulative sum / row index
```

`cumsum()` gives the numerator; `row_number()` gives the denominator.

---

### Q: What’s the difference between `.env` and `.Renviron`?
**A:**  
- `.Renviron` → auto-loaded by R  
- `.env` → requires `load_dot_env()`

---

### Q: What problem is a *global baseline estimate* trying to solve?

**A:**  
Missing ratings. Specifically: *what’s a reasonable default guess* when a user hasn’t rated an item yet.  
Baseline models give you a defensible first approximation before doing anything fancy.

---

### Q: What is the **global mean**?

**A:**  
The average of *all observed ratings*:

\[
\mu = \frac{1}{N}\sum_{(u,i)\in R} r_{ui}
\]

It answers: *“On average, how do people rate things in this system?”*

---

### Q: Why not just use the global mean for everything?

**A:**  
Because users and items are biased.

- Some users rate high → generous
- Some items are widely liked → strong pull upward

The global mean ignores systematic structure.

---

### Q: What is a **rater effect** (a.k.a. user effect / row effect)?

**A:**  
How much a user deviates from the global mean *on average*.

\[
b_u = \bar{r}_u - \mu
\]

Interpretation:
- Positive → generous rater
- Negative → harsh rater

---

### Q: What is an **item effect** (a.k.a. column effect)?

**A:**  
How much an item deviates from the global mean *on average*.

\[
b_i = \bar{r}_i - \mu
\]

Interpretation:
- Positive → broadly liked item
- Negative → broadly disliked item

---

### Q: Why “row effect” vs “column effect”?

**A:**  
It’s matrix language.

- Rows = users (raters)
- Columns = items

Same math, different framing. In recommender systems, “user effect” and “item effect” are more common.

---

### Q: What is the **baseline prediction equation**?

**A:**  

\[
\hat{r}_{ui} = \mu + b_u + b_i
\]

This is the **global baseline estimate**.

---

### Q: What does this equation assume?

**A:**  
Additivity and independence:
- User bias and item bias are separable
- No interaction effects
- No personalization beyond averages

It’s deliberately simple.

---

### Q: Why is this still useful if it’s so simple?

**A:**  
Because it’s:
- interpretable
- stable
- hard to overfit
- fast to compute
- a strong benchmark

Many “advanced” models barely beat a good baseline.

---

### Q: Where does **expected value** come in?

**A:**  
The baseline prediction is the **expected rating** given only:
- who the user is
- what the item is

Formally:
\[
\mathbb{E}[R_{ui} \mid u, i] \approx \mu + b_u + b_i
\]

---

### Q: How did you estimate the rater and item effects in practice?

**A:**  
By empirical means:
- compute each user’s mean rating
- compute each item’s mean rating
- subtract the global mean

This is the *unregularized* version.

---

### Q: Why is this technically risky?

**A:**  
Sparse data.

- Users with 1 rating get extreme effects
- Items with few ratings get noisy estimates

In production, you’d regularize.

---

### Q: What does regularization look like conceptually?

**A:**  
Shrink effects toward zero:

\[
b_u = \frac{\sum_i (r_{ui} - \mu)}{\lambda + n_u}
\]

Same idea for items.  
More data → less shrinkage.

---

### Q: If user's has no data, why is `rater_effect = 0` not  `rater_effect = NA`?

**A:**  
No data ⇒ no deviation signal.

So the safest assumption is:
\[
b_u = 0
\]

They’re treated as “average.”

---

### Q: Why does that make sense statistically?

**A:**  
Zero is the *prior mean* of the effect.  
Without evidence, don’t invent bias.

---

### Q: What does the model predict for a brand-new user?

**A:**  
\[
\hat{r}_{ui} = \mu + b_i
\]

Only item popularity matters.

---

### Q: What does it predict for a brand-new item?

**A:**  
\[
\hat{r}_{ui} = \mu + b_u
\]

Only user generosity matters.

---

### Q: When does this model completely fail?

**A:**  
When:
- interactions dominate (taste-specific effects)
- users behave inconsistently
- ratings are non-additive
- context matters (time, mood, genre)

That’s when you move to matrix factorization or neural models.

---

### Q: Why do recommender systems *still* start here?

**A:**  
Because every serious model decomposes into:

\[
\text{baseline} + \text{interaction residual}
\]

If your baseline is wrong, everything on top is garbage.

---

### Q: So, what's the base summary of it?

**A:**  
Global baseline estimates are:
- the statistical “idle speed”
- the null model you must beat
- the clean separation of bias from signal

They’re boring — and indispensable.
