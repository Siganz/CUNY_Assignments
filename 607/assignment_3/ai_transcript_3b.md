## Generative AI Usage Disclosure

I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
OpenAI. (2026). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed February 11, 2026.

---

## Q&A Appendix — Global Baseline / Variance Decomposition

### Q: What’s the English explanation of the Global Baseline Estimate?

**A:**  
Start with the overall average. Adjust for how the row entity typically deviates. Adjust for how the column entity typically deviates. The result is the expected value before modeling interactions.

---

### Q: What is the concept of a global baseline estimate?

**A:**  
A global baseline estimate is an additive model that explains an observation using the grand mean plus systematic row and column effects. It captures structural bias, not individual taste.

---

### Q: What do “row effect” and “column effect” mean? Is that canon?

**A:**  
They’re standard fixed effects.  
- **Row effect**: how a row entity consistently deviates from the global mean  
- **Column effect**: how a column entity consistently deviates from the global mean  
This is canonical in ANOVA, fixed-effects regression, and recommender systems.

---

### Q: What is variance decomposition?

**A:**  
Breaking total variation into explainable components (row effects, column effects) and residual noise. You’re identifying where systematic structure lives versus randomness.

---

### Q: So I’m breaking variance into parts and using them to predict?

**A:**  
Yes. You model the predictable structure (mean + effects) and ignore the residual, because noise is not forecastable.

---

### Q: Does this idea extend beyond dataframes?


**A:**  
Yes. It applies to any system with layered structure: panel data, experiments, economics, recommender systems, even physical measurements. Rows and columns are just one instance of structure.

---

### Q: Is  
`SS_Total = SS_Rows + SS_Columns + SS_Residual`  
actually a thing?

**A:**  
Yes. That’s the backbone of two-way ANOVA. It formally partitions total variation into row effects, column effects, and unexplained noise.

---

### Q: Should I separate row-based and column-based summaries into different dataframes?

**A:**  
Yes. Compute row means/effects and column means/effects separately, then join them back to the original data. This keeps the logic clean and avoids double-counting.

---

### Q: Am I wrong that this can be recreated exactly in R?

**A:**  
No. Excel is implicitly doing fixed-effects decomposition. R can reproduce the same math explicitly and more transparently.

---

### Q: Is the Global Baseline Estimate used to predict missing values?

**A:**  
Yes. It’s a baseline predictor. It fills missing cells using only structural bias (row + column + global mean), before any similarity or interaction modeling.

---

### Q: Should a prediction function take `(df, name, title)`?

**A:**  
Yes—if you’re predicting a single cell. That makes the inputs explicit and avoids fragile assumptions about matrix layout.

---

### Q: How do I join components to compute the GBE?

**A:**  
Join row effects by the row key and column effects by the column key, then compute:  
**GBE = global_mean + row_effect + column_effect**

---

### Q: Why isn’t GBE just “+++”? Why is there a minus?

**A:**  
Because row means and column means both already include the global mean. Subtracting one copy prevents double-counting. Algebraically:  
**row_mean + col_mean − global_mean**

---

### Q: Is the minus dependent on the value itself?

**A:**  
No. The subtraction is part of defining “relative to average.” Once you precompute effects, the final formula is purely additive.

---

### Q: What is UNIX time?

**A:**  
A timestamp stored as “seconds since 1970-01-01 00:00:00 UTC.” In your data, CoinGecko returns **milliseconds**, so you divide by **1000** before converting to datetime.

---

### Q: What is `POSIXct` (and why do I keep seeing it)?

**A:**  
It’s R’s common datetime storage type: a numeric count of seconds since the UNIX epoch plus timezone metadata. It’s the practical “workhorse” datetime class.

---

### Q: What does `as.POSIXct(start, tz = "UTC")` do here?

**A:**  
It parses your `"YYYY-MM-DD"` string into a UTC datetime so you can convert it to a numeric epoch (`from`) for the API query.

---

### Q: What is `request()` / `req_*()` / `resp_*()`?

**A:**  
That’s **httr2**’s pipeline style:
- `request(url)` creates a request object
- `req_headers()`, `req_url_query()`, `req_throttle()`, `req_retry()` modify it
- `req_perform()` sends it
- `resp_body_json()` parses the response body into R objects

---

### Q: What is `req_retry()`?

**A:**  
It retries transient failures up to `max_tries = n`, but only for status codes you call “transient” (408, 429, 5xx). That’s the right idea: don’t retry on “you messed up.”

---

### Q: What is `resp_status(resp)`?

**A:**  
It extracts the HTTP status code (e.g., 200, 401, 429, 503) so you can decide whether to retry.

---

### Q: What does `resp_body_json()` return?

**A:**  
Parsed JSON as an R list. For this endpoint, you use `resp$prices`, which is a list of `[timestamp_ms, price]` pairs.

---

### Q: What is `purrr::map_dfr`?

**A:**  
It maps each price pair, converts it into a one-row tibble, then row-binds them all into one data frame.

---

### Q: Difference between `map()`, `map_dfr()`, and `walk()`?

**A:**  
- `map()` → returns a list
- `map_dfr()` → returns a single data frame (row-binds outputs)
- `walk()` → returns nothing useful; use for “do this for each element”

---

### Q: Why `unlist(use.names = FALSE)` on `paths`?

**A:**  
Because `map()` gives you a list-of-vectors. `unlist()` flattens it into one character vector of file paths.

---