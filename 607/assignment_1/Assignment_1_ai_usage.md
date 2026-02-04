## Generative AI Usage Disclosure
I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. Each deliverable used for the assignment will have a section below, including brief commentary and relevant QA.

**Citation**:  
OpenAI. (2025). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed January 26, 2026.

---

## Approach
I am new to R, RStudio, Rpubs, Git/Github, so I had a bunch of general questions to help me with the approach deliverable. 

**Q:** Should I use QMD or RMD for this assignment?  
**A:** QMD is preferred. Quarto is the current standard, supports the same outputs as R Markdown, and is actively developed. RMD still works, but new work should use QMD.

**Q:** Does RPubs use the title from the YAML automatically?  
**A:** Yes. The `title:` field in YAML is rendered as the top-level heading. You should not manually add a `#` title in the body.

**Q:** How do I load and preview data fields in a QMD? Do I just code in R?  
**A:** Yes. Load the data in an executable R code chunk using `read.csv()` with a URL, then display fields using functions like `colnames(df)` or `head(df[ , c("ColumnA", "ColumnB")])`.

**Q:** Should I reference columns by name or by position?  
**A:** Column names should be used in submitted work because they are more stable and readable. Positional indexing is acceptable only for quick exploration.

**Q:** How do I hide code blocks in the rendered document?  
**A:** Use chunk options such as `#| echo: false` to hide code while keeping output visible.

**Q:** Why did my Git push fail with a non-fast-forward error?  
**A:** The local branch was behind the remote branch. The correct workflow is to commit local changes, pull from the remote branch, resolve any conflicts, and then push.

**Q:** What do merge conflict markers like `<<<<<<< HEAD` mean?  
**A:** They indicate a merge conflict where Git cannot automatically reconcile differences. The user must manually choose which content to keep, remove the markers, and commit the resolution.

## Codebase

ChatGPT was used to assist with understanding R syntax, data-type coercion, vectorized operations, and ggplot2 structure during development of the codebase. The interaction focused on reasoning through transformations and visualization patterns rather than generating final results.

**Q:** How should character `"true"` / `"false"` values be converted to logical in R?  
**A:** Normalize casing using `toupper()`, then convert using `as.logical()`.

**Q:** How can multiple logical columns be combined into a single “activity” indicator?  
**A:** Use `rowSums()` across the logical columns and compare the result to `> 0` to produce a logical value.

**Q:** How can empty strings in categorical fields be handled safely?  
**A:** Replace empty strings with `NA` to avoid treating missing values as valid categories.

**Q:** How should mixed character and numeric values be handled prior to numeric coercion?  
**A:** Identify and normalize non-numeric sentinel values (e.g., `""`, `"FALSE"`) before applying `as.numeric()`.

**Q:** How can non-numeric values be detected before conversion?  
**A:** Use regular expressions with `grepl()` to locate rows containing non-numeric characters.

**Q:** What caused the `NAs introduced by coercion` warning?  
**A:** Non-numeric values remained in the column at the time of coercion and needed to be normalized first.

**Q:** What is the difference between `is.numeric()` and `as.numeric()`?  
**A:** `is.numeric()` performs a type check, while `as.numeric()` converts values.

**Q:** How can logical values be aggregated for summary statistics?  
**A:** Logical values can be summed (`TRUE` = 1) or averaged to calculate rates.

**Q:** How were bar charts created using ggplot2?  
**A:** Guidance was provided on structuring `ggplot()` calls, including `aes()`, `geom_col()`, labeling, faceting, and axis scaling, while all plots were implemented and adjusted by the author.

**Q:** How were facet plots used to compare behaviors?  
**A:** Faceting (`facet_wrap()`) was used to visualize behavior rates across categories using a consistent scale.

**Q:** How were aggregation functions chosen for reporting?  
**A:** Guidance helped determine when to use `sum()` versus `mean()` depending on whether counts or rates were desired.

## Video
