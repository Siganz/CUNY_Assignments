## Generative AI Usage Disclosure

I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning related to classification models and data visualization. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
OpenAI. (2026). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed February 5, 2026.

---

### Q: Why did my ggplot bar chart show percentages far above 100%?

**A:**  
Because proportions were being computed more than once. The plot used `after_stat(prop)` (already scaled to [0,1]) and then applied percentage formatting or additional normalization, resulting in a “double-percent” error. The fix was to compute proportions a single time and plot the resulting values directly.

---

### Q: Why was using `after_stat()` confusing in this case?

**A:**  
`after_stat()` performs implicit calculations inside ggplot, which can obscure how values are derived. This makes it easier to accidentally normalize or scale data multiple times. Precomputing values makes transformations explicit and easier to reason about.

---

### Q: Why did ChatGPT recommend precomputing proportions before plotting?

**A:**  
Precomputing proportions avoids hidden statistical transformations, prevents double-counting errors, and keeps plotting declarative. The recommended pattern is: summarize once, plot simple values, then format labels last.

---

### Q: Why was my y-axis labeled “Count” when I was plotting proportions?

**A:**  
Because the axis label did not match the data being displayed. Although the plot showed proportions, the label still implied raw counts. Axis labels should always reflect the actual quantity being plotted to avoid semantic errors.

---

### Q: How do I correctly show percentages from 0 to 100 on the y-axis?

**A:**  
Convert proportions to percentages during data preparation, then plot those values directly and set axis limits and breaks explicitly (e.g., 0–100). This avoids formatter confusion and ensures the axis literally represents percentages.

---

### Q: What is the difference between `count()` and `add_count()` in dplyr?

**A:**  
`count()` aggregates data and returns one row per group, dropping all other columns.  
`add_count()` preserves all original rows and appends a count column. The choice depends on whether you want a summary table or to annotate the existing dataset.

---

### Q: Why did my dataframe end up with only two columns after using `count()`?

**A:**  
Because `count()` collapses the data to the grouping variable(s) and a single count column. This is expected behavior since it performs aggregation rather than annotation.

---

### Q: Does assigning `df2 <- df` create a full copy in R?

**A:**  
Yes, conceptually. R uses copy-on-modify semantics: both objects reference the same data until one is modified, at which point R automatically creates a copy. This behavior is sufficient for most analytical workflows.

---

### Q: What is a confusion matrix?

**A:**  
A confusion matrix compares predicted classes to actual classes and reports the counts of true positives, false positives, true negatives, and false negatives. These counts form the basis of all standard classification metrics.

---

### Q: Is this a valid confusion matrix?

**A:**  
Yes. The counts are placed in the correct cells, and all values sum to the total number of observations.

---

### Q: Why is accuracy misleading in imbalanced datasets?

**A:**  
Accuracy combines true positives and true negatives into a single value and hides how errors are distributed. A model can achieve high accuracy by always predicting the majority class while performing poorly on the minority class.

---

### Q: What is the null error rate?

**A:**  
The null error rate is the error rate of a model that always predicts the majority class. It represents the best possible performance achievable without using any features and serves as a baseline that any meaningful model must outperform.

---

### Q: Why doesn’t the null error rate “go either way”?

**A:**  
By definition, the null model predicts the majority class because this minimizes error. Predicting the minority class would intentionally increase error and does not constitute a meaningful baseline.

---

### Q: Is there a single mathematical measure of validity?

**A:**  
No. Validity concerns whether a measure captures the intended concept, which cannot be reduced to a single statistic. Quantitative tools can support validity arguments, but theoretical context and domain knowledge are required to interpret them.

---

### Q: Why separate computation from visualization?

**A:**  
Separating data preparation from plotting improves clarity, reduces side effects, and makes debugging easier. Computation functions should return clean data; visualization functions should only consume and display that data.

---
