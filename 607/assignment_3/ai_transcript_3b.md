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
