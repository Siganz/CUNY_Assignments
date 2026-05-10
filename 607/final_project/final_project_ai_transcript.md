## Generative AI Usage Disclosure

I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment.

## Citation

OpenAI. (2026). ChatGPT [Large language model]. Accessed May 10, 2026.

---

## Q:

For the NYC DOHMH restaurant inspection dataset, if the most recent inspection has a blank grade, can I look back to the previous non-missing grade?

## A:

Yes. Recent inspection rows do not always include a posted grade, so using the most recent non-missing grade by restaurant ID is a reasonable way to represent the current available grade in this dataset.

---

## Q:

What does the ~ mean in case_when()?

## A:

In case_when(), the ~ separates the condition from the value returned when that condition is true. For example, critical == "Critical" ~ 2 means that rows marked Critical should receive a value of 2.

---

## Q:

How should I create the severity score from the DOHMH critical flag?

## A:

I mapped the critical flag into a numeric severity score. Critical violations received 2, Not Critical violations received 1, Not Applicable received 0, and anything else was treated as missing. This let me summarize not only how many violations a restaurant had, but also how serious those violations were.

---

## Q:

Why use set.seed() with slice_sample()?

## A:

slice_sample() randomly selects rows from the data. I used set.seed(123) before sampling so the same random sample of 10,000 non-NYT restaurants would be selected each time the code was run. This made the analysis reproducible.

---

## Q:

What does this sampling step do?

## A:

The code takes a random sample of 10,000 restaurants from the non-NYT restaurant pool, then labels those restaurants as non_nyt. This creates the comparison group used against the NYT restaurants.

---

## Q:

What does transmute() do?

## A:

transmute() creates a new cleaned dataframe using only the variables listed inside it. It is similar to mutate(), but it drops columns that are not explicitly kept or created. I used it when cleaning the NYT restaurant list because I only needed rank, restaurant name, neighborhood, cuisine, and NYT group.

---

## Q:

Can I combine the join, date conversion, factor ordering, and final variable creation into one pipeline?

## A:

Yes. Those steps can be combined into the final analysis dataframe pipeline, since they are all part of preparing the dataset for analysis.

---

## Q:

What is happening in the latest inspection summary code?

## A:

That code groups the latest inspection rows by restaurant and creates restaurant-level inspection summaries. It counts critical violations, counts non-critical violations, counts total violations, and combines the violation codes into one semicolon-separated list. The .groups = "drop" argument removes the grouping after the summary is created.

---

## Q:

Should I set null and alternative hypotheses for the analysis?

## A:

Yes. I used one hypothesis test for inspection grades and three related hypothesis tests for inspection outcomes.

For inspection grades, the null hypothesis was that NYT and non-NYT restaurants have the same proportion of A grades. The alternative hypothesis was that the proportions are different.

For inspection outcomes, I tested total violations, critical violations, and severity scores. For each, the null hypothesis was that NYT and non-NYT restaurants have the same distribution. The alternative hypothesis was that the distributions are different.

---

## Q:

Should I use a chi-square test or Wilcoxon test?

## A:

Use the chi-square test for the categorical grade comparison, A grade versus Not A. Use Wilcoxon rank sum tests for numeric inspection outcomes such as total violations, critical violations, and severity scores.

---

## Q:

How should I interpret the chi-square result?

## A:

The chi-square test produced X²(1) = 3.52, p = 0.061. Since the p-value is greater than 0.05, I fail to reject the null hypothesis. Although NYT restaurants had a lower A-grade rate than non-NYT restaurants, the difference was not statistically significant at the 0.05 level.

---

## Q:

How should I interpret the Wilcoxon test results?

## A:

For total violations, the Wilcoxon rank sum test was not statistically significant, W = 393565, p = 0.300. I fail to reject the null hypothesis.

For critical violations, the test was statistically significant, W = 357093, p = 0.009. I reject the null hypothesis. NYT restaurants had more critical violations on average than non-NYT restaurants.

For severity scores, the test was statistically significant, W = 368117, p = 0.046. I reject the null hypothesis. NYT restaurants had higher severity scores on average than non-NYT restaurants.

---

## Q:

How should I summarize the final conclusion?

## A:

Overall, the results do not support the idea that NYT-recognized restaurants have better health inspection outcomes than non-NYT restaurants. A-grade rates and total violations were not significantly different, while critical violations and severity scores were significantly higher for NYT restaurants.

---

## Q:

My DOHMH CSV is too large for GitHub. Can I compress it?

## A:

Yes. Compressing the CSV as a .csv.gz file keeps the data in the repository while reducing file size. R can still read the compressed CSV directly.