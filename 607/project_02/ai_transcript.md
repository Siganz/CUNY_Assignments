## Generative AI Usage Disclosure

I used xAI’s Grok (Version 4.20) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
xAI. (2026). *Grok* (Version 4.20) [Large language model]. https://grok.com. Accessed March 5, 2026.
---

### Q: Why doesn’t the regex `[:-=]` work as expected?

**A:**  
Inside `[]`, the hyphen is treated as a **range operator** unless escaped or placed at an edge.  
`[:-=]` is interpreted as the character range from `:` to `=`.

Fix it by escaping or repositioning the hyphen:

- `[-:=]`
- `[\\-:=]`

Both match the literal characters `:`, `-`, or `=`.

---

### Q: How do we replace multiple values (e.g., "-", "missing", "", "n/a") with `NA` using a vector like `c()`?

**A:**  
Use `%in%` with `replace()` inside `mutate()` to match any value in the vector.

```r
mutate(values = replace(values, values %in% c("-", "missing", "", "n/a"), NA))
```
---

### Q: In `mutate()`, should I use `+` or `rowSums()` to add column values per row?

**A:**  
Use `rowSums()`. It’s built for row-wise addition across multiple columns and handles `NA` cleanly.

```r
mutate(total = rowSums(across(c(col1, col2, col3)), na.rm = TRUE))
```

---

### Q: How do you correctly order categorical values like "low", "mid", "high"?

**A:**  
Convert the column to a factor and explicitly define the level order.  
By default, R sorts character values alphabetically, which would incorrectly place `"high"` before `"low"` and `"mid"`. Defining factor levels ensures plots and summaries follow the intended logical order.

---

### Q: What are good `ggplot` geoms for visualizing relationships involving two or more variables?

**A:**  
Common choices include:

- **Bar charts (`geom_col` / `geom_bar`)** — compare counts or totals across categories.
- **Line charts (`geom_line`)** — show trends over an ordered variable such as time.
- **Point plots (`geom_point`)** — display relationships between two numeric variables.
- **Boxplots (`geom_boxplot`)** — compare distributions of a numeric variable across categories.
- **Heatmaps (`geom_tile`)** — show intensity or magnitude across two categorical dimensions.
- **Grouped or stacked bars** — useful when comparing multiple categories within another grouping variable.

The choice depends on whether you want to highlight **comparisons, trends, distributions, or relationships** between variables.
---