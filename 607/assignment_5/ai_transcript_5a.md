**Generative AI Usage Disclosure**  
I used Grok (built by xAI) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
xAI. (2026). *Grok* [Large language model]. https://grok.x.ai. Accessed February 23, 2026.

## Q&A Appendix — Airline Delays

---

### Q: When I convert a tibble without that has no value for two columns, is it ...1 and ...2? Is that the actual value for the column name?

**A:**  
Yes — when you coerce something like a matrix, list, or unnamed data without column names to a tibble (via `as_tibble()` or similar), unnamed columns get automatic names `...1`, `...2`, etc. Those are the literal, real column names in the resulting tibble.

---

### Q: how do i position labels correctly for a stacked column?

**A:**  
Use `geom_text()` or `geom_label()` with `position = position_stack(vjust = 0.5)` for middle-of-segment placement. For more control, pre-calculate the cumulative y-position (e.g., `mutate(label_y = cumsum(value) - 0.5 * value)`) and map `y = label_y` in `geom_text()`. Tweak `vjust`/`hjust` or `nudge_y` if needed.

---

### Q: What does .groups do within a summarize?

**A:**  
Controls whether the output stays grouped and how. Options:  
- `"drop_last"` (default for grouped df): drops the last grouping variable  
- `"drop"`: removes all groups  
- `"keep"`: preserves original groups  
- `"rowwise"`: rare, makes it rowwise  
Use it to silence the regrouping message or force ungrouped output.

---

### Q: AM WEST has a higher overall on time % than Alaska, but Alaska has a higher on time % for all the cities (total). How does that make sense.

**A:**  
Classic Simpson's paradox. Alaska beats AM West in every single city, but AM West flies way more flights out of Phoenix (easy weather, high on-time rate for both), while Alaska flies mostly out of tougher spots like Seattle. The unequal group sizes (city/route distribution) flip the weighted overall average, even though Alaska wins every subgroup.

---