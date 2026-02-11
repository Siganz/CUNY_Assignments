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

### Q: What does the `get_coin_range()` wrapper do?

**A:**  
It abstracts the boilerplate:
- Accepts coin, date range, currency
- Calls the API
- Parses JSON
- Converts nested lists into tidy rows (`map_dfr()`)

Pipeline:
```
API → JSON → list → rows → tibble
```

---

### Q: How should API keys and DB credentials be handled?
**A:**  
Never hardcode them. Use environment variables.

---

### Q: What is a `.env` file?
**A:**  
A plain text file containing key–value pairs:

```
KEY=value
```

Quotes are only needed if the value contains spaces.

---

### Q: What’s the difference between `.env` and `.Renviron`?
**A:**  
- `.Renviron` → auto-loaded by R  
- `.env` → requires `load_dot_env()`

---

### Q: What inputs does `dbConnect()` need for Postgres?

**A:**  
Driver + connection metadata, typically sourced from environment variables:

```r
dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = 5432,
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
```

---

### Q: What architecture did this pipeline implement?

**A:**  
A clean, reproducible ingest pattern:

1. Secure secrets  
2. Wrap external API  
3. Pull BTC / ETH / SOL  
4. Normalize into one table  
5. Apply window analytics  
6. Optionally persist to Postgres  

In short: **ingest → tidy → analyze → store**.
