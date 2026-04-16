**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed April 16, 2026.

---

### Q: list within a list within a list. Can you do like df$nobelPrizes$Laureates$fullName?

**A:**  
Not like that if those pieces are **lists of multiple items**.

`$` only works cleanly when you’re accessing a **named element of one list/data frame**, not when you have a whole column/list of many records.

So this usually fails or won’t do what you want:

```r
df$nobelPrizes$laureates$fullName
```

because:

* `df$nobelPrizes` is probably a **list-column** or list of prize records
* each prize has its own `laureates`
* each laureate has its own `fullName`

You usually need `[[ ]]` to drill into one specific element.

Example shape:

```r
x$nobelPrizes[[1]]$laureates[[1]]$fullName$en
```

That means:

* first prize
* first laureate in that prize
* full name
* English version

So if you want the first laureate full name from the first prize:

```r
x$nobelPrizes[[1]]$laureates[[1]]$fullName$en
```

If you want all full names from the first prize, use something like:

```r
sapply(x$nobelPrizes[[1]]$laureates, function(z) z$fullName$en)
```

Rule of thumb:

* `$name` = grab a named part
* `[[1]]` = grab one specific element
* `[1]` = keep it as a smaller list

So yes, you can chain accessors, but for nested repeated lists it’s usually:

```r
x$nobelPrizes[[1]]$laureates[[1]]$fullName$en
```

not the flat dataframe-style version.

---

### Q: Rows: 25 Columns: 8 $ awardYear <chr> "1901", "1901", "1901", "1901", "1901", "1902", "1902", "1902", "1902", "1902", … $ category <df[,3]> <data.frame[25 x 3]> $ categoryFullName <df[,3]> <data.frame[25 x 3]> $ dateAwarded <chr> "1901-11-12", "1901-11-14", "1901-12-10", "1901-11-12", "1901-10-30", "1902-1… $ prizeAmount <int> 150782, 150782, 150782, 150782, 150782, 141847, 141847, 141847, 141847, 14184… $ prizeAmountAdjusted <int> 10833458, 10833458, 10833458, 10833458, 10833458, 10191492, 10191492, 10191492, … $ links <list> [<data.frame[1 x 4]>], [<data.frame[1 x 4]>], [<data.frame[1 x 4]>], [<data.fram… $ laureates <list> [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<data.frame[2 x 7]>], [<data.fram…
**A:** 
Yep. Annoying, but normal.

You have:

* a regular data frame at the top
* and then **list-columns** inside it, especially `laureates`

So each row is one prize, and `laureates` is a little data frame stored inside that row.

The fix is usually to **unnest**.

Try:

```r
library(dplyr)
library(tidyr)

laureates_tbl <- df_prizes |>
  unnest(laureates)
```

That should give you one row per laureate.

Then if you want cleaner columns:

```r
laureates_tbl <- df_prizes |>
  unnest(laureates) |>
  mutate(
    category = category$en
  )
```

But because `category` is also nested weirdly, better is:

```r
clean_tbl <- df_prizes |>
  mutate(
    category_en = category$en,
    category_full_en = categoryFullName$en
  ) |>
  select(awardYear, category_en, category_full_en, dateAwarded, laureates) |>
  unnest(laureates)
```

If that gives trouble, use base extraction first:

```r
df_prizes$category_en <- df_prizes$category[, "en"]
df_prizes$category_full_en <- df_prizes$categoryFullName[, "en"]

clean_tbl <- df_prizes |>
  select(awardYear, category_en, category_full_en, dateAwarded, laureates) |>
  unnest(laureates)
```

That’s the main idea:

* `category` is a nested data frame column
* `laureates` is a list-column of small data frames
* flatten the easy columns first
* then `unnest(laureates)`

If you only want names, even simpler:

```r
names_tbl <- df_prizes |>
  mutate(category_en = category[, "en"]) |>
  select(awardYear, category_en, laureates) |>
  unnest(laureates) |>
  select(awardYear, category_en, fullName.en)
```

So yes, the structure is ugly, but it’s not broken. It just needs flattening.
---
