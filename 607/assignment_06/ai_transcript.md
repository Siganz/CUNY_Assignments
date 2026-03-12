**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, explaining book recommendations, identifying authors and publication details, and showing example data structures in Markdown, HTML, and JSON. All submitted writing, code, and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed March 12, 2026.

---

### Q: What are the structures for HTML and JSON? Is there like a reference I can use somewhere?

**A:**  
Yes.

- **HTML** is used to structure documents and web pages.  
- **JSON** is used to structure data.

A basic **HTML** structure looks like this:

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>My Page</title>
  </head>
  <body>
    <h1>Hello</h1>
    <p>This is a paragraph.</p>
  </body>
</html>
```

A basic **JSON** structure looks like this:

```json
{
  "title": "Mathematics for Machine Learning",
  "authors": [
    "Marc Peter Deisenroth",
    "A. Aldo Faisal",
    "Cheng Soon Ong"
  ],
  "publication_year": 2020,
  "publisher": "Cambridge University Press"
}
```

Useful references:

- **HTML**: MDN Web Docs
- **JSON**: RFC 8259 and JSON.org

---

### Q: What packages read HTML and JSON into a data frame in R? Does tidyverse have a built-in solution? If yes, how?

**A:**  
For reading these formats in R:

- **JSON:** use `jsonlite::fromJSON()`
- **HTML:** use `rvest::read_html()` and then either `html_table()` for real HTML tables, or `html_elements()` / `html_text2()` if you need to scrape page content

---

### Q: What HTML structure is easiest to turn into a data frame in R?

**A:**  
A real HTML table is the most direct fit, because it already mirrors tabular data. In that setup:

- the table represents the dataset
- each row represents one observation
- header cells represent column names
- data cells represent values

That structure is the cleanest match for how R stores rectangular data.

---

### Q: If the HTML uses spans and classes instead of a table, does that still work?

**A:**  
Yes, but it is less direct. Instead of reading the whole thing as a table, you would treat the page more like a collection of labeled pieces and pull out each field separately. That works fine when the markup is consistent, but it requires more scraping logic because the HTML is describing page content, not formal tabular structure.

---

### Q: Why is a table considered “cleaner” for this kind of assignment?

**A:**  
Because the data is already organized in rows and columns before R ever touches it. That means less interpretation is needed. With span-based markup, you first have to identify which pieces belong to title, authors, publisher, year, and so on, then rebuild the rows yourself. Same destination. More assembly.

---

### Q: Why wouldn’t paragraph tags and bold labels be ideal for this?

**A:**  
Because that format is mainly visual. It helps a person read the page, but it does not clearly tell a parser what counts as a field versus what counts as just text. In other words, it looks organized to a human, but it is not especially structured for data extraction.

---

### Q: What does it mean when people say a table “maps directly” to a data frame?

**A:**  
It means the structure lines up almost one-to-one. A row in the HTML becomes a row in the data frame, and each column stays a column. There is very little ambiguity. That is why table-based HTML is usually the path of least resistance here.

---

### Q: What role does JSON play differently from HTML in this assignment?

**A:**  
JSON is already designed to represent data as nested objects and arrays, so it usually behaves more like a data container than a webpage. HTML, by contrast, is a markup language for structuring content on a page. So JSON tends to feel more naturally “data-shaped,” while HTML can be either data-shaped or presentation-shaped depending on how it is written.

---

### Q: Does leading whitespace or an empty first line break JSON?

**A:**  
No. Extra spaces and blank lines outside the actual values are generally ignored by JSON parsers. It may look a little sloppy, but it does not usually change whether the file is valid.

---

### Q: Why would a year field sometimes need to be converted after reading HTML?

**A:**  
Because scraped HTML text is often read in as character data first. Even if something looks numeric on the page, it may still arrive as text when extracted. So a year can look like a number but still be stored as a string until it is explicitly treated as numeric.

---

### Q: Why does scraping span-based HTML feel more confusing inside a tibble workflow?

**A:**  
Because each column is being built from its own extraction process. That can feel odd at first since it looks like multiple little pipelines are happening inside one object constructor. But really, each field is just being gathered separately and then assembled side by side into columns. It is less magical than it looks. Just more nested than it needs to be.

---

### Q: When would selector-based HTML parsing make sense over a table?

**A:**  
When the HTML is not actually tabular to begin with. If the page is built from repeated content blocks—like cards, spans, divs, or article sections—then selector-based extraction is the right tool. It is not worse. It is just more manual because you are scraping structure out of layout rather than reading a ready-made table.