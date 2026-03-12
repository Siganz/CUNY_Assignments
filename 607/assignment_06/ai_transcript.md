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