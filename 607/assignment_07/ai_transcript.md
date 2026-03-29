**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment.

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed March 29, 2026.

---
### Q: How to set up a secret env variable for an API key in R

**A:**  
Use an `.Renviron` file. For a project-specific secret, put the variable in a `.Renviron` file in your project folder. For a user-wide secret, put it in your home directory. R reads that file at startup and loads the values as environment variables.

---

### Q: Does it need a library? 

**A:**  
No. R handles `.Renviron` itself. A package like `usethis` is optional convenience, not required.

---

### Q: Why R didn’t auto-read `.Renviron`

**A:**  
Because `.Renviron` is read when the R session starts. If you create or edit it after R is already open, R does not automatically reload it.

---

### Q: `Sys.getenv()`or can I just call the variable

**A:**  
No. You only use `Sys.getenv()` when you need to read the value inside your R code. Some packages will look for the environment variable automatically and use it without you calling anything.

---

### Q: whether you can paste the API key instead of using `httr`

**A:**  
Yes, you can paste the key directly into the URL and it will work. It is just not a good long-term habit because it exposes the secret in your script and makes leaks easier.

---

### Q: how to arrange a bar chart by count

**A:**  
Count the categories first, then reorder the category variable by that count before plotting. That way the bars appear sorted instead of alphabetically.

---

### Q: how to make a horizontal `geom_col()`

**A:**  
Use a coordinate flip so the axes are swapped. That turns a vertical column chart into a horizontal one.

---

### Q: how to remove the legend

**A:**  
Set the legend position to none in the plot theme. That removes the legend entirely.