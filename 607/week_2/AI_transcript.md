## Generative AI Usage Disclosure
I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. Each deliverable used for the assignment will have a section below, including brief commentary and relevant QA.

**Citation**:  
OpenAI. (2025). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed January 26, 2026.

---

**Q:** Why were foreign keys (`FOREIGN KEY`) used in the database design?  
**A:** Foreign keys enforce referential integrity by ensuring that each rating references a valid person and a valid movie. This prevents orphan records and keeps relationships consistent across tables.

**Q:** Why are `person_id` and `movie_id` defined as `INT` in the `ratings` table instead of `SERIAL`?  
**A:** `SERIAL` generates new values automatically. In the `ratings` table, these columns must reference existing IDs from the parent tables, so they are defined as `INT` to store references rather than generate new identifiers.

**Q:** Why was the join performed in SQL rather than entirely in R?  
**A:** SQL is responsible for enforcing relational logic and data integrity, while R is used for analysis and visualization. Performing the join in SQL ensures a clean, authoritative dataset before analysis.

**Q:** What is the purpose of creating a database view for the joined data?  
**A:** The view stores the join logic as a reusable, queryable object without duplicating data. It simplifies downstream analysis by exposing a single, analysis-ready dataset.

**Q:** Does the view store any data physically?  
**A:** No. A standard view stores only the query definition. The underlying data remains in the base tables and is resolved at query time.

**Q:** How does this design support reproducibility?  
**A:** The schema, constraints, and view definitions are all captured in SQL scripts, allowing the database and analysis pipeline to be recreated consistently across environments.

**Q:** Why must child tables be truncated before parent tables?  
**A:** Because child tables contain foreign keys referencing parent tables. Truncating a parent table first would violate referential integrity. PostgreSQL enforces this even when tables are empty.

**Q:** Why was `TRUNCATE` applied to multiple tables at once?  
**A:** Truncating related tables together allows PostgreSQL to resolve foreign key dependencies safely and reset all tables in a consistent state.

**Q:** What is `CASCADE`, and why is it used with `DROP` statements?  
**A:** `CASCADE` tells PostgreSQL to automatically drop dependent objects (such as tables, views, or foreign key constraints) when a parent object is dropped. It is useful when resetting a schema that contains interrelated tables.

**Q:** How is `CASCADE` different from `TRUNCATE`?  
**A:** `CASCADE` removes database objects entirely, while `TRUNCATE` removes all rows from existing tables but preserves the table structure. In this project, `TRUNCATE ... RESTART IDENTITY` was used to reset data, while `CASCADE` would only be used when dropping schemas or tables.

**Q:** How are missing movie ratings represented in the database?  
**A:** Missing ratings are stored as `NULL` values in the `rating` column. This represents survey respondents who did not watch a particular movie.

**Q:** Why is the `rating` column allowed to be `NULL`?  
**A:** Allowing `NULL` distinguishes “not rated” from an actual numeric rating. Assigning a default value (such as 0) would incorrectly imply a rating and bias summary statistics.

**Q:** Does the `CHECK (rating BETWEEN 1 AND 5)` constraint reject `NULL` values?  
**A:** No. SQL `CHECK` constraints do not fail on `NULL` values. The constraint is enforced only when a rating is present.

**Q:** How are `NULL` ratings handled during analysis?  
**A:** During analysis, rows with `NULL` ratings are excluded from summary statistics using `WHERE rating IS NOT NULL` in SQL or `na.rm = TRUE` in R, while missingness is still reported separately.

**Q:** How was missing data demonstrated in the dataset?  
**A:** Each participant was inserted with at least one `NULL` rating, ensuring that the dataset reflects realistic survey behavior where not all participants have seen all movies.

---