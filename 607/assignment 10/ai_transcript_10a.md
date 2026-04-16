**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed April 16, 2026.

---

### Q: any other books in packages like austen?

**A:**  
Yes.

Closest easy options:

sotu — full State of the Union addresses as text. Good if you want speeches instead of novels.
gutenbergr — pulls Project Gutenberg books into R, so you can choose basically any public-domain novels instead of being stuck with Austen.
janeaustenr itself is also used as a demo source alongside sotu in corporaexplorer, which is a good sign those two are standard teaching-friendly corpora.

A couple of practical distinctions:

If you want something plug-and-play like Austen, sotu is the nearest match: one package, ready-made text corpus.
If you want books, gutenbergr is more flexible, but you have to choose and download the book(s) yourself.

One caution:

There is a GitHub package with Harry Potter book text, but that is not the same as the CRAN harrypotter package, which is just color palettes. So that one is easy to mix up.

So if your goal is “same vibe as janeaustenr, but not Austen,” I’d rank them:

sotu
gutenbergr
GitHub-only text packages if your instructor allows non-CRAN sources

If you want, I can help you pick one that will make the comparison to the original example easiest.

---

### Q: gutenberg_id <int> text <chr> 30240 THE BIG TRIP 30240 UP YONDER 30240 30240 By KURT VONNEGUT, JR. 30240 30240 Illustrated by KOSSIN?

**A:** 
Yep. That’s normal.

---
