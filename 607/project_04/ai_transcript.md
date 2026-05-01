**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed April 30, 2026.

## Q:

Is this the simplest method to create a spam/ham classification predictor?

## A:

Yes. Naive Bayes with word counts is one of the simplest real methods for spam/ham classification. The main fix is that each email needs to be its own row, not all spam emails collapsed into one row and all ham emails collapsed into another.

---

## Q:

Since each folder has about 25,000 files, should the spam folder and ham folder both be used as training data?

## A:

Yes. The spam folder contains examples labeled spam, and the ham folder contains examples labeled not spam. Both folders become the training data, with each individual file treated as one training document.

---

## Q:

Would you approach this spam/ham classifier differently?

## A:

For a larger project, yes. I would use a package like quanteda because it handles large text datasets more efficiently. But for this assignment, Naive Bayes with word counts is simple and appropriate.

---

## Q:

Given the assignment prompt, is Naive Bayes a simple enough method for this project?

## A:

Yes. The assignment asks for labeled training documents and prediction on new test documents. Naive Bayes fits that exactly because it learns word patterns from labeled spam and ham emails, then predicts the label of a new email.

---

## Q:

In the code, are we first getting a list of files and then mapping over them?

## A:

Yes. `list.files()` gets the file paths from the spam and ham folders. Then `map_chr()` goes through each file path, reads the file, and turns each email into one text string.

---

## Q:

What are `words` and `dtm` doing in the code?

## A:

`words` is the long word-count table. It has one row for each email-word pair, like email 1 used the word "click" three times.

`dtm` is the document-term matrix. It turns those counts into a matrix where rows are emails, columns are words, and values are word counts.

---

## Q:

Is this the easiest method, or is there a way to classify emails as whole documents without turning them into word counts?

## A:

This is basically the easiest real method. The model cannot use raw email text directly. The email has to become numbers first. Word counts are the simple way to represent the whole email numerically.

---

## Q:

What does `anti_join(stop_words, by = "word")` do?

## A:

It removes common words like "the", "and", "is", and "to". These are called stop words. Removing them helps the model focus on more meaningful words, like "free", "click", "winner", or "meeting".

---

## Q:

Does `dtm` make each word into a column?

## A:

Yes. In the document-term matrix, each word becomes a column. Each email becomes a row. Each cell shows how many times that word appeared in that email.

---

## Q:

Why do we count the words before creating the DTM? Does the DTM do the counting itself?

## A:

We count first because `cast_dtm()` needs a document id, a word, and a value. The `count(id, word)` step creates that value. The DTM mostly reshapes the counted data into matrix form.

---

## Q:

In this code, how does the model know that `x` matches `y`?

## A:

It knows because the rows line up by position. Row 1 of `x` contains the word counts for email 1, and item 1 of `y` contains the class for email 1. The model learns how the word-count rows relate to the spam or ham labels.

---

## Q:

What are the packages `e1071` and `tm` used for?

## A:

`e1071` provides the `naiveBayes()` function.

`tm` supports document-term matrix objects used in text mining. In this code, it helps with the DTM structure created from the word counts.