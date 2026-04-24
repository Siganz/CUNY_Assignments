**Generative AI Usage Disclosure**  
I used ChatGPT (built by OpenAI) as a generative AI tool to assist with this assignment

**Citation**  
OpenAI. (2026). *ChatGPT* [Large language model]. Accessed April 23, 2026.

## Q:

```
algorithms <- list(`random items` = list(name = "RANDOM", param = NULL), `popular items` = list(name = "POPULAR",
    param = NULL), `user-based CF` = list(name = "UBCF", param = list(nn = 3)), `item-based CF` = list(name = "IBCF",
    param = list(k = 100)))

results <- evaluate(scheme, algorithms, type = "topNList", n = c(1, 3, 5, 10), progress = FALSE)

plot(results, annotate = 2, legend = "topleft")
```
Whats this? 

## A:

That part compares **multiple recommendation algorithms** and plots how well they perform.

```r
algorithms <- list(
  `random items` = list(name = "RANDOM", param = NULL),
  `popular items` = list(name = "POPULAR", param = NULL),
  `user-based CF` = list(name = "UBCF", param = list(nn = 3)),
  `item-based CF` = list(name = "IBCF", param = list(k = 100))
)
```

This defines four models to test:

* `RANDOM`: recommends random movies. Baseline trash model.
* `POPULAR`: recommends movies that are generally popular.
* `UBCF`: user-based collaborative filtering.

  * `nn = 3` means it uses the 3 nearest/similar users.
* `IBCF`: item-based collaborative filtering.

  * `k = 100` means it keeps 100 nearest/similar items.

```r
results <- evaluate(
  scheme,
  algorithms,
  type = "topNList",
  n = c(1, 3, 5, 10),
  progress = FALSE
)
```

This evaluates each algorithm using your `scheme`.

* `type = "topNList"` means it checks recommendation lists, not exact rating predictions.
* `n = c(1, 3, 5, 10)` means it tests top 1, top 3, top 5, and top 10 recommendations.
* `progress = FALSE` just hides progress messages.

```r
plot(results, annotate = 2, legend = "topleft")
```

This plots performance.

Usually the plot is like an ROC-style curve:

* **True positive rate**: how many actually-good movies got recommended.
* **False positive rate**: how many not-good movies got recommended.
* Better models are closer to the **top-left**.

In plain English:
It checks whether random recommendations, popular recommendations, user-based recommendations, or item-based recommendations perform best.
---

## Q: 

oh so it's just showing the different methods? Not dealing with the testing of what they did previously with rec and pre?

## A: 

Yes. That block is separate from the earlier `rec` and `pre`.

Earlier:

```r
rec <- Recommender(train, method = "UBCF")
pre <- predict(rec, new_users, n = 3)
```

That trains **one model** and gets recommendations for specific users.

This later block:

```r
algorithms <- list(...)
results <- evaluate(...)
plot(results)
```

compares **several methods** against each other using the evaluation scheme.

So:

```r
rec/pre = make recommendations
evaluate/plot = compare algorithms
```

It is not using the previous `rec` or `pre`. It starts fresh from `scheme` and trains/evaluates each algorithm listed in `algorithms`.
