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

---

### Q: 
```
$`0`
[1] "Sinners"

$`1`
[1] "The Materialist"

$`2`
[1] "One Battle After Another"

$`3`
[1] "Begonia"

$`4`
[1] "Wicked for Good"

$`5`
[1] "Sinners"

UBCF run fold/sample [model time/prediction time]
	 1  [0sec/0sec] 
	 2  [0sec/0.02sec] 
	 3  [0sec/0sec] 
	 4  [0sec/0sec] 
            TP       FP         FN        TN N precision    recall       TPR       FPR
[1,] 0.2500000 0.750000 0.83333333 1.1666667 3 0.2500000 0.2083333 0.2083333 0.5000000
[2,] 0.8333333 1.166667 0.25000000 0.7500000 3 0.4166667 0.7500000 0.7500000 0.7083333
[3,] 1.0000000 1.500000 0.08333333 0.4166667 3 0.4027778 0.9166667 0.9166667 0.8333333
[4,] 1.0833333 1.916667 0.00000000 0.0000000 3 0.3750000 1.0000000 1.0000000 1.0000000
[5,] 1.0833333 1.916667 0.00000000 0.0000000 3 0.3750000 1.0000000 1.0000000 1.0000000
     n
[1,] 1
[2,] 2
[3,] 3
[4,] 4
[5,] 5
```

what is this saying

### A: 

**This is your model's output broken down:**

Top part (recommendations)
```
$`0` [1] "Sinners"
$`1` [1] "The Materialist"
...
```
- These are the **top-1 recommendations** for each user (0–5 = the 6 users in your matrix).
- Example: For user 0 the model recommends **Sinners** as the single best movie.

Bottom part (evaluation table)

This is the **averaged results** from 4-fold cross-validation.

| n | precision | recall | TPR | FPR | Meaning |
|---|-----------|--------|-----|-----|---------|
| 1 | 0.25      | 0.21   | 0.21| 0.50| When recommending **1** movie, 25% of the time it's actually good for the user |
| 2 | 0.42      | 0.75   | 0.75| 0.71| At **2** recommendations, you catch 75% of the movies the user likes |
| 3 | 0.40      | 0.92   | 0.92| 0.83| At **3**, you catch 92% of good movies |
| 4–5 | 0.375   | 1.00   | 1.00| 1.00| At **4–5** you recommend everything → perfect recall but recommending a lot of junk |

Bottom line:  
Your UBCF model is **weak at small n** (precision ~0.25–0.42) but improves as you recommend more movies. With only 5 movies total and a tiny dataset, this is expected behavior. The model basically learns "recommend popular/good-rated movies" once n gets larger.

---

### Q: Shouldn't the model spit out a matrix with names and recommendations?  
### A: 