## Generative AI Usage Disclosure
I used OpenAI’s ChatGPT (Version 5.2) as a generative AI tool to assist with this assignment. Each deliverable used for the assignment will have a section below, including brief commentary and relevant QA.

**Citation**:  
OpenAI. (2025). *ChatGPT* (Version 5.2) [Large language model]. https://chat.openai.com. Accessed February 5th, 2026.

---

# Generative AI Usage Disclosure

### Q: Why is the null error rate important?

**A:**  
The null error rate represents the error rate of a model that always predicts the majority class. It serves as a baseline for evaluating whether the model is learning meaningful patterns or merely exploiting class imbalance.

---

### Q: Why is random accuracy not always 50%?

**A:**  
Random accuracy is only 50% when classes are evenly balanced. When the data is imbalanced, a model can achieve higher accuracy by always predicting the majority class. The null model accounts for this distribution.

---

### Q: What is a confusion matrix?

**A:**  
A confusion matrix summarizes model predictions by comparing predicted labels to true labels. It reports true positives, false positives, true negatives, and false negatives, forming the basis for performance evaluation.

---

### Q: How are performance metrics computed?

**A:**  
Performance metrics such as accuracy, precision, recall, specificity, and F1 score are derived directly from the confusion matrix. They summarize different aspects of classification performance.

---

### Q: Why were multiple probability thresholds used?

**A:**  
Different thresholds allow analysis of how model performance changes when classification criteria are adjusted. This demonstrates the tradeoff between sensitivity and specificity.

---

### Q: How does this evaluation support model interpretation?

**A:**  
By comparing performance across thresholds and against the null baseline, the analysis distinguishes meaningful predictive signal from performance driven by class imbalance or chance.
