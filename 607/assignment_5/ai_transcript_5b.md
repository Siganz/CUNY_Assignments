**Generative AI Usage Disclosure**  
I used Grok (built by xAI) as a generative AI tool to assist with this assignment. The tool was used for conceptual clarification, debugging, interpretation of error metrics, and validation of reasoning. All submitted code and analysis reflect my own understanding, and I can explain any portion of the work independently.

**Citation**  
xAI. (2026). *Grok* [Large language model]. https://grok.x.ai. Accessed February 23, 2026.

---

### Q: Is this the ELO calc? A = 1 / (1 + 10^((rating_B - rating_A) / 400)) are there other ones?

**A:**  
Yes, that's the classic logistic Elo expected score formula. The main alternatives are:  
- Glicko / Glicko-2 (adds rating deviation/volatility)  
- TrueSkill (Microsoft's Bayesian system, used in games)  
- Elo variants with different scaling factors (e.g., 800 instead of 400 for faster movement)  
- Linear approximations for very low-compute settings  
But 400-scale logistic is still the default in chess and most serious implementations.

---

### Q: Within this code `raw = str_replace_all(raw, "\\|([WDL])\\s+(\\d+)"` how do I return the value of [WDL] and the digit?

**A:**  
You don't — `str_replace_all` is for replacing, not capturing/returning groups.  
If you just want to reformat to `|W|5` style, do this:  
`raw = str_replace_all(raw, "\\|([WDL])\\s+(\\d+)", "|\\1|\\2")`  

That turns `|W 5` → `|W|5`, `|L 12` → `|L|12`, etc. Clean and fast.  

If you actually need to *extract* the W/D/L letters and numbers into variables/lists:  
Use `str_match_all` instead:  
`matches <- str_match_all(raw, "\\|([WDL])\\s+(\\d+)")`  
Then:  
- `matches[[1]][,2]` → vector of all W/D/L  
- `matches[[1]][,3]` → vector of all digits  

`str_replace_all` won't give you the captured groups — it only spits back the modified string.

---

### Q: In this code `raw = str_replace_all(raw, "\\|([HU])\\s*(?=\\|)", "|\\1|0")` wouldn't 0 not return anything, since it's based on the position and 0 would just be none?

**A:**  
No — this is inserting a literal `|0` right after the `[HU]` when followed by another `|`.  
It turns e.g. `|H|` → `|H|0|` or `|U |` → `|U|0|`.  
The replacement is explicit text, not a positional thing. The look ahead `(?=\\|)` just checks there's a `|` coming without consuming it, so the new `0` gets placed correctly between the two pipes.

---

### Q: What is D/H/U in chess mean? Are they scores or something?

**A:**  
In chess database PGN annotation (especially engine analysis or rapid notation):  
- D = Draw  
- H = Half-point? Wait, more commonly in some contexts it's a Draw flag, but…  
Actually in many modern chess tools/PGN:  
- D = Draw  
- H = Half (i.e. ½ point, draw)  
- U = Unknown / Unfinished / Undecided (game not finished or result unclear)  
But context matters — in some rating/list formats H/U can mean "bye" or "forfeit" related. Most likely here: D = draw, H = half-point/draw, U = unknown/no result.

---

### Q: What do you do for ELO when D/H/U? Do you just remove them?

**A:**  
Depends on the system and goal:  
- Draws (D/H): standard Elo treats them as 0.5 for both players — plug 0.5 into the formula instead of 1 or 0.  
- Unknown/Unfinished/U: usually exclude them entirely from rating calc (no update). Some systems give 0.5 by default or ignore.  
- Forfeits/byes: often treated as 1 or 0 depending on who won the forfeit.  
Most common clean approach: only use games with decisive W/L or explicit D (0.5), skip anything marked U or unclear.