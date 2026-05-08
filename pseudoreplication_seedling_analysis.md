# Pseudoreplication in seedling-level analysis — why bag-level matters

## The strict definition of pseudoreplication

Hurlbert (1984): treating subsamples as if they were independent experimental units, when the actual unit of treatment application is something larger.

So the question is: **what is the experimental unit in the design?**

## Application to ASPS experiments

In the ASPS experiments, the *treatment* (potency) is applied to a bag of seedlings — not to individual seedlings. All seedlings in bag 1 of potency C share the same water, same paper, same micro-environment, same handling. The potency was applied **once per bag**, not once per seedling.

That's the textbook definition of the bag being the experimental unit and the seedlings being subsamples. Treating each seedling as an independent replicate of the treatment *is* pseudoreplication in Hurlbert's strict sense — **regardless of what the ICC turns out to be empirically.**

## Where the earlier framing got muddled

Low ICC (~0.02) means seedlings within a bag don't cluster much — they behave almost as if they were independent. This is empirically interesting, but it doesn't change the *design*. The treatment was still applied at the bag level. Pseudoreplication is fundamentally a claim about the experimental design and the inferential structure, not about how correlated the subsamples happen to be.

So the low ICC tells you: "the cost of pseudoreplication here is small" (DEFF only 1.27–1.34). It does *not* tell you: "this isn't pseudoreplication." Those are different claims.

## What the intuition that "something is off" might be picking up on

If ICC ≈ 0 and seedlings really do behave independently, there's a reasonable argument that the *practical* harm of seedling-level analysis is minimal. Some statisticians (especially in the mixed-models tradition) would say: fit a mixed model with bag as a random effect, let the data tell you how much variance is at each level, and trust the model. If the bag-level variance component is essentially zero, you've recovered something close to the seedling-level analysis anyway — but *honestly*, with the design structure declared.

That's the cleaner answer than either "bag-level only" or "seedling-level treating bags as ignorable." A mixed model handles it properly without requiring you to throw away within-bag information when it's actually informative.

## The 5.7× p-value exaggeration for root length is the giveaway

If ICC really were 0, seedling-level and bag-level p-values should be similar. A 5.7× difference means the inflation is real — the seedling-level test is borrowing degrees of freedom it doesn't have. Something about the seedling-level analysis is treating non-independent observations as independent. So even with low nominal ICC, the inferential machinery is still being misled when you pool seedlings.

## Bottom line

Removing bag-level structure *does* cause pseudoreplication in the technical sense, because the treatment was applied at the bag level. The low ICC softens the practical consequences but doesn't eliminate the design problem. The earlier framing was right on the conclusion (use bag-level, or equivalently a mixed model) but a bit sloppy on the reasoning — it leaned on ICC magnitude when it should have leaned on design structure.
