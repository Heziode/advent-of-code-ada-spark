To run a project (build+run):
```bash
alr run
```

To launch prover:

```bash
alr exec -- gnatprove -P GPR_NAME.gpr --mode=silver -j0
```

If you needs to compute distance:

- **Numeric Distances**: Euclidean, Manhattan, Minkowski, Chebyshev, Canberra
- **Statistical Measures**: Cosine Similarity
- **Text Distances**: Levenshtein, Damerau-Levenshtein, Hamming, Jaro-Winkler, Sørensen-Dice

Use the library distance (add to the project with `alr with distance`).
