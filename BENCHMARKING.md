### Comparing benchmarks against baselines
```
cabal bench --benchmark-options="+RTS -T -RTS --baseline baseline.csv"
```

### Saving new baselines
```
cabal bench --benchmark-options="+RTS -T -RTS --csv baseline.csv"
```
