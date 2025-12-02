# Day 2: Gift Shop

## Problem

You've reached the North Pole gift shop, but one of the younger Elves was playing on a computer and added a bunch of invalid product IDs to their database. Your job is to identify and sum up all the invalid product IDs.

### Input Format

The puzzle input contains **product ID ranges** on a single line, separated by commas:
- Each range is formatted as `start-end`
- Example: `11-22,95-115,998-1012`

### Part 1

An ID is **invalid** if its decimal representation consists of a sequence of digits repeated **exactly twice**.

Examples of invalid IDs:
- `55` → `5` repeated twice
- `6464` → `64` repeated twice
- `123123` → `123` repeated twice

**Note:** Numbers cannot have leading zeros. `0101` is not a valid ID.

#### Example

```
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
```

| Range | Invalid IDs |
|-------|-------------|
| 11-22 | 11, 22 |
| 95-115 | 99 |
| 998-1012 | 1010 |
| 1188511880-1188511890 | 1188511885 |
| 222220-222224 | 222222 |
| 1698522-1698528 | (none) |
| 446443-446449 | 446446 |
| 38593856-38593862 | 38593859 |
| 565653-565659 | (none) |
| 824824821-824824827 | (none) |
| 2121212118-2121212124 | (none) |

Sum of all invalid IDs: **1227775554**

### Part 2

The rules change! Now an ID is **invalid** if it consists of a sequence of digits repeated **at least twice** (2, 3, 4, ... times).

Additional examples of invalid IDs:
- `111` → `1` repeated 3 times
- `123123123` → `123` repeated 3 times
- `1212121212` → `12` repeated 5 times
- `1111111` → `1` repeated 7 times

#### Example (continued)

| Range | Invalid IDs (Part 2) |
|-------|---------------------|
| 11-22 | 11, 22 |
| 95-115 | 99, 111 |
| 998-1012 | 999, 1010 |
| 1188511880-1188511890 | 1188511885 |
| 222220-222224 | 222222 |
| 1698522-1698528 | (none) |
| 446443-446449 | 446446 |
| 38593856-38593862 | 38593859 |
| 565653-565659 | 565656 |
| 824824821-824824827 | 824824824 |
| 2121212118-2121212124 | 2121212121 |

Sum of all invalid IDs: **4174379265**

## Solution

The solution uses an efficient algorithm that generates repeated patterns directly rather than checking every number in each range:

1. **Part 1:** For each possible half-length (1-9 digits), enumerate all "double patterns" by repeating each base value twice
2. **Part 2:** For each base length and repetition count (k ≥ 2), enumerate patterns with "primitive" bases (bases that aren't themselves repeated patterns) to avoid double-counting

This allows handling very large ranges efficiently using arithmetic series summation.

```bash
# Build
alr build

# Run
./bin/main
```
