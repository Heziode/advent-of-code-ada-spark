# Day 1: Secret Entrance

## Problem

The Elves need you to finish decorating the North Pole by December 12th. But first, you need to get past the secret entrance - and the password has been changed.

The password is locked in a safe with a dial numbered `0` through `99`. The dial makes a small *click* as it reaches each number.

### Input Format

The puzzle input contains a sequence of **rotations**, one per line:
- Each rotation starts with `L` (left, toward lower numbers) or `R` (right, toward higher numbers)
- Followed by a **distance** value indicating how many clicks to rotate

### Dial Behavior

- Starting position: **50**
- The dial wraps around:
  - Left from `0` → `99`
  - Right from `99` → `0`

### Examples

If dial is at `11`:
- `R8` → dial points at `19`
- Then `L19` → dial points at `0`

If dial is at `5`:
- `L10` → dial points at `95`
- Then `R5` → dial points at `0`

### Part 1

The safe is a decoy! The actual password is **the number of times the dial is left pointing at `0` after any rotation in the sequence**.

#### Example

```
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```

Following these rotations:
| Rotation | Result |
|----------|--------|
| Start    | 50     |
| L68      | 82     |
| L30      | 52     |
| R48      | **0**  |
| L5       | 95     |
| R60      | 55     |
| L55      | **0**  |
| L1       | 99     |
| L99      | **0**  |
| R14      | 14     |
| L82      | 32     |

The dial points at `0` three times, so the password is **3**.

### Part 2

Using "password method 0x434C49434B" (CLICK in ASCII), count the number of times **any click** causes the dial to point at `0` - not just at the end of rotations, but also during them.

#### Example (continued)

| Rotation | Result | Zero passes during rotation |
|----------|--------|----------------------------|
| Start    | 50     | -                          |
| L68      | 82     | 1 (passes through 0)       |
| L30      | 52     | 0                          |
| R48      | **0**  | 1 (lands on 0)             |
| L5       | 95     | 0                          |
| R60      | 55     | 1 (passes through 0)       |
| L55      | **0**  | 1 (lands on 0)             |
| L1       | 99     | 0                          |
| L99      | **0**  | 1 (lands on 0)             |
| R14      | 14     | 0                          |
| L82      | 32     | 1 (passes through 0)       |

Total: **6** (3 at end of rotations + 3 during rotations)

**Note:** A single rotation like `R1000` from position `50` would cause the dial to point at `0` ten times (every 100 clicks)!

## Solution

```bash
# Build
alr build

# Run
./bin/main
```
