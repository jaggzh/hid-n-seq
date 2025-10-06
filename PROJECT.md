# Single-Button Gesture Recognition System

## Purpose
Enable users with limited mobility to trigger complex actions using a single button through timing-based patterns.

## Context
- **Hardware**: Single HID button device (medical/accessibility use)
- **Problem**: Long holds cause muscle fatigue; need fast-twitch friendly patterns
- **Solution**: Pattern recognition on press/release sequences with fuzzy matching

## Architecture

### Core Components

1. **ButtonGesture::Recognizer** - Pattern matching engine
2. **gesture_test.pl** - Mouse-based testing interface  
3. **hid-learn** - Pattern recording/learning tool
4. **gestures.yaml** - Pattern definitions

### Key Concepts

**Quantum Time**
- Configurable time unit (default 10-50ms)
- All timing normalized to quantum units
- Enables consistent pattern matching

**Pattern Language** (experimental notation)
```
. = short tap (1-3 quanta)
- = medium hold (per dash ~2 quanta)
~ = pause between actions
) = terminal marker
```
*Note: This symbolic representation is placeholder - could use musical notation, morse, or actual ms values*

## Algorithm: Dynamic Pattern Elimination

### How It Works

1. **Start**: All patterns are candidates
2. **Input arrives**: Convert to quantum sequence (`**__*_` → press/pause/press)
3. **Normalize**: Transform to symbols (`.~.`)
4. **Score candidates**: Each pattern gets match score
5. **Eliminate impossible**: Drop patterns that can't match anymore
6. **Trigger decision**: Best match + confidence threshold + timeout

### Elastic Matching

Instead of exact match, uses fuzzy scoring:

```
score = base_match * completion * weight * timing_similarity

where:
- base_match: How well symbols align (. matches -, etc)
- completion: How much of pattern is complete (0-1)
- weight: Pattern priority 
- timing_similarity: Gaussian decay from ideal timing
```

### Trigger Logic

Fires when:
- Pattern complete + high confidence (>0.7)
- Timeout reached + decent match (>0.5)  
- Only one candidate left
- Clear winner emerged (1.5x better than second)

## Learning Mode

`hid-learn` captures multiple user demonstrations:

1. Records raw press/release timestamps
2. Converts to duration pairs (hold/pause)
3. Averages across samples
4. Calculates variance → elasticity
5. Generates YAML config

**Smart features**:
- Rejects broken patterns (unmatched press/release)
- Detects outliers with different structure
- Auto-calculates tolerance from user variance

## Current Limitations & Experiments

### What's Working
- Basic pattern recognition
- Mouse-based testing
- Learning mode with averaging

### What's Experimental  
- Symbol notation (`.`, `-`, `~`) - arbitrary choice
- Elasticity calculation - using stddev+1 quantum
- Scoring weights - hand-tuned not ML
- Quantum resolution - needs testing for optimal value

### What's Missing
- Visual pattern debugger
- Conflict resolution UI
- Pattern probability trees
- Adaptive timing per-user

## Technical Details

### Pattern Matching State Machine

```
IDLE → PRESS → RECORDING → RELEASE → PAUSE → [PRESS or TIMEOUT]
                    ↑______________|
```

Each state updates:
- `current_seq[]`: Raw quantum states (`*` or `_`)
- `candidates[]`: Live patterns still possible
- `scores{}`: Match quality per pattern

### Candidate Elimination

Pattern becomes impossible when:
1. User sequence longer than pattern
2. Wrong transition type (press vs release)
3. Timing outside elastic range
4. Incompatible symbols

### Scoring Formula

```perl
elastic_match(user_seq, pattern):
  score = 1.0
  for each position:
    if exact_match: score *= 1.0
    if compatible:  score *= 0.8  
    if elastic_skip: score *= 0.9
    else: return 0 (eliminated)
  
  score *= completion_ratio
  score *= pattern.weight
  return score
```

## Files

```
ButtonGesture/
  Recognizer.pm     # Core pattern engine
gesture_test.pl     # Mouse test harness  
hid-learn           # Pattern recorder
gestures.yaml       # Pattern definitions
hid-wifi-handler    # Original network HID handler (unused)
```

## Next Steps

Priority:
1. Fix quantum resolution in Recognizer.pm
2. Better symbol→duration mapping
3. Visual debugging mode

Future:
- ML-based pattern learning
- User-specific timing profiles
- Chorded patterns (multi-button)
- Haptic feedback integration

## Key Insight

Don't think "button combos" - think "rhythm patterns". Users can tap rhythms more easily than hold durations. The system should learn their natural timing, not force them to match preset values.