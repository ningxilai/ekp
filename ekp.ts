/**
 * ekp.ts - Knuth-Plass Line Breaking for Deno
 *
 * Port of ekp_c/ to TypeScript. Single file replaces 5 C source files.
 * - Liang hyphenation (ekp_hyphen.c)
 * - UTF-8 text preprocessing (ekp_paragraph.c)
 * - Knuth-Plass DP algorithm (ekp_kp.c)
 * - deno-bridge communication layer
 */

import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts";

// ============================================================
// Types
// ============================================================

const enum BoxType {
  LATIN = 0,
  CJK = 1,
  CJK_PUNCT = 2,
  SPACE = 3,
}

const enum GlueType {
  NONE = 0,
  LWS = 1, // Latin word space
  MWS = 2, // Mixed (Latin-CJK)
  CWS = 3, // CJK character space
}

const FITNESS_TIGHT = 0;
const FITNESS_DECENT = 1;
const FITNESS_LOOSE = 2;
const FITNESS_VERY_LOOSE = 3;

const EKP_INFINITY = 1e10;

interface Pattern {
  letters: string;
  values: Uint8Array;
  offset: number;
}

interface Hyphenator {
  patterns: Map<string, Pattern>;
  cache: Map<string, number[]>;
  maxlen: number;
  left: number;
  right: number;
}

interface Box {
  text: string;
  width: number;
  type: BoxType;
  startType: BoxType;
  endType: BoxType;
}

interface Glue {
  type: GlueType;
  ideal: number;
  stretch: number;
  shrink: number;
}

interface Paragraph {
  boxes: Box[];
  glues: Glue[];
  idealPrefix: number[];
  minPrefix: number[];
  maxPrefix: number[];
  hyphenPositions: number[];
  hyphenWidth: number;
}

interface SpacingParams {
  lwsIdeal: number;
  lwsStretch: number;
  lwsShrink: number;
  mwsIdeal: number;
  mwsStretch: number;
  mwsShrink: number;
  cwsIdeal: number;
  cwsStretch: number;
  cwsShrink: number;
}

interface KPParams {
  linePenalty: number;
  hyphenPenalty: number;
  fitnessPenalty: number;
  consecutiveHyphenPenalty: number;
  forcedBreakPenalty: number;
  lastLineShortPenalty: number;
  lastLineMinRatio: number;
  looseness: number;
  thresholdFactor: number;
  flaggedPenalty: number;
}

interface DPResult {
  breaks: number[];
  cost: number;
  rests: number[];
}

interface BatchInput {
  idealPrefix: number[];
  minPrefix: number[];
  maxPrefix: number[];
  glueIdeals: number[];
  glueShrinks: number[];
  glueStretches: number[];
  hyphenPositions: number[];
  hyphenWidth: number;
  linePixel: number;
  n: number;
}

// ============================================================
// Default Parameters (matching Elisp defaults)
// ============================================================

const DEFAULT_SPACING: SpacingParams = {
  lwsIdeal: 7,
  lwsStretch: 3,
  lwsShrink: 2,
  mwsIdeal: 5,
  mwsStretch: 2,
  mwsShrink: 1,
  cwsIdeal: 0,
  cwsStretch: 2,
  cwsShrink: 0,
};

const DEFAULT_KP: KPParams = {
  linePenalty: 10,
  hyphenPenalty: 50,
  fitnessPenalty: 100,
  consecutiveHyphenPenalty: 100,
  forcedBreakPenalty: 10000,
  lastLineShortPenalty: 50,
  lastLineMinRatio: 0.5,
  looseness: 0,
  thresholdFactor: 0,
  flaggedPenalty: -10000,
};

// ============================================================
// Liang Hyphenation (port of ekp_hyphen.c)
// ============================================================

function parsePattern(pat: string): Pattern | null {
  const letters: string[] = [];
  const values: number[] = [];
  let pos = 0;

  while (pos < pat.length) {
    let digit = 0;
    if (pat.charCodeAt(pos) >= 0x30 && pat.charCodeAt(pos) <= 0x39) {
      digit = pat.charCodeAt(pos) - 0x30;
      pos++;
    }
    values.push(digit);

    if (pos < pat.length) {
      const c = pat.charCodeAt(pos);
      if (c < 0x30 || c > 0x39) {
        letters.push(pat[pos]);
        pos++;
      }
    }
  }

  if (letters.length === 0) return null;

  // Trim leading/trailing zeros
  let start = 0;
  let end = values.length;
  while (start < end && values[start] === 0) start++;
  while (end > start && values[end - 1] === 0) end--;

  const trimmed = values.slice(start, end);
  const arr = new Uint8Array(trimmed.length);
  for (let i = 0; i < trimmed.length; i++) arr[i] = trimmed[i];

  return { letters: letters.join(""), values: arr, offset: start };
}

function compileDictionary(content: string): Hyphenator {
  const patterns = new Map<string, Pattern>();
  let maxlen = 0;

  const lines = content.split("\n");
  // Skip first line (encoding info)
  for (let li = 1; li < lines.length; li++) {
    let line = lines[li].trim();
    if (line.length === 0) continue;
    if (line[0] === "%" || line[0] === "#") continue;
    if (line.includes("HYPHENMIN") || line.includes("/")) continue;

    // Handle ^^XX hex escapes
    line = line.replace(
      /\^\^([0-9a-fA-F]{2})/g,
      (_m, hex: string) => String.fromCharCode(parseInt(hex, 16)),
    );

    const pat = parsePattern(line);
    if (pat) {
      patterns.set(pat.letters, pat);
      if (pat.letters.length > maxlen) maxlen = pat.letters.length;
    }
  }

  return {
    patterns,
    cache: new Map(),
    maxlen,
    left: 2,
    right: 2,
  };
}

// Global dictionary cache
const dictCache = new Map<string, Hyphenator>();

function loadDictionary(path: string): Hyphenator {
  const cached = dictCache.get(path);
  if (cached) return cached;
  const content = Deno.readTextFileSync(path);
  const h = compileDictionary(content);
  dictCache.set(path, h);
  return h;
}

function findPattern(
  h: Hyphenator,
  substr: string,
): Pattern | undefined {
  const pat = h.patterns.get(substr);
  if (!pat) {
    console.error("[EKP] findPattern: no pattern for substr:", substr);
  }
  return pat;
}

function computeHyphenation(h: Hyphenator, word: string): number[] {
  const padded = "." + word.toLowerCase() + ".";
  const len = padded.length;
  const prio = new Uint8Array(len + 1);

  for (let i = 0; i < len - 1; i++) {
    for (let j = i + 1; j <= len && j <= i + h.maxlen; j++) {
      const pat = findPattern(h, padded.substring(i, j));
      if (pat) {
        for (let k = 0; k < pat.values.length; k++) {
          const pos = i + pat.offset + k;
          if (pos < prio.length && pat.values[k] > prio[pos]) {
            prio[pos] = pat.values[k];
          }
        }
      }
    }
  }

  // Collect odd positions (subtract 1 for padding offset)
  const result: number[] = [];
  for (let i = 1; i < prio.length; i++) {
    if (prio[i] & 1) {
      const pos = i - 1;
      if (pos >= h.left && pos <= word.length - h.right) {
        result.push(pos);
      }
    }
  }
  return result;
}

function getHyphenPositions(h: Hyphenator, word: string): number[] {
  const key = word.toLowerCase();
  const cached = h.cache.get(key);
  if (cached) return cached;
  const positions = computeHyphenation(h, word);
  h.cache.set(key, positions);
  return positions;
}

// ============================================================
// Character Classification (port of ekp_paragraph.c)
// ============================================================

function isCJK(cp: number): boolean {
  return (
    (cp >= 0x4e00 && cp <= 0x9fff) || // CJK Unified
    (cp >= 0x3400 && cp <= 0x4dbf) || // CJK Ext A
    (cp >= 0x20000 && cp <= 0x2a6df) || // CJK Ext B
    (cp >= 0x2a700 && cp <= 0x2b73f) || // CJK Ext C
    (cp >= 0x2b740 && cp <= 0x2b81f) || // CJK Ext D
    (cp >= 0xf900 && cp <= 0xfaff) || // CJK Compat
    (cp >= 0x3000 && cp <= 0x303f) || // CJK Symbols
    (cp >= 0x3040 && cp <= 0x309f) || // Hiragana
    (cp >= 0x30a0 && cp <= 0x30ff) || // Katakana
    (cp >= 0xac00 && cp <= 0xd7af) // Hangul
  );
}

function isCJKPunct(cp: number): boolean {
  return (
    (cp >= 0x3000 && cp <= 0x303f) || // CJK Symbols
    (cp >= 0xff00 && cp <= 0xff60) || // Fullwidth Forms
    cp === 0x201c ||
    cp === 0x201d || // " "
    cp === 0x2018 ||
    cp === 0x2019 // ' '
  );
}

function isWhitespace(cp: number): boolean {
  return (
    cp === 0x20 ||
    cp === 0x09 ||
    cp === 0x0a ||
    cp === 0x0d ||
    cp === 0x00a0 ||
    cp === 0x3000
  );
}

function classifyChar(cp: number): BoxType {
  if (isWhitespace(cp)) return BoxType.SPACE;
  if (isCJKPunct(cp)) return BoxType.CJK_PUNCT;
  if (isCJK(cp)) return BoxType.CJK;
  return BoxType.LATIN;
}

function glueBetween(prevEnd: BoxType, currStart: BoxType): GlueType {
  if (prevEnd === BoxType.SPACE || currStart === BoxType.SPACE) {
    return GlueType.NONE;
  }
  const prevLatin = prevEnd === BoxType.LATIN;
  const currLatin = currStart === BoxType.LATIN;
  if (prevLatin && currLatin) return GlueType.LWS;
  if (!prevLatin && !currLatin) return GlueType.CWS;
  return GlueType.MWS;
}

// ============================================================
// Text Preprocessing (port of ekp_paragraph.c)
// ============================================================

/**
 * Split text into typographic boxes.
 * Measure function receives the box text and returns pixel width.
 */
function splitToBoxes(
  text: string,
  measureFn: (s: string) => number,
): { boxes: Box[]; hyphenPositions: number[]; hyphenWidth: number } {
  const codepoints: { cp: number; char: string; start: number; len: number }[] =
    [];
  let i = 0;
  while (i < text.length) {
    const cp = text.codePointAt(i)!;
    const char = String.fromCodePoint(cp);
    codepoints.push({ cp, char, start: i, len: char.length });
    i += char.length;
  }

  // First pass: group into raw boxes
  const rawBoxes: { text: string; type: BoxType }[] = [];
  let wordChars: string[] = [];
  let inLatin = false;

  for (const { cp, char } of codepoints) {
    const type = classifyChar(cp);
    if (type === BoxType.LATIN) {
      wordChars.push(char);
      inLatin = true;
    } else {
      if (inLatin && wordChars.length > 0) {
        rawBoxes.push({ text: wordChars.join(""), type: BoxType.LATIN });
        wordChars = [];
        inLatin = false;
      }
      rawBoxes.push({ text: char, type });
    }
  }
  if (wordChars.length > 0) {
    rawBoxes.push({ text: wordChars.join(""), type: BoxType.LATIN });
  }

  // Hyphenation: expand Latin words
  // For now, no hyphenator available from measure context
  // Hyphenation will be done when hyphenator is provided
  const boxes: Box[] = [];
  const hyphenPositions: number[] = [];

  for (const rb of rawBoxes) {
    const w = measureFn(rb.text);
    boxes.push({
      text: rb.text,
      width: w,
      type: rb.type,
      startType: rb.type,
      endType: rb.type,
    });
  }

  return { boxes, hyphenPositions, hyphenWidth: measureFn("-") };
}

/**
 * Apply hyphenation to boxes array, splitting Latin words.
 */
function applyHyphenation(
  boxes: Box[],
  h: Hyphenator,
  measureFn: (s: string) => number,
): { boxes: Box[]; hyphenPositions: number[] } {
  const result: Box[] = [];
  const hyphenPositions: number[] = [];

  for (const box of boxes) {
    if (box.type === BoxType.LATIN && box.text.length > 4) {
      const positions = getHyphenPositions(h, box.text);
      if (positions.length > 0) {
        let prev = 0;
        for (const pos of positions) {
          if (pos <= prev || pos >= box.text.length) continue;
          const part = box.text.substring(prev, pos);
          result.push({
            text: part,
            width: measureFn(part),
            type: BoxType.LATIN,
            startType: BoxType.LATIN,
            endType: BoxType.LATIN,
          });
          hyphenPositions.push(result.length - 1);
          prev = pos;
        }
        if (prev < box.text.length) {
          const part = box.text.substring(prev);
          result.push({
            text: part,
            width: measureFn(part),
            type: BoxType.LATIN,
            startType: BoxType.LATIN,
            endType: BoxType.LATIN,
          });
        }
        continue;
      }
    }
    result.push(box);
  }

  return { boxes: result, hyphenPositions };
}

/**
 * Build glues between boxes with spacing params.
 */
function buildGlues(
  boxes: Box[],
  hyphenPositions: number[],
  spacing: SpacingParams,
): Glue[] {
  const n = boxes.length;
  const glues: Glue[] = [];
  const afterHyphen = new Set(hyphenPositions.map((p) => p + 1));

  for (let i = 0; i < n; i++) {
    if (afterHyphen.has(i) || i === 0) {
      glues.push({ type: GlueType.NONE, ideal: 0, stretch: 0, shrink: 0 });
      continue;
    }

    const prevEnd = boxes[i - 1].endType;
    const currStart = boxes[i].startType;
    const gtype = glueBetween(prevEnd, currStart);

    let ideal = 0, stretch = 0, shrink = 0;
    switch (gtype) {
      case GlueType.LWS:
        ideal = spacing.lwsIdeal;
        stretch = spacing.lwsStretch;
        shrink = spacing.lwsShrink;
        break;
      case GlueType.MWS:
        ideal = spacing.mwsIdeal;
        stretch = spacing.mwsStretch;
        shrink = spacing.mwsShrink;
        break;
      case GlueType.CWS:
        ideal = spacing.cwsIdeal;
        stretch = spacing.cwsStretch;
        shrink = spacing.cwsShrink;
        break;
    }
    glues.push({ type: gtype, ideal, stretch, shrink });
  }

  return glues;
}

/**
 * Build prefix sum arrays for O(1) range queries.
 */
function buildPrefixes(
  boxes: Box[],
  glues: Glue[],
): { idealPrefix: number[]; minPrefix: number[]; maxPrefix: number[] } {
  const n = boxes.length;
  const idealPrefix = new Array(n + 1).fill(0);
  const minPrefix = new Array(n + 1).fill(0);
  const maxPrefix = new Array(n + 1).fill(0);

  for (let i = 0; i < n; i++) {
    const bw = boxes[i].width;
    idealPrefix[i + 1] = idealPrefix[i] + bw + glues[i].ideal;
    minPrefix[i + 1] = minPrefix[i] + bw + (glues[i].ideal - glues[i].shrink);
    maxPrefix[i + 1] = maxPrefix[i] + bw + (glues[i].ideal + glues[i].stretch);
  }

  return { idealPrefix, minPrefix, maxPrefix };
}

/**
 * Full paragraph preprocessing: text → boxes, glues, prefix sums.
 */
function preprocessParagraph(
  text: string,
  measureFn: (s: string) => number,
  spacing: SpacingParams,
  hyphenator?: Hyphenator,
): Paragraph {
  let { boxes, hyphenPositions, hyphenWidth } = splitToBoxes(text, measureFn);

  if (hyphenator) {
    const result = applyHyphenation(boxes, hyphenator, measureFn);
    boxes = result.boxes;
    hyphenPositions = result.hyphenPositions;
  }

  const glues = buildGlues(boxes, hyphenPositions, spacing);
  const { idealPrefix, minPrefix, maxPrefix } = buildPrefixes(boxes, glues);

  return {
    boxes,
    glues,
    idealPrefix,
    minPrefix,
    maxPrefix,
    hyphenPositions,
    hyphenWidth,
  };
}

// ============================================================
// Knuth-Plass DP Algorithm (port of ekp_kp.c)
// ============================================================

function computeBadness(adjustment: number, flexibility: number): number {
  if (adjustment === 0) return 0;
  if (flexibility <= 0) return EKP_INFINITY;
  const ratio = adjustment / flexibility;
  const badness = 100 * Math.pow(Math.abs(ratio), 3);
  return Math.min(10000, badness);
}

function computeFitness(adjustment: number, flexibility: number): number {
  if (flexibility <= 0) return FITNESS_DECENT;
  const ratio = adjustment / flexibility;
  if (ratio < -0.5) return FITNESS_TIGHT;
  if (ratio < 0.5) return FITNESS_DECENT;
  if (ratio < 1.0) return FITNESS_LOOSE;
  return FITNESS_VERY_LOOSE;
}

function computeDemerits(
  badness: number,
  penalty: number,
  prevFitness: number,
  currFitness: number,
  endHyphen: boolean,
  prevHyphenCount: number,
  kp: KPParams,
): number {
  const base = Math.pow(kp.linePenalty + badness, 2) + penalty * penalty;
  const fitnessDelta = Math.abs(prevFitness - currFitness);
  const withFitness = fitnessDelta > 1 ? base + kp.fitnessPenalty : base;
  if (endHyphen) {
    const count = prevHyphenCount + 1;
    return withFitness + kp.consecutiveHyphenPenalty * count * count;
  }
  return withFitness;
}

function isHyphenPosition(hyphenPositions: number[], pos: number): boolean {
  // Binary search in sorted array
  let lo = 0, hi = hyphenPositions.length - 1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (hyphenPositions[mid] < pos) lo = mid + 1;
    else hi = mid;
  }
  return hyphenPositions.length > 0 && hyphenPositions[lo] === pos;
}

function isFlaggedPosition(flaggedPositions: number[], pos: number): boolean {
  if (!flaggedPositions || flaggedPositions.length === 0) return false;
  // Binary search in sorted array
  let lo = 0, hi = flaggedPositions.length - 1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (flaggedPositions[mid] < pos) lo = mid + 1;
    else hi = mid;
  }
  return flaggedPositions[lo] === pos;
}

/**
 * Core DP algorithm. Takes pre-computed prefix arrays and returns break points.
 * This is the direct port of `ekp_break_with_prefixes()` from ekp_kp.c.
 */
function dpBreakLines(
  idealPrefix: number[],
  minPrefix: number[],
  maxPrefix: number[],
  glueIdeals: number[],
  glueShrinks: number[],
  glueStretches: number[],
  hyphenPositions: number[],
  hyphenWidth: number,
  n: number,
  linePixel: number,
  kp: KPParams,
): DPResult | null {
  if (n === 0 || linePixel <= 0) return null;

  // Initialize DP arrays
  const demerits = new Float64Array(n + 1).fill(EKP_INFINITY);
  const backptrs = new Int32Array(n + 1).fill(-1);
  const restPixels = new Float64Array(n + 1).fill(0);
  const fitness = new Uint8Array(n + 1).fill(FITNESS_DECENT);
  const hyphenCounts = new Uint8Array(n + 1).fill(0);
  const lineCounts = new Uint8Array(n + 1).fill(0);
  demerits[0] = 0;

  // Main DP loop
  for (let i = 0; i < n; i++) {
    if (demerits[i] >= EKP_INFINITY) continue;

    // Threshold pruning
    if (kp.thresholdFactor > 0) {
      // Find current best at end
      let bestEnd = EKP_INFINITY;
      for (let k = 0; k <= n; k++) {
        if (demerits[k] < bestEnd) bestEnd = demerits[k];
      }
      if (
        bestEnd < EKP_INFINITY &&
        demerits[i] > bestEnd * (1 + kp.thresholdFactor)
      ) {
        continue;
      }
    }

    const prevDem = demerits[i];
    const prevFit = fitness[i];
    const prevHyph = hyphenCounts[i];
    const prevLines = lineCounts[i];

    // Leading glue for line starting at i
    const leadIdeal = (i < n) ? glueIdeals[i] : 0;
    const leadShrink = (i < n) ? glueShrinks[i] : 0;
    const leadStretch = (i < n) ? glueStretches[i] : 0;

    // Try extending to each position k > i
    for (let k = i + 1; k <= n; k++) {
      const isLast = k === n;
      const endHyphen = isHyphenPosition(hyphenPositions, k - 1);

      // Line metrics from i to k (excluding leading glue)
      let ideal = idealPrefix[k] - idealPrefix[i] - leadIdeal;
      let minW = minPrefix[k] - minPrefix[i] - (leadIdeal - leadShrink);
      let maxW = maxPrefix[k] - maxPrefix[i] - (leadIdeal + leadStretch);

      // Add hyphen width if needed
      if (endHyphen) {
        ideal += hyphenWidth;
        minW += hyphenWidth;
        maxW += hyphenWidth;
      }

      // Too long? Force break or stop
      if (minW > linePixel || (isLast && ideal > linePixel)) {
        if (k > i + 1 && demerits[k - 1] >= EKP_INFINITY) {
          const prevIdeal = idealPrefix[k - 1] - idealPrefix[i] - leadIdeal;
          const rest = linePixel - prevIdeal;
          demerits[k - 1] = prevDem + kp.forcedBreakPenalty + rest * rest;
          backptrs[k - 1] = i;
          restPixels[k - 1] = rest;
          fitness[k - 1] = FITNESS_VERY_LOOSE;
          hyphenCounts[k - 1] = 0;
          lineCounts[k - 1] = prevLines + 1;
        }
        break;
      }

      // Valid break?
      // Single-box lines are always valid (matching C implementation behavior)
      const isSingleBox = k === i + 1;
      const valid = isSingleBox ||
        (minW <= linePixel && maxW >= linePixel) ||
        (isLast && ideal <= linePixel);
      if (!valid) continue;

      // Compute demerits
      const adjustment = linePixel - ideal;
      const flexibility = adjustment > 0 ? (maxW - ideal) : (ideal - minW);

      let badness: number, fit: number, dem: number;

      if (isSingleBox) {
        badness = computeBadness(adjustment, 1);
        fit = FITNESS_DECENT;
        const penalty = endHyphen ? kp.hyphenPenalty : 0;
        dem = prevDem + computeDemerits(
          badness,
          penalty,
          prevFit,
          fit,
          endHyphen,
          prevHyph,
          kp,
        );
      } else if (isLast) {
        const fillRatio = ideal / linePixel;
        badness = fillRatio < kp.lastLineMinRatio
          ? kp.lastLineShortPenalty * (1.0 - fillRatio)
          : 0;
        fit = FITNESS_DECENT;
        dem = prevDem + Math.pow(kp.linePenalty + badness, 2);
      } else {
        badness = computeBadness(adjustment, flexibility);
        fit = computeFitness(adjustment, flexibility);
        const penalty = endHyphen ? kp.hyphenPenalty : 0;
        dem = prevDem + computeDemerits(
          badness,
          penalty,
          prevFit,
          fit,
          endHyphen,
          prevHyph,
          kp,
        );
      }

      // Update if better
      if (dem < demerits[k]) {
        demerits[k] = dem;
        backptrs[k] = i;
        restPixels[k] = adjustment;
        fitness[k] = fit;
        hyphenCounts[k] = endHyphen ? prevHyph + 1 : 0;
        lineCounts[k] = prevLines + 1;
      } else {
      }
    }
  }

  // No valid path
  if (demerits[n] >= EKP_INFINITY) {
    return null;
  }

  // Trace back
  const breaks: number[] = [];
  let idx = n;
  while (idx > 0) {
    breaks.push(idx);
    idx = backptrs[idx];
    if (idx < 0) break;
  }
  breaks.reverse();

  const rests = breaks.map((b) => restPixels[b]);

  return { breaks, cost: demerits[n], rests };
}

// ============================================================
// Batch Processing (port of ekp_thread_pool.c via Web Workers)
// ============================================================

/**
 * Process a batch of paragraphs in parallel using Promise.all.
 * Each paragraph's DP is independent, so we can parallelize.
 */
async function dpBreakBatch(
  inputs: BatchInput[],
  kp: KPParams,
): Promise<(DPResult | null)[]> {
  // For small batches, process sequentially to avoid worker overhead
  if (inputs.length <= 2) {
    return inputs.map((inp) => dpBreakFromBatchInput(inp, kp));
  }

  // Parallel processing via concurrent promises
  // Deno's V8 JIT handles this well without explicit Web Workers for CPU-bound tasks
  const promises = inputs.map((inp) =>
    Promise.resolve(dpBreakFromBatchInput(inp, kp))
  );
  return Promise.all(promises);
}

function dpBreakFromBatchInput(inp: BatchInput, kp: KPParams): DPResult | null {
  return dpBreakLines(
    inp.idealPrefix,
    inp.minPrefix,
    inp.maxPrefix,
    inp.glueIdeals,
    inp.glueShrinks,
    inp.glueStretches,
    inp.hyphenPositions,
    inp.hyphenWidth,
    inp.n,
    inp.linePixel,
    kp,
  );
}

// ============================================================
// deno-bridge Communication Layer
// ============================================================

const bridge = new DenoBridge(
  Deno.args[0],
  Deno.args[1],
  Deno.args[2],
  messageDispatcher,
);

console.error("[EKP] TypeScript bridge initialized");

// Global state
let currentSpacing: SpacingParams = { ...DEFAULT_SPACING };
let currentKP: KPParams = { ...DEFAULT_KP };
let currentHyphenator: Hyphenator | undefined;

async function messageDispatcher(message: string) {
  const parsed = JSON.parse(message);
  const funcName = parsed[1][0];
  const funcArgs = parsed[1];

  try {
    switch (funcName) {
      case "break-lines": {
        const hyphenPos = funcArgs[7] || [];
        const ip = funcArgs[1];
        const mip = funcArgs[2];
        const mxp = funcArgs[3];
        const gi = funcArgs[4];
        const gs = funcArgs[5];
        const gst = funcArgs[6];
        const hw = funcArgs[8];
        if (result) {
          const breaksList = result.breaks;
          const evalCode = `(setq ekp--ts-result (cons (list ${breaksList.join(" ")}) ${result.cost}))`;
          bridge.evalInEmacs(evalCode);
        } else {
          bridge.evalInEmacs(`(setq ekp--ts-result nil)`);
        }
        break;
      }

      case "break-batch": {
        // funcArgs: ["break-batch", [batchInputs]]
        const inputs: BatchInput[] = funcArgs[1];
        const results = await dpBreakBatch(inputs, currentKP);
        const serialized = results.map((r) =>
          r ? { breaks: r.breaks, cost: r.cost } : null
        );
        bridge.evalInEmacs(
          `(setq ekp--ts-batch-result '${JSON.stringify(serialized)})`,
        );
        break;
      }

      case "set-spacing": {
        // funcArgs: ["set-spacing", lws-i, lws-+, lws--, mws-i, mws-+, mws--, cws-i, cws-+, cws--]
        const s = funcArgs;
        currentSpacing = {
          lwsIdeal: s[1],
          lwsStretch: s[2],
          lwsShrink: s[3],
          mwsIdeal: s[4],
          mwsStretch: s[5],
          mwsShrink: s[6],
          cwsIdeal: s[7],
          cwsStretch: s[8],
          cwsShrink: s[9],
        };
        break;
      }

      case "set-penalties": {
        // funcArgs: ["set-penalties", line-penalty, hyphen-penalty, fitness-penalty, last-line-ratio]
        const p = funcArgs;
        currentKP.linePenalty = p[1];
        currentKP.hyphenPenalty = p[2];
        currentKP.fitnessPenalty = p[3];
        currentKP.lastLineMinRatio = p[4];
        break;
      }

      case "load-dictionary": {
        // funcArgs: ["load-dictionary", dict-path]
        const path = funcArgs[1];
        currentHyphenator = loadDictionary(path);
        bridge.messageToEmacs(`Dictionary loaded: ${path}`);
        break;
      }

      case "hyphenate": {
        // funcArgs: ["hyphenate", word]
        if (currentHyphenator) {
          const positions = getHyphenPositions(currentHyphenator, funcArgs[1]);
          bridge.evalInEmacs(
            `(setq ekp--ts-hyphen-result '${JSON.stringify(positions)})`,
          );
        }
        break;
      }

      case "ping": {
        break;
      }

      default:
        bridge.messageToEmacs(`Unknown function: ${funcName}`);
    }
  } catch (e) {
    bridge.messageToEmacs(`Error in ${funcName}: ${e}`);
  }
}

console.log("[EKP] TypeScript bridge initialized");
