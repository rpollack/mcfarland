// The decimal digits readr (vroom) writes for a double.
//
// vroom prints doubles with Grisu3: the shortest digit string that round-trips, when Grisu3 can prove it is the shortest, and
// "%.17g" when it cannot (about 0.5% of doubles). So R writes 0.22267700000000001 where JavaScript prints 0.222677, and the
// committed baseline files contain such values: reproducing them byte for byte means reproducing the algorithm and its
// failures. Everything is 64-bit unsigned arithmetic, done here with BigInt.

const MASK64 = (1n << 64n) - 1n;
const HIDDEN_BIT = 1n << 52n;
const FRACTION_MASK = HIDDEN_BIT - 1n;
const EXPONENT_BIAS = 1023 + 52;

type Fp = { frac: bigint; exp: number };

// Cached powers of ten, 10^-348 ... 10^340 in steps of 8, each as a 64-bit significand and a binary exponent (rounded to nearest).
const POWER_COUNT = 87;
const POWER_STEP = 8;
const FIRST_POWER = -348;

function cachedPower(decimalExponent: number): Fp {
  if (decimalExponent >= 0) {
    const value = 10n ** BigInt(decimalExponent);
    const shift = value.toString(2).length - 64;
    if (shift <= 0) return { frac: value << BigInt(-shift), exp: shift };
    let frac = (value + (1n << BigInt(shift - 1))) >> BigInt(shift);
    let exp = shift;
    if (frac >> 64n) {
      frac >>= 1n;
      exp += 1;
    }
    return { frac, exp };
  }
  const divisor = 10n ** BigInt(-decimalExponent);
  const shift = divisor.toString(2).length + 63;
  let frac = ((1n << BigInt(shift)) + divisor / 2n) / divisor;
  let exp = -shift;
  if (frac >> 64n) {
    frac >>= 1n;
    exp += 1;
  }
  return { frac, exp };
}

const POWERS_OF_TEN: Fp[] = Array.from({ length: POWER_COUNT }, (_, i) => cachedPower(FIRST_POWER + i * POWER_STEP));

// Grisu3 picks the power of ten that brings the scaled value's binary exponent into [-60, -32].
function findCachedPower(normalizedExp: number): { fp: Fp; k: number } {
  const minimalExponent = -60 - (normalizedExp + 64);
  const k = Math.ceil((minimalExponent + 64 - 1) * 0.30102999566398114);
  const index = Math.trunc((348 + k - 1) / POWER_STEP) + 1;
  return { fp: POWERS_OF_TEN[index]!, k: FIRST_POWER + index * POWER_STEP };
}

function buildFp(d: number): Fp {
  const view = new DataView(new ArrayBuffer(8));
  view.setFloat64(0, d);
  const bits = view.getBigUint64(0);
  let frac = bits & FRACTION_MASK;
  let exp = Number((bits >> 52n) & 0x7ffn);
  if (exp) {
    frac += HIDDEN_BIT;
    exp -= EXPONENT_BIAS;
  } else {
    exp = -EXPONENT_BIAS + 1;
  }
  return { frac, exp };
}

function normalize(fp: Fp): Fp {
  let { frac, exp } = fp;
  while ((frac & HIDDEN_BIT) === 0n) {
    frac <<= 1n;
    exp--;
  }
  const shift = 64 - 52 - 1;
  return { frac: (frac << BigInt(shift)) & MASK64, exp: exp - shift };
}

function normalizedBoundaries(fp: Fp): { lower: Fp; upper: Fp } {
  let upperFrac = (fp.frac << 1n) + 1n;
  let upperExp = fp.exp - 1;
  while ((upperFrac & (HIDDEN_BIT << 1n)) === 0n) {
    upperFrac <<= 1n;
    upperExp--;
  }
  const upperShift = 64 - 52 - 2;
  upperFrac = (upperFrac << BigInt(upperShift)) & MASK64;
  upperExp -= upperShift;

  const lowerShift = fp.frac === HIDDEN_BIT ? 2 : 1;
  let lowerFrac = (fp.frac << BigInt(lowerShift)) - 1n;
  const lowerExp = fp.exp - lowerShift;
  lowerFrac = (lowerFrac << BigInt(lowerExp - upperExp)) & MASK64;
  return { lower: { frac: lowerFrac, exp: upperExp }, upper: { frac: upperFrac, exp: upperExp } };
}

function multiply(a: Fp, b: Fp): Fp {
  return { frac: ((a.frac * b.frac + (1n << 63n)) >> 64n) & MASK64, exp: a.exp + b.exp + 64 };
}

function roundWeed(digits: number[], distanceTooHighW: bigint, unsafeInterval: bigint, rest: bigint, tenKappa: bigint, unit: bigint): boolean {
  const smallDistance = distanceTooHighW - unit;
  const bigDistance = distanceTooHighW + unit;
  while (
    rest < smallDistance &&
    unsafeInterval - rest >= tenKappa &&
    (rest + tenKappa < smallDistance || smallDistance - rest >= rest + tenKappa - smallDistance)
  ) {
    digits[digits.length - 1]!--;
    rest += tenKappa;
  }
  if (rest < bigDistance && unsafeInterval - rest >= tenKappa && (rest + tenKappa < bigDistance || bigDistance - rest > rest + tenKappa - bigDistance)) {
    return false;
  }
  return 2n * unit <= rest && rest <= unsafeInterval - 4n * unit;
}

function digitGen(low: Fp, w: Fp, high: Fp): { ok: boolean; digits: number[]; kappa: number } {
  let unit = 1n;
  const tooLow = low.frac - unit;
  const tooHigh = high.frac + unit;
  let unsafeInterval = tooHigh - tooLow;
  const oneShift = BigInt(-w.exp);
  const oneFrac = 1n << oneShift;
  let integrals = tooHigh >> oneShift;
  let fractionals = tooHigh & (oneFrac - 1n);
  const digits: number[] = [];
  let kappa = integrals.toString().length;
  let divisor = 10n ** BigInt(kappa - 1);
  while (kappa > 0) {
    const digit = integrals / divisor;
    digits.push(Number(digit));
    integrals %= divisor;
    kappa--;
    const rest = (integrals << oneShift) + fractionals;
    if (rest < unsafeInterval) {
      return { ok: roundWeed(digits, tooHigh - w.frac, unsafeInterval, rest, divisor << oneShift, unit), digits, kappa };
    }
    divisor /= 10n;
  }
  for (;;) {
    fractionals *= 10n;
    unit *= 10n;
    unsafeInterval *= 10n;
    digits.push(Number(fractionals >> oneShift));
    fractionals &= oneFrac - 1n;
    kappa--;
    if (fractionals < unsafeInterval) {
      return { ok: roundWeed(digits, (tooHigh - w.frac) * unit, unsafeInterval, fractionals, oneFrac, unit), digits, kappa };
    }
  }
}

function sprintf17(value: number): { digits: string; k: number } {
  const [mantissa, exponent] = value.toExponential(16).split("e") as [string, string];
  const all = mantissa.replace(".", "");
  const digits = all.replace(/0+$/, "");
  return { digits, k: Number(exponent) - 16 + (all.length - digits.length) };
}

/** The digits and decimal exponent K of a positive finite double, so that value = digits * 10^K, as vroom prints it. */
export function vroomDigits(value: number): { digits: string; k: number } {
  const w = normalize(buildFp(value));
  const { lower, upper } = normalizedBoundaries(buildFp(value));
  const { fp: power, k: mk } = findCachedPower(w.exp);
  const scaledW = multiply(w, power);
  const scaledLower = multiply(lower, power);
  const scaledUpper = multiply(upper, power);
  const { ok, digits, kappa } = digitGen(scaledLower, scaledW, scaledUpper);
  if (!ok) return sprintf17(value);
  return { digits: digits.join(""), k: -mk + kappa };
}
