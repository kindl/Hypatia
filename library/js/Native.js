const length = a => a.length;

const textLength = text => text.length;

const slice = (text, start, end) => text.slice(start, end);

const isArray = v => typeof(v) === typeof([]);

const unsafeIndex = (i, a) => a[i];

const unsafeCoerce = x => x;

const unsafeInsert = (a, e) => {
    a.push(e);
    return a;
};

const unsafeSet = (a, i, v) => {
    a[i + 1] = v;
    return a;
}

const toString = x => x.toString();

const print = x => console.log(x);

const error = x => {
    throw x;
};

const toLowerCase = x => x.toLowerCase();

const sin = x => Math.sin(x);

const cos = x => Math.cos(x);

const tan = x => Math.tan(x);

const toNumber = x => parseFloat(x);

const sequenceCompare = (compare, left, right) => {
    // `isArray` will be true for objects and arrays.
    // This is fine for this use case where compare should work on both.
    if (!(isArray(left) && isArray(right))) {
        return false
    }

    for (const key in left) {
        if (!compare(left[key])(right[key])) {
            return false
        }
    }

    for (const key in right) {
        if (!(key in left)) {
            return false
        }
    }

    return true
}

export default {
    length,
    textLength,
    isArray,
    unsafeIndex,
    slice,
    unsafeCoerce,
    unsafeInsert,
    unsafeSet,
    toLowerCase,
    sin,
    cos,
    tan,
    toNumber,
    toString,
    print,
    error,
    sequenceCompare,
}
