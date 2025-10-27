import { V, H } from '@mathjax/src/mjs/output/common/Direction.js';
export const delimiters = {
    0x28: {
        dir: V,
        sizes: [.942, 1.472, 2.042, 2.553],
        stretch: [0x239B, 0x239C, 0x239D],
        HDW: [.726, .215, .421]
    },
    0x29: {
        dir: V,
        sizes: [.942, 1.472, 2.042, 2.553],
        stretch: [0x239E, 0x239F, 0x23A0],
        HDW: [.726, .215, .421]
    },
    0x2D: {
        c: 0x2212,
        dir: H,
        stretch: [0, 0x2212],
        HDW: [0.538, 0, .605],
        hd: [.538, 0]
    },
    0x2F: {
        dir: V,
        sizes: [.884, 1.206, 1.472, 1.796, 2.19]
    },
    0x3D: {
        dir: H,
        stretch: [0, 0x3D],
        HDW: [0.406, -0.134, .668],
        hd: [.406, -.134]
    },
    0x5B: {
        dir: V,
        sizes: [.911, 1.477, 2.046, 2.557],
        stretch: [0x23A1, 0x23A2, 0x23A3],
        HDW: [.726, .184, .382]
    },
    0x5D: {
        dir: V,
        sizes: [.911, 1.477, 2.046, 2.557],
        stretch: [0x23A4, 0x23A5, 0x23A6],
        HDW: [.726, .184, .382]
    },
    0x5E: {
        c: 0x302,
        dir: H,
        sizes: [.312, .453, .633, 1.055, 2.017, 3.026],
        schar: [0x302, 0x2C6]
    },
    0x5F: {
        c: 0x2013,
        dir: H,
        sizes: [.499],
        stretch: [0, 0x2013],
        HDW: [0.277, -0.219, .499],
        hd: [.277, -.219]
    },
    0x7B: {
        dir: V,
        sizes: [.902, 1.472, 2.042, 2.553],
        stretch: [0x23A7, 0x23AA, 0x23A9, 0x23A8],
        HDW: [.726, .175, .581]
    },
    0x7C: {
        dir: V,
        sizes: [.886, 1.276, 1.556, 1.898, 2.316, 2.713, 3.178],
        stretch: [0, 0x7C],
        stretchv: [0, 1],
        HDW: [.713, .172, .272]
    },
    0x7D: {
        dir: V,
        sizes: [.902, 1.472, 2.042, 2.553],
        stretch: [0x23AB, 0x23AA, 0x23AD, 0x23AC],
        HDW: [.726, .175, .581]
    },
    0x7E: {
        c: 0x303,
        dir: H,
        sizes: [.43, .7, 1.052, 1.402, 1.864, 2.797],
        schar: [0x303, 0x2DC]
    },
    0xAF: {
        c: 0x305,
        dir: H,
        sizes: [.433, .51, .674, 1.126],
        schar: [0x305, 0x2C9],
        stretch: [0, 0x2C9],
        stretchv: [0, 3],
        HDW: [0.587, -0.542, 0],
        hd: [.587, -.542]
    },
    0x2C6: {
        c: 0x302,
        dir: H,
        sizes: [.312, .453, .633, 1.055, 2.017, 3.026],
        schar: [0x302, 0x2C6]
    },
    0x2C7: {
        c: 0x30C,
        dir: H,
        sizes: [.312, .736, 1.104, 1.473, 1.959, 2.94],
        schar: [0x30C, 0x2C7, 0x2C7, 0x2C7, 0x2C7, 0x2D7]
    },
    0x2C9: {
        c: 0x305,
        dir: H,
        sizes: [.433, .51, .674, 1.126],
        schar: [0x305, 0x2C9],
        stretch: [0, 0x2C9],
        stretchv: [0, 3],
        HDW: [0.587, -0.542, 0],
        hd: [.587, -.542]
    },
    0x2D8: {
        c: 0x306,
        dir: H,
        sizes: [.282, .383, .541, .921, 1.761],
        schar: [0x306, 0x2D8]
    },
    0x2DC: {
        c: 0x303,
        dir: H,
        sizes: [.43, .7, 1.052, 1.402, 1.864, 2.797],
        schar: [0x303, 0x2DC]
    },
    0x302: {
        dir: H,
        sizes: [.312, .453, .633, 1.055, 2.017, 3.026],
        schar: [0x302, 0x2C6]
    },
    0x303: {
        dir: H,
        sizes: [.43, .7, 1.052, 1.402, 1.864, 2.797],
        schar: [0x303, 0x2DC]
    },
    0x305: {
        dir: H,
        sizes: [.433, .51, .674, 1.126],
        schar: [0x305, 0x2C9],
        stretch: [0, 0x2C9],
        stretchv: [0, 3],
        HDW: [0.587, -0.542, 0],
        hd: [.587, -.542]
    },
    0x306: {
        dir: H,
        sizes: [.282, .383, .541, .921, 1.761],
        schar: [0x306, 0x2D8]
    },
    0x30C: {
        dir: H,
        sizes: [.312, .736, 1.104, 1.473, 1.959, 2.94],
        schar: [0x30C, 0x2C7, 0x2C7, 0x2C7, 0x2C7, 0x2D7]
    },
    0x2013: {
        dir: H,
        sizes: [.499],
        stretch: [0, 0x2013],
        HDW: [0.277, -0.219, .499],
        hd: [.277, -.219]
    },
    0x2014: {
        dir: H,
        sizes: [1],
        stretch: [0, 0x2014],
        HDW: [0.277, -0.219, 1],
        hd: [.277, -.219]
    },
    0x2015: {
        dir: H,
        stretch: [0, 0x2015],
        HDW: [0.271, -0.213, 1],
        hd: [.271, -.213]
    },
    0x2016: {
        dir: V,
        sizes: [.886, 1.276, 1.556, 1.898, 2.316],
        stretch: [0, 0x2016],
        stretchv: [0, 1],
        HDW: [.713, .172, .553]
    },
    0x203E: {
        c: 0xAF,
        dir: H,
        sizes: [.433, .51, .674, 1.126],
        schar: [0x305, 0x2C9],
        stretch: [0, 0x2C9],
        stretchv: [0, 3],
        HDW: [0.587, -0.542, 0],
        hd: [.587, -.542]
    },
    0x2044: {
        dir: V,
        sizes: [.838, 1.206, 1.472, 1.796, 2.19],
        schar: [0x2044, 0x2F]
    },
    0x20D0: {
        dir: H,
        sizes: [.557],
        stretch: [0x20D0, 0x20D0],
        stretchv: [0, 2],
        HDW: [0.791, -0.636, .557],
        hd: [.673, -.636]
    },
    0x20D1: {
        dir: H,
        sizes: [.557],
        stretch: [0, 0x20D0, 0x20D1],
        stretchv: [0, 2, 0],
        HDW: [0.791, -0.636, .557],
        hd: [.673, -.636]
    },
    0x20D6: {
        dir: H,
        sizes: [.557, .807, 1.127, 1.878, 3.579],
        stretch: [0x20D6, 0x20D0],
        stretchv: [0, 2],
        HDW: [0.79, -0.519, .557],
        hd: [.673, -.636]
    },
    0x20D7: {
        dir: H,
        sizes: [.557, .807, 1.127, 1.878, 3.579],
        stretch: [0, 0x20D0, 0x20D7],
        stretchv: [0, 2, 0],
        HDW: [0.79, -0.519, .557],
        hd: [.673, -.636]
    },
    0x20E1: {
        dir: H,
        sizes: [.556],
        stretch: [0x20D6, 0x20D0, 0x20D7],
        stretchv: [0, 2, 0],
        HDW: [0.79, -0.519, .556],
        hd: [.673, -.636]
    },
    0x20EE: {
        dir: H,
        stretch: [0x20EE, 0x20EE],
        stretchv: [0, 2],
        HDW: [-0.083, 0.354, .556],
        hd: [-.2, .237]
    },
    0x20EF: {
        dir: H,
        stretch: [0, 0x20EE, 0x20EF],
        stretchv: [0, 2, 0],
        HDW: [-0.083, 0.354, .556],
        hd: [-.2, .237]
    },
    0x2190: {
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF],
        stretchv: [4, 0],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x2191: {
        dir: V,
        sizes: [.886],
        stretch: [0x2191, 0x2191],
        stretchv: [0, 2],
        HDW: [.713, .172, .524]
    },
    0x2192: {
        dir: H,
        sizes: [1.013],
        stretch: [0, 0x23AF, 0x2192],
        stretchv: [0, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x2193: {
        dir: V,
        sizes: [.886],
        stretch: [0, 0x2191, 0x2193],
        stretchv: [0, 2, 0],
        HDW: [.713, .172, .524]
    },
    0x2194: {
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF, 0x2192],
        stretchv: [4, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x2195: {
        dir: V,
        sizes: [.883],
        stretch: [0x2191, 0x2191, 0x2193],
        stretchv: [0, 2, 0],
        HDW: [.712, .172, .524]
    },
    0x219E: {
        dir: H,
        sizes: [1.012],
        stretch: [0x219E, 0x2190],
        stretchv: [0, 2],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21A0: {
        dir: H,
        sizes: [1.012],
        stretch: [0, 0x2190, 0x21A0],
        stretchv: [0, 2, 0],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21A4: {
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF, 0x21A4],
        stretchv: [4, 0, 2],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21A6: {
        dir: H,
        sizes: [1.013],
        stretch: [0x21A6, 0x23AF, 0x2192],
        stretchv: [2, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21A9: {
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF, 0x21A9],
        stretchv: [4, 0, 2],
        HDW: [0.494, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21AA: {
        dir: H,
        sizes: [1.013],
        stretch: [0x21AA, 0x23AF, 0x2192],
        stretchv: [2, 0, 4],
        HDW: [0.494, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x21BC: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BC, 0x2190],
        stretchv: [2, 2],
        HDW: [0.489, -0.241, 1.013],
        hd: [.3, -.241]
    },
    0x21BD: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BD, 0x2190],
        stretchv: [2, 2],
        HDW: [0.302, -0.056, 1.013],
        hd: [.3, -.241]
    },
    0x21BE: {
        dir: V,
        sizes: [.885],
        stretch: [0x21BE, 0x2191],
        stretchv: [0, 2],
        HDW: [.714, .171, .524]
    },
    0x21BF: {
        dir: V,
        sizes: [.885],
        stretch: [0x21BF, 0x2191],
        stretchv: [0, 2],
        HDW: [.714, .171, .524]
    },
    0x21C0: {
        dir: H,
        sizes: [1.012],
        stretch: [0, 0x2190, 0x21C0],
        stretchv: [0, 2, 2],
        HDW: [0.489, -0.241, 1.013],
        hd: [.3, -.241]
    },
    0x21C1: {
        dir: H,
        sizes: [1.012],
        stretch: [0, 0x2190, 0x21C1],
        stretchv: [0, 2, 2],
        HDW: [0.302, -0.056, 1.013],
        hd: [.3, -.241]
    },
    0x21C2: {
        dir: V,
        sizes: [.885],
        stretch: [0, 0x2191, 0x21C2],
        stretchv: [0, 2, 0],
        HDW: [.714, .171, .524]
    },
    0x21C3: {
        dir: V,
        sizes: [.885],
        stretch: [0, 0x2191, 0x21C3],
        stretchv: [0, 2, 0],
        HDW: [.714, .171, .524]
    },
    0x21D0: {
        dir: H,
        sizes: [1.013],
        stretch: [0x21D0, 0x21D2],
        stretchv: [4, 2],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x21D1: {
        dir: V,
        sizes: [.886],
        stretch: [0x21D1, 0x21D1],
        stretchv: [0, 2],
        HDW: [.713, .172, .578]
    },
    0x21D2: {
        dir: H,
        sizes: [1.013],
        stretch: [0, 0x21D2, 0x21D2],
        stretchv: [0, 2, 4],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x21D3: {
        dir: V,
        sizes: [.886],
        stretch: [0, 0x21D1, 0x21D3],
        stretchv: [0, 2, 0],
        HDW: [.713, .172, .578]
    },
    0x21D4: {
        dir: H,
        sizes: [1.013],
        stretch: [0x21D0, 0x21D2, 0x21D2],
        stretchv: [4, 2, 4],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x21D5: {
        dir: V,
        sizes: [.893],
        stretch: [0x21D1, 0x21D1, 0x21D3],
        stretchv: [0, 2, 0],
        HDW: [.718, .176, .578]
    },
    0x21DA: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21DA, 0x2261],
        HDW: [0.607, 0.067, 1.013],
        hd: [.479, -.062]
    },
    0x21DB: {
        dir: H,
        sizes: [1.012],
        stretch: [0, 0x2261, 0x21DB],
        HDW: [0.608, 0.066, 1.013],
        hd: [.479, -.062]
    },
    0x220F: {
        dir: V,
        sizes: [.938, 1.35, 1.943, 2.798],
        variants: [0, 2, 3, 4]
    },
    0x2210: {
        dir: V,
        sizes: [.938, 1.35, 1.943, 2.798],
        variants: [0, 2, 3, 4]
    },
    0x2211: {
        dir: V,
        sizes: [.931, 1.34, 1.929, 2.777],
        variants: [0, 2, 3, 4]
    },
    0x2212: {
        dir: H,
        stretch: [0, 0x2212],
        HDW: [0.538, 0, .605],
        hd: [.538, 0]
    },
    0x2215: {
        c: 0x2F,
        dir: V,
        sizes: [.884, 1.206, 1.472, 1.796, 2.19]
    },
    0x221A: {
        dir: V,
        sizes: [1.139, 1.281, 1.913, 2.544, 3.176],
        stretch: [0xE001, 0xE002, 0x23B7],
        stretchv: [2, 2, 0],
        HDW: [1.079, .059, .987],
        fullExt: [0.65, 1.802]
    },
    0x2223: {
        dir: V,
        sizes: [.886],
        stretch: [0, 0x2223],
        HDW: [.714, .171, .437]
    },
    0x2225: {
        dir: V,
        sizes: [.886],
        stretch: [0, 0x2225],
        HDW: [.714, .171, .641]
    },
    0x2229: {
        dir: V,
        sizes: [.604, 1.56, 2.246, 2.589],
        variants: [0, 2, 3, 3],
        schar: [0x2229, 0x2229, 0x2229, 0x22C2]
    },
    0x222B: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4],
        stretch: [0x2320, 0x23AE, 0x2321],
        HDW: [.885, .442, 1.371]
    },
    0x222C: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x222D: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x222E: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x222F: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x2230: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x2231: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x2232: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x2233: {
        dir: V,
        sizes: [1.328, 1.965, 2.712, 3.471],
        variants: [0, 2, 3, 4]
    },
    0x22C0: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x22C1: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x22C2: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3],
        schar: [0x22C2, 0x2229, 0x22C2]
    },
    0x22C3: {
        dir: V,
        sizes: [.94, 1.56, 2.246, 2.589],
        variants: [0, 2, 3, 4]
    },
    0x2308: {
        dir: V,
        sizes: [.886, 1.471, 2.042, 2.553],
        stretch: [0x23A1, 0x23A2],
        HDW: [.713, .172, .382]
    },
    0x2309: {
        dir: V,
        sizes: [.886, 1.471, 2.042, 2.553],
        stretch: [0x23A4, 0x23A5],
        HDW: [.713, .172, .382]
    },
    0x230A: {
        dir: V,
        sizes: [.886, 1.471, 2.042, 2.553],
        stretch: [0, 0x23A2, 0x23A3],
        HDW: [.713, .172, .382]
    },
    0x230B: {
        dir: V,
        sizes: [.886, 1.471, 2.042, 2.553],
        stretch: [0, 0x23A5, 0x23A6],
        HDW: [.713, .172, .382]
    },
    0x2312: {
        c: 0x23DC,
        dir: H,
        sizes: [1.069, .972, 1.348, 1.685],
        stretch: [0xE010, 0xE013, 0xE012],
        stretchv: [2, 2, 2],
        HDW: [0.908, -0.621, 1.069],
        hd: [.908, -.813]
    },
    0x2322: {
        c: 0x23DC,
        dir: H,
        sizes: [1.069, .972, 1.348, 1.685],
        stretch: [0xE010, 0xE013, 0xE012],
        stretchv: [2, 2, 2],
        HDW: [0.908, -0.621, 1.069],
        hd: [.908, -.813]
    },
    0x2323: {
        c: 0x23DD,
        dir: H,
        sizes: [1.069, .972, 1.348, 1.685],
        stretch: [0xE014, 0x3C6, 0xE016],
        stretchv: [2, 4, 2],
        HDW: [-0.371, 0.658, 1.069],
        hd: [-.563, .658]
    },
    0x2329: {
        c: 0x27E8,
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x232A: {
        c: 0x27E9,
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x23AA: {
        dir: V,
        sizes: [.326],
        stretch: [0, 0x23AA, 0],
        HDW: [.011, .315, .581]
    },
    0x23AF: {
        c: 0x2013,
        dir: H,
        sizes: [.499],
        stretch: [0, 0x2013],
        HDW: [0.277, -0.219, .499],
        hd: [.277, -.219]
    },
    0x23B0: {
        dir: V,
        sizes: [1.437],
        stretch: [0x23A7, 0x23AA, 0x23AD],
        HDW: [.231, 1.206, .581]
    },
    0x23B1: {
        dir: V,
        sizes: [1.437],
        stretch: [0x23AB, 0x23AA, 0x23A9],
        HDW: [.231, 1.206, .581]
    },
    0x23B4: {
        dir: H,
        sizes: [.601, .977, 1.352, 1.689],
        stretch: [0xE018, 0xE019, 0xE01A],
        stretchv: [2, 2, 2],
        HDW: [0.833, -0.544, .601],
        hd: [.827, -.712]
    },
    0x23B5: {
        dir: H,
        sizes: [.601, .977, 1.352, 1.689],
        stretch: [0xE01C, 0xE01D, 0xE01E],
        stretchv: [2, 2, 2],
        HDW: [-0.34, 0.633, .601],
        hd: [-.513, .628]
    },
    0x23D0: {
        dir: V,
        stretch: [0, 0x2223],
        HDW: [0, 0, .437]
    },
    0x23DC: {
        dir: H,
        sizes: [1.069, .972, 1.348, 1.685],
        stretch: [0xE010, 0xE013, 0xE012],
        stretchv: [2, 2, 2],
        HDW: [0.908, -0.621, 1.069],
        hd: [.908, -.813]
    },
    0x23DD: {
        dir: H,
        sizes: [1.069, .972, 1.348, 1.685],
        stretch: [0xE014, 0x3C6, 0xE016],
        stretchv: [2, 4, 2],
        HDW: [-0.371, 0.658, 1.069],
        hd: [-.563, .658]
    },
    0x23DE: {
        dir: H,
        sizes: [1.029, 1.572, 2.142, 2.653],
        stretch: [0xE010, 0xE013, 0xE012, 0xE011],
        stretchv: [2, 2, 2, 2],
        HDW: [1.1, -0.621, 1.029],
        hd: [.908, -.813]
    },
    0x23DF: {
        dir: H,
        sizes: [1.029, 1.572, 2.142, 2.653],
        stretch: [0xE014, 0x3C6, 0xE016, 0xE015],
        stretchv: [2, 4, 2, 2],
        HDW: [-0.371, 0.85, 1.029],
        hd: [-.563, .658]
    },
    0x23E0: {
        dir: H,
        sizes: [1.029, 1.359, 2.055, 3.107]
    },
    0x23E1: {
        dir: H,
        sizes: [1.029, 1.359, 2.055, 3.107]
    },
    0x2500: {
        c: 0x2013,
        dir: H,
        sizes: [.499],
        stretch: [0, 0x2013],
        HDW: [0.277, -0.219, .499],
        hd: [.277, -.219]
    },
    0x2758: {
        c: 0x2223,
        dir: V,
        sizes: [.886],
        stretch: [0, 0x2223],
        HDW: [.714, .171, .437]
    },
    0x27E6: {
        dir: V,
        sizes: [.911, 1.026, 1.536, 2.046, 2.557]
    },
    0x27E7: {
        dir: V,
        sizes: [.911, 1.026, 1.536, 2.046, 2.557]
    },
    0x27E8: {
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x27E9: {
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x27EA: {
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x27EB: {
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x27EE: {
        dir: V,
        sizes: [.949],
        stretch: [0x23A7, 0x23AA, 0x23A9],
        HDW: [.726, .223, .581]
    },
    0x27EF: {
        dir: V,
        sizes: [.949],
        stretch: [0x23AB, 0x23AA, 0x23AD],
        HDW: [.726, .223, .581]
    },
    0x27F5: {
        c: 0x2190,
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF],
        stretchv: [4, 0],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x27F6: {
        c: 0x2192,
        dir: H,
        sizes: [1.013],
        stretch: [0, 0x23AF, 0x2192],
        stretchv: [0, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x27F7: {
        c: 0x2194,
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF, 0x2192],
        stretchv: [4, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x27F8: {
        c: 0x21D0,
        dir: H,
        sizes: [1.013],
        stretch: [0x21D0, 0x21D2],
        stretchv: [4, 2],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x27F9: {
        c: 0x21D2,
        dir: H,
        sizes: [1.013],
        stretch: [0, 0x21D2, 0x21D2],
        stretchv: [0, 2, 4],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x27FA: {
        c: 0x21D4,
        dir: H,
        sizes: [1.013],
        stretch: [0x21D0, 0x21D2, 0x21D2],
        stretchv: [4, 2, 4],
        HDW: [0.537, -0.005, 1.013],
        hd: [.406, -.134]
    },
    0x27FB: {
        c: 0x21A4,
        dir: H,
        sizes: [1.013],
        stretch: [0x2190, 0x23AF, 0x21A4],
        stretchv: [4, 0, 2],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x27FC: {
        c: 0x21A6,
        dir: H,
        sizes: [1.013],
        stretch: [0x21A6, 0x23AF, 0x2192],
        stretchv: [2, 0, 4],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x294A: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BC, 0x2190, 0x21C1],
        stretchv: [2, 2, 2],
        HDW: [0.489, -0.056, 1.013],
        hd: [.3, -.241]
    },
    0x294B: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BD, 0x2190, 0x21C0],
        stretchv: [2, 2, 2],
        HDW: [0.489, -0.056, 1.013],
        hd: [.3, -.241]
    },
    0x294E: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BC, 0x2190, 0x21C0],
        stretchv: [2, 2, 2],
        HDW: [0.489, -0.241, 1.013],
        hd: [.3, -.241]
    },
    0x2950: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BD, 0x2190, 0x21C1],
        stretchv: [2, 2, 2],
        HDW: [0.302, -0.056, 1.013],
        hd: [.3, -.241]
    },
    0x295A: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BC, 0x2190, 0x21A4],
        stretchv: [2, 2, 2],
        HDW: [0.489, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x295B: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21A6, 0x2190, 0x21C0],
        stretchv: [2, 2, 2],
        HDW: [0.489, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x295E: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21BD, 0x2190, 0x21A4],
        stretchv: [2, 2, 2],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x295F: {
        dir: H,
        sizes: [1.012],
        stretch: [0x21A6, 0x2190, 0x21C1],
        stretchv: [2, 2, 2],
        HDW: [0.486, -0.055, 1.013],
        hd: [.3, -.241]
    },
    0x2A00: {
        dir: V,
        sizes: [1.147, 1.651, 2.377],
        variants: [0, 2, 3]
    },
    0x2A01: {
        dir: V,
        sizes: [1.15, 1.651, 2.377],
        variants: [0, 2, 3]
    },
    0x2A02: {
        dir: V,
        sizes: [1.15, 1.651, 2.377],
        variants: [0, 2, 3]
    },
    0x2A03: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x2A04: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x2A05: {
        dir: V,
        sizes: [.927, 1.538, 2.553],
        variants: [0, 2, 3]
    },
    0x2A06: {
        dir: V,
        sizes: [.927, 1.538, 2.553],
        variants: [0, 2, 3]
    },
    0x2A07: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x2A08: {
        dir: V,
        sizes: [.94, 1.56, 2.589],
        variants: [0, 2, 3]
    },
    0x2A09: {
        dir: V,
        sizes: [.927, 1.334, 1.921],
        variants: [0, 2, 3]
    },
    0x3008: {
        c: 0x27E8,
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0x3009: {
        c: 0x27E9,
        dir: V,
        sizes: [.886, 1.022, 2.043, 2.553]
    },
    0xFE37: {
        c: 0x23DE,
        dir: H,
        sizes: [1.029, 1.572, 2.142, 2.653],
        stretch: [0xE010, 0xE013, 0xE012, 0xE011],
        stretchv: [2, 2, 2, 2],
        HDW: [1.1, -0.621, 1.029],
        hd: [.908, -.813]
    },
    0xFE38: {
        c: 0x23DF,
        dir: H,
        sizes: [1.029, 1.572, 2.142, 2.653],
        stretch: [0xE014, 0x3C6, 0xE016, 0xE015],
        stretchv: [2, 4, 2, 2],
        HDW: [-0.371, 0.85, 1.029],
        hd: [-.563, .658]
    }
};
//# sourceMappingURL=delimiters.js.map