"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __read = (this && this.__read) || function (o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
};
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.MathJaxAsanaFont = void 0;
var FontData_js_1 = require("@mathjax/src/cjs/output/chtml/FontData.js");
var common_js_1 = require("./common.js");
var normal_js_1 = require("./chtml/normal.js");
var bold_js_1 = require("./chtml/bold.js");
var italic_js_1 = require("./chtml/italic.js");
var bold_italic_js_1 = require("./chtml/bold-italic.js");
var double_struck_js_1 = require("./chtml/double-struck.js");
var fraktur_js_1 = require("./chtml/fraktur.js");
var fraktur_bold_js_1 = require("./chtml/fraktur-bold.js");
var script_js_1 = require("./chtml/script.js");
var script_bold_js_1 = require("./chtml/script-bold.js");
var sans_serif_js_1 = require("./chtml/sans-serif.js");
var sans_serif_bold_js_1 = require("./chtml/sans-serif-bold.js");
var sans_serif_italic_js_1 = require("./chtml/sans-serif-italic.js");
var sans_serif_bold_italic_js_1 = require("./chtml/sans-serif-bold-italic.js");
var monospace_js_1 = require("./chtml/monospace.js");
var smallop_js_1 = require("./chtml/smallop.js");
var largeop_js_1 = require("./chtml/largeop.js");
var size3_js_1 = require("./chtml/size3.js");
var size4_js_1 = require("./chtml/size4.js");
var size5_js_1 = require("./chtml/size5.js");
var size6_js_1 = require("./chtml/size6.js");
var tex_calligraphic_js_1 = require("./chtml/tex-calligraphic.js");
var tex_calligraphic_bold_js_1 = require("./chtml/tex-calligraphic-bold.js");
var tex_oldstyle_js_1 = require("./chtml/tex-oldstyle.js");
var tex_variant_js_1 = require("./chtml/tex-variant.js");
var extend_js_1 = require("./chtml/extend.js");
var delimiters_js_1 = require("./chtml/delimiters.js");
var Base = (0, common_js_1.CommonMathJaxAsanaFontMixin)(FontData_js_1.ChtmlFontData);
var MathJaxAsanaFont = (function (_super) {
    __extends(MathJaxAsanaFont, _super);
    function MathJaxAsanaFont() {
        var _this = _super.apply(this, __spreadArray([], __read(arguments), false)) || this;
        _this.cssFontPrefix = 'ASNA';
        return _this;
    }
    MathJaxAsanaFont.NAME = 'MathJaxAsana';
    MathJaxAsanaFont.OPTIONS = __assign(__assign({}, Base.OPTIONS), { fontURL: '@mathjax/mathjax-asana-font/js/chtml/woff2', dynamicPrefix: '@mathjax/mathjax-asana-font/js/chtml/dynamic' });
    MathJaxAsanaFont.defaultCssFamilyPrefix = 'MJX-ASNA-ZERO';
    MathJaxAsanaFont.defaultVariantLetters = {
        'normal': '',
        'bold': 'B',
        'italic': 'I',
        'bold-italic': 'BI',
        'double-struck': 'DS',
        'fraktur': 'F',
        'bold-fraktur': 'FB',
        'script': 'S',
        'bold-script': 'SB',
        'sans-serif': 'SS',
        'bold-sans-serif': 'SSB',
        'sans-serif-italic': 'SSI',
        'sans-serif-bold-italic': 'SSBI',
        'monospace': 'M',
        '-smallop': 'SO',
        '-largeop': 'LO',
        '-size3': 'S3',
        '-size4': 'S4',
        '-size5': 'S5',
        '-size6': 'S6',
        '-tex-calligraphic': 'C',
        '-tex-bold-calligraphic': 'CB',
        '-tex-oldstyle': 'OS',
        '-tex-variant': 'V',
        '-extend': 'E'
    };
    MathJaxAsanaFont.defaultDelimiters = delimiters_js_1.delimiters;
    MathJaxAsanaFont.defaultChars = {
        'normal': normal_js_1.normal,
        'bold': bold_js_1.bold,
        'italic': italic_js_1.italic,
        'bold-italic': bold_italic_js_1.boldItalic,
        'double-struck': double_struck_js_1.doubleStruck,
        'fraktur': fraktur_js_1.fraktur,
        'bold-fraktur': fraktur_bold_js_1.frakturBold,
        'script': script_js_1.script,
        'bold-script': script_bold_js_1.scriptBold,
        'sans-serif': sans_serif_js_1.sansSerif,
        'bold-sans-serif': sans_serif_bold_js_1.sansSerifBold,
        'sans-serif-italic': sans_serif_italic_js_1.sansSerifItalic,
        'sans-serif-bold-italic': sans_serif_bold_italic_js_1.sansSerifBoldItalic,
        'monospace': monospace_js_1.monospace,
        '-smallop': smallop_js_1.smallop,
        '-largeop': largeop_js_1.largeop,
        '-size3': size3_js_1.size3,
        '-size4': size4_js_1.size4,
        '-size5': size5_js_1.size5,
        '-size6': size6_js_1.size6,
        '-tex-calligraphic': tex_calligraphic_js_1.texCalligraphic,
        '-tex-bold-calligraphic': tex_calligraphic_bold_js_1.texCalligraphicBold,
        '-tex-oldstyle': tex_oldstyle_js_1.texOldstyle,
        '-tex-variant': tex_variant_js_1.texVariant,
        '-extend': extend_js_1.extend
    };
    MathJaxAsanaFont.defaultStyles = __assign(__assign({}, FontData_js_1.ChtmlFontData.defaultStyles), { 'mjx-container[jax="CHTML"] > mjx-math.ASNA-N[breakable] > *': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-N'
        }, '.ASNA-N': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-N'
        }, '.ASNA-B': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-B'
        }, '.ASNA-I': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-I'
        }, '.ASNA-BI': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-BI'
        }, '.ASNA-DS': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-DS'
        }, '.ASNA-F': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-F'
        }, '.ASNA-FB': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-FB'
        }, '.ASNA-S': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-S'
        }, '.ASNA-SB': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SB'
        }, '.ASNA-SS': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SS'
        }, '.ASNA-SSB': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SSB'
        }, '.ASNA-SSI': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SSI'
        }, '.ASNA-SSBI': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SSBI'
        }, '.ASNA-M': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-M'
        }, '.ASNA-SO': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-SO'
        }, '.ASNA-LO': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-LO'
        }, '.ASNA-S3': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-S3'
        }, '.ASNA-S4': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-S4'
        }, '.ASNA-S5': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-S5'
        }, '.ASNA-S6': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-S6'
        }, '.ASNA-C': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-C'
        }, '.ASNA-CB': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-CB'
        }, '.ASNA-OS': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-OS'
        }, '.ASNA-V': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-V'
        }, '.ASNA-E': {
            'font-family': 'MJX-ASNA-ZERO, MJX-ASNA-E'
        } });
    MathJaxAsanaFont.defaultFonts = __assign(__assign({}, FontData_js_1.ChtmlFontData.defaultFonts), { '@font-face /* MJX-ASNA-ZERO */': {
            'font-family': 'MJX-ASNA-ZERO',
            src: 'url("%%URL%%/mjx-asna-zero.woff2") format("woff2")'
        }, '@font-face /* MJX-BRK */': {
            'font-family': 'MJX-BRK',
            src: 'url("%%URL%%/mjx-asna-brk.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-N */': {
            'font-family': 'MJX-ASNA-N',
            src: 'url("%%URL%%/mjx-asna-n.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-B */': {
            'font-family': 'MJX-ASNA-B',
            src: 'url("%%URL%%/mjx-asna-b.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-I */': {
            'font-family': 'MJX-ASNA-I',
            src: 'url("%%URL%%/mjx-asna-i.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-BI */': {
            'font-family': 'MJX-ASNA-BI',
            src: 'url("%%URL%%/mjx-asna-bi.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-DS */': {
            'font-family': 'MJX-ASNA-DS',
            src: 'url("%%URL%%/mjx-asna-ds.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-F */': {
            'font-family': 'MJX-ASNA-F',
            src: 'url("%%URL%%/mjx-asna-f.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-FB */': {
            'font-family': 'MJX-ASNA-FB',
            src: 'url("%%URL%%/mjx-asna-fb.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-S */': {
            'font-family': 'MJX-ASNA-S',
            src: 'url("%%URL%%/mjx-asna-s.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SB */': {
            'font-family': 'MJX-ASNA-SB',
            src: 'url("%%URL%%/mjx-asna-sb.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SS */': {
            'font-family': 'MJX-ASNA-SS',
            src: 'url("%%URL%%/mjx-asna-ss.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SSB */': {
            'font-family': 'MJX-ASNA-SSB',
            src: 'url("%%URL%%/mjx-asna-ssb.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SSI */': {
            'font-family': 'MJX-ASNA-SSI',
            src: 'url("%%URL%%/mjx-asna-ssi.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SSBI */': {
            'font-family': 'MJX-ASNA-SSBI',
            src: 'url("%%URL%%/mjx-asna-ssbi.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-M */': {
            'font-family': 'MJX-ASNA-M',
            src: 'url("%%URL%%/mjx-asna-m.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-SO */': {
            'font-family': 'MJX-ASNA-SO',
            src: 'url("%%URL%%/mjx-asna-so.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-LO */': {
            'font-family': 'MJX-ASNA-LO',
            src: 'url("%%URL%%/mjx-asna-lo.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-S3 */': {
            'font-family': 'MJX-ASNA-S3',
            src: 'url("%%URL%%/mjx-asna-s3.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-S4 */': {
            'font-family': 'MJX-ASNA-S4',
            src: 'url("%%URL%%/mjx-asna-s4.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-S5 */': {
            'font-family': 'MJX-ASNA-S5',
            src: 'url("%%URL%%/mjx-asna-s5.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-S6 */': {
            'font-family': 'MJX-ASNA-S6',
            src: 'url("%%URL%%/mjx-asna-s6.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-C */': {
            'font-family': 'MJX-ASNA-C',
            src: 'url("%%URL%%/mjx-asna-c.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-CB */': {
            'font-family': 'MJX-ASNA-CB',
            src: 'url("%%URL%%/mjx-asna-cb.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-OS */': {
            'font-family': 'MJX-ASNA-OS',
            src: 'url("%%URL%%/mjx-asna-os.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-V */': {
            'font-family': 'MJX-ASNA-V',
            src: 'url("%%URL%%/mjx-asna-v.woff2") format("woff2")'
        }, '@font-face /* MJX-ASNA-E */': {
            'font-family': 'MJX-ASNA-E',
            src: 'url("%%URL%%/mjx-asna-e.woff2") format("woff2")'
        } });
    MathJaxAsanaFont.dynamicFiles = FontData_js_1.ChtmlFontData.defineDynamicFiles([
        ['latin', {
                'normal': [
                    [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0xFF]
                ]
            }],
        ['double-struck', {
                'normal': [
                    0x2102, 0x210D, 0x2115, 0x2119, 0x211A, 0x211D, 0x2124, [0x213C, 0x2140], [0x2145, 0x2149], 0x1D538, 0x1D539, [0x1D53B, 0x1D53E], [0x1D540, 0x1D544], 0x1D546, [0x1D54A, 0x1D550], [0x1D552, 0x1D56B], [0x1D7D8, 0x1D7E1]
                ],
                'double-struck': [
                    0x131, 0x237
                ]
            }],
        ['fraktur', {
                'normal': [
                    0x210C, 0x2111, 0x211C, 0x2128, 0x212D, 0x1D504, 0x1D505, [0x1D507, 0x1D50A], [0x1D50D, 0x1D514], [0x1D516, 0x1D51C], [0x1D51E, 0x1D537], [0x1D56C, 0x1D59F]
                ],
                'fraktur': [
                    0x131, 0x237
                ],
                'bold-fraktur': [
                    0x131, 0x237
                ]
            }],
        ['script', {
                'normal': [
                    0x210A, 0x210B, 0x2110, 0x2112, 0x2113, 0x2118, 0x211B, 0x212C, [0x212F, 0x2131], 0x2133, 0x2134, 0x1D49C, 0x1D49E, 0x1D49F, 0x1D4A2, 0x1D4A5, 0x1D4A6, [0x1D4A9, 0x1D4AC], [0x1D4AE, 0x1D4B9], 0x1D4BB, [0x1D4BD, 0x1D4C3], [0x1D4C5, 0x1D503]
                ],
                'script': [
                    0x131, 0x237
                ],
                'bold-script': [
                    0x131, 0x237
                ]
            }],
        ['sans-serif', {
                'normal': [
                    [0x2141, 0x2144], [0x1D5A0, 0x1D66F], [0x1D7E2, 0x1D7F5]
                ],
                'sans-serif': [
                    0x131, 0x237
                ],
                'bold-sans-serif': [
                    0x131, 0x237
                ],
                'sans-serif-italic': [
                    0x131, 0x237
                ],
                'sans-serif-bold-italic': [
                    0x131, 0x237
                ]
            }],
        ['monospace', {
                'normal': [
                    [0x1D670, 0x1D6A3], [0x1D7F6, 0x1D7FF]
                ],
                'monospace': [
                    0x131, 0x237
                ]
            }],
        ['calligraphic', {
                '-tex-calligraphic': [
                    [0x41, 0x5A]
                ],
                '-tex-bold-calligraphic': [
                    [0x41, 0x5A]
                ]
            }],
        ['cherokee', {
                'normal': [
                    [0x13A0, 0x13F4]
                ]
            }],
        ['greek', {
                'normal': [
                    0x37E, 0x382, [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3CE], 0x3D0, 0x3DC, 0x3DD, [0x10143, 0x10147]
                ]
            }],
        ['math', {
                'normal': [
                    0x220A, 0x220D, 0x221B, 0x221C, 0x223A, 0x223B, 0x224E, 0x224F, [0x2251, 0x2253], [0x2256, 0x225C], 0x225E, 0x228C, [0x22B6, 0x22B9], [0x22BB, 0x22BF], 0x22C7, [0x22D0, 0x22E1], [0x22E4, 0x22E9], [0x22F2, 0x22FF], [0x27C0, 0x27CA], [0x27CE, 0x27D7], [0x27DA, 0x27DC], [0x27DF, 0x27E5], [0x2981, 0x2996], [0x2999, 0x29F4], 0x29F6, [0x29FA, 0x29FF], 0x2A0A, 0x2A0B, [0x2A1D, 0x2A2E], [0x2A30, 0x2A3E], [0x2A40, 0x2A7C], [0x2A7F, 0x2A84], [0x2A8D, 0x2A94], [0x2A97, 0x2AAE], [0x2ABB, 0x2AC4], [0x2AC7, 0x2ACA], [0x2ACD, 0x2ADD], [0x2AEC, 0x2AF1], [0x2AF6, 0x2AFB], [0x2AFD, 0x2AFF]
                ]
            }, [0x27C5, 0x27C6, 0x29FC, 0x29FD]],
        ['symbols', {
                'normal': [
                    0xA1, 0xA2, 0xA4, 0xA6, [0xA9, 0xAB], 0xAE, 0xB2, 0xB3, [0xB9, 0xBF], 0x200C, 0x200D, 0x2017, 0x201A, 0x201B, 0x201E, 0x2022, 0x2024, 0x2025, 0x2030, 0x2031, 0x203B, 0x203C, [0x203E, 0x2040], 0x2045, 0x2046, 0x2050, 0x205D, 0x205E, 0x2070, 0x2071, [0x2074, 0x208E], [0x2090, 0x2094], 0x2100, 0x2101, 0x2103, 0x2105, 0x2106, 0x2109, 0x2116, 0x2117, 0x211E, [0x2120, 0x2122], 0x2129, 0x212E, [0x2139, 0x213B], 0x214B, 0x214D, 0x214E, [0x2150, 0x217F], 0x2184, [0x2186, 0x218B], 0x2300, [0x2304, 0x2306], 0x233D, 0x2422, 0x2423
                ],
                '-largeop': []
            }, [0x2017, 0x2045, 0x2046]],
        ['arrows', {
                'normal': [
                    0x219C, 0x219D, 0x219F, 0x21A1, 0x21A5, 0x21A7, 0x21A8, 0x21AD, [0x21AF, 0x21B5], 0x21B8, 0x21B9, [0x21D6, 0x21D9], [0x21DC, 0x21DF], [0x21E6, 0x21F4], [0x21F7, 0x21FF], 0x27A1, [0x27F0, 0x27F4], 0x27FF, [0x2900, 0x2909], [0x290C, 0x2949], 0x294C, 0x294D, 0x294F, 0x2951, 0x2954, 0x2955, 0x2958, 0x2959, 0x295C, 0x295D, [0x2960, 0x2969], [0x2970, 0x297B], [0x2B00, 0x2B12], [0x2B30, 0x2B4C]
                ],
                'bold': []
            }, [0x21A5, 0x21A7, 0x27FD, 0x27FE, 0x2906, 0x2907, 0x294C, 0x294D, 0x294F, 0x2951, 0x295C, 0x295D, 0x2960, 0x2961]],
        ['accents', {
                'normal': [
                    0xB8, 0x2DB, 0x332, 0x333, 0x33F, 0x342, 0x20D4, 0x20D5, [0x20D8, 0x20DA], [0x20DD, 0x20E0], [0x20E2, 0x20EB]
                ],
                '-smallop': [
                    0x332, 0x333, 0x33F
                ],
                '-largeop': [
                    0x332, 0x333, 0x33F
                ],
                '-size3': [
                    0x332, 0x333, 0x33F
                ]
            }, [0x332, 0x333, 0x33F, 0x20E9]],
        ['shapes', {
                'normal': [
                    [0x25AC, 0x25B1], 0x25C8, 0x25C9, 0x25CC, 0x25CE, [0x25D0, 0x25D3], [0x25D6, 0x25D8], 0x25DA, [0x25E7, 0x25EE], [0x25F0, 0x25F3], 0x2605, 0x2606, [0x2660, 0x2667], 0x2669, 0x266A, [0x266D, 0x266F], [0x2680, 0x2685], 0x2713, 0x2720, 0x2731, 0x2736, 0x27A1, [0x2B12, 0x2B22], [0x2B24, 0x2B28], [0x2B50, 0x2B52]
                ]
            }],
        ['stretchy', {
                'normal': [
                    0x221B, 0x221C, 0x2320, 0x2321, 0x23B2, 0x23B3, [0x2A0C, 0x2A1C]
                ],
                '-largeop': [
                    [0x2A0C, 0x2A1C]
                ],
                '-size3': [
                    [0x220F, 0x2211], [0x222B, 0x2233], [0x2A00, 0x2A09], [0x2A0C, 0x2A1C]
                ],
                '-size4': [
                    [0x220F, 0x2211], [0x222B, 0x2233], [0x2A0C, 0x2A1C]
                ]
            }, [[0x2A0C, 0x2A1C]]]
    ]);
    return MathJaxAsanaFont;
}(Base));
exports.MathJaxAsanaFont = MathJaxAsanaFont;
//# sourceMappingURL=chtml.js.map