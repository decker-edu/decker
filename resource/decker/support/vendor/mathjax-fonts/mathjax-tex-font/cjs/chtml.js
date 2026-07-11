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
exports.MathJaxTexFont = void 0;
var FontData_js_1 = require("@mathjax/src/cjs/output/chtml/FontData.js");
var common_js_1 = require("./common.js");
var normal_js_1 = require("./chtml/normal.js");
var bold_js_1 = require("./chtml/bold.js");
var italic_js_1 = require("./chtml/italic.js");
var bold_italic_js_1 = require("./chtml/bold-italic.js");
var fraktur_js_1 = require("./chtml/fraktur.js");
var fraktur_bold_js_1 = require("./chtml/fraktur-bold.js");
var sans_serif_js_1 = require("./chtml/sans-serif.js");
var sans_serif_bold_js_1 = require("./chtml/sans-serif-bold.js");
var sans_serif_italic_js_1 = require("./chtml/sans-serif-italic.js");
var monospace_js_1 = require("./chtml/monospace.js");
var smallop_js_1 = require("./chtml/smallop.js");
var largeop_js_1 = require("./chtml/largeop.js");
var tex_calligraphic_js_1 = require("./chtml/tex-calligraphic.js");
var tex_calligraphic_bold_js_1 = require("./chtml/tex-calligraphic-bold.js");
var tex_oldstyle_js_1 = require("./chtml/tex-oldstyle.js");
var tex_oldstyle_bold_js_1 = require("./chtml/tex-oldstyle-bold.js");
var tex_mathit_js_1 = require("./chtml/tex-mathit.js");
var tex_variant_js_1 = require("./chtml/tex-variant.js");
var size3_js_1 = require("./chtml/size3.js");
var size4_js_1 = require("./chtml/size4.js");
var delimiters_js_1 = require("./chtml/delimiters.js");
var Base = (0, common_js_1.CommonMathJaxTexFontMixin)(FontData_js_1.ChtmlFontData);
var MathJaxTexFont = (function (_super) {
    __extends(MathJaxTexFont, _super);
    function MathJaxTexFont() {
        var _this = _super.apply(this, __spreadArray([], __read(arguments), false)) || this;
        _this.cssFontPrefix = 'TEX';
        return _this;
    }
    MathJaxTexFont.NAME = 'MathJaxTex';
    MathJaxTexFont.OPTIONS = __assign(__assign({}, Base.OPTIONS), { fontURL: '@mathjax/mathjax-tex-font/js/chtml/woff2', dynamicPrefix: '@mathjax/mathjax-tex-font/js/chtml/dynamic' });
    MathJaxTexFont.defaultCssFamilyPrefix = 'MJX-TEX-ZERO';
    MathJaxTexFont.defaultVariantLetters = {
        'normal': '',
        'bold': 'B',
        'italic': 'I',
        'bold-italic': 'BI',
        'fraktur': 'F',
        'bold-fraktur': 'FB',
        'sans-serif': 'SS',
        'bold-sans-serif': 'SSB',
        'sans-serif-italic': 'SSI',
        'monospace': 'M',
        '-smallop': 'SO',
        '-largeop': 'LO',
        '-tex-calligraphic': 'C',
        '-tex-bold-calligraphic': 'CB',
        '-tex-oldstyle': 'OS',
        '-tex-bold-oldstyle': 'OB',
        '-tex-mathit': 'MI',
        '-tex-variant': 'V',
        '-size3': 'S3',
        '-size4': 'S4'
    };
    MathJaxTexFont.defaultDelimiters = delimiters_js_1.delimiters;
    MathJaxTexFont.defaultChars = {
        'normal': normal_js_1.normal,
        'bold': bold_js_1.bold,
        'italic': italic_js_1.italic,
        'bold-italic': bold_italic_js_1.boldItalic,
        'fraktur': fraktur_js_1.fraktur,
        'bold-fraktur': fraktur_bold_js_1.frakturBold,
        'sans-serif': sans_serif_js_1.sansSerif,
        'bold-sans-serif': sans_serif_bold_js_1.sansSerifBold,
        'sans-serif-italic': sans_serif_italic_js_1.sansSerifItalic,
        'monospace': monospace_js_1.monospace,
        '-smallop': smallop_js_1.smallop,
        '-largeop': largeop_js_1.largeop,
        '-tex-calligraphic': tex_calligraphic_js_1.texCalligraphic,
        '-tex-bold-calligraphic': tex_calligraphic_bold_js_1.texCalligraphicBold,
        '-tex-oldstyle': tex_oldstyle_js_1.texOldstyle,
        '-tex-bold-oldstyle': tex_oldstyle_bold_js_1.texOldstyleBold,
        '-tex-mathit': tex_mathit_js_1.texMathit,
        '-tex-variant': tex_variant_js_1.texVariant,
        '-size3': size3_js_1.size3,
        '-size4': size4_js_1.size4
    };
    MathJaxTexFont.defaultStyles = __assign(__assign({}, FontData_js_1.ChtmlFontData.defaultStyles), { 'mjx-container[jax="CHTML"] > mjx-math.TEX-N[breakable] > *': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-N'
        }, '.TEX-N': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-N'
        }, '.TEX-B': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-B'
        }, '.TEX-I': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-I'
        }, '.TEX-BI': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-BI'
        }, '.TEX-F': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-F'
        }, '.TEX-FB': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-FB'
        }, '.TEX-SS': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-SS'
        }, '.TEX-SSB': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-SSB'
        }, '.TEX-SSI': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-SSI'
        }, '.TEX-M': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-M'
        }, '.TEX-SO': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-SO'
        }, '.TEX-LO': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-LO'
        }, '.TEX-C': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-C'
        }, '.TEX-CB': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-CB'
        }, '.TEX-OS': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-OS'
        }, '.TEX-OB': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-OB'
        }, '.TEX-MI': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-MI'
        }, '.TEX-V': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-V'
        }, '.TEX-S3': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-S3'
        }, '.TEX-S4': {
            'font-family': 'MJX-TEX-ZERO, MJX-TEX-S4'
        } });
    MathJaxTexFont.defaultFonts = __assign(__assign({}, FontData_js_1.ChtmlFontData.defaultFonts), { '@font-face /* MJX-TEX-ZERO */': {
            'font-family': 'MJX-TEX-ZERO',
            src: 'url("%%URL%%/mjx-tex-zero.woff2") format("woff2")'
        }, '@font-face /* MJX-BRK */': {
            'font-family': 'MJX-BRK',
            src: 'url("%%URL%%/mjx-tex-brk.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-N */': {
            'font-family': 'MJX-TEX-N',
            src: 'url("%%URL%%/mjx-tex-n.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-B */': {
            'font-family': 'MJX-TEX-B',
            src: 'url("%%URL%%/mjx-tex-b.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-I */': {
            'font-family': 'MJX-TEX-I',
            src: 'url("%%URL%%/mjx-tex-i.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-BI */': {
            'font-family': 'MJX-TEX-BI',
            src: 'url("%%URL%%/mjx-tex-bi.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-F */': {
            'font-family': 'MJX-TEX-F',
            src: 'url("%%URL%%/mjx-tex-f.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-FB */': {
            'font-family': 'MJX-TEX-FB',
            src: 'url("%%URL%%/mjx-tex-fb.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-SS */': {
            'font-family': 'MJX-TEX-SS',
            src: 'url("%%URL%%/mjx-tex-ss.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-SSB */': {
            'font-family': 'MJX-TEX-SSB',
            src: 'url("%%URL%%/mjx-tex-ssb.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-SSI */': {
            'font-family': 'MJX-TEX-SSI',
            src: 'url("%%URL%%/mjx-tex-ssi.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-M */': {
            'font-family': 'MJX-TEX-M',
            src: 'url("%%URL%%/mjx-tex-m.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-SO */': {
            'font-family': 'MJX-TEX-SO',
            src: 'url("%%URL%%/mjx-tex-so.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-LO */': {
            'font-family': 'MJX-TEX-LO',
            src: 'url("%%URL%%/mjx-tex-lo.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-C */': {
            'font-family': 'MJX-TEX-C',
            src: 'url("%%URL%%/mjx-tex-c.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-CB */': {
            'font-family': 'MJX-TEX-CB',
            src: 'url("%%URL%%/mjx-tex-cb.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-OS */': {
            'font-family': 'MJX-TEX-OS',
            src: 'url("%%URL%%/mjx-tex-os.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-OB */': {
            'font-family': 'MJX-TEX-OB',
            src: 'url("%%URL%%/mjx-tex-ob.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-MI */': {
            'font-family': 'MJX-TEX-MI',
            src: 'url("%%URL%%/mjx-tex-mi.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-V */': {
            'font-family': 'MJX-TEX-V',
            src: 'url("%%URL%%/mjx-tex-v.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-S3 */': {
            'font-family': 'MJX-TEX-S3',
            src: 'url("%%URL%%/mjx-tex-s3.woff2") format("woff2")'
        }, '@font-face /* MJX-TEX-S4 */': {
            'font-family': 'MJX-TEX-S4',
            src: 'url("%%URL%%/mjx-tex-s4.woff2") format("woff2")'
        } });
    return MathJaxTexFont;
}(Base));
exports.MathJaxTexFont = MathJaxTexFont;
//# sourceMappingURL=chtml.js.map