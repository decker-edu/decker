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
var __values = (this && this.__values) || function(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.MathJaxTexFont = void 0;
var FontData_js_1 = require("@mathjax/src/cjs/output/svg/FontData.js");
var common_js_1 = require("./common.js");
var normal_js_1 = require("./svg/normal.js");
var bold_js_1 = require("./svg/bold.js");
var italic_js_1 = require("./svg/italic.js");
var bold_italic_js_1 = require("./svg/bold-italic.js");
var fraktur_js_1 = require("./svg/fraktur.js");
var fraktur_bold_js_1 = require("./svg/fraktur-bold.js");
var sans_serif_js_1 = require("./svg/sans-serif.js");
var sans_serif_bold_js_1 = require("./svg/sans-serif-bold.js");
var sans_serif_italic_js_1 = require("./svg/sans-serif-italic.js");
var monospace_js_1 = require("./svg/monospace.js");
var smallop_js_1 = require("./svg/smallop.js");
var largeop_js_1 = require("./svg/largeop.js");
var tex_calligraphic_js_1 = require("./svg/tex-calligraphic.js");
var tex_calligraphic_bold_js_1 = require("./svg/tex-calligraphic-bold.js");
var tex_oldstyle_js_1 = require("./svg/tex-oldstyle.js");
var tex_oldstyle_bold_js_1 = require("./svg/tex-oldstyle-bold.js");
var tex_mathit_js_1 = require("./svg/tex-mathit.js");
var tex_variant_js_1 = require("./svg/tex-variant.js");
var size3_js_1 = require("./svg/size3.js");
var size4_js_1 = require("./svg/size4.js");
var delimiters_js_1 = require("./svg/delimiters.js");
var Base = (0, common_js_1.CommonMathJaxTexFontMixin)(FontData_js_1.SvgFontData);
var MathJaxTexFont = (function (_super) {
    __extends(MathJaxTexFont, _super);
    function MathJaxTexFont(options) {
        var e_1, _a;
        if (options === void 0) { options = {}; }
        var _this = _super.call(this, options) || this;
        var CLASS = _this.constructor;
        try {
            for (var _b = __values(Object.keys(_this.variant)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var variant = _c.value;
                _this.variant[variant].cacheID = 'TEX-' + (CLASS.variantCacheIds[variant] || 'N');
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return _this;
    }
    MathJaxTexFont.NAME = 'MathJaxTex';
    MathJaxTexFont.OPTIONS = __assign(__assign({}, Base.OPTIONS), { dynamicPrefix: '@mathjax/mathjax-tex-font/svg/dynamic' });
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
    MathJaxTexFont.variantCacheIds = {
        'normal': 'N',
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
    return MathJaxTexFont;
}(Base));
exports.MathJaxTexFont = MathJaxTexFont;
//# sourceMappingURL=svg.js.map