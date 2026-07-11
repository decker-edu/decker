import { ChtmlFontData } from '@mathjax/src/mjs/output/chtml/FontData.js';
import { CommonMathJaxTexFontMixin } from './common.js';
import { normal } from './chtml/normal.js';
import { bold } from './chtml/bold.js';
import { italic } from './chtml/italic.js';
import { boldItalic } from './chtml/bold-italic.js';
import { fraktur } from './chtml/fraktur.js';
import { frakturBold } from './chtml/fraktur-bold.js';
import { sansSerif } from './chtml/sans-serif.js';
import { sansSerifBold } from './chtml/sans-serif-bold.js';
import { sansSerifItalic } from './chtml/sans-serif-italic.js';
import { monospace } from './chtml/monospace.js';
import { smallop } from './chtml/smallop.js';
import { largeop } from './chtml/largeop.js';
import { texCalligraphic } from './chtml/tex-calligraphic.js';
import { texCalligraphicBold } from './chtml/tex-calligraphic-bold.js';
import { texOldstyle } from './chtml/tex-oldstyle.js';
import { texOldstyleBold } from './chtml/tex-oldstyle-bold.js';
import { texMathit } from './chtml/tex-mathit.js';
import { texVariant } from './chtml/tex-variant.js';
import { size3 } from './chtml/size3.js';
import { size4 } from './chtml/size4.js';
import { delimiters } from './chtml/delimiters.js';
const Base = CommonMathJaxTexFontMixin(ChtmlFontData);
export class MathJaxTexFont extends Base {
    constructor() {
        super(...arguments);
        this.cssFontPrefix = 'TEX';
    }
}
MathJaxTexFont.NAME = 'MathJaxTex';
MathJaxTexFont.OPTIONS = Object.assign(Object.assign({}, Base.OPTIONS), { fontURL: '@mathjax/mathjax-tex-font/js/chtml/woff2', dynamicPrefix: '@mathjax/mathjax-tex-font/js/chtml/dynamic' });
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
MathJaxTexFont.defaultDelimiters = delimiters;
MathJaxTexFont.defaultChars = {
    'normal': normal,
    'bold': bold,
    'italic': italic,
    'bold-italic': boldItalic,
    'fraktur': fraktur,
    'bold-fraktur': frakturBold,
    'sans-serif': sansSerif,
    'bold-sans-serif': sansSerifBold,
    'sans-serif-italic': sansSerifItalic,
    'monospace': monospace,
    '-smallop': smallop,
    '-largeop': largeop,
    '-tex-calligraphic': texCalligraphic,
    '-tex-bold-calligraphic': texCalligraphicBold,
    '-tex-oldstyle': texOldstyle,
    '-tex-bold-oldstyle': texOldstyleBold,
    '-tex-mathit': texMathit,
    '-tex-variant': texVariant,
    '-size3': size3,
    '-size4': size4
};
MathJaxTexFont.defaultStyles = Object.assign(Object.assign({}, ChtmlFontData.defaultStyles), { 'mjx-container[jax="CHTML"] > mjx-math.TEX-N[breakable] > *': {
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
MathJaxTexFont.defaultFonts = Object.assign(Object.assign({}, ChtmlFontData.defaultFonts), { '@font-face /* MJX-TEX-ZERO */': {
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
//# sourceMappingURL=chtml.js.map