import { ChtmlFontData } from '@mathjax/src/mjs/output/chtml/FontData.js';
import { CommonMathJaxFiraFontMixin } from './common.js';
import { normal } from './chtml/normal.js';
import { bold } from './chtml/bold.js';
import { italic } from './chtml/italic.js';
import { boldItalic } from './chtml/bold-italic.js';
import { sansSerifItalic } from './chtml/sans-serif-italic.js';
import { sansSerifBoldItalic } from './chtml/sans-serif-bold-italic.js';
import { monospace } from './chtml/monospace.js';
import { smallop } from './chtml/smallop.js';
import { largeop } from './chtml/largeop.js';
import { size3 } from './chtml/size3.js';
import { size4 } from './chtml/size4.js';
import { size5 } from './chtml/size5.js';
import { size6 } from './chtml/size6.js';
import { size7 } from './chtml/size7.js';
import { size8 } from './chtml/size8.js';
import { size9 } from './chtml/size9.js';
import { size10 } from './chtml/size10.js';
import { size11 } from './chtml/size11.js';
import { size12 } from './chtml/size12.js';
import { size13 } from './chtml/size13.js';
import { size14 } from './chtml/size14.js';
import { size15 } from './chtml/size15.js';
import { texVariant } from './chtml/tex-variant.js';
import { texMathit } from './chtml/tex-mathit.js';
import { texOldstyle } from './chtml/tex-oldstyle.js';
import { texOldstyleBold } from './chtml/tex-oldstyle-bold.js';
import { texCalligraphic } from './chtml/tex-calligraphic.js';
import { texCalligraphicBold } from './chtml/tex-calligraphic-bold.js';
import { lfTp } from './chtml/lf-tp.js';
import { rtBt } from './chtml/rt-bt.js';
import { ext } from './chtml/ext.js';
import { mid } from './chtml/mid.js';
import { up } from './chtml/up.js';
import { dup } from './chtml/dup.js';
import { delimiters } from './chtml/delimiters.js';
const Base = CommonMathJaxFiraFontMixin(ChtmlFontData);
export class MathJaxFiraFont extends Base {
    constructor() {
        super(...arguments);
        this.cssFontPrefix = 'FIRA';
    }
}
MathJaxFiraFont.NAME = 'MathJaxFira';
MathJaxFiraFont.OPTIONS = Object.assign(Object.assign({}, Base.OPTIONS), { fontURL: '@mathjax/mathjax-fira-font/js/chtml/woff2', dynamicPrefix: '@mathjax/mathjax-fira-font/js/chtml/dynamic' });
MathJaxFiraFont.defaultCssFamilyPrefix = 'MJX-FIRA-ZERO';
MathJaxFiraFont.defaultVariantLetters = {
    'normal': '',
    'bold': 'B',
    'italic': 'I',
    'bold-italic': 'BI',
    'sans-serif-italic': 'SSI',
    'sans-serif-bold-italic': 'SSBI',
    'monospace': 'M',
    '-smallop': 'SO',
    '-largeop': 'LO',
    '-size3': 'S3',
    '-size4': 'S4',
    '-size5': 'S5',
    '-size6': 'S6',
    '-size7': 'S7',
    '-size8': 'S8',
    '-size9': 'S9',
    '-size10': 'S10',
    '-size11': 'S11',
    '-size12': 'S12',
    '-size13': 'S13',
    '-size14': 'S14',
    '-size15': 'S15',
    '-tex-variant': 'V',
    '-tex-mathit': 'MI',
    '-tex-oldstyle': 'OS',
    '-tex-bold-oldstyle': 'OB',
    '-tex-calligraphic': 'C',
    '-tex-bold-calligraphic': 'CB',
    '-lf-tp': 'LT',
    '-rt-bt': 'RB',
    '-ext': 'E',
    '-mid': 'Me',
    '-up': 'U',
    '-dup': 'D'
};
MathJaxFiraFont.defaultDelimiters = delimiters;
MathJaxFiraFont.defaultChars = {
    'normal': normal,
    'bold': bold,
    'italic': italic,
    'bold-italic': boldItalic,
    'sans-serif-italic': sansSerifItalic,
    'sans-serif-bold-italic': sansSerifBoldItalic,
    'monospace': monospace,
    '-smallop': smallop,
    '-largeop': largeop,
    '-size3': size3,
    '-size4': size4,
    '-size5': size5,
    '-size6': size6,
    '-size7': size7,
    '-size8': size8,
    '-size9': size9,
    '-size10': size10,
    '-size11': size11,
    '-size12': size12,
    '-size13': size13,
    '-size14': size14,
    '-size15': size15,
    '-tex-variant': texVariant,
    '-tex-mathit': texMathit,
    '-tex-oldstyle': texOldstyle,
    '-tex-bold-oldstyle': texOldstyleBold,
    '-tex-calligraphic': texCalligraphic,
    '-tex-bold-calligraphic': texCalligraphicBold,
    '-lf-tp': lfTp,
    '-rt-bt': rtBt,
    '-ext': ext,
    '-mid': mid,
    '-up': up,
    '-dup': dup
};
MathJaxFiraFont.defaultStyles = Object.assign(Object.assign({}, ChtmlFontData.defaultStyles), { 'mjx-container[jax="CHTML"] > mjx-math.FIRA-N[breakable] > *': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-N'
    }, '.FIRA-N': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-N'
    }, '.FIRA-B': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-B'
    }, '.FIRA-I': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-I'
    }, '.FIRA-BI': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-BI'
    }, '.FIRA-SSI': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-SSI'
    }, '.FIRA-SSBI': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-SSBI'
    }, '.FIRA-M': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-M'
    }, '.FIRA-SO': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-SO'
    }, '.FIRA-LO': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-LO'
    }, '.FIRA-S3': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S3'
    }, '.FIRA-S4': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S4'
    }, '.FIRA-S5': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S5'
    }, '.FIRA-S6': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S6'
    }, '.FIRA-S7': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S7'
    }, '.FIRA-S8': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S8'
    }, '.FIRA-S9': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S9'
    }, '.FIRA-S10': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S10'
    }, '.FIRA-S11': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S11'
    }, '.FIRA-S12': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S12'
    }, '.FIRA-S13': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S13'
    }, '.FIRA-S14': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S14'
    }, '.FIRA-S15': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-S15'
    }, '.FIRA-V': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-V'
    }, '.FIRA-MI': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-MI'
    }, '.FIRA-OS': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-OS'
    }, '.FIRA-OB': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-OB'
    }, '.FIRA-C': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-C'
    }, '.FIRA-CB': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-CB'
    }, '.FIRA-LT': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-LT'
    }, '.FIRA-RB': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-RB'
    }, '.FIRA-E': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-E'
    }, '.FIRA-Me': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-Me'
    }, '.FIRA-U': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-U'
    }, '.FIRA-D': {
        'font-family': 'MJX-FIRA-ZERO, MJX-FIRA-D'
    } });
MathJaxFiraFont.defaultFonts = Object.assign(Object.assign({}, ChtmlFontData.defaultFonts), { '@font-face /* MJX-FIRA-ZERO */': {
        'font-family': 'MJX-FIRA-ZERO',
        src: 'url("%%URL%%/mjx-fira-zero.woff2") format("woff2")'
    }, '@font-face /* MJX-BRK */': {
        'font-family': 'MJX-BRK',
        src: 'url("%%URL%%/mjx-fira-brk.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-N */': {
        'font-family': 'MJX-FIRA-N',
        src: 'url("%%URL%%/mjx-fira-n.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-B */': {
        'font-family': 'MJX-FIRA-B',
        src: 'url("%%URL%%/mjx-fira-b.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-I */': {
        'font-family': 'MJX-FIRA-I',
        src: 'url("%%URL%%/mjx-fira-i.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-BI */': {
        'font-family': 'MJX-FIRA-BI',
        src: 'url("%%URL%%/mjx-fira-bi.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-SSI */': {
        'font-family': 'MJX-FIRA-SSI',
        src: 'url("%%URL%%/mjx-fira-ssi.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-SSBI */': {
        'font-family': 'MJX-FIRA-SSBI',
        src: 'url("%%URL%%/mjx-fira-ssbi.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-M */': {
        'font-family': 'MJX-FIRA-M',
        src: 'url("%%URL%%/mjx-fira-m.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-SO */': {
        'font-family': 'MJX-FIRA-SO',
        src: 'url("%%URL%%/mjx-fira-so.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-LO */': {
        'font-family': 'MJX-FIRA-LO',
        src: 'url("%%URL%%/mjx-fira-lo.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S3 */': {
        'font-family': 'MJX-FIRA-S3',
        src: 'url("%%URL%%/mjx-fira-s3.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S4 */': {
        'font-family': 'MJX-FIRA-S4',
        src: 'url("%%URL%%/mjx-fira-s4.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S5 */': {
        'font-family': 'MJX-FIRA-S5',
        src: 'url("%%URL%%/mjx-fira-s5.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S6 */': {
        'font-family': 'MJX-FIRA-S6',
        src: 'url("%%URL%%/mjx-fira-s6.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S7 */': {
        'font-family': 'MJX-FIRA-S7',
        src: 'url("%%URL%%/mjx-fira-s7.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S8 */': {
        'font-family': 'MJX-FIRA-S8',
        src: 'url("%%URL%%/mjx-fira-s8.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S9 */': {
        'font-family': 'MJX-FIRA-S9',
        src: 'url("%%URL%%/mjx-fira-s9.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S10 */': {
        'font-family': 'MJX-FIRA-S10',
        src: 'url("%%URL%%/mjx-fira-s10.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S11 */': {
        'font-family': 'MJX-FIRA-S11',
        src: 'url("%%URL%%/mjx-fira-s11.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S12 */': {
        'font-family': 'MJX-FIRA-S12',
        src: 'url("%%URL%%/mjx-fira-s12.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S13 */': {
        'font-family': 'MJX-FIRA-S13',
        src: 'url("%%URL%%/mjx-fira-s13.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S14 */': {
        'font-family': 'MJX-FIRA-S14',
        src: 'url("%%URL%%/mjx-fira-s14.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-S15 */': {
        'font-family': 'MJX-FIRA-S15',
        src: 'url("%%URL%%/mjx-fira-s15.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-V */': {
        'font-family': 'MJX-FIRA-V',
        src: 'url("%%URL%%/mjx-fira-v.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-MI */': {
        'font-family': 'MJX-FIRA-MI',
        src: 'url("%%URL%%/mjx-fira-mi.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-OS */': {
        'font-family': 'MJX-FIRA-OS',
        src: 'url("%%URL%%/mjx-fira-os.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-OB */': {
        'font-family': 'MJX-FIRA-OB',
        src: 'url("%%URL%%/mjx-fira-ob.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-C */': {
        'font-family': 'MJX-FIRA-C',
        src: 'url("%%URL%%/mjx-fira-c.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-CB */': {
        'font-family': 'MJX-FIRA-CB',
        src: 'url("%%URL%%/mjx-fira-cb.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-LT */': {
        'font-family': 'MJX-FIRA-LT',
        src: 'url("%%URL%%/mjx-fira-lt.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-RB */': {
        'font-family': 'MJX-FIRA-RB',
        src: 'url("%%URL%%/mjx-fira-rb.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-E */': {
        'font-family': 'MJX-FIRA-E',
        src: 'url("%%URL%%/mjx-fira-e.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-Me */': {
        'font-family': 'MJX-FIRA-Me',
        src: 'url("%%URL%%/mjx-fira-me.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-U */': {
        'font-family': 'MJX-FIRA-U',
        src: 'url("%%URL%%/mjx-fira-u.woff2") format("woff2")'
    }, '@font-face /* MJX-FIRA-D */': {
        'font-family': 'MJX-FIRA-D',
        src: 'url("%%URL%%/mjx-fira-d.woff2") format("woff2")'
    } });
MathJaxFiraFont.dynamicFiles = ChtmlFontData.defineDynamicFiles([
    ['latin', {
            'normal': [
                [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0x130], [0x132, 0x236], [0x238, 0x24F], [0x1E02, 0x1E05], [0x1E0A, 0x1E0F], 0x1E12, 0x1E13, 0x1E1E, 0x1E1F, 0x1E24, 0x1E25, 0x1E2E, 0x1E2F, 0x1E36, 0x1E37, [0x1E3C, 0x1E41], [0x1E44, 0x1E47], [0x1E4A, 0x1E4D], [0x1E50, 0x1E57], 0x1E5A, 0x1E5B, [0x1E60, 0x1E63], [0x1E6A, 0x1E71], 0x1E78, 0x1E79, [0x1E80, 0x1E85], 0x1E92, 0x1E93, 0x1E9E, [0x1EA0, 0x1EF9], [0x2C64, 0x2C66], 0x2C6D, [0x2C71, 0x2C73], [0xA789, 0xA78D], 0xA7AA, 0xA7AD, 0xA7AE, [0xA7B2, 0xA7B7], 0xAB53
            ]
        }],
    ['latin-b', {
            'bold': [
                [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0x130], [0x132, 0x236], [0x238, 0x24F], [0x1E02, 0x1E05], [0x1E0A, 0x1E0F], 0x1E12, 0x1E13, 0x1E1E, 0x1E1F, 0x1E24, 0x1E25, 0x1E2E, 0x1E2F, 0x1E36, 0x1E37, [0x1E3C, 0x1E41], [0x1E44, 0x1E47], [0x1E4A, 0x1E4D], [0x1E50, 0x1E57], 0x1E5A, 0x1E5B, [0x1E60, 0x1E63], [0x1E6A, 0x1E71], 0x1E78, 0x1E79, [0x1E80, 0x1E85], 0x1E92, 0x1E93, 0x1E9E, [0x1EA0, 0x1EF9], [0x2C64, 0x2C66], 0x2C6D, [0x2C71, 0x2C73]
            ]
        }],
    ['latin-i', {
            'italic': [
                [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0x130], [0x132, 0x236], [0x238, 0x24F], [0x1E02, 0x1E05], [0x1E0A, 0x1E0F], 0x1E12, 0x1E13, 0x1E1E, 0x1E1F, 0x1E24, 0x1E25, 0x1E2E, 0x1E2F, 0x1E36, 0x1E37, [0x1E3C, 0x1E41], [0x1E44, 0x1E47], [0x1E4A, 0x1E4D], [0x1E50, 0x1E57], 0x1E5A, 0x1E5B, [0x1E60, 0x1E63], [0x1E6A, 0x1E71], 0x1E78, 0x1E79, [0x1E80, 0x1E85], 0x1E92, 0x1E93, 0x1E9E, [0x1EA0, 0x1EF9], [0x2C64, 0x2C66], 0x2C6D, [0x2C71, 0x2C73]
            ]
        }],
    ['latin-bi', {
            'bold-italic': [
                [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0x130], [0x132, 0x236], [0x238, 0x24F], [0x1E02, 0x1E05], [0x1E0A, 0x1E0F], 0x1E12, 0x1E13, 0x1E1E, 0x1E1F, 0x1E24, 0x1E25, 0x1E2E, 0x1E2F, 0x1E36, 0x1E37, [0x1E3C, 0x1E41], [0x1E44, 0x1E47], [0x1E4A, 0x1E4D], [0x1E50, 0x1E57], 0x1E5A, 0x1E5B, [0x1E60, 0x1E63], [0x1E6A, 0x1E71], 0x1E78, 0x1E79, [0x1E80, 0x1E85], 0x1E92, 0x1E93, 0x1E9E, [0x1EA0, 0x1EF9], [0x2C64, 0x2C66], 0x2C6D, [0x2C71, 0x2C73]
            ]
        }],
    ['latin-m', {
            'monospace': [
                [0xC0, 0xD6], [0xD8, 0xEF], [0xF1, 0xF6], [0xF8, 0x130], [0x132, 0x17E], 0x192, [0x1FC, 0x1FF], [0x218, 0x21B], [0x1E80, 0x1E85], 0x1EF2, 0x1EF3
            ]
        }],
    ['greek', {
            'normal': [
                [0x370, 0x377], [0x37A, 0x37F], [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3D0], 0x3D3, 0x3D4, [0x3D7, 0x3E1], 0x3F2, 0x3F3, [0x3F7, 0x3FF], [0x1F00, 0x1F15], [0x1F18, 0x1F1D], [0x1F20, 0x1F45], [0x1F48, 0x1F4D], [0x1F50, 0x1F57], 0x1F59, 0x1F5B, 0x1F5D, [0x1F5F, 0x1F7D], [0x1F80, 0x1FB4], [0x1FB6, 0x1FC4], [0x1FC6, 0x1FD3], [0x1FD6, 0x1FDB], [0x1FDD, 0x1FEF], [0x1FF2, 0x1FF4], [0x1FF6, 0x1FFE]
            ],
            'bold': [
                [0x370, 0x377], [0x37A, 0x37F], [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3D0], 0x3D3, 0x3D4, [0x3D7, 0x3DB], [0x3DE, 0x3E1], 0x3F2, 0x3F3, [0x3F7, 0x3FF], [0x1F00, 0x1F15], [0x1F18, 0x1F1D], [0x1F20, 0x1F45], [0x1F48, 0x1F4D], [0x1F50, 0x1F57], 0x1F59, 0x1F5B, 0x1F5D, [0x1F5F, 0x1F7D], [0x1F80, 0x1FB4], [0x1FB6, 0x1FC4], [0x1FC6, 0x1FD3], [0x1FD6, 0x1FDB], [0x1FDD, 0x1FEF], [0x1FF2, 0x1FF4], [0x1FF6, 0x1FFE]
            ],
            'italic': [
                [0x370, 0x377], [0x37A, 0x37F], [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3D0], 0x3D3, 0x3D4, [0x3D7, 0x3E1], 0x3F2, 0x3F3, [0x3F7, 0x3FF], [0x1F00, 0x1F15], [0x1F18, 0x1F1D], [0x1F20, 0x1F45], [0x1F48, 0x1F4D], [0x1F50, 0x1F57], 0x1F59, 0x1F5B, 0x1F5D, [0x1F5F, 0x1F7D], [0x1F80, 0x1FB4], [0x1FB6, 0x1FC4], [0x1FC6, 0x1FD3], [0x1FD6, 0x1FDB], [0x1FDD, 0x1FEF], [0x1FF2, 0x1FF4], [0x1FF6, 0x1FFE]
            ],
            'bold-italic': [
                [0x370, 0x377], [0x37A, 0x37F], [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3D0], 0x3D3, 0x3D4, [0x3D7, 0x3E1], 0x3F2, 0x3F3, [0x3F7, 0x3FF], [0x1F00, 0x1F15], [0x1F18, 0x1F1D], [0x1F20, 0x1F45], [0x1F48, 0x1F4D], [0x1F50, 0x1F57], 0x1F59, 0x1F5B, 0x1F5D, [0x1F5F, 0x1F7D], [0x1F80, 0x1FB4], [0x1FB6, 0x1FC4], [0x1FC6, 0x1FD3], [0x1FD6, 0x1FDB], [0x1FDD, 0x1FEF], [0x1FF2, 0x1FF4], [0x1FF6, 0x1FFE]
            ],
            'monospace': [
                [0x370, 0x377], [0x37A, 0x37F], [0x384, 0x38A], 0x38C, [0x38E, 0x390], [0x3AA, 0x3B0], [0x3CA, 0x3D0], 0x3D3, 0x3D4, [0x3D7, 0x3E1], 0x3F2, 0x3F3, [0x3F7, 0x3FF], [0x1F00, 0x1F15], [0x1F18, 0x1F1D], [0x1F20, 0x1F45], [0x1F48, 0x1F4D], [0x1F50, 0x1F57], 0x1F59, 0x1F5B, 0x1F5D, [0x1F5F, 0x1F7D], [0x1F80, 0x1FB4], [0x1FB6, 0x1FC4], [0x1FC6, 0x1FD3], [0x1FD6, 0x1FDB], [0x1FDD, 0x1FEF], [0x1FF2, 0x1FF4], [0x1FF6, 0x1FFE]
            ]
        }],
    ['cyrillic', {
            'normal': [
                [0x400, 0x479], [0x48A, 0x52F]
            ],
            'bold': [
                [0x400, 0x479], [0x48A, 0x52F]
            ],
            'italic': [
                [0x400, 0x479], [0x48A, 0x52F]
            ],
            'bold-italic': [
                [0x400, 0x479], [0x48A, 0x52F]
            ],
            'monospace': [
                [0x400, 0x479], [0x48A, 0x52F]
            ]
        }],
    ['phonetics', {
            'normal': [
                [0x250, 0x2AF], 0x1D4A, [0x1D6C, 0x1D76], 0x1D7B, 0x1D91, 0x1DBF
            ],
            'bold': [
                [0x250, 0x2AF], 0x1D4A, [0x1D6C, 0x1D76], 0x1D7B, 0x1D91, 0x1DBF
            ],
            'italic': [
                [0x250, 0x2AF], 0x1D4A, [0x1D6C, 0x1D76], 0x1D7B, 0x1D91, 0x1DBF
            ],
            'bold-italic': [
                [0x250, 0x2AF], 0x1D4A, [0x1D6C, 0x1D76], 0x1D7B, 0x1D91, 0x1DBF
            ]
        }],
    ['double-struck', {
            'normal': [
                0x2102, 0x210D, 0x2115, 0x2119, 0x211A, 0x211D, 0x2124, 0x2140, 0x1D538, 0x1D539, [0x1D53B, 0x1D53E], [0x1D540, 0x1D544], 0x1D546, [0x1D54A, 0x1D550], [0x1D552, 0x1D56B]
            ]
        }, [0x2140]],
    ['fraktur', {
            'normal': [
                0x210C, 0x2111, 0x211C, 0x2128, 0x212D, 0x1D504, 0x1D505, [0x1D507, 0x1D50A], [0x1D50D, 0x1D514], [0x1D516, 0x1D51C], [0x1D51E, 0x1D537]
            ]
        }],
    ['script', {
            'normal': [
                0x210A, 0x210B, 0x2110, 0x2112, 0x2113, 0x211B, 0x212C, [0x212F, 0x2131], 0x2133, 0x2134, 0x1D49C, 0x1D49E, 0x1D49F, 0x1D4A2, 0x1D4A5, 0x1D4A6, [0x1D4A9, 0x1D4AC], [0x1D4AE, 0x1D4B9], 0x1D4BB, [0x1D4BD, 0x1D4C3], [0x1D4C5, 0x1D4CF]
            ]
        }],
    ['sans-serif', {
            'normal': [
                [0x1D5A0, 0x1D66F], [0x1D7E2, 0x1D7F5]
            ]
        }],
    ['monospace', {
            'normal': [
                [0x1D670, 0x1D6A3], [0x1D7F6, 0x1D7FF]
            ],
            'monospace': [
                [0x20, 0x2F], [0x3A, 0x40], [0x5B, 0x60], [0x7B, 0x7E], 0xA0, 0xA8, 0xB0, 0xB4, 0x131, 0x237, 0x2C6, 0x2C7, 0x2C9, [0x2D8, 0x2DC], [0x300, 0x304], [0x306, 0x308], [0x30A, 0x30C], [0x391, 0x3A1], [0x3A3, 0x3A9], [0x3B1, 0x3C9], 0x3D1, 0x3D2, 0x3D5, 0x3D6, 0x3F0, 0x3F1, 0x3F4, 0x3F5, [0x2190, 0x2199], 0x220F, 0x2211, 0x2212, 0x2215, 0x2219, 0x221A, 0x221E, 0x2229, 0x222B, 0x2248, 0x2260, 0x2261, 0x2264, 0x2265, 0x25CA
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
    ['symbols', {
            'normal': [
                0xA1, 0xA2, 0xA4, 0xA6, [0xA9, 0xAB], 0xAD, 0xAE, 0xB2, 0xB3, [0xB9, 0xBF], 0xE3F, 0x200C, 0x200D, 0x201A, 0x201E, 0x2022, 0x2030, 0x2039, 0x203A, 0x203F, 0x204A, 0x2070, [0x2074, 0x208E], 0x20A1, 0x20A6, [0x20A9, 0x20AB], 0x20AF, 0x20B4, 0x20B9, 0x20BA, 0x20BD, 0x2116, 0x2122, 0x212E, [0x2153, 0x215F], 0x2302, [0x2326, 0x2328], 0x232B, [0x23FB, 0x23FE], 0x2BFE, [0xE100, 0xE103], [0xFB00, 0xFB04]
            ]
        }],
    ['symbols-other', {
            'bold': [
                [0xA1, 0xA7], [0xA9, 0xAE], [0xB1, 0xB3], [0xB5, 0xB7], [0xB9, 0xBF], 0xD7, 0xF0, 0xF7, 0xE3F, 0x200C, 0x200D, 0x201A, 0x201E, [0x2020, 0x2022], 0x2030, 0x2039, 0x203A, 0x203F, 0x204A, 0x2070, [0x2074, 0x208E], 0x20A1, 0x20A6, [0x20A9, 0x20AB], 0x20AF, 0x20B4, 0x20B9, 0x20BA, 0x20BD, 0x2116, 0x2122, 0x212E, [0x2153, 0x215F], [0xE100, 0xE103], [0xFB00, 0xFB04]
            ],
            'italic': [
                [0xA1, 0xA7], [0xA9, 0xAE], [0xB1, 0xB3], [0xB5, 0xB7], [0xB9, 0xBF], 0xD7, 0xF0, 0xF7, 0xE3F, 0x200C, 0x200D, 0x201A, 0x201E, [0x2020, 0x2022], 0x2030, 0x2039, 0x203A, 0x203F, 0x204A, 0x2070, [0x2074, 0x208E], 0x20A1, 0x20A6, [0x20A9, 0x20AB], 0x20AF, 0x20B4, 0x20B9, 0x20BA, 0x20BD, 0x2116, 0x2122, 0x212E, [0x2153, 0x215F], [0xE100, 0xE103], [0xFB00, 0xFB04]
            ],
            'bold-italic': [
                [0xA1, 0xA7], [0xA9, 0xAE], [0xB1, 0xB3], [0xB5, 0xB7], [0xB9, 0xBF], 0xD7, 0xF0, 0xF7, 0xE3F, 0x200C, 0x200D, 0x201A, 0x201E, [0x2020, 0x2022], 0x2030, 0x2039, 0x203A, 0x203F, 0x204A, 0x2070, [0x2074, 0x208E], 0x20A1, 0x20A6, [0x20A9, 0x20AB], 0x20AF, 0x20B4, 0x20B9, 0x20BA, 0x20BD, 0x2116, 0x2122, 0x212E, [0x2153, 0x215F], [0xE100, 0xE103], [0xFB00, 0xFB04]
            ],
            'monospace': [
                [0xA1, 0xA7], [0xA9, 0xAE], [0xB1, 0xB3], [0xB5, 0xB7], [0xB9, 0xBF], 0xD7, 0xF0, 0xF7, 0x2017, 0x201A, 0x201E, [0x2020, 0x2022], 0x2030, 0x2039, 0x203A, 0x204A, 0x2070, [0x2074, 0x208E], 0x20AF, 0x20B9, 0x20BA, 0x20BD, 0x2116, 0x2122, 0x212E, [0x2153, 0x215F], 0xFB01, 0xFB02
            ]
        }],
    ['arrows', {
            'normal': [
                0x219C, 0x219D, 0x219F, 0x21A1, 0x21A5, 0x21A7, 0x21A8, 0x21AD, [0x21AF, 0x21B5], 0x21B8, 0x21B9, [0x21D6, 0x21D9], [0x21DC, 0x21DF], [0x21E6, 0x21F4], [0x21F7, 0x21FF], 0x23CE, 0x27A1, [0x2900, 0x2909], [0x2912, 0x291C], 0x294C, 0x294D, 0x294F, 0x2951, 0x295C, 0x295D, 0x2960, 0x2961, [0x2B05, 0x2B07], 0x2B31, [0x2B34, 0x2B36], [0x2B39, 0x2B3D]
            ]
        }, [0x21A5, 0x21A7, 0x2906, 0x2907, 0x294C, 0x294D, 0x294F, 0x2951, 0x295C, 0x295D, 0x2960, 0x2961]],
    ['up-int', {
            '-up': [
                [0x222B, 0x2230], 0x2A0C
            ],
            '-dup': [
                [0x222B, 0x2230], 0x2A0C
            ]
        }],
    ['accents', {
            'normal': [
                0xB8, [0x2B0, 0x2B2], 0x2B4, [0x2B7, 0x2BC], 0x2BF, 0x2C0, 0x2C8, 0x2CC, [0x2CE, 0x2D1], 0x2D4, 0x2D6, 0x2D7, 0x2DB, 0x2DD, 0x2DE, [0x2E0, 0x2E9], 0x2EE, 0x309, 0x30B, 0x30D, 0x30F, [0x311, 0x313], [0x316, 0x321], [0x323, 0x32D], [0x32F, 0x332], [0x334, 0x337], [0x339, 0x33D], 0x342, 0x345, 0x35C, 0x361, [0x1DC4, 0x1DC9], 0x20D3, [0x20E7, 0x20E9], 0x20F0
            ]
        }, [0x332]],
    ['accents-other', {
            'bold': [
                0xB8, [0x2B0, 0x2B2], 0x2B4, [0x2B7, 0x2BC], 0x2BF, 0x2C0, 0x2C8, 0x2CC, [0x2CE, 0x2D1], 0x2D4, 0x2D6, 0x2D7, 0x2DB, 0x2DD, 0x2DE, [0x2E0, 0x2E9], 0x2EE, 0x309, 0x30B, 0x30D, 0x30F, [0x311, 0x313], [0x316, 0x321], [0x323, 0x32D], [0x32F, 0x332], [0x334, 0x336], [0x339, 0x33D], 0x342, 0x345, 0x35C, 0x361, [0x1DC4, 0x1DC9]
            ],
            'italic': [
                0xB8, [0x2B0, 0x2B2], 0x2B4, [0x2B7, 0x2BC], 0x2BF, 0x2C0, 0x2C8, 0x2CC, [0x2CE, 0x2D1], 0x2D4, 0x2D6, 0x2D7, 0x2DB, 0x2DD, 0x2DE, [0x2E0, 0x2E9], 0x2EE, 0x309, 0x30B, 0x30D, 0x30F, [0x311, 0x313], [0x316, 0x321], [0x323, 0x32D], [0x32F, 0x332], [0x334, 0x336], [0x339, 0x33D], 0x342, 0x345, 0x35C, 0x361, [0x1DC4, 0x1DC9]
            ],
            'bold-italic': [
                0xB8, [0x2B0, 0x2B2], 0x2B4, [0x2B7, 0x2BC], 0x2BF, 0x2C0, 0x2C8, 0x2CC, [0x2CE, 0x2D1], 0x2D4, 0x2D6, 0x2D7, 0x2DB, 0x2DD, 0x2DE, [0x2E0, 0x2E9], 0x2EE, 0x309, 0x30B, 0x30D, 0x30F, [0x311, 0x313], [0x316, 0x321], [0x323, 0x32D], [0x32F, 0x332], [0x334, 0x336], [0x339, 0x33D], 0x342, 0x345, 0x35C, 0x361, [0x1DC4, 0x1DC9]
            ],
            'monospace': [
                0xB8, 0x2B9, 0x2BA, 0x2BC, 0x2DB, 0x2DD, 0x30B, 0x30F, 0x313, 0x314, 0x326, 0x327, 0x335, 0x336, 0x342, 0x345
            ]
        }],
    ['shapes', {
            'normal': [
                0x2302, [0x2326, 0x2328], 0x232B, [0x23FB, 0x23FE], [0x2580, 0x2590], [0x2594, 0x259F], 0x25A2, 0x25A3, [0x25AC, 0x25AF], 0x25C9, 0x25CE, [0x25D0, 0x25D3], [0x25D5, 0x25D7], [0x25D9, 0x25E5], [0x25E7, 0x25EB], [0x25F0, 0x25F7], 0x2620, [0x2639, 0x263C], 0x2640, 0x2642, 0x2660, 0x2663, 0x2665, 0x2666, 0x266A, 0x266B, [0x26AA, 0x26AC], 0x27A1, 0x2B24, 0x2B58, 0x1F784
            ],
            'bold': [
                0x25AF
            ],
            'italic': [],
            'bold-italic': []
        }],
    ['math-other', {
            'bold': [
                [0x2190, 0x2199], 0x2205, 0x2206, 0x220F, 0x2211, 0x2212, 0x2215, 0x2219, 0x221A, 0x221E, 0x222B, 0x2248, 0x2260, 0x2264, 0x2265, 0x22C5, 0x25B2, 0x25B4, 0x25B6, 0x25B8, 0x25BA, 0x25BC, 0x25BE, 0x25C0, 0x25C2, 0x25C4, 0x25CA, 0x25CF, 0x2981
            ],
            'italic': [
                [0x2190, 0x2199], 0x2205, 0x2206, 0x220F, 0x2211, 0x2212, 0x2215, 0x2219, 0x221A, 0x221E, 0x222B, 0x2248, 0x2260, 0x2264, 0x2265, 0x22C5, 0x25CA
            ],
            'bold-italic': [
                [0x2190, 0x2199], 0x2205, 0x2206, 0x220F, 0x2211, 0x2212, 0x2215, 0x2219, 0x221A, 0x221E, 0x222B, 0x2248, 0x2260, 0x2264, 0x2265, 0x22C5, 0x25CA
            ]
        }],
    ['stretchy', {
            'normal': [
                0x221B, 0x221C
            ],
            '-smallop': [
                0x221B, 0x221C
            ],
            '-largeop': [
                0x221B, 0x221C
            ],
            '-size3': [
                0x221B, 0x221C
            ],
            '-size4': [
                0x221B, 0x221C
            ],
            '-size5': [
                0x221B, 0x221C
            ],
            '-size6': [
                0x221B, 0x221C
            ],
            '-size7': [
                0x221B, 0x221C
            ],
            '-size8': [
                0x221B, 0x221C
            ],
            '-size9': [
                0x221B, 0x221C
            ],
            '-size10': [
                0x221B, 0x221C
            ],
            '-size11': [
                0x221B, 0x221C
            ],
            '-size12': [
                0x221B, 0x221C
            ],
            '-size13': [
                0x221B, 0x221C
            ],
            '-size14': [
                0x221B, 0x221C
            ],
            '-size15': [
                0x221B, 0x221C
            ],
            '-lf-tp': [
                0x221B, 0x221C
            ],
            '-rt-bt': [
                0x221B, 0x221C
            ],
            '-ext': [
                0x221B, 0x221C
            ]
        }, [0x221B, 0x221C]],
    ['variants', {
            '-tex-variant': [
                0x22, 0x27, 0x2A, [0x30, 0x39], 0x60, 0xAA, 0xB0, 0xB2, 0xB3, 0xB9, 0xBA, [0x200C, 0x200F], 0x2070, 0x2071, [0x2074, 0x208E]
            ]
        }]
]);
//# sourceMappingURL=chtml.js.map