import { FontData } from '@mathjax/src/mjs/output/common/FontData.js';
export function CommonMathJaxFiraFontMixin(Base) {
    var _a;
    return _a = class extends Base {
        },
        _a.defaultVariants = [
            ...FontData.defaultVariants,
            ['-size3', 'normal'],
            ['-size4', 'normal'],
            ['-size5', 'normal'],
            ['-size6', 'normal'],
            ['-size7', 'normal'],
            ['-size8', 'normal'],
            ['-size9', 'normal'],
            ['-size10', 'normal'],
            ['-size11', 'normal'],
            ['-size12', 'normal'],
            ['-size13', 'normal'],
            ['-size14', 'normal'],
            ['-size15', 'normal'],
            ['-tex-calligraphic', 'script'],
            ['-tex-bold-calligraphic', 'bold-script'],
            ['-lf-tp', 'normal'],
            ['-rt-bt', 'normal'],
            ['-ext', 'normal'],
            ['-mid', 'normal'],
            ['-up', 'normal'],
            ['-dup', 'normal']
        ],
        _a.defaultCssFonts = Object.assign(Object.assign({}, FontData.defaultCssFonts), { '-size3': ['serif', false, false], '-size4': ['serif', false, false], '-size5': ['serif', false, false], '-size6': ['serif', false, false], '-size7': ['serif', false, false], '-size8': ['serif', false, false], '-size9': ['serif', false, false], '-size10': ['serif', false, false], '-size11': ['serif', false, false], '-size12': ['serif', false, false], '-size13': ['serif', false, false], '-size14': ['serif', false, false], '-size15': ['serif', false, false], '-lf-tp': ['serif', false, false], '-rt-bt': ['serif', false, false], '-ext': ['serif', false, false], '-mid': ['serif', false, false], '-up': ['serif', false, false], '-dup': ['serif', false, false] }),
        _a.defaultAccentMap = {
            0x005E: '\u02C6',
            0x007E: '\u02DC',
            0x0300: '\u02CB',
            0x0301: '\u02CA',
            0x0302: '\u02C6',
            0x0303: '\u02DC',
            0x0304: '\u02C9',
            0x0306: '\u02D8',
            0x0307: '\u02D9',
            0x0308: '\u00A8',
            0x030A: '\u02DA',
            0x030C: '\u02C7',
            0x2192: '\u20D7'
        },
        _a.defaultParams = Object.assign(Object.assign({}, FontData.defaultParams), { surd_height: 0.075, rule_thickness: 0.075, x_height: .527 }),
        _a.defaultSizeVariants = [
            'normal', '-smallop', '-largeop', '-size3', '-size4', '-size5', '-size6', '-size7', '-size8', '-size9', '-size10', '-size11', '-size12', '-size13', '-size14', '-size15'
        ],
        _a.defaultStretchVariants = [
            'normal', '-ext', '-lf-tp', '-rt-bt', '-mid'
        ],
        _a;
}
//# sourceMappingURL=common.js.map