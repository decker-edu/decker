import { SvgFontData } from '@mathjax/src/mjs/output/svg/FontData.js';
import { CommonMathJaxTexFontMixin } from './common.js';
import { normal } from './svg/normal.js';
import { bold } from './svg/bold.js';
import { italic } from './svg/italic.js';
import { boldItalic } from './svg/bold-italic.js';
import { fraktur } from './svg/fraktur.js';
import { frakturBold } from './svg/fraktur-bold.js';
import { sansSerif } from './svg/sans-serif.js';
import { sansSerifBold } from './svg/sans-serif-bold.js';
import { sansSerifItalic } from './svg/sans-serif-italic.js';
import { monospace } from './svg/monospace.js';
import { smallop } from './svg/smallop.js';
import { largeop } from './svg/largeop.js';
import { texCalligraphic } from './svg/tex-calligraphic.js';
import { texCalligraphicBold } from './svg/tex-calligraphic-bold.js';
import { texOldstyle } from './svg/tex-oldstyle.js';
import { texOldstyleBold } from './svg/tex-oldstyle-bold.js';
import { texMathit } from './svg/tex-mathit.js';
import { texVariant } from './svg/tex-variant.js';
import { size3 } from './svg/size3.js';
import { size4 } from './svg/size4.js';
import { delimiters } from './svg/delimiters.js';
const Base = CommonMathJaxTexFontMixin(SvgFontData);
export class MathJaxTexFont extends Base {
    constructor(options = {}) {
        super(options);
        const CLASS = this.constructor;
        for (const variant of Object.keys(this.variant)) {
            this.variant[variant].cacheID = 'TEX-' + (CLASS.variantCacheIds[variant] || 'N');
        }
    }
}
MathJaxTexFont.NAME = 'MathJaxTex';
MathJaxTexFont.OPTIONS = Object.assign(Object.assign({}, Base.OPTIONS), { dynamicPrefix: '@mathjax/mathjax-tex-font/svg/dynamic' });
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
//# sourceMappingURL=svg.js.map