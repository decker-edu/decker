import { AddFontIds } from '@mathjax/src/mjs/output/chtml/DynamicFonts.js';
import { MathJaxFiraFont } from '../../chtml.js';
MathJaxFiraFont.dynamicSetup('', 'up-int', AddFontIds({
    'UP': {
        '-up': {
            0x222B: [.852, .22, .491],
            0x222C: [.852, .22, .791],
            0x222D: [.852, .22, 1.091],
            0x222E: [.852, .22, .491],
            0x222F: [.852, .22, .791],
            0x2230: [.852, .22, 1.146],
            0x2A0C: [.852, .22, 1.391]
        }
    },
    'DUP': {
        '-dup': {
            0x222B: [1.424, .65, .882],
            0x222C: [1.814, .594, 1.382],
            0x222D: [1.814, .594, 1.882],
            0x222E: [1.814, .594, .882],
            0x222F: [1.814, .594, 1.402],
            0x2230: [1.814, .594, 1.942],
            0x2A0C: [1.814, .594, 2.382]
        }
    }
}, 'FIRA'), {}, ['MJX-FIRA-UP', 'MJX-FIRA-DUP']);
//# sourceMappingURL=up-int.js.map