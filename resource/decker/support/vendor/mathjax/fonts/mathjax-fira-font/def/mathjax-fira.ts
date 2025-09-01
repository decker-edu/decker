import {CHARSET, CHARS} from '@mathjax/font-tools/js/CharMap.js';
import {Font, FontData, GlyphNames} from '@mathjax/font-tools/js/Font.js';
import {Variants} from '@mathjax/font-tools/js/Variant.js';
import {Delimiters} from '@mathjax/font-tools/js/Delimiters.js';
import {CommonFont, FontDef} from '@mathjax/font-tools/js/CommonFont.js';
import {RANGES, Ranges} from '@mathjax/font-tools/js/Ranges.js';
import {SVGFont} from '@mathjax/font-tools/js/SVGFont.js';
import {CHTMLFont} from '@mathjax/font-tools/js/CHTMLFont.js';
import {Components} from '@mathjax/font-tools/js/Components.js';

/***********************************************************************************/

try {

  //
  // Name-to-Unicode mapping needed for text fonts
  //
  const charNames: GlyphNames = [
    ['f_f', 0xFB00], ['f_f_i', 0xFB03], ['f_f_l', 0xFB04]
  ];

  const charOptions: FontData = {
    charNames,
    ignore: /^\.notdef$|\.(?:sc|locl|case|tf|tosf|dnom|numr)|_short$|^f_[il]$/,
    autoPUA: 0xE100
  };

  Font.load({
    'FIRA-M':  ['fonts/FiraMath-Regular.otf', {}],
    'FIRA-R':  ['fonts/FiraSans-Regular.otf', charOptions],
    'FIRA-B':  ['fonts/FiraSans-Bold.otf', charOptions],
    'FIRA-I':  ['fonts/FiraSans-Italic.otf', charOptions],
    'FIRA-BI': ['fonts/FiraSans-BoldItalic.otf', charOptions],
    'FIRA-T':  ['fonts/FiraMono-Regular.otf', charOptions],
    'CAL-R':   ['../subsets/CM-Calligraphic-Regular.otf', {}],
    'CAL-B':   ['../subsets/CM-Calligraphic-Bold.otf', {}],
    'SCR-R':   ['../subsets/Asana-Script-Regular.otf', {}],
    'SCR-B':   ['../subsets/Asana-Script-Bold.otf', {}],
    'FRK-R':   ['../subsets/Euler-Fraktur-Regular.otf', {}],
    'FRK-B':   ['../subsets/Euler-Fraktur-Bold.otf', {}],
    'MM-D':    ['../subsets/Modern-Delimiters-Regular.otf', {}],
  });

  Font.get('FIRA-M')
    .addGlyph(Font.buildV('FIRA-M', [0x23AD, 0x23A7], 0x23B0, 'lmoustache', -500))
    .addGlyph(Font.buildV('FIRA-M', [0x23A9, 0x23AB], 0x23B1, 'rmoustache', -500))

    .addGlyph(Font.buildH('FIRA-M', [0x2190], 0x2190, 'uni2190.left', -120, 0, -120))
    .addGlyph(Font.buildV('FIRA-R', [0x23FD], 0x2191, 'uni2191.ext', -10, 185, 185))
    .addGlyph(Font.buildH('FIRA-M', [0x2192], 0x2192, 'uni2192.right', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x219E], 0x219E, 'uni219E.left', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x21A0], 0x21A0, 'uni21A0.right', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x21BC], 0x21BC, 'uni21BC.left', -110, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x21BD], 0x21BD, 'uni21BD.left', -110, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x21C0], 0x21C0, 'uni21C0.right', -120, 0, -110))
    .addGlyph(Font.buildH('FIRA-M', [0x21C1], 0x21C1, 'uni21C1.right', -120, 0, -110))
    .addGlyph(Font.buildV('FIRA-R', [0x23FD], 0x21A4, 'uni21A4.left', -10, -100, -100))
    .addGlyph(Font.buildH('FIRA-M', [0x21D0], 0x21D0, 'uni21D0.left', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x003D], 0x21D0, 'uni21D0.ext', -62, 0, -62))
    .addGlyph(Font.buildH('FIRA-M', [0x21D2], 0x21D2, 'uni21D2.right', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x21DA], 0x21DA, 'uni21DA.left', -120, 0, -120))
    .addGlyph(Font.buildH('FIRA-M', [0x2261], 0x2261, 'uni2261.ext', -62, 0, -62))
    .addGlyph(Font.buildH('FIRA-M', [0x21DB], 0x21DB, 'uni21DB.right', -120, 0, -120))

    .addGlyph(Font.buildV('FIRA-M', [0x21BE], 0x294C, 'uni294C.top', 0, 172, 2))
    .addGlyph(Font.buildV('FIRA-M', [0x21BF], 0x294D, 'uni294D.top', 0, 2, 172))
    .addGlyph(Font.buildV('FIRA-M', [0x21C3], 0x294C, 'uni294C.bot', 0, 2, 172))
    .addGlyph(Font.buildV('FIRA-M', [0x21C2], 0x294D, 'uni294D.bot', 0, 172, 2))
  ;

  /***********************************************************************************/

  const FiraVariants = Variants.define({
    normal: [
      ['FIRA-M', [
        CHARS.InRange(0x20, 0x2FFF, 'FIRA-M').minus(CHARS.Range(0x200C, 0x200F), CHARS.At(0x210A)),
        CHARS.Range(0xFB00, 0xFB04),
        CHARS.InRange(0x1D400, 0x1D7FF, 'FIRA-M'),
        CHARS.At(0x1F784),
        CHARS.Map({0x2C9: 0xAF}),
        CHARS.MapFrom(0x41, CHARSET.MathSansSerifUC),
        CHARS.MapFrom(0x61, CHARSET.MathSansSerifLC),
        CHARS.MapFrom(0x1D400, CHARSET.MathBoldSansSerifUC),
        CHARS.MapFrom(0x1D41A, CHARSET.MathBoldSansSerifLC),
        CHARS.MapFrom(0x1D434, CHARSET.MathSansSerifItalicUC),
        CHARS.MapFrom(0x1D44E, CHARS.Range(0x1D622, 0x1D628)).plus(
          CHARS.Map({0x1D629: 0x210E}),
          CHARS.MapFrom(0x1D456, CHARS.Range(0x1D62A, 0x1D63B))
        ),
        CHARS.MapFrom(0x1D468, CHARSET.MathSansSerifBoldItalicUC),
        CHARS.MapFrom(0x1D482, CHARSET.MathSansSerifBoldItalicLC),
        CHARS.MapFrom(0x0391, CHARS.Range(0x1D756, 0x1D76E)).minus(CHARS.At(0x1D767)).plus(
          CHARS.Map({0x1D767: 0x03F4, 0x1D76F: 0x2207, 0x1D789: 0x2202, 0x1D78A: 0x03F5,
                     0x1D78B: 0x03D1, 0x1D78C: 0x03F0, 0x1D78D: 0x03D5, 0x1D78E: 0x03F1, 0x1D78F: 0x03D6}),
          CHARS.MapFrom(0x3B1, CHARS.Range(0x1D770, 0x1D788))
        ),
        CHARS.MapFrom(0x1D6E2, CHARSET.MathGreekSansSerifItalic),
        CHARS.MapFrom(0x30, CHARSET.MathNumbersSansSerif),
        CHARS.MapFrom(0x1D7CE, CHARSET.MathNumbersSansSerifBold)
      ]], ['FIRA-R', [
        CHARS.InRange(0xA780, 0xAB6F, 'FIRA-R'),
        CHARS.Range(0xE100, 0xE103),
        CHARS.InRange(0x20, 0x2FFF, 'FIRA-R').minus(CHARS.InRange(0x20, 0x2FFF, 'FIRA-M'))
      ]], ['FIRA-T', [
        CHARS.MapFrom(0x30, CHARSET.MathNumbersMonospace)
      ]], ['SCR-R', [
        CHARSET.MathScript
      ]], ['SCR-B', [
        CHARSET.MathBoldScript
      ]], ['FRK-R', [
        CHARSET.MathFraktur,
      ]], ['FRK-B', [
        CHARSET.MathBoldFraktur,
      ]],
    ],
    bold: ['FIRA-B', [
      CHARS.InRange(0x20, 0x25FF, 'FIRA-B').minus(
        CHARSET.Alpha, CHARSET.Numbers, CHARSET.Greek,
        CHARS.Range(0x21E6, 0x21EA),
        CHARS.At(0x210A, 0x2202, 0x2326, 0x2327, 0x2328, 0x232B, 0x23CE),
        CHARS.Range(0x23FB, 0x23FF)
      ),
      CHARS.At(0x2981),
      CHARS.InRange(0x2C60, 0x2C7F, 'FIRA-B'),
      CHARS.Range(0xE100, 0xE103),
      CHARS.Range(0xFB00, 0xFB04),
    ]],
    italic: ['FIRA-I', [
      CHARS.InRange(0x20, 0x22EF, 'FIRA-I').minus(
        CHARSET.Alpha, CHARSET.Greek,
        CHARS.At(0x210A, 0x2202),
        CHARS.Range(0x21E6, 0x21EA)
      ),
      CHARS.At(0x25CA),
      CHARS.InRange(0x2C60, 0x2C7F, 'FIRA-I'),
      CHARS.Range(0xE100, 0xE103),
      CHARS.Range(0xFB00, 0xFB04),
    ]],
    'bold-italic': ['FIRA-BI', [
      CHARS.InRange(0x20, 0x22EF, 'FIRA-BI').minus(
        CHARSET.Alpha, CHARSET.Greek,
        CHARS.At(0x210A, 0x2202),
        CHARS.Range(0x21E6, 0x21EA)
      ),
      CHARS.At(0x25CA),
      CHARS.InRange(0x2C60, 0x2C7F, 'FIRA-BI'),
      CHARS.Range(0xE100, 0xE103),
      CHARS.Range(0xFB00, 0xFB04),
    ]],
    'sans-serif-italic': ['FIRA-I', [
      CHARSET.Numbers
    ]],
    'sans-serif-bold-italic': ['FIRA-BI', [
      CHARSET.Numbers
    ]],
    monospace: ['FIRA-T', [
      CHARS.InRange(0x20, 0x22EF, 'FIRA-T').minus(
        CHARSET.Alpha, CHARSET.Numbers,
        CHARS.At(0x210A, 0x2202),
        CHARS.Range(0x21E6, 0x21EA)
      ),
      CHARS.At(0x25CA, 0xFB01, 0xFB02),
      CHARS.InRange(0x2C60, 0x2C7F, 'FIRA-T')
    ]],
    '-smallop': [
      ['FIRA-M', [
        CHARS.ForFeature('size1', 'FIRA-M'),
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE001, 0x5C: 0xE011, 0x302: 0xE021, 0x303: 0xE031})
      ]]
    ],
    '-largeop': [
      ['FIRA-M', [
        CHARS.ForFeature('size2', 'FIRA-M'),
        CHARS.ForFeature('display', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE002, 0x5C: 0xE012, 0x302: 0xE022, 0x303: 0xE032})
      ]]
    ],
    '-size3': [
      ['FIRA-M', [
        CHARS.ForFeature('size3', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE003, 0x5C: 0xE013, 0x302: 0xE023, 0x303: 0xE033})
      ]]
    ],
    '-size4': [
      ['FIRA-M', [
        CHARS.ForFeature('size4', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE004, 0x5C: 0xE014, 0x302: 0xE024, 0x303: 0xE034})
      ]]
    ],
    '-size5': [
      ['FIRA-M', [
        CHARS.ForFeature('size5', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE005, 0x5C: 0xE015, 0x302: 0xE025, 0x303: 0xE035})
      ]]
    ],
    '-size6': [
      ['FIRA-M', [
        CHARS.ForFeature('size6', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE006, 0x5C: 0xE016, 0x302: 0xE026, 0x303: 0xE036})
      ]]
    ],
    '-size7': [
      ['FIRA-M', [
        CHARS.ForFeature('size7', 'FIRA-M')
      ]],
      ['MM-D', [
        CHARS.Map({0x2F: 0xE007, 0x5C: 0xE017, 0x302: 0xE027, 0x303: 0xE037})
      ]]
    ],
    '-size8': ['FIRA-M', [
      CHARS.ForFeature('size8', 'FIRA-M')
    ]],
    '-size9': ['FIRA-M', [
      CHARS.ForFeature('size9', 'FIRA-M')
    ]],
    '-size10': ['FIRA-M', [
      CHARS.ForFeature('size10', 'FIRA-M')
    ]],
    '-size11': ['FIRA-M', [
      CHARS.ForFeature('size11', 'FIRA-M')
    ]],
    '-size12': ['FIRA-M', [
      CHARS.ForFeature('size12', 'FIRA-M')
    ]],
    '-size13': ['FIRA-M', [
      CHARS.ForFeature('size13', 'FIRA-M')
    ]],
    '-size14': ['FIRA-M', [
      CHARS.ForFeature('size14', 'FIRA-M')
    ]],
    '-size15': ['FIRA-M', [
      CHARS.ForFeature('size15', 'FIRA-M')
    ]],
    '-tex-variant': ['FIRA-M', [
      CHARS.Range(0x200C, 0x200F),
      CHARS.ForFeature('ss02', 'FIRA-M'),
      CHARS.ForFeature('ss03', 'FIRA-M'),
      CHARS.ForFeature('ssty1', 'FIRA-M'),
      CHARS.ForFeature('pnum', 'FIRA-M'),
      CHARSET.PseudoScriptsMain,
      CHARSET.PseudoScriptQuotes.feature('ssty1'),
    ]],
    '-tex-mathit': ['FIRA-I', [
      CHARSET.Alpha
    ]],
    '-tex-oldstyle': ['FIRA-R', [
      CHARSET.Numbers.feature('osf')
    ]],
    '-tex-bold-oldstyle': ['FIRA-B', [
      CHARSET.Numbers.feature('osf')
    ]],
    '-tex-calligraphic': ['CAL-R', [
      CHARSET.AlphaUC,
    ]],
    '-tex-bold-calligraphic': ['CAL-B', [
      CHARSET.AlphaUC,
    ]],
    '-lf-tp': ['FIRA-M', [
      CHARS.ForFeature('left', 'FIRA-M'),
      CHARS.ForFeature('top', 'FIRA-M')
    ]],
    '-rt-bt': ['FIRA-M', [
      CHARS.ForFeature('right', 'FIRA-M'),
      CHARS.ForFeature('bot', 'FIRA-M')
    ]],
    '-ext': [
      ['FIRA-M', [
        CHARS.ForFeature('ext', 'FIRA-M'),
        CHARS.Map({0x2190: 0x2212, 0x21A4: 0x2212})
      ]]
    ],
    '-mid': ['FIRA-M', [
      CHARS.ForFeature('mid', 'FIRA-M')
    ]],
    '-up': ['FIRA-M', [
      CHARS.ForFeature('up', 'FIRA-M')
    ]],
    '-dup': ['FIRA-M', [
      CHARS.ForFeature('display.up', 'FIRA-M')
    ]]
  }, {
    spaces: {
      normal: {
        0x2000: .5,
        0x2001: 1,
        0x2002: .5,
        0x2003: 1,
        0x2004: .3333,
        0x2005: .25,
        0x2006: .1667,
        0x2009: .2,
        0x200A: .1,
        0x200C: 0,
        0x200D: 0,
        0x202F: .2,
        0x205F: .222,
        0x2060: 0,
        0x2061: 0,
        0x2062: 0,
        0x2063: 0,
        0x2064: 0,
      }
    },
    transferHD: [
      [0x2212, 0x002B]    // make minus the same height/depth as plus
    ],
    fixIC: [
      ['-largeop', .3, CHARS.At(0x222B, 0x222C, 0x222D, 0x2A0C)],  // adjust integral italic correction
      ['-largeop', .15, CHARS.At(0x222E, 0x222F, 0x2230)],
      ['normal', .11, CHARS.At(0x222B, 0x222C, 0x222D)]
    ]
  });

  /***********************************************************************************/

  const FiraDelimiters = Delimiters.define({
    font: 'FIRA-M',
    variants: FiraVariants,
    sizeVariants: ['normal'],
    stretchVariants: ['normal'],
    readMathTable: true,
    add: {
      0x002F: {dir: 'V', sizes: 7},
      0x003D: {dir: 'H', sizes: 1, parts: [0, 0x003D]},
      0x005C: {dir: 'V', sizes: 7},
      0x005F: {dir: 'H', sizes: 1, parts: [0, 0x005F]},
      0x00AF: {dir: 'H', sizes: 1, parts: [0, 0x00AF]},
      0x0302: {dir: 'H', sizes: 7},
      0x0303: {dir: 'H', sizes: 7},
      0x0305: {dir: 'H', sizes: 1, parts: [0, 0x0305]},
      0x0332: {dir: 'H', sizes: 1, parts: [0, 0x0332]},
      0x2013: {dir: 'H', parts: [0, 0x2013]},
      0x2014: {dir: 'H', parts: [0, 0x2014]},
      0x2015: {dir: 'H', parts: [0, 0x2015]},
      0x2017: {dir: 'H', parts: [0, 0x2015]},
      0x2190: {dir: 'H', sizes: 1, parts: [[0x2190, '-lf-tp'], 0x2212]},
      0x2191: {dir: 'V', sizes: 1, parts: [0x2191, [0x2191, '-ext']]},
      0x2192: {dir: 'H', sizes: 1, parts: [0, 0x2212, [0x2192, '-rt-bt']]},
      0x2193: {dir: 'V', sizes: 1, parts: [0, [0x2191, '-ext'], 0x2193]},
      0x2194: {dir: 'H', sizes: 1, parts: [[0x2190, '-lf-tp'], 0x2212, [0x2192, '-rt-bt']]},
      0x2195: {dir: 'V', sizes: 1, parts: [0x2191, [0x2191, '-ext'], 0x2193]},
      0x219E: {dir: 'H', sizes: 1, parts: [[0x219E, '-lf-tp'], [0x2190, '-ext']]},
      0x21A0: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-ext'], [0x21A0, '-rt-bt']]},
      0x21A4: {dir: 'H', sizes: 1, parts: [[0x2190, '-lf-tp'], [0x21A4, '-ext'], [0x21A4, '-lf-tp']]},
      0x21A5: {dir: 'V', sizes: 1, parts: [0x2191, [0x2191, '-ext'], 0x005F]},
      0x21A6: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x21A4, '-ext'], [0x2192, '-rt-bt']]},
      0x21A7: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-ext'], 0x2193]},
      0x21BC: {dir: 'H', sizes: 1, parts: [[0x21BC, '-lf-tp'], [0x2190, '-ext']]},
      0x21BD: {dir: 'H', sizes: 1, parts: [[0x21BD, '-lf-tp'], [0x2190, '-ext']]},
      0x21BE: {dir: 'V', sizes: 1, parts: [[0x294C, '-lf-tp'], [0x2191, '-ext']]},
      0x21BF: {dir: 'V', sizes: 1, parts: [[0x294D, '-lf-tp'], [0x2191, '-ext']]},
      0x21C0: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-ext'], [0x21C0, '-rt-bt']]},
      0x21C1: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-ext'], [0x21C1, '-rt-bt']]},
      0x21C2: {dir: 'V', sizes: 1, parts: [0, [0x2191, '-ext'], [0x294D, '-rt-bt']]},
      0x21C3: {dir: 'V', sizes: 1, parts: [0, [0x2191, '-ext'], [0x294C, '-rt-bt']]},
      0x21D0: {dir: 'H', sizes: 1, parts: [[0x21D0, '-lf-tp'], [0x21D0, '-ext']]},
      0x21D1: {dir: 'V', sizes: 1, parts: [0x21D1, 0x2016]},
      0x21D2: {dir: 'H', sizes: 1, parts: [0, [0x21D0, '-ext'], [0x21D2, '-rt-bt']]},
      0x21D3: {dir: 'V', sizes: 1, parts: [0, 0x2016, 0x21D3]},
      0x21D4: {dir: 'H', sizes: 1, parts: [[0x21D0, '-lf-tp'], [0x21D0, '-ext'], [0x21D2, '-rt-bt']]},
      0x21D5: {dir: 'V', sizes: 1, parts: [0x21D1, 0x2016, 0x21D3]},
      0x21DA: {dir: 'H', sizes: 1, parts: [[0x21DA, '-lf-tp'], [0x2261, '-ext']]},
      0x21DB: {dir: 'H', sizes: 1, parts: [0, [0x2261, '-ext'], [0x21DB, '-rt-bt']]},
      0x2212: {dir: 'H', parts: [0, 0x2212]},
      0x2223: {dir: 'V', sizes: 1, parts: [0, 0x2223]},
      0x2225: {dir: 'V', sizes: 1, parts: [0, 0x2225]},
      0x2261: {dir: 'H', parts: [0, 0x2261]},
      0x2263: {dir: 'H', parts: [0, 0x2263]},
      0x23AA: {dir: 'V', sizes: 1, parts: [0, 0x23AA, 0]},
      0x23AF: {dir: 'H', parts: [0, 0x2013]},
      0x23B0: {dir: 'V', sizes: 1, parts: [0x23A7, 0x23AA, 0x23AD]},
      0x23B1: {dir: 'V', sizes: 1, parts: [0x23AB, 0x23AA, 0x23A9]},
      0x23D0: {dir: 'V', sizes: 1, parts: [0, 0x2223], schar: [0x2223]},
      0x23E0: {dir: 'H', sizes: 1, parts: [0x2CA, 0x2C9, 0x2CB]},
      0x23E1: {dir: 'H', sizes: 1, parts: [0x2CB, 0x2C9, 0x2CA]},
      0x2500: {dir: 'H', parts: [0, 0x2013]},
      0x2758: {dir: 'V', sizes: 1, parts: [0, 0x2223], schar: [0x2223]},
      0x27FB: {dir: 'H', sizes: 1, parts: [[0x2190, '-lf-tp'], [0x21A4, '-ext'], [0x21A4, '-lf-tp']]},
      0x27FC: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x21A4, '-ext'], [0x2192, '-rt-bt']]},
      0x27FD: {dir: 'H', sizes: 1, parts: [[0x21D0, '-lf-tp'], [0x21D0, '-ext'], [0x21A4, '-lf-tp']]},
      0x27FE: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x21D0, '-ext'], [0x21D2, '-rt-bt']]},
      0x2906: {dir: 'H', sizes: 1, parts: [[0x21D0, '-lf-tp'], [0x21D0, '-ext'], [0x21A4, '-lf-tp']]},
      0x2907: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x21D0, '-ext'], [0x21D2, '-rt-bt']]},
      0x294A: {dir: 'H', sizes: 1, parts: [[0x21BC, '-lf-tp'], [0x2190, '-ext'], [0x21C1, '-rt-bt']]},
      0x294B: {dir: 'H', sizes: 1, parts: [[0x21BD, '-lf-tp'], [0x2190, '-ext'], [0x21C0, '-rt-bt']]},
      0x294C: {dir: 'V', sizes: 1, parts: [[0x294C, '-lf-tp'], [0x2191, '-ext'], [0x294C, '-rt-bt']]},
      0x294D: {dir: 'V', sizes: 1, parts: [[0x294D, '-lf-tp'], [0x2191, '-ext'], [0x294D, '-rt-bt']]},
      0x294E: {dir: 'H', sizes: 1, parts: [[0x21BC, '-lf-tp'], [0x2190, '-ext'], [0x21C0, '-rt-bt']]},
      0x294F: {dir: 'V', sizes: 1, parts: [[0x294C, '-lf-tp'], [0x2191, '-ext'], [0x294D, '-rt-bt']]},
      0x2950: {dir: 'H', sizes: 1, parts: [[0x21BD, '-lf-tp'], [0x2190, '-ext'], [0x21C1, '-rt-bt']]},
      0x2951: {dir: 'V', sizes: 1, parts: [[0x294D, '-lf-tp'], [0x2191, '-ext'], [0x294C, '-rt-bt']]},
      0x295A: {dir: 'H', sizes: 1, parts: [[0x21BC, '-lf-tp'], [0x2190, '-ext'], [0x21A4, '-lf-tp']]},
      0x295B: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x2190, '-ext'], [0x21C0, '-rt-bt']]},
      0x295C: {dir: 'V', sizes: 1, parts: [[0x294C, '-lf-tp'], [0x2191, '-ext'], 0x005F]},
      0x295D: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-ext'], [0x294D, '-rt-bt']]},
      0x295E: {dir: 'H', sizes: 1, parts: [[0x21BD, '-lf-tp'], [0x2190, '-ext'], [0x21A4, '-lf-tp']]},
      0x295F: {dir: 'H', sizes: 1, parts: [[0x21A4, '-lf-tp'], [0x2190, '-ext'], [0x21C1, '-rt-bt']]},
      0x2960: {dir: 'V', sizes: 1, parts: [[0x294D, '-lf-tp'], [0x2191, '-ext'], 0x005F]},
      0x2961: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-ext'], [0x294C, '-rt-bt']]},
    },
    alias: {
      0x002D: 0x2212,
      0x005E: 0x0302,
      0x007E: 0x0303,
      0x02C6: 0x0302,
      0x02C9: 0x00AF,
      0x02DC: 0x0303,
      0x203E: 0x00AF,
      0x20D7: 0x2192,
      0x2215: 0x002F,
      0x2312: 0x23DC,
      0x2322: 0x23DC,
      0x2323: 0x23DD,
      0x2329: 0x27E8,
      0x232A: 0x27E9,
      0x27F5: 0x2190,
      0x27F6: 0x2192,
      0x27F7: 0x2194,
      0x27F8: 0x21D0,
      0x27F9: 0x21D2,
      0x27FA: 0x21D4,
      0x3008: 0x27E8,
      0x3009: 0x27E9,
      0xFE37: 0x23DE,
      0xFE38: 0x23DF
    }
  });

  /***********************************************************************************/
  /***********************************************************************************/

  /*
   * Ranges to use for dynamically loaded files
   */

  /**
   * The monospace characters other than the ones in the AlphaNumerics block.
   */
  const MONOSPACE: Ranges = [
    [0x20, 0x2F], [0x3A, 0x40], [0x5B, 0x60], [0x7B, 0x7E],
    0xA0, 0xA8, 0xB0, 0xB4, 0x131, 0x237,
    [0x2C6, 0x2DC],
    [0x300, 0x30C], [0x391, 0x3A9], [0x3B1, 0x3C9],
    0x3D1, 0x3D2, 0x3D5, 0x3D6, 0x3F0, 0x3F1, 0x3F4, 0x3F5,
    [0x2190, 0x2199], [0x2200, 0x2265], 0x23DE, 0x23DF, 0x25CA
  ];

  /**
   * Lesser-used symbols
   */
  const MORE_SYMBOLS: Ranges = [
      ...RANGES.MORE_SYMBOLS, [0xE100, 0xE103]
  ];

  /**
   * Lesser-used geometric shapes
   */
  const SHAPES: Ranges = [
      ...RANGES.SHAPES, 0x1F784
  ];

  /**
   * Math characters in bold and italic variants
   */
  const MATH: Ranges = [
    [0x2190, 0x2199],
    0x2205, 0x2206, [0x220F, 0x22EF],
    [0x25B0, 0x25C4], 0x25CA, 0x25CF, 0x2981
  ];

  /***********************************************************************************/

  const FiraData: FontDef = {
    name: 'MathJaxFira',
    prefix: 'FIRA',
    variants: FiraVariants,
    delimiters: FiraDelimiters,
    variantFallbacks: {
      '-tex-calligraphic': 'script',
      '-tex-bold-calligraphic': 'bold-script'
    },
    parameters: {
      surd_height: .075,
      rule_thickness: .075
    },
    ranges: [
      ['latin', {LR: {normal: RANGES.LATIN}}],
      ['latin-b', {LB: {bold: RANGES.LATIN}}],
      ['latin-i', {LI: {italic: RANGES.LATIN}}],
      ['latin-bi', {LIB: {'bold-italic': RANGES.LATIN}}],
      ['latin-m', {LM: {monospace: RANGES.LATIN}}],
      ['greek', {
        GR: {normal: RANGES.GREEK},
        GRB: {bold: RANGES.GREEK},
        GRI: {italic: RANGES.GREEK},
        GRBI: {'bold-italic': RANGES.GREEK},
        GRM: {monospace: RANGES.GREEK}
      }],
      ['cyrillic', {
        CY: {normal: RANGES.CYRILLIC},
        CYB: {bold: RANGES.CYRILLIC},
        CYI: {italic: RANGES.CYRILLIC},
        CYBI: {'bold-italic': RANGES.CYRILLIC},
        CYM: {monospace: RANGES.CYRILLIC}
      }],
      ['phonetics', {
        PH: {normal: RANGES.PHONETICS},
        PHB: {bold: RANGES.PHONETICS},
        PHI: {italic: RANGES.PHONETICS},
        PHBI: {'bold-italic': RANGES.PHONETICS}
      }],
      ['double-struck', {DS: {normal: RANGES.DOUBLESTRUCK}}, [0x2140]],
      ['fraktur', {F: {normal: RANGES.FRAKTUR_NORMAL}}],
      ['script', {S: {normal: RANGES.SCRIPT_NORMAL}}],
      ['sans-serif', {SS: {normal: RANGES.SANSSERIF}}],
      ['monospace', {
        M: {
          normal: RANGES.MONOSPACE,
          monospace: MONOSPACE
        }
      }],
      ['calligraphic', {
        C: {'-tex-calligraphic': RANGES.ALPHAUC},
        CB: {'-tex-bold-calligraphic': RANGES.ALPHAUC}
      }],
      ['symbols', {
        SY: {normal: [...RANGES.SYMBOLS, [0xE100, 0xE103]]}
      }],
      ['symbols-other', {
        SYB: {bold: MORE_SYMBOLS},
        SYI: {italic: MORE_SYMBOLS},
        SYBI: {'bold-italic': MORE_SYMBOLS},
        SYM: {monospace: MORE_SYMBOLS}
      }],
      ['arrows', {
        AR: {normal: RANGES.ARROWS}
      }, [
        0x21A5, 0x21A7, 0x2906, 0x2907, 0x294C, 0x294D, 0x294F,
        0x2951, 0x295C, 0x295D, 0x2960, 0x2961
      ]],
      ['up-int', {
        UP: {'-up': RANGES.INTEGRALS},
        DUP: {'-dup': RANGES.INTEGRALS}
      }],
      ['accents', {'': {normal: RANGES.ACCENTS}}, [0x332]],
      ['accents-other', {
        '': {
          bold: RANGES.ACCENTS,
          italic: RANGES.ACCENTS,
          'bold-italic': RANGES.ACCENTS,
          monospace: RANGES.ACCENTS,
        }
      }],
      ['shapes', {
        SH: {normal: SHAPES},
        SHB: {bold: SHAPES},
        SHI: {italic: SHAPES},
        SHBI: {'bold-italic': SHAPES}
      }],
      ['math-other', {
        MXB: {bold: MATH},
        MXI: {italic: MATH},
        MXBI: {'bold-italic': MATH}
      }],
      ['stretchy', {
        '': {
          normal: RANGES.PARTS,
          '-smallop': RANGES.PARTS,
          '-largeop': RANGES.PARTS,
          '-size3': RANGES.PARTS,
          '-size4': RANGES.PARTS,
          '-size5': RANGES.PARTS,
          '-size6': RANGES.PARTS,
          '-size7': RANGES.PARTS,
          '-size8': RANGES.PARTS,
          '-size9': RANGES.PARTS,
          '-size10': RANGES.PARTS,
          '-size11': RANGES.PARTS,
          '-size12': RANGES.PARTS,
          '-size13': RANGES.PARTS,
          '-size14': RANGES.PARTS,
          '-size15': RANGES.PARTS,
          '-lf-tp': RANGES.PARTS,
          '-rt-bt': RANGES.PARTS,
          '-ext': RANGES.PARTS
        }
      }, [
        0x221B, 0x221C
      ]],
      ['variants', {
        VX: {'-tex-variant': [[0x20, 0xFF], [0x200C, 0x200F], [0x2070, 0x209F]]}
      }]
    ],
    legal: {
      addCopyright: 'Copyright (c) 2022 MathJax, Inc. (www.mathjax.org)'
    }
  };

  /***********************************************************************************/

  CommonFont.define(FiraData).writeFont();

  Components.define('svg', FiraData).writeFont().writeComponent();
  SVGFont.define(FiraData).writeFont();

  Components.define('chtml', FiraData).writeFont().writeComponent();
  CHTMLFont.define(FiraData).writeFont().makeWoffFonts('FIRA-M');

} catch(err) {
  console.error(err);
  process.exit(1);
}
