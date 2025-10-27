import {CHARSET, CHARS, CharMap} from '@mathjax/font-tools/js/CharMap.js';
import {Font} from '@mathjax/font-tools/js/Font.js';
import {Variants} from '@mathjax/font-tools/js/Variant.js';
import {Delimiters} from '@mathjax/font-tools/js/Delimiters.js';
import {CommonFont, FontDef} from '@mathjax/font-tools/js/CommonFont.js';
import {RANGES} from '@mathjax/font-tools/js/Ranges.js';
import {SVGFont} from '@mathjax/font-tools/js/SVGFont.js';
import {CHTMLFont} from '@mathjax/font-tools/js/CHTMLFont.js';
import {Components} from '@mathjax/font-tools/js/Components.js';

/***********************************************************************************/
/***********************************************************************************/

try {

  //
  //  Load the Asana-Math font
  //
  Font.load({'ASNA-M': './fonts/Asana-Math.otf'});

  Font.get('ASNA-M')
    .addGlyph(Font.buildV('ASNA-M', [0x23AD, 0x23A7], 0x23B0, 'lmoustache', [-500, -25]))
    .addGlyph(Font.buildV('ASNA-M', [0x23A9, 0x23AB], 0x23B1, 'rmoustache', [-500, -25]))
    .addGlyph(Font.buildH('ASNA-M', [0x210E, 0x02C9], 0x210F, 'uni210F.var', [0, -480]))
    .addGlyph(Font.buildH('ASNA-M', [0x21BC], 0x21BC, 'uni21BC.ext', 0, 95))
    .addGlyph(Font.buildH('ASNA-M', [0x21BD], 0x21BD, 'uni21BD.ext', 0, -92))
    .addGlyph(Font.buildH('ASNA-M', [0x21C0], 0x21C0, 'uni21C0.ext', 0, 95))
    .addGlyph(Font.buildH('ASNA-M', [0x21C1], 0x21C1, 'uni21C1.ext', 0, -92))
    .addGlyph(Font.buildH('ASNA-M', [0x10FF6D], 0xE001, 'uniE001.ext', 0, 0, 72))
    .addGlyph(Font.buildH('ASNA-M', [0x20D3], 0xE002, 'uniE002.ext'))
  ;

  const integralIC = (variant: string, large: number, small: number) => {
    return [
      [variant, large, CHARS.Range(0x222B, 0x2233).plus(CHARS.Range(0x2A0C, 0x2A17), CHARS.At(0x2A1B))],
      [variant, small, CHARS.At(0x2A18, 0x2A19, 0x2A1A)]
    ] as [string, number, CharMap][];
  }

  /***********************************************************************************/
  /***********************************************************************************/

  const AsanaVariants = Variants.define({
    normal: ['ASNA-M', [
      CHARS.InRange(0x20, 0x4000, 'ASNA-M').minus(CHARS.Range(0x2032, 0x2037), CHARS.At(0x2057, 0x20D3)),
      CHARS.InRange(0x10140, 0x1D7FF, 'ASNA-M'),
      CHARS.Map({0x237: 0x10FEE9}).feature('dtls'),  // dotless j
      CHARS.Map({
        0x00AF: 0x02C9,
        0x2032: 0x2019,
        0x2033: '\u2019\u2019',
        0x2034: '\u2019\u2019\u2019',
        0x2035: '\u201B',
        0x2036: '\u201B\u201B',
        0x2037: '\u201B\u201B\u201B',
        0x2057: '\u2019\u2019\u2019\u2019'
      })
    ]],
    'bold': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEE7, 0x237: 0x10FEE8}).feature('dtls')
    ]],
    'italic': ['ASNA-M', [
      CHARS.Map({0x131: 0x1D6A4, 0x237: 0x1D6A5})
    ]],
    'bold-italic': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEE5, 0x237: 0x10FEE6}).feature('dtls')
    ]],
    'double-struck': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEDD, 0x237: 0x10FEDE}).feature('dtls')
    ]],
    'fraktur': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEDF, 0x237: 0x10FEE0}).feature('dtls')
    ]],
    'bold-fraktur': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEDB, 0x237: 0x10FEDC}).feature('dtls')
    ]],
    'script': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEE3, 0x237: 0x10FEE4}).feature('dtls')
    ]],
    'bold-script': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FEE1, 0x237: 0x10FEE2}).feature('dtls')
    ]],
    'sans-serif': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FED9, 0x237: 0x10FEDA}).feature('dtls')
    ]],
    'bold-sans-serif': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FED7, 0x237: 0x10FED8}).feature('dtls')
    ]],
    'sans-serif-italic': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FED5, 0x237: 0x10FED6}).feature('dtls')
    ]],
    'sans-serif-bold-italic': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FED3, 0x237: 0x10FED4}).feature('dtls')
    ]],
    'monospace': ['ASNA-M', [
      CHARS.Map({0x131: 0x10FED1, 0x237: 0x10FED2}).feature('dtls')
    ]],
    '-smallop': ['ASNA-M', [
      CHARS.Map({
        0x0028: 0x10FFF4, 0x0029: 0x10FFF7, 0x002F: 0x10FFD2, 0x005B: 0x10FFEE, 0x005D: 0x10FFF1,
        0x007B: 0x10FFFA, 0x007C: 0x10FFD6, 0x007D: 0x10FFFD, 0x02C6: 0x10FFA6, 0x02C7: 0x10FFAE,
        0x02C9: 0x10FF1E, 0x02D8: 0x10FFB2, 0x02DC: 0x10FFAA, 0x0332: 0x10FF1D, 0x0333: 0x10FF1C,
        0x033F: 0x10FF1B, 0x2016: 0x10FFCE, 0x2045: 0x10FFBB, 0x2046: 0x10FFBE, 0x20D6: 0x10FE4A,
        0x20D7: 0x10FE4E, 0x221A: 0x10FF6E, 0x2308: 0x10FFE2, 0x2309: 0x10FFE5, 0x230A: 0x10FFE8,
        0x230B: 0x10FFEB, 0x23B4: 0x10FEFD, 0x23B5: 0x10FF00, 0x23DC: 0x10FEF7, 0x23DD: 0x10FEFA,
        0x23DE: 0x10FFC1, 0x23DF: 0x10FFC4, 0x23E0: 0x10FEF1, 0x23E1: 0x10FEF4, 0x27C5: 0x10FDF3,
        0x27C6: 0x10FDF4, 0x27E6: 0x10FFDA, 0x27E7: 0x10FFDE, 0x27E8: 0x10FF89, 0x27E9: 0x10FF7C,
        0x27EA: 0x10FF76, 0x27EB: 0x10FF79, 0x29FC: 0x10FEC8, 0x29FD: 0x10FECB
      })
    ]],
    '-largeop': ['ASNA-M', [
      CHARS.Map({
        0x0028: 0x10FFF5, 0x0029: 0x10FFF8, 0x002F: 0x10FFD3, 0x005B: 0x10FFEF, 0x005D: 0x10FFF2,
        0x007B: 0x10FFFB, 0x007C: 0x10FFD7, 0x007D: 0x10FFFE, 0x02C6: 0x10FFA7, 0x02C7: 0x10FFAF,
        0x02C9: 0x10FF19, 0x02D8: 0x10FFB3, 0x02DC: 0x10FFAB, 0x0332: 0x10FF18, 0x0333: 0x10FF17,
        0x033F: 0x10FF16, 0x2016: 0x10FFCF, 0x2045: 0x10FFBC, 0x2046: 0x10FFBF, 0x20D6: 0x10FE4B,
        0x20D7: 0x10FE4F, 0x220F: 0x10FF9F, 0x2210: 0x10FFA2, 0x2211: 0x10FF9C, 0x221A: 0x10FF6F,
        0x2229: 0x10FF8E, 0x222B: 0x10FF99, 0x222C: 0x10FF6A, 0x222D: 0x10FF67, 0x222E: 0x10FF64,
        0x222F: 0x10FF61, 0x2230: 0x10FF5E, 0x2231: 0x10FF5B, 0x2232: 0x10FF58, 0x2233: 0x10FF55,
        0x22C0: 0x10FF92, 0x22C1: 0x10FF94, 0x22C2: 0x10FF8E, 0x22C3: 0x10FF8C, 0x2308: 0x10FFE3,
        0x2309: 0x10FFE6, 0x230A: 0x10FFE9, 0x230B: 0x10FFEC, 0x23B4: 0x10FEFE, 0x23B5: 0x10FF01,
        0x23DC: 0x10FEF8, 0x23DD: 0x10FEFB, 0x23DE: 0x10FFC2, 0x23DF: 0x10FFC5, 0x23E0: 0x10FEF2,
        0x23E1: 0x10FEF5, 0x27C5: 0x10FDF5, 0x27C6: 0x10FDF6, 0x27E6: 0x10FFDB, 0x27E7: 0x10FFDF,
        0x27E8: 0x10FF8A, 0x27E9: 0x10FF7D, 0x27EA: 0x10FF77, 0x27EB: 0x10FF7A, 0x29FC: 0x10FEC9,
        0x29FD: 0x10FECC, 0x2A00: 0x10FF96, 0x2A01: 0x10FF98, 0x2A02: 0x10FF7F, 0x2A03: 0x10FF81,
        0x2A04: 0x10FF90, 0x2A05: 0x10FF83, 0x2A06: 0x10FF85, 0x2A07: 0x10FF72, 0x2A08: 0x10FF74,
        0x2A09: 0x10FF87, 0x2A0C: 0x10FF1F, 0x2A0D: 0x10FF22, 0x2A0E: 0x10FF25, 0x2A0F: 0x10FF28,
        0x2A10: 0x10FF2B, 0x2A11: 0x10FF2E, 0x2A12: 0x10FF31, 0x2A13: 0x10FF34, 0x2A14: 0x10FF37,
        0x2A15: 0x10FF3A, 0x2A16: 0x10FF3D, 0x2A17: 0x10FF40, 0x2A18: 0x10FF43, 0x2A19: 0x10FF46,
        0x2A1A: 0x10FF49, 0x2A1B: 0x10FF4C, 0x2A1C: 0x10FF4F
      })
    ]],
    '-size3': ['ASNA-M', [
      CHARS.Map({
        0x0028: 0x10FFF6, 0x0029: 0x10FFF9, 0x002F: 0x10FFD4, 0x005B: 0x10FFF0, 0x005D: 0x10FFF3,
        0x007B: 0x10FFFC, 0x007C: 0x10FFD8, 0x007D: 0x10FFFF, 0x02C6: 0x10FFA8, 0x02C7: 0x10FFB0,
        0x02C9: 0x10FF15, 0x02D8: 0x10FFB4, 0x02DC: 0x10FFAC, 0x0332: 0x10FF14, 0x0333: 0x10FF13,
        0x033F: 0x10FF12, 0x2016: 0x10FFD0, 0x2045: 0x10FFBD, 0x2046: 0x10FFC0, 0x20D6: 0x10FE4C,
        0x20D7: 0x10FE50, 0x220F: 0x10FFA0, 0x2210: 0x10FFA3, 0x2211: 0x10FF9D, 0x221A: 0x10FF70,
        0x2229: 0x10FD73, 0x222B: 0x10FF9A, 0x222C: 0x10FF6B, 0x222D: 0x10FF68, 0x222E: 0x10FF65,
        0x222F: 0x10FF62, 0x2230: 0x10FF5F, 0x2231: 0x10FF5C, 0x2232: 0x10FF59, 0x2233: 0x10FF56,
        0x22C0: 0x10FF93, 0x22C1: 0x10FF95, 0x22C2: 0x10FF8F, 0x22C3: 0x10FD72, 0x2308: 0x10FFE4,
        0x2309: 0x10FFE7, 0x230A: 0x10FFEA, 0x230B: 0x10FFED, 0x23B4: 0x10FEFF, 0x23B5: 0x10FF02,
        0x23DC: 0x10FEF9, 0x23DD: 0x10FEFC, 0x23DE: 0x10FFC3, 0x23DF: 0x10FFC6, 0x23E0: 0x10FEF3,
        0x23E1: 0x10FEF6, 0x27C5: 0x10FDF7, 0x27C6: 0x10FDF8, 0x27E6: 0x10FFDC, 0x27E7: 0x10FFE0,
        0x27E8: 0x10FF8B, 0x27E9: 0x10FF7E, 0x27EA: 0x10FF78, 0x27EB: 0x10FF7B, 0x29FC: 0x10FECA,
        0x29FD: 0x10FECD, 0x2A00: 0x10FF97, 0x2A01: 0x10FFA5, 0x2A02: 0x10FF80, 0x2A03: 0x10FF82,
        0x2A04: 0x10FF91, 0x2A05: 0x10FF84, 0x2A06: 0x10FF86, 0x2A07: 0x10FF73, 0x2A08: 0x10FF75,
        0x2A09: 0x10FF88, 0x2A0C: 0x10FF20, 0x2A0D: 0x10FF23, 0x2A0E: 0x10FF26, 0x2A0F: 0x10FF29,
        0x2A10: 0x10FF2C, 0x2A11: 0x10FF2F, 0x2A12: 0x10FF32, 0x2A13: 0x10FF35, 0x2A14: 0x10FF38,
        0x2A15: 0x10FF3B, 0x2A16: 0x10FF3E, 0x2A17: 0x10FF41, 0x2A18: 0x10FF44, 0x2A19: 0x10FF47,
        0x2A1A: 0x10FF4A, 0x2A1B: 0x10FF4D, 0x2A1C: 0x10FF50
      })
    ]],
    '-size4': ['ASNA-M', [
      CHARS.Map({
        0x002F: 0x10FFD5, 0x007C: 0x10FFD9, 0x02C6: 0x10FFA9, 0x02C7: 0x10FFB1, 0x02D8: 0x10FFB5,
        0x02DC: 0x10FFAD, 0x2016: 0x10FFD1, 0x20D6: 0x10FE4D, 0x20D7: 0x10FE51, 0x220F: 0x10FFA1,
        0x2210: 0x10FFA4, 0x2211: 0x10FF9E, 0x221A: 0x10FF71, 0x2229: 0x10FF8F, 0x222B: 0x10FF9B,
        0x222C: 0x10FF6C, 0x222D: 0x10FF69, 0x222E: 0x10FF66, 0x222F: 0x10FF63, 0x2230: 0x10FF60,
        0x2231: 0x10FF5D, 0x2232: 0x10FF5A, 0x2233: 0x10FF57, 0x22C3: 0x10FF8D, 0x27C5: 0x10FDF9,
        0x27C6: 0x10FDFA, 0x27E6: 0x10FFDD, 0x27E7: 0x10FFE1, 0x2A0C: 0x10FF21, 0x2A0D: 0x10FF24,
        0x2A0E: 0x10FF27, 0x2A0F: 0x10FF2A, 0x2A10: 0x10FF2D, 0x2A11: 0x10FF30, 0x2A12: 0x10FF33,
        0x2A13: 0x10FF36, 0x2A14: 0x10FF39, 0x2A15: 0x10FF3C, 0x2A16: 0x10FF3F, 0x2A17: 0x10FF42,
        0x2A18: 0x10FF45, 0x2A19: 0x10FF48, 0x2A1A: 0x10FF4B, 0x2A1B: 0x10FF4E, 0x2A1C: 0x10FF51
      })
    ]],
    '-size5': ['ASNA-M', [
      CHARS.Map({
        0x007C: 0x10FD76, 0x02C6: 0x10FDFF, 0x02D7: 0x10FDFD, 0x02DC: 0x10FDFE, 0x27C5: 0x10FDFB,
        0x27C6: 0x10FDFC
      })
    ]],
    '-size6': ['ASNA-M', [
      CHARS.Map({
        0x007C: 0x10FD75,
      })
    ]],
    '-tex-calligraphic': ['ASNA-M', [
      CHARS.MapTo(0x10FEA2, CHARSET.AlphaUC, 0x41).feature('salt')   // Upper-case calligraphic
    ]],
    '-tex-bold-calligraphic': ['ASNA-M', [
      CHARS.MapTo(0x10FE87, CHARSET.AlphaUC, 0x41).feature('salt')   // Upper-case bold calligraphic
    ]],
    '-tex-oldstyle': ['ASNA-M', [
      CHARS.MapTo(0x10FEBD, CHARSET.Numbers, 0x31).feature('onum'),
    ]],
    '-tex-variant': ['ASNA-M', [
      CHARS.Map({0x03D0: 0x10FECE, 0x03D1: 0x10FECF}).feature('salt'),
      CHARS.At(0x210F).feature('var'),
      CHARS.Map({
        0x03C6: 0x10FEF0,
        0x2190: 0x10FF11, 0x2192: 0x10FF0F,
        0x21D0: 0x10FF0D, 0x21D2: 0x10FF0B
      }),
      CHARS.Range(0x2032, 0x2037),
      CHARS.At(0x2057),
      CHARSET.PseudoScripts
    ]],
    '-extend': ['ASNA-M', [
      CHARS.Map({
//        0xE001: 0x10FF6D, 0xE002: 0x20D3,                                        // root top and extender
        0xE010: 0x10FFC7, 0xE011: 0x10FFC8, 0xE012: 0x10FFC9, 0xE013: 0x10FFCA,  // overbrace parts
        0xE014: 0x10FFCB, 0xE015: 0x10FFCC, 0xE016: 0x10FFCD, 0xE017: 0x10FEF0,  // underbrace parts
        0xE018: 0x10FEEC, 0xE019: 0x10FEEB, 0xE01A: 0x10FEED,                    // overbracket parts
        0xE01C: 0x10FEEE, 0xE01D: 0x10FEEA, 0xE01E: 0x10FEEF,                    // underbracket parts
        0xE020: 0x10FFB6, 0xE021: 0x10FFBA, 0xE022: 0x10FFB7, 0xE023: 0x10FF53,
        0xE024: 0x10FFB8, 0xE025: 0x10FF52, 0xE026: 0x10FFB9, 0xE027: 0x10FF54,
        0x20D0: 0x10FF1A, 0x20EE: 0x10FD74,
        0x2190: 0x10FF10, 0x2191: 0x10FEC6, 0x2192: 0x10FF0E,
        0x21A4: 0x10FF08, 0x21A6: 0x10FF07,
        0x21A9: 0x10FF06, 0x21AA: 0x10FF05,
        0x21D0: 0x10FF0C, 0x21D1: 0x10FEC7, 0x21D2: 0x10FF09,
        0x2906: 0x10FF04, 0x2907: 0x10FF03
      }),
      CHARS.At(0x21BC, 0x21BD, 0x21C0, 0x21C1, 0xE001, 0xE002).feature('ext')
    ]]
  }, {
    spaces: {
      normal: {
        0x2000: .5, 0x2001: 1, 0x2007: .5, 0x2008: .5, 0x200B: 0,
        0x200C: 0, 0x200D: 0, 0x202F: .2, 0x205F: .222,
        0x2060: 0, 0x2061: 0, 0x2062: 0, 0x2063: 0, 0x2064: 0
      }
    },
    transferHD: [
      [0x2212, 0x002B]    // make minus the same height/depth as plus
    ],
    fixIC: [
      ...integralIC('-largeop', .3, .2),
      ...integralIC('-size3', .35, .25),
      ...integralIC('-size4', .4, .3),
      ...integralIC('normal', .15, .1)
    ],
  });

  /***********************************************************************************/
  /***********************************************************************************/

  const AsanaDelimiters = Delimiters.define({
    font: 'ASNA-M',
    variants: AsanaVariants,
    sizeVariants: ['normal'],
    stretchVariants: ['normal'],
    readMathTable: true,
    adjustMathTable: {
      0x2190: {parts: [ , , 0]},
      0x2192: {parts: [0]},
      0x21D0: {parts: [ , , 0]},
      0x21D2: {parts: [0]},
      0x221A: {parts: [[0xE001, 'ext'], [0xE002, 'ext']]}
    },
    add: {
      0x003D: {dir: 'H', parts: [0, 0x003D]},
      0x2013: {dir: 'H', sizes: 1, parts: [0, 0x2013]},
      0x2014: {dir: 'H', sizes: 1, parts: [0, 0x2014]},
      0x2015: {dir: 'H', parts: [0, 0x2015]},
      0x2017: {dir: 'H', sizes: 1, parts: [0, 0x2017]},
      0x2195: {dir: 'V', sizes: 1, parts: [0x2191, [0x2191, '-extend'], 0x2193]},
      0x219E: {dir: 'H', sizes: 1, parts: [0x219E, [0x2190, '-extend']]},
      0x21A0: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-extend'], 0x21A0]},
      0x21A5: {dir: 'V', sizes: 1, parts: [0x2191, [0x2191, '-extend'], 0x005F]},
      0x21A7: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-extend'], 0x2193]},
      0x21BC: {dir: 'H', sizes: 1, parts: [[0x21BC, '-extend'], [0x2190, '-extend']]},
      0x21BD: {dir: 'H', sizes: 1, parts: [[0x21BD, '-extend'], [0x2190, '-extend']]},
      0x21BE: {dir: 'V', sizes: 1, parts: [0x21BE, [0x2191, '-extend']]},
      0x21BF: {dir: 'V', sizes: 1, parts: [0x21BF, [0x2191, '-extend']]},
      0x21C0: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-extend'], [0x21C0, '-extend']]},
      0x21C1: {dir: 'H', sizes: 1, parts: [0, [0x2190, '-extend'], [0x21C1, '-extend']]},
      0x21C2: {dir: 'V', sizes: 1, parts: [0, [0x2191, '-extend'], 0x21C2]},
      0x21C3: {dir: 'V', sizes: 1, parts: [0, [0x2191, '-extend'], 0x21C3]},
      0x21D5: {dir: 'V', sizes: 1, parts: [0x21D1, [0x21D1, '-extend'], 0x21D3]},
      0x21DA: {dir: 'H', sizes: 1, parts: [0x21DA, 0x2261]},
      0x21DB: {dir: 'H', sizes: 1, parts: [0, 0x2261, 0x21DB]},
      0x2212: {dir: 'H', parts: [0, 0x2212]},
      0x23AA: {dir: 'V', sizes: 1, parts: [0, 0x23AA, 0]},
      0x23B0: {dir: 'V', sizes: 1, parts: [0x23A7, 0x23AA, 0x23AD]},
      0x23B1: {dir: 'V', sizes: 1, parts: [0x23AB, 0x23AA, 0x23A9]},
      0x23D0: {dir: 'V', parts: [0, 0x2223]},
      0x27EE: {dir: 'V', sizes: 1, parts: [0x23A7, 0x23AA, 0x23A9]},
      0x27EF: {dir: 'V', sizes: 1, parts: [0x23AB, 0x23AA, 0x23AD]},
      0x294A: {dir: 'H', sizes: 1, parts: [[0x21BC, '-extend'], [0x2190, '-extend'], [0x21C1, '-extend']]},
      0x294B: {dir: 'H', sizes: 1, parts: [[0x21BD, '-extend'], [0x2190, '-extend'], [0x21C0, '-extend']]},
      0x294C: {dir: 'V', sizes: 1, parts: [0x21BE, [0x2191, '-extend'], 0x21C3]},
      0x294D: {dir: 'V', sizes: 1, parts: [0x21BF, [0x2191, '-extend'], 0x21C2]},
      0x294E: {dir: 'H', sizes: 1, parts: [[0x21BC, '-extend'], [0x2190, '-extend'], [0x21C0, '-extend']]},
      0x294F: {dir: 'V', sizes: 1, parts: [0x21BE, [0x2191, '-extend'], 0x21C2]},
      0x2950: {dir: 'H', sizes: 1, parts: [[0x21BD, '-extend'], [0x2190, '-extend'], [0x21C1, '-extend']]},
      0x2951: {dir: 'V', sizes: 1, parts: [0x21BF, [0x2191, '-extend'], 0x21C3]},
      0x295A: {dir: 'H', sizes: 1, parts: [[0x21BC, '-extend'], [0x2190, '-extend'], [0x21A4, '-extend']]},
      0x295B: {dir: 'H', sizes: 1, parts: [[0x21A6, '-extend'], [0x2190, '-extend'], [0x21C0, '-extend']]},
      0x295C: {dir: 'V', sizes: 1, parts: [0x21BE, [0x2191, '-extend'], 0x005F]},
      0x295D: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-extend'], 0x21C2]},
      0x295E: {dir: 'H', sizes: 1, parts: [[0x21BD, '-extend'], [0x2190, '-extend'], [0x21A4, '-extend']]},
      0x295F: {dir: 'H', sizes: 1, parts: [[0x21A6, '-extend'], [0x2190, '-extend'], [0x21C1, '-extend']]},
      0x2960: {dir: 'V', sizes: 1, parts: [0x21BF, [0x2191, '-extend'], 0x005F]},
      0x2961: {dir: 'V', sizes: 1, parts: [0x005F, [0x2191, '-extend'], 0x21C3]},
      0x2A1A: {dir: 'V', sizes: 4, variants: ['normal', '-largeop', '-size3', '-size4']}, // normal not in MATH table
    },
    alias: {
      0x002D: 0x2212,
      0x005E: 0x0302,
      0x005F: 0x2013,
      0x007E: 0x0303,
      0x00AF: 0x0305,
      0x02C6: 0x0302,
      0x02C7: 0x030C,
      0x02C9: 0x0305,
      0x02D8: 0x0306,
      0x02DC: 0x0303,
      0x203E: 0x00AF,
      0x2215: 0x002F,
      0x2312: 0x23DC,
      0x2322: 0x23DC,
      0x2323: 0x23DD,
      0x2329: 0x27E8,
      0x232A: 0x27E9,
      0x23AF: 0x2013,
      0x2500: 0x2013,
      0x2758: 0x2223,
      0x27F5: 0x2190,
      0x27F6: 0x2192,
      0x27F7: 0x2194,
      0x27F8: 0x21D0,
      0x27F9: 0x21D2,
      0x27FA: 0x21D4,
      0x27FB: 0x21A4,
      0x27FC: 0x21A6,
      0x27FD: 0x2906,
      0x27FE: 0x2907,
      0x3008: 0x27E8,
      0x3009: 0x27E9,
      0xFE37: 0x23DE,
      0xFE38: 0x23DF,
    },
    fullExtenders: {0x221A: [.65, Math.round((.414 + 1.388 + Number.EPSILON) * 1000) / 1000]}
  });

  /***********************************************************************************/
  /***********************************************************************************/

  const AsanaData: FontDef = {
    name: 'MathJaxAsana',
    prefix: 'ASNA',
    variants: AsanaVariants,
    delimiters: AsanaDelimiters,
    ranges: [
      ['latin', {
        LR: {normal: RANGES.LATIN}
      }],
      ['double-struck', {
        DS: {
          normal: RANGES.DOUBLESTRUCK,
          'double-struck': RANGES.DOTLESS
        }
      }],
      ['fraktur', {
        F: {
          normal: RANGES.FRAKTUR_NORMAL,
          fraktur: RANGES.DOTLESS
        },
        FB: {
          normal: RANGES.FRAKTUR_BOLD,
          'bold-fraktur': RANGES.DOTLESS
        }
      }],
      ['script', {
        S: {
          normal: RANGES.SCRIPT_NORMAL,
          script: RANGES.DOTLESS
        },
        SB: {
          normal: RANGES.SCRIPT_BOLD,
          'bold-script': RANGES.DOTLESS
        }
      }],
      ['sans-serif', {
        SS: {
          normal: RANGES.SANSSERIF_NORMAL,
          'sans-serif': RANGES.DOTLESS
        },
        SSB: {
          normal: RANGES.SANSSERIF_BOLD,
          'bold-sans-serif': RANGES.DOTLESS
        },
        SSI: {
          normal: RANGES.SANSSERIF_ITALIC,
          'sans-serif-italic': RANGES.DOTLESS
        },
        SSBI: {
          normal: RANGES.SANSSERIF_BOLDITALIC,
          'sans-serif-bold-italic': RANGES.DOTLESS
        }
      }],
      ['monospace', {
        M: {
          normal: RANGES.MONOSPACE,
          monospace: RANGES.DOTLESS
        }
      }],
      ['calligraphic', {
        C: {'-tex-calligraphic': RANGES.ALPHAUC},
        CB: {'-tex-bold-calligraphic': RANGES.ALPHAUC}
      }],
      ['cherokee', {
        CK: {normal: [[0x13A0, 0x13FF]]}
      }],
      ['greek', {
        GK: {normal: RANGES.GREEK}
      }],
      ['math', {
        MM: {normal: RANGES.MATH}
      }, [0x27C5, 0x27C6, 0x29FC, 0x29FD]],
      ['symbols', {
        SY: {normal: RANGES.SYMBOLS},
        '': {'-largeop': [0x2140]}
      }, [
        0x2017, 0x2045, 0x2046
      ]],
      ['arrows', {
        AR: {normal: RANGES.ARROWS},
        ARB: {bold: RANGES.ARROWS}
      }, [
        0x21A5, 0x21A7, 0x27FD, 0x27FE, 0x2906, 0x2907, 0x294C,
        0x294D, 0x294F, 0x2951, 0x295C, 0x295D, 0x2960, 0x2961
      ]],
      ['accents', {
        '': {
          normal: RANGES.ACCENTS,
          '-smallop': RANGES.LARGE_ACCENTS,
          '-largeop': RANGES.LARGE_ACCENTS,
          '-size3':   RANGES.LARGE_ACCENTS
        }
      }, [
        0x332, 0x333, 0x33F, 0x20E9
      ]],
      ['shapes', {
        SH: {normal: RANGES.SHAPES}
      }],
      ['stretchy', {
        '': {
          normal: [...RANGES.PARTS, RANGES.INTEGRALS[1]],
          '-largeop': [RANGES.INTEGRALS[1]],
          '-size3': [...RANGES.INTEGRALS, [0x220F, 0x2211], [0x2A00, 0x2A09]],
          '-size4': [...RANGES.INTEGRALS, [0x220F, 0x2211], [0x2A00, 0x2A09]]
        }
      }, [
        RANGES.INTEGRALS[1]
      ]]
    ],
    legal: {
      addCopyright: 'Copyright (c) 2022 MathJax, Inc. (www.mathjax.org)'
    }
  };

  /***********************************************************************************/
  /***********************************************************************************/

  CommonFont.define(AsanaData).writeFont();

  Components.define('svg', AsanaData).writeFont().writeComponent();
  SVGFont.define(AsanaData).writeFont();

  Components.define('chtml', AsanaData).writeFont().writeComponent();
  CHTMLFont.define(AsanaData).writeFont().makeWoffFonts('ASNA-M');

} catch (err) {
  console.error(err);
  process.exit(1);
}
