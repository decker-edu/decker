import { ChtmlFontData, ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData, DelimiterMap, CharMapMap } from '@mathjax/src/cjs/output/chtml/FontData.js';
import { StringMap } from '@mathjax/src/cjs/output/common/Wrapper.js';
declare const Base: import("@mathjax/src/cjs/output/common/FontData.js").FontDataClass<ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData> & typeof ChtmlFontData;
export declare class MathJaxFiraFont extends Base {
    static NAME: string;
    static OPTIONS: {
        fontURL: string;
        dynamicPrefix: string;
    };
    protected static defaultCssFamilyPrefix: string;
    protected static defaultVariantLetters: StringMap;
    protected static defaultDelimiters: DelimiterMap<ChtmlDelimiterData>;
    protected static defaultChars: CharMapMap<ChtmlCharOptions>;
    protected static defaultStyles: {
        'mjx-container[jax="CHTML"] > mjx-math.FIRA-N[breakable] > *': {
            'font-family': string;
        };
        '.FIRA-N': {
            'font-family': string;
        };
        '.FIRA-B': {
            'font-family': string;
        };
        '.FIRA-I': {
            'font-family': string;
        };
        '.FIRA-BI': {
            'font-family': string;
        };
        '.FIRA-SSI': {
            'font-family': string;
        };
        '.FIRA-SSBI': {
            'font-family': string;
        };
        '.FIRA-M': {
            'font-family': string;
        };
        '.FIRA-SO': {
            'font-family': string;
        };
        '.FIRA-LO': {
            'font-family': string;
        };
        '.FIRA-S3': {
            'font-family': string;
        };
        '.FIRA-S4': {
            'font-family': string;
        };
        '.FIRA-S5': {
            'font-family': string;
        };
        '.FIRA-S6': {
            'font-family': string;
        };
        '.FIRA-S7': {
            'font-family': string;
        };
        '.FIRA-S8': {
            'font-family': string;
        };
        '.FIRA-S9': {
            'font-family': string;
        };
        '.FIRA-S10': {
            'font-family': string;
        };
        '.FIRA-S11': {
            'font-family': string;
        };
        '.FIRA-S12': {
            'font-family': string;
        };
        '.FIRA-S13': {
            'font-family': string;
        };
        '.FIRA-S14': {
            'font-family': string;
        };
        '.FIRA-S15': {
            'font-family': string;
        };
        '.FIRA-V': {
            'font-family': string;
        };
        '.FIRA-MI': {
            'font-family': string;
        };
        '.FIRA-OS': {
            'font-family': string;
        };
        '.FIRA-OB': {
            'font-family': string;
        };
        '.FIRA-C': {
            'font-family': string;
        };
        '.FIRA-CB': {
            'font-family': string;
        };
        '.FIRA-LT': {
            'font-family': string;
        };
        '.FIRA-RB': {
            'font-family': string;
        };
        '.FIRA-E': {
            'font-family': string;
        };
        '.FIRA-Me': {
            'font-family': string;
        };
        '.FIRA-U': {
            'font-family': string;
        };
        '.FIRA-D': {
            'font-family': string;
        };
    };
    protected static defaultFonts: {
        '@font-face /* MJX-FIRA-ZERO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-BRK */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-N */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-B */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-I */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-BI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-SSI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-SSBI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-M */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-SO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-LO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S3 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S4 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S5 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S6 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S7 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S8 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S9 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S10 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S11 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S12 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S13 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S14 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-S15 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-V */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-MI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-OS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-OB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-C */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-CB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-LT */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-RB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-E */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-Me */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-U */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-FIRA-D */': {
            'font-family': string;
            src: string;
        };
    };
    protected static dynamicFiles: import("@mathjax/src/cjs/output/common/FontData.js").DynamicFileList;
    cssFontPrefix: string;
}
export {};
