import { ChtmlFontData, ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData, DelimiterMap, CharMapMap } from '@mathjax/src/mjs/output/chtml/FontData.js';
import { StringMap } from '@mathjax/src/mjs/output/common/Wrapper.js';
declare const Base: import("@mathjax/src/mjs/output/common/FontData.js").FontDataClass<ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData> & typeof ChtmlFontData;
export declare class MathJaxTexFont extends Base {
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
        'mjx-container[jax="CHTML"] > mjx-math.TEX-N[breakable] > *': {
            'font-family': string;
        };
        '.TEX-N': {
            'font-family': string;
        };
        '.TEX-B': {
            'font-family': string;
        };
        '.TEX-I': {
            'font-family': string;
        };
        '.TEX-BI': {
            'font-family': string;
        };
        '.TEX-F': {
            'font-family': string;
        };
        '.TEX-FB': {
            'font-family': string;
        };
        '.TEX-SS': {
            'font-family': string;
        };
        '.TEX-SSB': {
            'font-family': string;
        };
        '.TEX-SSI': {
            'font-family': string;
        };
        '.TEX-M': {
            'font-family': string;
        };
        '.TEX-SO': {
            'font-family': string;
        };
        '.TEX-LO': {
            'font-family': string;
        };
        '.TEX-C': {
            'font-family': string;
        };
        '.TEX-CB': {
            'font-family': string;
        };
        '.TEX-OS': {
            'font-family': string;
        };
        '.TEX-OB': {
            'font-family': string;
        };
        '.TEX-MI': {
            'font-family': string;
        };
        '.TEX-V': {
            'font-family': string;
        };
        '.TEX-S3': {
            'font-family': string;
        };
        '.TEX-S4': {
            'font-family': string;
        };
    };
    protected static defaultFonts: {
        '@font-face /* MJX-TEX-ZERO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-BRK */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-N */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-B */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-I */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-BI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-F */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-FB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-SS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-SSB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-SSI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-M */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-SO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-LO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-C */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-CB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-OS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-OB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-MI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-V */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-S3 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-TEX-S4 */': {
            'font-family': string;
            src: string;
        };
    };
    cssFontPrefix: string;
}
export {};
