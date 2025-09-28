import { ChtmlFontData, ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData, DelimiterMap, CharMapMap } from '@mathjax/src/cjs/output/chtml/FontData.js';
import { StringMap } from '@mathjax/src/cjs/output/common/Wrapper.js';
declare const Base: import("@mathjax/src/cjs/output/common/FontData.js").FontDataClass<ChtmlCharOptions, ChtmlVariantData, ChtmlDelimiterData> & typeof ChtmlFontData;
export declare class MathJaxAsanaFont extends Base {
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
        'mjx-container[jax="CHTML"] > mjx-math.ASNA-N[breakable] > *': {
            'font-family': string;
        };
        '.ASNA-N': {
            'font-family': string;
        };
        '.ASNA-B': {
            'font-family': string;
        };
        '.ASNA-I': {
            'font-family': string;
        };
        '.ASNA-BI': {
            'font-family': string;
        };
        '.ASNA-DS': {
            'font-family': string;
        };
        '.ASNA-F': {
            'font-family': string;
        };
        '.ASNA-FB': {
            'font-family': string;
        };
        '.ASNA-S': {
            'font-family': string;
        };
        '.ASNA-SB': {
            'font-family': string;
        };
        '.ASNA-SS': {
            'font-family': string;
        };
        '.ASNA-SSB': {
            'font-family': string;
        };
        '.ASNA-SSI': {
            'font-family': string;
        };
        '.ASNA-SSBI': {
            'font-family': string;
        };
        '.ASNA-M': {
            'font-family': string;
        };
        '.ASNA-SO': {
            'font-family': string;
        };
        '.ASNA-LO': {
            'font-family': string;
        };
        '.ASNA-S3': {
            'font-family': string;
        };
        '.ASNA-S4': {
            'font-family': string;
        };
        '.ASNA-S5': {
            'font-family': string;
        };
        '.ASNA-S6': {
            'font-family': string;
        };
        '.ASNA-C': {
            'font-family': string;
        };
        '.ASNA-CB': {
            'font-family': string;
        };
        '.ASNA-OS': {
            'font-family': string;
        };
        '.ASNA-V': {
            'font-family': string;
        };
        '.ASNA-E': {
            'font-family': string;
        };
    };
    protected static defaultFonts: {
        '@font-face /* MJX-ASNA-ZERO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-BRK */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-N */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-B */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-I */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-BI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-DS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-F */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-FB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-S */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SSB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SSI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SSBI */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-M */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-SO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-LO */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-S3 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-S4 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-S5 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-S6 */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-C */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-CB */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-OS */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-V */': {
            'font-family': string;
            src: string;
        };
        '@font-face /* MJX-ASNA-E */': {
            'font-family': string;
            src: string;
        };
    };
    protected static dynamicFiles: import("@mathjax/src/cjs/output/common/FontData.js").DynamicFileList;
    cssFontPrefix: string;
}
export {};
