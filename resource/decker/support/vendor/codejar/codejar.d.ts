type Options = {
    tab: string;
    indentOn: RegExp;
    moveToNewLine: RegExp;
    spellcheck: boolean;
    catchTab: boolean;
    preserveIdent: boolean;
    addClosing: boolean;
    history: boolean;
    window: typeof window;
};
export type Position = {
    start: number;
    end: number;
    dir?: '->' | '<-';
};
export type CodeJar = ReturnType<typeof CodeJar>;
export declare function CodeJar(editor: HTMLElement, highlight: (e: HTMLElement, pos?: Position) => void, opt?: Partial<Options>): {
    updateOptions(newOptions: Partial<Options>): void;
    updateCode(code: string, callOnUpdate?: boolean): void;
    onUpdate(callback: (code: string) => void): void;
    toString: () => string;
    save: () => Position;
    restore: (pos: Position) => void;
    recordHistory: () => void;
    destroy(): void;
};
export {};
