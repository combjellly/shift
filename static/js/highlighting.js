const inputEl = document.querySelector('[data-el="input"]');
const highlightEl = document.querySelector('[data-el="highlight"]');

// Keep your showdown converter & resizeTextarea from before if you need them
const converter = new showdown.Converter({
    metadata: true,
    parseImgDimensions: true,
    strikethrough: true,
    tables: true,
    ghCodeBlocks: true,
    smoothLivePreview: true,
    simpleLineBreaks: true,
    emoji: true,
});

const resizeTextarea = (textArea) => {
    if (!textArea) return;
    window.requestAnimationFrame(() => {
        textArea.style.height = 0;
        if (textArea.scrollHeight > 0) {
            textArea.style.height = `${textArea.scrollHeight + 2}px`;
        }
    });
};

// --- NEW: block splitting & toggle logic ---
function splitEveryBlocks(text) {
    console.log (text.split(/(?=^\s*(?:\{\-\s*)?every\b)/gm))
  return text.split(/(?=^\s*(?:\{\-\s*)?every\b)/gm);
}

function toggleEveryComment(blockIndex) {
    const raw = inputEl.value;
    const blocks = splitEveryBlocks(raw);

    let blockText = blocks[blockIndex];
    const trimmed = blockText.trim();

    if (trimmed.startsWith('{-') && trimmed.includes('every') && trimmed.endsWith('-}')) {
        // Uncomment: remove comment markers only if it's a commented 'every' block
        let uncommented = blockText.replace(/^\s*{\-\s*/, '').replace(/\s*\-\}\s*$/, '');
        // Ensure uncommented block ends with a newline
        if (!uncommented.endsWith('\n')) {
            uncommented += '\n';
        }
        blocks[blockIndex] = uncommented;
    } else if (trimmed.startsWith('every')) {
        // Comment: wrap with {- at start and -} before trailing newlines
        const match = blockText.match(/([\s\S]*?)(\n*)$/);
        const mainText = match ? match[1] : blockText;
        const trailingNewlines = match ? match[2] : '';

        blocks[blockIndex] = `{- ${mainText} -}${trailingNewlines}`;
    } else {
        // For safety: leave unchanged if block doesn't start with 'every' or '{- every'
        blocks[blockIndex] = blockText;
    }

    inputEl.value = blocks.join('');
    highlight();
}




// --- COMBINED highlight() ---
const highlight = () => {
    window.requestAnimationFrame(() => {
        // Register your Shift language
        hljs.registerLanguage('shift', function (hljs) {
            return {
                keywords: {
                    literal: 'play',
                    control: 'if else for every while '
                },
                contains: [
                    hljs.COMMENT('--', '$'),
                    hljs.COMMENT('{-', '-}'),
                    {
                        className: 'string',
                        begin: /"/, end: /"/
                    },
                    {
                        className: 'number',
                        begin: /\b\d+(\.\d+)?\b/
                    },
                    {
                        className: 'symbol',
                        begin: /\*|\/|\.|:|==|!=|<=|>=|<|>|=/
                    }
                ]
            };
        });

        const raw = inputEl.value;
        const blocks = splitEveryBlocks(raw);

const htmlBlocks = blocks.map((block, idx) => {
    const highlightedBlock = hljs.highlight("shift", block).value;
    const isCommented = /^\s*\{\-\s*every[\s\S]*\-\}\s*$/.test(block);
    if (/^\s*\bevery\b/.test(block) || isCommented) {
        return `<div class="every-block ${isCommented ? 'commented' : ''}" data-block-index="${idx}">${highlightedBlock}<div class="click-zone" title="Click here to toggle mute"></div></div>`;
    }
    return highlightedBlock;
});


        highlightEl.innerHTML = htmlBlocks.join('');

        // Attach click events to each .every-block
highlightEl.querySelectorAll('.every-block .click-zone').forEach(zone => {
  zone.addEventListener('click', e => {
    e.stopPropagation();
    const parent = zone.closest('.every-block');
    const index = parseInt(parent.getAttribute('data-block-index'), 10);
    toggleEveryComment(index);
  });
});

    });
};

// --- Init ---
const init = () => {
    inputEl.addEventListener("input", () => {
        resizeTextarea(inputEl);
        highlight();
    });
    inputEl.setAttribute('data-initialized', true);
};

document.addEventListener("DOMContentLoaded", () => {
    init();
    resizeTextarea(inputEl);
    highlight();
});
