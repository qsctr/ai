'use strict';

const fs = require('fs');
const marked = require('marked');
const prism = require('prismjs');
const customRenderer = new marked.Renderer();

customRenderer.code = function (code, lang) {
    return marked.Renderer.prototype.code.call(this, code, lang)
        .replace('<pre><code>', '<pre><code class="language-none">')
        .replace(/<pre><code class="([^"]*)">/, '<pre class="$1"><code class="$1">');
};

marked.setOptions({
    highlight: (code, lang) => {
        if (!lang) return code;
        if (!(lang in prism.languages)) require('prismjs/components/prism-' + lang);
        return prism.highlight(code, prism.languages[lang]);
    },
    langPrefix: 'language-',
    renderer: customRenderer
});

const mds = fs.readdirSync('posts-md');
const width = Math.max(...mds.map(md => md.length)) + 4;

for (const md of mds) {
    const html = md.replace(/\.md$/, '.html');
    fs.writeFileSync('posts-html/' + html, marked(fs.readFileSync('posts-md/' + md, 'utf-8')));
    console.log(md + ' '.repeat(width - md.length) + '-> ' + html);
}

console.log('All done.');
