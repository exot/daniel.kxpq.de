MathJax.Hub.Config({
    jax: ["input/TeX", "output/SVG"],
    extensions: ["tex2jax.js","TeX/AMSmath.js","TeX/AMSsymbols.js",
                 "TeX/noUndefined.js"],
    tex2jax: {
        inlineMath: [ ["\\(","\\)"] ],
        displayMath: [ ['$$','$$'], ["\\[","\\]"], ["\\begin{displaymath}","\\end{displaymath}"] ],
        skipTags: ["script","noscript","style","textarea","pre","code"],
        ignoreClass: "tex2jax_ignore",
        processEscapes: false,
        processEnvironments: true,
        preview: "TeX"
    },
    showProcessingMessages: true,
    displayAlign: "center",
    displayIndent: "2em",

    "SVG": {
         scale: 100,
         availableFonts: ["STIX","TeX"],
         preferredFont: "TeX",
         webFont: "TeX",
         imageFont: "TeX",
         showMathMenu: true,
    },
});
