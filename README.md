# markdown-xwidget

Markdown preview using GitHub themes. Features:

- Render markdown using GitHub styles
- Mermaid diagrams
- Syntax highlighting
- Mathjax

| Light                     | Dark                    |
|---------------------------|-------------------------|
| ![light](./doc/light.png) | ![dark](./doc/dark.png) |

These are just two examples; there is a lot more room to customize the theme.

## Prerequisites

1. **Emacs xwidget support**

    This package displays rendered markdown in an xwidget (a embedded chromium
    browser), which only works if your Emacs was compiled `--with-xwidgets`. If
    you don't have xwidget support, you're missing out!

    To check whether your Emacs supports xwidgets, evaluate this in your
    `*scratch*` buffer:

    ```emacs-lisp
    (featurep 'xwidget-internal)
    ;; => t     ; you have xwidgets
    ;; => nil   ; you don't have xwidgets
    ```

    To install Emacs with xwidget support on MacOS, use [homebrew-emacs-plus][1]:

    ```sh
    brew tap d12frosted/emacs-plus
    brew install emacs-plus \
         --with-xwidgets \
         --with-imagemagick
    ```

2. **multimarkdown**

    [multimarkdown][2] is a CLI tool that transforms markdown into HTML
    (similar to `pandoc`). You just have to have it installed.

[1]: https://github.com/d12frosted/homebrew-emacs-plus
[2]: https://fletcher.github.io/MultiMarkdown-6/

## Installation

### package.el

TODO. This package is not on MELPA.

### straight and use-package

```emacs-lisp
(use-package markdown-xwidget
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources")))
```

It's is important to specify the `:files` directive! Without it, the non-elisp
files (CSS and HTML) won't be copied to the right place.

## Usage

After installing markdown-xwidget, open a markdown file and then run `M-x
markdown-live-preview-mode` (<kbd>C-c C-c l</kbd>).

You can run the same command again to kill the live-preview window, or just run
the standard `kill-buffer` (<kbd>C-x k</kbd>) right in the xwidget buffer.

## Configuration

The following variables can be customized, and are shown here along with their
default values:

```emacs-lisp
(setq markdown-xwidget-github-theme "light"
      markdown-xwidget-mermaid-theme "default"
      markdown-xwidget-code-block-theme "default")
```

1. `markdown-xwidget-github-theme`

    - The CSS theme used to stylize markdown elements. Valid values are:
      `"light"`, `"light-colorblind"`, `"light-high-contrast"`,
      `"light-tritanopia"`, `"dark"`, `"dark-dimmed"`, `"dark-colorblind"`,
      `"dark-high-contrast"`, `"dark-tritanopia"`.

      Default value: `"light"`.

2. `markdown-xwidget-mermaid-theme`

    - The mermaid theme to use when rendering mermaid diagrams. These themes are
      documented in mermaid's [Deployable Themes][3]. Valid values are:
      `forest`, `dark`, `default`, `neutral`.

      Default value: `"default"`'.

3. `markdown-xwidget-code-block-theme`

    - Theme to apply to fenced code blocks. A valid value is any filename in
      [highlight.js/src/styles][4] (without the `.css` extension).

      Default value: `"default"`.

[3]: https://mermaid-js.github.io/mermaid/#/theming?id=deployable-themes
[4]: https://github.com/highlightjs/highlight.js/tree/main/src/styles

### Examples

See: [examples.md](./doc/examples.md)

## Devolopment

### Update highlight.js, mermaid, and mathjax

To update the included versions of highlight.js, mermaid, and mathjax, run:

```sh
./scripts/fetch-resources.sh
```

### Update GitHub CSS

> **Info**
>
> This is still a more involved process that I haven't automated. I'm hoping
> find somewhere else to obtain these CSS files. Someone else _must_ create and
> maintain files like these! I just haven't found them.

The github CSS files in the [resources/github_css][5] directory are slightly
modified versions of what can be generated from the [github-markdown-css][6]
project. To generate the CSS, run the `gen_github_css.js` script for every
theme:

```sh
node ./scripts/gen_github_css.js
```

Each run results in a file with two themes. Browsers automatically choose one of
the two themes, using the [prefers-color-scheme][7] media query.

I manually split each generated file into two files -- one for each theme -- by
removing the media query and putting the color variables in a `:root` binding.

[5]: ./resources/github_css
[6]: https://github.com/sindresorhus/github-markdown-css
[7]: https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

## Inspiration

[Centaur Emacs][8] paved the way. I couldn't have figured this out without
Centaur Emacs!

[8]: https://github.com/seagle0128/.emacs.d
