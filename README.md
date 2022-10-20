# markdown-xwidget

An Emacs minor mode to preview markdown files using [xwidget-webkit][x].
Features:

- Render markdown using GitHub styles
- Mermaid diagrams
- Syntax highlighting
- Mathjax

| Light                     | Dark                    |
|---------------------------|-------------------------|
| ![light](./doc/light.png) | ![dark](./doc/dark.png) |

These are just two examples. There's a lot of room to customize the theme.

[x]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Embedded-WebKit-Widgets.html

## Prerequisites

This package will not work without the following prerequisites.

1. **Emacs xwidget support**

   This package displays rendered markdown in an xwidget (an embedded browser),
   which only works if your Emacs was compiled `--with-xwidgets`. If you don't
   have xwidget support, you're missing out!

   To check whether your Emacs supports xwidgets, evaluate this in your
   `*scratch*` buffer:

   ```emacs-lisp
   (featurep 'xwidget-internal)
   ;; => t     ; you have xwidgets
   ;; => nil   ; you don't have xwidgets
   ```

   To install Emacs with xwidget support on MacOS, check out
   [homebrew-emacs-plus][emacsplus]:

   ```sh
   brew tap d12frosted/emacs-plus
   brew install emacs-plus \
        --with-xwidgets \
        --with-imagemagick
   ```

2. **multimarkdown, pandoc, or similar**

   [multimarkdown][multimarkdown] and [pandoc][pandoc] are CLI programs that can
   create HTML from markdown. You have to have some kind of program like this
   installed.

[emacsplus]: https://github.com/d12frosted/homebrew-emacs-plus
[multimarkdown]: https://fletcher.github.io/MultiMarkdown-6
[pandoc]: https://pandoc.org/

## Installation

Here are some examples of installing and configuring `markdown-xwidget`.

This package is not yet on ELPA/MELPA.

### straight and use-package

```emacs-lisp
(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "light")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))
```

For a fully-working example, see: [straight/init.el][init-straight].

The `:bind` directive is not strictly necessary, but it conveniently lets you launch `markdown-xwidget-preview-mode` using the same key prefix as other markdown commands. By default, this binds <kbd>C-c C-c x</kbd> to launch `markdown-xwidget-preview-mode`.

> **Warning**
> It's important to specify the `:files` directive! Without it, the non-elisp
> files (CSS and HTML) won't be copied to the right place.

[init-straight]: ./doc/installation/straight/init.el

### quelpa

For a fully-working example, see: [quelpa/init.el][init-quelpa].

[init-quelpa]: ./doc/installation/quelpa/init.el

## Usage

Open a markdown file and then run `M-x markdown-xwidget-preview-mode` to toggle
the live-preview on or off.

See the installation documentation above for an example of creating a keybinding
for that command in `markdown-mode-command-map`.

## Configuration

The following variables can be customized, and are shown here along with their
default values:

```emacs-lisp
(setq markdown-xwidget-command nil
      markdown-xwidget-github-theme "light"
      markdown-xwidget-mermaid-theme "default"
      markdown-xwidget-code-block-theme "default")
```

### markdown-xwidget-command

An executable that can turn markdown into HTML. If `nil`, the value of
`markdown-command` is used (which defaults to the command "markdown"). Some suitable values are `"pandoc"`, `"markdown"`, and `"multimarkdown"`, assuming you have those tools installed. I've had best experience with pandoc.

Default value: `nil`

### markdown-xwidget-github-theme

The CSS theme used to stylize markdown elements. Valid values are: `"light"`,
`"light-colorblind"`, `"light-high-contrast"`, `"light-tritanopia"`, `"dark"`,
`"dark-dimmed"`, `"dark-colorblind"`, `"dark-high-contrast"`,
`"dark-tritanopia"`.

Default value: `"light"`

### markdown-xwidget-mermaid-theme

The mermaid theme to use when rendering mermaid diagrams. These themes are
documented in mermaid's [Deployable Themes][m]. Valid values are: `forest`,
`dark`, `default`, `neutral`.

Default value: `"default"`'

### markdown-xwidget-code-block-theme

Theme to apply to fenced code blocks. A valid value is any filename in
[highlight.js/src/styles][hjs] (without the `.css` extension).

Default value: `"default"`

[m]: https://mermaid-js.github.io/mermaid/#/theming?id=deployable-themes
[hjs]: https://github.com/highlightjs/highlight.js/tree/main/src/styles

## Development

### Update highlight.js, mermaid, and mathjax

To update the included versions of highlight.js, mermaid, and mathjax, run:

```sh
./scripts/fetch-resources.sh
```

### Update GitHub CSS

> **Note**
> I haven't automated this because I'm hoping find somewhere else to obtain
> these CSS files. Someone else _must_ create and maintain files like these! I
> just haven't found them.

The github CSS files in the [resources/github_css][g] directory are slightly
modified versions of what can be generated from the [github-markdown-css][gmc]
project. To generate the CSS, run the `gen_github_css.js` script for every
theme:

```sh
node ./scripts/gen_github_css.js
```

Each run results in a file with two themes. Browsers automatically choose one of
the two themes, using the [prefers-color-scheme][c] media query.

I manually split each generated file into two files -- one for each theme -- by
removing the media query and putting the color variables in a `:root` binding.

[g]: ./resources/github_css
[gmc]: https://github.com/sindresorhus/github-markdown-css
[c]: https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

## See Also

- [grip-mode][grip-mode] - Grip-mode makes an API request to GitHub every time
  re-rendering is needed. The benefit is that you get _exactly_ what GitHub
  would show. This requires an internet connection, a round-trip to GitHub for
  every change, and there is very little potential for customizing the HTML.

- [Emacs Application Framework][eaf] - EAF extends Emacs to be able to use
  Python and Javascript functions, which somehow allows it run a browser and
  other really cool-looking stuff within Emacs.

  I haven't used this personally yet, as it's a much bigger change than just
  installing an Emacs package. But based on the examples, its integration with a
  browser is way better than what I've experienced with xwidget-webkit.

- [Centaur Emacs][centaur] - Centaur Emacs was the first implementation I found
  for viewing rendered markdown with xwidget-webkit, and helped me understand
  how to accomplish that.

[grip-mode]: https://github.com/seagle0128/grip-mode
[eaf]: https://github.com/emacs-eaf/emacs-application-framework
[centaur]: https://github.com/seagle0128/.emacs.d
