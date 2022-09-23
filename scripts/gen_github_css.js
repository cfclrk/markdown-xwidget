import githubMarkdownCss from 'generate-github-markdown-css';

console.log(
  await githubMarkdownCss(
    {light: 'light', dark: 'dark'}
  )
);
