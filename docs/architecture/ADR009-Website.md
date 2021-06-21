# ADR009: Website

**Status**: Accepted and active.

## Context

quick-lint-js needs a website for marketing purposes but also as a way to
distribute installation and usage documentation.

Static HTML files are easy to manage for simple websites. However,
quick-lint-js' website has enough complex pages that manually maintaining static
HTML files becomes painful. Generated files need to be manually re-generated
when the source changes. Repeated elements such as navigation are error-prone
and time-consuming to write and update.

## Decision

[EJS][]-based template files are checked into the source repository.

Node.js-based software converts the EJS-based template files into HTML. A
dynamic web server exists for developers, and a build script exists for
deployment to GitHub Pages (or any other static file server).

HTML generation logic is written in Node.js as much as possible, and is invoked
from EJS template files.

## Consequences

Contributors and users can no longer see most of the website on their local
machine by just downloading a copy of the source repository. A build step is
needed to convert sources into HTML.

Generated HTML code is always up-to-date because generation is coupled with
deployment.

Code generation scripts are still necessary for man pages and other non-web
output formats.

[EJS]: https://ejs.co/
