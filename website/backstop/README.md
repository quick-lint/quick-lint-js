# Website screenshot testing with BackstopJS

Want to see the impact of your CSS or HTML changes? Try this screenshotting
tool.

1. Start dev server: `cd website/ && yarn start`
2. Take golden screenshots: `cd website/backstop/ && yarn test && yarn approve`
3. Make your CSS changes.
4. Take new screenshots and compare: `cd website/backstop/ && yarn test`
5. If things look good, then mark them as golden:
   `cd website/backstop/ && yarn approve`
