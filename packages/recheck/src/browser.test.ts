import * as browser from './browser';

test('check', async () => {
  const diagnostics = await browser.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
});

test('checkSync', () => {
  const diagnostics = browser.checkSync('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
});
