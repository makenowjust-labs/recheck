import { check } from './lib/recheck';

const TIMEOUT = 30000;

const extract = input => {
  if (!input.startsWith("/")) return null;
  const lastSlashPos = input.lastIndexOf('/');
  if (lastSlashPos === 0) return null;
  return {
    source: input.slice(1, lastSlashPos),
    flags: input.slice(lastSlashPos + 1),
  };
};

onmessage = e => {
  const { input } = e.data;
  const extracted = extract(input);
  if (extracted === null) {
    postMessage({ status: 'unknown', error: { kind: 'invalid', message: 'invalid input' } });
    return;
  }
  const { source, flags } = extracted;
  const result = check(source, flags, { timeout: TIMEOUT, checker: 'hybrid' });
  postMessage(result);
};
