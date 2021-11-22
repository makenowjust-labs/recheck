import { check as fallback } from './fallback';
import { ensureAgent } from './agent';

export async function check(source, flags, config) {
  let signal = config.signal ?? null;
  if (signal) {
    delete config.signal;
  }

  const agent = ensureAgent();
  if (agent === null) {
    return await fallback(source, flags, config);
  }

  const { id, promise } = agent.request('check', { source, flags, config });
  signal?.addEventListener('abort', () => {
    agent.notify('cancel', { id });
  });

  return await promise;
}
