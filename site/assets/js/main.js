import * as params from '@params';

const worker = (() => {
  let webWorker = null;
  return {
    run(input) {
      if (webWorker === null) {
        webWorker = new Worker(params.workerJS);
      }

      webWorker.postMessage({ input });
      return new Promise(resolve => {
        const handle = e => {
          resolve(e.data);
          webWorker.removeEventListener('message', handle);
        };
        webWorker.addEventListener('message', handle);
      });
    },
    cancel() {
      if (webWorker === null) return;

      webWorker.terminate();
      webWorker = null;
    },
  };
})();

window.checker = () => ({
  state: 'init', // One of 'init', 'checking' or 'checked'.
  input: '/^(a|a)*$/',
  checkedInput: '',
  checkedTime: 0,
  checkedResult: null,
  async check() {
    if (this.state === 'checking') return;

    this.state = 'checking';
    this.checkedInput = '';
    this.checkedTime = 0;
    this.checkedResult = null;

    const input = this.input;
    const startTime = Date.now();
    const result = await worker.run(input);
    const checkedTime = Date.now() - startTime;

    this.state = 'checked';
    this.checkedInput = input;
    this.checkedTime = checkedTime;
    this.checkedResult = result;
  },
  cancel() {
    this.state = 'init';
    this.checkedInput = '';
    this.checkedTime = 0;
    this.checkedResult = null;

    worker.cancel();
  },
  get checkedHotspot() {
    const input = this.checkedResult.source;
    let index = 0;
    const parts = [];
    for (const { start, end, temperature } of this.checkedResult.hotspot) {
      if (index < start) parts.push({ text: input.substring(index, start) });
      parts.push({ text: input.substring(start, end), temperature });
      index = end;
    }
    if (index < input.length) parts.push({ text: input.substring(index) });
    return parts;
  },
});
