import * as child_process from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

const osNames = {
  darwin: 'macos',
  linux: 'linux',
  win32: 'windows',
};

const cpuNames = {
  x64: 'x64',
};

const getCLIPath = () => {
  const RECHECK_PATH = process.env['RECHECK_PATH'] || null;
  if (RECHECK_PATH !== null) {
    return RECHECK_PATH;
  }

  const os = osNames[process.platform];
  const cpu = cpuNames[process.arch];
  const isWin32 = os === 'windows';
  if (!os || !cpu) {
    return null;
  }

  try {
    const dir = require.resolve(`recheck-${os}-${cpu}`);
    const fullPath = path.join(dir, isWin32 ? 'recheck.exe' : 'recheck');
    if (!fs.existsSync(fullPath)) {
      return null;
    }

    return fullPath;
  } catch {
    return null;
  }
};

class Agent {
  constructor(child) {
    this.child = child;
    this.id = 0;
    this.refs = new Map();
    this.isRunning = true;
    this.handle();
  }

  request(method, params) {
    const id = this.id++;
    const promise = new Promise((resolve, reject) => {
      const object = {
        jsonrpc: '2.0',
        id,
        method,
        params
      };
      const text = JSON.stringify(object) + '\n';
      this.child.stdin.write(text);
      this.refs.set(id, {resolve, reject});
    });
    return { id, promise };
  }

  notify(method, params) {
    const object = {
      jsonrpc: '2.0',
      method,
      params,
    };
    const text = JSON.stringify(object) + '\n';
    this.child.stdin.write(text);
  }

  handle() {
    let remainingLastLine = '';

    const handleLine = (line) => {
      if (line === '') {
        return;
      }

      const { id, result } = JSON.parse(line);
      const ref = this.refs.get(id) ?? null;
      if (ref === null) {
        return;
      }

      ref.resolve(result);
      this.refs.delete(id);
    };

    this.child.stdout.on('data', (data) => {
      const text = data.toString('utf-8');
      const lines = text.split('\n');

      const firstLine = lines.shift() ?? '';
      const lastLine = lines.pop() ?? '';

      handleLine(remainingLastLine + firstLine);
      for (const line of lines) {
        handleLine(line);
      }

      remainingLastLine = lastLine;
    });
  }
}

let agent = null;
const defaultCWD = process.cwd();
export const ensureAgent = () => {
  if (agent) {
    return agent;
  }

  const cli = getCLIPath();
  if (cli === null) {
    return null;
  }

  const child = child_process.spawn(cli, ['agent'], {
    windowsHide: true,
    stdio: ['pipe', 'pipe', 'inherit'],
    cwd: defaultCWD,
  });

  agent = new Agent(child);
  return agent;
};
