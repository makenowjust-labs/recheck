import * as index from './index';

import * as env from './lib/env';
import * as java from './lib/java';
import * as native from './lib/native';

const RECHECK_JAR = `${__dirname}/../../../modules/recheck-cli/target/scala-2.13/recheck.jar`;
const RECHECK_BIN = `${__dirname}/../../../modules/recheck-cli/target/native-image/recheck`;

jest.mock('./lib/env');
jest.mock('./lib/java');
jest.mock('./lib/native');

beforeEach(() => {
  index.__mock__.agent = undefined;
});

afterEach(() => {
  index.__mock__.agent?.kill();
  index.__mock__.agent = undefined;
});

test('check: auto (java)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('auto');
  const jar = jest.spyOn(env, 'RECHECK_JAR');
  jar.mockReturnValueOnce(RECHECK_JAR);
  const ensure = jest.spyOn(java, 'ensure');
  ensure.mockImplementationOnce(jest.requireActual('./lib/java').ensure);

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
  expect(ensure).toHaveBeenCalled();
});

test('check: auto (pure)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('auto');
  const javaEnsure = jest.spyOn(java, 'ensure');
  javaEnsure.mockResolvedValueOnce(null);
  const nativeEnsure = jest.spyOn(native, 'ensure');
  nativeEnsure.mockResolvedValueOnce(null);

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test('check: auto (pure, error)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('auto');
  const javaEnsure = jest.spyOn(java, 'ensure');
  javaEnsure.mockRejectedValueOnce(new Error('java.ensure error'));
  const nativeEnsure = jest.spyOn(native, 'ensure');
  nativeEnsure.mockRejectedValueOnce(new Error('native.ensure error'));

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test('check: java (1)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('java');
  const jar = jest.spyOn(env, 'RECHECK_JAR');
  jar.mockReturnValueOnce(RECHECK_JAR);
  const ensure = jest.spyOn(java, 'ensure');
  ensure.mockImplementationOnce(jest.requireActual('./lib/java').ensure);

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
  expect(ensure).toHaveBeenCalled();
});

test('check: java (2)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('java');
  const ensure = jest.spyOn(java, 'ensure');
  ensure.mockResolvedValueOnce(null);

  expect(index.check('^(a|a)+$', '')).rejects.toThrowError('there is no available implementation');
});

// This test is skipped because the native binary is not available on CI environment.
test.skip('check: native (1)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('native');
  const bin = jest.spyOn(env, 'RECHECK_BIN');
  bin.mockReturnValueOnce(RECHECK_BIN);
  const ensure = jest.spyOn(native, 'ensure');
  ensure.mockImplementationOnce(jest.requireActual('./lib/native').ensure);

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
  expect(ensure).toHaveBeenCalled();
});

test('check: native (2)', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('native');
  const ensure = jest.spyOn(native, 'ensure');
  ensure.mockResolvedValueOnce(null);

  expect(index.check('^(a|a)+$', '')).rejects.toThrowError('there is no available implementation');
});

test('check: pure', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('pure');

  const diagnostics = await index.check('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
});

test('check: pure', async () => {
  const backend = jest.spyOn(env, 'RECHECK_BACKEND');
  backend.mockReturnValueOnce('invalid' as any);

  expect(index.check('^(a|a)+$', '')).rejects.toThrowError('invalid backend: invalid');
});

test('checkSync', () => {
  const diagnostics = index.checkSync('^(a|a)+$', '');
  expect(diagnostics.status).toBe('vulnerable');
});
