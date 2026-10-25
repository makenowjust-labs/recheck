import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';

import { availableBuilds, findBuild, formatBuild, sameBuild } from './builds';
import { loadChecker } from './checker';
import DemoInput from './DemoInput';
import DemoResult from './DemoResult';
import { extract, parseParams } from './params';
import { decode, encode, makeSnapshot, sameRequest, summarize } from './permalink';

import type { Diagnostics } from 'recheck';
import type { InvalidInput, Snapshot } from './permalink';

const Demo: React.FC = () => {
  const { siteConfig } = useDocusaurusContext();
  const builds = React.useMemo(() => availableBuilds(siteConfig.customFields ?? {}), [siteConfig.customFields]);
  const defaultBuild = builds[0]!;

  const methods = useForm();
  const [running, setRunning] = React.useState(false);
  const [current, setCurrent] = React.useState<Snapshot | null>(null);
  const [log, setLog] = React.useState<string[]>([]);
  const [recorded, setRecorded] = React.useState<Snapshot | null>(null);
  const [loadError, setLoadError] = React.useState<string | null>(null);
  const abortControllerRef = React.useRef<AbortController | null>(null);

  const build = findBuild(builds, String(methods.watch('version'))) ?? defaultBuild;

  React.useEffect(() => {
    const snapshot = decode(window.location.hash);
    if (snapshot !== null) {
      setRecorded(snapshot);
      const recordedBuild = findBuild(builds, formatBuild(snapshot.build)) ?? defaultBuild;
      methods.reset({ input: snapshot.input, ...snapshot.params, version: formatBuild(recordedBuild) });
    }
  }, []);

  const check = methods.handleSubmit(async (values) => {
    setRunning(true);
    setCurrent(null);
    setLog([]);
    setLoadError(null);

    const input = String(values['input']);
    const params = parseParams(values);
    const extracted = extract(input);
    const build = findBuild(builds, String(values['version'])) ?? defaultBuild;

    if (recorded !== null && !sameRequest(recorded, input, params)) {
      setRecorded(null);
    }

    let checker;
    try {
      checker = await loadChecker(build);
    } catch (error) {
      setLoadError(`Failed to load recheck ${formatBuild(build)}: ${error instanceof Error ? error.message : error}`);
      setRunning(false);
      return;
    }

    const start = Date.now();
    let result: Diagnostics | InvalidInput;
    if (extracted === null) {
      result = { status: 'invalid' };
    } else {
      const { logger, ...checkParams } = params;
      const controller = new AbortController();
      abortControllerRef.current = controller;
      result = await checker.check(extracted.source, extracted.flags, {
        ...checkParams,
        logger:
          logger === 'on'
            ? (message: string) => {
                console.log(message);
                setLog((log) => log.concat(message));
              }
            : undefined,
        signal: controller.signal,
      });
      abortControllerRef.current = null;
    }

    const snapshot = makeSnapshot({
      build,
      input,
      params,
      result,
      time: Date.now() - start,
      date: new Date().toISOString(),
    });
    window.history.replaceState(null, '', `#${encode(snapshot)}`);
    setCurrent(snapshot);
    setRunning(false);
  });

  const cancel = React.useCallback(() => {
    abortControllerRef.current?.abort();
  }, []);

  const permalink = typeof window === 'undefined' ? null : window.location.href;

  return (
    <FormProvider {...methods}>
      <div className="container">
        {build.type === 'head' ? (
          <div className="alert alert--info margin-vert--lg" role="note">
            HEAD is recheck built from commit {build.commit.slice(0, 7)} of the main branch. Its results may change
            between builds, so a permalink records the build that produced them.
          </div>
        ) : null}

        {loadError !== null ? (
          <div className="alert alert--danger margin-vert--lg" role="alert">
            {loadError}
          </div>
        ) : null}

        {recorded !== null && !sameBuild(recorded.build, build) ? (
          <div className="alert alert--warning margin-vert--lg" role="alert">
            This permalink was recorded with recheck {formatBuild(recorded.build)}, but the Playground now runs{' '}
            {formatBuild(build)}. Press Check to run the same input and parameters again and compare the results.
          </div>
        ) : null}

        <DemoInput running={running} check={check} cancel={cancel} builds={builds} defaultBuild={defaultBuild} />

        {running || current !== null ? (
          <DemoResult
            title={recorded !== null ? 'Current Result' : 'Result'}
            result={current?.result ?? null}
            time={current?.time ?? 0}
            log={log}
            build={current?.build ?? build}
            date={current?.date ?? null}
            permalink={current === null ? null : permalink}
          >
            {recorded !== null && current !== null ? (
              summarize(recorded.result) === summarize(current.result) ? (
                <div className="alert alert--success margin-bottom--md">The result matches the recorded one.</div>
              ) : (
                <div className="alert alert--warning margin-bottom--md">
                  The result differs from the recorded one: {summarize(recorded.result)}
                </div>
              )
            ) : null}
          </DemoResult>
        ) : null}

        {recorded !== null ? (
          <DemoResult
            title="Recorded Result"
            result={recorded.result}
            time={recorded.time}
            log={[]}
            build={recorded.build}
            date={recorded.date}
            permalink={permalink}
          />
        ) : null}
      </div>
    </FormProvider>
  );
};

export default Demo;
