import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';
import * as recheck from 'recheck/lib/browser';

import DemoInput from './DemoInput';
import DemoResult from './DemoResult';
import { extract, parseParams } from './params';
import { decode, encode, formatBuild, sameBuild } from './permalink';

import type { Build, Snapshot } from './permalink';

const buildOf = (customFields: Record<string, unknown>): Build => {
  const commit = customFields['commit'];
  return typeof commit === 'string' && commit !== '' ? { type: 'head', commit } : { type: 'dev' };
};

const summarize = (result: Snapshot['result']): string =>
  result.status === 'vulnerable' ? `${result.status} (${result.complexity.summary})` : result.status;

const Demo: React.FC = () => {
  const { siteConfig } = useDocusaurusContext();
  const build = buildOf(siteConfig.customFields ?? {});

  const methods = useForm();
  const [running, setRunning] = React.useState(false);
  const [current, setCurrent] = React.useState<Snapshot | null>(null);
  const [log, setLog] = React.useState<string[]>([]);
  const [recorded, setRecorded] = React.useState<Snapshot | null>(null);
  const abortControllerRef = React.useRef<AbortController | null>(null);

  React.useEffect(() => {
    const snapshot = decode(window.location.hash);
    if (snapshot !== null) {
      setRecorded(snapshot);
      methods.reset({ input: snapshot.input, ...snapshot.params });
    }
  }, []);

  const check = methods.handleSubmit(async (values) => {
    setRunning(true);
    setCurrent(null);
    setLog([]);

    const input = String(values['input']);
    const params = parseParams(values);
    const extracted = extract(input);

    if (
      recorded !== null &&
      (recorded.input !== input || JSON.stringify(recorded.params) !== JSON.stringify(params))
    ) {
      setRecorded(null);
    }

    const start = Date.now();
    let result: Snapshot['result'];
    if (extracted === null) {
      result = { input, status: 'invalid' };
    } else {
      const { logger, ...checkParams } = params;
      const controller = new AbortController();
      abortControllerRef.current = controller;
      result = await recheck.check(extracted.source, extracted.flags, {
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

    const snapshot: Snapshot = {
      v: 1,
      build,
      input,
      params,
      result,
      time: Date.now() - start,
      date: new Date().toISOString(),
    };
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
            This Playground runs recheck built from commit {build.commit.slice(0, 7)} of the main branch. Results may
            change between builds, so a permalink records the build it was produced with.
          </div>
        ) : null}

        {recorded !== null && !sameBuild(recorded.build, build) ? (
          <div className="alert alert--warning margin-vert--lg" role="alert">
            This permalink was recorded with recheck {formatBuild(recorded.build)}, but the Playground now runs{' '}
            {formatBuild(build)}. Press Check to run the same input and parameters again and compare the results.
          </div>
        ) : null}

        <DemoInput running={running} check={check} cancel={cancel} />

        {running || current !== null ? (
          <DemoResult
            title={recorded !== null ? 'Current Result' : 'Result'}
            result={current?.result ?? null}
            time={current?.time ?? 0}
            log={log}
            build={build}
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
